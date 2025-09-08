import { spawn } from 'child_process';
import { assert } from 'console';
import { existsSync, readFileSync } from 'fs';
import { EOL } from 'os';
import path from 'path';
import split from 'split2';
import { tmpNameSync } from 'tmp';
import * as vscode from 'vscode';
import * as yaml from 'yaml';
import { NotificationType, TestsuiteNotification } from './e3TestsuiteNotifications';
import { logger } from './extension';
import { setTerminalEnvironment } from './helpers';

interface Testsuite {
    uri: vscode.Uri;
    python: string;
}

interface TestInfo {
    test_name: string;
    test_env: { [key: string]: string };
    test_dir: string;
    test_matcher?: string;
}

const TestStatuses = [
    'PASS',
    'FAIL',
    'XFAIL',
    'XPASS',
    'VERIFY',
    'SKIP',
    'NOT_APPLICABLE',
    'ERROR',
] as const;

type TestStatus = (typeof TestStatuses)[number];

type IndexEntry = {
    test_name: string;
    status: TestStatus;
    msg?: string;
    filename: string;
};

type ReportIndex = {
    entries: IndexEntry[];
};

type TestResult = {
    test_name: string;
    status: TestStatus;
    msg?: string;
    log?: string;
    out?: string;
    expected?: string;
};

const showLoadTestListErrorCmdId = 'e3-testsuite.showLoadTestListError';
let lastLoadError: string = '';
export let controller: vscode.TestController;
export let runProfile: vscode.TestRunProfile;
export let runHandler: (
    request: vscode.TestRunRequest,
    token: vscode.CancellationToken,
) => Promise<void>;

// This method is called when your extension is activated
// Your extension is activated the very first time the command is executed
export function activateE3TestsuiteIntegration(context: vscode.ExtensionContext) {
    context.subscriptions.push(
        vscode.commands.registerCommand(showLoadTestListErrorCmdId, async () => {
            const doc = await vscode.workspace.openTextDocument({
                content: lastLoadError,
            });
            await vscode.window.showTextDocument(doc);
        }),
    );

    controller = vscode.tests.createTestController('e3-testsuite', 'e3-testsuite');
    context.subscriptions.push(controller);

    const testData: Map<vscode.TestItem, TestInfo> = new Map();

    const ts: Testsuite = getTestsuite();
    let rootItem: vscode.TestItem;

    controller.refreshHandler = async function () {
        controller.items.replace([]);
        testData.clear();

        if (!existsSync(ts.uri.fsPath)) {
            return;
        }

        rootItem = this.createTestItem(
            getRootItemId(ts),
            vscode.workspace.asRelativePath(ts.uri),
            ts.uri,
        );
        controller.items.add(rootItem);

        const jsonFname = tmpNameSync({ postfix: '.json' });
        const cmd = [ts.python, ts.uri.fsPath, `--list-json=${jsonFname}`];

        logger.info(`Loading tests from: ${ts.uri.fsPath}`);
        rootItem.busy = true;
        await new Promise<TestInfo[]>((resolve, reject) => {
            const output: Buffer[] = [];
            const fullOutput: Buffer[] = [];
            const p = spawn(cmd[0], [...cmd].splice(1), {
                cwd: vscode.workspace.workspaceFolders![0].uri.fsPath,
                /**
                 * This environment influences the resolution of the spawned
                 * executable. If it's a basename 'python', then the PATH
                 * variable in the environment given by getEnv() will decide
                 * which Python gets used.
                 */
                env: getEnv(),
            });
            p.stdout.on('data', (chunk) => {
                // eslint-disable-next-line @typescript-eslint/no-unsafe-argument
                output.push(chunk);
                // eslint-disable-next-line @typescript-eslint/no-unsafe-argument
                fullOutput.push(chunk);
            });
            // eslint-disable-next-line @typescript-eslint/no-unsafe-argument
            p.stderr.on('data', (chunk) => fullOutput.push(chunk));
            p.on('close', (code) => {
                lastLoadError = '';

                if (code !== 0) {
                    lastLoadError = `Error getting test list from testsuite.py`;
                } else if (!existsSync(jsonFname)) {
                    lastLoadError = `Expected JSON file not found: ${jsonFname}`;
                }

                if (lastLoadError.length > 0) {
                    // Append command output
                    lastLoadError += `\n$ ${cmd.join(' ')}\n${Buffer.concat(
                        fullOutput,
                    ).toString()}`;

                    // Create an item in the test tree to hold the error
                    const errorItem = this.createTestItem('error', 'Error');
                    errorItem.error = new vscode.MarkdownString(
                        `[Failed to load test list](command:${showLoadTestListErrorCmdId})`,
                    );
                    errorItem.error.isTrusted = true;
                    rootItem.children.add(errorItem);
                    reject(new Error(lastLoadError));
                }

                resolve(JSON.parse(readFileSync(jsonFname, { encoding: 'utf-8' })) as TestInfo[]);
            });
            p.on('error', reject);
        })
            .then((testList) => {
                for (const test of testList) {
                    const testYaml = path.join(test.test_dir, 'test.yaml');
                    const isYamlTest = existsSync(testYaml);

                    const item = this.createTestItem(
                        test.test_name,
                        test.test_name,
                        vscode.Uri.file(isYamlTest ? testYaml : test.test_dir),
                    );
                    item.range = isYamlTest ? new vscode.Range(0, 0, 0, 0) : undefined;
                    testData.set(item, test);
                    rootItem.children.add(item);
                }
            })
            .finally(() => {
                rootItem.busy = false;
            });
    };

    runHandler = async (
        request: vscode.TestRunRequest,
        token: vscode.CancellationToken,
    ): Promise<void> => {
        const enableEventSystem: boolean =
            vscode.workspace.getConfiguration('e3-testsuite').get('enableEventSystem') ?? true;

        const cmd = [ts.python, ts.uri.fsPath, '--failure-exit-code=0'];

        const remainingSet: Set<vscode.TestItem> = new Set();
        function collectAll(i: vscode.TestItem) {
            remainingSet.add(i);
            i.children.forEach(collectAll);
        }

        function onlyRootSelected(rq: vscode.TestRunRequest) {
            return rq.include?.length === 1 && rq.include[0].id === getRootItemId(ts);
        }

        if (request.include && !onlyRootSelected(request)) {
            const testIds = new Set<string>();
            request.include.forEach(collectAll);
            remainingSet.forEach((item) => {
                const data =
                    testData.get(item) ??
                    // If item is a sub-result, it doesn't have data. So use its parent.
                    (item.parent ? testData.get(item.parent) : undefined);
                if (data) {
                    testIds.add(data.test_matcher ?? data.test_dir);
                }
            });

            assert(testIds.size > 0);
            cmd.push(...testIds);
        } else {
            /**
             * Do not append tests to the command line. Just run everything.
             */
            controller.items.forEach(collectAll);
        }

        const run = controller.createTestRun(request, 'e3-testsuite');

        const env = getEnv();
        if (enableEventSystem) {
            const modulePath = context.asAbsolutePath('media');
            const module = 'e3_notify_vscode';
            const hook = 'init_callback';
            env['PYTHONPATH'] = (env['PYTHONPATH'] ?? '')
                .split(path.delimiter)
                .filter((v) => !!v)
                .concat(modulePath)
                .join(path.delimiter);
            cmd.push(`--notify-events=python:${module}:${hook}`);
        }

        /**
         * Append CLI args defined in settings
         */
        cmd.push(
            ...(vscode.workspace.getConfiguration('e3-testsuite').get<string[]>('args') ?? []),
        );

        run.appendOutput(`Running: ${cmd.map((c) => '"' + c + '"').join(' ')}\n\r`);

        const cwd = vscode.workspace.workspaceFolders![0].uri.fsPath;
        await new Promise<void>((resolve, reject) => {
            const p = spawn(cmd[0], cmd.splice(1), {
                cwd: cwd,
                env: env,
            });
            token.onCancellationRequested(() => {
                p.kill();
                run.appendOutput('\r\n*** Test run was cancelled');
                remainingSet.forEach((item) =>
                    run.errored(item, new vscode.TestMessage('Test run was cancelled.')),
                );
                resolve();
            });

            if (!enableEventSystem) {
                remainingSet.forEach((t) => run.started(t));
            }

            function handleLine(line: string | Buffer) {
                const decodedLine: string = typeof line === 'string' ? line : line.toLocaleString();

                if (enableEventSystem) {
                    handleTestsuiteEvents(run, decodedLine, remainingSet);
                } else {
                    handleTestsuiteOutput(run, decodedLine, remainingSet);
                }
            }
            p.stdout.pipe(split()).on('data', handleLine);
            p.stderr.pipe(split()).on('data', handleLine);
            p.on('error', reject);
            p.on('close', (code) => {
                if (code === 0) {
                    resolve();
                } else {
                    const md = new vscode.MarkdownString(
                        `Test run failed, see ` + `[Output](command:testing.showMostRecentOutput)`,
                    );
                    md.isTrusted = true;
                    const msg = new vscode.TestMessage(md);
                    remainingSet.forEach((t) => run.errored(t, msg));
                    run.end();
                    reject(Error('Test run failed, see Test Results view for testsuite output.'));
                }
            });
        })
            .then(() => {
                if (!enableEventSystem) {
                    const e3ResultsPath = path.join(cwd, 'out', 'new');
                    processTestsuiteResultIndex(e3ResultsPath, run);
                }
            })
            .finally(() => {
                /**
                 * Always end the run, be it a nominal or an error end.
                 */
                run.end();
            });
    };

    runProfile = controller.createRunProfile(
        'e3-testsuite',
        vscode.TestRunProfileKind.Run,
        runHandler,
    );
    context.subscriptions.push(runProfile);

    function processTestsuiteResultIndex(e3ResultsPath: string, run: vscode.TestRun) {
        const indexPath = path.join(e3ResultsPath, '_index.json');
        if (existsSync(indexPath)) {
            const index = loadReportIndex(indexPath);
            for (const entry of index.entries) {
                let item = rootItem.children.get(entry.test_name);
                if (!item) {
                    /**
                     * This result does not match an existing test name so
                     * create a test for it on the fly.
                     */
                    item = controller.createTestItem(entry.test_name, entry.test_name);
                    rootItem.children.add(item);
                }

                const testResultPath = path.join(e3ResultsPath, entry.filename);
                if (existsSync(testResultPath)) {
                    const result = loadTestResult(testResultPath);
                    reportResult(
                        run,
                        entry.status,
                        item,
                        result.log ? [new vscode.TestMessage(result.log)] : undefined,
                    );
                }
            }
        }
    }

    function reportResult(
        run: vscode.TestRun,
        status: string,
        item: vscode.TestItem,
        messages: vscode.TestMessage[] = [],
    ) {
        switch (status) {
            case 'PASS':
            case 'XFAIL':
                run.passed(item);
                break;

            case 'FAIL':
            case 'XPASS':
            case 'VERIFY':
                run.failed(item, messages);
                break;
            case 'SKIP':
                run.skipped(item);
                break;
            case 'NOT_APPLICABLE':
            case 'ERROR':
                run.errored(item, messages);
                break;
        }
    }

    function handleTestsuiteOutput(
        run: vscode.TestRun,
        line: string,
        remainingSet: Set<vscode.TestItem>,
    ) {
        run.appendOutput(line + '\r\n');

        const statusLineReStr = `^INFO\\s+(${TestStatuses.join(
            '|',
        )})\\s+(([^ \\t\\n\\r:])+)(:\\s*(.*))?`;
        const statusLineRe = RegExp(statusLineReStr);
        const match = line.match(statusLineRe);
        if (match) {
            const status = match[1] as TestStatus;
            const testName = match[2];
            const message = match[5];
            if (testName) {
                const item = rootItem.children.get(testName);

                if (item) {
                    reportResult(
                        run,
                        status,
                        item,
                        message ? [new vscode.TestMessage(message)] : undefined,
                    );
                    remainingSet.delete(item);
                }
            }
        }
    }

    const resultsMap: Map<vscode.TestItem, TestResult> = new Map();
    function handleTestsuiteEvents(
        run: vscode.TestRun,
        line: string,
        remainingSet: Set<vscode.TestItem>,
    ) {
        let notif: TestsuiteNotification | undefined;
        try {
            /**
             * Try to parse the line as JSON matching TestsuiteNotification
             */
            // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment
            notif = JSON.parse(line);
        } catch {
            notif = undefined;
        }

        if (notif?.kind) {
            const test = rootItem.children.get(notif.testName);
            if (!test) {
                nonFatalError(
                    `Received ${notif.kind} notification of a ` +
                        `non-existing test: ${notif.testName}`,
                );
                return;
            }
            switch (notif.kind) {
                case NotificationType.TestQueue: {
                    run.enqueued(test);
                    test.children.forEach((c) => run.enqueued(c));
                    break;
                }
                case NotificationType.TestStart: {
                    run.started(test);
                    test.children.forEach((c) => run.started(c));
                    break;
                }
                case NotificationType.TestResult: {
                    const result = loadTestResult(notif.resultPath);
                    if (result.test_name == test.id) {
                        /**
                         * The result is for the main test. However we can't
                         * know if this test also produces other sub-results.
                         * So we must delay reporting this result until we
                         * receive the notification of the end of the test. We
                         * store the test result in a map until then.
                         */
                        resultsMap.set(test, result);
                    } else {
                        /**
                         * Get or create a sub-result
                         */
                        let targetItem = test.children.get(result.test_name);
                        if (!targetItem) {
                            targetItem = controller.createTestItem(
                                result.test_name,
                                result.test_name,
                                test.uri,
                            );
                            test.children.add(targetItem);
                        }

                        /**
                         * Report the sub-result immediately
                         */
                        reportE3Result(run, result, targetItem);
                    }

                    break;
                }
                case NotificationType.TestEnd: {
                    const result = resultsMap.get(test);
                    if (result) {
                        /**
                         * A result has already been reported. Report it!
                         */
                        resultsMap.delete(test);
                        reportE3Result(run, result, test);
                    } else {
                        /**
                         * No result has been received for this test
                         * specifically. Either only sub-results were reported,
                         * or no results at all. Both cases are valid in
                         * e3-testsuite, and it's okay not to report a result
                         * for the test. Here's why.
                         *
                         * If only sub-results were reported, the test icon in
                         * the Test Explorer will reflect the status of its
                         * children sub-results.
                         *
                         * If no sub-results were reported, the test icon in
                         * the Test Explorer will remain blank, as if the test
                         * was not run. But this doesn't impact the computation
                         * of the icon of parent nodes so that's fine.
                         */
                    }
                    remainingSet.delete(test);
                    break;
                }
            }
        } else {
            /**
             * Normal non-notification output
             */
            run.appendOutput(line + '\r\n');
        }
    }

    function reportE3Result(run: vscode.TestRun, result: TestResult, targetItem: vscode.TestItem) {
        const messages = [];

        let diff;
        if (result.expected != undefined && result.out != undefined) {
            diff = new vscode.TestMessage(result.msg ?? 'Diff');
            diff.actualOutput = result.out;
            diff.expectedOutput = result.expected;
            messages.push(diff);

            if (result.log) {
                /**
                 * If there's a log, add it as a separate message
                 */
                messages.push(new vscode.TestMessage('Test Log' + EOL + result.log));
            }
        } else {
            /**
             * Combine short message and log into one message
             */
            messages.push(
                new vscode.TestMessage(
                    [result.msg, result.log].filter((v) => !!v).join(EOL + EOL) ||
                        'No message in test result',
                ),
            );
        }

        reportResult(run, result.status, targetItem, messages);
    }

    if (existsSync(ts.uri.fsPath)) {
        void vscode.window.withProgress(
            {
                location: vscode.ProgressLocation.Notification,
                title: 'Loading e3-testsuite tests',
            },
            async (_, token) => {
                if (controller.refreshHandler) {
                    await controller.refreshHandler(token);
                }
            },
        );
    }
}

function getRootItemId(ts: Testsuite): string {
    return ts.uri.toString();
}

/**
 * Retrieves the testsuite configuration for the current workspace.
 *
 * This function determines the testsuite path by first checking the user's configuration.
 * If no path is configured, it automatically searches through predefined candidate locations
 * and uses the first existing file. It also retrieves the Python executable configuration.
 *
 * @returns {Testsuite} An object containing the testsuite URI and Python executable path
 *
 * @remarks
 * The function searches for testsuite files in the following order:
 * 1. 'testsuite.py' in the workspace root
 * 2. 'testsuite/testsuite.py' relative to workspace root
 *
 * If no testsuite file is found, it defaults to the first candidate path.
 * The Python executable defaults to 'python' if not configured.
 *
 * @throws Will throw an error if no workspace folders are available
 */
export function getTestsuite() {
    const wsPath = vscode.workspace.workspaceFolders![0].uri.fsPath;

    const config = vscode.workspace.getConfiguration('e3-testsuite');
    const configuredPath = config.get<string>('testsuitePath');

    let tsPath;
    if (configuredPath) {
        tsPath = configuredPath;
    } else {
        /**
         * Automatically look through candidates and return the first one that exists.
         */
        const candidates = ['testsuite.py', path.join('testsuite', 'testsuite.py')];
        tsPath =
            candidates.map((p) => path.join(wsPath, p)).find((p) => existsSync(p)) ?? candidates[0];
    }

    const python = config.get<string>('python') ?? 'python';

    const tsAbsUri: vscode.Uri = path.isAbsolute(tsPath)
        ? vscode.Uri.file(tsPath)
        : vscode.Uri.joinPath(vscode.workspace.workspaceFolders![0].uri, tsPath);

    const ts: Testsuite = { uri: tsAbsUri, python: python };
    return ts;
}

function getEnv(): NodeJS.ProcessEnv {
    const env = { ...process.env };
    setTerminalEnvironment(env);
    return env;
}

function loadReportIndex(indexPath: string): ReportIndex {
    const indexText = readFileSync(indexPath, { encoding: 'utf-8' });
    const index = JSON.parse(indexText) as ReportIndex;
    return index;
}

const testStatusTag: yaml.ScalarTag = {
    tag: '!e3.testsuite.result.TestStatus',
    resolve(value: string) {
        const intVal = Number.parseInt(value);
        const index = intVal - 1;
        return TestStatuses[index];
    },
};
const testResultTag: yaml.CollectionTag = {
    tag: '!e3.testsuite.result.TestResult',
    collection: 'map',
};
function loadTestResult(testResultPath: string) {
    const result = yaml.parse(readFileSync(testResultPath, { encoding: 'utf-8' }), {
        customTags: [testResultTag, testStatusTag],
    }) as TestResult;
    return result;
}

function nonFatalError(msg: string) {
    void vscode.window.showErrorMessage(msg);
}
