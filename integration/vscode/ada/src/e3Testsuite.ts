import { spawn } from 'child_process';
import { existsSync, readFileSync } from 'fs';
import path from 'path';
import { tmpNameSync } from 'tmp';
import * as vscode from 'vscode';
import * as yaml from 'yaml';
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
    log?: string;
};

const showLoadTestListErrorCmdId = 'e3-testsuite.showLoadTestListError';
let lastLoadError: string = '';

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

    const controller = vscode.tests.createTestController('e3-testsuite', 'e3-testsuite');
    context.subscriptions.push(controller);

    const testData: Map<vscode.TestItem, TestInfo> = new Map();

    controller.refreshHandler = async function () {
        controller.items.replace([]);
        testData.clear();

        const ts: Testsuite = getTestsuite();

        if (!existsSync(ts.uri.fsPath)) {
            return;
        }

        const rootItem = this.createTestItem(
            getRootItemId(),
            vscode.workspace.asRelativePath(ts.uri),
            ts.uri,
        );
        controller.items.add(rootItem);

        const jsonFname = tmpNameSync({ postfix: '.json' });
        const cmd = [ts.python, ts.uri.fsPath, `--list-json=${jsonFname}`];

        console.log(`Loading tests from: ${ts.uri.fsPath}`);
        const testList: TestInfo[] = await new Promise((resolve, reject) => {
            const output: Buffer[] = [];
            const fullOutput: Buffer[] = [];
            const p = spawn(cmd[0], [...cmd].splice(1), {
                cwd: vscode.workspace.workspaceFolders![0].uri.fsPath,
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
                if (!existsSync(jsonFname)) {
                    reject(new Error(`Expected JSON file not found: ${jsonFname}`));
                }

                if (code !== 0) {
                    lastLoadError = `Error getting test list from testsuite.py, ran: ${cmd.join(
                        ' ',
                    )}\n${Buffer.concat(fullOutput).toString()}`;
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
        });

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
    };

    const profile = controller.createRunProfile(
        'e3-testsuite',
        vscode.TestRunProfileKind.Run,
        async function (request, token) {
            const ts = getTestsuite();
            const cmd = [ts.python, ts.uri.fsPath, '--failure-exit-code=0'];

            const remainingSet: Set<vscode.TestItem> = new Set();
            function appendLeafs(i: vscode.TestItem) {
                if (i.children.size > 0) {
                    i.children.forEach(appendLeafs);
                } else {
                    remainingSet.add(i);
                }
            }

            function onlyRootSelected(rq: vscode.TestRunRequest) {
                return rq.include?.length === 1 && rq.include[0].id === getRootItemId();
            }

            if (request.include && !onlyRootSelected(request)) {
                request.include.forEach(appendLeafs);
                remainingSet.forEach((item) => {
                    const data = testData.get(item)!;
                    cmd.push(data.test_matcher ?? data.test_dir);
                });
            } else {
                /**
                 * Do not append tests to the command line. Just run everything.
                 */
                controller.items.forEach(appendLeafs);
            }

            const remainingArray = [...remainingSet];

            const run = controller.createTestRun(request, 'e3-testsuite');
            run.appendOutput(`Running: ${cmd.map((c) => '"' + c + '"').join(' ')}\n\r`);

            const cwd = vscode.workspace.workspaceFolders![0].uri.fsPath;
            await new Promise<void>((resolve, reject) => {
                const p = spawn(cmd[0], cmd.splice(1), {
                    cwd: cwd,
                    env: getEnv(),
                });
                token.onCancellationRequested(() => {
                    p.kill();
                    run.appendOutput('\r\n*** Test run was cancelled');
                    remainingSet.forEach((item) =>
                        run.errored(item, new vscode.TestMessage('Test run was cancelled.')),
                    );
                    resolve();
                });
                remainingSet.forEach((t) => run.started(t));
                function handleChunk(chunk: string | Buffer) {
                    const decoded: string =
                        typeof chunk === 'string' ? chunk : chunk.toLocaleString();
                    run.appendOutput(decoded.replace(/\n/g, '\n\r'));

                    const statusLineReStr = `^INFO\\s+(${TestStatuses.join(
                        '|',
                    )})\\s+(([^ \\t\\n\\r:])+)(:\\s*(.*))?`;
                    const statusLineRe = RegExp(statusLineReStr);
                    const match = decoded.match(statusLineRe);
                    if (match) {
                        const status = match[1] as TestStatus;
                        const testName = match[2];
                        const message = match[5];
                        if (testName) {
                            const item = remainingArray.find((v) => v.id === testName);

                            if (item) {
                                reportResult(
                                    status,
                                    item,
                                    message ? [new vscode.TestMessage(message)] : undefined,
                                );
                                remainingSet.delete(item);
                            }
                        }
                    }
                }
                p.stdout.on('data', handleChunk);
                p.stderr.on('data', handleChunk);
                p.on('error', reject);
                p.on('close', (code) => {
                    if (code === 0) {
                        resolve();
                    } else {
                        reject(Error(''));
                    }
                });
            });

            const indexPath = path.join(cwd, 'out', 'new', '_index.json');
            if (existsSync(indexPath)) {
                const index = loadReportIndex(indexPath);
                for (const entry of index.entries) {
                    const item = remainingArray.find((i) => i.id === entry.test_name);
                    if (item) {
                        const testResultPath = path.join(path.dirname(indexPath), entry.filename);

                        if (existsSync(testResultPath)) {
                            const result = loadTestResult(testResultPath);
                            reportResult(
                                entry.status,
                                item,
                                result.log ? [new vscode.TestMessage(result.log)] : undefined,
                            );
                        }
                    }
                }
            }

            run.end();

            function reportResult(
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
        },
    );

    context.subscriptions.push(profile);

    vscode.window.withProgress(
        {
            location: vscode.ProgressLocation.Notification,
            title: 'Loading e3-testsuite tests',
        },
        async (_, token) => {
            await controller.refreshHandler!(token);
        },
    );
}

function getRootItemId(): string {
    return getTestsuite().uri.toString();
}

function getTestsuite() {
    const config = vscode.workspace.getConfiguration('e3-testsuite');
    const tsPath = config.get<string>('testsuitePath') ?? 'testsuite.py';
    const python = config.get<string>('python') ?? 'python';

    const tsAbsUri: vscode.Uri = path.isAbsolute(tsPath)
        ? vscode.Uri.file(tsPath)
        : vscode.Uri.joinPath(vscode.workspace.workspaceFolders![0].uri, tsPath);

    const ts: Testsuite = { uri: tsAbsUri, python: python };
    return ts;
}

function getEnv(): NodeJS.ProcessEnv | undefined {
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
