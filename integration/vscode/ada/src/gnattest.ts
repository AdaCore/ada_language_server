import assert from 'assert';
import * as cp from 'child_process';
import { X2jOptions, XMLParser } from 'fast-xml-parser';
import * as fs from 'fs';
import * as path from 'path';
import * as vscode from 'vscode';
import { TestItem } from 'vscode';
import { CancellationToken } from 'vscode-languageclient';
import { adaExtState } from './extension';
import { addCoverageData, GnatcovFileCoverage } from './gnatcov';
import { getScenarioArgs } from './gnatTaskProvider';
import { escapeRegExp, exe, getObjectDir, setTerminalEnvironment } from './helpers';
import {
    DEFAULT_PROBLEM_MATCHER,
    findTaskByName,
    runTaskAndGetResult,
    runTaskSequence,
    SimpleTaskDef,
    TASK_BUILD_TEST_DRIVER,
    TASK_GNATCOV_SETUP,
    TASK_TYPE_ADA,
} from './taskProviders';

export let controller: vscode.TestController;
export let testRunProfile: vscode.TestRunProfile;
export let testCoverageProfile: vscode.TestRunProfile;

/**
 * This test controller is not used for actually running tests. It is merely
 * used for loading an existing GNATcoverage report obtained outside VS Code.
 */
let fileLoadController: vscode.TestController;

/**
 * Types definition for the Gnattest XML file structure. The types match the XML
 * generated by src/test-mapping.adb in the libadalang-tools repository. However
 * these types do not describe the entire XML structure. Only the elements used
 * are described.
 */
export type Root = {
    tests_mapping: TestMapping;
};

type TestMapping = {
    '@_mode': string;
    unit: Unit[];
    additional_tests: object[];
};

type Unit = {
    '@_source_file': string;
    test_unit: TestUnit[];
};

type TestUnit = {
    '@_target_file': string;
    tested: Tested[];
};

type Tested = {
    '@_line': string;
    '@_column': string;
    '@_name': string;
    test_case: TestCase[];
};

type TestCase = {
    '@_name': string;
    '@_line': string;
    '@_column': string;
    test: Test;
};

type Test = {
    '@_file': string;
    '@_line': string;
    '@_column': string;
    '@_name': string;
};

/**
 * The XML paths in GNATtest XML files that should always be parsed as arrays.
 * This is based the code in src/test-mapping.adb in the libadalang-tools
 * repository which emits the XML.
 */
const alwaysArray = [
    'tests_mapping.unit',
    'tests_mapping.unit.test_unit',
    'tests_mapping.unit.test_unit.tested',
    'tests_mapping.unit.test_unit.tested.test_case',
    'tests_mapping.additional_tests',
];

/**
 * The types of TestItem nodes that we will be creating in the tree of TestItems
 * maintained by the test controller.
 */
enum TestItemType {
    Unit,
    Subprogram,
    Test,
}

/**
 * A data object that TestItems map to for additional data from the XML parsing.
 */
type TestItemData = {
    type: TestItemType;
    data?: object;
};

/**
 * A map from TestItem objects to their respective TestItemData objects.
 */
const testData: Map<vscode.TestItem, TestItemData> = new Map();

let watcher: vscode.FileSystemWatcher;

/**
 * This function initializes the test features of the extension.
 *
 * @param context - the ExtensionContext
 * @returns the {@link TestController}
 */
export function initializeTesting(context: vscode.ExtensionContext): vscode.TestController {
    controller = vscode.tests.createTestController(
        'gnattest-test-controller',
        'GNATtest Test Controller',
    );
    context.subscriptions.push(controller);

    controller.resolveHandler = resolveHandler;

    // Refresh Button to re discover the tests on the project.
    controller.refreshHandler = refreshTestItemTree;

    configureTestExecution(controller);

    /**
     * Initialize the controller responsible for loading existing GNATcov
     * reports on demand.
     */
    fileLoadController = vscode.tests.createTestController(
        'gnatcoverage-report-loader',
        'GNATcoverage Report Loader',
    );

    return controller;
}

/**
 * Reset and recreate the tree of TestItems based on the GNATtest XML.
 */
export async function refreshTestItemTree() {
    controller.items.replace([]);
    testData.clear();
    await addTestsRootLevel();
}

/**
 * @returns the full path to the GNATtest XML file.
 */
async function getGnatTestXmlPath(): Promise<string> {
    const objDir = await getObjectDir();
    const gnatTestXmlPath = path.join(objDir, 'gnattest', 'harness', 'gnattest.xml');
    return gnatTestXmlPath;
}

/**
 *
 * @returns the full path to the GNATtest test driver GPR project.
 */
export async function getGnatTestDriverProjectPath(): Promise<string> {
    const objDir = await getObjectDir();
    const testDriverPath = path.join(objDir, 'gnattest', 'harness', 'test_driver.gpr');
    return testDriverPath;
}

/**
 *
 * @returns the full path to the GNATtest test driver executable.
 */
export async function getGnatTestDriverExecPath(): Promise<string> {
    const objDir = await getObjectDir();
    const testDriverPath = path.join(objDir, 'gnattest', 'harness', 'test_runner' + exe);
    return testDriverPath;
}

/**
 * Parse the GNATtest XML file and create the top-level TestItems in the test
 * controller for later lazy resolution.
 */
export async function addTestsRootLevel() {
    if (fs.existsSync(await getGnatTestXmlPath())) {
        const xmlDoc: Root = await parseGnatTestXml();
        const rootNode = xmlDoc.tests_mapping;
        for (const u of rootNode.unit) {
            await addUnitItem(u);
        }
    }
}

/**
 * @returns the object resulting from parsing the GNATtest XML file.
 */
async function parseGnatTestXml(): Promise<Root> {
    const gnatTestXmlUri = vscode.Uri.file(await getGnatTestXmlPath());
    const fileContent = await vscode.workspace.fs.readFile(gnatTestXmlUri);
    const fileContentAsBuffer = Buffer.from(fileContent);

    const options: Partial<X2jOptions> = {
        // By default the parser ignores attributes, so we set this option
        // to obtain attributes.
        ignoreAttributes: false,
        // This prefix is used in the JS objects resulting from the parsing
        // to differentiate attributes from child nodes.
        attributeNamePrefix: '@_',
        isArray: (_, jPath) => {
            return alwaysArray.indexOf(jPath) !== -1;
        },
    };
    const parser = new XMLParser(options);
    const xmlDoc: Root = parser.parse(fileContentAsBuffer) as Root;
    return xmlDoc;
}

/**
 * Create a TestItem for a GNATtest Unit node.
 *
 * @param unit - a Unit node from the GNATtest XML
 */
async function addUnitItem(unit: Unit) {
    const srcFile = unit['@_source_file'];
    const srcUri = await findFileInWorkspace(srcFile);
    const unitId = createUnitItemId(unit);
    if (!controller.items.get(unitId)) {
        const testItem = controller.createTestItem(
            unitId,
            `Tests for source file - ${srcFile}`,
            srcUri,
        );

        /**
         * To implement lazy loading of children, we set canResolveChildren and
         * store the XML data in a map so that it can be processed later when the UI
         * needs it. The API guarantees that the testItem object will be the same at
         * resolve-time, i.e. it can be used as a key in a data map.
         */
        testItem.canResolveChildren = true;
        testData.set(testItem, { type: TestItemType.Unit, data: unit });

        controller.items.add(testItem);
    }
}

/**
 * Resolve a TestItem by computing its children node based on the GNATtest XML.
 *
 * @param testItem - the TestItem node
 * @param unit - the corresponding Unit node from the GNATtest XML
 */
function resolveUnitItem(testItem: TestItem, unit: Unit) {
    for (const t of unit.test_unit.flatMap((u) => u.tested)) {
        addTestedItem(testItem, t);
    }
}

/**
 * Create a TestItem corresponding to a node from the GNATtest XML. The new
 * TestItem is added as a child of parentTestItem.
 *
 * @param parentTestItem - parent TestItem
 * @param tested - the "Tested" node from the GNATtest XML
 */
function addTestedItem(parentTestItem: vscode.TestItem, tested: Tested) {
    const testedSubprogramName = tested['@_name'];
    const pos = new vscode.Position(
        parseInt(tested['@_line']) - 1,
        parseInt(tested['@_column']) - 1,
    );
    const range = new vscode.Range(pos, pos);

    const testItemId = createSubprogramItemId(tested);
    const existingItem = parentTestItem.children.get(testItemId);
    if (!existingItem) {
        /**
         * If no item exist for this id, create it.
         */
        const testedItem = controller.createTestItem(
            testItemId,
            `Tests for subprogram - ${testedSubprogramName}`,
            parentTestItem.uri,
        );
        testedItem.range = range;

        /**
         * To implement lazy loading of children, we set canResolveChildren and
         * store the XML data in a map so that it can be processed later when the UI
         * needs it. The API guarantees that the testItem object will be the same at
         * resolve-time, i.e. it can be used as a key in a data map.
         */
        testedItem.canResolveChildren = true;
        testData.set(testedItem, { type: TestItemType.Subprogram, data: tested });

        parentTestItem.children.add(testedItem);
    }
}

/**
 * Resolve a TestItem by computing its children node based on the GNATtest XML.
 *
 * @param testItem - the TestItem to resolve
 * @param tested - the corresponding "Tested" node in the GNATtest XML
 */
async function resolveTestedItem(testItem: TestItem, tested: Tested) {
    for (const e of tested.test_case) {
        await addTestCaseItem(testItem, e);
    }
}

/**
 * Create a TestItem corresponding to a node from the GNATtest XML. The new
 * TestItem is added as a child of parentTestItem.
 *
 * @param parentItem - the parent TestItem
 * @param testCase - the "TestCase" node from the GNATtest XML
 */
async function addTestCaseItem(parentItem: vscode.TestItem, testCase: TestCase) {
    const test: Test = testCase.test;
    const testFileBasename = test['@_file'];
    const pos = new vscode.Position(parseInt(test['@_line']), parseInt(test['@_column']) - 1);
    const range = new vscode.Range(pos, pos);
    const testUri = await findFileInWorkspace(testFileBasename);

    // The name of the source file of the tested subprogram is not part of the
    // TestCase object, so we have to get it from the TestItem tree 2 levels
    // above.
    const srcFileName = parentItem.parent?.id;
    assert(srcFileName);
    const testId = createTestCaseItemId(testCase, srcFileName);

    if (!parentItem.children.get(testId)) {
        /**
         * We use the test ID as a name so that it can be easily matched by the User
         * with the test driver output if needed.
         */
        const testCaseItem = controller.createTestItem(testId, testId, testUri);
        testCaseItem.range = range;
        parentItem.children.add(testCaseItem);
        testData.set(testCaseItem, { type: TestItemType.Test, data: testCase });
    }
}

/**
 *
 * @param unit - a Unit object from the GNATtest XML
 * @returns a unique identifier to be used with the corresponding TestItem in VS Code
 */
function createUnitItemId(unit: Unit) {
    return unit['@_source_file'];
}

/**
 *
 * @param tested - a Tested object from the GNATtest XML
 * @returns an identifier to be used with the corresponding TestItem in VS Code
 * that is unique within the parent TestItem
 */
function createSubprogramItemId(tested: Tested) {
    /**
     * Due to the possibility of overloading in Ada, the subprogram name might
     * not be unique within the file. So this ID includes more info to ensure
     * uniqueness.
     */
    return `${tested['@_name']} ${tested['@_line']}:${tested['@_column']}`;
}

/**
 *
 * @param testCase - a TestCase object from the GNATtest XML
 * @param sourceFileName - the tested source file of the TestCase object (since
 * this information is not stored within the TestCase object itself)
 * @returns a unique identifier to be used with the corresponding TestItem in VS
 * Code. We choose the same convention as GNATtest in order to match identifiers
 * in VS Code with test results on the test driver output.
 */
function createTestCaseItemId(testCase: TestCase, sourceFileName: string): string {
    /**
     * Only the line should be included, otherwise the test driver --routines
     * option would not recognize the test.
     */
    return `${sourceFileName}:${testCase['@_line']}`;
}

/**
 *
 * @param basename - a basename
 * @returns - the Uri of the first file found in the workspace with the given basename.
 * @throws an Error in case no matching file is found in the workspace
 */
async function findFileInWorkspace(basename: string): Promise<vscode.Uri> {
    const matchingFiles = await vscode.workspace.findFiles(`**/${basename}`, undefined, 1);
    if (matchingFiles.length > 0) {
        return matchingFiles[0];
    } else {
        throw Error(`Source file ${basename} not found in workspace`);
    }
}

/**
 * @param p - path to file or directory
 * @returns true if the path exists and access rights allow reading it, otherwise false
 */
export function pathIsReadable(p: string): boolean {
    try {
        if (!fs.existsSync(p)) {
            return false;
        }
        fs.accessSync(p, fs.constants.R_OK);
        return true;
    } catch {
        return false;
    }
}

/**
 * This handler is called when the UI needs to expand a TestItem that has
 * children. It is supposed to compute the children of the TestItem being
 * expanded.
 *
 * @param item - the TestItem whose children must be computed, or `undefined` if
 * we should compute the root items of the tree.
 */
export async function resolveHandler(
    item: TestItem | undefined,
    recursive = false,
    token?: CancellationToken,
) {
    if (!item) {
        if (!watcher) {
            /**
             * Register a file watcher to listen to changes to the GNATtest XML file
             */
            watcher = vscode.workspace.createFileSystemWatcher(await getGnatTestXmlPath());
            // Add to extension disposables
            adaExtState.context.subscriptions.push(watcher);

            watcher.onDidCreate(refreshTestItemTree);
            watcher.onDidDelete(refreshTestItemTree);
            watcher.onDidChange(refreshTestItemTree);
        }

        // Perform an initial load of tests
        await refreshTestItemTree();
    } else {
        const testItemData = testData.get(item);
        assert(testItemData?.type !== undefined);
        switch (testItemData.type) {
            case TestItemType.Unit:
                resolveUnitItem(item, testItemData.data as Unit);
                break;
            case TestItemType.Subprogram:
                await resolveTestedItem(item, testItemData.data as Tested);
                break;
            case TestItemType.Test:
                /**
                 *  This type of node should be the leaf of the tree so nothing
                 *  to do here.
                 */
                break;
        }

        if (recursive) {
            const promises: Promise<void>[] = [];
            item.children.forEach((i) => {
                if (token?.isCancellationRequested) {
                    throw new vscode.CancellationError();
                }
                promises.push(resolveHandler(i, true, token));
            });
            await Promise.all(promises);
        }
    }
}

/**
 * Configure the handlers needed on the test controller to support test execution.
 *
 * @param controller - the TestController to configure
 */
function configureTestExecution(controller: vscode.TestController) {
    testRunProfile = controller.createRunProfile(
        'GNATtest',
        vscode.TestRunProfileKind.Run,
        runHandler,
    );
    testCoverageProfile = controller.createRunProfile(
        'GNATtest (with coverage)',
        vscode.TestRunProfileKind.Coverage,
        (r, t) => runHandler(r, t, true),
    );
    testCoverageProfile.loadDetailedCoverage = loadDetailedCoverage;
}

/**
 * This handler is called when the User trigger an action in the UI intended to
 * run tests. The request might be about a subset of tests, or all tests.
 *
 * @param request - the request based on the User selections
 * @param token - a cancellation token
 */
export async function runHandler(
    request: vscode.TestRunRequest,
    token?: vscode.CancellationToken,
    coverage: boolean = false,
) {
    /**
     * We remove the usage of handleRunAll temporarily to factorize all test
     * execution logic in handleRunRequestedTests.
     *
     * The difference between the two is that handleRunAll calls the test
     * harness once to run all tests, while handleRunRequestedTests makes
     * multiple invocation, one for each test, and is thus more flexible hence
     * prioritizing it.
     */

    // if ((request.include?.length ?? 0) === 0 && (request.exclude?.length ?? 0) === 0) {
    //     /**
    //      * Run all tests. This ignores request.exclude which is why we only use
    //      * it in this branch.
    //      */
    //     await handleRunAll(request, token, coverage);
    // } else {

    /**
     * Run a specific set of tests
     */
    await handleRunRequestedTests(request, token, coverage);
    // }
}

/**
 * This logic calls computes a list of tests to run based on request.include (or
 * controller.items) and request.exclude. It then runs the test driver for each
 * test, using the --routines argument at each run to select a specific test.
 */
async function handleRunRequestedTests(
    request: vscode.TestRunRequest,
    token?: CancellationToken,
    coverage = false,
) {
    const run = controller.createTestRun(request, undefined, false);
    try {
        const requestedRootTests = [];

        if (request.include) {
            requestedRootTests.push(...request.include);
        } else {
            /**
             * Consider all tests as included
             */
            controller.items.forEach((i) => requestedRootTests.push(i));
        }

        /**
         * First resolve included tests as the API says that it is the
         * responsibility of the run handler.
         */
        await Promise.all(requestedRootTests.map((i) => resolveHandler(i, true, token)));

        /**
         * Collect and filter tests to run.
         */
        const requestedLeafTests = (
            await Promise.all(requestedRootTests.map(async (i) => await collectLeafItems(i, token)))
        ).flat();
        const excludedLeafTests = request.exclude
            ? (
                  await Promise.all(
                      request.exclude.map(async (i) => await collectLeafItems(i, token)),
                  )
              ).flat()
            : [];
        const testsToRun = requestedLeafTests.filter((t) => {
            if (token?.isCancellationRequested) {
                throw new vscode.CancellationError();
            }
            return !excludedLeafTests?.includes(t);
        });

        /**
         * Mark tests as queued
         */
        testsToRun.forEach((t) => run.enqueued(t));

        /**
         * Build the test driver
         */
        await buildTestDriverAndReportErrors(run, testsToRun, coverage);

        if (token?.isCancellationRequested) {
            throw new vscode.CancellationError();
        }

        /**
         * Invoke the test driver for each test
         */
        const execPath = await getGnatTestDriverExecPath();
        const tracesDir = path.dirname(execPath);

        function getTracePath(test: TestItem): string {
            return path.join(tracesDir, test.id + '.srctrace');
        }

        /**
         * Use environment provided by terminal.integrated.env.* for test execution.
         */
        const env = { ...process.env };
        setTerminalEnvironment(env);

        for (const test of testsToRun) {
            if (token?.isCancellationRequested) {
                throw new vscode.CancellationError();
            }
            const start = Date.now();
            run.started(test);
            const cmd = [execPath, '--passed-tests=show', `--routines=${test.id}`];

            if (coverage) {
                /**
                 * Set the name of the trace file for coverage analysis.
                 */
                env['GNATCOV_TRACE_FILE'] = getTracePath(test);
            }

            const driver = logAndRun(run, cmd, env);
            const duration = Date.now() - start;
            if (driver.status !== null) {
                /**
                 * The child process was spawned successfully so we can use its
                 * status and outputs.
                 */
                const driverOutput = driver.stdout.toLocaleString();
                prepareAndAppendOutput(run, driverOutput);
                prepareAndAppendOutput(run, driver.stderr.toLocaleString());

                if (driver.status == 0) {
                    determineTestOutcome(test, driverOutput, run, duration);
                } else {
                    const msg =
                        `The test driver ended with an error code: ` +
                        `${driver.status.toString()}`;
                    run.appendOutput(msg + '\r\n');
                    run.errored(test, new vscode.TestMessage(msg));
                }
            } else if (driver.error) {
                /**
                 * The child process failed to spawn and there's an error.
                 */
                run.errored(test, new vscode.TestMessage(driver.error.toString()));
            } else {
                /**
                 * The child process failed to spawn and there's an error.
                 */
                run.errored(
                    test,
                    new vscode.TestMessage(
                        `Failed to spawn the test command: ${cmd.toLocaleString()}`,
                    ),
                );
            }
        }

        if (coverage) {
            /**
             * Produce a GNATcov XML report
             */
            const outputDir = path.join(await adaExtState.getObjectDir(), 'cov-xml');
            const adaTP = adaExtState.getAdaTaskProvider()!;
            const gnatcovReportTask = (await adaTP.resolveTask(
                new vscode.Task(
                    {
                        type: TASK_TYPE_ADA,
                        command: 'gnatcov',
                        args: [
                            'coverage',
                            '-P',
                            await getGnatTestDriverProjectPath(),
                            '--level=stmt',
                            '--annotate=xml',
                            `--output-dir=${outputDir}`,
                        ].concat(testsToRun.map(getTracePath)),
                    },
                    vscode.TaskScope.Workspace,
                    `Create GNATcoverage XML report`,
                    TASK_TYPE_ADA,
                    undefined,
                    DEFAULT_PROBLEM_MATCHER,
                ),
            ))!;
            gnatcovReportTask.presentationOptions.reveal = vscode.TaskRevealKind.Never;
            const result = await runTaskAndGetResult(gnatcovReportTask);
            if (result != 0) {
                const msg =
                    `Error while running coverage analysis.` +
                    ` See the [Terminal](command:terminal.focus) view for more information.`;
                void vscode.window.showErrorMessage(msg);
            } else {
                /**
                 * Convert GNATcoverage coverage report to VS Code
                 */
                await addCoverageData(run, outputDir);
            }
        }
    } finally {
        run.end();
    }
}

/**
 * Build the test driver and report build failure as errors on the tests
 * requested for execution.
 *
 * @param run - the TestRun hosting the execution
 * @param testsToRun - the tests requested for execution - build failure will be
 * reported on those tests.
 */
async function buildTestDriverAndReportErrors(
    run: vscode.TestRun,
    testsToRun: vscode.TestItem[],
    coverage: boolean,
) {
    class WriteEmitter extends vscode.EventEmitter<string> {
        override fire(data: string): void {
            run.appendOutput(data);
        }
    }

    const buildTasks = [];
    if (coverage) {
        const adaTP = adaExtState.getAdaTaskProvider()!;

        const instTaskDef: SimpleTaskDef = {
            type: TASK_TYPE_ADA,
            command: 'gnatcov',
            args: ['instrument', '--level=stmt', '-P', await getGnatTestDriverProjectPath()].concat(
                getScenarioArgs(),
            ),
        };
        const instTask = (await adaTP.resolveTask(
            new vscode.Task(
                instTaskDef,
                vscode.TaskScope.Workspace,
                `GNATcoverage - Generate instrumented sources for coverage analysis`,
                TASK_TYPE_ADA,
                undefined,
                DEFAULT_PROBLEM_MATCHER,
            ),
        ))!;
        instTask.presentationOptions.reveal = vscode.TaskRevealKind.Never;

        const buildTaskDef: SimpleTaskDef = {
            type: TASK_TYPE_ADA,
            command: 'gprbuild',
            args: [
                '-m',
                '-s',
                '--src-subdirs=gnatcov-instr',
                '--implicit-with=gnatcov_rts.gpr',
                '-P',
                await getGnatTestDriverProjectPath(),
            ]
                .concat(getScenarioArgs())
                .concat(['-cargs', '-g', '-fdump-scos', '-fpreserve-control-flow']),
        };
        const buildTask = (await adaTP.resolveTask(
            new vscode.Task(
                buildTaskDef,
                vscode.TaskScope.Workspace,
                `GNATcoverage - Build GNATtest harness project in coverage mode`,
                TASK_TYPE_ADA,
                undefined,
                DEFAULT_PROBLEM_MATCHER,
            ),
        ))!;
        buildTask.presentationOptions.reveal = vscode.TaskRevealKind.Never;

        buildTasks.push(instTask, buildTask);
    } else {
        const task = await findTaskByName(`${TASK_BUILD_TEST_DRIVER}`);
        buildTasks.push(task);
    }

    const result = await runTaskSequence(buildTasks, new WriteEmitter());

    if (result != 0) {
        let msg =
            `Error while building the test harness project.` +
            ` Check the [Problems](command:workbench.panel.markers.view.focus) view` +
            ` and the [Terminal](command:terminal.focus) view for more information.`;

        if (coverage) {
            const taskName = `${TASK_TYPE_ADA}: ${TASK_GNATCOV_SETUP.label}`;
            const taskNameEncoded = taskName.replace(/:/g, '%3A').replace(/ /g, '%20');
            msg +=
                `\n\nIf the errors relate to the GNATcoverage runtime library,` +
                ` remember to set it up using the` +
                ` [${taskName}]` +
                `(command:workbench.action.tasks.runTask?%22${taskNameEncoded}%22) task.`;
        }

        prepareAndAppendOutput(run, msg);

        const md = new vscode.MarkdownString(msg);
        md.isTrusted = true;
        const testMsg = new vscode.TestMessage(md);
        /**
         * Mark each test as errored, not failed, since the tests can't run
         * because of the build error.
         */
        testsToRun.forEach((test) => run.errored(test, testMsg));
        throw Error(msg);
    }
}

/**
 * VS Code terminals expect `\r\n` as a line separator, so this function is
 * a wrapper to prepare the given output to use that line separator and
 * append it to the TestRun object.
 *
 * @param out - a string to write to the TestRun object
 */
function prepareAndAppendOutput(run: vscode.TestRun, out: string) {
    run.appendOutput(out.replace(/\n/g, '\r\n'));
}

/**
 * Handle a run request of all tests. This logic calls the test driver without
 * the test selection argument `--routines`. This way if the usage of --routines
 * in {@link handleRunRequestedTests} fails because of GNATtest shortcomings, we
 * still have this approach of running all tests as a backup.
 */
// eslint-disable-next-line @typescript-eslint/no-unused-vars
async function handleRunAll(
    request: vscode.TestRunRequest,
    token?: CancellationToken,
    coverage = false,
) {
    const run = controller.createTestRun(request, undefined, false);
    try {
        /**
         * If the run request was created with the 'Tests: Run All Tests'
         * command before activating the Testing view, then the controller is
         * still empty. In that case let's refresh it to load the tests.
         */
        if (controller.items.size == 0) {
            await controller.refreshHandler!(token ?? new vscode.CancellationTokenSource().token);
        }

        /**
         * Collect all tests, i.e. all leafs of the TestItem tree.
         */
        const allTests: TestItem[] = await collectLeafsFromCollection(controller.items, token);

        /**
         * Mark tests as queued
         */
        allTests.forEach((t) => run.enqueued(t));

        /**
         * Build the test driver
         */
        await buildTestDriverAndReportErrors(run, allTests, coverage);

        if (token?.isCancellationRequested) {
            throw new vscode.CancellationError();
        }

        /**
         * Mark all tests as started.
         */
        allTests.forEach((t) => {
            if (token?.isCancellationRequested) {
                throw new vscode.CancellationError();
            }
            run.started(t);
        });

        /**
         * Invoke the test driver
         */
        run.appendOutput('Running the test driver\r\n');
        const execPath = await getGnatTestDriverExecPath();
        const driver = logAndRun(run, [execPath, '--passed-tests=show']);
        const driverOutput = driver.stdout.toLocaleString();
        prepareAndAppendOutput(run, driverOutput);
        prepareAndAppendOutput(run, driver.stderr.toLocaleString());

        for (const test of allTests) {
            if (token?.isCancellationRequested) {
                throw new vscode.CancellationError();
            }
            determineTestOutcome(test, driverOutput, run);
        }

        if (driver.status && driver.status !== 0) {
            const msg = `The test driver ended with an error code: ${driver.status.toString()}`;
            run.appendOutput(msg + '\r\n');
            throw Error(msg);
        }
    } finally {
        run.end();
    }
}

/**
 * Decide the outcome of the given test based on the test driver output, and
 * report it to the TestRun object.
 *
 * @param test - the test whose outcome must be decided
 * @param driverOutput - the test driver standard output
 * @param run - the TestRun object to report the outcome on
 * @param duration - the duration of execution of the test to be reported along
 * with the outcome, if the information is available.
 */
export function determineTestOutcome(
    test: vscode.TestItem,
    driverOutput: string,
    run: vscode.TestRun,
    duration?: number,
) {
    const escapedTestId = escapeRegExp(test.id);
    const passRE = new RegExp(`^${escapedTestId}(:\\d*)?: info: corresponding test PASSED$`, 'gm');
    const failureRE = new RegExp(`^${escapedTestId}(:\\d*)?:\\s*(error.*)$`, 'gm');

    const passMatches = Array.from(driverOutput.matchAll(passRE));
    const failureMatches = Array.from(driverOutput.matchAll(failureRE));

    if (passMatches.length == 0 && failureMatches.length == 0) {
        // No matches. We can't determine the test outcome.
        run.errored(
            test,
            new vscode.TestMessage(
                `Could not determine the outcome of the test from the ` +
                    `test driver output. Check the output of the test ` +
                    `run for the test id: ${test.id}`,
            ),
            duration,
        );
    } else if (passMatches.length > 0 && failureMatches.length == 0) {
        if (passMatches.length == 1) {
            run.passed(test, duration);
        } else {
            run.errored(
                test,
                [
                    new vscode.TestMessage(
                        'Detected multiple pass messages for this test, ' +
                            'this could indicate an error in the test run.',
                    ),
                ].concat(passMatches.map((m) => new vscode.TestMessage(m[0]))),
                duration,
            );
        }
    } else if (passMatches.length == 0 && failureMatches.length > 0) {
        if (failureMatches.length == 1) {
            run.failed(test, new vscode.TestMessage(failureMatches[0][0]), duration);
        } else {
            run.errored(
                test,
                [
                    new vscode.TestMessage(
                        'Detected multiple error messages for this test, ' +
                            'this could indicate an error in the test run.',
                    ),
                ].concat(failureMatches.map((m) => new vscode.TestMessage(m[0]))),
                duration,
            );
        }
    } else if (passMatches.length > 0 && failureMatches.length > 0) {
        run.errored(
            test,
            [
                new vscode.TestMessage(
                    'Detected both pass and fail messages for this test, ' +
                        'this probably indicates an error in the test run.',
                ),
            ]
                .concat(passMatches.map((m) => new vscode.TestMessage(m[0])))
                .concat(failureMatches.map((m) => new vscode.TestMessage(m[0]))),
            duration,
        );
    }
}

/**
 *
 * @param items - a {@link vscode.TestItemCollection}
 * @param token - a cancellation token to stop the traversal
 * @param resolve - whether to resolve items that can have children or process
 * them as they are
 * @returns the array of leaf TestItems reachable from the given collection.
 */
export async function collectLeafsFromCollection(
    items: vscode.TestItemCollection,
    token?: CancellationToken,
    resolve: boolean = true,
): Promise<vscode.TestItem[]> {
    const res: vscode.TestItem[] = [];

    /**
     * items.forEach() wouldn't work here because each iteration produces a
     * promise that we can't await. Ideally we could have used
     * Promise.all(items.map(...)) but the TestItemCollection type doesn't
     * offer a map method. So instead we use a regular for-loop and await in
     * each iteration.
     */
    for (const [, item] of items) {
        if (token?.isCancellationRequested) {
            throw new vscode.CancellationError();
        }
        res.push(...(await collectLeafItems(item, token, resolve)));
    }
    return res;
}

/**
 *
 * @param item - a {@link TestItem}
 * @param token - a cancellation token to stop the traversal
 * @param resolve - whether to resolve items that can have children or process
 * them as they are
 * @returns the array of leaf TestItems reachable from the given TestItem
 */
export async function collectLeafItems(
    item: TestItem,
    token?: CancellationToken,
    resolve: boolean = true,
): Promise<vscode.TestItem[]> {
    /**
     * If the TestItem has never been expanded in the UI, its children may have
     * not been populated yet. Force a resolve operation to load the children.
     */
    if (resolve && item.canResolveChildren && controller.resolveHandler) {
        await controller.resolveHandler(item);
    }

    if (item.children.size > 0) {
        const res: vscode.TestItem[] = [];

        /**
         * items.forEach() wouldn't work here because each iteration produces a
         * promise that we can't await. Ideally we could have used
         * Promise.all(items.map(...)) but the TestItemCollection type doesn't
         * offer a map method. So instead we use a regular for-loop and await in
         * each iteration.
         */
        for (const [, i] of item.children) {
            if (token?.isCancellationRequested) {
                throw new vscode.CancellationError();
            }
            res.push(...(await collectLeafItems(i, token, resolve)));
        }

        return res;
    } else {
        return [item];
    }
}

/**
 *
 * Run a command line as a child process and pipe the output into the TestRun
 * object managing the test execution.
 *
 * @param run - the TestRun object hosting the execution
 * @param cmd - a command line to run
 * @returns the child process object returned by {@link cp.spawnSync}
 */
function logAndRun(
    run: vscode.TestRun,
    cmd: string[],
    env?: NodeJS.ProcessEnv,
): cp.SpawnSyncReturns<Buffer> {
    run.appendOutput(`$ ${cmd.map((arg) => `"${arg}"`).join(' ')}\r\n`);
    return cp.spawnSync(cmd[0], cmd.slice(1), { env: env });
}

/**
 *
 * @param _testRun - The test run
 * @param fileCoverage - The FileCoverage object for which we want to load detailed coverage
 * @param token - a cancellation token
 * @returns
 */
async function loadDetailedCoverage(
    _testRun: vscode.TestRun,
    fileCoverage: vscode.FileCoverage,
    token: CancellationToken,
): Promise<vscode.FileCoverageDetail[]> {
    try {
        return await (fileCoverage as GnatcovFileCoverage).load(token);
    } catch (err) {
        let msg = `Error while loading detailed coverage data`;
        if (err instanceof Error) {
            msg += `:\n${err.toString()}`;
        }
        void vscode.window.showErrorMessage(msg);
    }

    return [];
}

/**
 * Load an external GNATcoverage XML report in the VS Code UI.
 *
 * @param indexXmlPath - path of the index.xml file of a GNATcoverage XML report
 */
export async function loadGnatCoverageReport(indexXmlPath: string) {
    const request = new vscode.TestRunRequest(
        undefined,
        undefined,
        /**
         * Associate the request with the coverage profile so that it becomes
         * possible to load detailed coverage data for each file.
         */
        testCoverageProfile,
    );
    const run = fileLoadController.createTestRun(request, path.basename(indexXmlPath), false);

    const covDir = path.dirname(indexXmlPath);
    await addCoverageData(run, covDir);
    run.end();
}
