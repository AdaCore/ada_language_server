import * as vscode from 'vscode';
import * as path from 'path';
import * as fs from 'fs';
import { XMLParser } from 'fast-xml-parser';
import { ContextClients } from './clients';
import { getProjectFile, getObjectDir } from './helpers';
import { integer } from 'vscode-languageclient';

export let controller: vscode.TestController;
export let testRunProfile: vscode.TestRunProfile;

/*
    Types definition for the Gnattest XML file structure
*/
export type Root = {
    tests_mapping: TestMapping;
};

type TestMapping = {
    '@_mode': string;
    additional_tests: string;
    unit: [Unit] | Unit;
};

type Unit = {
    '@_source_file': string;
    test_unit: TestUnit;
};

type TestUnit = {
    '@_target_file': string;
    tested: [Tested] | Tested;
};

type Tested = {
    '@_line': string;
    '@_name': string;
    test_case: TestCase | [TestCase];
};

type TestCase = {
    test: Test;
};

type Test = {
    '@_column': string;
    '@_line': string;
    '@_file': string;
    '@_name': string;
};

/*
    The Main function to initialize the test view
*/
export async function initializeTestView(
    context: vscode.ExtensionContext,
    clients: ContextClients
) {
    if (vscode.workspace.workspaceFolders !== undefined) {
        controller = vscode.tests.createTestController(
            'gnattest-test-controller',
            'GNATtest Test Controller'
        );
        context.subscriptions.push(controller);

        await clients.adaClient.onReady();
        // Getting Paths Information from the server
        const projectFile = await getProjectFile(clients.adaClient);
        const objectDir: string = await getObjectDir(clients.adaClient);
        const gnattestPath = path.join(objectDir, 'gnattest');

        startTestRun(controller, projectFile, gnattestPath);
        await discoverTests(controller, gnattestPath);
    }
}
/*
    Run Profile and other options configuration for the Test Controller
*/
function startTestRun(
    controller: vscode.TestController,
    projectFile: string,
    gnattestPath: string
) {
    // terminal ID to seperate between each run
    let terminal_id = 0;
    // the controller's Run Handler
    const runHandler = (request: vscode.TestRunRequest) => {
        if (request.include == undefined) {
            // The Run All tests request
            const run = controller.createTestRun(request, undefined, false);
            const tests = gatherChildTestItems(controller.items);
            const terminal_name = 'Test_terminal_' + terminal_id.toString();
            // Run all tests handler
            handleRunAll(tests, run, terminal_name, gnattestPath);
            terminal_id++;
            // Parse the results when the terminal is closed
            vscode.window.onDidCloseTerminal(async (terminal) => {
                if (terminal.name == terminal_name) {
                    const file = await readResultFile(path.join(gnattestPath, 'result.txt'));
                    if (file != undefined) {
                        parseResults(tests, run, file);
                    }
                    run.end();
                }
            });
        } else {
            // specifique tests run request
            const run = controller.createTestRun(request, undefined, false);
            const tests = gatherChildTestItems(request.include);
            // create a temporary terminal to execute the command lines then close it.
            const terminalName = 'Test_terminal_' + terminal_id.toString();
            // test unit run handler
            handleUnitRun(tests, run, terminalName, gnattestPath);
            terminal_id++;
            // Parse the results when the terminal is closed
            vscode.window.onDidCloseTerminal(async (terminal) => {
                if (terminal.name == terminalName) {
                    const file = await readResultFile(path.join(gnattestPath, 'result.txt'));
                    if (file != undefined) {
                        parseResults(tests, run, file);
                    }
                    run.end();
                }
            });
        }
    };

    testRunProfile = controller.createRunProfile(
        'GNATtest',
        vscode.TestRunProfileKind.Run,
        runHandler,
        true,
        undefined
    );
    // Tests Configuration Handler to Generates Tests for a Project.
    testRunProfile.configureHandler = () => {
        const terminal = vscode.window.createTerminal('Test Terminal');
        terminal.sendText('gnattest -P ' + projectFile);
        terminal.sendText('exit');
    };
    // Refresh Button to re discover the tests on the project.
    controller.refreshHandler = async () => {
        controller.items.forEach((item) => {
            controller.items.delete(item.id);
        });
        await discoverTests(controller, gnattestPath);
    };
}

/*
    Run all tests handler
*/
export function handleRunAll(
    tests: vscode.TestItem[],
    run: vscode.TestRun,
    terminalName: string,
    gnattestPath: string
) {
    tests.forEach((item) => {
        run.appendOutput(`Running ${item.id}\r\n`);
        run.started(item);
    });
    // create a temporary terminal to execute the command lines then close it.
    const ext: string = process.platform == 'win32' ? '.exe' : '';
    const terminal = vscode.window.createTerminal(terminalName);
    terminal.sendText('gprbuild -P ' + path.join(gnattestPath, 'harness', 'test_driver.gpr'));
    terminal.sendText(
        path.join(gnattestPath, 'harness', 'test_runner' + ext) +
            ' > ' +
            path.join(gnattestPath, 'result.txt')
    );
    terminal.sendText('exit');
}

/*
    test unit/case run handler
*/
function handleUnitRun(
    tests: vscode.TestItem[],
    run: vscode.TestRun,
    terminalName: string,
    gnattestPath: string
) {
    const terminal = vscode.window.createTerminal(terminalName);
    const ext: string = process.platform == 'win32' ? '.exe' : '';
    // clean the previous results
    terminal.sendText('> ' + path.join(gnattestPath, 'result.txt'));
    // run every test case seperatly and append the results
    for (const test of tests) {
        run.appendOutput(`Running ${test.id}\r\n`);
        run.started(test);
        const parent = getParentTestSourceName(test);
        const p: integer | undefined = test.parent?.range?.start.line;
        const line: integer = p ? p + 1 : 0;
        terminal.sendText('gprbuild -P ' + path.join(gnattestPath, 'harness', 'test_driver.gpr'));
        terminal.sendText(
            path.join(gnattestPath, 'harness', 'test_runner' + ext) +
                ' --routines=' +
                parent.id +
                ':' +
                line.toString() +
                ' >> ' +
                path.join(gnattestPath, 'result.txt')
        );
    }
    terminal.sendText('exit');
}

/*
    Resolves Tests to run for a selected test item in the Explorer
*/
export function gatherChildTestItems(
    collection: vscode.TestItemCollection | readonly vscode.TestItem[]
): vscode.TestItem[] {
    let items: vscode.TestItem[] = [];
    collection.forEach((item) => {
        if (item.children.size == 0) {
            items.push(item);
        } else {
            items = items.concat(gatherChildTestItems(item.children));
        }
    });
    return items;
}

/*
    Gets the Source file name for a test item
    Needed for the --routines switch
*/
export function getParentTestSourceName(item: vscode.TestItem) {
    let parent: vscode.TestItem = item;
    if (item.parent != undefined) {
        parent = getParentTestSourceName(item.parent);
    }
    return parent;
}

/*
    Return the test_runner output stored in the result.txt file
*/
export async function readResultFile(resultPath: string) {
    if (vscode.workspace.workspaceFolders !== undefined) {
        if (pathExists(resultPath)) {
            const file = await vscode.workspace.fs.readFile(vscode.Uri.file(resultPath));
            return file.toString();
        }
    }
    return undefined;
}

enum Test_State {
    PASSED = 'PASSED',
    FAILED = 'FAILED',
}

/*
    Parses the result of the file 'result.txt'
*/
export function parseResults(
    tests: vscode.TestItem[],
    run: vscode.TestRun | undefined,
    file: string
): boolean {
    const matchs = file.match(
        /(^|\n)((\w|-)+).ad[b|s]:\d+:\d+: (info|error): corresponding test (\w+)/g
    );
    if (matchs) {
        for (let i = 0; i < matchs.length; i++) {
            matchs[i] = matchs[i].replace(/\n/, '');
            for (const e of tests) {
                // Check if the result line is for the test 'e'
                const test_src = getParentTestSourceName(e);
                const p: integer | undefined = e.parent?.range?.start.line;
                const test_line: integer = p ? p + 1 : 0;
                const check_line = matchs[i].match(test_src.label + ':' + test_line.toString());
                // update the state of the test
                if (check_line != null && run != undefined) {
                    run.appendOutput(`Completed ${e.id}\r\n`);
                    const mm: string = matchs[i].substring(matchs[i].length - 6, matchs[i].length);
                    if (mm == Test_State.PASSED) {
                        run.passed(e);
                    } else {
                        run.failed(e, new vscode.TestMessage(matchs[i]));
                    }
                }
            }
        }
        return true;
    }
    return false;
}

/*
    Return the tests structure stored in gnattest.xml file
*/
export async function readXMLfile(harnessPath: string): Promise<string | undefined> {
    if (vscode.workspace.workspaceFolders !== undefined) {
        const mainPath = vscode.workspace.workspaceFolders[0].uri.path;
        const fullHarnessPath = path.join(mainPath, harnessPath);
        let file;
        if (pathExists(fullHarnessPath)) {
            file = await vscode.workspace.fs.readFile(
                vscode.Uri.file(path.join(fullHarnessPath, 'gnattest.xml'))
            );
        }
        return file?.toString().replace(/>\s+</g, '><').trim();
    }
    return undefined;
}

/*
    Discover tests by parsing the xml input
*/
export async function discoverTests(controller: vscode.TestController, gnattestPath: string) {
    if (vscode.workspace.workspaceFolders !== undefined) {
        const mainPath = vscode.workspace.workspaceFolders[0].uri.path;
        const file = await readXMLfile(path.join(gnattestPath, 'harness'));
        const options = {
            ignoreAttributes: false,
            attributeNamePrefix: '@_',
        };
        if (file !== undefined) {
            const parser = new XMLParser(options);
            const xmlDoc: Root = parser.parse(file) as Root;
            const rootNode = xmlDoc.tests_mapping;
            if (rootNode.unit instanceof Array) {
                for (const u of rootNode.unit) {
                    addUnitTestItems(u, controller, mainPath);
                }
            } else {
                addUnitTestItems(rootNode.unit, controller, mainPath);
            }
            return xmlDoc;
        }
    }
    return undefined;
}

/*
    Creating nested test items to visuliaze in the view
*/
function addUnitTestItems(unit: Unit, controller: vscode.TestController, mainPath: string) {
    const srcFile = unit['@_source_file'];
    const srcPath = findFile(srcFile, mainPath);
    const uri = vscode.Uri.file(srcPath);
    const testUnit = controller.createTestItem(srcFile, srcFile, uri);
    const tested = unit.test_unit.tested;
    if (tested instanceof Array) {
        for (const t of tested) {
            addChildCases(controller, testUnit, t, mainPath);
        }
    } else {
        addChildCases(controller, testUnit, tested, mainPath);
    }
    controller.items.add(testUnit);
}

/*
    Adding Test Cases to a Test Unit Item
*/
function addChildCases(
    controller: vscode.TestController,
    parentUnit: vscode.TestItem,
    tested: Tested,
    mainPath: string
) {
    const featureName = tested['@_name'];
    const range = new vscode.Range(
        new vscode.Position(parseInt(tested['@_line']) - 1, 1),
        new vscode.Position(parseInt(tested['@_line']) - 1, 1)
    );
    const item = controller.createTestItem(featureName, featureName, parentUnit.uri);
    item.range = range;
    if (tested.test_case instanceof Array) {
        for (const e of tested.test_case) {
            addChildTests(controller, item, e, mainPath);
        }
    } else {
        addChildTests(controller, item, tested.test_case, mainPath);
    }
    parentUnit.children.add(item);
}

/*
    Adding Test Childs to a Test Case Item
*/
function addChildTests(
    controller: vscode.TestController,
    parentCase: vscode.TestItem,
    testCase: TestCase,
    mainPath: string
) {
    const test: Test = testCase.test;
    const testfile = test['@_file'];
    const testName = test['@_name'];

    const testrange = new vscode.Range(
        new vscode.Position(parseInt(test['@_line']), 1),
        new vscode.Position(parseInt(test['@_line']), 1)
    );
    const testuri = vscode.Uri.file(findFile(testfile, mainPath));
    const itemChild = controller.createTestItem(testName, testName, testuri);
    itemChild.range = testrange;
    parentCase.children.add(itemChild);
}

/*
    looks for a specifique file in the workspace
*/
function findFile(name: string, directory: string): string {
    const files = fs.readdirSync(directory);
    for (const file of files) {
        const absolute: string = path.join(directory, file);
        if (fs.statSync(absolute).isDirectory()) {
            const res = findFile(name, absolute);
            if (res != '') return res;
        } else {
            if (file == name) {
                return absolute;
            }
        }
    }
    return '';
}

/*
    Checking if a path/file exists
*/
export function pathExists(p: string): boolean {
    try {
        fs.accessSync(p);
    } catch (err) {
        return false;
    }

    return true;
}
