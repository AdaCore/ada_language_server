import * as vscode from 'vscode';
import * as assert from 'assert';
import * as path from 'path';
import * as cp from 'child_process';
import { suite, test } from 'mocha';
import * as gnattest from '../../../src/gnattest';
import { adaExtState } from '../../../src/extension';
import { getObjectDir, getProjectFile } from '../../../src/helpers';
import { activate, assertEqualToFileContent } from '../utils';

suite('GNATtest Integration Tests', function () {
    this.beforeAll(async () => {
        await activate();
    });
    test('Generate Tests', async () => {
        const projectFile = await getProjectFile(adaExtState.adaClient);
        // Generate tests and redirect the stderr to stdout if command failed
        cp.execSync('gnattest -P ' + projectFile + ' 2>&1', { timeout: 60000 });
    });
    test('Build & Run the tests', () => {
        if (vscode.workspace.workspaceFolders) {
            const ext: string = process.platform == 'win32' ? '.exe' : '';
            const cwd = vscode.workspace.workspaceFolders[0].uri.fsPath;

            cp.execSync(
                'gprbuild -P ' + path.join(cwd, 'obj', 'gnattest', 'harness', 'test_driver.gpr'),
                { timeout: 60000 }
            );
            cp.execSync(
                path.join(cwd, 'obj', 'gnattest', 'harness', 'test_runner' + ext) +
                    ' > ' +
                    path.join(cwd, 'obj', 'gnattest', 'result.txt'),
                { timeout: 60000 }
            );
        } else {
            throw new Error('No workspace folder found for the specified URI');
        }
    });
    test('Expected Tests discovered', async () => {
        const root = await gnattest.discoverTests(
            gnattest.controller,
            path.join(await getObjectDir(adaExtState.adaClient), 'gnattest')
        );
        assert.notStrictEqual(root, undefined);
        const tests = gnattest.gatherChildTestItems(gnattest.controller.items);
        for (let i = 0; i < tests.length; i++) {
            assert.strictEqual(tests[i].label, expectedTests[i].name);
            assert.strictEqual(tests[i].range?.start.line, expectedTests[i].line);
        }
    });
    test('Read & Parse & compare the results', async () => {
        if (vscode.workspace.workspaceFolders) {
            const cwd = vscode.workspace.workspaceFolders[0].uri.fsPath;
            const resultPath = path.join(cwd, 'obj', 'gnattest', 'result.txt');
            const result = await gnattest.readResultFile(resultPath);
            assert.notStrictEqual(
                result,
                undefined,
                'Could not read the result file ' + resultPath
            );
            if (result != undefined) {
                const expectedUri = vscode.Uri.joinPath(
                    vscode.workspace.workspaceFolders[0].uri,
                    'result.txt.expected'
                );
                assert.strictEqual(gnattest.parseResults([], undefined, result), true);
                assertEqualToFileContent(result, expectedUri);
            }
        } else {
            throw new Error('No workspace folder found for the specified URI');
        }
    });
});

const expectedTests = [
    {
        name: 'Test_Speed_bdc804',
        line: 39,
    },
    {
        name: 'Test_Adjust_Speed_6fd48f',
        line: 60,
    },
    {
        name: 'Test_Desired_Speed_3a9813',
        line: 39,
    },
    {
        name: 'Test_Set_Desired_Speed_42cd33',
        line: 60,
    },
    {
        name: 'Test_Adjust_Speed_6fd48f',
        line: 81,
    },
];
