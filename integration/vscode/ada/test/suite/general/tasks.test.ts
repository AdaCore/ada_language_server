import * as vscode from 'vscode';
import * as assert from 'assert';
import * as path from 'path';
import { suite, test } from 'mocha';
import { contextClients } from '../../../src/extension';
import { activate } from '../utils';
import GprTaskProvider, { fullCommand } from '../../../src/gprTaskProvider';

suite('GPR Tasks Provider', function () {
    this.beforeAll(async () => {
        await activate();
    });

    const expectedTasksNames: string[] = [
        'Build Main: gnatpp.adb',
        'Build Main: test.adb',
        'Build And Run Main: gnatpp.adb',
        'Build And Run Main: test.adb',
    ];

    test('Fetching tasks', async () => {
        await contextClients.adaClient.onReady();
        const prov = new GprTaskProvider(contextClients.adaClient);
        const tasks = await prov.provideTasks();
        assert.notStrictEqual(tasks, undefined);
        if (tasks != undefined) {
            for (let i = 0; i < tasks.length; i++) {
                assert.strictEqual(tasks[i].name, expectedTasksNames[i]);
            }
        }
    });
    test('Command Generation', () => {
        const expectedCmd = 'gprbuild -c -u -P default.gpr "folder/src folder/file.adb" ';
        const args = [
            '-c',
            '  -u  ',
            '        -P',
            'default.gpr   ',
            '   "folder/src folder/file.adb"     ',
        ];
        const cmd = fullCommand('gprbuild', args);
        assert.strictEqual(cmd, expectedCmd);
    });
    test('Tasks resolving', async () => {
        const prov = new GprTaskProvider(contextClients.adaClient);
        const task = new vscode.Task(
            { type: 'gpr', main: path.join('src', 'program.adb') },
            'build main',
            'tasks'
        );
        const workspace = vscode.workspace.workspaceFolders?.at(0)?.uri.fsPath;
        if (workspace == undefined) {
            throw new Error('No workspace found');
        }
        const expectedCmd =
            'gprbuild -d -P ' +
            path.join(workspace, 'default.gpr') +
            ' ' +
            path.join('src', 'program.adb ');
        const resolved = await prov.resolveTask(task);
        assert.notStrictEqual(resolved, undefined);
        if (resolved != undefined && resolved.execution) {
            const exec = resolved.execution as vscode.ShellExecution;

            //  Comparing the cmd lines in lower-case since VS Code URI API returns drive letters
            //  (e.g: "C/") in lower-case mode, as defined in their doc.
            assert.strictEqual(exec.commandLine?.toLowerCase(), expectedCmd.toLowerCase());
        }
    });
});
