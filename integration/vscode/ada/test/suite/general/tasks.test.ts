import assert from 'assert';
import { suite, test } from 'mocha';
import * as vscode from 'vscode';
import { contextClients } from '../../../src/extension';
import {
    CustomTaskDefinition,
    PROJECT_FROM_CONFIG,
    createAdaTaskProvider,
    createSparkTaskProvider,
} from '../../../src/taskProviders';
import { activate } from '../utils';

suite('GPR Tasks Provider', function () {
    this.beforeAll(async () => {
        await activate();
    });

    test('Ada tasks list', async () => {
        const prov = createAdaTaskProvider();
        const tasks = await prov.provideTasks();
        assert.notStrictEqual(tasks, undefined);

        const expectedTasksNames: string[] = [
            'ada: Build current project - kind: buildProject',
            'ada: Check current file - kind: checkFile',
            'ada: Clean current project - kind: cleanProject',
            'ada: Build main - main1.adb - kind: buildMain',
            'ada: Build and run main - main1.adb - kind: buildAndRunMain',
            'ada: Build main - test.adb - kind: buildMain',
            'ada: Build and run main - test.adb - kind: buildAndRunMain',
        ];

        assert.strictEqual(
            tasks
                .map(
                    (t) =>
                        `${t.source}: ${t.name} - kind: ${
                            (t.definition as CustomTaskDefinition).configuration.kind
                        }`
                )
                .join('\n'),
            expectedTasksNames.join('\n')
        );
    });

    test('Spark tasks list', async () => {
        await contextClients.adaClient.onReady();
        const prov = createSparkTaskProvider();
        const tasks = await prov.provideTasks();
        assert.notStrictEqual(tasks, undefined);
        const expectedTasksNames: string[] = [
            'spark: Clean project for proof',
            'spark: Examine project',
            'spark: Examine file',
            'spark: Examine subprogram',
            'spark: Prove project',
            'spark: Prove file',
            'spark: Prove subprogram',
            'spark: Prove selected region',
            'spark: Prove line',
        ];
        assert.strictEqual(
            tasks.map((t) => `${t.source}: ${t.name}`).join('\n'),
            expectedTasksNames.join('\n')
        );
    });

    test('Resolving task', async () => {
        const prov = createAdaTaskProvider();

        const def: CustomTaskDefinition = {
            type: 'ada',
            configuration: {
                kind: 'buildProject',
                projectFile: PROJECT_FROM_CONFIG,
            },
        };
        const task = new vscode.Task(def, vscode.TaskScope.Workspace, 'My Task', 'ada');
        const resolved = await prov.resolveTask(task);

        assert(resolved);
        assert(resolved.execution);

        const exec = resolved.execution as vscode.ShellExecution;
        const actualCmd = [exec.command].concat(exec.args).join(' ');

        const expectedCmd = `gprbuild -P ${def.configuration.projectFile} -cargs -gnatef`;

        assert.strictEqual(actualCmd, expectedCmd);
    });

    test('Resolving task with main', async () => {
        const prov = createAdaTaskProvider();

        const def: CustomTaskDefinition = {
            type: 'ada',
            configuration: {
                kind: 'buildMain',
                projectFile: PROJECT_FROM_CONFIG,
                main: 'src/program.adb',
            },
        };
        const task = new vscode.Task(def, vscode.TaskScope.Workspace, 'My Task', 'ada');
        const resolved = await prov.resolveTask(task);

        assert(resolved);
        assert(resolved.execution);

        const exec = resolved.execution as vscode.ShellExecution;
        const actualCmd = [exec.command].concat(exec.args).join(' ');

        const expectedCmd = `gprbuild -P ${def.configuration.projectFile} ${
            def.configuration.main ?? ''
        } -cargs -gnatef`;

        assert.strictEqual(actualCmd, expectedCmd);
    });

    test('Resolving task with run main', async () => {
        const prov = createAdaTaskProvider();

        const def: CustomTaskDefinition = {
            type: 'ada',
            configuration: {
                kind: 'buildAndRunMain',
                projectFile: PROJECT_FROM_CONFIG,
                main: 'src/main1.adb',
            },
        };
        const task = new vscode.Task(def, vscode.TaskScope.Workspace, 'My Task', 'ada');
        const resolved = await prov.resolveTask(task);

        assert(resolved);
        assert(resolved.execution);

        const exec = resolved.execution as vscode.ShellExecution;
        const actualCmd = [exec.command].concat(exec.args).join(' ');

        // Note that the executable is named differently than the source file
        // via project attributes
        const expectedCmd = `gprbuild -P ${def.configuration.projectFile} ${
            def.configuration.main ?? ''
        } -cargs -gnatef && obj/main1exec`;

        assert.strictEqual(actualCmd, expectedCmd);
    });
});
