import assert from 'assert';
import { existsSync } from 'fs';
import { suite, test } from 'mocha';
import * as vscode from 'vscode';
import { adaExtState } from '../../../src/extension';
import { exe, getProjectFile } from '../../../src/helpers';
import {
    CustomTaskDefinition,
    PROJECT_FROM_CONFIG,
    createAdaTaskProvider,
    createSparkTaskProvider,
} from '../../../src/taskProviders';
import { activate } from '../utils';

suite('GPR Tasks Provider', function () {
    let projectPath: string;

    this.beforeAll(async () => {
        await activate();
        projectPath = await getProjectFile();
    });

    /**
     * Check that the list of offered Ada tasks is expected.
     */
    test('Ada tasks list', async () => {
        const prov = createAdaTaskProvider();
        const tasks = await prov.provideTasks();
        assert.notStrictEqual(tasks, undefined);

        const expectedTasksList = `
ada: Build current project - kind: buildProject
ada: Check current file - kind: checkFile
ada: Clean current project - kind: cleanProject
ada: Build main - src/main1.adb - kind: buildMain
ada: Run main - src/main1.adb - kind: runMain
ada: Build and run main - src/main1.adb - kind: buildAndRunMain
ada: Build main - src/test.adb - kind: buildMain
ada: Run main - src/test.adb - kind: runMain
ada: Build and run main - src/test.adb - kind: buildAndRunMain`.trim();

        const actualTaskList = tasks
            .map(
                (t) =>
                    `${t.source}: ${t.name} - kind: ${
                        (t.definition as CustomTaskDefinition).configuration.kind
                    }`
            )
            .join('\n');
        assert.strictEqual(actualTaskList, expectedTasksList);
    });

    /**
     * Check that the list of offered SPARK tasks is expected.
     */
    test('Spark tasks list', async () => {
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

    test('Automatic clean command', async () => {
        const task = (await vscode.tasks.fetchTasks({ type: 'ada' })).find(
            (t) => t.name == 'Clean current project'
        );
        assert(task);

        /**
         * Check the command line of the clean task.
         */
        const actualCmd = getCmdLine(task.execution as vscode.ShellExecution);
        /**
         * The workspace doesn't define an ada.projectFile setting, so the full
         * path of the project file is obtained from ALS and used in the command
         * line.
         */
        const expectedCmd = `gprclean -P ${projectPath}`;
        assert.equal(actualCmd, expectedCmd);

        /**
         * Now try running the task.
         */
        const status = await runTaskAndGetResult(task);
        assert.equal(status, 0);
        assert(!existsSync('obj/main1' + exe));
    });

    test('Automatic buildMain command', async () => {
        const task = (await vscode.tasks.fetchTasks({ type: 'ada' })).find(
            (t) => t.name == 'Build main - src/main1.adb'
        );
        assert(task);

        /**
         * Check the command line of the build task.
         */
        const actualCmd = getCmdLine(task.execution as vscode.ShellExecution);
        /**
         * The workspace doesn't define an ada.projectFile setting, so the full
         * path of the project file is obtained from ALS and used in the command
         * line.
         */
        const expectedCmd = `gprbuild -P ${projectPath} src/main1.adb -cargs:ada -gnatef`;
        assert.equal(actualCmd, expectedCmd);

        /**
         * Now try running the task.
         */
        const status = await runTaskAndGetResult(task);
        assert.equal(status, 0);

        /**
         * Check that the executable is produced. The project defines a
         * different name for the executable produced by main1.adb.
         */
        assert(vscode.workspace.workspaceFolders);
        assert(
            existsSync(`${vscode.workspace.workspaceFolders[0].uri.fsPath}/obj/main1exec` + exe)
        );
    });

    /**
     * Check that starting from a User-defined task, the task provider is able
     * to resolve it into a complete task with the expected command line.
     */
    test('Resolving task', async () => {
        const prov = createAdaTaskProvider();

        const def: CustomTaskDefinition = {
            type: 'ada',
            configuration: {
                kind: 'buildProject',
                projectFile: PROJECT_FROM_CONFIG,
                args: ['-d'],
            },
        };
        const task = new vscode.Task(def, vscode.TaskScope.Workspace, 'My Task', 'ada');
        const resolved = await prov.resolveTask(task);

        assert(resolved);
        assert(resolved.execution);

        const exec = resolved.execution as vscode.ShellExecution;

        const actualCmd = getCmdLine(exec);

        /**
         * This task defines the projectFile field as config:ada.projectFile so
         * this is reflected as is in the command line. However running this
         * task will fail because the workspace doesn't define the
         * ada.projectFile setting.
         */
        const expectedCmd = 'gprbuild -P ${config:ada.projectFile} -d -cargs:ada -gnatef';

        assert.strictEqual(actualCmd, expectedCmd);

        const status = runTaskAndGetResult(resolved);
        /**
         * The task should fail because the ada.projectFile is not set so the
         * command line cannot be resolved.
         */
        assert.notEqual(status, 0);
    });

    test('Resolving task buildMain', async () => {
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
        const actualCmd = getCmdLine(exec);

        assert(def.configuration.main);
        const expectedCmd =
            `gprbuild -P \${config:ada.projectFile} ${def.configuration.main} ` +
            `-cargs:ada -gnatef`;

        assert.strictEqual(actualCmd, expectedCmd);
    });

    test('Resolving task runMain', async () => {
        const prov = createAdaTaskProvider();

        const def: CustomTaskDefinition = {
            type: 'ada',
            configuration: {
                kind: 'runMain',
                projectFile: PROJECT_FROM_CONFIG,
                main: 'src/main1.adb',
                mainArgs: ['arg1', 'arg2'],
            },
        };
        const task = new vscode.Task(def, vscode.TaskScope.Workspace, 'My Task', 'ada');
        const resolved = await prov.resolveTask(task);

        assert(resolved);
        assert(resolved.execution);

        const exec = resolved.execution as vscode.ShellExecution;
        const actualCmd = getCmdLine(exec);

        // Note that the executable is named differently than the source file
        // via project attributes
        assert(def.configuration.main);
        const expectedCmd = `obj/main1exec${exe} arg1 arg2`;

        assert.strictEqual(actualCmd, expectedCmd);
    });

    test('buildAndRunMain task', async () => {
        const adaTasks = await vscode.tasks.fetchTasks({ type: 'ada' });
        const task = adaTasks.find((v) => v.name == 'Build and run main - src/main1.adb');
        assert(task);

        const execStatus: number | undefined = await runTaskAndGetResult(task);

        assert.equal(execStatus, 0);
    });

    /**
     * Test that buildAndRunMain fails when configured with non-existing tasks
     */
    test('buildAndRunMain failure', async () => {
        const prov = createAdaTaskProvider();
        let def: CustomTaskDefinition = {
            type: 'ada',
            configuration: {
                kind: 'buildAndRunMain',
                buildTask: 'non existing task',
            },
        };
        let task = new vscode.Task(def, vscode.TaskScope.Workspace, 'Task 1', 'ada');
        let resolved = await prov.resolveTask(task);
        assert(resolved);
        /**
         * The expected code when errors occur before the invocation of the
         * build and run tasks is 2.
         */
        assert.equal(await runTaskAndGetResult(resolved), 2);

        def = {
            type: 'ada',
            configuration: {
                kind: 'buildAndRunMain',
                buildTask: 'ada: Build current project', // Existing build task
                runTask: 'non existing task',
            },
        };
        task = new vscode.Task(def, vscode.TaskScope.Workspace, 'Task 2', 'ada');
        resolved = await prov.resolveTask(task);
        assert(resolved);
        assert.equal(await runTaskAndGetResult(resolved), 2);
    });
});

async function runTaskAndGetResult(task: vscode.Task): Promise<number | undefined> {
    return await new Promise((resolve) => {
        const disposable = vscode.tasks.onDidEndTaskProcess((e) => {
            if (e.execution.task == task) {
                disposable.dispose();
                resolve(e.exitCode);
            }
        });

        void vscode.tasks.executeTask(task);
    });
}

function getCmdLine(exec: vscode.ShellExecution) {
    return [exec.command]
        .concat(exec.args)
        .map((s) => {
            if (typeof s == 'object') {
                return s.value;
            } else {
                return s;
            }
        })
        .join(' ');
}
