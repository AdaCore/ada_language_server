/* eslint-disable max-len */
import assert from 'assert';
import { existsSync } from 'fs';
import path from 'path';
import * as vscode from 'vscode';
import { getEnclosingSymbol, getSelectedRegion } from '../../../src/commands';
import { exe, getProjectFile } from '../../../src/helpers';
import {
    BUILD_PROJECT_TASK_NAME,
    SimpleTaskDef,
    SimpleTaskProvider,
    createAdaTaskProvider,
    createSparkTaskProvider,
    findTaskByName,
    getConventionalTaskLabel,
} from '../../../src/taskProviders';
import { activate } from '../utils';

suite('Task Providers', function () {
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
        assert(tasks);

        const expectedTasksList = `
ada: Clean current project
ada: Build current project
ada: Check current file
ada: Analyze the project with GNAT SAS
ada: Analyze the current file with GNAT SAS
ada: Create a report after a GNAT SAS analysis
ada: Analyze the project with GNAT SAS and produce a report
ada: Generate documentation from the project
ada: Create/update test skeletons for the project
ada: Build main - src/main1.adb
ada: Run main - src/main1.adb
ada: Build and run main - src/main1.adb
ada: Build main - src/test.adb
ada: Run main - src/test.adb
ada: Build and run main - src/test.adb
`.trim();

        const actualTaskList = tasks.map((t) => `${t.source}: ${t.name}`).join('\n');
        assert.strictEqual(actualTaskList, expectedTasksList);
    });

    /**
     * Check that the list of offered SPARK tasks is expected.
     */
    test('Spark tasks list', async () => {
        const prov = createSparkTaskProvider();
        const tasks = await prov.provideTasks();
        assert(tasks);
        const expectedTasksList = `
spark: Clean project for proof
spark: Examine project
spark: Examine file
spark: Examine subprogram
spark: Prove project
spark: Prove file
spark: Prove subprogram
spark: Prove selected region
spark: Prove line
        `.trim();
        assert.strictEqual(
            tasks.map((t) => `${t.source}: ${t.name}`).join('\n'),
            expectedTasksList
        );
    });

    test('Ada task command lines', async function () {
        const expectedCmdLines = `
ada: Clean current project - gprclean -P ${projectPath}
ada: Build current project - gprbuild -P ${projectPath} -cargs:ada -gnatef
ada: Check current file - gprbuild -q -f -c -u -gnatc -P ${projectPath} \${fileBasename} -cargs:ada -gnatef
ada: Analyze the project with GNAT SAS - gnatsas analyze -P ${projectPath}
ada: Analyze the current file with GNAT SAS - gnatsas analyze -P ${projectPath} \${fileBasename}
ada: Create a report after a GNAT SAS analysis - gnatsas report sarif -P ${projectPath}
ada: Generate documentation from the project - gnatdoc -P ${projectPath}
ada: Create/update test skeletons for the project - gnattest -P ${projectPath}
ada: Build main - src/main1.adb - gprbuild -P ${projectPath} src/main1.adb -cargs:ada -gnatef
ada: Run main - src/main1.adb - obj/main1exec
ada: Build main - src/test.adb - gprbuild -P ${projectPath} src/test.adb -cargs:ada -gnatef
ada: Run main - src/test.adb - obj/test
`.trim();

        const prov = createAdaTaskProvider();
        const actualCommandLines = await getCommandLines(prov);
        assert.equal(actualCommandLines, expectedCmdLines);
    });

    test('Spark task command lines', async function () {
        const expectedCmdLines = `
spark: Clean project for proof - gnatprove -P ${projectPath} --clean
spark: Examine project - gnatprove -P ${projectPath} -j0 --mode=flow -cargs -gnatef
spark: Examine file - gnatprove -P ${projectPath} -j0 --mode=flow -u \${fileBasename} -cargs -gnatef
spark: Examine subprogram - gnatprove -P ${projectPath} -j0 --mode=flow \${command:ada.spark.limitSubpArg} -cargs -gnatef
spark: Prove project - gnatprove -P ${projectPath} -j0 -cargs -gnatef
spark: Prove file - gnatprove -P ${projectPath} -j0 -u \${fileBasename} -cargs -gnatef
spark: Prove subprogram - gnatprove -P ${projectPath} -j0 \${command:ada.spark.limitSubpArg} -cargs -gnatef
spark: Prove selected region - gnatprove -P ${projectPath} -j0 -u \${fileBasename} --limit-region=\${fileBasename}:0:0 -cargs -gnatef
spark: Prove line - gnatprove -P ${projectPath} -j0 -u \${fileBasename} --limit-line=\${fileBasename}:\${lineNumber} -cargs -gnatef
`.trim();

        const prov = createSparkTaskProvider();
        const actualCommandLines = await getCommandLines(prov);

        assert.equal(actualCommandLines, expectedCmdLines);
    });

    /**
     * Check that starting from a User-defined task, the task provider is able
     * to resolve it into a complete task with the expected command line.
     */
    test('Resolving User task', async () => {
        const prov = createAdaTaskProvider();

        const def: SimpleTaskDef = {
            type: 'ada',
            command: 'gprbuild',
            args: ['${command:ada.gprProjectArgs}', '-d'],
        };
        const task = new vscode.Task(def, vscode.TaskScope.Workspace, 'My Task', 'ada');
        const resolved = await prov.resolveTask(task);

        assert(resolved);
        assert(resolved.execution);

        const exec = resolved.execution as vscode.ShellExecution;

        const actualCmd = getCmdLine(exec);

        /**
         * The workspace doesn't have the ada.projectFile setting set, so the
         * extension will use the full path to the project file obtained from
         * the ALS.
         */
        const expectedCmd = `gprbuild -P ${projectPath} -d`;

        assert.strictEqual(actualCmd, expectedCmd);
    });

    test('current regions and subprograms', async () => {
        assert(vscode.workspace.workspaceFolders);
        const testAdbUri = vscode.Uri.joinPath(
            vscode.workspace.workspaceFolders[0].uri,
            'src',
            'test.adb'
        );

        /**
         * Position the cursor within the nested P3 subprogram
         */
        await vscode.window.showTextDocument(testAdbUri, {
            selection: new vscode.Range(17, 13, 17, 13),
        });
        assert.deepEqual(
            (await getEnclosingSymbol(vscode.window.activeTextEditor, [vscode.SymbolKind.Function]))
                ?.range,
            // The expected range is that of the inner-most subprogram P3
            new vscode.Range(13, 9, 18, 16)
        );
        assert.equal(getSelectedRegion(vscode.window.activeTextEditor), '18:18');

        /**
         * Select a multi-line range
         */
        await vscode.window.showTextDocument(testAdbUri, {
            selection: new vscode.Range(15, 13, 17, 13),
        });
        assert.equal(getSelectedRegion(vscode.window.activeTextEditor), '16:18');
    });

    test('spark tasks on current location', async () => {
        assert(vscode.workspace.workspaceFolders);
        const testAdbUri = vscode.Uri.joinPath(
            vscode.workspace.workspaceFolders[0].uri,
            'src',
            'test.adb'
        );

        {
            await vscode.window.showTextDocument(testAdbUri, {
                selection: new vscode.Range(17, 13, 17, 13),
            });
            const tasks = await vscode.tasks.fetchTasks({ type: 'spark' });
            const subPTask = tasks.find((t) => t.name == 'Prove subprogram');
            assert(subPTask);
            assert(subPTask.execution);
            assert.equal(
                getCmdLine(subPTask.execution as vscode.ShellExecution),
                `gnatprove -P ${projectPath} -j0 ` +
                    `--limit-subp=\${fileBasename}:14 -cargs -gnatef`
            );
        }

        {
            await vscode.window.showTextDocument(testAdbUri, {
                selection: new vscode.Range(20, 0, 23, 0),
            });
            /**
             * Compute the tasks again after the change of selection
             */
            const tasks = await vscode.tasks.fetchTasks({ type: 'spark' });
            const regionTask = tasks.find((t) => t.name == 'Prove selected region');
            assert(regionTask);
            assert(regionTask.execution);
            assert.equal(
                getCmdLine(regionTask.execution as vscode.ShellExecution),
                `gnatprove -P ${projectPath} -j0 -u \${fileBasename} ` +
                    `--limit-region=\${fileBasename}:21:24 -cargs -gnatef`
            );
        }
    });

    test('Obsolete task definition causes error', async function () {
        const obsoleteTaskDef: vscode.TaskDefinition = {
            type: 'ada',
            configuration: {},
        };
        const obsoleteTask = new vscode.Task(
            obsoleteTaskDef,
            vscode.TaskScope.Workspace,
            'Obsolete Task',
            'Workspace'
        );

        const prov = createAdaTaskProvider();

        /**
         * Assert that an Error is thrown with the word 'obsolete' in the message.
         */
        await assert.rejects(prov.resolveTask(obsoleteTask), /obsolete/);
    });

    test('Invalid task defs', async function () {
        const invalidTaskDefs: SimpleTaskDef[] = [
            {
                type: 'ada',
            },
            {
                type: 'ada',
                args: [],
            },
            {
                type: 'ada',
                command: 'cmd',
                compound: [],
            },
            {
                type: 'ada',
                args: [],
                compound: [],
            },
            {
                type: 'ada',
                command: 'cmd',
                args: [],
                compound: [],
            },
        ];

        const prov = createAdaTaskProvider();
        for (const t of invalidTaskDefs) {
            const invalidTask = new vscode.Task(
                t,
                vscode.TaskScope.Workspace,
                'Invalid Task',
                'Workspace'
            );

            /**
             * Assert that an Error is thrown
             */
            await assert.rejects(prov.resolveTask(invalidTask));
        }
    });
});

suite('Task Execution', function () {
    /**
     * Use longer timeout to accomodate for tool invocations
     */
    this.timeout('10s');

    const testedTaskLabels = new Set<string>();

    this.beforeAll(async () => {
        await activate();
    });

    test('Build current project', async () => {
        await testTask(BUILD_PROJECT_TASK_NAME.replace(/^ada: /, ''));
    });

    test('Run main', async () => {
        await testTask('Run main - src/main1.adb');
        await testTask('Run main - src/test.adb');
    });

    test('Check current file', async () => {
        assert(vscode.workspace.workspaceFolders);
        const testAdbUri = vscode.Uri.joinPath(
            vscode.workspace.workspaceFolders[0].uri,
            'src',
            'test.adb'
        );

        await vscode.window.showTextDocument(testAdbUri);

        await testTask('Check current file');
    });

    test('Clean current project', async () => {
        await testTask('Clean current project');
    });

    test('Build main - src/main1.adb', async () => {
        await testTask('Build main - src/main1.adb');

        /**
         * Check that the executable is produced. The project defines a
         * different name for the executable produced by main1.adb.
         */
        assert(vscode.workspace.workspaceFolders);
        assert(
            existsSync(`${vscode.workspace.workspaceFolders[0].uri.fsPath}/obj/main1exec` + exe)
        );
    });

    test('Build main - src/test.adb', async function () {
        await testTask('Build main - src/test.adb');
    });

    test('Build and run main', async () => {
        await testTask('Build and run main - src/main1.adb');
        await testTask('Build and run main - src/test.adb');
    });

    /**
     * Check that the 'buildAndRunMain' task works fine with projects that
     * do not explicitly define an object directory.
     */
    test('Build and run main task without object directory', async () => {
        // Load a custom project that does not define any object dir by
        // changing the 'ada.projectFile' setting.
        const initialProjectFile = vscode.workspace.getConfiguration().get('ada.projectFile');
        try {
            await vscode.workspace
                .getConfiguration()
                .update(
                    'ada.projectFile',
                    'default_without_obj_dir' + path.sep + 'default_without_obj_dir.gpr'
                );
            /**
             * Wait a bit until the ALS loads the new project
             */
            await new Promise((resolve) => setTimeout(resolve, 1000));
            await testTask('Build and run main - src/main1.adb');
        } finally {
            // Reset the 'ada.projectFile' setting. If the previous value was
            // empty, update to 'undefined' so that the setting gets removed.
            // That's because the default value of that setting is the empty
            // string.
            await vscode.workspace
                .getConfiguration()
                .update(
                    'ada.projectFile',
                    initialProjectFile === '' ? undefined : initialProjectFile
                );
        }
    });

    /**
     * Test that buildAndRunMain fails when configured with non-existing tasks
     */
    test('Compound task failure', async () => {
        const prov = createAdaTaskProvider();
        let def: SimpleTaskDef = {
            type: 'ada',
            compound: ['non existing task'],
        };
        let task = new vscode.Task(def, vscode.TaskScope.Workspace, 'Task 1', 'ada');
        let resolved = await prov.resolveTask(task);
        assert(resolved);
        /**
         * The expected code when errors occur before the invocation of the
         * build and run tasks is 2.
         */
        assert.equal(await runTaskAndGetResult(resolved), 2);
        testedTaskLabels.add(task.name);

        def = {
            type: 'ada',
            compound: [
                'ada: Build current project', // Existing build task
                'non existing task',
            ],
        };
        task = new vscode.Task(def, vscode.TaskScope.Workspace, 'Task 2', 'ada');
        resolved = await prov.resolveTask(task);
        assert(resolved);
        assert.equal(await runTaskAndGetResult(resolved), 2);
    });

    test('gnatsas analyze', async () => {
        await testTask('ada: Analyze the project with GNAT SAS');
    });

    test('gnatsas report', async () => {
        await testTask('ada: Create a report after a GNAT SAS analysis');
    });

    test('gnatsas analyze & report', async () => {
        await testTask('ada: Analyze the project with GNAT SAS and produce a report');
    });

    test('gnatdoc', async () => {
        await testTask('ada: Generate documentation from the project');
    });

    test('gnattest', async () => {
        await testTask('ada: Create/update test skeletons for the project');
    });

    test('All tasks tested', async () => {
        const adaTasks = await createAdaTaskProvider().provideTasks();
        assert(adaTasks);
        const sparkTasks = await createSparkTaskProvider().provideTasks();
        assert(sparkTasks);

        const allTaskNames = adaTasks.concat(sparkTasks).map((t) => getConventionalTaskLabel(t));

        const untested = allTaskNames.filter((v) => !testedTaskLabels.has(v));

        if (untested.length > 0) {
            assert.fail(
                `${untested.length} task kinds were not tested:\n${[...untested].join('\n')}`
            );
        }
    });

    async function testTask(taskName: string) {
        assert(vscode.workspace.workspaceFolders);
        const adaTasks = await vscode.tasks.fetchTasks({ type: 'ada' });
        const task = findTaskByName(adaTasks, taskName);
        assert(task);
        testedTaskLabels.add(getConventionalTaskLabel(task));

        const execStatus: number | undefined = await runTaskAndGetResult(task);

        assert.equal(execStatus, 0);
    }
});

export async function getCommandLines(prov: SimpleTaskProvider) {
    const tasks = await prov.provideTasks();
    assert(tasks);

    const actualCommandLines = (
        await Promise.all(
            tasks.map(async (t) => {
                return { task: t, execution: (await prov.resolveTask(t))?.execution };
            })
        )
    )
        .filter(function ({ execution }) {
            return execution instanceof vscode.ShellExecution;
        })
        .map(function ({ task, execution }) {
            assert(execution instanceof vscode.ShellExecution);
            return `${task.source}: ${task.name} - ${getCmdLine(execution)}`;
        })
        .join('\n');
    return actualCommandLines;
}

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
