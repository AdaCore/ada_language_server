/* eslint-disable max-len */
import assert from 'assert';
import path from 'path';
import * as vscode from 'vscode';
import { getEnclosingSymbol, getSelectedRegion } from '../../../src/commands';
import { exe, getProjectFile } from '../../../src/helpers';
import {
    SimpleTaskDef,
    TASK_TYPE_ADA,
    createAdaTaskProvider,
    findTaskByName,
    getConventionalTaskLabel,
    isFromWorkspace,
} from '../../../src/taskProviders';
import {
    activate,
    closeAllEditors,
    getCmdLine,
    getCommandLines,
    runTaskAndGetResult,
    testTask,
} from '../utils';

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
ada: Compile current file
ada: Analyze the project with GNAT SAS
ada: Analyze the current file with GNAT SAS
ada: Create a report after a GNAT SAS analysis
ada: Analyze the project with GNAT SAS and produce a report
ada: Analyze the current file with GNAT SAS and produce a report
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

    test('Ada task command lines', async function () {
        const expectedCmdLines = `
ada: Clean current project - gprclean -P ${projectPath}
ada: Build current project - gprbuild -P ${projectPath} -cargs:ada -gnatef
ada: Check current file - gprbuild -q -f -c -u -gnatc -P ${projectPath} \${fileBasename} -cargs:ada -gnatef
ada: Compile current file - gprbuild -q -f -c -u -P ${projectPath} \${fileBasename} -cargs:ada -gnatef
ada: Analyze the project with GNAT SAS - gnatsas analyze -P ${projectPath}
ada: Analyze the current file with GNAT SAS - gnatsas analyze -P ${projectPath} --file=\${fileBasename}
ada: Create a report after a GNAT SAS analysis - gnatsas report sarif -P ${projectPath} -o report.sarif
ada: Generate documentation from the project - gnatdoc -P ${projectPath}
ada: Create/update test skeletons for the project - gnattest -P ${projectPath}
ada: Build main - src/main1.adb - gprbuild -P ${projectPath} src/main1.adb -cargs:ada -gnatef
ada: Run main - src/main1.adb - obj/main1exec${exe}
ada: Build main - src/test.adb - gprbuild -P ${projectPath} src/test.adb -cargs:ada -gnatef
ada: Run main - src/test.adb - obj/test${exe}
`.trim();

        const prov = createAdaTaskProvider();
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

    /**
     * Verify that {@link findTaskByName} returns the predefined task that
     * has been customized in the workspaces's 'tasks.json' file if any, and
     * not the default predefined task from the extension's TaskProvider.
     */
    test('Customized predefined task command line', async function () {
        const initialTasks: vscode.TaskDefinition[] =
            vscode.workspace.getConfiguration('tasks').get('tasks') ?? [];

        try {
            // Customize the 'ada: Build current project' task
            const tasks: vscode.TaskDefinition[] = initialTasks.concat();
            const def: SimpleTaskDef = {
                type: 'ada',
                command: 'gprbuild',
                problemMatcher: ['$ada'],
                args: [
                    '${command:ada.gprProjectArgs}',
                    '--no-object-check',
                    '-cargs:ada',
                    '-gnatef',
                ],
                label: 'ada: Build current project',
            };
            tasks.push(def);
            await vscode.workspace.getConfiguration().update('tasks.tasks', tasks);

            //  Fetch the available tasks to find the one we have customized: make
            //  sure its source is 'workspace', since it has been manually customized.

            const prov = createAdaTaskProvider();
            const adaTasks = await vscode.tasks.fetchTasks({ type: TASK_TYPE_ADA });
            const buildTask = await findTaskByName('ada: Build current project', adaTasks);
            const resolved = await prov.resolveTask(buildTask);
            assert(resolved);
            assert(resolved.execution);
            assert(
                isFromWorkspace(resolved),
                'Build task does not come from workspace. Source is: ' + resolved.source
            );

            const exec = buildTask.execution as vscode.ShellExecution;
            const actualCmd = getCmdLine(exec);

            // The '--no-object-check' switch has been added to the 'ada: Build current project'
            // predefined task in the workspace's tasks.json file: check that it's indeed present
            // in the returned task's command line.
            const expectedCmd = `gprbuild -P ${projectPath} --no-object-check -cargs:ada -gnatef`;

            assert.strictEqual(actualCmd, expectedCmd);
        } finally {
            // Reset the 'tasks.tasks' setting. If the previous value was
            // empty, update to 'undefined' so that the setting gets removed.
            await vscode.workspace
                .getConfiguration()
                .update('tasks.tasks', initialTasks ?? undefined);
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

    const allProvidedTasks: vscode.Task[] = [];

    this.beforeAll(async () => {
        await activate();
        allProvidedTasks.push(...(await createAdaTaskProvider().provideTasks()));
    });

    this.beforeEach(async function () {
        await closeAllEditors();
    });

    declTaskTest('ada: Build current project');
    declTaskTest('ada: Run main - src/main1.adb');
    declTaskTest('ada: Run main - src/test.adb');
    declTaskTest('ada: Check current file', openSrcFile);
    declTaskTest('ada: Compile current file', openSrcFile);
    declTaskTest('ada: Clean current project');
    declTaskTest('ada: Build main - src/main1.adb');
    declTaskTest('ada: Build main - src/test.adb');
    declTaskTest('ada: Build and run main - src/main1.adb');
    declTaskTest('ada: Build and run main - src/test.adb');
    declTaskTest('ada: Analyze the project with GNAT SAS');
    declTaskTest('ada: Create a report after a GNAT SAS analysis');
    declTaskTest('ada: Analyze the project with GNAT SAS and produce a report');
    declTaskTest('ada: Analyze the current file with GNAT SAS and produce a report', openSrcFile);
    declTaskTest('ada: Analyze the current file with GNAT SAS', openSrcFile);
    declTaskTest('ada: Generate documentation from the project');
    declTaskTest('ada: Create/update test skeletons for the project');

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
            await testTask(
                'Build and run main - src/main1.adb',
                testedTaskLabels,
                allProvidedTasks
            );
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

    test('All tasks tested', function () {
        const allTaskNames = allProvidedTasks.map(getConventionalTaskLabel);

        const untested = allTaskNames.filter((v) => !testedTaskLabels.has(v));

        if (untested.length > 0) {
            assert.fail(`${untested.length} tasks were not tested:\n${untested.join('\n')}`);
        }
    });

    /**
     *
     * This function makes it easier to declare tests that execute a given task. It
     * has to be defined in the same module as the testsuite in order for the
     * testing GUI to detect the tests in VS Code.
     */
    function declTaskTest(taskName: string, prolog?: () => void | Promise<void>): Mocha.Test {
        return test(taskName, async function () {
            if (prolog) {
                await prolog();
            }

            /**
             * If there was a prolog, don't use the static task list computed at
             * the beginning. Tasks are sensitive to the current cursor location
             * so we recompute available tasks instead of using the static task
             * list.
             */
            await testTask(taskName, testedTaskLabels, prolog ? undefined : allProvidedTasks);
        });
    }
});

async function openSrcFile() {
    assert(vscode.workspace.workspaceFolders);
    const testAdbUri = vscode.Uri.joinPath(
        vscode.workspace.workspaceFolders[0].uri,
        'src',
        'test.adb'
    );

    await vscode.window.showTextDocument(testAdbUri);
}
