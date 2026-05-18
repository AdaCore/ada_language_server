import assert from 'assert';
import * as vscode from 'vscode';
import {
    CMD_EDIT_PROJECT_FILE,
    CMD_OPEN_PROJECT_FILE,
    CMD_SET_PROJECT_VIEW_FILTER,
} from '../../src/constants';
import { adaExtState } from '../../src/extension';
import { ProjectViewItemKind } from '../../src/projectViewProvider';
import { activate } from '../utils';

suite('Project View', function () {
    this.beforeAll(async () => {
        await activate();
        await adaExtState.refreshProjectView();
    });

    this.afterEach(async () => {
        const provider = adaExtState.projectViewProvider;
        const treeView = adaExtState.projectTreeView;

        // Clear any active filter after each test to avoid affecting
        // subsequent tests.
        provider?.setFilter('');
        if (treeView) {
            treeView.message = '';
        }
        await vscode.commands.executeCommand('setContext', 'projectViewFilterActive', false);
    });

    test('Open project file command updates ada.projectFile', async () => {
        assert.ok(vscode.workspace.workspaceFolders, 'Expected a workspace folder for this test');

        const workspaceUri = vscode.workspace.workspaceFolders[0].uri;
        const selectedUri = vscode.Uri.joinPath(workspaceUri, 'project_1.gpr');

        // Create patched versions of vscode.window and vscode.workspace to mock
        // the necessary methods.
        // This is needed to simulate the user selecting a project file in the open dialog and
        // to capture the configuration update without affecting other tests.
        const windowWithPatch = vscode.window as typeof vscode.window & {
            showOpenDialog: typeof vscode.window.showOpenDialog;
        };
        const workspaceWithPatch = vscode.workspace as typeof vscode.workspace & {
            getConfiguration: typeof vscode.workspace.getConfiguration;
        };
        const originalShowOpenDialog = vscode.window.showOpenDialog;
        const originalGetConfiguration = vscode.workspace.getConfiguration;

        let updatedSection: string | undefined;
        let updatedValue: string | undefined;

        // Mock configuration update to capture the updated section and value.
        const fakeConfiguration = {
            update: (section: string, value: string) => {
                updatedSection = section;
                updatedValue = value;
                return Promise.resolve();
            },
        } as Pick<vscode.WorkspaceConfiguration, 'update'> as vscode.WorkspaceConfiguration;

        // Check that the open dialog is configured to select a single .gpr
        //  file and return the expected URI.
        windowWithPatch.showOpenDialog = (options) => {
            assert.strictEqual(options?.canSelectFiles, true);
            assert.strictEqual(options?.canSelectFolders, false);
            assert.strictEqual(options?.canSelectMany, false);
            assert.deepStrictEqual(options?.filters, { 'GPR Project Files': ['gpr'] });
            return Promise.resolve([selectedUri]);
        };
        workspaceWithPatch.getConfiguration = () => fakeConfiguration;

        // Verify that executing the command results in the expected configuration
        //  update with the correct relative path.
        try {
            await vscode.commands.executeCommand(CMD_OPEN_PROJECT_FILE);

            assert.strictEqual(
                updatedSection,
                'projectFile',
                'The command should update the Ada projectFile setting key',
            );
            assert.strictEqual(
                updatedValue,
                'project_1.gpr',
                'The selected project file should be stored relative to the workspace root',
            );
        } finally {
            // Restore the original methods to avoid affecting other tests.
            windowWithPatch.showOpenDialog = originalShowOpenDialog;
            workspaceWithPatch.getConfiguration = originalGetConfiguration;
        }
    });

    test('"Edit project file" command opens the selected project file', async () => {
        const provider = adaExtState.projectViewProvider;
        assert.ok(provider, 'Project view provider should be initialized after activation');

        const rootItems = await provider.getChildren();
        assert.strictEqual(rootItems.length, 1, 'Expected exactly one root project item');

        const rootItem = rootItems[0];
        assert.ok(rootItem.uri, 'Root project item should expose the project file URI');

        await vscode.commands.executeCommand(CMD_EDIT_PROJECT_FILE, rootItem);

        const activeEditor = vscode.window.activeTextEditor;
        assert.ok(activeEditor, 'Executing the command should open a text editor');
        assert.strictEqual(
            activeEditor.document.uri.toString(),
            rootItem.uri.toString(),
            'The opened editor should match the selected project item URI',
        );
    });

    test('Context values: project items use gprFile, others use distinct values', async () => {
        // The "Edit Project File" context menu entry uses `viewItem == gprFile` to restrict
        // visibility to project items only.  This test verifies that the contextValue assigned
        // to each item kind matches those expectations so that a code change cannot silently
        // re-enable the menu for source files or source directories.
        const provider = adaExtState.projectViewProvider;
        assert.ok(provider, 'Project view provider should be initialized after activation');

        // Root project item
        const rootItems = await provider.getChildren();
        assert.strictEqual(rootItems.length, 1, 'Expected exactly one root project item');
        const rootItem = rootItems[0];
        assert.strictEqual(
            rootItem.contextValue,
            'gprFile',
            'Root project item should have contextValue "gprFile"',
        );

        // Sub-project items
        const aggrChildren = await provider.getChildren(rootItem);
        const subProjects = aggrChildren.filter(
            (i) => i.itemKind === ProjectViewItemKind.SUB_PROJECT,
        );
        assert.ok(subProjects.length > 0, 'Expected at least one sub-project item');
        for (const subItem of subProjects) {
            assert.strictEqual(
                subItem.contextValue,
                'gprFile',
                `Sub-project item '${String(subItem.label)}' should have contextValue "gprFile"`,
            );
        }

        // Source directory and source file items
        const subChildren = await provider.getChildren(subProjects[0]);
        const sourceDirs = subChildren.filter(
            (i) => i.itemKind === ProjectViewItemKind.SOURCE_DIRECTORY,
        );
        assert.ok(sourceDirs.length > 0, 'Expected at least one source directory item');
        for (const dirItem of sourceDirs) {
            assert.strictEqual(
                dirItem.contextValue,
                'sourceDirectory',
                `Source directory item '${String(dirItem.label)}' should ` +
                    `have contextValue "sourceDirectory"`,
            );

            const files = await provider.getChildren(dirItem);
            assert.ok(files.length > 0, 'Expected at least one source file item');
            for (const fileItem of files) {
                assert.strictEqual(
                    fileItem.contextValue,
                    'sourceFile',
                    `Source file item '${String(fileItem.label)}' ` +
                        `should have contextValue "sourceFile"`,
                );
            }
        }
    });

    test('Tree structure: source directories before dependencies', async () => {
        const provider = adaExtState.projectViewProvider;
        assert.ok(provider, 'Project view provider should be initialized after activation');

        // Calling getChildren() with no argument returns the single root project item.
        const rootItems = await provider.getChildren();
        assert.strictEqual(rootItems.length, 1, 'Expected exactly one root project item');

        const rootItem = rootItems[0];
        assert.strictEqual(
            rootItem.itemKind,
            ProjectViewItemKind.ROOT_PROJECT,
            'Root item should have kind ROOT_PROJECT',
        );
        assert.strictEqual(
            rootItem.label,
            'Aggr',
            'Root project label should match the aggregate project name',
        );

        // The aggregate project has no sources of its own, so its children
        // should be exclusively the two aggregated sub-projects.
        const aggrChildren = await provider.getChildren(rootItem);

        const aggrSourceDirs = aggrChildren.filter(
            (i) => i.itemKind === ProjectViewItemKind.SOURCE_DIRECTORY,
        );
        assert.strictEqual(
            aggrSourceDirs.length,
            0,
            'Aggregate project has no sources, so no source directory items expected',
        );

        const aggrSubProjects = aggrChildren.filter(
            (i) => i.itemKind === ProjectViewItemKind.SUB_PROJECT,
        );
        assert.strictEqual(
            aggrSubProjects.length,
            2,
            'Aggregate project should expose exactly two aggregated sub-projects',
        );

        const subProjectLabels = aggrSubProjects.map((i) => String(i.label)).sort();
        assert.deepStrictEqual(
            subProjectLabels,
            ['Project_1', 'Project_2'],
            'Sub-project labels should match the declared project names',
        );

        // Sub-project children: source directories come first
        for (const subItem of aggrSubProjects) {
            const children = await provider.getChildren(subItem);

            assert.ok(
                children.length > 0,
                `Sub-project '${String(subItem.label)}' should have at least one child`,
            );

            // Verify that all SOURCE_DIRECTORY items appear before any SUB_PROJECT item.
            let encounteredSubProject = false;
            for (const child of children) {
                if (child.itemKind === ProjectViewItemKind.SUB_PROJECT) {
                    encounteredSubProject = true;
                }
                if (child.itemKind === ProjectViewItemKind.SOURCE_DIRECTORY) {
                    assert.strictEqual(
                        encounteredSubProject,
                        false,
                        `SOURCE_DIRECTORY item '${String(child.label)}' ` +
                            ` appears after a SUB_PROJECT ` +
                            `item in '${String(subItem.label)}'`,
                    );
                }
            }

            // There should be at least one source directory (both sub-projects use src/).
            const srcDirs = children.filter(
                (i) => i.itemKind === ProjectViewItemKind.SOURCE_DIRECTORY,
            );
            assert.ok(
                srcDirs.length > 0,
                `Sub-project '${String(subItem.label)}' ` +
                    'should have at least one source directory',
            );

            // Source directory children: only source files
            for (const dirItem of srcDirs) {
                assert.strictEqual(
                    dirItem.itemKind,
                    ProjectViewItemKind.SOURCE_DIRECTORY,
                    'Directory item should have kind SOURCE_DIRECTORY',
                );

                const files = await provider.getChildren(dirItem);
                assert.ok(
                    files.length > 0,
                    `Source directory '${String(dirItem.label)}' ` +
                        `of '${String(subItem.label)}' ` +
                        `should contain at least one source file`,
                );

                for (const file of files) {
                    assert.strictEqual(
                        file.itemKind,
                        ProjectViewItemKind.SOURCE_FILE,
                        `Children of a SOURCE_DIRECTORY must be SOURCE_FILE items, ` +
                            `but found kind ${file.itemKind} for '${String(file.label)}'`,
                    );
                }

                // Source files should have no children.
                const grandChildren = await provider.getChildren(files[0]);
                assert.strictEqual(
                    grandChildren.length,
                    0,
                    'SOURCE_FILE items should have no children',
                );
            }
        }
    });

    test('Set project view filter command updates provider and tree contents', async () => {
        const provider = adaExtState.projectViewProvider;
        const treeView = adaExtState.projectTreeView;

        assert.ok(provider, 'Project view provider should be initialized after activation');
        assert.ok(treeView, 'Project tree view should be initialized after activation');

        // Patch vscode.window.showInputBox to simulate the user entering a filter string.
        const windowWithPatch = vscode.window as typeof vscode.window & {
            showInputBox: typeof vscode.window.showInputBox;
        };
        const originalShowInputBox = vscode.window.showInputBox;

        // Patch vscode.window.showInputBox to simulate the user entering a filter string.
        windowWithPatch.showInputBox = (options) => {
            assert.strictEqual(options?.prompt, 'Filter Project View');
            assert.strictEqual(options?.placeHolder, 'Enter filter text');
            return Promise.resolve('Project_1');
        };

        try {
            // Verify initial state: no filter and all items visible.
            const rootItems = await provider.getChildren();
            assert.strictEqual(rootItems.length, 1, 'Expected exactly one root project item');
            assert.strictEqual(rootItems[0].label, 'Aggr');

            // Execute the command to set the project view filter.
            await vscode.commands.executeCommand(CMD_SET_PROJECT_VIEW_FILTER);

            // Verify that the provider's filter is updated and the
            // tree view message reflects the active filter.
            assert.strictEqual(provider.getFilter(), 'project_1');
            assert.strictEqual(treeView.message, 'Filtered by: "Project_1"');

            // Verify that the tree view now only shows items matching the filter.
            const filteredChildren = await provider.getChildren(rootItems[0]);
            assert.deepStrictEqual(
                filteredChildren.map((item) => String(item.label)),
                ['Project_1'],
                'Only matching project view items should remain after filtering',
            );
            assert.ok(
                filteredChildren.every((item) => item.itemKind === ProjectViewItemKind.SUB_PROJECT),
                'Filtered aggregate project children should only contain ' +
                    'the matching sub-project',
            );
        } finally {
            windowWithPatch.showInputBox = originalShowInputBox;
        }
    });

    test('Ada task commands use the project passed as context-menu argument', async () => {
        // Verify that when a project-level Ada task is invoked from the
        // Project View context menu (i.e. with a ProjectViewItem argument),
        // the resolved task command line references the selected project file
        // rather than the currently loaded root project.

        const provider = adaExtState.projectViewProvider;
        assert.ok(provider, 'Project view provider should be initialized after activation');

        const rootItems = await provider.getChildren();
        const rootItem = rootItems[0];
        const aggrChildren = await provider.getChildren(rootItem);
        const project1Item = aggrChildren.find(
            (i) => i.itemKind === ProjectViewItemKind.SUB_PROJECT && i.label === 'Project_1',
        );
        assert.ok(project1Item, 'Expected to find Project_1 as a sub-project item');
        assert.ok(project1Item.uri, 'Sub-project item should expose a URI');

        // Intercept 'workbench.action.tasks.runTask' to avoid actually running
        // the task.
        let capturedProjectArgs: string[] | undefined;
        const commandsWithPatch = vscode.commands as typeof vscode.commands & {
            executeCommand: typeof vscode.commands.executeCommand;
        };
        const originalExecuteCommand = vscode.commands.executeCommand.bind(vscode.commands);
        commandsWithPatch.executeCommand = async (
            command: string,
            ...args: unknown[]
            // eslint-disable-next-line @typescript-eslint/no-explicit-any
        ): Promise<any> => {
            if (command === 'workbench.action.tasks.runTask') {
                capturedProjectArgs = await originalExecuteCommand('ada.gprProjectArgs');
                return undefined;
            }
            return originalExecuteCommand(command, ...args);
        };

        try {
            // Invoke the registered command wrapper as the context menu would,
            // passing the selected project item as the argument.
            await vscode.commands.executeCommand('ada.tasks.buildProject', project1Item);

            assert.ok(capturedProjectArgs, 'Expected ada.gprProjectArgs to have been called');

            // Check that the task project args reference the selected project file in the Project
            // View, not the loaded aggregate project.
            assert.deepStrictEqual(
                capturedProjectArgs,
                ['-P', project1Item.uri.fsPath],
                'Expected project args to reference the selected project file',
            );
            // After the command finishes, pendingProjectOverride should be cleared.
            assert.strictEqual(
                adaExtState.pendingProjectOverride,
                undefined,
                'pendingProjectOverride should be cleared after the task command finishes',
            );
        } finally {
            commandsWithPatch.executeCommand = originalExecuteCommand;
        }
    });
});
