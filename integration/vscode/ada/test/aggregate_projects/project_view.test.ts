import assert from 'assert';
import * as vscode from 'vscode';
import {
    CMD_EDIT_PROJECT_FILE,
    CMD_OPEN_PROJECT_FILE,
    CMD_SET_PROJECT_VIEW_FILTER,
} from '../../src/constants';
import { adaExtState } from '../../src/extension';
import { ProjectViewItemKind, ProjectViewProvider } from '../../src/projectViewProvider';
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

        // Reset view settings to defaults to avoid affecting subsequent tests.
        provider?.setViewSettings(false, false, false);
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

        const rootItems = provider.getChildren();
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

    test('Context values: project items use gprFile, others use distinct values', () => {
        // The "Edit Project File" context menu entry uses `viewItem == gprFile` to restrict
        // visibility to project items only.  This test verifies that the contextValue assigned
        // to each item kind matches those expectations so that a code change cannot silently
        // re-enable the menu for source files or source directories.
        const provider = adaExtState.projectViewProvider;
        assert.ok(provider, 'Project view provider should be initialized after activation');

        // Root project item
        const rootItems = provider.getChildren();
        assert.strictEqual(rootItems.length, 1, 'Expected exactly one root project item');
        const rootItem = rootItems[0];
        assert.strictEqual(
            rootItem.contextValue,
            'gprFile',
            'Root project item should have contextValue "gprFile"',
        );

        // Sub-project items
        const aggrChildren = provider.getChildren(rootItem);
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
        const subChildren = provider.getChildren(subProjects[0]);
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

            const files = provider.getChildren(dirItem);

            // The "empty_src" directory in Project_1 has no source files,
            // so ensure that it is still exposed as a SOURCE_DIRECTORY item
            //  but with no children.
            if (dirItem.label === 'empty_src') {
                assert.strictEqual(
                    files.length,
                    0,
                    'The "empty_src" directory should have no children ' +
                        'because it contains no source files',
                );
            } else {
                assert.ok(
                    files.length > 0,
                    `Expected at least one source file item for  ` +
                        `directory '${String(dirItem.label)}'`,
                );
            }

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

    test('Tree structure: source directories before dependencies', () => {
        const provider = adaExtState.projectViewProvider;
        assert.ok(provider, 'Project view provider should be initialized after activation');

        // Calling getChildren() with no argument returns the single root project item.
        const rootItems = provider.getChildren();
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
        const aggrChildren = provider.getChildren(rootItem);

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
            const children = provider.getChildren(subItem);

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

            // Source directory children: only source files (empty directories have none)
            for (const dirItem of srcDirs) {
                assert.strictEqual(
                    dirItem.itemKind,
                    ProjectViewItemKind.SOURCE_DIRECTORY,
                    'Directory item should have kind SOURCE_DIRECTORY',
                );

                const files = provider.getChildren(dirItem);
                for (const file of files) {
                    assert.strictEqual(
                        file.itemKind,
                        ProjectViewItemKind.SOURCE_FILE,
                        `Children of a SOURCE_DIRECTORY must be SOURCE_FILE items, ` +
                            `but found kind ${file.itemKind} for '${String(file.label)}'`,
                    );
                }

                // Source files should have no children.
                if (files.length > 0) {
                    const grandChildren = provider.getChildren(files[0]);
                    assert.strictEqual(
                        grandChildren.length,
                        0,
                        'SOURCE_FILE items should have no children',
                    );
                }
            }
        }

        // Project_1 declares "empty_src" which has no source files; verify it
        // is still exposed as a SOURCE_DIRECTORY item with no children.
        const project1Item = aggrSubProjects.find((i) => String(i.label) === 'Project_1');
        assert.ok(project1Item, 'Expected to find Project_1 in sub-projects');
        const project1Dirs = provider
            .getChildren(project1Item)
            .filter((i) => i.itemKind === ProjectViewItemKind.SOURCE_DIRECTORY);
        const emptySrcDir = project1Dirs.find((i) => String(i.label) === 'empty_src');
        assert.ok(
            emptySrcDir,
            'Project_1 must expose "empty_src" as a SOURCE_DIRECTORY item ' +
                'even though it contains no source files',
        );
        assert.strictEqual(
            provider.getChildren(emptySrcDir).length,
            0,
            '"empty_src" must have no children because it contains no source files',
        );
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
            const rootItems = provider.getChildren();
            assert.strictEqual(rootItems.length, 1, 'Expected exactly one root project item');
            assert.strictEqual(rootItems[0].label, 'Aggr');

            // Execute the command to set the project view filter.
            await vscode.commands.executeCommand(CMD_SET_PROJECT_VIEW_FILTER);

            // Verify that the provider's filter is updated and the
            // tree view message reflects the active filter.
            assert.strictEqual(provider.getFilter(), 'project_1');
            assert.strictEqual(treeView.message, 'Filtered by: "Project_1"');

            // Verify that the tree view now only shows items matching the filter.
            const filteredChildren = provider.getChildren(rootItems[0]);
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

    test('Filter shows parent project and source directory when child file matches', () => {
        // Verify the new deep-filtering logic: if only a source file matches the filter,
        // its parent source directory and grandparent project must both be visible even
        // though neither of them contains the filter string in their own name or path.

        const provider = adaExtState.projectViewProvider;
        assert.ok(provider, 'Project view provider should be initialized after activation');

        // 'main_1' is a substring of 'main_1.adb' but does NOT appear in any project
        // name ('Project_1', 'Project_2') or in the shared source-directory label ('src').
        provider.setFilter('main_1');

        const rootItem = provider.getChildren()[0];

        // Both sub-projects share the src/ directory which contains main_1.adb, so both
        // must be visible even though neither project name contains 'main_1'.
        const subProjects = provider.getChildren(rootItem);
        assert.deepStrictEqual(
            subProjects.map((i) => String(i.label)).sort(),
            ['Project_1', 'Project_2'],
            'Both sub-projects must be visible because their shared source directory ' +
                "contains 'main_1.adb', even though no project name contains 'main_1'",
        );

        // For each visible sub-project, the source directory must be exposed and must
        // contain only the file(s) that match the filter.
        for (const subProject of subProjects) {
            const children = provider.getChildren(subProject);
            const sourceDirs = children.filter(
                (i) => i.itemKind === ProjectViewItemKind.SOURCE_DIRECTORY,
            );
            assert.ok(
                sourceDirs.length > 0,
                `'${String(subProject.label)}' must have a visible source directory ` +
                    "because the directory contains 'main_1.adb'",
            );

            for (const dirItem of sourceDirs) {
                const files = provider.getChildren(dirItem);

                // main_1.adb must be present
                assert.ok(
                    files.some((f) => String(f.label) === 'main_1.adb'),
                    `'main_1.adb' must appear in source directory '${String(dirItem.label)}' ` +
                        `of '${String(subProject.label)}'`,
                );

                // Non-matching siblings must be hidden
                assert.ok(
                    files.every((f) => String(f.label).toLowerCase().includes('main_1')),
                    `Only files whose name contains 'main_1' should be shown in ` +
                        `'${String(dirItem.label)}' – 'main_2.adb' and 'main_3.adb' ` +
                        `must be filtered out`,
                );
            }
        }
    });

    test('Flat mode filter hides projects with no matching descendants', () => {
        // In flat mode every project is shown as a root-level item.  The filter must
        // hide a project entirely when neither the project itself nor any of its
        // descendants (source files, source directories, sub-projects) match.

        const provider = adaExtState.projectViewProvider;
        assert.ok(provider, 'Project view provider should be initialized after activation');

        provider.setViewSettings(true, false, false); // enable flat mode
        provider.setFilter('project_2');

        const flatRoots = provider.getChildren();
        const flatRootLabels = flatRoots.map((i) => String(i.label));

        // Project_2 matches directly via its name.
        assert.ok(
            flatRootLabels.includes('Project_2'),
            "Project_2 must be visible because its name contains 'project_2'",
        );

        // Project_1 has no content (name, file path, source dirs, or source files)
        // that matches 'project_2' and must therefore be hidden.
        assert.ok(
            !flatRootLabels.includes('Project_1'),
            "Project_1 must be hidden because nothing in it matches 'project_2'",
        );

        // Aggr has no sources of its own and its name / file path do not match, but
        // Project_2 is one of its aggregated sub-projects, so Aggr must be visible.
        assert.ok(
            flatRootLabels.includes('Aggr'),
            'Aggr must be visible because its descendant Project_2 matches the filter',
        );
    });

    test('Ada task commands use the project passed as context-menu argument', async () => {
        // Verify that when a project-level Ada task is invoked from the
        // Project View context menu (i.e. with a ProjectViewItem argument),
        // the resolved task command line references the selected project file
        // rather than the currently loaded root project.

        const provider = adaExtState.projectViewProvider;
        assert.ok(provider, 'Project view provider should be initialized after activation');

        const rootItems = provider.getChildren();
        const rootItem = rootItems[0];
        const aggrChildren = provider.getChildren(rootItem);
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

    test('ProjectViewProvider constructor reads view settings from VS Code configuration', () => {
        // Intercept getConfiguration to return specific non-default values
        const workspaceWithPatch = vscode.workspace as typeof vscode.workspace & {
            getConfiguration: typeof vscode.workspace.getConfiguration;
        };
        const originalGetConfiguration = vscode.workspace.getConfiguration;
        workspaceWithPatch.getConfiguration = () =>
            ({
                get: <T>(key: string, defaultValue: T): T => {
                    if (key === 'projectView.flatMode') return true as unknown as T;
                    if (key === 'projectView.showObjectDirectories') return true as unknown as T;
                    if (key === 'projectView.showRuntimeFiles') return false as unknown as T;
                    return defaultValue;
                },
                // eslint-disable-next-line @typescript-eslint/no-unused-vars
                update: (_section: string, _value: unknown) => Promise.resolve(),
            }) as unknown as vscode.WorkspaceConfiguration;

        try {
            const provider = new ProjectViewProvider();
            assert.strictEqual(
                provider.flatMode,
                true,
                'flatMode should be initialised from VS Code configuration',
            );
            assert.strictEqual(
                provider.showObjectDirs,
                true,
                'showObjectDirs should be initialised from VS Code configuration',
            );
            assert.strictEqual(
                provider.showRuntimeFiles,
                false,
                'showRuntimeFiles should be initialised from VS Code configuration',
            );
        } finally {
            workspaceWithPatch.getConfiguration = originalGetConfiguration;
        }
    });

    test('Flat mode: all projects appear at root level with root project first', () => {
        const provider = adaExtState.projectViewProvider;
        assert.ok(provider, 'Project view provider should be initialized after activation');

        // Hierarchical mode (default): only the root project appears at root level
        const hierarchicalRoots = provider.getChildren();
        assert.strictEqual(
            hierarchicalRoots.length,
            1,
            'Hierarchical mode should show only one root item',
        );
        assert.strictEqual(
            hierarchicalRoots[0].label,
            'Aggr',
            'The single root item should be the aggregate project',
        );

        // Enable flat mode: all projects (Aggr + sub-projects) appear at root level
        provider.setViewSettings(true, false, false);

        const flatRoots = provider.getChildren();
        assert.ok(flatRoots.length > 1, 'Flat mode should expose more than one item at root level');

        // The aggregate project must come first and keep its ROOT_PROJECT kind
        assert.strictEqual(
            flatRoots[0].itemKind,
            ProjectViewItemKind.ROOT_PROJECT,
            'First item in flat mode should have kind ROOT_PROJECT',
        );
        assert.strictEqual(
            flatRoots[0].label,
            'Aggr',
            'First item in flat mode should be the aggregate project',
        );

        // All remaining items should be SUB_PROJECT
        for (const item of flatRoots.slice(1)) {
            assert.strictEqual(
                item.itemKind,
                ProjectViewItemKind.SUB_PROJECT,
                `Item '${String(item.label)}' should be SUB_PROJECT in flat mode`,
            );
        }
    });

    test('showObjectDirs: object directory items appear when the setting is enabled', async () => {
        const provider = adaExtState.projectViewProvider;
        assert.ok(provider, 'Project view provider should be initialized after activation');

        const rootItems = provider.getChildren();
        const aggrChildren = provider.getChildren(rootItems[0]);
        const subProjects = aggrChildren.filter(
            (i) => i.itemKind === ProjectViewItemKind.SUB_PROJECT,
        );
        assert.ok(subProjects.length > 0, 'Expected at least one sub-project');

        // Without showObjectDirs: no OBJECT_DIRECTORY items in any sub-project's children
        for (const subProject of subProjects) {
            const children = provider.getChildren(subProject);
            assert.ok(
                !children.some((i) => i.itemKind === ProjectViewItemKind.OBJECT_DIRECTORY),
                `Sub-project '${String(subProject.label)}' should have no ` +
                    'OBJECT_DIRECTORY items when showObjectDirs is disabled',
            );
        }

        // Enable showObjectDirs
        provider.setViewSettings(false, true, false);

        // At least one sub-project should now expose an OBJECT_DIRECTORY item
        const childrenWithSetting = await Promise.all(
            subProjects.map((sp) => provider.getChildren(sp)),
        );
        assert.ok(
            childrenWithSetting.some((ch) =>
                ch.some((i) => i.itemKind === ProjectViewItemKind.OBJECT_DIRECTORY),
            ),
            'Expected at least one OBJECT_DIRECTORY item when showObjectDirs is enabled',
        );

        // OBJECT_DIRECTORY items must carry the right contextValue
        for (const children of childrenWithSetting) {
            const objDirItems = children.filter(
                (i) => i.itemKind === ProjectViewItemKind.OBJECT_DIRECTORY,
            );
            for (const item of objDirItems) {
                assert.strictEqual(
                    item.contextValue,
                    'objectDirectory',
                    'OBJECT_DIRECTORY items should have contextValue "objectDirectory"',
                );
            }
        }

        // Filter: object directory items are shown/hidden based on matchesFilter, which
        // checks the item label and its URI path.
        const allObjDirItems = childrenWithSetting
            .flat()
            .filter((i) => i.itemKind === ProjectViewItemKind.OBJECT_DIRECTORY);
        assert.ok(
            allObjDirItems.length > 0,
            'Expected at least one OBJECT_DIRECTORY item for filter test',
        );

        // A filter matching the object directory's label must keep it visible.
        const objDirLabel = String(allObjDirItems[0].label).toLowerCase();
        provider.setFilter(objDirLabel);
        const childrenMatchingFilter = await Promise.all(
            subProjects.map((sp) => provider.getChildren(sp)),
        );
        assert.ok(
            childrenMatchingFilter.some((ch) =>
                ch.some((i) => i.itemKind === ProjectViewItemKind.OBJECT_DIRECTORY),
            ),
            'An OBJECT_DIRECTORY item should remain visible when the filter matches its label',
        );

        // A filter that cannot match any object directory must hide all of them.
        provider.setFilter('__no_object_dir_will_match_this__');
        const childrenNotMatchingFilter = await Promise.all(
            subProjects.map((sp) => provider.getChildren(sp)),
        );
        assert.ok(
            !childrenNotMatchingFilter.some((ch) =>
                ch.some((i) => i.itemKind === ProjectViewItemKind.OBJECT_DIRECTORY),
            ),
            'OBJECT_DIRECTORY items should be hidden when the filter does not match',
        );
    });

    test('showRuntimeFiles: runtime project item and its source children are listed', () => {
        const provider = adaExtState.projectViewProvider;
        assert.ok(provider, 'Project view provider should be initialized after activation');

        provider.setViewSettings(false, false, true);

        const rootItems = provider.getChildren();
        const runtimeItem = rootItems.find(
            (i) => i.itemKind === ProjectViewItemKind.RUNTIME_PROJECT,
        );
        assert.ok(runtimeItem, 'Expected a RUNTIME_PROJECT item when showRuntimeFiles is enabled');
        assert.strictEqual(runtimeItem.contextValue, 'runtimeProject');

        const children = provider.getChildren(runtimeItem);
        assert.ok(
            children.length > 0,
            'Expected RUNTIME_PROJECT to have source directory children',
        );
        assert.ok(
            children.every((i) => i.itemKind === ProjectViewItemKind.SOURCE_DIRECTORY),
            'All children of RUNTIME_PROJECT should be SOURCE_DIRECTORY items',
        );

        // Verify that each runtime source directory exposes SOURCE_FILE children
        for (const dirItem of children) {
            const files = provider.getChildren(dirItem);
            assert.ok(
                files.length > 0,
                `Source directory '${String(dirItem.label)}' of the runtime project ` +
                    'should contain at least one source file',
            );
            for (const fileItem of files) {
                assert.strictEqual(
                    fileItem.itemKind,
                    ProjectViewItemKind.SOURCE_FILE,
                    `Children of a runtime SOURCE_DIRECTORY must be SOURCE_FILE items, ` +
                        `but found kind ${fileItem.itemKind} for '${String(fileItem.label)}'`,
                );
            }
        }
    });
});
