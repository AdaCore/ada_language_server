import assert from 'assert';
import * as vscode from 'vscode';
import { contributes } from '../../package.json';
import {
    CMD_EDIT_PROJECT_FILE,
    CMD_OPEN_PROJECT_FILE,
    CMD_PROJECT_VIEW_REVEAL_ACTIVE_FILE,
    CMD_SET_PROJECT_VIEW_FILTER,
    CMD_PROJECT_GO_TO_FILE,
    CMD_PROJECT_VIEW_OPTIONS,
    CMD_PROJECT_VIEW_SHOW_FILES,
    CMD_PROJECT_VIEW_SHOW_UNITS,
    CMD_UNIT_HIERARCHY_INFORMATION,
} from '../../src/constants';
import { adaExtState } from '../../src/extension';
import {
    ProjectViewItem,
    ProjectViewItemKind,
    ProjectViewProvider,
} from '../../src/projectViewProvider';
import { buildProjectFileItems, shouldIncludeRuntimeFiles } from '../../src/projectGoToFile';
import { ProjectViewMode, ProjectViewNode } from '../../src/projectViewTree';
import { activate, getWsUri, showTextDocument } from '../utils';

suite('Project View', function () {
    // Extension activation and the first Project View refresh both wait on
    // the ALS having loaded the project, which can exceed the default mocha
    // timeout used in CI. Leave timeouts disabled if they already are
    // (timeout() === 0), which is the default when running locally.
    const inheritedTimeout = this.timeout();
    if (inheritedTimeout !== 0) {
        this.timeout(Math.max(inheritedTimeout, 15000));
    }

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
        provider?.setMode(ProjectViewMode.FILES);
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
        // update with the correct relative path.
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

            // Also verify that passing a tree item (as VS Code does when the toolbar
            // button is clicked while an item is selected) falls back to the dialog.
            updatedSection = undefined;
            updatedValue = undefined;
            const treeItem = adaExtState.projectViewProvider?.getChildren()[0];
            assert.ok(treeItem, 'Expected at least one root project item');
            await vscode.commands.executeCommand(CMD_OPEN_PROJECT_FILE, treeItem);
            assert.strictEqual(updatedSection, 'projectFile');
            assert.strictEqual(updatedValue, 'project_1.gpr');
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
        assert.ok(
            rootItem instanceof ProjectViewItem && rootItem.uri,
            'Root project item should expose the project file URI',
        );

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

        // Check that limited imports are included as sub-projects
        const project2Item = aggrSubProjects.find((i) => String(i.label) === 'Project_2');
        assert.ok(project2Item, 'Expected to find Project_2 in sub-projects');
        const projectImport = provider
            .getChildren(project2Item)
            .find((i) => i.itemKind === ProjectViewItemKind.SUB_PROJECT);
        assert.ok(projectImport, 'Project_2 should include limited import as a sub-project');
        assert.deepStrictEqual(projectImport.label, 'Limited_Project');
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
        assert.ok(
            project1Item instanceof ProjectViewItem && project1Item.uri,
            'Sub-project item should expose a URI',
        );

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
            const provider = new ProjectViewProvider(adaExtState.adaClient.protocol2CodeConverter);
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

    test('Reveal in Project View: selects the active editor file in the tree', async () => {
        const provider = adaExtState.projectViewProvider;
        const treeView = adaExtState.projectTreeView;
        assert.ok(provider, 'Project view provider should be initialized after activation');
        assert.ok(treeView, 'Project tree view should be initialized after activation');

        assert.ok(vscode.workspace.workspaceFolders, 'Expected a workspace folder');
        const wsUri = vscode.workspace.workspaceFolders[0].uri;
        const fileUri = vscode.Uri.joinPath(wsUri, 'src', 'main_1.adb');

        const openAndReveal = async (uri: vscode.Uri) => {
            await vscode.window.showTextDocument(await vscode.workspace.openTextDocument(uri));
            await vscode.commands.executeCommand(CMD_PROJECT_VIEW_REVEAL_ACTIVE_FILE);
            return treeView.selection;
        };

        // ── 1. Hierarchical mode (default) ──────────────────────────────────────
        // Deepest getParent() path: SOURCE_FILE → SOURCE_DIRECTORY → SUB_PROJECT → ROOT_PROJECT
        let selection = await openAndReveal(fileUri);
        assert.strictEqual(selection.length, 1, 'Hierarchical: expected one selected item');
        assert.strictEqual(selection[0].itemKind, ProjectViewItemKind.SOURCE_FILE);
        assert.strictEqual(selection[0].uri.fsPath, fileUri.fsPath);

        // ── 2. File not in project ───────────────────────────────────────────────
        // Open a file that is not a project source; the command should show an
        // information message instead of revealing anything.
        const gprUri = vscode.Uri.joinPath(wsUri, 'aggr.gpr');
        await vscode.window.showTextDocument(await vscode.workspace.openTextDocument(gprUri));

        let infoMessageShown = false;
        const windowWithPatch = vscode.window as typeof vscode.window & {
            // eslint-disable-next-line @typescript-eslint/no-explicit-any
            showInformationMessage: (...args: any[]) => Thenable<any>;
        };
        const originalShowInformationMessage = vscode.window.showInformationMessage;
        windowWithPatch.showInformationMessage = (() => {
            infoMessageShown = true;
            return Promise.resolve(undefined);
        }) as typeof vscode.window.showInformationMessage;
        try {
            await vscode.commands.executeCommand(CMD_PROJECT_VIEW_REVEAL_ACTIVE_FILE);
        } finally {
            windowWithPatch.showInformationMessage = originalShowInformationMessage;
        }
        assert.ok(infoMessageShown, 'Expected an info message for a file not in the project');

        // ── 3. Flat mode ─────────────────────────────────────────────────────────
        // All projects appear at root level; getParent() path for a sub-project
        // source file is: SOURCE_FILE → SOURCE_DIRECTORY → SUB_PROJECT (root-level)
        provider.setViewSettings(true, false, false);
        selection = await openAndReveal(fileUri);
        assert.strictEqual(selection.length, 1, 'Flat mode: expected one selected item');
        assert.strictEqual(selection[0].itemKind, ProjectViewItemKind.SOURCE_FILE);
        assert.strictEqual(selection[0].uri.fsPath, fileUri.fsPath);

        // ── 4. Runtime project ───────────────────────────────────────────────────
        // Enable showRuntimeFiles so runtime sources are searchable and revealable.
        provider.setViewSettings(false, false, true);
        const runtimeItem = provider
            .getChildren()
            .find((i) => i.itemKind === ProjectViewItemKind.RUNTIME_PROJECT);
        assert.ok(runtimeItem, 'Expected a RUNTIME_PROJECT item when showRuntimeFiles is enabled');
        const runtimeDirs = provider.getChildren(runtimeItem);
        assert.ok(runtimeDirs.length > 0, 'Expected runtime source directories');
        const runtimeFiles = provider.getChildren(runtimeDirs[0]);
        assert.ok(runtimeFiles.length > 0, 'Expected runtime source files');

        const runtimeFileUri = (runtimeFiles[0] as ProjectViewItem).uri;
        selection = await openAndReveal(runtimeFileUri);
        assert.strictEqual(selection.length, 1, 'Runtime: expected one selected item');
        assert.strictEqual(selection[0].itemKind, ProjectViewItemKind.SOURCE_FILE);
        assert.strictEqual(selection[0].uri.fsPath, runtimeFileUri.fsPath);
    });

    test('Unit hierarchy: switch layouts, browse sources, filter and reveal', async () => {
        // Both the provider and its visible tree must be ready.
        const provider = adaExtState.projectViewProvider;
        const treeView = adaExtState.projectTreeView;
        assert.ok(provider);
        assert.ok(treeView);

        // Save workspace settings so the test can restore them.
        const config = vscode.workspace.getConfiguration('ada');
        const settings = [
            'projectView.mode',
            'projectView.flatMode',
            'projectView.showObjectDirectories',
        ];
        const initialSettings = settings.map((key) => config.inspect(key)?.workspaceValue);

        // Both layouts are in the native submenu; View Options stays in the overflow menu.
        const menus = contributes.menus;
        assert.deepStrictEqual(
            menus['ada.projectView.viewAs'].map((item) => item.command),
            [CMD_PROJECT_VIEW_SHOW_FILES, CMD_PROJECT_VIEW_SHOW_UNITS],
        );
        const titleItems = menus['view/title'];
        assert.ok(
            titleItems.some(
                (item) =>
                    'submenu' in item &&
                    item.submenu === 'ada.projectView.viewAs' &&
                    !item.group.startsWith('navigation'),
            ),
        );
        const options = titleItems.find(
            (item) => 'command' in item && item.command === CMD_PROJECT_VIEW_OPTIONS,
        );
        assert.ok(options);
        assert.ok(!options.group.startsWith('navigation'));

        // Resolve a tree path and report any missing item.
        const findItem = (...labels: string[]): ProjectViewNode => {
            let item: ProjectViewNode | undefined;
            for (const label of labels) {
                const child = provider.getChildren(item).find((node) => node.label === label);
                assert.ok(child, `Expected ${labels.join(' / ')}`);
                item = child;
            }
            assert.ok(item);
            return item;
        };

        try {
            // Set file options to check that switching layouts preserves them.
            await config.update('projectView.flatMode', true, vscode.ConfigurationTarget.Workspace);
            await config.update(
                'projectView.showObjectDirectories',
                true,
                vscode.ConfigurationTarget.Workspace,
            );

            // The command switches layouts, saves the choice and updates the title.
            await vscode.commands.executeCommand(CMD_PROJECT_VIEW_SHOW_UNITS);
            await adaExtState.refreshUnitHierarchy();
            assert.strictEqual(provider.mode, ProjectViewMode.UNIT_HIERARCHY);
            assert.strictEqual(
                vscode.workspace.getConfiguration('ada').get('projectView.mode'),
                ProjectViewMode.UNIT_HIERARCHY,
            );
            assert.strictEqual(treeView.description, 'Aggr · Unit Hierarchy');

            // A new provider restores the saved layout.
            const restoredProvider = new ProjectViewProvider(
                adaExtState.adaClient.protocol2CodeConverter,
            );

            try {
                assert.strictEqual(restoredProvider.mode, ProjectViewMode.UNIT_HIERARCHY);
            } finally {
                restoredProvider.dispose();
            }

            // Each aggregate namespace has its own root, beside 'Other Sources'.
            assert.deepStrictEqual(
                provider.getChildren().map((item) => item.label),
                ['Project_1', 'Project_2', 'Other Sources'],
            );

            // Equal unit names have distinct IDs; unit groups do not open files.
            const firstMain = findItem('Project_1', 'Main_1');
            const secondMain = findItem('Project_2', 'Main_1');
            assert.strictEqual(firstMain.itemKind, ProjectViewItemKind.UNIT);
            assert.notStrictEqual(firstMain.id, secondMain.id);
            assert.strictEqual(firstMain.contextValue, 'unitHierarchy');
            assert.strictEqual(firstMain.command, undefined);

            // Child units group their specification, body and separate in that order.
            // Labels follow the specification even when the body uses different casing.
            const child = findItem('Project_2', 'Pkg', 'Child');
            const parts = provider.getChildren(child);
            assert.deepStrictEqual(
                parts.map((part) => part.label),
                ['Specification', 'Body', 'Separate WORK'],
            );

            // Each part is a file leaf under the unit, with the correct source URI.
            for (const [index, file] of [
                'pkg-child.ads',
                'pkg-child.adb',
                'pkg-child-work.adb',
            ].entries()) {
                const part = parts[index];
                assert.ok(part instanceof ProjectViewItem);
                assert.strictEqual(part.uri.fsPath, getWsUri('units', file).fsPath);
                assert.strictEqual(part.contextValue, 'sourceFile');
                assert.strictEqual(provider.getParent(part), child);
                assert.deepStrictEqual(provider.getChildren(part), []);
            }

            // Opening the body selects its defining name in the correct source.
            const body = parts[1];
            assert.ok(body.command);
            const args = body.command.arguments as [vscode.Uri, vscode.TextDocumentShowOptions];
            await vscode.commands.executeCommand(body.command.command, ...args);
            const editor = vscode.window.activeTextEditor;
            assert.ok(editor);
            assert.strictEqual(
                editor.document.uri.fsPath,
                getWsUri('units', 'pkg-child.adb').fsPath,
            );
            assert.strictEqual(editor.document.getText(editor.selection), 'pkg.child');

            // 'Reveal in Project View' selects the body item for the active editor in
            // the Project view.
            await vscode.commands.executeCommand(CMD_PROJECT_VIEW_REVEAL_ACTIVE_FILE);
            assert.deepStrictEqual(
                treeView.selection.map((item) => item.id),
                [body.id],
            );

            // A qualified-name filter keeps ancestors and all matching unit parts.
            provider.setFilter('pkg.child');
            assert.deepStrictEqual(
                provider.getChildren().map((item) => item.label),
                ['Project_2'],
            );
            assert.deepStrictEqual(
                provider.getChildren(findItem('Project_2', 'Pkg')).map((item) => item.label),
                ['Child'],
            );
            assert.deepStrictEqual(
                provider.getChildren(findItem('Project_2', 'Pkg', 'Child')).map((item) => item.id),
                parts.map((item) => item.id),
            );

            // Reveal cannot resolve a source hidden by the filter.
            assert.strictEqual(
                provider.findSourceFileItem(getWsUri('src', 'main_1.adb')),
                undefined,
            );
            provider.setFilter('');

            // Other languages remain accessible through 'Other Sources'.
            const otherFiles = provider.getChildren(findItem('Other Sources', 'units'));
            assert.deepStrictEqual(
                otherFiles.map((item) => item.label),
                ['helper.c'],
            );

            // Reveal also reaches files outside the Ada unit hierarchy.
            await showTextDocument('units', 'helper.c');
            await vscode.commands.executeCommand(CMD_PROJECT_VIEW_REVEAL_ACTIVE_FILE);
            assert.deepStrictEqual(
                treeView.selection.map((item) => item.id),
                [otherFiles[0].id],
            );

            // Runtime sources become revealable only while their option is enabled.
            const info = adaExtState.getProjectViewInfo();
            assert.ok(info?.runtime_project);
            const runtimeUri = vscode.Uri.file(info.runtime_project.sources[0].file_name);
            assert.strictEqual(provider.findSourceFileItem(runtimeUri), undefined);
            provider.setViewSettings(true, true, true);
            assert.ok(provider.findSourceFileItem(runtimeUri));
            provider.setViewSettings(true, true, false);
            assert.strictEqual(provider.findSourceFileItem(runtimeUri), undefined);

            // Switching back saves Files and preserves the file display options.
            await vscode.commands.executeCommand(CMD_PROJECT_VIEW_SHOW_FILES);
            assert.strictEqual(provider.mode, ProjectViewMode.FILES);
            assert.strictEqual(
                vscode.workspace.getConfiguration('ada').get('projectView.mode'),
                ProjectViewMode.FILES,
            );
            assert.strictEqual(treeView.description, 'Aggr · Files');
            assert.strictEqual(provider.flatMode, true);
            assert.strictEqual(provider.showObjectDirs, true);

            // File browsing restores object directories and source filenames.
            const project = findItem('Project_2');
            assert.ok(
                provider
                    .getChildren(project)
                    .some((item) => item.itemKind === ProjectViewItemKind.OBJECT_DIRECTORY),
            );
            assert.strictEqual(
                provider.findSourceFileItem(getWsUri('units', 'pkg-child.adb'))?.label,
                'pkg-child.adb',
            );
        } finally {
            // Restore settings for subsequent tests.
            for (const [index, key] of settings.entries()) {
                await config.update(
                    key,
                    initialSettings[index],
                    vscode.ConfigurationTarget.Workspace,
                );
            }
            provider.applyViewSettingsFromConfig();
        }
    });

    test('Unit hierarchy: recover from a failed load and refresh cached data', async () => {
        // Both the provider and its visible tree must be ready.
        const provider = adaExtState.projectViewProvider;
        const treeView = adaExtState.projectTreeView;
        assert.ok(provider);
        assert.ok(treeView);

        // Count hierarchy requests and fail the first load; forward other commands.
        const commandsWithPatch = vscode.commands as typeof vscode.commands & {
            executeCommand: typeof vscode.commands.executeCommand;
        };
        const originalExecuteCommand = vscode.commands.executeCommand.bind(vscode.commands);
        let requests = 0;
        let fail = true;
        commandsWithPatch.executeCommand = <T>(command: string, ...args: unknown[]) => {
            if (command === CMD_UNIT_HIERARCHY_INFORMATION) {
                requests++;
                if (fail) return Promise.reject(new Error('Unit hierarchy unavailable'));
            }
            return originalExecuteCommand<T>(command, ...args);
        };

        try {
            // File browsing does not request unit data.
            await adaExtState.refreshProjectView();
            assert.strictEqual(requests, 0);

            // A failed unit load leaves an empty tree and an error message.
            provider.setMode(ProjectViewMode.UNIT_HIERARCHY);
            await adaExtState.refreshUnitHierarchy();
            assert.strictEqual(requests, 1);
            assert.deepStrictEqual(provider.getChildren(), []);
            assert.strictEqual(
                treeView.message,
                'Unable to load the unit hierarchy. See the Ada extension logs.',
            );

            // File browsing remains available after the failure.
            provider.setMode(ProjectViewMode.FILES);
            assert.ok(provider.findSourceFileItem(getWsUri('src', 'main_1.adb')));

            // Concurrent retries share one request to the real server.
            fail = false;
            provider.setMode(ProjectViewMode.UNIT_HIERARCHY);
            await Promise.all([
                adaExtState.refreshUnitHierarchy(),
                adaExtState.refreshUnitHierarchy(),
            ]);
            assert.strictEqual(requests, 2);

            // A successful retry clears the error and makes unit sources revealable.
            assert.strictEqual(treeView.message, undefined);
            assert.ok(provider.findSourceFileItem(getWsUri('units', 'pkg-child.adb')));
            const ids = provider.getChildren().map((item) => item.id);

            // Switching layouts reuses the cached tree without another request.
            provider.setMode(ProjectViewMode.FILES);
            provider.setMode(ProjectViewMode.UNIT_HIERARCHY);
            await adaExtState.refreshUnitHierarchy();
            assert.strictEqual(requests, 2);
            assert.deepStrictEqual(
                provider.getChildren().map((item) => item.id),
                ids,
            );

            // A project refresh fetches new unit data and preserves the active filter.
            provider.setFilter('main_1');
            await adaExtState.refreshProjectView();
            assert.strictEqual(requests, 3);
            assert.strictEqual(provider.getFilter(), 'main_1');
            assert.strictEqual(treeView.message, 'Filtered by: "main_1"');

            // The refreshed tree exposes matching files and hides their siblings.
            assert.ok(provider.findSourceFileItem(getWsUri('src', 'main_1.adb')));
            assert.strictEqual(
                provider.findSourceFileItem(getWsUri('src', 'main_2.adb')),
                undefined,
            );
        } finally {
            // Restore real commands and clear the unit cache in Files mode.
            commandsWithPatch.executeCommand = originalExecuteCommand;
            provider.setMode(ProjectViewMode.FILES);
            await adaExtState.refreshProjectView();
        }
    });

    test('Go to File lists each project source exactly once', function () {
        const info = adaExtState.getProjectViewInfo();
        assert.ok(info, 'Expected project view information to be available');

        const items = buildProjectFileItems(info, false);

        // project_1.gpr and project_2.gpr both use the 'src' directory, so every
        // source is reported by two projects. They must appear only once.
        // Include the Ada and C sources from the limited import too.
        const labels = items.map((i) => i.label).sort();
        assert.deepStrictEqual(labels, [
            'helper.c',
            'main_1.adb',
            'main_2.adb',
            'main_3.adb',
            'pkg-child-work.adb',
            'pkg-child.adb',
            'pkg-child.ads',
            'pkg.ads',
        ]);

        const paths = items.map((i) => i.uri.fsPath);
        assert.strictEqual(
            new Set(paths).size,
            paths.length,
            'Expected no duplicate files in the quick-pick items',
        );

        for (const item of items) {
            assert.ok(item.uri.fsPath.endsWith(item.label), `URI of ${item.label} should match`);
            assert.ok(item.description, `Expected an owning project name for ${item.label}`);
            assert.ok(item.detail !== undefined, `Expected a directory for ${item.label}`);
        }
    });

    test('Go to File excludes runtime sources unless requested', function () {
        const info = adaExtState.getProjectViewInfo();
        assert.ok(info, 'Expected project view information to be available');
        assert.ok(info.runtime_project, 'Expected a runtime project in this workspace');

        const withoutRuntime = buildProjectFileItems(info, false);
        const withRuntime = buildProjectFileItems(info, true);

        assert.ok(
            withRuntime.length > withoutRuntime.length,
            'Expected more items when runtime sources are included',
        );

        const runtimeName = info.runtime_project.project.name;
        assert.ok(
            !withoutRuntime.some((i) => i.description === runtimeName),
            'Did not expect runtime entries when includeRuntime is false',
        );
        assert.ok(
            withRuntime.some((i) => i.description === runtimeName),
            'Expected runtime entries when includeRuntime is true',
        );
    });

    test('Go to File command is registered', async function () {
        const commands = await vscode.commands.getCommands(true);
        assert.ok(
            commands.includes(CMD_PROJECT_GO_TO_FILE),
            `Expected command ${CMD_PROJECT_GO_TO_FILE} to be registered`,
        );
    });
    test('Runtime visibility stays consistent between Go to File and the tree', function () {
        const provider = adaExtState.projectViewProvider;
        assert.ok(provider, 'Expected a Project View provider');

        const info = adaExtState.getProjectViewInfo();
        assert.ok(info, 'Expected project view information to be available');
        assert.ok(info.runtime_project, 'Expected a runtime project in this workspace');

        const workspaceWithPatch = vscode.workspace as typeof vscode.workspace & {
            getConfiguration: typeof vscode.workspace.getConfiguration;
        };
        const originalGetConfiguration = vscode.workspace.getConfiguration;

        // Simulate 'ada.projectView.showRuntimeFiles' being enabled through the
        // Settings UI rather than through the View Options quick-pick.
        const patchConfig = (showRuntimeFiles: boolean) => {
            workspaceWithPatch.getConfiguration = ((section?: string) => {
                if (section === 'ada') {
                    return {
                        get: (key: string, defaultValue: boolean) =>
                            key === 'projectView.showRuntimeFiles'
                                ? showRuntimeFiles
                                : defaultValue,
                    };
                }
                return originalGetConfiguration(section);
            }) as typeof vscode.workspace.getConfiguration;
        };

        try {
            // Before the provider is synced, the setting and the provider's
            // cached flag disagree. The quick-pick must follow the provider,
            // otherwise it would offer runtime files that the reveal path,
            // gated on that same cached flag, cannot resolve.
            patchConfig(true);
            assert.strictEqual(
                shouldIncludeRuntimeFiles(),
                provider.showRuntimeFiles,
                'Go to File must follow the Project View flag, not the raw setting',
            );

            provider.applyViewSettingsFromConfig();
            assert.strictEqual(
                provider.showRuntimeFiles,
                true,
                'Expected the provider to pick up the setting change',
            );

            // Every runtime file the picker offers must be resolvable by the
            // reveal path, which is gated on the provider's own flag.
            const items = buildProjectFileItems(info, provider.showRuntimeFiles);
            const runtimeName = info.runtime_project.project.name;
            const runtimeItem = items.find((i) => i.description === runtimeName);
            assert.ok(runtimeItem, 'Expected runtime entries once the setting is enabled');
            assert.ok(
                provider.findSourceFileItem(runtimeItem.uri),
                'A runtime file listed by Go to File must be revealable in the Project View',
            );

            // And with the setting disabled, neither path exposes runtime files.
            patchConfig(false);
            provider.applyViewSettingsFromConfig();
            assert.strictEqual(provider.showRuntimeFiles, false);
            assert.strictEqual(shouldIncludeRuntimeFiles(), false);
            assert.ok(
                !buildProjectFileItems(info, provider.showRuntimeFiles).some(
                    (i) => i.description === runtimeName,
                ),
                'Did not expect runtime entries once the setting is disabled',
            );
            assert.ok(
                !provider.findSourceFileItem(runtimeItem.uri),
                'Did not expect a runtime file to be revealable once the setting is disabled',
            );
        } finally {
            workspaceWithPatch.getConfiguration = originalGetConfiguration;
        }
    });
    test('Go to File reports no project when the ALS is unreachable', async function () {
        const stateWithPatch = adaExtState as typeof adaExtState & {
            getProjectUri: typeof adaExtState.getProjectUri;
        };
        const originalGetProjectUri = adaExtState.getProjectUri.bind(adaExtState);

        const windowWithPatch = vscode.window as typeof vscode.window & {
            showInformationMessage: typeof vscode.window.showInformationMessage;
        };
        const originalShowInformationMessage = vscode.window.showInformationMessage;

        let infoMessage: string | undefined;
        windowWithPatch.showInformationMessage = ((message: string) => {
            infoMessage = message;
            return Promise.resolve(undefined);
        }) as typeof vscode.window.showInformationMessage;

        // Simulate the ALS being restarted or otherwise unavailable: the
        // 'als-project-file' request rejects.
        stateWithPatch.getProjectUri = () => Promise.reject(new Error('ALS is not available'));

        try {
            // The refresh must absorb the failure rather than propagate it.
            await adaExtState.refreshProjectView();
            assert.strictEqual(
                adaExtState.getProjectViewInfo(),
                undefined,
                'Expected an empty project state after a failed refresh',
            );

            // And the command must report it, not throw.
            await vscode.commands.executeCommand(CMD_PROJECT_GO_TO_FILE);
            assert.ok(
                infoMessage?.includes('No GPR project'),
                `Expected the 'no project' message, got: ${String(infoMessage)}`,
            );
        } finally {
            stateWithPatch.getProjectUri = originalGetProjectUri;
            windowWithPatch.showInformationMessage = originalShowInformationMessage;
            // Restore a valid project state for the remaining tests.
            await adaExtState.refreshProjectView();
        }

        assert.ok(
            adaExtState.getProjectViewInfo(),
            'Expected the project view information to be restored',
        );
    });
});
