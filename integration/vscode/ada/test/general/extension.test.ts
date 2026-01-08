import * as assert from 'assert';
import { adaExtState } from '../../src/extension';
import { getObjectDir, TERMINAL_ENV_SETTING_NAME } from '../../src/helpers';
import { activate, assertEqualToFileContent, showTextDocument } from '../utils';

import { readFileSync, writeFileSync } from 'fs';
import * as vscode from 'vscode';
import { CMD_RESTART_LANG_SERVERS } from '../../src/constants';

suite('Extensions Test Suite', function () {
    // Make sure the extension is activated
    this.beforeAll(async () => {
        await activate();
    });
    test('Project File Response', async () => {
        if (vscode.workspace.workspaceFolders !== undefined) {
            // Uri obtained from the ALS
            const alsUri = await adaExtState.getProjectUri();
            // Uri manually computed based on the loaded workspace
            const wsUri = vscode.Uri.joinPath(vscode.workspace.workspaceFolders[0].uri, 'prj.gpr');
            // Ask for fsPath, it will resolve wsUri._fsPath
            // eslint-disable-next-line @typescript-eslint/no-unused-expressions
            wsUri.fsPath != null;
            // Both should match
            assert.deepStrictEqual(alsUri?.fsPath, wsUri.fsPath);
        } else {
            throw new Error('No workspace folder found for the specified URI');
        }
    });
    test('Object Directory Response', async () => {
        if (vscode.workspace.workspaceFolders !== undefined) {
            const result: string = await getObjectDir(adaExtState.adaClient);
            const name = result?.replace(/^.*[\\/]/, '');
            assert.strictEqual(name, 'obj');
        } else {
            throw new Error('No workspace folder found for the specified URI');
        }
    });
    test('Test Add Subprogram Box', async () => {
        if (vscode.workspace.workspaceFolders !== undefined) {
            const cursorPositions: vscode.Position[] = [
                new vscode.Position(9, 1),
                new vscode.Position(4, 1),
                new vscode.Position(1, 1),
            ];
            const folder = vscode.workspace.workspaceFolders[0].uri;
            const fileUri = vscode.Uri.joinPath(folder, 'src', 'test_subprogram_box.adb');
            const contentBefore = readFileSync(fileUri.fsPath);
            const expectedUri = vscode.Uri.joinPath(folder, 'src', 'test_subprogram_box.expected');

            for (const cursorPos of cursorPositions) {
                await vscode.window.showTextDocument(fileUri, {
                    selection: new vscode.Range(cursorPos, cursorPos),
                });
                await vscode.commands.executeCommand('ada.subprogramBox');
            }
            const editorText = vscode.window.activeTextEditor?.document.getText() ?? '';
            vscode.window.activeTextEditor?.hide();

            try {
                assertEqualToFileContent(editorText, expectedUri);
            } finally {
                /**
                 * Restore the old file content
                 */
                writeFileSync(fileUri.fsPath, contentBefore);
            }
        } else {
            throw new Error('No workspace folder found for the specified URI');
        }
    });

    test('New File - Ada Main', async () => {
        if (vscode.workspace.workspaceFolders !== undefined) {
            await vscode.commands.executeCommand('ada.createNewAdaMainUnit');
            const activeEditor = vscode.window.activeTextEditor;
            assert.strictEqual(
                activeEditor?.document.languageId,
                'ada',
                'We should have a newly created Ada editor',
            );
            const text = activeEditor.document.getText() ?? '';
            assert.strictEqual(
                text.startsWith('procedure'),
                true,
                `We should have a main procedure snippet at the beginning ` +
                    `of the new editor, but instead we have:\n\n${text}`,
            );
        }
    });

    test('New File - Ada Package', async () => {
        if (vscode.workspace.workspaceFolders !== undefined) {
            await vscode.commands.executeCommand('ada.createNewAdaPackage');
            const activeEditor = vscode.window.activeTextEditor;
            assert.strictEqual(
                activeEditor?.document.languageId,
                'ada',
                'We should have a newly created Ada editor',
            );
            const text = activeEditor.document.getText() ?? '';
            assert.strictEqual(
                text.startsWith('package'),
                true,
                `We should have a package declaration snippet at the beginning ` +
                    `of the new editor, but instead we have:\n\n${text}`,
            );
        }
    });

    test('Restart Language Server Commands', async () => {
        if (vscode.workspace.workspaceFolders !== undefined) {
            const initialEnvSettings = vscode.workspace
                .getConfiguration()
                .get(TERMINAL_ENV_SETTING_NAME);

            try {
                // Update the environment ARCHIVE_SUFFIX environment variable which
                // controls the 'Archive_Suffix' GPR project attribute of the test
                // workspace's project.

                await vscode.workspace.getConfiguration().update(
                    TERMINAL_ENV_SETTING_NAME,
                    {
                        ...(initialEnvSettings ?? {}),
                        ARCHIVE_SUFFIX: '.b',
                    },
                    vscode.ConfigurationTarget.Workspace,
                );

                // Check that the 'Archive_Suffix' project attribute to
                // make sure it's set to its default value ('.a') before setting
                // the ARCHIVE_SUFFIX environment variable to '.b'.
                assert.strictEqual(
                    await adaExtState.getProjectAttributeValue('Archive_Suffix'),
                    '.a',
                    'Environment updates were not taken into account when restarting ALS',
                );

                // Restart the server and check that we still have the same project
                // loaded on ALS side
                const oldAlsUri = await adaExtState.getProjectUri();
                await vscode.commands.executeCommand(CMD_RESTART_LANG_SERVERS);
                const newAlsUri = await adaExtState.getProjectUri();
                assert.deepStrictEqual(
                    oldAlsUri?.fsPath,
                    newAlsUri?.fsPath,
                    'Wrong project URI received after Ada server restarted',
                );

                // Check that environment updates are taken into account after restarting:
                // 'Archive_Suffix' GPR project attribute should now be equal to '.b'
                assert.strictEqual(
                    await adaExtState.getProjectAttributeValue('Archive_Suffix'),
                    '.b',
                    'Environment updates were not taken into account when restarting ALS',
                );

                // Check that the GPR server runs correctly after restarting it
                assert.ok(
                    adaExtState.gprClient.isRunning(),
                    'The ALS instance for GPR files should be running after restarting it',
                );
            } finally {
                // Restore the terminal.integrate.env.* settings for the workspace
                await vscode.workspace
                    .getConfiguration()
                    .update(
                        TERMINAL_ENV_SETTING_NAME,
                        undefined,
                        vscode.ConfigurationTarget.Workspace,
                    );
            }
        }
    });

    test('Clear Cache On Project Reload', async () => {
        if (vscode.workspace.workspaceFolders !== undefined) {
            // Get the workspace root folder
            const folder = vscode.workspace.workspaceFolders[0].uri;

            // Check the object directory when 'for Object_Dir use "obj"' is present
            // in the GPR file
            const originalObjDir: string = await adaExtState.getObjectDir();
            const originalObjDirURI = vscode.Uri.file(originalObjDir);

            assert.strictEqual(originalObjDirURI.path, vscode.Uri.joinPath(folder, 'obj').path);

            // Remove the line that specifies the object directory in the GPR file
            const fileUri = vscode.Uri.joinPath(folder, 'prj.gpr');
            const contentBefore = readFileSync(fileUri.fsPath, 'utf-8');
            const newContent = contentBefore.replace('    for Object_Dir use "obj";', '');
            writeFileSync(fileUri.fsPath, newContent, 'utf-8');

            // Reload the project and check the object dir value: should be set
            // to the project's root directory (workspace directory)
            await vscode.commands.executeCommand('als-reload-project');
            try {
                const objDirValue = await adaExtState.getObjectDir();
                const objDirURI = vscode.Uri.file(objDirValue);
                assert.strictEqual(objDirURI.path, folder.path);
            } finally {
                // Restore the old GPR file contents
                writeFileSync(fileUri.fsPath, contentBefore);
            }

            // Reload the project to its original state, and check that
            // the object directory is back to its original value too.
            await vscode.commands.executeCommand('als-reload-project');
            assert.strictEqual(await adaExtState.getObjectDir(), originalObjDir);
        } else {
            throw new Error('No workspace folder found for the specified URI');
        }
    });

    test('Auto Reload Project On Save', async () => {
        if (vscode.workspace.workspaceFolders !== undefined) {
            // Get the workspace root folder
            const folder = vscode.workspace.workspaceFolders[0].uri;

            // Set the 'Always' preference for auto-reload, just for this test
            const alwaysReloadKey = 'ada.autoReloadProject.alwaysReload';
            await adaExtState.context.workspaceState.update(alwaysReloadKey, true);

            try {
                // Check that Exec_Dir is not initially set (should be empty or default)
                const initialExecDir = await adaExtState.getProjectAttributeValue('Exec_Dir');
                assert.strictEqual(initialExecDir, 'obj', 'Exec_Dir should be initially "obj"');

                // Open the GPR file
                const fileUri = vscode.Uri.joinPath(folder, 'prj.gpr');
                const document = await vscode.workspace.openTextDocument(fileUri);
                const editor = await vscode.window.showTextDocument(document);

                // Store the original content
                const contentBefore = document.getText();

                // Edit the document to add Exec_Dir attribute
                const text = document.getText();
                const modifiedText = text.replace(
                    '    for Object_Dir use "obj";',
                    '    for Object_Dir use "obj";\n    for Exec_Dir use "bin";',
                );

                await editor.edit((editBuilder) => {
                    const fullRange = new vscode.Range(
                        document.positionAt(0),
                        document.positionAt(text.length),
                    );
                    editBuilder.replace(fullRange, modifiedText);
                });

                // Save the document to trigger the auto-reload
                await document.save();

                // Wait a bit for the auto-reload to complete
                await new Promise((resolve) => setTimeout(resolve, 1000));

                // Check that Exec_Dir has been set to "bin"
                const newExecDir = await adaExtState.getProjectAttributeValue('Exec_Dir');
                assert.strictEqual(
                    newExecDir,
                    'bin',
                    'Exec_Dir should be set to "bin" after auto-reload',
                );

                // Restore the original content using the VS Code API
                const currentText = document.getText();
                await editor.edit((editBuilder) => {
                    const fullRange = new vscode.Range(
                        document.positionAt(0),
                        document.positionAt(currentText.length),
                    );
                    editBuilder.replace(fullRange, contentBefore);
                });

                // Save again to trigger another auto-reload
                await document.save();

                // Wait for the auto-reload
                await new Promise((resolve) => setTimeout(resolve, 1000));

                // Check that Exec_Dir is back to its initial value
                const restoredExecDir = await adaExtState.getProjectAttributeValue('Exec_Dir');
                assert.strictEqual(
                    restoredExecDir,
                    initialExecDir,
                    'Exec_Dir should be restored to its initial value',
                );
            } finally {
                // Clear the 'Always' preference
                await adaExtState.context.workspaceState.update(alwaysReloadKey, undefined);
            }
        } else {
            throw new Error('No workspace folder found for the specified URI');
        }
    });

    test('Split long comments on ENTER', async () => {
        if (vscode.workspace.workspaceFolders !== undefined) {
            // Get a file with a long comment (>80 chars)
            const srcRelPath = ['src', 'split_comment.ads'];
            const textEditor = await showTextDocument(...srcRelPath);

            // Set the cursor in the middle of the long comment
            const insertPos = new vscode.Position(0, 69);
            textEditor.selection = new vscode.Selection(insertPos, insertPos);

            // Simulate a keypress on the ENTER key
            const inputText = '\n';
            await vscode.commands.executeCommand('type', { text: inputText });

            // Check that we now have two lines, and that the second line
            // starts with the '--  ' comment tag
            const text = textEditor.document.getText() ?? '';
            assert.strictEqual(
                text,
                "--  Used to test the VS Code extension 'onEnterRule' that splits long\n" +
                    '--   comments when pressing ENTER\n',
                `Wrong text in the editor after pressing ENTER in the middle of a comment`,
            );
        }
    });
});
