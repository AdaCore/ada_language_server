import * as assert from 'assert';
import { adaExtState } from '../../src/extension';
import { getObjectDir } from '../../src/helpers';
import { activate, assertEqualToFileContent } from '../utils';

import { readFileSync, writeFileSync } from 'fs';
import * as vscode from 'vscode';

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
});
