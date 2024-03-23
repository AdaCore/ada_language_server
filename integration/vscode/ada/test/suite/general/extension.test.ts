import * as assert from 'assert';
import { adaExtState } from '../../../src/extension';
import { getProjectFile, getObjectDir } from '../../../src/helpers';
import { assertEqualToFileContent, activate } from '../utils';

import * as vscode from 'vscode';
import { readFileSync, writeFileSync } from 'fs';

suite('Extensions Test Suite', function () {
    // Make sure the extension is activated
    this.beforeAll(async () => {
        await activate();
    });
    test('Project File Response', async () => {
        if (vscode.workspace.workspaceFolders !== undefined) {
            const result: string = await getProjectFile(adaExtState.adaClient);
            const name = result.replace(/^.*[\\/]/, '');
            assert.strictEqual(name, 'default.gpr');
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
});
