import * as assert from 'assert';
import { contextClients } from '../../../../src/extension';
import { suite, test } from 'mocha';
import { getProjectFile, getObjectDir } from '../../../../src/helpers';
import { assertEqualToFileContent, activate } from '../../utils';

import * as vscode from 'vscode';

suite('Extensions Test Suite', function () {
    // Make sure the extension is activated
    this.beforeAll(async () => {
        await activate();
    });
    test('Project File Response', async () => {
        if (vscode.workspace.workspaceFolders !== undefined) {
            await contextClients.adaClient.onReady();
            const result: string = await getProjectFile(contextClients.adaClient);
            const name = result.replace(/^.*[\\/]/, '');
            assert.strictEqual(name, 'default.gpr');
        } else {
            throw new Error('No workspace folder found for the specified URI');
        }
    });
    test('Object Directory Response', async () => {
        if (vscode.workspace.workspaceFolders !== undefined) {
            await contextClients.adaClient.onReady();
            const result: string = await getObjectDir(contextClients.adaClient);
            const name = result?.replace(/^.*[\\/]/, '');
            assert.strictEqual(name, 'obj');
        } else {
            throw new Error('No workspace folder found for the specified URI');
        }
    });
    test('Test Add Subprogram Box', async () => {
        if (vscode.workspace.workspaceFolders !== undefined) {
            await contextClients.adaClient.onReady();
            const cursorPositions: vscode.Position[] = [
                new vscode.Position(9, 1),
                new vscode.Position(4, 1),
                new vscode.Position(1, 1),
            ];
            const folder = vscode.workspace.workspaceFolders[0].uri;
            const fileUri = vscode.Uri.joinPath(folder, 'src', 'test_subprogram_box.adb');
            const expectedUri = vscode.Uri.joinPath(folder, 'src', 'test_subprogram_box.expected');

            for (const cursorPos of cursorPositions) {
                await vscode.window.showTextDocument(fileUri, {
                    selection: new vscode.Range(cursorPos, cursorPos),
                });
                await vscode.commands.executeCommand('ada.subprogramBox');
            }
            const editorText = vscode.window.activeTextEditor?.document.getText() ?? '';

            assertEqualToFileContent(editorText, expectedUri);
        } else {
            throw new Error('No workspace folder found for the specified URI');
        }
    });
});
