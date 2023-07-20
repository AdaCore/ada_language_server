import * as assert from 'assert';
import { before } from 'mocha';
import { contextClients } from '../../src/extension';
import { GlsMainResult, GlsExecutableResult } from '../../src/gprTaskProvider';
import { assertEqualToFileContent } from './utils';
import * as vscode from 'vscode';

suite('Extension Tasks Test Suite', () => {
    before(() => {
        // eslint-disable-next-line @typescript-eslint/no-floating-promises
        vscode.window.showInformationMessage('Start all tests.');
    });

    test('Test Build Tasks Server Response', async () => {
        // eslint-disable-next-line @typescript-eslint/no-explicit-any
        const ext: vscode.Extension<any> | undefined =
            vscode.extensions.getExtension('AdaCore.ada');
        if (ext !== undefined) {
            if (!ext.isActive) {
                await ext.activate();
            }
        }
        if (vscode.workspace.workspaceFolders !== undefined) {
            await contextClients.adaClient.onReady();
            const result: GlsMainResult = await contextClients.adaClient.sendRequest('$/glsMains');
            let filename = result.mains[0].replace(/^.*[\\/]/, '');
            assert.strictEqual(filename, 'gnatpp.adb');
            filename = result.mains[1].replace(/^.*[\\/]/, '');
            assert.strictEqual(filename, 'test.adb');
        } else {
            throw new Error('No workspace folder found for the specified URI');
        }
    });
    test('Test Run Executables Tasks Server Response', async () => {
        if (vscode.workspace.workspaceFolders !== undefined) {
            await contextClients.adaClient.onReady();
            const result: GlsExecutableResult = await contextClients.adaClient.sendRequest(
                '$/glsExecutables'
            );
            let filename = result.executables[0].replace(/^.*[\\/]/, '');
            const exeExt = process.platform == 'win32' ? '.exe' : '';
            assert.strictEqual(filename, 'gnatpp' + exeExt);
            filename = result.executables[1].replace(/^.*[\\/]/, '');
            assert.strictEqual(filename, 'gnattest' + exeExt);
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
