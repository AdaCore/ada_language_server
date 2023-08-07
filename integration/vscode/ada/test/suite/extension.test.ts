import * as assert from 'assert';
import { contextClients } from '../../src/extension';
import { suite, test } from 'mocha';
import { GlsMainResult, GlsExecutableResult } from '../../src/gprTaskProvider';
import { getProjectFile, getObjectDir } from '../../src/helpers';
import { assertEqualToFileContent } from './utils';

import * as vscode from 'vscode';

suite('Extension Tasks Test Suite', () => {
    suite('Tests on custom project: SampleProject', function () {
        // Make sure the extension is activated
        before(async () => {
            // eslint-disable-next-line @typescript-eslint/no-explicit-any
            const ext: vscode.Extension<any> | undefined =
                vscode.extensions.getExtension('AdaCore.ada');
            if (ext !== undefined) {
                if (!ext.isActive) {
                    await ext.activate();
                }
            }
        });
        test('Mains Server Response', async () => {
            if (vscode.workspace.workspaceFolders !== undefined) {
                await contextClients.adaClient.onReady();
                const result: GlsMainResult = await contextClients.adaClient.sendRequest(
                    '$/glsMains'
                );
                let filename = result.mains[0].replace(/^.*[\\/]/, '');
                assert.strictEqual(filename, 'gnatpp.adb');
                filename = result.mains[1].replace(/^.*[\\/]/, '');
                assert.strictEqual(filename, 'test.adb');
            } else {
                throw new Error('No workspace folder found for the specified URI');
            }
        });

        test('Executables Server Response', async () => {
            if (vscode.workspace.workspaceFolders !== undefined) {
                await contextClients.adaClient.onReady();
                const result: GlsExecutableResult = await contextClients.adaClient.sendRequest(
                    '$/glsExecutables'
                );
                let filename = result.executables[0].replace(/^.*[\\/]/, '');
                assert.strictEqual(filename, 'gnatpp');
                filename = result.executables[1].replace(/^.*[\\/]/, '');
                assert.strictEqual(filename, 'gnattest');
            } else {
                throw new Error('No workspace folder found for the specified URI');
            }
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
                const expectedUri = vscode.Uri.joinPath(
                    folder,
                    'src',
                    'test_subprogram_box.expected'
                );

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
});
