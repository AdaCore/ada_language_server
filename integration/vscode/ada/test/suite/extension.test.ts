import * as assert from 'assert';
import { before } from 'mocha';
import { contextClients } from '../../src/extension';
import { GlsMainResult, GlsExecutableResult } from '../../src/gprTaskProvider';

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
            assert.strictEqual(filename, 'gnatpp');
            filename = result.executables[1].replace(/^.*[\\/]/, '');
            assert.strictEqual(filename, 'gnattest');
        } else {
            throw new Error('No workspace folder found for the specified URI');
        }
    });
});
