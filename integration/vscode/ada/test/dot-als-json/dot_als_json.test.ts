import assert from 'assert';
import { readFileSync, writeFileSync } from 'fs';
import path from 'path';
import * as vscode from 'vscode';
import { ConfigurationTarget, workspace } from 'vscode';
import { adaExtState } from '../../src/extension';
import { activate } from '../utils';

suite('dot-als-json', function () {
    this.beforeAll(async function () {
        await activate();
    });

    const alsDotJsonValue = 'als-dot-json-value';
    const settingsDotJsonValue = 'settings-json-value';

    /**
     * The goal of this testsuite is to verify the interaction between settings
     * coming from .als.json and .vscode/settings.json.
     *
     * We want to verify multiple cases:
     *
     * 1. A value set in .als.json and not overriden by settings.json
     * 2. A value set in .als.json and overriden by settings.json
     *
     * To check the outcome, we use the scenarioVariables setting to set a
     * scenario variable that is used in prj.gpr to set the Object_Dir which we
     * can then query to observe the effect on the ALS.
     */

    test('at-init', async function () {
        /**
         * Check that the project file is the one defined in .als.json
         */
        assert.equal(path.basename(await adaExtState.getProjectFile()), 'prj.gpr');
    });

    test('case-1', async function () {
        /**
         * Initially the scenario variable is set in .als.json but not in settings.json
         */
        const objDir = path.basename(await adaExtState.getObjectDir());
        const expected = alsDotJsonValue;
        assert.equal(objDir, expected);
    });

    test('case-2', async function () {
        const initial = workspace.getConfiguration('ada').get('scenarioVariables');
        try {
            await workspace
                .getConfiguration('ada')
                .update('scenarioVariables', { Var: settingsDotJsonValue });

            /**
             * Check that the change overwrote the .als.json value
             */
            const objDir = path.basename(await adaExtState.getObjectDir());
            assert.equal(objDir, settingsDotJsonValue);
        } finally {
            /**
             * Restore the initial value of the setting.
             */
            await workspace
                .getConfiguration('ada')
                .update(
                    'scenarioVariables',
                    initial == null ? undefined : initial,
                    ConfigurationTarget.Workspace,
                );
        }
    });

    test('undo-override', async function () {
        const initial = workspace.getConfiguration('ada').get('scenarioVariables');
        try {
            await workspace
                .getConfiguration('ada')
                .update('scenarioVariables', { Var: settingsDotJsonValue });

            /**
             * Check that the change overwrote the .als.json value
             */
            let objDir = path.basename(await adaExtState.getObjectDir());
            assert.equal(objDir, settingsDotJsonValue);

            /**
             * Now let's undo the override.
             */
            await workspace.getConfiguration('ada').update('scenarioVariables', undefined);

            /**
             * The value should go back to the .als.json value
             */
            objDir = path.basename(await adaExtState.getObjectDir());
            assert.equal(objDir, alsDotJsonValue);
        } finally {
            /**
             * Restore the initial value of the setting.
             */
            await workspace
                .getConfiguration('ada')
                .update(
                    'scenarioVariables',
                    initial == null ? undefined : initial,
                    ConfigurationTarget.Workspace,
                );
        }
    });

    test('als-json-file-change-clears-caches', async function () {
        const wsFolder = workspace.workspaceFolders?.[0];
        assert.ok(wsFolder, 'No workspace folder found');

        const alsJsonPath = path.join(wsFolder.uri.fsPath, '.als.json');
        const originalContent = readFileSync(alsJsonPath, 'utf-8');

        // Stub showWarningMessage to simulate the user clicking
        // "Restart Language Servers" in the popup.
        const originalShowWarningMessage = vscode.window.showWarningMessage;
        // eslint-disable-next-line @typescript-eslint/no-explicit-any
        const win: any = vscode.window;
        // eslint-disable-next-line @typescript-eslint/no-unsafe-member-access
        win.showWarningMessage = (...args: string[]) => {
            return Promise.resolve(args.find((a) => a !== args[0]));
        };

        try {
            // Populate caches by querying the object dir
            await adaExtState.getObjectDir();
            assert.notStrictEqual(
                adaExtState.cachedObjectDir,
                undefined,
                'cachedObjectDir should be populated after getObjectDir()',
            );

            // Modify .als.json to trigger the file watcher
            const modified = JSON.parse(originalContent) as Record<string, unknown>;
            modified['scenarioVariables'] = { Var: 'als-dot-json-value' };
            writeFileSync(alsJsonPath, JSON.stringify(modified, undefined, 2));

            // Wait for the file watcher to fire and the stub to resolve,
            // clearing the caches.
            await new Promise((resolve) => setTimeout(resolve, 2000));

            // Verify that the cache was cleared, because the user accepted
            // the restart popup.
            assert.strictEqual(
                adaExtState.cachedObjectDir,
                undefined,
                'cachedObjectDir should be cleared after .als.json change',
            );
        } finally {
            // Restore the original .als.json content and the original
            // showWarningMessage function.
            writeFileSync(alsJsonPath, originalContent);
            // eslint-disable-next-line @typescript-eslint/no-unsafe-member-access
            win.showWarningMessage = originalShowWarningMessage;
        }
    });
});
