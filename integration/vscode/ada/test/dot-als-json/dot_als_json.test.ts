import assert from 'assert';
import path from 'path';
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
});
