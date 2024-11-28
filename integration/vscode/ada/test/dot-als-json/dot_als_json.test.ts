import assert from 'assert';
import { adaExtState } from '../../src/extension';
import { activate } from '../utils';
import path from 'path';
import { ConfigurationTarget, workspace } from 'vscode';

suite('dot-als-json', function () {
    this.beforeAll(async function () {
        await activate();
    });

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
        const expected = 'als-dot-json-value';
        assert.equal(objDir, expected);
    });

    test('case-2', async function () {
        const initial = workspace.getConfiguration('ada').get('scenarioVariables');
        try {
            /**
             * First we change the VS Code settings to override the scenario variable.
             */
            const sentinel = 'settings-json-value';
            await workspace.getConfiguration('ada').update('scenarioVariables', { Var: sentinel });

            /**
             * Check that the change overwrote the .als.json value
             */
            const objDir = path.basename(await adaExtState.getObjectDir());
            assert.equal(objDir, sentinel);
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
            /**
             * First we change the VS Code settings to override the scenario variable.
             */
            const sentinel = 'settings-json-value';
            await workspace.getConfiguration('ada').update('scenarioVariables', { Var: sentinel });

            /**
             * Check that the change overwrote the .als.json value
             */
            let objDir = path.basename(await adaExtState.getObjectDir());
            assert.equal(objDir, sentinel);

            /**
             * Now let's undo the override.
             */
            await workspace.getConfiguration('ada').update('scenarioVariables', undefined);

            /**
             * Ideally we would want the ALS to switch back to the setting from
             * .als.json but this is not the current behavior.
             *
             * Currently the didChangeConfiguration notification contains \{
             * "scenarioVariables": null \} which makes ALS ignore the provided
             * value and retain the last stored value.
             */
            objDir = path.basename(await adaExtState.getObjectDir());
            assert.equal(objDir, sentinel);
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
