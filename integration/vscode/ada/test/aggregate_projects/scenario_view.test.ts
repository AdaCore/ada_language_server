import assert from 'assert';
import * as vscode from 'vscode';
import {
    CMD_SCENARIO_VIEW_RESET_VARIABLE,
    CMD_SCENARIO_VIEW_SET_VARIABLE,
} from '../../src/constants';
import { adaExtState } from '../../src/extension';
import { ScenarioViewItem } from '../../src/scenarioViewProvider';
import { activate } from '../utils';

/** Temporarily replaces `vscode.window.showQuickPick`/`showInputBox` for the duration of `fn`. */
async function withScenarioPickers<T>(
    answers: {
        quickPick?: (placeHolder: string | undefined) => string | undefined;
        input?: string;
    },
    fn: () => Thenable<T>,
): Promise<T> {
    const windowWithPatch = vscode.window as typeof vscode.window & {
        showQuickPick: typeof vscode.window.showQuickPick;
        showInputBox: typeof vscode.window.showInputBox;
    };
    const originalShowQuickPick = vscode.window.showQuickPick;
    const originalShowInputBox = vscode.window.showInputBox;

    windowWithPatch.showQuickPick = ((_items: string[], options?: vscode.QuickPickOptions) =>
        Promise.resolve(
            answers.quickPick?.(options?.placeHolder),
        )) as unknown as typeof vscode.window.showQuickPick;
    windowWithPatch.showInputBox = (() =>
        Promise.resolve(answers.input)) as unknown as typeof vscode.window.showInputBox;

    try {
        return await fn();
    } finally {
        windowWithPatch.showQuickPick = originalShowQuickPick;
        windowWithPatch.showInputBox = originalShowInputBox;
    }
}

/**
 * @returns the `ada.scenarioVariables` setting, normalized to `{}` when
 * unset (VS Code may report either `undefined` or its declared `null`
 * schema default depending on scope resolution).
 */
function currentScenarioVariablesSetting(): Record<string, string> {
    return (
        vscode.workspace.getConfiguration('ada').get<Record<string, string>>('scenarioVariables') ??
        {}
    );
}

/**
 * Clears `ada.scenarioVariables` and waits until a fresh read confirms it,
 * rather than just firing `update()` and trusting its promise.
 */
async function resetScenarioVariablesSetting(): Promise<void> {
    await vscode.workspace
        .getConfiguration('ada')
        .update('scenarioVariables', undefined, vscode.ConfigurationTarget.Workspace);

    for (let attempt = 0; attempt < 20; attempt++) {
        if (Object.keys(currentScenarioVariablesSetting()).length === 0) {
            return;
        }
        await new Promise((resolve) => setTimeout(resolve, 50));
    }
    throw new Error('ada.scenarioVariables did not clear after reset');
}

suite('Scenario View', function () {
    this.beforeAll(async () => {
        await activate();
    });

    // Reset both before and after each test: before, so every test starts
    // from a known-clean baseline regardless of what ran earlier; after, so
    // nothing lingers for whatever suite/file runs next.
    this.beforeEach(async () => {
        await resetScenarioVariablesSetting();
    });

    this.afterEach(async () => {
        await resetScenarioVariablesSetting();
        await adaExtState.refreshScenarioView();
    });

    this.afterAll(async () => {
        await resetScenarioVariablesSetting();
        await adaExtState.refreshScenarioView();
    });

    /** Refreshes the Scenario View from the real ALS and returns the named variable's item. */
    async function findScenarioItem(name: string): Promise<ScenarioViewItem> {
        await adaExtState.refreshScenarioView();
        const item = adaExtState.scenarioViewProvider
            ?.getChildren()
            .find((i) => i.info.name === name);
        assert.ok(item, `Expected a ${name} scenario variable`);
        return item;
    }

    test('Scenario View lists typed and untyped scenario variables', async () => {
        const mode = await findScenarioItem('MODE');
        const foo = await findScenarioItem('FOO');

        assert.strictEqual(mode.info.typed, true);
        assert.deepStrictEqual(mode.info.possibleValues, ['Debug', 'Release']);
        assert.strictEqual(mode.description, 'Default');

        assert.strictEqual(foo.info.typed, false);
        assert.strictEqual(foo.description, 'Default');
    });

    test('A variable set via ada.scenarioVariables shows its ALS-resolved value', async () => {
        await vscode.workspace
            .getConfiguration('ada')
            .update('scenarioVariables', { MODE: 'Release' }, vscode.ConfigurationTarget.Workspace);

        const mode = await findScenarioItem('MODE');
        assert.strictEqual(mode.info.value, 'Release');
        assert.strictEqual(mode.description, 'Release');
        assert.strictEqual(mode.contextValue, 'scenarioVariableTypedSet');
    });

    test('Setting a typed variable via the QuickPick updates ada.scenarioVariables', async () => {
        const item = await findScenarioItem('MODE');

        await withScenarioPickers({ quickPick: () => 'Release' }, () =>
            vscode.commands.executeCommand(CMD_SCENARIO_VIEW_SET_VARIABLE, item),
        );

        assert.deepStrictEqual(currentScenarioVariablesSetting(), { MODE: 'Release' });
    });

    test('Setting an untyped variable updates ada.scenarioVariables via input box', async () => {
        const item = await findScenarioItem('FOO');

        await withScenarioPickers({ input: 'baz' }, () =>
            vscode.commands.executeCommand(CMD_SCENARIO_VIEW_SET_VARIABLE, item),
        );

        assert.deepStrictEqual(currentScenarioVariablesSetting(), { FOO: 'baz' });
    });

    test('Editing one variable preserves another already-resolved variable', async () => {
        // Stand-in for a variable resolved from something other than this
        // setting (e.g. .als.json): from updateScenarioVariable's point of
        // view, "already resolved regardless of source" is exactly what
        // must survive a partial write, and als-scenario-variables-information
        // reports it identically either way.
        await vscode.workspace
            .getConfiguration('ada')
            .update(
                'scenarioVariables',
                { FOO: 'preexisting' },
                vscode.ConfigurationTarget.Workspace,
            );

        const item = await findScenarioItem('MODE');
        await withScenarioPickers({ quickPick: () => 'Release' }, () =>
            vscode.commands.executeCommand(CMD_SCENARIO_VIEW_SET_VARIABLE, item),
        );

        assert.deepStrictEqual(currentScenarioVariablesSetting(), {
            FOO: 'preexisting',
            MODE: 'Release',
        });
    });

    test('Resetting the only pinned variable clears ada.scenarioVariables entirely', async () => {
        await vscode.workspace
            .getConfiguration('ada')
            .update('scenarioVariables', { MODE: 'Release' }, vscode.ConfigurationTarget.Workspace);

        const item = await findScenarioItem('MODE');
        await vscode.commands.executeCommand(CMD_SCENARIO_VIEW_RESET_VARIABLE, item);

        assert.deepStrictEqual(
            currentScenarioVariablesSetting(),
            {},
            'Expected the setting to be cleared entirely rather than left as {}',
        );
    });

    test('Resetting one variable preserves another that is still pinned', async () => {
        await vscode.workspace
            .getConfiguration('ada')
            .update(
                'scenarioVariables',
                { MODE: 'Release', FOO: 'preexisting' },
                vscode.ConfigurationTarget.Workspace,
            );

        const item = await findScenarioItem('MODE');
        await vscode.commands.executeCommand(CMD_SCENARIO_VIEW_RESET_VARIABLE, item);

        assert.deepStrictEqual(currentScenarioVariablesSetting(), { FOO: 'preexisting' });
    });

    test('Concurrent edits to different variables are both preserved', async () => {
        // MODE and FOO are unrelated variables in this fixture, so editing
        // both at once exercises the same real read-modify-write path
        // without either edit depending on the other's outcome.
        const modeItem = await findScenarioItem('MODE');
        const fooItem = await findScenarioItem('FOO');

        await withScenarioPickers(
            { quickPick: () => 'Release', input: 'concurrent' },
            () =>
                Promise.all([
                    vscode.commands.executeCommand(CMD_SCENARIO_VIEW_SET_VARIABLE, modeItem),
                    vscode.commands.executeCommand(CMD_SCENARIO_VIEW_SET_VARIABLE, fooItem),
                ]) as unknown as Thenable<void>,
        );

        assert.deepStrictEqual(currentScenarioVariablesSetting(), {
            MODE: 'Release',
            FOO: 'concurrent',
        });
    });
});
