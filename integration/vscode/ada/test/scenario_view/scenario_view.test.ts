import assert from 'assert';
import path from 'path';
import * as vscode from 'vscode';
import {
    CMD_SCENARIO_VIEW_RESET_VARIABLE,
    CMD_SCENARIO_VIEW_SET_VARIABLE,
} from '../../src/constants';
import { adaExtState } from '../../src/extension';
import { ScenarioViewItem } from '../../src/scenarioViewProvider';
import { activate } from '../utils';

/**
 * The value `.als.json` gives to FILE_VAR, and which `prj.gpr` turns into
 * the project's object dir. Nothing sets it in `ada.scenarioVariables`, so
 * it is the value that must survive a partial write of that setting.
 */
const ALS_JSON_VALUE = 'als-json-obj';

/** Records what `setScenarioVariable` offered the user, for assertions. */
interface PickerCalls {
    inputBoxOptions?: vscode.InputBoxOptions;
    quickPickPlaceHolder?: string;
}

/** Temporarily replaces `vscode.window.showQuickPick`/`showInputBox` for the duration of `fn`. */
async function withScenarioPickers<T>(
    answers: { quickPick?: string; input?: string },
    fn: (calls: PickerCalls) => Thenable<T>,
): Promise<T> {
    const windowWithPatch = vscode.window as typeof vscode.window & {
        showQuickPick: typeof vscode.window.showQuickPick;
        showInputBox: typeof vscode.window.showInputBox;
    };
    const originalShowQuickPick = vscode.window.showQuickPick;
    const originalShowInputBox = vscode.window.showInputBox;
    const calls: PickerCalls = {};

    windowWithPatch.showQuickPick = ((_items: string[], options?: vscode.QuickPickOptions) => {
        calls.quickPickPlaceHolder = options?.placeHolder;
        return Promise.resolve(answers.quickPick);
    }) as unknown as typeof vscode.window.showQuickPick;
    windowWithPatch.showInputBox = ((options?: vscode.InputBoxOptions) => {
        calls.inputBoxOptions = options;
        return Promise.resolve(answers.input);
    }) as unknown as typeof vscode.window.showInputBox;

    try {
        return await fn(calls);
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
 * Writes `ada.scenarioVariables` and waits until a fresh read confirms the
 * new state, rather than just firing `update()` and trusting its promise.
 *
 * @param value - The value to write, or `undefined` to clear the setting.
 */
async function setScenarioVariablesSetting(value?: Record<string, string>): Promise<void> {
    await vscode.workspace
        .getConfiguration('ada')
        .update('scenarioVariables', value, vscode.ConfigurationTarget.Workspace);

    const expected = JSON.stringify(value ?? {});
    for (let attempt = 0; attempt < 40; attempt++) {
        if (JSON.stringify(currentScenarioVariablesSetting()) === expected) {
            return;
        }
        await new Promise((resolve) => setTimeout(resolve, 50));
    }
    throw new Error(`ada.scenarioVariables did not become ${expected}`);
}

/** @returns the base name of the object dir the ALS resolved the project with. */
async function resolvedObjectDir(): Promise<string> {
    return path.basename(await adaExtState.getObjectDir());
}

suite('Scenario View', function () {
    // Every test here goes through the real ALS: writing
    // ada.scenarioVariables makes the server reload the project, and the
    // next query queues behind that reload. That is well over the default
    // mocha timeout used in CI, so raise it -- unless timeouts have been
    // disabled altogether (timeout() === 0), which is the default when
    // running locally.
    const inheritedTimeout = this.timeout();
    if (inheritedTimeout !== 0) {
        this.timeout(Math.max(inheritedTimeout, 60000));
    }

    this.beforeAll(async () => {
        await activate();
        await setScenarioVariablesSetting(undefined);
    });

    // Restore the pristine state (nothing pinned, so FILE_VAR comes from
    // .als.json again) after every test, so tests stay independent and
    // nothing lingers for whatever runs next.
    this.afterEach(async () => {
        await setScenarioVariablesSetting(undefined);
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

    test('Variables are listed with the values the ALS resolved them to', async () => {
        const mode = await findScenarioItem('MODE');
        assert.strictEqual(mode.info.typed, true);
        assert.deepStrictEqual(mode.info.possibleValues, ['Debug', 'Release']);
        assert.strictEqual(mode.info.value, undefined);
        assert.strictEqual(mode.description, 'Default');
        assert.strictEqual(mode.contextValue, 'scenarioVariableTypedUnset');

        // FILE_VAR is set by .als.json only. It must show that value rather
        // than being reported as unset.
        const fileVar = await findScenarioItem('FILE_VAR');
        assert.strictEqual(fileVar.info.typed, false);
        assert.strictEqual(fileVar.info.value, ALS_JSON_VALUE);
        assert.strictEqual(fileVar.description, ALS_JSON_VALUE);
        assert.strictEqual(fileVar.contextValue, 'scenarioVariableUntypedSet');

        assert.strictEqual(await resolvedObjectDir(), ALS_JSON_VALUE);
    });

    test('Editing a variable preserves one supplied by .als.json', async () => {
        const mode = await findScenarioItem('MODE');

        await withScenarioPickers({ quickPick: 'Release' }, () =>
            vscode.commands.executeCommand(CMD_SCENARIO_VIEW_SET_VARIABLE, mode),
        );

        // FILE_VAR is written back alongside the edited MODE: ALS ignores
        // .als.json's scenarioVariables entirely once ada.scenarioVariables
        // is set, so omitting it here would silently drop it.
        assert.deepStrictEqual(currentScenarioVariablesSetting(), {
            FILE_VAR: ALS_JSON_VALUE,
            MODE: 'Release',
        });
        assert.strictEqual(await resolvedObjectDir(), ALS_JSON_VALUE);
    });

    test('An untyped variable is edited through a pre-filled input box', async () => {
        const fileVar = await findScenarioItem('FILE_VAR');

        const calls = await withScenarioPickers({ input: 'edited-obj' }, async (calls) => {
            await vscode.commands.executeCommand(CMD_SCENARIO_VIEW_SET_VARIABLE, fileVar);
            return calls;
        });

        // The input box starts from the resolved value, not empty.
        assert.strictEqual(calls.inputBoxOptions?.value, ALS_JSON_VALUE);

        assert.deepStrictEqual(currentScenarioVariablesSetting(), { FILE_VAR: 'edited-obj' });
        assert.strictEqual(await resolvedObjectDir(), 'edited-obj');
    });

    test('Resetting a variable preserves one supplied by .als.json', async () => {
        await setScenarioVariablesSetting({ FILE_VAR: ALS_JSON_VALUE, MODE: 'Release' });

        const mode = await findScenarioItem('MODE');
        await vscode.commands.executeCommand(CMD_SCENARIO_VIEW_RESET_VARIABLE, mode);

        assert.deepStrictEqual(currentScenarioVariablesSetting(), { FILE_VAR: ALS_JSON_VALUE });
        assert.strictEqual(await resolvedObjectDir(), ALS_JSON_VALUE);
    });

    test('Resetting the last pinned variable restores the .als.json value', async () => {
        await setScenarioVariablesSetting({ FILE_VAR: 'pinned-obj' });
        assert.strictEqual(await resolvedObjectDir(), 'pinned-obj');

        const fileVar = await findScenarioItem('FILE_VAR');
        await vscode.commands.executeCommand(CMD_SCENARIO_VIEW_RESET_VARIABLE, fileVar);

        // The setting is cleared entirely rather than left as `{}`, which VS
        // Code would still report as set and which would keep suppressing
        // the .als.json fallback.
        assert.deepStrictEqual(currentScenarioVariablesSetting(), {});
        assert.strictEqual(await resolvedObjectDir(), ALS_JSON_VALUE);
    });

    test('Concurrent edits to different variables are both preserved', async () => {
        // MODE and FILE_VAR are unrelated variables, so editing both at once
        // exercises the read-modify-write path without either edit depending
        // on the other's outcome.
        const mode = await findScenarioItem('MODE');
        const fileVar = await findScenarioItem('FILE_VAR');

        await withScenarioPickers({ quickPick: 'Release', input: 'concurrent-obj' }, () =>
            Promise.all([
                vscode.commands.executeCommand(CMD_SCENARIO_VIEW_SET_VARIABLE, mode),
                vscode.commands.executeCommand(CMD_SCENARIO_VIEW_SET_VARIABLE, fileVar),
            ]),
        );

        assert.deepStrictEqual(currentScenarioVariablesSetting(), {
            FILE_VAR: 'concurrent-obj',
            MODE: 'Release',
        });
    });
});
