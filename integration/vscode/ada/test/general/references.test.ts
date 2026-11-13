import assert from 'assert';
import { setTimeout } from 'timers/promises';
import * as vscode from 'vscode';
import { ReferencesInput } from '../../src/referencesProvider';
import { activate, closeAllEditors, showTextDocument } from '../utils';

suite('References with kinds', function () {
    // Save the method so we can restore it after checking calls from VS Code's References view.
    // eslint-disable-next-line @typescript-eslint/unbound-method
    const originalResolve = ReferencesInput.prototype.resolve;
    let resolutions: ReturnType<ReferencesInput['resolve']>[];

    this.beforeAll(activate);

    this.beforeEach(async function () {
        await closeAllEditors();
        const editor = await showTextDocument('src', 'reference_kinds.ads');
        const position = new vscode.Position(1, 4);
        editor.selection = new vscode.Selection(position, position);
        resolutions = [];
        // Keep the real requests made by VS Code's References view. setInput returns
        // before the results arrive, so the tests wait for each request to finish.
        ReferencesInput.prototype.resolve = function () {
            const result = originalResolve.call(this);
            resolutions.push(result);
            return result;
        };
    });

    this.afterEach(async function () {
        ReferencesInput.prototype.resolve = originalResolve;
        await vscode.commands.executeCommand('references-view.clear');
        await vscode.commands.executeCommand('references-view.clearHistory');
        await closeAllEditors();
    });

    test('read/write labels and opening a result', async function () {
        await vscode.commands.executeCommand('ada.findReferences');
        await waitForResolutions(resolutions, 1);
        const model = await resolutions[0];
        assert(model);

        const files = model.provider.getChildren();
        assert.strictEqual(files.length, 2);
        const references = files.flatMap((file) => model.provider.getChildren(file));
        assert.strictEqual(references.length, 3);
        const items = await Promise.all(references.map((ref) => model.provider.getTreeItem(ref)));
        assert.deepStrictEqual(
            items.map((item) => item.description),
            ['write', 'read', 'read'],
        );

        const command = items[0].command;
        assert(command);
        await vscode.commands.executeCommand(command.command, ...(command.arguments as unknown[]));
        const editor = vscode.window.activeTextEditor;
        assert(editor);
        assert(editor.document.uri.path.endsWith('/reference_kinds.adb'));
        assert.strictEqual(editor.document.getText(editor.selection), 'Counter');

        assert.strictEqual(model.navigation.next(references[0]), references[1]);
        assert.strictEqual(model.navigation.previous(references[0]), references[2]);
    });

    test('rerunning the search through References history', async function () {
        await vscode.commands.executeCommand('ada.findReferences');
        await waitForResolutions(resolutions, 1);
        const firstModel = await resolutions[0];
        assert(firstModel);

        // Let the References view finish saving the search in its history.
        await vscode.commands.executeCommand('references-view.tree.focus');
        await vscode.commands.executeCommand('references-view.refresh');
        await waitForResolutions(resolutions, 2);
        const model = await resolutions[1];
        assert(model);
        const references = model.provider
            .getChildren()
            .flatMap((file) => model.provider.getChildren(file));
        const items = await Promise.all(references.map((ref) => model.provider.getTreeItem(ref)));
        assert.deepStrictEqual(
            items.map((item) => item.description),
            ['write', 'read', 'read'],
        );

        const view = await vscode.extensions
            .getExtension<{ getInput(): ReferencesInput }>('vscode.references-view')
            ?.activate();
        assert(view);
        const input = view.getInput();
        assert(input instanceof ReferencesInput);
        const movedLocation = new vscode.Location(input.location.uri, new vscode.Position(2, 30));
        const movedInput = input.with(movedLocation);
        assert.strictEqual(movedInput.location.uri.toString(), movedLocation.uri.toString());
        assert(movedInput.location.range.isEqual(movedLocation.range));
    });
});

/** Waits for the References view to start the expected requests, for up to ten seconds. */
async function waitForResolutions(resolutions: readonly unknown[], count: number): Promise<void> {
    const deadline = Date.now() + 10_000;
    while (resolutions.length < count && Date.now() < deadline) {
        await setTimeout(20);
    }
    assert.strictEqual(resolutions.length, count);
}
