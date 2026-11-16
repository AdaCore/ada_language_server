import assert from 'assert';
import { setTimeout } from 'timers/promises';
import * as vscode from 'vscode';
import { ReferencesInput } from '../../src/referencesProvider';
import { activate, closeAllEditors, showTextDocument } from '../utils';

type ReferencesResult = NonNullable<Awaited<ReturnType<ReferencesInput['resolve']>>>;

suite('References with kinds', function () {
    // Save the method so we can restore it after checking calls from VS Code's References view.
    // eslint-disable-next-line @typescript-eslint/unbound-method
    const originalResolve = ReferencesInput.prototype.resolve;
    let resolutions: ReturnType<ReferencesInput['resolve']>[];
    let view: {
        getInput(): ReferencesInput;
        setInput(input: ReferencesInput): void;
    };

    this.beforeAll(async function () {
        // Activate Ada commands and get the References API so tests can run
        // and reopen searches.
        await activate();
        const extension = vscode.extensions.getExtension<typeof view>('vscode.references-view');
        assert(extension);
        view = await extension.activate();
    });

    this.beforeEach(async function () {
        // Start each test at the Ada variable 'Counter' with no previously recorded requests.
        await closeAllEditors();
        await selectAdaEntityInEditor();
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
        // Restore resolve and clear the view, history, and editors to avoid affecting
        // other tests.
        ReferencesInput.prototype.resolve = originalResolve;
        await vscode.commands.executeCommand('references-view.clear');
        await vscode.commands.executeCommand('references-view.clearHistory');
        await closeAllEditors();
    });

    test('read/write labels, filtering, and opening a result', async function () {
        await vscode.commands.executeCommand('ada.findReferences');
        const model = await waitForResolutions(resolutions, 1);

        // Show all three references, grouped by file, with their read/write labels.
        const files = model.provider.getChildren();
        assert.strictEqual(files.length, 2);
        const references = getReferences(model);
        assert.strictEqual(references.length, 3);
        const items = await Promise.all(references.map((ref) => model.provider.getTreeItem(ref)));
        assert.deepStrictEqual(
            items.map((item) => item.description),
            ['write', 'read', 'read'],
        );

        // Opening a result selects the referenced symbol in its source file.
        const command = items[0].command;
        assert(command);
        await vscode.commands.executeCommand(command.command, ...(command.arguments as unknown[]));
        const editor = vscode.window.activeTextEditor;
        assert(editor);
        assert(editor.document.uri.path.endsWith('/reference_kinds.adb'));
        assert.strictEqual(editor.document.getText(editor.selection), 'Counter');

        // Navigation follows source order and wraps across files.
        assert.strictEqual(model.navigation.next(references[0]), references[1]);
        assert.strictEqual(model.navigation.previous(references[0]), references[2]);
        const [write, bodyRead, specRead] = references.map((ref) => model.navigation.location(ref));
        assert(write && bodyRead && specRead);

        const input = view.getInput();
        assert(input instanceof ReferencesInput);

        // Writes keep only the assignment and hide the file containing only reads.
        input.setFilter('write');
        assert.strictEqual(model.message, 'Writes only — 1 of 3 references in 1 file');
        assert.deepStrictEqual(model.provider.getChildren(), [files[0]]);
        assert.deepStrictEqual(model.provider.getChildren(files[0]), [references[0]]);
        assert.deepStrictEqual(model.provider.getChildren(files[1]), []);

        // Menu counts still include results hidden by the filter.
        assert.deepStrictEqual(model.provider.getFilterOptions(), [
            { label: 'All references', description: '3 references' },
            { referenceKind: 'write', label: 'Writes', description: '1 reference (current)' },
            { referenceKind: 'reference', label: 'Reads', description: '2 references' },
        ]);

        // Navigation and highlights include only the visible write.
        assert.strictEqual(model.navigation.next(references[0]), references[0]);
        assert.strictEqual(model.navigation.previous(references[0]), references[0]);
        assert.strictEqual(model.navigation.location(references[1]), undefined);
        assert.deepStrictEqual(model.highlights.getEditorHighlights(references[0], write.uri), [
            write.range,
        ]);
        assert.deepStrictEqual(
            model.highlights.getEditorHighlights(references[0], specRead.uri),
            [],
        );

        // Reads keep one result in each file; navigation and highlights follow them.
        input.setFilter('reference');
        assert.strictEqual(model.message, 'Reads only — 2 of 3 references in 2 files');
        assert.deepStrictEqual(model.provider.getChildren(), files);
        assert.deepStrictEqual(model.provider.getChildren(files[0]), [references[1]]);
        assert.deepStrictEqual(model.provider.getChildren(files[1]), [references[2]]);
        assert.strictEqual(model.navigation.next(references[1]), references[2]);
        assert.strictEqual(model.navigation.previous(references[1]), references[2]);
        assert.deepStrictEqual(model.highlights.getEditorHighlights(references[1], bodyRead.uri), [
            bodyRead.range,
        ]);
        assert.deepStrictEqual(model.highlights.getEditorHighlights(references[1], specRead.uri), [
            specRead.range,
        ]);

        // Clearing restores the original results without another request.
        await vscode.commands.executeCommand('ada.references.showAll');
        assert.strictEqual(model.message, '3 references in 2 files');
        assert.deepStrictEqual(getReferences(model), references);
        assert.strictEqual(resolutions.length, 1, 'Filtering must reuse the loaded results');
    });

    test('preserving filters through refresh and References history', async function () {
        await vscode.commands.executeCommand('ada.findReferences');
        await waitForResolutions(resolutions, 1);
        const input = view.getInput();
        assert(input instanceof ReferencesInput);
        input.setFilter('write');

        // Let the References view finish saving the search in its history.
        await vscode.commands.executeCommand('references-view.tree.focus');

        // Refresh requests new results while keeping the selected kind.
        await vscode.commands.executeCommand('references-view.refresh');
        const model = await waitForResolutions(resolutions, 2);
        const references = getReferences(model);
        const items = await Promise.all(references.map((ref) => model.provider.getTreeItem(ref)));
        assert.deepStrictEqual(
            items.map((item) => item.description),
            ['write'],
        );
        assert.strictEqual(model.message, 'Writes only — 1 of 3 references in 1 file');

        // Reopening a saved input at an updated position keeps its filter.
        const movedLocation = new vscode.Location(input.location.uri, new vscode.Position(2, 30));
        const movedInput = input.with(movedLocation);
        assert.strictEqual(movedInput.location.uri.toString(), movedLocation.uri.toString());
        assert(movedInput.location.range.isEqual(movedLocation.range));

        await vscode.commands.executeCommand('references-view.clear');
        view.setInput(movedInput);
        const reopenedModel = await waitForResolutions(resolutions, 3);
        assert.strictEqual(reopenedModel.message, 'Writes only — 1 of 3 references in 1 file');

        // A kind with no matches leaves an empty tree and explains how to clear it.
        const oldReference = getReferences(reopenedModel)[0];
        movedInput.setFilter('call');
        assert.deepStrictEqual(reopenedModel.provider.getChildren(), []);
        assert.match(reopenedModel.message, /0 of 3 references in 0 files/);
        assert.match(reopenedModel.message, /Show All References/);

        // Empty results disable navigation and highlights, including stale selections.
        assert.strictEqual(
            reopenedModel.navigation.nearest(input.location.uri, movedLocation.range.start),
            undefined,
        );
        assert.strictEqual(reopenedModel.navigation.next(oldReference), oldReference);
        assert.strictEqual(reopenedModel.navigation.previous(oldReference), oldReference);
        assert.strictEqual(reopenedModel.navigation.location(oldReference), undefined);
        assert.deepStrictEqual(
            reopenedModel.highlights.getEditorHighlights(oldReference, input.location.uri),
            [],
        );

        // The selected kind stays in the menu even when its count is zero.
        assert.deepStrictEqual(reopenedModel.provider.getFilterOptions(), [
            { label: 'All references', description: '3 references' },
            { referenceKind: 'write', label: 'Writes', description: '1 reference' },
            { referenceKind: 'reference', label: 'Reads', description: '2 references' },
            { referenceKind: 'call', label: 'Calls', description: '0 references (current)' },
        ]);

        // Clearing an empty filter also restores results without another request.
        await vscode.commands.executeCommand('ada.references.showAll');
        assert.strictEqual(reopenedModel.message, '3 references in 2 files');
        assert.strictEqual(resolutions.length, 3);

        // A new search starts unfiltered, regardless of the previous search's filter.
        movedInput.setFilter('write');
        await selectAdaEntityInEditor();
        await vscode.commands.executeCommand('ada.findReferences');
        const newModel = await waitForResolutions(resolutions, 4);
        assert.strictEqual(newModel.message, '3 references in 2 files');
    });
});

/**
 * Places the cursor on the Ada variable 'Counter' in the test source
 * for 'Find All References'.
 */
async function selectAdaEntityInEditor(): Promise<void> {
    const editor = await showTextDocument('src', 'reference_kinds.ads');
    const position = new vscode.Position(1, 4);
    editor.selection = new vscode.Selection(position, position);
}

/** Collects visible reference rows in tree order. */
function getReferences(model: ReferencesResult) {
    return model.provider.getChildren().flatMap((file) => model.provider.getChildren(file));
}

/** Waits up to ten seconds for the expected requests, then returns the latest results. */
async function waitForResolutions(
    resolutions: readonly ReturnType<ReferencesInput['resolve']>[],
    count: number,
): Promise<ReferencesResult> {
    const deadline = Date.now() + 10_000;
    while (resolutions.length < count && Date.now() < deadline) {
        await setTimeout(20);
    }
    assert.strictEqual(resolutions.length, count);
    const model = await resolutions[count - 1];
    assert(model);
    return model;
}
