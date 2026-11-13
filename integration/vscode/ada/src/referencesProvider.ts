/**
 * Shows reference kinds from ALS in VS Code's existing References view.
 *
 * VS Code includes an extension named vscode.references-view. It provides the
 * References view, its search history, and commands such as Refresh and Next.
 * Users do not need to install that extension separately.
 *
 * This file provides the data shown in that view:
 * 1. findReferences gives the References extension a ReferencesInput object.
 * 2. The extension calls ReferencesInput.resolve to ask ALS for references.
 * 3. resolve returns a ReferencesModel. The extension uses its methods to display
 *    rows, open results, and highlight references in the source editor.
 *
 * We provide our own rows because the standard reference provider returns only
 * locations. It has no way to add read or write labels to the References view.
 */
import * as vscode from 'vscode';
import { ReferencesRequest } from 'vscode-languageclient/node';
import { ALSReferenceLocation } from './alsProtocolExtensions';
import { AdaLanguageClient } from './clients';

/**
 * The part of the References extension's API that this file uses.
 * Activating vscode.references-view returns an object with this method.
 * This interface describes that object; it does not create another view.
 *
 * @see https://github.com/microsoft/vscode/blob/main/extensions/references-view/src/references-view.d.ts
 */
interface ReferencesView {
    /** Replaces the view's current search with the search described by input. */
    setInput(input: ReferencesInput): void;
}

/**
 * Handles "Ada: Find All References" command (Shift+F12 by default).
 * Starts a search for the symbol at the cursor in the active Ada editor.
 *
 * This method gives the search to the References extension, which displays it in
 * the References view.
 * The extension then calls `resolve` to get results; this method
 * does not wait for those results.
 *
 * @param client - The language client used to send requests to ALS.
 */
export async function findReferences(client: AdaLanguageClient): Promise<void> {
    const editor = vscode.window.activeTextEditor;
    if (!editor || editor.document.languageId !== 'ada') {
        return;
    }

    // Save the cursor position before opening the References view, which can change focus.
    const location = new vscode.Location(editor.document.uri, editor.selection.active);

    // Get the extension that VS Code includes for the References view, then activate
    // it to obtain its API. Activating it does not install a separate extension.
    const extension = vscode.extensions.getExtension<ReferencesView>('vscode.references-view');
    const view = await extension?.activate();

    if (!view || typeof view.setInput !== 'function') {
        // Keep the usual reference search available if this installation of VS Code
        // does not provide the API needed for our labels.
        await vscode.commands.executeCommand('editor.action.goToReferences');
        return;
    }

    // Supply our search to the existing view. The References extension will call
    // the input's resolve method when it is ready to load the results.
    view.setInput(new ReferencesInput(client, location));
}

// ---------------------------------------------------------------------------
// Search description used by the References extension
// ---------------------------------------------------------------------------

/**
 * Describes what to search for, without storing a list of results.
 *
 * The References extension expects an input object with title, contextValue,
 * location, resolve, and with. These names are defined by that extension's API.
 *
 * The extension saves this object in its search history. It calls resolve again
 * when the user refreshes or reruns the search, so results are requested from ALS
 * each time. It also tracks the search position when the user edits the source.
 */
export class ReferencesInput {
    /** Heading displayed above the results in the References view. */
    readonly title = 'References';

    /**
     * Value assigned to reference-list.source by the References extension.
     * VS Code can use this value in menu conditions to identify our Ada searches.
     */
    readonly contextValue = 'ada.references';

    /**
     * @param client - The language client used to ask ALS for references.
     * @param location - The file and cursor position that identify the symbol to
     * search for. This is the search position, not the position of a result.
     */
    constructor(
        private readonly client: AdaLanguageClient,
        readonly location: vscode.Location,
    ) {}

    /**
     * Called by the References extension to load or reload this search.
     * Sends 'textDocument/references' to ALS and creates a model from the response.
     *
     * The returned object tells the extension which methods to use for the rows,
     * navigation, and source highlights.
     * Returning 'undefined' tells the extension to clear the results and show
     *  ts usual empty-results message and history.
     */
    async resolve() {
        try {
            // Ask ALS directly so we can keep 'alsKind', its extra field containing
            // kinds such as write and call. The language client's standard reference
            // provider removes this field when it converts results to VS Code locations.
            const locations: ALSReferenceLocation[] | null = await this.client.sendRequest(
                ReferencesRequest.type,
                {
                    textDocument: {
                        uri: this.client.code2ProtocolConverter.asUri(this.location.uri),
                    },
                    position: this.client.code2ProtocolConverter.asPosition(
                        this.location.range.start,
                    ),
                    // Show uses of the symbol, without adding its declaration as a result.
                    context: { includeDeclaration: false },
                },
            );

            // If ALS returns no results, the References extension will show
            // its usual empty-results message
            if (!locations?.length) {
                return undefined;
            }

            const model = new ReferencesModel(this.client, locations);
            return {
                // getChildren, getParent, and getTreeItem describe the rows to display.
                provider: model,

                // The References extension displays this text above the result rows.
                message: `${model.references.length} references in ${model.files.length} files`,

                // nearest, next, previous, and location support result navigation.
                navigation: model,

                // getEditorHighlights supplies the ranges to mark in source editors.
                highlights: model,
            };
        } catch (error) {
            // The exported setInput method does not return our request's promise.
            // Report failures here because findReferences cannot catch them later.
            void vscode.window.showErrorMessage(`Could not find Ada references: ${String(error)}`);
            return undefined;
        }
    }

    /**
     * Called by the References extension when a search from history has moved.
     * For example, inserting a line above the symbol changes its search position.
     *
     * Create a new input at the updated position. The client stays the same, and
     * resolve will request new results when the extension reruns the search.
     *
     * @param location - The search position updated by the References extension.
     */
    with(location: vscode.Location): ReferencesInput {
        return new ReferencesInput(this.client, location);
    }
}

// ---------------------------------------------------------------------------
// Files and references displayed as rows in the References view
// ---------------------------------------------------------------------------

/**
 * Represents an expandable file row in the References view.
 * Its children are references in that file, ordered by their position in the source.
 */
class ReferenceFile {
    readonly references: ReferenceItem[] = [];

    constructor(readonly uri: vscode.Uri) {}
}

/**
 * Represents one reference row below a file row.
 * location identifies the source text to open, kinds supplies the read/write or
 * other labels, and file links back to the parent row.
 */
class ReferenceItem {
    constructor(
        readonly location: vscode.Location,
        readonly kinds: ALSReferenceLocation['alsKind'],
        readonly file: ReferenceFile,
    ) {}
}

/** Every row in our results represents either a file or one reference in that file. */
type ReferenceNode = ReferenceFile | ReferenceItem;

/**
 * Holds the results of one completed ALS request.
 * It implements the methods the References extension calls to display and browse results.
 *
 * The view has two levels: file rows, then reference rows below each file.
 * A separate list of all references has the same order as those rows.
 * The Next and Previous commands (F4 and Shift+F4) use that list to
 * move between files too.
 *
 * A model is created only when ALS returns at least one result. Each file row
 * therefore has at least one reference, and the navigation list is never empty.
 */
class ReferencesModel implements vscode.TreeDataProvider<ReferenceNode> {
    /** File rows in display order. Each file holds its own reference rows. */
    readonly files: ReferenceFile[];

    /** All reference rows in display order, used by Next and Previous. */
    readonly references: ReferenceItem[];

    constructor(client: AdaLanguageClient, locations: ALSReferenceLocation[]) {
        const files = new Map<string, ReferenceFile>();

        // Convert locations to the types expected by VS Code, keeping ALS kinds in
        // a separate field. Sort by file, start position, then end position before
        // grouping results, so the row order and navigation order agree.
        this.references = locations
            .map((reference) => ({
                location: client.protocol2CodeConverter.asLocation(reference),
                kinds: reference.alsKind,
            }))
            .sort(
                (a, b) =>
                    a.location.uri.toString().localeCompare(b.location.uri.toString()) ||
                    a.location.range.start.compareTo(b.location.range.start) ||
                    a.location.range.end.compareTo(b.location.range.end),
            )
            .map(({ location, kinds }) => {
                // Share one parent row between all references with the same file URI.
                const uri = location.uri.toString();
                let file = files.get(uri);
                if (!file) {
                    file = new ReferenceFile(location.uri);
                    files.set(uri, file);
                }
                const item = new ReferenceItem(location, kinds, file);
                file.references.push(item);
                return item;
            });
        this.files = [...files.values()];
    }

    /**
     * Called by VS Code to find which rows to display below element.
     * With no element, return the file rows at the top of the tree. For a file,
     * return its references. A reference row has no children.
     */
    getChildren(element?: ReferenceNode): ReferenceNode[] {
        return element instanceof ReferenceItem ? [] : (element?.references ?? this.files);
    }

    /**
     * Returns the parent of a row. VS Code uses it to expand the containing file
     * when the References extension asks to select or show a reference.
     * File rows have no parent in this tree.
     */
    getParent(element: ReferenceNode): ReferenceFile | undefined {
        return element instanceof ReferenceItem ? element.file : undefined;
    }

    /**
     * Called by VS Code to get a row's text, description, and action when clicked.
     * A file row shows the file name and path. A reference row shows a source
     * line, with its kinds (such as read or write) in the description beside it.
     */
    async getTreeItem(element: ReferenceNode): Promise<vscode.TreeItem> {
        if (element instanceof ReferenceFile) {
            // Let VS Code choose the file name and icon. description = true asks
            // it to display the file's path beside the name.
            const item = new vscode.TreeItem(
                element.uri,
                vscode.TreeItemCollapsibleState.Collapsed,
            );
            item.description = true;
            item.iconPath = vscode.ThemeIcon.File;
            return item;
        }

        const { uri, range } = element.location;
        const document = await vscode.workspace.openTextDocument(uri);

        // Build a preview from the first line covered by the reference. If the
        // reference spans several lines, stop the preview at the end of this line.
        // VS Code columns and JavaScript string positions both count UTF-16 code
        // units, so these columns can be used directly with slice. The end is excluded.
        const line = document.lineAt(range.start.line).text;
        const start = range.start.character;
        const end = range.isSingleLine ? range.end.character : line.length;

        // Remove indentation from the preview only. Keep the original source range
        // unchanged so that clicking the result opens the correct source text.
        const before = line.slice(0, start).trimStart();
        const inside = line.slice(start, end);
        const label = before + inside + line.slice(end).trimEnd();
        const item = new vscode.TreeItem({
            label,
            // These positions refer to the preview text, which has no indentation.
            // They make VS Code emphasize the referenced symbol within the row.
            highlights: [[before.length, before.length + inside.length]],
        });

        // ALS uses the kind "reference" when no more specific kind applies. Display
        // it as "read", following ALS's existing documentHighlight behavior.
        // Keep other kinds such as write, access, and call, including combinations.
        // If ALS sends no kinds, leave the row description empty.
        item.description = element.kinds
            ?.map((kind) => (kind === 'reference' ? 'read' : kind))
            .join(', ');
        const path = vscode.workspace.asRelativePath(uri);

        // Show a readable source position on hover. The stored source range uses
        // zero-based lines and columns; the tooltip shows them starting at one.
        item.tooltip =
            `${path}:${range.start.line + 1}:${range.start.character + 1}\n` +
            `${item.description || 'reference'}\n${label}`;
        // Clicking a reference opens its file and selects the full source range.
        item.command = {
            command: 'vscode.open',
            title: 'Open Reference',
            arguments: [uri, { selection: range } satisfies vscode.TextDocumentShowOptions],
        };
        return item;
    }

    /**
     * Called by the References extension to get the source location for a row.
     * A reference has its own location. For a file row, use its first reference.
     */
    location(item: ReferenceNode): vscode.Location {
        return item instanceof ReferenceItem ? item.location : item.references[0].location;
    }

    /**
     * Finds a result near the given source position for the References extension.
     * For example, the extension calls this with the search position to choose
     * the selected result when opening the References view.
     *
     * Use the reference at the cursor, or the next one in the same file. If there
     * is no next reference, use the last one in that file. If the file has no
     * references, use the first result of the search.
     */
    nearest(uri: vscode.Uri, position: vscode.Position): ReferenceItem | undefined {
        const file = this.files.find((item) => item.uri.toString() === uri.toString());
        return (
            file?.references.find((item) => item.location.range.contains(position)) ??
            file?.references.find((item) => item.location.range.start.isAfterOrEqual(position)) ??
            file?.references[file.references.length - 1] ??
            this.references[0]
        );
    }

    /**
     * Called for Next (F4). Return the reference after the current one, or the
     * first result if the current one is last. If a file row is selected, treat
     * its first reference as the current one.
     */
    next(item: ReferenceNode): ReferenceItem {
        const reference = item instanceof ReferenceItem ? item : item.references[0];
        return this.references[(this.references.indexOf(reference) + 1) % this.references.length];
    }

    /**
     * Called for Previous (Shift+F4). Return the reference before the current one,
     * or the last result if the current one is first. If a file row is selected,
     * treat its first reference as the current one.
     */
    previous(item: ReferenceNode): ReferenceItem {
        const reference = item instanceof ReferenceItem ? item : item.references[0];
        return this.references[
            (this.references.indexOf(reference) + this.references.length - 1) %
                this.references.length
        ];
    }

    /**
     * Called by the References extension to mark results in a source editor.
     * Return all reference ranges for uri, regardless of which result is selected.
     * This is why _item, the selected row, is not used here.
     *
     * This part of the extension's API takes only ranges, with no kind information.
     * The editor highlights therefore do not distinguish reads from writes; those
     * labels are provided separately by getTreeItem in the References view.
     */
    getEditorHighlights(_item: ReferenceNode, uri: vscode.Uri): vscode.Range[] | undefined {
        return this.files
            .find((file) => file.uri.toString() === uri.toString())
            ?.references.map((reference) => reference.location.range);
    }
}
