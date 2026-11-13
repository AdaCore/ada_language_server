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

/** Reference kinds reported by ALS. */
type ReferenceKind = NonNullable<ALSReferenceLocation['alsKind']>[number];

/** Labels used in the filter menu and in the message above the results. */
const REFERENCE_KIND_LABELS: Record<ReferenceKind, string> = {
    write: 'Writes',
    reference: 'Reads',
    call: 'Calls',
    'dispatching call': 'Dispatching calls',
    access: 'Accesses',
    parent: 'Parent references',
    child: 'Child references',
    overriding: 'Overriding declarations',
};

/** A filter menu item. An absent referenceKind selects all results. */
interface ReferenceKindOption extends vscode.QuickPickItem {
    referenceKind?: ReferenceKind;
}

/**
 * The part of the References extension's API that this file uses.
 * Activating vscode.references-view returns an object with these methods.
 * This interface describes that object; it does not create another view.
 *
 * @see https://github.com/microsoft/vscode/blob/main/extensions/references-view/src/references-view.d.ts
 */
interface ReferencesView {
    /** Replaces the view's current search with the search described by input. */
    setInput(input: ReferencesInput): void;
    /** Returns the search currently displayed. Another extension may have supplied it. */
    getInput(): unknown;
}

/** Gets the API of VS Code's References extension, activating it if needed. */
async function getReferencesView(): Promise<ReferencesView | undefined> {
    return vscode.extensions.getExtension<ReferencesView>('vscode.references-view')?.activate();
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

    // Programmatic calls bypass UI conditions, so require an active Ada text editor.
    if (!editor || editor.document.languageId !== 'ada') {
        return;
    }

    // Save the cursor position before opening the References view, which can change focus.
    const location = new vscode.Location(editor.document.uri, editor.selection.active);

    // Get the extension that VS Code includes for the References view, then activate
    // it to obtain its API. Activating it does not install a separate extension.
    const view = await getReferencesView();

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

/**
 * Handles the Filter by Reference Kind toolbar button.
 * Only our Ada searches have the data and methods needed to filter by ALS kind.
 * Other extensions can also use the References view, so check its current input.
 */
export async function filterReferences(): Promise<void> {
    const view = await getReferencesView();
    const input = view?.getInput?.();
    if (view && input instanceof ReferencesInput) {
        await input.selectKind(view);
    }
}

/**
 * Handles the filled funnel button shown when a filter is active.
 * Clicking it shows every result again, without sending another request to ALS.
 */
export async function showAllReferences(): Promise<void> {
    const view = await getReferencesView();
    const input = view?.getInput?.();
    if (input instanceof ReferencesInput) {
        input.setFilter(undefined);
    }
}

// ---------------------------------------------------------------------------
// Search description used by the References extension
// ---------------------------------------------------------------------------

/**
 * Describes a reference search and remembers its selected kind.
 *
 * The References extension expects an input object with title, contextValue,
 * location, resolve, and with. These names are defined by that extension's API.
 *
 * The extension saves this object in its search history. It calls resolve again
 * when the user refreshes or reruns the search, so results are requested from ALS
 * each time. It also tracks the search position when the user edits the source.
 * The selected kind belongs to this search too. Refresh and history keep it,
 * while a new Find All References command creates an input with no filter.
 */
export class ReferencesInput {
    /** Heading displayed above the results in the References view. */
    readonly title = 'References';

    /**
     * Value assigned to reference-list.source by the References extension.
     * VS Code can use this value in menu conditions to identify our Ada searches.
     */
    readonly contextValue = 'ada.references';

    /** Most recent results for this search. Undefined while ALS is loading them. */
    private model?: ReferencesModel;

    /**
     * @param client - The language client used to ask ALS for references.
     * @param location - The file and cursor position that identify the symbol to
     * search for. This is the search position, not the position of a result.
     * @param kind - The selected kind, copied when history updates the search position.
     * Undefined shows all results.
     */
    constructor(
        private readonly client: AdaLanguageClient,
        readonly location: vscode.Location,
        private kind?: ReferenceKind,
    ) {}

    /**
     * Called by the References extension to load or reload this search.
     * Sends 'textDocument/references' to ALS and creates a model from the response.
     *
     * The returned object tells the extension which methods to use for the rows,
     * navigation, and source highlights.
     * Returning 'undefined' tells the extension to clear the results and show
     * its usual empty-results message and history. A filter with no matches still
     * returns a model, so the user can clear the filter without losing the search.
     */
    async resolve() {
        // Do not let a toolbar action change old results while Refresh is loading
        // new ones. Also restore the toolbar state when reopening a search from history.
        this.model = undefined;
        void vscode.commands.executeCommand('setContext', 'ada.referencesFiltered', !!this.kind);
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

            const model = new ReferencesModel(this.client, locations, this.kind);
            this.model = model;
            return {
                // getChildren, getParent, and getTreeItem describe the rows to display.
                provider: model,

                // The References extension reads this again when the provider fires
                // a change event. A getter keeps counts and the filter label current.
                get message() {
                    return model.message;
                },

                // nearest, next, previous, and location support result navigation.
                navigation: model,

                // getEditorHighlights supplies the ranges to mark in source editors.
                highlights: model,

                // The References extension calls this when replacing or clearing results.
                dispose: () => model.dispose(),
            };
        } catch (error) {
            // The exported setInput method does not return our request's promise.
            // Report failures here because findReferences cannot catch them later.
            void vscode.window.showErrorMessage(`Could not find Ada references: ${String(error)}`);
            return undefined;
        }
    }

    /**
     * Opens the toolbar's filter menu for the results currently displayed.
     * Counts include all results, so choosing a different kind is always possible.
     * Closing the menu without making a choice leaves the filter unchanged.
     */
    async selectKind(view: ReferencesView): Promise<void> {
        const model = this.model;
        if (!model) {
            return;
        }

        const selected = await vscode.window.showQuickPick(model.getFilterOptions(), {
            placeHolder: 'Show references of kind…',
        });

        // A user can start another search or refresh results while the menu is open.
        // Apply the choice only if it still belongs to this input and these results.
        if (selected && view.getInput() === this && this.model === model) {
            this.setFilter(selected.referenceKind);
        }
    }

    /**
     * Preserves the selected kind for refresh and history, updates the toolbar
     * context, and filters loaded results.
     */
    setFilter(kind: ReferenceKind | undefined): void {
        this.kind = kind;
        void vscode.commands.executeCommand('setContext', 'ada.referencesFiltered', !!kind);
        this.model?.setFilter(kind);
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
        return new ReferencesInput(this.client, location, this.kind);
    }
}

// ---------------------------------------------------------------------------
// Files and references displayed as rows in the References view
// ---------------------------------------------------------------------------

/**
 * Represents an expandable file row in the References view.
 * The provider stores its matching children separately in referencesByFile,
 * so changing a filter does not require replacing this file object.
 */
class ReferenceFile {
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
 * Visible references follow the tree order and support Next and Previous
 * (F4 and Shift+F4) across files.
 *
 * All ALS results are kept so that filters can change without another request.
 * Only matching references are used for rows, navigation, and source highlights.
 * The filtered list can be empty even when ALS returned results.
 */
class ReferencesModel implements vscode.TreeDataProvider<ReferenceNode> {
    /** All results from ALS, including results hidden by the selected filter. */
    private readonly allReferences: ReferenceItem[];

    /** Matching reference rows in display order, used by Next and Previous. */
    private visibleReferences: ReferenceItem[] = [];

    /** File rows with at least one match, and only the children to display below them. */
    private readonly referencesByFile = new Map<ReferenceFile, ReferenceItem[]>();

    /**
     * Tell VS Code when filtering changes the rows. The References extension also
     * listens to this event to update its message and the source editor highlights.
     */
    private readonly changed = new vscode.EventEmitter<ReferenceNode | undefined>();
    readonly onDidChangeTreeData = this.changed.event;

    constructor(
        client: AdaLanguageClient,
        locations: ALSReferenceLocation[],
        private kind?: ReferenceKind,
    ) {
        const files = new Map<string, ReferenceFile>();

        // Convert locations to the types expected by VS Code, keeping ALS kinds in
        // a separate field. Sort by file, start position, then end position before
        // grouping results, so the row order and navigation order agree.
        this.allReferences = locations
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
                return new ReferenceItem(location, kinds, file);
            });
        this.setFilter(kind);
    }

    /** Only files with visible matches are returned to the References extension. */
    get files(): ReferenceFile[] {
        return [...this.referencesByFile.keys()];
    }

    /** Every navigation method uses this same filtered list. */
    get references(): ReferenceItem[] {
        return this.visibleReferences;
    }

    /**
     * Text above the results. When filtered, show the selected kind and the number
     * of matches out of the full result count, so hidden results are easy to notice.
     */
    get message(): string {
        const count = this.references.length;
        const fileCount = this.referencesByFile.size;
        const files = `${fileCount} ${fileCount === 1 ? 'file' : 'files'}`;
        if (!this.kind) {
            return `${count} ${count === 1 ? 'reference' : 'references'} in ${files}`;
        }

        const message =
            `${REFERENCE_KIND_LABELS[this.kind]} only — ` +
            `${count} of ${this.allReferences.length} references in ${files}`;
        return count ? message : `${message}. Use Show All References to clear the filter.`;
    }

    /**
     * Build choices from the original results, not the filtered list. Show kinds
     * that occur in the results, plus the selected kind even if Refresh removed
     * its last match. A reference with several kinds is counted once for each kind.
     */
    getFilterOptions(): ReferenceKindOption[] {
        const total = this.allReferences.length;
        const options: ReferenceKindOption[] = [
            {
                label: 'All references',
                description:
                    `${total} ${total === 1 ? 'reference' : 'references'}` +
                    (this.kind ? '' : ' (current)'),
            },
        ];
        for (const kind of Object.keys(REFERENCE_KIND_LABELS) as ReferenceKind[]) {
            const count = this.allReferences.filter((reference) =>
                reference.kinds?.includes(kind),
            ).length;
            if (count || kind === this.kind) {
                options.push({
                    referenceKind: kind,
                    label: REFERENCE_KIND_LABELS[kind],
                    description:
                        `${count} ${count === 1 ? 'reference' : 'references'}` +
                        (kind === this.kind ? ' (current)' : ''),
                });
            }
        }
        return options;
    }

    /**
     * Change which results are visible without changing the original ALS results.
     * File and reference objects are reused, so existing selections still identify
     * the same source locations. Files without matches are left out of the tree.
     */
    setFilter(kind: ReferenceKind | undefined): void {
        this.kind = kind;
        this.visibleReferences = this.allReferences.filter(
            (reference) => !kind || reference.kinds?.includes(kind),
        );
        this.referencesByFile.clear();
        for (const reference of this.references) {
            let references = this.referencesByFile.get(reference.file);
            if (!references) {
                references = [];
                this.referencesByFile.set(reference.file, references);
            }
            references.push(reference);
        }
        // Undefined asks VS Code to reload the whole tree, since filtering can
        // remove complete file rows as well as individual references.
        this.changed.fire(undefined);
    }

    /** Disposes the change event emitter when the References extension releases these results. */
    dispose(): void {
        this.changed.dispose();
    }

    /**
     * Called by VS Code to find which rows to display below element.
     * With no element, return the file rows at the top of the tree. For a file,
     * return its references. A reference row has no children.
     */
    getChildren(element?: ReferenceNode): ReferenceNode[] {
        if (!element) {
            return this.files;
        }
        return element instanceof ReferenceFile ? (this.referencesByFile.get(element) ?? []) : [];
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
            // Keep the same row identity when filtering hides other files with the same name.
            item.id = JSON.stringify(['file', element.uri.toString()]);
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
        // Several references can have the same preview text, even on the same line.
        // A source-based ID keeps selection tied to the correct reference when
        // filtering removes or restores other rows with that same text.
        item.id = JSON.stringify(['reference', uri.toString(), range.start, range.end]);

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
     * Use only visible matches. A previously selected row may have been hidden
     * by a filter; returning undefined prevents navigation from opening that row.
     */
    location(item: ReferenceNode): vscode.Location | undefined {
        if (item instanceof ReferenceFile) {
            return this.referencesByFile.get(item)?.[0]?.location;
        }
        return this.references.includes(item) ? item.location : undefined;
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
        const references = file ? this.referencesByFile.get(file) : undefined;
        return (
            references?.find((item) => item.location.range.contains(position)) ??
            references?.find((item) => item.location.range.start.isAfterOrEqual(position)) ??
            references?.[references.length - 1] ??
            this.references[0]
        );
    }

    /**
     * Called for Next (F4). Return the reference after the current one, or the
     * first result if the current one is last. If a file row is selected, treat
     * its first visible reference as the current one. If filtering hid the selected
     * row, start at the first visible result. With no matches, leave the row alone;
     * location returns undefined so the References extension will not open it.
     */
    next(item: ReferenceNode): ReferenceNode {
        if (!this.references.length) {
            return item;
        }
        const reference =
            item instanceof ReferenceItem ? item : this.referencesByFile.get(item)?.[0];
        const index = reference ? this.references.indexOf(reference) : -1;
        return this.references[(index + 1) % this.references.length] ?? item;
    }

    /**
     * Called for Previous (Shift+F4). Return the reference before the current one,
     * or the last result if the current one is first. If a file row is selected,
     * treat its first visible reference as the current one. If filtering hid the
     * selected row, start at the last visible result. With no matches, keep the
     * row and let location prevent opening it.
     */
    previous(item: ReferenceNode): ReferenceNode {
        const reference =
            item instanceof ReferenceItem ? item : this.referencesByFile.get(item)?.[0];
        const index = reference ? this.references.indexOf(reference) : -1;
        if (index < 0) {
            return this.references[this.references.length - 1] ?? item;
        }
        return (
            this.references[(index + this.references.length - 1) % this.references.length] ?? item
        );
    }

    /**
     * Called by the References extension to mark results in a source editor.
     * Return the visible reference ranges for uri, regardless of which result is selected.
     * This is why _item, the selected row, is not used here.
     *
     * This part of the extension's API takes only ranges, with no kind information.
     * The editor highlights therefore do not distinguish reads from writes; those
     * labels are provided separately by getTreeItem in the References view.
     */
    getEditorHighlights(_item: ReferenceNode, uri: vscode.Uri): vscode.Range[] | undefined {
        const file = this.files.find((item) => item.uri.toString() === uri.toString());
        return file
            ? this.referencesByFile.get(file)?.map((reference) => reference.location.range)
            : [];
    }
}
