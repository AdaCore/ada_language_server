/*----------------------------------------------------------------------------
--              Rendering of GNATcoverage external annotations              --
--                                                                          --
-- Displays the annotations returned by extAnnotations.ts in the editor:    --
--                                                                          --
--   * annotations that cover a region get a tinted background;             --
--   * annotations that designate a single point (Exempt_On, Exempt_Off,    --
--     Cov_On, ...) get an inline marker, since a one-character background  --
--     would be invisible;                                                  --
--   * annotations that gnatcov could not resolve against the current       --
--     source have no location at all, so they cannot be decorated. They    --
--     are reported in the Problems panel instead, otherwise they would be  --
--     silently invisible.                                                  --
----------------------------------------------------------------------------*/

import * as vscode from 'vscode';
import {
    AnnotationCategory,
    ExternalAnnotation,
    categoryOf,
    describeAnnotation,
    labelOf,
    showAnnotations,
    toVscodeRange,
} from './extAnnotations';
import { logger } from './extension';

/** Languages for which gnatcov can resolve annotations. */
const supportedLanguages = ['ada', 'c', 'cpp'];

/**
 * The pair of decoration types used for one annotation category.
 */
type CategoryDecorations = {
    /** Applied to annotations spanning a region of text. */
    region: vscode.TextEditorDecorationType;
    /** Applied to annotations designating a single point. */
    point: vscode.TextEditorDecorationType;
    /** Kept so that per-annotation labels can reuse the category's colors. */
    backgroundColorId: string;
    labelColorId: string;
};

/**
 * Create the decoration types for a category.
 *
 * The colors are contributed by the extension (see `contributes.colors` in
 * package.json) rather than hardcoded, so that they adapt to the active theme
 * and can be overridden by users.
 *
 * Two colors are needed per category: a translucent one to tint a region
 * without hiding the syntax highlighting underneath, and an opaque one for the
 * inline label, which would be illegible in the translucent shade.
 *
 * The labels themselves are set per annotation rather than on the type, since
 * only `before` and `after` can be overridden per decoration instance.
 */
function createCategoryDecorations(
    backgroundColorId: string,
    labelColorId: string,
): CategoryDecorations {
    const background = new vscode.ThemeColor(backgroundColorId);
    const label = new vscode.ThemeColor(labelColorId);

    return {
        backgroundColorId: backgroundColorId,
        labelColorId: labelColorId,
        region: vscode.window.createTextEditorDecorationType({
            backgroundColor: background,
            overviewRulerColor: label,
            overviewRulerLane: vscode.OverviewRulerLane.Right,
            rangeBehavior: vscode.DecorationRangeBehavior.ClosedClosed,
        }),
        point: vscode.window.createTextEditorDecorationType({
            overviewRulerColor: label,
            overviewRulerLane: vscode.OverviewRulerLane.Right,
            rangeBehavior: vscode.DecorationRangeBehavior.ClosedClosed,
        }),
    };
}

/**
 * Build the inline badge shown next to an annotation.
 *
 * `textDecoration` is used to smuggle in the padding and rounded corners that
 * the decoration API does not expose directly; this is the usual way to style
 * a decoration attachment.
 */
function buildLabel(
    annotation: ExternalAnnotation,
    backgroundColorId: string,
    labelColorId: string,
): vscode.ThemableDecorationAttachmentRenderOptions {
    return {
        contentText: labelOf(annotation.kind),
        color: new vscode.ThemeColor(labelColorId),
        backgroundColor: new vscode.ThemeColor(backgroundColorId),
        margin: '0 4px 0 0',
        fontStyle: 'italic',
        textDecoration: 'none; padding: 0 4px; border-radius: 3px; font-size: 0.85em',
    };
}

/**
 * Owns the decoration types, the annotation cache and the refresh logic.
 */
export class ExternalAnnotationDecorator implements vscode.Disposable {
    private readonly decorations: Map<AnnotationCategory, CategoryDecorations>;

    private readonly diagnostics =
        vscode.languages.createDiagnosticCollection('gnatcov-annotations');

    /**
     * Annotations per source file, as last reported by gnatcov. Entries are
     * dropped when the source file is saved or when the annotation files
     * change, since either may change the resolved locations.
     */
    private readonly cache = new Map<string, ExternalAnnotation[]>();

    /**
     * In-flight gnatcov invocations, keyed by source path. Editor events fire
     * in bursts, and each invocation spawns a process that loads the project,
     * so concurrent requests for the same file share one invocation.
     */
    private readonly pending = new Map<string, Promise<ExternalAnnotation[]>>();

    /**
     * Incremented by every invalidation. A gnatcov run started before an
     * invalidation resolved against the source as it was then, so its result
     * must neither populate the cache nor be decorated: it is discarded by
     * comparing the generation it started in with the current one.
     */
    private generation = 0;

    private readonly disposables: vscode.Disposable[] = [];

    constructor() {
        this.decorations = new Map([
            [
                AnnotationCategory.Exemption,
                createCategoryDecorations(
                    'ada.exemptedRegionBackground',
                    'ada.exemptedAnnotationLabel',
                ),
            ],
            [
                AnnotationCategory.CoverageSwitch,
                createCategoryDecorations(
                    'ada.coverageSwitchRegionBackground',
                    'ada.coverageSwitchAnnotationLabel',
                ),
            ],
            [
                AnnotationCategory.Buffers,
                createCategoryDecorations(
                    'ada.buffersRegionBackground',
                    'ada.buffersAnnotationLabel',
                ),
            ],
        ]);
    }

    /**
     * Register the event listeners driving the refresh, and decorate the
     * already-visible editors.
     */
    public activate(context: vscode.ExtensionContext): void {
        this.disposables.push(
            // Decorations only exist for visible editors, so refresh whenever
            // the set of visible editors changes.
            vscode.window.onDidChangeVisibleTextEditors(() => void this.refreshVisible()),

            /*
             * Saving a source file may move the code that annotations point
             * at, and gnatcov resolves stable slocs against the file on disk,
             * so the cached locations are stale after a save. We deliberately
             * do not refresh on every keystroke: each refresh spawns gnatcov,
             * so decorations drift slightly while typing and snap back on
             * save.
             */
            vscode.workspace.onDidSaveTextDocument((doc) => {
                this.invalidate(doc.uri.fsPath);
                void this.refreshVisible();
            }),

            /*
             * A project source with a nonstandard extension only becomes an Ada
             * document once the language override has been applied, which
             * happens after this decorator's first refresh and does not change
             * the set of visible editors. Reacting to the document opening
             * covers it, since changing a language re-opens the document.
             */
            vscode.workspace.onDidOpenTextDocument((doc) => {
                if (supportedLanguages.includes(doc.languageId)) {
                    void this.refreshVisible();
                }
            }),

            vscode.workspace.onDidCloseTextDocument((doc) => {
                this.cache.delete(doc.uri.fsPath);
                this.diagnostics.delete(doc.uri);
            }),

            vscode.workspace.onDidChangeConfiguration((e) => {
                if (
                    e.affectsConfiguration('ada.externalAnnotations') ||
                    e.affectsConfiguration('ada.scenarioVariables') ||
                    e.affectsConfiguration('ada.projectFile')
                ) {
                    void this.refresh();
                }
            }),
        );

        context.subscriptions.push(this);

        void this.refreshVisible();
    }

    private invalidateAll(): void {
        this.generation += 1;
        this.cache.clear();
        this.pending.clear();
    }

    /**
     * Drop what is known about one source file, including any run in flight for
     * it. Called when the file is saved, since gnatcov resolves stable slocs
     * against the file on disk and every earlier answer is now out of date.
     */
    private invalidate(sourcePath: string): void {
        this.generation += 1;
        this.cache.delete(sourcePath);
        this.pending.delete(sourcePath);
    }

    /**
     * Drop every decoration and diagnostic currently displayed.
     */
    private clearAll(): void {
        for (const editor of vscode.window.visibleTextEditors) {
            for (const decoration of this.decorations.values()) {
                editor.setDecorations(decoration.region, []);
                editor.setDecorations(decoration.point, []);
            }
        }
        this.diagnostics.clear();
    }

    /**
     * Force a re-query of gnatcov for all visible editors.
     */
    public async refresh(): Promise<void> {
        this.invalidateAll();
        await this.refreshVisible();
    }

    private isEnabled(): boolean {
        return (
            vscode.workspace
                .getConfiguration()
                .get<boolean>('ada.externalAnnotations.showInEditor') ?? true
        );
    }

    private async refreshVisible(): Promise<void> {
        if (!this.isEnabled()) {
            this.clearAll();
            return;
        }

        await Promise.all(
            vscode.window.visibleTextEditors.map((editor) => this.refreshEditor(editor)),
        );
    }

    private async refreshEditor(editor: vscode.TextEditor): Promise<void> {
        const doc = editor.document;

        if (doc.uri.scheme !== 'file' || !supportedLanguages.includes(doc.languageId)) {
            return;
        }

        const startedAt = this.generation;
        const annotations = await this.getAnnotations(doc.uri.fsPath);

        /*
         * The editor may have been closed, or scrolled to a different
         * document, while gnatcov was running.
         */
        if (!vscode.window.visibleTextEditors.includes(editor)) {
            return;
        }

        /*
         * Something invalidated these annotations while gnatcov was running, so
         * a newer refresh is on its way. Decorating now would show locations
         * resolved against a previous state of the source.
         */
        if (startedAt !== this.generation) {
            return;
        }

        this.applyDecorations(editor, annotations);
        this.applyDiagnostics(doc, annotations);
    }

    private async getAnnotations(sourcePath: string): Promise<ExternalAnnotation[]> {
        const cached = this.cache.get(sourcePath);
        if (cached !== undefined) {
            return cached;
        }

        const inFlight = this.pending.get(sourcePath);
        if (inFlight !== undefined) {
            return inFlight;
        }

        const startedAt = this.generation;

        const promise = showAnnotations(sourcePath)
            .then((result) => {
                if (result.error !== undefined) {
                    /*
                     * Failures here are expected in normal use (no annotation
                     * file configured yet, gnatcov not installed, project not
                     * loaded), so they are logged rather than shown as a
                     * popup.
                     */
                    logger.debug(
                        `External annotations unavailable for ${sourcePath}: ${result.error}`,
                    );
                    return [];
                }

                //  Superseded while gnatcov was running: the locations it
                //  resolved no longer describe the current source.
                if (startedAt !== this.generation) {
                    return [];
                }

                this.cache.set(sourcePath, result.annotations);
                return result.annotations;
            })
            .finally(() => {
                /*
                 * Only retract this run's own entry. An invalidation has already
                 * cleared the map, and a newer run for the same path may be
                 * registered by now.
                 */
                if (this.pending.get(sourcePath) === promise) {
                    this.pending.delete(sourcePath);
                }
            });

        this.pending.set(sourcePath, promise);
        return promise;
    }

    private applyDecorations(editor: vscode.TextEditor, annotations: ExternalAnnotation[]): void {
        // Start from empty lists for every type, so that types with no
        // annotation left get cleared.
        const options = new Map<vscode.TextEditorDecorationType, vscode.DecorationOptions[]>();
        for (const decoration of this.decorations.values()) {
            options.set(decoration.region, []);
            options.set(decoration.point, []);
        }

        for (const annotation of annotations) {
            if (annotation.range === undefined) {
                //  A stale annotation has no location to decorate. It is
                //  reported in the Problems panel instead.
                continue;
            }

            const decoration = this.decorations.get(categoryOf(annotation.kind));
            if (decoration === undefined) {
                continue;
            }

            /*
             * gnatcov reports a point annotation as a location whose start
             * and end coincide. toVscodeRange turns that into a one-character
             * range, which must be kept as is: a zero-width range would render
             * the label but never trigger the hover, since VS Code only matches
             * hovers against non-empty decoration ranges.
             */
            const range = toVscodeRange(annotation.range);
            const isPoint =
                annotation.range.startLine === annotation.range.endLine &&
                annotation.range.startColumn === annotation.range.endColumn;

            const target = isPoint ? decoration.point : decoration.region;
            const label = buildLabel(
                annotation,
                decoration.backgroundColorId,
                decoration.labelColorId,
            );

            /*
             * The badge always marks the location gnatcov reported.
             *
             * For the buffer kinds that is the statement designated, not where
             * the pragma ends up. The hover states which side of it applies.
             * Moving the badge instead would put it at a position gnatcov
             * never reported.
             */
            options.get(target)?.push({
                range: range,
                hoverMessage: buildHover(annotation),
                renderOptions: { before: label },
            });
        }

        for (const [type, opts] of options) {
            editor.setDecorations(type, opts);
        }
    }

    /**
     * Report stale annotations in the Problems panel.
     *
     * A stale annotation has no resolved location, so there is nothing to
     * decorate. Without this, an annotation that stopped matching its source
     * would simply disappear from the IDE, which is the most misleading
     * outcome possible: the user would believe the exemption is still in
     * effect.
     */
    private applyDiagnostics(doc: vscode.TextDocument, annotations: ExternalAnnotation[]): void {
        const diags: vscode.Diagnostic[] = [];

        for (const annotation of annotations) {
            if (!annotation.stale) {
                continue;
            }

            const diag = new vscode.Diagnostic(
                // There is no location to point at; flag the top of the file.
                new vscode.Range(0, 0, 0, 0),
                `Stale external annotation '${annotation.id}' (${annotation.kind}): ` +
                    (annotation.diagnostic ?? 'could not be resolved against the current source'),
                vscode.DiagnosticSeverity.Warning,
            );
            diag.source = 'gnatcov';
            diags.push(diag);
        }

        if (diags.length > 0) {
            this.diagnostics.set(doc.uri, diags);
        } else {
            this.diagnostics.delete(doc.uri);
        }
    }

    public dispose(): void {
        for (const decoration of this.decorations.values()) {
            decoration.region.dispose();
            decoration.point.dispose();
        }
        this.diagnostics.dispose();
        for (const d of this.disposables) {
            d.dispose();
        }
    }
}

/**
 * Build the hover shown when the mouse is over an annotated region.
 */
function buildHover(annotation: ExternalAnnotation): vscode.MarkdownString {
    const md = new vscode.MarkdownString();

    /*
     * Lead with the kind, spelled exactly as gnatcov spells it, so that the
     * hover doubles as the value to pass to `add-annotation --kind`.
     */
    md.appendMarkdown(`**${annotation.kind}** — GNATcoverage external annotation\n\n`);

    const details = describeAnnotation(annotation);
    if (details.length > 0) {
        md.appendMarkdown(`${details}\n\n`);
    }

    if (annotation.justification !== undefined) {
        md.appendMarkdown(`${annotation.justification}\n\n`);
    }

    md.appendMarkdown(`\`id: ${annotation.id}\``);
    return md;
}
