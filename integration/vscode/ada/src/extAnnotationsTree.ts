/*----------------------------------------------------------------------------
--            Tree view of the GNATcoverage external annotations            --
--                                                                          --
-- Lists the annotations of the whole project, grouped by file. This exists --
-- for two reasons the editor decorations cannot cover:                     --
--                                                                          --
--   * decorations only show annotations in files that happen to be open,   --
--     whereas stable slocs are scattered across the whole project;         --
--   * deleting an annotation is keyed by an identifier that has no         --
--     presence in the source text, so it needs something concrete to act   --
--     on.                                                                  --
----------------------------------------------------------------------------*/

import * as path from 'path';
import * as vscode from 'vscode';
import {
    ExternalAnnotation,
    ShowAnnotationsResult,
    describeAnnotation,
    labelOf,
    showAnnotations,
    toVscodeRange,
} from './extAnnotations';
import { logger } from './extension';

/*
 * The tree elements are TreeItem subclasses carrying their own model, as
 * ProjectViewItem does. A command contributed to view/item/context is invoked
 * with the element, so keeping the model on it is what lets the delete command
 * know which annotation to remove, whatever the element is rendered into.
 */

/** A file grouping several annotations. */
export class FileNode extends vscode.TreeItem {
    public constructor(
        /** Absolute path, as reported by gnatcov. */
        public readonly file: string,
        public readonly annotations: ExternalAnnotation[],
    ) {
        super(path.basename(file), vscode.TreeItemCollapsibleState.Expanded);

        this.resourceUri = vscode.Uri.file(file);
        this.description = `${String(annotations.length)} annotation${
            annotations.length === 1 ? '' : 's'
        }`;
        this.iconPath = vscode.ThemeIcon.File;
    }
}

/** A single annotation. */
export class AnnotationNode extends vscode.TreeItem {
    public constructor(public readonly annotation: ExternalAnnotation) {
        super(labelOf(annotation.kind));

        this.tooltip = buildTooltip(annotation);

        // Drives the inline delete action contributed in package.json.
        this.contextValue = 'gnatcovAnnotation';

        if (annotation.stale) {
            /*
             * A stale annotation has no location, so it cannot be revealed in
             * the editor. Flag it clearly: it is not in effect any more, even
             * though it is still in the file.
             */
            this.description = 'stale';
            this.iconPath = new vscode.ThemeIcon(
                'warning',
                new vscode.ThemeColor('problemsWarningIcon.foreground'),
            );
            return;
        }

        /*
         * Defensive: gnatcov reports a location for every annotation it
         * resolved, so this only guards against output it did not produce.
         */
        if (annotation.range === undefined) {
            return;
        }

        this.description = `line ${String(annotation.range.startLine)}`;
        this.iconPath = new vscode.ThemeIcon('shield');

        this.command = {
            command: 'vscode.open',
            title: 'Open',
            arguments: [
                vscode.Uri.file(annotation.file),
                {
                    selection: toVscodeRange(annotation.range),
                } satisfies vscode.TextDocumentShowOptions,
            ],
        };
    }
}

/** Shown instead of the tree when there is nothing to show. */
export class MessageNode extends vscode.TreeItem {
    public constructor(message: string) {
        super(message);
    }
}

export type AnnotationTreeItem = FileNode | AnnotationNode | MessageNode;

export class ExternalAnnotationTreeProvider
    implements vscode.TreeDataProvider<AnnotationTreeItem>, vscode.Disposable
{
    private readonly onDidChangeEmitter = new vscode.EventEmitter<AnnotationTreeItem | undefined>();

    public readonly onDidChangeTreeData = this.onDidChangeEmitter.event;

    /**
     * The whole-project query, or undefined when none has run since the last
     * refresh. It runs gnatcov, so it is made lazily and cached.
     *
     * The promise is cached rather than its result, so dropping it is all a
     * refresh has to do. A superseded query then has nothing to install into.
     * It cannot put back the annotations of a previous project or scenario,
     * nor delete actions for identifiers that no longer exist.
     *
     * Concurrent callers share one run for the same reason.
     */
    private cached: Promise<ShowAnnotationsResult> | undefined;

    public refresh(): void {
        this.cached = undefined;
        this.onDidChangeEmitter.fire(undefined);
    }

    public getTreeItem(element: AnnotationTreeItem): vscode.TreeItem {
        return element;
    }

    public async getChildren(element?: AnnotationTreeItem): Promise<AnnotationTreeItem[]> {
        if (element === undefined) {
            return this.getRoots();
        }

        if (element instanceof FileNode) {
            return element.annotations.map((annotation) => new AnnotationNode(annotation));
        }

        return [];
    }

    private async getRoots(): Promise<AnnotationTreeItem[]> {
        const result = await this.query();
        const annotations = result.annotations;

        if (result.notConfigured === true) {
            return [new MessageNode("No project defines Coverage'External_Annotations")];
        }

        if (result.error !== undefined) {
            return [new MessageNode(result.error)];
        }

        if (annotations.length === 0) {
            return [new MessageNode('No annotations')];
        }

        // Group by file, preserving a stable alphabetical order: gnatcov's own
        // ordering is not guaranteed and puts stale entries first.
        const byFile = new Map<string, ExternalAnnotation[]>();
        for (const annotation of annotations) {
            const existing = byFile.get(annotation.file);
            if (existing === undefined) {
                byFile.set(annotation.file, [annotation]);
            } else {
                existing.push(annotation);
            }
        }

        return [...byFile.entries()]
            .sort(([a], [b]) => path.basename(a).localeCompare(path.basename(b)))
            .map(([file, fileAnnotations]) => new FileNode(file, fileAnnotations.sort(byPosition)));
    }

    private query(): Promise<ShowAnnotationsResult> {
        // No source file argument: report the annotations of the whole project.
        this.cached ??= showAnnotations().then((result) => {
            if (result.error !== undefined) {
                logger.debug(`External annotation tree unavailable: ${result.error}`);
            }
            return result;
        });

        return this.cached;
    }

    public dispose(): void {
        this.onDidChangeEmitter.dispose();
    }
}

/**
 * Order annotations within a file: resolved ones by position, stale ones last
 * since they have no position to sort on.
 */
function byPosition(a: ExternalAnnotation, b: ExternalAnnotation): number {
    if (a.range === undefined || b.range === undefined) {
        return a.range === undefined ? (b.range === undefined ? 0 : 1) : -1;
    }

    return a.range.startLine - b.range.startLine || a.range.startColumn - b.range.startColumn;
}

function buildTooltip(annotation: ExternalAnnotation): vscode.MarkdownString {
    const md = new vscode.MarkdownString();
    md.appendMarkdown(`**${annotation.kind}**\n\n`);

    const details = describeAnnotation(annotation);
    if (details.length > 0) {
        md.appendMarkdown(`${details}\n\n`);
    }

    if (annotation.justification !== undefined) {
        md.appendMarkdown(`${annotation.justification}\n\n`);
    }

    if (annotation.stale && annotation.diagnostic !== undefined) {
        md.appendMarkdown(`_Stale:_ ${annotation.diagnostic}\n\n`);
    }

    md.appendMarkdown(`\`id: ${annotation.id}\``);
    return md;
}
