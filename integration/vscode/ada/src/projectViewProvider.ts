/*----------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2018-2026, AdaCore                     --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
----------------------------------------------------------------------------*/

import * as path from 'path';
import * as vscode from 'vscode';

// ---------------------------------------------------------------------------
// Raw wire types – match the JSON field names returned by the server
// ---------------------------------------------------------------------------

interface Raw_ProjectSourceInfo {
    'file-name': string;
    'simple-name': string;
    directory: string;
    language?: string;
}

interface Raw_ProjectInfo {
    id: string;
    name: string;
    kind: string;
    qualifier: string;
    'simple-name': string;
    'file-name': string;
    directory: string;
    'is-externally-built': boolean;
    languages?: string[];
    'source-directories'?: string[];
    'object-directory'?: string;
}

interface Raw_ProjectEntry {
    project: Raw_ProjectInfo;
    imports?: string[];
    aggregated?: string[];
    extended?: string[];
    extending?: { 'extending-all': boolean; 'project-id': string };
    'imported-by'?: string[];
    sources?: Raw_ProjectSourceInfo[];
}

interface Raw_RuntimeProjectInfo {
    id: string;
    name: string;
    'source-directories'?: string[];
    'object-directory'?: string;
    sources?: Raw_ProjectSourceInfo[];
}

export interface Raw_ProjectViewResponse {
    info?: { 'generated-on': string; version: string };
    tree: { 'root-project': { name: string; id: string } };
    projects: Raw_ProjectEntry[];
    'runtime-project'?: Raw_RuntimeProjectInfo;
}

// ---------------------------------------------------------------------------
// Parsed types – clean field names, arrays always present
// ---------------------------------------------------------------------------

/**
 * Represents a source file belonging to a project.
 */
export interface ProjectSourceInfo {
    file_name: string;
    simple_name: string;
    directory: string;
    /** GPR-reported language name (e.g. 'ada', 'c'), if available */
    language?: string;
}

/**
 * Metadata for a single GPR project.
 */
export interface ProjectInfo {
    id: string;
    name: string;
    kind: string;
    qualifier: string;
    simple_name: string;
    file_name: string;
    directory: string;
    is_externally_built: boolean;
    languages: string[];
    source_directories: string[];
    object_dir?: string;
}

/**
 * A project together with its dependency ids and source files.
 */
export interface ProjectEntry {
    project: ProjectInfo;
    imports: string[];
    aggregated: string[];
    extended: string[];
    extending?: { extending_all: boolean; project_id: string };
    imported_by: string[];
    sources: ProjectSourceInfo[];
}

/**
 * The full project view information returned by the als-project-view-information command.
 */
export interface ProjectViewInformation {
    root_project_id: string;
    projects: Map<string, ProjectEntry>;
    /** Runtime project entry, if the server returned one */
    runtime_project?: ProjectEntry;
}

// ---------------------------------------------------------------------------
// Parsing helper
// ---------------------------------------------------------------------------

export function parseProjectViewResponse(raw: Raw_ProjectViewResponse): ProjectViewInformation {
    const projects = new Map<string, ProjectEntry>();

    for (const rawEntry of raw.projects) {
        // Guard against malformed entries that are missing the 'project' field entirely.
        const p = rawEntry.project;
        if (!p?.id) {
            throw new Error('Project View response contains an entry with a missing project id');
        }

        const entry: ProjectEntry = {
            project: {
                id: p.id,
                name: p.name,
                kind: p.kind,
                qualifier: p.qualifier,
                simple_name: p['simple-name'],
                file_name: p['file-name'],
                directory: p.directory,
                is_externally_built: p['is-externally-built'],
                languages: p.languages ?? [],
                source_directories: p['source-directories'] ?? [],
                object_dir: p['object-directory'],
            },
            imports: rawEntry.imports ?? [],
            aggregated: rawEntry.aggregated ?? [],
            extended: rawEntry.extended ?? [],
            extending: rawEntry.extending
                ? {
                      extending_all: rawEntry.extending['extending-all'],
                      project_id: rawEntry.extending['project-id'],
                  }
                : undefined,
            imported_by: rawEntry['imported-by'] ?? [],
            sources: (rawEntry.sources ?? []).map((s) => ({
                file_name: s['file-name'],
                simple_name: s['simple-name'],
                directory: s.directory,
                language: s.language,
            })),
        };
        projects.set(entry.project.id, entry);
    }

    // Guard against a response that is missing the tree/root-project structure.
    // This would indicate a server-side schema change or a corrupted response.
    const rootId = raw.tree?.['root-project']?.id;
    if (!rootId) {
        throw new Error('Project View response is missing the root-project id');
    }

    // Parse optional runtime project entry returned by the server
    let runtime_project: ProjectEntry | undefined;
    const rawRuntime = raw['runtime-project'];
    if (rawRuntime) {
        runtime_project = {
            project: {
                id: rawRuntime.id,
                name: rawRuntime.name,
                kind: 'runtime',
                qualifier: 'runtime',
                simple_name: rawRuntime.name,
                file_name: '',
                directory: '',
                is_externally_built: true,
                languages: [],
                source_directories: rawRuntime['source-directories'] ?? [],
                object_dir: rawRuntime['object-directory'],
            },
            imports: [],
            aggregated: [],
            extended: [],
            imported_by: [],
            sources: (rawRuntime.sources ?? []).map((s) => ({
                file_name: s['file-name'],
                simple_name: s['simple-name'],
                directory: s.directory,
            })),
        };
    }

    return {
        root_project_id: rootId,
        projects,
        runtime_project,
    };
}

/**
 * The kind of a project view item, used for display purposes.
 */
export enum ProjectViewItemKind {
    ROOT_PROJECT,
    SUB_PROJECT,
    SOURCE_DIRECTORY,
    SOURCE_FILE,
    OBJECT_DIRECTORY,
    RUNTIME_PROJECT,
}

/**
 * A tree item for the Project View
 */
export class ProjectViewItem extends vscode.TreeItem {
    /** The kind of the project view item */
    public itemKind: ProjectViewItemKind;

    /** The URI of the item */
    public uri: vscode.Uri;

    /** For project items: the project id used to look up children */
    public projectId?: string;

    /** The id of the parent project, used for stable unique ID generation */
    public parentProjectId?: string;

    /** For source directory items: the source files belonging to this directory */
    public sources?: ProjectSourceInfo[];

    constructor(
        public label: string,
        public collapsibleState: vscode.TreeItemCollapsibleState = vscode.TreeItemCollapsibleState
            .None,
        itemKind: ProjectViewItemKind = ProjectViewItemKind.SUB_PROJECT,
        uri: vscode.Uri,
        projectId?: string,
        parentProjectId?: string,
    ) {
        super(label, collapsibleState);
        this.itemKind = itemKind;
        this.uri = uri;
        this.projectId = projectId;
        this.parentProjectId = parentProjectId;
        // For source files, let the active file icon theme supply the icon by
        // setting resourceUri and leaving iconPath unset.
        if (itemKind === ProjectViewItemKind.SOURCE_FILE) {
            this.resourceUri = uri;
        } else {
            this.iconPath = this.getIcon();
        }
        this.id = this.getUniqueID(itemKind, projectId, parentProjectId, uri);

        if (itemKind === ProjectViewItemKind.SOURCE_FILE) {
            this.contextValue = 'sourceFile';
            this.command = {
                command: 'vscode.open',
                title: 'Open File',
                arguments: [uri],
            };
        } else if (itemKind === ProjectViewItemKind.SOURCE_DIRECTORY) {
            this.contextValue = 'sourceDirectory';
        } else if (itemKind === ProjectViewItemKind.OBJECT_DIRECTORY) {
            this.contextValue = 'objectDirectory';
        } else if (itemKind === ProjectViewItemKind.RUNTIME_PROJECT) {
            this.contextValue = 'runtimeProject';
        } else {
            this.contextValue = 'gprFile';
        }
    }

    private getUniqueID(
        itemKind: ProjectViewItemKind,
        projectId: string | undefined,
        parentProjectId: string | undefined,
        uri: vscode.Uri,
    ): string {
        // Ids are used to identify items for state persistence (e.g. expansion state)
        // and must be stable across refreshes. Each formula encodes enough context to
        // make the id unique even when the same project/directory/file appears at
        // multiple positions in the tree (diamond imports, shared source directories).
        switch (itemKind) {
            case ProjectViewItemKind.ROOT_PROJECT:
                return `project-${projectId}`;
            case ProjectViewItemKind.SUB_PROJECT:
                return `project-${projectId}-under-${parentProjectId ?? 'root'}`;
            case ProjectViewItemKind.SOURCE_DIRECTORY:
                return `dir-${uri.toString()}-in-${parentProjectId}`;
            case ProjectViewItemKind.SOURCE_FILE:
                return `file-${uri.toString()}-in-${parentProjectId}`;
            case ProjectViewItemKind.OBJECT_DIRECTORY:
                return `objdir-${uri.toString()}-in-${parentProjectId}`;
            case ProjectViewItemKind.RUNTIME_PROJECT:
                return `runtime-project`;
        }
    }

    private getIcon(): vscode.ThemeIcon | undefined {
        switch (this.itemKind) {
            case ProjectViewItemKind.SOURCE_FILE:
                // Icon is driven by the file icon theme via resourceUri; no iconPath needed.
                return undefined;
            case ProjectViewItemKind.SOURCE_DIRECTORY:
                return new vscode.ThemeIcon('folder');
            case ProjectViewItemKind.OBJECT_DIRECTORY:
                return new vscode.ThemeIcon('package');
            case ProjectViewItemKind.ROOT_PROJECT:
                return new vscode.ThemeIcon('project');
            case ProjectViewItemKind.RUNTIME_PROJECT:
                return new vscode.ThemeIcon('library');
            default:
                return new vscode.ThemeIcon('project');
        }
    }
}

/**
 * Tree data provider for the Project View
 */
export class ProjectViewProvider implements vscode.TreeDataProvider<ProjectViewItem> {
    private _onDidChangeTreeData: vscode.EventEmitter<ProjectViewItem | undefined | null | void> =
        new vscode.EventEmitter<ProjectViewItem | undefined | null | void>();
    readonly onDidChangeTreeData: vscode.Event<ProjectViewItem | undefined | null | void> =
        this._onDidChangeTreeData.event;

    /** Cached and parsed project view information, set externally by ExtensionState */
    private projectViewInfo: ProjectViewInformation | undefined;
    private filterString: string = '';

    /** When true, all projects are shown as a flat list instead of a hierarchy */
    public flatMode: boolean = false;

    /** When true, object directories are shown as children of each project */
    public showObjectDirs: boolean = false;

    /** When true, the runtime project (if returned by the server) is shown */
    public showRuntimeFiles: boolean = false;

    constructor() {
        // Restore persisted display preferences from VS Code settings
        const config = vscode.workspace.getConfiguration('ada');
        this.flatMode = config.get<boolean>('projectView.flatMode', false);
        this.showObjectDirs = config.get<boolean>('projectView.showObjectDirectories', false);
        this.showRuntimeFiles = config.get<boolean>('projectView.showRuntimeFiles', false);
    }

    /**
     * Sets the project view information and refreshes the tree.
     * Called by ExtensionState whenever the project is (re-)loaded.
     *
     * @param info - The newly fetched project view information, or undefined
     * if no project is loaded
     */
    setProjectViewInfo(info: ProjectViewInformation | undefined): void {
        this.projectViewInfo = info;
        this._onDidChangeTreeData.fire();
    }

    /**
     * Retrieves the TreeItem representation of a given ProjectViewItem.
     * In this implementation, the ProjectViewItem itself is a TreeItem,
     * so it is returned directly.
     *
     * @param element - The ProjectViewItem for which to retrieve the TreeItem
     * @returns The TreeItem representation of the given ProjectViewItem
     */
    getTreeItem(element: ProjectViewItem): vscode.TreeItem {
        return element;
    }

    /**
     * Retrieves the children of a given element in the Project View.
     * If no element is provided, this returns the root project as the only child.
     * If an element is provided but it has no project id and is not a source file,
     * this returns an empty array.
     * Otherwise, retrieves the children (sub-projects and source files) of the project.
     *
     * @param element - The ProjectViewItem for which to retrieve children,
     * or undefined to retrieve the root project
     * @returns A promise that resolves to an array of ProjectViewItem
     * representing the children of the given element
     */
    getChildren(element?: ProjectViewItem): ProjectViewItem[] {
        // Source files have no children
        if (element?.itemKind === ProjectViewItemKind.SOURCE_FILE) {
            return [];
        }

        // Object directory items have no children
        if (element?.itemKind === ProjectViewItemKind.OBJECT_DIRECTORY) {
            return [];
        }

        // Source directory items return their source files as children
        if (element?.itemKind === ProjectViewItemKind.SOURCE_DIRECTORY) {
            if (!element.sources) return [];
            const filteredSources = this.filterString
                ? element.sources.filter((s) => this.sourceInfoMatchesFilter(s))
                : element.sources;
            return filteredSources.map((source) => {
                const sourceUri = vscode.Uri.file(source.file_name);
                return new ProjectViewItem(
                    source.simple_name,
                    vscode.TreeItemCollapsibleState.None,
                    ProjectViewItemKind.SOURCE_FILE,
                    sourceUri,
                    undefined,
                    element.parentProjectId,
                );
            });
        }

        // The runtime project item returns its own source directory / file children
        if (element?.itemKind === ProjectViewItemKind.RUNTIME_PROJECT) {
            if (!this.projectViewInfo?.runtime_project) return [];
            const items = this.getProjectChildren(this.projectViewInfo.runtime_project);
            if (this.filterString) {
                return items.filter((item) => this.filterProjectChild(item));
            }
            return items;
        }

        // If no element is provided, return the root item(s)
        if (!element) {
            if (!this.projectViewInfo) {
                return [];
            }

            const roots: ProjectViewItem[] = [];

            if (this.flatMode) {
                // Flat mode: show all projects as a flat list, root project first
                const rootId = this.projectViewInfo.root_project_id;
                const sortedEntries = [...this.projectViewInfo.projects.values()].sort((a, b) =>
                    a.project.id === rootId ? -1 : b.project.id === rootId ? 1 : 0,
                );
                for (const entry of sortedEntries) {
                    const projectUri = vscode.Uri.file(entry.project.file_name);
                    const isRoot = entry.project.id === rootId;
                    roots.push(
                        new ProjectViewItem(
                            entry.project.name,
                            vscode.TreeItemCollapsibleState.Collapsed,
                            isRoot
                                ? ProjectViewItemKind.ROOT_PROJECT
                                : ProjectViewItemKind.SUB_PROJECT,
                            projectUri,
                            entry.project.id,
                        ),
                    );
                }
            } else {
                // Hierarchical mode: show only the root project
                const rootEntry = this.projectViewInfo.projects.get(
                    this.projectViewInfo.root_project_id,
                );
                if (!rootEntry) {
                    return [];
                }

                const rootUri = vscode.Uri.file(rootEntry.project.file_name);
                roots.push(
                    new ProjectViewItem(
                        rootEntry.project.name,
                        vscode.TreeItemCollapsibleState.Expanded,
                        ProjectViewItemKind.ROOT_PROJECT,
                        rootUri,
                        rootEntry.project.id,
                    ),
                );
            }

            // Append the runtime project item when enabled
            if (this.showRuntimeFiles && this.projectViewInfo.runtime_project) {
                const rp = this.projectViewInfo.runtime_project;
                roots.push(
                    new ProjectViewItem(
                        rp.project.name,
                        vscode.TreeItemCollapsibleState.Collapsed,
                        ProjectViewItemKind.RUNTIME_PROJECT,
                        vscode.Uri.file(''),
                    ),
                );
            }

            // In flat mode, filter out projects whose names and descendants
            // don't match the filter string at all.
            if (this.filterString && this.flatMode) {
                return roots.filter((item) => {
                    // The 'runtime' project item is just a logical group, so it should
                    // always match: the runtime sources are still filtered though.
                    if (item.itemKind === ProjectViewItemKind.RUNTIME_PROJECT) return true;
                    const entry = this.projectViewInfo?.projects.get(item.projectId!);
                    return entry ? this.projectOrDescendantMatchesFilter(entry) : false;
                });
            }

            return roots;
        }

        // If element has no project id, return empty
        if (!element.projectId || !this.projectViewInfo) {
            return [];
        }

        const entry = this.projectViewInfo.projects.get(element.projectId);
        if (!entry) {
            return [];
        }

        const items: ProjectViewItem[] = this.getProjectChildren(entry);

        // In flat mode, suppress sub-project children (projects already shown at root level)
        const filtered = this.flatMode
            ? items.filter(
                  (item) =>
                      item.itemKind !== ProjectViewItemKind.SUB_PROJECT &&
                      item.itemKind !== ProjectViewItemKind.ROOT_PROJECT,
              )
            : items;

        // Filter items if a filter is active
        if (this.filterString) {
            return filtered.filter((item) => this.filterProjectChild(item));
        }
        return filtered;
    }

    /**
     * Build the list of child ProjectViewItems for a given project entry.
     * Children are: source directory items (each containing source files)
     * first, followed by imported/aggregated/extended sub-project items.
     */
    private getProjectChildren(entry: ProjectEntry): ProjectViewItem[] {
        const items: ProjectViewItem[] = [];

        if (!this.projectViewInfo) {
            return items;
        }

        const sourcesByDir = this.buildSourcesByDir(entry);

        for (const [dir, sources] of sourcesByDir) {
            const dirItem = new ProjectViewItem(
                path.basename(dir),
                vscode.TreeItemCollapsibleState.Collapsed,
                ProjectViewItemKind.SOURCE_DIRECTORY,
                vscode.Uri.file(dir),
                undefined,
                entry.project.id,
            );
            dirItem.sources = sources;
            dirItem.description = dir;
            items.push(dirItem);
        }

        // Add object directory item when the setting is enabled
        if (this.showObjectDirs && entry.project.object_dir) {
            const objDir = entry.project.object_dir;
            const objDirItem = new ProjectViewItem(
                path.basename(objDir),
                vscode.TreeItemCollapsibleState.Collapsed,
                ProjectViewItemKind.OBJECT_DIRECTORY,
                vscode.Uri.file(objDir),
                undefined,
                entry.project.id,
            );
            objDirItem.description = objDir;
            objDirItem.tooltip = `Object directory: ${objDir}`;
            items.push(objDirItem);
        }

        // Add sub-project children (imports, aggregated, extended)
        const subProjectIds = [...entry.imports, ...entry.aggregated, ...entry.extended];

        for (const subId of subProjectIds) {
            const subEntry = this.projectViewInfo.projects.get(subId);
            if (subEntry) {
                const subUri = vscode.Uri.file(subEntry.project.file_name);
                items.push(
                    new ProjectViewItem(
                        subEntry.project.name,
                        vscode.TreeItemCollapsibleState.Collapsed,
                        ProjectViewItemKind.SUB_PROJECT,
                        subUri,
                        subId,
                        entry.project.id,
                    ),
                );
            }
        }

        return items;
    }

    /**
     * Groups a project entry's source files by their normalized directory path
     * and unions the result with the project's declared source directories
     * (which may be empty).
     * Keys are normalized via `path.resolve` so that the
     * trailing separators and other path variations are handled correctly on all
     * platforms.
     */
    private buildSourcesByDir(entry: ProjectEntry): Map<string, ProjectSourceInfo[]> {
        const sourcesByDir = new Map<string, ProjectSourceInfo[]>();

        // Normalize directory keys via path.resolve to remove any trailing
        // separators and ensure consistent formatting across platforms.
        const normalizeDir = (dir: string): string => path.resolve(dir);

        // First group sources by their directory path
        for (const source of entry.sources) {
            const key = normalizeDir(source.directory);
            let group = sourcesByDir.get(key);
            if (!group) {
                group = [];
                sourcesByDir.set(key, group);
            }
            group.push(source);
        }

        // Then ensure that all declared project source directories are present
        // in the map, even if they have no source files: we want to display
        // empty source directories in the Project View
        for (const dir of entry.project.source_directories) {
            const key = normalizeDir(dir);
            if (!sourcesByDir.has(key)) {
                sourcesByDir.set(key, []);
            }
        }
        return sourcesByDir;
    }

    /**
     * Sets a filter string for the Project View and refreshes it.
     * Only items whose labels include the filter string (case-insensitive)
     * will be shown in the Project View. If the filter string is empty,
     * all items will be shown.
     *
     * @param filter - The filter string to set, or an empty string to unset the filter
     */
    setFilter(filter: string): void {
        this.filterString = filter.toLowerCase();
        this._onDidChangeTreeData.fire();
    }

    /**
     * Gets the current filter string for the Project View.
     * If no filter is active, this returns an empty string.
     *
     * @returns The current filter string, or an empty string if no filter is active
     */
    getFilter(): string {
        return this.filterString;
    }

    /**
     * Checks if a given ProjectViewItem matches the current filter string.
     * An item matches the filter if its label or its full URI path includes
     * the filter string (case-insensitive).
     *
     * @param item - The ProjectViewItem to check
     * @returns True if the item matches the filter, false otherwise
     */
    private matchesFilter(item: ProjectViewItem): boolean {
        const filter = this.filterString;
        const label = item.label?.toString().toLowerCase() ?? '';
        if (label.includes(filter)) return true;
        return item.uri.fsPath.toLowerCase().includes(filter);
    }

    /**
     * Checks if a source file matches the current filter string.
     * Matches against the file's base name only; directory matching is
     * handled separately by sourceDirOrChildMatchesFilter.
     */
    private sourceInfoMatchesFilter(source: ProjectSourceInfo): boolean {
        return source.simple_name.toLowerCase().includes(this.filterString);
    }

    /**
     * Checks if a source directory or any of its source files match the
     * current filter string.
     */
    private sourceDirOrChildMatchesFilter(dirPath: string, sources: ProjectSourceInfo[]): boolean {
        const filter = this.filterString;
        if (dirPath.toLowerCase().includes(filter)) return true;
        return sources.some((s) => this.sourceInfoMatchesFilter(s));
    }

    /**
     * Checks if a project entry or any of its descendants (source directories,
     * source files, object directory, sub-projects) match the current filter.
     *
     * A visited set is kept to guard against cycles in the project graph.
     */
    private projectOrDescendantMatchesFilter(
        entry: ProjectEntry,
        visited = new Set<string>(),
    ): boolean {
        if (!this.projectViewInfo || visited.has(entry.project.id)) return false;
        visited.add(entry.project.id);

        const filter = this.filterString;

        // Match on project name or project file path
        if (entry.project.name.toLowerCase().includes(filter)) return true;
        if (entry.project.file_name.toLowerCase().includes(filter)) return true;

        // Match on source directories and their files.
        const sourcesByDir = this.buildSourcesByDir(entry);
        for (const [dir, sources] of sourcesByDir) {
            if (this.sourceDirOrChildMatchesFilter(dir, sources)) return true;
        }

        // Match on object directory path
        if (this.showObjectDirs && entry.project.object_dir) {
            if (entry.project.object_dir.toLowerCase().includes(filter)) return true;
        }

        // Recurse into sub-projects
        const subIds = [...entry.imports, ...entry.aggregated, ...entry.extended];
        for (const subId of subIds) {
            const subEntry = this.projectViewInfo.projects.get(subId);
            if (subEntry && this.projectOrDescendantMatchesFilter(subEntry, visited)) return true;
        }

        return false;
    }

    /**
     * Determines whether a direct child of a project item should be shown
     * when a filter is active.
     *
     * - Source directories: shown if the directory path or any contained file matches.
     * - Sub-projects: shown if the project itself or any descendant matches.
     * - Other items (object directory): shown if their label or URI path matches.
     */
    private filterProjectChild(item: ProjectViewItem): boolean {
        if (!this.filterString) return true;
        switch (item.itemKind) {
            case ProjectViewItemKind.SOURCE_DIRECTORY:
                return this.sourceDirOrChildMatchesFilter(item.uri.fsPath, item.sources ?? []);
            case ProjectViewItemKind.SUB_PROJECT:
            case ProjectViewItemKind.ROOT_PROJECT: {
                if (!item.projectId || !this.projectViewInfo) return false;
                const entry = this.projectViewInfo.projects.get(item.projectId);
                return entry ? this.projectOrDescendantMatchesFilter(entry) : false;
            }
            default:
                return this.matchesFilter(item);
        }
    }

    /**
     * Updates the view display settings and refreshes the tree.
     *
     * @param flatMode - Show all projects as a flat list instead of a hierarchy
     * @param showObjectDirs - Show object directories as children of each project
     * @param showRuntimeFiles - Show the runtime project (when available from the server)
     */
    setViewSettings(flatMode: boolean, showObjectDirs: boolean, showRuntimeFiles: boolean): void {
        this.flatMode = flatMode;
        this.showObjectDirs = showObjectDirs;
        this.showRuntimeFiles = showRuntimeFiles;
        this._onDidChangeTreeData.fire();
    }
}

/**
 * MIME type used for drag-and-drop within the Project View tree.
 * VS Code requires the view id (lowercased) as the suffix.
 */
export const PROJECT_VIEW_MIME_TYPE = 'application/vnd.code.tree.projectview';

/**
 * Implements drag-and-drop for the Project View, allowing source files to be
 * moved from one source directory to another by dragging and dropping.
 *
 * Only `SOURCE_FILE` items may be dragged, and they may only be dropped onto
 * `SOURCE_DIRECTORY` items.  Dropping a file onto its own parent directory is
 * a no-op.  If a file with the same name already exists in the target
 * directory the user is asked to confirm before overwriting.
 *
 * After one or more files are successfully moved, `onFilesMoved` is called so
 * the caller can trigger a project reload and view refresh. If a rename fails
 * the loop continues so that already-moved files are still reflected in the view.
 */
export class ProjectViewDragAndDropController
    implements vscode.TreeDragAndDropController<ProjectViewItem>
{
    readonly dragMimeTypes = [PROJECT_VIEW_MIME_TYPE];
    readonly dropMimeTypes = [PROJECT_VIEW_MIME_TYPE];

    /**
     * @param onFilesMoved - Async callback invoked after one or more files have
     *   been successfully moved. Typically triggers a project reload.
     *   Called even if some files in the batch failed to move.
     */
    constructor(private readonly onFilesMoved: () => Promise<void>) {}

    handleDrag(source: readonly ProjectViewItem[], dataTransfer: vscode.DataTransfer): void {
        const sourceFiles = source.filter(
            (item) => item.itemKind === ProjectViewItemKind.SOURCE_FILE,
        );
        if (sourceFiles.length === 0) return;
        dataTransfer.set(PROJECT_VIEW_MIME_TYPE, new vscode.DataTransferItem(sourceFiles));
    }

    async handleDrop(
        target: ProjectViewItem | undefined,
        dataTransfer: vscode.DataTransfer,
    ): Promise<void> {
        if (target?.itemKind !== ProjectViewItemKind.SOURCE_DIRECTORY) return;

        const transferItem = dataTransfer.get(PROJECT_VIEW_MIME_TYPE);
        if (!transferItem) return;

        const droppedItems = transferItem.value as ProjectViewItem[];
        const targetDir = target.uri;
        const targetDirName = path.basename(targetDir.fsPath);

        const filesToMove = droppedItems.filter(
            (item) =>
                item.itemKind === ProjectViewItemKind.SOURCE_FILE &&
                path.dirname(item.uri.fsPath) !== targetDir.fsPath,
        );
        if (filesToMove.length === 0) return;

        // Ask for confirmation before performing any move.
        const confirmMsg =
            filesToMove.length === 1
                ? `Move '${path.basename(filesToMove[0].uri.fsPath)}' to '${targetDirName}'?`
                : `Move ${filesToMove.length} files to '${targetDirName}'?`;
        const confirmed = await vscode.window.showWarningMessage(
            confirmMsg,
            { modal: true },
            'Move',
        );
        if (confirmed !== 'Move') return;

        let anyMoved = false;

        for (const fileItem of filesToMove) {
            const srcUri = fileItem.uri;
            const fileName = path.basename(srcUri.fsPath);
            const destUri = vscode.Uri.joinPath(targetDir, fileName);

            // Check for a name collision in the target directory.
            let destExists = false;
            try {
                await vscode.workspace.fs.stat(destUri);
                destExists = true;
            } catch {
                // File does not exist at destination – proceed normally.
            }

            if (destExists) {
                const choice = await vscode.window.showWarningMessage(
                    `A file or folder with the name '${fileName}' already exists in the` +
                        ` destination folder. Do you want to replace it?`,
                    { modal: true },
                    'Replace',
                );
                if (choice !== 'Replace') continue;
            }

            try {
                // Use applyEdit so that any open editor for the source URI is
                // automatically redirected to the new location.
                const edit = new vscode.WorkspaceEdit();
                edit.renameFile(srcUri, destUri, { overwrite: true });
                const ok = await vscode.workspace.applyEdit(edit);
                if (ok) {
                    anyMoved = true;
                } else {
                    void vscode.window.showErrorMessage(`Failed to move '${fileName}'.`);
                }
            } catch (err) {
                void vscode.window.showErrorMessage(`Failed to move '${fileName}': ${String(err)}`);
                // Do not return – continue moving remaining files and let
                // onFilesMoved() fire so the view stays in sync with disk.
            }
        }

        if (anyMoved) {
            await this.onFilesMoved();
        }
    }
}
