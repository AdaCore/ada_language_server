/*----------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2026, AdaCore                          --
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
import { ProjectEntry, ProjectViewInformation, ProjectSourceInfo } from './projectViewData';
import {
    ProjectViewItem,
    ProjectViewItemKind,
    ProjectViewTree,
    normalizeFsPath,
    buildSourcesByDir,
    createSourceFileItem,
    createSourceDirectoryItem,
} from './projectViewTree';

/**
 * Builds the project and file layout for the Project View
 */
export class ProjectFileTree implements ProjectViewTree<ProjectViewItem> {
    /** Cached and parsed project view information, set externally by ExtensionState */
    private projectViewInfo: ProjectViewInformation | undefined;
    private filterString: string = '';

    /** When true, all projects are shown as a flat list instead of a hierarchy */
    public flatMode: boolean = false;

    /** When true, object directories are shown as children of each project */
    public showObjectDirs: boolean = false;

    /** When true, the runtime project (if returned by the server) is shown */
    public showRuntimeFiles: boolean = false;

    /**
     * Sets the project view information and refreshes the tree.
     * Called by ExtensionState whenever the project is (re-)loaded.
     *
     * @param info - The newly fetched project view information, or undefined
     * if no project is loaded
     */
    setProjectViewInfo(info: ProjectViewInformation | undefined): void {
        this.projectViewInfo = info;
    }

    /**
     * Returns the parent of a given ProjectViewItem, or `undefined` if the
     * item is a root-level item. Required by VS Code for `TreeView.reveal()`
     * to work.
     *
     * Items are not cached — `getChildren()` recreates them on every call.
     * VS Code matches items by their `id` (from `getUniqueID()`), so the items
     * returned here just need to carry the right IDs, not be the same object
     * instances as those already in the tree.
     *
     * We cannot delegate to `getChildren(undefined)` here: in hierarchical mode
     * that only returns the single root project, so any source file whose owning
     * project is a sub-project would appear parentless to VS Code. Instead we
     * construct parent items directly from `projectViewInfo`.
     *
     * @param element - The ProjectViewItem whose parent to retrieve
     * @returns The parent ProjectViewItem, or undefined for root-level items
     */
    getParent(element: ProjectViewItem): ProjectViewItem | undefined {
        if (!this.projectViewInfo) return undefined;

        switch (element.itemKind) {
            case ProjectViewItemKind.ROOT_PROJECT:
            case ProjectViewItemKind.RUNTIME_PROJECT:
                return undefined;

            case ProjectViewItemKind.SUB_PROJECT: {
                // In flat mode all projects are shown at root level.
                if (this.flatMode) return undefined;
                if (!element.parentProjectId) return undefined;
                return this.makeProjectItem(element.parentProjectId);
            }

            case ProjectViewItemKind.SOURCE_DIRECTORY:
            case ProjectViewItemKind.OBJECT_DIRECTORY: {
                if (!element.parentProjectId) return undefined;
                if (element.parentProjectId === this.projectViewInfo.runtime_project?.project.id) {
                    return this.makeRuntimeProjectItem();
                }
                return this.makeProjectItem(element.parentProjectId);
            }

            case ProjectViewItemKind.SOURCE_FILE: {
                if (!element.parentProjectId) return undefined;

                // Reconstruct the SOURCE_DIRECTORY that is the logical parent of
                // this source file directly from projectViewInfo, without going
                // through getChildren().  Using getChildren(undefined) would not
                // work here because in hierarchical mode it only returns the root
                // project; source files in a sub-project would therefore appear
                // parentless to VS Code.
                let entry: ProjectEntry | undefined;
                if (element.parentProjectId === this.projectViewInfo.runtime_project?.project.id) {
                    entry = this.projectViewInfo.runtime_project;
                } else {
                    entry = this.projectViewInfo.projects.get(element.parentProjectId);
                }
                if (!entry) return undefined;

                // Use the directory carried on the item if available (set by
                // findSourceFileItem / getChildren from source.directory);
                // fall back to dirname(uri) only for items created by older
                // code paths that do not set sourceDirectory.
                const normalizedDir =
                    element.sourceDirectory ?? normalizeFsPath(path.dirname(element.uri.fsPath));

                const sourcesByDir = buildSourcesByDir(
                    entry.sources,
                    entry.project.source_directories,
                );
                const sources = sourcesByDir.get(normalizedDir) ?? [];

                const dirItem = createSourceDirectoryItem(
                    normalizedDir,
                    sources,
                    element.parentProjectId,
                );
                return dirItem;
            }
        }
    }

    /**
     * Constructs a ProjectViewItem for the given project id with the correct
     * kind (ROOT_PROJECT vs SUB_PROJECT) and parentProjectId, so that its
     * `id` matches what `getChildren()` would produce for the same node.
     * Used internally by `getParent`.
     */
    private makeProjectItem(projectId: string): ProjectViewItem | undefined {
        if (!this.projectViewInfo) return undefined;
        const entry = this.projectViewInfo.projects.get(projectId);
        if (!entry) return undefined;

        const isRoot = projectId === this.projectViewInfo.root_project_id;
        const kind = isRoot ? ProjectViewItemKind.ROOT_PROJECT : ProjectViewItemKind.SUB_PROJECT;

        // In flat mode all projects are at root level (no parentProjectId).
        // In hierarchical mode use the first importer as the canonical parent
        // so we always follow one consistent path to the root.
        const parentProjectId = !isRoot && !this.flatMode ? entry.imported_by[0] : undefined;

        return new ProjectViewItem(
            entry.project.name,
            vscode.TreeItemCollapsibleState.Collapsed,
            kind,
            vscode.Uri.file(entry.project.file_name),
            projectId,
            parentProjectId,
        );
    }

    /**
     * Constructs the ProjectViewItem for the runtime project.
     * Used internally by `getParent`.
     */
    private makeRuntimeProjectItem(): ProjectViewItem | undefined {
        if (!this.projectViewInfo?.runtime_project) return undefined;
        const rp = this.projectViewInfo.runtime_project;
        return new ProjectViewItem(
            rp.project.name,
            vscode.TreeItemCollapsibleState.Collapsed,
            ProjectViewItemKind.RUNTIME_PROJECT,
            vscode.Uri.file(''),
        );
    }

    /**
     * Searches the project view data for a source file matching `fileUri` and
     * returns a `ProjectViewItem` that can be passed to `TreeView.reveal()`.
     *
     * The runtime project is only searched when `showRuntimeFiles` is enabled,
     * since the runtime project node is not shown otherwise.
     *
     * @param fileUri - The URI of the file to locate in the project tree
     * @returns A ProjectViewItem for the file, or `undefined` if not found
     */
    findSourceFileItem(fileUri: vscode.Uri): ProjectViewItem | undefined {
        if (!this.projectViewInfo) return undefined;

        const filePath = normalizeFsPath(fileUri.fsPath);

        for (const entry of this.projectViewInfo.projects.values()) {
            for (const source of entry.sources) {
                if (normalizeFsPath(source.file_name) === filePath) {
                    const fileItem = createSourceFileItem(source, entry.project.id);
                    return fileItem;
                }
            }
        }

        // Also check the runtime project when it is currently shown.
        if (this.showRuntimeFiles && this.projectViewInfo.runtime_project) {
            const rp = this.projectViewInfo.runtime_project;
            for (const source of rp.sources) {
                if (normalizeFsPath(source.file_name) === filePath) {
                    const fileItem = createSourceFileItem(source, rp.project.id);
                    return fileItem;
                }
            }
        }

        return undefined;
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
                const fileItem = createSourceFileItem(source, element.parentProjectId);
                return fileItem;
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

        const sourcesByDir = buildSourcesByDir(entry.sources, entry.project.source_directories);

        for (const [dir, sources] of sourcesByDir) {
            const dirItem = createSourceDirectoryItem(dir, sources, entry.project.id);
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
     * Sets a filter string for the Project View and refreshes it.
     * Only items whose labels include the filter string (case-insensitive)
     * will be shown in the Project View. If the filter string is empty,
     * all items will be shown.
     *
     * @param filter - The filter string to set, or an empty string to unset the filter
     */
    setFilter(filter: string): void {
        this.filterString = filter.toLowerCase();
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
        const sourcesByDir = buildSourcesByDir(entry.sources, entry.project.source_directories);
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
    }
}
