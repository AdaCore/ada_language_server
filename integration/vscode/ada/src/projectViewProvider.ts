/*----------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2018-2024, AdaCore                     --
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
import { LanguageClient } from 'vscode-languageclient/node';
import { CMD_PROJECT_VIEW_INFORMATION } from './constants';

// ---------------------------------------------------------------------------
// Raw wire types – match the JSON field names returned by the server
// ---------------------------------------------------------------------------

interface Raw_ProjectSourceInfo {
    'file-name': string;
    'simple-name': string;
    directory: string;
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

interface Raw_ProjectViewResponse {
    info?: { 'generated-on': string; version: string };
    tree: { 'root-project': { name: string; id: string } };
    projects: Raw_ProjectEntry[];
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
}

// ---------------------------------------------------------------------------
// Parsing helper
// ---------------------------------------------------------------------------

function parseProjectViewResponse(raw: Raw_ProjectViewResponse): ProjectViewInformation {
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

    return {
        root_project_id: rootId,
        projects,
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
}

/**
 * A tree item for the Project View
 */
export class ProjectViewItem extends vscode.TreeItem {
    /** The kind of the project view item */
    public itemKind: ProjectViewItemKind;

    /** The URI of the item, if applicable */
    public uri?: vscode.Uri;

    /** For project items: the project id used to look up children */
    public projectId?: string;

    /** For source directory items: the source files belonging to this directory */
    public sources?: ProjectSourceInfo[];

    constructor(
        public label: string,
        public collapsibleState: vscode.TreeItemCollapsibleState = vscode.TreeItemCollapsibleState
            .None,
        itemKind: ProjectViewItemKind = ProjectViewItemKind.SUB_PROJECT,
        uri?: vscode.Uri,
        projectId?: string,
    ) {
        super(label, collapsibleState);
        this.itemKind = itemKind;
        this.uri = uri;
        this.projectId = projectId;
        this.iconPath = this.getIcon();

        if (itemKind === ProjectViewItemKind.SOURCE_FILE) {
            this.contextValue = 'sourceFile';
            this.command = uri
                ? {
                      command: 'vscode.open',
                      title: 'Open File',
                      arguments: [uri],
                  }
                : undefined;
        } else if (itemKind === ProjectViewItemKind.SOURCE_DIRECTORY) {
            this.contextValue = 'sourceDirectory';
        } else {
            this.contextValue = 'gprFile';
        }
    }

    private getIcon(): vscode.ThemeIcon | undefined {
        switch (this.itemKind) {
            case ProjectViewItemKind.SOURCE_FILE:
                return new vscode.ThemeIcon('file-code');
            case ProjectViewItemKind.SOURCE_DIRECTORY:
                return new vscode.ThemeIcon('folder');
            case ProjectViewItemKind.ROOT_PROJECT:
                return new vscode.ThemeIcon('project');
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

    private adaClient: LanguageClient;
    private rootProjectUri: vscode.Uri | undefined;
    /** Cached and parsed project view information */
    private projectViewInfo: ProjectViewInformation | undefined;
    private filterString: string = '';

    constructor(adaClient: LanguageClient) {
        this.adaClient = adaClient;
    }

    /**
     * Sets the root project URI for the Project View and
     * refreshes it.
     * This should be called when the root project changes
     * (e.g. after opening a new folder or after refreshing the project).
     *
     * @param uri - The URI of the new root project, or undefined
     * if there is no root project
     */
    setRootProjectUri(uri: vscode.Uri | undefined): void {
        this.rootProjectUri = uri;
        this.projectViewInfo = undefined;
        this.refresh();
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
    async getChildren(element?: ProjectViewItem): Promise<ProjectViewItem[]> {
        // Source files have no children
        if (element?.itemKind === ProjectViewItemKind.SOURCE_FILE) {
            return [];
        }

        // Source directory items return their source files as children
        if (element?.itemKind === ProjectViewItemKind.SOURCE_DIRECTORY) {
            if (!element.sources) return [];
            return element.sources.map((source) => {
                const sourceUri = vscode.Uri.file(source.file_name);
                return new ProjectViewItem(
                    source.simple_name,
                    vscode.TreeItemCollapsibleState.None,
                    ProjectViewItemKind.SOURCE_FILE,
                    sourceUri,
                );
            });
        }

        // Ensure project information is loaded
        await this.ensureProjectInfoLoaded();

        // If no element is provided, return the root project
        if (!element) {
            if (!this.projectViewInfo) {
                return [];
            }

            const rootEntry = this.projectViewInfo.projects.get(
                this.projectViewInfo.root_project_id,
            );
            if (!rootEntry) {
                return [];
            }

            const rootUri = vscode.Uri.file(rootEntry.project.file_name);
            const rootItem = new ProjectViewItem(
                rootEntry.project.name,
                vscode.TreeItemCollapsibleState.Expanded,
                ProjectViewItemKind.ROOT_PROJECT,
                rootUri,
                rootEntry.project.id,
            );

            return [rootItem];
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

        // Filter items if a filter is active
        if (this.filterString) {
            return items.filter((item) => this.matchesFilter(item));
        }
        return items;
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

        // Group sources by directory and add source directory items first
        const sourcesByDir = new Map<string, ProjectSourceInfo[]>();
        for (const source of entry.sources) {
            const dir = source.directory;
            let group = sourcesByDir.get(dir);
            if (!group) {
                group = [];
                sourcesByDir.set(dir, group);
            }
            group.push(source);
        }

        for (const [dir, sources] of sourcesByDir) {
            const dirItem = new ProjectViewItem(
                path.basename(dir),
                vscode.TreeItemCollapsibleState.Collapsed,
                ProjectViewItemKind.SOURCE_DIRECTORY,
                vscode.Uri.file(dir),
            );
            dirItem.sources = sources;
            dirItem.description = dir;
            items.push(dirItem);
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
                    ),
                );
            }
        }

        return items;
    }

    /**
     * Ensures that project information is loaded from the server.
     * If already cached, this is a no-op.
     */
    private async ensureProjectInfoLoaded(): Promise<void> {
        if (this.projectViewInfo) {
            return;
        }

        if (!this.rootProjectUri) {
            return;
        }

        try {
            const raw = await vscode.commands.executeCommand<Raw_ProjectViewResponse | null>(
                CMD_PROJECT_VIEW_INFORMATION,
            );

            if (!raw?.projects) {
                return;
            }

            this.projectViewInfo = parseProjectViewResponse(raw);
        } catch (error) {
            console.log(`Failed to fetch project view information:`, error);
        }
    }

    /**
     * Refreshes the Project View by firing the onDidChangeTreeData event.
     * This will cause VS Code to call getChildren again for all visible items.
     */
    refresh(): void {
        this.projectViewInfo = undefined;
        this._onDidChangeTreeData.fire();
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
     * An item matches the filter if its label includes the filter string
     * (case-insensitive).
     *
     * @param item - The ProjectViewItem to check
     * @returns True if the item matches the filter, false otherwise
     */
    private matchesFilter(item: ProjectViewItem): boolean {
        const label = item.label?.toString().toLowerCase() ?? '';
        return label.includes(this.filterString);
    }
}
