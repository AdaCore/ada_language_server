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
import { ProjectSourceInfo } from './projectViewData';

/**
 * Normalizes a file-system path for equality comparison.
 * On Windows, file paths are case-insensitive, so we lower-case after
 * resolving to avoid mismatches between paths returned by VS Code
 * (e.g. lowercase drive letter from `fsPath`) and paths returned by
 * the Ada language server (typically uppercase drive letter).
 */
export function normalizeFsPath(p: string): string {
    const resolved = path.resolve(p);
    return process.platform === 'win32' ? resolved.toLowerCase() : resolved;
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
    NAMESPACE,
    UNIT,
    OTHER_SOURCES,
}

/**
 * The mode of the Project View, which determines how the project is displayed.
 * The "files" mode shows the project as a tree of files and directories,
 * while the "units" mode shows the project as a hierarchy of units.
 */
export enum ProjectViewMode {
    FILES = 'files',
    UNIT_HIERARCHY = 'units',
}

/**
 * The kind of a project view item that represents a file or directory in the project.
 * This is a subset of ProjectViewItemKind that only includes items that correspond
 * to actual files or directories in the project.
 */
export type FileViewItemKind =
    | ProjectViewItemKind.ROOT_PROJECT
    | ProjectViewItemKind.SUB_PROJECT
    | ProjectViewItemKind.SOURCE_DIRECTORY
    | ProjectViewItemKind.SOURCE_FILE
    | ProjectViewItemKind.OBJECT_DIRECTORY
    | ProjectViewItemKind.RUNTIME_PROJECT;

/**
 * A logical grouping in the unit hierarchy. These nodes do not refer to files.
 */
export class UnitViewItem extends vscode.TreeItem {
    declare id: string;

    constructor(
        public label: string,
        public readonly itemKind:
            | ProjectViewItemKind.NAMESPACE
            | ProjectViewItemKind.UNIT
            | ProjectViewItemKind.OTHER_SOURCES,
        id: string,
    ) {
        super(label, vscode.TreeItemCollapsibleState.Collapsed);
        this.id = id;
        this.contextValue = 'unitHierarchy';
    }
}

export type ProjectViewNode = ProjectViewItem | UnitViewItem;

/**
 * Operations shared by the two Project View layouts.
 * Data is supplied by ExtensionState; building the tree makes no ALS requests.
 */
export interface ProjectViewTree<T extends ProjectViewNode = ProjectViewNode> {
    getChildren(parent?: T): T[];
    getParent(node: T): T | undefined;
    findSourceFileItem(uri: vscode.Uri, position?: vscode.Position): ProjectViewItem | undefined;
}

/**
 * A tree item for the Project View
 */
export class ProjectViewItem extends vscode.TreeItem {
    declare id: string;

    /** The kind of the project view item */
    public itemKind: FileViewItemKind;

    /** The URI of the item */
    public uri: vscode.Uri;

    /** For project items: the project id used to look up children */
    public projectId?: string;

    /** The id of the parent project, used for stable unique ID generation */
    public parentProjectId?: string;

    /** For source directory items: the source files belonging to this directory */
    public sources?: ProjectSourceInfo[];

    /** For source file items: the normalized directory path of the source file,
     * as reported by the server. Used by `getParent` to locate the parent
     * SOURCE_DIRECTORY item without recomputing it. */
    public sourceDirectory?: string;

    constructor(
        public label: string,
        public collapsibleState: vscode.TreeItemCollapsibleState = vscode.TreeItemCollapsibleState
            .None,
        itemKind: FileViewItemKind = ProjectViewItemKind.SUB_PROJECT,
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
        itemKind: FileViewItemKind,
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
 * Creates a source file item using the active file icon theme.
 * Both layouts use this helper; unit parts can supply a navigation range.
 */
export function createSourceFileItem(
    source: ProjectSourceInfo,
    projectId: string | undefined,
    selection?: vscode.Range,
): ProjectViewItem {
    const uri = vscode.Uri.file(source.file_name);
    const item = new ProjectViewItem(
        source.simple_name,
        vscode.TreeItemCollapsibleState.None,
        ProjectViewItemKind.SOURCE_FILE,
        uri,
        undefined,
        projectId,
    );
    item.sourceDirectory = normalizeFsPath(source.directory);
    if (selection) {
        item.command = {
            command: 'vscode.open',
            title: 'Open File',
            arguments: [uri, { selection }],
        };
    }
    return item;
}

/**
 * Creates a source directory item with its source files attached.
 * The full directory path distinguishes folders with the same base name.
 */
export function createSourceDirectoryItem(
    directory: string,
    sources: ProjectSourceInfo[],
    projectId: string,
): ProjectViewItem {
    const item = new ProjectViewItem(
        path.basename(directory),
        vscode.TreeItemCollapsibleState.Collapsed,
        ProjectViewItemKind.SOURCE_DIRECTORY,
        vscode.Uri.file(directory),
        undefined,
        projectId,
    );
    item.sources = sources;
    item.description = directory;
    return item;
}

/**
 * Groups a project entry's source files by their normalized directory path
 * and unions the result with the project's declared source directories
 * (which may be empty).
 * Keys are normalized via `path.resolve` so that the
 * trailing separators and other path variations are handled correctly on all
 * platforms.
 */
export function buildSourcesByDir(
    sources: readonly ProjectSourceInfo[],
    directories: readonly string[] = [],
): Map<string, ProjectSourceInfo[]> {
    const sourcesByDir = new Map<string, ProjectSourceInfo[]>();

    // Normalize directory keys via normalizeFsPath to remove any trailing
    // separators and ensure consistent formatting across platforms.
    // On Windows this also lowercases the drive letter to avoid mismatches
    // between paths from VS Code (lowercase drive) and the Ada server (uppercase drive).
    const normalizeDir = (dir: string): string => normalizeFsPath(dir);

    // First group sources by their directory path
    for (const source of sources) {
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
    for (const dir of directories) {
        const key = normalizeDir(dir);
        if (!sourcesByDir.has(key)) {
            sourcesByDir.set(key, []);
        }
    }
    return sourcesByDir;
}
