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

import * as vscode from 'vscode';
import type { Protocol2CodeConverter } from 'vscode-languageclient';
import { ProjectViewInformation, UnitHierarchyInformation } from './projectViewData';
import { ProjectFileTree } from './projectFileTree';
import { ProjectUnitTree } from './projectUnitTree';
import { ProjectViewItem, ProjectViewMode, ProjectViewNode } from './projectViewTree';

// Keep the existing exports available to commands and extension clients.
export * from './projectViewData';
export { ProjectViewItem, ProjectViewItemKind, normalizeFsPath } from './projectViewTree';
export { ProjectViewDragAndDropController, PROJECT_VIEW_MIME_TYPE } from './projectViewDragAndDrop';

/**
 * Reads the Project View display preferences from the VS Code settings.
 *
 * These preferences are cached on the provider, so they must be re-read
 * whenever the corresponding settings change, be it through the View Options
 * quick-pick or directly through the VS Code Settings UI.
 */
function readViewSettingsFromConfig() {
    const config = vscode.workspace.getConfiguration('ada');
    return {
        mode: config.get<ProjectViewMode>('projectView.mode', ProjectViewMode.FILES),
        flatMode: config.get<boolean>('projectView.flatMode', false),
        showObjectDirs: config.get<boolean>('projectView.showObjectDirectories', false),
        showRuntimeFiles: config.get<boolean>('projectView.showRuntimeFiles', false),
    };
}

/**
 * Tree data provider for the Project View.
 * Each layout builds its own hierarchy from the data supplied by ExtensionState.
 */
export class ProjectViewProvider
    implements vscode.TreeDataProvider<ProjectViewNode>, vscode.Disposable
{
    private readonly _onDidChangeTreeData = new vscode.EventEmitter<void>();
    readonly onDidChangeTreeData = this._onDidChangeTreeData.event;
    private readonly _onDidChangeMode = new vscode.EventEmitter<ProjectViewMode>();
    readonly onDidChangeMode = this._onDidChangeMode.event;

    private readonly fileTree = new ProjectFileTree();
    private readonly unitTree: ProjectUnitTree;
    private projectViewInfo: ProjectViewInformation | undefined;
    private unitHierarchyInfo: UnitHierarchyInformation | undefined;
    private filterString = '';
    private currentMode: ProjectViewMode = ProjectViewMode.FILES;

    /** When true, all projects are shown as a flat list instead of a hierarchy */
    public flatMode = false;
    /** When true, object directories are shown as children of each project */
    public showObjectDirs = false;
    /** When true, the runtime project (if returned by the server) is shown */
    public showRuntimeFiles = false;

    /**
     * @param converter - The language client's converter, used by the unit tree
     * to turn LSP source ranges into VS Code navigation ranges.
     */
    constructor(converter: Protocol2CodeConverter) {
        this.unitTree = new ProjectUnitTree(converter);
        this.applyViewSettingsFromConfig();
    }

    get mode(): ProjectViewMode {
        return this.currentMode;
    }

    /**
     * Switches layouts without discarding either layout's data or preferences.
     * ExtensionState listens for this event to fetch unit information if needed.
     */
    setMode(mode: ProjectViewMode): void {
        if (mode === this.currentMode) return;
        this.currentMode = mode;
        this._onDidChangeTreeData.fire();
        this._onDidChangeMode.fire(mode);
    }

    /**
     * Sets the project view information and refreshes the tree.
     * Called by ExtensionState whenever the project is (re-)loaded.
     */
    setProjectViewInfo(info: ProjectViewInformation | undefined): void {
        this.projectViewInfo = info;
        this.fileTree.setProjectViewInfo(info);
        this.refreshUnitTree();
        this._onDidChangeTreeData.fire();
    }

    /**
     * Sets the unit information separately so file browsing remains available
     * while the ALS is collecting library units and navigation ranges.
     */
    setUnitHierarchyInfo(info: UnitHierarchyInformation | undefined): void {
        this.unitHierarchyInfo = info;
        this.refreshUnitTree();
        this._onDidChangeTreeData.fire();
    }

    /**
     * Retrieves the TreeItem representation of a given ProjectViewNode.
     * In this implementation, the ProjectViewNode itself is a TreeItem,
     * so it is returned directly.
     *
     * @param element - The ProjectViewNode for which to retrieve the TreeItem
     * @returns The TreeItem representation of the given ProjectViewNode
     */
    getTreeItem(element: ProjectViewNode): vscode.TreeItem {
        return element;
    }

    getChildren(element?: ProjectViewNode): ProjectViewNode[] {
        if (this.mode === ProjectViewMode.UNIT_HIERARCHY) return this.unitTree.getChildren(element);
        return !element || element instanceof ProjectViewItem
            ? this.fileTree.getChildren(element)
            : [];
    }

    getParent(element: ProjectViewNode): ProjectViewNode | undefined {
        if (this.mode === ProjectViewMode.UNIT_HIERARCHY) return this.unitTree.getParent(element);
        return element instanceof ProjectViewItem ? this.fileTree.getParent(element) : undefined;
    }

    findSourceFileItem(uri: vscode.Uri, position?: vscode.Position): ProjectViewItem | undefined {
        return this.mode === ProjectViewMode.UNIT_HIERARCHY
            ? this.unitTree.findSourceFileItem(uri, position)
            : this.fileTree.findSourceFileItem(uri);
    }

    /**
     * Sets a filter string for the Project View and refreshes it.
     * Each layout retains the ancestors needed to reach matching sources.
     */
    setFilter(filter: string): void {
        this.filterString = filter.toLowerCase();
        this.fileTree.setFilter(this.filterString);
        this.refreshUnitTree();
        this._onDidChangeTreeData.fire();
    }

    getFilter(): string {
        return this.filterString;
    }

    /**
     * Updates the display preferences, preserving file-only options in unit mode.
     */
    setViewSettings(flatMode: boolean, showObjectDirs: boolean, showRuntimeFiles: boolean): void {
        this.flatMode = flatMode;
        this.showObjectDirs = showObjectDirs;
        this.showRuntimeFiles = showRuntimeFiles;
        this.fileTree.setViewSettings(flatMode, showObjectDirs, showRuntimeFiles);
        this.refreshUnitTree();
        this._onDidChangeTreeData.fire();
    }

    /**
     * Re-reads the display preferences when settings change outside the picker.
     * This keeps the tree and 'Go to File in Project' in sync with the settings.
     */
    applyViewSettingsFromConfig(): void {
        const settings = readViewSettingsFromConfig();
        this.setViewSettings(settings.flatMode, settings.showObjectDirs, settings.showRuntimeFiles);
        this.setMode(
            settings.mode === ProjectViewMode.UNIT_HIERARCHY
                ? ProjectViewMode.UNIT_HIERARCHY
                : ProjectViewMode.FILES,
        );
    }

    private refreshUnitTree(): void {
        this.unitTree.setInformation(
            this.projectViewInfo,
            this.unitHierarchyInfo,
            this.showRuntimeFiles,
            this.filterString,
        );
    }

    dispose(): void {
        this._onDidChangeTreeData.dispose();
        this._onDidChangeMode.dispose();
    }
}
