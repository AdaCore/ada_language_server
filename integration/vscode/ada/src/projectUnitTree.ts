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
import type { Protocol2CodeConverter } from 'vscode-languageclient';
import {
    ProjectSourceInfo,
    ProjectViewInformation,
    UnitHierarchyInformation,
    UnitHierarchyPart,
    UnitHierarchyUnit,
} from './projectViewData';
import {
    buildSourcesByDir,
    createSourceDirectoryItem,
    createSourceFileItem,
    normalizeFsPath,
    ProjectViewItem,
    ProjectViewItemKind,
    ProjectViewNode,
    ProjectViewTree,
    UnitViewItem,
} from './projectViewTree';

/**
 * Builds the library unit layout for the Project View.
 * Units are grouped by their qualified names, independently of GPR ownership.
 * Aggregate namespaces remain separate because they can define the same name.
 */
export class ProjectUnitTree implements ProjectViewTree {
    private roots: ProjectViewNode[] = [];
    private readonly children = new Map<string, ProjectViewNode[]>();
    private readonly parents = new Map<string, ProjectViewNode>();
    private readonly searchText = new Map<string, string>();
    private readonly fileItems = new Map<
        string,
        { item: ProjectViewItem; range?: vscode.Range }[]
    >();

    /**
     * @param converter - The language client's converter for source ranges used
     * to open unit parts and reveal the unit at the editor position.
     */
    constructor(private readonly converter: Protocol2CodeConverter) {}

    /**
     * Rebuilds the tree from a pair of snapshots belonging to the same project.
     * Filtering and runtime visibility are applied before indexing visible files.
     */
    setInformation(
        projects: ProjectViewInformation | undefined,
        hierarchy: UnitHierarchyInformation | undefined,
        showRuntime: boolean,
        filter: string,
    ): void {
        this.roots = [];
        this.children.clear();
        this.parents.clear();
        this.searchText.clear();
        this.fileItems.clear();
        if (!projects || !hierarchy) return;

        const units = new Map<string, UnitViewItem>();
        const namespaces = new Map<string, UnitViewItem>();
        const ranges = new Map<string, vscode.Range>();
        const multipleNamespaces = hierarchy.namespaces.length > 1;

        for (const namespace of hierarchy.namespaces) {
            const node = new UnitViewItem(
                namespace.name,
                ProjectViewItemKind.NAMESPACE,
                JSON.stringify(['namespace', namespace.id]),
            );
            node.iconPath = new vscode.ThemeIcon('project');
            node.tooltip = namespace.uri ? vscode.Uri.parse(namespace.uri).fsPath : namespace.name;
            namespaces.set(namespace.id, node);
        }

        // Create parents even when their source is unavailable. The namespace
        // is part of the key, so equal names in aggregate projects stay distinct.
        const ensureUnit = (namespace: string, name: string): UnitViewItem => {
            const key = JSON.stringify(['unit', namespace, name]);
            const existing = units.get(key);
            if (existing) return existing;

            const dot = name.lastIndexOf('.');
            const node = new UnitViewItem(name.slice(dot + 1), ProjectViewItemKind.UNIT, key);
            node.iconPath = new vscode.ThemeIcon('symbol-namespace');
            node.tooltip = `${name} (source unavailable)`;
            this.searchText.set(key, name.toLowerCase());
            units.set(key, node);

            const parent =
                dot >= 0
                    ? ensureUnit(namespace, name.slice(0, dot))
                    : multipleNamespaces
                      ? namespaces.get(namespace)
                      : undefined;
            this.attach(node, parent);
            return node;
        };

        for (const unit of hierarchy.units) {
            const parts = unit.parts.filter((part) => showRuntime || !part.project['is-runtime']);
            if (parts.length === 0) continue;

            const node = ensureUnit(unit['namespace-id'], unit.name);
            this.describeUnit(node, unit);
            for (const part of parts) {
                const item = this.createPart(unit, part);
                const range = this.converter.asRange(part.range);
                if (range) ranges.set(item.id, range);
                this.attach(item, node);
            }
        }

        // Namespace nodes are only useful when they contain visible units.
        // A single namespace has no wrapper, keeping the common case compact.
        if (multipleNamespaces) {
            this.roots.unshift(
                ...[...namespaces.values()].filter(
                    (node) => (this.children.get(node.id)?.length ?? 0) > 0,
                ),
            );
        }
        this.addOtherSources(projects, hierarchy, showRuntime);

        // Sort once when rebuilding, rather than on every expansion. Source
        // parts precede child units; specifications precede bodies and separates.
        for (const nodes of [this.roots, ...this.children.values()]) {
            nodes.sort((left, right) => {
                const rank = (node: ProjectViewNode) => {
                    if (node.itemKind === ProjectViewItemKind.OTHER_SOURCES) return 5;
                    if (!(node instanceof ProjectViewItem)) return 4;
                    return node.label === 'Specification' ? 0 : node.label === 'Body' ? 1 : 2;
                };
                return (
                    rank(left) - rank(right) ||
                    String(left.label).localeCompare(String(right.label))
                );
            });
        }

        // Keep ancestors of matches. A matching unit shows all its source parts
        // and children, so searching for a qualified name still gives context.
        const retain = (node: ProjectViewNode, inheritedMatch = false): boolean => {
            const ownMatch =
                inheritedMatch ||
                !filter ||
                (this.searchText.get(node.id) ?? String(node.label).toLowerCase()).includes(filter);
            const children = (this.children.get(node.id) ?? []).filter((child) =>
                retain(child, ownMatch),
            );
            this.children.set(node.id, children);
            if (children.length === 0) node.collapsibleState = vscode.TreeItemCollapsibleState.None;
            return ownMatch || children.length > 0;
        };
        this.roots = this.roots.filter((node) => retain(node));

        // Only visible leaves are indexed, so Reveal cannot select an item
        // hidden by the runtime setting or by the current filter.
        const index = (node: ProjectViewNode): void => {
            if (node.itemKind === ProjectViewItemKind.SOURCE_FILE) {
                const key = normalizeFsPath(node.uri.fsPath);
                const items = this.fileItems.get(key) ?? [];
                items.push({ item: node, range: ranges.get(node.id) });
                this.fileItems.set(key, items);
            }
            for (const child of this.children.get(node.id) ?? []) index(child);
        };
        for (const node of this.roots) index(node);
    }

    getChildren(parent?: ProjectViewNode): ProjectViewNode[] {
        return parent ? (this.children.get(parent.id) ?? []) : this.roots;
    }

    getParent(node: ProjectViewNode): ProjectViewNode | undefined {
        return this.parents.get(node.id);
    }

    /**
     * Prefer the unit containing the editor position in a multi-unit source.
     * Without a position, select the first visible occurrence of the file.
     */
    findSourceFileItem(uri: vscode.Uri, position?: vscode.Position): ProjectViewItem | undefined {
        const items = this.fileItems.get(normalizeFsPath(uri.fsPath)) ?? [];
        const containing = position
            ? items.find(
                  ({ range }) =>
                      range && !position.isBefore(range.start) && position.isBefore(range.end),
              )
            : undefined;
        return containing?.item ?? items[0]?.item;
    }

    /**
     * Adds a node and remembers its parent for TreeView.reveal().
     */
    private attach(node: ProjectViewNode, parent?: ProjectViewNode): void {
        if (parent) {
            const children = this.children.get(parent.id) ?? [];
            children.push(node);
            this.children.set(parent.id, children);
            this.parents.set(node.id, parent);
        } else {
            this.roots.push(node);
        }
    }

    private describeUnit(node: UnitViewItem, unit: UnitHierarchyUnit): void {
        // Labels use declaration casing; tree keys keep the canonical name.
        node.label = unit['simple-name'];
        node.iconPath = new vscode.ThemeIcon(
            unit.kind === 'package'
                ? 'package'
                : unit.kind === 'subprogram'
                  ? 'symbol-function'
                  : 'symbol-namespace',
        );
        const flags = [
            unit['is-generic'] ? 'generic' : '',
            unit['is-instantiation'] ? 'instantiation' : '',
            unit['is-renaming'] ? 'renaming' : '',
        ].filter(Boolean);
        node.description = flags.join(', ');
        node.tooltip = `${unit.name} (${[...flags, unit.kind].join(' ')})`;
    }

    /**
     * Uses the source part's own URI and selection range. This also handles
     * custom naming schemes and multiple units sharing a source file.
     */
    private createPart(unit: UnitHierarchyUnit, part: UnitHierarchyPart): ProjectViewItem {
        const uri = vscode.Uri.parse(part.uri);
        const item = createSourceFileItem(
            {
                file_name: uri.fsPath,
                simple_name: path.basename(uri.fsPath),
                directory: path.dirname(uri.fsPath),
            },
            part.project.id,
            this.converter.asRange(part['selection-range'] ?? part.range),
        );
        item.id = JSON.stringify([
            'part',
            unit['namespace-id'],
            unit.name,
            part.kind,
            part.uri,
            part['unit-index'],
            part['separate-name'],
        ]);
        item.label =
            part.kind === 'specification'
                ? 'Specification'
                : part.kind === 'body'
                  ? 'Body'
                  : part.kind === 'separate'
                    ? `Separate ${part['separate-name'] ?? ''}`.trim()
                    : 'Source';
        item.description = path.basename(uri.fsPath);
        item.tooltip = `${uri.fsPath}\nProject: ${part.project.name}`;
        this.searchText.set(item.id, `${unit.name} ${item.label} ${uri.fsPath}`.toLowerCase());
        return item;
    }

    /**
     * Keeps sources without a library unit accessible, including other languages
     * and sources whose language metadata is missing. Shared files appear once.
     */
    private addOtherSources(
        projects: ProjectViewInformation,
        hierarchy: UnitHierarchyInformation,
        showRuntime: boolean,
    ): void {
        const knownFiles = new Set(
            hierarchy.units.flatMap((unit) =>
                unit.parts.map((part) => normalizeFsPath(vscode.Uri.parse(part.uri).fsPath)),
            ),
        );
        const sources: ProjectSourceInfo[] = [];
        const entries = [...projects.projects.values()];
        if (showRuntime && projects.runtime_project) entries.push(projects.runtime_project);
        for (const entry of entries) {
            for (const source of entry.sources) {
                const key = normalizeFsPath(source.file_name);
                if (knownFiles.has(key)) continue;
                knownFiles.add(key);
                sources.push(source);
            }
        }
        if (sources.length === 0) return;

        const group = new UnitViewItem(
            'Other Sources',
            ProjectViewItemKind.OTHER_SOURCES,
            'units:other',
        );
        group.iconPath = new vscode.ThemeIcon('files');
        this.attach(group);
        for (const [directory, files] of buildSourcesByDir(sources)) {
            const node = createSourceDirectoryItem(directory, files, 'units:other');
            this.searchText.set(node.id, directory.toLowerCase());
            this.attach(node, group);
            for (const source of files) {
                const item = createSourceFileItem(source, 'units:other');
                this.searchText.set(item.id, source.file_name.toLowerCase());
                this.attach(item, node);
            }
        }
    }
}
