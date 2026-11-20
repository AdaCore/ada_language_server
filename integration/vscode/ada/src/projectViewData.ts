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

import type { Range } from 'vscode-languageclient';

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
    'limited-imports'?: string[];
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
            //  Fold limited imports into import view
            imports: (rawEntry.imports ?? []).concat(rawEntry['limited-imports'] ?? []),
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
                language: s.language,
            })),
        };
    }

    return {
        root_project_id: rootId,
        projects,
        runtime_project,
    };
}

// ---------------------------------------------------------------------------
// Unit hierarchy types – match the JSON field names returned by the server
// ---------------------------------------------------------------------------

/**
 * A project owning a source part, or defining a compilation namespace.
 */
export interface UnitHierarchyProject {
    id: string;
    name: string;
    uri?: string;
    'is-runtime': boolean;
    'is-externally-built': boolean;
}

/**
 * A specification, body or separate, including its navigation ranges.
 * Several parts can share a URI when a source file contains multiple units.
 */
export interface UnitHierarchyPart {
    kind: 'specification' | 'body' | 'separate' | 'unknown';
    uri: string;
    'unit-index': number;
    project: UnitHierarchyProject;
    'separate-name'?: string;
    range?: Range;
    'selection-range'?: Range;
}

/**
 * A library unit identified by its namespace and qualified name.
 * Nested declarations are not part of the library unit hierarchy.
 */
export interface UnitHierarchyUnit {
    'namespace-id': string;
    name: string;
    /** Declaration casing when available, otherwise the canonical simple name. */
    'simple-name': string;
    'parent-name': string;
    kind: 'package' | 'subprogram' | 'unknown';
    'is-generic'?: boolean;
    'is-instantiation'?: boolean;
    'is-renaming'?: boolean;
    parts: UnitHierarchyPart[];
}

export interface UnitHierarchyInformation {
    'root-project': UnitHierarchyProject;
    namespaces: UnitHierarchyProject[];
    units: UnitHierarchyUnit[];
}
