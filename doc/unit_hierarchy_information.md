# Unit Hierarchy Information

## Short introduction

This custom command returns Ada compilation units and their source files across
the loaded GPR project tree. Clients can use the result to build a unit
hierarchy without grouping units by project or source directory.

## Change description

The command takes no arguments and is executed through `workspace/executeCommand`:

```json
{
    "command": "als-unit-hierarchy-information",
    "arguments": []
}
```

The command is advertised in `executeCommandProvider.commands` in the
`initialize` response. It returns `null` when no project tree is available.
Otherwise, it returns `UnitHierarchyInformation`:

```typescript
export interface UnitHierarchyProject {
    id: string;
    name: string;
    uri?: DocumentUri;
    "is-runtime": boolean;
    "is-externally-built": boolean;
}

export interface UnitHierarchyPart {
    kind: "specification" | "body" | "separate" | "unknown";
    uri: DocumentUri;
    "unit-index": number;
    project: UnitHierarchyProject;
    "separate-name"?: string;
    range?: Range;
    "selection-range"?: Range;
}

export interface UnitHierarchyUnit {
    "namespace-id": string;
    name: string;
    "simple-name": string;
    "parent-name": string;
    kind: "package" | "subprogram" | "unknown";
    "is-generic"?: boolean;
    "is-instantiation"?: boolean;
    "is-renaming"?: boolean;
    parts: UnitHierarchyPart[];
}

export interface UnitHierarchyInformation {
    "root-project": UnitHierarchyProject;
    namespaces: UnitHierarchyProject[];
    units: UnitHierarchyUnit[];
}

export type UnitHierarchyResponse = UnitHierarchyInformation | null;
```

`DocumentUri` and `Range` are the standard LSP types.

### Units and namespaces

`root-project` describes the loaded root project. `namespaces` lists the project
views that define separate compilation namespaces. An aggregate project can
contain several namespaces, each with its own definition of the same unit name.
Project IDs are opaque GPR2 view IDs.

Each unit is identified by its `namespace-id` and qualified `name`. Its
`namespace-id` refers to an entry in `namespaces`. Shared dependencies can appear
in more than one namespace, so clients should not merge definitions by name alone.

Names use GPR2's canonical casing. For a unit named `A.B.C`, `simple-name` is `C`
and `parent-name` is `A.B`. Root units have an empty `parent-name`. A parent can
be missing from `units` if its source is unavailable; clients can still create
the corresponding parent node from its name.

The list follows GPR2's compilation-unit model. It includes packages and library
subprograms, including generic declarations, instantiations, and renamings.
Nested package declarations are not listed. The declaration flags distinguish
generic units, instances, and renamings without changing their package or
subprogram `kind`.

### Source parts and navigation

`parts` contains the unit's specification, body, and separates when available.
Each part has its own source URI and owning project. GPR2 selects the visible
parts according to the loaded project, including custom naming rules, project
extensions, and runtime overrides.

`unit-index` is zero for a single-unit source. For multi-unit sources, it is the
one-based position of the compilation unit within the file. Several parts can
therefore have the same URI and different indices and ranges.

For a separate, `separate-name` is its name relative to the containing library
unit. Separates are returned as parts of that unit rather than as nested package
declarations.

`range` covers the declaration, and `selection-range` covers its defining name.
Both use zero-based LSP lines and UTF-16 columns, with an excluded end position.
Clients can use the selection range to navigate directly to a declaration,
including declarations in multi-unit files.

If a declaration cannot be identified, `kind` is `unknown`. Declaration flags
and navigation ranges may be absent, but the source and project metadata remain
available. Parser recovery can still provide ranges for malformed source files.

Runtime and externally built units are included. Clients can identify or hide
them using the `is-runtime` and `is-externally-built` flags on each part's project.
A project URI is omitted when the project view has no project file.
