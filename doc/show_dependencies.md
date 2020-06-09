# Show dependencies

## Short introduction

This implements the functionality 'show dependencies', allowing to query
all the dependencies of a given unit or to query all the units that import
a given unit.

## Capabilities

The `initialize` request returns a boolean `alsShowDepsProvider` as part of
the `capabilities`, set to true if the server supports this functionality.

## Change description

We introduce three new types to represent the request parameters and results:

```typescript

export namespace ALS_ShowDependenciesKind {
   export const Show_Imported = 1;
   export const Show_Importing = 2;
}

interface ALS_ShowDependenciesParams {
   textDocument : TextDocumentIdentifier; /* The queried unit */
   kind         : ALS_ShowDependenciesKind; /* The dependencies query kind */
   showImplicit : boolean; /* True if implicit dependencies should be returned */
}

interface ALS_Unit_Description {
   uri        : DocumentUri; /* The dependency unit's file */
   projectUri : DocumentUri; /* The dependency's project file */
}
```

And a new request:

  method: `textDocument/alsShowDependencies`
  params: `ALS_ShowDependenciesParams`

Returning the references to the method identified at the given position:

  result: `ALS_Unit_Description[]`

We also introduce a new boolean field `ALS_showDepsProvider` in the
interface `ServerCapabilities` indicating whether the server supports
this extension.
