# Show dependencies

## Short introduction

This implements the functionality 'show dependencies', allowing to query
all the dependencies of a given unit or to query all the units that import
a given unit.

## Capabilities

The `initialize` request returns `als-show-dependencies` in the list of
supported commands if the server supports this functionality.

## Change description

We introduce three new types to represent the request parameters and results:

```typescript

export namespace ALS_ShowDependenciesKind {
   export const Show_Imported = 1;
   export const Show_Importing = 2;
}

interface ALS_ShowDependenciesParams {
   uri          : DocumentUri; /* The queried unit */
   kind         : ALS_ShowDependenciesKind; /* The dependencies query kind */
   showImplicit : boolean; /* True if implicit dependencies should be returned */
}

interface ALS_Unit_Description {
   uri        : DocumentUri; /* The dependency unit's file */
   projectUri : DocumentUri; /* The dependency's project file */
}
```

And a new command `als-show-dependencies`:

    method: `workspace/executeCommand`
    "params": {
       "command": "als-show-dependencies",
       "arguments": [
          <ALS_ShowDependenciesParams>
       ]
    }

It returns list of `ALS_Unit_Description`:

  result: `ALS_Unit_Description[]`
