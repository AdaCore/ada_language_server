# GPR dependencies

## Short introduction

This implements the functionality 'show gpr dependencies', allowing to
query all the dependencies of a given GPR file.

This request returns an array of items, each one containing a URI
to another GPR file and the kind of dependency that binds them together.

The dependencies are listed in the `ALS_GPR_DependencyKind` enum below.

You can control whether you get the GPR files that depend on the
requested GPR file or the requested GPR file's own dependencies.

## Capabilities

The `initialize` request returns `als-gpr-dependencies` in the list of
supported commands if the server supports this functionality.

## Change description

We introduce four new types to represent the request parameters and results:

```typescript
export enum ALS_GPR_DependencyKind {
   AGGREGATED,
   EXTENDED,
   EXTENDING,
   IMPORTED,
}

export enum ALS_GprDependencyDirection {
   SHOW_OUTGOING= 1 /* Get the outgoing dependencies */,
   SHOW_INCOMING = 2 /* Get the incoming dependencies */,
}

interface ALS_GPR_DependenciesParams {
   uri: DocumentUri /* The queried unit */;
   direction: ALS_GprDependencyDirection /* The direction in which to fetch the dependencies */;
}

export type ALS_GPR_DependencyItem = {
   uri: DocumentUri /* The dependency's gpr file */;
   kind: ALS_GPRDependencyKind /* The dependency kind */;
};
```

And a new command `als-gpr-dependencies`:

```json
    method: `workspace/executeCommand`
    "params": {
       "command": "als-gpr-dependencies",
       "arguments": [
          {
            "uri": "default.gpr",
            "direction": 1
          }
       ]
    }
```

It returns list of `ALS_GPR_DependencyItem`:

result: `ALS_GPR_DependencyItem[]`
