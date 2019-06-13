# Reference kinds

## Short introduction

This implements the functionality 'is called by', allowing to query
all the calls to a function, organized by the entities where these
calls happen.

## Capabilities

The `initialize` request returns a boolean `alsCalledByProvider` as part of
the `capabilities`, set to true if the server supports this functionality.

## Change description

We introduce a new type to represent the results:

```typescript

interface ALSSubprogramAndReferences {

   loc: Location; /* The location of the entity containing the references */
   name: string; /* The name of the entity containing the references */

   refs: Location[];  /* The references contained in this entity */
}
```

And a new request:

  method: `textDocument/ALS_called_by`
  params: `TextDocumentPositionParams`

Returning the references to the method identified at the given position:

  result: `ALSSubprogramAndReferences[]`

We also introduce a new boolean field `ALS_calledbyProvider` in the
interface `ServerCapabilities` indicating whether the server supports
this extension.
