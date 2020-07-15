# Calls

## Short introduction

This implements the functionality 'calls', allowing to query all the
calls made by a function, organized by the entities where these calls
happen.

Currently, there is no support for tasks.

## Capabilities

The `initialize` request returns a boolean `alsCallsProvider` as part of
the `capabilities`, set to true if the server supports this functionality.

## Change description

To represent the results the ```ALSSubprogramAndReferences``` type is used,
which was intruduced by the functionality [is called by](called_by.md).

And a new request:

  method: `textDocument/alsCalls`
  params: `TextDocumentPositionParams`

Returning the references to the method identified at the given position:

  result: `ALSSubprogramAndReferences[]`

We also introduce a new boolean field `alsCallsProvider` in the
interface `ServerCapabilities` indicating whether the server supports
this extension.
