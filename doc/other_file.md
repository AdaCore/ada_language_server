# Other file

## Short introduction

New command to switch between specification and body documents.

## Change description

We introduce a new command (`als-other-file`). It takes `TextDocumentIdentifier`
as parameter and returns nothing. On execution it finds other Ada file and
issues `ShowDocument` request to the client.

VS Code extension wraps this with another parameter-less command `ada.otherFile`.
It checks if an active editor exists and triggers the LSP `als-other-file` command.