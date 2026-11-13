# Reference kinds

## Short introduction

This feature allows the LSP server to provide reference kinds in
results for the `textDocument/references` request. A reference can
be 'write', 'parent', 'dispatching call', etc.

## Change description

We extend the result of `textDocument/references` by adding an
extra field to the `Location` type:
  alsKind field represents the reference kind
  hidden field is set to True if the location is from the hidden source file (extended project)

```typescript

export type AlsReferenceKind = 'write' | 'access' | 'call' | 'dispatching call' | 'parent' | 'child';

export namespace AlsReferenceKind {
   export const Write            : AlsReferenceKind = 'write';
   export const Access           : AlsReferenceKind = 'access';
   export const Static_Call      : AlsReferenceKind = 'call';
   export const Dispatching_Call : AlsReferenceKind = 'dispatching call';
   export const Parent           : AlsReferenceKind = 'parent';
   export const Child            : AlsReferenceKind = 'child';
}

interface Location {
	uri: DocumentUri;
	range: Range;
        alsKind?: AlsReferenceKind[];
        hidden?: boolean;
}
```

### See also

[documentHighlight] request also provides `Read` and `Write` markers to highlight
the code on the fly.

[documentHighlight]: https://microsoft.github.io/language-server-protocol/specification#textDocument_documentHighlight

## VS Code

In Ada files, Shift+F12 and **Ada: Find All References** in the Command Palette
show reference kinds in VS Code's existing References view. Results are grouped
by file and show labels such as `read`, `write`, and `call`. Plain `reference`
occurrences are displayed as `read`, following the document highlight convention.
The declaration is excluded from this search.

F4 and Shift+F4 navigate between results. The view's Refresh and history actions
rerun the Ada search and preserve the kind labels.

VS Code's built-in context-menu command and Peek References keep their standard
presentation.

