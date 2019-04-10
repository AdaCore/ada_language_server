# Reference kinds

## Short introduction

This feature allows the LSP server to provide reference kinds in
results for the `textDocument/references` request. A reference can
be 'read', 'write', 'call', etc.

## Change description

We extend the result of `textDocument/references` by adding an
extra field to the `Location` type:

```typescript

export type AlsReferenceKind = 'read' | 'write' | 'call';

export namespace AlsReferenceKind {
   export const Read  : AlsReferenceKind = 'read';
   export const Write : AlsReferenceKind = 'write';
   export const Call  : AlsReferenceKind = 'call';
}

interface Location {
	uri: DocumentUri;
	range: Range;
        AlsKind?: AlsReferenceKind;
}
```

### See also

[documentHighlight] request also provides `Read` and `Write` markers to highlight
the code on the fly.

[documentHighlight]: https://microsoft.github.io/language-server-protocol/specification#textDocument_documentHighlight

