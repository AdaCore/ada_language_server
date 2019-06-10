# Reference kinds

## Short introduction

This feature allows the LSP server to provide reference kinds in
results for the `textDocument/references` request. A reference can
be 'write', 'static call', 'dispatching call', etc.

## Change description

We extend the result of `textDocument/references` by adding an
extra field to the `Location` type:

```typescript

export type AlsReferenceKind = 'w' | 'c' | 'd';

export namespace AlsReferenceKind {
   export const Write            : AlsReferenceKind = 'w';
   export const Static_Call      : AlsReferenceKind = 'c';
   export const Dispatching_Call : AlsReferenceKind = 'd';
}

interface Location {
	uri: DocumentUri;
	range: Range;
        alsKind?: AlsReferenceKind[];
}
```

### See also

[documentHighlight] request also provides `Read` and `Write` markers to highlight
the code on the fly.

[documentHighlight]: https://microsoft.github.io/language-server-protocol/specification#textDocument_documentHighlight

