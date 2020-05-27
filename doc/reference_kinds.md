# Reference kinds

## Short introduction

This feature allows the LSP server to provide reference kinds in
results for the `textDocument/references` request. A reference can
be 'write', 'parent', 'dispatching call', etc.

## Change description

We extend the result of `textDocument/references` by adding an
extra field to the `Location` type:

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
}
```

### See also

[documentHighlight] request also provides `Read` and `Write` markers to highlight
the code on the fly.

[documentHighlight]: https://microsoft.github.io/language-server-protocol/specification#textDocument_documentHighlight

