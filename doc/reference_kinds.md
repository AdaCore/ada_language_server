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

For the user workflow, see [Navigating references](vscode-ug.md#navigating-references)
in the VS Code user's guide.

In Ada files, Shift+F12 and **Ada: Find All References** in the Command Palette
show reference kinds in VS Code's existing References view. Results are grouped
by file and show labels such as `read`, `write`, and `call`. Plain `reference`
occurrences are displayed as `read`, following the document highlight convention.
The declaration is excluded from this search.

F4 and Shift+F4 navigate between results. The view's Refresh and history actions
rerun the Ada search and preserve the kind labels.

Use **Filter by Reference Kind...** (the funnel button in the References toolbar)
to show only writes, reads, or another kind. The menu shows counts for each kind
in the full results. The message above the results shows the selected kind and
how many references remain visible. Files without matches are hidden, and F4,
Shift+F4, and source highlights follow the filtered results.

When a filter is active, the outline funnel changes to a filled funnel in the
same toolbar position, as in the Project view. Click the filled funnel
(**Show All References**) to clear the filter. Changing a filter uses the
results already loaded; it does not request references again.
Refresh and history preserve the filter for that search. A new Shift+F12 search
starts with all references visible.

VS Code's built-in context-menu command and Peek References keep their standard
presentation.

