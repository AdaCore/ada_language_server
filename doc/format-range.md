# Format range

## Short introduction

This extension reexports "format selection" request as a command,
so that completion is able to trigger it.
The execution of this command applies "format selection" edits to the document.

## Capabilities

The `initialize` respond lists the `als-format-range` command in the list of supported commands.

## Change description

We introduce `als-format-range` command with parameters:

```typescript
interface AlsFormatRangeParams {
	/**
	 * The document to format.
	 */
	textDocument: TextDocumentIdentifier;

	/**
	 * The range to format
	 */
	range: Range;
}
```
