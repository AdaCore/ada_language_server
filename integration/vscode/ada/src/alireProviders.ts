import {
    FieldCompletion,
    Fields,
    Snippets,
    TomlText,
    SnippetCompletion,
    AlireCompletionItem,
} from './alireProperties';
import * as vscode from 'vscode';

/** Select only local manifest files */
export const AlireTomlSelector = {
    language: 'toml',
    pattern: '**/alire.toml',
};

export class AlireCompletionProvider implements vscode.CompletionItemProvider<AlireCompletionItem> {
    static readonly SnippetTrigger = ' ';

    /** Provide completions for top-level property keys */
    private fillFields(existingKeys: string[]): FieldCompletion[] {
        // Only consider top-level keys that are not present
        const filteredFields = Fields.filter((prop) => !existingKeys.includes(prop.title));
        const items = filteredFields.map((field) => field.toCompletionItem());
        return items;
    }

    /* Provide completions with snippets */
    private fillSnippets(existingKeys: string[]): SnippetCompletion[] {
        // Only consider top-level keys that are not present
        const results = Snippets.flatMap((snip) => {
            const matchedProperties = existingKeys.filter((k) =>
                snip.completions.map((c) => c.title).includes(k),
            );
            if (matchedProperties.length == snip.completions.length) {
                return [];
            } else if (matchedProperties.length == 0) {
                return snip.toCompletionItem();
            } else {
                // partial filter
                return snip.toPartialCompletionItem(matchedProperties);
            }
        });
        return results;
    }

    /**
     * Check TOML syntax to see if position allows top-level fields.
     * If so, we can insert individual fields or snippets.
     * Otherwise, dispatch to table provider methods
     */
    provideCompletionItems(
        doc: vscode.TextDocument,
        position: vscode.Position,
        token: vscode.CancellationToken,
        context: vscode.CompletionContext,
    ): AlireCompletionItem[] {
        let results: AlireCompletionItem[] = [];

        /* Check position data */
        const pos: vscode.Position = doc.validatePosition(position);
        const line: vscode.TextLine = doc.lineAt(pos.line);
        /* Return if inside a comment or not in the first word on the line */
        if (token.isCancellationRequested || !TomlText.allowCompletion(line)) {
            return results;
        }

        /* Check if position is before or after first table configured */
        const firstTableIdx: number = TomlText.getFirstTableIdx(doc);
        const tablePos = doc.positionAt(firstTableIdx);
        const beforeTable = firstTableIdx < 0 || pos.line < tablePos.line;

        /* TOML syntax does not allow top-level fields after the first table */
        if (beforeTable) {
            /* Scan existing top-level toml properties */

            const existingKeys: string[] = doc
                .getText()
                .slice(0, firstTableIdx)
                .split('\n') // Cleanup strings and compare in lowercase
                .flatMap(
                    (line) => line.match(TomlText.TOP_KEY_RGX)?.[0]?.trim().toLowerCase() ?? [],
                );

            const triggerChar = context.triggerCharacter;
            /* Suggest snippets first */
            if (triggerChar && triggerChar == AlireCompletionProvider.SnippetTrigger) {
                results = results.concat(this.fillSnippets(existingKeys));
            }
            results = results.concat(this.fillFields(existingKeys));
        }
        return results;
    }

    /**
     * Select completion item
     */
    resolveCompletionItem(item: AlireCompletionItem, token: vscode.CancellationToken) {
        if (token.isCancellationRequested) {
            return null;
        }
        return item;
    }
}
