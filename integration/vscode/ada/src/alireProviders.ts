import {
    TomlText,
    Parser,
    PropertyInfo,
    PropertyDb,
    TypeOptions,
    resolvePath,
} from './alireParser';
import * as vscode from 'vscode';
import { logger } from './extension';

/** Select only local manifest files */
export const AlireTomlSelector = {
    language: 'toml',
    pattern: '**/alire.toml',
};

/** Differences between kinds of manifest that the schema does not capture */
export interface ManifestProfile {
    /** Top-level keys not offered as completions */
    excluded: string[];
    /** Top-level keys required on top of those the schema requires */
    required: string[];
    /** Top-level keys the schema requires but this kind of manifest does not */
    optional: string[];
}

/** Workspace manifests (alire.toml): 'origin' is forbidden */
export const WorkspaceProfile: ManifestProfile = {
    excluded: ['origin'],
    required: [],
    optional: [],
};

/** Index manifests (<crate>-<version>.toml) */
export const IndexProfile: ManifestProfile = {
    excluded: [],
    required: ['maintainers', 'maintainers-logins', 'licenses', 'origin'],
    optional: [],
};

/** Index files that only define external releases */
export const ExternalsProfile: ManifestProfile = {
    excluded: ['origin'],
    required: ['external'],
    optional: ['version'],
};

/** Schema properties, adjusted for one kind of manifest */
class AlireManifestProvider {
    public readonly properties: PropertyDb = new PropertyDb();
    public readonly profile: ManifestProfile;

    constructor(properties: PropertyDb, profile: ManifestProfile) {
        this.profile = profile;
        for (const [key, prop] of properties) {
            const required =
                (prop.required || profile.required.includes(key)) &&
                !profile.optional.includes(key);
            this.properties.set(
                key,
                new PropertyInfo(prop.title, prop.description, prop.typeData, required),
            );
        }
    }

    /** Property at a dotted path, e.g. ['configuration', 'disabled'] */
    protected lookup(path: string[]): PropertyInfo | undefined {
        if (path.length === 1) return this.properties.get(path[0]);
        const info = resolvePath(this.properties, path);
        return info && PropertyInfo.fromType(info, path.slice(0, -1));
    }
}

export type AlireCompletionItem = vscode.CompletionItem;
export class AlireCompletionProvider
    extends AlireManifestProvider
    implements vscode.CompletionItemProvider<AlireCompletionItem>
{
    /* Top-level keys offered as completions */
    public readonly labels: string[];

    constructor(properties: PropertyDb, profile: ManifestProfile = WorkspaceProfile) {
        super(properties, profile);
        this.labels = [...properties.keys()].filter((k) => !profile.excluded.includes(k));
    }

    /** Top-level keys not yet set, excluding tables */
    private fillFields(existingKeys: string[]): AlireCompletionItem[] {
        return this.labels.flatMap((key) => {
            const property = this.properties.get(key);
            if (!property || property.isTable() || existingKeys.includes(key)) return [];
            return property.toCompletionItem();
        });
    }
    /** Tables, except [singletons] already defined; items replace typed brackets */
    private fillTables(
        doc: vscode.TextDocument,
        line: vscode.TextLine,
        pos: vscode.Position,
    ): AlireCompletionItem[] {
        const defined = TomlText.definedTables(doc, pos.line);
        const range = new vscode.Range(
            line.lineNumber,
            line.firstNonWhitespaceCharacterIndex,
            pos.line,
            pos.character,
        );
        const typed = line.text.substring(range.start.character, pos.character);
        return this.labels.flatMap((prop) => {
            const property = this.properties.get(prop);
            // Filter completions to only table/table list properties
            if (!property || !property.isTable()) return [];
            // Filter out singleton tables already configured
            if (!property.isTableList() && defined.has(prop)) return [];
            const item = property.toCompletionItem();
            item.range = range;
            // Match with brackets only if they are typed
            item.filterText = typed.startsWith('[') ? (item.insertText as string).trim() : prop;
            return item;
        });
    }

    /**
     * Top-level keys before the first table, else keys of the enclosing table,
     * and tables where one can start
     */
    provideCompletionItems(
        doc: vscode.TextDocument,
        position: vscode.Position,
        token: vscode.CancellationToken,
        // eslint-disable-next-line @typescript-eslint/no-unused-vars
        _context: vscode.CompletionContext,
    ): AlireCompletionItem[] {
        const pos = doc.validatePosition(position);
        const line = doc.lineAt(pos.line);
        /* Only complete the first word or table name of a line */
        if (
            token.isCancellationRequested ||
            TomlText.inComment(line) ||
            !TomlText.inFirstLabel(line)
        ) {
            return [];
        }

        /* Section of the line: before the first table, or a table's body */
        const table = TomlText.tableAt(doc, pos.line);
        const end = table ? table.end : TomlText.firstTableLine(doc);

        /* A new table here would capture any keys below it in this section */
        const keysBelow = TomlText.keysIn(doc, pos.line + 1, end).length > 0;

        /* A table header is being typed (the line then counts as a header) */
        if (line.text.trimStart().startsWith('[')) {
            return keysBelow ? [] : this.fillTables(doc, line, pos);
        }

        /* Top-level keys are only allowed before the first table */
        const start = table ? table.header + 1 : 0;
        const existingKeys = TomlText.keysIn(doc, start, end);
        const results = table
            ? this.fillChildren(table.path, existingKeys, TomlText.definedTables(doc, pos.line))
            : this.fillFields(existingKeys);
        if (keysBelow) {
            // Includes nested tables, e.g. [configuration.variables]
            return results.filter((item) => !(item.insertText as string).startsWith('['));
        }
        return results.concat(this.fillTables(doc, line, pos));
    }

    /** Keys of the table at 'path' not yet set, nor defined as nested tables */
    private fillChildren(
        path: string[],
        existingKeys: string[],
        defined: TypeOptions,
    ): AlireCompletionItem[] {
        const info = resolvePath(this.properties, path);
        if (!info) return [];
        // Skip children that are already configured
        return [...info.children.values()]
            .filter((child) => !existingKeys.includes(child.title))
            .filter(
                (child) =>
                    !(child.isNestedTable() && defined.has([...path, child.title].join('.'))),
            )
            .map((child) => PropertyInfo.fromType(child, path).toCompletionItem());
    }
}

/** Hover info for keys and table names, including excluded ones (e.g. 'origin') */
export class AlireHoverProvider extends AlireManifestProvider implements vscode.HoverProvider {
    constructor(properties: PropertyDb, profile: ManifestProfile = WorkspaceProfile) {
        super(properties, profile);
    }

    provideHover(
        document: vscode.TextDocument,
        position: vscode.Position,
        token: vscode.CancellationToken,
    ): vscode.Hover | null {
        const pos = document.validatePosition(position);
        if (token.isCancellationRequested) return null;

        const labelRgx = /\b\w[\w-]+\b/;
        const wr = document.getWordRangeAtPosition(pos, labelRgx);
        if (!wr || wr.isEmpty) return null;

        /* Only the key or table name at the start of the line */
        const line = document.lineAt(pos.line);
        if (!TomlText.validHoverKey(line, wr.start)) return null;

        /* Full word, so maintainers-logins is not taken for maintainers */
        const word = document.getText(wr).toLowerCase();

        let property: PropertyInfo | undefined;
        const header = TomlText.headerPath(line);
        if (header) {
            /* Table name, possibly nested: [a.b] */
            const idx = header.indexOf(word);
            property = idx < 0 ? undefined : this.lookup(header.slice(0, idx + 1));
        } else {
            /* Key of the enclosing table, or top-level key */
            const table = TomlText.tableAt(document, pos.line);
            property = this.lookup(table ? [...table.path, word] : [word]);
        }
        return property ? property.toHoverItem() : null;
    }
}

export function getAlireData(schemaPath: string): Parser | null {
    try {
        const p = Parser.init(schemaPath);
        if (!p) return null;
        return p;
    } catch (e) {
        const msg = e instanceof Error ? e.message : String(e);
        logger.error(`Failed to parse Alire specification. Reason: "${msg}"`);
        return null;
    }
}
