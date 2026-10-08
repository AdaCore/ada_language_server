import * as vscode from 'vscode';
import * as yaml from 'yaml';
import { readFileSync } from 'fs';
import { logger } from './extension';

/** JSON Schema keywords */
const Keys = {
    ref: '$ref',
    defs: '$defs',
    defPfx: '#/$defs/',
    type: 'type',
    object: 'object',
    array: 'array',
    items: 'items',
    props: 'properties',
    patProps: 'patternProperties',
    oneOf: 'oneOf',
    allOf: 'allOf',
    if: 'if',
    then: 'then',
    not: 'not',
    required: 'required',
    default: 'default',
    description: 'description',
    pattern: 'pattern',
    table: 'table',
} as const;

class YamlNode extends Map<string, string | YamlNode | YamlNode[]> {}

export enum TomlType {
    SCALAR = 0,
    LIST,
    TABLE,
    TABLES,
}

export class TomlText {
    static readonly COMMENT_START = '#';
    static readonly TOP_KEY_RGX = /^\s*([\w-])+\s*/;
    static readonly FIRST_KEY_OR_TABLE = /^\s*\[{0,2}([\w'-])*\s*$/;
    // Only whitespace, or a table header's opening brackets and parent keys, may precede a key
    static readonly KEY_PREFIX = /^\s*(\[{1,2}\s*([\w'"-]+\s*\.\s*)*)?$/;
    static readonly HEADER_RGX = /^\s*\[{1,2}\s*([^\]]*?)\s*\]{0,2}\s*(#.*)?$/;

    /* True if first non-whitespace character is a # */
    static inComment(line: vscode.TextLine | string): boolean {
        if (typeof line == 'string') {
            return line.trimStart().startsWith(this.COMMENT_START);
        } else {
            const idx = line.firstNonWhitespaceCharacterIndex;
            return line.text.charAt(idx) == this.COMMENT_START;
        }
    }

    /* True if position in first 'word', allowing square brackets */
    static inFirstLabel(line: vscode.TextLine): boolean {
        return this.FIRST_KEY_OR_TABLE.test(line.text);
    }
    /* True if a word starting at 'start' is the key or table name of the line */
    static validHoverKey(line: vscode.TextLine, start: vscode.Position): boolean {
        return (
            !this.inComment(line) && this.KEY_PREFIX.test(line.text.substring(0, start.character))
        );
    }
    /* Dotted path of a [table] or [[table]] header line, if it is one */
    static headerPath(line: vscode.TextLine | string): string[] | undefined {
        const text = typeof line === 'string' ? line : line.text;
        const match = text.match(this.HEADER_RGX);
        if (!match) return undefined;
        return match[1].split('.').map((s) => s.trim().replace(/^["']|["']$/g, ''));
    }

    /**
     * Innermost table enclosing a line: its path, header line and the line
     * after its last one. Undefined above the first table.
     */
    static tableAt(
        doc: vscode.TextDocument,
        lineNo: number,
    ): { path: string[]; header: number; end: number } | undefined {
        let header = lineNo;
        let path: string[] | undefined;
        for (; header >= 0 && !path; header--) {
            path = this.headerPath(doc.lineAt(header));
        }
        if (!path) return undefined;
        header++;
        let end = lineNo + 1;
        while (end < doc.lineCount && !this.headerPath(doc.lineAt(end))) end++;
        return { path, header, end };
    }

    /* Top-level keys set in lines [start, end) */
    static keysIn(doc: vscode.TextDocument, start: number, end: number): string[] {
        const keys: string[] = [];
        for (let i = start; i < end; i++) {
            const text = doc.lineAt(i).text;
            const key = this.inComment(text) ? undefined : text.match(this.TOP_KEY_RGX)?.[0];
            if (key) keys.push(key.trim().toLowerCase());
        }
        return keys;
    }

    /**
     * Paths of tables defined by headers (except on line 'skip') or by
     * top-level keys, e.g. 'configuration.disabled = true' or an inline 'origin' table
     */
    static definedTables(doc: vscode.TextDocument, skip: number): TypeOptions {
        const first = this.firstTableLine(doc);
        const defined = new TypeOptions(this.keysIn(doc, 0, first));
        for (let i = first; i < doc.lineCount; i++) {
            const path = i === skip ? undefined : this.headerPath(doc.lineAt(i));
            if (path) defined.add(path.join('.'));
        }
        return defined;
    }

    /* First table header line, or the line count if there is none */
    static firstTableLine(doc: vscode.TextDocument): number {
        let line = 0;
        while (line < doc.lineCount && !this.headerPath(doc.lineAt(line))) line++;
        return line;
    }
}

/** Type of the property at a dotted path, e.g. ['configuration', 'disabled'] */
export function resolvePath(properties: PropertyDb, path: string[]): TypeInfo | undefined {
    let info = properties.get(path[0])?.typeData;
    for (const key of path.slice(1)) {
        info = info?.children.get(key);
    }
    return info;
}

export class TypeOptions extends Set<string> {}
export class TypeMap extends Map<string, TypeInfo> {}
export class PropertyDb extends Map<string, PropertyInfo> {}

/** Partially and fully parsed type definition info */
export class TypeInfo {
    public title: string;
    public description: string;
    public types: TypeOptions = new TypeOptions();
    public itemTypes: TypeOptions = new TypeOptions();
    public children: TypeMap = new TypeMap();
    public textType: TomlType = TomlType.SCALAR;
    public default: string | undefined;
    public readonly topLevel: boolean;
    public required: boolean = false;

    constructor(topLevel: boolean, title: string, description: string = '') {
        this.topLevel = topLevel;
        this.title = title;
        this.description = description;
    }

    /* Object below a top-level table, e.g. [configuration.variables] */
    isNestedTable(): boolean {
        return !this.topLevel && this.types.has(Keys.table);
    }
    hasDefault(): boolean {
        return this.default != undefined;
    }

    hasData(): boolean {
        return (
            this.children.size + this.types.size + this.itemTypes.size > 0 ||
            this.hasDefault() ||
            this.description !== ''
        );
    }

    merge(other: TypeInfo, withTextType: boolean = true) {
        other.types.forEach((t) => this.types.add(t));
        other.itemTypes.forEach((t) => this.itemTypes.add(t));
        if (withTextType) {
            this.textType = Math.max(this.textType, other.textType) as TomlType;
        }
        if (this.description === '') {
            this.description = other.description;
        }
        if (other.hasDefault() && !this.hasDefault()) {
            this.default = other.default;
        }
        for (const [key, child] of other.children) {
            if (!this.children.has(key)) this.children.set(key, child);
        }
        return this;
    }

    setToList() {
        switch (this.textType) {
            case TomlType.TABLE:
                this.textType = TomlType.TABLES;
                break;
            default:
                this.textType = TomlType.LIST;
                break;
        }
    }

    finaliseTypes() {
        switch (this.textType) {
            case TomlType.TABLE:
                this.types.add(Keys.table);
                break;
            case TomlType.TABLES:
                this.types.add(`${Keys.table} list`);
                break;
            default:
                break;
        }
    }
    /* Types shown to the user: values first, then lists, then tables */
    typeHint(): string {
        const itemHints = [...this.itemTypes].map((s) => `${s} list`);
        const hints = [...new TypeOptions([...this.types, ...itemHints])];
        const rank = (s: string) => (s.startsWith(Keys.table) ? 2 : s.endsWith(' list') ? 1 : 0);
        return hints
            .filter((s) => s.trim() !== '')
            .sort((a, b) => rank(a) - rank(b))
            .join(' | ');
    }
}

/**
 * Alire property class
 */
export class PropertyInfo {
    public title: string;
    public description: string;
    public required: boolean;
    public typeData: TypeInfo;
    /* Keys of the enclosing tables, for nested properties */
    public parentPath: string[] = [];

    /* Dotted path, e.g. configuration.auto_gpr_with */
    get path(): string {
        return [...this.parentPath, this.title].join('.');
    }

    constructor(title: string, description: string, typeData: TypeInfo, required: boolean = false) {
        this.title = title;
        this.description = description;
        this.typeData = typeData;
        this.required = required;
    }
    static fromType(info: TypeInfo, parentPath: string[] = []): PropertyInfo {
        const prop = new PropertyInfo(info.title, info.description, info, info.required);
        prop.parentPath = parentPath;
        return prop;
    }
    isTable(): boolean {
        return this.typeData.textType >= TomlType.TABLE;
    }
    /* [[table]], which can be repeated */
    isTableList(): boolean {
        return this.typeData.textType === TomlType.TABLES;
    }
    typeHint(): string {
        return this.typeData.typeHint();
    }
    toCompletion(): string {
        let template: string = this.title;
        switch (this.typeData.textType) {
            case TomlType.SCALAR: {
                if (this.typeData.types.has('string')) {
                    template += ' = ""';
                } else if (this.typeData.isNestedTable()) {
                    template = `[${this.path}]\n`;
                } else {
                    template += ' = ';
                }
                break;
            }
            case TomlType.LIST:
                template += ' = []';
                break;
            case TomlType.TABLE:
                template = `[${this.title}]\n`;
                break;
            case TomlType.TABLES:
                template = `[[${this.title}]]\n`;
                break;
        }
        return template;
    }
    /* CompletionItemLabels do not render markdown */
    public toCompletionLabel(): vscode.CompletionItemLabel {
        const label = {
            label: this.path,
            // Enforce a space between label and detail
            detail: this.required ? ' (required)' : '',
            description:
                this.typeHint() +
                (this.typeData.default ? ` (default: ${this.typeData.default})` : ''),
        };
        return label;
    }
    public toCompletionItem(): vscode.CompletionItem {
        const item = new vscode.CompletionItem(
            this.toCompletionLabel(),
            vscode.CompletionItemKind.Text,
        );
        item.documentation = this.description;
        // Match on the key name, not its path
        if (this.parentPath.length > 0) item.filterText = this.title;
        item.insertText = this.toCompletion();
        item.commitCharacters = ['.', ',', ' '];
        item.sortText = this.required ? 'b' : this.isTable() ? 'z' : 'y';
        return item;
    }
    /* Hover text can use Markdown */
    public toHoverItem(): vscode.Hover {
        const defaultVal = this.typeData.default ? `(default: ${this.typeData.default})` : '';
        const req = this.required ? ' (required)' : '';
        const parents = this.parentPath.map((key) => `${key}.`).join('');
        const header =
            `${parents}**${this.title}**${req} — "${this.typeHint()}" ${defaultVal}`.trimEnd();
        const sentences = this.description.replace(/\. /g, '.\n').split('\n');
        const text: string[] = [header].concat(sentences.filter((s) => s.trim() !== ''));
        /* Convert quotes to markdown monospace */
        const md = text.map((line) => new vscode.MarkdownString(line.replace(/"/g, '`')));
        return new vscode.Hover(md);
    }
}

export class Parser {
    private readonly propdefs: Map<string, YamlNode>;
    private readonly typedefs: Map<string, YamlNode>;
    public readonly propCache: PropertyDb = new PropertyDb();
    public readonly refCache: TypeMap = new TypeMap();
    /* Definitions being parsed, for self-references */
    private inProgress: TypeMap = new TypeMap();

    constructor(schema: YamlNode) {
        for (const key of [Keys.props, Keys.defs]) {
            if (!(schema.get(key) instanceof Map)) {
                throw Error(`Malformed schema: missing '${key}'`);
            }
        }
        this.propdefs = schema.get(Keys.props) as Map<string, YamlNode>;
        this.typedefs = schema.get(Keys.defs) as Map<string, YamlNode>;
        this.parseProperties(Parser.collectRequired(schema));
    }
    /**
     * TOML forms preferred by the catalog format specification, where the
     * schema allows several: [test] is the usual form, [[test]] is allowed.
     */
    static readonly TomlForms: Map<string, TomlType> = new Map([['test', TomlType.TABLE]]);

    static hasTypeInfo(n: YamlNode): boolean {
        return n.has(Keys.ref) || n.has(Keys.type) || n.has(Keys.pattern) || n.has(Keys.oneOf);
    }
    /**
     * Keys listed in 'required', including those in 'allOf' entries. Their
     * 'then' branches are included when the 'if' condition only tests which
     * keys are present (e.g. 'version' unless 'external' is set), not when it
     * tests values (e.g. keys required for one 'kind' of external).
     * Alternatives ('oneOf') only require keys in some cases, so are ignored.
     */
    static collectRequired(n: YamlNode | undefined): string[] {
        if (!(n instanceof Map)) return [];
        const own = n.get(Keys.required);
        const keys: string[] = Array.isArray(own) ? [...(own as unknown as string[])] : [];
        const allOf = n.get(Keys.allOf);
        if (Array.isArray(allOf)) {
            for (const entry of allOf) {
                keys.push(...Parser.collectRequired(entry));
                const cond = entry instanceof Map ? entry.get(Keys.if) : undefined;
                if (cond instanceof Map && !Parser.testsValues(cond)) {
                    keys.push(...Parser.collectRequired(entry.get(Keys.then) as YamlNode));
                }
            }
        }
        return [...new TypeOptions(keys)];
    }

    /** True if a condition inspects property values rather than key presence */
    static testsValues(cond: YamlNode): boolean {
        if (cond.has(Keys.props)) return true;
        const not = cond.get(Keys.not);
        return not instanceof Map && Parser.testsValues(not);
    }

    static getRefName(n: YamlNode): string {
        const ref = n.get(Keys.ref);
        return typeof ref === 'string' ? ref.replace(Keys.defPfx, '') : '';
    }

    static description(n: YamlNode): string {
        const description = n.get(Keys.description);
        return typeof description === 'string' ? description : '';
    }

    /** Expected keys: items */
    private setArrayItems(n: YamlNode, info: TypeInfo) {
        const items = new TypeInfo(info.topLevel, info.title);
        this.parseTypeData(n.get(Keys.items) as YamlNode, items);
        info.itemTypes = new TypeOptions([...items.types, ...items.itemTypes]);
        for (const [key, child] of items.children) {
            if (!info.children.has(key)) info.children.set(key, child);
        }
        // Arrays of tables are written as [[table]]
        if (items.textType >= TomlType.TABLE) {
            info.textType = TomlType.TABLE;
        }
        info.setToList();
    }

    /** True for 'case(...)' expressions: objects with only case patterns */
    static isDynamicCase(n: YamlNode): boolean {
        const patterns = n.get(Keys.patProps);
        return (
            !n.has(Keys.props) &&
            patterns instanceof Map &&
            [...patterns.keys()].every((k: string) => k.startsWith('^case\\('))
        );
    }

    /** Expected keys: type */
    private parseType(n: YamlNode, info: TypeInfo) {
        const typeHint = n.get(Keys.type);
        if (typeHint == Keys.object) {
            if (info.topLevel) {
                info.textType = Math.max(info.textType, TomlType.TABLE) as TomlType;
            } else {
                // Nested tables, e.g. [configuration.variables]
                info.types.add(Keys.table);
            }
        } else if (typeHint == Keys.array) {
            this.setArrayItems(n, info);
        } else if (typeof typeHint == 'string') {
            info.types.add(typeHint);
        }
    }

    /** Expected keys: properties */
    private parseChildren(parentNode: YamlNode, parentType: TypeInfo) {
        const children = parentNode.get(Keys.props) as Map<string, YamlNode>;
        const required = Parser.collectRequired(parentNode);
        for (const [key, childNode] of children) {
            const childType = new TypeInfo(false, key, Parser.description(childNode));
            this.parseTypeData(childNode, childType);
            childType.required = required.includes(key);
            parentType.children.set(key, childType);
        }
        parentType.textType = Math.max(parentType.textType, TomlType.TABLE) as TomlType;
    }

    /** Expected keys: default, type, $ref, oneOf, properties */
    private parseTypeData(n: YamlNode, info: TypeInfo) {
        // A default next to a $ref overrides the referenced one
        if (n.has(Keys.default) && !info.hasDefault()) {
            info.default = String(n.get(Keys.default));
        }
        if (n.has(Keys.type)) {
            this.parseType(n, info);
        }
        const ref = Parser.getRefName(n);
        if (ref) {
            const def = this.resolveRef(ref);
            if (def && def !== info) info.merge(def);
        }
        if (n.has(Keys.oneOf)) {
            this.parseTypeList(n, info);
        }
        if (n.get(Keys.props) instanceof Map) {
            this.parseChildren(n, info);
        }
    }

    /**
     * Expected keys: oneOf
     * Collects the types of all alternatives, except 'case(...)' ones. The TOML
     * form is the richest non-table one (prefer 'key = []' to 'key = ""'), else
     * the richest table one.
     */
    private parseTypeList(n: YamlNode, info: TypeInfo) {
        const options = (n.get(Keys.oneOf) as YamlNode[]).filter(
            (o) => Parser.hasTypeInfo(o) && !Parser.isDynamicCase(this.resolve(o)),
        );
        const textTypes: TomlType[] = [];
        for (const optionNode of options) {
            const option = new TypeInfo(info.topLevel, info.title);
            this.parseTypeData(optionNode, option);
            option.finaliseTypes();
            info.merge(option, false);
            textTypes.push(option.textType);
            // Updated per option: later ones may refer back to this type
            const values = textTypes.filter((t) => t < TomlType.TABLE);
            info.textType = Math.max(...(values.length > 0 ? values : textTypes)) as TomlType;
        }
    }

    /** Follow a $ref to its definition, if any */
    private resolve(n: YamlNode): YamlNode {
        const ref = Parser.getRefName(n);
        return (ref && this.typedefs.get(ref)) || n;
    }

    /**
     * Type of a definition, parsed once. A definition referring to itself
     * gets what is known of it so far.
     */
    private resolveRef(ref: string): TypeInfo | undefined {
        const known = this.refCache.get(ref) ?? this.inProgress.get(ref);
        if (known) return known;
        const defNode = this.typedefs.get(ref);
        if (!defNode) {
            return undefined;
        }
        const refInfo = new TypeInfo(true, ref, Parser.description(defNode));
        this.inProgress.set(ref, refInfo);
        this.parseTypeData(defNode, refInfo);
        this.inProgress.delete(ref);
        refInfo.finaliseTypes();
        this.refCache.set(ref, refInfo);
        return refInfo;
    }

    /** Parse top-level properties specified in schema */
    private parseProperties(required: string[]) {
        for (const [key, propNode] of this.propdefs) {
            if (!Parser.hasTypeInfo(propNode)) {
                continue;
            }
            const typeData = new TypeInfo(true, key);
            const description = Parser.description(propNode);
            this.parseTypeData(propNode, typeData);
            typeData.finaliseTypes();
            const form = Parser.TomlForms.get(key);
            if (form !== undefined) typeData.textType = form;
            if (!typeData.hasData()) {
                logger.debug(`Could not find type information for property ${key}`);
            }
            // Keep the property's own description and requirement on its type,
            // which is what providers resolve when looking up dotted paths
            if (description !== '') typeData.description = description;
            typeData.required = required.includes(key);
            this.propCache.set(key, PropertyInfo.fromType(typeData));
        }
    }

    static init(schemaPath: string): Parser | null {
        const contents = readFileSync(schemaPath, 'utf8');
        const yamlSchema = yaml.parse(contents, {
            merge: true,
            mapAsMap: true,
        }) as YamlNode;
        if (!yamlSchema) {
            logger.error(`Failed to parse schema at: ${schemaPath}`);
            return null;
        }
        return new Parser(yamlSchema);
    }
}
