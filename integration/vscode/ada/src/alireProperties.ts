import * as vscode from 'vscode';

export class TomlText {
    static COMMENT_START = '#';
    static readonly TOP_KEY_RGX = /^\s*([\w,-])+\s*/;
    static readonly FIRST_WORD = /^\s*([\w,-])*\s*/;
    // Only match square bracket at start of line
    static readonly TABLE_RGX = /(?<=^\s*)\[/m;

    /** True if line is commented out */
    static inComment(line: vscode.TextLine | string): boolean {
        if (typeof line == 'string') {
            return line.trimStart().startsWith(this.COMMENT_START);
        } else {
            const idx = line.firstNonWhitespaceCharacterIndex;
            return line.text.charAt(idx) == this.COMMENT_START;
        }
    }
    static inFirstWord(line: vscode.TextLine): boolean {
        return this.FIRST_WORD.test(line.text);
    }
    static inStringValue(line: vscode.TextLine, pos: vscode.Position): boolean {
        return /\S\s*=\s*"/.test(line.text.substring(0, pos.character));
    }
    /* Top-level keys must be first word on line */
    static allowCompletion(line: vscode.TextLine): boolean {
        return !this.inComment(line) && this.inFirstWord(line);
    }
    /**
     * TOML syntax does not allow top-level fields after the first table
     */
    static getFirstTableIdx(doc: vscode.TextDocument): number {
        // Multiline regex for '[' char at start of line (allow whitespace)
        const firstTableIdx = doc.getText().match(this.TABLE_RGX)?.index;
        return firstTableIdx ?? -1;
    }
}
export abstract class AlireCompletionItem extends vscode.CompletionItem {
    public readonly required: boolean;
    constructor(
        label: vscode.CompletionItemLabel,
        kind: vscode.CompletionItemKind,
        required: boolean = false,
    ) {
        super(label, kind);
        this.required = required;
    }
    toName(): string {
        const itemLabel: string | vscode.CompletionItemLabel = this.label;
        if (typeof itemLabel === 'string') {
            return itemLabel;
        }
        return itemLabel.label;
    }
    isSnippet(): boolean {
        return this.kind == vscode.CompletionItemKind.Snippet;
    }
    isRequired(): boolean {
        return this.required;
    }
}
export class FieldCompletion extends AlireCompletionItem {
    constructor(
        label: vscode.CompletionItemLabel,
        docstring: string,
        insertion: string,
        required: boolean = false,
    ) {
        /* Enforce a space between label and detail */
        if (label.detail != undefined) {
            label.detail = ' ' + label.detail.trimStart();
        }
        super(label, vscode.CompletionItemKind.Text, required);
        this.documentation = docstring;
        this.insertText = insertion;
        this.commitCharacters = ['.', ',', ' '];
        this.sortText = this.required ? 'b' : 'z';
    }
}
export class SnippetCompletion extends AlireCompletionItem {
    constructor(label: vscode.CompletionItemLabel, insertion: vscode.SnippetString) {
        /* Enforce a space between label and detail */
        if (typeof label != 'string' && label.detail) {
            label.detail = ' ' + label.detail.trimStart();
        }
        super(label, vscode.CompletionItemKind.Snippet);
        this.insertText = insertion;
        this.keepWhitespace = false; // snippets triggered by whitespace
        this.sortText = 'a'; // Order  snippets first
    }
}

/** Classes for top-level properties */
interface CompProvider {
    toCompletionItem(): vscode.CompletionItem;
}
abstract class SimpleField implements CompProvider {
    static readonly dataType: string = '';

    public readonly title: string;
    public description: string;
    protected readonly dataType: string;
    public readonly required: boolean;

    constructor(title: string, descr: string, mandatory: boolean = false) {
        this.title = title;
        this.description = descr;
        this.dataType = (this.constructor as typeof SimpleField).dataType;
        this.required = mandatory;
    }
    public textInsertion(): string {
        return this.title;
    }

    /* Completion label will look like this:
     * with no added space between [label] and [detail]
     *
     * | [label][detail]     [description] |
     *
     * examples:
     * | name (required)      String               |
     * | authors              String List          |
     * | auto-gpr-with        Bool (Default: true) |
     *
     * Note: label 'description' field will be used for type info
     * Actual field description goes to completionItem.documentation
     */
    public toCompletionLabel(): vscode.CompletionItemLabel {
        const label = {
            label: this.title,
            detail: this.required ? '(required)' : '',
            description: this.dataType,
        };
        return label;
    }
    public toCompletionItem(): FieldCompletion {
        return new FieldCompletion(
            this.toCompletionLabel(), // completionItem.label
            this.description, // completionItem.documentation
            this.textInsertion(),
            this.required, //
        );
    }
}
class StringField extends SimpleField {
    static readonly dataType: string = 'String';

    constructor(title: string, descr: string, required: boolean = false) {
        super(title, descr, required);
    }

    public textInsertion(): string {
        return `${this.title} = ""`;
    }
}
class BoolField extends SimpleField {
    static readonly dataType: string = 'Bool';
    static readonly allowedValues: string[] = ['true', 'false'];
    public readonly defaultValue: string;

    constructor(title: string, descr: string, defaultVal = '') {
        super(title, descr);
        this.defaultValue = defaultVal ? defaultVal : '';
    }
    public textInsertion(): string {
        return `${this.title} = ${this.defaultValue ?? ''}`;
    }
    public toCompletionLabel(): vscode.CompletionItemLabel {
        const annotation = this.defaultValue ? ` (Default: ${this.defaultValue})` : '';
        const label = super.toCompletionLabel();
        label.description = label.description + annotation;
        return label;
    }
}
class StringListField extends SimpleField {
    static readonly dataType: string = 'String List';

    constructor(title: string, descr: string) {
        super(title, descr);
    }
    public textInsertion(): string {
        return `${this.title} = []`;
    }
}
class ManifestSnippet implements CompProvider {
    static readonly dataType: string = 'TOML';

    public completions: SimpleField[];
    public title: string;
    public note: string;

    constructor(name: string, props: SimpleField[], note: string = '') {
        this.title = name;
        this.completions = props;
        this.note = note;
    }
    private toSnippet(skipProperties: string[] = []): vscode.SnippetString {
        const value = this.completions
            .filter((prop) => !skipProperties.includes(prop.title))
            .map((prop) => prop.textInsertion().trim())
            .join('\n');
        return new vscode.SnippetString(value);
    }
    protected toCompletionLabel(): vscode.CompletionItemLabel {
        return {
            label: this.title,
            detail: this.note,
            description: ManifestSnippet.dataType,
        };
    }
    public toCompletionItem(): SnippetCompletion {
        return new SnippetCompletion(this.toCompletionLabel(), this.toSnippet());
    }
    public toPartialCompletionItem(withoutProperties: string[]) {
        return new SnippetCompletion(this.toCompletionLabel(), this.toSnippet(withoutProperties));
    }
}

/** Directly used for completion */
export const Fields: SimpleField[] = [
    /* Mandatory fields first */
    new StringField('name', 'Name of the crate this release belongs to. Length: 3-64 chars', true),
    new StringField(
        'description',
        'One-line description of crate. Limited to 72 chars, unlike long-description.',
        true,
    ),
    new StringField('version', 'Semantic version of the release.', true),

    /* Optional bool properties */
    new BoolField(
        'auto-gpr-with',
        'Whether to automatically add "with" clauses to the GPR configuration file ' +
            'generated by Alire.',
        'true',
    ),
    /* Optional string properties */
    new StringField(
        'licenses',
        'SPDX expression of licenses that apply to the crate release. (Required for indexing.)',
    ),
    new StringField('long-description', 'Detailed description of package. No length limit.'),
    new StringField('notes', 'Notes for this crate release.'),
    new StringField('website', 'URL of the project website.'),

    /* String array properties */
    new StringListField('authors', 'List of the package authors by name.'),
    new StringListField(
        'executables',
        'List of the executables provided by the package.' +
            ' Each entry must provide the base name of the executable only. Used by "alr run".',
    ),
    new StringListField(
        'maintainers',
        'List of crate maintainers.' +
            ' Each entry must include a contact email address. Names are optional.' +
            ' Examples: "alice@example.com", "Bob For Instance <bob@athome.com>"' +
            ' (Required for indexing.)',
    ),
    new StringListField(
        'maintainers-logins',
        'List of GitHub usernames authorized to modify crate.' +
            ' (Required for crates submitted to the community index.)',
    ),
    new StringListField(
        'project-files',
        'List of GPR project file paths used by crate.' +
            ' Each path is relative to the source directory root of the alire.toml file.' +
            ' Only required if the GPR project file name differs to the crate name.',
    ),
    new StringListField('tags', 'List of related topics or keywords. Used by "alr search".'),
];

export const Snippets: ManifestSnippet[] = [
    new ManifestSnippet(
        'simple',
        Fields.filter((prop) => prop.required),
        '(required fields)',
    ),
    new ManifestSnippet(
        'community',
        Fields.filter((prop) => prop.required || prop.description.includes('index')),
        '(required for community crates)',
    ),
];
