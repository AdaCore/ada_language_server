import assert from 'assert';
import * as vscode from 'vscode';
import { adaExtState } from '../../src/extension';
import { activate } from '../utils';
import { existsSync } from 'fs';
import path from 'path';
import {
    AlireCompletionProvider,
    ExternalsProfile,
    getAlireData,
    IndexProfile,
    ManifestProfile,
} from '../../src/alireProviders';

/** Scalar/list top-level properties: valid before the first table */
const topLevelFields = [
    'name',
    'version',
    'description',
    'long-description',
    'notes',
    'website',
    'auto-gpr-with',
    'authors',
    'maintainers',
    'maintainers-logins',
    'licenses',
    'tags',
    'available',
    'executables',
    'project-files',
];
/** Properties that must be written as [table] headers */
const tableFields = [
    'gpr-externals',
    'gpr-set-externals',
    'environment',
    'configuration',
    'test',
    'build-profiles',
    'build-switches',
];
/** Properties that must be written as [[table]] headers */
const tableListFields = ['depends-on', 'forbids', 'pins', 'actions', 'external'];

const folder = vscode.workspace.workspaceFolders![0].uri;

async function openToml(subdir: string): Promise<vscode.TextDocument> {
    const uri = vscode.Uri.joinPath(folder, subdir, 'alire.toml');
    assert.ok(existsSync(uri.fsPath), `Path not found: ${uri.fsPath}`);
    const toml = await vscode.workspace.openTextDocument(uri);
    assert.strictEqual(toml.languageId, 'toml', 'Expected alire.toml languageId to be toml');
    return toml;
}

function labelOf(item: vscode.CompletionItem): string {
    return typeof item.label === 'string' ? item.label : item.label.label;
}
/** Key name, without the path shown in child labels */
function keyOf(item: vscode.CompletionItem): string {
    return labelOf(item).split('.').pop()!;
}

/**
 * In-memory manifest for states that are not valid TOML (e.g. a key or table
 * header being typed): malformed alire.toml files on disk crash the ALS.
 */
async function draftToml(content: string): Promise<vscode.TextDocument> {
    return vscode.workspace.openTextDocument({ language: 'toml', content });
}

const draftTables = [
    'name = "tables_crate"',
    'version = "0.1"',
    'description = "Crate with tables"',
    'web',
    '',
    '[depends-on]',
    'gnat = ">=13"',
    '',
    '[gpr',
].join('\n');

async function complete(src: string | vscode.TextDocument, line: number, column: number) {
    const provider = adaExtState.alireCompletionProvider;
    assert.ok(provider, 'Alire completion provider should be initialized');
    const toml = typeof src === 'string' ? await openToml(src) : src;
    const pos = new vscode.Position(line, column);
    assert.deepStrictEqual(toml.validatePosition(pos), pos, `Invalid position ${line}:${column}`);
    const token = new vscode.CancellationTokenSource().token;
    const ctxt: vscode.CompletionContext = {
        triggerKind: vscode.CompletionTriggerKind.Invoke,
        triggerCharacter: undefined,
    };
    const items = (await provider.provideCompletionItems(toml, pos, token, ctxt)) ?? [];
    assert.ok(Array.isArray(items), 'Expected an array of completion items');
    return items;
}

function assertIncludes(labels: string[], expected: string[]) {
    const missing = expected.filter((e) => !labels.includes(e));
    assert.deepStrictEqual(missing, [], `Missing completions: ${missing.join(', ')}`);
}
function assertExcludes(labels: string[], unexpected: string[]) {
    const present = unexpected.filter((e) => labels.includes(e));
    assert.deepStrictEqual(present, [], `Unexpected completions: ${present.join(', ')}`);
}

suite('Alire completion: positions (alire.toml)', function () {
    this.beforeAll(async () => {
        await activate();
        assert.ok(adaExtState, 'Extension should be initialized after activation');
        assert.ok(
            adaExtState.alireCompletionProvider,
            'Alire completion provider should be initialized after activation',
        );
    });

    test('Empty file suggests all top-level fields and tables', async () => {
        const labels = (await complete('empty', 0, 0)).map(labelOf);
        assertIncludes(labels, topLevelFields);
        assertIncludes(labels, tableFields.concat(tableListFields));
        assertExcludes(labels, ['origin']);
    });

    test('Tables are suggested after the last top-level key', async () => {
        const labels = (await complete('.', 3, 0)).map(labelOf);
        assertIncludes(labels, tableFields.concat(tableListFields));
    });

    test('Tables are not suggested above top-level keys', async () => {
        // Keys below the cursor would become part of the new table
        const labels = (await complete('comm', 3, 0)).map(labelOf);
        assertExcludes(labels, tableFields.concat(tableListFields));
    });

    test('No table header completions above top-level keys', async () => {
        // Keys above and below a line with only '[' (then '[e'), and a table further down
        for (const typed of ['[', '[e']) {
            const doc = await draftToml(
                `name = "x"\n${typed}\nversion = "1"\ndescription = "d"\n\n[[depends-on]]\n`,
            );
            assert.deepStrictEqual(await complete(doc, 1, typed.length), [], `After '${typed}'`);
        }
    });

    test('No table header completions above keys of a table', async () => {
        const doc = await draftToml('[configuration]\n[e\ndisabled = true\n');
        assert.deepStrictEqual(await complete(doc, 1, 2), []);
    });

    test('Singleton tables already defined are not suggested again', async () => {
        const doc = await draftToml('[configuration]\n\n[[depends-on]]\n\n');
        const labels = (await complete(doc, 4, 0)).map(labelOf);
        assertExcludes(labels, ['configuration']);
        // [[tables]] can be repeated
        assertIncludes(labels, ['depends-on', 'environment']);
    });

    test('Tables defined by top-level keys are not suggested', async () => {
        const doc = await draftToml('configuration.disabled = true\n\n');
        assertExcludes((await complete(doc, 1, 0)).map(labelOf), ['configuration']);
    });

    test('Nested singleton tables already defined are not suggested', async () => {
        const doc = await draftToml('[configuration]\n\n[configuration.variables]\n');
        const labels = (await complete(doc, 1, 0)).map(labelOf);
        assertExcludes(labels, ['configuration.variables']);
        assertIncludes(labels, ['configuration.values']);
    });

    test('Bare table names match without brackets', async () => {
        const doc = await draftToml('name = "x"\ngpr');
        const item = (await complete(doc, 1, 3)).find((i) => labelOf(i) === 'gpr-externals');
        assert.ok(item, 'No completion item for gpr-externals');
        assert.strictEqual(item.insertText, '[gpr-externals]\n');
        assert.strictEqual(item.filterText, 'gpr-externals');
        assert.strictEqual((item.range as vscode.Range).start.character, 0);
    });

    test('Existing keys are not suggested again', async () => {
        const labels = (await complete('.', 3, 0)).map(labelOf);
        assertExcludes(labels, ['name', 'version', 'description']);
        assertIncludes(labels, ['website', 'authors', 'licenses', 'tags']);
    });

    test('Keys present before a table are not suggested', async () => {
        const labels = (await complete('comm', 3, 0)).map(labelOf);
        assertExcludes(labels, [
            'name',
            'version',
            'description',
            'authors',
            'maintainers',
            'maintainers-logins',
            'licenses',
            'website',
            'tags',
        ]);
        assertIncludes(labels, ['long-description', 'notes', 'executables', 'project-files']);
    });

    test('Partially typed key still gets completions', async () => {
        const labels = (await complete(await draftToml(draftTables), 3, 3)).map(labelOf);
        assertIncludes(labels, ['website']);
        assertExcludes(labels, ['name', 'version', 'description']);
    });

    test('No completions after key/value separator', async () => {
        // 'name = "tables_crate"', cursor inside the value
        assert.deepStrictEqual(await complete(await draftToml(draftTables), 0, 9), []);
    });

    test('No completions in comments', async () => {
        assert.deepStrictEqual(await complete('comm', 11, 5), []);
    });

    test('No top-level field completions below the first table', async () => {
        // 'gnat = ">=13"' inside [depends-on]
        const labels = (await complete('comm', 13, 0)).map(labelOf);
        assertExcludes(labels, topLevelFields);
    });

    test('Table header being typed suggests tables only', async () => {
        // '[gpr' after existing tables
        const labels = (await complete(await draftToml(draftTables), 8, 4)).map(labelOf);
        assertIncludes(labels, tableFields);
        assertExcludes(labels, topLevelFields);
    });
});

suite('Alire completion: item text', function () {
    let items: Map<string, vscode.CompletionItem>;

    this.beforeAll(async () => {
        await activate();
        items = new Map((await complete('empty', 0, 0)).map((i) => [labelOf(i), i]));
    });

    function check(title: string, typeHint: string, insertion: string, required = false) {
        const item = items.get(title);
        assert.ok(item, `No completion item for ${title}`);
        const label = item.label as vscode.CompletionItemLabel;
        assert.strictEqual(label.description, typeHint, `${title}: wrong type hint`);
        assert.strictEqual(item.insertText, insertion, `${title}: wrong insert text`);
        assert.strictEqual(
            label.detail,
            required ? ' (required)' : '',
            `${title}: wrong 'required' detail`,
        );
        assert.ok(item.documentation, `${title}: missing documentation`);
    }

    test('Required string properties', () => {
        check('name', 'string', 'name = ""', true);
        check('description', 'string', 'description = ""', true);
        // Required through 'allOf'
        check('version', 'string', 'version = ""', true);
    });

    test('Optional string properties', () => {
        ['long-description', 'notes', 'website', 'licenses'].forEach((p) =>
            check(p, 'string', `${p} = ""`),
        );
    });

    test('String list properties (inline and via $ref)', () => {
        ['authors', 'maintainers', 'maintainers-logins', 'tags', 'provides'].forEach((p) =>
            check(p, 'string list', `${p} = []`),
        );
    });

    test('Properties accepting a string or a list insert a list', () => {
        ['executables', 'project-files'].forEach((p) =>
            check(p, 'string | string list', `${p} = []`),
        );
    });

    test('Boolean properties with defaults', () => {
        check('auto-gpr-with', 'boolean (default: true)', 'auto-gpr-with = ');
        // Default declared on the property itself, alongside a $ref
        const available = items.get('available');
        assert.ok(available, 'No completion item for available');
        const desc = (available.label as vscode.CompletionItemLabel).description ?? '';
        assert.ok(desc.includes('(default: true)'), `available: default missing in '${desc}'`);
    });

    test('Required fields sort before optional ones', () => {
        const name = items.get('name')!;
        const notes = items.get('notes')!;
        assert.ok(name.sortText! < notes.sortText!, 'Required fields should sort first');
    });

    test('Table properties insert a table header', async () => {
        const tables = new Map(
            (await complete(await draftToml(draftTables), 8, 4)).map((i) => [labelOf(i), i]),
        );
        tableFields.forEach((t) => {
            const item = tables.get(t);
            assert.ok(item, `No completion item for ${t}`);
            assert.strictEqual(item.insertText, `[${t}]\n`);
        });
        // Forbidden in workspace manifests
        assert.ok(!tables.has('origin'), 'origin should not be offered in alire.toml');
        tableListFields.forEach((t) => {
            const item = tables.get(t);
            assert.ok(item, `No completion item for ${t}`);
            assert.strictEqual(item.insertText, `[[${t}]]\n`);
        });
    });

    test('Table completions replace the brackets already typed', async () => {
        // '  [gpr' : range must start at '[' so accepting does not give '[[gpr-externals]'
        const doc = await draftToml('  [gpr');
        const item = (await complete(doc, 0, 6)).find((i) => labelOf(i) === 'gpr-externals');
        assert.ok(item, 'No completion item for gpr-externals');
        const range = item.range as vscode.Range;
        assert.ok(range, 'Table completion should set a range');
        assert.strictEqual(range.start.character, 2);
        assert.strictEqual(range.end.character, 6);
        assert.strictEqual(item.filterText, '[gpr-externals]');
    });
});

suite('Alire completion: child properties', function () {
    this.beforeAll(async () => {
        await activate();
    });

    test('Keys of the enclosing table are suggested', async () => {
        const doc = await draftToml('name = "x"\n\n[configuration]\ndisabled = false\n\n');
        const labels = (await complete(doc, 4, 0)).map(keyOf);
        assertIncludes(labels, ['output_dir', 'generate_ada', 'generate_gpr', 'generate_c']);
        // Already set in this table
        assertExcludes(labels, ['disabled']);
        // Top-level keys are not valid inside a table
        assertExcludes(labels, topLevelFields);
    });

    test('Keys already set in another table are still suggested', async () => {
        const doc = await draftToml(
            '[[actions]]\ntype = "pre-build"\ncommand = ["a"]\n\n[[actions]]\n\n',
        );
        const labels = (await complete(doc, 5, 0)).map(keyOf);
        assertIncludes(labels, ['type', 'command', 'directory']);
    });

    test('Required child keys are marked and sorted first', async () => {
        const doc = await draftToml('[[actions]]\n\n');
        const items = new Map((await complete(doc, 1, 0)).map((i) => [keyOf(i), i]));
        for (const key of ['type', 'command']) {
            const item = items.get(key);
            assert.ok(item, `No completion item for ${key}`);
            assert.strictEqual((item.label as vscode.CompletionItemLabel).detail, ' (required)');
        }
        const directory = items.get('directory');
        assert.ok(directory, 'No completion item for directory');
        assert.strictEqual((directory.label as vscode.CompletionItemLabel).detail, '');
        assert.ok(items.get('type')!.sortText! < directory.sortText!);
    });

    test('Child labels show the full path, matching on the key name', async () => {
        const doc = await draftToml('[configuration]\n\n');
        const item = (await complete(doc, 1, 0)).find((i) => keyOf(i) === 'auto_gpr_with');
        assert.ok(item, 'No completion item for auto_gpr_with');
        assert.strictEqual(labelOf(item), 'configuration.auto_gpr_with');
        assert.strictEqual(item.filterText, 'auto_gpr_with');
        assert.strictEqual(item.insertText, 'auto_gpr_with = ');
    });

    test('Child item text', async () => {
        const doc = await draftToml('[configuration]\n\n');
        const items = new Map((await complete(doc, 1, 0)).map((i) => [keyOf(i), i]));
        const expected: [string, string][] = [
            ['disabled', 'disabled = '],
            ['output_dir', 'output_dir = ""'],
            ['variables', '[configuration.variables]\n'],
        ];
        for (const [key, insertion] of expected) {
            assert.strictEqual(items.get(key)?.insertText, insertion, `${key}: wrong insert text`);
        }
    });

    test('Keys required only for some kinds of external are not marked', async () => {
        const doc = await draftToml('[[external]]\n\n');
        const items = await complete(doc, 1, 0);
        assertIncludes(items.map(keyOf), ['kind', 'version-command', 'version-regexp']);
        const marked = items.filter((i) => (i.label as vscode.CompletionItemLabel).detail);
        assert.deepStrictEqual(marked.map(keyOf), []);
    });

    test('Tables without known keys only suggest other tables', async () => {
        // [depends-on] keys are crate names
        const doc = await draftToml('[[depends-on]]\n\n');
        const items = await complete(doc, 1, 0);
        const keys = items.filter((i) => !(i.insertText as string).startsWith('['));
        assert.deepStrictEqual(keys.map(keyOf), []);
        assertIncludes(items.map(keyOf), tableFields);
    });

    test('Tables are suggested after the last key of a table', async () => {
        const doc = await draftToml('[configuration]\ndisabled = true\n\n');
        const labels = (await complete(doc, 2, 0)).map(keyOf);
        assertIncludes(labels, ['output_dir', 'variables']);
        // Singleton table should not be suggested again
        const allowedTableFields = tableFields.filter((s) => s != 'configuration');
        assertIncludes(labels, allowedTableFields);
    });

    test('No tables, nested or not, above keys of a table', async () => {
        const doc = await draftToml('[configuration]\n\ndisabled = true\n');
        const labels = (await complete(doc, 1, 0)).map(keyOf);
        assertIncludes(labels, ['output_dir']);
        assertExcludes(labels, tableFields.concat(['variables', 'values']));
    });
});

suite('Alire completion: manifest profiles', function () {
    let provider: (profile: ManifestProfile) => AlireCompletionProvider;

    this.beforeAll(async () => {
        await activate();
        const ext = vscode.extensions.getExtension('AdaCore.ada');
        assert.ok(ext, 'Ada extension not found');
        const parser = getAlireData(path.join(ext.extensionPath, 'schemas', 'alire-manifest.yaml'));
        assert.ok(parser, 'Failed to parse the Alire schema');
        provider = (profile) => new AlireCompletionProvider(parser.propCache, profile);
    });

    async function items(profile: ManifestProfile, content: string, line: number, col: number) {
        const doc = await draftToml(content);
        const pos = new vscode.Position(line, col);
        const token = new vscode.CancellationTokenSource().token;
        const ctxt: vscode.CompletionContext = {
            triggerKind: vscode.CompletionTriggerKind.Invoke,
            triggerCharacter: undefined,
        };
        return provider(profile).provideCompletionItems(doc, pos, token, ctxt);
    }
    function required(list: vscode.CompletionItem[]): string[] {
        return list
            .filter((i) => (i.label as vscode.CompletionItemLabel).detail === ' (required)')
            .map(labelOf);
    }

    test('Index manifests offer origin', async () => {
        const labels = (await items(IndexProfile, '[', 0, 1)).map(labelOf);
        assertIncludes(labels, ['origin']);
    });

    test('Index manifests require indexing fields', async () => {
        const labels = required(await items(IndexProfile, '', 0, 0));
        assertIncludes(labels, [
            'name',
            'version',
            'description',
            'maintainers',
            'maintainers-logins',
            'licenses',
        ]);
    });

    test('Externals files do not require a version', async () => {
        const labels = required(await items(ExternalsProfile, '', 0, 0));
        assertIncludes(labels, ['name', 'description']);
        assertExcludes(labels, ['version']);
        const tables = (await items(ExternalsProfile, '[', 0, 1)).map(labelOf);
        assertExcludes(tables, ['origin']);
    });
});
