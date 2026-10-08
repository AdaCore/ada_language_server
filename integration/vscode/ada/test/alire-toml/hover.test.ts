import assert from 'assert';
import * as vscode from 'vscode';
import { adaExtState } from '../../src/extension';
import { activate } from '../utils';
import { existsSync } from 'fs';

/*
 * Fixture: ws/hover/alire.toml
 *   0 name = "hover_crate"
 *   1 version = "0.1"
 *   2 # name of the crate
 *   3 description = "version tags"
 *   4 maintainers = ["Me <me@example.com>"]
 *   5 maintainers-logins = ["me"]
 *   6
 *   7 [depends-on]
 *   8 gnat = ">=13"
 */
suite('Alire hover (alire.toml)', function () {
    let toml: vscode.TextDocument;

    this.beforeAll(async () => {
        await activate();
        assert.ok(adaExtState, 'Extension should be initialized after activation');
        assert.ok(
            adaExtState.alireHoverProvider,
            'Alire hover provider should be initialised after activation',
        );
        const folder = vscode.workspace.workspaceFolders![0].uri;
        const uri = vscode.Uri.joinPath(folder, 'hover', 'alire.toml');
        assert.ok(existsSync(uri.fsPath), `Path not found: ${uri.fsPath}`);
        toml = await vscode.workspace.openTextDocument(uri);
    });

    async function hover(
        line: number,
        column: number,
        doc: vscode.TextDocument = toml,
    ): Promise<string[] | null> {
        const pos = new vscode.Position(line, column);
        assert.deepStrictEqual(
            doc.validatePosition(pos),
            pos,
            `Invalid position ${line}:${column}`,
        );
        const token = new vscode.CancellationTokenSource().token;
        const result = await adaExtState.alireHoverProvider!.provideHover(doc, pos, token);
        if (!result) return null;
        return result.contents.map((c) => (typeof c === 'string' ? c : c.value));
    }

    async function assertHoverFor(
        line: number,
        column: number,
        title: string,
        doc: vscode.TextDocument = toml,
    ) {
        const lines = await hover(line, column, doc);
        assert.ok(lines && lines.length > 0, `Expected hover for ${title} at ${line}:${column}`);
        assert.ok(
            lines[0].startsWith(`**${title}**`),
            `Expected hover header for ${title}, got '${lines[0]}'`,
        );
        return lines;
    }

    async function assertNoHover(line: number, column: number) {
        const lines = await hover(line, column);
        assert.strictEqual(lines, null, `Unexpected hover at ${line}:${column}: ${lines?.[0]}`);
    }

    test('Hover on top-level keys', async () => {
        await assertHoverFor(0, 0, 'name');
        await assertHoverFor(0, 3, 'name');
        await assertHoverFor(1, 2, 'version');
    });

    test('Hover content: required marker, type hint and description', async () => {
        const name = await assertHoverFor(0, 1, 'name');
        assert.ok(name[0].includes('(required)'), `name should be required: '${name[0]}'`);
        const version = await assertHoverFor(1, 1, 'version');
        assert.ok(version[0].includes('`string`'), `version type missing: '${version[0]}'`);
        assert.ok(version.length > 1, 'Description missing from hover');
    });

    test('Hover splits every sentence of the description', async () => {
        // origin's description has three sentences. In-memory document: an
        // [origin] table is invalid in a local manifest and would upset the ALS.
        const doc = await vscode.workspace.openTextDocument({
            language: 'toml',
            content: '[origin]\n',
        });
        const lines = await assertHoverFor(0, 2, 'origin', doc);
        const body = lines.slice(1).filter((l) => l.trim() !== '');
        assert.ok(body.length >= 3, `Expected one line per sentence, got ${body.length}`);
    });

    test('Hyphenated keys match the full word', async () => {
        await assertHoverFor(4, 2, 'maintainers');
        await assertHoverFor(5, 2, 'maintainers-logins');
        await assertHoverFor(5, 14, 'maintainers-logins');
    });

    test('Hover on table header', async () => {
        await assertHoverFor(7, 1, 'depends-on');
        await assertHoverFor(7, 5, 'depends-on');
    });

    test('No hover on values that look like keys', async () => {
        // description = "version tags"
        await assertNoHover(3, 17);
        await assertNoHover(3, 25);
    });

    test('No hover in comments', async () => {
        // # name of the crate
        await assertNoHover(2, 3);
    });

    test('No hover on whitespace or separators', async () => {
        await assertNoHover(6, 0);
        await assertNoHover(0, 5);
    });

    test('No hover on unknown keys inside tables', async () => {
        await assertNoHover(8, 1);
    });
});

/*
 * Child properties, using an in-memory document:
 *   0 [configuration]
 *   1 disabled = true
 *   2 name = "not a configuration key"
 *   3
 *   4 [configuration.values]
 *   5
 *   6 [[actions]]
 *   7 type = "pre-build"
 *   8 directory = "."
 */
suite('Alire hover: child properties', function () {
    let doc: vscode.TextDocument;

    this.beforeAll(async () => {
        await activate();
        assert.ok(adaExtState.alireHoverProvider, 'Alire hover provider should be initialised');
        doc = await vscode.workspace.openTextDocument({
            language: 'toml',
            content: [
                '[configuration]',
                'disabled = true',
                'name = "not a configuration key"',
                '',
                '[configuration.values]',
                '',
                '[[actions]]',
                'type = "pre-build"',
                'directory = "."',
            ].join('\n'),
        });
    });

    async function header(line: number, column: number): Promise<string | undefined> {
        const pos = new vscode.Position(line, column);
        const token = new vscode.CancellationTokenSource().token;
        const result = await adaExtState.alireHoverProvider!.provideHover(doc, pos, token);
        const first = result?.contents[0];
        return typeof first === 'string' ? first : first?.value;
    }

    test('Hover on keys of the enclosing table', async () => {
        assert.ok((await header(1, 2))?.startsWith('configuration.**disabled**'));
        assert.ok((await header(8, 2))?.startsWith('actions.**directory**'));
    });

    test('Required child keys are marked', async () => {
        const text = await header(7, 1);
        assert.ok(text?.startsWith('actions.**type** (required)'), `Unexpected hover: '${text}'`);
        assert.ok(!(await header(8, 2))?.includes('(required)'));
    });

    test('Top-level key names inside a table give no hover', async () => {
        assert.strictEqual(await header(2, 1), undefined);
    });

    test('Hover on nested table headers', async () => {
        assert.ok((await header(4, 3))?.startsWith('**configuration**'));
        assert.ok((await header(4, 17))?.startsWith('configuration.**values**'));
    });
});
