import assert from 'assert';
import * as vscode from 'vscode';
import { adaExtState } from '../../src/extension';
import { activate } from '../utils';
import { existsSync } from 'fs';
import { Fields, Snippets } from '../../src/alireProperties';

const allFields = Fields.map((f) => f.title);
const allSnippets = Snippets.map((f) => f.title);

suite('provideCompletion (alire.toml)', function () {
    this.beforeAll(async () => {
        await activate();
        assert.ok(adaExtState, 'Extension should be initialized after activation');
    });

    this.beforeEach(() => {
        assert.ok(
            adaExtState.alireCompletionProvider,
            'Alire completion provider should be initialised after activation',
        );
    });
    const folder = vscode.workspace.workspaceFolders![0].uri;

    /**
     * Expects an 'alire.toml' file at ws/[subdir]/alire.toml
     * Check completions provided against those expected
     */
    async function testCrate(
        subdir: string,
        line: number,
        column: number,
        expectedFields: string[] = allFields,
        expectedSnippets: string[] = [],
        trigger?: string,
    ) {
        const uri = vscode.Uri.joinPath(folder, subdir, 'alire.toml');
        assert.ok(existsSync(uri.fsPath), `Path not found: ${uri.fsPath}`);
        const toml = await vscode.workspace.openTextDocument(uri);
        assert.ok(toml, 'Failed to open alire.toml');
        const langId = toml.languageId;
        assert.strictEqual(
            langId,
            'toml',
            `Expected alire.toml languageId is 'toml', not '${langId}'`,
        );

        /** Create params for completion request */
        const position = new vscode.Position(line, column);
        // Check document hasn't changed
        const pos = toml.validatePosition(position);
        assert.strictEqual(position, pos, 'Document changed during test');
        const token = new vscode.CancellationTokenSource().token;
        assert.strictEqual(false, token.isCancellationRequested);
        const ctxt: vscode.CompletionContext = {
            triggerKind: vscode.CompletionTriggerKind.Invoke,
            triggerCharacter: trigger,
        };
        /* List of all completions provided */
        const provider = adaExtState.alireCompletionProvider;
        const completions = provider.provideCompletionItems(toml, pos, token, ctxt);
        const labels = completions?.map((label) => label.toName());

        /* Check if any snippet completions were provided */
        const providedSnippets = completions.flatMap((comp) =>
            comp.isSnippet() ? comp.toName() : [],
        );
        const numSnippets = providedSnippets.length;
        const shouldProvideSnippets = trigger && trigger == ' ' && expectedSnippets.length > 0;

        /* Only expect snippets if the correct trigger character is provided*/
        if (shouldProvideSnippets) {
            assert.deepStrictEqual(numSnippets, expectedSnippets.length);
        } else {
            assert.deepStrictEqual(
                numSnippets,
                0,
                "Snippets not expected, triggerCharacter != ' '",
            );
        }
        // Then required fields listed alphabetically
        const mandatory = completions.flatMap((comp) => (comp.isRequired() ? comp.toName() : []));
        const numRequired = mandatory.length;
        assert.deepStrictEqual(
            labels.slice(numSnippets, numSnippets + numRequired),
            mandatory,
            'Required fields should be listed before others',
        );

        // Check total number of completions provided
        const expectedCompletions = expectedSnippets.concat(expectedFields);
        const numExpectedCompletions = expectedCompletions.length;
        const numCompletions = labels.length;
        assert.deepStrictEqual(
            numCompletions,
            numExpectedCompletions,
            `Expected ${numExpectedCompletions} completions, not ${numCompletions}`,
        );

        // Now check all completions suggested in correct order
        assert.deepStrictEqual(labels?.join(','), expectedCompletions.join(','));
    }

    test('Empty file (no trigger)', async () => {
        /** Expect all fields to be suggested */
        await testCrate('empty', 0, 0, allFields);
    });
    test('Empty file (trigger)', async () => {
        /** Expect all fields to be suggested */
        await testCrate('empty', 0, 0, allFields, allSnippets, ' ');
    });
    test('Minimal file (no trigger)', async () => {
        /** Expect all fields except those included to be suggested */
        await testCrate('.', 3, 0, [
            'auto-gpr-with',
            'licenses',
            'long-description',
            'notes',
            'website',
            'authors',
            'executables',
            'maintainers',
            'maintainers-logins',
            'project-files',
            'tags',
        ]);
    });
    /** All top-level keys must come before tables in TOML syntax */
    test('Tables (above)', async () => {
        await testCrate('comm', 3, 0, [
            'auto-gpr-with',
            'long-description',
            'notes',
            'executables',
            'project-files',
        ]);
    });
    test('Tables (below)', async () => {
        /** Expect no completions whatsoever */
        await testCrate('comm', 14, 0, [], [], ' ');
    });
});

suite('Completion item text', function () {
    const listProperties = [
        'executables',
        'maintainers',
        'maintainers-logins',
        'project-files',
        'tags',
    ];
    const stringProperties = [
        'name',
        'description',
        'version',
        'licenses',
        'long-description',
        'notes',
        'website',
    ];

    function testProperty(title: string, typeAnnotation: string, insertion: string) {
        const prop = Fields.find((f) => f.title == title);
        assert.ok(prop, `Could not find property named ${title}`);
        const label = prop.toCompletionLabel();
        assert.deepStrictEqual(label.label, title);
        assert.deepStrictEqual(label.description, typeAnnotation, `${title} type wrong`);
        if (prop.required) {
            assert.deepStrictEqual(
                label.detail,
                '(required)',
                `${title} missing 'required' detail`,
            );
        }
        const completion = prop.toCompletionItem();
        assert.deepStrictEqual(completion.insertText, `${title}${insertion}`);
    }

    test('Test all completions inserted', () => {
        listProperties.forEach((p) => testProperty(p, 'String List', ' = []'));
        stringProperties.forEach((p) => testProperty(p, 'String', ' = ""'));
        // Only have one bool property so test individually
        testProperty('auto-gpr-with', 'Bool (Default: true)', ' = true');
    });
});
