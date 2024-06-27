import assert from 'assert';
import { DocumentSymbol, SymbolKind, commands } from 'vscode';
import { envHasExec, getSymbols } from '../../../src/helpers';
import {
    activate,
    closeAllEditors,
    codeLensesToString,
    getCodeLenses,
    showTextDocument,
    simplifyCodelenses,
} from '../utils';

suite('CodeLens', function () {
    this.beforeAll(async () => {
        await activate();
    });

    this.beforeEach(async function () {
        await closeAllEditors();
    });

    test('in main file offer run & debug', async () => {
        const codelenses = await getCodeLenses('src', 'main1.adb');
        assert.deepEqual(simplifyCodelenses(codelenses), [
            {
                range: '9:0 -> 15:9',
                command: {
                    title: '$(run) Run',
                    command: 'ada.buildAndRunMain',
                    arguments: ['src/main1.adb'],
                },
            },
            {
                range: '9:0 -> 15:9',
                command: {
                    title: '$(debug-alt-small) Debug',
                    command: 'ada.buildAndDebugMain',
                    arguments: ['src/main1.adb'],
                },
            },
        ]);
    });

    test("in non-main file don't offer run & debug", async () => {
        const srcRelPath = ['src', 'foo.ads'];
        const codelenses = await getCodeLenses(...srcRelPath);

        if (codelenses) {
            /**
             * Assert that run and debug codelenses have not been provided
             */
            assert(
                codelenses.every((cl) => {
                    assert(cl.command?.command);
                    return (
                        !cl.command.command.toLowerCase().includes('run') &&
                        !cl.command.command.toLowerCase().includes('debug')
                    );
                })
            );
        }
    });

    test('no SPARK codelenses in SPARK-less env', async function () {
        assert(!envHasExec('gnatprove'), 'This test must run in an env without gnatprove');

        const srcRelPath = ['src', 'bar.ads'];
        /**
         * Check that the test file contains subprograms.
         */
        const textEditor = await showTextDocument(...srcRelPath);
        const symbols = await commands.executeCommand<DocumentSymbol[]>(
            'vscode.executeDocumentSymbolProvider',
            textEditor.document.uri
        );
        assert(getSymbols(symbols, [SymbolKind.Function]).length > 0);

        const codelenses = await getCodeLenses(...srcRelPath);
        if (codelenses) {
            /**
             * Assert that SPARK codelenses were not provided for the subprograms
             */
            assert(
                !codeLensesToString(codelenses).toLowerCase().includes('prove'),
                `CodeLense for SPARK was unexpectedly provided:\n${codeLensesToString(codelenses)}`
            );
        }
    });
});
