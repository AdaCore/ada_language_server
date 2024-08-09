/* eslint-disable @typescript-eslint/no-non-null-assertion */
import assert from 'assert';
import { envHasExec, findAdaMain, getSymbols, which } from '../../../src/helpers';
import { DocumentSymbol, SymbolKind, Uri, commands, workspace } from 'vscode';
import { rangeToStr } from '../utils';

suite('which and envHasExec', function () {
    test('existing', function () {
        switch (process.platform) {
            case 'win32':
                /* which() relies on PATHEXT which could contain .EXE or .exe.
                   Lowercase the comparison for this test.
                */
                assert(which('where')?.toLowerCase().endsWith('where.exe'));
                assert(envHasExec('where'));
                break;

            default:
                assert(which('sh')?.endsWith('/sh'));
                assert(envHasExec('sh'));
                break;
        }
    });
    test('non-existing', function () {
        assert.equal(which('some-non-existing-exec'), undefined);
        assert(!envHasExec('some-non-existing-exec'));
    });
});

suite('findAdaMain', function () {
    test('Find one main (simple case)', async function () {
        /* Test that findAdaMain works in a simple case */
        const folders = workspace.workspaceFolders;
        assert(folders && folders.length > 0);
        const uri = Uri.joinPath(folders[0].uri, 'src', 'main1.adb');
        const adaMain = await findAdaMain(uri.fsPath);
        assert(adaMain);
    });
    test('Find one main (case sensitivity)', async function () {
        /* Test the behavior of findAdaMain with respect to case sensitivity */
        const folders = workspace.workspaceFolders;
        assert(folders && folders.length > 0);
        const uri_uppercase = Uri.joinPath(folders[0].uri, 'src', 'MAIN1.ADB');
        const adaMain_from_uppercase = await findAdaMain(uri_uppercase.fsPath);

        /* On Windows we should have a main here, otherwise we should not */
        if (process.platform === 'win32') {
            assert(adaMain_from_uppercase);
        } else {
            assert(!adaMain_from_uppercase);
        }
    });
});

suite('getSymbols', function () {
    let symbols: DocumentSymbol[];

    this.beforeAll(async function () {
        assert(workspace.workspaceFolders);
        const uri = Uri.joinPath(workspace.workspaceFolders[0].uri, 'src', 'symbols.adb');
        symbols = await commands.executeCommand<DocumentSymbol[]>(
            'vscode.executeDocumentSymbolProvider',
            uri
        );
    });

    test('Root symbols', function () {
        assert.deepEqual(simplifySymbols(symbols), [
            { range: '0:0 -> 0:9', kind: 'Namespace', name: 'With clauses' },
            { range: '2:0 -> 25:12', kind: 'Module', name: 'Symbols' },
        ]);
    });

    test('getSymbols default recursion args', function () {
        assert.deepEqual(simplifySymbols(getSymbols(symbols, [SymbolKind.Function])), [
            { range: '4:3 -> 23:12', kind: 'Function', name: 'Test' },
            { range: '6:6 -> 17:13', kind: 'Function', name: 'P1' },
            { range: '7:9 -> 14:16', kind: 'Function', name: 'P2' },
            { range: '8:12 -> 11:19', kind: 'Function', name: 'P3' },
        ]);
    });

    test('getSymbols only recurse Module', function () {
        assert.deepEqual(
            simplifySymbols(getSymbols(symbols, [SymbolKind.Function], [SymbolKind.Module])),
            [{ range: '4:3 -> 23:12', kind: 'Function', name: 'Test' }]
        );
    });

    /**
     * A simplified type with a subset of DocumentSymbol properties intended for
     * checking test results.
     */
    type SimpleDocumentSymbol = {
        range: string;
        kind: string;
        name: string;
    };

    function simplifySymbols(s: DocumentSymbol[]): SimpleDocumentSymbol[] {
        return s.map(simplifySymbol);
    }

    function simplifySymbol(s: DocumentSymbol): SimpleDocumentSymbol {
        return {
            range: rangeToStr(s.range),
            kind: SymbolKind[s.kind],
            name: s.name,
        };
    }
});
