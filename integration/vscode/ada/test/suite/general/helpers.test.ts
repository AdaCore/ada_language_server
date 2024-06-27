import assert from 'assert';
import { envHasExec, getSymbols, which } from '../../../src/helpers';
import { DocumentSymbol, SymbolKind, Uri, commands, workspace } from 'vscode';
import { rangeToStr } from '../utils';

suite('which and envHasExec', function () {
    test('existing', function () {
        switch (process.platform) {
            case 'win32':
                assert(which('where')?.endsWith('where.exe'));
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
