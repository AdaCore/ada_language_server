import assert from 'assert';
import {
    envHasExec,
    findAdaMain,
    getSymbols,
    parallelize,
    staggerProgress,
    which,
} from '../../src/helpers';
import {
    CancellationError,
    DocumentSymbol,
    ProgressLocation,
    SymbolKind,
    Uri,
    commands,
    window,
    workspace,
} from 'vscode';
import { rangeToStr } from '../utils';
import { adaExtState } from '../../src/extension';

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

suite('getProjectAttributeValue', function () {
    test('Get project attribute value (simple case)', async function () {
        /* Basic test for getProjectAttributeValue */
        const switches = await adaExtState.getProjectAttributeValue(
            'Default_Switches',
            'Compiler',
            'Ada',
        );
        assert.deepEqual(switches, ['-g', '-O0']);
    });

    test('Get unknown project attribute value', async function () {
        /* Check that we get an exception when trying to get the value
        of an unknown attribute */

        let error = undefined;
        try {
            await adaExtState.getProjectAttributeValue('Unknown');
        } catch (e) {
            error = e;
        }
        assert.notEqual(error, undefined);
    });
});

suite('getSymbols', function () {
    let symbols: DocumentSymbol[];

    this.beforeAll(async function () {
        assert(workspace.workspaceFolders);
        const uri = Uri.joinPath(workspace.workspaceFolders[0].uri, 'src', 'symbols.adb');
        symbols = await commands.executeCommand<DocumentSymbol[]>(
            'vscode.executeDocumentSymbolProvider',
            uri,
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
            [{ range: '4:3 -> 23:12', kind: 'Function', name: 'Test' }],
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

suite('parallelize', function () {
    const cases = [
        {
            size: 4,
            threads: 1,
            workDuration: 200,
        },
        {
            size: 4,
            threads: 2,
            workDuration: 200,
        },
        {
            size: 4,
            threads: 3,
            workDuration: 200,
        },
        {
            size: 4,
            threads: 4,
            workDuration: 200,
        },
        {
            size: 4,
            threads: 5,
            workDuration: 200,
        },
        {
            size: 2 ** 10,
            threads: 3,
            workDuration: 0,
        },
        {
            size: 2 ** 20,
            threads: 3,
            workDuration: 0,
        },
    ];

    for (const tc of cases) {
        testCase(tc.size, tc.threads, tc.workDuration);
    }

    function testCase(dataSize: number, threads: number, singleWorkDurationMs: number) {
        test(`Process ${dataSize} elements with ${threads} threads`, async function () {
            const data: number[] = Array.from({ length: dataSize }, (_, i) => i);
            let done = 0;
            let lastProgressDone = 0;
            const start = Date.now();
            const result = await window.withProgress(
                {
                    location: ProgressLocation.Notification,
                    cancellable: true,
                    title: this.currentTest?.fullTitle(),
                },
                async (progress, token) => {
                    return await parallelize(
                        data,
                        threads,
                        async (i) => {
                            if (token.isCancellationRequested) {
                                throw new CancellationError();
                            }

                            /**
                             * Do we need a lock to increment this counter given
                             * that we are processing with threads?
                             *
                             * The answer is no.
                             *
                             * In JavaScript semantics each function runs to
                             * completion uninterrupted. 'async' doesn't mean that
                             * functions run concurrently. It means that something
                             * will be executed later.
                             *
                             * When a function flow encounters 'await', it hands
                             * off processing to another async operation.
                             *
                             * So at any one time, only one function is executing
                             * and accessing the counter. It is safe to increment
                             * without locking.
                             */
                            ++done;

                            const reportEveryProgress = false;
                            if (reportEveryProgress) {
                                progress.report({
                                    message: `${done} / ${data.length}`,
                                    increment: (1 * 100) / data.length,
                                });
                            } else {
                                lastProgressDone = staggerProgress(
                                    done,
                                    data.length,
                                    lastProgressDone,
                                    (increment) => {
                                        progress.report({
                                            message: `${done} / ${data.length}`,
                                            increment: increment,
                                        });
                                    },
                                );
                            }

                            /**
                             * Short-circuit the timing emulation if the test case doesn't want it.
                             */
                            if (singleWorkDurationMs > 0) {
                                /**
                                 * Create artificial delay in the processing of each item.
                                 */
                                await new Promise((resolve) => {
                                    setTimeout(resolve, singleWorkDurationMs);
                                });
                            }

                            return i + 1;
                        },
                        token,
                    );
                },
            );
            const duration = Date.now() - start;

            assert.equal(result.length, data.length);

            /**
             * Don't check timing when the test cases doesn't use timing emulation.
             */
            if (singleWorkDurationMs > 0) {
                /**
                 * Given the number of threads, the overal duration should
                 * not go beyond a certain limit. For example, if the data
                 * size is 4, and we requested 4 threads to do the
                 * operation, then all the data is processed in one go. So
                 * overall the computation shouldn't take more than the
                 * time needed to process one data item (with a margin).
                 */
                const margin = singleWorkDurationMs * 0.2;
                /**
                 * When processing 4 items with 3 threads, the first parallel
                 * batch processes 3 items and a second batch processes 1 item.
                 * So there are 2 sequential iterations, hence the use of
                 * Math.ceil.
                 */
                const sequentialChunks = Math.ceil(dataSize / threads);
                const expectedTotalWorkDuration = sequentialChunks * singleWorkDurationMs + margin;

                assert.ok(
                    duration < expectedTotalWorkDuration,
                    `The computation took ${duration}ms when we expected` +
                        ` no more than ${expectedTotalWorkDuration}`,
                );
            }
        });
    }
});

suite('staggerProgress', function () {
    test('staggerProgress', function () {
        const total = 1000;
        let lastProgress = 0;

        lastProgress = staggerProgress(5, total, lastProgress, () => {
            /**
             * progress is less than 1% so this shouldn't be called.
             */
            assert.fail('Progress reporting was called unexpectedly');
        });
        assert.equal(lastProgress, 0);

        let called = false;
        lastProgress = staggerProgress(10, total, lastProgress, (increment) => {
            /**
             * Progress is 1%. Reporting should be called.
             */
            called = true;
            assert.equal(increment, 1);
        });
        assert.ok(called, 'Progress reporting function was unexpectedly not called');
        assert.equal(lastProgress, 10);

        lastProgress = staggerProgress(50, total, lastProgress, (increment) => {
            /**
             * Progress is 1%. Reporting should be called.
             */
            called = true;
            assert.equal(increment, 4);
        });
        assert.ok(called, 'Progress reporting function was unexpectedly not called');
        assert.equal(lastProgress, 50);
    });
});
