import {
    CancellationError,
    CancellationToken,
    CodeLens,
    CodeLensProvider,
    DocumentSymbol,
    Event,
    ProviderResult,
    SymbolKind,
    TextDocument,
    commands,
    Uri,
} from 'vscode';
import {
    CMD_BUILD_AND_DEBUG_GNATEMULATOR,
    CMD_BUILD_AND_DEBUG_MAIN,
    CMD_BUILD_AND_RUN_GNATEMULATOR,
    CMD_BUILD_AND_RUN_MAIN,
    CMD_SPARK_PROVE_SUBP
} from './constants';
import { envHasExec, getSymbols } from './helpers';
import { adaExtState } from './extension';

export class AdaCodeLensProvider implements CodeLensProvider {
    onDidChangeCodeLenses?: Event<void> | undefined;
    provideCodeLenses(
        document: TextDocument,
        token?: CancellationToken,
    ): ProviderResult<CodeLens[]> {
        const symbols = commands.executeCommand<DocumentSymbol[] | undefined>(
            'vscode.executeDocumentSymbolProvider',
            document.uri,
        );

        /**
         * For main procedures, provide Run and Debug CodeLenses.
         */
        const res1 = adaExtState.getTargetPrefix().then((targetPrefix) => {
            return adaExtState.getMains().then((mains) => {
                if (
                    mains.some(
                        (m) =>
                            // Here we go through the Uri class to benefit from the normalization
                            // of path casing on Windows. See Uri.fsPath documentation.
                            Uri.file(m).fsPath == document.uri.fsPath,
                    )
                ) {
                    // It's a main file, so let's offer Run and Debug actions on the main subprogram
                    return symbols.then((symbols) => {
                        if (!symbols) {
                            return [];
                        }

                        const functions = symbols.filter((s) => s.kind == SymbolKind.Function);
                        if (functions.length > 0) {
                            /**
                             * We choose to provide the CodeLenses on the first
                             * subprogram of the file. It may be possible that the
                             * main subprogram is not the first one, but that's an
                             * unlikely scenario that we choose not to handle for
                             * the moment.
                             */
                            let codeLenses = [
                                new CodeLens(functions[0].range, {
                                    command: CMD_BUILD_AND_RUN_MAIN,
                                    title: '$(run) Run',
                                    arguments: [document.uri],
                                }),
                                new CodeLens(functions[0].range, {
                                    command: CMD_BUILD_AND_DEBUG_MAIN,
                                    title: '$(debug-alt-small) Debug',
                                    arguments: [document.uri],
                                }),
                            ];

                            // It's not a native project: provide a 'Run with GNATemulator' CodeLens
                            // if GNATemulator for the given target is present in the user's env.
                            if (targetPrefix && envHasExec(targetPrefix + '-gnatemu')) {
                                codeLenses = codeLenses.concat([
                                    new CodeLens(functions[0].range, {
                                        command: CMD_BUILD_AND_RUN_GNATEMULATOR,
                                        title: '$(run) Run with GNATemulator',
                                        arguments: [document.uri],
                                    }),
                                    new CodeLens(functions[0].range, {
                                        command: CMD_BUILD_AND_DEBUG_GNATEMULATOR,
                                        title: '$(debug-alt-small) Debug with GNATemulator',
                                        arguments: [document.uri],
                                    }),
                                ]);
                            }

                            return codeLenses;
                        } else {
                            return [];
                        }
                    });
                } else {
                    return [];
                }
            });
        });

        let res2;
        if (envHasExec('gnatprove')) {
            /**
             * This is tentative deactivated code in preparation of SPARK support.
             */
            res2 = symbols.then<CodeLens[]>((symbols) => {
                if (!symbols) {
                    return [];
                }

                const symbolKinds = [SymbolKind.Function];
                const recurseInto = [SymbolKind.Module, SymbolKind.Package, SymbolKind.Function];

                // Create a named reduce function to implement a recursive visit of symbols
                const functions = getSymbols(symbols, symbolKinds, recurseInto, token);

                return functions.map((f) => {
                    if (token?.isCancellationRequested) {
                        throw new CancellationError();
                    }

                    return new CodeLens(f.selectionRange, {
                        title: '$(check) Prove',
                        command: CMD_SPARK_PROVE_SUBP,
                        arguments: [document.uri, f.selectionRange],
                    });
                });
            });
        } else {
            res2 = Promise.resolve([]);
        }

        return Promise.all([res1, res2]).then((results) => {
            return results[0].concat(results[1]);
        });
    }
    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    resolveCodeLens?(codeLens: CodeLens, _token: CancellationToken): ProviderResult<CodeLens> {
        if (codeLens.command) {
            return codeLens;
        } else {
            throw new Error(`Cannot resolve CodeLens`);
        }
    }
}
