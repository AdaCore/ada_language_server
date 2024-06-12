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
import { CMD_BUILD_AND_DEBUG_MAIN, CMD_BUILD_AND_RUN_MAIN } from './commands';
import { getMains, getSymbols } from './helpers';

export class AdaCodeLensProvider implements CodeLensProvider {
    static readonly ENABLE_SPARK_CODELENS = false;

    onDidChangeCodeLenses?: Event<void> | undefined;
    provideCodeLenses(
        document: TextDocument,
        token?: CancellationToken
    ): ProviderResult<CodeLens[]> {
        const symbols = commands.executeCommand<DocumentSymbol[]>(
            'vscode.executeDocumentSymbolProvider',
            document.uri
        );

        /**
         * For main procedures, provide Run and Debug CodeLenses.
         */
        const res1 = getMains().then((mains) => {
            if (
                mains.some(
                    (m) =>
                        // Here we go through the Uri class to benefit from the normalization
                        // of path casing on Windows. See Uri.fsPath documentation.
                        Uri.file(m).fsPath == document.uri.fsPath
                )
            ) {
                // It's a main file, so let's offer Run and Debug actions on the main subprogram
                return symbols.then((symbols) => {
                    const functions = symbols.filter((s) => s.kind == SymbolKind.Function);
                    if (functions.length > 0) {
                        /**
                         * We choose to provide the CodeLenses on the first
                         * subprogram of the file. It may be possible that the
                         * main subprogram is not the first one, but that's an
                         * unlikely scenario that we choose not to handle for
                         * the moment.
                         */
                        return [
                            new CodeLens(functions[0].range, {
                                command: CMD_BUILD_AND_RUN_MAIN,
                                title: '$(run) Run',
                                arguments: [document.uri],
                            }),
                            // TODO implement this command
                            new CodeLens(functions[0].range, {
                                command: CMD_BUILD_AND_DEBUG_MAIN,
                                title: '$(debug-alt-small) Debug',
                                arguments: [document.uri],
                            }),
                        ];
                    } else {
                        return [];
                    }
                });
            } else {
                return [];
            }
        });

        let res2;
        if (AdaCodeLensProvider.ENABLE_SPARK_CODELENS) {
            /**
             * This is tentative deactivated code in preparation of SPARK support.
             */
            res2 = symbols.then<CodeLens[]>((symbols) => {
                const symbolKinds = [SymbolKind.Function];
                const recurseInto = [SymbolKind.Module, SymbolKind.Package, SymbolKind.Function];

                // Create a named reduce function to implement a recursive visit of symbols
                const functions = getSymbols(symbols, symbolKinds, recurseInto, token);

                return functions.map((f) => {
                    if (token?.isCancellationRequested) {
                        throw new CancellationError();
                    }
                    // TODO make SPARK codelenses conditional to the availability of SPARK on PATH
                    return new CodeLens(f.range, {
                        title: '$(play-circle) Prove',
                        command: 'TODO',
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
