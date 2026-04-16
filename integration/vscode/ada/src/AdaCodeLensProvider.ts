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
    CMD_SPARK_PROVE_SUBP,
    CMD_DELETE_METRICS_FOR_FILE,
} from './constants';
import { envHasExec, getSymbols } from './helpers';
import { findMetricsXmlForSource, formatMetric, parseMetricsXml } from './metricsUtils';
import * as vscode from 'vscode';
import { adaExtState } from './extension';

export class AdaCodeLensProvider implements CodeLensProvider {
    private emitter = new vscode.EventEmitter<void>();
    readonly onDidChangeCodeLenses = this.emitter.event;

    async provideCodeLenses(
        document: TextDocument,
        token?: CancellationToken,
    ): Promise<CodeLens[]> {
        const symbols = await commands.executeCommand<DocumentSymbol[] | undefined>(
            'vscode.executeDocumentSymbolProvider',
            document.uri,
        );

        // For main procedures, provide Run and Debug CodeLenses.
        const targetPrefix = await adaExtState.getTargetPrefix();
        const mains = await adaExtState.getMains();
        let codeLenses: CodeLens[] = [];
        if (mains.some((m) => Uri.file(m).fsPath === document.uri.fsPath)) {
            if (symbols) {
                const functions = symbols.filter((s) => s.kind === SymbolKind.Function);
                if (functions.length > 0) {
                    codeLenses = [
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
                }
            }
        }

        // --- Metrics CodeLens ---
        const objectDir = await adaExtState.getObjectDir();
        const metricsXml = findMetricsXmlForSource(document.uri.fsPath, objectDir);
        if (metricsXml) {
            const parsed = await parseMetricsXml(metricsXml);
            if (parsed && Array.isArray(parsed.units)) {
                const { units, displayNames } = parsed;

                for (const unit of units) {
                    if (unit.sloc) {
                        const metrics: string[] = [];
                        for (const [key, value] of Object.entries(unit.metrics)) {
                            const label = displayNames[key] || key;
                            metrics.push(formatMetric(key, label, Number(value)));
                        }
                        if (metrics.length > 0) {
                            const summary = `$(graph) Metrics: ${metrics.join(', ')}`;
                            codeLenses.push(
                                new CodeLens(new vscode.Range(unit.sloc, unit.sloc), {
                                    title: summary,
                                    command: '',
                                }),
                            );
                            codeLenses.push(
                                new CodeLens(new vscode.Range(unit.sloc, unit.sloc), {
                                    title: '$(trash)',
                                    command: CMD_DELETE_METRICS_FOR_FILE,
                                    arguments: [document.uri],
                                }),
                            );
                        }
                    }
                }
            }
        }

        // SPARK CodeLenses (if gnatprove is available)
        let sparkLenses: CodeLens[] = [];
        if (envHasExec('gnatprove')) {
            if (symbols) {
                const symbolKinds = [SymbolKind.Function];
                const recurseInto = [SymbolKind.Module, SymbolKind.Package, SymbolKind.Function];
                const functions = getSymbols(symbols, symbolKinds, recurseInto, token);
                sparkLenses = functions.map((f) => {
                    if (token?.isCancellationRequested) {
                        throw new CancellationError();
                    }
                    return new CodeLens(f.selectionRange, {
                        title: '$(check) Prove',
                        command: CMD_SPARK_PROVE_SUBP,
                        arguments: [document.uri, f.selectionRange],
                    });
                });
            }
        }

        return codeLenses.concat(sparkLenses);
    }
    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    resolveCodeLens?(codeLens: CodeLens, _token: CancellationToken): ProviderResult<CodeLens> {
        if (codeLens.command) {
            return codeLens;
        } else {
            throw new Error(`Cannot resolve CodeLens`);
        }
    }

    refresh() {
        this.emitter.fire();
    }
}
