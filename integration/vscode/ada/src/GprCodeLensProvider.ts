import { CMD_TOOL_DOCUMENTATION } from './constants';
import { getSymbols, which } from './helpers';
import * as path from 'path';
import * as vscode from 'vscode';

/*
 * Add a CodeLensProvider for .gpr files
 * The codelenses allow to open documentation for GNATSAS, GNATDAS
 * and GNATProve. It will first search for local documentations if the
 * tools are found in the PATH and fallback for the online documentation.
 */

export class GprCodeLensProvider implements vscode.CodeLensProvider {
    constructor() {
        this.updateDocumentation();
    }

    private onlineDocs: Record<string, string> = {
        PROVE: 'https://docs.adacore.com/spark2014-docs/html/ug/en/appendix/project_attributes.html',
        ANALYZER:
            'https://docs.adacore.com/live/wave/gnatsas/html/user_guide/project_setup.html#configuring-the-analysis',
        COVERAGE: 'https://docs.adacore.com/gnatdas-docs/html/gnatcov/getting_started.html',
    };

    private localDocs: Record<string, string> = {};

    private updateDocumentation(): void {
        const gnatprove_path = which('gnatprove');
        if (gnatprove_path) {
            this.localDocs['PROVE'] = path.join(
                path.dirname(gnatprove_path),
                '..',
                'share',
                'doc',
                'spark',
                'html',
                'ug',
                'en',
                'gnatprove.html',
            );
        }
        const gnatsas = which('gnatsas');
        if (gnatsas) {
            this.localDocs['ANALYZER'] = path.join(
                path.dirname(gnatsas),
                '..',
                'share',
                'doc',
                'gnatsas',
                'users_guide',
                'html',
                'index.html',
            );
        }
        const gnatdas = which('gnatcov');
        if (gnatdas) {
            this.localDocs['COVERAGE'] = path.join(
                path.dirname(gnatdas),
                '..',
                'share',
                'doc',
                'gnatdas',
                'html',
                'index.html',
            );
        }
    }

    async provideCodeLenses(document: vscode.TextDocument): Promise<vscode.CodeLens[]> {
        const codeLenses: vscode.CodeLens[] = [];

        const symbols = await vscode.commands.executeCommand<vscode.DocumentSymbol[]>(
            'vscode.executeDocumentSymbolProvider',
            document.uri,
        );

        if (symbols) {
            const symbolKinds = [vscode.SymbolKind.Package];
            const recurseInto = [vscode.SymbolKind.Module, vscode.SymbolKind.Package];
            const packages = getSymbols(symbols, symbolKinds, recurseInto);

            for (const pack of packages) {
                const packageName = pack.name;
                const line = pack.range.start.line;

                // Insensitive string comparison, do not change packageName casing itself
                // because it will be reused in the tooltip
                const local = this.localDocs[packageName.toUpperCase()];
                const url = this.onlineDocs[packageName.toUpperCase()];

                if (local) {
                    codeLenses.push(
                        new vscode.CodeLens(new vscode.Range(line, 0, line, 0), {
                            title: `$(link-external) Open Local ${packageName} Documentation`,
                            command: CMD_TOOL_DOCUMENTATION,
                            arguments: [local],
                        }),
                    );
                } else if (url) {
                    codeLenses.push(
                        new vscode.CodeLens(new vscode.Range(line, 0, line, 0), {
                            title: `$(link-external) Open Online ${packageName} Documentation`,
                            command: CMD_TOOL_DOCUMENTATION,
                            arguments: [url],
                        }),
                    );
                } else {
                    // Most packages are defined by GPR, use it as the default documentation
                    codeLenses.push(
                        new vscode.CodeLens(new vscode.Range(line, 0, line, 0), {
                            title: `$(link-external) Open GPRbuild Documentation`,
                            command: CMD_TOOL_DOCUMENTATION,
                            arguments: [
                                'https://docs.adacore.com/gprbuild-docs/html/gprbuild_ug/introduction.html',
                            ],
                        }),
                    );
                }
            }
        }

        return codeLenses;
    }
}
