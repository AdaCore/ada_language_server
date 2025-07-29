import * as vscode from 'vscode';
import { VisualizerHandler } from '../alsVisualizerProvider';

/**
 * Specialization of the Generic Visualizer Handler for C/CPP language.
 */
export class CPPVisualizerHandler extends VisualizerHandler {
    async getFunctionBodyLocation(location: vscode.Location) {
        const implementations = await vscode.commands.executeCommand<
            (vscode.Location | vscode.LocationLink)[]
        >('vscode.executeDefinitionProvider', location.uri, location.range.start);
        if (implementations.length > 0) return implementations[0];
        return null;
    }

    getParentSymbolWholeRange(
        symbol: vscode.SymbolInformation | vscode.DocumentSymbol,
        label: string,
        location: vscode.Location | vscode.LocationLink,
    ): { name: string; range: vscode.Range } | null {
        symbol.name = symbol.name.split('(')[0];
        label = label.split('(')[0];
        return super.getParentSymbolWholeRange(symbol, label, location);
    }
}
