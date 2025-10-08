import * as vscode from 'vscode';
import { VisualizerHandler } from '../alsVisualizerProvider';
import { Hierarchy } from '../visualizerTypes';
import * as fs from 'fs';

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

    async getBodyLocation(location: vscode.Location, hierarchy: Hierarchy) {
        if (!fs.existsSync(location.uri.fsPath)) return null;
        if (hierarchy === Hierarchy.TYPE) return [location];

        // The Go To Implementation is not implemented in Clangd so we use the
        // Go To Definition.
        const implementations = await vscode.commands.executeCommand<
            (vscode.Location | vscode.LocationLink)[]
        >('vscode.executeDefinitionProvider', location.uri, location.range.start);

        if (implementations.length > 0) return implementations;
        return null;
    }
}
