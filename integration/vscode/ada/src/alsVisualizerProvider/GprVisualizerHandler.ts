import { VisualizerHandler } from '../alsVisualizerProvider';
import {
    ALS_GprDependencyDirection,
    ALS_GprDependencyItem,
    ALS_GprDependencyKind,
    ALS_GprDependencyParam,
    EdgeType,
    Hierarchy,
    RelationDirection,
} from '../visualizerTypes';
import * as vscode from 'vscode';
import { logger } from '../extension';
import path from 'path';
import * as fs from 'fs';
import { bindNodes, createNodeHierarchy, NodesSingleton } from '../alsVisualizerUtils';

/**
 * Specialization of the Generic Visualizer Handler for Ada GPR language.
 */
export class GprVisualizerHandler extends VisualizerHandler {
    async provideHierarchy(
        location: vscode.Location,
        hierarchy: Hierarchy,
        languageId: string,
        direction: RelationDirection,
    ) {
        if (hierarchy !== Hierarchy.GPR) {
            logger.error('Gpr Visualizer only handles GPR Hierarchies');
            return;
        }

        const baseSymbol = {
            name: path.basename(location.uri.fsPath),
            location: new vscode.Location(location.uri, new vscode.Position(0, 0)),
            kind: vscode.SymbolKind.Module,
        };

        // The middle node represent the current main symbol in the graph (the symbol the
        // visualizer was launched on or the symbol for which we are calculating
        // its parent or children).
        const middleNode = NodesSingleton.insertSymbolsMap(
            await createNodeHierarchy(baseSymbol, hierarchy, languageId, false),
        );

        if (!middleNode) return;

        const gprDependencies = await vscode.commands.executeCommand<ALS_GprDependencyItem[]>(
            'als-gpr-dependencies',
            {
                uri: location.uri.toString(),
                direction:
                    direction === RelationDirection.SUB
                        ? ALS_GprDependencyDirection.SHOW_OUTGOING
                        : ALS_GprDependencyDirection.SHOW_INCOMING,
            } as ALS_GprDependencyParam,
        );

        for (const gprDependency of gprDependencies) {
            const uri = vscode.Uri.parse(gprDependency.uri);
            if (!fs.existsSync(uri.fsPath)) continue;

            // As the symbol represent the whole file, the position doesn't matter so
            // we set it to the beginning
            const symbol = {
                name: path.basename(uri.fsPath),
                location: new vscode.Location(uri, new vscode.Position(0, 0)),
                kind: vscode.SymbolKind.Module,
            };
            const node = await createNodeHierarchy(symbol, Hierarchy.GPR, languageId, false);
            if (!node) continue;
            bindNodes(
                middleNode,
                node,
                direction,
                gprDependency.kind === ALS_GprDependencyKind.IMPORTED
                    ? EdgeType.DOTTED
                    : gprDependency.kind === ALS_GprDependencyKind.AGGREGATED
                      ? EdgeType.BOXED
                      : EdgeType.REGULAR,
            );
        }

        // Once the process is done we want the graph to focus on this specific node.
        middleNode.focus = true;
        NodesSingleton.focusedNode = middleNode;

        // The middle node is expanded by default when we are getting its children.
        middleNode.expanded = direction === RelationDirection.SUPER ? middleNode.expanded : true;

        if (
            (direction === RelationDirection.SUB || direction === RelationDirection.BOTH) &&
            middleNode.children.length === 0
        )
            middleNode.hasChildren = false;
        else if (
            (direction === RelationDirection.SUPER || direction === RelationDirection.BOTH) &&
            middleNode.parents.length === 0
        )
            middleNode.hasParent = false;
        NodesSingleton.findRoots();
        return middleNode;
    }
}
