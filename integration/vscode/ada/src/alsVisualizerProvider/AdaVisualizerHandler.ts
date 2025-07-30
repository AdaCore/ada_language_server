import * as vscode from 'vscode';
import * as fs from 'fs';
import { VisualizerHandler } from '../alsVisualizerProvider';
import {
    ALS_ShowDependenciesKind,
    ALS_Unit_Description,
    EdgeType,
    Hierarchy,
    NodeHierarchy,
    RelationDirection,
} from '../visualizerTypes';
import { bindNodes, createNodeHierarchy, NodesSingleton } from '../alsVisualizerUtils';
import path from 'path';
import { logger } from '../extension';

/**
 * Specialization of the Generic Visualizer Handler for the Ada language.
 */
export class AdaVisualizerHandler extends VisualizerHandler {
    async generateNodeId(nodeLocation: vscode.Location, label: string = '') {
        let hoverValues: string = '';

        let path = nodeLocation.uri.fsPath;

        if (fs.existsSync(nodeLocation.uri.fsPath)) {
            const hovers = await vscode.commands.executeCommand<vscode.Hover[]>(
                'vscode.executeHoverProvider',
                nodeLocation.uri,
                nodeLocation.range.start,
            );
            for (const hover of hovers) {
                hoverValues +=
                    (hoverValues.length === 0 ? '' : '/') +
                    // Collapse multiple following whitespaces into one
                    (hover.contents[0] as vscode.MarkdownString).value.replace(/\s+/g, ' ').trim();
            }
            path = fs.realpathSync(nodeLocation.uri.fsPath);
        } else hoverValues = label;

        // Expand the symlinks to avoid getting the same node twice with a different path.
        const clearId = path + ':' + hoverValues;

        // Hash the file uri and the symbol location to get the id
        const hash = await crypto.subtle.digest('SHA-1', new TextEncoder().encode(clearId));
        return Array.from(new Uint8Array(hash))
            .map((byte) => byte.toString(16).padStart(2, '0'))
            .join('');
    }

    isWithinWorkspace(uri: vscode.Uri) {
        return !uri.fsPath.includes('adainclude');
    }

    async provideHierarchy(
        location: vscode.Location,
        hierarchy: Hierarchy,
        languageId: string,
        direction: RelationDirection,
    ) {
        if (hierarchy === Hierarchy.CALL || hierarchy === Hierarchy.TYPE)
            return await super.provideHierarchy(location, hierarchy, languageId, direction);
        else if (hierarchy !== Hierarchy.FILE) {
            logger.error('Unknown type of hierarchy for Ada Language');
            return;
        }

        // -------------------------------- BEGIN NESTED FUNCTIONS ---------------------------------
        /**
         *
         * @param dependenciesKind - Whether the user want the imported or importing files.
         * @param location - The location of the files we want to get the hierarchy.
         * @param direction - The direction of the hierarchy.
         * @param languageId - The id of the language the symbol is in.
         * @param middleNode - The node from which the hierarchy is executed.
         */
        async function getFile(
            dependenciesKind: ALS_ShowDependenciesKind,
            location: vscode.Location,
            direction: RelationDirection,
            languageId: string,
            middleNode: NodeHierarchy,
        ) {
            // Get the packages depending on the current one.
            const dependencies = await vscode.commands.executeCommand<ALS_Unit_Description[]>(
                'als-show-dependencies',
                {
                    uri: location.uri.toString(),
                    kind: dependenciesKind,
                    showImplicit: false,
                },
            );

            for (const dependency of dependencies) {
                const uri = vscode.Uri.parse(dependency.uri);
                if (!fs.existsSync(uri.fsPath)) continue;

                // As the symbol represents the whole file, the position doesn't matter so
                // we set it to the beginning.
                const symbol = {
                    name: path.basename(uri.fsPath),
                    location: new vscode.Location(uri, new vscode.Position(0, 0)),
                    kind: vscode.SymbolKind.File,
                };
                const node = await createNodeHierarchy(symbol, Hierarchy.FILE, languageId, false);
                if (!node) continue;
                bindNodes(middleNode, node, direction, EdgeType.REGULAR);
            }
        }
        // -------------------------------- END NESTED FUNCTIONS --------------------------------

        const baseSymbol = {
            name: path.basename(location.uri.fsPath),
            location: new vscode.Location(location.uri, new vscode.Position(0, 0)),
            kind: vscode.SymbolKind.File,
        };

        // The middle node represents the current main symbol in the graph (the symbol the
        // visualizer was launched on or the symbol for which we are calculating
        // its parent or children).
        const middleNode = NodesSingleton.insertSymbolsMap(
            await createNodeHierarchy(baseSymbol, hierarchy, languageId, false),
        );

        if (!middleNode) return;

        // If we want to expand the graph in both directions, we will enter into the
        // two if which will fill the children first and the the parents.
        if (direction === RelationDirection.SUB || direction === RelationDirection.BOTH) {
            await getFile(
                ALS_ShowDependenciesKind.SHOW_IMPORTED,
                location,
                RelationDirection.SUB,
                languageId,
                middleNode,
            );
        }
        if (direction === RelationDirection.SUPER || direction === RelationDirection.BOTH) {
            await getFile(
                ALS_ShowDependenciesKind.SHOW_IMPORTING,
                location,
                RelationDirection.SUPER,
                languageId,
                middleNode,
            );
        }

        // Once the process is done, we want the graph to focus on this specific node.
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

    getParentSymbolWholeRange(
        symbol: vscode.SymbolInformation | vscode.DocumentSymbol,
        label: string,
        location: vscode.Location | vscode.LocationLink,
    ): { name: string; range: vscode.Range } | null {
        const range = 'range' in location ? location.range : location.targetRange;
        // If children property is set this is a DocumentSymbol.
        if ('children' in symbol) {
            // If the symbol has the same name and selection range we directly return it.
            if (symbol.name === label && symbol.selectionRange.isEqual(range))
                return { name: symbol.name, range: symbol.range };
            // Else if the symbol contains the range we check if we can find a smaller range
            // and return the small range found.
            else if (symbol.range.contains(range)) {
                for (const child of symbol.children) {
                    // In Ada a type declaration is considered a class but its scope
                    // is too small so we ignore it to get the super scope.
                    if (child.kind === vscode.SymbolKind.Class) continue;
                    const range = this.getParentSymbolWholeRange(child, label, location);
                    if (range !== null) return range;
                }
                return { name: symbol.name, range: symbol.range };
            } else return null;
        }
        // Else it is a SymbolInformation.
        else if (symbol.name === label && symbol.location.range.contains(range))
            return { name: symbol.name, range: symbol.location.range };
        return null;
    }
}
