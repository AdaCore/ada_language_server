import * as vscode from 'vscode';
import * as fs from 'fs';
import { logger } from './extension';
import {
    EdgeType,
    Hierarchy,
    NodeHierarchy,
    RelationDirection,
    RevealReferencesResponse,
    StringLocation,
    VisualizerSymbol,
} from './visualizerTypes';
import {
    bindNodes,
    createNodeHierarchy,
    getSymbolLocation,
    NodesSingleton,
} from './alsVisualizerUtils';
import { panels } from './alsVisualizer';

/**
 * Base class for all the VisualizerHandler, providing a default implementation of the function.
 *
 * /!\\ Those default implementations should work in most cases, but they are not foolproof.
 * In case of problems, it is possible to overload the behavior for a specific language to get the
 * expected behavior.
 */
export class VisualizerHandler {
    /**
     * Generate an id for a symbol based on its hover information and it hierarchy inside a file.
     *
     * @param nodeLocation - The location of the node in the project.
     * @param label - The label of the node.
     * @returns A position-independent id for the symbol.
     */
    async generateNodeId(nodeLocation: vscode.Location, label: string) {
        const clearId = nodeLocation.uri.fsPath + ':' + label;

        // Hash the file URI and the symbol location to get the ID.
        const hash = await crypto.subtle.digest('SHA-1', new TextEncoder().encode(clearId));
        return Array.from(new Uint8Array(hash))
            .map((byte) => byte.toString(16).padStart(2, '0'))
            .join('');
    }

    /**
     * Check if a given file belongs to the currently opened VS Code workspace.
     *
     * @param uri - The URI of the file tested.
     * @returns True if the file is part of the current VS Code workspace and false otherwise
     * (if the URI points to a file generated at runtime for example).
     */
    isWithinWorkspace(uri: vscode.Uri) {
        return vscode.workspace.getWorkspaceFolder(uri) !== undefined || !fs.existsSync(uri.fsPath);
    }

    /**
     * Get the entire range of a symbol (for a function, its entire body for
     * example).
     *
     * @param symbol - The symbol tree in which to search.
     * @param label - The name of the symbol.
     * @param location - The selection range of the symbol to search.
     * @returns The location of the body of the symbol.
     */
    getParentSymbolWholeRange(
        symbol: vscode.SymbolInformation | vscode.DocumentSymbol,
        label: string,
        location: vscode.Location | vscode.LocationLink,
    ): { name: string; range: vscode.Range } | null {
        const range = 'range' in location ? location.range : location.targetRange;
        // If children the property is set, this is a DocumentSymbol.
        if ('children' in symbol) {
            // If the symbol has the same name and selection range, we directly return it.
            if (symbol.name === label && symbol.selectionRange.isEqual(range))
                return { name: symbol.name, range: symbol.range };
            // Else if the symbol contains the range, we check if we can find a smaller range
            // and return the small range found.
            else if (symbol.range.contains(range)) {
                for (const child of symbol.children) {
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

    /**
     * Get the location (URI and range) of the body of a specific node.
     *
     * @param location - The location node to get the body location from.
     * @param hierarchy - The type of data being visualized.
     * @returns The symbol's body location.
     */
    async getBodyLocation(location: vscode.Location, hierarchy: Hierarchy) {
        if (!fs.existsSync(location.uri.fsPath)) return null;
        if (hierarchy === Hierarchy.TYPE) return [location];

        const implementations = await vscode.commands.executeCommand<
            (vscode.Location | vscode.LocationLink)[]
        >('vscode.executeImplementationProvider', location.uri, location.range.start);

        if (implementations.length > 0) return implementations;
        return null;
    }

    /**
     * Construct a set of nodes from the location of a symbol in the code. Supports multiples
     * different types of hierarchy.
     *
     * @param location - The location of the symbol in the code.
     * @param hierarchy - The type of hierarchy needed.
     * @param languageId - The id of the language the symbol is in.
     * @param direction - The direction of the hierarchy
     * @returns The node linked to the location passed as an argument with
     * possibly children and/or parents added.
     */
    async provideHierarchy(
        location: vscode.Location,
        hierarchy: Hierarchy,
        languageId: string,
        direction: RelationDirection,
    ) {
        // -------------------------------- BEGIN NESTED FUNCTIONS ---------------------------------
        /**
         * Get the additional hierarchy information from a node.
         *
         * @param hierarchyItem - The type of hierarchy needed.
         * @param direction - The direction of the hierarchy
         * @param middleNode - The node from which the hierarchy is executed.
         * @param hierarchy - The type of hierarchy needed.
         */
        async function getHierarchy(
            hierarchyItem: vscode.TypeHierarchyItem | vscode.CallHierarchyItem,
            direction: RelationDirection,
            middleNode: NodeHierarchy,
            hierarchy: Hierarchy,
        ) {
            const commands = [
                ['vscode.provideSupertypes', 'vscode.provideIncomingCalls'],
                ['vscode.provideSubtypes', 'vscode.provideOutgoingCalls'],
            ];
            // Request the hierarchy items and cast them to a unified type of data.
            const items: (vscode.TypeHierarchyItem | vscode.CallHierarchyItem)[] = (
                await vscode.commands.executeCommand<
                    | vscode.TypeHierarchyItem[]
                    | vscode.CallHierarchyIncomingCall[]
                    | vscode.CallHierarchyOutgoingCall[]
                >(commands[direction][hierarchy], hierarchyItem)
            ).map((item) => ('name' in item ? item : 'from' in item ? item.from : item.to));

            for (const item of items) {
                // Get only the useful information from the item.
                const symbol = {
                    name: item.name,
                    location: new vscode.Location(item.uri, item.selectionRange),
                    kind: item.kind,
                } as VisualizerSymbol;

                const newNodeTmp = await createNodeHierarchy(
                    symbol,
                    hierarchy,
                    middleNode.languageId,
                );
                if (!newNodeTmp) continue;

                //Link the two nodes together.
                bindNodes(middleNode, newNodeTmp, direction, EdgeType.REGULAR);
            }

            // Update the parent and children markers of middleNode.
            if (direction === RelationDirection.SUB && middleNode.children.length === 0)
                middleNode.hasChildren = false;
            if (direction === RelationDirection.SUPER && middleNode.parents.length === 0)
                middleNode.hasParent = false;
        }

        // -------------------------------- END NESTED FUNCTIONS --------------------------------

        // Other types of hierarchy will be called from subclasses of this class.
        if (hierarchy !== Hierarchy.CALL && hierarchy !== Hierarchy.TYPE) {
            logger.error(
                'alsVisualizerProvider.ts: provideHierarchy: This hierarchy types is not handled.',
            );
            return;
        }

        const commands = ['vscode.prepareTypeHierarchy', 'vscode.prepareCallHierarchy'];

        const items = await vscode.commands.executeCommand<
            (vscode.CallHierarchyItem | vscode.TypeHierarchyItem)[]
        >(commands[hierarchy], location.uri, location.range.start);

        if (items.length == 0) return;

        // The middle node represents the current main symbol in the graph (the symbol the
        // visualizer was launched on or the symbol for which we are calculating its
        // parent or children).
        let middleNode;

        for (const item of items) {
            const symbol: VisualizerSymbol = {
                name: item.name,
                location: new vscode.Location(item.uri, item.selectionRange),
                kind: item.kind,
            };
            const tmpNode = await createNodeHierarchy(symbol, hierarchy, languageId);
            if (!tmpNode) continue;
            middleNode = NodesSingleton.insertSymbolsMap(tmpNode);
            // Once the process is done, we want the graph to focus on this specific node.
            middleNode.focus = true;

            NodesSingleton.focusedNode = middleNode;

            if (direction === RelationDirection.BOTH || direction === RelationDirection.SUPER) {
                await getHierarchy(item, RelationDirection.SUPER, middleNode, hierarchy);
            }
            if (direction === RelationDirection.BOTH || direction === RelationDirection.SUB) {
                await getHierarchy(item, RelationDirection.SUB, middleNode, hierarchy);
            }

            // The middle node is expanded by default when we are getting its children.
            middleNode.expanded =
                direction === RelationDirection.SUPER ? middleNode.expanded : true;
        }

        // Get all the nodes that do not have parents
        NodesSingleton.findRoots();
        return middleNode;
    }

    /**
     * Search all the references of a specific symbol in the context of another.
     * If no target is provided, the references will all be gathered, ordered
     * regarding the symbol they are located in, and then sent to the client side.
     *
     * @param targetNodeId - The symbol in which to search for references, or null for all.
     * @param referenceNodeId - The references to search.
     * @param bothDirection - True if the edge between the target and reference node goes into the
     * two directions.
     */
    async revealReference(targetNodeId: string, referenceNodeId: string, bothDirection: boolean) {
        const symbolsMap = NodesSingleton.symbolsMap;

        const targetNode = symbolsMap.get(targetNodeId);
        const referenceNode = symbolsMap.get(referenceNodeId);

        if (
            !referenceNode ||
            !fs.existsSync(referenceNode.location.uri.fsPath) ||
            (referenceNode.hierarchy !== Hierarchy.CALL &&
                referenceNode.hierarchy !== Hierarchy.TYPE)
        )
            return;

        // We first gather the names and ranges of all the symbols that contain a reference
        // to the symbol we are interested in.
        const symbolLocations: [vscode.Range, vscode.Uri, string][] = [];
        // Case where the target is known and it is a call-graph.
        if (targetNode && targetNode.hierarchy === Hierarchy.CALL) {
            const locations = await getSymbolLocation(
                targetNode.label,
                targetNode.location,
                targetNode.handler,
                referenceNode.hierarchy,
            );

            if (locations === null) return;

            for (const location of locations) {
                if (location.functionRange === null) return;
                symbolLocations.push([location.functionRange, location.uri, location.name]);
            }
            if (bothDirection) {
                const locations = await getSymbolLocation(
                    referenceNode.label,
                    referenceNode.location,
                    referenceNode.handler,
                    referenceNode.hierarchy,
                );
                // If locations is null don't return just continue to at least
                //display the first direction.
                if (locations !== null) {
                    for (const location of locations) {
                        if (location.functionRange === null) return;
                        symbolLocations.push([location.functionRange, location.uri, location.name]);
                    }
                }
            }
        }
        // Case where the target is unknown and we need all the references.
        else {
            // Query all the incoming symbol in the node.
            const prepare = await vscode.commands.executeCommand<
                (vscode.CallHierarchyItem | vscode.TypeHierarchyItem)[]
            >(
                referenceNode.hierarchy === Hierarchy.CALL
                    ? 'vscode.prepareCallHierarchy'
                    : 'vscode.prepareTypeHierarchy',
                referenceNode.location.uri,
                referenceNode.location.range.start,
            );
            if (prepare.length > 0) {
                const incomings = await vscode.commands.executeCommand<
                    (vscode.CallHierarchyIncomingCall | vscode.TypeHierarchyItem)[]
                >(
                    referenceNode.hierarchy === Hierarchy.CALL
                        ? 'vscode.provideIncomingCalls'
                        : 'vscode.provideSupertypes',
                    prepare[0],
                );
                for (const incoming of incomings) {
                    let incomingItem = null;
                    // Convert the result of the request to a unified result to ease the
                    // rest of the function.
                    if (referenceNode.hierarchy === Hierarchy.CALL) {
                        incomingItem = (incoming as vscode.CallHierarchyIncomingCall).from;
                    } else if (referenceNode.hierarchy === Hierarchy.TYPE) {
                        incomingItem = incoming as vscode.TypeHierarchyItem;
                    }

                    if (!incomingItem) continue;

                    const locations = await getSymbolLocation(
                        incomingItem.name,
                        new vscode.Location(incomingItem.uri, incomingItem.range.start),
                        referenceNode.handler,
                        referenceNode.hierarchy,
                    );
                    if (locations === null) return;
                    for (const location of locations) {
                        if (location.functionRange === null) return;
                        symbolLocations.push([location.functionRange, location.uri, location.name]);
                    }
                }
            }
        }

        const locations = await vscode.commands.executeCommand<vscode.Location[]>(
            'vscode.executeReferenceProvider',
            referenceNode.location.uri,
            referenceNode.location.range.start,
        );
        if (bothDirection && targetNode && targetNode.hierarchy === Hierarchy.CALL) {
            locations.push(
                ...(await vscode.commands.executeCommand<vscode.Location[]>(
                    'vscode.executeReferenceProvider',
                    targetNode.location.uri,
                    targetNode.location.range.start,
                )),
            );
        }
        const stringLocationsMap: Map<string, StringLocation[]> = new Map();
        for (const location of locations) {
            // Don't sort by file name but by the parent function
            const symbolLocation = symbolLocations.find(
                (symbolLocation) =>
                    symbolLocation[0].contains(location.range) &&
                    symbolLocation[1].fsPath === location.uri.fsPath,
            );
            if (symbolLocation) {
                // Check if the key already exist and add it if not.
                if (!stringLocationsMap.has(symbolLocation[2]))
                    stringLocationsMap.set(symbolLocation[2], []);
                // Add the location to the array pointed by the key.
                stringLocationsMap.get(symbolLocation[2])?.push({
                    path: location.uri.fsPath,
                    range_start: location.range.start,
                    range_end: location.range.end,
                    string_location:
                        `Ln ${location.range.start.line}, ` +
                        `Col ${location.range.start.character}`,
                } as StringLocation);
            }
        }

        // Access the right panel by using hierarchy enum values.
        const panel = panels[referenceNode.hierarchy];
        panel?.webview.postMessage({
            command: 'revealResponse',
            locationsKeys: Array.from(stringLocationsMap.keys()),
            locationsValues: Array.from(stringLocationsMap.values()),
        } as RevealReferencesResponse);
    }
}
