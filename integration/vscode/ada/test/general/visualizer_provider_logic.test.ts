import * as assert from 'assert';
import 'mocha';
import * as vscode from 'vscode';
import {
    buildRevealReferencesResponse,
    VisualizerHandler,
    VisualizerProviderDependencies,
} from '../../src/alsVisualizerProvider';
import { NodesSingleton } from '../../src/alsVisualizerUtils';
import {
    Hierarchy,
    NodeHierarchy,
    RelationDirection,
    RevealReferencesResponse,
} from '../../src/visualizerTypes';
import { getSymbolLocation } from '../../src/alsVisualizerUtils';
import { activate, getWsUri } from '../utils';

type SymbolLocationResult = Awaited<ReturnType<typeof getSymbolLocation>>;

teardown(() => {
    // Reset singleton state to keep tests isolated.
    NodesSingleton.symbolsMap.clear();
    NodesSingleton.focusedNode = null;
});

/**
 * Helper function to find a symbol by name in a document symbol tree.
 */
function findSymbolByName(symbols: vscode.DocumentSymbol[], symbolName: string) {
    const queue = [...symbols];
    while (queue.length > 0) {
        const symbol = queue.shift();
        if (!symbol) continue;
        if (symbol.name === symbolName) return symbol;
        queue.push(...symbol.children);
    }
    return undefined;
}

/**
 * Get a node hierarchy for the call-hierarchy LSP request on
 * a symbol identified by its name in a given file.
 */
async function getCallHierarchyNode(fileName: string, symbolName: string): Promise<NodeHierarchy> {
    // Open an Ada source file from the test workspace.
    const uri = getWsUri('src', fileName);
    const document = await vscode.workspace.openTextDocument(uri);
    await vscode.window.showTextDocument(document);

    // Get all the symbols of the file
    const symbols = await vscode.commands.executeCommand<vscode.DocumentSymbol[]>(
        'vscode.executeDocumentSymbolProvider',
        uri,
    );

    // Find the symbol with the given name
    const symbol = findSymbolByName(symbols, symbolName);
    assert.ok(symbol, `Could not find symbol '${symbolName}' in ${fileName}`);

    // Run the actual call-hierarchy request against the language server.
    const handler = new VisualizerHandler();
    const location = new vscode.Location(uri, symbol.selectionRange);
    const node = await handler.provideHierarchy(
        location,
        Hierarchy.CALL,
        document.languageId,
        RelationDirection.BOTH,
    );

    assert.ok(node, `Could not build call hierarchy from symbol '${symbolName}' in ${fileName}`);
    return node;
}

suite('Visualizer Provider Logic', () => {
    suiteSetup(async () => {
        await activate();
    });

    test('References Picker: grouped entries', () => {
        // Step 1 (User context): user opens the references picker from the graph.
        const uri = vscode.Uri.file('/tmp/example.adb');
        const symbolRange = new vscode.Range(new vscode.Position(0, 0), new vscode.Position(20, 0));

        // Step 2 (Backend action): build payload from raw references.
        const response = buildRevealReferencesResponse(
            [
                {
                    range: symbolRange,
                    uri: uri,
                    name: 'Proc_A',
                },
            ],
            [
                new vscode.Location(uri, new vscode.Range(1, 2, 1, 5)),
                new vscode.Location(uri, new vscode.Range(3, 4, 3, 7)),
            ],
        );

        // Step 3 (User-visible result): one section named Proc_A is shown.
        assert.deepStrictEqual(response.locationsKeys, ['Proc_A']);
        assert.strictEqual(response.locationsValues.length, 1);
        assert.strictEqual(response.locationsValues[0].length, 2);

        // Step 4 (User-visible result): displayed positions are 1-based.
        assert.strictEqual(response.locationsValues[0][0].string_location, 'Ln 2, Col 3');
        assert.strictEqual(response.locationsValues[0][1].string_location, 'Ln 4, Col 5');
    });

    test('References Picker: one refresh payload', async () => {
        // Step 1 (User context): user triggers "Reveal References" on an edge.
        const realNode = await getCallHierarchyNode('bar.ads', 'A_Procedure');
        const uri = realNode.location.uri;
        const collectedResponses: RevealReferencesResponse[] = [];
        const expectedLocationLabel =
            `Ln ${realNode.location.range.start.line + 1}, ` +
            `Col ${realNode.location.range.start.character + 1}`;

        let referenceProviderCalls = 0;

        // Step 2 (Backend setup): stub collaborators used by the interaction.
        const deps: VisualizerProviderDependencies = {
            executeCommand: <T>(command: string) => {
                if (command === 'vscode.executeReferenceProvider') {
                    referenceProviderCalls += 1;
                    return Promise.resolve([
                        new vscode.Location(uri, realNode.location.range),
                    ] as T);
                }
                return Promise.resolve([] as T);
            },
            fileExists: () => true,
            getSymbolLocation: (
                label,
                location,
                handler,
                hierarchy,
            ): Promise<SymbolLocationResult> => {
                void label;
                void location;
                void handler;
                void hierarchy;
                return Promise.resolve([
                    {
                        uri,
                        functionRange: realNode.location.range,
                        name: 'Proc_Target',
                    },
                ]);
            },
            postRevealResponse: (hierarchy, response) => {
                void hierarchy;
                collectedResponses.push(response);
            },
        };

        const handler = new VisualizerHandler(deps);
        // Reuse real node data, but route behavior through the test handler.
        realNode.handler = handler;
        NodesSingleton.symbolsMap.set(realNode.id, realNode);

        // Step 3 (Backend action): run the same method called by the UI flow.
        await handler.revealReference(realNode.id, realNode.id, false);

        // Step 4 (User-visible result): one picker refresh payload is emitted.
        assert.strictEqual(referenceProviderCalls, 1);
        assert.strictEqual(collectedResponses.length, 1);
        assert.deepStrictEqual(collectedResponses[0].locationsKeys, ['Proc_Target']);
        assert.strictEqual(
            collectedResponses[0].locationsValues[0][0].string_location,
            expectedLocationLabel,
        );
    });

    test('References Picker: bidirectional merge', async () => {
        // Step 1 (User context): user requests references on a bidirectional edge.
        const targetNode = await getCallHierarchyNode('bar.ads', 'A_Procedure');
        const referenceNode = await getCallHierarchyNode('test.adb', 'P1');
        const targetLabel = targetNode.label;
        const targetUri = targetNode.location.uri;
        const referenceUri = referenceNode.location.uri;
        const collectedResponses: RevealReferencesResponse[] = [];

        let referenceProviderCalls = 0;

        const deps: VisualizerProviderDependencies = {
            executeCommand: <T>(command: string, ...rest: unknown[]) => {
                const uriArg = rest[0] as vscode.Uri | undefined;
                if (command === 'vscode.executeReferenceProvider') {
                    referenceProviderCalls += 1;
                    if (uriArg?.fsPath === targetUri.fsPath) {
                        return Promise.resolve([
                            new vscode.Location(targetUri, targetNode.location.range),
                        ] as T);
                    }
                    if (uriArg?.fsPath === referenceUri.fsPath) {
                        return Promise.resolve([
                            new vscode.Location(referenceUri, referenceNode.location.range),
                        ] as T);
                    }
                }
                return Promise.resolve([] as T);
            },
            fileExists: () => true,
            getSymbolLocation: (label): Promise<SymbolLocationResult> => {
                if (label === targetLabel) {
                    return Promise.resolve([
                        {
                            uri: targetUri,
                            functionRange: targetNode.location.range,
                            name: 'Target_Proc',
                        },
                    ]);
                }
                return Promise.resolve([
                    {
                        uri: referenceUri,
                        functionRange: referenceNode.location.range,
                        name: 'Reference_Proc',
                    },
                ]);
            },
            postRevealResponse: (hierarchy, response) => {
                void hierarchy;
                collectedResponses.push(response);
            },
        };

        const handler = new VisualizerHandler(deps);
        targetNode.handler = handler;
        referenceNode.handler = handler;

        NodesSingleton.symbolsMap.set(targetNode.id, targetNode);
        NodesSingleton.symbolsMap.set(referenceNode.id, referenceNode);

        // Step 2 (Backend action): reveal references with bothDirection=true.
        await handler.revealReference(targetNode.id, referenceNode.id, true);

        // Step 3 (User-visible result): one refresh contains both groups.
        assert.strictEqual(referenceProviderCalls, 2);
        assert.strictEqual(collectedResponses.length, 1);
        assert.deepStrictEqual(collectedResponses[0].locationsKeys.sort(), [
            'Reference_Proc',
            'Target_Proc',
        ]);
    });

    test('References Picker: unknown target uses incoming symbols', async () => {
        // Step 1 (User context): user asks for references while no explicit target node is known.
        const referenceNode = await getCallHierarchyNode('bar.ads', 'A_Procedure');
        const uri = referenceNode.location.uri;
        const collectedResponses: RevealReferencesResponse[] = [];

        let prepareCalls = 0;
        let incomingCalls = 0;
        let referenceProviderCalls = 0;

        const incomingItemA: vscode.CallHierarchyItem = {
            name: 'Incoming_A',
            kind: vscode.SymbolKind.Function,
            tags: [],
            detail: 'Incoming_A',
            uri,
            range: new vscode.Range(0, 0, 5, 0),
            selectionRange: new vscode.Range(0, 0, 0, 8),
        };

        const incomingItemB: vscode.CallHierarchyItem = {
            name: 'Incoming_B',
            kind: vscode.SymbolKind.Function,
            tags: [],
            detail: 'Incoming_B',
            uri,
            range: new vscode.Range(6, 0, 12, 0),
            selectionRange: new vscode.Range(6, 0, 6, 8),
        };

        const deps: VisualizerProviderDependencies = {
            executeCommand: <T>(command: string) => {
                if (command === 'vscode.prepareCallHierarchy') {
                    prepareCalls += 1;
                    return Promise.resolve([incomingItemA] as T);
                }
                if (command === 'vscode.provideIncomingCalls') {
                    incomingCalls += 1;
                    return Promise.resolve([
                        {
                            from: incomingItemA,
                            fromRanges: [new vscode.Range(1, 0, 1, 1)],
                        } as vscode.CallHierarchyIncomingCall,
                        {
                            from: incomingItemB,
                            fromRanges: [new vscode.Range(7, 0, 7, 1)],
                        } as vscode.CallHierarchyIncomingCall,
                    ] as T);
                }
                if (command === 'vscode.executeReferenceProvider') {
                    referenceProviderCalls += 1;
                    return Promise.resolve([
                        new vscode.Location(uri, new vscode.Range(2, 0, 2, 1)),
                        new vscode.Location(uri, new vscode.Range(8, 0, 8, 1)),
                    ] as T);
                }
                return Promise.resolve([] as T);
            },
            fileExists: () => true,
            getSymbolLocation: (label): Promise<SymbolLocationResult> => {
                if (label === 'Incoming_A') {
                    return Promise.resolve([
                        {
                            uri,
                            functionRange: new vscode.Range(0, 0, 5, 0),
                            name: 'Incoming_A_Proc',
                        },
                    ]);
                }
                return Promise.resolve([
                    {
                        uri,
                        functionRange: new vscode.Range(6, 0, 12, 0),
                        name: 'Incoming_B_Proc',
                    },
                ]);
            },
            postRevealResponse: (hierarchy, response) => {
                void hierarchy;
                collectedResponses.push(response);
            },
        };

        const handler = new VisualizerHandler(deps);
        referenceNode.handler = handler;
        NodesSingleton.symbolsMap.set(referenceNode.id, referenceNode);

        // Step 2 (Backend action): reveal references with an unknown target node id.
        await handler.revealReference('unknown-target-id', referenceNode.id, false);

        // Step 3 (User-visible result): one refresh contains groups from incoming symbols.
        assert.strictEqual(prepareCalls, 1);
        assert.strictEqual(incomingCalls, 1);
        assert.strictEqual(referenceProviderCalls, 1);
        assert.strictEqual(collectedResponses.length, 1);
        assert.deepStrictEqual(collectedResponses[0].locationsKeys.sort(), [
            'Incoming_A_Proc',
            'Incoming_B_Proc',
        ]);
        assert.strictEqual(
            collectedResponses[0].locationsValues[0][0].string_location,
            'Ln 3, Col 1',
        );
        assert.strictEqual(
            collectedResponses[0].locationsValues[1][0].string_location,
            'Ln 9, Col 1',
        );
    });

    test('Hierarchy Expansion: cancel early', async () => {
        // Step 1 (User context): user starts expansion then cancels immediately.
        const uri = vscode.Uri.file('/tmp/example.adb');
        let commandCalls = 0;

        const deps: VisualizerProviderDependencies = {
            executeCommand: <T>(command: string) => {
                void command;
                commandCalls += 1;
                return Promise.resolve([] as T);
            },
            fileExists: () => true,
            getSymbolLocation: getSymbolLocation,
            postRevealResponse: (hierarchy, response) => {
                void hierarchy;
                void response;
                return;
            },
        };

        const handler = new VisualizerHandler(deps);
        const cancelledToken = { isCancellationRequested: true } as vscode.CancellationToken;

        // Step 2 (Backend action): expansion request receives cancelled token.
        const result = await handler.provideHierarchy(
            new vscode.Location(uri, new vscode.Range(0, 0, 0, 0)),
            Hierarchy.CALL,
            'ada',
            0,
            cancelledToken,
        );

        // Step 3 (User-visible result): no expansion side effects happen.
        assert.strictEqual(result, undefined);
        assert.strictEqual(commandCalls, 0);
    });
});
