/* eslint-disable @typescript-eslint/restrict-template-expressions */
// Needed for importing the script in the HTML snippet.
import * as vscode from 'vscode';
import * as fs from 'fs';
import {
    RelationDirection,
    Message,
    NodeData,
    HierarchyMessage,
    NodeHierarchy,
    Hierarchy,
    NodeEdge as NodeEdgeMessage,
    UpdateMessage,
    DirectedEdge,
    EdgeType,
    IsRenderedMessage,
} from './visualizerTypes';
import { logger } from './extension';
import { convertHierarchyToData, createHandler, NodesSingleton } from './alsVisualizerUtils';

/**
 * Store the webView panel for all the different hierarchies in the order of the Hierarchy Enum.
 */
export const panels: (vscode.WebviewPanel | null)[] = [null, null, null, null];

/**
 * Boolean used to indicate when to stop the process of recursively expanding the graph.
 */
let stopProcess: boolean = false;
/**
 * Boolean used to indicate whether the server is ready to accept a new request or
 * if he is still processing one.
 */
let rejectProcess: boolean = false;
/**
 * Boolean used to indicate to the server when the client has finished rendering the last batch
 * of data and is able to receive more data.
 */
let canSendNextData: boolean = true;

/**
 * Start a progress animation in the status bar indicate to the user that a request is being
 * processed.
 * If cancellable is set to true, it becomes a notification in the bottom right corner with a button
 * allowing the user to stop the task whenever he wants.
 *
 * @param task - The task to launch and wait for while displaying the progress animation. The
 * `token` parameter can be used to check if the user requested cancellation of the task.
 * @param message - The message to display alongside the progress animation.
 * @param cancellable - Indicates if the progress should be cancellable, which will put it in
 * a notification instead of the status bar.
 */
function withVizProgress(
    task: (token?: vscode.CancellationToken) => void | Promise<void>,
    message: string,
    cancellable = false,
) {
    vscode.window.withProgress(
        {
            location: cancellable
                ? vscode.ProgressLocation.Notification
                : vscode.ProgressLocation.Window,
            title: message,
            cancellable: cancellable,
        },
        async (progress, token) => {
            void progress;
            if (rejectProcess) {
                logger.warn('ALS: handleMessage: another command is already running');
                return;
            }
            // If the variable is changed here, whatever happens it will be changed
            // back before the end of the function, even when an exception is thrown.
            rejectProcess = true;
            token.onCancellationRequested(() => {
                rejectProcess = false;
                stopProcess = true;
            });
            try {
                await task(token);
            } catch (error) {
                logger.error(error);
                return;
            } finally {
                rejectProcess = false;
            }
            return new Promise<void>((resolve) => {
                resolve();
            }).finally(() => (rejectProcess = false));
        },
    );
}

/**
 * Gather codes information and aggregate them in a graph to better understand them.
 *
 * @param context - The VS Code context of the extension.
 * @param hierarchy - The type of hierarchy to visualize.
 */
export function startVisualize(context: vscode.ExtensionContext, hierarchy: Hierarchy) {
    const direction =
        hierarchy === Hierarchy.CALL
            ? RelationDirection.SUPER
            : hierarchy === Hierarchy.GPR
              ? RelationDirection.SUB
              : RelationDirection.BOTH;

    const progressLabel = getProgressLabel(hierarchy, true);

    withVizProgress(
        async (token?: vscode.CancellationToken) => {
            if (vscode.window.activeTextEditor) {
                const input = new vscode.Location(
                    vscode.window.activeTextEditor.document.uri,
                    vscode.window.activeTextEditor.selection.active,
                );

                const languageId = vscode.window.activeTextEditor.document.languageId;

                const middleNode = await createHandler(languageId).provideHierarchy(
                    input,
                    hierarchy,
                    languageId,
                    direction,
                    token,
                );

                // Create the webView only if there is something to display and if the
                // user did not cancel the operation.
                if (middleNode && !token?.isCancellationRequested) {
                    setupWebView(context, hierarchy);
                    const panel = panels[hierarchy];
                    // Make sure the webView was created and initialized.
                    if (panel) {
                        // Wait for the webView to notify the end of it's rendering
                        const receive = panel.webview.onDidReceiveMessage((message: Message) => {
                            if (message.command === 'rendered') {
                                // Remove the listener as it will not be used after sending
                                // the initial request.
                                sendMessage(middleNode.id, hierarchy);
                                receive.dispose();
                            }
                        });
                        // Check if the client has already been rendered.
                        panel.webview.postMessage({ command: 'isRendered' } as IsRenderedMessage);
                    }
                }
            }
        },
        progressLabel,
        true,
    );
}

/**
 * Get the label to display in the progress bar depending on the hierarchy type.
 *
 * @param hierarchy - The type of hierarchy to visualize.
 * @param onCreate - Whether the progress is for creation or not.
 * @returns The updated progress label.
 */
function getProgressLabel(hierarchy: Hierarchy, onCreate: boolean = false): string {
    let hierarchyTypeLabel: string;
    const headerLabel: string = onCreate ? 'Creating' : 'Updating';
    switch (hierarchy) {
        case Hierarchy.TYPE:
            hierarchyTypeLabel = 'Type Hierarchy';
            break;
        case Hierarchy.CALL:
            hierarchyTypeLabel = 'Call Hierarchy';
            break;
        case Hierarchy.GPR:
            hierarchyTypeLabel = 'GPR Dependencies';
            break;
        default:
            hierarchyTypeLabel = 'File Dependencies';
            break;
    }
    return `${headerLabel} ${hierarchyTypeLabel} Graph`;
}

/**
 * Handle the message received from client side and dispatch it to the right function
 * depending on its command.
 *
 * @param message - The message received from the server.
 */
function handleMessage(message: Message) {
    switch (message.command) {
        // Add new nodes to the graph or fold/unfold.
        case 'requestHierarchy': {
            void requestHierarchy(message);
            break;
        }
        // Reveal the position of the definition or implementation of a specific node.
        case 'revealNode': {
            const node = NodesSingleton.symbolsMap.get(message.nodeId);
            if (node === undefined) return;
            void revealSymbol(node.location, node.hierarchy, message.gotoImplementation);
            break;
        }
        // Gather all references of a symbol in the context of another symbol.
        // This will find all the references of the referenceNodeId in the body of the targetNodeId.
        case 'revealReferences': {
            const node = NodesSingleton.symbolsMap.get(message.referenceNodeId);
            if (node)
                void node.handler.revealReference(
                    message.targetNodeId,
                    message.referenceNodeId,
                    message.bothDirection,
                );
            break;
        }
        // Reconstruct a location and reveal the symbol under it in the code.
        case 'revealLocation': {
            const location = new vscode.Location(
                vscode.Uri.file(message.path),
                new vscode.Range(message.range_start, message.range_end),
            );
            void revealSymbol(location, Hierarchy.CALL, false);
            break;
        }
        // Delete a set of nodes and their descendants if recursive is true.
        case 'deleteNodes': {
            deleteNodes(message.nodesId, message.recursive);
            break;
        }
        // Refresh the information of an already existing node by fetching it again from the
        // code base.
        case 'refreshNodes': {
            withVizProgress(() => {
                const data = message;
                void refreshNodes(data.nodesId);
            }, 'Refreshing Graph');
            break;
        }
        // Stop the current loop of process (mostly for the recursive hierarchy).
        case 'stopProcess': {
            stopProcess = true;
            break;
        }
        // This message is not handled here, but it is caught here to avoid falling into
        // the default case.
        case 'rendered':
            break;
        // The client finished rendering the last batch of data and is ready to receive more.
        case 'canSendNextData':
            canSendNextData = true;
            break;
        default:
            logger.warn('ALS: handleMessage: Command not found or empty request');
            break;
    }
}

/**
 * Expand the graph by adding parents or children to the targeted node.
 * If recursive is set to true, the algorithm will continue to expand until finding symbols
 * that are not in the project.
 *
 * @param data - The data necessary to expand a node.
 */
function requestHierarchy(data: HierarchyMessage) {
    const progressLabel = getProgressLabel(data.hierarchy);

    withVizProgress(
        async () => {
            stopProcess = false;
            const node = NodesSingleton.symbolsMap.get(data.id);
            // Check that the symbol is not a runtime-generated one.
            if (node === undefined) return;
            const oldExpand = node.expanded;
            node.expanded = data.expand;
            const queue: NodeHierarchy[] = [node];

            /**
             * Subfunction that performs one iteration of the graph expansion
             * (adds only direct children or parents of currNode).
             */
            async function subRequestHierarchy() {
                // We do an iteration only if we are not in recursive mode (only one iteration)
                // or if we expect multiple iterations and the client is ready to receive the next
                // data.
                if (!data.recursive || (data.recursive && canSendNextData)) {
                    const currNode = queue.pop();
                    if (currNode) {
                        // We want to expand the graph only if the node is currently expanded,
                        // or we want to get the parents of the node and that the path contained
                        // in the node exists on the machine.
                        if (
                            ((data.expand === oldExpand && data.expand) ||
                                data.direction === RelationDirection.SUPER) &&
                            fs.existsSync(currNode.location.uri.fsPath)
                        ) {
                            await currNode.handler.provideHierarchy(
                                currNode.location,
                                currNode.hierarchy,
                                currNode.languageId,
                                data.direction,
                            );

                            // Add the children or parents to the queue only if we are
                            // in recursive mode.
                            if (data.recursive) {
                                if (data.direction === RelationDirection.SUB)
                                    queue.push(
                                        ...currNode.children
                                            .filter(
                                                (child) =>
                                                    child.target.inProject &&
                                                    child.target.id !== currNode.id,
                                            )
                                            .map((child) => child.target),
                                    );
                                else
                                    queue.push(
                                        ...currNode.parents
                                            .filter((parent) => parent.target.inProject)
                                            .map((child) => child.target),
                                    );
                            }
                        } else if (data.expand && node) {
                            node.focus = true;
                            NodesSingleton.focusedNode = node;
                        }
                        //Set the marker for the next data to false and wait for the client to
                        // send a message to update it.
                        canSendNextData = false;
                        // Update the graph in real time so the user can see it grow.
                        sendMessage(currNode.id, data.hierarchy, !data.recursive || stopProcess);
                    }
                }
            }
            // Call the first iteration of the process.
            await subRequestHierarchy();

            let finalStop = false;
            // Wait for a response from the client to call the following iterations.
            const receiveMessageEvent = panels[data.hierarchy]?.webview.onDidReceiveMessage(
                async (message: Message) => {
                    if (message.command === 'canSendNextData') {
                        if (queue.length !== 0 && !stopProcess) await subRequestHierarchy();
                        // When the queue is empty or the user asked to stop the process, send the
                        // final request to update the final state of the graph and stop the
                        // waiting bar of the client.
                        else {
                            if (data.recursive) sendMessage(node.id, data.hierarchy);
                            receiveMessageEvent?.dispose();

                            // The reason there is two stop variable is that :
                            // - if the user stop the process himself, the interval below will
                            // trigger first and the this event listener will be call.
                            // - if the process ended naturally this event listener will be called
                            // and trigger the interval
                            if (stopProcess) stopProcess = false;
                            else finalStop = true;
                        }
                    }
                },
            );
            // This promise makes sur the task exit until the user requests it or the queue
            // became empty.
            await new Promise<void>((resolve) => {
                const intervalId = setInterval(() => {
                    if (stopProcess || finalStop) {
                        clearInterval(intervalId);
                        resolve();
                    }
                }, 100);
            });
        },
        progressLabel,
        data.recursive,
    );
}

/**
 * Update the location of the nodes in case their position in the file changed.
 * /!\\ If the function changed too much (for example if the signature changed) it will be
 * impossible to find it again and the user will need to remove the node to regenerate it manually.
 *
 * @param nodesId - The nodes to refresh.
 */
async function refreshNodes(nodesId: string[]) {
    const toUpdate: NodeHierarchy[] = [];
    for (const nodeId of nodesId) {
        const node = NodesSingleton.symbolsMap.get(nodeId);
        if (!node || !fs.existsSync(node.location.uri.fsPath)) continue;
        const symbols = await vscode.commands.executeCommand<
            vscode.SymbolInformation[] | vscode.DocumentSymbol[]
        >('vscode.executeDocumentSymbolProvider', node.location.uri);

        // Breadth First Search to locate the symbol with an ID that match the node we
        // want to refresh.
        const queue = [...symbols];
        while (queue.length !== 0) {
            const symbol = queue.pop();
            if (!symbol) continue;
            const loc: vscode.Location =
                'selectionRange' in symbol
                    ? new vscode.Location(node.location.uri, symbol.selectionRange)
                    : symbol.location;

            // If the generated ID is the same then it is the same symbol.
            if (node.id === (await node.handler.generateNodeId(loc, node.label))) {
                node.location = loc;
                toUpdate.push(node);
                break;
            } else {
                queue.push(...('children' in symbol ? symbol.children : []));
            }
        }
    }
    updateNodes(toUpdate, []);
}

/**
 * Recursively traverse the graph from a specific node to find if there is a cycle.
 *
 * @param initialNode - The node from which the search began.
 * @param currNode - The node being currently tested, should be a child of the initialNode
 * when first calling the function.
 * @param visited - A set containing the id of all the already visited nodes.
 * @returns True if a cycle was found, false otherwise.
 */
function hasCycle(
    initialNode: NodeHierarchy,
    currNode: NodeHierarchy,
    visited: Set<string> = new Set(),
) {
    if (visited.has(currNode.id)) return false;
    visited.add(currNode.id);
    for (const child of currNode.children) {
        if (child.target.id === initialNode.id) return true;
        if (hasCycle(initialNode, child.target, visited)) return true;
    }
    return false;
}

/**
 * Remove nodes from the symbolMap and the nodeHierarchy.
 *
 * If recursive is set to true, all the descendants of the nodes will also
 * be deleted if they are not linked to any other node not staged for
 * deletion and if they are not linked to a parent of the original node.
 *
 * Update the remaining node to change their parent and child list.
 *
 * @param nodeIds - The ids of the nodes to remove.
 */
function deleteNodes(nodeIds: string[], recursive: boolean) {
    let toUpdate: NodeHierarchy[] = [];
    let toDelete: NodeHierarchy[] = [];

    for (const nodeId of nodeIds) {
        const node = NodesSingleton.symbolsMap.get(nodeId);
        if (!node) continue;

        toDelete.push(node);
        for (const child of node.children) {
            // Only delete the children who are not part of a cycle with their parents.
            if (recursive && !hasCycle(node, child.target)) {
                const queue: NodeHierarchy[] = [child.target];
                while (queue.length !== 0) {
                    const del = queue.pop();
                    if (!del) continue;
                    toDelete.push(del);
                    queue.push(...del.children.map((child) => child.target));
                }
            }
        }
    }

    // We only delete the nodes whose parents are all in the toDelete array.
    // As we can't know the order of the toDeleteArray we continue to check for nodes
    // to exclude while there were changes in the previous iteration.
    let deleteLen;
    let newDeleteLen;
    do {
        deleteLen = toDelete.length;
        toDelete = toDelete.filter(
            (node) =>
                nodeIds.find((id) => node.id === id) ||
                node.parents.every(
                    (parent) => toDelete.find((del) => del.id === parent.target.id) !== undefined,
                ),
        );
        newDeleteLen = toDelete.length;
    } while (deleteLen !== newDeleteLen);

    for (const node of toDelete) {
        // Remove the reference to the node from its parent's children and
        // from its children's parent
        for (const child of node.children) {
            child.target.parents = child.target.parents.filter(
                (parent) => parent.target.id !== node.id,
            );
            if (child.target.parents.length === 0) {
                child.target.hasParent = null;
                toUpdate.push(child.target);
            }
        }
        for (const parent of node.parents) {
            parent.target.children = parent.target.children.filter(
                (child) => child.target.id !== node.id,
            );
            if (parent.target.children.length === 0) {
                parent.target.hasChildren = null;
                toUpdate.push(parent.target);
            }
        }
        NodesSingleton.symbolsMap.delete(node.id);
    }

    // Remove nodes that were deleted from the array of nodes to update.
    toUpdate = toUpdate.filter((node) => !toDelete.some((del) => del.id === node.id));
    NodesSingleton.findRoots();
    updateNodes(toUpdate, toDelete);
}

/**
 * Send a message to the client side to update the content of certain nodes.
 *
 * @param toUpdate - The nodes to updates.
 * @param toDelete - The nodes to delete.
 */
function updateNodes(toUpdate: NodeHierarchy[], toDelete: NodeHierarchy[]) {
    const sendUpdate: NodeData[] = toUpdate.map((node) => convertHierarchyToData(node));
    const sendDelete: NodeData[] = toDelete.map((node) => convertHierarchyToData(node));
    if (sendUpdate.length !== 0 || sendDelete.length !== 0) {
        const panel = panels[(sendUpdate.length !== 0 ? sendUpdate[0] : sendDelete[0]).hierarchy];
        panel?.webview.postMessage({
            command: 'updateNodes',
            toUpdate: sendUpdate,
            toDelete: sendDelete,
        } as UpdateMessage);
    }
}

/**
 * Get the document linked to the node's symbol and focus the user on it.
 *
 * @param location - The location to reveal in the code base.
 * @param hierarchy -  The type of graph being made.
 * @param gotoImplementation - True to go to the implementation of a symbol, False to go to the
 * definition.
 */
async function revealSymbol(
    location: vscode.Location,
    hierarchy: Hierarchy,
    gotoImplementation: boolean,
) {
    // If the path does not exist, immediately exit.
    if (!fs.existsSync(location.uri.fsPath)) return;

    // If gotoImplementation, replace current location by the location of the implementation.
    if (gotoImplementation) {
        const implementationLocation = await vscode.commands.executeCommand<
            (vscode.Location | vscode.LocationLink)[]
        >('vscode.executeImplementationProvider', location.uri, location.range.start);
        if (implementationLocation.length > 0) {
            if ('uri' in implementationLocation[0]) {
                location = implementationLocation[0];
            } else {
                location = new vscode.Location(
                    implementationLocation[0].targetUri,
                    implementationLocation[0].targetRange,
                );
            }
        }
    }
    const tabsGroup = vscode.window.tabGroups.all;
    let viewColumn: vscode.ViewColumn | undefined;
    // Find the tab that contains the same URI as the node and return its viewColumn.
    for (const tabGroup of tabsGroup) {
        for (const tab of tabGroup.tabs) {
            if (tab.input instanceof vscode.TabInputText) {
                if (tab.input.uri.fsPath === location.uri.fsPath) {
                    viewColumn = tabGroup.viewColumn;
                    break;
                }
            }
        }
    }
    // If the tab does not exist in a column yet, place it on the left of the webView column
    // (or the right if it is the first one).
    if (viewColumn === undefined) {
        const panel = panels[hierarchy];
        if (panel && panel.viewColumn) {
            if (panel.viewColumn !== vscode.ViewColumn.One) viewColumn = panel.viewColumn - 1;
            else viewColumn = panel.viewColumn + 1;
        }
    }
    const document = await vscode.workspace.openTextDocument(location.uri);
    // Show the text document on either its original column or in the current if the document
    // wasn't opened
    const editor = await vscode.window.showTextDocument(document, {
        viewColumn: viewColumn !== undefined ? viewColumn : vscode.ViewColumn.Beside,
        preserveFocus: false,
    });
    editor.selection = new vscode.Selection(location.range.start, location.range.start);
    editor.revealRange(location.range, vscode.TextEditorRevealType.InCenterIfOutsideViewport);
}

/**
 * Convert a NodeHierarchy object into a set of nodes and edges.
 *
 * @param nodes - The set of nodes that will contain the converted nodes.
 * @param edges - The set of edges that will contain the converted edges.
 * @param root - The NodeHierarchy object that will be converted.
 * @param alreadyAdded - A set that keeps track of all the already converted symbols into nodes.
 */
function convertToMessage(
    nodes: NodeData[],
    edges: DirectedEdge[],
    root: NodeHierarchy,
    alreadyAdded: Set<string>,
) {
    nodes.push(convertHierarchyToData(root));
    const queue: NodeHierarchy[] = [];

    // If the node is expanded we also want to convert its children.
    if (root.expanded) queue.push(...root.children.map((child) => child.target));

    while (queue.length !== 0) {
        const node: NodeHierarchy = queue.splice(0, 1)[0];
        if (alreadyAdded.has(node.id)) continue;
        alreadyAdded.add(node.id);
        nodes.push(convertHierarchyToData(node));

        for (const parent of node.parents) {
            if (!parent.target.expanded) continue;
            // Check if the edge between the current node and its parents already exists.
            const edge = Array.from(edges).find(
                (edge) =>
                    (edge.src === parent.target.id && edge.dst === node.id) ||
                    (edge.src === node.id && edge.dst === parent.target.id),
            );
            if (!edge)
                // If the edge does not exist, create it.
                edges.push({
                    src: parent.target.id,
                    dst: node.id,
                    edgeDirection: RelationDirection.SUB,
                    edgeType:
                        parent.target.id === node.id ? EdgeType.SELF_CONNECTED : parent.edgeType,
                });
            else if (edge.src === node.id) edge.edgeDirection = RelationDirection.BOTH;
        }
        if (node.expanded) queue.push(...node.children.map((child) => child.target));
    }
}

/**
 * Convert all the root nodes and add them to a set of nodes and edges before
 * sending them to the client side.
 *
 * @param nodeId - The id of the node that was added/modified
 * @param hierarchy - The type of hierarchy needed.
 */
function sendMessage(nodeId: string, hierarchy: Hierarchy, focus = true) {
    const nodes: NodeData[] = [];
    const edges: DirectedEdge[] = [];
    const alreadyAdded: Set<string> = new Set();

    // Convert all the graph starting from the root nodes to make sur every node is reached.
    for (const root of NodesSingleton.rootNodes) {
        if (root.hierarchy === hierarchy) convertToMessage(nodes, edges, root, alreadyAdded);
    }

    if (nodes.length !== 0) {
        const panel = panels[hierarchy];
        panel?.webview.postMessage({
            command: 'hierarchyResponse',
            nodesData: nodes,
            edges: edges,
            mainNodeId: nodeId,
            focus: focus,
        } as NodeEdgeMessage);
        panel?.reveal();
    }
    if (NodesSingleton.focusedNode) NodesSingleton.focusedNode.focus = false;
}

/**
 * Create a new webView panel if one is not already active.
 *
 * @param context - The vscode context of the extension.
 */
function setupWebView(context: vscode.ExtensionContext, hierarchy: Hierarchy) {
    // Array to quickly access the right string from hierarchy enum position.
    const hierarchyTypeArr = ['Type', 'Call', 'File', 'GPR'];
    // The values are duplicated here to facilitate access to array by using Hierarchy
    // enum value.
    const hierarchyTitleArr = ['Hierarchy', 'Hierarchy', 'Dependencies', 'Dependencies'];

    const id = 'alsVisualizer' + hierarchyTypeArr[hierarchy];
    const title = hierarchyTypeArr[hierarchy] + ' ' + hierarchyTitleArr[hierarchy] + ' Graph';

    let panel = panels[hierarchy];
    if (panel != undefined && panel != null) return;
    panel = vscode.window.createWebviewPanel(id, title, vscode.ViewColumn.Beside, {
        enableScripts: true,
        retainContextWhenHidden: true,
    });

    // Setup the HTML code for the webView.
    panel.webview.html = getWebviewContent(panel.webview, context.extensionUri);

    // Setup the event listener to get messages and call the right functions.
    panel.webview.onDidReceiveMessage((message: Message) => {
        void handleMessage(message);
    });

    // On panel suppression, clear the symbol map of all the related symbols.
    panel.onDidDispose(() => {
        const symbolsMap = NodesSingleton.symbolsMap;

        panels[hierarchy] = null;
        for (const key of symbolsMap.keys()) {
            if (symbolsMap.get(key)?.hierarchy === hierarchy) symbolsMap.delete(key);
        }
        stopProcess = true;
        // Update the root node array to filter out the removed nodes.
        NodesSingleton.findRoots();
    });
    panels[hierarchy] = panel;
}

/**
 * Create the HTML part of the webview.
 *
 * @param webview - The webView panel.
 * @param extensionUri - The URI of the extensions.
 * @returns An HTML string that represents the main body of the webView.
 */
function getWebviewContent(webview: vscode.Webview, extensionUri: vscode.Uri) {
    const scriptUri = webview.asWebviewUri(
        vscode.Uri.joinPath(extensionUri, 'out', 'src', 'visualizing', 'AppMain.js'),
    );
    const codiconsUri = webview.asWebviewUri(
        vscode.Uri.joinPath(
            extensionUri,
            'node_modules',
            '@vscode/codicons',
            'dist',
            'codicon.css',
        ),
    );
    return `
        <!DOCTYPE html>
        <html lang="en">
        <head>
            <meta charset="UTF-8">
            <meta name="viewport" content="width=device-width,initial-scale=1,shrink-to-fit=no">
            <title>Visualizer</title>
            <link href="${codiconsUri}" rel="stylesheet" />
        </head>
        <body>
            <div id="root"></div>
            <script type="module" src="${scriptUri}"></script>
        </body>
        </html>
    `;
}
