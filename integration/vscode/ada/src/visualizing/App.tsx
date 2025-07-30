import * as React from 'react';
import ReactDOM from 'react-dom/client';
import {
    Direction,
    Message,
    NodeEdge,
    UpdateMessage,
    RelationDirection,
    RevealReferencesMessage,
    RevealReferencesResponse,
    StringLocation,
    NodeData,
    Hierarchy,
    RevealMessage,
    NodeType,
    EdgeType,
    RenderedMessage,
    SendNextDataMessage,
    DeleteMessage,
} from '../visualizerTypes';

import {
    Node,
    Edge,
    useNodesState,
    useEdgesState,
    ReactFlow,
    ReactFlowProvider,
    addEdge,
    Controls,
    ControlButton,
    SelectionMode,
    useReactFlow,
    Background,
    Panel,
    useOnSelectionChange,
    useStore,
} from '@xyflow/react';

import '@xyflow/react/dist/style.css';
import './visualizerStyleSheet.css';
import { edgeFactory, edgeTypes, floatingConnectionLine } from './customEdges';
import { moveNodes, nodeFactory, nodeTypes } from './customNodes';
import { elkOptions, layoutSubFlow, layoutSubgraph, layoutSubgraphs } from './layouting';
import { changeEdge, focusNode, setIntervalCapped, waitingBar } from './utils';
import { NodeContextMenu, NodeContextMenuProps } from './nodeContextMenu';
import { closeSearchBar as onSearchBarClose, SearchBar } from './searchBar';
import { ReferencesPickerMenu, ReferencesPickerMenuProps } from './referencesPickerMenu';

/**
 * Current direction of the graph layout.
 */
export let currentDirection = Direction.RIGHT;

/**
 * The VS Code object used to send message from the webView
 */
export const vscode = acquireVsCodeApi();

/**
 * Variables used to store the node state and access it in the rest of the program.
 */
let onNodesChange;
let onEdgesChange;
let nodes: Node<NodeData>[] = [];
let edges: Edge[] = [];
let setNodes: React.Dispatch<React.SetStateAction<Node<NodeData>[]>>;
let setEdges: React.Dispatch<React.SetStateAction<Edge[]>>;
let getNodes: () => Node[];
const nodeWidth = 250;
const nodeHeight = 300;
let referencesPickerMenu: ReferencesPickerMenuProps | null = null;
let setReferencesPickerMenu: React.Dispatch<React.SetStateAction<ReferencesPickerMenuProps | null>>;
let nodeContextMenu: NodeContextMenuProps | null = null;
let setNodeContextMenu: React.Dispatch<React.SetStateAction<NodeContextMenuProps | null>>;

// Store the id of a setTimeout to ensure the uniqueness of the timeout.
let timeoutId: NodeJS.Timeout | null = null;

type Graph = {
    nodes: Node[];
    edges: Edge[];
};

/**
 * Dispatch the message received according to the command passed as a field to the message.
 *
 * @param text - A message received by the client
 */
const handleMessage = (text: MessageEvent<Message>) => {
    switch (text.data.command) {
        case 'hierarchyResponse': {
            void handleHierarchy(text.data);
            break;
        }
        case 'updateNodes': {
            void handleUpdate(text.data);
            break;
        }
        case 'isRendered': {
            vscode.postMessage({ command: 'rendered' } as RenderedMessage);
            break;
        }
        case 'revealResponse': {
            void handleReveal(text.data);
            break;
        }
        default:
            console.log('Command not found or empty');
            break;
    }
};

/**
 * Handle the 'hierarchyResponse' request sent from the server.
 * Create the nodes and edges according to the data passed as arguments.
 * Layout the graph if nodes were added.
 *
 * @param data  - JSON string containing the data of all the nodes to display.
 */
async function handleHierarchy(data: NodeEdge) {
    const newNodes: Node<NodeData>[] = [];
    let subGraphNode: Node | undefined = undefined;
    const numNodes = nodes.length;

    // Remove all node that are not in the graph anymore (after a folding for example).
    nodes = nodes.filter((node) => data.nodesData.some((nodeData) => nodeData.id === node.data.id));
    // Reset edges to ensure only new/current edges are in the graph.
    edges = [];

    // Create or update each node of nodesData.
    for (const nodeData of data.nodesData) {
        const boxedSrc = data.edges.filter(
            (edge) => edge.src === nodeData.id && edge.edgeType === EdgeType.BOXED,
        );

        const boxedDst = data.edges.filter(
            (edge) => edge.dst === nodeData.id && edge.edgeType === EdgeType.BOXED,
        );

        const newNode = nodeFactory(
            0,
            0,
            nodeData,
            // For a subFlow node, the shape is a rectangle with a width of two nodes
            // and a height of twice the number of subNodes it has to allow spacing inside the node.
            boxedSrc.length === 0 ? nodeWidth : nodeWidth * 2,
            boxedSrc.length === 0 ? nodeHeight : nodeHeight * boxedSrc.length * 2,
            boxedSrc.length === 0 ? NodeType.REGULAR : NodeType.GROUP,
            boxedDst.length !== 0 ? boxedDst[0].src : undefined,
        );
        const foundNodeIndex = nodes.findIndex((node) => node.id === newNode.id);

        // Only add node that does not already exist.
        if (foundNodeIndex === -1) {
            // Place the initial position of the node at the same position as the
            // parent it was expanded from.
            newNodes.push(newNode);
            //If it's a node representing a subFlow, place it first to allow its
            //child nodes to be processed correctly.
            if (newNode.type === 'groupedNode') nodes.splice(0, 0, newNode);
            else nodes.push(newNode);
        } else {
            // Update the content of the node.
            nodes[foundNodeIndex] = {
                ...nodes[foundNodeIndex],
                selected: false,
                data: newNode.data,
                parentId: newNode.parentId,
                extent: newNode.extent,
                type: newNode.type,
                width: newNode.width,
                height: newNode.height,
            };
            //If it's a node representing a subFlow, place it first to allow its
            //child nodes to be processed correctly.
            if (nodes[foundNodeIndex].type === 'groupedNode') {
                const tmpNode = nodes.splice(foundNodeIndex, 1);
                if (tmpNode.length > 0) nodes.splice(0, 0, tmpNode[0]);
            }
        }
    }

    const focusIndex = nodes.findIndex((node) => node.data.focus);

    // Focus only if the number of nodes increased.
    if (data.focus && focusIndex !== -1 && nodes.length > numNodes)
        // Recreate the node to force an update.
        nodes[focusIndex] = { ...nodes[focusIndex] };
    else if (focusIndex !== -1 && !data.focus) nodes[focusIndex].data.focus = false;

    // Place the original position of all the new node to the focused Node.
    if (focusIndex !== -1) {
        for (const node of newNodes) {
            node.position = { ...nodes[focusIndex].position };
        }
    }

    // Create all the edges.
    data.edges.forEach(({ src, dst, edgeDirection, edgeType }) => {
        // addEdge checks if an edge src dst already exists.
        if (edgeType !== EdgeType.BOXED)
            edges = addEdge(edgeFactory(src, dst, edgeDirection, edgeType), edges);
    });

    // Recreate the nodes object to force the re-render.
    nodes = nodes.map((node) => {
        return { ...node };
    });

    // Search for the node from which the graph was expanded.
    subGraphNode = nodes.find((node) => node.id === data.mainNodeId);
    // Check if there was a change in the number of nodes and if the reference node for layouting
    // exists and is expanded or was never layouted before.
    if (
        nodes.length !== numNodes &&
        subGraphNode !== undefined &&
        (subGraphNode.data.expanded ||
            (subGraphNode.position.x === 0 && subGraphNode.position.y === 0))
    ) {
        await layoutSubgraph(subGraphNode, nodes, edges, currentDirection, elkOptions);

        // Check if there are subFlows.
        const subFlows = nodes.filter((node) => node.type === 'groupedNode');
        for (const subFlow of subFlows) {
            const subFlowNodes = data.edges
                .filter((edge) => edge.src === subFlow.id && edge.edgeType === EdgeType.BOXED)
                .map((edge) => nodes.find((node) => node.id === edge.dst))
                .filter((node) => node !== undefined);
            if (subFlowNodes.length > 0) {
                layoutSubFlow(subFlow, subFlowNodes, currentDirection);
            }
        }
        moveNodes(nodes, setNodes);
    } else {
        // Send a message to the server to indicate he can handle the next request.
        vscode.postMessage({ command: 'canSendNextData' } as SendNextDataMessage);
        setNodes(nodes);
    }
    setEdges(edges);
    // Stop the waiting bar only if the server has finished sending data.
    // Here the node won't be focused except if the recursive hierarchy process finished or
    // was just a single level.
    if (data.focus) waitingBar(true);
}

/**
 * Update the graph by deleting nodes and updating nodes data.
 *
 * @param data - The data containing the nodes to delete and to update.
 */
function handleUpdate(data: UpdateMessage) {
    for (const node of data.toUpdate) {
        const index = nodes.findIndex((searchNode) => node.id === searchNode.id);
        if (index === -1) continue;
        // Recreate the node object with the new data.
        nodes[index] = {
            ...nodes[index],
            data: node,
        };
    }
    for (const node of data.toDelete) {
        const index = nodes.findIndex((searchNode) => node.id === searchNode.id);
        // Remove the edge that comes from or to the node to delete.
        edges = edges.filter((edge) => edge.source !== node.id && edge.target !== node.id);
        if (index === -1) continue;
        {
            nodes.splice(index, 1);
        }
        const children = nodes.filter((child) => child.parentId === node.id);
        for (const child of children) {
            child.parentId = undefined;
            child.extent = undefined;
        }
    }
    // Recreate the nodes object to force the re-render
    nodes = [...nodes];
    setNodes(nodes);
    waitingBar(true);
}

/**
 * Fill the references picker menu with the locations the user can jump to.
 *
 * @param response - The data containing a name and all the references of interest
 * contained in that name.
 */
function handleReveal(response: RevealReferencesResponse) {
    const locationsMap: Map<string, StringLocation[]> = new Map();
    for (let i = 0; i < response.locationsKeys.length; i++) {
        locationsMap.set(response.locationsKeys[i], response.locationsValues[i]);
    }
    // Check which menu currently exists to fill it with the different locations.
    if (referencesPickerMenu)
        setReferencesPickerMenu({ ...referencesPickerMenu, locationsMap: locationsMap });
    else if (nodeContextMenu) {
        setNodeContextMenu({ ...nodeContextMenu, locationsMap: locationsMap });
    }
}

/**
 * Main function that configures and renders the graph.
 *
 * @returns A div containing the react flow graph's viewPort.
 */
export default function App() {
    // The max and min zoom values of the viewPort.
    const minZoom = 0;
    const maxZoom = 4;

    [nodes, setNodes, onNodesChange] = useNodesState(nodes);
    [edges, setEdges, onEdgesChange] = useEdgesState(edges);
    // Save the state of the context menu (right click on a node).
    // setNodeMenu will be called outside of this function but this is the only place
    // where useState can be used.
    [nodeContextMenu, setNodeContextMenu] = React.useState<NodeContextMenuProps | null>(null);
    // The references picker menu is filled with information from the server side so
    // setReferencesPickerMenu will be called outside this function but this is the only place
    // where useState can be used.
    [referencesPickerMenu, setReferencesPickerMenu] =
        React.useState<ReferencesPickerMenuProps | null>(null);
    // Save the state of the currently selected edges.
    const [selected, setSelected] = React.useState<Edge[]>([]);
    // Save the state of the last focused element to avoid uselessly focusing on it.
    const [lastFocus, setLastFocus] = React.useState<string>('');
    // Used to check if the references picker can be opened, to avoid it opening on repeat when the
    // user let his mouse hover on the edge after closing the picker for example.
    const [canOpenReferencesPicker, setCanOpenReferencesPicker] = React.useState(true);
    const ref = React.useRef<HTMLDivElement>(null);
    // Get container DOM element
    const dom = useStore((s) => s.domNode);

    const { setCenter, getNode, getNodes: getNodes_, getViewport } = useReactFlow();
    getNodes = getNodes_;
    void getNodes;

    React.useEffect(() => {
        // Listener on the message from the server side.
        window.addEventListener('message', handleMessage);
        return () => {
            window.removeEventListener('message', handleMessage);
        };
    }, []);

    React.useEffect(() => {
        const handleKeyDown = (event: KeyboardEvent) => {
            if (event.ctrlKey)
                document.documentElement.style.setProperty(
                    '--visualizer-icon-underline',
                    '1.5px solid',
                );
            // Delete the selected nodes when pressing backspace or delete.
            // If ctrl is pressed at the same time also remove all of its children not linked to
            // other part of the graph.
            if (event.key === 'Delete' || event.key === 'Backspace') {
                const toDeleteId = nodes.filter((node) => node.selected).map((node) => node.id);
                if (toDeleteId.length > 0) {
                    waitingBar();
                    deleteNodes(toDeleteId, event.ctrlKey);
                }
            }
        };

        window.addEventListener('keydown', handleKeyDown);
        return () => window.removeEventListener('keydown', handleKeyDown);
    }, []);

    // When pressing control, underline all the buttons to indicate they have another use
    // (recursively unfold the graph)
    React.useEffect(() => {
        const handleKeyUp = (event: KeyboardEvent) => {
            if (!event.ctrlKey)
                document.documentElement.style.setProperty('--visualizer-icon-underline', 'none');
        };

        window.addEventListener('keyup', handleKeyUp);
        return () => window.removeEventListener('keyup', handleKeyUp);
    }, []);

    /**
     * Send a delete message with the ID of the main node to remove to the server side.
     * @param toDeleteId -  An array of strings containing all the nodes to delete.
     * @param recursive - Whether or not to also remove the descendants of the nodes to delete.
     */
    const deleteNodes = React.useCallback(
        (toDeleteId: string[], recursive: boolean) => {
            waitingBar();
            vscode.postMessage({
                command: 'deleteNodes',
                nodesId: toDeleteId,
                recursive: recursive,
            } as DeleteMessage);

            setNodes(nodes);
        },
        [nodes],
    );

    /**
     * Relayout the whole graph in the opposite direction than the current one.
     */
    const onLayout = React.useCallback(() => {
        currentDirection = currentDirection === Direction.RIGHT ? Direction.DOWN : Direction.RIGHT;

        void layoutSubgraphs(nodes, edges, currentDirection, elkOptions).then(() => {
            nodes = nodes.map((node) => {
                return { ...node };
            });
            const subFlows = nodes.filter((node) => node.type === 'groupedNode');
            for (const subFlow of subFlows) {
                const subFlowNodes = nodes.filter((node) => node.parentId === subFlow.id);
                if (subFlowNodes.length > 0) {
                    layoutSubFlow(subFlow, subFlowNodes, currentDirection);
                }
            }
            moveNodes(nodes, setNodes);
        });
    }, [nodes, edges]);

    /**
     * Reveal the symbol represented by the node in the code.
     * @param event - The mouse click event.
     * @param node - The node that was clicked.
     */
    const onNodeDoubleClick = React.useCallback((event: React.MouseEvent, node: Node) => {
        if ((event.target as HTMLElement).className.includes('visualizer__hierarchy-button'))
            return;
        vscode.postMessage({
            command: 'revealNode',
            nodeId: node.id,
            gotoImplementation: event.ctrlKey,
        } as RevealMessage);

        // Unselect the node after the double click.
        const nodeElem = document.querySelector(`[data-node-id="${node.id}"]`) as HTMLElement;
        const nodeWrapper = document.querySelector(`[data-id="${node.id}"]`) as HTMLElement;

        nodeElem.classList.remove('visualizer__selected');
        nodeElem.blur();
        nodeWrapper.classList.remove('selected');
    }, []);

    /**
     * Highlight all edges linked to the node when hovered.
     * @param event - The mouse hover event.
     * @param node - The node that was hovered.
     */
    const onNodeMouseEnter = React.useCallback((event: React.MouseEvent, node: Node) => {
        void event;
        edges = edges.map((edge) => {
            if (edge.target === node.id || edge.source === node.id)
                edge = changeEdge(
                    edge,
                    'var(--visualizer-border-color-focused)',
                    'visualizer__highlight',
                    null,
                );
            return edge;
        });
        setEdges(edges);
    }, []);

    /**
     * Unhighlight the edges linked to the node when the user stops hovering a node.
     * @param event - The mouse hover event.
     * @param node - The node that was un-hovered.
     */
    const onNodeMouseLeave = React.useCallback((event: React.MouseEvent, node: Node) => {
        void event;
        if (!node.selected) {
            edges = edges.map((edge) => {
                if (edge.target === node.id || edge.source === node.id)
                    edge = changeEdge(edge, '', undefined, null);
                return edge;
            });
        }
        setEdges(edges);
    }, []);

    /**
     * Open the references picker menu under the current position of the mouse.
     *
     * @param event - The react mouse event.
     * @param edge  - The edge the mouse is currently on.
     * @param openedByClick - True if the user clicked on the edge, false if the user
     * hovered the edge.
     */
    function openReferencesPicker(event: React.MouseEvent, edge: Edge, openedByClick: boolean) {
        // When using the references picker if the user selects a location without moving the
        // mouse, the picker would reopen alone causing the user to lose focus on its code.
        if (nodes[0].data.hierarchy === Hierarchy.FILE || nodes[0].data.hierarchy === Hierarchy.GPR)
            return;
        if (ref.current && canOpenReferencesPicker) {
            closeAllPopUp();
            event.preventDefault();

            let targetNodeId: string = '';
            let referenceNodeId: string = '';
            const menuWidth = nodeWidth;
            const pane = ref.current.getBoundingClientRect();
            if (!edge.data) return;
            if (
                edge.data.edgeDirection === RelationDirection.SUB ||
                edge.data.edgeDirection === RelationDirection.BOTH
            ) {
                targetNodeId = edge.source;
                referenceNodeId = edge.target;
            } else if (edge.data.edgeDirection === RelationDirection.SUPER) {
                targetNodeId = edge.target;
                referenceNodeId = edge.source;
            }

            //Make sure the popup doesn't overflow through the left or right side.
            // The top/bottom overflow will be handled in the ReferencesPickerMenu itself
            // when all the references have been gathered.
            let left = event.clientX;
            if (event.clientX - menuWidth / 2 < 0) {
                left += (menuWidth - event.clientX) / 2;
            } else if (event.clientX + menuWidth / 2 > pane.width) {
                left -= event.clientX + menuWidth / 2 - pane.width;
            }

            // The locationsMap with all the references will be filled later when the server
            // sended the data.
            setReferencesPickerMenu({
                onReferencesPickerClose: onReferencesPickerClose,
                top: event.clientY,
                left: left,
                edge: edge,
                source: getNode(referenceNodeId),
                target: getNode(targetNodeId),
                locationsMap: new Map(),
                openedByClick: openedByClick,
                menuWidth: menuWidth,
                pane: pane,
            } as ReferencesPickerMenuProps);

            if (targetNodeId !== '' && referenceNodeId !== '')
                // Ask the server for the references that will fill the references picker.
                vscode.postMessage({
                    command: 'revealReferences',
                    targetNodeId: targetNodeId,
                    referenceNodeId: referenceNodeId,
                    bothDirection: edge.data.edgeDirection === RelationDirection.BOTH,
                } as RevealReferencesMessage);

            // Wait until the menu is created before adding the class to open it.
            const intervalId = setIntervalCapped(
                () => {
                    const edgeMenu = document.getElementsByClassName(
                        'visualizer__references-picker-menu',
                    );
                    if (edgeMenu.length !== 0) {
                        edgeMenu[0].classList.add('visualizer__open');
                        clearInterval(intervalId);
                    }
                },
                50,
                50,
            );
        }
    }

    /**
     * Highlight the marker of the edge when hovering the edge.
     * Additionally if the user hovers for a sufficiently long time, open the references
     * picker menu.
     * @param event - The mouse hover event.
     * @param edge - The edge being hovered.
     */
    const onEdgeMouseEnter = React.useCallback(
        (event: React.MouseEvent, edge: Edge) => {
            void event;
            edge = changeEdge(
                edge,
                'var(--visualizer-border-color-focused)',
                'visualizer__highlight',
                null,
            );

            edges[edges.findIndex((searchEdge) => searchEdge.id === edge.id)] = { ...edge };
            // Refresh the array to force re rendering
            edges = [...edges];
            setEdges(edges);
            // If the user is still hovering the edge after a set time, open the references
            // picker menu.
            timeoutId = setTimeout(() => {
                if (referencesPickerMenu === null) openReferencesPicker(event, edge, false);
            }, 750);
        },

        [edges, canOpenReferencesPicker],
    );

    /**
     * Close the references picker menu.
     */
    const onReferencesPickerClose = React.useCallback(() => {
        const edgeMenu = document.getElementsByClassName('visualizer__references-picker-menu');
        if (edgeMenu.length !== 0) {
            edgeMenu[0].classList.remove('visualizer__open');
        }
        // Prevent the references picker from being open until the mouse is moved after it
        // has been closed.
        setCanOpenReferencesPicker(false);
        setReferencesPickerMenu(null);
    }, [setReferencesPickerMenu]);

    /**
     * Un-highlight the marker of the edge when hovering the edge.
     */
    const onEdgeMouseLeave = React.useCallback(
        (event: React.MouseEvent, edge: Edge) => {
            void event;
            edge = changeEdge(edge, '', undefined, null);

            edges[edges.findIndex((searchEdge) => searchEdge.id === edge.id)] = edge;
            // Refresh the array to force re rendering
            edges = [...edges];
            setEdges(edges);
            if (timeoutId !== null) {
                clearTimeout(timeoutId);
                timeoutId = null;
            }
        },
        [edges],
    );

    /**
     * Close the node context menu and clear the node search bar on pane click.
     *
     * @param event - The mouse event.
     */
    const onPaneClick = React.useCallback(
        (event: React.MouseEvent) => {
            void event;
            setLastFocus('');

            // Force the unselection of all the nodes and edges as sometimes the
            // onChange callback is not called.
            onChange({ nodes: [], edges: [] });
            closeAllPopUp();
        },
        [selected],
    );

    /**
     * Prevent the regular pane click from happening as its features are useless here.
     *
     * @param event - The mouse event.
     */
    const onPaneContextMenu = React.useCallback((event: MouseEvent | React.MouseEvent) => {
        event.preventDefault();
    }, []);

    /**
     * Close the node context menu.
     */
    const onNodeContextClose = React.useCallback(
        () => setNodeContextMenu(null),
        [setNodeContextMenu],
    );

    /**
     * Create the context menu and position it close to the mouse position.
     *
     * @param event - The mouse event.
     */
    const onNodeContextMenu = React.useCallback(
        (event: React.MouseEvent, node: Node) => {
            event.preventDefault();
            if (ref.current) {
                closeAllPopUp();
                const pane = ref.current.getBoundingClientRect();

                setTimeout(() => {
                    setNodeContextMenu({
                        node: node,
                        // Handle the case where the mouse is close to a border
                        // (displace the context menu to another quadrant)
                        top: event.clientY < pane.height - nodeHeight ? event.clientY : undefined,
                        left: event.clientX < pane.width - nodeWidth ? event.clientX : undefined,
                        right:
                            event.clientX >= pane.width - nodeWidth
                                ? pane.width - event.clientX
                                : undefined,
                        bottom:
                            event.clientY >= pane.height - nodeHeight
                                ? pane.height - event.clientY
                                : undefined,
                        locationsMap: new Map(),
                        pane: pane,
                        onContextClose: onNodeContextClose,
                        deleteNodes: deleteNodes,
                    } as NodeContextMenuProps);
                }, 200);
            }
        },
        [setNodeContextMenu, ref],
    );

    /**
     * Send a message to the server side when initialized to indicate it can start sending
     * information.
     */
    const onInit = React.useCallback(() => {
        vscode.postMessage({ command: 'rendered' } as RenderedMessage);
    }, []);

    /**
     * Set the view to the center of the webView.
     */
    const onCenter = React.useCallback(() => {
        void setCenter(0, 0, { zoom: 0.5, duration: 1000 });
    }, []);

    /**
     * Handle the un-selection of the edge to remove their edge marker.
     *
     * @param nodes - The nodes currently selected.
     * @param edges - The edges currently selected.
     */
    const onChange = React.useCallback(
        ({ nodes: selectedNodes, edges: selectedEdges }: Graph) => {
            // Unselect all the edges that are not in the selectedEdges array anymore.
            for (const oldEdge of selected) {
                if (!selectedEdges.some((edge) => edge.id === oldEdge.id)) {
                    const edge = changeEdge(oldEdge, '', undefined, false);
                    edges[edges.findIndex((searchEdge) => searchEdge.id === edge.id)] = edge;
                }
            }

            // Select all the edges neighboring a selected node.
            for (const selectedNode of selectedNodes) {
                const nodeEdge = edges.filter(
                    (edge) => edge.source === selectedNode.id || edge.target === selectedNode.id,
                );
                for (let edge of nodeEdge) {
                    edge = changeEdge(
                        edge,
                        'var(--visualizer-border-color-focused)',
                        'visualizer__highlight',
                        true,
                    );
                    edges[edges.findIndex((searchEdge) => searchEdge.id === edge.id)] = { ...edge };
                    if (selectedEdges.find((searchEdge) => searchEdge.id === edge.id) === undefined)
                        selectedEdges.push(edge);
                }
            }
            // Refresh the array to force re-rendering.
            edges = [...edges];
            setEdges(edges);
            setSelected(selectedEdges);
        },
        [selected, edges],
    );

    // Hook called when the user selects or unselects nodes or edges.
    useOnSelectionChange({
        onChange,
    });

    /**
     * Allow to cycle through the element of the webView and focus on the element if it's a node.
     *
     * @param focus - The focus event.
     */
    const onFocus = React.useCallback(
        (focus: React.FocusEvent) => {
            // Focus on the node currently focused only the mouse is not already on it
            // (avoid triggering the focus on node clicks).
            if (focus.target.classList.contains('visualizer__basic_node')) {
                const nodeId = focus.target.getAttribute('data-node-id');
                if (!nodeId) return;
                if (!focus.target.matches(':hover') && lastFocus !== nodeId) {
                    const node = getNode(nodeId);
                    if (node) {
                        const viewPort = getViewport();
                        if (!dom) return;

                        const x = node.position.x * viewPort.zoom + viewPort.x;
                        const y = node.position.y * viewPort.zoom + viewPort.y;

                        if (x < 0 || x > dom.clientWidth || y < 0 || y > dom.clientHeight) {
                            focusNode(node, getViewport(), setCenter);
                            setLastFocus(nodeId);
                        }
                    }
                }
            } else setLastFocus('');
        },
        [lastFocus, dom],
    );

    /**
     * Small helper function to close all kinds of popups (searchbar, context menu,
     * references picker).
     */
    function closeAllPopUp() {
        onSearchBarClose();
        onNodeContextClose();
        onReferencesPickerClose();
    }
    /**
     * Close all menus on node click.
     */
    const onNodeClick = React.useCallback(() => {
        closeAllPopUp();
    }, []);

    /**
     * Close all menus and open the references picker menu on edge click
     *
     * @param event - The mouse event.
     * @param edge - The clicked edge.
     */
    const onEdgeClick = React.useCallback(
        (event: React.MouseEvent, edge: Edge) => {
            onSearchBarClose();
            onNodeContextClose();
            if (referencesPickerMenu === null) openReferencesPicker(event, edge, true);
        },
        [referencesPickerMenu, canOpenReferencesPicker],
    );

    /**
     * Enable the opening of the references picker menu after the mouse moved.
     */
    const onMouseMove = React.useCallback(() => {
        if (!canOpenReferencesPicker) setCanOpenReferencesPicker(true);
    }, [canOpenReferencesPicker]);

    /**
     * Close all popups when starting to drag a node.
     */
    const onNodeDrag = React.useCallback(() => {
        closeAllPopUp();
    }, []);

    /**
     * Track the escape key to close all popups on press.
     *
     * @param event - The keyboard event.
     */
    const onKeyDown = React.useCallback((event: React.KeyboardEvent) => {
        if (event.key === 'Escape') closeAllPopUp();
    }, []);

    return (
        <div style={{ width: '100vw', height: '100vh' }}>
            {
                <ReactFlow
                    panOnDrag
                    zoomOnScroll
                    ref={ref}
                    nodes={nodes}
                    edges={edges}
                    onInit={onInit}
                    onFocus={onFocus}
                    maxZoom={maxZoom}
                    minZoom={minZoom}
                    nodeTypes={nodeTypes}
                    edgeTypes={edgeTypes}
                    onKeyDown={onKeyDown}
                    // The nodes remain focusable by their inner objects not the outer.
                    nodesFocusable={false}
                    edgesFocusable={false}
                    onNodeDrag={onNodeDrag}
                    nodesConnectable={false}
                    onPaneClick={onPaneClick}
                    onNodeClick={onNodeClick}
                    onMouseMove={onMouseMove}
                    onEdgeClick={onEdgeClick}
                    onNodesChange={onNodesChange}
                    onEdgesChange={onEdgesChange}
                    onEdgeMouseEnter={onEdgeMouseEnter}
                    onEdgeMouseLeave={onEdgeMouseLeave}
                    onNodeMouseEnter={onNodeMouseEnter}
                    onNodeMouseLeave={onNodeMouseLeave}
                    onPaneContextMenu={onPaneContextMenu}
                    onNodeDoubleClick={onNodeDoubleClick}
                    onNodeContextMenu={onNodeContextMenu}
                    selectionMode={SelectionMode.Partial}
                    deleteKeyCode={[]}
                    connectionLineComponent={floatingConnectionLine}
                    className="visualizer__colors"
                >
                    <Controls>
                        <ControlButton
                            className="codicon codicon-layout"
                            title="Layout Graph"
                            onClick={() => onLayout()}
                        />
                        <ControlButton
                            className="codicon codicon-record visualizer__bottom-button"
                            title="Center View"
                            onClick={onCenter}
                        />
                    </Controls>
                    {nodeContextMenu && <NodeContextMenu {...nodeContextMenu} />}
                    {referencesPickerMenu && <ReferencesPickerMenu {...referencesPickerMenu} />}
                    <Background size={3} gap={56} />
                    <Panel position="top-right">
                        <SearchBar />
                    </Panel>
                </ReactFlow>
            }
        </div>
    );
}

ReactDOM.createRoot(document.getElementById('root')!).render(
    <React.StrictMode>
        <ReactFlowProvider>
            <App />
        </ReactFlowProvider>
    </React.StrictMode>,
);
