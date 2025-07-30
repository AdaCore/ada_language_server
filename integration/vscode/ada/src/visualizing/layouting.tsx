import { addEdge, Edge, Node, Position } from '@xyflow/react';
import ELK, { ElkNode } from 'elkjs/lib/elk.bundled.js';
import { currentDirection } from './App';
import { Direction, EdgeType, NodeData, RelationDirection } from '../visualizerTypes';
import { edgeFactory } from './customEdges';

/**
 * Represent the smallest box that can contain all the nodes of a subgraph.
 */
type BoundingBox = {
    minX: number;
    maxX: number;
    minY: number;
    maxY: number;
    width: number;
    height: number;
};

/**
 * Represent a subgraph.
 */
type Subgraph = {
    nodes: Node[];
    edges: Edge[];
};

export const elkOptions = {
    'elk.algorithm': 'mrtree',
    'elk.layered.spacing.nodeNodeBetweenLayers': '500',
    'elk.layered.spacing.edgeNodeBetweenLayers': '500',
    'elk.spacing.nodeNode': '300',
    'elk.layered.layering.strategy': 'INTERACTIVE',
    'elk.layered.cycleBreaking.strategy': 'INTERACTIVE',
    'elk.layered.crossingMinimization.strategy': 'INTERACTIVE',
    'elk.layered.nodePlacement.strategy': 'INTERACTIVE',
};

const elk = new ELK();

/**
 * Layout a subgraph by converting all the nodes and edges to Elkjs' ones, calculating
 * their positions and then converting them back to their original types.
 *
 * @param subGraph - The subgraph to layout.
 * @param direction - The direction in which to layout the subgraph.
 * @param options - Elkjs options used to customize the layouting algorithm.
 * @returns A subgraph with the same nodes but with their positions updated.
 */
export const getLayoutedElements = async (
    subGraph: Subgraph,
    direction = Direction.RIGHT,
    options = {},
) => {
    const nodes = subGraph.nodes;
    const edges = subGraph.edges;
    const isHorizontal = direction === Direction.RIGHT;
    // Convert current graph to ELK graph.
    const graph: ElkNode = {
        id: 'root',
        layoutOptions: { 'elk.direction': direction.toString(), ...options },
        children: nodes.map((node) => ({
            ...node,
            'elk.position': {
                x: node.position.x,
                y: node.position.y,
            },
        })),
        edges: edges.map((edge) => ({
            id: edge.id,
            sources: [edge.source],
            targets: [edge.target],
        })),
    };

    // Layout the ELK graph.
    const layout = await elk.layout(graph);

    if (!layout || !layout.children) return { nodes: [], edges: [] } as Subgraph;

    // Convert the ELK graph back to the original one with the new positions.
    return {
        nodes: layout.children.map((node) => {
            const initialNode = nodes.find((n) => n.id === node.id);
            if (!initialNode) {
                throw new Error('Node not found');
            }
            // Do not change the node's position yet, it will be changed later by the
            // moveNodes function.
            initialNode.data.newPosition = {
                x: node.x,
                y: node.y,
            };
            initialNode.targetPosition = isHorizontal ? Position.Left : Position.Top;
            initialNode.sourcePosition = isHorizontal ? Position.Right : Position.Bottom;
            return initialNode;
        }),
        edges: (layout.edges ?? []).map((edge) => {
            const initialEdge = edges.find((e) => e.id === edge.id);
            if (!initialEdge) {
                throw new Error('Edge not found');
            }
            initialEdge.source = edge.sources[0];
            initialEdge.target = edge.targets[0];
            return initialEdge;
        }),
    } as Subgraph;
};

/**
 * Extract all the nodes and edges forming a subgraph from the nodes and edges array, starting from
 * a specific node
 *
 * This is a recursive function.
 *
 * @param node - The node that mark the subgraph to extract.
 * @param nodes - An array containing all the nodes not already part of any subgraphs.
 * @param edges - An array containing all the edges not already part of any subgraphs.
 * @param onlyChilds - If true only get the graph starting from node with all its children,
 * if false return the whole subgraph.
 */
function getSubGraph(node: Node, nodes: Node[], edges: Edge[], onlyChilds = false) {
    const subNodes: Node[] = [];
    const subEdges: Edge[] = [];
    const nodeQueue: Node[] = [];
    nodeQueue.push(node);
    while (nodeQueue.length !== 0) {
        const currentNode = nodeQueue.pop();
        if (!currentNode) break;

        if (subNodes.some((subNode) => subNode.id === currentNode.id)) continue;

        subNodes.push(currentNode);
        if (!onlyChilds) nodes.splice(nodes.indexOf(currentNode), 1);

        const subEdge: Edge[] = edges.filter(
            (edge) =>
                (onlyChilds ? false : edge.target === currentNode.id) ||
                edge.source === currentNode.id,
        );
        subEdge.forEach((subEdge) => {
            if (!subEdges.some((edge) => edge.id === subEdge.id)) {
                subEdges.push(subEdge);
                if (!onlyChilds) edges.splice(edges.indexOf(subEdge), 1);
            }
        });
        for (const edge of subEdges) {
            const otherId: string = edge.source === currentNode.id ? edge.target : edge.source;
            if (!subNodes.some((node) => node.id === otherId)) {
                const otherNode = nodes.find((node) => node.id === otherId);
                if (otherNode !== undefined) {
                    nodeQueue.push(otherNode);
                }
            }
        }
    }
    return { nodes: subNodes, edges: subEdges } as Subgraph;
}

/**
 * Sort all the nodes and edges into subgraphs
 *
 * @param nodes - An array containing all the nodes from the graph.
 * @param edges - An array containing all the edges from the edges.
 * @returns An array containing all the subgraphs.
 */
function getSubGraphs(nodes: Node[], edges: Edge[]) {
    const subgraphs: Subgraph[] = [];
    while (nodes.length != 0) {
        subgraphs.push(getSubGraph(nodes[0], nodes, edges));
    }
    return subgraphs;
}

/**
 * Concatenate the subgraphs back to their original node array and edge array.
 *
 * @param subGraphs - The array containing all the subgraphs.
 * @param nodes - The original nodes array.
 * @param edges - The original edges array.
 */
function concatSubgraphs(subGraphs: Subgraph[], nodes: Node[], edges: Edge[]) {
    subGraphs.forEach((subGraph) => {
        nodes.push(...subGraph.nodes);
        for (const edge of subGraph.edges) {
            //We don't want to add the temporary edge to the true edge array.
            if (edge.type !== 'temporary') edges.push(edge);
        }
    });
    return { nodes: nodes, edges: edges } as Subgraph;
}

/**
 * Get the smallest box containing all the nodes of a subgraph.
 *
 * @param nodes - The array of all the nodes contained in the subgraph.
 * @param current - True if the subgraph is the one being currently layouted, else false.
 * @returns The position and size of the smallest box containing the subgraph.
 */
function getBoundingBox(nodes: Node[], current: boolean) {
    //Only the node being currently layouted has a newPosition field, for the other,
    //their positions field already has the right values.
    const xs = nodes.map((node) =>
        current ? ((node.data as NodeData).newPosition?.x ?? 0) : node.position.x,
    );
    const ys = nodes.map((node) =>
        current ? ((node.data as NodeData).newPosition?.y ?? 0) : node.position.y,
    );
    const nodeWidth = nodes[0].width ?? 0;
    const nodeHeight = nodes[0].height ?? 0;
    const minX = Math.min(...xs);
    const maxX = Math.max(...xs) + nodeWidth;
    const minY = Math.min(...ys);
    const maxY = Math.max(...ys) + nodeHeight;
    return {
        minX: minX,
        maxX: maxX,
        minY: minY,
        maxY: maxY,
        width: maxX - minX,
        height: maxY - minY,
    };
}

/**
 * Helper function to check if the current subgraph is overlapping
 * with any other subgraphs.
 *
 * @param x - The x position of the subgraph.
 * @param y - The y position of the subgraph.
 * @param width - The width of the subgraph.
 * @param height - The height of the subgraph.
 * @param existingBoxes - The array of all the other subgraphs' boxes.
 * @param padding- Minimal distance between two subgraphs.
 * @returns True if the subgraph does not overlap with any other subgraphs else False.
 */
function isOverlapping(
    x: number,
    y: number,
    width: number,
    height: number,
    existingBoxes: BoundingBox[],
    padding: number,
) {
    return existingBoxes.some(
        (box) =>
            !(
                x + width < box.minX - padding ||
                y + height < box.minY - padding ||
                x > box.maxX + padding ||
                y > box.maxY + padding
            ),
    );
}

/**
 * Find a position for a subgraph with no overlapping with any other subgraphs.
 *
 * @param subBox - The subgraph box that is being layouted
 * @param existingBoxes - All the other subgraph boxes
 * @param padding - Additional distance added to avoid the subgraphs being to
 * close from one another.
 * @param step - Distance between to overlapping check.
 * @returns A position on the viewport, that does not overlap with any other graph.
 */
function findNonOverlappingPosition(
    subBox: BoundingBox,
    existingBoxes: BoundingBox[],
    padding = 300,
    step = 300,
) {
    let newX = subBox.minX;
    let newY = subBox.minY;
    let numberOfStep = 1;
    let stepX = currentDirection === Direction.RIGHT ? 0 : step;
    let stepY = currentDirection === Direction.RIGHT ? -step : 0;

    let overlap = isOverlapping(newX, newY, subBox.width, subBox.height, existingBoxes, padding);
    //Spiral around the current location of the subgraph to find the closest location that fits it.
    while (overlap) {
        // Every two directions change make 1 more step before changing direction.
        for (let c = 0; c < 2; c++) {
            for (let i = 0; i < numberOfStep; i++) {
                newX += stepX;
                newY += stepY;
                overlap = isOverlapping(
                    newX,
                    newY,
                    subBox.width,
                    subBox.height,
                    existingBoxes,
                    padding,
                );
                if (!overlap) break;
            }
            // Rotate the coordinates between [step, 0], [0, step], [-step, 0], [0, -step].
            [stepX, stepY] = [-stepY, stepX];
        }
        numberOfStep++;
    }

    return { x: newX, y: newY };
}

/**
 * Extract the subgraph currNode is part of, layout it, place it somewhere with no overlapping.
 *
 * This function changes nodes and edges in place.
 *
 * @param currNode - The node from which will be extracted the subgraph that will be layouted.
 * @param nodes - The array of all the nodes of the graph.
 * @param edges - The array of all the edges of the graph.
 * @param direction - The direction in which to layout the graph.
 * @param options - Elkjs option used to customize how the layout is done
 * (case of aggregate GPR project for example).
 */
export async function layoutSubgraph(
    currNode: Node,
    nodes: Node[],
    edges: Edge[],
    direction = Direction.RIGHT,
    options = {},
) {
    if (currNode.parentId) {
        const tmpNode = nodes.find((node) => node.id === currNode.parentId);
        if (!tmpNode) return;
        currNode = tmpNode;
    }

    let layoutEdges: Edge[] = [];
    // Get all the nodes that are in a sub Flow and for each when temporarily replace all their
    // edges connected to the rest of the graph by edges linked to their parent nodes who represent
    // the subFlow.
    const childNodes = nodes.filter((node) => node.parentId);
    for (const childNode of childNodes) {
        const childEdges = edges.filter(
            (edge) => edge.source === childNode.id || edge.target === childNode.id,
        );
        for (const childEdge of childEdges) {
            // Add a temporary edge.
            const newEdge = edgeFactory(
                childEdge.source === childNode.id ? (childNode.parentId ?? '') : childEdge.target,
                childEdge.source === childNode.id ? childEdge.target : (childNode.parentId ?? ''),
                childEdge.markerStart ? RelationDirection.SUB : RelationDirection.SUPER,
                EdgeType.TEMPORARY,
            );
            if (layoutEdges.find((edge) => newEdge.id === edge.id) === undefined) {
                layoutEdges = addEdge(newEdge, layoutEdges);
            }
        }
    }

    // Add all the edges except the one that got replaced by a temporary edge to the layout array.
    edges
        .filter(
            (edge) =>
                childNodes.find((node) => node.id === edge.target || node.id === edge.source) ===
                undefined,
        )
        .forEach((edge) => (layoutEdges = addEdge(edge, layoutEdges)));

    // Remove all the nodes that are located in a subFlow.
    const layoutNodes = nodes.filter((node) => !node.parentId);
    // Split the nodes and edges array into subgraphs.
    const subGraphs: Subgraph[] = getSubGraphs(layoutNodes, layoutEdges);

    // Get the graph that will be layouted.
    const currSubGraph = subGraphs.find((subGraph) =>
        subGraph.nodes.find((node) => currNode.id === node.id),
    );
    if (currSubGraph === undefined) return;

    // Remove the current subgraph from the list.
    subGraphs.splice(subGraphs.indexOf(currSubGraph), 1);

    const { x: xpos, y: ypos } = currNode.position;
    // Layout the subgraph.
    const currLayoutedSubGraph = await getLayoutedElements(currSubGraph, direction, options);

    // Offset the whole subgraph to put back the current node to its original location.
    const layoutedCurrNode = currLayoutedSubGraph.nodes.find((node) => node.id === currNode.id);
    if (layoutedCurrNode !== undefined) {
        const position = (layoutedCurrNode.data as NodeData).newPosition;
        if (position) {
            const xDiff = xpos - position.x;
            const yDiff = ypos - position.y;
            currLayoutedSubGraph.nodes.forEach((node: Node) => {
                const newPosition = (node.data as NodeData).newPosition;
                node.data.newPosition = {
                    x: (newPosition?.x ?? 0) + xDiff,
                    y: (newPosition?.y ?? 0) + yDiff,
                };
            });
        }
    }

    const currBox = getBoundingBox(currSubGraph.nodes, true);
    const allBoxes = subGraphs.map((subGraph) => getBoundingBox(subGraph.nodes, false));

    // Re-offset the graph to a position that does not overlap another graph.
    const newPosition = findNonOverlappingPosition(currBox, allBoxes);
    currLayoutedSubGraph.nodes = currLayoutedSubGraph.nodes.map((node) => {
        const position = (node.data as NodeData).newPosition ?? { x: 0, y: 0 };
        return {
            ...node,
            data: {
                ...node.data,
                // Do not change the node's position yet, it will be changed later by the
                // moveNodes function.
                newPosition: {
                    x: position.x + (newPosition.x - currBox.minX),
                    y: position.y + (newPosition.y - currBox.minY),
                },
            },
        };
    });
    return { nodes: nodes, edges: edges };
}

/**
 * Layout all the subgraphs one by one.
 *
 * @param nodes - The array of all the nodes of the graph.
 * @param edges - The array of all the edges of the graph.
 * @param direction - The direction in which to layout the graph.
 * @param options - Elkjs option used to customize how the layout is done.
 */
export async function layoutSubgraphs(
    nodes: Node[],
    edges: Edge[],
    direction = Direction.RIGHT,
    options = {},
) {
    nodes.map((node) => {
        if (node.type === 'groupedNode') {
            const tmp = node.width;
            node.width = node.height;
            node.height = tmp;
        }
    });
    const subgraphs: Subgraph[] = getSubGraphs(nodes, edges);
    const layoutedSubGraphs: Subgraph[] = [];
    for (const subgraph of subgraphs) {
        const layoutedSubGraph = await getLayoutedElements(subgraph, direction, options);
        const allBoxes = layoutedSubGraphs.map((layouted) => getBoundingBox(layouted.nodes, true));
        const currBox = getBoundingBox(layoutedSubGraph.nodes, true);
        const newPosition = findNonOverlappingPosition(currBox, allBoxes);
        layoutedSubGraph.nodes.forEach((node) => {
            const position = (node.data as NodeData).newPosition;
            if (position && !node.parentId) {
                // Do not change the node's position yet, it will be changed later by the
                // moveNodes function.
                node.data.newPosition = {
                    x: position.x + (newPosition.x - currBox.minX),
                    y: position.y + (newPosition.y - currBox.minY),
                };
            }
            // If the  node is contained in a subFlow, it must not change position
            // with the whole graph.
            else if (node.parentId) {
                node.data.newPosition = undefined;
            }
        });
        layoutedSubGraphs.push(layoutedSubGraph);
    }

    concatSubgraphs(layoutedSubGraphs, nodes, edges);
}

/**
 * Order the subNodes of a subFlow so they fill the subFlow.
 * @param subFlowNode - The parent node that contains all the other nodes.
 * @param children  - An array of nodes that contain all the children inside a subFlowNode.
 * @param direction - The direction in which to layout the graph.
 */
export function layoutSubFlow(subFlowNode: Node, children: Node<NodeData>[], direction: Direction) {
    if (children.length === 0) return;

    // Can never be undefined but is needed to remove the type error.
    const nodeWidth = children[0].width ?? 0;
    const nodeHeight = children[0].height ?? 0;

    subFlowNode.width =
        direction === Direction.RIGHT ? nodeWidth * 2 : nodeWidth * children.length * 2;
    subFlowNode.height =
        direction === Direction.RIGHT ? nodeHeight * children.length * 2 : nodeHeight * 2;

    const box = getBoundingBox([subFlowNode], true);

    let position: { x: number; y: number } = {
        x:
            direction === Direction.RIGHT
                ? box.minX + box.width / 2 - nodeWidth / 2
                : box.minX + nodeWidth / 2,
        y:
            direction === Direction.RIGHT
                ? box.minY + nodeHeight / 2
                : box.minY + box.height / 2 - nodeHeight / 2,
    };

    for (const child of children) {
        child.data.newPosition = {
            x: position.x - box.minX,
            y: position.y - box.minY,
        };
        position = {
            x: direction === Direction.RIGHT ? position.x : position.x + nodeWidth * 2,
            y: direction === Direction.RIGHT ? position.y + nodeHeight * 2 : position.y,
        };
    }
}
