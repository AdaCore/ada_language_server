import * as React from 'react';
import { Handle, Node, NodeProps, Position, useReactFlow, XYPosition } from '@xyflow/react';
import './visualizerStyleSheet.css';
import {
    Direction,
    NodeData,
    RelationDirection,
    HierarchyMessage,
    NodeType,
    SendNextDataMessage,
} from '../visualizerTypes';
import { currentDirection, vscode } from './App';
import { setIntervalCapped, waitingBar } from './utils';

type DataNode = Node<NodeData, 'data'>;

/**
 *  The possible type of node the graph can have.
 */
export const nodeTypes = {
    basicNode: BasicNode,
    groupedNode: GroupedNode,
};

const nodeString: string[] = ['basicNode', 'groupedNode'];

/**
 * Return a new react flow node.
 *
 * @param x - x position of the node.
 * @param y - y position of the node.
 * @param data - Data stored by the node.
 * @param width - The width of the node.
 * @param height - The height of the node.
 * @param nodeType - The type of the node that will influence its shape.
 * @param parentId - The id of another node that will serve as a parent for this node. Used
 * for subFlow graphs.
 * @returns A new react flow Node
 */
export function nodeFactory(
    x: number,
    y: number,
    data: NodeData,
    width: number = 150,
    height: number = 200,
    nodeType: NodeType,
    parentId: string | undefined = undefined,
) {
    const { ...objData } = data;
    return {
        id: data.id,
        type: nodeString[nodeType],
        position: { x: x, y: y },
        data: objData,
        width: width,
        height: height,
        parentId: parentId,
        style: {
            borderRadius: '6px',
        },
        extent: parentId ? 'parent' : undefined,
    } as Node<NodeData>;
}

/**
 * Animate the movement of the nodes from their original locations to their new locations.
 * This cannot be during the node creation as you can't access the internal data of a node during
 * its creation and thus you can't modify its position and style.
 *
 * @param movingNodes - The array of node that need to move to their new positions.
 * @param setNodes - The function to set the nodes' state of the graph.
 * @param duration - The duration of the animation.
 */
export function moveNodes(
    movingNodes: Node<NodeData>[],
    setNodes: (payload: Node<NodeData>[] | ((nodes: Node<NodeData>[]) => Node<NodeData>[])) => void,
    duration: number = 100,
) {
    const newPositions: XYPosition[] = [];
    const parents: HTMLDivElement[] = [];
    setNodes(movingNodes);
    const notFound: Node[] = [...movingNodes];
    //Wait for all the nodes to be rendered so that their parents are created and can be gotten.
    const interval = setIntervalCapped(
        () => {
            for (let i = 0; i < notFound.length; i++) {
                const newNode = notFound[i];
                const parent = document.querySelector(`[data-id='${newNode.id}'`) as HTMLDivElement;

                if (parent) {
                    parents.push(parent);
                    parent.style.transition = `transform ${duration}ms ease-out`;
                    notFound.splice(i--, 1);
                    // Unselect all the nodes (the nodes can automatically be selected when
                    // clicking on one of their buttons).
                    parent.blur();
                    newNode.selected = false;

                    //Either they have a new position or they just stay to their original position.
                    if (newNode.data.newPosition) {
                        newPositions.push(newNode.data.newPosition as XYPosition);
                        newNode.data.newPosition = undefined;
                    } else {
                        newPositions.push(newNode.position);
                    }
                }
            }
            // Become true when all the parents were successfully queried.
            if (parents.length === movingNodes.length) {
                setTimeout(() => {
                    for (let i = 0; i < movingNodes.length; i++) {
                        movingNodes[i].position = newPositions[i];
                    }
                    // Recreates the node objects to force the re-rendering.
                    const nodes = movingNodes.map((node) => {
                        return { ...node };
                    });

                    setNodes(nodes);
                    //Remove the transition animation.
                    setTimeout(() => {
                        for (const parent of parents) {
                            parent.style.transition = 'inherit';
                        }
                        vscode.postMessage({ command: 'canSendNextData' } as SendNextDataMessage);
                    }, duration);
                }, 100);
                // Stop the interval loop.
                clearInterval(interval);
            }
        },
        10,
        1000,
    );
}

/**
 * Customize a basic node with style, callbacks and buttons.
 *
 * @param node - The base node to customize.
 * @returns A react JSX object representing the node.
 */
function BasicNode(node: NodeProps<DataNode>) {
    const data = node.data;
    const { setCenter, getViewport } = useReactFlow();

    // Dynamically assign class to DOM element to take into account layouting direction,
    // type of data being displayed....
    const color = 'var(--vscode-symbolIcon-' + data.kind + 'Foreground)';
    const nodeClass =
        'visualizer__basic_node' +
        (node.selected ? ' visualizer__selected ' : '') +
        (!data.inProject ? ' visualizer__out-of-project' : '');
    const iconClass = 'visualizer__icon codicon codicon-symbol-' + data.kind;

    const subHierarchyTypeArr = ['type-hierarchy-sub', 'call-outgoing', 'file', 'unfold'];
    // Modify the icon in the button depending on the situation:
    // - Call outgoing when the user didn't request the children nodes yet.
    // - Chevron Down when the children are unfolded.
    // - Chevron Up when the children are folded.
    const subButtonClass =
        'codicon codicon-' +
        (data.hasChildren === null
            ? subHierarchyTypeArr[data.hierarchy]
            : data.expanded
              ? 'chevron-down'
              : 'chevron-right') +
        ' visualizer__hierarchy-button visualizer__sub-button-' +
        (currentDirection === Direction.RIGHT ? 'right' : 'down') +
        (!data.inProject ? ' visualizer__out-of-project' : '');

    // Background of the button to avoid transparency problems.
    const subButtonBackgroundClass =
        'visualizer__button-background ' +
        'visualizer__sub-button-' +
        (currentDirection === Direction.RIGHT ? 'right' : 'down');

    const superHierarchyTypeArr = ['type-hierarchy-super', 'call-incoming', 'file', 'unfold'];
    // Modify the icon in the button depending on the situation:
    // - Call incoming when the user didn't request the parent nodes yet.
    // - No button once the node has parents.
    const superButtonClass =
        'codicon codicon-' +
        superHierarchyTypeArr[data.hierarchy] +
        ' visualizer__hierarchy-button visualizer__super-button-' +
        (currentDirection === Direction.RIGHT ? 'left' : 'up') +
        (!data.inProject ? ' visualizer__out-of-project' : '');

    // Background of the button to avoid transparency problems.
    const superButtonBackgroundClass =
        'visualizer__button-background ' +
        'visualizer__super-button-' +
        (currentDirection === Direction.RIGHT ? 'left' : 'up') +
        (!data.inProject ? ' visualizer__out-of-project' : '');

    const superTooltipTitle = [
        'supertypes',
        'incoming calls',
        'dependent files',
        'dependent gpr files',
    ];
    // Tooltips for the buttons.
    const superButtonTitle =
        (node.data.expanded && node.data.hasParent ? 'Hide ' : 'Display ') +
        superTooltipTitle[data.hierarchy];

    const subTooltipTitle = [
        'subtypes',
        'outgoing calls',
        'dependent files',
        'dependent gpr files',
    ];
    const subButtonTitle =
        (node.data.expanded && node.data.hasChildren ? 'Hide ' : 'Display ') +
        subTooltipTitle[data.hierarchy];

    // Handle Windows/Linux/MacOS filesystems
    const fileName = data.string_location.path.replace(/^.*(\\|\/|:)/, '');

    // Focus on the graph on this node
    if (data.focus) {
        data.focus = false;
        const x = node.positionAbsoluteX + (node.width ?? 0) / 2;
        const y = node.positionAbsoluteY + (node.height ?? 0) / 2;

        const viewPort = getViewport();
        void setCenter(x, y, {
            zoom: viewPort.zoom,
            duration: 250,
        });
    }

    /**
     * Request additional nodes to be added for the graph.
     *
     * @param event - The mouse event.
     * @param direction - The direction in which to get the new nodes, either the parents,
     * the children or both of the current nodes.
     */
    const requestHierarchy = React.useCallback(
        (event: React.MouseEvent, direction = RelationDirection.SUPER) => {
            event.preventDefault();
            waitingBar();
            vscode.postMessage({
                command: 'requestHierarchy',
                id: data.id,
                direction: direction,
                expand:
                    direction === RelationDirection.SUB && data.hasChildren
                        ? !data.expanded
                        : data.expanded,
                hierarchy: data.hierarchy,
                // If the actual button is the folding button the recursive expansion cannot be
                // triggered.
                recursive: subButtonClass.includes('chevron') ? false : event.ctrlKey,
            } as HierarchyMessage);
        },
        [data.id, data.kind, data.expanded, data.hasChildren],
    );

    return (
        <div
            tabIndex={0}
            className={nodeClass}
            data-node-id={data.id}
            title={(data.inProject ? '' : '[out of project] ') + data.label}
        >
            {/* Hide the handles in the node and make them invisible as the user does not need to
            connect edges. */}
            <Handle
                className="visualizer__invis"
                type="target"
                position={currentDirection === Direction.RIGHT ? Position.Left : Position.Top}
                style={{
                    top: currentDirection === Direction.RIGHT ? undefined : '1%',
                    left: currentDirection === Direction.RIGHT ? '1%' : undefined,
                }}
            />
            <Handle
                className="visualizer__invis"
                type="source"
                position={currentDirection === Direction.RIGHT ? Position.Right : Position.Bottom}
                style={{
                    bottom: currentDirection === Direction.RIGHT ? undefined : '1%',
                    right: currentDirection === Direction.RIGHT ? '1%' : undefined,
                }}
            />
            {/* The upper part of the node that contains the name of the symbol
            and its kind as an icon. */}
            <div className="visualizer__node-title">
                <span className={iconClass} style={{ color: color }}></span>
                <div className={'visualizer__text visualizer__ellipsis-text'}>{data.label}</div>
            </div>
            {/* The lower part of the node that contains information about the symbol. */}
            <div className="visualizer__node-body">
                <div className="visualizer__ellipsis-text">File : {fileName}</div>
                <div className="visualizer__ellipsis-text">
                    Position : {data.string_location.position}
                </div>
            </div>
            {/* The super class button allows requesting the parents of the node. */}
            <button
                className={superButtonClass}
                title={superButtonTitle}
                // Null means that the presence of parents has not been checked yet.
                style={{ display: data.hasParent !== null ? 'none' : 'inherit' }}
                onClick={(event) => {
                    requestHierarchy(event, RelationDirection.SUPER);
                }}
            ></button>
            <div
                style={{
                    display: data.hasParent !== null ? 'none' : 'inherit',
                }}
                className={superButtonBackgroundClass}
            ></div>
            <button
                className={subButtonClass}
                title={subButtonTitle}
                // Here we only want to hide the button when there is no child.
                // If we don't know the request hierarchy button is displayed, if
                //there are children, the fold button is displayed.
                style={{
                    display: data.hasChildren === false ? 'none' : 'inherit',
                }}
                onClick={(event) => {
                    requestHierarchy(event, RelationDirection.SUB);
                }}
            ></button>
            <div
                style={{ display: data.hasChildren === false ? 'none' : 'inherit' }}
                className={subButtonBackgroundClass}
            ></div>
        </div>
    );
}

/**
 * Create a node that will serve as an area for a subGraph which will be contained
 * within its boundaries.
 *
 * @param node - The base node to customize.
 * @returns A react JSX object representing the node.
 */
function GroupedNode(node: NodeProps<DataNode>) {
    const data = node.data;

    const color = 'var(--vscode-symbolIcon-' + data.kind + 'Foreground)';
    const iconClass = 'visualizer__icon codicon codicon-symbol-' + data.kind;
    return (
        <div
            tabIndex={0}
            className="visualizer__grouped_node"
            data-node-id={data.id}
            title={(data.inProject ? '' : '[out of project') + data.label}
        >
            {/* Hide the handles in the node and make them invisible as the user does not need to
            connect edges. */}
            <Handle
                className="visualizer__invis"
                type="target"
                position={currentDirection === Direction.RIGHT ? Position.Left : Position.Top}
                style={{
                    top: currentDirection === Direction.RIGHT ? undefined : '1%',
                    left: currentDirection === Direction.RIGHT ? '1%' : undefined,
                }}
            />
            <Handle
                className="visualizer__invis"
                type="source"
                position={currentDirection === Direction.RIGHT ? Position.Right : Position.Bottom}
                style={{
                    bottom: currentDirection === Direction.RIGHT ? undefined : '1%',
                    right: currentDirection === Direction.RIGHT ? '1%' : undefined,
                }}
            />
            {/* The upper part of the node that contains the name of the symbol
            and its kind as an icon. */}
            <div className="visualizer__grouped_node-title">
                <span className={iconClass} style={{ color: color, fontSize: 'x-large' }}></span>
                <div className={'visualizer__text visualizer__ellipsis-text'}>{data.label}</div>
            </div>
        </div>
    );
}
