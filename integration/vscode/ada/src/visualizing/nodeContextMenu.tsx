import { Node, ReactFlowProvider } from '@xyflow/react';
import React from 'react';
import { vscode } from './App';
import {
    Hierarchy,
    HierarchyMessage,
    NodeData,
    RefreshMessage,
    RelationDirection,
    RevealMessage,
    RevealReferencesMessage,
    StringLocation,
} from '../visualizerTypes';
import { setIntervalCapped, waitingBar } from './utils';
import {
    referencePickerCreateList,
    referencesPickerOnClick,
    referencesPickerOnKeyDown,
} from './referencesPickerMenu';

/**
 * Type representing the data necessary to open a node context menu.
 */
export type NodeContextMenuProps = {
    node: Node<NodeData>;
    top: number | undefined;
    left: number | undefined;
    right: number | undefined;
    bottom: number | undefined;
    locationsMap: Map<string, StringLocation[]>;
    pane: DOMRect;
    onContextClose: () => void;
    deleteNodes: (toDeleteId: string[], recursive: boolean) => void;
};

// Store the ID of the timeout used for closing the menu.
let intervalId: NodeJS.Timeout | null = null;

/**
 * Open a menu with different options on node click
 *
 * @param props  - Data passed to the context menu
 * @returns a div containing a context menu for a specific node
 */
export function NodeContextMenu(props: NodeContextMenuProps) {
    const [current, setCurrent] = React.useState(-1);
    const [canClose, setCanClose] = React.useState(true);
    const locations: React.JSX.Element[] = [];

    // Close the context menu if the mouse leave the window
    React.useEffect(() => {
        const handleLostFocus = (): void => {
            if (canClose) {
                props.onContextClose();
            }
        };

        window.addEventListener('blur', handleLostFocus);

        return () => {
            window.removeEventListener('blur', handleLostFocus);
        };
    }, [canClose]);

    /**
     * Send a refresh node request to the server side.
     */
    const refreshNode = React.useCallback(() => {
        waitingBar();
        vscode.postMessage({
            command: 'refreshNodes',
            nodesId: [props.node.id],
            recursive: false,
        } as RefreshMessage);
        props.onContextClose();
    }, [props.node.id]);

    /**
     * Send a delete node request to the server side.
     */
    const deleteNode = React.useCallback(() => {
        props.deleteNodes([props.node.id], false);
        props.onContextClose();
    }, [props.node]);

    /**
     * Send a hierarchy request to the server side.
     * @param event - The mouse event.
     * @param direction - The direction in which to get the new nodes, either the parents,
     * the children or both of the current nodes.
     */
    const requestHierarchy = React.useCallback(
        (event: React.MouseEvent, direction = RelationDirection.SUPER) => {
            event.preventDefault();
            const hierarchy = props.node.data.hierarchy;
            waitingBar();
            vscode.postMessage({
                command: 'requestHierarchy',
                id: props.node.id,
                direction: direction,
                expand: props.node.data.expanded,
                hierarchy: hierarchy,
                recursive: event.ctrlKey,
            } as HierarchyMessage);
            props.onContextClose();
        },
        [props.node.data.expanded, props.node.id, props.node.data.kind],
    );

    /**
     * Open the references picker sub list when hovering the corresponding button.
     */
    const onMouseEnter = React.useCallback(() => {
        const list = document.getElementsByClassName('visualizer__references-picker-menu');
        // Show the menu only if there is something to display.
        if (list.length > 0) (list[0] as HTMLElement).style.visibility = 'visible';
        if (props.locationsMap.size === 0) {
            vscode.postMessage({
                command: 'revealReferences',
                referenceNodeId: props.node.id,
                targetNodeId: '',
                bothDirection: false,
            } as RevealReferencesMessage);
        }

        // If the list is already focused do not focus it again.
        if (!document.activeElement?.classList.contains('visualizer__references-picker-list')) {
            if (intervalId !== null) clearInterval(intervalId);
            // Try to focus on the list, retry until it works once.
            intervalId = setIntervalCapped(
                () => {
                    const ul = document.getElementById(
                        'visualizer__context-references-button',
                    ) as HTMLUListElement;
                    if (ul) {
                        ul.focus();
                        if (intervalId !== null) clearInterval(intervalId);
                        intervalId = null;
                    }
                },
                50,
                50,
            );
        }
    }, []);

    /**
     * Hide the references list when the user is not hovering the references button.
     */
    const onMouseLeave = React.useCallback(() => {
        const list = document.getElementsByClassName('visualizer__references-picker-menu');
        if (list.length > 0) (list[0] as HTMLElement).style.visibility = 'hidden';
    }, []);

    /**
     * Send a request to reveal the location of the list item being currently hovered.
     */
    const onClick = React.useCallback(
        (event: React.MouseEvent<HTMLLIElement>) => {
            referencesPickerOnClick(event, props.locationsMap, setCanClose, props.onContextClose);
        },
        [props.locationsMap],
    );

    /**
     * Update the current position of the user in the location list and reveal the update position.
     * @param event - The keyboard event.
     */
    const onKeyDown = React.useCallback(
        (event: React.KeyboardEvent) => {
            referencesPickerOnKeyDown(
                event,
                current,
                setCurrent,
                setCanClose,
                props.onContextClose,
                props.locationsMap,
                'visualizer__context-references-button',
            );
        },
        [current, props.locationsMap],
    );

    /**
     * Reveal the definition location of the current symbol in the code.
     */
    const gotoDefinition = React.useCallback(() => {
        vscode.postMessage({
            command: 'revealNode',
            nodeId: props.node.id,
            gotoImplementation: false,
        } as RevealMessage);
    }, []);

    /**
     * Reveal the implementation location of the current symbol in the code.
     */
    const gotoImplementation = React.useCallback(() => {
        vscode.postMessage({
            command: 'revealNode',
            nodeId: props.node.id,
            gotoImplementation: true,
        } as RevealMessage);
    }, []);

    // If no locations have been registered yet create all the list elements.
    if (locations.length === 0 && props.locationsMap.size > 0) {
        referencePickerCreateList(props.locationsMap, locations, onClick, true);
    }

    let left: number | undefined = undefined;
    let right: number | undefined = undefined;
    let bottom: number | undefined = undefined;
    const hierarchy = props.node.data.hierarchy;
    const subArray = ['Sub Types', 'Outgoing Calls', ' imported files', 'dependent GPR file'];
    const superArray = ['Super Types', 'Incoming Calls', ' importing files', 'depending GPR file'];

    const subContent = 'Get ' + subArray[hierarchy];
    const superContent = 'Get ' + superArray[hierarchy];

    const gotoArray = ['Definition', 'Definition', 'File', 'GPR File'];
    const gotoText = 'Go To ' + gotoArray[hierarchy];

    const pickerMenuWidth = 200;
    const pickerMenuHeight = 200;
    // In case the menu is to close from the top or the bottom, add a little space for visibility.
    const padding = 20;
    const contextButton = document.getElementById('visualizer__context-references-button');
    if (contextButton) {
        const rect = contextButton.getBoundingClientRect();

        // A location item is more or less half the size of the references button.
        let menu_height = (rect.height / 2) * locations.length;
        if (menu_height > pickerMenuHeight) menu_height = pickerMenuHeight;
        bottom = rect.height / 2;
        // Handle the case where the menu overflow through the bottom of the window.
        if (rect.bottom + menu_height / 2 > props.pane.height) {
            //Divide by 2 at this end because of the transformY(-50%) in the css.
            bottom += (rect.bottom + menu_height / 2 - props.pane.height) / 2 + padding;
        }
        // The menu can't overflow through the top as its max size is less than the whole
        // context menu.

        // Handle the overflow through the sides.
        if (rect.right + pickerMenuWidth < props.pane.width || rect.left < pickerMenuHeight) {
            left = rect.width;
            right = undefined;
        } else {
            right = rect.width;
            left = undefined;
        }
    }
    return (
        <ReactFlowProvider>
            <div
                style={{
                    left: props.left,
                    top: props.top,
                    right: props.right,
                    bottom: props.bottom,
                }}
                className="visualizer__node-context-menu"
            >
                {hierarchy !== Hierarchy.GPR && (
                    <button className="visualizer__context-button" onClick={refreshNode}>
                        Refresh Node
                    </button>
                )}
                <button className="visualizer__context-button" onClick={gotoDefinition}>
                    {gotoText}
                </button>
                {hierarchy !== Hierarchy.GPR && hierarchy !== Hierarchy.FILE && (
                    <button className="visualizer__context-button" onClick={gotoImplementation}>
                        Goto Implementation
                    </button>
                )}
                <button className="visualizer__context-button" onClick={deleteNode}>
                    Delete Node
                </button>
                <button
                    className="visualizer__context-button"
                    onClick={(event) => requestHierarchy(event, RelationDirection.SUB)}
                >
                    {subContent}
                </button>
                {hierarchy !== Hierarchy.GPR && (
                    <button
                        className="visualizer__context-button"
                        onClick={(event) => requestHierarchy(event, RelationDirection.SUPER)}
                    >
                        {superContent}
                    </button>
                )}
                {hierarchy !== Hierarchy.FILE && hierarchy !== Hierarchy.GPR && (
                    <button
                        className="visualizer__context-button"
                        id="visualizer__context-references-button"
                        onMouseEnter={onMouseEnter}
                        onMouseLeave={onMouseLeave}
                        onKeyDown={onKeyDown}
                    >
                        <span> Go To References </span>{' '}
                        <div className="codicon codicon-chevron-right" />
                        <div
                            className={
                                'visualizer__references-picker-menu' +
                                ' visualizer__references-picker-node-menu'
                            }
                            style={{
                                left: left,
                                right: right,
                                bottom: bottom,
                            }}
                        >
                            <nav>
                                <ul
                                    id="visualizer__references-picker-list"
                                    className="visualizer__scrollbar"
                                    tabIndex={0}
                                >
                                    {locations}
                                </ul>
                            </nav>
                        </div>
                    </button>
                )}
            </div>
        </ReactFlowProvider>
    );
}
