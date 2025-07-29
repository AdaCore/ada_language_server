import { Node, Edge, EdgeMarker, Viewport, SetCenterOptions } from '@xyflow/react';
import { Hierarchy } from '../visualizerTypes';

/**
 * Helper function to return the type of Hierarchy needing to be called depending on the kind of
 * the node.
 *
 * As VS Code cannot be imported here, it can't be used to compare the kind parameter.
 *
 * @param kind - the symbol kind of a node.
 * @returns The kind enum value associated with the input string value.
 */
export function getNodeKind(kind: string) {
    if (kind === 'class' || kind === 'object' || kind === 'struct') return Hierarchy.TYPE;
    else if (kind === 'package') return Hierarchy.FILE;
    return Hierarchy.CALL;
}

/**
 * Modify the edge to update its color and re create the marker CSV in the right color.
 *
 * @param edge - The edge to change.
 * @param color - The color to apply on the marker.
 * @param additionalClass - An additional class that will be added to the edge on render.
 * @param select - A boolean indicating if the edge will be unselected at re-rendering,
 * if null, no change
 * @returns The modified edge.
 */
export function changeEdge(
    edge: Edge,
    color: string,
    additionalClass: string | undefined,
    select: boolean | null = false,
) {
    edge.data = { ...edge.data, additionalClass: additionalClass };

    if (edge.selected) {
        if (select === false) edge.selected = false;
        else return edge;
    } else {
        if (select) edge.selected = true;
    }
    // Recreate the marker to generate the right SVG
    // (a marker can't be modified in place)
    if (edge.markerEnd && 'width' in (edge.markerEnd as EdgeMarker)) {
        edge.markerEnd = {
            ...(edge.markerEnd as EdgeMarker),
            color: color,
        };
        if (color.length === 0) delete edge.markerEnd.color;
    }
    if (edge.markerStart && 'width' in (edge.markerStart as EdgeMarker)) {
        edge.markerStart = {
            ...(edge.markerStart as EdgeMarker),
            color: color,
        };
        if (color.length === 0) delete edge.markerStart.color;
    }
    return { ...edge };
}

/**
 *  Display or remove a waiting bar to indicate to the user that the extension is processing data.
 *
 * @param stop - True to remove the bar, False to enable it.
 */
export function waitingBar(stop: boolean = false) {
    const panes = document.getElementsByClassName('react-flow__pane');
    if (panes.length === 0) return;
    const pane = panes[0];
    if (!stop) {
        const element = document.createElement('div');
        const childArr = Array.from(pane.children);

        // There can only be a single instance of the waiting bar.
        if (!childArr.some((child) => child.classList.contains('visualizer__wait'))) {
            element.classList.add('visualizer__wait');
            pane.appendChild(element);
        }
    } else {
        const load = document.getElementsByClassName('visualizer__wait');
        if (load.length !== 0) {
            pane.removeChild(load[0]);
        }
    }
}

/**
 * Focus the graph on a specific node and animate the camera movement toward this node.
 *
 * @param node - The node to focus on.
 * @param viewPort - The viewPort containing the node.
 * @param baseSpeed - The base speed at which to go toward the node.
 * @param minDuration - The minimal duration of the displacement animation.
 * @param maxDuration  - The maximal duration of the displacement animation.
 * @param setCenter - Function that will center the viewPort on the node.
 */
export function focusNode(
    node: Node,
    viewPort: Viewport,
    setCenter: (x: number, y: number, options?: SetCenterOptions) => Promise<boolean>,
    baseSpeed = 1,
    minDuration = 750,
    maxDuration = 3000,
) {
    const newX = node.position.x + (node.width ?? 0) / 2;
    const newY = node.position.y + (node.height ?? 0) / 2;
    const { x, y, zoom } = viewPort;
    const regX = (-x + window.innerWidth / 2) / zoom;
    const regY = (-y + window.innerHeight / 2) / zoom;

    const distance = Math.sqrt(Math.pow(regX - newX, 2) + Math.pow(regY - newY, 2));
    const duration = Math.max(Math.min(distance * baseSpeed, maxDuration), minDuration);
    void setCenter(newX, newY, { duration: duration, zoom: zoom });
}

/**
 * Wrapper around setInterval to make sure the loop ends at one point.
 *
 * @param callback - The function to call when the timer elapses.
 * @param delay - The number of milliseconds to wait before calling the callback.
 * @param repetitions - The max number of times the callback can be called.
 * @returns The id  of the interval loop.
 */
export function setIntervalCapped(callback: () => void, delay: number, repetitions: number) {
    let rep = 0;
    const intervalId = setInterval(() => {
        callback();
        if (rep === repetitions) clearInterval(intervalId);
        rep++;
    }, delay);
    return intervalId;
}
