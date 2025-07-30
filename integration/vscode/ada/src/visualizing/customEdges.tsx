import {
    getBezierPath,
    Position,
    useInternalNode,
    XYPosition,
    InternalNode,
    MarkerType,
    Edge,
    BaseEdge,
    useReactFlow,
} from '@xyflow/react';

import React from 'react';
import { Direction, EdgeType, RelationDirection } from '../visualizerTypes';
import { currentDirection } from './App';

export const edgeTypes = {
    floating: floatingEdge,
    selfConnection: selfConnection,
};
const edgeString = ['floating', 'selfConnection', 'temporary'];

const markerHeight = 25;
const markerWidth = 25;
const edgeStrokeWidth = 4;

/**
 * Return a new react flow edge.
 * @param src - Source node of the edge.
 * @param dst - Destination node of the edge.
 * @param edgeDirection - The direction of the edge (from src to dst, dst to src or both way).
 * @param edgeType - The type of edge, that will change how it will be represented on the graph.
 * @returns A new react flow Edge.
 */
export function edgeFactory(
    src: string,
    dst: string,
    edgeDirection: RelationDirection,
    edgeType: EdgeType,
) {
    // If the Type is DOTTED or BOXED, keep it as regular and the change will be applied on
    // top of it.
    const index = edgeType > EdgeType.TEMPORARY ? 0 : edgeType;
    return {
        id: 'e' + src + '-' + dst,
        source: src,
        target: dst,
        type: edgeString[index],
        markerEnd:
            edgeDirection === RelationDirection.BOTH || edgeDirection === RelationDirection.SUB
                ? { height: markerHeight, width: markerWidth, type: MarkerType.Arrow }
                : undefined,
        markerStart:
            edgeDirection === RelationDirection.BOTH || edgeDirection === RelationDirection.SUPER
                ? { height: markerHeight, width: markerWidth, type: MarkerType.Arrow }
                : undefined,
        style: {
            strokeWidth: edgeStrokeWidth,
            strokeDasharray: edgeType === EdgeType.DOTTED ? '10,10' : 'none',
        },
        data: {
            additionalClass: undefined,
            edgeDirection: edgeDirection,
        },
    } as Edge;
}

/**
 * Get the intersection point between the edge (center intersectionNode -\> targetNode)
 * and the outer border of the intersection Node.
 * Used to determine where to place the beginning of the edge for a better visual.
 *
 * @param intersectionNode - The source node of the edge for which we search the intersection.
 * @param targetNode - The target node of the edge.
 * @returns The XY coordinates of the intersection point between the edge and the outer edge of
 * the node.
 */
function getNodeIntersection(intersectionNode: InternalNode, targetNode: InternalNode) {
    const measure = intersectionNode.measured;
    const intersectionNodePosition = intersectionNode.internals.positionAbsolute;
    const targetPosition = targetNode.internals.positionAbsolute;

    // Immediately return if one of the elements is undefined.
    if (
        measure?.width === undefined ||
        measure.height === undefined ||
        targetNode.measured.width === undefined ||
        targetNode.measured.height === undefined
    )
        return undefined;

    /**
     * The algorithm is more precisely explained here
     * https://math.stackexchange.com/questions/1724792/an-algorithm-for-finding-the-intersection-point-between-a-center-of-vision-and-a
     */
    const w = measure.width / 2;
    const h = measure.height / 2;

    const x2 = intersectionNodePosition.x + w;
    const y2 = intersectionNodePosition.y + h;
    const x1 = targetPosition.x + targetNode.measured.width / 2;
    const y1 = targetPosition.y + targetNode.measured.height / 2;

    const xx1 = (x1 - x2) / (2 * w) - (y1 - y2) / (2 * h);
    const yy1 = (x1 - x2) / (2 * w) + (y1 - y2) / (2 * h);
    const a = 1 / (Math.abs(xx1) + Math.abs(yy1));
    const xx3 = a * xx1;
    const yy3 = a * yy1;
    const x = w * (xx3 + yy3) + x2;
    const y = h * (-xx3 + yy3) + y2;

    return { x, y };
}

/**
 * Returns the position of the intersectionPoint compared to the node.
 *
 * @param node - The source node of the edge.
 * @param intersectionPoint - The XY coordinate of the intersection between
 * the node and the outer border of the node.
 * @returns The Position of the intersection point.
 */
function getEdgePosition(node: InternalNode, intersectionPoint: XYPosition) {
    const n = { ...node.internals.positionAbsolute, ...node };
    if (n.measured.width === undefined || n.measured.height === undefined) return;

    const nx = Math.round(n.x);
    const ny = Math.round(n.y);
    const px = Math.round(intersectionPoint.x);
    const py = Math.round(intersectionPoint.y);

    if (px <= nx + 1) {
        return Position.Left;
    }
    if (px >= nx + n.measured.width - 1) {
        return Position.Right;
    }
    if (py <= ny + 1) {
        return Position.Top;
    }
    if (py >= n.y + n.measured.height - 1) {
        return Position.Bottom;
    }

    return Position.Top;
}

/**
 * Returns the parameters necessary to create an edge
 *
 * @param source - The source node of the edge.
 * @param target - The target node of the edge.
 * @returns The intersections points and position of the source and target node.
 */
function getEdgeParams(source: InternalNode, target: InternalNode) {
    const sourceIntersectionPoint: XYPosition | undefined = getNodeIntersection(source, target);
    const targetIntersectionPoint: XYPosition | undefined = getNodeIntersection(target, source);

    if (sourceIntersectionPoint === undefined || targetIntersectionPoint === undefined) return;

    const sourcePos = getEdgePosition(source, sourceIntersectionPoint);
    const targetPos = getEdgePosition(target, targetIntersectionPoint);

    return {
        sx: sourceIntersectionPoint.x,
        sy: sourceIntersectionPoint.y,
        tx: targetIntersectionPoint.x,
        ty: targetIntersectionPoint.y,
        sourcePos,
        targetPos,
    };
}

type EdgeProps = {
    id: string;
    source: string;
    target: string;
    style?: React.CSSProperties;
    markerEnd?: string;
    markerStart?: string;
    sourceX: number;
    sourceY: number;
    targetX: number;
    targetY: number;
    data: {
        additionalClass?: string;
    };
};

/**
 * Custom edge function, calculate and create the position of the edge between two nodes.
 *
 * @param floatingEdge - The data necessary to create the edge and style it.
 * @returns A react JSX element representing the edge.
 */
export function floatingEdge(floatingEdge: EdgeProps) {
    const sourceNode = useInternalNode(floatingEdge.source);
    const targetNode = useInternalNode(floatingEdge.target);

    // The multiplier to increase the zone of hover of the edge.
    // 2 is not 100% increase but more like 25%.
    const strokeMultiplier = 6;

    // If an internal node is not defined just return an empty path.
    if (!sourceNode || !targetNode) {
        return <path />;
    }

    const { sx, sy, tx, ty, sourcePos, targetPos } = getEdgeParams(sourceNode, targetNode) ?? {};

    // If an edge param is not defined, just return an empty path.
    if (sx === undefined || sy === undefined || tx === undefined || ty === undefined) {
        return <path />;
    }

    const [edgePath] = getBezierPath({
        sourceX: sx,
        sourceY: sy,
        sourcePosition: sourcePos,
        targetPosition: targetPos,
        targetX: tx,
        targetY: ty,
    });

    let pathClass = 'react-flow__edge-path';
    if (floatingEdge.data.additionalClass !== undefined)
        pathClass += ' ' + floatingEdge.data.additionalClass;

    // If bezier path returned nan returns empty path
    if (edgePath.includes('NaN')) return <path />;
    return (
        <>
            <path
                id={floatingEdge.id}
                className={pathClass}
                d={edgePath}
                style={floatingEdge.style}
                markerStart={floatingEdge.markerStart}
                markerEnd={floatingEdge.markerEnd}
            />
            {/* The second path is invisible and serves to make the hovering area larger
            to help the user when selecting an edge.*/}
            <path
                d={edgePath}
                style={{
                    stroke: 'transparent',
                    fill: 'none',

                    strokeWidth: edgeStrokeWidth * strokeMultiplier,
                }}
            />
        </>
    );
}

/**
 * Represent the data necessary to create an edge.
 */
type FloatingConnectionLine = {
    toX: number;
    toY: number;
    fromPosition: Position;
    toPosition: Position;
    fromNode: InternalNode;
};

/**
 * Create the connection line used by the edges to link two nodes together.
 *
 * @param floatingConnectionLine - The data necessary to create the connection line.
 * @returns A React JSX component that will be used as a connection line between nodes.
 */
export function floatingConnectionLine(floatingConnectionLine: FloatingConnectionLine) {
    if (!floatingConnectionLine.fromNode) {
        return null;
    }

    const [edgePath] = getBezierPath({
        sourceX: floatingConnectionLine.fromNode.position.x,
        sourceY: floatingConnectionLine.fromNode.position.y,
        sourcePosition: floatingConnectionLine.fromPosition,
        targetPosition: floatingConnectionLine.toPosition,
        targetX: floatingConnectionLine.toX,
        targetY: floatingConnectionLine.toY,
    });

    return (
        <g>
            <path fill="none" stroke="#222" strokeWidth={1.5} d={edgePath} />
            <circle
                cx={floatingConnectionLine.toX}
                cy={floatingConnectionLine.toY}
                fill="#fff"
                r={3}
                stroke="#222"
                strokeWidth={1.5}
            />
        </g>
    );
}

/**
 * Create an edge to represent recursive nodes that call themselves.
 *
 * @param props - The data necessary to create the edge.
 * @returns An edge that connects a node with itself.
 */
export function selfConnection(props: EdgeProps) {
    if (props.source !== props.target) return floatingEdge(props);

    const { sourceX, sourceY, targetX, targetY, markerEnd } = props;

    const { getNode } = useReactFlow();
    const node = getNode(props.source);
    const radiusX = currentDirection === Direction.RIGHT ? node?.width : (node?.width ?? 0) / 2;
    const radiusY = currentDirection === Direction.RIGHT ? (node?.height ?? 0) / 2 : node?.height;
    const edgePath =
        `M ${sourceX} ${sourceY} A ${radiusX} ${radiusY} 0 1 0` + ` ${targetX} ${targetY}`;

    let pathClass = 'react-flow__edge-path';
    if (props.data.additionalClass !== undefined) pathClass += ' ' + props.data.additionalClass;

    return (
        <BaseEdge path={edgePath} markerEnd={markerEnd} className={pathClass} style={props.style} />
    );
}
