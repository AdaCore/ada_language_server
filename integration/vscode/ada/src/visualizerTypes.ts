import * as vscode from 'vscode';
import { VisualizerHandler } from './alsVisualizerProvider';

/**
 * The base format of all the messages exchanged between the server side and the client side.
 */
export type Message =
    | HierarchyMessage
    | DeleteMessage
    | UpdateMessage
    | RefreshMessage
    | StopProcessMessage
    | SendNextDataMessage
    | IsRenderedMessage
    | RenderedMessage
    | RevealMessage
    | RevealLocationMessage
    | RevealReferencesMessage
    | RevealReferencesResponse
    | NodeEdge;

/**
 * Message sent from the client to the server side to request for new
 * nodes up or down from the hierarchy.
 */
export type HierarchyMessage = {
    command: 'requestHierarchy';
    id: string;
    direction: RelationDirection;
    expand: boolean;
    hierarchy: Hierarchy;
    recursive: boolean;
};

/**
 * Message sent from the client to the server used to indicate which node
 * to delete from the storage.
 * The recursive field is used to apply the delete  function to all children of the node.
 */
export type DeleteMessage = {
    command: 'deleteNodes';
    nodesId: string[];
    recursive: boolean;
};

/**
 * Message sent from the client to the server used to indicate which node
 * to refresh from the storage.
 */
export type RefreshMessage = {
    command: 'refreshNodes';
    nodesId: string[];
};

/**
 * Message sent from the client to the server to stop a process currently running
 * (when the user is recursively discovering nodes for example).
 */
export type StopProcessMessage = {
    command: 'stopProcess';
};

/**
 * Message sent from the server to the client to indicate which node to remove and which
 * node to update.
 */
export type UpdateMessage = {
    command: 'updateNodes';
    toUpdate: NodeData[];
    toDelete: NodeData[];
};

/**
 * Message sent from the client to the server to indicate it can send the next batch of data.
 */
export type SendNextDataMessage = {
    command: 'canSendNextData';
};

/**
 * Message sent from the server to the client to check if it is initialized and
 * the interface is rendered to the user.
 */
export type IsRenderedMessage = {
    command: 'isRendered';
};

/**
 * Message sent from the client to the server as a response when it has finished rendering
 * the interface.
 */
export type RenderedMessage = {
    command: 'rendered';
};
/**
 * Message sent from the client to the server to ask the server to reveal a
 * symbol location in the code.
 */
export type RevealMessage = {
    command: 'revealNode';
    nodeId: string;
    gotoImplementation: boolean;
};

/**
 * Message sent from the client to the server used to reveal a specific symbol location
 * from the graph in the code base.
 */
export type RevealLocationMessage = StringLocation & {
    command: 'revealLocation';
};

/**
 * Represent the location of a symbol in a file.
 */
export type StringLocation = {
    path: string;
    range_start: vscode.Position;
    range_end: vscode.Position;
    string_location: string;
};

/**
 * Message sent from the client to the server to get all the references of
 * targetNode in referenceNode.
 * The server will then respond with a RevealReferenceResponse.
 */
export type RevealReferencesMessage = {
    command: 'revealReferences';
    targetNodeId: string;
    referenceNodeId: string;
    bothDirection: boolean;
};

/** Message sent from the server to the client in response to a RevealReferencesMessage.
 * The server sends a mapping of location names associated with all the references of the required
 * symbols.
 */
export type RevealReferencesResponse = {
    command: 'revealResponse';
    locationsKeys: string[];
    locationsValues: StringLocation[][];
};

/**
 * Request sent to the ALS for the als_show_dependencies request.
 */
export type ALS_ShowDependenciesParams = {
    uri: string /* The queried unit */;
    kind: ALS_ShowDependenciesKind /* The dependencies query kind */;
    showImplicit: boolean /* True if implicit dependencies should be returned */;
};

/**
 * Request sent from the ALS in response to an als_show_dependencies request.
 */
export type ALS_Unit_Description = {
    uri: string /* The dependency unit's file */;
    projectUri: string /* The dependency's project file */;
};

/**
 * Request sent from the ALS in response to an als_gpr_dependencies request.
 */
export type ALS_GprDependencyItem = {
    uri: string;
    kind: ALS_GprDependencyKind;
};

/**
 * Request sent to the ALS for an als_gpr_dependencies custom request.
 */
export type ALS_GprDependencyParam = {
    uri: string;
    direction: ALS_GprDependencyDirection;
};

/**
 * Describe a relation between a parent of this object and the target by specifying the
 * type of edge to display.
 */
export type Relation = {
    target: NodeHierarchy;
    edgeType: EdgeType;
};

/**
 * Data stored in a node client side.
 */
export type NodeData = {
    // The id of the node.
    id: string;
    // The name of the symbol represented by the node.
    label: string;
    // The kind of symbol this node represents.
    kind: string;
    // A boolean indicating if this now is showing his child or not.
    expanded: boolean;
    // A boolean indicating if this node has parents or not (null means not checked yet).
    hasParent: boolean | null;
    // A boolean indicating if this node has children or not (null means not checked yet).
    hasChildren: boolean | null;
    // A boolean indicating if the node must be focused in the graph.
    focus: boolean;
    // Boolean indicating if the symbol is located in the project or in the runtime.
    inProject: boolean;
    // The symbol position in the project as a string.
    string_location: {
        path: string;
        position: string;
    };

    // A temporary position used to allow the node to slide from its parents to its real position.
    newPosition:
        | undefined
        | {
              x: number;
              y: number;
          };

    // Indicate if it is a type hierarchy or a call hierarchy.
    hierarchy: Hierarchy;
};

/**
 * Data stored in a node server side.
 */
export type NodeHierarchy = NodeData & {
    // The symbol location in the project (this structure is not well json formatted so it is stored
    // only on the 'server side').
    location: vscode.Location;
    // The array of parents' relations.
    parents: Relation[];
    // The array of children's relations.
    children: Relation[];
    // The language the symbol is from.
    languageId: string;
    // The object tasked to handle the language-specific  operation (ID Generation, ...).
    handler: VisualizerHandler;
};

/**
 * Contain all the nodes and edges that will be displayed on the viewPort.
 */
export type NodeEdge = {
    command: 'hierarchyResponse';
    nodesData: NodeData[];
    edges: DirectedEdge[];
    focus: boolean;
    mainNodeId: string;
};

/**
 * Represent a directed edge.
 */
export type DirectedEdge = {
    src: string;
    dst: string;
    edgeDirection: RelationDirection;
    edgeType: EdgeType;
};

/**
 * Describe the minimal data necessary to get from an LSP to create a node.
 */
export type VisualizerSymbol = {
    name: string;
    location: vscode.Location;
    kind: vscode.SymbolKind;
};

/**
 * Indicate the direction of the hierarchy call to make.
 */
export enum RelationDirection {
    SUPER,
    SUB,
    BOTH,
}

/**
 * Store the four usual directions.
 */
export enum Direction {
    LEFT = 'LEFT',
    UP = 'UP',
    RIGHT = 'RIGHT',
    DOWN = 'DOWN',
}

/**
 * The type of hierarchy that can be called.
 */
export enum Hierarchy {
    TYPE,
    CALL,
    FILE,
    GPR,
}

/**
 * The type of an edge, which will change its appearance.
 */
export enum EdgeType {
    REGULAR,
    SELF_CONNECTED,
    TEMPORARY,
    DOTTED,
    BOXED,
}

/**
 * The type of the node, which will change its appearance.
 */
export enum NodeType {
    REGULAR,
    GROUP,
}

/**
 * Store the values used for als_show_dependency.
 */
export enum ALS_ShowDependenciesKind {
    SHOW_IMPORTED = 1,
    SHOW_IMPORTING = 2,
}

/**
 * Store the values used for als_gpr_dependency.
 */
export enum ALS_GprDependencyDirection {
    SHOW_OUTGOING = 1,
    SHOW_INCOMING = 2,
}

/**
 * Store the dependency kind two gpr files can have.
 */
export enum ALS_GprDependencyKind {
    AGGREGATED,
    EXTENDED,
    EXTENDING,
    IMPORTED,
}
