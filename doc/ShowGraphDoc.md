<!-- markdownlint-disable MD030 -->

# Code Visualizer for Visual Studio Code

This extension provide a tool allowing to display the code base as an interactive graph using LSP requests.

## Features

This extension relies on LSP to gather data for the graphs which means most of the extension should work on any language as long as there is a Language Server implemented for it and linked to VS Code.

/!\ All Languages Servers differs in their implementation of the Language Server Protocol which means that some features might not work optimally for all languages, the implementation being made to work on _most language_.

To counter this issue, it is possible to add [language-specific features](#multi-language-handler).

### Graph

The main feature of this extension is the graph.

There are several kinds of graphs available:

#### Call Graph

The most basic kind of graph is the Call Graph, which allows the user to visualize the relationship between the different function calls of the project.

To use it, you can simply right-click on the function symbol you wish to visualize and select the option `Show Call Hierarchy (graph)`.

This will open a new tab on the side of your IDE with a view containing the created graph. You can then interact with it, move the nodes around and use the different features listed below.

#### Type Graph

You can also generate Type Graph which allow the user to visualize the hierarchies between the different types of your project.

To use it, the process is the same as with the Call Graph: right-click on the type symbol you wish to visualize and select the option `Show Type Hierarchy (graph)`.

#### File Dependency Graph (Ada)

Some graph types can also be available to specific languages. For example, when on an Ada file, you can right-click anywhere in the editor and then on `Show File Dependency (graph)` to generate a graph representing the different files of your project and their relationship between them.

#### GPR Dependency Graph (GPR)

The last kind of graph available allows you to see the relationships between the different GPR files composing a project. Again, to use it, you can click anywhere in a GPR file and then click on `Show GPR Dependency (graph)`.

The graph generated will be composed of different kinds of nodes and edges:
For imported projects, the edges will be dotted.

For extended projects, the edges will be plain.

For aggregated projects, the node representing the aggregate project will be transformed into a sub-graph containing its aggregated project to better understand their relationship.

### ViewPort

The nodes are placed on a viewport that the user can interact with.

It is possible to move around by dragging the viewport, zoom in and out by using the scroll wheel.

On the bottom left corner of the viewport, the user has access to a set of buttons that summarize its features.

-   A button to zoom in.
-   A button to zoom out.
-   A button to fit the whole graph into the view.
-   A button to lock the graph interactivity, preventing the user from moving the nodes around.
-   A button that takes the user back to the center of the graph (0,0) in case he went too far and can't find the graph on the port.
-   Additionally, a button can be found allowing the user to layout the graph, toggling between orienting the graph to the right or downside.

### Expansion

To prevent lag or slowness, the graph initially only displays the required symbols and their direct relations, but you can then interact with the buttons on the side of each node to either get the parent symbols or the child symbols for a specific node.

If you want to discover a large part of the graph, you can hold the _Control Key_ to recursively unravel the graph and see the graph expand in real time.
This process can be stopped at any moment by clicking the cancel notification at the bottom right of the window.

### Folding

Once a node has children, the button to get those children is replaced by a button used to fold the children in the node, hiding them from the user view.

When folded, the user can re-click on the button to redisplay the nodes stored in the parent node.

### Layouting

When nodes are added to the graph, it is automatically layouted to make sure each node is visible to the user and well placed.
If the graph is disconnected, the program will try to re-layout only the sub-part that was modified.
In either case, when adding nodes, the overall order of the nodes in the graph may be drastically reordered, but the view will be focused on the node from which the other nodes were added to help the user keep track of its position in the graph.

You can also manually trigger the layout by clicking the `Layout Graph` button on the bottom left side, which will also change the direction of the graph on screen (toggling between top-bottom to left-right).

### Search Bar

As the graph grows bigger, the user might find it hard to locate himself in the graph or to find specific nodes.
To help with that, he has access to a search bar on the top right corner allowing him to find nodes based on their name and to be taken to them.

Once some characters have been entered, a list of node names will appear.
The user can then either click on a list item or use the `arrow` key or the `tab` key to go through the list.
When going through the list, the graph will focus on the node linked to the current list item.
When satisfied, the user can either press the `Enter` key or click outside the search bar range to close it.

### Node Context Menu

When right clicking on a node, you can get access to a menu summarizing all the requests that can be made from a node.
You can find the following features :

-   `Refresh node`: Refresh the information of a node, [see Node Refresh](#node-refresh)
-   `Go To Definition`: Take the user to the definition of the symbol represented by the node, [see Go To Definition](#go-to-definition)
-   `Go To Implementation`: Take the user to the implementation of the symbol represented by the node, [see Go To Implementation](#go-to-implementation)
-   `Delete Node`: Delete the node associated with the context menu, [see Node Deletion](#node-deletion)
-   `Get Super Hierarchy`: The label of this button changes on the hierarchy of the graph and allows the user to request the parents of the node to be added to the graph, [see Expansion](#expansion)
-   `Get Sub Hierarchy`: The label of this button changes on the hierarchy of the graph and allows the user to request the children of the node to be added to the graph, [see Expansion](#expansion)
-   `Go To References`: Lists the references of the symbol in the code base and then takes the user to the chosen reference of the symbol in the code base, [see Go To Implementation](#go-to-implementation)

### Node Selection

The user can select nodes by either clicking on one, holding the `control` key and clicking on multiple nodes for multiple selections or holding the `shift` key and then dragging the mouse to select an area of the graph.

### Node Deletion

The user also has the option to delete nodes from the graph by selecting nodes and then pressing the `Backspace` key or the `Del` key.
This will remove the nodes from the graph and from the extension internal storage, along with all the edges connected to them.

The user can also hold the `Control` key while deleting nodes to also delete all of their children recursively.
In case of a cycle, only the node not connected to any parent of a deleted node will be deleted.
For example in a configuration `A -> B -> C -> A`, recursively deleting node B will only remove B as C is a parent of A which is a parent of B.

### Node Refresh

When modifying the code base while the graph is open, the location of some symbol might change, invalidating the node as LSP requests depends on location.

To fix this issue the user can refresh the node by accessing it from the context menu under the label `Refresh Node`.

/!\ The tool was mainly developed as a way to visualize a code base, understand how different parts of the code interact with each other,...
It was not thought as a way to assist during active development so modifying the code base while the graph is open might invalidate a lot of nodes causing a part of the graph to not respond to requests anymore.

### Jump To Code

When using the graph the user might want to see to which part of the code the node is related.

To help with that, the application provides a few ways to access the code from the graph.

#### Go To Definition

First, by `double clicking` (or through the context menu), the user will be taken to the definition of the symbol in the code base.
If the editor is already open, it will only be focused on the right position.
If the editor was not open, it will first be opened in a column next to the graph view.

#### Go To Implementation

In the same vein, the user can also jump to the implementation of a symbol in a code base by holding the `control` key while double-clicking on a node (or, once again, through the context menu).

#### Go To References

Finally, the user can also jump to the references of a symbol in the code base.

This can be achieved through two ways with a slight difference:

-   First, the user can hover or click on an edge to open a popup containing a list representing all the references of the symbol at the destination of the edge in the context of the symbol at the source of the edge.
    If we take the Call Graph as an example (which is the most interesting), hovering an edge linking `Bar` to `Foo` would create a list of all the function calls of `Foo` in the `Bar` function.
-   The user can also use the context menu from a node to get a list of all the references of the symbol in the code base, divided by a header indicating the name of the function and the file they can be found in.

In both cases, the user can move using either the `arrows` key or the `Tab` key and then press `Enter` to be taken to the references. It is also possible to click on a list item.
When going through the list, the user will see in a column on the side of the graph view, the code symbol represented by the list item revealed in real time, giving him context about what the symbol refers to.

This feature is only available for the Type Graph and the Call Graph as GPR and Files Graph don't really have references.

### Out of Project Node

When requesting data from the LSP, you can sometimes get symbols not directly present in the code base but from the diverse standard, shared, static libraries of your project.

Those symbols, when detected, are still placed on the graph but in a different color from the regular node and have the border dotted to indicate to the user that it is not code present in the project.

/!\ The symbols that do not belong to the project might not have all the information necessary for the node to work correctly, so some features might not work as the extension might not be able to build the LSP request or the LSP might not be able to respond.

### Multi Language Handler

The goal of the extension was to provide a tool that would work on any language if they implemented a Language Server.

Unfortunately, each Language Server has its own implementation, which might differ from the others.
Furthermore, each language has specificities that are not covered by the standard request of the LSP.

To handle those problems, this extension provides a _generic interface_ containing some feature implementations that should work for most languages.
Those implementations can then be overloaded in sub-classes, allowing to handle behavior differently depending on the language the user is currently using.

This also allows implementing specific features for a specific language.
For example, the _generic interface_ provides the Call Graph and the Type Graph, but when overloaded with an Ada class, it will additionally provide the File Dependency Graph.
