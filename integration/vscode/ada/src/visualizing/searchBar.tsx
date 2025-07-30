import { useReactFlow, ReactFlowProvider } from '@xyflow/react';
import React from 'react';
import { NodeData } from '../visualizerTypes';
import { focusNode } from './utils';

let current: number;
let setCurrent: React.Dispatch<React.SetStateAction<number>>;
let filteredNodes = [];
let setFilteredNodes: React.Dispatch<React.SetStateAction<React.JSX.Element[]>>;

/**
 * Reset the state of the search bar.
 */
export const closeSearchBar = (): void => {
    setFilteredNodes([]);
    setCurrent(-1);
    const searchBar = document.getElementById('visualizer__node-search-bar');
    if (!searchBar) return;
    (searchBar as HTMLInputElement).value = '';
};

/**
 *  Create a search bar allowing to focus on a specific node of the graph.
 *
 * @returns A div containing the search bar itself and the list that will contain the child results
 */
export function SearchBar() {
    const { getViewport, getNodes, getNode, setCenter } = useReactFlow();
    [current, setCurrent] = React.useState(-1);
    [filteredNodes, setFilteredNodes] = React.useState<React.JSX.Element[]>([]);

    /**
     * Display a dropdown list of nodes that match the request inputted in the search bar.
     *
     * @param event - The change event
     */
    const onChange = React.useCallback(
        (event: React.ChangeEvent<HTMLInputElement>) => {
            const nodes = getNodes();
            const search = (event.target as HTMLInputElement).value.toLowerCase();
            if (search.length === 0) {
                setFilteredNodes([]);
                return;
            }

            const searchResults: React.JSX.Element[] = [];
            for (const node of nodes) {
                const data = node.data as NodeData;
                if (data.label.toLowerCase().indexOf(search) > -1) {
                    searchResults.push(
                        <li
                            data-item-id={node.id}
                            onClick={onListClick}
                            className="visualizer__node-search-item"
                            title={(node.data as NodeData).label}
                            key={node.id}
                        >
                            {(node.data as NodeData).label}
                        </li>,
                    );
                }
            }
            setFilteredNodes(searchResults);
        },
        [filteredNodes],
    );

    /**
     * Handle the key press to navigate the list and handle the node focus.
     *
     * @param event - The keyboard event.
     */
    const onKeyDown = React.useCallback(
        (event: React.KeyboardEvent<HTMLInputElement>) => {
            const ul = document.getElementById('visualizer__node-search-list') as HTMLUListElement;
            const childs = ul.children;
            if (childs.length === 0) return;
            if (current > -1) childs[current].classList.remove('visualizer__node-search-selected');
            let newCurrent = current;
            // Go to the previous element in the list (go to the last element in case of underflow).
            if (event.key === 'ArrowUp') {
                event.preventDefault();
                newCurrent = current - 1 >= 0 ? current - 1 : ul.childElementCount - 1;
            }
            // Go to the next element in the list (go to the first element in case of overflow).
            else if (event.key === 'ArrowDown' || event.key === 'Tab') {
                event.preventDefault();
                newCurrent = (current + 1) % ul.childElementCount;
                // Close the search bar when the user presses escape.
            } else if (event.key === 'Escape') {
                closeSearchBar();
                return;
            }
            // Focus the current node on the graph.
            else if (event.key === 'Enter' && current !== -1) {
                event.preventDefault();
            }
            // If anything else is typed reset the list.
            else {
                newCurrent = -1;
            }

            setCurrent(newCurrent);
            if (newCurrent === -1) return;

            // Focus on the current choice
            const nodeId = childs[newCurrent].getAttribute('data-item-id');
            if (!nodeId) return;
            const node = getNode(nodeId);
            if (node) focusNode(node, getViewport(), setCenter);

            //Add the class to the current selected option and scroll the list to make sure
            // the element is in view
            childs[newCurrent].classList.add('visualizer__node-search-selected');
            childs[newCurrent].scrollIntoView({ behavior: 'auto', block: 'nearest' });
        },
        [filteredNodes, current],
    );

    // Handle the case where the user mouse when is on another window.
    React.useEffect(() => {
        window.addEventListener('blur', closeSearchBar);

        return () => {
            window.removeEventListener('blur', closeSearchBar);
        };
    }, []);

    /**
     * On list item click, focus on the node represented by this item.
     *
     * @param event - The mouse event.
     */
    const onListClick = React.useCallback((event: React.MouseEvent<HTMLLIElement>) => {
        event.preventDefault();
        const nodeId = (event.target as HTMLLIElement).getAttribute('data-item-id');
        if (!nodeId) return;
        const node = getNode(nodeId);
        if (node) focusNode(node, getViewport(), setCenter);
    }, []);

    return (
        <ReactFlowProvider>
            <div className="visualizer__node-search">
                <input
                    type="search"
                    inputMode="search"
                    autoComplete="off"
                    placeholder="Search symbol name"
                    id="visualizer__node-search-bar"
                    onChange={onChange}
                    onKeyDown={onKeyDown}
                />
                <nav>
                    <ul className="visualizer__scrollbar" id="visualizer__node-search-list">
                        {filteredNodes}
                    </ul>
                </nav>
            </div>
        </ReactFlowProvider>
    );
}
