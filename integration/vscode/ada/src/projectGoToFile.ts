/**
 * Implementation of the 'Go to File in Project' command: a quick-pick listing
 * the source files of the loaded GPR project, restricted to the files that GPR
 * actually reports as belonging to the project tree.
 *
 * The file list is not computed here: it comes from the project view
 * information already fetched from the ALS by ExtensionState.refreshProjectView.
 */

import * as vscode from 'vscode';
import { adaExtState } from './extension';
import { ProjectEntry, ProjectViewInformation, normalizeFsPath } from './projectViewProvider';

/**
 * A quick-pick item standing for one source file of the project.
 */
export interface ProjectFileQuickPickItem extends vscode.QuickPickItem {
    /** The URI of the source file this item refers to */
    uri: vscode.Uri;
}

/**
 * Builds the quick-pick items for all source files of the given project view
 * information.
 *
 * A given file can legitimately be reported by several projects of the tree
 * (shared source directories, diamond imports, extended projects), so files are
 * deduplicated on their normalized path. The root project is visited first, so
 * that a file shared with a sub-project is attributed to the root project.
 *
 * This function performs no UI interaction, which makes it directly testable.
 *
 * @param info - the project view information to list the sources of
 * @param includeRuntime - whether to also list the runtime sources
 * @returns the quick-pick items, sorted by file name then by directory
 */
export function buildProjectFileItems(
    info: ProjectViewInformation,
    includeRuntime: boolean,
): ProjectFileQuickPickItem[] {
    const items: ProjectFileQuickPickItem[] = [];
    const seen = new Set<string>();

    const addEntry = (entry: ProjectEntry) => {
        for (const source of entry.sources) {
            const key = normalizeFsPath(source.file_name);
            if (seen.has(key)) {
                continue;
            }
            seen.add(key);

            items.push({
                label: source.simple_name,
                description: entry.project.name,
                detail: vscode.workspace.asRelativePath(vscode.Uri.file(source.directory), false),
                uri: vscode.Uri.file(source.file_name),
            });
        }
    };

    //  Visit the root project first so that shared files are attributed to it
    const rootEntry = info.projects.get(info.root_project_id);
    if (rootEntry) {
        addEntry(rootEntry);
    }

    for (const entry of info.projects.values()) {
        if (entry !== rootEntry) {
            addEntry(entry);
        }
    }

    if (includeRuntime && info.runtime_project) {
        addEntry(info.runtime_project);
    }

    items.sort(
        (left, right) =>
            left.label.localeCompare(right.label) ||
            (left.detail ?? '').localeCompare(right.detail ?? ''),
    );

    return items;
}

/**
 * Shows a quick-pick listing the source files of the loaded GPR project and
 * opens the selected one in an editor.
 */
export async function goToFileInProject(): Promise<void> {
    let info = adaExtState.getProjectViewInfo();

    if (!info) {
        //  The project view information is normally fetched at activation and
        //  refreshed on project reload. Try once more in case the command is
        //  invoked before the first refresh completed.
        await adaExtState.refreshProjectView();
        info = adaExtState.getProjectViewInfo();
    }

    if (!info) {
        void vscode.window.showInformationMessage(
            'No GPR project is currently loaded: cannot list the project source files.',
        );
        return;
    }

    //  Follow the Project View setting so that both show the same set of files
    const includeRuntime = vscode.workspace
        .getConfiguration('ada')
        .get<boolean>('projectView.showRuntimeFiles', false);

    const items = buildProjectFileItems(info, includeRuntime);

    if (items.length === 0) {
        void vscode.window.showInformationMessage(
            'The loaded GPR project does not contain any source file.',
        );
        return;
    }

    const revealButton: vscode.QuickInputButton = {
        iconPath: new vscode.ThemeIcon('list-tree'),
        tooltip: 'Reveal in Project View',
    };

    const qp = vscode.window.createQuickPick<ProjectFileQuickPickItem>();
    qp.title = 'Go to File in Project';
    qp.placeholder = 'Search project source files by name';
    //  Allow narrowing on the owning project and on the directory as well
    qp.matchOnDescription = true;
    qp.matchOnDetail = true;
    qp.items = items.map((item) => ({ ...item, buttons: [revealButton] }));

    //  Array for event handlers to be disposed after the quick picker is disposed
    const disposables: vscode.Disposable[] = [];
    try {
        const choice: ProjectFileQuickPickItem | undefined = await new Promise((resolve) => {
            disposables.push(
                qp.onDidChangeSelection((selection) => {
                    //  When the User selects an option, resolve the Promise
                    //  and hide the quick picker
                    const item = selection[0];
                    if (item) {
                        resolve(item);
                        qp.hide();
                    }
                }),
                qp.onDidHide(() => {
                    resolve(undefined);
                }),
                qp.onDidTriggerItemButton(async (event) => {
                    //  There's only one button, so selecting it can only mean
                    //  revealing the file in the Project View
                    resolve(undefined);
                    qp.hide();
                    await adaExtState.revealUriInProjectView(event.item.uri);
                }),
            );

            qp.show();
        });

        if (choice) {
            await vscode.commands.executeCommand('vscode.open', choice.uri);
        }
    } finally {
        qp.dispose();
        disposables.forEach((d) => {
            d.dispose();
        });
    }
}
