/*----------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2018-2026, AdaCore                     --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
----------------------------------------------------------------------------*/

import * as path from 'path';
import * as vscode from 'vscode';
import { ProjectViewItem, ProjectViewItemKind, ProjectViewNode } from './projectViewTree';

/**
 * MIME type used for drag-and-drop within the Project View tree.
 * VS Code requires the view id (lowercased) as the suffix.
 */
export const PROJECT_VIEW_MIME_TYPE = 'application/vnd.code.tree.projectview';

/**
 * Implements drag-and-drop for the Project View, allowing source files to be
 * moved from one source directory to another by dragging and dropping.
 *
 * Only `SOURCE_FILE` items may be dragged, and they may only be dropped onto
 * `SOURCE_DIRECTORY` items.  Dropping a file onto its own parent directory is
 * a no-op.  If a file with the same name already exists in the target
 * directory the user is asked to confirm before overwriting.
 *
 * After one or more files are successfully moved, `onFilesMoved` is called so
 * the caller can trigger a project reload and view refresh. If a rename fails
 * the loop continues so that already-moved files are still reflected in the view.
 */
export class ProjectViewDragAndDropController
    implements vscode.TreeDragAndDropController<ProjectViewNode>
{
    readonly dragMimeTypes = [PROJECT_VIEW_MIME_TYPE];
    readonly dropMimeTypes = [PROJECT_VIEW_MIME_TYPE];

    /**
     * @param onFilesMoved - Async callback invoked after one or more files have
     *   been successfully moved. Typically triggers a project reload.
     *   Called even if some files in the batch failed to move.
     */
    constructor(
        private readonly onFilesMoved: () => Promise<void>,
        private readonly isEnabled: () => boolean = () => true,
    ) {}

    handleDrag(source: readonly ProjectViewNode[], dataTransfer: vscode.DataTransfer): void {
        if (!this.isEnabled()) return;
        const sourceFiles = source.filter(
            (item): item is ProjectViewItem => item.itemKind === ProjectViewItemKind.SOURCE_FILE,
        );
        if (sourceFiles.length === 0) return;
        dataTransfer.set(PROJECT_VIEW_MIME_TYPE, new vscode.DataTransferItem(sourceFiles));
    }

    async handleDrop(
        target: ProjectViewNode | undefined,
        dataTransfer: vscode.DataTransfer,
    ): Promise<void> {
        if (!this.isEnabled()) return;
        if (target?.itemKind !== ProjectViewItemKind.SOURCE_DIRECTORY) return;

        const transferItem = dataTransfer.get(PROJECT_VIEW_MIME_TYPE);
        if (!transferItem) return;

        const droppedItems = transferItem.value as ProjectViewItem[];
        const targetDir = target.uri;
        const targetDirName = path.basename(targetDir.fsPath);

        const filesToMove = droppedItems.filter(
            (item) =>
                item.itemKind === ProjectViewItemKind.SOURCE_FILE &&
                path.dirname(item.uri.fsPath) !== targetDir.fsPath,
        );
        if (filesToMove.length === 0) return;

        // Ask for confirmation before performing any move.
        const confirmMsg =
            filesToMove.length === 1
                ? `Move '${path.basename(filesToMove[0].uri.fsPath)}' to '${targetDirName}'?`
                : `Move ${filesToMove.length} files to '${targetDirName}'?`;
        const confirmed = await vscode.window.showWarningMessage(
            confirmMsg,
            { modal: true },
            'Move',
        );
        if (confirmed !== 'Move') return;

        let anyMoved = false;

        for (const fileItem of filesToMove) {
            const srcUri = fileItem.uri;
            const fileName = path.basename(srcUri.fsPath);
            const destUri = vscode.Uri.joinPath(targetDir, fileName);

            // Check for a name collision in the target directory.
            let destExists = false;
            try {
                await vscode.workspace.fs.stat(destUri);
                destExists = true;
            } catch {
                // File does not exist at destination – proceed normally.
            }

            if (destExists) {
                const choice = await vscode.window.showWarningMessage(
                    `A file or folder with the name '${fileName}' already exists in the` +
                        ` destination folder. Do you want to replace it?`,
                    { modal: true },
                    'Replace',
                );
                if (choice !== 'Replace') continue;
            }

            try {
                // Use applyEdit so that any open editor for the source URI is
                // automatically redirected to the new location.
                const edit = new vscode.WorkspaceEdit();
                edit.renameFile(srcUri, destUri, { overwrite: true });
                const ok = await vscode.workspace.applyEdit(edit);
                if (ok) {
                    anyMoved = true;
                } else {
                    void vscode.window.showErrorMessage(`Failed to move '${fileName}'.`);
                }
            } catch (err) {
                void vscode.window.showErrorMessage(`Failed to move '${fileName}': ${String(err)}`);
                // Do not return – continue moving remaining files and let
                // onFilesMoved() fire so the view stays in sync with disk.
            }
        }

        if (anyMoved) {
            await this.onFilesMoved();
        }
    }
}
