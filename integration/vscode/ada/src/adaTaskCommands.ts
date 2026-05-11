/*----------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2026, AdaCore                          --
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

import * as vscode from 'vscode';
import { adaExtState } from './extension';
import { ProjectViewItem } from './projectViewProvider';
import { adaTasks, gnatCovTasks, TASK_TYPE_ADA } from './taskProviders';

/**
 * Registers VS Code commands as wrappers around GPR project-level tasks.
 * This allows those tasks to be invoked from UI surfaces such as context
 * menus (e.g. the Project View) without requiring the user to go through
 * the Tasks: Run Task picker.
 *
 * Only tasks that declare a {@link PredefinedTask.commandId} are registered
 * here: file-scoped tasks (which rely on the active editor context)
 * intentionally do not expose a commandId and are therefore not registered.
 *
 * When invoked from the Project View context menu, VS Code passes the selected
 * {@link ProjectViewItem} as the first argument.  The wrapper stores its URI in
 * {@link ExtensionState.pendingProjectOverride} before launching the task and
 * clears it in a finally block, so that {@link gprProjectArgs} targets the
 * selected project only for that specific invocation.
 */
export function registerAdaTaskWrappers(context: vscode.ExtensionContext) {
    for (const task of [...adaTasks, ...gnatCovTasks]) {
        if (task.commandId) {
            context.subscriptions.push(
                vscode.commands.registerCommand(task.commandId, async (item?: ProjectViewItem) => {
                    adaExtState.pendingProjectOverride = item?.uri;
                    try {
                        await vscode.commands.executeCommand(
                            'workbench.action.tasks.runTask',
                            `${TASK_TYPE_ADA}: ${task.label}`,
                        );
                    } finally {
                        adaExtState.pendingProjectOverride = undefined;
                    }
                }),
            );
        }
    }
}
