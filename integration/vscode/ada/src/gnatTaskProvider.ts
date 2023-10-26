/*----------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2018-2023, AdaCore                     --
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
import {
    AllTaskKinds,
    DEFAULT_PROBLEM_MATCHER,
    TaskProperties,
    alire,
    allTaskProperties,
    getDiagnosticArgs,
    getProjectArgs,
    getScenarioArgs,
} from './taskProviders';

/**
 * @deprecated This interface defines the data structures matching the JSON
 * schema for deprecated 'gnat' tasks. We use it for better type checking in
 * code providing backwards compatibility with deprecated 'gnat' tasks.
 */
interface GnatTaskDefinition extends vscode.TaskDefinition {
    type: 'gnat';
    taskKind: string;
    projectFile?: string;
    args?: string[];
}

/**
 *
 * @deprecated This class handles the deprecated task type 'gnat'. It is kept in
 * order to preserve support for 'gnat' task types that still exist in User
 * workspaces.
 */
export default class GnatTaskProvider implements vscode.TaskProvider<vscode.Task> {
    /**
     * This flag is used to restore the proposal of 'gnat' tasks for debugging
     * purposes.
     */
    public static DEPRECATED = true;

    public static gnatType = 'gnat' as const; // Task provider name
    gnatTasks: vscode.Task[] | undefined; // Known tasks

    constructor() {
        this.gnatTasks = undefined; //  Do we really need this???
    }

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    provideTasks(_token: vscode.CancellationToken): vscode.ProviderResult<vscode.Task[]> {
        if (GnatTaskProvider.DEPRECATED) {
            // We return a single dummy task to convey an obsoletion message to Users.
            const msg = 'The "gnat" task type is obsolete. Use "ada" tasks instead.';
            return [
                new vscode.Task(
                    {
                        type: GnatTaskProvider.gnatType,
                        taskKind: 'dummy',
                        projectFile: '${config:ada.projectFile}',
                    },
                    vscode.TaskScope.Workspace,
                    msg,
                    GnatTaskProvider.gnatType,
                    new vscode.ShellExecution(`echo ${msg}`),
                    DEFAULT_PROBLEM_MATCHER
                ),
            ];
        } else {
            // Provide the set of tasks as deprecated 'gnat' tasks for debugging
            if (!this.gnatTasks) {
                return getTasks().then((list) => {
                    this.gnatTasks = list;
                    return list;
                });
            }
            return this.gnatTasks;
        }
    }

    resolveTask(
        task: vscode.Task,
        // eslint-disable-next-line @typescript-eslint/no-unused-vars
        _token: vscode.CancellationToken
    ): vscode.ProviderResult<vscode.Task> {
        // We keep the previous task resolution code so that tasks in User
        // workspaces still work like before. Such tasks are also flagged as
        // obsolete in the tasks.json file to encourage Users to migrate to the
        // 'ada' task type.
        const definition = task.definition as GnatTaskDefinition;

        // Check if the task in our known task
        if (definition.taskKind in allTaskProperties) {
            const taskKind = definition.taskKind as AllTaskKinds;
            const item: TaskProperties = allTaskProperties[taskKind];
            const extraArgsFromUser: string[] = Array.isArray(definition.args)
                ? definition.args.map((x) => String(x))
                : [];
            const extraArgsFromTask = item.extra ? item.extra() : [];
            return alire().then(async (alr) => {
                const cmd = alr.concat(
                    item.command,
                    await getProjectArgs(),
                    getScenarioArgs(),
                    extraArgsFromUser,
                    await extraArgsFromTask,
                    getDiagnosticArgs()
                );
                const shell = new vscode.ShellExecution(cmd[0], cmd.slice(1));
                return new vscode.Task(
                    definition,
                    task.scope ?? vscode.TaskScope.Workspace,
                    task.name,
                    'ada',
                    shell,
                    DEFAULT_PROBLEM_MATCHER // problemMatchers
                );
            });
        } else {
            return task;
        }
    }
}

/**
 *
 * @deprecated This function creates obsolete tasks of type 'gnat'. It is kept
 * temporarily for debugging backwards support of 'gnat' tasks. It can be activated
 * with {@link GnatTaskProvider.DEPRECATED} to create 'gnat' tasks like before.
 */

export async function getTasks(): Promise<vscode.Task[]> {
    return alire().then(async (alr) => {
        const result: vscode.Task[] = [];

        for (const taskKind in allTaskProperties) {
            const item: TaskProperties = allTaskProperties[taskKind as AllTaskKinds];
            const title: string = item.title;
            const definition: GnatTaskDefinition = {
                type: GnatTaskProvider.gnatType,
                projectFile: '${config:ada.projectFile}',
                taskKind: taskKind,
            };
            const extraArgsFromTask = item.extra ? item.extra() : [];
            const cmd = alr.concat(
                item.command,
                await getProjectArgs(),
                getScenarioArgs(),
                await extraArgsFromTask,
                getDiagnosticArgs()
            );
            const shell = new vscode.ShellExecution(cmd[0], cmd.slice(1));
            const task = new vscode.Task(
                definition,
                vscode.TaskScope.Workspace,
                title,
                'gnat',
                shell,
                '$ada'
            );
            task.group = vscode.TaskGroup.Build;
            result.push(task);
        }

        return result;
    });
}
