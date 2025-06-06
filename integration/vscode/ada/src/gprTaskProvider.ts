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
import { LanguageClient } from 'vscode-languageclient/node';
import { getMains, getExecutables, getProjectFile } from './helpers';
import { DEFAULT_PROBLEM_MATCHERS, WarningMessageExecution } from './taskProviders';

/**
 *
 * @deprecated This class handles the deprecated task type 'gpr'. It is kept in
 * order to preserve support for 'gnat' task types that still exist in User
 * workspaces.
 */

export class GprTaskProvider implements vscode.TaskProvider<vscode.Task> {
    /**
     * This flag is used to restore the proposal of 'gpr' tasks for debugging
     * purposes.
     */
    public static DEPRECATED = true;
    private readonly client: LanguageClient;
    public static gprTaskType = 'gpr';
    glsTasks: vscode.Task[] | undefined;

    private readonly obsoletionMsg =
        "The 'gpr' task type is obsolete. Please use 'ada' tasks instead.";

    private readonly obsoleteWarningExecution = new WarningMessageExecution(this.obsoletionMsg);

    constructor(client: LanguageClient) {
        this.glsTasks = undefined;
        this.client = client;
    }

    async provideTasks(): Promise<vscode.Task[] | undefined> {
        if (GprTaskProvider.DEPRECATED) {
            // We return a single dummy task to convey an obsoletion message to Users.
            const msg = this.obsoletionMsg;
            return [
                new vscode.Task(
                    {
                        type: GprTaskProvider.gprTaskType,
                        taskKind: 'dummy',
                        projectFile: '${config:ada.projectFile}',
                    },
                    vscode.TaskScope.Workspace,
                    msg,
                    GprTaskProvider.gprTaskType,
                    this.obsoleteWarningExecution,
                    DEFAULT_PROBLEM_MATCHERS,
                ),
            ];
        } else {
            if (!this.glsTasks) {
                const project_file = await getProjectFile(this.client);
                const mains: string[] = await getMains(this.client);
                this.glsTasks = getBuildTasks(project_file, mains);
                const execs: string[] = await getExecutables(this.client);
                this.glsTasks = this.glsTasks.concat(
                    getBuildAndRunTasks(project_file, mains, execs),
                );
            }
        }
        return this.glsTasks;
    }

    resolveTask(task: vscode.Task) {
        /**
         * Here we resolve 'gpr' tasks still defined in User workspaces. We
         * handle them by displaying an obsoletion warning if the task gets
         * executed.
         */
        return new vscode.Task(
            task.definition,
            task.scope ?? vscode.TaskScope.Workspace,
            task.name,
            GprTaskProvider.gprTaskType,
            this.obsoleteWarningExecution,
            DEFAULT_PROBLEM_MATCHERS,
        );
    }
}

/**
 * Generates the build and run tasks for mains
 * @param projectFile - the project file path
 * @param mainFiles - a list of main files paths
 * @param execs - a list of executable files paths
 * @returns a list of tasks
 *
 * @deprecated 'gpr' tasks are deprecated
 */
function getBuildAndRunTasks(
    projectFile: string,
    mainFiles: string[],
    execs: string[],
): vscode.Task[] {
    const result: vscode.Task[] = [];
    //  build current project file
    for (let i = 0; i < mainFiles.length; i++) {
        const kind = {
            type: GprTaskProvider.gprTaskType,
            projectFile: projectFile,
            main: vscode.workspace.asRelativePath(mainFiles[i]),
            executable: vscode.workspace.asRelativePath(execs[i]),
        };
        const args = getMainBuildArgs(kind.projectFile, kind.main);
        args.push('&&', 'clear', '&&', kind.executable);
        const shell = new vscode.ShellExecution(fullCommand('gprbuild', args));
        const filename = mainFiles[i].replace(/^.*[\\/]/, '');
        const task = new vscode.Task(
            kind,
            vscode.TaskScope.Workspace,
            'Build And Run Main: ' + filename,
            'gpr',
            shell,
            '$gpr',
        );
        task.group = vscode.TaskGroup.Build;
        result.push(task);
    }

    return result;
}

/**
 * Generates the build tasks for mains
 * @param projectFile - the project file path
 * @param mainFiles - a list of main files paths
 * @returns a list of tasks
 *
 * @deprecated 'gpr' tasks are deprecated
 */
function getBuildTasks(projectFile: string, mainFiles: string[]): vscode.Task[] {
    const result: vscode.Task[] = [];
    //  build current project file
    for (let i = 0; i < mainFiles.length; i++) {
        const kind = {
            type: GprTaskProvider.gprTaskType,
            projectFile: projectFile,
            main: vscode.workspace.asRelativePath(mainFiles[i]),
        };
        const filename = mainFiles[i].replace(/^.*[\\/]/, '');
        const args = getMainBuildArgs(kind.projectFile, kind.main);
        const shell = new vscode.ShellExecution(fullCommand('gprbuild', args));
        const task = new vscode.Task(
            kind,
            vscode.TaskScope.Workspace,
            'Build Main: ' + filename,
            'gpr',
            shell,
            '$gpr',
        );
        task.group = vscode.TaskGroup.Build;
        result.push(task);
    }

    return result;
}

/**
 * Adds relative arguments for the target build command
 * @param projectFile - the project file path
 * @param mainFile - the main file path
 * @returns a list of arguments
 */
function getMainBuildArgs(projectFile?: string, mainFile?: string): string[] {
    const vars: string[][] = Object.entries(
        vscode.workspace.getConfiguration('ada').get('scenarioVariables') ?? [],
    );
    const fold = (args: string[], item: string[]): string[] => {
        const option = '-X' + item[0] + '=' + item[1];
        return args.concat([option]);
    };
    //  for each scenarioVariables put `-Xname=value` option
    const args = vars.reduce(fold, []).concat(
        //  append projectFile is any
        projectFile ? [projectFile] : [],
    );
    // append the file to build
    args.push(mainFile ? mainFile : '');
    args.unshift('-P');
    args.unshift('-d');
    return args;
}

/**
 * Return the command while ignoring unecessary spaces
 * @param command - the command to execute
 * @param args - the list of arguments
 * @returns a string
 *
 * @deprecated 'gpr' tasks are deprecated
 */
export function fullCommand(command: string, args: string[]) {
    let cmd: string = command + ' ';
    for (const arg of args) {
        cmd += arg.replace(/^\s+|\s+$/g, '') + ' ';
    }
    return cmd;
}
