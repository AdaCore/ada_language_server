/*----------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2018-2021, AdaCore                     --
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


export default class cleanTaskProvider implements vscode.TaskProvider<vscode.Task> {
    public static cleanTaskType = 'gprclean';
    cleanTasks: vscode.Task[] | undefined;

    constructor() {
        this.cleanTasks = undefined;
    }

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    provideTasks(_token: vscode.CancellationToken): vscode.Task[] {
        if (!this.cleanTasks) {
            this.cleanTasks = getcleanTasks();
        }
        return this.cleanTasks;
    }

    resolveTask(
        task: vscode.Task,
        // eslint-disable-next-line @typescript-eslint/no-unused-vars
        _token: vscode.CancellationToken
    ): vscode.ProviderResult<vscode.Task> {
        const definition = task.definition;
        // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment
        const projectFile: string = definition.projectFile;
        // Make sure that this looks like a clean task by checking that there is a projectFile.
        if (projectFile || projectFile === '') {
            //  Refresh gprclean command line
            const args = getcleanBuildArgs(projectFile);
            const shell = new vscode.ShellExecution('gprclean', args);
            const title = `Clean project: ${projectFile}`;
            // resolveTask requires that the same definition object be used.
            return new vscode.Task(
                definition,
                vscode.TaskScope.Workspace,
                title,
                'ada',
                shell,
                '$ada'
            );
        }
        return undefined;
    }
}

const getcleanTasks = (): vscode.Task[] => {
    const result: vscode.Task[] = [];
    //  Clean current project
    const kind = {
        type: cleanTaskProvider.cleanTaskType,
        projectFile: '${config:ada.projectFile}',
    };
    const args = getcleanBuildArgs(kind.projectFile);
    const shell = new vscode.ShellExecution('gprclean', args);
    const task = new vscode.Task(
        kind,
        vscode.TaskScope.Workspace,
        'Clean current project',
        'ada',
        shell,
        '$ada'
    );
    task.group = vscode.TaskGroup.Build;
    result.push(task);

    return result;
};

const getcleanBuildArgs = (projectFile?: string): string[] => {
    const vars: string[][] = Object.entries(
        vscode.workspace.getConfiguration('ada').get('scenarioVariables') ?? []
    );
    const fold = (args: string[], item: string[]): string[] => {
        const option = '-X' + item[0] + '=' + item[1];
        return args.concat([option]);
    };
    //  for each scenarioVariables put `-Xname=value` option
    return vars.reduce(fold, []).concat(
        //  append projectFile is any
        projectFile ? [projectFile] : []
    );
};
