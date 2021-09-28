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

enum taskKinds {
    proveProject, // Run gnatprove for the project
    proveFile, // Run gnatprove for the single file
}

export default class gnatproveTaskProvider implements vscode.TaskProvider<vscode.Task> {
    public static gnatproveType = 'gnatprove';
    gnatproveTasks: vscode.Task[] | undefined;

    constructor() {
        this.gnatproveTasks = undefined;
    }

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    provideTasks(_token: vscode.CancellationToken): vscode.Task[] {
        if (!this.gnatproveTasks) {
            this.gnatproveTasks = getTasks();
        }
        return this.gnatproveTasks;
    }

    resolveTask(
        task: vscode.Task,
        // eslint-disable-next-line @typescript-eslint/no-unused-vars
        _token: vscode.CancellationToken
    ): vscode.ProviderResult<vscode.Task> {
        return task;
    }
}

const getTasks = (): vscode.Task[] => {
    function makeTask(taskKind: taskKinds, args: string[], title: string) {
        const kind = {
            type: gnatproveTaskProvider.gnatproveType,
            projectFile: '${config:ada.projectFile}',
            taskKind: taskKind,
        };
        const shell = new vscode.ShellExecution('gnatprove', args);
        const task = new vscode.Task(kind, vscode.TaskScope.Workspace, title, 'ada', shell, '$ada');
        task.group = vscode.TaskGroup.Build;
        return task;
    }

    const result: vscode.Task[] = [];

    //  Run gnatprove on current project
    const proveProject = makeTask(
        taskKinds.proveProject,
        getGnatproveArgs('${config:ada.projectFile}'),
        'Run gnatprove on the current project'
    );

    //  Run gnatprove on a file
    const proveFile = makeTask(
        taskKinds.proveFile,
        getGnatproveArgs('${config:ada.projectFile}', '${fileBasename}'),
        'Run gnatprove on the current file'
    );
    result.push(proveProject);
    result.push(proveFile);

    return result;
};

const getGnatproveArgs = (projectFile?: string, file?: string): string[] => {
    //  Append file (if any) and `-gnatef` to generate full file names in errors/warnings
    const arg = file ? [file] : [];
    const p_gnatef = ['-cargs', '-gnatef'];
    return commonArgs(projectFile).concat(arg, p_gnatef);
};

//  return '-P project.gpr -XVAR=value` optiona
const commonArgs = (projectFile?: string): string[] => {
    const vars: string[][] = Object.entries(
        vscode.workspace.getConfiguration('ada').get('scenarioVariables') ?? []
    );
    const fold = (args: string[], item: string[]): string[] => {
        const option = '-X' + item[0] + '=' + item[1];
        return args.concat([option]);
    };
    //  Set projectFile is any
    const prj = projectFile ? ['-P', projectFile] : [];
    //  for each scenarioVariables put `-Xname=value` option
    return vars.reduce(fold, prj);
};
