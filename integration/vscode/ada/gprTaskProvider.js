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

"use strict";
const vscode = require("vscode");

const checkArgs = ['-q', '-f', '-c', '-u', '-gnatc', '${fileBasename}'];

class GPRTaskProvider {
    constructor() {
        this.gprPromise = undefined;
    }
    provideTasks() {
        if (!this.gprPromise) {
            this.gprPromise = getGprTasks();
        }
        return this.gprPromise;
    }
    resolveTask(task) {
        const definition = task.definition;
        const projectFile = definition.projectFile;
        // Make sure that this looks like a gprbuild task by checking that there is a projectFile.
        if (projectFile || projectFile === '') {
            //  Refresh gprbuild command line
            const args = getGprbuildArgs(projectFile).concat
              (definition.checkFile ? checkArgs : []);
            const shell = new vscode.ShellExecution('gprbuild', args);
            const title = definition.checkFile ? 'Check file' : `Build project: ${projectFile}`;
            // resolveTask requires that the same definition object be used.
            return new vscode.Task(definition, vscode.TaskScope.Workspace, title, 'ada', shell, '$ada');
        }
        return undefined;
    }
}
exports.GPRTaskProvider = GPRTaskProvider;

GPRTaskProvider.gprBuildType = 'gprbuild';

async function getGprTasks() {
    const result = [];
    //  Build current project
    const kind = {
        type: GPRTaskProvider.gprBuildType,
        projectFile: '${config:ada.projectFile}'
    };
    const args = getGprbuildArgs(kind.projectFile);
    const shell = new vscode.ShellExecution('gprbuild', args);
    const task = new vscode.Task
      (kind,
       vscode.TaskScope.Workspace,
       'Build current project',
       'ada', shell, '$ada');
    task.group = { kind: vscode.TaskGroup.Build, isDefault: true};
    result.push(task);
    //  Check semantic in the current file
    const kind_check = {
        type: GPRTaskProvider.gprBuildType,
        projectFile: '${config:ada.projectFile}',
        checkFile: true
    };
    const args_check = args.concat(checkArgs);
    const shell_check = new vscode.ShellExecution('gprbuild', args_check);
    const task_check = new vscode.Task
      (kind_check,
       vscode.TaskScope.Workspace,
       'Check current file',
       'ada', shell_check, '$ada');
    result.push(task_check);
    return result;
}

function getGprbuildArgs(projectFile){
    const vars = vscode.workspace.getConfiguration('ada').
      get('scenarioVariables') || {};
    function fold(args, item) {
        const option='-X' + item[0] + '=' + item[1];
        return args.concat ([option]);
    }
    const args =
      //  for each scenarioVariables put `-Xname=value` option
      Object.entries(vars).reduce(fold, []).concat
        (//  append `-p` to create dirs and `-gnatef` to generate full
         //  file name in errors/warnings
         ['-p', '-gnatef'],
         //  append projectFile is any 
         (projectFile ? [projectFile] : [] )
         );
    return args;
}