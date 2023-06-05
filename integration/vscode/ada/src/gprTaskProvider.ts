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

export type GlsMainResult = {
    mains: string[];
};

export type GlsExecutableResult = {
    executables: string[];
};

export default class GprTaskProvider implements vscode.TaskProvider<vscode.Task> {
    private readonly client: LanguageClient;
    public static gprTaskType = 'GPR Tasks';
    glsTasks: vscode.Task[] | undefined;

    constructor(client: LanguageClient) {
        this.glsTasks = undefined;
        this.client = client;
    }

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    public async provideTasks(): Promise<vscode.Task[] | undefined> {
        if (!this.glsTasks) {
            const result: GlsMainResult = await this.client.sendRequest('$/glsMains');
            this.glsTasks = getMainBuildTasks(result.mains);
            const execs: GlsExecutableResult = await this.client.sendRequest('$/glsExecutables');
            this.glsTasks = this.glsTasks.concat(getExecutableRunTasks(execs.executables));
        }
        return this.glsTasks;
    }

    resolveTask(
        task: vscode.Task,
        // eslint-disable-next-line @typescript-eslint/no-unused-vars
        _token: vscode.CancellationToken
    ): vscode.ProviderResult<vscode.Task> {
        const definition = task.definition;
        // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment
        const projectFile: string = definition.projectFile;
        // Make sure that this looks like a execute task by checking that there is a projectFile.
        // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment
        const executableFile: string = definition.executable;
        if (projectFile || projectFile === '') {
            //  Refresh gprbuild command line
            const args = getMainBuildArgs(projectFile, executableFile);
            const shell = new vscode.ShellExecution('gprbuild', args);
            const title = `Build Executable for: ${executableFile}`;
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

const getMainBuildTasks = (mainFiles: string[]): vscode.Task[] => {
    const result: vscode.Task[] = [];
    //  build current project file
    for (let i = 0; i < mainFiles.length; i++) {
        const kind = {
            type: GprTaskProvider.gprTaskType,
            projectFile: '${config:ada.projectFile}',
            mainFile: vscode.workspace.asRelativePath(mainFiles[i]),
        };
        const args = getMainBuildArgs(kind.projectFile, kind.mainFile);
        const shell = new vscode.ShellExecution('gprbuild', args);
        const filename = mainFiles[i].replace(/^.*[\\/]/, '');
        const task = new vscode.Task(
            kind,
            vscode.TaskScope.Workspace,
            'Build Executable for File ' + filename,
            'ada',
            shell,
            '$ada'
        );
        task.group = vscode.TaskGroup.Build;
        result.push(task);
    }

    return result;
};

const getExecutableRunTasks = (executables: string[]): vscode.Task[] => {
    const result: vscode.Task[] = [];
    //  build current project file
    for (let i = 0; i < executables.length; i++) {
        const kind = {
            type: GprTaskProvider.gprTaskType,
            projectFile: '${config:ada.projectFile}',
            executable: vscode.workspace.asRelativePath(executables[i]),
        };
        const filename = executables[i].replace(/^.*[\\/]/, '');
        const shell = new vscode.ShellExecution('./' + kind.executable, []);
        const task = new vscode.Task(
            kind,
            vscode.TaskScope.Workspace,
            'Run Executable: ' + filename,
            'ada',
            shell,
            '$ada'
        );
        task.group = vscode.TaskGroup.Build;
        result.push(task);
    }

    return result;
};

const getMainBuildArgs = (projectFile?: string, mainFile?: string): string[] => {
    const vars: string[][] = Object.entries(
        vscode.workspace.getConfiguration('ada').get('scenarioVariables') ?? []
    );
    const fold = (args: string[], item: string[]): string[] => {
        const option = '-X' + item[0] + '=' + item[1];
        return args.concat([option]);
    };
    //  for each scenarioVariables put `-Xname=value` option
    const args = vars.reduce(fold, []).concat(
        //  append projectFile is any
        projectFile ? [projectFile] : []
    );
    // append the file to build
    args.push(mainFile ? mainFile : '');
    args.unshift('-P');
    args.unshift('-d');
    return args;
};
