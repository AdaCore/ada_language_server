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
import { SymbolKind } from 'vscode';

enum taskKinds {
    proveProject, // Run gnatprove for the project
    proveFile, // Run gnatprove for the single file
    proveSubprogram, // Run gnatprove for the subprogram
    proveRegion, // Run gnatprove for the selection
    proveLine, // Run gnatprove for the single line
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
        const definition = task.definition;
        //  Arguments for proof of selected region need to be evaluated here
        if (definition.taskKind == taskKinds.proveRegion) {
            const args = getGnatproveArgs('${config:ada.projectFile}', ['-u', '${fileBasename}', '--limit-region=${fileBasename}:' + getSelectedRegion(vscode.window.activeTextEditor)]);
            const shell = new vscode.ShellExecution('gnatprove', args);
            return new vscode.Task(definition, vscode.TaskScope.Workspace, task.name, 'ada', shell, '$ada');
        }
        else if (definition.taskKind == taskKinds.proveSubprogram) {
            return getSubprogram(vscode.window.activeTextEditor)
                .then(subprogram =>
                {
                    const args = getGnatproveArgs('${config:ada.projectFile}', ['-u', '${fileBasename}', '--limit-subp=${fileBasename}:' + subprogram]);
                    const shell = new vscode.ShellExecution('gnatprove', args);
                    return new vscode.Task(definition, vscode.TaskScope.Workspace, task.name, 'ada', shell, '$ada');
                });
        }
        else {
            return task;
        }
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
        getGnatproveArgs('${config:ada.projectFile}', []),
        'Prove project'
    );

    //  Run gnatprove on a file
    const proveFile = makeTask(
        taskKinds.proveFile,
        getGnatproveArgs('${config:ada.projectFile}', ['-u', '${fileBasename}']),
        'Prove file'
    );

    //  Run gnatprove on a subprogram
    const proveSubprogram = makeTask(
        taskKinds.proveSubprogram,
        [], //  Actual arguments added at the time of resolveTask
        'Prove subprogram'
    );

    //  Run gnatprove on a region
    const proveRegion = makeTask(
        taskKinds.proveRegion,
        [], //  Actual arguments added at the time of resolveTask
        'Prove selected region'
    );

    //  Run gnatprove on a line
    const proveLine = makeTask(
        taskKinds.proveLine,
        getGnatproveArgs('${config:ada.projectFile}', ['-u', '${fileBasename}', '--limit-line=${fileBasename}:${lineNumber}']),
        'Prove line'
    );

    result.push(proveProject);
    result.push(proveFile);
    result.push(proveSubprogram);
    result.push(proveRegion);
    result.push(proveLine);

    return result;
};

const getSubprogram = async (editor: vscode.TextEditor | undefined): Promise<string> => {
    let subprogramLine = 0;
    if (editor) {
        const line = editor.selection.active.line;

        //  First get all symbols for current file
        const symbols: vscode.DocumentSymbol[] = await vscode.commands.executeCommand('vscode.executeDocumentSymbolProvider', editor.document.uri)

        //  Then select all subprograms
        let subprograms: vscode.DocumentSymbol[] = []

        function getAllSubprograms(symbols: vscode.DocumentSymbol[]) {
            var sym;
            for (sym of symbols) {
                if (sym.kind == SymbolKind.Function) {
                    subprograms.push(sym);
                }
                if (sym.kind == SymbolKind.Function || sym.kind == SymbolKind.Module) {
                    getAllSubprograms(sym.children)
                }
            }
        }

        getAllSubprograms(symbols)

        //  Finally select from the subprograms the smallest one containing the current line
        let scopeSymbols = subprograms.filter(sym => line >= sym.range.start.line && line <= sym.range.end.line);
        if (scopeSymbols.length > 0) {
            scopeSymbols.sort((a,b) => (a.range.end.line - a.range.start.line) - (b.range.end.line - b.range.start.line));
            subprogramLine = scopeSymbols[0].range.start.line;
        };
    };
    //  Line numbers start at 0 in VS Code, and at 1 in GNAT
    return (subprogramLine + 1).toString();
};

const getSelectedRegion = (editor: vscode.TextEditor | undefined): string => {
    if (editor) {
        const selection = editor.selections[0];
        //  Line numbers start at 0 in VS Code, and at 1 in GNAT
        return (selection.start.line + 1).toString() + ':' + (selection.end.line + 1).toString();
    }
    else {
        return '0:0';
    }
};

const getGnatproveArgs = (projectFile: string, args: string[]): string[] => {
    //  Append args (if any) and `-gnatef` to generate full file names in errors/warnings
    const p_gnatef = ['-cargs', '-gnatef'];
    return commonArgs(projectFile).concat(args, p_gnatef);
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
