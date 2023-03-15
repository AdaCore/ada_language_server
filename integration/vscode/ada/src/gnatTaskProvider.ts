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
import commandExists from 'command-exists';
import { SymbolKind } from 'vscode';

/**
 * Callback to provide an extra argument for a tool
 */
type ExtraArgCallback = () => Promise<string>;

/**
 * Tool description
 */
interface TaskProperties {
    command: string[]; //  Executable like gprbuild, gprclean, gnatprove,
    // etc. and static list of arguments
    extra: ExtraArgCallback | undefined; //  Dynamic argument callback if any
    // Args and extra argument will be wrapped with getGnatArgs if this is set.
    title: string; // Title like 'Examine project'
}

/**
 * Return the `--limit-subp=file:line` associated to the subprogram enclosing the
 * the current editor's cursor position, if any. Return an empty string otherwise.
 * @returns Return the option corresponding to the enclosing subprogram as a string
 * or '' if not found.
 */
const limitSubp = async (): Promise<string> => {
    return getSubprogramSymbol(vscode.window.activeTextEditor).then((Symbol) => {
        if (Symbol) {
            const subprogram_line: string = (Symbol.range.start.line + 1).toString();
            return `--limit-subp=\${fileBasename}:${subprogram_line}`;
        } else {
            return '';
        }
    });
};

/**
 * Return the `--limit-region=file:from:to` associated to the current editor's selection.
 * @returns Return the option corresponding to the current selected region.
 */
const limitRegion = (): Promise<string> => {
    return new Promise((resolve) => {
        resolve(
            `--limit-region=\${fileBasename}:${getSelectedRegion(vscode.window.activeTextEditor)}`
        );
    });
};

//  Append '-P project.gpr -XVAR=value` options to args
const commonArgs = (args: string[]): string[] => {
    const vars: string[][] = Object.entries(
        vscode.workspace.getConfiguration('ada').get('scenarioVariables') ?? []
    );
    const fold = (args: string[], item: string[]): string[] => {
        const option = '-X' + item[0] + '=' + item[1];
        return args.concat([option]);
    };

    // Set projectFile is any
    const prj =
        vscode.workspace.getConfiguration('ada').get('projectFile') != ''
            ? ['-P', '${config:ada.projectFile}']
            : [];

    // for each scenarioVariables put `-Xname=value` option
    return args.concat(vars.reduce(fold, prj));
};

// Call commonArgs on args and append `-gnatef` to generate full file names in errors/warnings
const getGnatArgs = (args: string[]): string[] => {
    const p_gnatef = ['-cargs', '-gnatef'];
    return commonArgs(args).concat(p_gnatef);
};

/**
 * Map of known tasks/tools indexed by a string/taskKind
 */
const knownTaskKinds: { [id: string]: TaskProperties } = {
    examineProject: {
        command: getGnatArgs(['gnatprove', '--mode=flow']),
        extra: undefined,
        title: 'Examine project',
    },
    examineFile: {
        command: getGnatArgs(['gnatprove', '--mode=flow', '-u', '${fileBasename}']),
        extra: undefined,
        title: 'Examine file',
    },
    examineSubprogram: {
        command: ['gnatprove', '--mode=flow'],
        extra: limitSubp,
        title: 'Examine subprogram',
    },
    proveProject: {
        command: getGnatArgs(['gnatprove']),
        extra: undefined,
        title: 'Prove project',
    },
    proveFile: {
        command: getGnatArgs(['gnatprove', '-u', '${fileBasename}']),
        extra: undefined,
        title: 'Prove file',
    },
    proveSubprogram: {
        command: ['gnatprove'],
        extra: limitSubp,
        title: 'Prove subprogram',
    },
    proveRegion: {
        command: ['gnatprove', '-u', '${fileBasename}'],
        extra: limitRegion,
        title: 'Prove selected region',
    },
    proveLine: {
        command: getGnatArgs([
            'gnatprove',
            '-u',
            '${fileBasename}',
            '--limit-line=${fileBasename}:${lineNumber}',
        ]),
        extra: undefined,
        title: 'Prove line',
    },
    buildProject: {
        command: getGnatArgs(['gprbuild']),
        extra: undefined,
        title: 'Build current project',
    },
    checkFile: {
        command: getGnatArgs(['gprbuild', '-q', '-f', '-c', '-u', '-gnatc', '${fileBasename}']),
        extra: undefined,
        title: 'Check current file',
    },
    cleanProject: {
        command: commonArgs(['gprbuild']), // No -cargs -gnatef is accepted by gprclean
        extra: undefined,
        title: 'Clean current project',
    },
};

//  Alire `exec` command if we have `alr` installed and `alire.toml`
async function alire(): Promise<string[]> {
    return vscode.workspace.findFiles('alire.toml').then((found) =>
        found.length == 0
            ? [] // not alire.toml found, return no command
            : // if alire.toml found, search for `alr`
              commandExists('alr')
                  .then(() => ['alr', 'exec', '--'])
                  .catch(() => [])
    );
}

/**
 * Task provider itself
 */
export default class GnatTaskProvider implements vscode.TaskProvider<vscode.Task> {
    public static gnatType = 'gnat'; // Task provider name
    gnatTasks: vscode.Task[] | undefined; // Known tasks

    constructor() {
        this.gnatTasks = undefined; //  Do we really need this???
    }

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    provideTasks(_token: vscode.CancellationToken): vscode.ProviderResult<vscode.Task[]> {
        if (!this.gnatTasks) {
            return getTasks().then((list) => {
                this.gnatTasks = list;
                return list;
            });
        }
        return this.gnatTasks;
    }

    resolveTask(
        task: vscode.Task,
        // eslint-disable-next-line @typescript-eslint/no-unused-vars
        _token: vscode.CancellationToken
    ): vscode.ProviderResult<vscode.Task> {
        const definition = task.definition;

        // Check if the task in our known task
        if (definition.taskKind in knownTaskKinds) {
            const item: TaskProperties = knownTaskKinds[String(definition.taskKind)];
            const extraArgs: string[] = Array.isArray(definition.args)
                ? definition.args.map((x) => String(x))
                : [];
            if (item.extra) {
                // We have a callback, evaluate it to get an extra argument and
                // wrap all args with getGnatArgs
                return item.extra().then((extra) => {
                    return alire().then((alr) => {
                        const cmd = getGnatArgs(
                            alr.concat(item.command, extraArgs, extra ? [extra] : [])
                        );
                        const shell = new vscode.ShellExecution(cmd[0], cmd.slice(1));
                        return new vscode.Task(
                            definition,
                            vscode.TaskScope.Workspace, // scope
                            task.name,
                            'ada', // source
                            shell,
                            '$ada' // problemMatchers
                        );
                    });
                });
            } else {
                return alire().then((alr) => {
                    const cmd = alr.concat(item.command, extraArgs);
                    const shell = new vscode.ShellExecution(cmd[0], cmd.slice(1));
                    return new vscode.Task(
                        definition,
                        vscode.TaskScope.Workspace, // scope
                        task.name,
                        'ada', // source
                        shell,
                        '$ada' // problemMatchers
                    );
                });
            }
        } else {
            return task;
        }
    }
}

/**
 * Return all known tasks
 */
async function getTasks(): Promise<vscode.Task[]> {
    return alire().then((alr) => {
        const result: vscode.Task[] = [];

        for (const taskKind in knownTaskKinds) {
            const item: TaskProperties = knownTaskKinds[taskKind];
            const title: string = item.title;
            const kind = {
                type: GnatTaskProvider.gnatType,
                projectFile: '${config:ada.projectFile}',
                taskKind: taskKind,
            };
            const cmd = alr.concat(item.command);
            const shell = new vscode.ShellExecution(cmd[0], cmd.slice(1));
            const task = new vscode.Task(
                kind,
                vscode.TaskScope.Workspace,
                title,
                'ada',
                shell,
                '$ada'
            );
            task.group = vscode.TaskGroup.Build;
            result.push(task);
        }

        return result;
    });
}

/**
 * Return the DocumentSymbol associated to the subprogram enclosing the
 * the given editor's cursor position, if any.
 * @param editor - The editor in which we want
 * to find the suprogram's body enclosing the cursor's position.
 * @returns Return the symbol corresponding to the
 * enclosing subprogram or null if not found.
 */
export const getSubprogramSymbol = async (
    editor: vscode.TextEditor | undefined
): Promise<vscode.DocumentSymbol | null> => {
    if (editor) {
        const line = editor.selection.active.line;

        // First get all symbols for current file
        const symbols: vscode.DocumentSymbol[] = await vscode.commands.executeCommand(
            'vscode.executeDocumentSymbolProvider',
            editor.document.uri
        );

        // Then select all subprograms
        const subprograms: vscode.DocumentSymbol[] = [];

        const getAllSubprograms = (symbols: vscode.DocumentSymbol[]) => {
            let sym;
            for (sym of symbols) {
                if (sym.kind == SymbolKind.Function) {
                    subprograms.push(sym);
                }
                if (sym.kind == SymbolKind.Function || sym.kind == SymbolKind.Module) {
                    getAllSubprograms(sym.children);
                }
            }
        };

        getAllSubprograms(symbols);

        // Finally select from the subprograms the smallest one containing the current line
        const scopeSymbols = subprograms.filter(
            (sym) => line >= sym.range.start.line && line <= sym.range.end.line
        );
        if (scopeSymbols.length > 0) {
            scopeSymbols.sort(
                (a, b) =>
                    a.range.end.line - a.range.start.line - (b.range.end.line - b.range.start.line)
            );
            return scopeSymbols[0];
        }
    }

    return null;
};

const getSelectedRegion = (editor: vscode.TextEditor | undefined): string => {
    if (editor) {
        const selection = editor.selections[0];
        //  Line numbers start at 0 in VS Code, and at 1 in GNAT
        return (selection.start.line + 1).toString() + ':' + (selection.end.line + 1).toString();
    } else {
        return '0:0';
    }
};
