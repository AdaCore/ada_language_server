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

import assert from 'assert';
import commandExists from 'command-exists';
import { basename } from 'path';
import * as vscode from 'vscode';
import { SymbolKind } from 'vscode';
import { adaExtState } from './extension';
import { getAdaMains, getProjectFile } from './helpers';
import { task } from 'fp-ts';

/**
 * Callback to provide an extra argument for a tool
 */
type ExtraArgCallback = () => Promise<string[]>;

/**
 * Tool description
 */
export interface TaskProperties {
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
const limitSubp = async (): Promise<string[]> => {
    return getEnclosingSymbol(vscode.window.activeTextEditor, [SymbolKind.Function]).then(
        (Symbol) => {
            if (Symbol) {
                const subprogram_line: string = (Symbol.range.start.line + 1).toString();
                return [`--limit-subp=\${fileBasename}:${subprogram_line}`];
            } else {
                return [];
            }
        }
    );
};

/**
 * Return the `--limit-region=file:from:to` associated to the current editor's selection.
 * @returns Return the option corresponding to the current selected region.
 */
const limitRegion = (): Promise<string[]> => {
    return new Promise((resolve) => {
        resolve([
            `--limit-region=\${fileBasename}:${getSelectedRegion(vscode.window.activeTextEditor)}`,
        ]);
    });
};

async function computeProject(taskDef?: CustomTaskDefinition): Promise<string> {
    // If the task definition defines a project file, use that. Otherwise if
    // ada.projectFile is defined, use ${config:ada.projectFile}. Finally,
    // fallback to querying the ALS for the full path to the project file.
    return taskDef?.configuration?.projectFile ?? getProjectFromConfigOrALS();
}

// Call commonArgs on args and append `-gnatef` to generate full file names in errors/warnings
export const getDiagnosticArgs = (): string[] => {
    const p_gnatef = ['-cargs', '-gnatef'];
    return p_gnatef;
};

// The following pair of declarations allow creating a set of string values both
// as an iterable (constant) array, and as a union type.
const adaTaskKinds = [
    'buildProject',
    'checkFile',
    'cleanProject',
    'buildMain',
    'buildAndRunMain',
] as const;
type AdaTaskKinds = (typeof adaTaskKinds)[number];

// The following pair of declarations allow creating a set of string values both
// as an iterable (constant) array, and as a union type.
const sparkTaskKinds = [
    'cleanProjectForProof',
    'examineProject',
    'examineFile',
    'examineSubprogram',
    'proveProject',
    'proveFile',
    'proveSubprogram',
    'proveRegion',
    'proveLine',
] as const;
type SparkTaskKinds = (typeof sparkTaskKinds)[number];

export type AllTaskKinds = AdaTaskKinds | SparkTaskKinds;

/**
 * This interface defines the data structure expected in vscode task
 * definitions. It intends to match as closely as possible with the JSON schemas
 * defined in the package.json file for the "ada" and "spark" tasks. However
 * JSON schemas are more expressive in terms of constraints between properties
 * within the data structure. As a result this interface simply marks fields as
 * optional when they may or may not occur while the JSON schemas of
 * package.json describe the structure more precisely.
 */
export interface CustomTaskDefinition extends vscode.TaskDefinition {
    configuration: {
        kind: AllTaskKinds;
        projectFile: string;
        args?: string[];
        main?: string;
        executable?: string;
        mainArgs?: string[];
    };
}

/**
 * Map of known tasks/tools indexed by a string/taskKind
 */
export const allTaskProperties: { [id in AllTaskKinds]: TaskProperties } = {
    cleanProjectForProof: {
        command: ['gnatprove', '--clean'],
        extra: undefined,
        title: 'Clean project for proof',
    },
    examineProject: {
        command: ['gnatprove', '-j0', '--mode=flow'],
        extra: undefined,
        title: 'Examine project',
    },
    examineFile: {
        command: ['gnatprove', '-j0', '--mode=flow', '-u', '${fileBasename}'],
        extra: undefined,
        title: 'Examine file',
    },
    examineSubprogram: {
        command: ['gnatprove', '-j0', '--mode=flow'],
        extra: limitSubp,
        title: 'Examine subprogram',
    },
    proveProject: {
        command: ['gnatprove', '-j0'],
        extra: undefined,
        title: 'Prove project',
    },
    proveFile: {
        command: ['gnatprove', '-j0', '-u', '${fileBasename}'],
        extra: undefined,
        title: 'Prove file',
    },
    proveSubprogram: {
        command: ['gnatprove', '-j0'],
        extra: limitSubp,
        title: 'Prove subprogram',
    },
    proveRegion: {
        command: ['gnatprove', '-j0', '-u', '${fileBasename}'],
        extra: limitRegion,
        title: 'Prove selected region',
    },
    proveLine: {
        command: [
            'gnatprove',
            '-j0',
            '-u',
            '${fileBasename}',
            '--limit-line=${fileBasename}:${lineNumber}',
        ],
        extra: undefined,
        title: 'Prove line',
    },
    buildProject: {
        command: ['gprbuild'],
        extra: undefined,
        title: 'Build current project',
    },
    checkFile: {
        command: ['gprbuild', '-q', '-f', '-c', '-u', '-gnatc', '${fileBasename}'],
        extra: undefined,
        title: 'Check current file',
    },
    cleanProject: {
        command: ['gprclean'],
        extra: undefined,
        title: 'Clean current project',
    },
    buildMain: {
        command: ['gprbuild'],
        extra: undefined,
        title: 'Build main - ',
    },
    buildAndRunMain: {
        command: ['gprbuild'],
        extra: undefined,
        title: 'Build and run main - ',
    },
};

export const PROJECT_FROM_CONFIG = '${config:ada.projectFile}';
async function getProjectFromConfigOrALS(): Promise<string> {
    return vscode.workspace.getConfiguration('ada').get('projectFile')
        ? PROJECT_FROM_CONFIG
        : await getProjectFile(adaExtState.adaClient);
}

export function getScenarioArgs() {
    const vars: string[][] = Object.entries(
        vscode.workspace.getConfiguration('ada').get('scenarioVariables') ?? []
    );
    const fold = (args: string[], item: string[]): string[] => {
        const option = '-X' + item[0] + '=' + item[1];
        return args.concat([option]);
    };

    // for each scenarioVariables put `-Xname=value` option
    return vars.reduce(fold, []);
}

export async function getProjectArgs(taskDef?: CustomTaskDefinition) {
    return await computeProject(taskDef)
        .then((prj) => ['-P', prj])
        .catch(() => []);
}

//  Alire `exec` command if we have `alr` installed and `alire.toml`
export async function alire(): Promise<string[]> {
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
 * Create a fully resolved task.
 *
 * @param taskType - the type of task to create, 'ada' or 'spark'
 * @param taskKind - the task kind
 * @param item - the TaskProperties object
 * @param commandPrefix - a prefix to use in the construction of the command line of the task
 * @returns
 */
async function createResolvedTask(
    taskType: TaskType,
    taskKind: AllTaskKinds,
    item: TaskProperties,
    commandPrefix: string[]
): Promise<vscode.Task> {
    const title: string = item.title;
    const definition: CustomTaskDefinition = {
        type: taskType,
        configuration: {
            kind: taskKind,
            projectFile: await getProjectFromConfigOrALS(),
        },
    };

    const cmd = commandPrefix.concat(await buildFullCommandLine(title, definition));

    const shell = new vscode.ShellExecution(cmd[0], cmd.slice(1));

    const task = new vscode.Task(
        definition,
        vscode.TaskScope.Workspace,
        title,
        // Always use the task type as a source string in the UI for consistency
        // between the tasks.json definitions and what Users see in the UI
        taskType,
        shell,
        DEFAULT_PROBLEM_MATCHER
    );
    task.group = vscode.TaskGroup.Build;
    return task;
}

/**
 * Return the closest DocumentSymbol of the given kinds enclosing the
 * the given editor's cursor position, if any.
 * @param editor - The editor in which we want
 * to find the closest symbol enclosing the cursor's position.
 * @returns Return the closest enclosing symbol.
 */
export async function getEnclosingSymbol(
    editor: vscode.TextEditor | undefined,
    kinds: vscode.SymbolKind[]
): Promise<vscode.DocumentSymbol | null> {
    if (editor) {
        const line = editor.selection.active.line;

        // First get all symbols for current file
        const symbols: vscode.DocumentSymbol[] = await vscode.commands.executeCommand(
            'vscode.executeDocumentSymbolProvider',
            editor.document.uri
        );

        // Then filter them according to the specified kinds
        const filtered_symbols: vscode.DocumentSymbol[] = [];

        const getAllSymbols = (symbols: vscode.DocumentSymbol[]) => {
            let sym;
            for (sym of symbols) {
                if (kinds.includes(sym.kind)) {
                    filtered_symbols.push(sym);
                }
                if (sym.kind == SymbolKind.Function || sym.kind == SymbolKind.Module) {
                    getAllSymbols(sym.children);
                }
            }
        };

        getAllSymbols(symbols);

        // Finally select from the filtered symbols the smallest one containing the current line
        const scopeSymbols = filtered_symbols.filter(
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
}

const getSelectedRegion = (editor: vscode.TextEditor | undefined): string => {
    if (editor) {
        const selection = editor.selections[0];
        //  Line numbers start at 0 in VS Code, and at 1 in GNAT
        return (selection.start.line + 1).toString() + ':' + (selection.end.line + 1).toString();
    } else {
        return '0:0';
    }
};

// eslint-disable-next-line @typescript-eslint/no-unused-vars
type ObsoleteTaskType = 'gnat' | 'gpr';
type TaskType = 'ada' | 'spark';

export function registerTaskProviders() {
    return [
        vscode.tasks.registerTaskProvider('ada', createAdaTaskProvider()),
        vscode.tasks.registerTaskProvider('spark', createSparkTaskProvider()),
    ];
}

export const DEFAULT_PROBLEM_MATCHER = '$ada';

/**
 * This class implements the TaskProvider interface with some configurable functionality.
 */
export class ConfigurableTaskProvider implements vscode.TaskProvider {
    public static taskTypeAda = 'ada';
    public static taskTypeSpark = 'spark';
    tasks: vscode.Task[] | undefined = undefined;
    taskType: TaskType;
    taskKinds: AllTaskKinds[];

    constructor(taskType: TaskType, taskKinds: AllTaskKinds[]) {
        this.taskType = taskType;
        this.taskKinds = taskKinds;
    }

    async provideTasks(token?: vscode.CancellationToken): Promise<vscode.Task[]> {
        if (!this.tasks) {
            this.tasks = [];
            const cmdPrefix = await alire();
            for (const kind of this.taskKinds) {
                if (token?.isCancellationRequested) {
                    this.tasks = undefined;
                    break;
                }
                if (kind in allTaskProperties && kind != 'buildMain' && kind != 'buildAndRunMain') {
                    // Do not provide a task for buildMain because we provide
                    // one per project main below

                    const taskProperties = allTaskProperties[kind];
                    // provideTasks() is expected to provide fully resolved
                    // tasks ready for execution
                    const task = createResolvedTask(this.taskType, kind, taskProperties, cmdPrefix);
                    this.tasks.push(await task);
                }
            }

            if (this.taskType == 'ada') {
                for (const main of await getAdaMains()) {
                    if (token?.isCancellationRequested) {
                        this.tasks = undefined;
                        break;
                    }
                    {
                        const def: CustomTaskDefinition = {
                            type: this.taskType,
                            configuration: {
                                kind: 'buildMain',
                                projectFile: await computeProject(),
                                main: main.mainRelPath(),
                            },
                        };
                        const name = `${allTaskProperties['buildMain'].title}${basename(
                            main.mainRelPath()
                        )}`;
                        const cmdLine = await buildFullCommandLine(name, def);
                        this.tasks?.push(
                            new vscode.Task(
                                def,
                                vscode.TaskScope.Workspace,
                                name,
                                this.taskType,
                                new vscode.ShellExecution(cmdLine[0], cmdLine.slice(1)),
                                DEFAULT_PROBLEM_MATCHER
                            )
                        );
                    }

                    {
                        const def: CustomTaskDefinition = {
                            type: this.taskType,
                            configuration: {
                                kind: 'buildAndRunMain',
                                projectFile: await computeProject(),
                                main: main.mainRelPath(),
                            },
                        };
                        const name = `${allTaskProperties['buildAndRunMain'].title}${basename(
                            main.mainRelPath()
                        )}`;
                        const cmdLineBuildAndRun = await buildFullCommandLine(name, def);
                        this.tasks?.push(
                            new vscode.Task(
                                def,
                                vscode.TaskScope.Workspace,
                                name,
                                this.taskType,
                                new vscode.ShellExecution(
                                    cmdLineBuildAndRun[0],
                                    cmdLineBuildAndRun.slice(1)
                                ),
                                DEFAULT_PROBLEM_MATCHER
                            )
                        );
                    }
                }
            }
        }

        return this.tasks ?? [];
    }
    resolveTask(
        task: vscode.Task,
        // eslint-disable-next-line @typescript-eslint/no-unused-vars
        _token?: vscode.CancellationToken
    ): vscode.ProviderResult<vscode.Task> {
        // This is called for tasks that are not fully resolved, in particular
        // tasks that don't have an undefined 'execution' property.
        const definition = task.definition as CustomTaskDefinition;

        switch (definition.configuration.kind) {
            case 'buildMain':
                break;

            default:
                break;
        }

        // Check that the task is known
        if (definition.configuration.kind in allTaskProperties) {
            return alire().then(async (alr) => {
                const cmd = alr.concat(await buildFullCommandLine(task.name, definition));
                const shell = new vscode.ShellExecution(cmd[0], cmd.slice(1));
                return new vscode.Task(
                    // vscode requires that the returned Task has the same
                    // definition as the input task
                    definition,
                    task.scope ?? vscode.TaskScope.Workspace,
                    task.name,
                    this.taskType,
                    shell,
                    DEFAULT_PROBLEM_MATCHER
                );
            });
        }

        return undefined;
    }
}

export function createSparkTaskProvider(): ConfigurableTaskProvider {
    return new ConfigurableTaskProvider(
        'spark',
        sparkTaskKinds.map((v) => v)
    );
}

export function createAdaTaskProvider(): ConfigurableTaskProvider {
    return new ConfigurableTaskProvider(
        'ada',
        adaTaskKinds.map((v) => v)
    );
}

/**
 *
 * @param task - the task for which to resolve the full command line
 * @param extraArgs - User-provided arguments if the command line is being
 * resolved in the context of an explicit task definition in tasks.json
 * @returns The full command line after adding common arguments and task-specific arguments.
 */
async function buildFullCommandLine(
    name: string,
    taskDef: CustomTaskDefinition,
    extraArgs?: string[]
): Promise<string[]> {
    const task = allTaskProperties[taskDef.configuration.kind];

    let cmd = task.command;

    // Add project and scenario args
    cmd = cmd.concat(await getProjectArgs(taskDef), getScenarioArgs());

    // If the task has a callback to compute extra arguments, call it
    if (task.extra) {
        cmd = cmd.concat(await task.extra());
    }

    const alsProjectFullPath = await getProjectFile(adaExtState.adaClient);
    const alsProjectRelPath = vscode.workspace.asRelativePath(alsProjectFullPath);
    const taskProject = taskDef.configuration.projectFile;

    const taskProjectIsALSProject: boolean =
        [PROJECT_FROM_CONFIG, alsProjectFullPath, alsProjectRelPath].find(
            (v) => v == taskProject
        ) != undefined;

    // Add main argument or other task- and definition-specific args
    switch (taskDef.configuration.kind) {
        case 'buildMain':
        case 'buildAndRunMain': {
            assert(taskDef.configuration.main);

            if (taskProjectIsALSProject) {
                // The task project is the same as the ALS project. Check that the main is found.
                const projectMains = await getAdaMains();
                const adaMain = projectMains.find(
                    (val) =>
                        val.mainRelPath() == taskDef.configuration.main ||
                        val.mainFullPath == taskDef.configuration.main
                );
                if (adaMain) {
                    // A matching main was found. Set the executable field
                    // accordingly if this is a buildAndRunMain task.
                    if (taskDef.configuration.kind == 'buildAndRunMain') {
                        taskDef.configuration.executable = adaMain.execRelPath();
                    }
                } else {
                    const msg =
                        `Task '${name}': ` +
                        `The specified main '${taskDef.configuration.main}' does not ` +
                        `match any value of the Mains attribute of the main GPR project: ` +
                        `${alsProjectRelPath}.`;
                    void vscode.window.showWarningMessage(msg);
                }
            } else {
                // The specified project is not the same as the ALS project. We
                // cannot lookup the main using the ALS. So we can't make any checks.
            }

            // Add the main source file to the build command
            cmd = cmd.concat([taskDef.configuration.main]);

            break;
        }

        default:
            break;
    }

    // Append User args before diagnostic args because the latter use `-cargs`
    if (taskDef.configuration.args) {
        cmd = cmd.concat(taskDef.configuration.args);
    }
    if (extraArgs) {
        cmd = cmd.concat(extraArgs);
    }

    // Append diagnostic args except for gprclean which doesn't need them
    if (cmd[0] != 'gprclean') {
        cmd = cmd.concat(getDiagnosticArgs());
    }

    switch (taskDef.configuration.kind) {
        // Append the run of the main executable
        case 'buildAndRunMain': {
            assert(taskDef.configuration.main);

            if (taskDef.configuration.executable) {
                // The "executable" property is either set explicitly by the
                // User, or automatically by querying the ALS in previous code.
                cmd = cmd.concat('&&', taskDef.configuration.executable);
                if (taskDef.configuration.mainArgs) {
                    cmd = cmd.concat(taskDef.configuration.mainArgs);
                }
            } else {
                if (taskProjectIsALSProject) {
                    // The task project is the same as the ALS project, and apparently we were
                    // unable to find the executable. So warn about it.
                    const msg =
                        `Task '${name}': ` +
                        `Could not compute the executable corresponding to the main source file ` +
                        `'${taskDef.configuration.main}' of the project ` +
                        `${alsProjectRelPath}. The main executable will not be run.`;
                    void vscode.window.showWarningMessage(msg);
                } else {
                    // The specified project is not the same as the ALS project. We
                    // cannot lookup the executable using the ALS. The 'executable'
                    // field must be specified.
                    const msg =
                        `Task '${name}': ` +
                        `The project file specified in this task is different than the workspace ` +
                        `project. It is not possible to automatically compute the path to the ` +
                        `executable to run. Please specify the 'executable' attribute in the ` +
                        `task definition.`;
                    void vscode.window.showWarningMessage(msg);
                }
            }

            break;
        }

        default:
            break;
    }

    // Prepend alire command if available
    return alire().then((alr) => {
        return alr.concat(cmd);
    });
}

/**
 * This class is a {@link vscode.CustomExecution} that displays a warning
 * message as a popup message and in the terminal associated with the task
 * execution. In particular, it is useful for displaying a warning when the User
 * tries to execute an obsolete or malformed task.
 */
export class WarningMessageExecution extends vscode.CustomExecution {
    warningMsg: string;

    constructor(warningMsg: string) {
        super(() => {
            return this.callback();
        });
        this.warningMsg = warningMsg;
    }

    /**
     * This callback is called when the task is executed.
     *
     * @returns a Pseudoterminal object that controls a Terminal in the VS Code UI.
     */
    callback(): Thenable<vscode.Pseudoterminal> {
        return new Promise((resolve) => {
            const writeEmitter = new vscode.EventEmitter<string>();
            const closeEmitter = new vscode.EventEmitter<number>();
            const msg = this.warningMsg;
            const pseudoTerminal: vscode.Pseudoterminal = {
                onDidWrite: writeEmitter.event,
                onDidClose: closeEmitter.event,
                open() {
                    /**
                     * Printing to the terminal is done by firing the onDidWrite event.
                     */
                    writeEmitter.fire(msg + '\r\n\r\n');

                    /**
                     * Display the warning in a popup without awaiting the dismissal of the popup.
                     */
                    void vscode.window.showWarningMessage(msg);

                    /**
                     * Firing the onDidClose event causes the Terminal to end.
                     */
                    closeEmitter.fire(0);
                },
                close() {
                    //
                },
            };
            resolve(pseudoTerminal);
        });
    }
}
