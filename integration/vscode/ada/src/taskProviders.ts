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
import path from 'path';
import * as vscode from 'vscode';
import { SymbolKind } from 'vscode';
import {
    CMD_GPR_PROJECT_ARGS,
    PROJECT_FROM_CONFIG,
    getProjectFromConfigOrALS,
    gprProjectArgs,
    sparkLimitRegionArg,
    sparkLimitSubpArg,
} from './commands';
import { adaExtState } from './extension';
import { AdaMain, getAdaMains, getProjectFile, getSymbols } from './helpers';

export const ADA_TASK_TYPE = 'ada';
export const DEFAULT_PROBLEM_MATCHER = '$ada';

/**
 * Callback to provide an extra argument for a tool
 */
type ExtraArgCallback = () => Promise<string[]>;

/**
 * Tool description
 */
export interface TaskProperties {
    // Executable like gprbuild, gprclean, gnatprove, etc. and static list of
    // arguments.
    command?: string[];
    // Dynamic argument callback called at the time of task execution. Args and
    // extra argument will be wrapped with getGnatArgs if this is set.
    extra?: ExtraArgCallback;
    // Short title displayed in task list
    title: string;
    // Long description displayed in the task list on a separate line
    description?: string;
    // Use project and scenario args. Treated as true if unspecified.
    projectArgs?: boolean;
    // Use -cargs:ada -gnatef to obtain full paths in diagnostics. Treated as true if unspecified.
    diagnosticArgs?: boolean;
}

async function computeProject(taskDef?: CustomTaskDefinition): Promise<string> {
    // If the task definition defines a project file, use that. Otherwise if
    // ada.projectFile is defined, use ${config:ada.projectFile}. Finally,
    // fallback to querying the ALS for the full path to the project file.
    return taskDef?.configuration?.projectFile ?? getProjectFromConfigOrALS();
}

// Call commonArgs on args and append `-gnatef` to generate full file names in errors/warnings
export const getDiagnosticArgs = (): string[] => {
    const p_gnatef = ['-cargs:ada', '-gnatef'];
    return p_gnatef;
};

// The following pair of declarations allow creating a set of string values both
// as an iterable (constant) array, and as a union type.
export const adaTaskKinds = [
    'buildProject',
    'checkFile',
    'cleanProject',
    'buildMain',
    'runMain',
    'buildAndRunMain',
    'gnatsasAnalyze',
    'gnatsasReport',
    'gnatsasAnalyzeAndReport',
    'gnatdoc',
    'gnattest',
] as const;
export type AdaTaskKinds = (typeof adaTaskKinds)[number];

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
        projectFile?: string;
        args?: string[];
        main?: string;
        mainArgs?: string[];
        buildTask?: string;
        runTask?: string;
    };
}

export interface SimpleTaskDef extends vscode.TaskDefinition {
    command?: string | vscode.ShellQuotedString;
    args?: (string | vscode.ShellQuotedString)[];
    compound?: string[];
}

interface TaskDeclaration {
    label: string;
    description?: string;
    taskDef: SimpleTaskDef;
    problemMatchers?: string | string[];
}

const predefinedTasks: TaskDeclaration[] = [
    {
        label: 'Clean current project',
        taskDef: {
            type: ADA_TASK_TYPE,
            command: 'gprclean',
            args: ['${command:ada.gprProjectArgs}'],
        },
        problemMatchers: '',
    },
    {
        label: 'Build current project',
        taskDef: {
            type: ADA_TASK_TYPE,
            command: 'gprbuild',
            args: ['${command:ada.gprProjectArgs}', '-cargs:ada', '-gnatef'],
        },
        problemMatchers: DEFAULT_PROBLEM_MATCHER,
    },
    {
        label: 'Check current file',
        taskDef: {
            type: ADA_TASK_TYPE,
            command: 'gprbuild',
            args: [
                '-q',
                '-f',
                '-c',
                '-u',
                '-gnatc',
                '${command:ada.gprProjectArgs}',
                '${fileBasename}',
                '-cargs:ada',
                '-gnatef',
            ],
        },
        problemMatchers: DEFAULT_PROBLEM_MATCHER,
    },
    {
        label: 'Analyze the project with GNAT SAS',
        taskDef: {
            type: ADA_TASK_TYPE,
            command: 'gnatsas',
            args: ['analyze', '${command:ada.gprProjectArgs}'],
        },
        problemMatchers: '',
    },
    {
        label: 'Analyze the current file with GNAT SAS',
        taskDef: {
            type: ADA_TASK_TYPE,
            command: 'gnatsas',
            args: ['analyze', '${command:ada.gprProjectArgs}', '--file=${fileBasename}'],
        },
        problemMatchers: '',
    },
    {
        label: 'Create a report after a GNAT SAS analysis',
        taskDef: {
            type: ADA_TASK_TYPE,
            command: 'gnatsas',
            args: ['report', 'sarif', '${command:ada.gprProjectArgs}'],
        },
        problemMatchers: '',
    },
    {
        label: 'Analyze the project with GNAT SAS and produce a report',
        taskDef: {
            type: ADA_TASK_TYPE,
            compound: [
                'Analyze the project with GNAT SAS',
                'Create a report after a GNAT SAS analysis',
            ],
        },
        problemMatchers: '',
    },
    {
        label: 'Generate documentation from the project',
        taskDef: {
            type: ADA_TASK_TYPE,
            command: 'gnatdoc',
            args: ['${command:ada.gprProjectArgs}'],
        },
        problemMatchers: '',
    },
    {
        label: 'Create/update test skeletons for the project',
        taskDef: {
            type: ADA_TASK_TYPE,
            command: 'gnattest',
            args: ['${command:ada.gprProjectArgs}'],
        },
        problemMatchers: '',
    },
    /**
     * SPARK
     */
    {
        label: 'Clean project for proof',
        taskDef: {
            type: 'spark',
            command: 'gnatprove',
            args: ['${command:ada.gprProjectArgs}', '--clean'],
        },
        problemMatchers: '',
    },
    {
        label: 'Examine project',
        taskDef: {
            type: 'spark',
            command: 'gnatprove',
            args: ['${command:ada.gprProjectArgs}', '-j0', '--mode=flow', '-cargs', '-gnatef'],
        },
        problemMatchers: DEFAULT_PROBLEM_MATCHER,
    },
    {
        label: 'Examine file',
        taskDef: {
            type: 'spark',
            command: 'gnatprove',
            args: [
                '${command:ada.gprProjectArgs}',
                '-j0',
                '--mode=flow',
                '-u',
                '${fileBasename}',
                '-cargs',
                '-gnatef',
            ],
        },
        problemMatchers: DEFAULT_PROBLEM_MATCHER,
    },
    {
        label: 'Examine subprogram',
        taskDef: {
            type: 'spark',
            command: 'gnatprove',
            args: [
                '${command:ada.gprProjectArgs}',
                '-j0',
                '--mode=flow',
                '${command:ada.spark.limitSubpArg}',
                '-cargs',
                '-gnatef',
            ],
        },
        problemMatchers: DEFAULT_PROBLEM_MATCHER,
    },
    {
        label: 'Prove project',
        taskDef: {
            type: 'spark',
            command: 'gnatprove',
            args: ['${command:ada.gprProjectArgs}', '-j0', '-cargs', '-gnatef'],
        },
        problemMatchers: DEFAULT_PROBLEM_MATCHER,
    },
    {
        label: 'Prove file',
        taskDef: {
            type: 'spark',
            command: 'gnatprove',
            args: [
                '${command:ada.gprProjectArgs}',
                '-j0',
                '-u',
                '${fileBasename}',
                '-cargs',
                '-gnatef',
            ],
        },
        problemMatchers: DEFAULT_PROBLEM_MATCHER,
    },
    {
        label: 'Prove subprogram',
        taskDef: {
            type: 'spark',
            command: 'gnatprove',
            args: [
                '${command:ada.gprProjectArgs}',
                '-j0',
                '${command:ada.spark.limitSubpArg}',
                '-cargs',
                '-gnatef',
            ],
        },
        problemMatchers: DEFAULT_PROBLEM_MATCHER,
    },
    {
        label: 'Prove selected region',
        taskDef: {
            type: 'spark',
            command: 'gnatprove',
            args: [
                '${command:ada.gprProjectArgs}',
                '-j0',
                '-u',
                '${fileBasename}',
                '${command:ada.spark.limitRegionArg}',
                '-cargs',
                '-gnatef',
            ],
        },
        problemMatchers: DEFAULT_PROBLEM_MATCHER,
    },
    {
        label: 'Prove line',
        taskDef: {
            type: 'spark',
            command: 'gnatprove',
            args: [
                '${command:ada.gprProjectArgs}',
                '-j0',
                '-u',
                '${fileBasename}',
                '--limit-line=${fileBasename}:${lineNumber}',
                '-cargs',
                '-gnatef',
            ],
        },
        problemMatchers: DEFAULT_PROBLEM_MATCHER,
    },
];

class SimpleTaskProvider implements vscode.TaskProvider {
    constructor(public taskType: string, private taskDecls: TaskDeclaration[]) {}

    async provideTasks(token: vscode.CancellationToken): Promise<vscode.Task[]> {
        if (token.isCancellationRequested) {
            throw new vscode.CancellationError();
        }

        const result: vscode.Task[] = [];

        /**
         * Start with the list of predefined tasks.
         */
        const taskDeclsToOffer = this.taskDecls.concat();

        if (this.taskType == ADA_TASK_TYPE) {
            /**
             * Add tasks based on the Mains of the project.
             */
            taskDeclsToOffer.push(
                ...(await getAdaMains()).flatMap((main) => {
                    if (token.isCancellationRequested) {
                        throw new vscode.CancellationError();
                    }

                    const buildTask: TaskDeclaration = {
                        label: getBuildTaskPlainName(main),
                        taskDef: {
                            type: this.taskType,
                            command: 'gprbuild',
                            args: [
                                `\${command:${CMD_GPR_PROJECT_ARGS}}`,
                                main.mainRelPath(),
                                '-cargs:ada',
                                '-gnatef',
                            ],
                        },
                        problemMatchers: DEFAULT_PROBLEM_MATCHER,
                    };

                    const runTask: TaskDeclaration = {
                        label: getRunTaskPlainName(main),
                        taskDef: {
                            type: this.taskType,
                            command: main.execRelPath(),
                            args: [],
                        },
                    };

                    const buildAndRunTask: TaskDeclaration = {
                        label: getBuildAndRunTaskName(main),
                        taskDef: {
                            type: this.taskType,
                            compound: [buildTask.label, runTask.label],
                        },
                        problemMatchers: '',
                    };

                    return [buildTask, runTask, buildAndRunTask];
                })
            );
        }

        /**
         * Create vscode.Task objects for all tasks to offer.
         */
        for (const tDecl of taskDeclsToOffer) {
            if (token.isCancellationRequested) {
                throw new vscode.CancellationError();
            }

            let execution = undefined;
            if (tDecl.taskDef.compound) {
                /**
                 * It's a compound task.
                 */
                execution = new SequentialExecutionByName(tDecl.label, tDecl.taskDef.compound);
            } else {
                /**
                 * It's a shell invocation task.
                 */
                assert(tDecl.taskDef.command);
                assert(tDecl.taskDef.args);

                /**
                 * Ideally we would have liked to provide unresolved tasks and
                 * let resolving only happen in the resolveTask method, but
                 * that's not how VS Code works. This method is expected to
                 * return fully resolved tasks, hence we must provide an
                 * execution object with arguments evaluated now.
                 */
                execution = new vscode.ShellExecution(
                    tDecl.taskDef.command,
                    await evaluateArgs(tDecl.taskDef.args)
                );
            }

            const task = new vscode.Task(
                tDecl.taskDef,
                vscode.TaskScope.Workspace,
                tDecl.label,
                tDecl.taskDef.type,
                execution,
                tDecl.problemMatchers
            );

            result.push(task);
        }

        return result;
    }

    async resolveTask(task: vscode.Task, token: vscode.CancellationToken): Promise<vscode.Task> {
        /**
         * Note that this method is never called for tasks created by the
         * provideTasks method above (see parent method documentation). It is
         * called for tasks defined (/customized by the user) in the tasks.json
         * file.
         */

        if (token.isCancellationRequested) {
            throw new vscode.CancellationError();
        }

        if ('configuration' in task.definition) {
            /**
             * This is an obsolete configuration, so use the old mechanism.
             */
            return createOrResolveTask(
                task.definition as CustomTaskDefinition,
                undefined,
                undefined,
                task
            );
        }

        const taskDef = task.definition as SimpleTaskDef;

        /**
         * Validate the task.
         */
        if (!(taskDef.compound || taskDef.command)) {
            /**
             * We allow args to be unspecified, but command has to.
             */
            const msg =
                `A task of type '${this.taskType}' must specify either the 'command' and 'args' ` +
                "properties or the 'compound' property.";
            void vscode.window.showErrorMessage(msg);
            throw Error(msg);
        }

        if (taskDef.compound && (taskDef.command || taskDef.args)) {
            const msg =
                `A task of type '${this.taskType}' must specify either the 'command' and 'args' ` +
                "properties or the 'compound' property, but not both.";
            void vscode.window.showErrorMessage(msg);
            throw Error(msg);
        }

        /**
         * Resolve the task.
         */
        let execution;
        if (taskDef.compound) {
            assert(!taskDef.command);
            assert(!taskDef.args);
            execution = new SequentialExecutionByName(task.name, taskDef.compound);
        } else {
            assert(taskDef.command);
            /**
             * We support working with just the command property, in which case
             * fallback to an empty args array.
             */
            const args = taskDef.args ?? [];
            const evaluatedArgs: (string | vscode.ShellQuotedString)[] = await evaluateArgs(args);
            execution = new vscode.ShellExecution(taskDef.command, evaluatedArgs);
        }

        return new vscode.Task(
            task.definition,
            task.scope ?? vscode.TaskScope.Workspace,
            task.name,
            task.source,
            execution,
            task.problemMatchers
        );
    }
}

/**
 * Evaluate task arguments with support for commands returning arrays (VS Code
 * native evaluation of task arguments only allows commands returning a plain
 * string).
 *
 * @param args - an array of command line arguments from a task
 * @returns the array of arguments where items matching the pattern
 * '$\{command:ada.*\} have been evaluated. If they return an array of strings,
 * then the array is inserted into the argument array at the location of the
 * command.
 */
async function evaluateArgs(args: (string | vscode.ShellQuotedString)[]) {
    const commandRegex = new RegExp(/^\${command:\s*((ada|spark)\.[^}]*)\s*}$/);
    const evaluatedArgs: (string | vscode.ShellQuotedString)[] = (
        await Promise.all(
            args.flatMap(async function (
                a: string | vscode.ShellQuotedString
            ): Promise<(string | vscode.ShellQuotedString)[]> {
                if (typeof a == 'string') {
                    /**
                     * Perform command evaluation in strings, not in ShellQuotedStrings
                     */
                    const match = a.match(commandRegex);
                    if (match) {
                        /**
                         * The string matches an ada.* command, so evaluate it.
                         */
                        const command = match[1];
                        const evalRes = await vscode.commands.executeCommand(command);
                        if (typeof evalRes == 'string') {
                            /**
                             * Result is a string so wrap it in an array for flattening
                             */
                            return [evalRes];
                        } else if (isNonEmptyStringArray(evalRes)) {
                            /**
                             * Return the array result
                             */
                            return evalRes as string[];
                        } else {
                            /**
                             * Do not use the evaluated result. The original value
                             * will be returned below.
                             */
                        }
                    }
                }

                return [a];
            })
        )
    ).flat();
    return evaluatedArgs;
}

function isNonEmptyStringArray(obj: unknown): boolean {
    if (obj instanceof Array) {
        if (obj.length > 0) {
            if (typeof obj[0] == 'string') {
                return true;
            }
        }
    }

    return false;
}

/**
 * Map of known tasks/tools indexed by a string/taskKind
 */
export const allTaskProperties: { [id in AllTaskKinds]: TaskProperties } = {
    cleanProjectForProof: {
        command: ['gnatprove', '--clean'],
        title: 'Clean project for proof',
        diagnosticArgs: false,
    },
    examineProject: {
        command: ['gnatprove', '-j0', '--mode=flow'],
        title: 'Examine project',
    },
    examineFile: {
        command: ['gnatprove', '-j0', '--mode=flow', '-u', '${fileBasename}'],
        title: 'Examine file',
    },
    examineSubprogram: {
        command: ['gnatprove', '-j0', '--mode=flow'],
        extra: sparkLimitSubpArg,
        title: 'Examine subprogram',
    },
    proveProject: {
        command: ['gnatprove', '-j0'],
        title: 'Prove project',
    },
    proveFile: {
        command: ['gnatprove', '-j0', '-u', '${fileBasename}'],
        title: 'Prove file',
    },
    proveSubprogram: {
        command: ['gnatprove', '-j0'],
        extra: sparkLimitSubpArg,
        title: 'Prove subprogram',
    },
    proveRegion: {
        command: ['gnatprove', '-j0', '-u', '${fileBasename}'],
        extra: sparkLimitRegionArg,
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
        title: 'Prove line',
    },
    buildProject: {
        command: ['gprbuild'],
        title: 'Build current project',
    },
    checkFile: {
        command: ['gprbuild', '-q', '-f', '-c', '-u', '-gnatc', '${fileBasename}'],
        title: 'Check current file',
    },
    cleanProject: {
        command: ['gprclean'],
        title: 'Clean current project',
        diagnosticArgs: false,
    },
    buildMain: {
        command: ['gprbuild'],
        title: 'Build main - ',
    },
    runMain: {
        command: [],
        title: 'Run main - ',
        projectArgs: false,
        diagnosticArgs: false,
    },
    buildAndRunMain: {
        title: 'Build and run main - ',
        // description: 'Run the build task followed by the run task for the given main',
    },
    gnatsasAnalyze: {
        command: ['gnatsas', 'analyze'],
        title: 'Analyze the project with GNAT SAS',
        diagnosticArgs: false,
    },
    gnatsasReport: {
        command: ['gnatsas', 'report'],
        title: 'Create a report after a GNAT SAS analysis',
        // We set this flag to false because project args are added later as
        // part of the 'args' task property
        projectArgs: false,
        diagnosticArgs: false,
    },
    gnatsasAnalyzeAndReport: {
        title: 'Analyze the project with GNAT SAS and produce a report',
    },
    gnatdoc: {
        command: ['gnatdoc'],
        title: 'Generate documentation from the project',
        diagnosticArgs: false,
    },
    gnattest: {
        command: ['gnattest'],
        title: 'Create/update test skeletons for the project',
        diagnosticArgs: false,
    },
};

// eslint-disable-next-line max-len
export const BUILD_PROJECT_TASK_NAME = `${ADA_TASK_TYPE}: ${allTaskProperties['buildProject'].title}`;

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
 * This function returns a fully resolved task, either based on a
 * TaskDefinition, or on an incomplete task to be resolved.
 *
 * @param definition - CustomTaskDefinition to base the new task on. If 'task'
 * is also given, then it must be that `definition == task.definition`.
 * @param commandPrefix - a prefix for the command of the new task
 * @param name - the name to give the new task
 * @param task - the task to be resolved
 *
 * @returns a new fully resolved task based on the definition or based on the
 * incomplete task given.
 */
async function createOrResolveTask(
    definition: CustomTaskDefinition,
    commandPrefix: string[] = [],
    name?: string,
    task?: vscode.Task
): Promise<vscode.Task> {
    if (task) {
        assert(definition == task.definition);
    }

    name = name ?? task?.name ?? allTaskProperties[definition.configuration.kind].title;

    let execution;
    if (definition.configuration.kind == 'buildAndRunMain') {
        execution = new BuildAndRunExecution(definition);
    } else if (definition.configuration.kind == 'gnatsasAnalyzeAndReport') {
        execution = new SequentialExecutionByName(name, [
            allTaskProperties['gnatsasAnalyze'].title,
            allTaskProperties['gnatsasReport'].title,
        ]);
    } else {
        /**
         * Quote the command line so that no shell interpretations can happen.
         */
        const cmd = quoteCommandLine(
            commandPrefix.concat(await buildFullCommandLine(name, definition))
        );

        /**
         * It is necessary to use a ShellExecution instead of a ProcessExecution to
         * go through a terminal where terminal.integrated.env.* is applicable and
         * tools can be resolved and can run according to the User's environment
         * settings. Alternatively, a ProcessExecution could be used if the
         * extension resolves the full path to the called executable and passes the
         * terminal.integrated.env.* environment to the child process. But this is
         * deemed overkill for the moment.
         */
        execution = new vscode.ShellExecution(cmd[0], cmd.slice(1));
    }

    /**
     * If task was given to be resolved, use its properties in priority.
     */
    const newTask = new vscode.Task(
        definition,
        task?.scope ?? vscode.TaskScope.Workspace,
        name,
        // Always use the task type as a source string in the UI for consistency
        // between the tasks.json definitions and what Users see in the UI
        definition.type,
        execution,
        []
    );

    newTask.detail = task?.detail ?? allTaskProperties[definition.configuration.kind].description;

    switch (definition.configuration.kind) {
        case 'buildProject':
        case 'buildMain':
            newTask.group = vscode.TaskGroup.Build;
            newTask.problemMatchers = [DEFAULT_PROBLEM_MATCHER];
            break;
        case 'cleanProject':
        case 'cleanProjectForProof': {
            newTask.group = vscode.TaskGroup.Clean;
            newTask.problemMatchers = [DEFAULT_PROBLEM_MATCHER];
            break;
        }

        case 'checkFile':
        case 'examineProject':
        case 'examineFile':
        case 'examineSubprogram':
        case 'proveProject':
        case 'proveFile':
        case 'proveSubprogram':
        case 'proveRegion':
        case 'proveLine':
        case 'gnattest':
        case 'gnatdoc':
            /**
             * Tasks that can issue problems
             */
            newTask.problemMatchers = [DEFAULT_PROBLEM_MATCHER];
            break;

        case 'runMain':
        case 'buildAndRunMain':
        case 'gnatsasAnalyze':
        case 'gnatsasReport':
        case 'gnatsasAnalyzeAndReport':
            /**
             * Tasks that don't issue problems
             */
            break;
    }

    return newTask;
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
        const filtered_symbols = getSymbols(symbols, kinds, [
            SymbolKind.Function,
            SymbolKind.Module,
        ]);

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

export const getSelectedRegion = (editor: vscode.TextEditor | undefined): string => {
    if (editor) {
        const selection = editor.selection;
        //  Line numbers start at 0 in VS Code, and at 1 in GNAT
        return (selection.start.line + 1).toString() + ':' + (selection.end.line + 1).toString();
    } else {
        return '0:0';
    }
};

export function registerTaskProviders() {
    return [
        vscode.tasks.registerTaskProvider('ada', createAdaTaskProvider()),
        vscode.tasks.registerTaskProvider('spark', createSparkTaskProvider()),
    ];
}

/**
 * The name of the build task of a main, without the task type.
 */
function getBuildTaskPlainName(main: AdaMain) {
    return `${allTaskProperties['buildMain'].title}${main.mainRelPath()}`;
}

/**
 * The full name of the build task of a main, including the task type.
 */
export function getBuildTaskName(main: AdaMain) {
    return `ada: ${getBuildTaskPlainName(main)}`;
}

/**
 * The name of the run task of a main, without the task type.
 */
function getRunTaskPlainName(main: AdaMain) {
    return `${allTaskProperties['runMain'].title}${main.mainRelPath()}`;
}

/**
 * The full name of the build task of a main, including the task type.
 */
export function getRunTaskName(main: AdaMain) {
    return `ada: ${getRunTaskPlainName(main)}`;
}

export function getBuildAndRunTaskName(main: AdaMain) {
    return `${allTaskProperties['buildAndRunMain'].title}${main.mainRelPath()}`;
}

export function createSparkTaskProvider(): vscode.TaskProvider {
    return new SimpleTaskProvider(
        'spark',
        predefinedTasks.filter((v) => v.taskDef.type == 'spark')
    );
}

export function createAdaTaskProvider(): vscode.TaskProvider {
    return new SimpleTaskProvider(
        ADA_TASK_TYPE,
        predefinedTasks.filter((v) => v.taskDef.type == ADA_TASK_TYPE)
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

    let cmd = task.command?.concat() ?? [];

    if (task.projectArgs === undefined || task.projectArgs) {
        // Add project and scenario args
        cmd = cmd.concat(await getProjectArgs(taskDef), getScenarioArgs());
    }

    // If the task has a callback to compute extra arguments, call it. This is
    // used e.g. to get the current file or location for tasks that call SPARK
    // on a specific location.
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

    // Determine main in the case of tasks based on a main
    let adaMain;
    switch (taskDef.configuration.kind) {
        case 'runMain':
        case 'buildMain': {
            assert(taskDef.configuration.main);

            if (taskProjectIsALSProject) {
                // The task project is the same as the ALS project. Check that the main is found.
                adaMain = await getAdaMain(taskDef);
                if (adaMain) {
                    // A matching main was found. Continue normally.
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

            break;
        }
        default:
            break;
    }

    // Add task- and definition-specific args
    if (taskDef.configuration.kind == 'buildMain') {
        assert(taskDef.configuration.main);

        // Add the main source file to the build command
        cmd = cmd.concat([taskDef.configuration.main]);
    }

    // Append User args before diagnostic args because the latter use `-cargs`
    if (taskDef.configuration.args && taskDef.configuration.args.length > 0) {
        const gprProjectArgsCmd = `\${command:${CMD_GPR_PROJECT_ARGS}}`;
        const evaluatedArgs: string[] = (
            await Promise.all(
                taskDef.configuration.args.map(async (a) => {
                    if (a == gprProjectArgsCmd) {
                        return await gprProjectArgs();
                    } else {
                        return [a];
                    }
                })
            )
        ).flat();
        cmd = cmd.concat(evaluatedArgs);
    }
    if (extraArgs) {
        cmd = cmd.concat(extraArgs);
    }

    // Append diagnostic args if needed
    if (task.diagnosticArgs === undefined || task.diagnosticArgs) {
        cmd = cmd.concat(getDiagnosticArgs());
    }

    if (taskDef.configuration.kind == 'runMain') {
        if (adaMain) {
            // Append the main executable's relative path, prepending './'
            // (or '.\\' on Windows) when needed, to make sure it's executable from
            // a shell.
            let execRelPath = adaMain.execRelPath();
            if (!execRelPath.includes(path.sep)) {
                execRelPath = '.' + path.sep + execRelPath;
            }

            cmd.push(execRelPath);
            if (taskDef.configuration.mainArgs) {
                cmd = cmd.concat(taskDef.configuration.mainArgs);
            }
        } else {
            assert(taskDef.configuration.main);

            if (taskProjectIsALSProject) {
                // The task project is the same as the ALS project, and apparently we were
                // unable to find the executable. We already warned about it before.
            } else {
                // The specified project is not the same as the ALS project. We
                // cannot lookup the executable using the ALS. Another task type
                // must be used.
                const msg =
                    `Task '${name}': ` +
                    `The project file specified in this task is different than the workspace ` +
                    `project. It is not possible to automatically compute the path to the ` +
                    `executable to run. Please use a task of type 'process' or 'shell' to ` +
                    `invoke the executable directly.`;
                void vscode.window.showWarningMessage(msg);
            }
        }
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

/**
 * This is an abstract class providing the scaffolding for running multiple
 * tests in sequence. Child classes can override methods to customize which
 * tasks should executed.
 */
abstract class SequentialExecution extends vscode.CustomExecution {
    constructor() {
        super(() => {
            return this.callback();
        });
    }

    /**
     * This callback is called when the task is executed.
     *
     * @returns a Pseudoterminal object that controls a Terminal in the VS Code UI.
     */
    protected callback(): Thenable<vscode.Pseudoterminal> {
        return new Promise((resolve) => {
            const writeEmitter = new vscode.EventEmitter<string>();
            const closeEmitter = new vscode.EventEmitter<number>();
            const pseudoTerminal: vscode.Pseudoterminal = {
                onDidWrite: writeEmitter.event,
                onDidClose: closeEmitter.event,
                open: () => {
                    this.getTasksToRun()
                        .then((tasks) => {
                            return runTaskSequence(tasks, writeEmitter);
                        })
                        .then(
                            (status) => {
                                closeEmitter.fire(status);
                            },
                            (reason) => {
                                try {
                                    if (reason instanceof Error) {
                                        void vscode.window.showErrorMessage(reason.message);
                                        writeEmitter.fire(reason.message + '\r\n');
                                    }
                                } finally {
                                    closeEmitter.fire(2);
                                }
                            }
                        );
                },
                close() {
                    //
                },
            };
            resolve(pseudoTerminal);
        });
    }

    protected abstract getTasksToRun(): Promise<vscode.Task[]>;
}

/**
 * Task names contributed by the extension don't have the task type prefix
 * while tasks coming from the workspace typically do since VS Code includes
 * the type prefix when converting an automatic extension task into a
 * configurable workspace task. getConventionalTaskLabel() takes care of
 * that fact.
 *
 * @returns the task that has the given name, or the given name with the
 * prefix `ada: `
 */
export function findTaskByName(tasks: vscode.Task[], taskName: string): vscode.Task {
    const task = tasks.find((v) => {
        return taskName == getConventionalTaskLabel(v) || taskName == v.name;
    });
    if (task) {
        return task;
    } else {
        const msg = `Could not find a task named: ${taskName}`;
        throw Error(msg);
    }
}

/**
 * This task execution implements the 'buildAndRunMain' task kind. It is
 * initialized with a 'buildAndRunMain' task definition. When executed, it looks
 * up the build tasks and run tasks corresponding to the main targeted by the
 * task definition, and runs them in sequence.
 *
 */
class BuildAndRunExecution extends SequentialExecution {
    buildAndRunDef: CustomTaskDefinition;

    constructor(buildAndRunDef: CustomTaskDefinition) {
        super();
        assert(buildAndRunDef.configuration.kind == 'buildAndRunMain');
        this.buildAndRunDef = buildAndRunDef;
    }

    protected async getTasksToRun() {
        const adaTasks = await vscode.tasks.fetchTasks({ type: 'ada' });
        assert(this.buildAndRunDef.configuration.buildTask);
        assert(this.buildAndRunDef.configuration.runTask);
        /**
         * Find the tasks that match the task names
         * specified in buildTask and runTask, prioritizing
         * Workspace tasks.
         */
        adaTasks.sort(workspaceTasksFirst);
        const buildMainTask = findTaskByName(adaTasks, this.buildAndRunDef.configuration.buildTask);
        const runMainTask = findTaskByName(adaTasks, this.buildAndRunDef.configuration.runTask);
        return [buildMainTask, runMainTask];
    }
}

/**
 * This class is a task execution that runs other tasks in sequence. The names
 * of the tasks to run are given at construction.
 */
class SequentialExecutionByName extends SequentialExecution {
    constructor(private taskName: string, private taskNames: string[]) {
        super();
    }

    protected async getTasksToRun(): Promise<vscode.Task[]> {
        const adaTasks = await vscode.tasks.fetchTasks({ type: 'ada' });
        return this.taskNames.map((name) => findTaskByName(adaTasks, name));
    }
}

/**
 * Runs a list of tasks in sequence, as long as the execution succeeds. The
 * sequence stops if a task ends with a failure status, or when all tasks
 * complete successfully.
 *
 * @param tasks - list of tasks to run in sequence.
 * @returns Status of the last executed task.
 */
function runTaskSequence(
    tasks: vscode.Task[],
    writeEmitter: vscode.EventEmitter<string>
): Promise<number> {
    let p = new Promise<number>((resolve) => resolve(0));
    for (const t of tasks) {
        p = p.then((status) => {
            if (status == 0) {
                return new Promise<number>((resolve, reject) => {
                    const disposable = vscode.tasks.onDidEndTaskProcess((e) => {
                        if (e.execution.task == t) {
                            disposable.dispose();
                            resolve(e.exitCode ?? 1);
                        }
                    });

                    writeEmitter.fire(`Executing task: ${getConventionalTaskLabel(t)}\r\n`);
                    vscode.tasks.executeTask(t).then(undefined, (reason) => {
                        // eslint-disable-next-line @typescript-eslint/restrict-template-expressions
                        writeEmitter.fire(`Could not execute task: ${reason}\r\n`);
                        reject(reason);
                    });
                });
            } else {
                return status;
            }
        });
    }
    return p;
}

/**
 * A sorting function that puts tasks defined by the User in the workspace first.
 */
const workspaceTasksFirst = (a: vscode.Task, b: vscode.Task): number => {
    if (a.source == b.source) {
        return a.name.localeCompare(b.name);
    } else if (isFromWorkspace(a)) {
        return -1;
    } else {
        return 1;
    }
};

/**
 *
 * @returns Array of tasks of type `ada` and kind `buildAndRunMain`. This
 * includes tasks automatically provided by the extension as well as
 * user-defined tasks in tasks.json.
 */
export async function getBuildAndRunTasks(): Promise<vscode.Task[]> {
    return await vscode.tasks.fetchTasks({ type: 'ada' }).then((tasks) =>
        tasks
            .filter(
                (t) =>
                    (t.definition as CustomTaskDefinition).configuration.kind == 'buildAndRunMain'
            )
            // Return workspace-defined tasks first
            .sort(workspaceTasksFirst)
    );
}

export async function findBuildAndRunTask(adaMain: AdaMain): Promise<vscode.Task | undefined> {
    return (await getBuildAndRunTasks()).find(
        // Tasks defined in tasks.json will have a leading 'ada: ' while the
        // ones auto-generated by the extension don't. We want to match both.
        (t) => t.name.replace(/^ada: /, '') == getBuildAndRunTaskName(adaMain)
    );
}

/**
 *
 * @param taskDef - a task definition with a defined main
 * @returns the {@link AdaMain} object representing the main program
 */
async function getAdaMain(taskDef: CustomTaskDefinition): Promise<AdaMain | undefined> {
    assert(taskDef.configuration.main);
    const projectMains = await getAdaMains();
    return projectMains.find(
        (val) =>
            val.mainRelPath() == taskDef.configuration.main ||
            val.mainFullPath == taskDef.configuration.main
    );
}

/**
 * Convert a command line into a list of strongly quoted
 * {@link vscode.ShellQuotedString} that would be processed verbatim by any
 * shells without interpretation or expansion of special symbols.
 *
 * @param cmd - a list of strings representing a command line
 * @returns a list of strongly quoted {@link ShellQuotedString}
 */
function quoteCommandLine(cmd: string[]): vscode.ShellQuotedString[] {
    return cmd.map((v) => ({ value: v, quoting: vscode.ShellQuoting.Strong }));
}

/**
 *
 * @param task - a task
 * @returns `true` if the task is defined explicitly in the workspace's tasks.json
 */
export function isFromWorkspace(task: vscode.Task): boolean {
    return task.source == 'Workspace';
}

/**
 *
 * @param task - a task
 * @returns the label typically generated for that task by vscode. For tasks not
 * defined explicitly in the workspace, this is `ada: <task name>`. For tasks
 * defined in the workspace simply return the name which should already include
 * the convention.
 */
export function getConventionalTaskLabel(task: vscode.Task): string {
    return isFromWorkspace(task) ? task.name : `${task.source}: ${task.name}`;
}

/*
 * @returns Array of tasks of type `ada` and kind `buildMain`. This
 * includes tasks automatically provided by the extension as well as
 * user-defined tasks in tasks.json.
 */
export async function getBuildMainTasks() {
    return await vscode.tasks.fetchTasks({ type: 'ada' }).then((tasks) =>
        tasks
            .filter((t) => (t.definition as CustomTaskDefinition).configuration.kind == 'buildMain')
            // Return workspace-defined tasks first
            .sort(workspaceTasksFirst)
    );
}

export async function findBuildMainTask(adaMain: AdaMain): Promise<vscode.Task | undefined> {
    return (await getBuildMainTasks()).find(
        // Tasks defined in tasks.json will have a leading 'ada: ' while the
        // ones auto-generated by the extension don't. We want to match both.
        (t) => t.name.replace(/^ada: /, '') == getBuildTaskPlainName(adaMain)
    );
}
