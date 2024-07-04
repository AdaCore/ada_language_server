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
import { basename } from 'path';
import * as vscode from 'vscode';
import { CMD_GPR_PROJECT_ARGS } from './commands';
import { adaExtState } from './extension';
import { AdaMain, getAdaMains } from './helpers';

export const TASK_TYPE_ADA = 'ada';
export const TASK_TYPE_SPARK = 'spark';
export const DEFAULT_PROBLEM_MATCHER = '$ada';

/**
 * A type representing task definitions as they appear in tasks.json files.
 */
export interface SimpleTaskDef extends vscode.TaskDefinition {
    /**
     * The name of the executable to invoke.
     */
    command?: string | vscode.ShellQuotedString;
    /**
     * Arguments to pass to the invocation.
     */
    args?: (string | vscode.ShellQuotedString)[];
    /**
     * This property should not occur at the same time as command and args.
     * {@link SimpleTaskProvider.resolveTask} checks that in case it occurs in
     * User task definitions and raises errors accordingly.
     */
    compound?: string[];
}

/**
 * An internal type to represent predefined tasks that will offered by the extension.
 */
interface PredefinedTask {
    label: string;
    description?: string;
    taskDef: SimpleTaskDef;
    problemMatchers?: string | string[];
    taskGroup?: vscode.TaskGroup;
}

/**
 * The main build project task defined as a constant to allow it to be referenced directly.
 */
const TASK_BUILD_PROJECT: PredefinedTask = {
    label: 'Build current project',
    taskDef: {
        type: TASK_TYPE_ADA,
        command: 'gprbuild',
        args: ['${command:ada.gprProjectArgs}', '-cargs:ada', '-gnatef'],
    },
    problemMatchers: DEFAULT_PROBLEM_MATCHER,
    taskGroup: vscode.TaskGroup.Build,
};
// eslint-disable-next-line max-len
export const BUILD_PROJECT_TASK_NAME = `${TASK_BUILD_PROJECT.taskDef.type}: ${TASK_BUILD_PROJECT.label}`;

const TASK_CLEAN_PROJECT = {
    label: 'Clean current project',
    taskDef: {
        type: TASK_TYPE_ADA,
        command: 'gprclean',
        args: ['${command:ada.gprProjectArgs}'],
    },
    problemMatchers: '',
    taskGroup: vscode.TaskGroup.Clean,
};

export const TASK_PROVE_SUPB_PLAIN_NAME = 'Prove subprogram';

/**
 * Predefined tasks offered by the extension. Both 'ada' and 'spark' tasks are
 * included in this array. They are later on split and provided by different
 * task providers.
 */
const predefinedTasks: PredefinedTask[] = [
    /**
     * Ada
     */
    TASK_CLEAN_PROJECT,
    TASK_BUILD_PROJECT,
    {
        label: 'Check current file',
        taskDef: {
            type: TASK_TYPE_ADA,
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
        label: 'Compile current file',
        taskDef: {
            type: TASK_TYPE_ADA,
            command: 'gprbuild',
            args: [
                '-q',
                '-f',
                '-c',
                '-u',
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
            type: TASK_TYPE_ADA,
            command: 'gnatsas',
            args: ['analyze', '${command:ada.gprProjectArgs}'],
        },
        /**
         * Analysis results are not printed on stdio so no need to parse them
         * with a problem matcher. Results should be viewed with the
         * `gnatsas report` task below
         */
        problemMatchers: '',
    },
    {
        label: 'Analyze the current file with GNAT SAS',
        taskDef: {
            type: TASK_TYPE_ADA,
            command: 'gnatsas',
            args: ['analyze', '${command:ada.gprProjectArgs}', '--file=${fileBasename}'],
        },
        /**
         * Analysis results are not printed on stdio so no need to parse them
         * with a problem matcher. Results should be viewed with the
         * `gnatsas report` task below
         */
        problemMatchers: '',
    },
    {
        label: 'Create a report after a GNAT SAS analysis',
        taskDef: {
            type: TASK_TYPE_ADA,
            command: 'gnatsas',
            args: ['report', 'sarif', '${command:ada.gprProjectArgs}', '-o', 'report.sarif'],
        },
        /**
         * Analysis results are not printed on stdio so no need to parse them
         * with a problem matcher.
         */
        problemMatchers: '',
    },
    {
        label: 'Analyze the project with GNAT SAS and produce a report',
        taskDef: {
            type: TASK_TYPE_ADA,
            compound: [
                'Analyze the project with GNAT SAS',
                'Create a report after a GNAT SAS analysis',
            ],
        },
        /**
         * Analysis results are not printed on stdio so no need to parse them
         * with a problem matcher.
         */
        problemMatchers: '',
    },
    {
        label: 'Analyze the current file with GNAT SAS and produce a report',
        taskDef: {
            type: TASK_TYPE_ADA,
            compound: [
                'Analyze the current file with GNAT SAS',
                'Create a report after a GNAT SAS analysis',
            ],
        },
        /**
         * Analysis results are not printed on stdio so no need to parse them
         * with a problem matcher.
         */
        problemMatchers: '',
    },
    {
        label: 'Generate documentation from the project',
        taskDef: {
            type: TASK_TYPE_ADA,
            command: 'gnatdoc',
            args: ['${command:ada.gprProjectArgs}'],
        },
        problemMatchers: '',
    },
    {
        label: 'Create/update test skeletons for the project',
        taskDef: {
            type: TASK_TYPE_ADA,
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
            type: TASK_TYPE_SPARK,
            command: 'gnatprove',
            args: ['${command:ada.gprProjectArgs}', '--clean'],
        },
        problemMatchers: '',
    },
    {
        label: 'Examine project',
        taskDef: {
            type: TASK_TYPE_SPARK,
            command: 'gnatprove',
            args: ['${command:ada.gprProjectArgs}', '-j0', '--mode=flow', '-cargs', '-gnatef'],
        },
        problemMatchers: DEFAULT_PROBLEM_MATCHER,
    },
    {
        label: 'Examine file',
        taskDef: {
            type: TASK_TYPE_SPARK,
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
            type: TASK_TYPE_SPARK,
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
            type: TASK_TYPE_SPARK,
            command: 'gnatprove',
            args: ['${command:ada.gprProjectArgs}', '-j0', '-cargs', '-gnatef'],
        },
        problemMatchers: DEFAULT_PROBLEM_MATCHER,
    },
    {
        label: 'Prove file',
        taskDef: {
            type: TASK_TYPE_SPARK,
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
        label: TASK_PROVE_SUPB_PLAIN_NAME,
        taskDef: {
            type: TASK_TYPE_SPARK,
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
            type: TASK_TYPE_SPARK,
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
            type: TASK_TYPE_SPARK,
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

/**
 * A provider of tasks based on the {@link SimpleTaskDef} task definition.
 *
 * It is instantiated with a string task type and an array of {@link
 * PredefinedTask} to provide. This way the same class can be reused to provide
 * both 'ada' and 'spark' tasks.
 */
export class SimpleTaskProvider implements vscode.TaskProvider {
    constructor(public taskType: string, private taskDecls: PredefinedTask[]) {}

    async provideTasks(token?: vscode.CancellationToken): Promise<vscode.Task[]> {
        if (token?.isCancellationRequested) {
            throw new vscode.CancellationError();
        }

        const result: vscode.Task[] = [];

        /**
         * Start with the list of predefined tasks.
         */
        let taskDeclsToOffer = this.taskDecls.concat();

        if (this.taskType == TASK_TYPE_ADA) {
            /**
             * Add tasks based on the Mains of the project.
             */
            taskDeclsToOffer.push(
                ...(await getAdaMains()).flatMap((main) => {
                    if (token?.isCancellationRequested) {
                        throw new vscode.CancellationError();
                    }

                    const buildTask: PredefinedTask = {
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
                        taskGroup: vscode.TaskGroup.Build,
                    };

                    const runTask: PredefinedTask = {
                        label: getRunTaskPlainName(main),
                        taskDef: {
                            type: this.taskType,
                            command: main.execRelPath(),
                            args: [],
                        },
                    };

                    const buildAndRunTask: PredefinedTask = {
                        label: getBuildAndRunTaskPlainName(main),
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
         * Port tasks to ALIRE if applicable
         */
        if (await useAlire()) {
            taskDeclsToOffer = taskDeclsToOffer.map((t) => ({
                ...t,
                taskDef: updateToAlire(t.taskDef),
            }));
        }

        /**
         * Create vscode.Task objects for all tasks to offer.
         */
        for (const tDecl of taskDeclsToOffer) {
            if (token?.isCancellationRequested) {
                throw new vscode.CancellationError();
            }

            const task = new vscode.Task(
                tDecl.taskDef,
                vscode.TaskScope.Workspace,
                tDecl.label,
                tDecl.taskDef.type,
                undefined,
                tDecl.problemMatchers
            );

            if (tDecl.taskGroup) {
                task.group = tDecl.taskGroup;
            }

            /**
             * Ideally we would have liked to provide unresolved tasks and let
             * resolving only happen in the resolveTask method, but that's not
             * how VS Code works. This method is expected to return fully
             * resolved tasks, hence we must resolve pre-defined tasks here.
             */
            const resolvedTask = await this.resolveTask(task, token);
            assert(resolvedTask);

            result.push(resolvedTask);
        }

        return result;
    }

    async resolveTask(
        task: vscode.Task,
        token?: vscode.CancellationToken
    ): Promise<vscode.Task | undefined> {
        /**
         * Note that this method is never called for tasks created by the
         * provideTasks method above (see parent method documentation). It is
         * called for tasks defined (/customized by the user) in the tasks.json
         * file.
         */

        if (token?.isCancellationRequested) {
            throw new vscode.CancellationError();
        }
        /**
         * Validate that the task is based on a {@link SimpleTaskDef} and has
         * the expected structure.
         */
        this.validateTask(task);

        const taskDef = task.definition as SimpleTaskDef;

        /**
         * Resolve the task.
         */
        let execution;
        if (taskDef.compound) {
            /**
             * It's a compound task.
             */
            assert(!taskDef.command);
            assert(!taskDef.args);
            execution = new SequentialExecutionByName(task.name, taskDef.compound);
        } else {
            /**
             * It's a shell invocation task.
             */
            assert(taskDef.command);
            /**
             * We support working with just the command property, in which case
             * fallback to an empty args array.
             */
            const args = taskDef.args ?? [];
            try {
                const evaluatedArgs: (string | vscode.ShellQuotedString)[] = await evaluateArgs(
                    args
                );
                execution = new vscode.ShellExecution(taskDef.command, evaluatedArgs);
            } catch (err) {
                let msg = 'Error while evaluating task arguments.';
                if (err instanceof Error) {
                    msg += ' ' + err.message;
                }
                void vscode.window.showErrorMessage(msg);
                return undefined;
            }
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

    /**
     *
     * Validate that the given task is based on a {@link SimpleTaskDef} and has
     * the expected structure, e.g. command/args properties should not occur
     * with the compound property. The method displays an error message in the
     * UI and throws an error in case of violations.
     *
     * @param task - a User-defined {@link vscode.Task}
     */
    private validateTask(task: vscode.Task): void {
        if ('configuration' in task.definition) {
            const msg = `You are trying to use a '${task.definition.type}' task with an
                obsolete property 'configuration' that is no longer supported.
                It is recommended to remove this task from your workspace
                configuration and use tasks automatically provided by the
                extension or customize them to your needs.`;
            /**
             * This is an obsolete configuration, so warn and don't do anything
             */
            void vscode.window.showErrorMessage(msg);
            throw Error(msg);
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
    }
}

/**
 *
 * @returns true if ALIRE should be used for task execution, i.e. when the
 * workspace contains a `alire.toml` file.
 */
async function useAlire() {
    return (await adaExtState.getAlireTomls()).length > 0;
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
    const commandRegex = new RegExp(
        `^\\\${command:\\s*((${TASK_TYPE_ADA}|${TASK_TYPE_SPARK})\\.[^}]*)\\s*}$`
    );
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
                             * Result is a string so wrap it in an array for flattening.
                             */
                            return [evalRes];
                        } else if (isNonEmptyStringArray(evalRes)) {
                            /**
                             * Return the array result.
                             */
                            return evalRes as string[];
                        } else if (isEmptyArray(evalRes)) {
                            /**
                             * Not sure if evalRes can be casted to string[] in
                             * this case so it's easier to just return an empty
                             * array.
                             */
                            return [];
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

/**
 *
 * @param obj - an object
 * @returns true if the given object is a non-empty array of string objects.
 */
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
 *
 * @param obj - an object
 * @returns true if the given object is an empty array.
 */
function isEmptyArray(obj: unknown): boolean {
    if (obj instanceof Array) {
        return obj.length === 0;
    }

    return false;
}

/**
 * The name of the build task of a main, without the task type.
 */
function getBuildTaskPlainName(main?: AdaMain) {
    return `Build main - ${main?.mainRelPath() ?? ''}`;
}

/**
 * The full name of the build task of a main, including the task type.
 */
export function getBuildTaskName(main?: AdaMain) {
    return `${TASK_TYPE_ADA}: ${getBuildTaskPlainName(main)}`;
}

/**
 * The name of the run task of a main, without the task type.
 */
function getRunTaskPlainName(main?: AdaMain) {
    return `Run main - ${main?.mainRelPath() ?? ''}`;
}

/**
 * The full name of the build task of a main, including the task type.
 */
export function getRunTaskName(main?: AdaMain) {
    return `${TASK_TYPE_ADA}: ${getRunTaskPlainName(main)}`;
}

export function getBuildAndRunTaskPlainName(main?: AdaMain) {
    return `Build and run main - ${main?.mainRelPath() ?? ''}`;
}

export function getBuildAndRunTaskName(main?: AdaMain) {
    return `${TASK_TYPE_ADA}: ${getBuildAndRunTaskPlainName(main)}`;
}

export function createSparkTaskProvider(): SimpleTaskProvider {
    return new SimpleTaskProvider(
        TASK_TYPE_SPARK,
        predefinedTasks.filter((v) => v.taskDef.type == TASK_TYPE_SPARK)
    );
}

export function createAdaTaskProvider(): SimpleTaskProvider {
    return new SimpleTaskProvider(
        TASK_TYPE_ADA,
        predefinedTasks.filter((v) => v.taskDef.type == TASK_TYPE_ADA)
    );
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
 * Finds and returns the task of the given name.
 *
 * Task names contributed by the extension don't have the task type prefix in
 * the name while tasks coming from the workspace typically do since VS Code
 * includes the type prefix when converting an automatic extension task into a
 * configurable workspace task. This function accounts for that fact by search
 * for the task with and without the type prefix.
 *
 * If an array of tasks is given, the search will applied to this array and the
 * API will not be queried for tasks. This can be used to achieve a performance
 * boost.
 *
 * @returns the task that has the given name, or the given name with the
 * prefix `ada: ` or `spark: `.
 * @throws an Error if the task is not found.
 */
export async function findTaskByName(
    taskName: string,
    tasks?: vscode.Task[]
): Promise<vscode.Task> {
    if (!tasks) {
        tasks = (
            await Promise.all([
                vscode.tasks.fetchTasks({ type: 'ada' }),
                vscode.tasks.fetchTasks({ type: 'spark' }),
            ])
        ).flat();
    }

    if (tasks.length == 0) {
        throw Error('The task list is empty.' + ` Cannot find task '${taskName}'`);
    }

    const task = tasks.find((v) => {
        return taskName == getConventionalTaskLabel(v) || taskName == v.name;
    });
    if (task) {
        return task;
    } else {
        const msg = `Could not find a task named '${taskName}' among the tasks:\n${tasks
            .map((t) => t.name)
            .join('\n')}`;
        throw Error(msg);
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
        const adaTasks = await vscode.tasks.fetchTasks({ type: TASK_TYPE_ADA });
        return Promise.all(this.taskNames.map((name) => findTaskByName(name, adaTasks)));
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
export const workspaceTasksFirst = (a: vscode.Task, b: vscode.Task): number => {
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
 * @returns Array of tasks of type `ada` and label starting with 'Build and run
 * main -'. This includes tasks automatically provided by the extension as well
 * as user-defined tasks in tasks.json.
 */
export async function getBuildAndRunTasks(): Promise<vscode.Task[]> {
    return await vscode.tasks.fetchTasks({ type: TASK_TYPE_ADA }).then((tasks) =>
        tasks
            // Filter to tasks starting with the conventional name of "Build and run main" tasks
            .filter((t) => getConventionalTaskLabel(t).startsWith(getBuildAndRunTaskName()))

            // Return workspace-defined tasks first
            .sort(workspaceTasksFirst)
    );
}

export async function findBuildAndRunTask(adaMain: AdaMain): Promise<vscode.Task | undefined> {
    return (await getBuildAndRunTasks()).find(
        // Tasks defined in tasks.json will have a leading 'ada: ' while the
        // ones auto-generated by the extension don't. We want to match both.
        (t) => getConventionalTaskLabel(t) == getBuildAndRunTaskName(adaMain)
    );
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

/**
 * @returns Array of tasks of type `ada` and label starting with 'Build main -'.
 * This includes tasks automatically provided by the extension as well as
 * user-defined tasks in tasks.json.
 */
export async function getBuildMainTasks() {
    return await vscode.tasks.fetchTasks({ type: TASK_TYPE_ADA }).then((tasks) =>
        tasks
            .filter((t) => getConventionalTaskLabel(t).startsWith(getBuildTaskName()))
            // Return workspace-defined tasks first
            .sort(workspaceTasksFirst)
    );
}

export async function findBuildMainTask(adaMain: AdaMain): Promise<vscode.Task | undefined> {
    return (await getBuildMainTasks()).find(
        // Tasks defined in tasks.json will have a leading 'ada: ' while the
        // ones auto-generated by the extension don't. We want to match both.
        (t) => getConventionalTaskLabel(t) == getBuildTaskName(adaMain)
    );
}

/**
 * Create an updated task that uses the `alr` executable.
 *
 * If the task matches the pre-defined tasks for building and cleaning the project,
 * then the commands used will be respectively `alr build` and `alr clean`.
 *
 * Otherwise `alr exec -- ...` is used.
 *
 * @param taskDef - a task definition to update to ALIRE
 * @returns a copy of the given task where the `alr` executable is used.
 */
function updateToAlire(taskDef: SimpleTaskDef): SimpleTaskDef {
    /**
     * Only process shell command tasks, if they are not already using ALIRE
     */
    if (taskDef.command && !isAlire(taskDef.command)) {
        /**
         * Create a copy of the task definition to modify its properties
         */
        const newTaskDef = { ...taskDef };
        const command = taskDef.command;
        const args = taskDef.args?.concat() ?? [];

        /**
         * Change command to alire. No need to use `alr.exe` on Windows, just
         * `alr` works.
         */
        newTaskDef.command = 'alr';

        if (taskDef == TASK_BUILD_PROJECT.taskDef) {
            /**
             * Replace the entire command with `alr build`. Ignore project and
             * scenario args because they are managed by ALIRE.
             *
             */
            args.splice(0, args.length, 'build', '--', '-cargs:ada', '-gnatef');
        } else if (taskDef == TASK_CLEAN_PROJECT.taskDef) {
            /**
             * Replace the entire command with `alr clean`. Ignore project and
             * scenario args because they are managed by ALIRE.
             */
            args.splice(0, args.length, 'clean');
        } else {
            /**
             * Use `alr exec` for any other commands.
             */
            args.splice(0, 0, 'exec', '--', command);
        }

        newTaskDef.args = args;

        return newTaskDef;
    }

    return taskDef;
}

/**
 *
 * @param command - a string or {@link vscode.ShellQuotedString} from a task definition
 * @returns true if the command points to ALIRE, i.e. if it's `alr` or `alr.exe`
 * or a path to those executables.
 */
function isAlire(command: string | vscode.ShellQuotedString): boolean {
    const value = typeof command == 'string' ? command : command.value;
    const commandBasename = basename(value);
    return commandBasename.match(/^alr(\.exe)?$/) != null;
}
