/* eslint-disable max-len */
/*----------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2018-2026, AdaCore                     --
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
import { existsSync, statSync } from 'fs';
import path, { basename } from 'path';
import * as vscode from 'vscode';
import {
    CMD_GET_OBJECT_DIR,
    CMD_GNATCOV_LEVEL_ARGS_COVERAGE,
    CMD_GNATCOV_LEVEL_ARGS_INSTRUMENT,
    CMD_GPR_PROJECT_ARGS,
    CMD_SPARK_CURRENT_GNATPROVE_OPTIONS,
    CMD_SPARK_LIMIT_REGION_ARG,
    CMD_SPARK_LIMIT_SUBP_ARG,
} from './constants';
import { adaExtState, logger } from './extension';
import { getGnatTestDriverProjectPath, loadGnatCoverageReport } from './gnattest';
import { AdaMain, getAdaMains, showErrorMessageWithOpenLogButton } from './helpers';

export const TASK_TYPE_ADA = 'ada';
export const TASK_TYPE_SPARK = 'spark';
export const DEFAULT_PROBLEM_MATCHERS = ['$ada-info', '$ada-warning', '$ada-error'];

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
     * The tasks' names should be prefixed by their source
     * (e.g: 'ada: Build current project' instead of 'Build current project').
     */
    compound?: string[];
}

/**
 * A type to represent predefined tasks that will be offered by the extension.
 */
export interface PredefinedTask {
    label: string;
    description?: string;
    taskDef: SimpleTaskDef;
    problemMatchers?: string | string[];
    taskGroup?: vscode.TaskGroup;
    revealKind?: vscode.TaskRevealKind;
    isBackground?: boolean;
    /**
     * A command ID to create a wrapper command for this task.
     */
    commandId?: string;
}

interface SPARKPredefinedTask extends PredefinedTask {
    /**
     * A command ID to create a wrapper command for this task.
     */
    commandId: string;
    /**
     * Disable the options picker that is normally shown before running a SPARK task.
     */
    noOptionPicker?: boolean;
}

/**
 * The main build project task defined as a constant to allow it to be referenced directly.
 */
const TASK_BUILD_PROJECT: PredefinedTask = {
    label: 'Build current project',
    taskDef: {
        type: TASK_TYPE_ADA,
        command: 'gprbuild',
        args: [`\${command:${CMD_GPR_PROJECT_ARGS}}`, "'-cargs:ada'", '-gnatef'],
    },
    problemMatchers: DEFAULT_PROBLEM_MATCHERS,
    taskGroup: vscode.TaskGroup.Build,
    commandId: 'ada.tasks.buildProject',
};

export const BUILD_PROJECT_TASK_NAME = `${TASK_BUILD_PROJECT.taskDef.type}: ${TASK_BUILD_PROJECT.label}`;

const TASK_CLEAN_PROJECT = {
    label: 'Clean current project',
    taskDef: {
        type: TASK_TYPE_ADA,
        command: 'gprclean',
        args: [`\${command:${CMD_GPR_PROJECT_ARGS}}`],
    },
    problemMatchers: [],
    taskGroup: vscode.TaskGroup.Clean,
    commandId: 'ada.tasks.cleanProject',
};

export const TASK_PROVE_SUPB_PLAIN_NAME = 'Prove subprogram';
export const TASK_BUILD_TEST_DRIVER = 'Build GNATtest test harness project';

export const TASK_GNATCOV_SETUP: PredefinedTask = {
    label: 'GNATcoverage - Setup runtime library',
    taskDef: {
        type: TASK_TYPE_ADA,
        command: 'gnatcov',
        args: ['setup'],
    },
    problemMatchers: DEFAULT_PROBLEM_MATCHERS,
    commandId: 'ada.tasks.gnatcovSetup',
};

/**
 * Instruments the project's sources so that running an executable built from
 * them produces a source trace usable for coverage analysis.
 *
 * `--dump-filename-simple` makes the instrumented executable write its trace
 * as `<exe-basename>.srctrace` in its current working directory (the
 * workspace root, for tasks run by this extension), which is the convention
 * relied upon by {@link getGnatcovGenerateReportTaskPlainName}'s task.
 */
export const TASK_GNATCOV_INSTRUMENT: PredefinedTask = {
    label: 'GNATcoverage - Instrument project',
    taskDef: {
        type: TASK_TYPE_ADA,
        command: 'gnatcov',
        args: [
            'instrument',
            `\${command:${CMD_GPR_PROJECT_ARGS}}`,
            `\${command:${CMD_GNATCOV_LEVEL_ARGS_INSTRUMENT}}`,
            '--dump-filename-simple',
        ],
    },
    problemMatchers: DEFAULT_PROBLEM_MATCHERS,
    commandId: 'ada.tasks.gnatcovInstrument',
};

export const TASK_GNATCOV_BUILD_INSTRUMENTED: PredefinedTask = {
    label: 'GNATcoverage - Build instrumented project',
    taskDef: {
        type: TASK_TYPE_ADA,
        command: 'gprbuild',
        args: [
            '-m',
            '-s',
            `\${command:${CMD_GPR_PROJECT_ARGS}}`,
            '--src-subdirs=gnatcov-instr',
            '--implicit-with=gnatcov_rts.gpr',
            "'-cargs:ada'",
            '-gnatef',
        ],
    },
    problemMatchers: DEFAULT_PROBLEM_MATCHERS,
    taskGroup: vscode.TaskGroup.Build,
    commandId: 'ada.tasks.gnatcovBuildInstrumented',
};

export const gnatCovTasks: PredefinedTask[] = [
    TASK_GNATCOV_SETUP,
    TASK_GNATCOV_INSTRUMENT,
    TASK_GNATCOV_BUILD_INSTRUMENTED,
];

export const TASK_GNATSAS_REPORT: PredefinedTask = {
    label: 'Create a report after a GNAT SAS analysis',
    taskDef: {
        type: TASK_TYPE_ADA,
        command: 'gnatsas',
        args: [
            'report',
            'sarif',
            `\${command:${CMD_GPR_PROJECT_ARGS}}`,
            '-o',
            'report.sarif',
            '--root',
            '${workspaceFolder}',
        ],
    },
    /**
     * Analysis results are not printed on stdio so no need to parse them
     * with a problem matcher.
     */
    problemMatchers: [],
};
/**
 * Predefined tasks offered by the extension. Both 'ada' and 'spark' tasks are
 * included in this array. They are later on split and provided by different
 * task providers.
 */
export const adaTasks: PredefinedTask[] = [
    /**
     * Ada
     */
    TASK_CLEAN_PROJECT,
    TASK_BUILD_PROJECT,
    {
        label: 'Compute metrics for current file',
        description:
            'Run `gnatmetric` on the current file and produce a metrics XML in the object directory',
        taskDef: {
            type: TASK_TYPE_ADA,
            command: 'gnatmetric',
            args: [
                `\${command:${CMD_GPR_PROJECT_ARGS}}`,
                '--complexity-cyclomatic',
                '--construct-nesting',
                '--lines-code',
                `--xml-file-name=\${command:${CMD_GET_OBJECT_DIR}}/\${fileBasenameNoExtension}.metrics.xml`,
                '${file}',
            ],
        },
        problemMatchers: [],
    },
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
                `\${command:${CMD_GPR_PROJECT_ARGS}}`,
                '${fileBasename}',
                "'-cargs:ada'",
                '-gnatef',
            ],
        },
        problemMatchers: DEFAULT_PROBLEM_MATCHERS,
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
                `\${command:${CMD_GPR_PROJECT_ARGS}}`,
                '${fileBasename}',
                "'-cargs:ada'",
                '-gnatef',
            ],
        },
        problemMatchers: DEFAULT_PROBLEM_MATCHERS,
    },
    {
        label: 'Analyze the project with GNAT SAS',
        taskDef: {
            type: TASK_TYPE_ADA,
            command: 'gnatsas',
            args: ['analyze', `\${command:${CMD_GPR_PROJECT_ARGS}}`],
        },
        /**
         * Analysis results are not printed on stdio so no need to parse them
         * with a problem matcher. Results should be viewed with the
         * `gnatsas report` task below
         */
        problemMatchers: [],
        commandId: 'ada.tasks.analyzeProjectGnatSas',
    },
    {
        label: 'Analyze the current file with GNAT SAS',
        taskDef: {
            type: TASK_TYPE_ADA,
            command: 'gnatsas',
            args: ['analyze', `\${command:${CMD_GPR_PROJECT_ARGS}}`, '--file=${fileBasename}'],
        },
        /**
         * Analysis results are not printed on stdio so no need to parse them
         * with a problem matcher. Results should be viewed with the 'gnatsas
         * report' task below. Note that the "--root" argument is required so
         * the file URIs in the SARIF file are relative to the workspace root
         * folder and SARIF-viewer extensions can correctly resolve them.
         */
        problemMatchers: [],
    },
    TASK_GNATSAS_REPORT,
    {
        label: 'Analyze the project with GNAT SAS and produce a report',
        taskDef: {
            type: TASK_TYPE_ADA,
            compound: [
                'ada: Analyze the project with GNAT SAS',
                'ada: Create a report after a GNAT SAS analysis',
            ],
        },
        /**
         * Analysis results are not printed on stdio so no need to parse them
         * with a problem matcher.
         */
        problemMatchers: [],
        commandId: 'ada.tasks.analyzeProjectGnatSasAndReport',
    },
    {
        label: 'Analyze the current file with GNAT SAS and produce a report',
        taskDef: {
            type: TASK_TYPE_ADA,
            compound: [
                'ada: Analyze the current file with GNAT SAS',
                'ada: Create a report after a GNAT SAS analysis',
            ],
        },
        /**
         * Analysis results are not printed on stdio so no need to parse them
         * with a problem matcher.
         */
        problemMatchers: [],
    },
    {
        label: 'Generate documentation from the project',
        taskDef: {
            type: TASK_TYPE_ADA,
            command: 'gnatdoc',
            args: [`\${command:${CMD_GPR_PROJECT_ARGS}}`],
        },
        problemMatchers: [],
        commandId: 'ada.tasks.generateDocumentation',
    },
    {
        label: 'Create or update GNATtest test framework',
        taskDef: {
            type: TASK_TYPE_ADA,
            command: 'gnattest',
            args: [`\${command:${CMD_GPR_PROJECT_ARGS}}`],
        },
        problemMatchers: [],
        commandId: 'ada.tasks.createOrUpdateGnattest',
    },
];

export const sparkTasks: SPARKPredefinedTask[] = [
    /**
     * SPARK
     */
    {
        label: 'Clean project for proof',
        taskDef: {
            type: TASK_TYPE_SPARK,
            command: 'gnatprove',
            args: [`\${command:${CMD_GPR_PROJECT_ARGS}}`, '--clean'],
        },
        problemMatchers: [],
        commandId: 'ada.spark.tasks.cleanProject',
        noOptionPicker: true,
    },
    {
        label: 'Examine project',
        taskDef: {
            type: TASK_TYPE_SPARK,
            command: 'gnatprove',
            args: [
                '--output=oneline',
                `\${command:${CMD_GPR_PROJECT_ARGS}}`,
                '--mode=flow',
                `\${command:${CMD_SPARK_CURRENT_GNATPROVE_OPTIONS}}`,
                '-cargs',
                '-gnatef',
            ],
        },
        problemMatchers: DEFAULT_PROBLEM_MATCHERS,
        commandId: 'ada.spark.tasks.examineProject',
    },
    {
        label: 'Examine file',
        taskDef: {
            type: TASK_TYPE_SPARK,
            command: 'gnatprove',
            args: [
                '--output=oneline',
                `\${command:${CMD_GPR_PROJECT_ARGS}}`,
                '--mode=flow',
                '-u',
                '${fileBasename}',
                `\${command:${CMD_SPARK_CURRENT_GNATPROVE_OPTIONS}}`,
                '-cargs',
                '-gnatef',
            ],
        },
        problemMatchers: DEFAULT_PROBLEM_MATCHERS,
        commandId: 'ada.spark.tasks.examineFile',
    },
    {
        label: 'Examine subprogram',
        taskDef: {
            type: TASK_TYPE_SPARK,
            command: 'gnatprove',
            args: [
                '--output=oneline',
                `\${command:${CMD_GPR_PROJECT_ARGS}}`,
                '--mode=flow',
                `\${command:${CMD_SPARK_LIMIT_SUBP_ARG}}`,
                `\${command:${CMD_SPARK_CURRENT_GNATPROVE_OPTIONS}}`,
                '-cargs',
                '-gnatef',
            ],
        },
        problemMatchers: DEFAULT_PROBLEM_MATCHERS,
        commandId: 'ada.spark.tasks.examineSubprogram',
    },
    {
        label: 'Prove project',
        taskDef: {
            type: TASK_TYPE_SPARK,
            command: 'gnatprove',
            args: [
                '--output=oneline',
                `\${command:${CMD_GPR_PROJECT_ARGS}}`,
                `\${command:${CMD_SPARK_CURRENT_GNATPROVE_OPTIONS}}`,
                '-cargs',
                '-gnatef',
            ],
        },
        problemMatchers: DEFAULT_PROBLEM_MATCHERS,
        commandId: 'ada.spark.tasks.proveProject',
    },
    {
        label: 'Prove file',
        taskDef: {
            type: TASK_TYPE_SPARK,
            command: 'gnatprove',
            args: [
                '--output=oneline',
                `\${command:${CMD_GPR_PROJECT_ARGS}}`,
                '-u',
                '${fileBasename}',
                `\${command:${CMD_SPARK_CURRENT_GNATPROVE_OPTIONS}}`,
                '-cargs',
                '-gnatef',
            ],
        },
        problemMatchers: DEFAULT_PROBLEM_MATCHERS,
        commandId: 'ada.spark.tasks.proveFile',
    },
    {
        label: TASK_PROVE_SUPB_PLAIN_NAME,
        taskDef: {
            type: TASK_TYPE_SPARK,
            command: 'gnatprove',
            args: [
                '--output=oneline',
                `\${command:${CMD_GPR_PROJECT_ARGS}}`,
                `\${command:${CMD_SPARK_LIMIT_SUBP_ARG}}`,
                `\${command:${CMD_SPARK_CURRENT_GNATPROVE_OPTIONS}}`,
                '-cargs',
                '-gnatef',
            ],
        },
        problemMatchers: DEFAULT_PROBLEM_MATCHERS,
        commandId: 'ada.spark.tasks.proveSubprogram',
    },
    {
        label: 'Prove selected region',
        taskDef: {
            type: TASK_TYPE_SPARK,
            command: 'gnatprove',
            args: [
                '--output=oneline',
                `\${command:${CMD_GPR_PROJECT_ARGS}}`,
                '-u',
                '${fileBasename}',
                `\${command:${CMD_SPARK_LIMIT_REGION_ARG}}`,
                `\${command:${CMD_SPARK_CURRENT_GNATPROVE_OPTIONS}}`,
                '-cargs',
                '-gnatef',
            ],
        },
        problemMatchers: DEFAULT_PROBLEM_MATCHERS,
        commandId: 'ada.spark.tasks.proveSelectedRegion',
    },
    {
        label: 'Prove line',
        taskDef: {
            type: TASK_TYPE_SPARK,
            command: 'gnatprove',
            args: [
                '--output=oneline',
                `\${command:${CMD_GPR_PROJECT_ARGS}}`,
                '-u',
                '${fileBasename}',
                '--limit-line=${fileBasename}:${lineNumber}',
                `\${command:${CMD_SPARK_CURRENT_GNATPROVE_OPTIONS}}`,
                '-cargs',
                '-gnatef',
            ],
        },
        problemMatchers: DEFAULT_PROBLEM_MATCHERS,
        commandId: 'ada.spark.tasks.proveLine',
    },
];

const predefinedTasks: PredefinedTask[] = adaTasks.concat(sparkTasks, gnatCovTasks);

/**
 * A provider of tasks based on the {@link SimpleTaskDef} task definition.
 *
 * It is instantiated with a string task type and an array of {@link
 * PredefinedTask} to provide. This way the same class can be reused to provide
 * both 'ada' and 'spark' tasks.
 */
export class SimpleTaskProvider implements vscode.TaskProvider {
    constructor(
        public taskType: string,
        private taskDecls: PredefinedTask[],
    ) {}

    async provideTasks(token?: vscode.CancellationToken): Promise<vscode.Task[]> {
        if (token?.isCancellationRequested) {
            throw new vscode.CancellationError();
        }

        const result: vscode.Task[] = [];
        const targetPrefix = await adaExtState.getTargetPrefix().catch((err) => {
            logger.error('Error in task provider:\n' + err);
            return '';
        });
        const isNativeProject = await adaExtState.isNativeProject().catch((err) => {
            logger.error('Error in task provider:\n' + err);
            return true;
        });

        /**
         * Start with the list of predefined tasks.
         */
        let taskDeclsToOffer = this.taskDecls.concat();

        if (this.taskType == TASK_TYPE_ADA) {
            /**
             * Add tasks based on the Mains of the project.
             */
            taskDeclsToOffer.push(
                ...(
                    await getAdaMains().catch((err) => {
                        logger.error('Error in task provider:\n' + err);
                        return [];
                    })
                ).flatMap((main) => {
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
                                "'-cargs:ada'",
                                '-gnatef',
                            ],
                        },
                        problemMatchers: DEFAULT_PROBLEM_MATCHERS,
                        taskGroup: vscode.TaskGroup.Build,
                    };

                    let execRelPath = main.execRelPath();
                    /**
                     * Always prepend ./ to the relative path. When the path
                     * only has one component, ./ is necessary for the shell to
                     * spawn the executable. We choose to always prepend ./ for
                     * consistency.
                     *
                     * We can't use path.join() because it calls
                     * path.normalize() which removes ./
                     *
                     * We use path.normalize() to make sure the path uses
                     * platform-specific separators.
                     */
                    execRelPath = '.' + path.sep + path.normalize(execRelPath);
                    const runTask: PredefinedTask = {
                        label: getRunTaskPlainName(main),
                        taskDef: {
                            type: this.taskType,
                            command: execRelPath,
                            args: [],
                        },
                        revealKind: vscode.TaskRevealKind.Always,
                    };

                    const buildAndRunTask: PredefinedTask = {
                        label: getBuildAndRunTaskPlainName(main),
                        taskDef: {
                            type: this.taskType,
                            compound: [
                                getConventionalTaskLabel(buildTask),
                                getConventionalTaskLabel(runTask),
                            ],
                        },
                        problemMatchers: [],
                    };

                    /**
                     * The report task reads the trace produced by `runTask`
                     * above. `TASK_GNATCOV_INSTRUMENT` passes
                     * `--dump-filename-simple`, so the instrumented
                     * executable writes its trace as `<exe-basename>.srctrace`
                     * in its working directory, which for tasks run by this
                     * extension is the workspace root.
                     */
                    const gnatcovReportTask: PredefinedTask = {
                        label: getGnatcovGenerateReportTaskPlainName(main),
                        taskDef: {
                            type: this.taskType,
                            command: 'gnatcov',
                            args: [
                                'coverage',
                                `\${command:${CMD_GPR_PROJECT_ARGS}}`,
                                `\${command:${CMD_GNATCOV_LEVEL_ARGS_COVERAGE}}`,
                                '--annotate=xml',
                                `--output-dir=\${command:${CMD_GET_OBJECT_DIR}}`,
                                '-T',
                                `\${workspaceFolder}/${path.basename(execRelPath)}.srctrace`,
                            ],
                        },
                        problemMatchers: [],
                    };

                    const gnatcovRunAllActionsTask: PredefinedTask = {
                        label: getGnatcovRunAllActionsTaskPlainName(main),
                        taskDef: {
                            type: this.taskType,
                            compound: [
                                getConventionalTaskLabel(TASK_GNATCOV_INSTRUMENT),
                                getConventionalTaskLabel(TASK_GNATCOV_BUILD_INSTRUMENTED),
                                getConventionalTaskLabel(runTask),
                                getConventionalTaskLabel(gnatcovReportTask),
                            ],
                        },
                        problemMatchers: [],
                    };

                    const tasks = [
                        buildTask,
                        runTask,
                        buildAndRunTask,
                        gnatcovReportTask,
                        gnatcovRunAllActionsTask,
                    ];

                    if (!isNativeProject) {
                        const gnatemulatorExe = targetPrefix
                            ? targetPrefix + '-gnatemu'
                            : 'gnatemu';
                        const runGNATemulatorTask: PredefinedTask = {
                            label: getRunGNATemulatorTaskPlainName(main),
                            taskDef: {
                                type: this.taskType,
                                command: gnatemulatorExe,
                                args: [`\${command:${CMD_GPR_PROJECT_ARGS}}`, execRelPath],
                            },
                            revealKind: vscode.TaskRevealKind.Always,
                        };

                        const runGNATemulatorTaskForDebug: PredefinedTask = {
                            ...runGNATemulatorTask,
                            label: getRunGNATemulatorTaskPlainName(main, true),
                            taskDef: {
                                ...runGNATemulatorTask.taskDef,
                                args: [`\${command:${CMD_GPR_PROJECT_ARGS}}`, `-g`, execRelPath],
                            },
                            isBackground: true,
                            // This task is run before some debug tasks.
                            // Problem is, it's a watch script, and since it never exits, VSCode
                            // complains. All this is needed so VSCode just lets it run.
                            problemMatchers: [],
                        };

                        const buildAndRunGNATemulatorTask: PredefinedTask = {
                            label: getBuildAndRunGNATemulatorTaskPlainName(main),
                            taskDef: {
                                type: this.taskType,
                                compound: [
                                    getConventionalTaskLabel(buildTask),
                                    getConventionalTaskLabel(runGNATemulatorTask),
                                ],
                            },
                            problemMatchers: [],
                        };

                        const buildAndRunGNATemulatorTaskForDebug: PredefinedTask = {
                            label: getBuildAndRunGNATemulatorTaskPlainName(main, true),
                            taskDef: {
                                type: this.taskType,
                                compound: [
                                    getConventionalTaskLabel(buildTask),
                                    getConventionalTaskLabel(runGNATemulatorTaskForDebug),
                                ],
                            },
                            problemMatchers: [],
                        };

                        tasks.push(
                            runGNATemulatorTask,
                            runGNATemulatorTaskForDebug,
                            buildAndRunGNATemulatorTask,
                            buildAndRunGNATemulatorTaskForDebug,
                        );
                    }

                    return tasks;
                }),
            );

            /**
             * If a test harness project exists, provide a task to build it.
             */
            const harnessPrj = await getGnatTestDriverProjectPath().catch(
                /**
                 * In case of errors fallback silently to keep providing tasks
                 * even in case of errors.
                 */
                (reason) => {
                    logger.error(
                        'Error while querying for the GNATtest test driver project:\n' + reason,
                    );
                    return undefined;
                },
            );
            if (harnessPrj && existsSync(harnessPrj)) {
                taskDeclsToOffer.push({
                    label: TASK_BUILD_TEST_DRIVER,
                    taskDef: {
                        type: TASK_TYPE_ADA,
                        command: 'gprbuild',
                        args: ['-P', harnessPrj, "'-cargs:ada'", '-gnatef'],
                    },
                    problemMatchers: DEFAULT_PROBLEM_MATCHERS,
                    taskGroup: vscode.TaskGroup.Build,
                });
            }
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
                tDecl.problemMatchers,
            );

            /**
             * Ideally we would have liked to provide unresolved tasks and let
             * resolving only happen in the resolveTask method, but that's not
             * how VS Code works. This method is expected to return fully
             * resolved tasks, hence we must resolve pre-defined tasks here.
             */
            const resolvedTask = await this.resolveTask(task, token);

            if (resolvedTask) {
                /**
                 * Set other properties on the resolved task
                 */
                if (tDecl.taskGroup) {
                    resolvedTask.group = tDecl.taskGroup;
                }

                /**
                 * If the task has a specified revealKind, use it.
                 * Otherwise, by default, do not reveal the Terminal panel when
                 * running the task: we want to highlight the issues displayed
                 * in the Problems view instead.
                 */
                resolvedTask.presentationOptions = {
                    reveal: tDecl.revealKind ?? vscode.TaskRevealKind.Never,
                };
                resolvedTask.isBackground = tDecl.isBackground ?? false;

                result.push(resolvedTask);
            } else {
                logger.error(`Failed to resolve task: ${JSON.stringify(task, undefined, 2)}`);
            }
        }

        return result;
    }

    async resolveTask(
        task: vscode.Task,
        token?: vscode.CancellationToken,
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
            execution = getConventionalTaskLabel(task).startsWith(getGnatcovRunAllActionsTaskName())
                ? new GnatcovRunAllActionsExecution(task.name, taskDef.compound)
                : new SequentialExecutionByName(task.name, taskDef.compound);
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
                const evaluatedArgs: (string | vscode.ShellQuotedString)[] =
                    await evaluateArgs(args);
                execution = new vscode.ShellExecution(taskDef.command, evaluatedArgs);
            } catch (err) {
                if (err instanceof vscode.CancellationError) {
                    // It's just a cancellation, propagate as is
                    throw err;
                }
                let msg = 'Error while evaluating task arguments.';
                logger.error(msg);
                logger.error(err);
                if (err instanceof Error) {
                    msg += ' ' + err.message;
                }
                void showErrorMessageWithOpenLogButton(msg);
                return undefined;
            }
        }

        return new vscode.Task(
            task.definition,
            task.scope ?? vscode.TaskScope.Workspace,
            task.name,
            task.source,
            execution,
            task.problemMatchers,
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
    return (await adaExtState.getAlireCrateFile()) != null;
}

/**
 * Evaluate task arguments with support for commands returning arrays (VS Code
 * native evaluation of task arguments only allows commands returning a plain
 * string).
 *
 * @param args - an array of command line arguments from a task
 * @returns the array of arguments where items matching the pattern
 * '$\{command:ada.*\}' have been evaluated. If they return an array of strings,
 * then the array is inserted into the argument array at the location of the
 * command.
 */
async function evaluateArgs(args: (string | vscode.ShellQuotedString)[]) {
    const commandRegex = new RegExp(
        `^\\\${command:\\s*((${TASK_TYPE_ADA}|${TASK_TYPE_SPARK})\\.[^}]*)\\s*}$`,
    );
    const evaluatedArgs: (string | vscode.ShellQuotedString)[] = (
        await Promise.all(
            args.flatMap(async function (
                a: string | vscode.ShellQuotedString,
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
            }),
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
 * The full name of the 'Run GNATemulator' task of a main, including the task type.
 */
export function getRunGNATemulatorTaskName(main?: AdaMain, forDebug = false) {
    return `${TASK_TYPE_ADA}: ${getRunGNATemulatorTaskPlainName(main, forDebug)}`;
}

/**
 * The name of the 'Run GNATemulator' task of a main, without the task type.
 */
function getRunGNATemulatorTaskPlainName(main?: AdaMain, forDebug = false) {
    const forDebugLabelSuffix = forDebug ? ' (debug)' : '';
    return `Run main with GNATemulator${forDebugLabelSuffix} - ${main?.mainRelPath() ?? ''}`;
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

export function getBuildAndRunGNATemulatorTaskPlainName(main?: AdaMain, forDebug = false) {
    const forDebugLabelSuffix = forDebug ? ' (debug)' : '';
    return (
        `Build and run main with GNATemulator` +
        `${forDebugLabelSuffix} - ${main?.mainRelPath() ?? ''}`
    );
}

export function getBuildAndRunTaskName(main?: AdaMain) {
    return `${TASK_TYPE_ADA}: ${getBuildAndRunTaskPlainName(main)}`;
}

export function getBuildAndRunGNATemulatorTaskName(main?: AdaMain, forDebug = false) {
    return `${TASK_TYPE_ADA}: ${getBuildAndRunGNATemulatorTaskPlainName(main, forDebug)}`;
}

function getGnatcovGenerateReportTaskPlainName(main?: AdaMain) {
    return `GNATcoverage - Generate report - ${main?.mainRelPath() ?? ''}`;
}

function getGnatcovRunAllActionsTaskPlainName(main?: AdaMain) {
    return `GNATcoverage - Run all actions - ${main?.mainRelPath() ?? ''}`;
}

/**
 * The full name of the compound task that runs the whole GNATcoverage
 * instrumentation-based analysis workflow for a given Main, including the
 * task type.
 */
export function getGnatcovRunAllActionsTaskName(main?: AdaMain) {
    return `${TASK_TYPE_ADA}: ${getGnatcovRunAllActionsTaskPlainName(main)}`;
}

export function createSparkTaskProvider(): SimpleTaskProvider {
    return new SimpleTaskProvider(
        TASK_TYPE_SPARK,
        predefinedTasks.filter((v) => v.taskDef.type == TASK_TYPE_SPARK),
    );
}

export function createAdaTaskProvider(): SimpleTaskProvider {
    return new SimpleTaskProvider(
        TASK_TYPE_ADA,
        predefinedTasks.filter((v) => v.taskDef.type == TASK_TYPE_ADA),
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
 * Run the named tasks in sequence in a VS Code task terminal.
 */
class SequentialExecutionByName extends vscode.CustomExecution {
    constructor(
        protected taskName: string,
        private taskNames: string[],
    ) {
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
        const writeEmitter = new vscode.EventEmitter<string>();
        const closeEmitter = new vscode.EventEmitter<number>();
        const cancellation = new vscode.CancellationTokenSource();
        const run = async () => {
            try {
                closeEmitter.fire(await this.runTasks(writeEmitter, cancellation.token));
            } catch (err) {
                if (
                    err instanceof Error &&
                    !(err instanceof vscode.CancellationError) &&
                    !cancellation.token.isCancellationRequested
                ) {
                    void vscode.window.showErrorMessage(err.message);
                    // Write the error message to the terminal, replacing newlines with
                    // CRLF for proper display.
                    writeEmitter.fire(err.message.replace(/\r?\n|\r/g, '\r\n') + '\r\n');
                }
                closeEmitter.fire(2);
            } finally {
                cancellation.dispose();
            }
        };
        return Promise.resolve({
            onDidWrite: writeEmitter.event,
            onDidClose: closeEmitter.event,
            open: () => {
                void run();
            },
            close: () => cancellation.cancel(),
        });
    }

    protected async getTasksToRun(): Promise<vscode.Task[]> {
        const adaTasks = await vscode.tasks.fetchTasks({ type: TASK_TYPE_ADA });
        return Promise.all(this.taskNames.map((name) => findTaskByName(name, adaTasks)));
    }

    protected async runTasks(
        writeEmitter: vscode.EventEmitter<string>,
        token: vscode.CancellationToken,
        onTaskFailure?: (task: vscode.Task, exitCode: number) => void,
    ): Promise<number> {
        const tasks = await this.getTasksToRun();
        if (token.isCancellationRequested) {
            return 1;
        }
        return runTaskSequence(tasks, writeEmitter, onTaskFailure, token);
    }
}

/**
 * Finds and returns the task of the given name, prioritizing the one
 * defined/customized in the workspace's 'tasks.json' file if any.
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
    taskFullName: string,
    tasks?: vscode.Task[],
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
        throw Error('The task list is empty.' + ` Cannot find task '${taskFullName}'`);
    }

    // Sort the given tasks to put the workspace-defined ones first
    // in the list. This allows to pick-up predefined tasks that
    // have been customized by the user in priority.
    tasks = tasks.sort(workspaceTasksFirst);

    const task = tasks.find((v) => {
        return taskFullName == getConventionalTaskLabel(v) || taskFullName == v.name;
    });
    if (task) {
        return task;
    } else {
        const msg = `Could not find a task named '${taskFullName}' among the tasks:\n${tasks
            .map((t) => t.name)
            .join('\n')}`;
        throw Error(msg);
    }
}

/**
 * Load coverage only for the complete GNATcoverage workflow, after every task
 * succeeds. Individual instrumentation, build and report tasks stay independent.
 */
class GnatcovRunAllActionsExecution extends SequentialExecutionByName {
    protected override async runTasks(
        writeEmitter: vscode.EventEmitter<string>,
        token: vscode.CancellationToken,
    ): Promise<number> {
        // Capture the destination before running, since the active project may
        // change while the workflow executes.
        const outputDir = await adaExtState.getObjectDir();
        const reportPaths = [
            path.join(outputDir, 'index.xml'),
            path.join(outputDir, 'xml', 'index.xml'),
        ];
        const previousReports = reportPaths.map((p) => statSync(p, { throwIfNoEntry: false }));
        const status = await super.runTasks(writeEmitter, token, (task, exitCode) => {
            // Build failures already have a notification offering runtime setup.
            if (
                getConventionalTaskLabel(task) ===
                getConventionalTaskLabel(TASK_GNATCOV_BUILD_INSTRUMENTED)
            ) {
                return;
            }
            const msg = `GNATcoverage analysis failed at '${getConventionalTaskLabel(task)}' (exit code ${exitCode}). See the task terminal for details.`;
            writeEmitter.fire(msg + '\r\n');
            void showTaskError(task, msg);
        });
        if (status === 0 && !token.isCancellationRequested) {
            try {
                // Only import a report updated by this execution. In particular,
                // an old flat report must not hide a new report under xml/.
                const reportPath = reportPaths.find((p, i) => {
                    const current = statSync(p, { throwIfNoEntry: false });
                    const previous = previousReports[i];
                    return (
                        current?.isFile() &&
                        (current.mtimeMs !== previous?.mtimeMs ||
                            current.ctimeMs !== previous?.ctimeMs ||
                            current.size !== previous?.size ||
                            current.ino !== previous?.ino)
                    );
                });
                if (!reportPath) {
                    throw new Error(`No new GNATcoverage report found under ${outputDir}`);
                }
                await loadGnatCoverageReport(reportPath, this.taskName, token);
            } catch (err) {
                if (err instanceof vscode.CancellationError || token.isCancellationRequested) {
                    return 1;
                }
                const msg = `GNATcoverage analysis completed, but the report could not be loaded: ${
                    err instanceof Error ? err.message : String(err)
                }`;
                writeEmitter.fire(msg + '\r\n');
                void vscode.window.showErrorMessage(msg);
            }
        }
        return token.isCancellationRequested ? 1 : status;
    }
}

/** Show a task error with an action to reveal its terminal. */
export async function showTaskError(
    task: vscode.Task,
    message: string,
    ...actions: string[]
): Promise<string | undefined> {
    const action = await vscode.window.showErrorMessage(message, 'Show Terminal', ...actions);
    if (action === 'Show Terminal') {
        const terminal = vscode.window.terminals.find(
            (terminal) =>
                terminal.name === task.name || terminal.name === getConventionalTaskLabel(task),
        );
        if (terminal) {
            terminal.show();
        } else {
            await vscode.commands.executeCommand('workbench.action.terminal.focus');
        }
    }
    return action;
}

/**
 * Runs a list of tasks in sequence, as long as the execution succeeds. The
 * sequence stops if a task ends with a failure status, or when all tasks
 * complete successfully.
 *
 * @param tasks - list of tasks to run in sequence.
 * @param onTaskFailure - optional callback for a nonzero process exit code;
 * task termination without an exit code does not invoke it
 * @param token - optional cancellation token to stop the active task and sequence
 * @returns Status of the last executed task.
 */
export async function runTaskSequence(
    tasks: vscode.Task[],
    writeEmitter?: vscode.EventEmitter<string>,
    onTaskFailure?: (task: vscode.Task, exitCode: number) => void,
    token?: vscode.CancellationToken,
): Promise<number> {
    for (const task of tasks) {
        if (token?.isCancellationRequested) return 1;
        writeEmitter?.fire(`Executing task: ${getConventionalTaskLabel(task)}\r\n`);
        const exitCode = await runTaskAndGetResult(task, token);
        if (exitCode !== 0) {
            if (exitCode !== undefined && !token?.isCancellationRequested)
                onTaskFailure?.(task, exitCode);
            return exitCode ?? 1;
        }
    }
    return 0;
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
 * @returns Array of tasks of type `ada` and label starting with the given prefix.
 * This includes tasks automatically provided by the extension as well
 * as user-defined tasks in tasks.json.
 */
export async function getTasksWithPrefix(prefix: string): Promise<vscode.Task[]> {
    return await vscode.tasks.fetchTasks({ type: TASK_TYPE_ADA }).then((tasks) =>
        tasks
            // Filter to tasks starting with a conventional name that has the given
            // prefix.
            .filter((t) => getConventionalTaskLabel(t).startsWith(prefix))

            // Return workspace-defined tasks first
            .sort(workspaceTasksFirst),
    );
}

export async function findBuildAndRunTask(
    adaMain: AdaMain,
    useGNATemulator: boolean = false,
): Promise<vscode.Task | undefined> {
    const label = useGNATemulator
        ? getBuildAndRunGNATemulatorTaskName(adaMain)
        : getBuildAndRunTaskName(adaMain);

    const tasks = await getTasksWithPrefix(label);

    return tasks.length == 1 ? tasks[0] : undefined;
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
 * @returns the label typically generated for that task by vscode. For tasks
 * not defined explicitly in the workspace, this is `<task type>: <task name>`.
 * For tasks defined in the workspace simply return the name which should
 * already include the convention.
 */
export function getConventionalTaskLabel(task: vscode.Task | PredefinedTask): string {
    if (task instanceof vscode.Task) {
        return isFromWorkspace(task) ? task.name : `${task.source}: ${task.name}`;
    } else {
        return `${task.taskDef.type}: ${task.label}`;
    }
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
            .sort(workspaceTasksFirst),
    );
}

export async function findBuildMainTask(adaMain: AdaMain): Promise<vscode.Task | undefined> {
    return (await getBuildMainTasks()).find(
        // Tasks defined in tasks.json will have a leading 'ada: ' while the
        // ones auto-generated by the extension don't. We want to match both.
        (t) => getConventionalTaskLabel(t) == getBuildTaskName(adaMain),
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
            args.splice(0, args.length, 'build', '--', "'-cargs:ada'", '-gnatef');
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

/**
 * Execute the given task, wait until it finishes and return the underlying
 * process exit code.
 *
 * @param task - a {@link vscode.Task}
 * @param token - optional cancellation token to terminate the task
 * @returns a Promise that resolves to the underlying process exit code when the
 * task finishes execution.
 */
export function runTaskAndGetResult(
    task: vscode.Task,
    token?: vscode.CancellationToken,
): Promise<number | undefined> {
    if (token?.isCancellationRequested) return Promise.resolve(undefined);
    return new Promise((resolve, reject) => {
        let execution: vscode.TaskExecution | undefined;
        let finished = false;
        const disposables: vscode.Disposable[] = [];
        const finish = (exitCode?: number, error?: Error) => {
            if (finished) return;
            finished = true;
            disposables.forEach((d) => {
                d.dispose();
            });
            if (error) reject(error);
            else resolve(exitCode);
        };
        // executeTask resolves when the task starts, so subscribe to completion
        // before launching it, including for tasks that finish immediately.
        disposables.push(
            vscode.tasks.onDidEndTaskProcess((e) => {
                if (e.execution.task === task && !token?.isCancellationRequested)
                    finish(e.exitCode);
            }),
        );
        if (token) {
            disposables.push(
                token.onCancellationRequested(() => {
                    // Stop waiting even if task startup has not returned a handle yet.
                    finish();
                    execution?.terminate();
                }),
            );
        }
        Promise.resolve()
            .then(() => {
                if (token?.isCancellationRequested) throw new vscode.CancellationError();
                return vscode.tasks.executeTask(task);
            })
            .then(
                (started) => {
                    execution = started;
                    // Cancellation may have happened before executeTask resolved.
                    if (token?.isCancellationRequested) {
                        finish();
                        execution.terminate();
                    }
                },
                (reason: unknown) => {
                    if (
                        token?.isCancellationRequested ||
                        reason instanceof vscode.CancellationError
                    ) {
                        finish();
                    } else {
                        const detail = reason instanceof Error ? reason.message : String(reason);
                        finish(
                            undefined,
                            new Error(
                                `Could not execute task '${getConventionalTaskLabel(task)}': ${detail}`,
                            ),
                        );
                    }
                },
            );
    });
}

/**
 *
 * Search for a task of a given name, or create one if no matching existing task is found.
 *
 * @param taskPlainName - plain name of the task (without the task type prefix)
 * @param taskDef - callback to create a task definition if a matching existing task is not found
 * @param taskType - type identifier of the task
 * @returns an existing task with a matching name if found, otherwise creates a
 * task of that name using the callback to create the definition
 */
export async function getOrCreateTask(
    taskPlainName: string,
    taskDef: () => Promise<vscode.TaskDefinition>,
    taskType = TASK_TYPE_ADA,
): Promise<vscode.Task> {
    const adaTP = adaExtState.getAdaTaskProvider();
    assert(adaTP);
    const existingTask = await findTaskByName(`${taskType}: ${taskPlainName}`).then(
        undefined,
        /**
         * Return undefined in case of errors when searching for the task.
         */
        () => undefined,
    );
    let task;
    if (existingTask) {
        task = existingTask;
    } else {
        /**
         * If there's no existing task of that name, create one on the fly.
         */
        task = (await adaTP.resolveTask(
            new vscode.Task(
                await taskDef(),
                vscode.TaskScope.Workspace,
                taskPlainName,
                taskType,
                undefined,
                DEFAULT_PROBLEM_MATCHERS,
            ),
        ))!;
        task.presentationOptions.reveal =
            task.presentationOptions.reveal ?? vscode.TaskRevealKind.Never;
    }
    return task;
}
