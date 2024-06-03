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

import commandExists from 'command-exists';
import * as vscode from 'vscode';
import { getProjectFromConfigOrALS, sparkLimitRegionArg, sparkLimitSubpArg } from './commands';
import { DEFAULT_PROBLEM_MATCHER, WarningMessageExecution } from './taskProviders';

/**
 * Callback to provide an extra argument for a tool
 */
type ExtraArgCallback = () => Promise<string[]>;

/**
 * Tool description
 */
interface TaskProperties {
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
 * Map of known tasks/tools indexed by a string/taskKind
 */
const allTaskProperties: { [id in AllTaskKinds]: TaskProperties } = {
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

/**
 * @deprecated This interface defines the data structures matching the JSON
 * schema for deprecated 'gnat' tasks. We use it for better type checking in
 * code providing backwards compatibility with deprecated 'gnat' tasks.
 */
interface GnatTaskDefinition extends vscode.TaskDefinition {
    type: 'gnat';
    taskKind: string;
    projectFile?: string;
    args?: string[];
}

/**
 *
 * @deprecated This class handles the deprecated task type 'gnat'. It is kept in
 * order to preserve support for 'gnat' task types that still exist in User
 * workspaces.
 */
export class GnatTaskProvider implements vscode.TaskProvider<vscode.Task> {
    /**
     * This flag is used to restore the proposal of 'gnat' tasks for debugging
     * purposes.
     */
    public static DEPRECATED = true;

    public static gnatType = 'gnat' as const; // Task provider name
    gnatTasks: vscode.Task[] | undefined; // Known tasks

    private readonly obsoletionMsg =
        "Tasks of type 'gnat' are obsolete. Please use tasks of type 'ada' or 'spark'.";

    private readonly obsoleteWarningExecution = new WarningMessageExecution(this.obsoletionMsg);

    constructor() {
        this.gnatTasks = undefined; //  Do we really need this???
    }

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    provideTasks(_token: vscode.CancellationToken): vscode.ProviderResult<vscode.Task[]> {
        if (GnatTaskProvider.DEPRECATED) {
            // We return a single dummy task to convey an obsoletion message to Users.
            const msg = this.obsoletionMsg;
            return [
                new vscode.Task(
                    {
                        type: GnatTaskProvider.gnatType,
                        taskKind: 'dummy',
                        projectFile: '${config:ada.projectFile}',
                    },
                    vscode.TaskScope.Workspace,
                    msg,
                    GnatTaskProvider.gnatType,
                    this.obsoleteWarningExecution,
                    DEFAULT_PROBLEM_MATCHER
                ),
            ];
        } else {
            // Provide the set of tasks as deprecated 'gnat' tasks for debugging
            if (!this.gnatTasks) {
                return getTasks().then((list) => {
                    this.gnatTasks = list;
                    return list;
                });
            }
            return this.gnatTasks;
        }
    }

    resolveTask(
        task: vscode.Task,
        // eslint-disable-next-line @typescript-eslint/no-unused-vars
        _token: vscode.CancellationToken
    ): vscode.ProviderResult<vscode.Task> {
        /**
         * Here we resolve 'gnat' tasks still defined in User workspaces. We
         * handle them by displaying an obsoletion warning if the task gets
         * executed.
         */
        return new vscode.Task(
            task.definition,
            task.scope ?? vscode.TaskScope.Workspace,
            task.name,
            GnatTaskProvider.gnatType,
            this.obsoleteWarningExecution,
            DEFAULT_PROBLEM_MATCHER
        );
    }
}

/**
 *
 * @deprecated This function creates obsolete tasks of type 'gnat'. It is kept
 * temporarily for debugging backwards support of 'gnat' tasks. It can be activated
 * with {@link GnatTaskProvider.DEPRECATED} to create 'gnat' tasks like before.
 */

export async function getTasks(): Promise<vscode.Task[]> {
    return alire().then(async (alr) => {
        const result: vscode.Task[] = [];

        for (const taskKind in allTaskProperties) {
            const item: TaskProperties = allTaskProperties[taskKind as AllTaskKinds];
            const title: string = item.title;
            const definition: GnatTaskDefinition = {
                type: GnatTaskProvider.gnatType,
                projectFile: '${config:ada.projectFile}',
                taskKind: taskKind,
            };
            const extraArgsFromTask = item.extra ? item.extra() : [];
            const cmd = alr.concat(
                item.command ?? [],
                await getProjectArgs(),
                getScenarioArgs(),
                await extraArgsFromTask,
                getDiagnosticArgs()
            );
            const shell = new vscode.ShellExecution(cmd[0], cmd.slice(1));
            const task = new vscode.Task(
                definition,
                vscode.TaskScope.Workspace,
                title,
                'gnat',
                shell,
                '$ada'
            );
            task.group = vscode.TaskGroup.Build;
            result.push(task);
        }

        return result;
    });
}
export async function getProjectArgs() {
    return await computeProject()
        .then((prj) => ['-P', prj])
        .catch(() => []);
}
export async function computeProject(): Promise<string> {
    // If the task definition defines a project file, use that. Otherwise if
    // ada.projectFile is defined, use ${config:ada.projectFile}. Finally,
    // fallback to querying the ALS for the full path to the project file.
    return getProjectFromConfigOrALS();
} // Call commonArgs on args and append `-gnatef` to generate full file names in errors/warnings

export const getDiagnosticArgs = (): string[] => {
    const p_gnatef = ['-cargs:ada', '-gnatef'];
    return p_gnatef;
};
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
