import assert from 'assert';
import { spawnSync } from 'child_process';
import { existsSync, readFileSync, writeFileSync } from 'fs';
import path from 'path';
import * as vscode from 'vscode';
import { CodeLens, Uri, window, workspace } from 'vscode';
import { adaExtState } from '../../src/extension';
import { getArgValue, setTerminalEnvironment } from '../../src/helpers';
import {
    SimpleTaskProvider,
    findTaskByName,
    getConventionalTaskLabel,
} from '../../src/taskProviders';

/**
 * This function compares some actual output to an expected referenced stored in
 * a file.
 *
 * If the environment variable MOCHA_ALS_UPDATE is set, the function overwrites
 * the expected reference with the current actual output.
 *
 * @param actual - actual output to be compared with the reference
 * @param expectedUri - path to the file containing the expected output
 */
export function assertEqualToFileContent(actual: string, expectedUri: vscode.Uri) {
    // Normalize the actual string
    actual = normalizeLineEndings(actual);

    if (update()) {
        writeFileSync(expectedUri.fsPath, actual);
    } else {
        if (!existsSync(expectedUri.fsPath)) {
            throw Error(`Expected output file does not exist: ${expectedUri.fsPath}`);
        }

        const expected: string = readFileSync(expectedUri.fsPath, 'utf-8');

        assert.strictEqual(actual, expected);
    }
}

/**
 *
 * Normalize line endings in the given string to the `\n` or to `lineEnding` if
 * given.
 */
export function normalizeLineEndings(str: string, lineEnding = '\n'): string {
    return str.replace(/\r?\n/g, lineEnding);
}

/**
 *
 * @returns true if the testsuite is running in update mode, i.e. the
 * environment variable MOCHA_ALS_UPDATE is set. For example, the VS Code
 * workspace of this repository provides a launch configuration with that
 * environment variable set to allow quickly updating test references.
 */
export function update(): boolean {
    return process.env.MOCHA_ALS_UPDATE ? true : false;
}

/**
 * This function queries the VS Code API for the Ada extension and waits until
 * it is activated.
 */
export async function activate(): Promise<void> {
    const ext = vscode.extensions.getExtension('AdaCore.ada');
    assert(ext);
    /**
     * Previously this code returned when ext.isActive was true. This is not
     * enough because it doesn't indicate if any errors occured during
     * activation. Instead, always awaiting the result of the activate() method
     * does report activation errors as a promise rejection.
     */
    await ext.activate();
}

/**
 *
 * @param prov - a TaskProvider
 * @returns a string representation of the subset of tasks offered by the
 * provider that are based on a ShellExecution. The string includes the command
 * line of each task.
 */
export async function getCommandLines(
    prov: SimpleTaskProvider,
    filter?: (t: vscode.Task) => boolean
) {
    let tasks = await prov.provideTasks();
    assert(tasks);

    if (filter) {
        tasks = tasks.filter(filter);
    }

    const actualCommandLines = (
        await Promise.all(
            tasks.map(async (t) => {
                return { task: t, execution: (await prov.resolveTask(t))?.execution };
            })
        )
    )
        .filter(function ({ execution }) {
            return execution instanceof vscode.ShellExecution;
        })
        .map(function ({ task, execution }) {
            assert(execution instanceof vscode.ShellExecution);
            return `${task.source}: ${task.name} - ${getCmdLine(execution)}`;
        })
        .join('\n');
    return actualCommandLines;
}

/**
 * Execute the given task, wait until it finishes and return the underlying
 * process exit code.
 *
 * @param task - a {@link vscode.Task}
 * @returns a Promise that resolves to the underlying process exit code when the
 * task finishes execution.
 */
export async function runTaskAndGetResult(task: vscode.Task): Promise<number | undefined> {
    return await new Promise<number | undefined>((resolve, reject) => {
        let started = false;

        const startDisposable = vscode.tasks.onDidStartTask((e) => {
            if (e.execution.task == task) {
                /**
                 * Task was started, let's listen to the end.
                 */
                started = true;
                startDisposable.dispose();
            }
        });

        const endDisposable = vscode.tasks.onDidEndTaskProcess((e) => {
            if (e.execution.task == task) {
                endDisposable.dispose();
                resolve(e.exitCode);
            }
        });

        setTimeout(() => {
            /**
             * If the task has not started within the timeout below, it means an
             * error occured during startup. Reject the promise.
             */
            if (!started) {
                const msg = `The task '${getConventionalTaskLabel(
                    task
                )}' was not started, likely due to an error.\n`;
                reject(Error(msg));
            }
        }, 3000);

        void vscode.tasks.executeTask(task);
    }).catch(async (reason) => {
        if (reason instanceof Error) {
            let msg = 'The current list of tasks is:\n';
            msg += await vscode.tasks.fetchTasks({ type: task.definition.type }).then(
                (list) => list.map(getConventionalTaskLabel).join('\n'),
                // eslint-disable-next-line @typescript-eslint/restrict-template-expressions
                (reason) => `fetchTasks promise was rejected: ${reason}`
            );

            reason.message += '\n' + msg;
        }

        return Promise.reject(reason);
    });
}

/**
 *
 * @param exec - a ShellExecution
 * @returns the command line of the ShellExecution as a string
 */
export function getCmdLine(exec: vscode.ShellExecution) {
    return [exec.command]
        .concat(exec.args)
        .map((s) => {
            if (typeof s == 'object') {
                return s.value;
            } else {
                return s;
            }
        })
        .join(' ');
}

/**
 *
 * @param taskName - the name of the task to test
 * @param testedTasks - an array where the given task will be pushed to indicate
 * that it has been tested.
 * @param allProvidedTasks - an optional array of tasks where the task name
 * should be looked up. If not given, {@link vscode.tasks.fetchTasks} is used.
 */
export async function testTask(
    taskName: string,
    testedTasks?: Set<string>,
    allProvidedTasks?: vscode.Task[]
) {
    assert(vscode.workspace.workspaceFolders);

    const task = await findTaskByName(taskName, allProvidedTasks);
    assert(task);
    testedTasks?.add(getConventionalTaskLabel(task));

    const execStatus: number | undefined = await runTaskAndGetResult(task);

    if (execStatus != 0) {
        let msg = `Got status ${execStatus ?? "'undefined'"} for task '${taskName}'`;
        if (task.execution instanceof vscode.ShellExecution) {
            const cmdLine = [task.execution.command].concat(task.execution.args).map((arg) => {
                return getArgValue(arg);
            });
            msg += ` with command line: ${cmdLine.join(' ')}`;

            try {
                /**
                 * Let's try re-running the command line explicitlely to obtain its output.
                 */
                const cwd = vscode.workspace.workspaceFolders[0].uri.fsPath;
                msg += `\nTrying to re-run the command explicitly in: ${cwd}`;
                const env = { ...process.env };
                setTerminalEnvironment(env);
                const cp = spawnSync(cmdLine[0], cmdLine.slice(1), { cwd: cwd, env: env });

                if (cp.status != null) {
                    // eslint-disable-next-line @typescript-eslint/restrict-template-expressions
                    msg += `\nProcess ended with exit code ${cp.status} and output:\n`;
                    // msg += cp.stdout.toString() + cp.stderr.toString();
                    msg += cp.output?.map((b) => (b != null ? b.toString() : '')).join('');
                } else if (cp.signal != null) {
                    // eslint-disable-next-line @typescript-eslint/restrict-template-expressions
                    msg += `\nProcess ended with signal: ${cp.signal}`;
                } else if (cp.error != undefined) {
                    throw cp.error;
                }
            } catch (error) {
                // eslint-disable-next-line @typescript-eslint/restrict-template-expressions
                msg += `\nEncountered an error: ${error}`;
                // eslint-disable-next-line @typescript-eslint/restrict-template-expressions
                if (`${error}`.includes('ENOENT')) {
                    msg += '\nIt is likely that the executable is not on PATH';
                }
            }
        }

        assert.fail(msg);
    }
}

/**
 * Call the `workbench.action.closeActiveEditor` command to close all open editors.
 */
export async function closeAllEditors() {
    while (vscode.window.activeTextEditor) {
        await vscode.commands.executeCommand('workbench.action.closeActiveEditor');
    }
}

/**
 *
 * @param srcRelPath - a path to a source file relative to the workspace root
 * @returns a text editor opened for the source file.
 */
export async function showTextDocument(...srcRelPath: string[]) {
    const mainUri = getWsUri(...srcRelPath);
    const textEditor = await window.showTextDocument(mainUri);
    return textEditor;
}

/**
 *
 * @param srcRelPath - a path relative to the workspace root
 * @returns a {@link vscode.Uri} representing the given path.
 */
export function getWsUri(...srcRelPath: string[]) {
    assert(workspace.workspaceFolders !== undefined);
    const rootUri = workspace.workspaceFolders[0].uri;
    const uri = Uri.joinPath(rootUri, ...srcRelPath);
    return uri;
}

/**
 * Opens the given source file in an editor and returns the CodeLenses provided
 * for that file.
 *
 * @param srcRelPath - relative path of a source file in the workspace
 * @returns array of CodeLenses provided for that source file.
 */
export async function getCodeLenses(...srcRelPath: string[]) {
    const textEditor = await showTextDocument(...srcRelPath);
    const codelenses = await adaExtState.codelensProvider.provideCodeLenses(textEditor.document);
    return codelenses ?? [];
}

/**
 * A testing utility to simplify CodeLenses for convenient comparison with
 * expected results. This selects a subset of properties of CodeLenses and
 * converts Uris to relative Posix paths and ranges to a convenient string
 * representation.
 */
export function simplifyCodelenses(cls: CodeLens[]) {
    return cls.map((cl) => ({
        range: rangeToStr(cl.range),
        command: {
            title: cl.command?.title,
            command: cl.command?.command,
            arguments: cl.command?.arguments?.map((a) =>
                // eslint-disable-next-line @typescript-eslint/no-unsafe-return
                a instanceof Uri
                    ? /**
                       * Normalize URI to a relative Posix path.
                       */
                      workspace.asRelativePath(a.fsPath).split(path.sep).join(path.posix.sep)
                    : a instanceof vscode.Range
                    ? /**
                       * Represent Ranges as a string
                       */
                      rangeToStr(a)
                    : a
            ),
        },
    }));
}

/**
 *
 * @param codelenses - array of CodeLens
 * @returns a JSON string representation of the given CodeLenses.
 */
export function codeLensesToString(codelenses: CodeLens[]): string {
    return JSON.stringify(simplifyCodelenses(codelenses), null, 2);
}

/**
 *
 * @param range - a {@link vscode.Range}
 * @returns a string representation of the range, convenient for comparison to
 * references in testing.
 */
export function rangeToStr(range: vscode.Range): string {
    // eslint-disable-next-line max-len
    return `${range.start.line}:${range.start.character} -> ${range.end.line}:${range.end.character}`;
}

/**
 * Utility filter for selecting GNAT SAS tasks.
 */
export function isGNATSASTask(t: vscode.Task): boolean {
    return t.name.includes('GNAT SAS');
}

/**
 * Utility function for creating a predicated that is the negation of another predicate.
 */
export function negate<T extends unknown[]>(predicate: (...args: T) => boolean) {
    return (...inputs: T) => !predicate(...inputs);
}

/**
 * Utility function for creating a predicated that is the negation of another predicate.
 */
export function and<T extends unknown[]>(...predicates: ((...args: T) => boolean)[]) {
    return (...inputs: T) => {
        return predicates.reduce<boolean>((acc, cur) => acc && cur(...inputs), true);
    };
}

/**
 * Utility filter for selecting GNATtest tasks.
 */
export function isGNATTestTask(t: vscode.Task): boolean {
    return t.name.includes('test skeleton');
}

/**
 * Utility filter for selecting core tasks that are not related to other tools
 * such as GNAT SAS or GNATtest.
 */
export const isCoreTask = and(negate(isGNATSASTask), negate(isGNATTestTask));
