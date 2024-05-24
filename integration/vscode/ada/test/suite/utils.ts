import assert from 'assert';
import { spawnSync } from 'child_process';
import { existsSync, readFileSync, writeFileSync } from 'fs';
import * as vscode from 'vscode';
import {
    SimpleTaskProvider,
    findTaskByName,
    getConventionalTaskLabel,
} from '../../src/taskProviders';
import { setTerminalEnvironment } from '../../src/helpers';

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
export async function getCommandLines(prov: SimpleTaskProvider) {
    const tasks = await prov.provideTasks();
    assert(tasks);

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
export async function runTaskAndGetResult(task: vscode.Task): Promise<number | undefined> {
    return await new Promise((resolve, reject) => {
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
                reject(
                    Error(
                        `The task '${getConventionalTaskLabel(
                            task
                        )}' was not started, likely due to an error`
                    )
                );
            }
        }, 3000);

        void vscode.tasks.executeTask(task);
    });
}
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

export async function testTask(
    taskName: string,
    allProvidedTasks: vscode.Task[],
    testedTasks: Set<string>
) {
    assert(vscode.workspace.workspaceFolders);

    const task = findTaskByName(allProvidedTasks, taskName);
    assert(task);
    testedTasks.add(getConventionalTaskLabel(task));

    const execStatus: number | undefined = await runTaskAndGetResult(task);

    if (execStatus != 0) {
        let msg = `Got status ${execStatus ?? "'undefined'"} for task '${taskName}'`;
        if (task.execution instanceof vscode.ShellExecution) {
            msg += ` with command line: ${getCmdLine(task.execution)}`;

            try {
                /**
                 * Let's try re-running the command line explicitlely to obtain its output.
                 */
                const cwd = vscode.workspace.workspaceFolders[0].uri.fsPath;
                msg += `\nTrying to re-run the command explicitly in: ${cwd}`;
                const env = { ...process.env };
                setTerminalEnvironment(env);
                const cp = spawnSync(
                    typeof task.execution.command == 'string'
                        ? task.execution.command
                        : task.execution.command.value,
                    task.execution.args.map((arg) => {
                        if (typeof arg == 'string') {
                            return arg;
                        } else {
                            return arg.value;
                        }
                    }),
                    { cwd: cwd, env: env }
                );

                // eslint-disable-next-line @typescript-eslint/restrict-template-expressions
                msg += `\nProcess ended with exit code ${cp.status} and output:\n`;
                // msg += cp.stdout.toString() + cp.stderr.toString();
                msg += cp.output.map((b) => (b != null ? b.toString() : '')).join('');
            } catch (error) {
                // eslint-disable-next-line @typescript-eslint/restrict-template-expressions
                msg += `\nEncountered an error: ${error}`;
            }
        }

        assert.fail(msg);
    }
}

export async function closeAllEditors() {
    while (vscode.window.activeTextEditor) {
        await vscode.commands.executeCommand('workbench.action.closeActiveEditor');
    }
}
