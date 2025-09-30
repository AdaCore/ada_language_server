import * as vscode from 'vscode';
import { commands } from 'vscode';
import { CMD_SPARK_ASK_OPTIONS } from './constants';
import { sparkTasks, TASK_TYPE_SPARK } from './taskProviders';

/**
 * The following commands are wrappers around VS Code tasks that allow setting
 * key shortcuts to the wrapped tasks. Technically it is possible to set
 * shortcuts directly on the `workbench.action.tasks.runTask` command with the
 * target task as a command argument, however in several places the UI doesn't
 * take into consideration the command argument, and thus it becomes impossible
 * to distinguish the different tasks, and worse, our shortcut becomes
 * displayed for the vanilla `Run Task` command.
 *
 * To avoid all that, we provide these commands as wrappers.
 *
 * In addition, these wrappers also prompt for GNATprove options before running
 * the task.
 */
export function registerSPARKTaskWrappers(context: vscode.ExtensionContext) {
    for (const task of sparkTasks) {
        if (task.commandId) {
            context.subscriptions.push(
                commands.registerCommand(
                    task.commandId,
                    sparkTaskWrapper(task.label, !task.noOptionPicker),
                ),
            );
        }
    }
}

function sparkTaskWrapper(taskPlainName: string, optionPicker = true) {
    return async () => {
        if (optionPicker) {
            await commands.executeCommand(CMD_SPARK_ASK_OPTIONS);
        }
        await commands.executeCommand(
            'workbench.action.tasks.runTask',
            `${TASK_TYPE_SPARK}: ${taskPlainName}`,
        );
    };
}
