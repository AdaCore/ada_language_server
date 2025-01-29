import assert from 'assert';
import { existsSync } from 'fs';
import { basename } from 'path';
import * as vscode from 'vscode';
import { SymbolKind, commands } from 'vscode';
import { Disposable } from 'vscode-jsonrpc';
import { ExecuteCommandRequest } from 'vscode-languageclient';
import { ExtensionState } from './ExtensionState';
import { AdaConfig, getOrAskForProgram, initializeConfig } from './debugConfigProvider';
import { adaExtState, logger, mainOutputChannel } from './extension';
import { findAdaMain, getProjectFileRelPath, getSymbols } from './helpers';
import {
    DEFAULT_PROBLEM_MATCHER,
    SimpleTaskDef,
    TASK_PROVE_FILE_PLAIN_NAME,
    TASK_PROVE_LINE_PLAIN_NAME,
    TASK_PROVE_REGION_PLAIN_NAME,
    TASK_PROVE_SUPB_PLAIN_NAME,
    TASK_TYPE_SPARK,
    findBuildAndRunTask,
    getBuildAndRunTasks,
    getConventionalTaskLabel,
    isFromWorkspace,
    workspaceTasksFirst,
} from './taskProviders';
import { createHelloWorldProject, walkthroughStartDebugging } from './walkthrough';

/**
 * Identifier for a hidden command used for building and running a project main.
 * The command accepts a parameter which is the URI of the main source file.
 * It is triggered by CodeLenses provided by the extension.
 *
 * @see {@link buildAndRunSpecifiedMain}
 */
export const CMD_BUILD_AND_RUN_MAIN = 'ada.buildAndRunMain';

/**
 * Identifier for a hidden command used for building and debugging a project main.
 * The command accepts a parameter which is the URI of the main source file.
 * It is triggered by CodeLenses provided by the extension.
 *
 * @see {@link buildAndDebugSpecifiedMain}
 */
export const CMD_BUILD_AND_DEBUG_MAIN = 'ada.buildAndDebugMain';

/**
 * Identifier for a hidden command that returns an array of strings constituting
 * the -P and -X project and scenario arguments.
 */
export const CMD_GPR_PROJECT_ARGS = 'ada.gprProjectArgs';

/**
 * Identifier for a hidden command that returns a string referencing the current
 * project.  That string is either `"$\{config:ada.projectFile\}"` if that
 * setting is configured, or otherwise the full path to the project file
 * returned from a query to the
 */
export const CMD_GET_PROJECT_FILE = 'ada.getProjectFile';

export const CMD_SPARK_LIMIT_SUBP_ARG = 'ada.spark.limitSubpArg';
export const CMD_SPARK_LIMIT_REGION_ARG = 'ada.spark.limitRegionArg';
export const CMD_SPARK_PROVE_SUBP = 'ada.spark.proveSubprogram';

export function registerCommands(context: vscode.ExtensionContext, clients: ExtensionState) {
    context.subscriptions.push(
        vscode.commands.registerCommand('ada.createHelloWorldProject', createHelloWorldProject),
    );
    context.subscriptions.push(
        vscode.commands.registerCommand('ada.walkthroughStartDebugging', walkthroughStartDebugging),
    );
    context.subscriptions.push(vscode.commands.registerCommand('ada.otherFile', otherFileHandler));
    context.subscriptions.push(
        vscode.commands.registerCommand('ada.subprogramBox', addSubprogramBoxCommand),
    );
    context.subscriptions.push(
        vscode.commands.registerCommand('ada.showExtensionOutput', () => mainOutputChannel.show()),
    );
    context.subscriptions.push(
        vscode.commands.registerCommand('ada.showAdaLSOutput', () =>
            clients.adaClient.outputChannel.show(),
        ),
    );
    context.subscriptions.push(
        vscode.commands.registerCommand('ada.showGprLSOutput', () =>
            clients.gprClient.outputChannel.show(),
        ),
    );

    context.subscriptions.push(
        vscode.commands.registerCommand('ada.buildAndRunMainLast', buildAndRunMainLast),
    );
    context.subscriptions.push(
        vscode.commands.registerCommand('ada.buildAndRunMainAsk', buildAndRunMainAsk),
    );

    // This is a hidden command that gets called in the default debug
    // configuration snippet that gets offered in the launch.json file.
    // It is expected to return the relative path of the main program chosen for
    // debugging.
    context.subscriptions.push(
        vscode.commands.registerCommand('ada.getOrAskForProgram', async () => {
            const p = await getOrAskForProgram();
            return p?.execRelPath();
        }),
    );

    context.subscriptions.push(
        vscode.commands.registerCommand(
            'ada.addMissingDirsToWorkspace',
            async (
                atStartup: boolean = false,

                displayYesNoPopup: boolean = true,
            ) => {
                await checkSrcDirectories(atStartup, displayYesNoPopup);
            },
        ),
    );

    /**
     * The following commands are not defined in package.json and hence not
     * exposed through the command palette but are called from CodeLenses.
     */
    context.subscriptions.push(
        vscode.commands.registerCommand(CMD_BUILD_AND_RUN_MAIN, buildAndRunSpecifiedMain),
    );
    context.subscriptions.push(
        vscode.commands.registerCommand(CMD_BUILD_AND_DEBUG_MAIN, buildAndDebugSpecifiedMain),
    );

    context.subscriptions.push(
        vscode.commands.registerCommand(CMD_GPR_PROJECT_ARGS, gprProjectArgs),
    );
    context.subscriptions.push(
        vscode.commands.registerCommand(CMD_GET_PROJECT_FILE, getProjectFromConfigOrALS),
    );
    context.subscriptions.push(
        vscode.commands.registerCommand(CMD_SPARK_LIMIT_SUBP_ARG, sparkLimitSubpArg),
    );
    context.subscriptions.push(
        vscode.commands.registerCommand(CMD_SPARK_LIMIT_REGION_ARG, sparkLimitRegionArg),
    );
    context.subscriptions.push(
        vscode.commands.registerCommand(CMD_SPARK_PROVE_SUBP, sparkProveSubprogram),
    );

    registerTaskWrappers(context);
}

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
 */
function registerTaskWrappers(context: vscode.ExtensionContext) {
    const sparkTaskWrappers: { [cmdId: string]: string } = {
        'ada.spark.tasks.proveFile': `${TASK_TYPE_SPARK}: ${TASK_PROVE_FILE_PLAIN_NAME}`,
        'ada.spark.tasks.proveSubprogram': `${TASK_TYPE_SPARK}: ${TASK_PROVE_SUPB_PLAIN_NAME}`,
        // eslint-disable-next-line max-len
        'ada.spark.tasks.proveSelectedRegion': `${TASK_TYPE_SPARK}: ${TASK_PROVE_REGION_PLAIN_NAME}`,
        'ada.spark.tasks.proveLine': `${TASK_TYPE_SPARK}: ${TASK_PROVE_LINE_PLAIN_NAME}`,
    };
    for (const cmdId of Object.keys(sparkTaskWrappers)) {
        const taskId = sparkTaskWrappers[cmdId];
        context.subscriptions.push(
            commands.registerCommand(cmdId, () =>
                commands.executeCommand('workbench.action.tasks.runTask', taskId),
            ),
        );
    }
}

/**
 * Add a subprogram box above the subprogram enclosing the cursor's position, if any.
 *
 * @example
 *
 *  -------
 *  - Foo -
 *  -------
 *
 *  procedure Foo is
 */
async function addSubprogramBoxCommand() {
    const activeEditor = vscode.window.activeTextEditor;

    await getEnclosingSymbol(activeEditor, [
        SymbolKind.Function,
        SymbolKind.Package,
        SymbolKind.Module,
    ]).then(async (symbol) => {
        if (symbol !== null) {
            const name: string = symbol.name ?? '';
            const insertPos = new vscode.Position(symbol.range.start.line, 0);
            const indentationRange = new vscode.Range(insertPos, symbol.range.start);
            const indentation: string = activeEditor?.document.getText(indentationRange) ?? '';
            const eol: string = activeEditor?.document.eol == vscode.EndOfLine.CRLF ? '\r\n' : '\n';

            // Generate the subprogram box after retrieving the indentation of the line of
            // the subprogram's body declaration.
            const text: string =
                indentation +
                '---' +
                '-'.repeat(name.length) +
                '---' +
                eol +
                indentation +
                '-- ' +
                name +
                ' --' +
                eol +
                indentation +
                '---' +
                '-'.repeat(name.length) +
                '---' +
                eol +
                eol;

            if (activeEditor) {
                await activeEditor.edit((editBuilder) => {
                    editBuilder.insert(insertPos, text);
                });
            }
        }
    });
}

let lastUsedTaskInfo: { source: string; name: string } | undefined;

/**
 * If a task was previously run through the commands `ada.buildAndRunMainAsk` or
 * `ada.buildAndRunMainLast`, re-run the same task. If not, defer to {@link buildAndRunMainAsk}
 * to ask the User to select a task to run.
 *
 * @returns the TaskExecution corresponding to the task.
 */
async function buildAndRunMainLast() {
    const buildAndRunTasks = await getBuildAndRunTasks();
    if (lastUsedTaskInfo) {
        const matchingTasks = buildAndRunTasks.filter(matchesLastUsedTask);
        assert(matchingTasks.length <= 1);
        const lastTask = matchingTasks.length == 1 ? matchingTasks[0] : undefined;
        if (lastTask) {
            return await vscode.tasks.executeTask(lastTask);
        }
    }

    // No task was run so far, or the last one run no longer exists
    return buildAndRunMainAsk();
}

/**
 *
 * @param t - a task
 * @returns `true` if the given task matches the last executed task
 */
function matchesLastUsedTask(t: vscode.Task): boolean {
    return t.source == lastUsedTaskInfo?.source && t.name == lastUsedTaskInfo?.name;
}

/**
 *
 * @param task - a task
 * @returns the label to be displayed to the user in the quick picker for that task
 */
function getTaskLabel(task: vscode.Task): string {
    return isFromWorkspace(task) ? `(From Workspace) ${task.name}` : getConventionalTaskLabel(task);
}

interface TaskQuickPickItem extends vscode.QuickPickItem {
    task: vscode.Task;
}

/**
 * Propose to the User a list of build and run tasks, one for each main defined
 * in the project.
 *
 * Tasks defined explicitly in the workspace are identified as such in the
 * offered list and proposed first.
 *
 * The User can choose either to run the task as is, or click the secondary
 * button to add the task to tasks.json (if not already there) and configure it
 * there.
 */
async function buildAndRunMainAsk() {
    function createQuickPickItem(task: vscode.Task): TaskQuickPickItem {
        return {
            // Mark the last used task with a leading star
            label: (matchesLastUsedTask(task) ? '$(star) ' : '') + getTaskLabel(task),
            // Add a description to the last used task
            description: matchesLastUsedTask(task) ? 'last used' : undefined,
            task: task,
            // Add a button allowing to configure the task in tasks.json
            buttons: [
                {
                    iconPath: new vscode.ThemeIcon('gear'),
                    tooltip: 'Configure task in tasks.json, e.g. to add main arguments',
                },
            ],
        };
    }
    const adaTasksMain = await getBuildAndRunTasks();

    if (adaTasksMain.length > 0) {
        const tasksFromWorkspace = adaTasksMain.filter(isFromWorkspace);
        const tasksFromExtension = adaTasksMain.filter((v) => !isFromWorkspace(v));

        // Propose workspace-configured tasks first
        const quickPickItems: TaskQuickPickItem[] = tasksFromWorkspace.map(createQuickPickItem);

        if (tasksFromWorkspace.length > 0) {
            // Use a separator between workspace tasks and implicit tasks provided by the extension
            quickPickItems.push({
                kind: vscode.QuickPickItemKind.Separator,
                label: '',
                // Use any valid task to avoid allowing 'undefined' in the type declaration
                task: adaTasksMain[0],
            });
        }

        quickPickItems.push(...tasksFromExtension.map(createQuickPickItem));

        // Create the quick picker
        const qp = vscode.window.createQuickPick<TaskQuickPickItem>();
        qp.items = qp.items.concat(quickPickItems);

        // Array for event handlers to be disposed after the quick picker is disposed
        const disposables: Disposable[] = [];
        try {
            const choice: TaskQuickPickItem | undefined = await new Promise((resolve) => {
                // Add event handlers to the quick picker
                disposables.push(
                    qp.onDidChangeSelection((items) => {
                        // When the User selects an option, resolve the Promise
                        // and hide the quick picker
                        const item = items[0];
                        if (item) {
                            resolve(item);
                            qp.hide();
                        }
                    }),
                    qp.onDidHide(() => {
                        resolve(undefined);
                    }),
                    qp.onDidTriggerItemButton(async (e) => {
                        // When the User selects the secondary button, find or
                        // create the task in the tasks.json file

                        // There's only one button, so let's assert that
                        assert(e.item.buttons && e.item.buttons[0]);
                        assert(e.button == e.item.buttons[0]);

                        const tasks: vscode.TaskDefinition[] =
                            vscode.workspace.getConfiguration('tasks').get('tasks') ?? [];

                        // Check if the task is already defined in tasks.json
                        if (!tasks.find((t) => t?.label == getConventionalTaskLabel(e.item.task))) {
                            // If the task doesn't exist, create it

                            // Copy the definition and add a label
                            const def: SimpleTaskDef = {
                                ...(e.item.task.definition as SimpleTaskDef),
                                label: getConventionalTaskLabel(e.item.task),
                            };
                            tasks.push(def);
                            await vscode.workspace.getConfiguration().update('tasks.tasks', tasks);
                        }

                        // Then open tasks.json in an editor
                        if (vscode.workspace.workspaceFolders) {
                            const tasksUri = vscode.workspace.workspaceFolders
                                .map((ws) => vscode.Uri.joinPath(ws.uri, '.vscode', 'tasks.json'))
                                .find((v) => existsSync(v.fsPath));
                            if (tasksUri) {
                                await vscode.window.showTextDocument(tasksUri);
                            }
                        }
                        resolve(undefined);
                        qp.hide();
                    }),
                );

                // Show the quick picker
                qp.show();
            });

            if (choice) {
                // If a task was selected, mark it as the last executed task and
                // run it
                lastUsedTaskInfo = {
                    source: choice.task.source,
                    name: choice.task.name,
                };
                return await vscode.tasks.executeTask(choice.task);
            } else {
                return undefined;
            }
        } finally {
            disposables.forEach((d) => d.dispose());
        }
    } else {
        void vscode.window.showWarningMessage(
            `There are no Mains defined in the workspace project ${await getProjectFileRelPath()}`,
        );
        return undefined;
    }
}

//  Take active editor URI and call execute 'als-other-file' command in LSP
const otherFileHandler = () => {
    const activeEditor = vscode.window.activeTextEditor;
    if (!activeEditor) {
        return;
    }
    void adaExtState.adaClient.sendRequest(ExecuteCommandRequest.type, {
        command: 'als-other-file',
        arguments: [
            {
                uri: activeEditor.document.uri.toString(),
            },
        ],
    });
};

/**
 *
 * Check if we need to add some source directories to the workspace (e.g: when imported
 * projects' source directories are not placed under the root project's directory).
 * Do nothing if the user did not setup any workspace file.
 *
 * @param alsClient - the running ALS client
 * @param atStartup - whether or not the command is triggered when activating the extension
 * or explicitly by the user later via the Command Palette
 * @param displayYesNoPopup - whether or not we should display a yes/no popup
 * when missing directories
 */
export async function checkSrcDirectories(atStartup = false, displayYesNoPopup = true) {
    type ALSSourceDirDescription = {
        name: string;
        uri: string;
    };

    const foldersInSettings = vscode.workspace.getConfiguration().get('folders');
    const alsClient = adaExtState.adaClient;
    const doNotShowAgainKey = 'ada.addMissingDirsToWorkspace.doNotShowAgain';
    const doNotShowAgain = adaExtState.context.workspaceState.get(doNotShowAgainKey);

    //  Don't propose any popup if we multi-root workspace folders are already set
    //  explicitly in the workspace's settings, or if the command has been
    //  triggered at startup while the user previously clicked on the
    //  'Don't show again' button for this workspace
    if (foldersInSettings === undefined && !(atStartup && doNotShowAgain)) {
        const sourceDirs: ALSSourceDirDescription[] = (await alsClient.sendRequest(
            ExecuteCommandRequest.type,
            {
                command: 'als-source-dirs',
            },
        )) as ALSSourceDirDescription[];

        const isSubdirectory = (dir: string, parent: string) => {
            //  Use lower-case on Windows since drives can be specified in VS Code
            //  either with lower or upper case characters.
            if (process.platform == 'win32') {
                dir = dir.toLowerCase();
                parent = parent.toLowerCase();
            }

            return dir.startsWith(parent + '/');
        };

        const workspaceFolders = vscode.workspace.workspaceFolders ?? [];
        const workspaceDirsToAdd: { uri: vscode.Uri; name?: string | undefined }[] = [];

        for (const source_dir of sourceDirs) {
            const sourceDirURI = vscode.Uri.parse(source_dir.uri);
            const sourceDirPath = sourceDirURI.path;

            //  If the source directory is not under one of the workspace folders and
            //  if it's not already present in the workspace's folders, push
            //  this source directory to the workspace folders to add later.
            if (
                !workspaceFolders.some(
                    (workspaceFolder) =>
                        workspaceFolder.uri.path == sourceDirPath ||
                        isSubdirectory(sourceDirPath, workspaceFolder.uri.path),
                )
            ) {
                workspaceDirsToAdd.push({
                    name: source_dir.name,
                    uri: sourceDirURI,
                });
            }
        }

        //  If there are some source directories missing in the workspace, ask the user
        //  to add them in his workspace.
        if (workspaceDirsToAdd.length > 0) {
            let doAdd = true;

            if (displayYesNoPopup) {
                const buttons: ('Yes' | 'No' | "Don't Show Again")[] = ['Yes', 'No'];

                //  Show the 'Don't Show Again' button only at startup
                if (atStartup) {
                    buttons.push("Don't Show Again");
                }

                await vscode.window
                    .showInformationMessage(
                        'Some project source directories are not \
                    listed in your workspace: do you want to add them?',
                        ...buttons,
                    )
                    .then((answer) => {
                        if (answer !== 'Yes') {
                            doAdd = false;

                            if (answer === "Don't Show Again") {
                                void adaExtState.context.workspaceState.update(
                                    doNotShowAgainKey,
                                    true,
                                );
                            }
                        }
                    });
            }

            if (doAdd) {
                vscode.workspace.updateWorkspaceFolders(
                    vscode.workspace.workspaceFolders
                        ? vscode.workspace.workspaceFolders.length
                        : 0,
                    null,
                    ...workspaceDirsToAdd,
                );
            }
        } else if (!atStartup) {
            void vscode.window.showInformationMessage(
                "All the project's source directories are already \
                available in the current workspace.",
            );
        }
    }
}

/*
 * This is a command handler that builds and runs the main given as parameter.
 * If the given URI does not match one of the project Mains an error is
 * displayed.
 *
 * @param main - a URI of a document
 */
async function buildAndRunSpecifiedMain(main: vscode.Uri): Promise<void> {
    const adaMain = await findAdaMain(main.fsPath);
    if (adaMain) {
        const task = await findBuildAndRunTask(adaMain);
        if (task) {
            lastUsedTaskInfo = { source: task.source, name: task.name };
            await vscode.tasks.executeTask(task);
        } else {
            void vscode.window.showErrorMessage(
                `Could not find the 'Build and Run' task for the project main ` +
                    `${adaMain.mainRelPath()}`,
                { modal: true },
            );
        }
    } else {
        void vscode.window.showErrorMessage(
            `The document ${vscode.workspace.asRelativePath(main)} does not match ` +
                `any of the Mains of the project ${await getProjectFileRelPath()}`,
            { modal: true },
        );
    }
}

/**
 * This is a command handler that builds the main given as parameter and starts
 * a debug session on that main.  If the given URI does not match one of the
 * project Mains an error is displayed.
 *
 * @param main - a URI of a document
 */
async function buildAndDebugSpecifiedMain(main: vscode.Uri): Promise<void> {
    function isMatchingConfig(cfg: vscode.DebugConfiguration, configToMatch: AdaConfig): boolean {
        return cfg.type == configToMatch.type && cfg.name == configToMatch.name;
    }

    const wsFolder = vscode.workspace.getWorkspaceFolder(main);
    const adaMain = await findAdaMain(main.fsPath);
    const target = await adaExtState.getTargetPrefix();
    if (adaMain) {
        /**
         * The vscode API doesn't provide a way to list both automatically
         * provided and User-defined debug configurations. So instead, we
         * inspect the launch.json data if it exists, and the dynamic configs
         * provided by the extension. We look for a debug config that matches
         * the given main URI.
         */
        // Create a launch config for this main to help with matching
        const configToMatch = initializeConfig(adaMain, target);
        logger.debug('Debug config to match:\n' + JSON.stringify(configToMatch, null, 2));

        let matchingConfig = undefined;

        {
            // Find matching config in the list of workspace-defined launch configs
            const configs: vscode.DebugConfiguration[] =
                vscode.workspace.getConfiguration('launch').get('configurations') ?? [];
            logger.debug(`Workspace debug configurations:\n${JSON.stringify(configs, null, 2)}`);
            matchingConfig = configs.find((cfg) => isMatchingConfig(cfg, configToMatch));
        }

        if (!matchingConfig) {
            logger.debug('Could not find matching config in workspace configs.');
            // Look for a matching config among the configs dynamically provided by the extension
            const dynamicConfigs =
                await adaExtState.dynamicDebugConfigProvider.provideDebugConfigurations();
            logger.debug(
                `Dynamic debug configurations:\n${JSON.stringify(dynamicConfigs, null, 2)}`,
            );
            matchingConfig = dynamicConfigs.find((cfg) => isMatchingConfig(cfg, configToMatch));
        }

        if (matchingConfig) {
            logger.debug('Found matching config: ' + JSON.stringify(matchingConfig, null, 2));
            const success = await vscode.debug.startDebugging(wsFolder, matchingConfig);
            if (!success) {
                void vscode.window.showErrorMessage(
                    `Failed to start debug configuration: ${matchingConfig.name}`,
                );
            }
        } else {
            logger.error('Could not find matching config');
            void vscode.window.showErrorMessage(
                `Could not find a debug configuration for the project main ` +
                    `${adaMain.mainRelPath()}`,
                { modal: true },
            );
        }
    } else {
        void vscode.window.showErrorMessage(
            `The document ${vscode.workspace.asRelativePath(main)} does not match ` +
                `any of the Mains of the project ${await getProjectFileRelPath()}`,
            { modal: true },
        );
    }
}

/**
 * @returns an array of -P and -X project and scenario command lines arguments
 * for use with GPR-based tools.
 */
export async function gprProjectArgs(): Promise<string[]> {
    const scenarioArgs = gprScenarioArgs();

    return ['-P', await getProjectFromConfigOrALS()].concat(scenarioArgs);
}

export const PROJECT_FROM_CONFIG = '${config:ada.projectFile}';

/**
 * @returns an array of -X scenario command lines arguments for use with
 * GPR-based tools.
 */
export function gprScenarioArgs() {
    const vars: string[][] = Object.entries(
        vscode.workspace.getConfiguration('ada').get('scenarioVariables') ?? [],
    );
    const scenarioArgs = vars.map(([key, value]) => `-X${key}=${value}`);
    return scenarioArgs;
}

/**
 * @returns `"$\{config:ada.projectFile\}"` if that setting has a value, or else
 * queries the ALS for the current project and returns the full path.
 */
export async function getProjectFromConfigOrALS(): Promise<string> {
    /**
     * If ada.projectFile is set, use the $\{config:ada.projectFile\} macro
     */
    return vscode.workspace.getConfiguration().get('ada.projectFile')
        ? PROJECT_FROM_CONFIG
        : await adaExtState.getProjectFile();
}

/**
 * @returns the gnatprove `--limit-subp=file:line` argument associated to the
 * subprogram enclosing the the current editor's cursor position. If a enclosing
 * subprogram is not found at the current location, we use the \$\{lineNumber\}
 * predefined variable as a fallback to avoid raising an Error which would
 * prevent the task provider from offering the task.
 */
export async function sparkLimitSubpArg(): Promise<string[]> {
    return getEnclosingSymbol(vscode.window.activeTextEditor, [vscode.SymbolKind.Function]).then(
        (Symbol) => {
            if (Symbol) {
                const range = Symbol.range;
                return [getLimitSubpArg('${fileBasename}', range)];
            } else {
                /**
                 * If we can't find a subprogram, we use the VS Code predefined
                 * variable lineNumber to avoid raising an Error. This function
                 * is called through the corresponding command during task
                 * resolution in the task provider.  Raising an error would
                 * prevent the task from appear in the list of provided tasks.
                 * For this reason we use the acceptable fallback of lineNumber
                 * and rely on SPARK tooling to provide an explanatory message.
                 */
                return [`--limit-subp=\${fileBasename}:\${lineNumber}`];
            }
        },
    );
}

/**
 *
 * @param filename - a filename
 * @param range - a {@link vscode.Range}
 * @returns the --limit-subp `gnatprove` CLI argument corresponding to the given
 * arguments. Note that lines and columns in {@link vscode.Range}es are
 * zero-based while the `gnatprove` convention is one-based. This function does
 * the conversion.
 */
function getLimitSubpArg(filename: string, range: vscode.Range) {
    return `--limit-subp=${filename}:${range.start.line + 1}`;
}

/**
 * @returns the gnatprove `--limit-region=file:from:to` argument corresponding
 * to the current editor's selection.
 */
export const sparkLimitRegionArg = (): Promise<string[]> => {
    return Promise.resolve([
        `--limit-region=\${fileBasename}:${getSelectedRegion(vscode.window.activeTextEditor)}`,
    ]);
};

/**
 *
 * @param editor - the editor to get the selection from
 * @returns a `<start-line>:<end-line>` string representation of the editor's
 * current selection where lines are indexed starting 1. If the given editor is
 * undefined, returns '0:0'.
 */
export const getSelectedRegion = (editor: vscode.TextEditor | undefined): string => {
    if (editor) {
        const selection = editor.selection;
        //  Line numbers start at 0 in VS Code, and at 1 in GNAT
        return `${selection.start.line + 1}:${selection.end.line + 1}`;
    } else {
        return '0:0';
    }
};

/**
 * Return the closest DocumentSymbol of the given kinds enclosing the
 * the given editor's cursor position, if any.
 *
 * If the given editor is undefined or not on an Ada file, the function returns
 * `null` immediately.
 *
 * @param editor - The editor in which we want
 * to find the closest symbol enclosing the cursor's position.
 * @returns Return the closest enclosing symbol.
 */

export async function getEnclosingSymbol(
    editor: vscode.TextEditor | undefined,
    kinds: vscode.SymbolKind[],
): Promise<vscode.DocumentSymbol | null> {
    if (editor && editor.document.languageId == 'ada') {
        const line = editor.selection.active.line;

        // First get all symbols for current file
        const symbols: vscode.DocumentSymbol[] = await vscode.commands.executeCommand(
            'vscode.executeDocumentSymbolProvider',
            editor.document.uri,
        );

        // Then filter them according to the specified kinds
        const filtered_symbols = getSymbols(symbols, kinds, [
            SymbolKind.Function,
            SymbolKind.Module,
        ]);

        // Finally select from the filtered symbols the smallest one containing the current line
        const scopeSymbols = filtered_symbols.filter(
            (sym) => line >= sym.range.start.line && line <= sym.range.end.line,
        );

        if (scopeSymbols.length > 0) {
            scopeSymbols.sort(
                (a, b) =>
                    a.range.end.line - a.range.start.line - (b.range.end.line - b.range.start.line),
            );

            return scopeSymbols[0];
        }
    }

    return null;
}

/**
 * Command corresponding to the 'Prove' CodeLens provided on subprograms.
 *
 * It is implemented by fetching the 'Prove subbprogram' task and using it as a
 * template such that the User can customize the task to impact the CodeLens.
 *
 * Another option could have been to use the command
 * `workbench.action.tasks.runTask` with the name of the SPARK task as argument
 * however that would run the task using the current cursor location which may
 * not match the CodeLens that was triggered. So it is better to create a task
 * dedicated to the location of the CodeLens.
 */
async function sparkProveSubprogram(
    uri: vscode.Uri,
    range: vscode.Range,
): Promise<vscode.TaskExecution> {
    /**
     * Get the 'Prove subprogram' task. Prioritize workspace tasks so that User
     * customization of the task takes precedence.
     */
    const task = (await vscode.tasks.fetchTasks({ type: TASK_TYPE_SPARK }))
        .sort(workspaceTasksFirst)
        .find(
            (t) =>
                getConventionalTaskLabel(t) == `${TASK_TYPE_SPARK}: ${TASK_PROVE_SUPB_PLAIN_NAME}`,
        );
    assert(task);

    /**
     * Create a copy of the task.
     */
    const newTask = new vscode.Task(
        { ...task.definition },
        task.scope ?? vscode.TaskScope.Workspace,
        task.name,
        task.source,
        undefined,
        /**
         * In some cases the task returned by the API has an empty list of
         * problem matchers despite being configured with problem matchers in
         * tasks.json. In that case use the default problem matcher to avoid an
         * empty problem matcher list which would hide problems from the
         * Problems view.
         */
        task.problemMatchers.length > 0 ? task.problemMatchers : [DEFAULT_PROBLEM_MATCHER],
    );

    /**
     * Replace the subp-region argument based on the parameter given to the
     * command.
     */
    const taskDef = newTask.definition as SimpleTaskDef;
    assert(taskDef.args);
    const regionArg = '${command:ada.spark.limitSubpArg}';
    const regionArgIdx = taskDef.args.findIndex((arg) => arg == regionArg);
    if (regionArgIdx >= 0) {
        const fileBasename = basename(uri.fsPath);
        taskDef.args[regionArgIdx] = getLimitSubpArg(fileBasename, range);
        /**
         * Change the task name accordingly, otherwise all invocations appear
         * with the same name in the task history.
         */
        newTask.name = `${task.name} - ${fileBasename}:${range.start.line + 1}`;
    } else {
        throw Error(
            `Task '${getConventionalTaskLabel(task)}' is missing a '${regionArg}' argument`,
        );
    }

    /**
     * Resolve the task.
     */
    const resolvedTask = await adaExtState.getSparkTaskProvider()?.resolveTask(newTask);
    assert(resolvedTask);

    /**
     * Execute the task.
     */
    return await vscode.tasks.executeTask(resolvedTask);
}
