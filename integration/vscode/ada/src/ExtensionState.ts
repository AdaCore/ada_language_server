import * as vscode from 'vscode';
import {
    Disposable,
    ExecuteCommandParams,
    ExecuteCommandRequest,
    LanguageClient,
} from 'vscode-languageclient/node';
import { AdaCodeLensProvider } from './AdaCodeLensProvider';
import { createClient } from './clients';
import { AdaInitialDebugConfigProvider, initializeDebugging } from './debugConfigProvider';
import { logger } from './extension';
import { existsSync } from 'fs';
import path from 'path';
import { GnatTaskProvider } from './gnatTaskProvider';
import { initializeTesting } from './gnattest';
import { GprTaskProvider } from './gprTaskProvider';
import { getArgValue, TERMINAL_ENV_SETTING_NAME, exe, getEvaluatedTerminalEnv } from './helpers';
import {
    SimpleTaskDef,
    SimpleTaskProvider,
    TASK_TYPE_ADA,
    TASK_TYPE_SPARK,
    createAdaTaskProvider,
    createSparkTaskProvider,
} from './taskProviders';
import { isAbsolute } from 'path';

/**
 * This class encapsulates all state that should be maintained throughout the
 * lifecyle of the extension. This includes e.g. the Ada and GPR LSP clients,
 * task providers, etc...
 *
 * The intent is for there to be a global singleton instance of this class
 * created during the activation of the extension and referenced subsequently
 * wherever needed.
 */
export class ExtensionState {
    public readonly adaClient: LanguageClient;
    public readonly gprClient: LanguageClient;
    public readonly context: vscode.ExtensionContext;
    public readonly dynamicDebugConfigProvider: {
        provideDebugConfigurations(
            _folder?: vscode.WorkspaceFolder | undefined,
        ): Promise<vscode.DebugConfiguration[]>;
    };
    public readonly initialDebugConfigProvider: AdaInitialDebugConfigProvider;

    private taskDisposables: Disposable[];

    public readonly codelensProvider = new AdaCodeLensProvider();
    public readonly testController: vscode.TestController;
    public readonly testData: Map<vscode.TestItem, object> = new Map();

    /**
     * The following fields are caches for ALS requests or costly properties.
     */
    cachedProjectUri: vscode.Uri | undefined;
    cachedObjectDir: string | undefined;
    cachedTargetPrefix: string | undefined;
    cachedMains: string[] | undefined;
    cachedExecutables: string[] | undefined;
    cachedAlireTomls: vscode.Uri[] | undefined;
    cachedGdb: string | undefined | null = undefined;

    private adaTaskProvider?: SimpleTaskProvider;
    private sparkTaskProvider?: SimpleTaskProvider;

    private clearALSCache() {
        this.cachedProjectUri = undefined;
        this.cachedObjectDir = undefined;
        this.cachedTargetPrefix = undefined;
        this.cachedMains = undefined;
        this.cachedExecutables = undefined;
        this.cachedAlireTomls = undefined;
        this.cachedGdb = undefined;
    }

    constructor(context: vscode.ExtensionContext) {
        this.context = context;
        this.gprClient = createClient(
            context,
            'gpr',
            'GPR Language Server',
            ['--language-gpr'],
            '**/.{gpr}',
        );
        this.adaClient = createClient(
            context,
            'ada',
            'Ada Language Server',
            [],
            '**/.{adb,ads,adc,ada}',
        );
        this.taskDisposables = [];
        const result = initializeDebugging(this.context);
        this.initialDebugConfigProvider = result.providerInitial;
        this.dynamicDebugConfigProvider = result.providerDynamic;
        this.testController = initializeTesting(context);
    }

    public start = async () => {
        await Promise.all([this.gprClient.start(), this.adaClient.start()]);
        this.registerTaskDisposables();
        this.context.subscriptions.push(
            vscode.languages.registerCodeLensProvider('ada', this.codelensProvider),
        );
    };

    public dispose = () => {
        this.unregisterTaskDisposables();
    };

    /**
     * Register all the task disposables needed by the extension (e.g: task providers,
     * listeners...).
     */
    public registerTaskDisposables = (): void => {
        this.adaTaskProvider = createAdaTaskProvider();
        this.sparkTaskProvider = createSparkTaskProvider();

        this.taskDisposables = [
            vscode.tasks.registerTaskProvider(GnatTaskProvider.gnatType, new GnatTaskProvider()),
            vscode.tasks.registerTaskProvider(
                GprTaskProvider.gprTaskType,
                new GprTaskProvider(this.adaClient),
            ),
            vscode.tasks.registerTaskProvider(TASK_TYPE_ADA, this.adaTaskProvider),
            vscode.tasks.registerTaskProvider(TASK_TYPE_SPARK, this.sparkTaskProvider),

            //  Add a listener on tasks to open the SARIF Viewer when the
            //  task that ends outputs a SARIF file.
            vscode.tasks.onDidEndTaskProcess(async (e) => {
                const task = e.execution.task;
                await openSARIFViewerIfNeeded(task);
            }),
        ];
    };

    /**
     * Unregister all the task disposables needed by the extension (e.g: task providers,
     * listeners...).
     */
    public unregisterTaskDisposables = (): void => {
        for (const item of this.taskDisposables) {
            item.dispose();
        }
        this.taskDisposables = [];
    };

    /**
     * Show a popup asking the user to reload the VS Code window after
     * changes made in the VS Code environment settings
     * (e.g: terminal.integrated.env.linux).
     */
    public showReloadWindowPopup = async () => {
        const selection = await vscode.window.showWarningMessage(
            `The workspace environment has changed: the VS Code window needs
            to be reloaded in order for the Ada Language Server to take the
            new environment into account.
            Do you want to reload the VS Code window?`,
            'Reload Window',
        );

        // Reload the VS Code window if the user selected 'Yes'
        if (selection == 'Reload Window') {
            void vscode.commands.executeCommand('workbench.action.reloadWindow');
        }
    };

    //  React to changes in configuration to recompute predefined tasks if the user
    //  changes scenario variables' values.
    public configChanged = (e: vscode.ConfigurationChangeEvent) => {
        logger.info('didChangeConfiguration event received');

        if (
            e.affectsConfiguration('ada.scenarioVariables') ||
            e.affectsConfiguration('ada.projectFile')
        ) {
            this.clearCacheAndTasks(
                'project related settings have changed: clearing caches and tasks',
            );
        }

        //  React to changes made in the environment variables, showing
        //  a popup to reload the VS Code window and thus restart the
        //  Ada extension.
        if (e.affectsConfiguration(TERMINAL_ENV_SETTING_NAME)) {
            const new_value = vscode.workspace.getConfiguration().get(TERMINAL_ENV_SETTING_NAME);
            logger.info(`${TERMINAL_ENV_SETTING_NAME} has changed: show reload popup`);
            logger.info(`${TERMINAL_ENV_SETTING_NAME}: ${JSON.stringify(new_value, undefined, 2)}`);

            void this.showReloadWindowPopup();
        }
    };

    /**
     * Clear the extension's cache and, unregister its predefined tasks and register them
     * again to make sure they take into account the current extension's state
     * (environment, loaded project...).
     *
     * @param msg - Log message explaning why the cache is being cleared.
     */
    public clearCacheAndTasks(logMsg = ''): void {
        logger.info(logMsg);
        this.clearALSCache();
        this.unregisterTaskDisposables();
        this.registerTaskDisposables();
    }

    /**
     * Returns the value of the given project attribute, or raises
     * an exception when the queried attribute is not known
     * (i.e: not registered in GPR2's knowledge database).
     *
     * @param attribute - The name of the project attribute (e.g: 'Target')
     * @param pkg - The name of the attribute's package (e.g: 'Compiler').
     * Can be empty for project-level attributes.
     * @param index - Attribute's index, if any. Can be a file or a language
     * (e.g: 'for Runtime ("Ada") use...').
     * @returns the value of the queried project attribute. Can be either a string or a
     * list of strings, depending on the attribute itself (e.g: 'Main' attribute is
     * specified as a list of strings while 'Target' as a string)
     */
    public async getProjectAttributeValue(
        attribute: string,
        pkg: string = '',
        index: string = '',
    ): Promise<string | string[]> {
        const params: ExecuteCommandParams = {
            command: 'als-get-project-attribute-value',
            arguments: [
                {
                    attribute: attribute,
                    pkg: pkg,
                    index: index,
                },
            ],
        };

        const attrValue = (await this.adaClient.sendRequest(ExecuteCommandRequest.type, params)) as
            | string
            | string[];
        return attrValue;
    }

    /**
     *
     * @returns the full path to the `gdb` executable, taking into consideration the
     * `PATH` variable in the `terminal.integrated.env.*` setting if set. Otherwise,
     * the `PATH` variable of the current process environment is considered. When
     * a non-native target is specified, it will be prepeneded to the `gdb` executable
     * before looking in the `PATH` (e.g: `arm-eabi-gdb` for `arm-eabi` target).
     *
     * The current process environment is unlikely to change during the lifetime of
     * the extension, and we already prompt the User to reload the window in case
     * the `terminal.integrated.env.*` variables change. For this reason, we compute
     * the value only on the first call, and cache it for subsequent calls to return
     * it efficiently.
     *
     * @param target - The target for which gdb should be looked for.
     */
    public getOrFindGdb(target: string = ''): string | undefined {
        if (this.cachedGdb === undefined) {
            /**
             * If undefined yet, try to compute it.
             */
            const env = getEvaluatedTerminalEnv();
            let pathVal: string;
            if (env && 'PATH' in env) {
                pathVal = env.PATH ?? '';
            } else if ('PATH' in process.env) {
                pathVal = process.env.PATH ?? '';
            } else {
                pathVal = '';
            }

            const gdbExeBasename = target != '' ? `${target}-gdb${exe}` : `gdb${exe}`;
            const gdb = pathVal
                .split(path.delimiter)
                .map<string>((v) => path.join(v, gdbExeBasename))
                .find(existsSync);

            if (gdb) {
                // Found
                this.cachedGdb = gdb;
                return this.cachedGdb;
            } else {
                // Not found. Assign null to cache to avoid recomputing at every call.
                this.cachedGdb = null;
            }
        }

        // When returning, coerce null to undefined because the distinction doesn't
        // matter on the caller side.
        return this.cachedGdb ?? undefined;
    }

    /**
     * @returns the URI of the main project file from the ALS
     */
    public async getProjectUri(): Promise<vscode.Uri | undefined> {
        if (!this.cachedProjectUri) {
            const strUri = (await this.adaClient.sendRequest(ExecuteCommandRequest.type, {
                command: 'als-project-file',
            })) as string;
            if (strUri != '') {
                this.cachedProjectUri = vscode.Uri.parse(strUri, true);
            }
        }

        return this.cachedProjectUri;
    }

    /**
     * Returns the target prefix that should be used when spawning tools
     * like gnat, gcc or gdb.
     * For instance if the project has an 'arm-eabi' target, this
     * function will return 'arm-eabi'. For native projects, this
     * will return an empty string.
     *
     * @returns the target prefix
     */
    public async getTargetPrefix(): Promise<string> {
        if (!this.cachedTargetPrefix) {
            // Get the compiler driver's path from the Compiler.Driver project
            // attribute, and delete the last bit to get the prefix
            const driverPath = (await this.getProjectAttributeValue(
                'driver',
                'compiler',
                'ada',
            )) as string;
            const driver = path.basename(driverPath);
            this.cachedTargetPrefix = driver.substring(0, driver.lastIndexOf('-'));
        }

        return this.cachedTargetPrefix;
    }

    /**
     * @returns the full path of the main project file from the ALS
     */
    public async getProjectFile(): Promise<string> {
        return (await this.getProjectUri())?.fsPath ?? '';
    }

    /**
     *
     * @returns the full path of the project object directory obtained from the ALS
     */
    public async getObjectDir(): Promise<string> {
        if (!this.cachedObjectDir) {
            this.cachedObjectDir = (await this.adaClient.sendRequest(ExecuteCommandRequest.type, {
                command: 'als-object-dir',
            })) as string;
        }

        return this.cachedObjectDir;
    }

    /**
     *
     * @returns the list of full paths of main sources defined in the project from the ALS
     */
    public async getMains(): Promise<string[]> {
        if (!this.cachedMains) {
            this.cachedMains = (await this.adaClient.sendRequest(ExecuteCommandRequest.type, {
                command: 'als-mains',
            })) as string[];
        }

        return this.cachedMains;
    }

    /**
     *
     * @returns the list of full paths of executables corresponding to main
     * sources defined in the project from the ALS
     */
    public async getExecutables(): Promise<string[]> {
        if (!this.cachedExecutables) {
            this.cachedExecutables = (await this.adaClient.sendRequest(ExecuteCommandRequest.type, {
                command: 'als-executables',
            })) as string[];
        }

        return this.cachedExecutables;
    }

    public async getAlireTomls(): Promise<vscode.Uri[]> {
        if (!this.cachedAlireTomls) {
            this.cachedAlireTomls = await vscode.workspace.findFiles('alire.toml');
        }

        return this.cachedAlireTomls;
    }

    /**
     *
     * @returns the SPARK task provider which can be useful for resolving tasks
     * created on the fly, e.g. when running SPARK CodeLenses.
     */
    public getSparkTaskProvider() {
        return this.sparkTaskProvider;
    }
}

/**
 *
 * Open the SARIF Viewer if the given task outputs its results in
 * a SARIF file (e.g: GNAT SAS Report task).
 */
async function openSARIFViewerIfNeeded(task: vscode.Task) {
    const definition: SimpleTaskDef = task.definition;

    if (definition) {
        const args = definition.args;

        if (args?.some((arg) => getArgValue(arg).includes('sarif'))) {
            const execution = task.execution;
            let cwd = undefined;

            if (execution && execution instanceof vscode.ShellExecution) {
                cwd = execution.options?.cwd;
            }

            if (!cwd && vscode.workspace.workspaceFolders) {
                cwd = vscode.workspace.workspaceFolders[0].uri.fsPath;
            }

            const sarifExt = vscode.extensions.getExtension('ms-sarifvscode.sarif-viewer');

            // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment
            const sarifExtAPI = sarifExt ? await sarifExt.activate() : undefined;

            if (cwd && sarifExtAPI) {
                const cwdURI = vscode.Uri.file(cwd);
                const outputFilePathArgRaw = args.find((arg) =>
                    getArgValue(arg).includes('.sarif'),
                );

                if (outputFilePathArgRaw) {
                    // Convert the raw argument into a string
                    const outputFilePathArg = getArgValue(outputFilePathArgRaw);

                    // The SARIF output file can be specified as '--output=path/to/file.sarif':
                    // split the argument on '=' if that's the case, to retrieve only the file path
                    const outputFilePath = outputFilePathArg.includes('=')
                        ? outputFilePathArg.split('=').pop()
                        : outputFilePathArg;

                    if (outputFilePath) {
                        const sarifFileURI = isAbsolute(outputFilePath)
                            ? vscode.Uri.file(outputFilePath)
                            : vscode.Uri.joinPath(cwdURI, outputFilePath);

                        // eslint-disable-next-line max-len
                        // eslint-disable-next-line @typescript-eslint/no-unsafe-call, @typescript-eslint/no-unsafe-member-access
                        await sarifExtAPI.openLogs([sarifFileURI]);
                    }
                }
            }
        }
    }
}
