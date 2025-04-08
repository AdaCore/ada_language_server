import { existsSync } from 'fs';
import path, { isAbsolute } from 'path';
import * as vscode from 'vscode';
import {
    Disposable,
    ExecuteCommandParams,
    ExecuteCommandRequest,
} from 'vscode-languageclient/node';
import { AdaCodeLensProvider } from './AdaCodeLensProvider';
import { AdaLanguageClient, createClient } from './clients';
import {
    CMD_RELOAD_PROJECT,
    CMD_RESTART_LANG_SERVERS,
    CMD_SHOW_ADA_LS_OUTPUT,
    CMD_SHOW_EXTENSION_LOGS,
    CMD_SHOW_GPR_LS_OUTPUT,
} from './commands';
import { AdaInitialDebugConfigProvider, initializeDebugging } from './debugConfigProvider';
import { logger } from './extension';
import { GnatTaskProvider } from './gnatTaskProvider';
import { initializeTesting } from './gnattest';
import { GprTaskProvider } from './gprTaskProvider';
import { TERMINAL_ENV_SETTING_NAME, exe, getArgValue, getEvaluatedTerminalEnv } from './helpers';
import {
    SimpleTaskDef,
    SimpleTaskProvider,
    TASK_TYPE_ADA,
    TASK_TYPE_SPARK,
    createAdaTaskProvider,
    createSparkTaskProvider,
} from './taskProviders';

/**
 * Return type of the 'als-source-dirs' LSP request.
 */
export type ALSSourceDirDescription = {
    name: string;
    uri: string;
};

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
    public readonly adaClient: AdaLanguageClient;
    public readonly gprClient: AdaLanguageClient;
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
    public readonly statusBar: vscode.StatusBarItem;

    /**
     * The following fields are caches for ALS requests or costly properties.
     */
    cachedProjectUri: vscode.Uri | undefined;
    cachedObjectDir: string | undefined;
    cachedSourceDirs: ALSSourceDirDescription[] | undefined;
    cachedTargetPrefix: string | undefined;
    cachedMains: string[] | undefined;
    cachedExecutables: string[] | undefined;
    cachedAlireTomls: vscode.Uri[] | undefined;
    cachedDebugServerAddress: string | undefined | null;
    cachedGdb: string | undefined | null = undefined;
    projectAttributeCache: Map<string, Promise<string | string[]>> = new Map();

    private adaTaskProvider?: SimpleTaskProvider;
    private sparkTaskProvider?: SimpleTaskProvider;

    private clearALSCache() {
        this.cachedProjectUri = undefined;
        this.cachedObjectDir = undefined;
        this.cachedSourceDirs = undefined;
        this.cachedTargetPrefix = undefined;
        this.cachedMains = undefined;
        this.cachedExecutables = undefined;
        this.cachedAlireTomls = undefined;
        this.cachedDebugServerAddress = undefined;
        this.cachedGdb = undefined;
        this.projectAttributeCache.clear();
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
        this.statusBar = vscode.window.createStatusBarItem(vscode.StatusBarAlignment.Left);
        this.context.subscriptions.push(this.statusBar);
    }

    public start = async () => {
        await Promise.all([this.gprClient.start(), this.adaClient.start()]);
        this.registerTaskDisposables();
        this.context.subscriptions.push(
            vscode.languages.registerCodeLensProvider('ada', this.codelensProvider),
        );
        this.updateStatusBarVisibility(undefined);
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
     * Update the status bar item's visibility according to the current context.
     */
    public updateStatusBarVisibility = (editor: vscode.TextEditor | undefined) => {
        const activeEditor = editor ?? vscode.window.activeTextEditor;

        // Show the status bar if the active editor is on an Ada or a GPR source,
        // a JSON configuration file that might have an impact on the running ALS
        // instance (config.json, als.json, settings.json...) or
        // if it's the Output view that is focused (i.e: when the active editor's
        // document scheme is set to 'output') and showing Ada & SPARK extension's logs.
        if (
            activeEditor &&
            (['ada', 'gpr'].includes(activeEditor.document.languageId) ||
                activeEditor.document.fileName.endsWith('config.json') ||
                activeEditor.document.fileName.endsWith('als.json') ||
                activeEditor.document.fileName.endsWith('settings.json') ||
                (activeEditor.document.uri.scheme == 'output' &&
                    activeEditor.document.fileName.includes('AdaCore')))
        ) {
            this.statusBar.show();
        } else {
            this.statusBar.hide();
        }
    };

    /**
     * Update the status bar item's content according to currently displayed
     * diagnostics.
     */
    public updateStatusBarItem = () => {
        // Use markdown for the status bar item tiooltip. This allows to have
        // hyperlinks that run actual commands.
        this.statusBar.tooltip = new vscode.MarkdownString('', true);
        this.statusBar.tooltip.isTrusted = true;

        // Show the Problems view by default when clicking on the status
        // bar item.
        this.statusBar.command = 'workbench.panel.markers.view.focus';
        let alireProjectLoaded = false;

        // Monitor diagnostics related to project-loading in general, including
        // potential issues reported by Alire.
        const PROJECT_DIAGS_SOURCE = 'ada.project';
        const ALIRE_DIAGS_SOURCE = 'ada.alire';
        const diagnosticSources = [PROJECT_DIAGS_SOURCE, ALIRE_DIAGS_SOURCE];

        // Gather all the diagnostics from the interesting ALS diagnostics' sources.
        // For the status bar we are interested only in project-related diagnostics.
        const alsDiagnostics: vscode.Diagnostic[] = vscode.languages
            .getDiagnostics()
            .flatMap(([, diagnostics]) => diagnostics)
            .filter((diag) => diagnosticSources.includes(diag.source ?? ''));

        // Update the status bar according to the ALS project-related diagnostics
        if (alsDiagnostics.length > 0) {
            // Get the highest severity of the currently displayed project-diagnostics, to
            // update the status bar's colors accordingly.
            const statusBarSeverity: vscode.DiagnosticSeverity = alsDiagnostics
                .map((a) => a.severity)
                .reduce((a, b) => (a < b ? a : b));
            this.statusBar.text = 'Ada & SPARK';

            switch (statusBarSeverity) {
                case vscode.DiagnosticSeverity.Error:
                    this.statusBar.tooltip.appendMarkdown(
                        'Project loading has issued errors, see the [Problems]' +
                            '(command:workbench.panel.markers.view.focus) view' +
                            ' for more information.',
                    );
                    this.statusBar.backgroundColor = new vscode.ThemeColor(
                        'statusBarItem.errorBackground',
                    );
                    this.statusBar.color = new vscode.ThemeColor('statusBarItem.errorForeground');
                    break;

                case vscode.DiagnosticSeverity.Warning:
                    this.statusBar.tooltip.appendMarkdown(
                        'Project loading has issued warnings, see the [Problems]' +
                            '(command:workbench.panel.markers.view.focus) view' +
                            ' for more information.',
                    );
                    this.statusBar.backgroundColor = new vscode.ThemeColor(
                        'statusBarItem.warningBackground',
                    );
                    this.statusBar.color = new vscode.ThemeColor('statusBarItem.warningForeground');
                    break;

                default:
                    this.statusBar.backgroundColor = undefined;
                    this.statusBar.color = undefined;

                    // Check if we have successfully loaded the project through Alire
                    // and adapt the status bar item's text contents if it's the case, to
                    // mention that Alire was used for project-loading.
                    alireProjectLoaded = alsDiagnostics.some(
                        (diagnostic) =>
                            diagnostic.source == PROJECT_DIAGS_SOURCE &&
                            diagnostic.message.includes('Alire'),
                    );

                    if (alireProjectLoaded) {
                        this.statusBar.text += ' (Alire)';
                        this.statusBar.tooltip.appendMarkdown(
                            'Project was loaded successfully through Alire',
                        );
                    } else {
                        this.statusBar.tooltip.appendMarkdown('Project was loaded successfully.');
                    }
            }
        } else {
            // We don't have any project-related diagnostics, just clear any color/background color
            // of the status bar.
            this.statusBar.tooltip.appendMarkdown('Project was loaded successfully.');
            this.statusBar.backgroundColor = undefined;
            this.statusBar.color = undefined;
        }

        if (this.statusBar.tooltip.value) {
            this.statusBar.tooltip.appendMarkdown('\n\n---\n\n');
        }
        this.statusBar.tooltip.appendMarkdown(
            `[$(terminal) Open Extension Logs](command:${CMD_SHOW_EXTENSION_LOGS}
 "Show Ada Extension Output")

[$(terminal) Open Logs for Ada & SPARK](command:${CMD_SHOW_ADA_LS_OUTPUT}
 "Show Ada Language Server for Ada & SPARK Output")

[$(terminal) Open Logs for GPR](command:${CMD_SHOW_GPR_LS_OUTPUT}
 "Show Ada Language Server for GPR Output")

[$(refresh) Reload Project](command:${CMD_RELOAD_PROJECT} "Reload Project")

[$(debug-restart) Restart Language Servers](command:${CMD_RESTART_LANG_SERVERS}
 "Restart Ada Language Servers")`,
        );
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
     * Query results are cached by default so that querying the same attribute
     * multiple times will result in only one request to the server.
     *
     * @param attribute - The name of the project attribute (e.g: 'Target')
     * @param pkg - The name of the attribute's package (e.g: 'Compiler').
     * Can be empty for project-level attributes.
     * @param index - Attribute's index, if any. Can be a file or a language
     * (e.g: 'for Runtime ("Ada") use...').
     * @param useCache - whether to use cached result from previous query. If
     * set to false, the request will be sent to the server and the result will
     * be used to update the cache. This allows refreshing a particular project
     * attribute in the cache.
     * @returns the value of the queried project attribute. Can be either a string or a
     * list of strings, depending on the attribute itself (e.g: 'Main' attribute is
     * specified as a list of strings while 'Target' as a string)
     */
    public getProjectAttributeValue(
        attribute: string,
        pkg: string = '',
        index: string = '',
        useCache = true,
    ): Promise<string | string[]> {
        const queryArgs = {
            attribute: attribute,
            pkg: pkg,
            index: index,
        };

        /**
         * In the JS Map class, keys are compared using something equivalent to
         * ===. So two distinct objects with the same deep content will
         * constitute different keys. A common way of using objects as Map keys
         * is to stringify them.
         */
        const mapKey = JSON.stringify(queryArgs);
        const cachedPromise = useCache ? this.projectAttributeCache.get(mapKey) : undefined;

        if (cachedPromise === undefined) {
            const params: ExecuteCommandParams = {
                command: 'als-get-project-attribute-value',
                arguments: [queryArgs],
            };

            const queryPromise = this.adaClient
                .sendRequest(ExecuteCommandRequest.type, params)
                .then((value) => {
                    /**
                     * Only cache the promise if it was fulfilled.
                     */
                    this.projectAttributeCache.set(mapKey, queryPromise);
                    return value as string | string[];
                });

            return queryPromise;
        } else {
            return cachedPromise;
        }
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
     * Returns true if the loaded project is a native project, false if
     * it's a cross/bare-metal one.
     *
     * @returns a boolean indicating if the loaded project is a native one.
     */
    public async isNativeProject(): Promise<boolean> {
        const targetPrefix = await this.getTargetPrefix();
        return targetPrefix == '' || targetPrefix === undefined;
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
        if (this.cachedTargetPrefix === undefined) {
            // Get the compiler driver's path from the Compiler.Driver project
            // attribute, and delete the last bit to get the prefix.
            // We get an exception when the attribute is not defined, which can
            // happen when there is no available toolchain for the project's target:
            // in that case, consider the project as a native one.
            try {
                const driverPath = (await this.getProjectAttributeValue(
                    'driver',
                    'compiler',
                    'ada',
                )) as string;
                logger.info(`Got Project.Compiler.Driver ("ada") = ${driverPath}`);
                const driver = path.basename(driverPath);
                this.cachedTargetPrefix = driver.substring(0, driver.lastIndexOf('-'));
                logger.info(`Computed target prefix: ${this.cachedTargetPrefix}`);
            } catch (err) {
                const errMessage =
                    err instanceof Error ? err.message : typeof err === 'string' ? err : '';
                logger.warn(`Failed to get Project.Compiler.Driver ("ada"): ${errMessage}`);
                logger.warn('Assuming empty target prefix');
                this.cachedTargetPrefix = '';
            }
        }

        return this.cachedTargetPrefix;
    }

    /**
     * Returns the debug server address that should be used when debugging executables
     * built for non-native projects.
     * This checks for the IDE'Program_Host project attribute if {@link forGNATemulator} is
     * set to false, and the Emulator'Debug_Port otherwise (GNATemulator project attribute).
     * @param forGNATemulator - Set it to true to retrieve the debug server address that should
     * be used by GNATemulator.
     * @returns the debug server address, as a string (e.g: 'localhost:1234')
     */
    public async getDebugServerAddress(forGNATemulator = false): Promise<string | null> {
        type AttributeID = {
            name: string;
            package: string;
        };

        if (this.cachedDebugServerAddress === undefined) {
            const attrID: AttributeID = {
                name: forGNATemulator ? 'Debug_Port' : 'Program_Host',
                package: forGNATemulator ? 'Emulator' : 'IDE',
            };
            try {
                const debugServerAddress = (await this.getProjectAttributeValue(
                    attrID.name,
                    attrID.package,
                )) as string;
                logger.info(`Got Project.${attrID.package}.${attrID.name} = ${debugServerAddress}`);
                this.cachedDebugServerAddress = forGNATemulator
                    ? `localhost:${debugServerAddress}`
                    : debugServerAddress;
                logger.info(`Computed debug server address: ${this.cachedDebugServerAddress}`);
            } catch (err) {
                const errMessage =
                    err instanceof Error ? err.message : typeof err === 'string' ? err : '';
                logger.warn(
                    `Failed to get Project.${attrID.package}.${attrID.name}: ${errMessage}`,
                );
                logger.warn('Assuming there is no debug server address');
                this.cachedDebugServerAddress = null;
            }
        }

        return this.cachedDebugServerAddress;
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
     * @returns the list of source directories defined in the project loaded by the ALS
     */
    public async getSourceDirs(): Promise<ALSSourceDirDescription[]> {
        if (this.cachedSourceDirs === undefined) {
            this.cachedSourceDirs = (await this.adaClient.sendRequest(ExecuteCommandRequest.type, {
                command: 'als-source-dirs',
            })) as ALSSourceDirDescription[];
        }

        return this.cachedSourceDirs;
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

    /**
     *
     * @returns the Ada task provider which can be useful for resolving tasks
     * created on the fly, e.g. when running GNATtest tests in coverage mode.
     */
    public getAdaTaskProvider() {
        return this.adaTaskProvider;
    }

    /**
     *
     * @returns path under the project object dir where the VS Code extension can
     * store temporary state
     */
    public async getVSCodeObjectSubdir(): Promise<string> {
        return path.join(await this.getObjectDir(), 'vscode-ada');
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

                        /**
                         * If we open a SARIF report that was already open, the
                         * SARIF Viewer extension does not refresh the
                         * contents. It is necessary to close the report and
                         * reopen it.
                         */
                        // eslint-disable-next-line max-len
                        // eslint-disable-next-line @typescript-eslint/no-unsafe-call, @typescript-eslint/no-unsafe-member-access
                        await sarifExtAPI.closeLogs([sarifFileURI]);

                        // eslint-disable-next-line max-len
                        // eslint-disable-next-line @typescript-eslint/no-unsafe-call, @typescript-eslint/no-unsafe-member-access
                        await sarifExtAPI.openLogs([sarifFileURI]);
                    }
                }
            }
        }
    }
}
