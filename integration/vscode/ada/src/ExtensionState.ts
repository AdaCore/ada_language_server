import * as vscode from 'vscode';
import { Disposable, ExecuteCommandRequest, LanguageClient } from 'vscode-languageclient/node';
import { AdaCodeLensProvider } from './AdaCodeLensProvider';
import { createClient } from './clients';
import { AdaInitialDebugConfigProvider, initializeDebugging } from './debugConfigProvider';
import { GnatTaskProvider } from './gnatTaskProvider';
import { initializeTesting } from './gnattest';
import { GprTaskProvider } from './gprTaskProvider';
import { TERMINAL_ENV_SETTING_NAME } from './helpers';
import { registerTaskProviders } from './taskProviders';
import { initializeTesting } from './gnattest';

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
            _folder?: vscode.WorkspaceFolder | undefined
        ): Promise<vscode.DebugConfiguration[]>;
    };
    public readonly initialDebugConfigProvider: AdaInitialDebugConfigProvider;

    private registeredTaskProviders: Disposable[];

    public readonly codelensProvider = new AdaCodeLensProvider();
    public readonly testController: vscode.TestController;
    public readonly testData: Map<vscode.TestItem, object> = new Map();

    /**
     * The following fields are caches for ALS requests
     */
    cachedProjectFile: string | undefined;
    cachedObjectDir: string | undefined;
    cachedMains: string[] | undefined;
    cachedExecutables: string[] | undefined;

    public clearALSCache() {
        this.cachedProjectFile = undefined;
        this.cachedObjectDir = undefined;
        this.cachedMains = undefined;
        this.cachedExecutables = undefined;
    }

    constructor(context: vscode.ExtensionContext) {
        this.context = context;
        this.gprClient = createClient(
            context,
            'gpr',
            'GPR Language Server',
            ['--language-gpr'],
            '**/.{gpr}'
        );
        this.adaClient = createClient(
            context,
            'ada',
            'Ada Language Server',
            [],
            '**/.{adb,ads,adc,ada}'
        );
        this.registeredTaskProviders = [];
        const result = initializeDebugging(this.context);
        this.initialDebugConfigProvider = result.providerInitial;
        this.dynamicDebugConfigProvider = result.providerDynamic;
        this.testController = initializeTesting(context);
    }

    public start = async () => {
        await Promise.all([this.gprClient.start(), this.adaClient.start()]);
        this.registerTaskProviders();
        this.context.subscriptions.push(
            vscode.languages.registerCodeLensProvider('ada', this.codelensProvider)
        );
    };

    public dispose = () => {
        this.unregisterTaskProviders();
    };

    public registerTaskProviders = (): void => {
        this.registeredTaskProviders = [
            vscode.tasks.registerTaskProvider(GnatTaskProvider.gnatType, new GnatTaskProvider()),
            vscode.tasks.registerTaskProvider(
                GprTaskProvider.gprTaskType,
                new GprTaskProvider(this.adaClient)
            ),
        ].concat(registerTaskProviders());
    };

    public unregisterTaskProviders = (): void => {
        for (const item of this.registeredTaskProviders) {
            item.dispose();
        }
        this.registeredTaskProviders = [];
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
            'Reload Window'
        );

        // Reload the VS Code window if the user selected 'Yes'
        if (selection == 'Reload Window') {
            void vscode.commands.executeCommand('workbench.action.reloadWindow');
        }
    };

    //  React to changes in configuration to recompute predefined tasks if the user
    //  changes scenario variables' values.
    public configChanged = (e: vscode.ConfigurationChangeEvent) => {
        if (
            e.affectsConfiguration('ada.scenarioVariables') ||
            e.affectsConfiguration('ada.projectFile')
        ) {
            this.clearALSCache();
            this.unregisterTaskProviders();
            this.registerTaskProviders();
        }

        //  React to changes made in the environment variables, showing
        //  a popup to reload the VS Code window and thus restart the
        //  Ada extension.
        if (e.affectsConfiguration(TERMINAL_ENV_SETTING_NAME)) {
            void this.showReloadWindowPopup();
        }
    };

    /**
     * @returns the full path of the main project file from the ALS
     */
    public async getProjectFile(): Promise<string> {
        if (!this.cachedProjectFile) {
            this.cachedProjectFile = (await this.adaClient.sendRequest(ExecuteCommandRequest.type, {
                command: 'als-project-file',
            })) as string;
        }

        return this.cachedProjectFile;
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
}
