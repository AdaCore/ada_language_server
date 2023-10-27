import * as vscode from 'vscode';
import { Disposable, LanguageClient } from 'vscode-languageclient/node';
import { createClient } from './clients';
import GnatTaskProvider from './gnatTaskProvider';
import GprTaskProvider from './gprTaskProvider';
import { registerTaskProviders } from './taskProviders';

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

    private clientsDisposables: Disposable[];
    private registeredTaskProviders: Disposable[];

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
        this.clientsDisposables = [];
        this.registeredTaskProviders = [];
    }

    public start = () => {
        this.clientsDisposables = [this.gprClient.start(), this.adaClient.start()];
        this.registerTaskProviders();
    };

    public dispose = () => {
        this.unregisterTaskProviders();
        this.clientsDisposables.forEach((clientDisposable: Disposable) =>
            clientDisposable.dispose()
        );
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

    //  React to changes in configuration to recompute predefined tasks if the user
    //  changes scenario variables' values.
    public configChanged = (e: vscode.ConfigurationChangeEvent) => {
        if (
            e.affectsConfiguration('ada.scenarioVariables') ||
            e.affectsConfiguration('ada.projectFile')
        ) {
            this.unregisterTaskProviders();
            this.registerTaskProviders();
        }
    };
}
