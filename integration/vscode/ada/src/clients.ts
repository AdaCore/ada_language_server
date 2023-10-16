import { existsSync } from 'fs';
import * as vscode from 'vscode';
import {
    Disposable,
    ExecuteCommandRequest,
    LanguageClient,
    LanguageClientOptions,
    ServerOptions,
} from 'vscode-languageclient/node';
import { mainLogChannel } from './extension';
import GnatTaskProvider from './gnatTaskProvider';
import GprTaskProvider from './gprTaskProvider';
import { logErrorAndThrow } from './helpers';
import { registerTaskProviders } from './taskProviders';

export class ContextClients {
    public readonly gprClient: LanguageClient;
    public readonly adaClient: LanguageClient;

    private clientsDisposables: Disposable[];
    private registeredTaskProviders: Disposable[];

    constructor(context: vscode.ExtensionContext) {
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

    //  Take active editor URI and call execute 'als-other-file' command in LSP
    public otherFileHandler = () => {
        const activeEditor = vscode.window.activeTextEditor;
        if (!activeEditor) {
            return;
        }
        void this.adaClient.sendRequest(ExecuteCommandRequest.type, {
            command: 'als-other-file',
            arguments: [
                {
                    uri: activeEditor.document.uri.toString(),
                },
            ],
        });
    };
}

function createClient(
    context: vscode.ExtensionContext,
    id: string,
    name: string,
    extra: string[],
    pattern: string
) {
    let serverExecPath: string;

    if (process.arch == 'arm64' && process.platform == 'darwin') {
        // On arm64 darwin use the x64 darwin executable thanks to Apple Rosetta.
        serverExecPath = context.asAbsolutePath(`x64/darwin/ada_language_server`);
    } else {
        serverExecPath = context.asAbsolutePath(
            `${process.arch}/${process.platform}/ada_language_server`
        );
    }

    if (process.platform == 'win32') {
        // Add the extension for the file lookup further below
        serverExecPath = `${serverExecPath}.exe`;
    }

    // If the ALS environment variable is specified, use it as the path of the
    // server executable.
    if (process.env.ALS) {
        serverExecPath = process.env.ALS;
        if (!existsSync(serverExecPath)) {
            logErrorAndThrow(
                `The Ada language server given in the ALS environment ` +
                    `variable does not exist: ${serverExecPath}`,
                mainLogChannel
            );
        }
    } else {
        if (!existsSync(serverExecPath)) {
            logErrorAndThrow(
                `This installation of the Ada extension does not have the Ada ` +
                    `language server for your architecture (${process.arch}) ` +
                    `and platform (${process.platform}) ` +
                    `at the expected location: ${serverExecPath}`,
                mainLogChannel
            );
        }
    }

    // The debug options for the server
    // let debugOptions = { execArgv: [] };
    // If the extension is launched in debug mode then the debug server options are used
    // Otherwise the run options are used

    // Options to control the server
    const serverOptions: ServerOptions = {
        run: { command: serverExecPath, args: extra },
        debug: { command: serverExecPath, args: extra },
    };

    // Options to control the language client
    const clientOptions: LanguageClientOptions = {
        // Register the server for ada sources documents
        documentSelector: [{ scheme: 'file', language: id }],
        synchronize: {
            // Synchronize the setting section 'ada' to the server
            configurationSection: 'ada',
            // Notify the server about file changes to Ada files contain in the workspace
            fileEvents: vscode.workspace.createFileSystemWatcher(pattern),
        },
    };
    // Create the language client
    return new LanguageClient(id, name, serverOptions, clientOptions);
}
