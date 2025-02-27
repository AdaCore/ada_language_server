import { existsSync } from 'fs';
import * as vscode from 'vscode';
import {
    LanguageClient,
    LanguageClientOptions,
    MessageSignature,
    ServerOptions,
} from 'vscode-languageclient/node';
import { logger } from './extension';
import { logErrorAndThrow, setTerminalEnvironment } from './helpers';

export class AdaLanguageClient extends LanguageClient {
    /**
     * Used to store the client's process environment.
     * Changes are taken into account when calling the
     * {@link vscode.LanguageClient.restart} function.
     */
    public serverEnv: NodeJS.ProcessEnv = {};

    /**
     * Override this function to avoid displaying popup notifications on LSP errors when
     * the 'showNotificationsOnErrors' setting is disabled.
     */
    override handleFailedRequest<T>(
        type: MessageSignature,
        token: vscode.CancellationToken | undefined,
        // eslint-disable-next-line @typescript-eslint/no-explicit-any
        error: any,
        defaultValue: T,
        showNotification?: boolean | undefined,
    ): T {
        const showError = vscode.workspace.getConfiguration('ada').get('showNotificationsOnErrors');
        if (!showError) {
            showNotification = false;
        }

        return super.handleFailedRequest(type, token, error, defaultValue, showNotification);
    }

    /**
     * Override this function to update the client's process environment according to
     * VS Code settings before restarting it.
     */
    override restart(): Promise<void> {
        // Update the process environment before restarting the language servers, so
        // that potential changes in VS Code settings related to the environment are
        // taken into account.
        setTerminalEnvironment(this.serverEnv);

        // Restart the server
        logger.info('Restarting language server: ' + this.name);
        return super.restart();
    }
}
export function createClient(
    context: vscode.ExtensionContext,
    id: string,
    name: string,
    extra: string[],
    pattern: string,
): AdaLanguageClient {
    let serverExecPath: string;

    // If the ALS environment variable is specified, use it as the path of the
    // server executable.
    if (process.env.ALS) {
        serverExecPath = process.env.ALS;
        if (!existsSync(serverExecPath)) {
            logErrorAndThrow(
                `The Ada language server given in the ALS environment ` +
                    `variable does not exist: ${serverExecPath}`,
                logger,
            );
        }
    } else {
        serverExecPath = context.asAbsolutePath(
            `${process.arch}/${process.platform}/ada_language_server`,
        );

        if (process.arch == 'arm64' && process.platform == 'darwin') {
            // On arm64 darwin check if the executable exists, and if not, try to
            // fallback to the x64 darwin executable thanks to Apple Rosetta.
            if (!existsSync(serverExecPath)) {
                // The arm64 executable doesn't exist. Try x86.
                const alternateExecPath = context.asAbsolutePath(
                    `x64/${process.platform}/ada_language_server`,
                );
                if (existsSync(alternateExecPath)) {
                    // The x86 executable exists, use that instead.
                    serverExecPath = alternateExecPath;
                }
            }
        } else if (process.platform == 'win32') {
            // Add the extension for the file lookup further below
            serverExecPath = `${serverExecPath}.exe`;
        }

        if (!existsSync(serverExecPath)) {
            logErrorAndThrow(
                `This installation of the Ada extension does not have the Ada ` +
                    `language server for your architecture (${process.arch}) ` +
                    `and platform (${process.platform}) ` +
                    `at the expected location: ${serverExecPath}`,
                logger,
            );
        }
    }

    logger.info(`Using ALS at: ${serverExecPath}`);

    // Copy this process's environment
    const serverEnv: NodeJS.ProcessEnv = { ...process.env };
    // Set custom environment
    setTerminalEnvironment(serverEnv);

    // Options to control the server
    const serverOptions: ServerOptions = {
        run: { command: serverExecPath, args: extra, options: { env: serverEnv } },
        debug: { command: serverExecPath, args: extra, options: { env: serverEnv } },
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
        // Include the ada.* settings in the initialize request sent to the server
        initializationOptions: () => ({ ada: vscode.workspace.getConfiguration('ada') }),
    };
    // Create the language client
    const client = new AdaLanguageClient(id, name, serverOptions, clientOptions);
    client.serverEnv = serverEnv;
    return client;
}
