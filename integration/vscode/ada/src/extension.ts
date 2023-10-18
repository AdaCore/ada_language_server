/* eslint-disable @typescript-eslint/no-unsafe-call */
/*----------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2018-2023, AdaCore                     --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
----------------------------------------------------------------------------*/

import * as process from 'process';
import * as vscode from 'vscode';

import { LanguageClient, Middleware } from 'vscode-languageclient/node';
import { ALSClientFeatures } from './alsClientFeatures';
import { alsCommandExecutor } from './alsExecuteCommand';
import { ContextClients } from './clients';
import { registerCommands } from './commands';
import { initializeDebugging } from './debugConfigProvider';
import { initializeTestView } from './gnattest';
import {
    assertSupportedEnvironments,
    getEvaluatedCustomEnv,
    setCustomEnvironment,
} from './helpers';
import { ExecuteCommandRequest } from 'vscode-languageclient/node';

const ADA_CONTEXT = 'ADA_PROJECT_CONTEXT';
export let contextClients: ContextClients;
export let mainLogChannel: vscode.OutputChannel;

export async function activate(context: vscode.ExtensionContext): Promise<void> {
    // Create an output channel for the extension. There are dedicated channels
    // for the Ada and Gpr language servers, and this one is a general channel
    // for non-LSP features of the extension.
    mainLogChannel = vscode.window.createOutputChannel('Ada Extension');
    mainLogChannel.appendLine('Starting Ada extension');

    // Log the environment that the extension (and all VS Code) will be using
    const customEnv = getEvaluatedCustomEnv();

    if (customEnv && Object.keys(customEnv).length > 0) {
        mainLogChannel.appendLine('Setting environment variables:');
        for (const varName in customEnv) {
            const varValue: string = customEnv[varName];
            mainLogChannel.appendLine(`${varName}=${varValue}`);
        }
    }

    // Set the custom environment into the current node process. This must be
    // done before calling assertSupportedEnvironments in order to set the ALS
    // environment variable if provided.
    setCustomEnvironment();

    assertSupportedEnvironments(mainLogChannel);

    // Create the Ada and GPR clients.
    contextClients = new ContextClients(context);

    const alsMiddleware: Middleware = {
        executeCommand: alsCommandExecutor(contextClients.adaClient),
    };
    contextClients.adaClient.clientOptions.middleware = alsMiddleware;
    contextClients.adaClient.registerFeature(new ALSClientFeatures());

    contextClients.start();
    context.subscriptions.push(contextClients);

    context.subscriptions.push(
        vscode.workspace.onDidChangeConfiguration(contextClients.configChanged)
    );

    await Promise.all([contextClients.adaClient.onReady(), contextClients.gprClient.onReady()]);

    await vscode.commands.executeCommand('setContext', ADA_CONTEXT, true);

    await checkSrcDirectories(contextClients.adaClient);

    await initializeTestView(context, contextClients);

    await Promise.all([contextClients.adaClient.onReady(), contextClients.gprClient.onReady()]);

    initializeDebugging(context);

    registerCommands(context, contextClients);

    mainLogChannel.appendLine('Started Ada extension');
}

export async function deactivate() {
    await vscode.commands.executeCommand('setContext', ADA_CONTEXT, undefined);
}

type ALSSourceDirDescription = {
    name: string;
    uri: string;
};

/**
 *
 * Check if we need to add some source directories to the workspace (e.g: when imported
 * projects' source directories are not placed under the root project's directory).
 * Do nothing is the user did not setup any workspace file.
 *
 */
async function checkSrcDirectories(alsClient: LanguageClient) {
    const foldersInSettings = vscode.workspace.getConfiguration().get('folders') ?? [];

    //  Don't propose any popup if we multi-root workspace folders are already set
    //  explicitly in the workspace's settings.
    if (foldersInSettings !== undefined) {
        const sourceDirs: ALSSourceDirDescription[] = (await alsClient.sendRequest(
            ExecuteCommandRequest.type,
            {
                command: 'als-source-dirs',
            }
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
                        isSubdirectory(sourceDirPath, workspaceFolder.uri.path)
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
            await vscode.window
                .showInformationMessage(
                    'Some project source directories are not \
                    listed in your workspace: do you want to add them?',
                    'Yes',
                    'No'
                )
                .then((answer) => {
                    if (answer === 'Yes') {
                        for (const workspaceDir of workspaceDirsToAdd) {
                            vscode.workspace.updateWorkspaceFolders(
                                vscode.workspace.workspaceFolders
                                    ? vscode.workspace.workspaceFolders.length
                                    : 0,
                                null,
                                workspaceDir
                            );
                        }
                    }
                });
        }
    }
}
