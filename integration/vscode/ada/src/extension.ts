/*----------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2018-2021, AdaCore                     --
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

import * as vscode from 'vscode';
import {
    ExecuteCommandRequest,
    LanguageClient,
    LanguageClientOptions,
    Middleware,
    ServerOptions,
} from 'vscode-languageclient/node';
import * as process from 'process';

import GPRTaskProvider from './gprTaskProvider';
import cleanTaskProvider from './cleanTaskProvider';
import gnatproveTaskProvider from './gnatproveTaskProvider';
import { alsCommandExecutor } from './alsExecuteCommand';
import { ALSClientFeatures } from './alsClientFeatures';

let alsTaskProvider: vscode.Disposable[] = [
    vscode.tasks.registerTaskProvider(GPRTaskProvider.gprBuildType, new GPRTaskProvider()),
    vscode.tasks.registerTaskProvider(cleanTaskProvider.cleanTaskType, new cleanTaskProvider()),

    vscode.tasks.registerTaskProvider(
        gnatproveTaskProvider.gnatproveType,
        new gnatproveTaskProvider()
    ),
];

export function activate(context: vscode.ExtensionContext): void {
    function createClient(id: string, name: string, extra: string[], pattern: string) {
        let serverModule = context.asAbsolutePath(process.platform + '/ada_language_server');
        if (process.env.ALS) serverModule = process.env.ALS;
        // The debug options for the server
        // let debugOptions = { execArgv: [] };
        // If the extension is launched in debug mode then the debug server options are used
        // Otherwise the run options are used
        const serverOptions: ServerOptions = {
            run: { command: serverModule, args: extra },
            debug: { command: serverModule, args: extra },
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
    // Create the GPR language client and start it.
    const gprClient = createClient('gpr', 'GPR Language Server', ['--language-gpr'], '**/.{gpr}');
    context.subscriptions.push(gprClient.start());
    // Create the Ada language client and start it.
    const client = createClient('ada', 'Ada Language Server', [], '**/.{adb,ads,adc,ada}');
    const alsMiddleware: Middleware = {
        executeCommand: alsCommandExecutor(client),
    };
    client.clientOptions.middleware = alsMiddleware;
    client.registerFeature(new ALSClientFeatures());

    const disposable = client.start();
    // Push the disposable to the context's subscriptions so that the
    // client can be deactivated on extension deactivation
    context.subscriptions.push(disposable);

    //  Take active editor URI and call execute 'als-other-file' command in LSP
    function otherFileHandler() {
        const activeEditor = vscode.window.activeTextEditor;
        if (!activeEditor) {
            return;
        }
        void client.sendRequest(ExecuteCommandRequest.type, {
            command: 'als-other-file',
            arguments: [
                {
                    uri: activeEditor.document.uri.toString(),
                },
            ],
        });
    }

    //  React to changes in configuration to recompute predefined tasks if the user
    //  changes scenario variables' values.
    function configChanged(e: vscode.ConfigurationChangeEvent) {
        if (e.affectsConfiguration('ada.scenarioVariables')) {
            for (const item of alsTaskProvider) {
                item.dispose();
            }
            alsTaskProvider = [
                vscode.tasks.registerTaskProvider(
                    GPRTaskProvider.gprBuildType,
                    new GPRTaskProvider()
                ),
                vscode.tasks.registerTaskProvider(
                    cleanTaskProvider.cleanTaskType,
                    new cleanTaskProvider()
                ),
                vscode.tasks.registerTaskProvider(
                    gnatproveTaskProvider.gnatproveType,
                    new gnatproveTaskProvider()
                ),
            ];
        }
    }

    context.subscriptions.push(vscode.workspace.onDidChangeConfiguration(configChanged));
    context.subscriptions.push(vscode.commands.registerCommand('ada.otherFile', otherFileHandler));
}

export function deactivate(): void {
    for (const item of alsTaskProvider) {
        item.dispose();
    }
    alsTaskProvider = [];
}
