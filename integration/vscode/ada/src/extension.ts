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
import { string } from 'fp-ts';

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

    //  Check if we need to add some source directories to the workspace (e.g: when imported projects'
    //  source directories are not placed under the root project's directory).
    client.onReady().then(() => {
        client.sendRequest("workspace/alsSourceDirs").then(
            data => {
                const source_dirs = JSON.parse(JSON.stringify(data));
                const workspace_folders = vscode.workspace.workspaceFolders ?? []
                let workspace_dirs_to_add: { uri: vscode.Uri, name?: string | undefined}[] = []

                for (var source_dir of source_dirs) {
                    const source_dir_uri = vscode.Uri.parse(source_dir.uri)
                    let source_dir_path = source_dir_uri.path
                    let workspace_folder_path = workspace_folders[0].uri.path

                    function is_subdirectory(dir: string, parent: string) {
                        //  Use lower-case on Windows since drives can be specified in VS Code either
                        //  with lower or upper case characters.
                        if (process.platform == "win32") {
                            dir = dir.toLowerCase();
                            parent = parent.toLowerCase();
                        }

                        return dir.startsWith(parent + '/');

                    }

                    //  If the source directory is not under one of the workspace folders, push this
                    //  source directory to the workspace folders to add later.
                    if (!workspace_folders.some(workspace_folder =>
                        is_subdirectory(source_dir_path, workspace_folder.uri.path))) {
                        workspace_dirs_to_add.push({name: source_dir.name, uri: source_dir_uri});
                    }
                }

                //  If there are some source directories missing in the workspace, ask the user
                //  to add them in his workspace.
                if (workspace_dirs_to_add.length > 0) {
                    vscode.window
                        .showInformationMessage("Some project source directories are not "
                            + "listed in your workspace: do you want to add them?", "Yes", "No")
                        .then(answer => {
                            if (answer === "Yes") {
                                for (var workspace_dir of workspace_dirs_to_add) {
                                  vscode.workspace.updateWorkspaceFolders(
                                        vscode.workspace.workspaceFolders ? vscode.workspace.workspaceFolders.length : 0, null,
                                        workspace_dir);
                                    }
                            }
                        })
                }
            }
        )
    })
}

export function deactivate(): void {
    for (const item of alsTaskProvider) {
        item.dispose();
    }
    alsTaskProvider = [];
}
