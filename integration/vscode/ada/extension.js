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

'use strict';
Object.defineProperty(exports, "__esModule", { value: true });
const vscode = require("vscode");
const vscode_languageclient_1 = require("vscode-languageclient");
const process = require("process");
const gprTaskProvider = require("./gprTaskProvider");
let myTaskProvider;
function activate(context) {
    // The server is implemented in node
    let serverModule = context.asAbsolutePath(process.platform + '/ada_language_server');
    // let serverModule = "/tmp/lsp_test";
    // The debug options for the server
    let debugOptions = { execArgv: [] };
    // If the extension is launched in debug mode then the debug server options are used
    // Otherwise the run options are used
    let serverOptions = {
        run: { command: serverModule },
        debug: { command: serverModule }
    };
    // Options to control the language client
    let clientOptions = {
        // Register the server for ada sources documents
        documentSelector: [{ scheme: 'file', language: 'ada' }],
        synchronize: {
            // Synchronize the setting section 'ada' to the server
            configurationSection: 'ada',
            // Notify the server about file changes to '.clientrc files contain in the workspace
            fileEvents: vscode.workspace.createFileSystemWatcher('**/.clientrc')
        }
    };
    // Create the language client and start the client.
    let client = new vscode_languageclient_1.LanguageClient
        ('ada', 'Ada Language Server', serverOptions, clientOptions);
    let disposable = client.start();
    // Push the disposable to the context's subscriptions so that the 
    // client can be deactivated on extension deactivation
    context.subscriptions.push(disposable);
    myTaskProvider = vscode.tasks.registerTaskProvider
      (gprTaskProvider.GPRTaskProvider.gprBuildType,
       new gprTaskProvider.GPRTaskProvider());

    //  Take active editor URI and call execute 'als-other-file' command in LSP
    function otherFileHandler () {
        const activeEditor = vscode.window.activeTextEditor;
        if (!activeEditor) {
          return;
        }
        client.sendRequest(
            vscode_languageclient_1.ExecuteCommandRequest.type,
            {
                'command': 'als-other-file',
                'arguments': [{
                    'uri': activeEditor.document.uri.toString()
                }]
            }
        );
    };

    context.subscriptions.push(vscode.commands.registerCommand('ada.otherFile', otherFileHandler));
}

exports.activate = activate;

function deactivate() {
    if (myTaskProvider) {
        myTaskProvider.dispose();
        myTaskProvider = undefined;
    }
}
exports.deactivate = deactivate;
