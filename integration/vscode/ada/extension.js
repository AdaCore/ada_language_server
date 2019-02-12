'use strict';
Object.defineProperty(exports, "__esModule", { value: true });
const vscode_1 = require("vscode");
const vscode_languageclient_1 = require("vscode-languageclient");
const process = require("process");
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
            fileEvents: vscode_1.workspace.createFileSystemWatcher('**/.clientrc')
        }
    };
    // Create the language client and start the client.
    let disposable = new vscode_languageclient_1.LanguageClient('ada', 'Ada Language Server', serverOptions, clientOptions).start();
    // Push the disposable to the context's subscriptions so that the 
    // client can be deactivated on extension deactivation
    context.subscriptions.push(disposable);
}
exports.activate = activate;
