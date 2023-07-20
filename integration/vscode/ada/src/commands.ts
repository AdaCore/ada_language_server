import * as vscode from 'vscode';
import { SymbolKind } from 'vscode';
import { getEnclosingSymbol } from './gnatTaskProvider';
import { mainLogChannel } from './extension';
import { ContextClients } from './clients';
import { AdaDebugConfigProvider } from './debugConfigProvider';

export function registerCommands(
    context: vscode.ExtensionContext,
    clients: ContextClients,
    debug: AdaDebugConfigProvider
) {
    context.subscriptions.push(
        vscode.commands.registerCommand('ada.otherFile', clients.otherFileHandler)
    );
    context.subscriptions.push(
        vscode.commands.registerCommand('ada.subprogramBox', addSupbrogramBoxCommand)
    );
    context.subscriptions.push(
        vscode.commands.registerCommand('ada.showExtensionOutput', () => mainLogChannel.show())
    );
    context.subscriptions.push(
        vscode.commands.registerCommand('ada.showAdaLSOutput', () =>
            clients.adaClient.outputChannel.show()
        )
    );
    context.subscriptions.push(
        vscode.commands.registerCommand('ada.showGprLSOutput', () =>
            clients.gprClient.outputChannel.show()
        )
    );
    context.subscriptions.push(
        vscode.commands.registerCommand('ada.initDebugFile', async () => {
            const p = await debug.initDebugCmd();
            return p;
        })
    );
}
/**
 * Add a subprogram box above the subprogram enclosing the cursor's position, if any.
 *
 * @example
 *
 *  -------
 *  - Foo -
 *  -------
 *
 *  procedure Foo is
 */
async function addSupbrogramBoxCommand() {
    const activeEditor = vscode.window.activeTextEditor;

    await getEnclosingSymbol(activeEditor, [
        SymbolKind.Function,
        SymbolKind.Package,
        SymbolKind.Module,
    ]).then(async (symbol) => {
        if (symbol !== null) {
            const name: string = symbol.name ?? '';
            const insertPos = new vscode.Position(symbol.range.start.line, 0);
            const indentationRange = new vscode.Range(insertPos, symbol.range.start);
            const indentation: string = activeEditor?.document.getText(indentationRange) ?? '';
            const eol: string = activeEditor?.document.eol == vscode.EndOfLine.CRLF ? '\r\n' : '\n';

            // Generate the subprogram box after retrieving the indentation of the line of
            // the subprogram's body declaration.
            const text: string =
                indentation +
                '---' +
                '-'.repeat(name.length) +
                '---' +
                eol +
                indentation +
                '-- ' +
                name +
                ' --' +
                eol +
                indentation +
                '---' +
                '-'.repeat(name.length) +
                '---' +
                eol +
                eol;

            if (activeEditor) {
                await activeEditor.edit((editBuilder) => {
                    editBuilder.insert(insertPos, text);
                });
            }
        }
    });
}
