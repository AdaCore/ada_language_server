import assert from 'assert';
import * as vscode from 'vscode';
import { SemanticTokensParams, SemanticTokensRequest, integer } from 'vscode-languageclient';
import { contextClients } from '../../src/extension';
import { assertEqualToFileContent } from './utils';

suite('Semantic Highlighting', function () {
    this.beforeAll(async function () {
        await activate();
    });

    this.afterEach(async function () {
        await vscode.commands.executeCommand('workbench.action.closeActiveEditor');
    });

    test('test1', async function () {
        const relFilePath = 'src/gnatpp.adb';
        await testFile(relFilePath);
    });
});

async function testFile(relFilePath: string) {
    const docUri = getDocUri(relFilePath);
    const expFilePath = `${relFilePath}.sem.tokens`;
    const expectedUri = getDocUri(expFilePath);

    //   const doc = await vscode.workspace.openTextDocument(docUri);
    //   await vscode.window.showTextDocument(doc);
    const initResult = contextClients.adaClient.initializeResult;
    const legend = initResult?.capabilities.semanticTokensProvider?.legend;

    assert(legend);

    // console.debug('Legend: ' + JSON.stringify(legend, null, 4));
    const doc = await vscode.workspace.openTextDocument(docUri);

    const request: SemanticTokensParams = {
        textDocument: { uri: docUri.toString() },
    };
    const semanticTokens = await contextClients.adaClient.sendRequest(
        SemanticTokensRequest.type,
        request
    );
    // console.debug('Semantic Tokens: ' + JSON.stringify(semanticTokens, null, 4));
    const data = semanticTokens?.data || [];
    type TokenInfo = {
        line: integer;
        column: integer;
        endColumn: integer;
        tokenType: string;
        modifiers: string[];
        text: string;
    };
    const result: TokenInfo[] = [];
    let lastLine = 0; // zero-based
    let lastColumn = 0; // zero-based
    for (let index = 0; index < data.length; index += 5) {
        // line delta relative to previous token
        const deltaLine = data[index];
        // column delta relative to 0 or the previous token start column if
        // they are on the same line
        const deltaStartChar = data[index + 1];
        const length = data[index + 2];
        // index of the token type in the legend
        const tokenType = data[index + 3];
        // bit map of active token modifiers
        const tokenModifiers = data[index + 4];

        const line = lastLine + deltaLine;
        const column = deltaLine == 0 ? lastColumn + deltaStartChar : deltaStartChar;
        const modifiers: string[] = [];

        for (let modIdx = 0; modIdx < legend.tokenModifiers.length; modIdx++) {
            if (tokenModifiers & (1 << modIdx)) {
                modifiers.push(legend.tokenModifiers[modIdx]);
            }
        }

        const text = doc.getText(new vscode.Range(line, column, line, column + length));

        const token: TokenInfo = {
            line: line,
            column: column,
            endColumn: column + length,
            tokenType: legend.tokenTypes[tokenType],
            modifiers: modifiers,
            text: text,
        };

        result.push(token);

        // Update state
        lastLine = line;
        lastColumn = column;
    }
    const maxTypeLen = Math.max(...legend.tokenTypes.map((s) => s.length));
    const maxModifiers = 2;
    const maxModifierLen =
        (maxModifiers - 1) * ', '.length +
        ' ['.length +
        ']'.length +
        maxModifiers * Math.max(...legend.tokenModifiers.map((s) => s.length));
    const actual = result
        .map(
            (t) =>
                `line ${(t.line + 1).toString().padStart(3)}: column ${(t.column + 1)
                    .toString()
                    .padStart(2)} - ${t.endColumn.toString().padStart(2)}: ${t.tokenType.padEnd(
                    maxTypeLen
                )}${(t.modifiers.length > 0 ? ' [' + t.modifiers.join(', ') + ']' : '').padEnd(
                    maxModifierLen
                )} : ${t.text}`
        )
        .join('\n');

    assertEqualToFileContent(actual, expectedUri);
}

async function activate(): Promise<void> {
    const ext = vscode.extensions.getExtension('AdaCore.ada');
    if (ext !== undefined) {
        if (!ext.isActive) {
            await ext.activate();
        }
    }
}

function getDocUri(path: string): vscode.Uri {
    assert(vscode.workspace.workspaceFolders !== undefined);
    return vscode.Uri.joinPath(vscode.workspace.workspaceFolders[0].uri, path);
}
