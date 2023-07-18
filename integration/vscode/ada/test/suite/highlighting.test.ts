import assert from 'assert';
import { spawnSync } from 'child_process';
import { existsSync, opendirSync, renameSync } from 'fs';
import path, { basename, dirname } from 'path';
import * as vscode from 'vscode';
import { SemanticTokensParams, SemanticTokensRequest, integer } from 'vscode-languageclient';
import { contextClients } from '../../src/extension';
import { assertEqualToFileContent, update } from './utils';

suite('Highlighting', function () {
    this.beforeAll(async function () {
        await activate();
    });

    this.afterEach(async function () {
        await vscode.commands.executeCommand('workbench.action.closeActiveEditor');
    });

    const highlightingTestRoot = getDocUri('src/highlighting').fsPath;
    const adaFilePaths: string[] = [];

    function walk(dir: string) {
        const openDir = opendirSync(dir);
        try {
            let child;
            while ((child = openDir.readSync()) != null) {
                const childPath = path.join(dir, child.name);
                if (child.isDirectory()) {
                    walk(childPath);
                } else if (child.isFile()) {
                    if (child.name.match(/\.ad[bs]$/)) {
                        adaFilePaths.push(childPath);
                    }
                }
            }
        } finally {
            openDir.closeSync();
        }
    }

    walk(highlightingTestRoot);

    for (const absPath of adaFilePaths) {
        const testName = `${basename(dirname(absPath))}/${basename(absPath)}`;
        const absFileUri = vscode.Uri.file(absPath);

        suite(testName, function () {
            test('syntax.main', function () {
                testSyntaxHighlighting(absPath, 'syntaxes');
            });

            test('syntax.advanced', function () {
                testSyntaxHighlighting(absPath, 'advanced');
            });

            test('semantic', async function () {
                await testSemanticHighlighting(absFileUri);
            });
        });
    }
});

/**
 * This function runs a semantic highlighting test on the given Ada source file
 * Uri.  The test works as follows:
 *
 * 1. A SemanticTokensRequest is sent to the ALS for the given input Uri
 *
 * 2. The tokens received from the ALS are converted to a string representation
 * that helps assess the result in comparison with the source file
 *
 * 3. The string representation is compared to a test reference stored as a file
 * next to the original source file. The convention is to append '.sem.tokens'
 * to the original file name.
 *
 * @param docUri - a Uri to an Ada source file to apply semantic highlighting
 * testing to
 */
async function testSemanticHighlighting(docUri: vscode.Uri) {
    const expectedUri = docUri.with({ path: docUri.path + '.sem.tokens' });

    const initResult = contextClients.adaClient.initializeResult;
    const legend = initResult?.capabilities.semanticTokensProvider?.legend;

    assert(legend);

    const doc = await vscode.workspace.openTextDocument(docUri);

    const request: SemanticTokensParams = {
        textDocument: { uri: docUri.toString() },
    };
    const semanticTokens = await contextClients.adaClient.sendRequest(
        SemanticTokensRequest.type,
        request
    );
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

/**
 * This function queries the VS Code API for the Ada extension and waits until
 * it is activated.
 */
async function activate(): Promise<void> {
    const ext = vscode.extensions.getExtension('AdaCore.ada');
    if (ext !== undefined) {
        if (!ext.isActive) {
            await ext.activate();
        }
    }
}

/**
 *
 * @param path - path of a file or directory relative to the TestWorkspace
 * workspace.
 * @returns a Uri representing the full path to the given relative workspace
 * path
 */
function getDocUri(path: string): vscode.Uri {
    assert(vscode.workspace.workspaceFolders !== undefined);
    return vscode.Uri.joinPath(vscode.workspace.workspaceFolders[0].uri, path);
}

const extensionRootPath = path.resolve(__dirname, '../../../');

/**
 * A type representing the two TextMate grammars available in the repository.
 * The values match directory names in the extension source directory. The
 * 'syntaxes' grammar is the one currently in use in the package.json, while the
 * 'advanced' one is an experimental alternative that is not used in production.
 */
type Syntaxes = 'syntaxes' | 'advanced';

/**
 * This function runs a syntax highlighting test on the given Ada source file
 * using the chose TextMate grammar. The test relies on the
 * vscode-tmgrammar-snap tool which operates on a preexisting test reference
 * file (aka a snapshot) and reports differences wrt that reference.
 *
 * @param absFilePath - an Ada source file to apply syntax highlighting to
 * @param syntax - the selected TextMate grammar to use for the test
 */
function testSyntaxHighlighting(absFilePath: string, syntax: Syntaxes) {
    const syntaxPath = path.join(extensionRootPath, syntax, 'ada.tmLanguage.json');

    const basename = path.basename(absFilePath);
    const workDirPath = path.dirname(absFilePath);

    /*
     * vscode-tmgrammar-snap works with .snap files, but since we're testing two
     * grammars, the snapshots are stored as .snap.<syntax-name> files. Before
     * calling vscode-tmgrammar-snap, the test will rename the
     * .snap.<syntax-name> file to .snap and rename it back after.
     */
    const workSnapPath = path.join(workDirPath, `${basename}.snap`);
    const refSnapPath = `${workSnapPath}.${syntax}`;

    try {
        if (existsSync(refSnapPath)) {
            // Rename .snap.<syntax> --> .snap
            renameSync(refSnapPath, workSnapPath);
        } else if (!update()) {
            // Complain if the reference snapshot doesn't exist, except if we're
            // running in update mode, in which case the test will create the
            // snapshot.
            throw Error(
                `Could not find reference snapshot: ${refSnapPath}\n` +
                    'Re-run testsuite in update mode to create a snapshot.'
            );
        }

        const cmd = [
            // Use npx to avoid sensitivity to PATH env var. On Windows, the
            // Node installation provides a 'npx' executable file which is a
            // Bash script which doesn't work on Windows. Instead on Windows,
            // the 'npx.cmd' file should be used.
            process.platform == 'win32' ? 'npx.cmd' : 'npx',
            'vscode-tmgrammar-snap',
            // We pass a non-existing language configuration, otherwise the tool
            // picks up the package.json file and always loads the grammar in
            // use.
            '--config',
            'none',
            // Show diffs on separate lines because color coding isn't visible
            // in the VS Code debug console.
            '--expandDiff',
            '-g',
            syntaxPath,
            '-s',
            'source.ada',
            absFilePath,
        ];

        if (update()) {
            cmd.push('--updateSnapshot');
        }

        const proc = spawnSync(cmd[0], cmd.slice(1), { cwd: workDirPath });

        if (proc.error) {
            // proc.error is set if we fail to spawn the child process
            throw proc.error;
        }

        if (proc.status === null) {
            const msg =
                `Null return code for command: ${cmd.join(' ')}\n` +
                String(proc.stdout) +
                String(proc.stderr);
            assert.fail(msg);
        } else if (proc.status != 0) {
            const msg =
                `Return code ${proc.status.toString()} for command: cd ${workDirPath}; ${cmd.join(
                    ' '
                )}\n` +
                String(proc.stdout) +
                String(proc.stderr);
            assert.fail(msg);
        }
    } finally {
        if (existsSync(workSnapPath)) {
            // Rename .snap --> .snap.<syntax>
            renameSync(workSnapPath, refSnapPath);
        }
    }
}
