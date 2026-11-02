import { existsSync } from 'fs';
import * as vscode from 'vscode';
import { envHasExec, exe, which } from './helpers';
import { createInteractiveToolTask, runTaskAndGetResult } from './taskProviders';

const HELLO_WALKTHROUGH_FOLDER = 'ada.walkthrough.helloFolder';

/** Fetch and build the Alire hello crate, then open it as the workspace. */
export async function createHelloWorldProject(context: vscode.ExtensionContext) {
    if (!envHasExec('alr')) {
        void vscode.window.showErrorMessage(
            'Alire was not found. Install Alire and add the directory containing ' +
                'the alr executable to PATH, then restart VS Code.',
        );
        return;
    }

    const folders = await vscode.window.showOpenDialog({
        title: 'Select a parent folder for the Alire Hello World project',
        openLabel: 'Download Hello Here',
        canSelectFiles: false,
        canSelectFolders: true,
        canSelectMany: false,
        defaultUri: vscode.workspace.workspaceFolders?.[0].uri,
    });
    if (!folders?.length) return;

    try {
        const crateUri = await downloadHelloWorldProject(folders[0]);
        // Opening a folder restarts the extension host. Resume the walkthrough
        // after activation in the new workspace.
        await context.globalState.update(HELLO_WALKTHROUGH_FOLDER, crateUri.toString());
        await vscode.commands.executeCommand('vscode.openFolder', crateUri);
    } catch (error) {
        void vscode.window.showErrorMessage(
            `Could not set up the Alire Hello World project: ${String(error)}`,
        );
    }
}

/**
 * Download into a new subfolder, leaving existing projects untouched. Wait for
 * the build to finish so dependencies and generated configuration are ready
 * before the language server loads the crate.
 */
export async function downloadHelloWorldProject(parent: vscode.Uri): Promise<vscode.Uri> {
    const fs = vscode.workspace.fs;
    const existing = new Set((await fs.readDirectory(parent)).map(([name]) => name));
    const task = createInteractiveToolTask(
        'Download and build Alire Hello World',
        which('alr') ?? `alr${exe}`,
        ['get', '--build', 'hello'],
        parent.fsPath,
    );
    if ((await runTaskAndGetResult(task)) !== 0) {
        throw new Error('Alire did not complete successfully. See the task terminal for details.');
    }

    // Alire names downloaded folders hello_<version>_<hash>.
    const downloaded = (await fs.readDirectory(parent)).filter(
        ([name, type]) =>
            type === vscode.FileType.Directory && name.startsWith('hello_') && !existing.has(name),
    );
    if (downloaded.length !== 1) {
        throw new Error('Could not locate the downloaded hello crate.');
    }
    const crateUri = vscode.Uri.joinPath(parent, downloaded[0][0]);
    for (const file of ['alire.toml', 'hello.gpr', 'src/hello.adb']) {
        await fs.stat(vscode.Uri.joinPath(crateUri, file));
    }
    return crateUri;
}

/** Restore the walkthrough after opening the downloaded crate's folder. */
export async function resumeHelloWorldWalkthrough(context: vscode.ExtensionContext) {
    const pendingFolder = context.globalState.get<string>(HELLO_WALKTHROUGH_FOLDER);
    const folder = vscode.workspace.workspaceFolders?.find(
        (folder) => folder.uri.toString() === pendingFolder,
    );
    if (!folder) return;

    await context.globalState.update(HELLO_WALKTHROUGH_FOLDER, undefined);
    await vscode.window.showTextDocument(vscode.Uri.joinPath(folder.uri, 'alire.toml'), {
        preserveFocus: true,
        preview: false,
    });
    await vscode.window.showTextDocument(getHelloWorldMainUri(folder), {
        preserveFocus: true,
        preview: false,
    });
    await vscode.commands.executeCommand(
        'workbench.action.openWalkthrough',
        'AdaCore.ada#als-get-started',
        true,
    );
}

/** Start debugging, adding a breakpoint in the hello crate when present. */
export async function walkthroughStartDebugging() {
    const folder = vscode.workspace.workspaceFolders?.[0];
    if (!folder) {
        void vscode.window.showInformationMessage(
            'You must open a workspace folder before using this command',
        );
        return;
    }

    const mainUri = getHelloWorldMainUri(folder);
    if (existsSync(mainUri.fsPath)) {
        const document = await vscode.workspace.openTextDocument(mainUri);
        const lines = document.getText().split(/\r?\n/);
        const begin = lines.findIndex((line) => /^\s*begin\s*$/i.test(line));
        const line = lines.findIndex(
            (text, index) => index > begin && text.trim() !== '' && !text.trim().startsWith('--'),
        );
        if (begin >= 0 && line >= 0) {
            const location = new vscode.Location(mainUri, new vscode.Position(line, 0));
            await vscode.window.showTextDocument(document, {
                selection: location.range,
            });
            const alreadySet = vscode.debug.breakpoints.some(
                (breakpoint) =>
                    breakpoint instanceof vscode.SourceBreakpoint &&
                    breakpoint.location.uri.toString() === mainUri.toString() &&
                    breakpoint.location.range.start.line === line,
            );
            if (!alreadySet) vscode.debug.addBreakpoints([new vscode.SourceBreakpoint(location)]);
        }
    }

    await vscode.commands.executeCommand('workbench.action.debug.start');
}

function getHelloWorldMainUri(folder: vscode.WorkspaceFolder) {
    return vscode.Uri.joinPath(folder.uri, 'src', 'hello.adb');
}
