import { existsSync } from 'fs';
import * as vscode from 'vscode';

/**
 * This function creates a hello world project in the current workspace. It is
 * called from the walkthrough of the extension.
 */
export async function createHelloWorldProject() {
    if (vscode.workspace.workspaceFolders) {
        const wsFolder = vscode.workspace.workspaceFolders[0];
        const fs = vscode.workspace.fs;
        const prjUri = vscode.Uri.joinPath(wsFolder.uri, 'hello_world.gpr');
        const srcUri = vscode.Uri.joinPath(wsFolder.uri, 'src');
        const objUri = vscode.Uri.joinPath(wsFolder.uri, 'obj');
        const mainUri = getHelloWorldMainUri(wsFolder);

        await fs.createDirectory(srcUri);
        // Create obj/ directory to avoid GPR warning
        await fs.createDirectory(objUri);
        await createFile(
            prjUri,
            `project Hello_World is
   for Source_Dirs use ("src");
   for Object_Dir use "obj";
   for Main use ("hello_world.adb");

   package Compiler is
      for Switches ("Ada") use ("-g", "-O0");
   end Compiler;
end Hello_World;
`
        );
        await createFile(
            mainUri,
            `with Ada.Text_IO;

procedure Hello_World is
    Msg : constant String := "Hello World!";
begin
    Ada.Text_IO.Put_Line (Msg);
end Hello_World;
`
        );

        /**
         * Set ada.projectFile explicitly since that's recommended.
         */
        await vscode.workspace
            .getConfiguration()
            .update('ada.projectFile', vscode.workspace.asRelativePath(prjUri));

        /**
         * Show the created files for convenience.
         */
        await vscode.window.showTextDocument(prjUri, { preserveFocus: true, preview: false });
        await vscode.window.showTextDocument(mainUri, { preserveFocus: true, preview: false });
    } else {
        void vscode.window.showErrorMessage(
            'You must open a workspace folder before creating the Ada Hello World project'
        );
    }
}

async function createFile(uri: vscode.Uri, content: string) {
    const fs = vscode.workspace.fs;
    try {
        // First we check that the file doesn't exist.
        await fs.stat(uri);
        const msg = `The file ${vscode.workspace.asRelativePath(
            uri
        )} already exists. Retry in an empty workspace.`;
        void vscode.window.showErrorMessage(msg);
        throw Error(msg);
    } catch (error) {
        if (
            error instanceof vscode.FileSystemError &&
            error.code == vscode.FileSystemError.FileNotFound.name
        ) {
            // The file doesn't exist. We can proceed.
        } else {
            // Unexpected error, throw it.
            throw error;
        }
    }

    await fs.writeFile(uri, Buffer.from(content, 'utf-8'));
}

/**
 * This function is called from the walkthrough to start a debugging session.
 *
 * If the hello world source file exists, it opens it and sets sets a
 * breakpoint. Otherwise no breakpoint is set.
 *
 * Finally the workbench.action.debug.start command is called to start the
 * debug session.
 */
export async function walkthroughStartDebugging() {
    if (vscode.workspace.workspaceFolders) {
        const wsFolder = vscode.workspace.workspaceFolders[0];
        const mainUri = getHelloWorldMainUri(wsFolder);

        if (existsSync(mainUri.fsPath)) {
            await vscode.window.showTextDocument(mainUri, {
                preserveFocus: false,
                selection: new vscode.Range(5, 0, 5, 0),
            });

            /**
             * Since there's only a command to toggle breakpoints, we must
             * clear all breakpoints first to make sure we are enabling the
             * desired breakpoint.
             */
            await vscode.commands.executeCommand(
                'workbench.debug.viewlet.action.removeAllBreakpoints'
            );
            await vscode.commands.executeCommand('editor.debug.action.toggleBreakpoint');
        }

        await vscode.commands.executeCommand('workbench.action.debug.start');
    } else {
        void vscode.window.showInformationMessage(
            'You must open a workspace folder before using this command'
        );
    }
}

/**
 * This function is defined to share the main source file location between the
 * creation of the hello world project, and the starting of the debug session.
 *
 * @param wsFolder - the root WorkspaceFolder
 * @returns the path to the hello world main source file.
 */
function getHelloWorldMainUri(wsFolder: vscode.WorkspaceFolder) {
    return vscode.Uri.joinPath(wsFolder.uri, 'src', 'hello_world.adb');
}
