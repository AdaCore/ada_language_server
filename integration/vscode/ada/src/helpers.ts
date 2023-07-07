/*----------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2021-2023, AdaCore                     --
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
import * as path from 'path';

/**
 * Substitue any variable reference present in the given string. VS Code
 * variable references are listed here:
 * https://code.visualstudio.com/docs/editor/variables-reference
 * @param str
 * @param recursive
 * @returns
 */
export function substituteVariables(str: string, recursive = false) {
    const workspaces = vscode.workspace.workspaceFolders ?? [];
    const workspace = workspaces.length ? workspaces[0] : null;
    const activeEditor = vscode.window.activeTextEditor;
    const activeFile = activeEditor?.document;
    const absoluteFilePath = activeFile?.uri.fsPath ?? '';

    if (workspace != null) {
        str = str.replace(/\${workspaceFolder}/g, workspace?.uri.fsPath);
        str = str.replace(/\${workspaceFolderBasename}/g, workspace?.name);
    }

    str = str.replace(/\${file}/g, absoluteFilePath);
    let activeWorkspace = workspace;
    let relativeFilePath = absoluteFilePath;
    for (const workspace of workspaces) {
        if (absoluteFilePath.replace(workspace.uri.fsPath, '') !== absoluteFilePath) {
            activeWorkspace = workspace;
            relativeFilePath = absoluteFilePath
                .replace(workspace.uri.fsPath, '')
                .substr(path.sep.length);
            break;
        }
    }
    const parsedPath = path.parse(absoluteFilePath);

    if (activeWorkspace != null) {
        str = str.replace(/\${fileWorkspaceFolder}/g, activeWorkspace?.uri.fsPath);
    }

    str = str.replace(/\${relativeFile}/g, relativeFilePath);
    str = str.replace(
        /\${relativeFileDirname}/g,
        relativeFilePath.substr(0, relativeFilePath.lastIndexOf(path.sep))
    );
    str = str.replace(/\${fileBasename}/g, parsedPath.base);
    str = str.replace(/\${fileBasenameNoExtension}/g, parsedPath.name);
    str = str.replace(/\${fileExtname}/g, parsedPath.ext);
    str = str.replace(
        /\${fileDirname}/g,
        parsedPath.dir.substr(parsedPath.dir.lastIndexOf(path.sep) + 1)
    );
    str = str.replace(/\${cwd}/g, parsedPath.dir);
    str = str.replace(/\${pathSeparator}/g, path.sep);

    if (activeEditor != null) {
        str = str.replace(/\${lineNumber}/g, (activeEditor.selection.start.line + 1).toString());
        str = str.replace(
            /\${selectedText}/g,
            activeEditor.document.getText(
                new vscode.Range(activeEditor.selection.start, activeEditor.selection.end)
            )
        );
    }

    str = str.replace(/\${env:(.*?)}/g, function (variable) {
        return process.env[variable.match(/\${env:(.*?)}/)![1]] || '';
    });

    str = str.replace(/\${config:(.*?)}/g, function (variable) {
        return vscode.workspace.getConfiguration().get(variable.match(/\${config:(.*?)}/)![1], '');
    });

    if (
        recursive &&
        str.match(
            /\${(workspaceFolder|workspaceFolderBasename|fileWorkspaceFolder|relativeFile|fileBasename|fileBasenameNoExtension|fileExtname|fileDirname|cwd|pathSeparator|lineNumber|selectedText|env:(.*?)|config:(.*?))}/
        )
    ) {
        str = substituteVariables(str, recursive);
    }
    return str;
}
