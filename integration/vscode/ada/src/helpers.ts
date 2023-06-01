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
import { platform } from 'os';
import * as path from 'path';
import * as vscode from 'vscode';
import { LanguageClient } from 'vscode-languageclient/node';

/**
 * Substitue any variable reference present in the given string. VS Code
 * variable references are listed here:
 * https://code.visualstudio.com/docs/editor/variables-reference
 * @param str - string to perform substitution on
 * @param recursive - whether to perform substitution recursively on the result
 * of each substitution until there are no variables to substitute.
 *
 * @returns string after applying substitutions
 */
export function substituteVariables(str: string, recursive = false): string {
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
        // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
        return process.env[variable.match(/\${env:(.*?)}/)![1]] || '';
    });

    str = str.replace(/\${config:(.*?)}/g, function (variable) {
        // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
        return vscode.workspace.getConfiguration().get(variable.match(/\${config:(.*?)}/)![1], '');
    });

    if (
        recursive &&
        str.match(
            // eslint-disable-next-line max-len
            /\${(workspaceFolder|workspaceFolderBasename|fileWorkspaceFolder|relativeFile|fileBasename|fileBasenameNoExtension|fileExtname|fileDirname|cwd|pathSeparator|lineNumber|selectedText|env:(.*?)|config:(.*?))}/
        )
    ) {
        str = substituteVariables(str, recursive);
    }
    return str;
}

/*
    Environment setting helper functions
*/

export function getCustomEnv() {
    const user_platform = platform();
    let env_config_name = 'terminal.integrated.env';

    switch (user_platform) {
        case 'darwin':
            env_config_name += '.osx';
            break;
        case 'win32':
            env_config_name += '.windows';
            break;
        default:
            env_config_name += '.linux';
    }

    const custom_env = vscode.workspace
        .getConfiguration()
        .get<{ [name: string]: string }>(env_config_name);

    return custom_env;
}

export function getEvaluatedCustomEnv() {
    const custom_env = getCustomEnv();

    if (custom_env) {
        for (const var_name in custom_env) {
            // Substitute VS Code variable references that might be present
            // in the JSON settings configuration (e.g: "PATH": "${workspaceFolder}/obj")
            custom_env[var_name] = custom_env[var_name].replace(/(\$\{.*\})/, (substring) =>
                substituteVariables(substring, false)
            );
        }
    }

    return custom_env;
}

export function assertSupportedEnvironments(mainChannel: vscode.OutputChannel) {
    type Env = {
        arch: 'arm' | 'arm64' | 'x64';
        platform: 'win32' | 'linux' | 'darwin';
    };
    const supportedEnvs: Env[] = [
        { arch: 'x64', platform: 'linux' },
        { arch: 'x64', platform: 'win32' },
        { arch: 'x64', platform: 'darwin' },
        { arch: 'arm64', platform: 'darwin' },
    ];

    if (
        !supportedEnvs.some((val) => {
            return val.arch == process.arch && val.platform == process.platform;
        })
    ) {
        const msg =
            `The Ada extension is not supported on ` +
            `architecture '${process.arch}' and platform '${process.platform}'`;
        logErrorAndThrow(msg, mainChannel);
    }
}

export function logErrorAndThrow(msg: string, channel: vscode.OutputChannel) {
    void vscode.window.showErrorMessage(msg);
    channel.appendLine('[Error] ' + msg);
    throw new Error(msg);
}

/*
    GPR extensions helper functions
*/
type ProjectFileResponse = {
    projectFile: string;
};

type ObjDirResponse = {
    objectDir: string;
};

export async function getProjectFile(client: LanguageClient): Promise<string> {
    const config: string | undefined = vscode.workspace.getConfiguration('ada').get('projectFile');
    if (config != undefined && config != '') {
        return config;
    } else {
        const result: ProjectFileResponse = await client.sendRequest('$/glsProjectFile');
        return result.projectFile;
    }
}

export async function getObjectDir(client: LanguageClient): Promise<string> {
    const result: ObjDirResponse = await client.sendRequest('$/glsObjectDir');
    return result.objectDir;
}
