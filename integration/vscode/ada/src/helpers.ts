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
import assert from 'assert';
import { platform } from 'os';
import * as path from 'path';
import * as vscode from 'vscode';
import { ExecuteCommandRequest, LanguageClient } from 'vscode-languageclient/node';
import winston from 'winston';
import { adaExtState, logger } from './extension';

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

/**
 * Name of the `terminal.integrated.env.*` setting applicable to the current platform.
 */
export const TERMINAL_ENV_SETTING_NAME =
    'terminal.integrated.env.' +
    (platform() == 'darwin' ? 'osx' : platform() == 'win32' ? 'windows' : 'linux');

/**
 *
 * @returns the value of the applicable `terminal.integrated.env.*` setting,
 * without evaluation of macros such as `${env:...}`.
 */
export function getTerminalEnv() {
    const custom_env = vscode.workspace
        .getConfiguration()
        .get<{ [name: string]: string }>(TERMINAL_ENV_SETTING_NAME);

    return custom_env;
}

/**
 *
 * @returns the value of the applicable `terminal.integrated.env.*` setting,
 * after evaluation of macros such as `${env:...}`.
 */
export function getEvaluatedTerminalEnv() {
    const custom_env = getTerminalEnv();

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

/**
 * Read the environment variables specified in the vscode setting
 * `terminal.integrated.env.<os>` and set them in the given ProcessEnv object.
 *
 * The targetEnv can be `process.env` to apply the changes to the environment of
 * the running process.
 */
export function setTerminalEnvironment(targetEnv: NodeJS.ProcessEnv) {
    // Retrieve the user's custom environment variables if specified in their
    // settings/workspace
    const custom_env = getEvaluatedTerminalEnv();

    if (custom_env) {
        for (const var_name in custom_env) {
            const var_value: string = custom_env[var_name];
            targetEnv[var_name] = var_value;
        }
    }
}

export function assertSupportedEnvironments(mainChannel: winston.Logger) {
    // Get the ALS environment variable from the custom environment, or from the
    // process environment
    const customEnv = getEvaluatedTerminalEnv();
    const als = customEnv?.ALS ?? process.env.ALS;
    if (als) {
        // The User provided an external ALS executable. Do not perform any
        // platform support checks because we may be on an unsupported platform
        // where the User built and provided ALS.
        return;
    }

    type Env = {
        arch: 'arm' | 'arm64' | 'x64';
        platform: 'win32' | 'linux' | 'darwin';
    };
    const supportedEnvs: Env[] = [
        { arch: 'x64', platform: 'linux' },
        { arch: 'x64', platform: 'win32' },
        { arch: 'x64', platform: 'darwin' },
        { arch: 'arm64', platform: 'linux' },
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

    logger.debug(
        `Asserted compatibility with runtime environment: ${process.arch}, ${process.platform}`
    );
}

export function logErrorAndThrow(msg: string, logger: winston.Logger) {
    void vscode.window.showErrorMessage(msg);
    logger.error(msg);
    throw new Error(msg);
}

/*
    GPR extensions helper functions
*/

/**
 * Get the project file from the workspace configuration if available, or from
 * the ALS if not.
 *
 * @param client - the client to send the request to. If not provided, the main
 * Ada client of the extension is used.
 * @returns the full path of the currently loaded project file
 */
export async function getProjectFile(client?: LanguageClient): Promise<string> {
    if (!client) {
        client = adaExtState.adaClient;
    }
    const result: string = (await client.sendRequest(ExecuteCommandRequest.type, {
        command: 'als-project-file',
    })) as string;
    return result;
}

/**
 *
 * @returns The path of the project file loaded by the ALS relative to the workspace
 */
export async function getProjectFileRelPath(): Promise<string> {
    return vscode.workspace.asRelativePath(await getProjectFile());
}

/**
 * Get the Object Directory path
 * @param client - the client to send the request to
 * @returns a string path
 */
export async function getObjectDir(client: LanguageClient): Promise<string> {
    const result: string = (await client.sendRequest(ExecuteCommandRequest.type, {
        command: 'als-object-dir',
    })) as string;
    return result;
}

/**
 * Get the mains in the project
 * @param client - the client to send the request to
 * @returns a vector of string paths
 */
export async function getMains(client: LanguageClient): Promise<string[]> {
    const result: string[] = (await client.sendRequest(ExecuteCommandRequest.type, {
        command: 'als-mains',
    })) as string[];
    return result;
}

/**
 * Get the executables in the project
 * @param client - the client to send the request to
 * @returns a vector of string paths
 */
export async function getExecutables(client: LanguageClient): Promise<string[]> {
    const result: string[] = (await client.sendRequest(ExecuteCommandRequest.type, {
        command: 'als-executables',
    })) as string[];
    return result;
}
/**
 * @returns The list of Mains defined for the current project as an array of AdaMains.
 */
export async function getAdaMains(): Promise<AdaMain[]> {
    const mains = await getMains(adaExtState.adaClient);
    const execs = await getExecutables(adaExtState.adaClient);
    assert(
        execs.length == mains.length,
        `The ALS returned mains.length = ${mains.length} and ` +
            `execs.length = ${execs.length}` +
            `when they should be equal`
    );

    const result: AdaMain[] = [];
    for (let i = 0; i < mains.length; i++) {
        result.push(new AdaMain(mains[i], execs[i]));
    }

    return result;
}
/**
 * A class that represents an Ada main entry point. It encapsulate both the
 * source file path and the executable file path.
 */

export class AdaMain {
    mainFullPath: string;
    execFullPath: string;
    constructor(mainFullPath: string, execFullPath: string) {
        this.mainFullPath = mainFullPath;
        this.execFullPath = execFullPath;
    }

    /**
     * @returns path of the main source file relative to the workspace
     */
    mainRelPath(): string {
        return vscode.workspace.asRelativePath(this.mainFullPath);
    }

    /**
     * @returns path of the executable file relative to the workspace
     */
    execRelPath(): string {
        return vscode.workspace.asRelativePath(this.execFullPath);
    }
}

/**
 *
 * @returns true if the Node process was started with debug command line arguments
 */
export function startedInDebugMode() {
    const args = process.execArgv;
    if (args) {
        return args.some((arg) => /^--(debug(-brk)?|inspect-brk)=?/.test(arg));
    }
    return false;
}

/**
 * This constant is set to the string `.exe` on Windows, and to the empty string
 * otherwise. It is intended for computingk executable filenames conveniently by
 * simply appending the constant at the end of the name and obtaining a result
 * compatible with the running platform.
 */
export const exe: '.exe' | '' = process.platform == 'win32' ? '.exe' : '';
