/*----------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                    Copyright (C) 2021-2022, AdaCore                      --
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

/**
 * Implementation of a Middleware.executeCommand that intercepts and executes commands
 */

import { ExecuteCommandSignature } from 'vscode-languageclient';
import { LanguageClient } from 'vscode-languageclient/node';
import {
    alsAddParameterCommandExecutor,
    AddParameterCommandArgs,
} from './refactoring/alsAddParameterCommand';

import {
    alsChangeParametersTypeCommandExecutor,
    ChangeParametersTypeCommandArgs,
} from './refactoring/alsChangeParametersTypeCommand';

import {
    alsChangeParametersDefaultValueCommandExecutor,
    ChangeParametersDefaultValueCommandArgs,
} from './refactoring/alsChangeParametersDefaultValueCommand';

/**
 * Type alias for a function that intercepts a command and executes it by return a promise that
 * resolve to an ExecuteCommandSignature if the command was executed successfully or undefined
 * otherwise.
 */
type CommandExecutor = (
    command: string,
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    args: any[],
    next: ExecuteCommandSignature
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
) => Promise<ExecuteCommandSignature | undefined>;

/**
 * Returns a CommandExecutor that will use client to intercept and execute commands
 *
 * @param client - An ALS LanguageClient
 * @returns A CommandExecutor that dispatches command to a specific executor
 */
export const alsCommandExecutor = (client: LanguageClient): CommandExecutor => {
    return async (
        command: string,
        // eslint-disable-next-line @typescript-eslint/no-explicit-any
        args: any[],
        next: ExecuteCommandSignature
    ): Promise<ExecuteCommandSignature | undefined> => {
        if (command === 'als-refactor-add-parameters') {
            const proceedWithExecution = await alsAddParameterCommandExecutor(
                client,
                args[0] as AddParameterCommandArgs
            );
            if (!proceedWithExecution) return Promise.resolve(undefined);
        } else if (command === 'als-refactor-change_parameters_type') {
            const proceedWithExecution = await alsChangeParametersTypeCommandExecutor(
                client,
                args[0] as ChangeParametersTypeCommandArgs
            );
            if (!proceedWithExecution) return Promise.resolve(undefined);
        } else if (command === 'als-refactor-change_parameters_default_value') {
            const proceedWithExecution = await alsChangeParametersDefaultValueCommandExecutor(
                client,
                args[0] as ChangeParametersDefaultValueCommandArgs
            );
            if (!proceedWithExecution) return Promise.resolve(undefined);
        }
        // eslint-disable-next-line @typescript-eslint/no-unsafe-return
        return next(command, args);
    };
};
