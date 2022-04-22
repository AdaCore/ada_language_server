/*----------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                       Copyright (C) 2022, AdaCore                        --
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
 * This file contains all the constructs necessary to execute the
 * 'als-refactor-change_parameters_default_value' command.
 */

import { window, InputBoxOptions } from 'vscode';
import { LanguageClient } from 'vscode-languageclient/node';

import { AdaGrammarRule, AdaSyntaxCheckProvider } from '../alsProtocolExtensions';

/* ALS will send a 'als-refactor-change_parameters_default_value' command with at least the
 * argument 'newParametersDefaultValue'.
 * 'newParametersDefaultValue' will be filled by alsChangeParametersDefaultValueCommandExecutor.
 */
export type ChangeParametersDefaultValueCommandArgs = {
    newParametersDefaultValue: string;
};

/**
 * Executes the 'als-refactor-change_parameters_default_value' command by manipulating
 * args.newParametersDefaultValue with the user input. The user input is also syntactically checked,
 * by sending a '$/alsCheckSyntax' request to ALS with the AdaGrammerRule.Expr_Rule.
 *
 * @param client - The language server client needed to interact with ALS
 * @param args - Arguments of the 'als-refactor-change_parameters_default_value' command
 * @returns A Promise<boolean> that resolves to true if the command was executed successfully and
 * false otherwise
 */
export const alsChangeParametersDefaultValueCommandExecutor = async (
    client: LanguageClient,
    args: ChangeParametersDefaultValueCommandArgs
): Promise<boolean> => {
    // If the server command attributes changed, some of args fields might be undefined
    if (args.newParametersDefaultValue === undefined) {
        return Promise.reject('Invalid als-refactor-change_parameters_default_value');
    }

    // Create an input box with the messages adjusted according to if we require a full parameter
    // specification or not

    const prompt = 'Insert the new parameter default value';

    const rules = [AdaGrammarRule.Expr_Rule];

    const diagnostic = 'This is not a syntactically valid expression';

    const adaSyntaxCheckProvider = new AdaSyntaxCheckProvider(client, rules, diagnostic, true);
    const { sendCheckSyntaxRequest } = adaSyntaxCheckProvider;

    const inputBoxOptions: InputBoxOptions = {
        title: 'Change parameter(s) default value',
        prompt: prompt,
        ignoreFocusOut: true,
        validateInput: sendCheckSyntaxRequest,
    };

    const input = await window.showInputBox(inputBoxOptions);

    // If input is undefined, then the user cancelled the operation. Return false
    // to indicate that the executor should no proced. Otherwise, set the
    // newParameter.

    if (input !== undefined) {
        args.newParametersDefaultValue = input;
        return Promise.resolve(true);
    } else {
        return Promise.resolve(false);
    }
};
