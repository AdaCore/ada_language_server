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
 * 'als-refactor-change_parameters_type' command.
 */

import { window, InputBoxOptions } from 'vscode';
import { LanguageClient } from 'vscode-languageclient/node';

import { AdaGrammarRule, AdaSyntaxCheckProvider } from '../alsProtocolExtensions';

/* ALS will send a 'als-refactor-change_parameters_type' command with at least two arguments:
 * 'newParametersType' and 'syntaxRules'.
 * 'newParametersType' will be filled by alsChangeParametersTypeCommandExecutor and
 * 'syntaxRules' is used to determined the syntax rules used to check if the input is valid.
 */
export type ChangeParametersTypeCommandArgs = {
    newParametersType: string;
    syntaxRules: AdaGrammarRule[];
};

/**
 * Executes the 'als-refactor-change_parameters_type' command by manipulating
 * args.newParametersType with the user input. The user input is also syntactically checked,
 * by sending a '$/alsCheckSyntax' request to ALS. This request requires a set of rules, which
 * depend on args.syntaxRules
 *
 * @param client - The language server client needed to interact with ALS
 * @param args - Arguments of the 'als-refactor-change_parameters_type' command
 * @returns A Promise<boolean> that resolves to true if the command was executed successfully and
 * false otherwise
 */
export const alsChangeParametersTypeCommandExecutor = async (
    client: LanguageClient,
    args: ChangeParametersTypeCommandArgs
): Promise<boolean> => {
    // If the server command attributes changed, some of args fields might be undefined
    if (args.newParametersType === undefined || args.syntaxRules === undefined) {
        return Promise.reject('Invalid als-refactor-change_parameters_type');
    }

    const prompt = 'Insert the new parameter type';

    const diagnostic = 'Invalid parameter type';

    const adaSyntaxCheckProvider = new AdaSyntaxCheckProvider(client, args.syntaxRules, diagnostic);
    const { sendCheckSyntaxRequest } = adaSyntaxCheckProvider;

    const inputBoxOptions: InputBoxOptions = {
        title: 'Change Parameter(s) Type',
        prompt: prompt,
        ignoreFocusOut: true,
        validateInput: sendCheckSyntaxRequest,
    };

    const input = await window.showInputBox(inputBoxOptions);

    // If input is undefined, then the user cancelled the operation. Return false to indicate
    // that the executor should no proced. Otherwise, set args.newParametersType

    if (input !== undefined) {
        args.newParametersType = input;
        return Promise.resolve(true);
    } else {
        return Promise.resolve(false);
    }
};
