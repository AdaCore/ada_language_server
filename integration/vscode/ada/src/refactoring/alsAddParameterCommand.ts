/*----------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                       Copyright (C) 2021, AdaCore                        --
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

import { window, InputBoxOptions } from 'vscode';
import { LanguageClient } from 'vscode-languageclient/node';

import { AdaGrammarRule, AdaSyntaxCheckProvider } from '../alsProtocolExtensions';

export type AddParameterCommandArgs = {
    newParameter: string;
    requiresFullSpecification: boolean;
};

export const alsAddParameterCommandExecutor = async (
    client: LanguageClient,
    args: AddParameterCommandArgs
): Promise<boolean> => {
    // If the server command attributes changed, some of args fields might be undefined
    if (args.requiresFullSpecification === undefined || args.newParameter === undefined) {
        return Promise.reject(
            'Invalid als-refactor-add-parameters command: missing "requiresFullSpecification" field'
        );
    }

    // Create an input box with the messages adjusted according to if we require a full parameter
    // specification or not

    const prompt = args.requiresFullSpecification
        ? 'Insert a full parameter specification'
        : 'Insert one or more comma-separated parameter names or a full parameter specification';

    const rules = args.requiresFullSpecification
        ? [AdaGrammarRule.Param_Spec_Rule]
        : [
              AdaGrammarRule.Defining_Id_Rule,
              AdaGrammarRule.Defining_Id_List_Rule,
              AdaGrammarRule.Param_Spec_Rule,
          ];

    const diagnostic = args.requiresFullSpecification
        ? 'This is not a syntactically valid full parameter specification'
        : 'This is not a syntactically valid parameter name or full parameter specification';

    const adaSyntaxCheckProvider = new AdaSyntaxCheckProvider(client, rules, diagnostic);
    const { sendCheckSyntaxRequest } = adaSyntaxCheckProvider;

    const inputBoxOptions: InputBoxOptions = {
        title: 'Add Parameter(s)',
        prompt: prompt,
        ignoreFocusOut: true,
        validateInput: sendCheckSyntaxRequest,
    };

    const input = await window.showInputBox(inputBoxOptions);

    // If input is undefined, then the user cancelled the operation. Return false
    // to indicate that the executor should no proced. Otherwise, set the
    // newParameter.

    if (input !== undefined) {
        args.newParameter = input;
        return Promise.resolve(true);
    } else {
        return Promise.resolve(false);
    }
};
