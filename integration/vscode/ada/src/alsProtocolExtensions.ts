/*----------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2021-2022, AdaCore                     --
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
 * Implementation of AdaSyntaxCheckProvider, an extension to the LSP that allows the client to send
 * a request to the server, that checks the validity of some input according to a sert of rules
 */

import { LanguageClient } from 'vscode-languageclient/node';

/**
 * Enum with the available grammar rules for Ada. These must match the Ada_Node_Kind_Type literals
 * defined in the Libadalang.Common package, since ALS is expected to parse the
 * AdaSyntaxCheckRequest.rules as an array of Ada_Node_Kind_Types literals.
 *
 * This set is imcomplete and will rules can be added as needed.
 */
export enum AdaGrammarRule {
    Defining_Id_Rule = 'Defining_Id_Rule',
    Defining_Id_List_Rule = 'Defining_Id_List_Rule',
    Param_Spec_Rule = 'Param_Spec_Rule',
    Anonymous_Type_Rule = 'Anonymous_Type_Rule',
    Subtype_Indication_Rule = 'Subtype_Indication_Rule',
    Expr_Rule = 'Expr_Rule',
}

/**
 * Structure with the data needed for the request
 *
 * @typeParam input - Input provided by the user
 * @typeParam rules - Array of rules that input will be checked against to
 */
type AdaSyntaxCheckRequest = {
    input: string;
    rules: string[];
};

/**
 * Structure with data responsded by the server
 *
 * @typeParam diagnostic - A diagnostic message in case the systax was not valid. undefined if the
 * syntax was valid
 */
type AdaSyntaxCheckResponse = {
    diagnostic?: string;
};

/**
 * Class that implements an a syntax check request as an extension to the LSP
 */
export class AdaSyntaxCheckProvider {
    private readonly client: LanguageClient;
    rules: string[];
    diagnostic?: string;
    canBeEmpty: boolean;

    /**
     * AdaSyntaxCheckProvider constructor
     *
     * @param client - An ALS LanguageClient
     * @param rules - Set of rules used to check an input against
     * @param diagnotic - And optional diagnostic message that overwrites the one sent by the server
     * @param canBeEmpty - Flag to allow empty inputs
     */
    constructor(
        client: LanguageClient,
        rules: AdaGrammarRule[],
        diagnotic?: string,
        canBeEmpty = false
    ) {
        this.client = client;
        this.rules = rules.map((rule) => rule.toString());
        this.diagnostic = diagnotic;
        this.canBeEmpty = canBeEmpty;
    }

    /**
     * Method that uses this.client to send a request to the user with
     *
     * @param input - Input provided by the user
     * @returns A promise that resolves to undefined if the input is syntactically correct, or a
     * string with a human-readable diagnostic message in case it is not.
     */
    public sendCheckSyntaxRequest = async (input: string): Promise<undefined | string> => {
        // Resolve immediately for empty inputs
        if (input === '') {
            if (this.canBeEmpty) {
                return new Promise<undefined | string>((resolve) => {
                    resolve(undefined);
                });
            } else {
                return new Promise<undefined | string>((reject) => {
                    reject('Input cannot be empty');
                });
            }
        }

        const method = '$/alsCheckSyntax';

        const request: AdaSyntaxCheckRequest = {
            input: input,
            rules: this.rules,
        };

        const response: AdaSyntaxCheckResponse = await this.client.sendRequest(method, request);

        return new Promise<undefined | string>((resolve, reject) => {
            if (Object.keys(response).length === 0) {
                resolve(undefined);
            } else if (Object.keys(response).length > 1) {
                reject('Invalid response from $/alsCheckSyntax request');
            } else {
                if (typeof response.diagnostic !== 'string') {
                    reject('Invalid response from $/alsCheckSyntax request');
                }
                resolve(this.diagnostic ? this.diagnostic : response.diagnostic);
            }
        });
    };
}
