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

import { LanguageClient } from 'vscode-languageclient/node';

export enum AdaGrammarRule {
    Defining_Id_Rule = 'Defining_Id_Rule',
    Defining_Id_List_Rule = 'Defining_Id_List_Rule',
    Param_Spec_Rule = 'Param_Spec_Rule',
}

type AdaSyntaxCheckRequest = {
    input: string;
    rules: string[];
};

type AdaSyntaxCheckResponse = {
    diagnostic?: string;
};

export class AdaSyntaxCheckProvider {
    private readonly client: LanguageClient;
    rules: string[];
    diagnostic?: string;

    constructor(client: LanguageClient, rules: AdaGrammarRule[], diagnotic?: string) {
        this.client = client;
        this.rules = rules.map((rule) => rule.toString());
        this.diagnostic = diagnotic;
    }

    public sendCheckSyntaxRequest = async (input: string): Promise<undefined | string> => {
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
