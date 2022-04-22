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
 * Implementation of a LanguageClient feature for Ada
 */

import {
    ClientCapabilities,
    DocumentSelector,
    InitializeParams,
    ServerCapabilities,
    StaticFeature,
} from 'vscode-languageclient/node';

/**
 * Class that determines that the client has a feature to provide user input
 */

export class ALSClientFeatures implements StaticFeature {
    /**
     * Unused since there is no need to manipulate params
     */
    fillInitializeParams?: ((params: InitializeParams) => void) | undefined = (
        // eslint-disable-next-line @typescript-eslint/no-unused-vars
        _params: InitializeParams
    ) => {
        // eslint-disable-next-line @typescript-eslint/no-empty-function
    };

    /**
     * Extend capabilities.experimental with an userInputProvider boolean that determines
     * that this client can provide user inputs
     */
    fillClientCapabilities(capabilities: ClientCapabilities): void {
        if (capabilities.experimental === undefined) {
            capabilities.experimental = {
                advanced_refactorings: [
                    'add_parameter',
                    'change_parameters_type',
                    'change_parameters_default_value',
                ],
            };
        } else {
            (
                capabilities.experimental as { advanced_refactorings: string[] }
            ).advanced_refactorings = [
                'add_parameter',
                'change_parameters_type',
                'change_parameters_default_value',
            ];
        }
    }

    /**
     * Unused since there are no necessary actions when initializing an object of this class
     */
    initialize(
        // eslint-disable-next-line max-len
        // eslint-disable-next-line @typescript-eslint/no-unused-vars, @typescript-eslint/no-explicit-any
        _capabilities: ServerCapabilities<any>,
        // eslint-disable-next-line @typescript-eslint/no-unused-vars
        _documentSelector: DocumentSelector | undefined
    ): void {
        // eslint-disable-next-line @typescript-eslint/no-empty-function
    }

    /**
     * Unused since there are no necessary actions when disposing an object of this class
     */
    dispose(): void {
        // eslint-disable-next-line @typescript-eslint/no-empty-function
    }
}
