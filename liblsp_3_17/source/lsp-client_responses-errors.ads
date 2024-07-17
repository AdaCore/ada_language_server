--
--  Copyright (C) 2022-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

package LSP.Client_Responses.Errors is
   pragma Preelaborate;

   type Response is new LSP.Client_Responses.Client_Response with record
      Error : LSP.Errors.ResponseError;
   end record;

   overriding procedure Visit_Client_Receiver
     (Self  : Response;
      Value : in out LSP.Client_Response_Receivers.Client_Response_Receiver'
        Class);

end LSP.Client_Responses.Errors;
