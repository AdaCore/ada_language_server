--
--  Copyright (C) 2022-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

package body LSP.Client_Responses.Errors is

   ---------------------------
   -- Visit_Client_Receiver --
   ---------------------------

   overriding procedure Visit_Client_Receiver
     (Self  : Response;
      Value : in out LSP.Client_Response_Receivers.Client_Response_Receiver'
        Class) is
   begin
      Value.On_Error_Response (Self.Id, Self.Error);
   end Visit_Client_Receiver;

end LSP.Client_Responses.Errors;
