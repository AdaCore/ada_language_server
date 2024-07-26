--
--  Copyright (C) 2022-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

package body LSP.Server_Responses.Errors is

   ---------------------------
   -- Visit_Server_Receiver --
   ---------------------------

   overriding procedure Visit_Server_Receiver
     (Self  : Response;
      Value : in out
        LSP.Server_Response_Receivers.Server_Response_Receiver'Class) is
   begin
      Value.On_Error_Response (Self.Id, Self.Error);
   end Visit_Server_Receiver;

end LSP.Server_Responses.Errors;
