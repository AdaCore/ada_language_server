--
--  Copyright (C) <YEAR>, <COPYRIGHT HOLDER>
--
--  SPDX-License-Identifier: MIT
--

package body LSP.Server_Responses.Inline_Refresh is

   overriding procedure Visit_Server_Receiver
     (Self  : Response;
      Value : in out LSP.Server_Response_Receivers.Server_Response_Receiver'
        Class) is
   begin
      Value.On_Inline_Refresh_Response (Self.Id, Self.Result);
   end Visit_Server_Receiver;

end LSP.Server_Responses.Inline_Refresh;
