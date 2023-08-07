--
--  Copyright (C) <YEAR>, <COPYRIGHT HOLDER>
--
--  SPDX-License-Identifier: MIT
--

package body LSP.Client_Responses.Hover is

   overriding procedure Visit_Client_Receiver
     (Self  : Response;
      Value : in out LSP.Client_Response_Receivers.Client_Response_Receiver'
        Class) is
   begin
      Value.On_Hover_Response (Self.Id, Self.Result);
   end Visit_Client_Receiver;

end LSP.Client_Responses.Hover;
