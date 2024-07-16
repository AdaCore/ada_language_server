--
--  Copyright (C) <YEAR>, <COPYRIGHT HOLDER>
--
--  SPDX-License-Identifier: MIT
--
--  DON'T EDIT THIS FILE! It was generated from metaModel.json.
--

package body LSP.Client_Responses.PrepareTypeHierarchy is

   overriding procedure Visit_Client_Receiver
     (Self  : Response;
      Value : in out LSP.Client_Response_Receivers.Client_Response_Receiver'
        Class) is
   begin
      Value.On_PrepareTypeHierarchy_Response (Self.Id, Self.Result);
   end Visit_Client_Receiver;

end LSP.Client_Responses.PrepareTypeHierarchy;
