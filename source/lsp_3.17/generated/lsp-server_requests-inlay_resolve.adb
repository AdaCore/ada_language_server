--
--  Copyright (C) <YEAR>, <COPYRIGHT HOLDER>
--
--  SPDX-License-Identifier: MIT
--
--  DON'T EDIT THIS FILE! It was generated from metaModel.json.
--

package body LSP.Server_Requests.Inlay_Resolve is

   overriding procedure Visit_Server_Receiver
     (Self  : Request;
      Value : in out LSP.Server_Request_Receivers.Server_Request_Receiver'
        Class) is
   begin
      Value.On_Inlay_Resolve_Request (Self.Id, Self.Params);
   end Visit_Server_Receiver;

end LSP.Server_Requests.Inlay_Resolve;
