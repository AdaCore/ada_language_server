--
--  Copyright (C) <YEAR>, <COPYRIGHT HOLDER>
--
--  SPDX-License-Identifier: MIT
--
--  DON'T EDIT THIS FILE! It was generated from metaModel.json.
--

package body LSP.Client_Requests.ShowMessageRequest is

   overriding procedure Visit_Client_Receiver
     (Self  : Request;
      Value : in out LSP.Client_Request_Receivers.Client_Request_Receiver'
        Class) is
   begin
      Value.On_ShowMessageRequest_Request (Self.Id, Self.Params);
   end Visit_Client_Receiver;

end LSP.Client_Requests.ShowMessageRequest;
