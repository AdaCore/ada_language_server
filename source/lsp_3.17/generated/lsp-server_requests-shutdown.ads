--
--  Copyright (C) <YEAR>, <COPYRIGHT HOLDER>
--
--  SPDX-License-Identifier: MIT
--
--  DON'T EDIT THIS FILE! It was generated from metaModel.json.
--

package LSP.Server_Requests.Shutdown is
   pragma Preelaborate;

   type Request is new LSP.Server_Requests.Server_Request with record
      null;
   end record;

   overriding procedure Visit_Server_Receiver
     (Self  : Request;
      Value : in out LSP.Server_Request_Receivers.Server_Request_Receiver'
        Class);

end LSP.Server_Requests.Shutdown;
