--
--  Copyright (C) <YEAR>, <COPYRIGHT HOLDER>
--
--  SPDX-License-Identifier: MIT
--

with LSP.Structures;

package LSP.Server_Requests.Implementation is
   pragma Preelaborate;

   type Request is new LSP.Server_Requests.Server_Request with record
      Params : LSP.Structures.ImplementationParams;
   end record;

   overriding procedure Visit_Server_Receiver
     (Self  : Request;
      Value : in out LSP.Server_Request_Receivers.Server_Request_Receiver'
        Class);

end LSP.Server_Requests.Implementation;
