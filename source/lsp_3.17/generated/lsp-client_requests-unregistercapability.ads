--
--  Copyright (C) <YEAR>, <COPYRIGHT HOLDER>
--
--  SPDX-License-Identifier: MIT
--

with LSP.Structures;

package LSP.Client_Requests.UnregisterCapability is
   pragma Preelaborate;

   type Request is new LSP.Client_Requests.Client_Request with record
      Params : LSP.Structures.UnregistrationParams;
   end record;

   overriding procedure Visit_Client_Receiver
     (Self  : Request;
      Value : in out LSP.Client_Request_Receivers.Client_Request_Receiver'
        Class);

end LSP.Client_Requests.UnregisterCapability;
