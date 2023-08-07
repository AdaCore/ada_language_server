--
--  Copyright (C) <YEAR>, <COPYRIGHT HOLDER>
--
--  SPDX-License-Identifier: MIT
--

with LSP.Structures;

package LSP.Server_Responses.RegisterCapability is
   pragma Preelaborate;

   type Response is new LSP.Server_Responses.Server_Response with record
      Result : LSP.Structures.Null_Record;
   end record;

   overriding procedure Visit_Server_Receiver
     (Self  : Response;
      Value : in out LSP.Server_Response_Receivers.Server_Response_Receiver'
        Class);

end LSP.Server_Responses.RegisterCapability;
