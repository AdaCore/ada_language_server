--
--  Copyright (C) <YEAR>, <COPYRIGHT HOLDER>
--
--  SPDX-License-Identifier: MIT
--
--  DON'T EDIT THIS FILE! It was generated from metaModel.json.
--

with LSP.Structures;

package LSP.Server_Responses.WorkspaceFolders is
   pragma Preelaborate;

   type Response is new LSP.Server_Responses.Server_Response with record
      Result : LSP.Structures.WorkspaceFolder_Vector_Or_Null;
   end record;

   overriding procedure Visit_Server_Receiver
     (Self  : Response;
      Value : in out LSP.Server_Response_Receivers.Server_Response_Receiver'
        Class);

end LSP.Server_Responses.WorkspaceFolders;
