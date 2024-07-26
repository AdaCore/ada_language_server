--
--  Copyright (C) <YEAR>, <COPYRIGHT HOLDER>
--
--  SPDX-License-Identifier: MIT
--
--  DON'T EDIT THIS FILE! It was generated from metaModel.json.
--

with LSP.Structures;

package LSP.Server_Requests.Symbol_Resolve is
   pragma Preelaborate;

   type Request is new LSP.Server_Requests.Server_Request with record
      Params : LSP.Structures.WorkspaceSymbol;
   end record;

   overriding procedure Visit_Server_Receiver
     (Self  : Request;
      Value : in out LSP.Server_Request_Receivers.Server_Request_Receiver'
        Class);

private

   overriding function Method
     (Self : Request) return VSS.Strings.Virtual_String is
     ("workspaceSymbol/resolve");

end LSP.Server_Requests.Symbol_Resolve;
