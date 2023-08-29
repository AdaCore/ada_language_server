--
--  Copyright (C) <YEAR>, <COPYRIGHT HOLDER>
--
--  SPDX-License-Identifier: MIT
--
--  DON'T EDIT THIS FILE! It was generated from metaModel.json.
--

with LSP.Structures;

package LSP.Client_Responses.Link_Resolve is
   pragma Preelaborate;

   type Response is new LSP.Client_Responses.Client_Response with record
      Result : LSP.Structures.DocumentLink;
   end record;

   overriding procedure Visit_Client_Receiver
     (Self  : Response;
      Value : in out LSP.Client_Response_Receivers.Client_Response_Receiver'
        Class);

end LSP.Client_Responses.Link_Resolve;
