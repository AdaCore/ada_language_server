--
--  Copyright (C) <YEAR>, <COPYRIGHT HOLDER>
--
--  SPDX-License-Identifier: MIT
--
--  DON'T EDIT THIS FILE! It was generated from metaModel.json.
--

package LSP.Client_Requests.Inlay_Refresh is
   pragma Preelaborate;

   type Request is new LSP.Client_Requests.Client_Request with record
      null;
   end record;

   overriding procedure Visit_Client_Receiver
     (Self  : Request;
      Value : in out LSP.Client_Request_Receivers.Client_Request_Receiver'
        Class);

end LSP.Client_Requests.Inlay_Refresh;
