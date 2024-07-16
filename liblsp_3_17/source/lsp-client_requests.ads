--
--  Copyright (C) 2022-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with LSP.Client_Message_Visitors;
with LSP.Client_Messages;
with LSP.Client_Request_Receivers;
with LSP.Structures;

package LSP.Client_Requests is
   pragma Preelaborate;

   type Client_Request is
     abstract limited new LSP.Client_Messages.Client_Message with
   record
      Id : LSP.Structures.Integer_Or_Virtual_String;
   end record;

   procedure Visit_Client_Receiver
     (Self  : Client_Request;
      Value : in out LSP.Client_Request_Receivers
        .Client_Request_Receiver'Class)
   is abstract;

   overriding procedure Visit_Client_Message_Visitor
     (Self     : Client_Request;
      Visitor : in out LSP.Client_Message_Visitors
                   .Client_Message_Visitor'Class);

end LSP.Client_Requests;
