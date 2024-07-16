--
--  Copyright (C) 2022-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with LSP.Client_Message_Visitors;
with LSP.Client_Messages;
with LSP.Client_Response_Receivers;
with LSP.Errors;
with LSP.Structures;

package LSP.Client_Responses is
   pragma Preelaborate;

   type Client_Response is
     abstract limited new LSP.Client_Messages.Client_Message with
   record
      Id : LSP.Structures.Integer_Or_Virtual_String;
   end record;

   procedure Visit_Client_Receiver
     (Self  : Client_Response;
      Value : in out LSP.Client_Response_Receivers
        .Client_Response_Receiver'Class)
   is abstract;

   overriding procedure Visit_Client_Message_Visitor
     (Self     : Client_Response;
      Visitor : in out LSP.Client_Message_Visitors
                   .Client_Message_Visitor'Class);

   type Client_Error_Response is new Client_Response with record
      Error : LSP.Errors.ResponseError;
      --  The error object in case a request fails.
   end record;

   overriding procedure Visit_Client_Receiver
     (Self    : Client_Error_Response;
      Visitor : in out LSP.Client_Response_Receivers
        .Client_Response_Receiver'Class);

end LSP.Client_Responses;
