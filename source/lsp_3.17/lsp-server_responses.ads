--
--  Copyright (C) 2022-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with LSP.Errors;
with LSP.Server_Message_Visitors;
with LSP.Server_Messages;
with LSP.Server_Response_Receivers;
with LSP.Structures;

package LSP.Server_Responses is
   pragma Preelaborate;

   type Server_Response is
     abstract limited new LSP.Server_Messages.Server_Message with
   record
      Id : LSP.Structures.Integer_Or_Virtual_String;
   end record;

   procedure Visit_Server_Receiver
     (Self  : Server_Response;
      Value : in out LSP.Server_Response_Receivers
        .Server_Response_Receiver'Class)
   is abstract;

   overriding procedure Visit_Server_Message_Visitor
     (Self    : Server_Response;
      Visitor : in out LSP.Server_Message_Visitors
                   .Server_Message_Visitor'Class);

   type Server_Error_Response is new Server_Response with record
      Error : LSP.Errors.ResponseError;
      --  The error object in case a request fails.
   end record;

   overriding procedure Visit_Server_Receiver
     (Self    : Server_Error_Response;
      Visitor : in out LSP.Server_Response_Receivers
        .Server_Response_Receiver'Class);

end LSP.Server_Responses;
