--
--  Copyright (C) 2022-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with LSP.Server_Message_Visitors;
with LSP.Server_Messages;
with LSP.Server_Request_Receivers;
with LSP.Structures;

with VSS.Strings;

package LSP.Server_Requests is
   pragma Preelaborate;

   type Server_Request is
     abstract limited new LSP.Server_Messages.Server_Message with
   record
      Id       : LSP.Structures.Integer_Or_Virtual_String;

      Canceled : Boolean := False
         with Atomic;
      --  The cancelation flag set by the input task and read by processing
      --  task
   end record;

   function Method
     (Self : Server_Request) return VSS.Strings.Virtual_String is abstract;
   --  Returns name of the method.

   procedure Visit_Server_Receiver
     (Self  : Server_Request;
      Value : in out
        LSP.Server_Request_Receivers.Server_Request_Receiver'Class) is abstract;

   overriding procedure Visit_Server_Message_Visitor
     (Self    : Server_Request;
      Visitor : in out LSP.Server_Message_Visitors
                   .Server_Message_Visitor'Class);

end LSP.Server_Requests;
