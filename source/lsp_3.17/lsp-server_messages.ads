--
--  Copyright (C) 2022-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with LSP.Server_Message_Visitors;

package LSP.Server_Messages is
   pragma Preelaborate;

   type Server_Message is abstract tagged limited null record;
   --  Base class of messages send by a client to a server

   function Assigned (Self : access Server_Message'Class) return Boolean
     is (Self /= null);
   --  Check if the argument is not null

   type Server_Message_Access is access all Server_Message'Class;

   procedure Visit_Server_Message_Visitor
     (Self  : Server_Message;
      Value : in out LSP.Server_Message_Visitors
                .Server_Message_Visitor'Class) is null;

end LSP.Server_Messages;
