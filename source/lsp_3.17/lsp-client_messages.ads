--
--  Copyright (C) 2022-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with LSP.Client_Message_Visitors;

package LSP.Client_Messages is
   pragma Preelaborate;

   type Client_Message is abstract tagged limited null record;
   --  Base class of messages send by a server to a client

   type Client_Message_Access is access all Client_Message'Class;

   procedure Visit_Client_Message_Visitor
     (Self     : Client_Message;
      Visitor : in out LSP.Client_Message_Visitors
                   .Client_Message_Visitor'Class) is null;

end LSP.Client_Messages;
