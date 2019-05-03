------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2018-2019, AdaCore                     --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------
--
--  This package provides requests and notifications handler for Ada
--  language.

with LSP.Message_Handlers;
with LSP.Messages;
with LSP.Server_Notifications;
with LSP.Servers;

with LSP.Ada_Contexts;

package LSP.Ada_Handlers is

   type Message_Handler
     (Server  : access LSP.Servers.Server;
      Context : access LSP.Ada_Contexts.Context) is
   limited new LSP.Message_Handlers.Request_Handler
     and LSP.Server_Notifications.Server_Notification_Handler with private;
   --  A handler of LSP notifications and requests from Ada language

private

   type Message_Handler
     (Server : access LSP.Servers.Server;
      Context : access LSP.Ada_Contexts.Context)
   is limited new LSP.Message_Handlers.Request_Handler
     and LSP.Server_Notifications.Server_Notification_Handler with record
      null;
   end record;

   overriding function Handle_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.RequestMessage'Class)
      return LSP.Messages.ResponseMessage'Class;

   overriding procedure Handle_Notification
     (Self         : access Message_Handler;
      Notification : LSP.Messages.NotificationMessage'Class);

end LSP.Ada_Handlers;
