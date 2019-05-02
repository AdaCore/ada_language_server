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
--  This package provides set of handler for notifications and requests (with
--  direction from client to server). Each handler decodes corresponding
--  message from the stream and calls a method of Handler.
--
--  Requests are processed in synchronous way: for a request corresponding
--  response is returned by the handler.

private package LSP.Servers.Handlers is

   procedure DidChangeConfiguration
     (Stream  : access Ada.Streams.Root_Stream_Type'Class;
      Handler : not null
        LSP.Server_Notifications.Server_Notification_Handler_Access);

   procedure DidOpenTextDocument
     (Stream  : access Ada.Streams.Root_Stream_Type'Class;
      Handler : not null
        LSP.Server_Notifications.Server_Notification_Handler_Access);

   procedure DidCloseTextDocument
     (Stream  : access Ada.Streams.Root_Stream_Type'Class;
      Handler : not null
        LSP.Server_Notifications.Server_Notification_Handler_Access);

   procedure DidChangeTextDocument
     (Stream  : access Ada.Streams.Root_Stream_Type'Class;
      Handler : not null
        LSP.Server_Notifications.Server_Notification_Handler_Access);

   procedure DidSaveTextDocument
     (Stream  : access Ada.Streams.Root_Stream_Type'Class;
      Handler : not null
        LSP.Server_Notifications.Server_Notification_Handler_Access);

   procedure Ignore_Notification
     (Stream  : access Ada.Streams.Root_Stream_Type'Class;
      Handler : not null
       LSP.Server_Notifications.Server_Notification_Handler_Access);

   procedure Do_Exit
     (Stream  : access Ada.Streams.Root_Stream_Type'Class;
      Handler : not null
        LSP.Server_Notifications.Server_Notification_Handler_Access);

end LSP.Servers.Handlers;
