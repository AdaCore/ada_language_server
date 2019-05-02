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

package body LSP.Servers.Handlers is

   ----------------------------
   -- DidChangeConfiguration --
   ----------------------------

   procedure DidChangeConfiguration
     (Stream  : access Ada.Streams.Root_Stream_Type'Class;
      Handler : not null
        LSP.Server_Notifications.Server_Notification_Handler_Access)
   is
      Params : LSP.Messages.DidChangeConfigurationParams;
   begin
      LSP.Messages.DidChangeConfigurationParams'Read (Stream, Params);

      Handler.Workspace_Did_Change_Configuration (Params);
   end DidChangeConfiguration;

   ---------------------------
   -- DidChangeTextDocument --
   ---------------------------

   procedure DidChangeTextDocument
     (Stream  : access Ada.Streams.Root_Stream_Type'Class;
      Handler : not null
        LSP.Server_Notifications.Server_Notification_Handler_Access)
   is
      Params : LSP.Messages.DidChangeTextDocumentParams;
   begin
      LSP.Messages.DidChangeTextDocumentParams'Read (Stream, Params);

      Handler.Text_Document_Did_Change (Params);
   end DidChangeTextDocument;

   --------------------------
   -- DidCloseTextDocument --
   --------------------------

   procedure DidCloseTextDocument
     (Stream  : access Ada.Streams.Root_Stream_Type'Class;
      Handler : not null
        LSP.Server_Notifications.Server_Notification_Handler_Access)
   is
      Params : LSP.Messages.DidCloseTextDocumentParams;
   begin
      LSP.Messages.DidCloseTextDocumentParams'Read (Stream, Params);

      Handler.Text_Document_Did_Close (Params);
   end DidCloseTextDocument;

   -------------------------
   -- DidOpenTextDocument --
   -------------------------

   procedure DidOpenTextDocument
     (Stream  : access Ada.Streams.Root_Stream_Type'Class;
      Handler : not null
        LSP.Server_Notifications.Server_Notification_Handler_Access)
   is
      Params : LSP.Messages.DidOpenTextDocumentParams;
   begin
      LSP.Messages.DidOpenTextDocumentParams'Read (Stream, Params);

      Handler.Text_Document_Did_Open (Params);
   end DidOpenTextDocument;

   -------------------------
   -- DidSaveTextDocument --
   -------------------------

   procedure DidSaveTextDocument
     (Stream  : access Ada.Streams.Root_Stream_Type'Class;
      Handler : not null
        LSP.Server_Notifications.Server_Notification_Handler_Access)
   is
      Params : LSP.Messages.DidSaveTextDocumentParams;
   begin
      LSP.Messages.DidSaveTextDocumentParams'Read (Stream, Params);

      Handler.Text_Document_Did_Save (Params);
   end DidSaveTextDocument;

   -------------------------
   -- Ignore_Notification --
   -------------------------

   procedure Ignore_Notification
     (Stream  : access Ada.Streams.Root_Stream_Type'Class;
      Handler : not null
        LSP.Server_Notifications.Server_Notification_Handler_Access) is
   begin
      null;
   end Ignore_Notification;

   -------------
   -- Do_Exit --
   -------------

   procedure Do_Exit
     (Stream  : access Ada.Streams.Root_Stream_Type'Class;
      Handler : not null
        LSP.Server_Notifications.Server_Notification_Handler_Access)
   is
      pragma Unreferenced (Stream);
   begin
      Handler.Exit_Notification;
   end Do_Exit;

end LSP.Servers.Handlers;
