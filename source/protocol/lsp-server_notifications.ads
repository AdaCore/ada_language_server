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

with LSP.Messages;

package LSP.Server_Notifications is

   type Server_Notification_Handler is limited interface;
   type Server_Notification_Handler_Access is
     access all Server_Notification_Handler'Class;

   procedure Initialized (Self : access Server_Notification_Handler) is null;

   procedure Workspace_Did_Change_Configuration
    (Self     : access Server_Notification_Handler;
     Value    : LSP.Messages.DidChangeConfigurationParams) is null;

   procedure Text_Document_Did_Open
     (Self  : access Server_Notification_Handler;
      Value : LSP.Messages.DidOpenTextDocumentParams) is null;

   procedure Text_Document_Did_Change
     (Self  : access Server_Notification_Handler;
      Value : LSP.Messages.DidChangeTextDocumentParams) is null;

   procedure Text_Document_Did_Save
     (Self  : access Server_Notification_Handler;
      Value : LSP.Messages.DidSaveTextDocumentParams) is null;

   procedure Text_Document_Did_Close
     (Self  : access Server_Notification_Handler;
      Value : LSP.Messages.DidCloseTextDocumentParams) is null;

   procedure Exit_Notification
    (Self : access Server_Notification_Handler) is null;

end LSP.Server_Notifications;
