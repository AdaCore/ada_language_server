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

with LSP.Messages;
with LSP.Messages.Requests;
with LSP.Messages.Notifications;
with LSP.Servers;

with LSP.Ada_Contexts;

package LSP.Ada_Handlers is

   type Message_Handler
     (Server  : access LSP.Servers.Server;
      Context : access LSP.Ada_Contexts.Context) is
   limited new LSP.Messages.Requests.Server_Request_Handler
     and LSP.Messages.Notifications.Server_Notification_Handler with private;
   --  A handler of LSP notifications and requests from Ada language

private

   type Message_Handler
     (Server : access LSP.Servers.Server;
      Context : access LSP.Ada_Contexts.Context)
   is limited new LSP.Messages.Requests.Server_Request_Handler
     and LSP.Messages.Notifications.Server_Notification_Handler with
   record
      null;
   end record;

   overriding function On_Initialize_Request
     (Self  : access Message_Handler;
      Value : LSP.Messages.InitializeParams)
      return LSP.Messages.Initialize_Response;

   overriding function On_Shutdown_Request
     (Self  : access Message_Handler)
      return LSP.Messages.ResponseMessage;

   overriding function On_CodeAction_Request
     (Self  : access Message_Handler;
      Value : LSP.Messages.CodeActionParams)
      return LSP.Messages.CodeAction_Response;

   overriding function On_Completion_Request
     (Self  : access Message_Handler;
      Value : LSP.Messages.TextDocumentPositionParams)
      return LSP.Messages.Completion_Response;

   overriding function On_Definition_Request
     (Self  : access Message_Handler;
      Value : LSP.Messages.TextDocumentPositionParams)
      return LSP.Messages.Location_Response;

   overriding function On_Type_Definition_Request
     (Self  : access Message_Handler;
      Value : LSP.Messages.TextDocumentPositionParams)
      return LSP.Messages.Location_Response;

   overriding function On_Highlight_Request
     (Self  : access Message_Handler;
      Value : LSP.Messages.TextDocumentPositionParams)
      return LSP.Messages.Highlight_Response;

   overriding function On_Hover_Request
     (Self  : access Message_Handler;
      Value : LSP.Messages.TextDocumentPositionParams)
      return LSP.Messages.Hover_Response;

   overriding function On_References_Request
     (Self  : access Message_Handler;
      Value : LSP.Messages.ReferenceParams)
      return LSP.Messages.Location_Response;

   overriding function On_Signature_Help_Request
     (Self  : access Message_Handler;
      Value : LSP.Messages.TextDocumentPositionParams)
      return LSP.Messages.SignatureHelp_Response;

   overriding function On_Document_Symbols_Request
     (Self  : access Message_Handler;
      Value : LSP.Messages.DocumentSymbolParams)
      return LSP.Messages.Symbol_Response;

   overriding function On_Rename_Request
     (Self  : access Message_Handler;
      Value : LSP.Messages.RenameParams)
      return LSP.Messages.Rename_Response;

   overriding function On_Execute_Command_Request
     (Self  : access Message_Handler;
      Value : LSP.Messages.ExecuteCommandParams)
      return LSP.Messages.ExecuteCommand_Response;

   overriding function On_Workspace_Symbols_Request
     (Self  : access Message_Handler;
      Value : LSP.Messages.WorkspaceSymbolParams)
      return LSP.Messages.Symbol_Response;

   overriding function On_Workspace_Execute_Command_Request
     (Self  : access Message_Handler;
      Value : LSP.Messages.ExecuteCommandParams)
      return LSP.Messages.ExecuteCommand_Response;

   overriding function On_ALS_Called_By_Request
     (Self  : access Message_Handler;
      Value : LSP.Messages.TextDocumentPositionParams)
      return LSP.Messages.ALS_Called_By_Response;

   overriding procedure On_Initialized_Notification
     (Self  : access Message_Handler) is null;

   overriding procedure On_Exit_Notification
     (Self  : access Message_Handler);

   overriding procedure On_DidChangeTextDocument_Notification
     (Self  : access Message_Handler;
      Value : LSP.Messages.DidChangeTextDocumentParams);

   overriding procedure On_DidCloseTextDocument_Notification
     (Self  : access Message_Handler;
      Value : LSP.Messages.DidCloseTextDocumentParams);

   overriding procedure On_DidOpenTextDocument_Notification
     (Self  : access Message_Handler;
      Value : LSP.Messages.DidOpenTextDocumentParams);

   overriding procedure On_DidSaveTextDocument_Notification
     (Self  : access Message_Handler;
      Value : LSP.Messages.DidSaveTextDocumentParams) is null;

   overriding procedure On_DidChangeConfiguration_Notification
     (Self  : access Message_Handler;
      Value : LSP.Messages.DidChangeConfigurationParams);

   overriding procedure Handle_Error
     (Self : access Message_Handler);
end LSP.Ada_Handlers;
