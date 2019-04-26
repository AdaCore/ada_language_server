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

   overriding procedure Exit_Notification
     (Self  : access Message_Handler);

   overriding function Initialize_Request
     (Self  : access Message_Handler;
      Value : LSP.Messages.InitializeParams)
      return LSP.Messages.Initialize_Response;

   overriding function Shutdown_Request
     (Self  : access Message_Handler)
      return LSP.Messages.ResponseMessage;

   overriding function Text_Document_Code_Action_Request
     (Self  : access Message_Handler;
      Value : LSP.Messages.CodeActionParams)
      return LSP.Messages.CodeAction_Response;

   overriding function Text_Document_Definition_Request
     (Self  : access Message_Handler;
      Value : LSP.Messages.TextDocumentPositionParams)
      return LSP.Messages.Location_Response;

   overriding procedure Text_Document_Did_Change
     (Self  : access Message_Handler;
      Value : LSP.Messages.DidChangeTextDocumentParams);

   overriding procedure Text_Document_Did_Close
     (Self  : access Message_Handler;
      Value : LSP.Messages.DidCloseTextDocumentParams);

   overriding procedure Text_Document_Did_Open
     (Self  : access Message_Handler;
      Value : LSP.Messages.DidOpenTextDocumentParams);

   overriding function Text_Document_Highlight_Request
     (Self  : access Message_Handler;
      Value : LSP.Messages.TextDocumentPositionParams)
      return LSP.Messages.Highlight_Response;

   overriding function Text_Document_Hover_Request
     (Self  : access Message_Handler;
      Value : LSP.Messages.TextDocumentPositionParams)
      return LSP.Messages.Hover_Response;

   overriding function Text_Document_References_Request
     (Self  : access Message_Handler;
      Value : LSP.Messages.ReferenceParams)
      return LSP.Messages.Location_Response;

   overriding function Text_Document_Signature_Help_Request
     (Self  : access Message_Handler;
      Value : LSP.Messages.TextDocumentPositionParams)
      return LSP.Messages.SignatureHelp_Response;

   overriding function Text_Document_Symbol_Request
     (Self  : access Message_Handler;
      Value : LSP.Messages.DocumentSymbolParams)
     return LSP.Messages.Symbol_Response;

   overriding procedure Workspace_Did_Change_Configuration
     (Self  : access Message_Handler;
      Value : LSP.Messages.DidChangeConfigurationParams);

   overriding function Workspace_Execute_Command_Request
     (Self  : access Message_Handler;
      Value : LSP.Messages.ExecuteCommandParams)
      return LSP.Messages.ExecuteCommand_Response;

   overriding function Workspace_Symbol_Request
     (Self  : access Message_Handler;
      Value : LSP.Messages.WorkspaceSymbolParams)
      return LSP.Messages.Symbol_Response;

   overriding function Text_Document_Completion_Request
     (Self  : access Message_Handler;
      Value : LSP.Messages.TextDocumentPositionParams)
     return LSP.Messages.Completion_Response;

end LSP.Ada_Handlers;
