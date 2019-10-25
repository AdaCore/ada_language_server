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

with Ada.Containers.Doubly_Linked_Lists;

with GNATCOLL.VFS;    use GNATCOLL.VFS;
with GNATCOLL.Traces;

with LSP.Messages.Server_Requests;
with LSP.Messages.Server_Responses;
with LSP.Server_Request_Handlers;
with LSP.Server_Notification_Receivers;
with LSP.Servers;

with LSP.Ada_Contexts;

package LSP.Ada_Handlers is

   type Message_Handler
     (Server  : access LSP.Servers.Server;
      Trace   : GNATCOLL.Traces.Trace_Handle) is
   limited new LSP.Server_Request_Handlers.Server_Request_Handler
     and LSP.Server_Notification_Receivers.Server_Notification_Receiver
   with private;
   --  A handler of LSP notifications and requests from Ada language

   procedure Handle_Error (Self : access Message_Handler);
   --  This procedure will be called when an unexpected error is raised in the
   --  request processing loop.

private

   type Context_Access is access LSP.Ada_Contexts.Context;

   package Context_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Context_Access);

   type Message_Handler
     (Server  : access LSP.Servers.Server;
      Trace   : GNATCOLL.Traces.Trace_Handle)
   is limited new LSP.Server_Request_Handlers.Server_Request_Handler
     and LSP.Server_Notification_Receivers.Server_Notification_Receiver with
   record
      Contexts : Context_Lists.List;
      --  There is one context in this list per loaded project.
      --  Note: the last context in this list is a special case, not associated
      --  to a given project.
      --  There should always be at least one "project" context, and exactly
      --  one "projectless" context.

      Root : Virtual_File;
      --  The directory passed under rootURI/rootPath during the initialize
      --  request.

      Diagnostics_Enabled : Boolean := True;
      --  Whether to publish diagnostics

      Token_Id : Integer := 0;
      --  An ever-increasing number used to generate unique progress tokens
   end record;

   overriding function On_Initialize_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Initialize_Request)
      return LSP.Messages.Server_Responses.Initialize_Response;

   overriding function On_Shutdown_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Shutdown_Request)
      return LSP.Messages.Server_Responses.Shutdown_Response;

   overriding function On_CodeAction_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.CodeAction_Request)
      return LSP.Messages.Server_Responses.CodeAction_Response;

   overriding function On_Completion_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Completion_Request)
      return LSP.Messages.Server_Responses.Completion_Response;

   overriding function On_Definition_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Definition_Request)
      return LSP.Messages.Server_Responses.Location_Response;

   overriding function On_Type_Definition_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Type_Definition_Request)
      return LSP.Messages.Server_Responses.Location_Response;

   overriding function On_Highlight_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Highlight_Request)
      return LSP.Messages.Server_Responses.Highlight_Response;

   overriding function On_Hover_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Hover_Request)
      return LSP.Messages.Server_Responses.Hover_Response;

   overriding function On_References_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.References_Request)
      return LSP.Messages.Server_Responses.Location_Response;

   overriding function On_Signature_Help_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Signature_Help_Request)
      return LSP.Messages.Server_Responses.SignatureHelp_Response;

   overriding function On_Document_Symbols_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Document_Symbols_Request)
      return LSP.Messages.Server_Responses.Symbol_Response;

   overriding function On_Rename_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Rename_Request)
      return LSP.Messages.Server_Responses.Rename_Response;

   overriding function On_Execute_Command_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Execute_Command_Request)
      return LSP.Messages.Server_Responses.ExecuteCommand_Response;

   overriding function On_Workspace_Symbols_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Workspace_Symbols_Request)
      return LSP.Messages.Server_Responses.Symbol_Response;

   overriding function On_Workspace_Execute_Command_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Workspace_Execute_Command_Request)
      return LSP.Messages.Server_Responses.ExecuteCommand_Response;

   overriding function On_ALS_Called_By_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.ALS_Called_By_Request)
      return LSP.Messages.Server_Responses.ALS_Called_By_Response;

   overriding function On_ALS_Debug_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.ALS_Debug_Request)
      return LSP.Messages.Server_Responses.ALS_Debug_Response;

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

   overriding procedure On_Cancel_Notification
     (Self  : access Message_Handler;
      Value : LSP.Messages.CancelParams) is null;
   --  This is intentionally null procedure, because cancel is implemented by
   --  LSP server itself.

end LSP.Ada_Handlers;
