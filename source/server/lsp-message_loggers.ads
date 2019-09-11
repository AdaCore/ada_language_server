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
--  This package provides a class to log a message into GNATCOLL trace.
--

with GNATCOLL.Traces;

with LSP.Client_Notification_Receivers;
with LSP.Client_Request_Receivers;
with LSP.Client_Response_Senders;
with LSP.Messages.Client_Notifications;
with LSP.Messages.Client_Requests;
with LSP.Messages.Client_Responses;
with LSP.Messages.Server_Notifications;
with LSP.Messages.Server_Requests;
with LSP.Messages.Server_Responses;
with LSP.Messages.Visitors;
with LSP.Server_Notification_Receivers;
with LSP.Server_Request_Receivers;
with LSP.Server_Response_Senders;

package LSP.Message_Loggers is

   type Message_Logger is limited
     new LSP.Messages.Visitors.Visitor
     and LSP.Server_Notification_Receivers.Server_Notification_Receiver
     and LSP.Server_Request_Receivers.Server_Request_Receiver
     and LSP.Server_Response_Senders.Server_Response_Sender
     and LSP.Client_Notification_Receivers.Client_Notification_Receiver
     and LSP.Client_Request_Receivers.Client_Request_Receiver
     and LSP.Client_Response_Senders.Client_Response_Sender
   with private;

   procedure Initialize
     (Self  : in out Message_Logger;
      Trace : not null GNATCOLL.Traces.Trace_Handle);

private

   type Message_Logger is limited
     new LSP.Messages.Visitors.Visitor
     and LSP.Server_Notification_Receivers.Server_Notification_Receiver
     and LSP.Server_Request_Receivers.Server_Request_Receiver
     and LSP.Server_Response_Senders.Server_Response_Sender
     and LSP.Client_Notification_Receivers.Client_Notification_Receiver
     and LSP.Client_Request_Receivers.Client_Request_Receiver
     and LSP.Client_Response_Senders.Client_Response_Sender
   with record
      Trace : GNATCOLL.Traces.Trace_Handle;
   end record;

   overriding procedure On_Server_Notification
     (Self   : access Message_Logger;
      Value  : LSP.Messages.Server_Notifications.Server_Notification'Class);

   overriding procedure On_Server_Request
     (Self   : access Message_Logger;
      Value  : LSP.Messages.Server_Requests.Server_Request'Class);

   overriding procedure On_Server_Response
     (Self   : access Message_Logger;
      Value  : LSP.Messages.Server_Responses.Server_Response'Class);

   overriding procedure On_Client_Notification
     (Self   : access Message_Logger;
      Value  : LSP.Messages.Client_Notifications.Client_Notification'Class);

   overriding procedure On_Client_Request
     (Self   : access Message_Logger;
      Value  : LSP.Messages.Client_Requests.Client_Request'Class);

   overriding procedure On_Client_Response
     (Self   : access Message_Logger;
      Value  : LSP.Messages.Client_Responses.Client_Response'Class);

   overriding procedure On_Initialized_Notification
     (Self   : access Message_Logger);

   overriding procedure On_Exit_Notification
     (Self   : access Message_Logger);

   overriding procedure On_DidChangeConfiguration_Notification
     (Self   : access Message_Logger;
      Value  : LSP.Messages.DidChangeConfigurationParams);

   overriding procedure On_DidOpenTextDocument_Notification
     (Self   : access Message_Logger;
      Value  : LSP.Messages.DidOpenTextDocumentParams);

   overriding procedure On_DidChangeTextDocument_Notification
     (Self   : access Message_Logger;
      Value  : LSP.Messages.DidChangeTextDocumentParams);

   overriding procedure On_DidSaveTextDocument_Notification
     (Self   : access Message_Logger;
      Value  : LSP.Messages.DidSaveTextDocumentParams);

   overriding procedure On_DidCloseTextDocument_Notification
     (Self   : access Message_Logger;
      Value  : LSP.Messages.DidCloseTextDocumentParams);

   overriding procedure On_Cancel_Notification
     (Self  : access Message_Logger;
      Value : LSP.Messages.CancelParams);

   overriding procedure On_Initialize_Request
     (Self   : access Message_Logger;
      Value  : LSP.Messages.Server_Requests.Initialize_Request);

   overriding procedure On_Shutdown_Request
     (Self   : access Message_Logger;
      Value  : LSP.Messages.Server_Requests.Shutdown_Request);

   overriding procedure On_CodeAction_Request
     (Self   : access Message_Logger;
      Value  : LSP.Messages.Server_Requests.CodeAction_Request);

   overriding procedure On_Completion_Request
     (Self   : access Message_Logger;
      Value  : LSP.Messages.Server_Requests.Completion_Request);

   overriding procedure On_Definition_Request
     (Self   : access Message_Logger;
      Value  : LSP.Messages.Server_Requests.Definition_Request);

   overriding procedure On_Type_Definition_Request
     (Self   : access Message_Logger;
      Value  : LSP.Messages.Server_Requests.Type_Definition_Request);

   overriding procedure On_Highlight_Request
     (Self   : access Message_Logger;
      Value  : LSP.Messages.Server_Requests.Highlight_Request);

   overriding procedure On_Hover_Request
     (Self   : access Message_Logger;
      Value  : LSP.Messages.Server_Requests.Hover_Request);

   overriding procedure On_References_Request
     (Self   : access Message_Logger;
      Value  : LSP.Messages.Server_Requests.References_Request);

   overriding procedure On_Signature_Help_Request
     (Self   : access Message_Logger;
      Value  : LSP.Messages.Server_Requests.Signature_Help_Request);

   overriding procedure On_Document_Symbols_Request
     (Self   : access Message_Logger;
      Value  : LSP.Messages.Server_Requests.Document_Symbols_Request);

   overriding procedure On_Rename_Request
     (Self   : access Message_Logger;
      Value  : LSP.Messages.Server_Requests.Rename_Request);

   overriding procedure On_Execute_Command_Request
     (Self   : access Message_Logger;
      Value  : LSP.Messages.Server_Requests.Execute_Command_Request);

   overriding procedure On_Workspace_Symbols_Request
     (Self   : access Message_Logger;
      Value  : LSP.Messages.Server_Requests.Workspace_Symbols_Request);

   overriding procedure On_Workspace_Execute_Command_Request
     (Self   : access Message_Logger;
      Value  : LSP.Messages.Server_Requests.Workspace_Execute_Command_Request);

   overriding procedure On_ALS_Called_By_Request
     (Self   : access Message_Logger;
      Value  : LSP.Messages.Server_Requests.ALS_Called_By_Request);

   overriding procedure On_Initialize_Response
     (Self   : in out Message_Logger;
      Value  : LSP.Messages.Server_Responses.Initialize_Response);

   overriding procedure On_Completion_Response
     (Self   : in out Message_Logger;
      Value  : LSP.Messages.Server_Responses.Completion_Response);

   overriding procedure On_Hover_Response
     (Self   : in out Message_Logger;
      Value  : LSP.Messages.Server_Responses.Hover_Response);

   overriding procedure On_SignatureHelp_Response
     (Self   : in out Message_Logger;
      Value  : LSP.Messages.Server_Responses.SignatureHelp_Response);

   overriding procedure On_Highlight_Response
     (Self   : in out Message_Logger;
      Value  : LSP.Messages.Server_Responses.Highlight_Response);

   overriding procedure On_Symbol_Response
     (Self   : in out Message_Logger;
      Value  : LSP.Messages.Server_Responses.Symbol_Response);

   overriding procedure On_Rename_Response
     (Self   : in out Message_Logger;
      Value  : LSP.Messages.Server_Responses.Rename_Response);

   overriding procedure On_CodeAction_Response
     (Self   : in out Message_Logger;
      Value  : LSP.Messages.Server_Responses.CodeAction_Response);

   overriding procedure On_Location_Response
     (Self   : in out Message_Logger;
      Value  : LSP.Messages.Server_Responses.Location_Response);

   overriding procedure On_ALS_Called_By_Response
     (Self   : in out Message_Logger;
      Value  : LSP.Messages.Server_Responses.ALS_Called_By_Response);

   overriding procedure On_ExecuteCommand_Response
     (Self   : in out Message_Logger;
      Value  : LSP.Messages.Server_Responses.ExecuteCommand_Response);

   overriding procedure On_Shutdown_Response
     (Self   : in out Message_Logger;
      Value  : LSP.Messages.Server_Responses.Shutdown_Response);

   overriding procedure On_Show_Message
     (Self   : access Message_Logger;
      Params : LSP.Messages.ShowMessageParams);

   overriding procedure On_Log_Message
     (Self   : access Message_Logger;
      Params : LSP.Messages.LogMessageParams);

   overriding procedure On_Publish_Diagnostics
     (Self   : access Message_Logger;
      Params : LSP.Messages.PublishDiagnosticsParams);

   overriding procedure On_ShowMessage_Request
     (Self   : access Message_Logger;
      Value  : LSP.Messages.Client_Requests.ShowMessage_Request);

   overriding procedure On_Workspace_Apply_Edit_Request
     (Self   : access Message_Logger;
      Value  : LSP.Messages.Client_Requests.Workspace_Apply_Edit_Request);

   overriding procedure On_ApplyWorkspaceEdit_Response
     (Self   : in out Message_Logger;
      Value  : LSP.Messages.Client_Responses.ApplyWorkspaceEdit_Response);

   overriding procedure On_ShowMessage_Response
     (Self   : in out Message_Logger;
      Value  : LSP.Messages.Client_Responses.ShowMessage_Response);

end LSP.Message_Loggers;
