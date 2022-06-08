------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2018-2021, AdaCore                     --
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

with LSP.Types;
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

   overriding procedure On_DidChangeWorkspaceFolders_Notification
     (Self  : access Message_Logger;
      Value : LSP.Messages.DidChangeWorkspaceFoldersParams);

   overriding procedure On_DidChangeWatchedFiles_Notification
     (Self  : access Message_Logger;
      Value : LSP.Messages.DidChangeWatchedFilesParams);

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

   overriding procedure On_CompletionItemResolve_Request
     (Self   : access Message_Logger;
      Value  : LSP.Messages.Server_Requests.CompletionItemResolve_Request);

   overriding procedure On_Definition_Request
     (Self   : access Message_Logger;
      Value  : LSP.Messages.Server_Requests.Definition_Request);

   overriding procedure On_Declaration_Request
     (Self  : access Message_Logger;
      Value : LSP.Messages.Server_Requests.Declaration_Request);

   overriding procedure On_Implementation_Request
     (Self  : access Message_Logger;
      Value : LSP.Messages.Server_Requests.Implementation_Request);

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

   overriding procedure On_Document_Links_Request
     (Self  : access Message_Logger;
      Value : LSP.Messages.Server_Requests.Document_Links_Request);

   overriding procedure On_Document_Symbols_Request
     (Self   : access Message_Logger;
      Value  : LSP.Messages.Server_Requests.Document_Symbols_Request);

   overriding procedure On_Document_Tokens_Full_Request
     (Self   : access Message_Logger;
      Value : LSP.Messages.Server_Requests.Document_Tokens_Full_Request);

   overriding procedure On_Document_Tokens_Range_Request
     (Self   : access Message_Logger;
      Value : LSP.Messages.Server_Requests.Document_Tokens_Range_Request);

   overriding procedure On_Rename_Request
     (Self   : access Message_Logger;
      Value  : LSP.Messages.Server_Requests.Rename_Request);

   overriding procedure On_Prepare_Rename_Request
     (Self  : access Message_Logger;
      Value : LSP.Messages.Server_Requests.Prepare_Rename_Request);

   overriding procedure On_Execute_Command_Request
     (Self   : access Message_Logger;
      Value  : LSP.Messages.Server_Requests.Execute_Command_Request);

   overriding procedure On_Workspace_Symbols_Request
     (Self   : access Message_Logger;
      Value  : LSP.Messages.Server_Requests.Workspace_Symbols_Request);

   overriding procedure On_Workspace_Execute_Command_Request
     (Self   : access Message_Logger;
      Value  : LSP.Messages.Server_Requests.Workspace_Execute_Command_Request);

   overriding procedure On_Workspace_Will_Create_Files_Request
     (Self   : access Message_Logger;
      Value  : LSP.Messages.Server_Requests.
                 Workspace_Will_Create_Files_Request);

   overriding procedure On_Workspace_Will_Rename_Files_Request
     (Self   : access Message_Logger;
      Value  : LSP.Messages.Server_Requests.
                 Workspace_Will_Rename_Files_Request);

   overriding procedure On_Workspace_Will_Delete_Files_Request
     (Self   : access Message_Logger;
      Value  : LSP.Messages.Server_Requests.
                 Workspace_Will_Delete_Files_Request);

   overriding procedure On_Color_Presentation_Request
     (Self  : access Message_Logger;
      Value : LSP.Messages.Server_Requests.Color_Presentation_Request);

   overriding procedure On_Document_Color_Request
     (Self  : access Message_Logger;
      Value : LSP.Messages.Server_Requests.Document_Color_Request);

   overriding procedure On_Folding_Range_Request
     (Self  : access Message_Logger;
      Value : LSP.Messages.Server_Requests.Folding_Range_Request);

   overriding procedure On_Selection_Range_Request
     (Self  : access Message_Logger;
      Value : LSP.Messages.Server_Requests.Selection_Range_Request);

   overriding procedure On_Prepare_Call_Hierarchy_Request
     (Self  : access Message_Logger;
      Value : LSP.Messages.Server_Requests.Prepare_Call_Hierarchy_Request);

   overriding procedure On_Incoming_Calls_Request
     (Self  : access Message_Logger;
      Value : LSP.Messages.Server_Requests.Incoming_Calls_Request);

   overriding procedure On_Outgoing_Calls_Request
     (Self  : access Message_Logger;
      Value : LSP.Messages.Server_Requests.Outgoing_Calls_Request);

   overriding procedure On_ALS_Show_Dependencies_Request
     (Self   : access Message_Logger;
      Value  : LSP.Messages.Server_Requests.ALS_Show_Dependencies_Request);

   overriding procedure On_ALS_Source_Dirs_Request
     (Self  : access Message_Logger;
      Value : LSP.Messages.Server_Requests.ALS_Source_Dirs_Request);

   overriding procedure On_ALS_Debug_Request
     (Self  : access Message_Logger;
      Value : LSP.Messages.Server_Requests.ALS_Debug_Request);

   overriding procedure On_Initialize_Response
     (Self   : in out Message_Logger;
      Value  : LSP.Messages.Server_Responses.Initialize_Response);

   overriding procedure On_Completion_Response
     (Self   : in out Message_Logger;
      Value  : LSP.Messages.Server_Responses.Completion_Response);

   overriding procedure On_CompletionItemResolve_Response
     (Self   : in out Message_Logger;
      Value  : LSP.Messages.Server_Responses.CompletionItemResolve_Response);

   overriding procedure On_Hover_Response
     (Self   : in out Message_Logger;
      Value  : LSP.Messages.Server_Responses.Hover_Response);

   overriding procedure On_SignatureHelp_Response
     (Self   : in out Message_Logger;
      Value  : LSP.Messages.Server_Responses.SignatureHelp_Response);

   overriding procedure On_FoldingRange_Response
     (Self  : in out Message_Logger;
      Value : LSP.Messages.Server_Responses.FoldingRange_Response);

   overriding procedure On_Prepare_Call_Hierarchy_Response
     (Self  : in out Message_Logger;
      Value : LSP.Messages.Server_Responses.PrepareCallHierarchy_Response);

   overriding procedure On_Incoming_Calls_Response
     (Self  : in out Message_Logger;
      Value : LSP.Messages.Server_Responses.IncomingCalls_Response);

   overriding procedure On_Outgoing_Calls_Response
     (Self  : in out Message_Logger;
      Value : LSP.Messages.Server_Responses.OutgoingCalls_Response);

   overriding procedure On_SelectionRange_Response
     (Self  : in out Message_Logger;
      Value : LSP.Messages.Server_Responses.SelectionRange_Response);

   overriding procedure On_Highlight_Response
     (Self   : in out Message_Logger;
      Value  : LSP.Messages.Server_Responses.Highlight_Response);

   overriding procedure On_Symbol_Response
     (Self   : in out Message_Logger;
      Value  : LSP.Messages.Server_Responses.Symbol_Response);

   overriding procedure On_Links_Response
     (Self  : in out Message_Logger;
      Value : LSP.Messages.Server_Responses.Links_Response);

   overriding procedure On_Rename_Response
     (Self   : in out Message_Logger;
      Value  : LSP.Messages.Server_Responses.Rename_Response);

   overriding procedure On_Prepare_Rename_Response
     (Self  : in out Message_Logger;
      Value : LSP.Messages.Server_Responses.Prepare_Rename_Response);

   overriding procedure On_CodeAction_Response
     (Self   : in out Message_Logger;
      Value  : LSP.Messages.Server_Responses.CodeAction_Response);

   overriding procedure On_Location_Response
     (Self   : in out Message_Logger;
      Value  : LSP.Messages.Server_Responses.Location_Response);

   overriding procedure On_Location_Link_Response
     (Self   : in out Message_Logger;
      Value  : LSP.Messages.Server_Responses.Location_Link_Response);

   overriding procedure On_ALS_ShowDependencies_Response
     (Self   : in out Message_Logger;
      Value  : LSP.Messages.Server_Responses.ALS_ShowDependencies_Response);

   overriding procedure On_ALS_SourceDirs_Response
     (Self   : in out Message_Logger;
      Value  : LSP.Messages.Server_Responses.ALS_SourceDirs_Response);

   overriding procedure On_ALS_Debug_Response
     (Self   : in out Message_Logger;
      Value  : LSP.Messages.Server_Responses.ALS_Debug_Response);

   overriding procedure On_ExecuteCommand_Response
     (Self   : in out Message_Logger;
      Value  : LSP.Messages.Server_Responses.ExecuteCommand_Response);

   overriding procedure On_WillCreateFiles_Response
     (Self   : in out Message_Logger;
      Value  : LSP.Messages.Server_Responses.WillCreateFiles_Response);

   overriding procedure On_DidCreateFiles_Notification
     (Self  : access Message_Logger;
      Value : LSP.Messages.CreateFilesParams);

   overriding procedure On_WillRenameFiles_Response
     (Self   : in out Message_Logger;
      Value  : LSP.Messages.Server_Responses.WillRenameFiles_Response);

   overriding procedure On_DidRenameFiles_Notification
     (Self  : access Message_Logger;
      Value : LSP.Messages.RenameFilesParams);

   overriding procedure On_WillDeleteFiles_Response
     (Self   : in out Message_Logger;
      Value  : LSP.Messages.Server_Responses.WillDeleteFiles_Response);

   overriding procedure On_DidDeleteFiles_Notification
     (Self  : access Message_Logger;
      Value : LSP.Messages.DeleteFilesParams);

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

   overriding function Get_Progress_Type
     (Self  : access Message_Logger;
      Token : LSP.Types.LSP_Number_Or_String)
      return LSP.Client_Notification_Receivers.Progress_Value_Kind;

   overriding procedure On_Progress
     (Self   : access Message_Logger;
      Params : LSP.Messages.Progress_Params);

   overriding procedure On_Progress_SymbolInformation_Vector
     (Self   : access Message_Logger;
      Params : LSP.Messages.Progress_SymbolInformation_Vector);

   overriding procedure On_ShowMessage_Request
     (Self   : access Message_Logger;
      Value  : LSP.Messages.Client_Requests.ShowMessage_Request);

   overriding procedure On_ShowDocument_Request
     (Self   : access Message_Logger;
      Value  : LSP.Messages.Client_Requests.ShowDocument_Request);

   overriding procedure On_Workspace_Folders_Request
     (Self  : access Message_Logger;
      Value : LSP.Messages.Client_Requests.Workspace_Folders_Request);

   overriding procedure On_RegisterCapability_Request
     (Self  : access Message_Logger;
      Value : LSP.Messages.Client_Requests.RegisterCapability_Request);

   overriding procedure On_UnregisterCapability_Request
     (Self  : access Message_Logger;
      Value : LSP.Messages.Client_Requests.UnregisterCapability_Request);

   overriding procedure On_WorkDoneProgress_Create_Request
     (Self  : access Message_Logger;
      Value : LSP.Messages.Client_Requests.WorkDoneProgressCreate_Request);

   overriding procedure On_Workspace_Apply_Edit_Request
     (Self   : access Message_Logger;
      Value  : LSP.Messages.Client_Requests.Workspace_Apply_Edit_Request);

   overriding procedure On_Workspace_Configuration_Request
     (Self  : access Message_Logger;
      Value : LSP.Messages.Client_Requests.Workspace_Configuration_Request);

   overriding procedure On_ApplyWorkspaceEdit_Response
     (Self   : in out Message_Logger;
      Value  : LSP.Messages.Client_Responses.ApplyWorkspaceEdit_Response);

   overriding procedure On_Configuration_Response
     (Self  : in out Message_Logger;
      Value : LSP.Messages.Client_Responses.Configuration_Response);

   overriding procedure On_ColorPresentation_Response
     (Self  : in out Message_Logger;
      Value : LSP.Messages.Server_Responses.ColorPresentation_Response);

   overriding procedure On_DocumentColor_Response
     (Self  : in out Message_Logger;
      Value : LSP.Messages.Server_Responses.DocumentColor_Response);

   overriding procedure On_ShowMessage_Response
     (Self   : in out Message_Logger;
      Value  : LSP.Messages.Client_Responses.ShowMessage_Response);

   overriding procedure On_WorkspaceFolders_Response
     (Self  : in out Message_Logger;
      Value : LSP.Messages.Client_Responses.WorkspaceFolders_Response);

   overriding procedure On_WorkDoneProgressCreate_Response
     (Self  : in out Message_Logger;
      Value : LSP.Messages.Client_Responses.WorkDoneProgressCreate_Response);

   overriding procedure On_RegisterCapability_Response
     (Self  : in out Message_Logger;
      Value : LSP.Messages.Client_Responses.RegisterCapability_Response);

   overriding procedure On_UnregisterCapability_Response
     (Self  : in out Message_Logger;
      Value : LSP.Messages.Client_Responses.UnregisterCapability_Response);

   overriding procedure On_Formatting_Request
     (Self  : access Message_Logger;
      Value : LSP.Messages.Server_Requests.Formatting_Request);

   overriding procedure On_Range_Formatting_Request
     (Self  : access Message_Logger;
      Value : LSP.Messages.Server_Requests.Range_Formatting_Request);

   overriding procedure On_SemanticTokens_Response
     (Self  : in out Message_Logger;
      Value : LSP.Messages.Server_Responses.SemanticTokens_Response);

   overriding procedure On_Formatting_Response
     (Self     : in out Message_Logger;
      Response : LSP.Messages.Server_Responses.Formatting_Response);

   overriding procedure On_Range_Formatting_Response
     (Self     : in out Message_Logger;
      Response : LSP.Messages.Server_Responses.Range_Formatting_Response);

   overriding procedure On_ALS_Check_Syntax_Request
     (Self  : access Message_Logger;
      Value : LSP.Messages.Server_Requests.ALS_Check_Syntax_Request);

   overriding procedure On_ALS_Check_Syntax_Response
     (Self     : in out Message_Logger;
      Response : LSP.Messages.Server_Responses.ALS_Check_Syntax_Response);

end LSP.Message_Loggers;
