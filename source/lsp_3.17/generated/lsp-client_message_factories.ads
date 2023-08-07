--
--  Copyright (C) <YEAR>, <COPYRIGHT HOLDER>
--
--  SPDX-License-Identifier: MIT
--

with LSP.Client_Message_Receivers;
with LSP.Client_Messages;
with LSP.Errors;
with LSP.Structures;

package LSP.Client_Message_Factories is
   pragma Preelaborate;

   type Client_Message_Factory is
   abstract limited new LSP.Client_Message_Receivers
     .Client_Message_Receiver with
   null record;

   procedure On_Message
     (Self  : in out Client_Message_Factory;
      Value : LSP.Client_Messages.Client_Message_Access) is abstract;

   overriding procedure On_Error_Response
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Errors.ResponseError);

   overriding procedure On_LogTrace_Notification
     (Self  : in out Client_Message_Factory;
      Value : LSP.Structures.LogTraceParams);

   overriding procedure On_Event_Notification
     (Self : in out Client_Message_Factory; Value : LSP.Structures.LSPAny);

   overriding procedure On_PublishDiagnostics_Notification
     (Self  : in out Client_Message_Factory;
      Value : LSP.Structures.PublishDiagnosticsParams);

   overriding procedure On_LogMessage_Notification
     (Self  : in out Client_Message_Factory;
      Value : LSP.Structures.LogMessageParams);

   overriding procedure On_ShowMessage_Notification
     (Self  : in out Client_Message_Factory;
      Value : LSP.Structures.ShowMessageParams);

   overriding procedure On_RegisterCapability_Request
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.RegistrationParams);

   overriding procedure On_UnregisterCapability_Request
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.UnregistrationParams);

   overriding procedure On_ShowDocument_Request
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.ShowDocumentParams);

   overriding procedure On_ShowMessageRequest_Request
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.ShowMessageRequestParams);

   overriding procedure On_Progress_Create_Request
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.WorkDoneProgressCreateParams);

   overriding procedure On_ApplyEdit_Request
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.ApplyWorkspaceEditParams);

   overriding procedure On_Code_Lens_Refresh_Request
     (Self : in out Client_Message_Factory;
      Id   : LSP.Structures.Integer_Or_Virtual_String);

   overriding procedure On_Configuration_Request
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.ConfigurationParams);

   overriding procedure On_Diagnostic_Refresh_Request
     (Self : in out Client_Message_Factory;
      Id   : LSP.Structures.Integer_Or_Virtual_String);

   overriding procedure On_Inlay_Refresh_Request
     (Self : in out Client_Message_Factory;
      Id   : LSP.Structures.Integer_Or_Virtual_String);

   overriding procedure On_Inline_Refresh_Request
     (Self : in out Client_Message_Factory;
      Id   : LSP.Structures.Integer_Or_Virtual_String);

   overriding procedure On_Tokens_Refresh_Request
     (Self : in out Client_Message_Factory;
      Id   : LSP.Structures.Integer_Or_Virtual_String);

   overriding procedure On_WorkspaceFolders_Request
     (Self : in out Client_Message_Factory;
      Id   : LSP.Structures.Integer_Or_Virtual_String);

   overriding procedure On_IncomingCalls_Response
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.CallHierarchyIncomingCall_Vector_Or_Null);

   overriding procedure On_OutgoingCalls_Response
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.CallHierarchyOutgoingCall_Vector_Or_Null);

   overriding procedure On_Code_Action_Resolve_Response
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.CodeAction);

   overriding procedure On_Code_Lens_Resolve_Response
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.CodeLens);

   overriding procedure On_Completion_Resolve_Response
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.CompletionItem);

   overriding procedure On_Link_Resolve_Response
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.DocumentLink);

   overriding procedure On_Initialize_Response
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.InitializeResult);

   overriding procedure On_Inlay_Resolve_Response
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.InlayHint);

   overriding procedure On_Shutdown_Response
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Null_Record);

   overriding procedure On_CodeAction_Response
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Command_Or_CodeAction_Vector_Or_Null);

   overriding procedure On_CodeLens_Response
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.CodeLens_Vector_Or_Null);

   overriding procedure On_ColorPresentation_Response
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.ColorPresentation_Vector);

   overriding procedure On_Completion_Response
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Completion_Result);

   overriding procedure On_Declaration_Response
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Declaration_Result);

   overriding procedure On_Definition_Response
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Definition_Result);

   overriding procedure On_Diagnostic_Response
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.DocumentDiagnosticReport);

   overriding procedure On_DocumentColor_Response
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.ColorInformation_Vector);

   overriding procedure On_DocumentHighlight_Response
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.DocumentHighlight_Vector_Or_Null);

   overriding procedure On_DocumentLink_Response
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.DocumentLink_Vector_Or_Null);

   overriding procedure On_DocumentSymbol_Response
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.DocumentSymbol_Result);

   overriding procedure On_FoldingRange_Response
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.FoldingRange_Vector_Or_Null);

   overriding procedure On_Formatting_Response
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.TextEdit_Vector_Or_Null);

   overriding procedure On_Hover_Response
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Hover_Or_Null);

   overriding procedure On_Implementation_Response
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Definition_Result);

   overriding procedure On_InlayHint_Response
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.InlayHint_Vector_Or_Null);

   overriding procedure On_InlineValue_Response
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.InlineValue_Vector_Or_Null);

   overriding procedure On_LinkedEditingRange_Response
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.LinkedEditingRanges_Or_Null);

   overriding procedure On_Moniker_Response
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Moniker_Vector_Or_Null);

   overriding procedure On_OnTypeFormatting_Response
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.TextEdit_Vector_Or_Null);

   overriding procedure On_PrepareCallHierarchy_Response
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.CallHierarchyItem_Vector_Or_Null);

   overriding procedure On_PrepareRename_Response
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.PrepareRenameResult_Or_Null);

   overriding procedure On_PrepareTypeHierarchy_Response
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.TypeHierarchyItem_Vector_Or_Null);

   overriding procedure On_RangeFormatting_Response
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.TextEdit_Vector_Or_Null);

   overriding procedure On_References_Response
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Location_Vector_Or_Null);

   overriding procedure On_Rename_Response
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.WorkspaceEdit_Or_Null);

   overriding procedure On_SelectionRange_Response
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.SelectionRange_Vector_Or_Null);

   overriding procedure On_Full_Response
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.SemanticTokens_Or_Null);

   overriding procedure On_Tokens_Delta_Response
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Tokens_Delta_Result);

   overriding procedure On_Tokens_Range_Response
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.SemanticTokens_Or_Null);

   overriding procedure On_SignatureHelp_Response
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.SignatureHelp_Or_Null);

   overriding procedure On_TypeDefinition_Response
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Definition_Result);

   overriding procedure On_WillSaveWaitUntil_Response
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.TextEdit_Vector_Or_Null);

   overriding procedure On_Subtypes_Response
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.TypeHierarchyItem_Vector_Or_Null);

   overriding procedure On_Supertypes_Response
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.TypeHierarchyItem_Vector_Or_Null);

   overriding procedure On_Workspace_Diagnostic_Response
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.WorkspaceDiagnosticReport);

   overriding procedure On_ExecuteCommand_Response
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.LSPAny_Or_Null);

   overriding procedure On_Symbol_Response
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Symbol_Result);

   overriding procedure On_WillCreateFiles_Response
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.WorkspaceEdit_Or_Null);

   overriding procedure On_WillDeleteFiles_Response
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.WorkspaceEdit_Or_Null);

   overriding procedure On_WillRenameFiles_Response
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.WorkspaceEdit_Or_Null);

   overriding procedure On_Symbol_Resolve_Response
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.WorkspaceSymbol);

   overriding procedure On_IncomingCalls_Partial_Result
     (Self  : in out Client_Message_Factory;
      Token : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.CallHierarchyIncomingCall_Vector);

   overriding procedure On_OutgoingCalls_Partial_Result
     (Self  : in out Client_Message_Factory;
      Token : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.CallHierarchyOutgoingCall_Vector);

   overriding procedure On_CodeAction_Partial_Result
     (Self  : in out Client_Message_Factory;
      Token : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Command_Or_CodeAction_Vector);

   overriding procedure On_CodeLens_Partial_Result
     (Self  : in out Client_Message_Factory;
      Token : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.CodeLens_Vector);

   overriding procedure On_ColorPresentation_Partial_Result
     (Self  : in out Client_Message_Factory;
      Token : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.ColorPresentation_Vector);

   overriding procedure On_Completion_Partial_Result
     (Self  : in out Client_Message_Factory;
      Token : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.CompletionItem_Vector);

   overriding procedure On_Declaration_Partial_Result
     (Self  : in out Client_Message_Factory;
      Token : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Declaration_Progress_Report);

   overriding procedure On_Definition_Partial_Result
     (Self  : in out Client_Message_Factory;
      Token : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Definition_Progress_Report);

   overriding procedure On_Diagnostic_Partial_Result
     (Self  : in out Client_Message_Factory;
      Token : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.DocumentDiagnosticReportPartialResult);

   overriding procedure On_DocumentColor_Partial_Result
     (Self  : in out Client_Message_Factory;
      Token : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.ColorInformation_Vector);

   overriding procedure On_DocumentHighlight_Partial_Result
     (Self  : in out Client_Message_Factory;
      Token : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.DocumentHighlight_Vector);

   overriding procedure On_DocumentLink_Partial_Result
     (Self  : in out Client_Message_Factory;
      Token : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.DocumentLink_Vector);

   overriding procedure On_DocumentSymbol_Partial_Result
     (Self  : in out Client_Message_Factory;
      Token : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.DocumentSymbol_Progress_Report);

   overriding procedure On_FoldingRange_Partial_Result
     (Self  : in out Client_Message_Factory;
      Token : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.FoldingRange_Vector);

   overriding procedure On_Implementation_Partial_Result
     (Self  : in out Client_Message_Factory;
      Token : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Definition_Progress_Report);

   overriding procedure On_InlayHint_Partial_Result
     (Self  : in out Client_Message_Factory;
      Token : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.InlayHint_Vector);

   overriding procedure On_InlineValue_Partial_Result
     (Self  : in out Client_Message_Factory;
      Token : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.InlineValue_Vector);

   overriding procedure On_Moniker_Partial_Result
     (Self  : in out Client_Message_Factory;
      Token : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Moniker_Vector);

   overriding procedure On_References_Partial_Result
     (Self  : in out Client_Message_Factory;
      Token : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Location_Vector);

   overriding procedure On_SelectionRange_Partial_Result
     (Self  : in out Client_Message_Factory;
      Token : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.SelectionRange_Vector);

   overriding procedure On_Full_Partial_Result
     (Self  : in out Client_Message_Factory;
      Token : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.SemanticTokensPartialResult);

   overriding procedure On_Tokens_Delta_Partial_Result
     (Self  : in out Client_Message_Factory;
      Token : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures
        .SemanticTokensPartialResult_Or_SemanticTokensDeltaPartialResult);

   overriding procedure On_Tokens_Range_Partial_Result
     (Self  : in out Client_Message_Factory;
      Token : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.SemanticTokensPartialResult);

   overriding procedure On_TypeDefinition_Partial_Result
     (Self  : in out Client_Message_Factory;
      Token : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Definition_Progress_Report);

   overriding procedure On_Subtypes_Partial_Result
     (Self  : in out Client_Message_Factory;
      Token : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.TypeHierarchyItem_Vector);

   overriding procedure On_Supertypes_Partial_Result
     (Self  : in out Client_Message_Factory;
      Token : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.TypeHierarchyItem_Vector);

   overriding procedure On_Workspace_Diagnostic_Partial_Result
     (Self  : in out Client_Message_Factory;
      Token : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.WorkspaceDiagnosticReportPartialResult);

   overriding procedure On_Symbol_Partial_Result
     (Self  : in out Client_Message_Factory;
      Token : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Symbol_Progress_Report);

   overriding procedure On_ProgressBegin_Work_Done
     (Self  : in out Client_Message_Factory;
      Token : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.WorkDoneProgressBegin);

   overriding procedure On_ProgressReport_Work_Done
     (Self  : in out Client_Message_Factory;
      Token : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.WorkDoneProgressReport);

   overriding procedure On_ProgressEnd_Work_Done
     (Self  : in out Client_Message_Factory;
      Token : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.WorkDoneProgressEnd);

end LSP.Client_Message_Factories;
