--
--  Copyright (C) <YEAR>, <COPYRIGHT HOLDER>
--
--  SPDX-License-Identifier: MIT
--

with VSS.JSON.Content_Handlers;

with LSP.Enumerations;
with LSP.Structures;

package LSP.Outputs is
   pragma Preelaborate;

   procedure Write_InitializeParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.InitializeParams);

   procedure Write_DocumentLink_Vector_Or_Null
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DocumentLink_Vector_Or_Null);

   procedure Write_Command_Or_CodeAction_Vector
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.Command_Or_CodeAction_Vector);

   procedure Write_Null_Record
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.Null_Record);

   procedure Write_CallHierarchyOutgoingCall_Vector
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.CallHierarchyOutgoingCall_Vector);

   procedure Write_DocumentDiagnosticParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DocumentDiagnosticParams);

   procedure Write_DocumentLink_Vector
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DocumentLink_Vector);

   procedure Write_InlineValueParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.InlineValueParams);

   procedure Write_DidSaveNotebookDocumentParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DidSaveNotebookDocumentParams);

   procedure Write_Declaration_Result
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.Declaration_Result);

   procedure Write_Definition_Progress_Report
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.Definition_Progress_Report);

   procedure Write_SemanticTokensParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.SemanticTokensParams);

   procedure Write_ColorInformation_Vector
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.ColorInformation_Vector);

   procedure Write_DocumentSymbol_Progress_Report
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DocumentSymbol_Progress_Report);

   procedure Write_CallHierarchyIncomingCall_Vector_Or_Null
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.CallHierarchyIncomingCall_Vector_Or_Null);

   procedure Write_CodeLens_Vector
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.CodeLens_Vector);

   procedure Write_LSPAny
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.LSPAny);

   procedure Write_LogTraceParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.LogTraceParams);

   procedure Write_PrepareRenameResult_Or_Null
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.PrepareRenameResult_Or_Null);

   procedure Write_CompletionItem_Vector
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.CompletionItem_Vector);

   procedure Write_Moniker_Vector_Or_Null
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.Moniker_Vector_Or_Null);

   procedure Write_WorkDoneProgressReport
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.WorkDoneProgressReport);

   procedure Write_WorkspaceSymbolParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.WorkspaceSymbolParams);

   procedure Write_InitializeResult
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.InitializeResult);

   procedure Write_WorkDoneProgressCancelParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.WorkDoneProgressCancelParams);

   procedure Write_DidChangeWatchedFilesParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DidChangeWatchedFilesParams);

   procedure Write_DidOpenNotebookDocumentParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DidOpenNotebookDocumentParams);

   procedure Write_SignatureHelp_Or_Null
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.SignatureHelp_Or_Null);

   procedure Write_CodeLens_Vector_Or_Null
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.CodeLens_Vector_Or_Null);

   procedure Write_WorkspaceFolder_Vector_Or_Null
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.WorkspaceFolder_Vector_Or_Null);

   procedure Write_DocumentLinkParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DocumentLinkParams);

   procedure Write_ConfigurationParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.ConfigurationParams);

   procedure Write_DidChangeTextDocumentParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DidChangeTextDocumentParams);

   procedure Write_Declaration_Progress_Report
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.Declaration_Progress_Report);

   procedure Write_DidSaveTextDocumentParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DidSaveTextDocumentParams);

   procedure Write_ShowMessageParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.ShowMessageParams);

   procedure Write_ReferenceParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.ReferenceParams);

   procedure Write_DidChangeConfigurationParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DidChangeConfigurationParams);

   procedure Write_Hover_Or_Null
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.Hover_Or_Null);

   procedure Write_CallHierarchyOutgoingCall_Vector_Or_Null
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.CallHierarchyOutgoingCall_Vector_Or_Null);

   procedure Write_ShowDocumentResult
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.ShowDocumentResult);

   procedure Write_TypeDefinitionParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.TypeDefinitionParams);

   procedure Write_RenameParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.RenameParams);

   procedure Write_LSPAny_Or_Null
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.LSPAny_Or_Null);

   procedure Write_WorkspaceSymbol
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.WorkspaceSymbol);

   procedure Write_LSPAny_Vector
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.LSPAny_Vector);

   procedure Write_PrepareRenameParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.PrepareRenameParams);

   procedure Write_CancelParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.CancelParams);

   procedure Write_DefinitionParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DefinitionParams);

   procedure Write_ColorPresentationParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.ColorPresentationParams);

   procedure Write_ErrorCodes
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Enumerations.ErrorCodes);

   procedure Write_FoldingRange_Vector_Or_Null
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.FoldingRange_Vector_Or_Null);

   procedure Write_MessageActionItem_Or_Null
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.MessageActionItem_Or_Null);

   procedure Write_ApplyWorkspaceEditParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.ApplyWorkspaceEditParams);

   procedure Write_CodeAction
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.CodeAction);

   procedure Write_InlayHint_Vector_Or_Null
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.InlayHint_Vector_Or_Null);

   procedure Write_DeclarationParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DeclarationParams);

   procedure Write_DocumentRangeFormattingParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DocumentRangeFormattingParams);

   procedure Write_MonikerParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.MonikerParams);

   procedure Write_InlayHint
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.InlayHint);

   procedure Write_UnregistrationParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.UnregistrationParams);

   procedure Write_SelectionRangeParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.SelectionRangeParams);

   procedure Write_Tokens_Delta_Result
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.Tokens_Delta_Result);

   procedure Write_RenameFilesParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.RenameFilesParams);

   procedure Write_Symbol_Progress_Report
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.Symbol_Progress_Report);

   procedure Write_TypeHierarchySubtypesParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.TypeHierarchySubtypesParams);

   procedure Write_ApplyWorkspaceEditResult
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.ApplyWorkspaceEditResult);

   procedure Write_SemanticTokensPartialResult
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.SemanticTokensPartialResult);

   procedure Write_Completion_Result
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.Completion_Result);

   procedure Write_CallHierarchyItem_Vector_Or_Null
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.CallHierarchyItem_Vector_Or_Null);

   procedure Write_DidCloseTextDocumentParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DidCloseTextDocumentParams);

   procedure Write_CallHierarchyOutgoingCallsParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.CallHierarchyOutgoingCallsParams);

   procedure Write_SelectionRange_Vector
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.SelectionRange_Vector);

   procedure Write_SemanticTokensDeltaParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.SemanticTokensDeltaParams);

   procedure Write_DidChangeWorkspaceFoldersParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DidChangeWorkspaceFoldersParams);

   procedure Write_CodeLensParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.CodeLensParams);

   procedure Write_TypeHierarchyItem_Vector_Or_Null
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.TypeHierarchyItem_Vector_Or_Null);

   procedure Write_Symbol_Result
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.Symbol_Result);

   procedure Write_TypeHierarchySupertypesParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.TypeHierarchySupertypesParams);

   procedure Write_PublishDiagnosticsParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.PublishDiagnosticsParams);

   procedure Write_WorkspaceDiagnosticReport
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.WorkspaceDiagnosticReport);

   procedure Write_TextEdit_Vector_Or_Null
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.TextEdit_Vector_Or_Null);

   procedure Write_LinkedEditingRanges_Or_Null
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.LinkedEditingRanges_Or_Null);

   procedure Write_SetTraceParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.SetTraceParams);

   procedure Write_WorkspaceDiagnosticReportPartialResult
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.WorkspaceDiagnosticReportPartialResult);

   procedure Write_DidOpenTextDocumentParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DidOpenTextDocumentParams);

   procedure Write_SemanticTokensPartialResult_Or_SemanticTokensDeltaPartialResult
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures
        .SemanticTokensPartialResult_Or_SemanticTokensDeltaPartialResult);

   procedure Write_DocumentDiagnosticReport
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DocumentDiagnosticReport);

   procedure Write_ExecuteCommandParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.ExecuteCommandParams);

   procedure Write_WorkDoneProgressBegin
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.WorkDoneProgressBegin);

   procedure Write_InlayHintParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.InlayHintParams);

   procedure Write_WorkDoneProgressCreateParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.WorkDoneProgressCreateParams);

   procedure Write_DocumentHighlightParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DocumentHighlightParams);

   procedure Write_SemanticTokens_Or_Null
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.SemanticTokens_Or_Null);

   procedure Write_SemanticTokensRangeParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.SemanticTokensRangeParams);

   procedure Write_CompletionParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.CompletionParams);

   procedure Write_CallHierarchyIncomingCallsParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.CallHierarchyIncomingCallsParams);

   procedure Write_Location_Vector_Or_Null
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.Location_Vector_Or_Null);

   procedure Write_DidChangeNotebookDocumentParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DidChangeNotebookDocumentParams);

   procedure Write_WorkspaceDiagnosticParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.WorkspaceDiagnosticParams);

   procedure Write_InlineValue_Vector_Or_Null
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.InlineValue_Vector_Or_Null);

   procedure Write_DidCloseNotebookDocumentParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DidCloseNotebookDocumentParams);

   procedure Write_InlayHint_Vector
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.InlayHint_Vector);

   procedure Write_ShowDocumentParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.ShowDocumentParams);

   procedure Write_CreateFilesParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.CreateFilesParams);

   procedure Write_ColorPresentation_Vector
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.ColorPresentation_Vector);

   procedure Write_DocumentFormattingParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DocumentFormattingParams);

   procedure Write_DeleteFilesParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DeleteFilesParams);

   procedure Write_TypeHierarchyItem_Vector
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.TypeHierarchyItem_Vector);

   procedure Write_DocumentOnTypeFormattingParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DocumentOnTypeFormattingParams);

   procedure Write_Location_Vector
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.Location_Vector);

   procedure Write_DocumentColorParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DocumentColorParams);

   procedure Write_DocumentLink
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DocumentLink);

   procedure Write_WorkDoneProgressEnd
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.WorkDoneProgressEnd);

   procedure Write_DocumentSymbolParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DocumentSymbolParams);

   procedure Write_InlineValue_Vector
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.InlineValue_Vector);

   procedure Write_InitializedParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.InitializedParams);

   procedure Write_ShowMessageRequestParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.ShowMessageRequestParams);

   procedure Write_WorkspaceEdit_Or_Null
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.WorkspaceEdit_Or_Null);

   procedure Write_DocumentHighlight_Vector_Or_Null
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DocumentHighlight_Vector_Or_Null);

   procedure Write_LogMessageParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.LogMessageParams);

   procedure Write_CodeLens
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.CodeLens);

   procedure Write_DocumentDiagnosticReportPartialResult
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DocumentDiagnosticReportPartialResult);

   procedure Write_CallHierarchyIncomingCall_Vector
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.CallHierarchyIncomingCall_Vector);

   procedure Write_ProgressParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.ProgressParams);

   procedure Write_DocumentSymbol_Result
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DocumentSymbol_Result);

   procedure Write_TypeHierarchyPrepareParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.TypeHierarchyPrepareParams);

   procedure Write_WillSaveTextDocumentParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.WillSaveTextDocumentParams);

   procedure Write_Command_Or_CodeAction_Vector_Or_Null
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.Command_Or_CodeAction_Vector_Or_Null);

   procedure Write_Definition_Result
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.Definition_Result);

   procedure Write_CompletionItem
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.CompletionItem);

   procedure Write_Moniker_Vector
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.Moniker_Vector);

   procedure Write_HoverParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.HoverParams);

   procedure Write_DocumentHighlight_Vector
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DocumentHighlight_Vector);

   procedure Write_Integer_Or_Virtual_String
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.Integer_Or_Virtual_String);

   procedure Write_FoldingRangeParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.FoldingRangeParams);

   procedure Write_RegistrationParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.RegistrationParams);

   procedure Write_SignatureHelpParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.SignatureHelpParams);

   procedure Write_CodeActionParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.CodeActionParams);

   procedure Write_FoldingRange_Vector
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.FoldingRange_Vector);

   procedure Write_SelectionRange_Vector_Or_Null
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.SelectionRange_Vector_Or_Null);

   procedure Write_LinkedEditingRangeParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.LinkedEditingRangeParams);

   procedure Write_ImplementationParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.ImplementationParams);

   procedure Write_CallHierarchyPrepareParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.CallHierarchyPrepareParams);

end LSP.Outputs;
