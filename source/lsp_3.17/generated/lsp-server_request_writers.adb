--
--  Copyright (C) <YEAR>, <COPYRIGHT HOLDER>
--
--  SPDX-License-Identifier: MIT
--
--  DON'T EDIT THIS FILE! It was generated from metaModel.json.
--

with LSP.Output_Tools;
with LSP.Outputs;

package body LSP.Server_Request_Writers is

   overriding procedure On_IncomingCalls_Request
     (Self  : in out Server_Request_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.CallHierarchyIncomingCallsParams) is
   begin
      LSP.Output_Tools.Write_Start_Request
        (Self.Output.all, "callHierarchy/incomingCalls", Id);
      Self.Output.Key_Name ("params");
      LSP.Outputs.Write_CallHierarchyIncomingCallsParams
        (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_IncomingCalls_Request;

   overriding procedure On_OutgoingCalls_Request
     (Self  : in out Server_Request_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.CallHierarchyOutgoingCallsParams) is
   begin
      LSP.Output_Tools.Write_Start_Request
        (Self.Output.all, "callHierarchy/outgoingCalls", Id);
      Self.Output.Key_Name ("params");
      LSP.Outputs.Write_CallHierarchyOutgoingCallsParams
        (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_OutgoingCalls_Request;

   overriding procedure On_Code_Action_Resolve_Request
     (Self  : in out Server_Request_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.CodeAction) is
   begin
      LSP.Output_Tools.Write_Start_Request
        (Self.Output.all, "codeAction/resolve", Id);
      Self.Output.Key_Name ("params");
      LSP.Outputs.Write_CodeAction (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_Code_Action_Resolve_Request;

   overriding procedure On_Code_Lens_Resolve_Request
     (Self  : in out Server_Request_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.CodeLens) is
   begin
      LSP.Output_Tools.Write_Start_Request
        (Self.Output.all, "codeLens/resolve", Id);
      Self.Output.Key_Name ("params");
      LSP.Outputs.Write_CodeLens (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_Code_Lens_Resolve_Request;

   overriding procedure On_Completion_Resolve_Request
     (Self  : in out Server_Request_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.CompletionItem) is
   begin
      LSP.Output_Tools.Write_Start_Request
        (Self.Output.all, "completionItem/resolve", Id);
      Self.Output.Key_Name ("params");
      LSP.Outputs.Write_CompletionItem (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_Completion_Resolve_Request;

   overriding procedure On_Link_Resolve_Request
     (Self  : in out Server_Request_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.DocumentLink) is
   begin
      LSP.Output_Tools.Write_Start_Request
        (Self.Output.all, "documentLink/resolve", Id);
      Self.Output.Key_Name ("params");
      LSP.Outputs.Write_DocumentLink (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_Link_Resolve_Request;

   overriding procedure On_Initialize_Request
     (Self  : in out Server_Request_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.InitializeParams) is
   begin
      LSP.Output_Tools.Write_Start_Request (Self.Output.all, "initialize", Id);
      Self.Output.Key_Name ("params");
      LSP.Outputs.Write_InitializeParams (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_Initialize_Request;

   overriding procedure On_Inlay_Resolve_Request
     (Self  : in out Server_Request_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.InlayHint) is
   begin
      LSP.Output_Tools.Write_Start_Request
        (Self.Output.all, "inlayHint/resolve", Id);
      Self.Output.Key_Name ("params");
      LSP.Outputs.Write_InlayHint (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_Inlay_Resolve_Request;

   overriding procedure On_Shutdown_Request
     (Self : in out Server_Request_Writer;
      Id   : LSP.Structures.Integer_Or_Virtual_String) is
   begin
      LSP.Output_Tools.Write_Start_Request (Self.Output.all, "shutdown", Id);
      Self.Output.End_Object;
   end On_Shutdown_Request;

   overriding procedure On_CodeAction_Request
     (Self  : in out Server_Request_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.CodeActionParams) is
   begin
      LSP.Output_Tools.Write_Start_Request
        (Self.Output.all, "textDocument/codeAction", Id);
      Self.Output.Key_Name ("params");
      LSP.Outputs.Write_CodeActionParams (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_CodeAction_Request;

   overriding procedure On_CodeLens_Request
     (Self  : in out Server_Request_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.CodeLensParams) is
   begin
      LSP.Output_Tools.Write_Start_Request
        (Self.Output.all, "textDocument/codeLens", Id);
      Self.Output.Key_Name ("params");
      LSP.Outputs.Write_CodeLensParams (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_CodeLens_Request;

   overriding procedure On_ColorPresentation_Request
     (Self  : in out Server_Request_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.ColorPresentationParams) is
   begin
      LSP.Output_Tools.Write_Start_Request
        (Self.Output.all, "textDocument/colorPresentation", Id);
      Self.Output.Key_Name ("params");
      LSP.Outputs.Write_ColorPresentationParams (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_ColorPresentation_Request;

   overriding procedure On_Completion_Request
     (Self  : in out Server_Request_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.CompletionParams) is
   begin
      LSP.Output_Tools.Write_Start_Request
        (Self.Output.all, "textDocument/completion", Id);
      Self.Output.Key_Name ("params");
      LSP.Outputs.Write_CompletionParams (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_Completion_Request;

   overriding procedure On_Declaration_Request
     (Self  : in out Server_Request_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.DeclarationParams) is
   begin
      LSP.Output_Tools.Write_Start_Request
        (Self.Output.all, "textDocument/declaration", Id);
      Self.Output.Key_Name ("params");
      LSP.Outputs.Write_DeclarationParams (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_Declaration_Request;

   overriding procedure On_Definition_Request
     (Self  : in out Server_Request_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.DefinitionParams) is
   begin
      LSP.Output_Tools.Write_Start_Request
        (Self.Output.all, "textDocument/definition", Id);
      Self.Output.Key_Name ("params");
      LSP.Outputs.Write_DefinitionParams (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_Definition_Request;

   overriding procedure On_Diagnostic_Request
     (Self  : in out Server_Request_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.DocumentDiagnosticParams) is
   begin
      LSP.Output_Tools.Write_Start_Request
        (Self.Output.all, "textDocument/diagnostic", Id);
      Self.Output.Key_Name ("params");
      LSP.Outputs.Write_DocumentDiagnosticParams (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_Diagnostic_Request;

   overriding procedure On_DocumentColor_Request
     (Self  : in out Server_Request_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.DocumentColorParams) is
   begin
      LSP.Output_Tools.Write_Start_Request
        (Self.Output.all, "textDocument/documentColor", Id);
      Self.Output.Key_Name ("params");
      LSP.Outputs.Write_DocumentColorParams (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_DocumentColor_Request;

   overriding procedure On_DocumentHighlight_Request
     (Self  : in out Server_Request_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.DocumentHighlightParams) is
   begin
      LSP.Output_Tools.Write_Start_Request
        (Self.Output.all, "textDocument/documentHighlight", Id);
      Self.Output.Key_Name ("params");
      LSP.Outputs.Write_DocumentHighlightParams (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_DocumentHighlight_Request;

   overriding procedure On_DocumentLink_Request
     (Self  : in out Server_Request_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.DocumentLinkParams) is
   begin
      LSP.Output_Tools.Write_Start_Request
        (Self.Output.all, "textDocument/documentLink", Id);
      Self.Output.Key_Name ("params");
      LSP.Outputs.Write_DocumentLinkParams (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_DocumentLink_Request;

   overriding procedure On_DocumentSymbol_Request
     (Self  : in out Server_Request_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.DocumentSymbolParams) is
   begin
      LSP.Output_Tools.Write_Start_Request
        (Self.Output.all, "textDocument/documentSymbol", Id);
      Self.Output.Key_Name ("params");
      LSP.Outputs.Write_DocumentSymbolParams (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_DocumentSymbol_Request;

   overriding procedure On_FoldingRange_Request
     (Self  : in out Server_Request_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.FoldingRangeParams) is
   begin
      LSP.Output_Tools.Write_Start_Request
        (Self.Output.all, "textDocument/foldingRange", Id);
      Self.Output.Key_Name ("params");
      LSP.Outputs.Write_FoldingRangeParams (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_FoldingRange_Request;

   overriding procedure On_Formatting_Request
     (Self  : in out Server_Request_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.DocumentFormattingParams) is
   begin
      LSP.Output_Tools.Write_Start_Request
        (Self.Output.all, "textDocument/formatting", Id);
      Self.Output.Key_Name ("params");
      LSP.Outputs.Write_DocumentFormattingParams (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_Formatting_Request;

   overriding procedure On_Hover_Request
     (Self  : in out Server_Request_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.HoverParams) is
   begin
      LSP.Output_Tools.Write_Start_Request
        (Self.Output.all, "textDocument/hover", Id);
      Self.Output.Key_Name ("params");
      LSP.Outputs.Write_HoverParams (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_Hover_Request;

   overriding procedure On_Implementation_Request
     (Self  : in out Server_Request_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.ImplementationParams) is
   begin
      LSP.Output_Tools.Write_Start_Request
        (Self.Output.all, "textDocument/implementation", Id);
      Self.Output.Key_Name ("params");
      LSP.Outputs.Write_ImplementationParams (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_Implementation_Request;

   overriding procedure On_InlayHint_Request
     (Self  : in out Server_Request_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.InlayHintParams) is
   begin
      LSP.Output_Tools.Write_Start_Request
        (Self.Output.all, "textDocument/inlayHint", Id);
      Self.Output.Key_Name ("params");
      LSP.Outputs.Write_InlayHintParams (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_InlayHint_Request;

   overriding procedure On_InlineValue_Request
     (Self  : in out Server_Request_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.InlineValueParams) is
   begin
      LSP.Output_Tools.Write_Start_Request
        (Self.Output.all, "textDocument/inlineValue", Id);
      Self.Output.Key_Name ("params");
      LSP.Outputs.Write_InlineValueParams (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_InlineValue_Request;

   overriding procedure On_LinkedEditingRange_Request
     (Self  : in out Server_Request_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.LinkedEditingRangeParams) is
   begin
      LSP.Output_Tools.Write_Start_Request
        (Self.Output.all, "textDocument/linkedEditingRange", Id);
      Self.Output.Key_Name ("params");
      LSP.Outputs.Write_LinkedEditingRangeParams (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_LinkedEditingRange_Request;

   overriding procedure On_Moniker_Request
     (Self  : in out Server_Request_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.MonikerParams) is
   begin
      LSP.Output_Tools.Write_Start_Request
        (Self.Output.all, "textDocument/moniker", Id);
      Self.Output.Key_Name ("params");
      LSP.Outputs.Write_MonikerParams (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_Moniker_Request;

   overriding procedure On_OnTypeFormatting_Request
     (Self  : in out Server_Request_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.DocumentOnTypeFormattingParams) is
   begin
      LSP.Output_Tools.Write_Start_Request
        (Self.Output.all, "textDocument/onTypeFormatting", Id);
      Self.Output.Key_Name ("params");
      LSP.Outputs.Write_DocumentOnTypeFormattingParams
        (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_OnTypeFormatting_Request;

   overriding procedure On_PrepareCallHierarchy_Request
     (Self  : in out Server_Request_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.CallHierarchyPrepareParams) is
   begin
      LSP.Output_Tools.Write_Start_Request
        (Self.Output.all, "textDocument/prepareCallHierarchy", Id);
      Self.Output.Key_Name ("params");
      LSP.Outputs.Write_CallHierarchyPrepareParams (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_PrepareCallHierarchy_Request;

   overriding procedure On_PrepareRename_Request
     (Self  : in out Server_Request_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.PrepareRenameParams) is
   begin
      LSP.Output_Tools.Write_Start_Request
        (Self.Output.all, "textDocument/prepareRename", Id);
      Self.Output.Key_Name ("params");
      LSP.Outputs.Write_PrepareRenameParams (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_PrepareRename_Request;

   overriding procedure On_PrepareTypeHierarchy_Request
     (Self  : in out Server_Request_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.TypeHierarchyPrepareParams) is
   begin
      LSP.Output_Tools.Write_Start_Request
        (Self.Output.all, "textDocument/prepareTypeHierarchy", Id);
      Self.Output.Key_Name ("params");
      LSP.Outputs.Write_TypeHierarchyPrepareParams (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_PrepareTypeHierarchy_Request;

   overriding procedure On_RangeFormatting_Request
     (Self  : in out Server_Request_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.DocumentRangeFormattingParams) is
   begin
      LSP.Output_Tools.Write_Start_Request
        (Self.Output.all, "textDocument/rangeFormatting", Id);
      Self.Output.Key_Name ("params");
      LSP.Outputs.Write_DocumentRangeFormattingParams (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_RangeFormatting_Request;

   overriding procedure On_References_Request
     (Self  : in out Server_Request_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.ReferenceParams) is
   begin
      LSP.Output_Tools.Write_Start_Request
        (Self.Output.all, "textDocument/references", Id);
      Self.Output.Key_Name ("params");
      LSP.Outputs.Write_ReferenceParams (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_References_Request;

   overriding procedure On_Rename_Request
     (Self  : in out Server_Request_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.RenameParams) is
   begin
      LSP.Output_Tools.Write_Start_Request
        (Self.Output.all, "textDocument/rename", Id);
      Self.Output.Key_Name ("params");
      LSP.Outputs.Write_RenameParams (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_Rename_Request;

   overriding procedure On_SelectionRange_Request
     (Self  : in out Server_Request_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.SelectionRangeParams) is
   begin
      LSP.Output_Tools.Write_Start_Request
        (Self.Output.all, "textDocument/selectionRange", Id);
      Self.Output.Key_Name ("params");
      LSP.Outputs.Write_SelectionRangeParams (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_SelectionRange_Request;

   overriding procedure On_Full_Request
     (Self  : in out Server_Request_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.SemanticTokensParams) is
   begin
      LSP.Output_Tools.Write_Start_Request
        (Self.Output.all, "textDocument/semanticTokens/full", Id);
      Self.Output.Key_Name ("params");
      LSP.Outputs.Write_SemanticTokensParams (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_Full_Request;

   overriding procedure On_Tokens_Delta_Request
     (Self  : in out Server_Request_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.SemanticTokensDeltaParams) is
   begin
      LSP.Output_Tools.Write_Start_Request
        (Self.Output.all, "textDocument/semanticTokens/full/delta", Id);
      Self.Output.Key_Name ("params");
      LSP.Outputs.Write_SemanticTokensDeltaParams (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_Tokens_Delta_Request;

   overriding procedure On_Tokens_Range_Request
     (Self  : in out Server_Request_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.SemanticTokensRangeParams) is
   begin
      LSP.Output_Tools.Write_Start_Request
        (Self.Output.all, "textDocument/semanticTokens/range", Id);
      Self.Output.Key_Name ("params");
      LSP.Outputs.Write_SemanticTokensRangeParams (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_Tokens_Range_Request;

   overriding procedure On_SignatureHelp_Request
     (Self  : in out Server_Request_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.SignatureHelpParams) is
   begin
      LSP.Output_Tools.Write_Start_Request
        (Self.Output.all, "textDocument/signatureHelp", Id);
      Self.Output.Key_Name ("params");
      LSP.Outputs.Write_SignatureHelpParams (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_SignatureHelp_Request;

   overriding procedure On_TypeDefinition_Request
     (Self  : in out Server_Request_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.TypeDefinitionParams) is
   begin
      LSP.Output_Tools.Write_Start_Request
        (Self.Output.all, "textDocument/typeDefinition", Id);
      Self.Output.Key_Name ("params");
      LSP.Outputs.Write_TypeDefinitionParams (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_TypeDefinition_Request;

   overriding procedure On_WillSaveWaitUntil_Request
     (Self  : in out Server_Request_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.WillSaveTextDocumentParams) is
   begin
      LSP.Output_Tools.Write_Start_Request
        (Self.Output.all, "textDocument/willSaveWaitUntil", Id);
      Self.Output.Key_Name ("params");
      LSP.Outputs.Write_WillSaveTextDocumentParams (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_WillSaveWaitUntil_Request;

   overriding procedure On_Subtypes_Request
     (Self  : in out Server_Request_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.TypeHierarchySubtypesParams) is
   begin
      LSP.Output_Tools.Write_Start_Request
        (Self.Output.all, "typeHierarchy/subtypes", Id);
      Self.Output.Key_Name ("params");
      LSP.Outputs.Write_TypeHierarchySubtypesParams (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_Subtypes_Request;

   overriding procedure On_Supertypes_Request
     (Self  : in out Server_Request_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.TypeHierarchySupertypesParams) is
   begin
      LSP.Output_Tools.Write_Start_Request
        (Self.Output.all, "typeHierarchy/supertypes", Id);
      Self.Output.Key_Name ("params");
      LSP.Outputs.Write_TypeHierarchySupertypesParams (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_Supertypes_Request;

   overriding procedure On_Workspace_Diagnostic_Request
     (Self  : in out Server_Request_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.WorkspaceDiagnosticParams) is
   begin
      LSP.Output_Tools.Write_Start_Request
        (Self.Output.all, "workspace/diagnostic", Id);
      Self.Output.Key_Name ("params");
      LSP.Outputs.Write_WorkspaceDiagnosticParams (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_Workspace_Diagnostic_Request;

   overriding procedure On_ExecuteCommand_Request
     (Self  : in out Server_Request_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.ExecuteCommandParams) is
   begin
      LSP.Output_Tools.Write_Start_Request
        (Self.Output.all, "workspace/executeCommand", Id);
      Self.Output.Key_Name ("params");
      LSP.Outputs.Write_ExecuteCommandParams (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_ExecuteCommand_Request;

   overriding procedure On_Symbol_Request
     (Self  : in out Server_Request_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.WorkspaceSymbolParams) is
   begin
      LSP.Output_Tools.Write_Start_Request
        (Self.Output.all, "workspace/symbol", Id);
      Self.Output.Key_Name ("params");
      LSP.Outputs.Write_WorkspaceSymbolParams (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_Symbol_Request;

   overriding procedure On_WillCreateFiles_Request
     (Self  : in out Server_Request_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.CreateFilesParams) is
   begin
      LSP.Output_Tools.Write_Start_Request
        (Self.Output.all, "workspace/willCreateFiles", Id);
      Self.Output.Key_Name ("params");
      LSP.Outputs.Write_CreateFilesParams (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_WillCreateFiles_Request;

   overriding procedure On_WillDeleteFiles_Request
     (Self  : in out Server_Request_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.DeleteFilesParams) is
   begin
      LSP.Output_Tools.Write_Start_Request
        (Self.Output.all, "workspace/willDeleteFiles", Id);
      Self.Output.Key_Name ("params");
      LSP.Outputs.Write_DeleteFilesParams (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_WillDeleteFiles_Request;

   overriding procedure On_WillRenameFiles_Request
     (Self  : in out Server_Request_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.RenameFilesParams) is
   begin
      LSP.Output_Tools.Write_Start_Request
        (Self.Output.all, "workspace/willRenameFiles", Id);
      Self.Output.Key_Name ("params");
      LSP.Outputs.Write_RenameFilesParams (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_WillRenameFiles_Request;

   overriding procedure On_Symbol_Resolve_Request
     (Self  : in out Server_Request_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.WorkspaceSymbol) is
   begin
      LSP.Output_Tools.Write_Start_Request
        (Self.Output.all, "workspaceSymbol/resolve", Id);
      Self.Output.Key_Name ("params");
      LSP.Outputs.Write_WorkspaceSymbol (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_Symbol_Resolve_Request;

end LSP.Server_Request_Writers;
