--
--  Copyright (C) <YEAR>, <COPYRIGHT HOLDER>
--
--  SPDX-License-Identifier: MIT
--
--  DON'T EDIT THIS FILE! It was generated from metaModel.json.
--

with VSS.Strings;

package body LSP.Client_Response_Loggers is

   overriding procedure On_AlsCheckSyntax_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.AlsCheckSyntaxResult) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'$/alsCheckSyntax'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" result : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_AlsCheckSyntax_Response;

   overriding procedure On_IncomingCalls_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.CallHierarchyIncomingCall_Vector_Or_Null) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'callHierarchy/incomingCalls'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" result : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_IncomingCalls_Response;

   overriding procedure On_OutgoingCalls_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.CallHierarchyOutgoingCall_Vector_Or_Null) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'callHierarchy/outgoingCalls'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" result : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_OutgoingCalls_Response;

   overriding procedure On_Code_Action_Resolve_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.CodeAction) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'codeAction/resolve'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" result : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_Code_Action_Resolve_Response;

   overriding procedure On_Code_Lens_Resolve_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.CodeLens) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'codeLens/resolve'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" result : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_Code_Lens_Resolve_Response;

   overriding procedure On_Completion_Resolve_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.CompletionItem) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'completionItem/resolve'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" result : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_Completion_Resolve_Response;

   overriding procedure On_Link_Resolve_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.DocumentLink) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'documentLink/resolve'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" result : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_Link_Resolve_Response;

   overriding procedure On_Initialize_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.InitializeResult) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'initialize'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" result : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_Initialize_Response;

   overriding procedure On_Inlay_Resolve_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.InlayHint) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'inlayHint/resolve'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" result : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_Inlay_Resolve_Response;

   overriding procedure On_Shutdown_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Null_Record) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'shutdown'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" result : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_Shutdown_Response;

   overriding procedure On_CodeAction_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Command_Or_CodeAction_Vector_Or_Null) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'textDocument/codeAction'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" result : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_CodeAction_Response;

   overriding procedure On_CodeLens_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.CodeLens_Vector_Or_Null) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'textDocument/codeLens'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" result : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_CodeLens_Response;

   overriding procedure On_ColorPresentation_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.ColorPresentation_Vector) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'textDocument/colorPresentation'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" result : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_ColorPresentation_Response;

   overriding procedure On_Completion_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Completion_Result) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'textDocument/completion'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" result : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_Completion_Response;

   overriding procedure On_Declaration_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Declaration_Result) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'textDocument/declaration'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" result : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_Declaration_Response;

   overriding procedure On_Definition_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Definition_Result) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'textDocument/definition'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" result : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_Definition_Response;

   overriding procedure On_Diagnostic_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.DocumentDiagnosticReport) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'textDocument/diagnostic'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" result : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_Diagnostic_Response;

   overriding procedure On_DocumentColor_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.ColorInformation_Vector) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'textDocument/documentColor'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" result : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_DocumentColor_Response;

   overriding procedure On_DocumentHighlight_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.DocumentHighlight_Vector_Or_Null) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'textDocument/documentHighlight'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" result : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_DocumentHighlight_Response;

   overriding procedure On_DocumentLink_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.DocumentLink_Vector_Or_Null) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'textDocument/documentLink'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" result : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_DocumentLink_Response;

   overriding procedure On_DocumentSymbol_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.DocumentSymbol_Result) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'textDocument/documentSymbol'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" result : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_DocumentSymbol_Response;

   overriding procedure On_FoldingRange_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.FoldingRange_Vector_Or_Null) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'textDocument/foldingRange'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" result : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_FoldingRange_Response;

   overriding procedure On_Formatting_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.TextEdit_Vector_Or_Null) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'textDocument/formatting'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" result : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_Formatting_Response;

   overriding procedure On_Hover_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Hover_Or_Null) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'textDocument/hover'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" result : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_Hover_Response;

   overriding procedure On_Implementation_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Definition_Result) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'textDocument/implementation'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" result : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_Implementation_Response;

   overriding procedure On_InlayHint_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.InlayHint_Vector_Or_Null) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'textDocument/inlayHint'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" result : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_InlayHint_Response;

   overriding procedure On_InlineValue_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.InlineValue_Vector_Or_Null) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'textDocument/inlineValue'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" result : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_InlineValue_Response;

   overriding procedure On_LinkedEditingRange_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.LinkedEditingRanges_Or_Null) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'textDocument/linkedEditingRange'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" result : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_LinkedEditingRange_Response;

   overriding procedure On_Moniker_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Moniker_Vector_Or_Null) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'textDocument/moniker'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" result : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_Moniker_Response;

   overriding procedure On_OnTypeFormatting_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.TextEdit_Vector_Or_Null) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'textDocument/onTypeFormatting'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" result : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_OnTypeFormatting_Response;

   overriding procedure On_PrepareCallHierarchy_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.CallHierarchyItem_Vector_Or_Null) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'textDocument/prepareCallHierarchy'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" result : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_PrepareCallHierarchy_Response;

   overriding procedure On_PrepareRename_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.PrepareRenameResult_Or_Null) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'textDocument/prepareRename'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" result : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_PrepareRename_Response;

   overriding procedure On_PrepareTypeHierarchy_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.TypeHierarchyItem_Vector_Or_Null) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'textDocument/prepareTypeHierarchy'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" result : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_PrepareTypeHierarchy_Response;

   overriding procedure On_RangeFormatting_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.TextEdit_Vector_Or_Null) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'textDocument/rangeFormatting'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" result : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_RangeFormatting_Response;

   overriding procedure On_References_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Location_Vector_Or_Null) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'textDocument/references'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" result : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_References_Response;

   overriding procedure On_Rename_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.WorkspaceEdit_Or_Null) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'textDocument/rename'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" result : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_Rename_Response;

   overriding procedure On_SelectionRange_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.SelectionRange_Vector_Or_Null) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'textDocument/selectionRange'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" result : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_SelectionRange_Response;

   overriding procedure On_Tokens_Full_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.SemanticTokens_Or_Null) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'textDocument/semanticTokens/full'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" result : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_Tokens_Full_Response;

   overriding procedure On_Tokens_Delta_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Tokens_Delta_Result) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'textDocument/semanticTokens/full/delta'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" result : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_Tokens_Delta_Response;

   overriding procedure On_Tokens_Range_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.SemanticTokens_Or_Null) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'textDocument/semanticTokens/range'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" result : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_Tokens_Range_Response;

   overriding procedure On_SignatureHelp_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.SignatureHelp_Or_Null) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'textDocument/signatureHelp'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" result : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_SignatureHelp_Response;

   overriding procedure On_TypeDefinition_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Definition_Result) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'textDocument/typeDefinition'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" result : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_TypeDefinition_Response;

   overriding procedure On_WillSaveWaitUntil_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.TextEdit_Vector_Or_Null) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'textDocument/willSaveWaitUntil'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" result : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_WillSaveWaitUntil_Response;

   overriding procedure On_Subtypes_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.TypeHierarchyItem_Vector_Or_Null) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'typeHierarchy/subtypes'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" result : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_Subtypes_Response;

   overriding procedure On_Supertypes_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.TypeHierarchyItem_Vector_Or_Null) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'typeHierarchy/supertypes'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" result : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_Supertypes_Response;

   overriding procedure On_Workspace_Diagnostic_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.WorkspaceDiagnosticReport) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'workspace/diagnostic'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" result : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_Workspace_Diagnostic_Response;

   overriding procedure On_ExecuteCommand_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.LSPAny_Or_Null) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'workspace/executeCommand'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" result : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_ExecuteCommand_Response;

   overriding procedure On_Symbol_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Symbol_Result) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'workspace/symbol'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" result : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_Symbol_Response;

   overriding procedure On_WillCreateFiles_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.WorkspaceEdit_Or_Null) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'workspace/willCreateFiles'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" result : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_WillCreateFiles_Response;

   overriding procedure On_WillDeleteFiles_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.WorkspaceEdit_Or_Null) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'workspace/willDeleteFiles'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" result : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_WillDeleteFiles_Response;

   overriding procedure On_WillRenameFiles_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.WorkspaceEdit_Or_Null) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'workspace/willRenameFiles'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" result : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_WillRenameFiles_Response;

   overriding procedure On_Symbol_Resolve_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.WorkspaceSymbol) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'workspaceSymbol/resolve'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" result : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_Symbol_Resolve_Response;

   overriding procedure On_Error_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Errors.ResponseError) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'Error response'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" error : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_Error_Response;

   procedure Put_Id
     (Self : in out Client_Response_Logger'Class;
      Id   : LSP.Structures.Integer_Or_Virtual_String;
      Ok   : in out Boolean) is
   begin
      Self.Output.Put (" Id=", Ok);

      if Id.Is_Integer then
         Self.Output.Put
           (VSS.Strings.To_Virtual_String (Id.Integer'Wide_Wide_Image), Ok);
      else
         Self.Output.Put (Id.Virtual_String, Ok);
      end if;
   end Put_Id;

end LSP.Client_Response_Loggers;
