--
--  Copyright (C) <YEAR>, <COPYRIGHT HOLDER>
--
--  SPDX-License-Identifier: MIT
--
--  DON'T EDIT THIS FILE! It was generated from metaModel.json.
--

with VSS.Strings;

package body LSP.Server_Request_Loggers is

   overriding procedure On_AlsCheckSyntax_Request
     (Self  : in out Server_Request_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.AlsCheckSyntaxParams) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'$/alsCheckSyntax'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" Params : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_AlsCheckSyntax_Request;

   overriding procedure On_IncomingCalls_Request
     (Self  : in out Server_Request_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.CallHierarchyIncomingCallsParams) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'callHierarchy/incomingCalls'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" Params : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_IncomingCalls_Request;

   overriding procedure On_OutgoingCalls_Request
     (Self  : in out Server_Request_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.CallHierarchyOutgoingCallsParams) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'callHierarchy/outgoingCalls'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" Params : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_OutgoingCalls_Request;

   overriding procedure On_Code_Action_Resolve_Request
     (Self  : in out Server_Request_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.CodeAction) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'codeAction/resolve'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" Params : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_Code_Action_Resolve_Request;

   overriding procedure On_Code_Lens_Resolve_Request
     (Self  : in out Server_Request_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.CodeLens) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'codeLens/resolve'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" Params : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_Code_Lens_Resolve_Request;

   overriding procedure On_Completion_Resolve_Request
     (Self  : in out Server_Request_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.CompletionItem) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'completionItem/resolve'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" Params : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_Completion_Resolve_Request;

   overriding procedure On_Link_Resolve_Request
     (Self  : in out Server_Request_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.DocumentLink) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'documentLink/resolve'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" Params : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_Link_Resolve_Request;

   overriding procedure On_Initialize_Request
     (Self  : in out Server_Request_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.InitializeParams) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'initialize'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" Params : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_Initialize_Request;

   overriding procedure On_Inlay_Resolve_Request
     (Self  : in out Server_Request_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.InlayHint) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'inlayHint/resolve'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" Params : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_Inlay_Resolve_Request;

   overriding procedure On_Shutdown_Request
     (Self : in out Server_Request_Logger;
      Id   : LSP.Structures.Integer_Or_Virtual_String) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'shutdown'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.New_Line (Ok);
   end On_Shutdown_Request;

   overriding procedure On_CodeAction_Request
     (Self  : in out Server_Request_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.CodeActionParams) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'textDocument/codeAction'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" Params : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_CodeAction_Request;

   overriding procedure On_CodeLens_Request
     (Self  : in out Server_Request_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.CodeLensParams) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'textDocument/codeLens'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" Params : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_CodeLens_Request;

   overriding procedure On_ColorPresentation_Request
     (Self  : in out Server_Request_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.ColorPresentationParams) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'textDocument/colorPresentation'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" Params : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_ColorPresentation_Request;

   overriding procedure On_Completion_Request
     (Self  : in out Server_Request_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.CompletionParams) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'textDocument/completion'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" Params : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_Completion_Request;

   overriding procedure On_Declaration_Request
     (Self  : in out Server_Request_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.DeclarationParams) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'textDocument/declaration'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" Params : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_Declaration_Request;

   overriding procedure On_Definition_Request
     (Self  : in out Server_Request_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.DefinitionParams) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'textDocument/definition'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" Params : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_Definition_Request;

   overriding procedure On_Diagnostic_Request
     (Self  : in out Server_Request_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.DocumentDiagnosticParams) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'textDocument/diagnostic'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" Params : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_Diagnostic_Request;

   overriding procedure On_DocumentColor_Request
     (Self  : in out Server_Request_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.DocumentColorParams) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'textDocument/documentColor'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" Params : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_DocumentColor_Request;

   overriding procedure On_DocumentHighlight_Request
     (Self  : in out Server_Request_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.DocumentHighlightParams) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'textDocument/documentHighlight'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" Params : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_DocumentHighlight_Request;

   overriding procedure On_DocumentLink_Request
     (Self  : in out Server_Request_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.DocumentLinkParams) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'textDocument/documentLink'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" Params : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_DocumentLink_Request;

   overriding procedure On_DocumentSymbol_Request
     (Self  : in out Server_Request_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.DocumentSymbolParams) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'textDocument/documentSymbol'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" Params : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_DocumentSymbol_Request;

   overriding procedure On_FoldingRange_Request
     (Self  : in out Server_Request_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.FoldingRangeParams) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'textDocument/foldingRange'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" Params : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_FoldingRange_Request;

   overriding procedure On_Formatting_Request
     (Self  : in out Server_Request_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.DocumentFormattingParams) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'textDocument/formatting'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" Params : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_Formatting_Request;

   overriding procedure On_Hover_Request
     (Self  : in out Server_Request_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.HoverParams) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'textDocument/hover'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" Params : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_Hover_Request;

   overriding procedure On_Implementation_Request
     (Self  : in out Server_Request_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.ImplementationParams) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'textDocument/implementation'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" Params : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_Implementation_Request;

   overriding procedure On_InlayHint_Request
     (Self  : in out Server_Request_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.InlayHintParams) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'textDocument/inlayHint'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" Params : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_InlayHint_Request;

   overriding procedure On_InlineValue_Request
     (Self  : in out Server_Request_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.InlineValueParams) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'textDocument/inlineValue'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" Params : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_InlineValue_Request;

   overriding procedure On_LinkedEditingRange_Request
     (Self  : in out Server_Request_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.LinkedEditingRangeParams) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'textDocument/linkedEditingRange'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" Params : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_LinkedEditingRange_Request;

   overriding procedure On_Moniker_Request
     (Self  : in out Server_Request_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.MonikerParams) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'textDocument/moniker'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" Params : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_Moniker_Request;

   overriding procedure On_OnTypeFormatting_Request
     (Self  : in out Server_Request_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.DocumentOnTypeFormattingParams) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'textDocument/onTypeFormatting'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" Params : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_OnTypeFormatting_Request;

   overriding procedure On_PrepareCallHierarchy_Request
     (Self  : in out Server_Request_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.CallHierarchyPrepareParams) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'textDocument/prepareCallHierarchy'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" Params : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_PrepareCallHierarchy_Request;

   overriding procedure On_PrepareRename_Request
     (Self  : in out Server_Request_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.PrepareRenameParams) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'textDocument/prepareRename'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" Params : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_PrepareRename_Request;

   overriding procedure On_PrepareTypeHierarchy_Request
     (Self  : in out Server_Request_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.TypeHierarchyPrepareParams) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'textDocument/prepareTypeHierarchy'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" Params : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_PrepareTypeHierarchy_Request;

   overriding procedure On_RangeFormatting_Request
     (Self  : in out Server_Request_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.DocumentRangeFormattingParams) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'textDocument/rangeFormatting'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" Params : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_RangeFormatting_Request;

   overriding procedure On_References_Request
     (Self  : in out Server_Request_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.ReferenceParams) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'textDocument/references'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" Params : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_References_Request;

   overriding procedure On_Rename_Request
     (Self  : in out Server_Request_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.RenameParams) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'textDocument/rename'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" Params : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_Rename_Request;

   overriding procedure On_SelectionRange_Request
     (Self  : in out Server_Request_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.SelectionRangeParams) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'textDocument/selectionRange'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" Params : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_SelectionRange_Request;

   overriding procedure On_Tokens_Full_Request
     (Self  : in out Server_Request_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.SemanticTokensParams) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'textDocument/semanticTokens/full'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" Params : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_Tokens_Full_Request;

   overriding procedure On_Tokens_Delta_Request
     (Self  : in out Server_Request_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.SemanticTokensDeltaParams) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'textDocument/semanticTokens/full/delta'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" Params : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_Tokens_Delta_Request;

   overriding procedure On_Tokens_Range_Request
     (Self  : in out Server_Request_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.SemanticTokensRangeParams) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'textDocument/semanticTokens/range'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" Params : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_Tokens_Range_Request;

   overriding procedure On_SignatureHelp_Request
     (Self  : in out Server_Request_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.SignatureHelpParams) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'textDocument/signatureHelp'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" Params : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_SignatureHelp_Request;

   overriding procedure On_TypeDefinition_Request
     (Self  : in out Server_Request_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.TypeDefinitionParams) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'textDocument/typeDefinition'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" Params : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_TypeDefinition_Request;

   overriding procedure On_WillSaveWaitUntil_Request
     (Self  : in out Server_Request_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.WillSaveTextDocumentParams) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'textDocument/willSaveWaitUntil'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" Params : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_WillSaveWaitUntil_Request;

   overriding procedure On_Subtypes_Request
     (Self  : in out Server_Request_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.TypeHierarchySubtypesParams) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'typeHierarchy/subtypes'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" Params : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_Subtypes_Request;

   overriding procedure On_Supertypes_Request
     (Self  : in out Server_Request_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.TypeHierarchySupertypesParams) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'typeHierarchy/supertypes'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" Params : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_Supertypes_Request;

   overriding procedure On_Workspace_Diagnostic_Request
     (Self  : in out Server_Request_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.WorkspaceDiagnosticParams) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'workspace/diagnostic'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" Params : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_Workspace_Diagnostic_Request;

   overriding procedure On_ExecuteCommand_Request
     (Self  : in out Server_Request_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.ExecuteCommandParams) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'workspace/executeCommand'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" Params : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_ExecuteCommand_Request;

   overriding procedure On_Symbol_Request
     (Self  : in out Server_Request_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.WorkspaceSymbolParams) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'workspace/symbol'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" Params : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_Symbol_Request;

   overriding procedure On_WillCreateFiles_Request
     (Self  : in out Server_Request_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.CreateFilesParams) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'workspace/willCreateFiles'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" Params : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_WillCreateFiles_Request;

   overriding procedure On_WillDeleteFiles_Request
     (Self  : in out Server_Request_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.DeleteFilesParams) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'workspace/willDeleteFiles'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" Params : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_WillDeleteFiles_Request;

   overriding procedure On_WillRenameFiles_Request
     (Self  : in out Server_Request_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.RenameFilesParams) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'workspace/willRenameFiles'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" Params : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_WillRenameFiles_Request;

   overriding procedure On_Symbol_Resolve_Request
     (Self  : in out Server_Request_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.WorkspaceSymbol) is
      Ok : Boolean := False;
   begin
      Self.Output.Put ("'workspaceSymbol/resolve'", Ok);
      Self.Put_Id (Id, Ok);
      Self.Output.Put (" Params : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_Symbol_Resolve_Request;

   procedure Put_Id
     (Self : in out Server_Request_Logger'Class;
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

end LSP.Server_Request_Loggers;
