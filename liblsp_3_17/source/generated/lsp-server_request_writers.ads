--
--  Copyright (C) <YEAR>, <COPYRIGHT HOLDER>
--
--  SPDX-License-Identifier: MIT
--
--  DON'T EDIT THIS FILE! It was generated from metaModel.json.
--

with LSP.Structures;
with VSS.JSON.Content_Handlers;
with LSP.Server_Request_Receivers;

package LSP.Server_Request_Writers is
   pragma Preelaborate;

   type Server_Request_Writer
     (Output : access VSS.JSON.Content_Handlers.JSON_Content_Handler'Class) is
   new LSP.Server_Request_Receivers.Server_Request_Receiver with null record;

   overriding procedure On_AlsCheckSyntax_Request
     (Self  : in out Server_Request_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.AlsCheckSyntaxParams);

   overriding procedure On_IncomingCalls_Request
     (Self  : in out Server_Request_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.CallHierarchyIncomingCallsParams);

   overriding procedure On_OutgoingCalls_Request
     (Self  : in out Server_Request_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.CallHierarchyOutgoingCallsParams);

   overriding procedure On_Code_Action_Resolve_Request
     (Self  : in out Server_Request_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.CodeAction);

   overriding procedure On_Code_Lens_Resolve_Request
     (Self  : in out Server_Request_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.CodeLens);

   overriding procedure On_Completion_Resolve_Request
     (Self  : in out Server_Request_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.CompletionItem);

   overriding procedure On_Link_Resolve_Request
     (Self  : in out Server_Request_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.DocumentLink);

   overriding procedure On_Initialize_Request
     (Self  : in out Server_Request_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.InitializeParams);

   overriding procedure On_Inlay_Resolve_Request
     (Self  : in out Server_Request_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.InlayHint);

   overriding procedure On_Shutdown_Request
     (Self : in out Server_Request_Writer;
      Id   : LSP.Structures.Integer_Or_Virtual_String);

   overriding procedure On_CodeAction_Request
     (Self  : in out Server_Request_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.CodeActionParams);

   overriding procedure On_CodeLens_Request
     (Self  : in out Server_Request_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.CodeLensParams);

   overriding procedure On_ColorPresentation_Request
     (Self  : in out Server_Request_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.ColorPresentationParams);

   overriding procedure On_Completion_Request
     (Self  : in out Server_Request_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.CompletionParams);

   overriding procedure On_Declaration_Request
     (Self  : in out Server_Request_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.DeclarationParams);

   overriding procedure On_Definition_Request
     (Self  : in out Server_Request_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.DefinitionParams);

   overriding procedure On_Diagnostic_Request
     (Self  : in out Server_Request_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.DocumentDiagnosticParams);

   overriding procedure On_DocumentColor_Request
     (Self  : in out Server_Request_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.DocumentColorParams);

   overriding procedure On_DocumentHighlight_Request
     (Self  : in out Server_Request_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.DocumentHighlightParams);

   overriding procedure On_DocumentLink_Request
     (Self  : in out Server_Request_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.DocumentLinkParams);

   overriding procedure On_DocumentSymbol_Request
     (Self  : in out Server_Request_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.DocumentSymbolParams);

   overriding procedure On_FoldingRange_Request
     (Self  : in out Server_Request_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.FoldingRangeParams);

   overriding procedure On_Formatting_Request
     (Self  : in out Server_Request_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.DocumentFormattingParams);

   overriding procedure On_Hover_Request
     (Self  : in out Server_Request_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.HoverParams);

   overriding procedure On_Implementation_Request
     (Self  : in out Server_Request_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.ImplementationParams);

   overriding procedure On_InlayHint_Request
     (Self  : in out Server_Request_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.InlayHintParams);

   overriding procedure On_InlineValue_Request
     (Self  : in out Server_Request_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.InlineValueParams);

   overriding procedure On_LinkedEditingRange_Request
     (Self  : in out Server_Request_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.LinkedEditingRangeParams);

   overriding procedure On_Moniker_Request
     (Self  : in out Server_Request_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.MonikerParams);

   overriding procedure On_OnTypeFormatting_Request
     (Self  : in out Server_Request_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.DocumentOnTypeFormattingParams);

   overriding procedure On_PrepareCallHierarchy_Request
     (Self  : in out Server_Request_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.CallHierarchyPrepareParams);

   overriding procedure On_PrepareRename_Request
     (Self  : in out Server_Request_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.PrepareRenameParams);

   overriding procedure On_PrepareTypeHierarchy_Request
     (Self  : in out Server_Request_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.TypeHierarchyPrepareParams);

   overriding procedure On_RangeFormatting_Request
     (Self  : in out Server_Request_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.DocumentRangeFormattingParams);

   overriding procedure On_References_Request
     (Self  : in out Server_Request_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.ReferenceParams);

   overriding procedure On_Rename_Request
     (Self  : in out Server_Request_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.RenameParams);

   overriding procedure On_SelectionRange_Request
     (Self  : in out Server_Request_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.SelectionRangeParams);

   overriding procedure On_Tokens_Full_Request
     (Self  : in out Server_Request_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.SemanticTokensParams);

   overriding procedure On_Tokens_Delta_Request
     (Self  : in out Server_Request_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.SemanticTokensDeltaParams);

   overriding procedure On_Tokens_Range_Request
     (Self  : in out Server_Request_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.SemanticTokensRangeParams);

   overriding procedure On_SignatureHelp_Request
     (Self  : in out Server_Request_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.SignatureHelpParams);

   overriding procedure On_TypeDefinition_Request
     (Self  : in out Server_Request_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.TypeDefinitionParams);

   overriding procedure On_WillSaveWaitUntil_Request
     (Self  : in out Server_Request_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.WillSaveTextDocumentParams);

   overriding procedure On_Subtypes_Request
     (Self  : in out Server_Request_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.TypeHierarchySubtypesParams);

   overriding procedure On_Supertypes_Request
     (Self  : in out Server_Request_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.TypeHierarchySupertypesParams);

   overriding procedure On_Workspace_Diagnostic_Request
     (Self  : in out Server_Request_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.WorkspaceDiagnosticParams);

   overriding procedure On_ExecuteCommand_Request
     (Self  : in out Server_Request_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.ExecuteCommandParams);

   overriding procedure On_Symbol_Request
     (Self  : in out Server_Request_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.WorkspaceSymbolParams);

   overriding procedure On_WillCreateFiles_Request
     (Self  : in out Server_Request_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.CreateFilesParams);

   overriding procedure On_WillDeleteFiles_Request
     (Self  : in out Server_Request_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.DeleteFilesParams);

   overriding procedure On_WillRenameFiles_Request
     (Self  : in out Server_Request_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.RenameFilesParams);

   overriding procedure On_Symbol_Resolve_Request
     (Self  : in out Server_Request_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.WorkspaceSymbol);

end LSP.Server_Request_Writers;
