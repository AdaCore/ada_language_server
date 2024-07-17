--
--  Copyright (C) <YEAR>, <COPYRIGHT HOLDER>
--
--  SPDX-License-Identifier: MIT
--
--  DON'T EDIT THIS FILE! It was generated from metaModel.json.
--

with VSS.Text_Streams;

with LSP.Errors;
with LSP.Structures;
with LSP.Client_Response_Receivers;

package LSP.Client_Response_Loggers is
   pragma Preelaborate;

   type Client_Response_Logger
     (Output : access VSS.Text_Streams.Output_Text_Stream'Class) is
   new LSP.Client_Response_Receivers.Client_Response_Receiver with null record;

   overriding procedure On_AlsCheckSyntax_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.AlsCheckSyntaxResult);

   overriding procedure On_IncomingCalls_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.CallHierarchyIncomingCall_Vector_Or_Null);

   overriding procedure On_OutgoingCalls_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.CallHierarchyOutgoingCall_Vector_Or_Null);

   overriding procedure On_Code_Action_Resolve_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.CodeAction);

   overriding procedure On_Code_Lens_Resolve_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.CodeLens);

   overriding procedure On_Completion_Resolve_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.CompletionItem);

   overriding procedure On_Link_Resolve_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.DocumentLink);

   overriding procedure On_Initialize_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.InitializeResult);

   overriding procedure On_Inlay_Resolve_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.InlayHint);

   overriding procedure On_Shutdown_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Null_Record);

   overriding procedure On_CodeAction_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Command_Or_CodeAction_Vector_Or_Null);

   overriding procedure On_CodeLens_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.CodeLens_Vector_Or_Null);

   overriding procedure On_ColorPresentation_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.ColorPresentation_Vector);

   overriding procedure On_Completion_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Completion_Result);

   overriding procedure On_Declaration_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Declaration_Result);

   overriding procedure On_Definition_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Definition_Result);

   overriding procedure On_Diagnostic_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.DocumentDiagnosticReport);

   overriding procedure On_DocumentColor_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.ColorInformation_Vector);

   overriding procedure On_DocumentHighlight_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.DocumentHighlight_Vector_Or_Null);

   overriding procedure On_DocumentLink_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.DocumentLink_Vector_Or_Null);

   overriding procedure On_DocumentSymbol_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.DocumentSymbol_Result);

   overriding procedure On_FoldingRange_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.FoldingRange_Vector_Or_Null);

   overriding procedure On_Formatting_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.TextEdit_Vector_Or_Null);

   overriding procedure On_Hover_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Hover_Or_Null);

   overriding procedure On_Implementation_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Definition_Result);

   overriding procedure On_InlayHint_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.InlayHint_Vector_Or_Null);

   overriding procedure On_InlineValue_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.InlineValue_Vector_Or_Null);

   overriding procedure On_LinkedEditingRange_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.LinkedEditingRanges_Or_Null);

   overriding procedure On_Moniker_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Moniker_Vector_Or_Null);

   overriding procedure On_OnTypeFormatting_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.TextEdit_Vector_Or_Null);

   overriding procedure On_PrepareCallHierarchy_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.CallHierarchyItem_Vector_Or_Null);

   overriding procedure On_PrepareRename_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.PrepareRenameResult_Or_Null);

   overriding procedure On_PrepareTypeHierarchy_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.TypeHierarchyItem_Vector_Or_Null);

   overriding procedure On_RangeFormatting_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.TextEdit_Vector_Or_Null);

   overriding procedure On_References_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Location_Vector_Or_Null);

   overriding procedure On_Rename_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.WorkspaceEdit_Or_Null);

   overriding procedure On_SelectionRange_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.SelectionRange_Vector_Or_Null);

   overriding procedure On_Tokens_Full_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.SemanticTokens_Or_Null);

   overriding procedure On_Tokens_Delta_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Tokens_Delta_Result);

   overriding procedure On_Tokens_Range_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.SemanticTokens_Or_Null);

   overriding procedure On_SignatureHelp_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.SignatureHelp_Or_Null);

   overriding procedure On_TypeDefinition_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Definition_Result);

   overriding procedure On_WillSaveWaitUntil_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.TextEdit_Vector_Or_Null);

   overriding procedure On_Subtypes_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.TypeHierarchyItem_Vector_Or_Null);

   overriding procedure On_Supertypes_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.TypeHierarchyItem_Vector_Or_Null);

   overriding procedure On_Workspace_Diagnostic_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.WorkspaceDiagnosticReport);

   overriding procedure On_ExecuteCommand_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.LSPAny_Or_Null);

   overriding procedure On_Symbol_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Symbol_Result);

   overriding procedure On_WillCreateFiles_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.WorkspaceEdit_Or_Null);

   overriding procedure On_WillDeleteFiles_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.WorkspaceEdit_Or_Null);

   overriding procedure On_WillRenameFiles_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.WorkspaceEdit_Or_Null);

   overriding procedure On_Symbol_Resolve_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.WorkspaceSymbol);

   overriding procedure On_Error_Response
     (Self  : in out Client_Response_Logger;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Errors.ResponseError);

   procedure Put_Id
     (Self : in out Client_Response_Logger'Class;
      Id   : LSP.Structures.Integer_Or_Virtual_String;
      Ok   : in out Boolean);

end LSP.Client_Response_Loggers;
