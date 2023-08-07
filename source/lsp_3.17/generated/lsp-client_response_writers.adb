--
--  Copyright (C) <YEAR>, <COPYRIGHT HOLDER>
--
--  SPDX-License-Identifier: MIT
--

with LSP.Output_Tools;
with LSP.Outputs;

package body LSP.Client_Response_Writers is

   overriding procedure On_IncomingCalls_Response
     (Self  : in out Client_Response_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.CallHierarchyIncomingCall_Vector_Or_Null) is
   begin
      LSP.Output_Tools.Write_Start_Response (Self.Output.all, Id);
      Self.Output.Key_Name ("result");
      LSP.Outputs.Write_CallHierarchyIncomingCall_Vector_Or_Null
        (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_IncomingCalls_Response;

   overriding procedure On_OutgoingCalls_Response
     (Self  : in out Client_Response_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.CallHierarchyOutgoingCall_Vector_Or_Null) is
   begin
      LSP.Output_Tools.Write_Start_Response (Self.Output.all, Id);
      Self.Output.Key_Name ("result");
      LSP.Outputs.Write_CallHierarchyOutgoingCall_Vector_Or_Null
        (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_OutgoingCalls_Response;

   overriding procedure On_Code_Action_Resolve_Response
     (Self  : in out Client_Response_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.CodeAction) is
   begin
      LSP.Output_Tools.Write_Start_Response (Self.Output.all, Id);
      Self.Output.Key_Name ("result");
      LSP.Outputs.Write_CodeAction (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_Code_Action_Resolve_Response;

   overriding procedure On_Code_Lens_Resolve_Response
     (Self  : in out Client_Response_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.CodeLens) is
   begin
      LSP.Output_Tools.Write_Start_Response (Self.Output.all, Id);
      Self.Output.Key_Name ("result");
      LSP.Outputs.Write_CodeLens (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_Code_Lens_Resolve_Response;

   overriding procedure On_Completion_Resolve_Response
     (Self  : in out Client_Response_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.CompletionItem) is
   begin
      LSP.Output_Tools.Write_Start_Response (Self.Output.all, Id);
      Self.Output.Key_Name ("result");
      LSP.Outputs.Write_CompletionItem (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_Completion_Resolve_Response;

   overriding procedure On_Link_Resolve_Response
     (Self  : in out Client_Response_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.DocumentLink) is
   begin
      LSP.Output_Tools.Write_Start_Response (Self.Output.all, Id);
      Self.Output.Key_Name ("result");
      LSP.Outputs.Write_DocumentLink (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_Link_Resolve_Response;

   overriding procedure On_Initialize_Response
     (Self  : in out Client_Response_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.InitializeResult) is
   begin
      LSP.Output_Tools.Write_Start_Response (Self.Output.all, Id);
      Self.Output.Key_Name ("result");
      LSP.Outputs.Write_InitializeResult (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_Initialize_Response;

   overriding procedure On_Inlay_Resolve_Response
     (Self  : in out Client_Response_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.InlayHint) is
   begin
      LSP.Output_Tools.Write_Start_Response (Self.Output.all, Id);
      Self.Output.Key_Name ("result");
      LSP.Outputs.Write_InlayHint (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_Inlay_Resolve_Response;

   overriding procedure On_Shutdown_Response
     (Self  : in out Client_Response_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Null_Record) is
   begin
      LSP.Output_Tools.Write_Start_Response (Self.Output.all, Id);
      Self.Output.Key_Name ("result");
      LSP.Outputs.Write_Null_Record (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_Shutdown_Response;

   overriding procedure On_CodeAction_Response
     (Self  : in out Client_Response_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Command_Or_CodeAction_Vector_Or_Null) is
   begin
      LSP.Output_Tools.Write_Start_Response (Self.Output.all, Id);
      Self.Output.Key_Name ("result");
      LSP.Outputs.Write_Command_Or_CodeAction_Vector_Or_Null
        (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_CodeAction_Response;

   overriding procedure On_CodeLens_Response
     (Self  : in out Client_Response_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.CodeLens_Vector_Or_Null) is
   begin
      LSP.Output_Tools.Write_Start_Response (Self.Output.all, Id);
      Self.Output.Key_Name ("result");
      LSP.Outputs.Write_CodeLens_Vector_Or_Null (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_CodeLens_Response;

   overriding procedure On_ColorPresentation_Response
     (Self  : in out Client_Response_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.ColorPresentation_Vector) is
   begin
      LSP.Output_Tools.Write_Start_Response (Self.Output.all, Id);
      Self.Output.Key_Name ("result");
      LSP.Outputs.Write_ColorPresentation_Vector (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_ColorPresentation_Response;

   overriding procedure On_Completion_Response
     (Self  : in out Client_Response_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Completion_Result) is
   begin
      LSP.Output_Tools.Write_Start_Response (Self.Output.all, Id);
      Self.Output.Key_Name ("result");
      LSP.Outputs.Write_Completion_Result (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_Completion_Response;

   overriding procedure On_Declaration_Response
     (Self  : in out Client_Response_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Declaration_Result) is
   begin
      LSP.Output_Tools.Write_Start_Response (Self.Output.all, Id);
      Self.Output.Key_Name ("result");
      LSP.Outputs.Write_Declaration_Result (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_Declaration_Response;

   overriding procedure On_Definition_Response
     (Self  : in out Client_Response_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Definition_Result) is
   begin
      LSP.Output_Tools.Write_Start_Response (Self.Output.all, Id);
      Self.Output.Key_Name ("result");
      LSP.Outputs.Write_Definition_Result (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_Definition_Response;

   overriding procedure On_Diagnostic_Response
     (Self  : in out Client_Response_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.DocumentDiagnosticReport) is
   begin
      LSP.Output_Tools.Write_Start_Response (Self.Output.all, Id);
      Self.Output.Key_Name ("result");
      LSP.Outputs.Write_DocumentDiagnosticReport (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_Diagnostic_Response;

   overriding procedure On_DocumentColor_Response
     (Self  : in out Client_Response_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.ColorInformation_Vector) is
   begin
      LSP.Output_Tools.Write_Start_Response (Self.Output.all, Id);
      Self.Output.Key_Name ("result");
      LSP.Outputs.Write_ColorInformation_Vector (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_DocumentColor_Response;

   overriding procedure On_DocumentHighlight_Response
     (Self  : in out Client_Response_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.DocumentHighlight_Vector_Or_Null) is
   begin
      LSP.Output_Tools.Write_Start_Response (Self.Output.all, Id);
      Self.Output.Key_Name ("result");
      LSP.Outputs.Write_DocumentHighlight_Vector_Or_Null
        (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_DocumentHighlight_Response;

   overriding procedure On_DocumentLink_Response
     (Self  : in out Client_Response_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.DocumentLink_Vector_Or_Null) is
   begin
      LSP.Output_Tools.Write_Start_Response (Self.Output.all, Id);
      Self.Output.Key_Name ("result");
      LSP.Outputs.Write_DocumentLink_Vector_Or_Null (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_DocumentLink_Response;

   overriding procedure On_DocumentSymbol_Response
     (Self  : in out Client_Response_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.DocumentSymbol_Result) is
   begin
      LSP.Output_Tools.Write_Start_Response (Self.Output.all, Id);
      Self.Output.Key_Name ("result");
      LSP.Outputs.Write_DocumentSymbol_Result (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_DocumentSymbol_Response;

   overriding procedure On_FoldingRange_Response
     (Self  : in out Client_Response_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.FoldingRange_Vector_Or_Null) is
   begin
      LSP.Output_Tools.Write_Start_Response (Self.Output.all, Id);
      Self.Output.Key_Name ("result");
      LSP.Outputs.Write_FoldingRange_Vector_Or_Null (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_FoldingRange_Response;

   overriding procedure On_Formatting_Response
     (Self  : in out Client_Response_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.TextEdit_Vector_Or_Null) is
   begin
      LSP.Output_Tools.Write_Start_Response (Self.Output.all, Id);
      Self.Output.Key_Name ("result");
      LSP.Outputs.Write_TextEdit_Vector_Or_Null (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_Formatting_Response;

   overriding procedure On_Hover_Response
     (Self  : in out Client_Response_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Hover_Or_Null) is
   begin
      LSP.Output_Tools.Write_Start_Response (Self.Output.all, Id);
      Self.Output.Key_Name ("result");
      LSP.Outputs.Write_Hover_Or_Null (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_Hover_Response;

   overriding procedure On_Implementation_Response
     (Self  : in out Client_Response_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Definition_Result) is
   begin
      LSP.Output_Tools.Write_Start_Response (Self.Output.all, Id);
      Self.Output.Key_Name ("result");
      LSP.Outputs.Write_Definition_Result (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_Implementation_Response;

   overriding procedure On_InlayHint_Response
     (Self  : in out Client_Response_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.InlayHint_Vector_Or_Null) is
   begin
      LSP.Output_Tools.Write_Start_Response (Self.Output.all, Id);
      Self.Output.Key_Name ("result");
      LSP.Outputs.Write_InlayHint_Vector_Or_Null (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_InlayHint_Response;

   overriding procedure On_InlineValue_Response
     (Self  : in out Client_Response_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.InlineValue_Vector_Or_Null) is
   begin
      LSP.Output_Tools.Write_Start_Response (Self.Output.all, Id);
      Self.Output.Key_Name ("result");
      LSP.Outputs.Write_InlineValue_Vector_Or_Null (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_InlineValue_Response;

   overriding procedure On_LinkedEditingRange_Response
     (Self  : in out Client_Response_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.LinkedEditingRanges_Or_Null) is
   begin
      LSP.Output_Tools.Write_Start_Response (Self.Output.all, Id);
      Self.Output.Key_Name ("result");
      LSP.Outputs.Write_LinkedEditingRanges_Or_Null (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_LinkedEditingRange_Response;

   overriding procedure On_Moniker_Response
     (Self  : in out Client_Response_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Moniker_Vector_Or_Null) is
   begin
      LSP.Output_Tools.Write_Start_Response (Self.Output.all, Id);
      Self.Output.Key_Name ("result");
      LSP.Outputs.Write_Moniker_Vector_Or_Null (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_Moniker_Response;

   overriding procedure On_OnTypeFormatting_Response
     (Self  : in out Client_Response_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.TextEdit_Vector_Or_Null) is
   begin
      LSP.Output_Tools.Write_Start_Response (Self.Output.all, Id);
      Self.Output.Key_Name ("result");
      LSP.Outputs.Write_TextEdit_Vector_Or_Null (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_OnTypeFormatting_Response;

   overriding procedure On_PrepareCallHierarchy_Response
     (Self  : in out Client_Response_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.CallHierarchyItem_Vector_Or_Null) is
   begin
      LSP.Output_Tools.Write_Start_Response (Self.Output.all, Id);
      Self.Output.Key_Name ("result");
      LSP.Outputs.Write_CallHierarchyItem_Vector_Or_Null
        (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_PrepareCallHierarchy_Response;

   overriding procedure On_PrepareRename_Response
     (Self  : in out Client_Response_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.PrepareRenameResult_Or_Null) is
   begin
      LSP.Output_Tools.Write_Start_Response (Self.Output.all, Id);
      Self.Output.Key_Name ("result");
      LSP.Outputs.Write_PrepareRenameResult_Or_Null (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_PrepareRename_Response;

   overriding procedure On_PrepareTypeHierarchy_Response
     (Self  : in out Client_Response_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.TypeHierarchyItem_Vector_Or_Null) is
   begin
      LSP.Output_Tools.Write_Start_Response (Self.Output.all, Id);
      Self.Output.Key_Name ("result");
      LSP.Outputs.Write_TypeHierarchyItem_Vector_Or_Null
        (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_PrepareTypeHierarchy_Response;

   overriding procedure On_RangeFormatting_Response
     (Self  : in out Client_Response_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.TextEdit_Vector_Or_Null) is
   begin
      LSP.Output_Tools.Write_Start_Response (Self.Output.all, Id);
      Self.Output.Key_Name ("result");
      LSP.Outputs.Write_TextEdit_Vector_Or_Null (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_RangeFormatting_Response;

   overriding procedure On_References_Response
     (Self  : in out Client_Response_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Location_Vector_Or_Null) is
   begin
      LSP.Output_Tools.Write_Start_Response (Self.Output.all, Id);
      Self.Output.Key_Name ("result");
      LSP.Outputs.Write_Location_Vector_Or_Null (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_References_Response;

   overriding procedure On_Rename_Response
     (Self  : in out Client_Response_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.WorkspaceEdit_Or_Null) is
   begin
      LSP.Output_Tools.Write_Start_Response (Self.Output.all, Id);
      Self.Output.Key_Name ("result");
      LSP.Outputs.Write_WorkspaceEdit_Or_Null (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_Rename_Response;

   overriding procedure On_SelectionRange_Response
     (Self  : in out Client_Response_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.SelectionRange_Vector_Or_Null) is
   begin
      LSP.Output_Tools.Write_Start_Response (Self.Output.all, Id);
      Self.Output.Key_Name ("result");
      LSP.Outputs.Write_SelectionRange_Vector_Or_Null (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_SelectionRange_Response;

   overriding procedure On_Full_Response
     (Self  : in out Client_Response_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.SemanticTokens_Or_Null) is
   begin
      LSP.Output_Tools.Write_Start_Response (Self.Output.all, Id);
      Self.Output.Key_Name ("result");
      LSP.Outputs.Write_SemanticTokens_Or_Null (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_Full_Response;

   overriding procedure On_Tokens_Delta_Response
     (Self  : in out Client_Response_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Tokens_Delta_Result) is
   begin
      LSP.Output_Tools.Write_Start_Response (Self.Output.all, Id);
      Self.Output.Key_Name ("result");
      LSP.Outputs.Write_Tokens_Delta_Result (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_Tokens_Delta_Response;

   overriding procedure On_Tokens_Range_Response
     (Self  : in out Client_Response_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.SemanticTokens_Or_Null) is
   begin
      LSP.Output_Tools.Write_Start_Response (Self.Output.all, Id);
      Self.Output.Key_Name ("result");
      LSP.Outputs.Write_SemanticTokens_Or_Null (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_Tokens_Range_Response;

   overriding procedure On_SignatureHelp_Response
     (Self  : in out Client_Response_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.SignatureHelp_Or_Null) is
   begin
      LSP.Output_Tools.Write_Start_Response (Self.Output.all, Id);
      Self.Output.Key_Name ("result");
      LSP.Outputs.Write_SignatureHelp_Or_Null (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_SignatureHelp_Response;

   overriding procedure On_TypeDefinition_Response
     (Self  : in out Client_Response_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Definition_Result) is
   begin
      LSP.Output_Tools.Write_Start_Response (Self.Output.all, Id);
      Self.Output.Key_Name ("result");
      LSP.Outputs.Write_Definition_Result (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_TypeDefinition_Response;

   overriding procedure On_WillSaveWaitUntil_Response
     (Self  : in out Client_Response_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.TextEdit_Vector_Or_Null) is
   begin
      LSP.Output_Tools.Write_Start_Response (Self.Output.all, Id);
      Self.Output.Key_Name ("result");
      LSP.Outputs.Write_TextEdit_Vector_Or_Null (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_WillSaveWaitUntil_Response;

   overriding procedure On_Subtypes_Response
     (Self  : in out Client_Response_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.TypeHierarchyItem_Vector_Or_Null) is
   begin
      LSP.Output_Tools.Write_Start_Response (Self.Output.all, Id);
      Self.Output.Key_Name ("result");
      LSP.Outputs.Write_TypeHierarchyItem_Vector_Or_Null
        (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_Subtypes_Response;

   overriding procedure On_Supertypes_Response
     (Self  : in out Client_Response_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.TypeHierarchyItem_Vector_Or_Null) is
   begin
      LSP.Output_Tools.Write_Start_Response (Self.Output.all, Id);
      Self.Output.Key_Name ("result");
      LSP.Outputs.Write_TypeHierarchyItem_Vector_Or_Null
        (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_Supertypes_Response;

   overriding procedure On_Workspace_Diagnostic_Response
     (Self  : in out Client_Response_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.WorkspaceDiagnosticReport) is
   begin
      LSP.Output_Tools.Write_Start_Response (Self.Output.all, Id);
      Self.Output.Key_Name ("result");
      LSP.Outputs.Write_WorkspaceDiagnosticReport (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_Workspace_Diagnostic_Response;

   overriding procedure On_ExecuteCommand_Response
     (Self  : in out Client_Response_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.LSPAny_Or_Null) is
   begin
      LSP.Output_Tools.Write_Start_Response (Self.Output.all, Id);
      Self.Output.Key_Name ("result");
      LSP.Outputs.Write_LSPAny_Or_Null (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_ExecuteCommand_Response;

   overriding procedure On_Symbol_Response
     (Self  : in out Client_Response_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Symbol_Result) is
   begin
      LSP.Output_Tools.Write_Start_Response (Self.Output.all, Id);
      Self.Output.Key_Name ("result");
      LSP.Outputs.Write_Symbol_Result (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_Symbol_Response;

   overriding procedure On_WillCreateFiles_Response
     (Self  : in out Client_Response_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.WorkspaceEdit_Or_Null) is
   begin
      LSP.Output_Tools.Write_Start_Response (Self.Output.all, Id);
      Self.Output.Key_Name ("result");
      LSP.Outputs.Write_WorkspaceEdit_Or_Null (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_WillCreateFiles_Response;

   overriding procedure On_WillDeleteFiles_Response
     (Self  : in out Client_Response_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.WorkspaceEdit_Or_Null) is
   begin
      LSP.Output_Tools.Write_Start_Response (Self.Output.all, Id);
      Self.Output.Key_Name ("result");
      LSP.Outputs.Write_WorkspaceEdit_Or_Null (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_WillDeleteFiles_Response;

   overriding procedure On_WillRenameFiles_Response
     (Self  : in out Client_Response_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.WorkspaceEdit_Or_Null) is
   begin
      LSP.Output_Tools.Write_Start_Response (Self.Output.all, Id);
      Self.Output.Key_Name ("result");
      LSP.Outputs.Write_WorkspaceEdit_Or_Null (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_WillRenameFiles_Response;

   overriding procedure On_Symbol_Resolve_Response
     (Self  : in out Client_Response_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.WorkspaceSymbol) is
   begin
      LSP.Output_Tools.Write_Start_Response (Self.Output.all, Id);
      Self.Output.Key_Name ("result");
      LSP.Outputs.Write_WorkspaceSymbol (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_Symbol_Resolve_Response;

   overriding procedure On_Error_Response
     (Self  : in out Client_Response_Writer;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Errors.ResponseError) is
   begin
      LSP.Output_Tools.Write_Start_Response (Self.Output.all, Id);
      Self.Output.Key_Name ("error");
      Self.Output.Start_Object;
      Self.Output.Key_Name ("code");
      LSP.Outputs.Write_ErrorCodes (Self.Output.all, Value.code);
      Self.Output.Key_Name ("message");
      Self.Output.String_Value (Value.message);
      Self.Output.End_Object;
      Self.Output.End_Object;
   end On_Error_Response;

end LSP.Client_Response_Writers;
