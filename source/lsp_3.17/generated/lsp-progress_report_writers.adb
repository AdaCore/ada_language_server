--
--  Copyright (C) <YEAR>, <COPYRIGHT HOLDER>
--
--  SPDX-License-Identifier: MIT
--

with LSP.Output_Tools;
with LSP.Outputs;

package body LSP.Progress_Report_Writers is

   overriding procedure On_IncomingCalls_Partial_Result
     (Self  : in out Progress_Report_Writer;
      Token : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.CallHierarchyIncomingCall_Vector) is
   begin
      LSP.Output_Tools.Write_Start_Progress_Report (Self.Output.all, Token);
      LSP.Outputs.Write_CallHierarchyIncomingCall_Vector
        (Self.Output.all, Value);
      Self.Output.End_Object;
      Self.Output.End_Object;
   end On_IncomingCalls_Partial_Result;

   overriding procedure On_OutgoingCalls_Partial_Result
     (Self  : in out Progress_Report_Writer;
      Token : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.CallHierarchyOutgoingCall_Vector) is
   begin
      LSP.Output_Tools.Write_Start_Progress_Report (Self.Output.all, Token);
      LSP.Outputs.Write_CallHierarchyOutgoingCall_Vector
        (Self.Output.all, Value);
      Self.Output.End_Object;
      Self.Output.End_Object;
   end On_OutgoingCalls_Partial_Result;

   overriding procedure On_CodeAction_Partial_Result
     (Self  : in out Progress_Report_Writer;
      Token : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Command_Or_CodeAction_Vector) is
   begin
      LSP.Output_Tools.Write_Start_Progress_Report (Self.Output.all, Token);
      LSP.Outputs.Write_Command_Or_CodeAction_Vector (Self.Output.all, Value);
      Self.Output.End_Object;
      Self.Output.End_Object;
   end On_CodeAction_Partial_Result;

   overriding procedure On_CodeLens_Partial_Result
     (Self  : in out Progress_Report_Writer;
      Token : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.CodeLens_Vector) is
   begin
      LSP.Output_Tools.Write_Start_Progress_Report (Self.Output.all, Token);
      LSP.Outputs.Write_CodeLens_Vector (Self.Output.all, Value);
      Self.Output.End_Object;
      Self.Output.End_Object;
   end On_CodeLens_Partial_Result;

   overriding procedure On_ColorPresentation_Partial_Result
     (Self  : in out Progress_Report_Writer;
      Token : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.ColorPresentation_Vector) is
   begin
      LSP.Output_Tools.Write_Start_Progress_Report (Self.Output.all, Token);
      LSP.Outputs.Write_ColorPresentation_Vector (Self.Output.all, Value);
      Self.Output.End_Object;
      Self.Output.End_Object;
   end On_ColorPresentation_Partial_Result;

   overriding procedure On_Completion_Partial_Result
     (Self  : in out Progress_Report_Writer;
      Token : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.CompletionItem_Vector) is
   begin
      LSP.Output_Tools.Write_Start_Progress_Report (Self.Output.all, Token);
      LSP.Outputs.Write_CompletionItem_Vector (Self.Output.all, Value);
      Self.Output.End_Object;
      Self.Output.End_Object;
   end On_Completion_Partial_Result;

   overriding procedure On_Declaration_Partial_Result
     (Self  : in out Progress_Report_Writer;
      Token : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Declaration_Progress_Report) is
   begin
      LSP.Output_Tools.Write_Start_Progress_Report (Self.Output.all, Token);
      LSP.Outputs.Write_Declaration_Progress_Report (Self.Output.all, Value);
      Self.Output.End_Object;
      Self.Output.End_Object;
   end On_Declaration_Partial_Result;

   overriding procedure On_Definition_Partial_Result
     (Self  : in out Progress_Report_Writer;
      Token : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Definition_Progress_Report) is
   begin
      LSP.Output_Tools.Write_Start_Progress_Report (Self.Output.all, Token);
      LSP.Outputs.Write_Definition_Progress_Report (Self.Output.all, Value);
      Self.Output.End_Object;
      Self.Output.End_Object;
   end On_Definition_Partial_Result;

   overriding procedure On_Diagnostic_Partial_Result
     (Self  : in out Progress_Report_Writer;
      Token : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.DocumentDiagnosticReportPartialResult) is
   begin
      LSP.Output_Tools.Write_Start_Progress_Report (Self.Output.all, Token);
      LSP.Outputs.Write_DocumentDiagnosticReportPartialResult
        (Self.Output.all, Value);
      Self.Output.End_Object;
      Self.Output.End_Object;
   end On_Diagnostic_Partial_Result;

   overriding procedure On_DocumentColor_Partial_Result
     (Self  : in out Progress_Report_Writer;
      Token : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.ColorInformation_Vector) is
   begin
      LSP.Output_Tools.Write_Start_Progress_Report (Self.Output.all, Token);
      LSP.Outputs.Write_ColorInformation_Vector (Self.Output.all, Value);
      Self.Output.End_Object;
      Self.Output.End_Object;
   end On_DocumentColor_Partial_Result;

   overriding procedure On_DocumentHighlight_Partial_Result
     (Self  : in out Progress_Report_Writer;
      Token : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.DocumentHighlight_Vector) is
   begin
      LSP.Output_Tools.Write_Start_Progress_Report (Self.Output.all, Token);
      LSP.Outputs.Write_DocumentHighlight_Vector (Self.Output.all, Value);
      Self.Output.End_Object;
      Self.Output.End_Object;
   end On_DocumentHighlight_Partial_Result;

   overriding procedure On_DocumentLink_Partial_Result
     (Self  : in out Progress_Report_Writer;
      Token : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.DocumentLink_Vector) is
   begin
      LSP.Output_Tools.Write_Start_Progress_Report (Self.Output.all, Token);
      LSP.Outputs.Write_DocumentLink_Vector (Self.Output.all, Value);
      Self.Output.End_Object;
      Self.Output.End_Object;
   end On_DocumentLink_Partial_Result;

   overriding procedure On_DocumentSymbol_Partial_Result
     (Self  : in out Progress_Report_Writer;
      Token : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.DocumentSymbol_Progress_Report) is
   begin
      LSP.Output_Tools.Write_Start_Progress_Report (Self.Output.all, Token);
      LSP.Outputs.Write_DocumentSymbol_Progress_Report
        (Self.Output.all, Value);
      Self.Output.End_Object;
      Self.Output.End_Object;
   end On_DocumentSymbol_Partial_Result;

   overriding procedure On_FoldingRange_Partial_Result
     (Self  : in out Progress_Report_Writer;
      Token : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.FoldingRange_Vector) is
   begin
      LSP.Output_Tools.Write_Start_Progress_Report (Self.Output.all, Token);
      LSP.Outputs.Write_FoldingRange_Vector (Self.Output.all, Value);
      Self.Output.End_Object;
      Self.Output.End_Object;
   end On_FoldingRange_Partial_Result;

   overriding procedure On_Implementation_Partial_Result
     (Self  : in out Progress_Report_Writer;
      Token : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Definition_Progress_Report) is
   begin
      LSP.Output_Tools.Write_Start_Progress_Report (Self.Output.all, Token);
      LSP.Outputs.Write_Definition_Progress_Report (Self.Output.all, Value);
      Self.Output.End_Object;
      Self.Output.End_Object;
   end On_Implementation_Partial_Result;

   overriding procedure On_InlayHint_Partial_Result
     (Self  : in out Progress_Report_Writer;
      Token : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.InlayHint_Vector) is
   begin
      LSP.Output_Tools.Write_Start_Progress_Report (Self.Output.all, Token);
      LSP.Outputs.Write_InlayHint_Vector (Self.Output.all, Value);
      Self.Output.End_Object;
      Self.Output.End_Object;
   end On_InlayHint_Partial_Result;

   overriding procedure On_InlineValue_Partial_Result
     (Self  : in out Progress_Report_Writer;
      Token : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.InlineValue_Vector) is
   begin
      LSP.Output_Tools.Write_Start_Progress_Report (Self.Output.all, Token);
      LSP.Outputs.Write_InlineValue_Vector (Self.Output.all, Value);
      Self.Output.End_Object;
      Self.Output.End_Object;
   end On_InlineValue_Partial_Result;

   overriding procedure On_Moniker_Partial_Result
     (Self  : in out Progress_Report_Writer;
      Token : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Moniker_Vector) is
   begin
      LSP.Output_Tools.Write_Start_Progress_Report (Self.Output.all, Token);
      LSP.Outputs.Write_Moniker_Vector (Self.Output.all, Value);
      Self.Output.End_Object;
      Self.Output.End_Object;
   end On_Moniker_Partial_Result;

   overriding procedure On_References_Partial_Result
     (Self  : in out Progress_Report_Writer;
      Token : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Location_Vector) is
   begin
      LSP.Output_Tools.Write_Start_Progress_Report (Self.Output.all, Token);
      LSP.Outputs.Write_Location_Vector (Self.Output.all, Value);
      Self.Output.End_Object;
      Self.Output.End_Object;
   end On_References_Partial_Result;

   overriding procedure On_SelectionRange_Partial_Result
     (Self  : in out Progress_Report_Writer;
      Token : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.SelectionRange_Vector) is
   begin
      LSP.Output_Tools.Write_Start_Progress_Report (Self.Output.all, Token);
      LSP.Outputs.Write_SelectionRange_Vector (Self.Output.all, Value);
      Self.Output.End_Object;
      Self.Output.End_Object;
   end On_SelectionRange_Partial_Result;

   overriding procedure On_Full_Partial_Result
     (Self  : in out Progress_Report_Writer;
      Token : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.SemanticTokensPartialResult) is
   begin
      LSP.Output_Tools.Write_Start_Progress_Report (Self.Output.all, Token);
      LSP.Outputs.Write_SemanticTokensPartialResult (Self.Output.all, Value);
      Self.Output.End_Object;
      Self.Output.End_Object;
   end On_Full_Partial_Result;

   overriding procedure On_Tokens_Delta_Partial_Result
     (Self  : in out Progress_Report_Writer;
      Token : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures
        .SemanticTokensPartialResult_Or_SemanticTokensDeltaPartialResult) is
   begin
      LSP.Output_Tools.Write_Start_Progress_Report (Self.Output.all, Token);
      LSP.Outputs
        .Write_SemanticTokensPartialResult_Or_SemanticTokensDeltaPartialResult
        (Self.Output.all, Value);
      Self.Output.End_Object;
      Self.Output.End_Object;
   end On_Tokens_Delta_Partial_Result;

   overriding procedure On_Tokens_Range_Partial_Result
     (Self  : in out Progress_Report_Writer;
      Token : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.SemanticTokensPartialResult) is
   begin
      LSP.Output_Tools.Write_Start_Progress_Report (Self.Output.all, Token);
      LSP.Outputs.Write_SemanticTokensPartialResult (Self.Output.all, Value);
      Self.Output.End_Object;
      Self.Output.End_Object;
   end On_Tokens_Range_Partial_Result;

   overriding procedure On_TypeDefinition_Partial_Result
     (Self  : in out Progress_Report_Writer;
      Token : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Definition_Progress_Report) is
   begin
      LSP.Output_Tools.Write_Start_Progress_Report (Self.Output.all, Token);
      LSP.Outputs.Write_Definition_Progress_Report (Self.Output.all, Value);
      Self.Output.End_Object;
      Self.Output.End_Object;
   end On_TypeDefinition_Partial_Result;

   overriding procedure On_Subtypes_Partial_Result
     (Self  : in out Progress_Report_Writer;
      Token : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.TypeHierarchyItem_Vector) is
   begin
      LSP.Output_Tools.Write_Start_Progress_Report (Self.Output.all, Token);
      LSP.Outputs.Write_TypeHierarchyItem_Vector (Self.Output.all, Value);
      Self.Output.End_Object;
      Self.Output.End_Object;
   end On_Subtypes_Partial_Result;

   overriding procedure On_Supertypes_Partial_Result
     (Self  : in out Progress_Report_Writer;
      Token : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.TypeHierarchyItem_Vector) is
   begin
      LSP.Output_Tools.Write_Start_Progress_Report (Self.Output.all, Token);
      LSP.Outputs.Write_TypeHierarchyItem_Vector (Self.Output.all, Value);
      Self.Output.End_Object;
      Self.Output.End_Object;
   end On_Supertypes_Partial_Result;

   overriding procedure On_Workspace_Diagnostic_Partial_Result
     (Self  : in out Progress_Report_Writer;
      Token : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.WorkspaceDiagnosticReportPartialResult) is
   begin
      LSP.Output_Tools.Write_Start_Progress_Report (Self.Output.all, Token);
      LSP.Outputs.Write_WorkspaceDiagnosticReportPartialResult
        (Self.Output.all, Value);
      Self.Output.End_Object;
      Self.Output.End_Object;
   end On_Workspace_Diagnostic_Partial_Result;

   overriding procedure On_Symbol_Partial_Result
     (Self  : in out Progress_Report_Writer;
      Token : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Symbol_Progress_Report) is
   begin
      LSP.Output_Tools.Write_Start_Progress_Report (Self.Output.all, Token);
      LSP.Outputs.Write_Symbol_Progress_Report (Self.Output.all, Value);
      Self.Output.End_Object;
      Self.Output.End_Object;
   end On_Symbol_Partial_Result;

   overriding procedure On_ProgressBegin_Work_Done
     (Self  : in out Progress_Report_Writer;
      Token : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.WorkDoneProgressBegin) is
   begin
      LSP.Output_Tools.Write_Start_Progress_Report (Self.Output.all, Token);
      LSP.Outputs.Write_WorkDoneProgressBegin (Self.Output.all, Value);
      Self.Output.End_Object;
      Self.Output.End_Object;
   end On_ProgressBegin_Work_Done;

   overriding procedure On_ProgressReport_Work_Done
     (Self  : in out Progress_Report_Writer;
      Token : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.WorkDoneProgressReport) is
   begin
      LSP.Output_Tools.Write_Start_Progress_Report (Self.Output.all, Token);
      LSP.Outputs.Write_WorkDoneProgressReport (Self.Output.all, Value);
      Self.Output.End_Object;
      Self.Output.End_Object;
   end On_ProgressReport_Work_Done;

   overriding procedure On_ProgressEnd_Work_Done
     (Self  : in out Progress_Report_Writer;
      Token : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.WorkDoneProgressEnd) is
   begin
      LSP.Output_Tools.Write_Start_Progress_Report (Self.Output.all, Token);
      LSP.Outputs.Write_WorkDoneProgressEnd (Self.Output.all, Value);
      Self.Output.End_Object;
      Self.Output.End_Object;
   end On_ProgressEnd_Work_Done;

end LSP.Progress_Report_Writers;
