--
--  Copyright (C) <YEAR>, <COPYRIGHT HOLDER>
--
--  SPDX-License-Identifier: MIT
--
--  DON'T EDIT THIS FILE! It was generated from metaModel.json.
--

with LSP.Structures;
with VSS.JSON.Content_Handlers;
with LSP.Progress_Report_Receivers;

package LSP.Progress_Report_Writers is
   pragma Preelaborate;

   type Progress_Report_Writer
     (Output : access VSS.JSON.Content_Handlers.JSON_Content_Handler'Class) is
   new LSP.Progress_Report_Receivers.Progress_Report_Receiver with null record;

   overriding procedure On_IncomingCalls_Partial_Result
     (Self  : in out Progress_Report_Writer;
      Token : LSP.Structures.ProgressToken;
      Value : LSP.Structures.CallHierarchyIncomingCall_Vector);

   overriding procedure On_OutgoingCalls_Partial_Result
     (Self  : in out Progress_Report_Writer;
      Token : LSP.Structures.ProgressToken;
      Value : LSP.Structures.CallHierarchyOutgoingCall_Vector);

   overriding procedure On_CodeAction_Partial_Result
     (Self  : in out Progress_Report_Writer;
      Token : LSP.Structures.ProgressToken;
      Value : LSP.Structures.Command_Or_CodeAction_Vector);

   overriding procedure On_CodeLens_Partial_Result
     (Self  : in out Progress_Report_Writer;
      Token : LSP.Structures.ProgressToken;
      Value : LSP.Structures.CodeLens_Vector);

   overriding procedure On_ColorPresentation_Partial_Result
     (Self  : in out Progress_Report_Writer;
      Token : LSP.Structures.ProgressToken;
      Value : LSP.Structures.ColorPresentation_Vector);

   overriding procedure On_Completion_Partial_Result
     (Self  : in out Progress_Report_Writer;
      Token : LSP.Structures.ProgressToken;
      Value : LSP.Structures.CompletionItem_Vector);

   overriding procedure On_Declaration_Partial_Result
     (Self  : in out Progress_Report_Writer;
      Token : LSP.Structures.ProgressToken;
      Value : LSP.Structures.Declaration_Progress_Report);

   overriding procedure On_Definition_Partial_Result
     (Self  : in out Progress_Report_Writer;
      Token : LSP.Structures.ProgressToken;
      Value : LSP.Structures.Definition_Progress_Report);

   overriding procedure On_Diagnostic_Partial_Result
     (Self  : in out Progress_Report_Writer;
      Token : LSP.Structures.ProgressToken;
      Value : LSP.Structures.DocumentDiagnosticReportPartialResult);

   overriding procedure On_DocumentColor_Partial_Result
     (Self  : in out Progress_Report_Writer;
      Token : LSP.Structures.ProgressToken;
      Value : LSP.Structures.ColorInformation_Vector);

   overriding procedure On_DocumentHighlight_Partial_Result
     (Self  : in out Progress_Report_Writer;
      Token : LSP.Structures.ProgressToken;
      Value : LSP.Structures.DocumentHighlight_Vector);

   overriding procedure On_DocumentLink_Partial_Result
     (Self  : in out Progress_Report_Writer;
      Token : LSP.Structures.ProgressToken;
      Value : LSP.Structures.DocumentLink_Vector);

   overriding procedure On_DocumentSymbol_Partial_Result
     (Self  : in out Progress_Report_Writer;
      Token : LSP.Structures.ProgressToken;
      Value : LSP.Structures.DocumentSymbol_Progress_Report);

   overriding procedure On_FoldingRange_Partial_Result
     (Self  : in out Progress_Report_Writer;
      Token : LSP.Structures.ProgressToken;
      Value : LSP.Structures.FoldingRange_Vector);

   overriding procedure On_Implementation_Partial_Result
     (Self  : in out Progress_Report_Writer;
      Token : LSP.Structures.ProgressToken;
      Value : LSP.Structures.Definition_Progress_Report);

   overriding procedure On_InlayHint_Partial_Result
     (Self  : in out Progress_Report_Writer;
      Token : LSP.Structures.ProgressToken;
      Value : LSP.Structures.InlayHint_Vector);

   overriding procedure On_InlineValue_Partial_Result
     (Self  : in out Progress_Report_Writer;
      Token : LSP.Structures.ProgressToken;
      Value : LSP.Structures.InlineValue_Vector);

   overriding procedure On_Moniker_Partial_Result
     (Self  : in out Progress_Report_Writer;
      Token : LSP.Structures.ProgressToken;
      Value : LSP.Structures.Moniker_Vector);

   overriding procedure On_References_Partial_Result
     (Self  : in out Progress_Report_Writer;
      Token : LSP.Structures.ProgressToken;
      Value : LSP.Structures.Location_Vector);

   overriding procedure On_SelectionRange_Partial_Result
     (Self  : in out Progress_Report_Writer;
      Token : LSP.Structures.ProgressToken;
      Value : LSP.Structures.SelectionRange_Vector);

   overriding procedure On_Full_Partial_Result
     (Self  : in out Progress_Report_Writer;
      Token : LSP.Structures.ProgressToken;
      Value : LSP.Structures.SemanticTokensPartialResult);

   overriding procedure On_Tokens_Delta_Partial_Result
     (Self  : in out Progress_Report_Writer;
      Token : LSP.Structures.ProgressToken;
      Value : LSP.Structures
        .SemanticTokensPartialResult_Or_SemanticTokensDeltaPartialResult);

   overriding procedure On_Tokens_Range_Partial_Result
     (Self  : in out Progress_Report_Writer;
      Token : LSP.Structures.ProgressToken;
      Value : LSP.Structures.SemanticTokensPartialResult);

   overriding procedure On_TypeDefinition_Partial_Result
     (Self  : in out Progress_Report_Writer;
      Token : LSP.Structures.ProgressToken;
      Value : LSP.Structures.Definition_Progress_Report);

   overriding procedure On_Subtypes_Partial_Result
     (Self  : in out Progress_Report_Writer;
      Token : LSP.Structures.ProgressToken;
      Value : LSP.Structures.TypeHierarchyItem_Vector);

   overriding procedure On_Supertypes_Partial_Result
     (Self  : in out Progress_Report_Writer;
      Token : LSP.Structures.ProgressToken;
      Value : LSP.Structures.TypeHierarchyItem_Vector);

   overriding procedure On_Workspace_Diagnostic_Partial_Result
     (Self  : in out Progress_Report_Writer;
      Token : LSP.Structures.ProgressToken;
      Value : LSP.Structures.WorkspaceDiagnosticReportPartialResult);

   overriding procedure On_Symbol_Partial_Result
     (Self  : in out Progress_Report_Writer;
      Token : LSP.Structures.ProgressToken;
      Value : LSP.Structures.Symbol_Progress_Report);

   overriding procedure On_ProgressBegin_Work_Done
     (Self  : in out Progress_Report_Writer;
      Token : LSP.Structures.ProgressToken;
      Value : LSP.Structures.WorkDoneProgressBegin);

   overriding procedure On_ProgressReport_Work_Done
     (Self  : in out Progress_Report_Writer;
      Token : LSP.Structures.ProgressToken;
      Value : LSP.Structures.WorkDoneProgressReport);

   overriding procedure On_ProgressEnd_Work_Done
     (Self  : in out Progress_Report_Writer;
      Token : LSP.Structures.ProgressToken;
      Value : LSP.Structures.WorkDoneProgressEnd);

end LSP.Progress_Report_Writers;
