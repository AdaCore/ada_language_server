--
--  Copyright (C) <YEAR>, <COPYRIGHT HOLDER>
--
--  SPDX-License-Identifier: MIT
--

with Minimal_Perfect_Hash;
with LSP.Inputs;
with LSP.Input_Tools;
with LSP.Structures;

with LSP.Progress_Reports.IncomingCalls;
with LSP.Progress_Reports.OutgoingCalls;
with LSP.Progress_Reports.CodeAction;
with LSP.Progress_Reports.CodeLens;
with LSP.Progress_Reports.ColorPresentation;
with LSP.Progress_Reports.Completion;
with LSP.Progress_Reports.Declaration;
with LSP.Progress_Reports.Definition;
with LSP.Progress_Reports.Diagnostic;
with LSP.Progress_Reports.DocumentColor;
with LSP.Progress_Reports.DocumentHighlight;
with LSP.Progress_Reports.DocumentLink;
with LSP.Progress_Reports.DocumentSymbol;
with LSP.Progress_Reports.FoldingRange;
with LSP.Progress_Reports.Implementation;
with LSP.Progress_Reports.InlayHint;
with LSP.Progress_Reports.InlineValue;
with LSP.Progress_Reports.Moniker;
with LSP.Progress_Reports.References;
with LSP.Progress_Reports.SelectionRange;
with LSP.Progress_Reports.Full;
with LSP.Progress_Reports.Tokens_Delta;
with LSP.Progress_Reports.Tokens_Range;
with LSP.Progress_Reports.TypeDefinition;
with LSP.Progress_Reports.Subtypes;
with LSP.Progress_Reports.Supertypes;
with LSP.Progress_Reports.Workspace_Diagnostic;
with LSP.Progress_Reports.Symbol;
with LSP.Progress_Reports.ProgressBegin;
with LSP.Progress_Reports.ProgressReport;
with LSP.Progress_Reports.ProgressEnd;

package body LSP.Progress_Report_Readers is
   package Method_Map is new Minimal_Perfect_Hash
     (["callHierarchy/incomingCalls",
       "callHierarchy/outgoingCalls",
       "textDocument/codeAction",
       "textDocument/codeLens",
       "textDocument/colorPresentation",
       "textDocument/completion",
       "textDocument/declaration",
       "textDocument/definition",
       "textDocument/diagnostic",
       "textDocument/documentColor",
       "textDocument/documentHighlight",
       "textDocument/documentLink",
       "textDocument/documentSymbol",
       "textDocument/foldingRange",
       "textDocument/implementation",
       "textDocument/inlayHint",
       "textDocument/inlineValue",
       "textDocument/moniker",
       "textDocument/references",
       "textDocument/selectionRange",
       "textDocument/semanticTokens/full",
       "textDocument/semanticTokens/full/delta",
       "textDocument/semanticTokens/range",
       "textDocument/typeDefinition",
       "typeHierarchy/subtypes",
       "typeHierarchy/supertypes",
       "workspace/diagnostic",
       "workspace/symbol",
       "WorkDoneProgressBegin",
       "WorkDoneProgressReport",
       "WorkDoneProgressEnd"]);

   procedure Initialize is
   begin
      Method_Map.Initialize;
   end Initialize;

   procedure Read_IncomingCalls is new LSP.Input_Tools.Read_Progress_Report
     (LSP.Structures.CallHierarchyIncomingCall_Vector,
      LSP.Inputs.Read_CallHierarchyIncomingCall_Vector);

   procedure Read_OutgoingCalls is new LSP.Input_Tools.Read_Progress_Report
     (LSP.Structures.CallHierarchyOutgoingCall_Vector,
      LSP.Inputs.Read_CallHierarchyOutgoingCall_Vector);

   procedure Read_CodeAction is new LSP.Input_Tools.Read_Progress_Report
     (LSP.Structures.Command_Or_CodeAction_Vector,
      LSP.Inputs.Read_Command_Or_CodeAction_Vector);

   procedure Read_CodeLens is new LSP.Input_Tools.Read_Progress_Report
     (LSP.Structures.CodeLens_Vector, LSP.Inputs.Read_CodeLens_Vector);

   procedure Read_ColorPresentation is new LSP.Input_Tools.Read_Progress_Report
     (LSP.Structures.ColorPresentation_Vector,
      LSP.Inputs.Read_ColorPresentation_Vector);

   procedure Read_Completion is new LSP.Input_Tools.Read_Progress_Report
     (LSP.Structures.CompletionItem_Vector,
      LSP.Inputs.Read_CompletionItem_Vector);

   procedure Read_Declaration is new LSP.Input_Tools.Read_Progress_Report
     (LSP.Structures.Declaration_Progress_Report,
      LSP.Inputs.Read_Declaration_Progress_Report);

   procedure Read_Definition is new LSP.Input_Tools.Read_Progress_Report
     (LSP.Structures.Definition_Progress_Report,
      LSP.Inputs.Read_Definition_Progress_Report);

   procedure Read_Diagnostic is new LSP.Input_Tools.Read_Progress_Report
     (LSP.Structures.DocumentDiagnosticReportPartialResult,
      LSP.Inputs.Read_DocumentDiagnosticReportPartialResult);

   procedure Read_DocumentColor is new LSP.Input_Tools.Read_Progress_Report
     (LSP.Structures.ColorInformation_Vector,
      LSP.Inputs.Read_ColorInformation_Vector);

   procedure Read_DocumentHighlight is new LSP.Input_Tools.Read_Progress_Report
     (LSP.Structures.DocumentHighlight_Vector,
      LSP.Inputs.Read_DocumentHighlight_Vector);

   procedure Read_DocumentLink is new LSP.Input_Tools.Read_Progress_Report
     (LSP.Structures.DocumentLink_Vector, LSP.Inputs.Read_DocumentLink_Vector);

   procedure Read_DocumentSymbol is new LSP.Input_Tools.Read_Progress_Report
     (LSP.Structures.DocumentSymbol_Progress_Report,
      LSP.Inputs.Read_DocumentSymbol_Progress_Report);

   procedure Read_FoldingRange is new LSP.Input_Tools.Read_Progress_Report
     (LSP.Structures.FoldingRange_Vector, LSP.Inputs.Read_FoldingRange_Vector);

   procedure Read_Implementation is new LSP.Input_Tools.Read_Progress_Report
     (LSP.Structures.Definition_Progress_Report,
      LSP.Inputs.Read_Definition_Progress_Report);

   procedure Read_InlayHint is new LSP.Input_Tools.Read_Progress_Report
     (LSP.Structures.InlayHint_Vector, LSP.Inputs.Read_InlayHint_Vector);

   procedure Read_InlineValue is new LSP.Input_Tools.Read_Progress_Report
     (LSP.Structures.InlineValue_Vector, LSP.Inputs.Read_InlineValue_Vector);

   procedure Read_Moniker is new LSP.Input_Tools.Read_Progress_Report
     (LSP.Structures.Moniker_Vector, LSP.Inputs.Read_Moniker_Vector);

   procedure Read_References is new LSP.Input_Tools.Read_Progress_Report
     (LSP.Structures.Location_Vector, LSP.Inputs.Read_Location_Vector);

   procedure Read_SelectionRange is new LSP.Input_Tools.Read_Progress_Report
     (LSP.Structures.SelectionRange_Vector,
      LSP.Inputs.Read_SelectionRange_Vector);

   procedure Read_Full is new LSP.Input_Tools.Read_Progress_Report
     (LSP.Structures.SemanticTokensPartialResult,
      LSP.Inputs.Read_SemanticTokensPartialResult);

   procedure Read_Tokens_Delta is new LSP.Input_Tools.Read_Progress_Report
     (LSP.Structures
        .SemanticTokensPartialResult_Or_SemanticTokensDeltaPartialResult,
      LSP.Inputs
        .Read_SemanticTokensPartialResult_Or_SemanticTokensDeltaPartialResult);

   procedure Read_Tokens_Range is new LSP.Input_Tools.Read_Progress_Report
     (LSP.Structures.SemanticTokensPartialResult,
      LSP.Inputs.Read_SemanticTokensPartialResult);

   procedure Read_TypeDefinition is new LSP.Input_Tools.Read_Progress_Report
     (LSP.Structures.Definition_Progress_Report,
      LSP.Inputs.Read_Definition_Progress_Report);

   procedure Read_Subtypes is new LSP.Input_Tools.Read_Progress_Report
     (LSP.Structures.TypeHierarchyItem_Vector,
      LSP.Inputs.Read_TypeHierarchyItem_Vector);

   procedure Read_Supertypes is new LSP.Input_Tools.Read_Progress_Report
     (LSP.Structures.TypeHierarchyItem_Vector,
      LSP.Inputs.Read_TypeHierarchyItem_Vector);

   procedure Read_Workspace_Diagnostic is new LSP.Input_Tools
     .Read_Progress_Report
     (LSP.Structures.WorkspaceDiagnosticReportPartialResult,
      LSP.Inputs.Read_WorkspaceDiagnosticReportPartialResult);

   procedure Read_Symbol is new LSP.Input_Tools.Read_Progress_Report
     (LSP.Structures.Symbol_Progress_Report,
      LSP.Inputs.Read_Symbol_Progress_Report);

   procedure Read_ProgressBegin is new LSP.Input_Tools.Read_Progress_Report
     (LSP.Structures.WorkDoneProgressBegin,
      LSP.Inputs.Read_WorkDoneProgressBegin);

   procedure Read_ProgressReport is new LSP.Input_Tools.Read_Progress_Report
     (LSP.Structures.WorkDoneProgressReport,
      LSP.Inputs.Read_WorkDoneProgressReport);

   procedure Read_ProgressEnd is new LSP.Input_Tools.Read_Progress_Report
     (LSP.Structures.WorkDoneProgressEnd, LSP.Inputs.Read_WorkDoneProgressEnd);

   function Read_Progress_Report
     (Input  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Method : VSS.Strings.Virtual_String)
      return LSP.Progress_Reports.Progress_Report'Class is
      Index : constant Natural := Method_Map.Get_Index (Method);
   begin
      case Index is
         when 1 =>  --  callHierarchy/incomingCalls
            return
              Result : LSP.Progress_Reports.IncomingCalls.Partial_Result do
               Read_IncomingCalls (Input, Result.Token, Result.Params);
            end return;

         when 2 =>  --  callHierarchy/outgoingCalls
            return
              Result : LSP.Progress_Reports.OutgoingCalls.Partial_Result do
               Read_OutgoingCalls (Input, Result.Token, Result.Params);
            end return;

         when 3 =>  --  textDocument/codeAction
            return Result : LSP.Progress_Reports.CodeAction.Partial_Result do
               Read_CodeAction (Input, Result.Token, Result.Params);
            end return;

         when 4 =>  --  textDocument/codeLens
            return Result : LSP.Progress_Reports.CodeLens.Partial_Result do
               Read_CodeLens (Input, Result.Token, Result.Params);
            end return;

         when 5 =>  --  textDocument/colorPresentation
            return
              Result : LSP.Progress_Reports.ColorPresentation.Partial_Result do
               Read_ColorPresentation (Input, Result.Token, Result.Params);
            end return;

         when 6 =>  --  textDocument/completion
            return Result : LSP.Progress_Reports.Completion.Partial_Result do
               Read_Completion (Input, Result.Token, Result.Params);
            end return;

         when 7 =>  --  textDocument/declaration
            return Result : LSP.Progress_Reports.Declaration.Partial_Result do
               Read_Declaration (Input, Result.Token, Result.Params);
            end return;

         when 8 =>  --  textDocument/definition
            return Result : LSP.Progress_Reports.Definition.Partial_Result do
               Read_Definition (Input, Result.Token, Result.Params);
            end return;

         when 9 =>  --  textDocument/diagnostic
            return Result : LSP.Progress_Reports.Diagnostic.Partial_Result do
               Read_Diagnostic (Input, Result.Token, Result.Params);
            end return;

         when 10 =>  --  textDocument/documentColor
            return
              Result : LSP.Progress_Reports.DocumentColor.Partial_Result do
               Read_DocumentColor (Input, Result.Token, Result.Params);
            end return;

         when 11 =>  --  textDocument/documentHighlight
            return
              Result : LSP.Progress_Reports.DocumentHighlight.Partial_Result do
               Read_DocumentHighlight (Input, Result.Token, Result.Params);
            end return;

         when 12 =>  --  textDocument/documentLink
            return Result : LSP.Progress_Reports.DocumentLink.Partial_Result do
               Read_DocumentLink (Input, Result.Token, Result.Params);
            end return;

         when 13 =>  --  textDocument/documentSymbol
            return
              Result : LSP.Progress_Reports.DocumentSymbol.Partial_Result do
               Read_DocumentSymbol (Input, Result.Token, Result.Params);
            end return;

         when 14 =>  --  textDocument/foldingRange
            return Result : LSP.Progress_Reports.FoldingRange.Partial_Result do
               Read_FoldingRange (Input, Result.Token, Result.Params);
            end return;

         when 15 =>  --  textDocument/implementation
            return
              Result : LSP.Progress_Reports.Implementation.Partial_Result do
               Read_Implementation (Input, Result.Token, Result.Params);
            end return;

         when 16 =>  --  textDocument/inlayHint
            return Result : LSP.Progress_Reports.InlayHint.Partial_Result do
               Read_InlayHint (Input, Result.Token, Result.Params);
            end return;

         when 17 =>  --  textDocument/inlineValue
            return Result : LSP.Progress_Reports.InlineValue.Partial_Result do
               Read_InlineValue (Input, Result.Token, Result.Params);
            end return;

         when 18 =>  --  textDocument/moniker
            return Result : LSP.Progress_Reports.Moniker.Partial_Result do
               Read_Moniker (Input, Result.Token, Result.Params);
            end return;

         when 19 =>  --  textDocument/references
            return Result : LSP.Progress_Reports.References.Partial_Result do
               Read_References (Input, Result.Token, Result.Params);
            end return;

         when 20 =>  --  textDocument/selectionRange
            return
              Result : LSP.Progress_Reports.SelectionRange.Partial_Result do
               Read_SelectionRange (Input, Result.Token, Result.Params);
            end return;

         when 21 =>  --  textDocument/semanticTokens/full
            return Result : LSP.Progress_Reports.Full.Partial_Result do
               Read_Full (Input, Result.Token, Result.Params);
            end return;

         when 22 =>  --  textDocument/semanticTokens/full/delta
            return Result : LSP.Progress_Reports.Tokens_Delta.Partial_Result do
               Read_Tokens_Delta (Input, Result.Token, Result.Params);
            end return;

         when 23 =>  --  textDocument/semanticTokens/range
            return Result : LSP.Progress_Reports.Tokens_Range.Partial_Result do
               Read_Tokens_Range (Input, Result.Token, Result.Params);
            end return;

         when 24 =>  --  textDocument/typeDefinition
            return
              Result : LSP.Progress_Reports.TypeDefinition.Partial_Result do
               Read_TypeDefinition (Input, Result.Token, Result.Params);
            end return;

         when 25 =>  --  typeHierarchy/subtypes
            return Result : LSP.Progress_Reports.Subtypes.Partial_Result do
               Read_Subtypes (Input, Result.Token, Result.Params);
            end return;

         when 26 =>  --  typeHierarchy/supertypes
            return Result : LSP.Progress_Reports.Supertypes.Partial_Result do
               Read_Supertypes (Input, Result.Token, Result.Params);
            end return;

         when 27 =>  --  workspace/diagnostic
            return
              Result : LSP.Progress_Reports.Workspace_Diagnostic.Partial_Result
            do
               Read_Workspace_Diagnostic (Input, Result.Token, Result.Params);
            end return;

         when 28 =>  --  workspace/symbol
            return Result : LSP.Progress_Reports.Symbol.Partial_Result do
               Read_Symbol (Input, Result.Token, Result.Params);
            end return;

         when 29 =>  --  WorkDoneProgressBegin
            return Result : LSP.Progress_Reports.ProgressBegin.Work_Done do
               Read_ProgressBegin (Input, Result.Token, Result.Params);
            end return;

         when 30 =>  --  WorkDoneProgressReport
            return Result : LSP.Progress_Reports.ProgressReport.Work_Done do
               Read_ProgressReport (Input, Result.Token, Result.Params);
            end return;

         when 31 =>  --  WorkDoneProgressEnd
            return Result : LSP.Progress_Reports.ProgressEnd.Work_Done do
               Read_ProgressEnd (Input, Result.Token, Result.Params);
            end return;

         when others =>
            return raise Program_Error with "Unknown method";
      end case;
   end Read_Progress_Report;
end LSP.Progress_Report_Readers;
