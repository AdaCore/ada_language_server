--
--  Copyright (C) <YEAR>, <COPYRIGHT HOLDER>
--
--  SPDX-License-Identifier: MIT
--
--  DON'T EDIT THIS FILE! It was generated from metaModel.json.
--

with LSP.Structures;

package LSP.Progress_Report_Receivers is
   pragma Preelaborate;

   type Progress_Report_Receiver is limited interface;

   procedure On_IncomingCalls_Partial_Result
     (Self  : in out Progress_Report_Receiver;
      Token : LSP.Structures.ProgressToken;
      Value : LSP.Structures.CallHierarchyIncomingCall_Vector) is null;
   --  A request to resolve the incoming calls for a given `CallHierarchyItem`.
   --
   --  @since 3.16.0

   procedure On_OutgoingCalls_Partial_Result
     (Self  : in out Progress_Report_Receiver;
      Token : LSP.Structures.ProgressToken;
      Value : LSP.Structures.CallHierarchyOutgoingCall_Vector) is null;
   --  A request to resolve the outgoing calls for a given `CallHierarchyItem`.
   --
   --  @since 3.16.0

   procedure On_CodeAction_Partial_Result
     (Self  : in out Progress_Report_Receiver;
      Token : LSP.Structures.ProgressToken;
      Value : LSP.Structures.Command_Or_CodeAction_Vector) is null;
   --  A request to provide commands for the given text document and range.

   procedure On_CodeLens_Partial_Result
     (Self  : in out Progress_Report_Receiver;
      Token : LSP.Structures.ProgressToken;
      Value : LSP.Structures.CodeLens_Vector) is null;
   --  A request to provide code lens for the given text document.

   procedure On_ColorPresentation_Partial_Result
     (Self  : in out Progress_Report_Receiver;
      Token : LSP.Structures.ProgressToken;
      Value : LSP.Structures.ColorPresentation_Vector) is null;
   --  A request to list all presentation for a color. The request's parameter
   --  is of type [ColorPresentationParams](#ColorPresentationParams) the
   --  response is of type [ColorInformation[]](#ColorInformation) or a
   --  Thenable that resolves to such.

   procedure On_Completion_Partial_Result
     (Self  : in out Progress_Report_Receiver;
      Token : LSP.Structures.ProgressToken;
      Value : LSP.Structures.CompletionItem_Vector) is null;
   --  Request to request completion at a given text
   --  document position. The request's parameter is of type
   --  [TextDocumentPosition](#TextDocumentPosition) the response is of type
   --  [CompletionItem[]](#CompletionItem) or [CompletionList](#CompletionList)
   --  or a Thenable that resolves to such.
   --
   --  The request can delay the computation of
   --  the [`detail`](#CompletionItem.detail) and
   --  [`documentation`](#CompletionItem.documentation) properties to the
   --  `completionItem/resolve` request. However, properties that are needed
   --  for the initial sorting and filtering, like `sortText`, `filterText`,
   --  `insertText`, and `textEdit`, must not be changed during resolve.

   procedure On_Declaration_Partial_Result
     (Self  : in out Progress_Report_Receiver;
      Token : LSP.Structures.ProgressToken;
      Value : LSP.Structures.Declaration_Progress_Report) is null;
   --  A request to resolve the type definition locations of a symbol at
   --  a given text document position. The request's parameter is of type
   --  [TextDocumentPositionParams] (#TextDocumentPositionParams) the
   --  response is of type [Declaration](#Declaration) or a typed array of
   --  [DeclarationLink](#DeclarationLink) or a Thenable that resolves to such.

   procedure On_Definition_Partial_Result
     (Self  : in out Progress_Report_Receiver;
      Token : LSP.Structures.ProgressToken;
      Value : LSP.Structures.Definition_Progress_Report) is null;
   --  A request to resolve the definition location of a symbol at a
   --  given text document position. The request's parameter is of type
   --  [TextDocumentPosition] (#TextDocumentPosition) the response is
   --  of either type [Definition](#Definition) or a typed array of
   --  [DefinitionLink](#DefinitionLink) or a Thenable that resolves to such.

   procedure On_Diagnostic_Partial_Result
     (Self  : in out Progress_Report_Receiver;
      Token : LSP.Structures.ProgressToken;
      Value : LSP.Structures.DocumentDiagnosticReportPartialResult) is null;
   --  The document diagnostic request definition.
   --
   --  @since 3.17.0

   procedure On_DocumentColor_Partial_Result
     (Self  : in out Progress_Report_Receiver;
      Token : LSP.Structures.ProgressToken;
      Value : LSP.Structures.ColorInformation_Vector) is null;
   --  A request to list all color symbols found in a given
   --  text document. The request's parameter is of type
   --  [DocumentColorParams](#DocumentColorParams) the response is of type
   --  [ColorInformation[]](#ColorInformation) or a Thenable that resolves
   --  to such.

   procedure On_DocumentHighlight_Partial_Result
     (Self  : in out Progress_Report_Receiver;
      Token : LSP.Structures.ProgressToken;
      Value : LSP.Structures.DocumentHighlight_Vector) is null;
   --  Request to resolve a [DocumentHighlight](#DocumentHighlight) for
   --  a given text document position. The request's parameter is of type
   --  [TextDocumentPosition] (#TextDocumentPosition) the request response is
   --  of type [DocumentHighlight[]] (#DocumentHighlight) or a Thenable that
   --  resolves to such.

   procedure On_DocumentLink_Partial_Result
     (Self  : in out Progress_Report_Receiver;
      Token : LSP.Structures.ProgressToken;
      Value : LSP.Structures.DocumentLink_Vector) is null;
   --  A request to provide document links

   procedure On_DocumentSymbol_Partial_Result
     (Self  : in out Progress_Report_Receiver;
      Token : LSP.Structures.ProgressToken;
      Value : LSP.Structures.DocumentSymbol_Progress_Report) is null;
   --  A request to list all symbols found in a given
   --  text document. The request's parameter is of type
   --  [TextDocumentIdentifier](#TextDocumentIdentifier) the response is
   --  of type [SymbolInformation[]](#SymbolInformation) or a Thenable
   --  that resolves to such.

   procedure On_FoldingRange_Partial_Result
     (Self  : in out Progress_Report_Receiver;
      Token : LSP.Structures.ProgressToken;
      Value : LSP.Structures.FoldingRange_Vector) is null;
   --  A request to provide folding ranges in a document. The request's
   --  parameter is of type [FoldingRangeParams](#FoldingRangeParams), the
   --  response is of type [FoldingRangeList](#FoldingRangeList) or a Thenable
   --  that resolves to such.

   procedure On_Implementation_Partial_Result
     (Self  : in out Progress_Report_Receiver;
      Token : LSP.Structures.ProgressToken;
      Value : LSP.Structures.Definition_Progress_Report) is null;
   --  A request to resolve the implementation locations of a symbol at
   --  a given text document position. The request's parameter is of type
   --  [TextDocumentPositionParams] (#TextDocumentPositionParams) the response
   --  is of type [Definition](#Definition) or a Thenable that resolves to
   --  such.

   procedure On_InlayHint_Partial_Result
     (Self  : in out Progress_Report_Receiver;
      Token : LSP.Structures.ProgressToken;
      Value : LSP.Structures.InlayHint_Vector) is null;
   --  A request to provide inlay hints in a document. The request's parameter
   --  is of type [InlayHintsParams](#InlayHintsParams), the response is of
   --  type [InlayHint[]](#InlayHint[]) or a Thenable that resolves to such.
   --
   --  @since 3.17.0

   procedure On_InlineValue_Partial_Result
     (Self  : in out Progress_Report_Receiver;
      Token : LSP.Structures.ProgressToken;
      Value : LSP.Structures.InlineValue_Vector) is null;
   --  A request to provide inline values in a document. The request's
   --  parameter is of type [InlineValueParams](#InlineValueParams), the
   --  response is of type [InlineValue[]](#InlineValue[]) or a Thenable
   --  that resolves to such.
   --
   --  @since 3.17.0

   procedure On_Moniker_Partial_Result
     (Self  : in out Progress_Report_Receiver;
      Token : LSP.Structures.ProgressToken;
      Value : LSP.Structures.Moniker_Vector) is null;
   --  A request to get the moniker of a symbol at a given
   --  text document position. The request parameter is of type
   --  [TextDocumentPositionParams](#TextDocumentPositionParams). The
   --  response is of type [Moniker[]](#Moniker[]) or `null`.

   procedure On_References_Partial_Result
     (Self  : in out Progress_Report_Receiver;
      Token : LSP.Structures.ProgressToken;
      Value : LSP.Structures.Location_Vector) is null;
   --  A request to resolve project-wide references for the symbol denoted
   --  by the given text document position. The request's parameter is of
   --  type [ReferenceParams](#ReferenceParams) the response is of type
   --  [Location[]](#Location) or a Thenable that resolves to such.

   procedure On_SelectionRange_Partial_Result
     (Self  : in out Progress_Report_Receiver;
      Token : LSP.Structures.ProgressToken;
      Value : LSP.Structures.SelectionRange_Vector) is null;
   --  A request to provide selection ranges in a document. The request's
   --  parameter is of type [SelectionRangeParams](#SelectionRangeParams), the
   --  response is of type [SelectionRange[]](#SelectionRange[]) or a Thenable
   --  that resolves to such.

   procedure On_Tokens_Full_Partial_Result
     (Self  : in out Progress_Report_Receiver;
      Token : LSP.Structures.ProgressToken;
      Value : LSP.Structures.SemanticTokensPartialResult) is null;
   --  @since 3.16.0

   procedure On_Tokens_Delta_Partial_Result
     (Self  : in out Progress_Report_Receiver;
      Token : LSP.Structures.ProgressToken;
      Value : LSP.Structures
        .SemanticTokensPartialResult_Or_SemanticTokensDeltaPartialResult) is null;
   --  @since 3.16.0

   procedure On_Tokens_Range_Partial_Result
     (Self  : in out Progress_Report_Receiver;
      Token : LSP.Structures.ProgressToken;
      Value : LSP.Structures.SemanticTokensPartialResult) is null;
   --  @since 3.16.0

   procedure On_TypeDefinition_Partial_Result
     (Self  : in out Progress_Report_Receiver;
      Token : LSP.Structures.ProgressToken;
      Value : LSP.Structures.Definition_Progress_Report) is null;
   --  A request to resolve the type definition locations of a symbol at
   --  a given text document position. The request's parameter is of type
   --  [TextDocumentPositionParams] (#TextDocumentPositionParams) the response
   --  is of type [Definition](#Definition) or a Thenable that resolves to
   --  such.

   procedure On_Subtypes_Partial_Result
     (Self  : in out Progress_Report_Receiver;
      Token : LSP.Structures.ProgressToken;
      Value : LSP.Structures.TypeHierarchyItem_Vector) is null;
   --  A request to resolve the subtypes for a given `TypeHierarchyItem`.
   --
   --  @since 3.17.0

   procedure On_Supertypes_Partial_Result
     (Self  : in out Progress_Report_Receiver;
      Token : LSP.Structures.ProgressToken;
      Value : LSP.Structures.TypeHierarchyItem_Vector) is null;
   --  A request to resolve the supertypes for a given `TypeHierarchyItem`.
   --
   --  @since 3.17.0

   procedure On_Workspace_Diagnostic_Partial_Result
     (Self  : in out Progress_Report_Receiver;
      Token : LSP.Structures.ProgressToken;
      Value : LSP.Structures.WorkspaceDiagnosticReportPartialResult) is null;
   --  The workspace diagnostic request definition.
   --
   --  @since 3.17.0

   procedure On_Symbol_Partial_Result
     (Self  : in out Progress_Report_Receiver;
      Token : LSP.Structures.ProgressToken;
      Value : LSP.Structures.Symbol_Progress_Report) is null;
   --  A request to list project-wide symbols matching the query string given
   --  by the [WorkspaceSymbolParams](#WorkspaceSymbolParams). The response
   --  is of type [SymbolInformation[]](#SymbolInformation) or a Thenable
   --  that resolves to such.
   --
   --  @since 3.17.0 - support for WorkspaceSymbol in the returned data.
   --  Clients
   --   need to advertise support for WorkspaceSymbols via the client capability
   --   `workspace.symbol.resolveSupport`.

   procedure On_ProgressBegin_Work_Done
     (Self  : in out Progress_Report_Receiver;
      Token : LSP.Structures.ProgressToken;
      Value : LSP.Structures.WorkDoneProgressBegin) is null;

   procedure On_ProgressReport_Work_Done
     (Self  : in out Progress_Report_Receiver;
      Token : LSP.Structures.ProgressToken;
      Value : LSP.Structures.WorkDoneProgressReport) is null;

   procedure On_ProgressEnd_Work_Done
     (Self  : in out Progress_Report_Receiver;
      Token : LSP.Structures.ProgressToken;
      Value : LSP.Structures.WorkDoneProgressEnd) is null;

end LSP.Progress_Report_Receivers;
