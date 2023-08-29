--
--  Copyright (C) <YEAR>, <COPYRIGHT HOLDER>
--
--  SPDX-License-Identifier: MIT
--
--  DON'T EDIT THIS FILE! It was generated from metaModel.json.
--

with LSP.Errors;
with LSP.Structures;

package LSP.Client_Response_Receivers is
   pragma Preelaborate;

   type Client_Response_Receiver is limited interface;

   procedure On_IncomingCalls_Response
     (Self  : in out Client_Response_Receiver;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.CallHierarchyIncomingCall_Vector_Or_Null) is null;
   --  A request to resolve the incoming calls for a given `CallHierarchyItem`.
   --
   --  @since 3.16.0

   procedure On_OutgoingCalls_Response
     (Self  : in out Client_Response_Receiver;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.CallHierarchyOutgoingCall_Vector_Or_Null) is null;
   --  A request to resolve the outgoing calls for a given `CallHierarchyItem`.
   --
   --  @since 3.16.0

   procedure On_Code_Action_Resolve_Response
     (Self  : in out Client_Response_Receiver;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.CodeAction) is null;
   --  Request to resolve additional information for a given code action.The
   --  request's parameter is of type [CodeAction](#CodeAction) the response is
   --  of type [CodeAction](#CodeAction) or a Thenable that resolves to such.

   procedure On_Code_Lens_Resolve_Response
     (Self  : in out Client_Response_Receiver;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.CodeLens) is null;
   --  A request to resolve a command for a given code lens.

   procedure On_Completion_Resolve_Response
     (Self  : in out Client_Response_Receiver;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.CompletionItem) is null;
   --  Request to resolve additional information for a given
   --  completion item.The request's parameter is of type
   --  [CompletionItem](#CompletionItem) the response is of type
   --  [CompletionItem](#CompletionItem) or a Thenable that resolves to such.

   procedure On_Link_Resolve_Response
     (Self  : in out Client_Response_Receiver;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.DocumentLink) is null;
   --  Request to resolve additional information for a given document link.
   --  The request's parameter is of type [DocumentLink](#DocumentLink) the
   --  response is of type [DocumentLink](#DocumentLink) or a Thenable that
   --  resolves to such.

   procedure On_Initialize_Response
     (Self  : in out Client_Response_Receiver;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.InitializeResult) is null;
   --  The initialize request is sent from the client to the server. It is sent
   --  once as the request after starting up the server. The requests parameter
   --  is of type [InitializeParams](#InitializeParams) the response if of type
   --  [InitializeResult](#InitializeResult) of a Thenable that resolves to
   --  such.

   procedure On_Inlay_Resolve_Response
     (Self  : in out Client_Response_Receiver;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.InlayHint) is null;
   --  A request to resolve additional properties for an inlay hint. The
   --  request's parameter is of type [InlayHint](#InlayHint), the response
   --  is of type [InlayHint](#InlayHint) or a Thenable that resolves to such.
   --
   --  @since 3.17.0

   procedure On_Shutdown_Response
     (Self  : in out Client_Response_Receiver;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Null_Record) is null;
   --  A shutdown request is sent from the client to the server. It is
   --  sent once when the client decides to shutdown the server. The only
   --  notification that is sent after a shutdown request is the exit event.

   procedure On_CodeAction_Response
     (Self  : in out Client_Response_Receiver;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Command_Or_CodeAction_Vector_Or_Null) is null;
   --  A request to provide commands for the given text document and range.

   procedure On_CodeLens_Response
     (Self  : in out Client_Response_Receiver;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.CodeLens_Vector_Or_Null) is null;
   --  A request to provide code lens for the given text document.

   procedure On_ColorPresentation_Response
     (Self  : in out Client_Response_Receiver;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.ColorPresentation_Vector) is null;
   --  A request to list all presentation for a color. The request's parameter
   --  is of type [ColorPresentationParams](#ColorPresentationParams) the
   --  response is of type [ColorInformation[]](#ColorInformation) or a
   --  Thenable that resolves to such.

   procedure On_Completion_Response
     (Self  : in out Client_Response_Receiver;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Completion_Result) is null;
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

   procedure On_Declaration_Response
     (Self  : in out Client_Response_Receiver;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Declaration_Result) is null;
   --  A request to resolve the type definition locations of a symbol at
   --  a given text document position. The request's parameter is of type
   --  [TextDocumentPositionParams] (#TextDocumentPositionParams) the
   --  response is of type [Declaration](#Declaration) or a typed array of
   --  [DeclarationLink](#DeclarationLink) or a Thenable that resolves to such.

   procedure On_Definition_Response
     (Self  : in out Client_Response_Receiver;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Definition_Result) is null;
   --  A request to resolve the definition location of a symbol at a
   --  given text document position. The request's parameter is of type
   --  [TextDocumentPosition] (#TextDocumentPosition) the response is
   --  of either type [Definition](#Definition) or a typed array of
   --  [DefinitionLink](#DefinitionLink) or a Thenable that resolves to such.

   procedure On_Diagnostic_Response
     (Self  : in out Client_Response_Receiver;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.DocumentDiagnosticReport) is null;
   --  The document diagnostic request definition.
   --
   --  @since 3.17.0

   procedure On_DocumentColor_Response
     (Self  : in out Client_Response_Receiver;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.ColorInformation_Vector) is null;
   --  A request to list all color symbols found in a given
   --  text document. The request's parameter is of type
   --  [DocumentColorParams](#DocumentColorParams) the response is of type
   --  [ColorInformation[]](#ColorInformation) or a Thenable that resolves
   --  to such.

   procedure On_DocumentHighlight_Response
     (Self  : in out Client_Response_Receiver;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.DocumentHighlight_Vector_Or_Null) is null;
   --  Request to resolve a [DocumentHighlight](#DocumentHighlight) for
   --  a given text document position. The request's parameter is of type
   --  [TextDocumentPosition] (#TextDocumentPosition) the request response is
   --  of type [DocumentHighlight[]] (#DocumentHighlight) or a Thenable that
   --  resolves to such.

   procedure On_DocumentLink_Response
     (Self  : in out Client_Response_Receiver;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.DocumentLink_Vector_Or_Null) is null;
   --  A request to provide document links

   procedure On_DocumentSymbol_Response
     (Self  : in out Client_Response_Receiver;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.DocumentSymbol_Result) is null;
   --  A request to list all symbols found in a given
   --  text document. The request's parameter is of type
   --  [TextDocumentIdentifier](#TextDocumentIdentifier) the response is
   --  of type [SymbolInformation[]](#SymbolInformation) or a Thenable
   --  that resolves to such.

   procedure On_FoldingRange_Response
     (Self  : in out Client_Response_Receiver;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.FoldingRange_Vector_Or_Null) is null;
   --  A request to provide folding ranges in a document. The request's
   --  parameter is of type [FoldingRangeParams](#FoldingRangeParams), the
   --  response is of type [FoldingRangeList](#FoldingRangeList) or a Thenable
   --  that resolves to such.

   procedure On_Formatting_Response
     (Self  : in out Client_Response_Receiver;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.TextEdit_Vector_Or_Null) is null;
   --  A request to to format a whole document.

   procedure On_Hover_Response
     (Self  : in out Client_Response_Receiver;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Hover_Or_Null) is null;
   --  Request to request hover information at a given text
   --  document position. The request's parameter is of type
   --  [TextDocumentPosition](#TextDocumentPosition) the response is of
   --  type [Hover](#Hover) or a Thenable that resolves to such.

   procedure On_Implementation_Response
     (Self  : in out Client_Response_Receiver;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Definition_Result) is null;
   --  A request to resolve the implementation locations of a symbol at
   --  a given text document position. The request's parameter is of type
   --  [TextDocumentPositionParams] (#TextDocumentPositionParams) the response
   --  is of type [Definition](#Definition) or a Thenable that resolves to
   --  such.

   procedure On_InlayHint_Response
     (Self  : in out Client_Response_Receiver;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.InlayHint_Vector_Or_Null) is null;
   --  A request to provide inlay hints in a document. The request's parameter
   --  is of type [InlayHintsParams](#InlayHintsParams), the response is of
   --  type [InlayHint[]](#InlayHint[]) or a Thenable that resolves to such.
   --
   --  @since 3.17.0

   procedure On_InlineValue_Response
     (Self  : in out Client_Response_Receiver;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.InlineValue_Vector_Or_Null) is null;
   --  A request to provide inline values in a document. The request's
   --  parameter is of type [InlineValueParams](#InlineValueParams), the
   --  response is of type [InlineValue[]](#InlineValue[]) or a Thenable
   --  that resolves to such.
   --
   --  @since 3.17.0

   procedure On_LinkedEditingRange_Response
     (Self  : in out Client_Response_Receiver;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.LinkedEditingRanges_Or_Null) is null;
   --  A request to provide ranges that can be edited together.
   --
   --  @since 3.16.0

   procedure On_Moniker_Response
     (Self  : in out Client_Response_Receiver;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Moniker_Vector_Or_Null) is null;
   --  A request to get the moniker of a symbol at a given
   --  text document position. The request parameter is of type
   --  [TextDocumentPositionParams](#TextDocumentPositionParams). The
   --  response is of type [Moniker[]](#Moniker[]) or `null`.

   procedure On_OnTypeFormatting_Response
     (Self  : in out Client_Response_Receiver;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.TextEdit_Vector_Or_Null) is null;
   --  A request to format a document on type.

   procedure On_PrepareCallHierarchy_Response
     (Self  : in out Client_Response_Receiver;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.CallHierarchyItem_Vector_Or_Null) is null;
   --  A request to result a `CallHierarchyItem` in a document at a given
   --  position. Can be used as an input to an incoming or outgoing call
   --  hierarchy.
   --
   --  @since 3.16.0

   procedure On_PrepareRename_Response
     (Self  : in out Client_Response_Receiver;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.PrepareRenameResult_Or_Null) is null;
   --  A request to test and perform the setup necessary for a rename.
   --
   --  @since 3.16 - support for default behavior

   procedure On_PrepareTypeHierarchy_Response
     (Self  : in out Client_Response_Receiver;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.TypeHierarchyItem_Vector_Or_Null) is null;
   --  A request to result a `TypeHierarchyItem` in a document at a given
   --  position. Can be used as an input to a subtypes or supertypes type
   --  hierarchy.
   --
   --  @since 3.17.0

   procedure On_RangeFormatting_Response
     (Self  : in out Client_Response_Receiver;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.TextEdit_Vector_Or_Null) is null;
   --  A request to to format a range in a document.

   procedure On_References_Response
     (Self  : in out Client_Response_Receiver;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Location_Vector_Or_Null) is null;
   --  A request to resolve project-wide references for the symbol denoted
   --  by the given text document position. The request's parameter is of
   --  type [ReferenceParams](#ReferenceParams) the response is of type
   --  [Location[]](#Location) or a Thenable that resolves to such.

   procedure On_Rename_Response
     (Self  : in out Client_Response_Receiver;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.WorkspaceEdit_Or_Null) is null;
   --  A request to rename a symbol.

   procedure On_SelectionRange_Response
     (Self  : in out Client_Response_Receiver;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.SelectionRange_Vector_Or_Null) is null;
   --  A request to provide selection ranges in a document. The request's
   --  parameter is of type [SelectionRangeParams](#SelectionRangeParams), the
   --  response is of type [SelectionRange[]](#SelectionRange[]) or a Thenable
   --  that resolves to such.

   procedure On_Full_Response
     (Self  : in out Client_Response_Receiver;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.SemanticTokens_Or_Null) is null;
   --  @since 3.16.0

   procedure On_Tokens_Delta_Response
     (Self  : in out Client_Response_Receiver;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Tokens_Delta_Result) is null;
   --  @since 3.16.0

   procedure On_Tokens_Range_Response
     (Self  : in out Client_Response_Receiver;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.SemanticTokens_Or_Null) is null;
   --  @since 3.16.0

   procedure On_SignatureHelp_Response
     (Self  : in out Client_Response_Receiver;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.SignatureHelp_Or_Null) is null;

   procedure On_TypeDefinition_Response
     (Self  : in out Client_Response_Receiver;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Definition_Result) is null;
   --  A request to resolve the type definition locations of a symbol at
   --  a given text document position. The request's parameter is of type
   --  [TextDocumentPositionParams] (#TextDocumentPositionParams) the response
   --  is of type [Definition](#Definition) or a Thenable that resolves to
   --  such.

   procedure On_WillSaveWaitUntil_Response
     (Self  : in out Client_Response_Receiver;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.TextEdit_Vector_Or_Null) is null;
   --  A document will save request is sent from the client to the server
   --  before the document is actually saved. The request can return an array
   --  of TextEdits which will be applied to the text document before it is
   --  saved. Please note that clients might drop results if computing the text
   --  edits took too long or if a server constantly fails on this request.
   --  This is done to keep the save fast and reliable.

   procedure On_Subtypes_Response
     (Self  : in out Client_Response_Receiver;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.TypeHierarchyItem_Vector_Or_Null) is null;
   --  A request to resolve the subtypes for a given `TypeHierarchyItem`.
   --
   --  @since 3.17.0

   procedure On_Supertypes_Response
     (Self  : in out Client_Response_Receiver;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.TypeHierarchyItem_Vector_Or_Null) is null;
   --  A request to resolve the supertypes for a given `TypeHierarchyItem`.
   --
   --  @since 3.17.0

   procedure On_Workspace_Diagnostic_Response
     (Self  : in out Client_Response_Receiver;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.WorkspaceDiagnosticReport) is null;
   --  The workspace diagnostic request definition.
   --
   --  @since 3.17.0

   procedure On_ExecuteCommand_Response
     (Self  : in out Client_Response_Receiver;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.LSPAny_Or_Null) is null;
   --  A request send from the client to the server to execute a command. The
   --  request might return a workspace edit which the client will apply to the
   --  workspace.

   procedure On_Symbol_Response
     (Self  : in out Client_Response_Receiver;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Symbol_Result) is null;
   --  A request to list project-wide symbols matching the query string given
   --  by the [WorkspaceSymbolParams](#WorkspaceSymbolParams). The response
   --  is of type [SymbolInformation[]](#SymbolInformation) or a Thenable
   --  that resolves to such.
   --
   --  @since 3.17.0 - support for WorkspaceSymbol in the returned data.
   --  Clients
   --   need to advertise support for WorkspaceSymbols via the client capability
   --   `workspace.symbol.resolveSupport`.

   procedure On_WillCreateFiles_Response
     (Self  : in out Client_Response_Receiver;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.WorkspaceEdit_Or_Null) is null;
   --  The will create files request is sent from the client to the server
   --  before files are actually created as long as the creation is triggered
   --  from within the client.
   --
   --  @since 3.16.0

   procedure On_WillDeleteFiles_Response
     (Self  : in out Client_Response_Receiver;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.WorkspaceEdit_Or_Null) is null;
   --  The did delete files notification is sent from the client to the server
   --  when files were deleted from within the client.
   --
   --  @since 3.16.0

   procedure On_WillRenameFiles_Response
     (Self  : in out Client_Response_Receiver;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.WorkspaceEdit_Or_Null) is null;
   --  The will rename files request is sent from the client to the server
   --  before files are actually renamed as long as the rename is triggered
   --  from within the client.
   --
   --  @since 3.16.0

   procedure On_Symbol_Resolve_Response
     (Self  : in out Client_Response_Receiver;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.WorkspaceSymbol) is null;
   --  A request to resolve the range inside the workspace symbol's location.
   --
   --  @since 3.17.0

   procedure On_Error_Response
     (Self  : in out Client_Response_Receiver;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Errors.ResponseError) is null;

end LSP.Client_Response_Receivers;
