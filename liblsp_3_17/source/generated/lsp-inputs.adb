--
--  Copyright (C) <YEAR>, <COPYRIGHT HOLDER>
--
--  SPDX-License-Identifier: MIT
--
--  DON'T EDIT THIS FILE! It was generated from metaModel.json.
--

with LSP.Inputs.Part_1;
with LSP.Inputs.Part_2;
with LSP.Inputs.Part_3;
with LSP.Inputs.Part_4;
with LSP.Inputs.Part_5;
with LSP.Inputs.Part_6;
with LSP.Inputs.Part_7;
with LSP.Inputs.Part_8;
with LSP.Inputs.Part_9;
with LSP.Inputs.Part_10;
with LSP.Inputs.Part_11;
with LSP.Inputs.Part_12;
with LSP.Inputs.Part_13;
with LSP.Inputs.Part_14;
with LSP.Inputs.Part_15;
with LSP.Inputs.Part_16;
with LSP.Inputs.Part_17;
with LSP.Inputs.Part_18;
with LSP.Inputs.Part_19;
with LSP.Inputs.Part_20;
with LSP.Inputs.Part_21;
with LSP.Inputs.Part_22;
with LSP.Inputs.Part_23;
with LSP.Inputs.Part_24;
with LSP.Inputs.Part_25;
with LSP.Inputs.Part_26;
with LSP.Inputs.Part_27;
with LSP.Inputs.Part_28;
with LSP.Inputs.Part_29;
with LSP.Inputs.Part_30;
with LSP.Inputs.Part_31;
with LSP.Inputs.Part_32;

package body LSP.Inputs is
   procedure Read_ClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ClientCapabilities) renames
     Part_27.Read_ClientCapabilities;

   procedure Read_FileChangeType
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.FileChangeType) renames
     Part_14.Read_FileChangeType;

   procedure Read_InitializeParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.InitializeParams) renames
     Part_18.Read_InitializeParams;

   procedure Read_DocumentLink_Vector_Or_Null
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentLink_Vector_Or_Null) renames
     Part_11.Read_DocumentLink_Vector_Or_Null;

   procedure Read_Command_Or_CodeAction_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Command_Or_CodeAction_Vector) renames
     Part_11.Read_Command_Or_CodeAction_Vector;

   procedure Read_Virtual_String
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Virtual_String) renames
     Part_31.Read_Virtual_String;

   procedure Read_Declaration
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Declaration) renames
     Part_22.Read_Declaration;

   procedure Read_Null_Record
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Null_Record) renames
     Part_13.Read_Null_Record;

   procedure Read_FileOperationRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.FileOperationRegistrationOptions) renames
     Part_8.Read_FileOperationRegistrationOptions;

   procedure Read_CallHierarchyOutgoingCall_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CallHierarchyOutgoingCall_Vector) renames
     Part_3.Read_CallHierarchyOutgoingCall_Vector;

   procedure Read_WorkspaceFolder_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.WorkspaceFolder_Vector) renames
     Part_17.Read_WorkspaceFolder_Vector;

   procedure Read_TextDocumentContentChangeEvent_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value : out LSP.Structures.TextDocumentContentChangeEvent_Vector) renames
     Part_19.Read_TextDocumentContentChangeEvent_Vector;

   procedure Read_FoldingRangeClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.FoldingRangeClientCapabilities) renames
     Part_5.Read_FoldingRangeClientCapabilities;

   procedure Read_CreateFile
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CreateFile) renames
     Part_8.Read_CreateFile;

   procedure Read_CompletionContext
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CompletionContext) renames
     Part_31.Read_CompletionContext;

   procedure Read_NotebookCell_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.NotebookCell_Vector) renames
     Part_13.Read_NotebookCell_Vector;

   procedure Read_DocumentDiagnosticParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentDiagnosticParams) renames
     Part_18.Read_DocumentDiagnosticParams;

   procedure Read_DocumentLink_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentLink_Vector) renames
     Part_29.Read_DocumentLink_Vector;

   procedure Read_CodeActionKind
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.CodeActionKind) renames
     Part_15.Read_CodeActionKind;

   procedure Read_RelatedUnchangedDocumentDiagnosticReport
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures
        .RelatedUnchangedDocumentDiagnosticReport) renames
     Part_22.Read_RelatedUnchangedDocumentDiagnosticReport;

   procedure Read_DidChangeWatchedFilesClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures
        .DidChangeWatchedFilesClientCapabilities) renames
     Part_9.Read_DidChangeWatchedFilesClientCapabilities;

   procedure Read_SemanticTokenTypes
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.SemanticTokenTypes) renames
     Part_32.Read_SemanticTokenTypes;

   procedure Read_AnnotatedTextEdit
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.AnnotatedTextEdit) renames
     Part_27.Read_AnnotatedTextEdit;

   procedure Read_DeclarationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DeclarationOptions) renames
     Part_6.Read_DeclarationOptions;

   procedure Read_SignatureInformation
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SignatureInformation) renames
     Part_14.Read_SignatureInformation;

   procedure Read_TextDocumentSaveReason
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.TextDocumentSaveReason) renames
     Part_19.Read_TextDocumentSaveReason;

   procedure Read_SemanticTokens
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SemanticTokens) renames
     Part_8.Read_SemanticTokens;

   procedure Read_InlineValueParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.InlineValueParams) renames
     Part_28.Read_InlineValueParams;

   procedure Read_TokenFormat
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.TokenFormat) renames
     Part_30.Read_TokenFormat;

   procedure Read_TextDocumentRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TextDocumentRegistrationOptions) renames
     Part_7.Read_TextDocumentRegistrationOptions;

   procedure Read_URI
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.URI) renames
     Part_28.Read_URI;

   procedure Read_SemanticTokenModifiers
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.SemanticTokenModifiers) renames
     Part_17.Read_SemanticTokenModifiers;

   procedure Read_ConfigurationItem
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ConfigurationItem) renames
     Part_17.Read_ConfigurationItem;

   procedure Read_DocumentOnTypeFormattingClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures
        .DocumentOnTypeFormattingClientCapabilities) renames
     Part_12.Read_DocumentOnTypeFormattingClientCapabilities;

   procedure Read_SymbolKind
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.SymbolKind) renames
     Part_12.Read_SymbolKind;

   procedure Read_DidSaveNotebookDocumentParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DidSaveNotebookDocumentParams) renames
     Part_24.Read_DidSaveNotebookDocumentParams;

   procedure Read_Declaration_Result
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Declaration_Result) renames
     Part_4.Read_Declaration_Result;

   procedure Read_Definition_Progress_Report
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Definition_Progress_Report) renames
     Part_20.Read_Definition_Progress_Report;

   procedure Read_SemanticTokensParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SemanticTokensParams) renames
     Part_12.Read_SemanticTokensParams;

   procedure Read_ColorInformation_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ColorInformation_Vector) renames
     Part_3.Read_ColorInformation_Vector;

   procedure Read_SelectionRangeClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SelectionRangeClientCapabilities) renames
     Part_6.Read_SelectionRangeClientCapabilities;

   procedure Read_DocumentSymbol_Progress_Report
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentSymbol_Progress_Report) renames
     Part_16.Read_DocumentSymbol_Progress_Report;

   procedure Read_CallHierarchyIncomingCall_Vector_Or_Null
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures
        .CallHierarchyIncomingCall_Vector_Or_Null) renames
     Part_5.Read_CallHierarchyIncomingCall_Vector_Or_Null;

   procedure Read_Range_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Range_Vector) renames
     Part_29.Read_Range_Vector;

   procedure Read_Definition
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Definition) renames
     Part_17.Read_Definition;

   procedure Read_NotebookDocumentFilter
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.NotebookDocumentFilter) renames
     Part_20.Read_NotebookDocumentFilter;

   procedure Read_Color
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Color) renames
     Part_25.Read_Color;

   procedure Read_FileOperationPattern
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.FileOperationPattern) renames
     Part_31.Read_FileOperationPattern;

   procedure Read_LinkedEditingRanges
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.LinkedEditingRanges) renames
     Part_5.Read_LinkedEditingRanges;

   procedure Read_Virtual_String_Or_MarkupContent
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Virtual_String_Or_MarkupContent) renames
     Part_11.Read_Virtual_String_Or_MarkupContent;

   procedure Read_CodeLens_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CodeLens_Vector) renames
     Part_27.Read_CodeLens_Vector;

   procedure Read_ServerCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ServerCapabilities) renames
     Part_7.Read_ServerCapabilities;

   procedure Read_DidChangeWatchedFilesRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures
        .DidChangeWatchedFilesRegistrationOptions) renames
     Part_7.Read_DidChangeWatchedFilesRegistrationOptions;

   procedure Read_CallHierarchyOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CallHierarchyOptions) renames
     Part_17.Read_CallHierarchyOptions;

   procedure Read_LSPAny
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.LSPAny) renames
     Part_13.Read_LSPAny;

   procedure Read_ImplementationRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ImplementationRegistrationOptions) renames
     Part_7.Read_ImplementationRegistrationOptions;

   procedure Read_LogTraceParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.LogTraceParams) renames
     Part_29.Read_LogTraceParams;

   procedure Read_FoldingRangeRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.FoldingRangeRegistrationOptions) renames
     Part_11.Read_FoldingRangeRegistrationOptions;

   procedure Read_InlineValueContext
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.InlineValueContext) renames
     Part_25.Read_InlineValueContext;

   procedure Read_TextDocumentIdentifier_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TextDocumentIdentifier_Vector) renames
     Part_19.Read_TextDocumentIdentifier_Vector;

   procedure Read_DefinitionLink
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DefinitionLink) renames
     Part_17.Read_DefinitionLink;

   procedure Read_DocumentFormattingOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentFormattingOptions) renames
     Part_30.Read_DocumentFormattingOptions;

   procedure Read_SymbolInformation_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SymbolInformation_Vector) renames
     Part_10.Read_SymbolInformation_Vector;

   procedure Read_RenameFile
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.RenameFile) renames
     Part_4.Read_RenameFile;

   procedure Read_PrepareRenameResult_Or_Null
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.PrepareRenameResult_Or_Null) renames
     Part_28.Read_PrepareRenameResult_Or_Null;

   procedure Read_DidChangeConfigurationRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures
        .DidChangeConfigurationRegistrationOptions) renames
     Part_12.Read_DidChangeConfigurationRegistrationOptions;

   procedure Read_CallHierarchyItem
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CallHierarchyItem) renames
     Part_22.Read_CallHierarchyItem;

   procedure Read_SymbolTag_Set
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SymbolTag_Set) renames
     Part_13.Read_SymbolTag_Set;

   procedure Read_CompletionItem_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CompletionItem_Vector) renames
     Part_19.Read_CompletionItem_Vector;

   procedure Read_RelativePattern
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.RelativePattern) renames
     Part_26.Read_RelativePattern;

   procedure Read_Moniker_Vector_Or_Null
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Moniker_Vector_Or_Null) renames
     Part_29.Read_Moniker_Vector_Or_Null;

   procedure Read_WorkDoneProgressReport
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.WorkDoneProgressReport) renames
     Part_8.Read_WorkDoneProgressReport;

   procedure Read_ReferenceOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ReferenceOptions) renames
     Part_15.Read_ReferenceOptions;

   procedure Read_tagSupport_OfWorkspaceSymbolClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures
        .tagSupport_OfWorkspaceSymbolClientCapabilities) renames
     Part_3.Read_tagSupport_OfWorkspaceSymbolClientCapabilities;

   procedure Read_TextDocumentClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TextDocumentClientCapabilities) renames
     Part_9.Read_TextDocumentClientCapabilities;

   procedure Read_WorkspaceSymbolParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.WorkspaceSymbolParams) renames
     Part_11.Read_WorkspaceSymbolParams;

   procedure Read_DocumentSymbol_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentSymbol_Vector) renames
     Part_11.Read_DocumentSymbol_Vector;

   procedure Read_GlobPattern
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.GlobPattern) renames
     Part_12.Read_GlobPattern;

   procedure Read_NotebookDocumentIdentifier
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.NotebookDocumentIdentifier) renames
     Part_1.Read_NotebookDocumentIdentifier;

   procedure Read_InsertReplaceEdit
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.InsertReplaceEdit) renames
     Part_19.Read_InsertReplaceEdit;

   procedure Read_InitializeResult
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.InitializeResult) renames
     Part_21.Read_InitializeResult;

   procedure Read_InlineValueVariableLookup
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.InlineValueVariableLookup) renames
     Part_4.Read_InlineValueVariableLookup;

   procedure Read_FileEvent
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.FileEvent) renames
     Part_32.Read_FileEvent;

   procedure Read_SignatureHelpClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SignatureHelpClientCapabilities) renames
     Part_32.Read_SignatureHelpClientCapabilities;

   procedure Read_WorkDoneProgressCancelParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.WorkDoneProgressCancelParams) renames
     Part_4.Read_WorkDoneProgressCancelParams;

   procedure Read_NotebookDocumentSyncClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures
        .NotebookDocumentSyncClientCapabilities) renames
     Part_28.Read_NotebookDocumentSyncClientCapabilities;

   procedure Read_ProgressToken
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ProgressToken) renames
     Part_4.Read_ProgressToken;

   procedure Read_DiagnosticOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DiagnosticOptions) renames
     Part_3.Read_DiagnosticOptions;

   procedure Read_WindowClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.WindowClientCapabilities) renames
     Part_23.Read_WindowClientCapabilities;

   procedure Read_GeneralClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.GeneralClientCapabilities) renames
     Part_13.Read_GeneralClientCapabilities;

   procedure Read_SemanticTokensWorkspaceClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures
        .SemanticTokensWorkspaceClientCapabilities) renames
     Part_14.Read_SemanticTokensWorkspaceClientCapabilities;

   procedure Read_LinkedEditingRangeClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value : out LSP.Structures.LinkedEditingRangeClientCapabilities) renames
     Part_9.Read_LinkedEditingRangeClientCapabilities;

   procedure Read_SignatureHelp
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SignatureHelp) renames
     Part_1.Read_SignatureHelp;

   procedure Read_DidChangeWatchedFilesParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DidChangeWatchedFilesParams) renames
     Part_28.Read_DidChangeWatchedFilesParams;

   procedure Read_DidOpenNotebookDocumentParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DidOpenNotebookDocumentParams) renames
     Part_3.Read_DidOpenNotebookDocumentParams;

   procedure Read_SignatureHelp_Or_Null
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SignatureHelp_Or_Null) renames
     Part_23.Read_SignatureHelp_Or_Null;

   procedure Read_Diagnostic_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Diagnostic_Vector) renames
     Part_5.Read_Diagnostic_Vector;

   procedure Read_DocumentHighlightKind
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.DocumentHighlightKind) renames
     Part_19.Read_DocumentHighlightKind;

   procedure Read_CodeLens_Vector_Or_Null
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CodeLens_Vector_Or_Null) renames
     Part_13.Read_CodeLens_Vector_Or_Null;

   procedure Read_ResourceOperationKind
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.ResourceOperationKind) renames
     Part_3.Read_ResourceOperationKind;

   procedure Read_WorkspaceFolder_Vector_Or_Null
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.WorkspaceFolder_Vector_Or_Null) renames
     Part_7.Read_WorkspaceFolder_Vector_Or_Null;

   procedure Read_InlineValueClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.InlineValueClientCapabilities) renames
     Part_9.Read_InlineValueClientCapabilities;

   procedure Read_PrepareRenameResult
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.PrepareRenameResult) renames
     Part_14.Read_PrepareRenameResult;

   procedure Read_DocumentLinkParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentLinkParams) renames
     Part_23.Read_DocumentLinkParams;

   procedure Read_DocumentLinkClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentLinkClientCapabilities) renames
     Part_6.Read_DocumentLinkClientCapabilities;

   procedure Read_MarkupKind_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.MarkupKind_Vector) renames
     Part_28.Read_MarkupKind_Vector;

   procedure Read_CodeActionContext
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CodeActionContext) renames
     Part_10.Read_CodeActionContext;

   procedure Read_ConfigurationParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ConfigurationParams) renames
     Part_26.Read_ConfigurationParams;

   procedure Read_DidChangeTextDocumentParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DidChangeTextDocumentParams) renames
     Part_21.Read_DidChangeTextDocumentParams;

   procedure Read_InlayHintRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.InlayHintRegistrationOptions) renames
     Part_29.Read_InlayHintRegistrationOptions;

   procedure Read_Declaration_Progress_Report
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Declaration_Progress_Report) renames
     Part_23.Read_Declaration_Progress_Report;

   procedure Read_Natural_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Natural_Vector) renames
     Part_3.Read_Natural_Vector;

   procedure Read_CodeDescription
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CodeDescription) renames
     Part_15.Read_CodeDescription;

   procedure Read_Virtual_String_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Virtual_String_Vector) renames
     Part_25.Read_Virtual_String_Vector;

   procedure Read_LSPObject
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.LSPObject) renames
     Part_32.Read_LSPObject;

   procedure Read_TextDocumentPositionParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TextDocumentPositionParams) renames
     Part_19.Read_TextDocumentPositionParams;

   procedure Read_Boolean_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Boolean_Vector) renames
     Part_8.Read_Boolean_Vector;

   procedure Read_DidSaveTextDocumentParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DidSaveTextDocumentParams) renames
     Part_4.Read_DidSaveTextDocumentParams;

   procedure Read_CodeLensRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CodeLensRegistrationOptions) renames
     Part_30.Read_CodeLensRegistrationOptions;

   procedure Read_SelectionRange
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SelectionRange) renames
     Part_27.Read_SelectionRange;

   procedure Read_NotebookCellTextDocumentFilter
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.NotebookCellTextDocumentFilter) renames
     Part_9.Read_NotebookCellTextDocumentFilter;

   procedure Read_WorkspaceFoldersInitializeParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.WorkspaceFoldersInitializeParams) renames
     Part_2.Read_WorkspaceFoldersInitializeParams;

   procedure Read_ShowMessageParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ShowMessageParams) renames
     Part_18.Read_ShowMessageParams;

   procedure Read_FileSystemWatcher
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.FileSystemWatcher) renames
     Part_9.Read_FileSystemWatcher;

   procedure Read_CodeActionKind_Set
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CodeActionKind_Set) renames
     Part_4.Read_CodeActionKind_Set;

   procedure Read_NotebookDocumentSyncOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.NotebookDocumentSyncOptions) renames
     Part_5.Read_NotebookDocumentSyncOptions;

   procedure Read_CodeActionRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CodeActionRegistrationOptions) renames
     Part_24.Read_CodeActionRegistrationOptions;

   procedure Read_NotebookDocumentSyncRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures
        .NotebookDocumentSyncRegistrationOptions) renames
     Part_6.Read_NotebookDocumentSyncRegistrationOptions;

   procedure Read_WorkspaceUnchangedDocumentDiagnosticReport
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures
        .WorkspaceUnchangedDocumentDiagnosticReport) renames
     Part_32.Read_WorkspaceUnchangedDocumentDiagnosticReport;

   procedure Read_ReferenceParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ReferenceParams) renames
     Part_3.Read_ReferenceParams;

   procedure Read_HoverClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.HoverClientCapabilities) renames
     Part_13.Read_HoverClientCapabilities;

   procedure Read_RenameClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.RenameClientCapabilities) renames
     Part_27.Read_RenameClientCapabilities;

   procedure Read_DidChangeConfigurationParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DidChangeConfigurationParams) renames
     Part_7.Read_DidChangeConfigurationParams;

   procedure Read_DefinitionOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DefinitionOptions) renames
     Part_17.Read_DefinitionOptions;

   procedure Read_Hover_Or_Null
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Hover_Or_Null) renames
     Part_6.Read_Hover_Or_Null;

   procedure Read_InlayHintKind
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.InlayHintKind) renames
     Part_22.Read_InlayHintKind;

   procedure Read_symbolKind_OfWorkspaceSymbolClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures
        .symbolKind_OfWorkspaceSymbolClientCapabilities) renames
     Part_2.Read_symbolKind_OfWorkspaceSymbolClientCapabilities;

   procedure Read_CallHierarchyOutgoingCall_Vector_Or_Null
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures
        .CallHierarchyOutgoingCall_Vector_Or_Null) renames
     Part_21.Read_CallHierarchyOutgoingCall_Vector_Or_Null;

   procedure Read_FailureHandlingKind
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.FailureHandlingKind) renames
     Part_3.Read_FailureHandlingKind;

   procedure Read_FileOperationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.FileOperationOptions) renames
     Part_7.Read_FileOperationOptions;

   procedure Read_ShowDocumentResult
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ShowDocumentResult) renames
     Part_1.Read_ShowDocumentResult;

   procedure Read_TypeDefinitionParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TypeDefinitionParams) renames
     Part_5.Read_TypeDefinitionParams;

   procedure Read_DocumentSelector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentSelector) renames
     Part_16.Read_DocumentSelector;

   procedure Read_CompletionClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CompletionClientCapabilities) renames
     Part_15.Read_CompletionClientCapabilities;

   procedure Read_CodeLensWorkspaceClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CodeLensWorkspaceClientCapabilities) renames
     Part_7.Read_CodeLensWorkspaceClientCapabilities;

   procedure Read_RenameOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.RenameOptions) renames
     Part_22.Read_RenameOptions;

   procedure Read_SymbolKind_Set
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SymbolKind_Set) renames
     Part_7.Read_SymbolKind_Set;

   procedure Read_DidChangeConfigurationClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures
        .DidChangeConfigurationClientCapabilities) renames
     Part_22.Read_DidChangeConfigurationClientCapabilities;

   procedure Read_DocumentHighlightRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value : out LSP.Structures.DocumentHighlightRegistrationOptions) renames
     Part_12.Read_DocumentHighlightRegistrationOptions;

   procedure Read_DeclarationLink
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DeclarationLink) renames
     Part_22.Read_DeclarationLink;

   procedure Read_T
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.T) renames
     Part_2.Read_T;

   procedure Read_FullDocumentDiagnosticReport
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.FullDocumentDiagnosticReport) renames
     Part_17.Read_FullDocumentDiagnosticReport;

   procedure Read_RenameParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.RenameParams) renames
     Part_10.Read_RenameParams;

   procedure Read_DocumentSymbolRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentSymbolRegistrationOptions) renames
     Part_14.Read_DocumentSymbolRegistrationOptions;

   procedure Read_LSPAny_Or_Null
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.LSPAny_Or_Null) renames
     Part_27.Read_LSPAny_Or_Null;

   procedure Read_DeclarationRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DeclarationRegistrationOptions) renames
     Part_5.Read_DeclarationRegistrationOptions;

   procedure Read_WorkspaceSymbol
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.WorkspaceSymbol) renames
     Part_7.Read_WorkspaceSymbol;

   procedure Read_TypeHierarchyRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TypeHierarchyRegistrationOptions) renames
     Part_28.Read_TypeHierarchyRegistrationOptions;

   procedure Read_DeleteFileOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DeleteFileOptions) renames
     Part_11.Read_DeleteFileOptions;

   procedure Read_WatchKind
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.WatchKind) renames
     Part_21.Read_WatchKind;

   procedure Read_DiagnosticSeverity
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.DiagnosticSeverity) renames
     Part_18.Read_DiagnosticSeverity;

   procedure Read_LSPAny_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.LSPAny_Vector) renames
     Part_11.Read_LSPAny_Vector;

   procedure Read_PrepareRenameParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.PrepareRenameParams) renames
     Part_9.Read_PrepareRenameParams;

   procedure Read_CodeActionTriggerKind
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.CodeActionTriggerKind) renames
     Part_23.Read_CodeActionTriggerKind;

   procedure Read_CodeActionClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CodeActionClientCapabilities) renames
     Part_10.Read_CodeActionClientCapabilities;

   procedure Read_MarkdownClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.MarkdownClientCapabilities) renames
     Part_14.Read_MarkdownClientCapabilities;

   procedure Read_CancelParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CancelParams) renames
     Part_16.Read_CancelParams;

   procedure Read_DefinitionLink_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DefinitionLink_Vector) renames
     Part_23.Read_DefinitionLink_Vector;

   procedure Read_DefinitionParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DefinitionParams) renames
     Part_29.Read_DefinitionParams;

   procedure Read_ImplementationClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ImplementationClientCapabilities) renames
     Part_9.Read_ImplementationClientCapabilities;

   procedure Read_TextDocumentItem
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TextDocumentItem) renames
     Part_19.Read_TextDocumentItem;

   procedure Read_ColorPresentationParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ColorPresentationParams) renames
     Part_19.Read_ColorPresentationParams;

   procedure Read_ErrorCodes
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.ErrorCodes) renames
     Part_20.Read_ErrorCodes;

   procedure Read_InsertTextFormat
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.InsertTextFormat) renames
     Part_13.Read_InsertTextFormat;

   procedure Read_relatedDocuments_OfDocumentDiagnosticReportPartialResult_Item
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures
        .relatedDocuments_OfDocumentDiagnosticReportPartialResult_Item) renames
     Part_7.Read_relatedDocuments_OfDocumentDiagnosticReportPartialResult_Item;

   procedure Read_ExecuteCommandClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ExecuteCommandClientCapabilities) renames
     Part_3.Read_ExecuteCommandClientCapabilities;

   procedure Read_FoldingRange_Vector_Or_Null
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.FoldingRange_Vector_Or_Null) renames
     Part_28.Read_FoldingRange_Vector_Or_Null;

   procedure Read_CallHierarchyItem_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CallHierarchyItem_Vector) renames
     Part_4.Read_CallHierarchyItem_Vector;

   procedure Read_DeleteFile
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DeleteFile) renames
     Part_11.Read_DeleteFile;

   procedure Read_SaveOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SaveOptions) renames
     Part_5.Read_SaveOptions;

   procedure Read_NotebookDocument
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.NotebookDocument) renames
     Part_4.Read_NotebookDocument;

   procedure Read_RelatedFullDocumentDiagnosticReport
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.RelatedFullDocumentDiagnosticReport) renames
     Part_16.Read_RelatedFullDocumentDiagnosticReport;

   procedure Read_CodeLensClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CodeLensClientCapabilities) renames
     Part_4.Read_CodeLensClientCapabilities;

   procedure Read_DocumentSymbolOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentSymbolOptions) renames
     Part_13.Read_DocumentSymbolOptions;

   procedure Read_CompletionRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CompletionRegistrationOptions) renames
     Part_1.Read_CompletionRegistrationOptions;

   procedure Read_InlayHintClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.InlayHintClientCapabilities) renames
     Part_19.Read_InlayHintClientCapabilities;

   procedure Read_ReferenceContext
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ReferenceContext) renames
     Part_18.Read_ReferenceContext;

   procedure Read_WorkspaceEditClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.WorkspaceEditClientCapabilities) renames
     Part_20.Read_WorkspaceEditClientCapabilities;

   procedure Read_WorkspaceSymbolOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.WorkspaceSymbolOptions) renames
     Part_7.Read_WorkspaceSymbolOptions;

   procedure Read_MessageActionItem_Or_Null
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.MessageActionItem_Or_Null) renames
     Part_20.Read_MessageActionItem_Or_Null;

   procedure Read_ChangeAnnotation
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ChangeAnnotation) renames
     Part_5.Read_ChangeAnnotation;

   procedure Read_FileOperationPatternOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.FileOperationPatternOptions) renames
     Part_31.Read_FileOperationPatternOptions;

   procedure Read_VersionedTextDocumentIdentifier
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.VersionedTextDocumentIdentifier) renames
     Part_16.Read_VersionedTextDocumentIdentifier;

   procedure Read_SemanticTokensDelta
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SemanticTokensDelta) renames
     Part_32.Read_SemanticTokensDelta;

   procedure Read_ShowDocumentClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ShowDocumentClientCapabilities) renames
     Part_23.Read_ShowDocumentClientCapabilities;

   procedure Read_ApplyWorkspaceEditParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ApplyWorkspaceEditParams) renames
     Part_5.Read_ApplyWorkspaceEditParams;

   procedure Read_InlayHintLabelPart
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.InlayHintLabelPart) renames
     Part_29.Read_InlayHintLabelPart;

   procedure Read_SelectionRangeOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SelectionRangeOptions) renames
     Part_27.Read_SelectionRangeOptions;

   procedure Read_LocationLink
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.LocationLink) renames
     Part_9.Read_LocationLink;

   procedure Read_CompletionList
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CompletionList) renames
     Part_4.Read_CompletionList;

   procedure Read_DiagnosticRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DiagnosticRegistrationOptions) renames
     Part_4.Read_DiagnosticRegistrationOptions;

   procedure Read_SignatureHelpTriggerKind
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.SignatureHelpTriggerKind) renames
     Part_1.Read_SignatureHelpTriggerKind;

   procedure Read_CodeAction
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CodeAction) renames
     Part_23.Read_CodeAction;

   procedure Read_InlayHint_Vector_Or_Null
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.InlayHint_Vector_Or_Null) renames
     Part_14.Read_InlayHint_Vector_Or_Null;

   procedure Read_DeclarationParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DeclarationParams) renames
     Part_26.Read_DeclarationParams;

   procedure Read_DocumentRangeFormattingParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentRangeFormattingParams) renames
     Part_15.Read_DocumentRangeFormattingParams;

   procedure Read_TypeHierarchyClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TypeHierarchyClientCapabilities) renames
     Part_6.Read_TypeHierarchyClientCapabilities;

   procedure Read_DocumentOnTypeFormattingOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentOnTypeFormattingOptions) renames
     Part_21.Read_DocumentOnTypeFormattingOptions;

   procedure Read_WorkspaceSymbolRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.WorkspaceSymbolRegistrationOptions) renames
     Part_8.Read_WorkspaceSymbolRegistrationOptions;

   procedure Read_MonikerParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.MonikerParams) renames
     Part_1.Read_MonikerParams;

   procedure Read_InlayHint
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.InlayHint) renames
     Part_14.Read_InlayHint;

   procedure Read_FileDelete
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.FileDelete) renames
     Part_11.Read_FileDelete;

   procedure Read_DocumentFormattingRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value : out LSP.Structures.DocumentFormattingRegistrationOptions) renames
     Part_29.Read_DocumentFormattingRegistrationOptions;

   procedure Read_ColorPresentation
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ColorPresentation) renames
     Part_31.Read_ColorPresentation;

   procedure Read_UnregistrationParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.UnregistrationParams) renames
     Part_4.Read_UnregistrationParams;

   procedure Read_SelectionRangeParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SelectionRangeParams) renames
     Part_23.Read_SelectionRangeParams;

   procedure Read_Tokens_Delta_Result
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Tokens_Delta_Result) renames
     Part_13.Read_Tokens_Delta_Result;

   procedure Read_RenameFilesParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.RenameFilesParams) renames
     Part_29.Read_RenameFilesParams;

   procedure Read_TypeHierarchyItem
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TypeHierarchyItem) renames
     Part_32.Read_TypeHierarchyItem;

   procedure Read_Boolean_Or_Any
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Boolean_Or_Any) renames
     Part_27.Read_Boolean_Or_Any;

   procedure Read_Location
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Location) renames
     Part_9.Read_Location;

   procedure Read_RenameRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.RenameRegistrationOptions) renames
     Part_21.Read_RenameRegistrationOptions;

   procedure Read_ParameterInformation
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ParameterInformation) renames
     Part_17.Read_ParameterInformation;

   procedure Read_PositionEncodingKind
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.PositionEncodingKind) renames
     Part_8.Read_PositionEncodingKind;

   procedure Read_Symbol_Progress_Report
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Symbol_Progress_Report) renames
     Part_17.Read_Symbol_Progress_Report;

   procedure Read_MessageType
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.MessageType) renames
     Part_7.Read_MessageType;

   procedure Read_An_InitializeParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.An_InitializeParams) renames
     Part_18.Read_An_InitializeParams;

   procedure Read_TypeHierarchySubtypesParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TypeHierarchySubtypesParams) renames
     Part_26.Read_TypeHierarchySubtypesParams;

   procedure Read_FileOperationFilter
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.FileOperationFilter) renames
     Part_23.Read_FileOperationFilter;

   procedure Read_ApplyWorkspaceEditResult
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ApplyWorkspaceEditResult) renames
     Part_2.Read_ApplyWorkspaceEditResult;

   procedure Read_SemanticTokensPartialResult
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SemanticTokensPartialResult) renames
     Part_30.Read_SemanticTokensPartialResult;

   procedure Read_Completion_Result
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Completion_Result) renames
     Part_8.Read_Completion_Result;

   procedure Read_MonikerOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.MonikerOptions) renames
     Part_13.Read_MonikerOptions;

   procedure Read_InlineValue
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.InlineValue) renames
     Part_24.Read_InlineValue;

   procedure Read_Virtual_String_Or_NotebookDocumentFilter
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures
        .Virtual_String_Or_NotebookDocumentFilter) renames
     Part_22.Read_Virtual_String_Or_NotebookDocumentFilter;

   procedure Read_MonikerKind
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.MonikerKind) renames
     Part_21.Read_MonikerKind;

   procedure Read_NotebookCellKind
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.NotebookCellKind) renames
     Part_19.Read_NotebookCellKind;

   procedure Read_CallHierarchyItem_Vector_Or_Null
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CallHierarchyItem_Vector_Or_Null) renames
     Part_22.Read_CallHierarchyItem_Vector_Or_Null;

   procedure Read_DidCloseTextDocumentParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DidCloseTextDocumentParams) renames
     Part_5.Read_DidCloseTextDocumentParams;

   procedure Read_CallHierarchyOutgoingCallsParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CallHierarchyOutgoingCallsParams) renames
     Part_12.Read_CallHierarchyOutgoingCallsParams;

   procedure Read_SelectionRange_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SelectionRange_Vector) renames
     Part_29.Read_SelectionRange_Vector;

   procedure Read_HoverRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.HoverRegistrationOptions) renames
     Part_3.Read_HoverRegistrationOptions;

   procedure Read_CompletionItemTag
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.CompletionItemTag) renames
     Part_23.Read_CompletionItemTag;

   procedure Read_SemanticTokensDeltaParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SemanticTokensDeltaParams) renames
     Part_20.Read_SemanticTokensDeltaParams;

   procedure Read_DidChangeWorkspaceFoldersParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DidChangeWorkspaceFoldersParams) renames
     Part_23.Read_DidChangeWorkspaceFoldersParams;

   procedure Read_DefinitionRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DefinitionRegistrationOptions) renames
     Part_18.Read_DefinitionRegistrationOptions;

   procedure Read_AlsCheckSyntaxResult
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.AlsCheckSyntaxResult) renames
     Part_30.Read_AlsCheckSyntaxResult;

   procedure Read_clientInfo_Of_InitializeParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.clientInfo_Of_InitializeParams) renames
     Part_16.Read_clientInfo_Of_InitializeParams;

   procedure Read_CodeLensParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CodeLensParams) renames
     Part_17.Read_CodeLensParams;

   procedure Read_DiagnosticTag
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.DiagnosticTag) renames
     Part_1.Read_DiagnosticTag;

   procedure Read_TypeHierarchyItem_Vector_Or_Null
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TypeHierarchyItem_Vector_Or_Null) renames
     Part_32.Read_TypeHierarchyItem_Vector_Or_Null;

   procedure Read_Symbol_Result
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Symbol_Result) renames
     Part_6.Read_Symbol_Result;

   procedure Read_DiagnosticClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DiagnosticClientCapabilities) renames
     Part_30.Read_DiagnosticClientCapabilities;

   procedure Read_MonikerRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.MonikerRegistrationOptions) renames
     Part_14.Read_MonikerRegistrationOptions;

   procedure Read_ExecutionSummary
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ExecutionSummary) renames
     Part_28.Read_ExecutionSummary;

   procedure Read_TypeHierarchySupertypesParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TypeHierarchySupertypesParams) renames
     Part_13.Read_TypeHierarchySupertypesParams;

   procedure Read_PreviousResultId
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.PreviousResultId) renames
     Part_17.Read_PreviousResultId;

   procedure Read_FoldingRangeKind
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.FoldingRangeKind) renames
     Part_4.Read_FoldingRangeKind;

   procedure Read_PublishDiagnosticsParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.PublishDiagnosticsParams) renames
     Part_5.Read_PublishDiagnosticsParams;

   procedure Read_DocumentFilter
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentFilter) renames
     Part_11.Read_DocumentFilter;

   procedure Read_WorkspaceDiagnosticReport
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.WorkspaceDiagnosticReport) renames
     Part_26.Read_WorkspaceDiagnosticReport;

   procedure Read_CallHierarchyIncomingCall
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CallHierarchyIncomingCall) renames
     Part_21.Read_CallHierarchyIncomingCall;

   procedure Read_TextEdit_Vector_Or_Null
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TextEdit_Vector_Or_Null) renames
     Part_5.Read_TextEdit_Vector_Or_Null;

   procedure Read_LinkedEditingRanges_Or_Null
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.LinkedEditingRanges_Or_Null) renames
     Part_19.Read_LinkedEditingRanges_Or_Null;

   procedure Read_TextDocumentChangeRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value : out LSP.Structures.TextDocumentChangeRegistrationOptions) renames
     Part_17.Read_TextDocumentChangeRegistrationOptions;

   procedure Read_DeclarationLink_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DeclarationLink_Vector) renames
     Part_4.Read_DeclarationLink_Vector;

   procedure Read_SetTraceParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SetTraceParams) renames
     Part_27.Read_SetTraceParams;

   procedure Read_DocumentFormattingClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value : out LSP.Structures.DocumentFormattingClientCapabilities) renames
     Part_19.Read_DocumentFormattingClientCapabilities;

   procedure Read_NotebookDocumentChangeEvent
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.NotebookDocumentChangeEvent) renames
     Part_10.Read_NotebookDocumentChangeEvent;

   procedure Read_FormattingOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.FormattingOptions) renames
     Part_19.Read_FormattingOptions;

   procedure Read_WorkspaceDiagnosticReportPartialResult
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures
        .WorkspaceDiagnosticReportPartialResult) renames
     Part_4.Read_WorkspaceDiagnosticReportPartialResult;

   procedure Read_resolveSupport_OfWorkspaceSymbolClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures
        .resolveSupport_OfWorkspaceSymbolClientCapabilities) renames
     Part_21.Read_resolveSupport_OfWorkspaceSymbolClientCapabilities;

   procedure Read_DidOpenTextDocumentParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DidOpenTextDocumentParams) renames
     Part_23.Read_DidOpenTextDocumentParams;

   procedure Read_WorkspaceSymbol_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.WorkspaceSymbol_Vector) renames
     Part_1.Read_WorkspaceSymbol_Vector;

   procedure Read_TypeDefinitionRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TypeDefinitionRegistrationOptions) renames
     Part_10.Read_TypeDefinitionRegistrationOptions;

   procedure Read_CodeLensOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CodeLensOptions) renames
     Part_29.Read_CodeLensOptions;

   procedure Read_FileRename
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.FileRename) renames
     Part_4.Read_FileRename;

   procedure Read_SemanticTokensPartialResult_Or_SemanticTokensDeltaPartialResult
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures
        .SemanticTokensPartialResult_Or_SemanticTokensDeltaPartialResult) renames
     Part_1
       .Read_SemanticTokensPartialResult_Or_SemanticTokensDeltaPartialResult;

   procedure Read_DocumentDiagnosticReport
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentDiagnosticReport) renames
     Part_20.Read_DocumentDiagnosticReport;

   procedure Read_DocumentRangeFormattingClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures
        .DocumentRangeFormattingClientCapabilities) renames
     Part_30.Read_DocumentRangeFormattingClientCapabilities;

   procedure Read_Moniker
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Moniker) renames
     Part_13.Read_Moniker;

   procedure Read_ExecuteCommandParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ExecuteCommandParams) renames
     Part_18.Read_ExecuteCommandParams;

   procedure Read_WorkDoneProgressBegin
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.WorkDoneProgressBegin) renames
     Part_15.Read_WorkDoneProgressBegin;

   procedure Read_AlsDisplayMethodAncestryOnNavigationPolicy
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations
        .AlsDisplayMethodAncestryOnNavigationPolicy) renames
     Part_17.Read_AlsDisplayMethodAncestryOnNavigationPolicy;

   procedure Read_DiagnosticWorkspaceClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value : out LSP.Structures.DiagnosticWorkspaceClientCapabilities) renames
     Part_25.Read_DiagnosticWorkspaceClientCapabilities;

   procedure Read_DefinitionClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DefinitionClientCapabilities) renames
     Part_16.Read_DefinitionClientCapabilities;

   procedure Read_SignatureHelpRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SignatureHelpRegistrationOptions) renames
     Part_2.Read_SignatureHelpRegistrationOptions;

   procedure Read_TypeDefinitionOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TypeDefinitionOptions) renames
     Part_9.Read_TypeDefinitionOptions;

   procedure Read_MarkupContent
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.MarkupContent) renames
     Part_13.Read_MarkupContent;

   procedure Read_WorkspaceEdit
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.WorkspaceEdit) renames
     Part_13.Read_WorkspaceEdit;

   procedure Read_AlsVisibility
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.AlsVisibility) renames
     Part_30.Read_AlsVisibility;

   procedure Read_DocumentHighlight
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentHighlight) renames
     Part_11.Read_DocumentHighlight;

   procedure Read_MessageActionItem
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.MessageActionItem) renames
     Part_6.Read_MessageActionItem;

   procedure Read_TextDocumentItem_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TextDocumentItem_Vector) renames
     Part_21.Read_TextDocumentItem_Vector;

   procedure Read_InlineValueRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.InlineValueRegistrationOptions) renames
     Part_7.Read_InlineValueRegistrationOptions;

   procedure Read_VersionedNotebookDocumentIdentifier
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.VersionedNotebookDocumentIdentifier) renames
     Part_28.Read_VersionedNotebookDocumentIdentifier;

   procedure Read_BaseSymbolInformation
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.BaseSymbolInformation) renames
     Part_27.Read_BaseSymbolInformation;

   procedure Read_InlayHintParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.InlayHintParams) renames
     Part_2.Read_InlayHintParams;

   procedure Read_WorkDoneProgressCreateParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.WorkDoneProgressCreateParams) renames
     Part_2.Read_WorkDoneProgressCreateParams;

   procedure Read_TextEdit
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TextEdit) renames
     Part_21.Read_TextEdit;

   procedure Read_DeclarationClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DeclarationClientCapabilities) renames
     Part_11.Read_DeclarationClientCapabilities;

   procedure Read_SignatureHelpOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SignatureHelpOptions) renames
     Part_1.Read_SignatureHelpOptions;

   procedure Read_SemanticTokensEdit
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SemanticTokensEdit) renames
     Part_28.Read_SemanticTokensEdit;

   procedure Read_DocumentHighlightParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentHighlightParams) renames
     Part_7.Read_DocumentHighlightParams;

   procedure Read_FileCreate
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.FileCreate) renames
     Part_8.Read_FileCreate;

   procedure Read_WorkspaceFullDocumentDiagnosticReport
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value : out LSP.Structures.WorkspaceFullDocumentDiagnosticReport) renames
     Part_6.Read_WorkspaceFullDocumentDiagnosticReport;

   procedure Read_SemanticTokens_Or_Null
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SemanticTokens_Or_Null) renames
     Part_18.Read_SemanticTokens_Or_Null;

   procedure Read_SemanticTokensRangeParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SemanticTokensRangeParams) renames
     Part_21.Read_SemanticTokensRangeParams;

   procedure Read_FoldingRange
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.FoldingRange) renames
     Part_28.Read_FoldingRange;

   procedure Read_SemanticTokensDeltaPartialResult
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SemanticTokensDeltaPartialResult) renames
     Part_6.Read_SemanticTokensDeltaPartialResult;

   procedure Read_NotebookDocumentClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.NotebookDocumentClientCapabilities) renames
     Part_29.Read_NotebookDocumentClientCapabilities;

   procedure Read_CompletionParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CompletionParams) renames
     Part_30.Read_CompletionParams;

   procedure Read_CallHierarchyIncomingCallsParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CallHierarchyIncomingCallsParams) renames
     Part_28.Read_CallHierarchyIncomingCallsParams;

   procedure Read_Location_Vector_Or_Null
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Location_Vector_Or_Null) renames
     Part_25.Read_Location_Vector_Or_Null;

   procedure Read_WorkspaceSymbolClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.WorkspaceSymbolClientCapabilities) renames
     Part_26.Read_WorkspaceSymbolClientCapabilities;

   procedure Read_Boolean_Or_Something
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Boolean_Or_Something) renames
     Part_5.Read_Boolean_Or_Something;

   procedure Read_TypeHierarchyOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TypeHierarchyOptions) renames
     Part_27.Read_TypeHierarchyOptions;

   procedure Read_SelectionRangeRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SelectionRangeRegistrationOptions) renames
     Part_28.Read_SelectionRangeRegistrationOptions;

   procedure Read_DidChangeNotebookDocumentParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DidChangeNotebookDocumentParams) renames
     Part_1.Read_DidChangeNotebookDocumentParams;

   procedure Read_RegularExpressionsClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value : out LSP.Structures.RegularExpressionsClientCapabilities) renames
     Part_2.Read_RegularExpressionsClientCapabilities;

   procedure Read_WorkspaceDocumentDiagnosticReport
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.WorkspaceDocumentDiagnosticReport) renames
     Part_7.Read_WorkspaceDocumentDiagnosticReport;

   procedure Read_WorkspaceClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.WorkspaceClientCapabilities) renames
     Part_16.Read_WorkspaceClientCapabilities;

   procedure Read_WorkspaceDiagnosticParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.WorkspaceDiagnosticParams) renames
     Part_28.Read_WorkspaceDiagnosticParams;

   procedure Read_TextDocumentSyncKind
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.TextDocumentSyncKind) renames
     Part_9.Read_TextDocumentSyncKind;

   procedure Read_InlayHintOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.InlayHintOptions) renames
     Part_30.Read_InlayHintOptions;

   procedure Read_InlineValueOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.InlineValueOptions) renames
     Part_8.Read_InlineValueOptions;

   procedure Read_WorkspaceFoldersChangeEvent
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.WorkspaceFoldersChangeEvent) renames
     Part_32.Read_WorkspaceFoldersChangeEvent;

   procedure Read_TypeDefinitionClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TypeDefinitionClientCapabilities) renames
     Part_24.Read_TypeDefinitionClientCapabilities;

   procedure Read_CreateFileOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CreateFileOptions) renames
     Part_24.Read_CreateFileOptions;

   procedure Read_AlsReferenceKind
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.AlsReferenceKind) renames
     Part_25.Read_AlsReferenceKind;

   procedure Read_DiagnosticRelatedInformation
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DiagnosticRelatedInformation) renames
     Part_2.Read_DiagnosticRelatedInformation;

   procedure Read_InsertTextMode
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.InsertTextMode) renames
     Part_13.Read_InsertTextMode;

   procedure Read_InlineValue_Vector_Or_Null
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.InlineValue_Vector_Or_Null) renames
     Part_24.Read_InlineValue_Vector_Or_Null;

   procedure Read_DidCloseNotebookDocumentParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DidCloseNotebookDocumentParams) renames
     Part_17.Read_DidCloseNotebookDocumentParams;

   procedure Read_ImplementationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ImplementationOptions) renames
     Part_8.Read_ImplementationOptions;

   procedure Read_InlayHint_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.InlayHint_Vector) renames
     Part_28.Read_InlayHint_Vector;

   procedure Read_DocumentColorRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentColorRegistrationOptions) renames
     Part_7.Read_DocumentColorRegistrationOptions;

   procedure Read_CodeActionOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CodeActionOptions) renames
     Part_23.Read_CodeActionOptions;

   procedure Read_CompletionItemKind
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.CompletionItemKind) renames
     Part_13.Read_CompletionItemKind;

   procedure Read_ShowDocumentParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ShowDocumentParams) renames
     Part_6.Read_ShowDocumentParams;

   procedure Read_Command_Or_CodeAction
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Command_Or_CodeAction) renames
     Part_13.Read_Command_Or_CodeAction;

   procedure Read_CreateFilesParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CreateFilesParams) renames
     Part_25.Read_CreateFilesParams;

   procedure Read_ColorPresentation_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ColorPresentation_Vector) renames
     Part_25.Read_ColorPresentation_Vector;

   procedure Read_ChangeAnnotationIdentifier
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ChangeAnnotationIdentifier) renames
     Part_8.Read_ChangeAnnotationIdentifier;

   procedure Read_DocumentFormattingParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentFormattingParams) renames
     Part_2.Read_DocumentFormattingParams;

   procedure Read_SignatureHelpContext
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SignatureHelpContext) renames
     Part_32.Read_SignatureHelpContext;

   procedure Read_DeleteFilesParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DeleteFilesParams) renames
     Part_6.Read_DeleteFilesParams;

   procedure Read_TypeHierarchyItem_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TypeHierarchyItem_Vector) renames
     Part_10.Read_TypeHierarchyItem_Vector;

   procedure Read_DocumentOnTypeFormattingParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentOnTypeFormattingParams) renames
     Part_25.Read_DocumentOnTypeFormattingParams;

   procedure Read_CallHierarchyOutgoingCall
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CallHierarchyOutgoingCall) renames
     Part_5.Read_CallHierarchyOutgoingCall;

   procedure Read_Location_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Location_Vector) renames
     Part_15.Read_Location_Vector;

   procedure Read_TextDocumentContentChangeEvent
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TextDocumentContentChangeEvent) renames
     Part_21.Read_TextDocumentContentChangeEvent;

   procedure Read_DocumentColorParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentColorParams) renames
     Part_28.Read_DocumentColorParams;

   procedure Read_WorkspaceFolder
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.WorkspaceFolder) renames
     Part_23.Read_WorkspaceFolder;

   procedure Read_MarkedString
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.MarkedString) renames
     Part_5.Read_MarkedString;

   procedure Read_NotebookCell
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.NotebookCell) renames
     Part_11.Read_NotebookCell;

   procedure Read_DocumentLink
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentLink) renames
     Part_27.Read_DocumentLink;

   procedure Read_WorkspaceFoldersServerCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.WorkspaceFoldersServerCapabilities) renames
     Part_23.Read_WorkspaceFoldersServerCapabilities;

   procedure Read_WorkDoneProgressEnd
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.WorkDoneProgressEnd) renames
     Part_7.Read_WorkDoneProgressEnd;

   procedure Read_SemanticTokensRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SemanticTokensRegistrationOptions) renames
     Part_23.Read_SemanticTokensRegistrationOptions;

   procedure Read_CompletionTriggerKind
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.CompletionTriggerKind) renames
     Part_2.Read_CompletionTriggerKind;

   procedure Read_CallHierarchyRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CallHierarchyRegistrationOptions) renames
     Part_18.Read_CallHierarchyRegistrationOptions;

   procedure Read_CallHierarchyClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CallHierarchyClientCapabilities) renames
     Part_16.Read_CallHierarchyClientCapabilities;

   procedure Read_DocumentSymbolParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentSymbolParams) renames
     Part_1.Read_DocumentSymbolParams;

   procedure Read_DocumentRangeFormattingOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentRangeFormattingOptions) renames
     Part_3.Read_DocumentRangeFormattingOptions;

   procedure Read_InlayHintWorkspaceClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value : out LSP.Structures.InlayHintWorkspaceClientCapabilities) renames
     Part_8.Read_InlayHintWorkspaceClientCapabilities;

   procedure Read_InlineValue_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.InlineValue_Vector) renames
     Part_2.Read_InlineValue_Vector;

   procedure Read_DiagnosticServerCancellationData
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DiagnosticServerCancellationData) renames
     Part_17.Read_DiagnosticServerCancellationData;

   procedure Read_DocumentColorOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentColorOptions) renames
     Part_8.Read_DocumentColorOptions;

   procedure Read_InitializedParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.InitializedParams) renames
     Part_6.Read_InitializedParams;

   procedure Read_FileOperationPatternKind
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.FileOperationPatternKind) renames
     Part_7.Read_FileOperationPatternKind;

   procedure Read_Unregistration
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Unregistration) renames
     Part_16.Read_Unregistration;

   procedure Read_HoverOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.HoverOptions) renames
     Part_4.Read_HoverOptions;

   procedure Read_DocumentOnTypeFormattingRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures
        .DocumentOnTypeFormattingRegistrationOptions) renames
     Part_22.Read_DocumentOnTypeFormattingRegistrationOptions;

   procedure Read_relatedDocuments_OfDocumentDiagnosticReportPartialResult
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures
        .relatedDocuments_OfDocumentDiagnosticReportPartialResult) renames
     Part_13.Read_relatedDocuments_OfDocumentDiagnosticReportPartialResult;

   procedure Read_ReferenceClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ReferenceClientCapabilities) renames
     Part_18.Read_ReferenceClientCapabilities;

   procedure Read_MonikerClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.MonikerClientCapabilities) renames
     Part_20.Read_MonikerClientCapabilities;

   procedure Read_CompletionItemTag_Set
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CompletionItemTag_Set) renames
     Part_28.Read_CompletionItemTag_Set;

   procedure Read_ColorInformation
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ColorInformation) renames
     Part_5.Read_ColorInformation;

   procedure Read_A_Range
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.A_Range) renames
     Part_21.Read_A_Range;

   procedure Read_ShowMessageRequestParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ShowMessageRequestParams) renames
     Part_17.Read_ShowMessageRequestParams;

   procedure Read_DocumentColorClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentColorClientCapabilities) renames
     Part_9.Read_DocumentColorClientCapabilities;

   procedure Read_WorkspaceEdit_Or_Null
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.WorkspaceEdit_Or_Null) renames
     Part_27.Read_WorkspaceEdit_Or_Null;

   procedure Read_InlineValueEvaluatableExpression
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.InlineValueEvaluatableExpression) renames
     Part_2.Read_InlineValueEvaluatableExpression;

   procedure Read_Pattern
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Pattern) renames
     Part_14.Read_Pattern;

   procedure Read_DiagnosticTag_Set
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DiagnosticTag_Set) renames
     Part_14.Read_DiagnosticTag_Set;

   procedure Read_NotebookCellArrayChange
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.NotebookCellArrayChange) renames
     Part_6.Read_NotebookCellArrayChange;

   procedure Read_DocumentLinkRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentLinkRegistrationOptions) renames
     Part_28.Read_DocumentLinkRegistrationOptions;

   procedure Read_DocumentHighlight_Vector_Or_Null
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentHighlight_Vector_Or_Null) renames
     Part_27.Read_DocumentHighlight_Vector_Or_Null;

   procedure Read_LogMessageParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.LogMessageParams) renames
     Part_23.Read_LogMessageParams;

   procedure Read_CodeLens
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CodeLens) renames
     Part_29.Read_CodeLens;

   procedure Read_DocumentDiagnosticReportPartialResult
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value : out LSP.Structures.DocumentDiagnosticReportPartialResult) renames
     Part_10.Read_DocumentDiagnosticReportPartialResult;

   procedure Read_CallHierarchyIncomingCall_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CallHierarchyIncomingCall_Vector) renames
     Part_19.Read_CallHierarchyIncomingCall_Vector;

   procedure Read_ProgressParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ProgressParams) renames
     Part_5.Read_ProgressParams;

   procedure Read_DocumentLinkOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentLinkOptions) renames
     Part_27.Read_DocumentLinkOptions;

   procedure Read_PrepareSupportDefaultBehavior
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.PrepareSupportDefaultBehavior) renames
     Part_29.Read_PrepareSupportDefaultBehavior;

   procedure Read_TextDocumentIdentifier
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TextDocumentIdentifier) renames
     Part_21.Read_TextDocumentIdentifier;

   procedure Read_DocumentSymbol_Result
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentSymbol_Result) renames
     Part_11.Read_DocumentSymbol_Result;

   procedure Read_TraceValues
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.TraceValues) renames
     Part_29.Read_TraceValues;

   procedure Read_TypeHierarchyPrepareParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TypeHierarchyPrepareParams) renames
     Part_8.Read_TypeHierarchyPrepareParams;

   procedure Read_SymbolInformation
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SymbolInformation) renames
     Part_32.Read_SymbolInformation;

   procedure Read_WillSaveTextDocumentParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.WillSaveTextDocumentParams) renames
     Part_21.Read_WillSaveTextDocumentParams;

   procedure Read_Command_Or_CodeAction_Vector_Or_Null
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value : out LSP.Structures.Command_Or_CodeAction_Vector_Or_Null) renames
     Part_29.Read_Command_Or_CodeAction_Vector_Or_Null;

   procedure Read_FileOperationClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.FileOperationClientCapabilities) renames
     Part_26.Read_FileOperationClientCapabilities;

   procedure Read_Definition_Result
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Definition_Result) renames
     Part_23.Read_Definition_Result;

   procedure Read_DocumentHighlightClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentHighlightClientCapabilities) renames
     Part_22.Read_DocumentHighlightClientCapabilities;

   procedure Read_InlineValueText
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.InlineValueText) renames
     Part_27.Read_InlineValueText;

   procedure Read_UnchangedDocumentDiagnosticReport
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.UnchangedDocumentDiagnosticReport) renames
     Part_27.Read_UnchangedDocumentDiagnosticReport;

   procedure Read_SymbolTag
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.SymbolTag) renames
     Part_2.Read_SymbolTag;

   procedure Read_AlsCheckSyntaxParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.AlsCheckSyntaxParams) renames
     Part_25.Read_AlsCheckSyntaxParams;

   procedure Read_Registration
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Registration) renames
     Part_21.Read_Registration;

   procedure Read_CompletionItem
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CompletionItem) renames
     Part_21.Read_CompletionItem;

   procedure Read_DocumentRangeFormattingRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures
        .DocumentRangeFormattingRegistrationOptions) renames
     Part_4.Read_DocumentRangeFormattingRegistrationOptions;

   procedure Read_Moniker_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Moniker_Vector) renames
     Part_11.Read_Moniker_Vector;

   procedure Read_RenameFileOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.RenameFileOptions) renames
     Part_20.Read_RenameFileOptions;

   procedure Read_HoverParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.HoverParams) renames
     Part_32.Read_HoverParams;

   procedure Read_DocumentSymbol
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentSymbol) renames
     Part_13.Read_DocumentSymbol;

   procedure Read_DocumentSymbolClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentSymbolClientCapabilities) renames
     Part_20.Read_DocumentSymbolClientCapabilities;

   procedure Read_LinkedEditingRangeRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value : out LSP.Structures.LinkedEditingRangeRegistrationOptions) renames
     Part_7.Read_LinkedEditingRangeRegistrationOptions;

   procedure Read_InitializeError
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.InitializeError) renames
     Part_6.Read_InitializeError;

   procedure Read_SemanticTokensClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SemanticTokensClientCapabilities) renames
     Part_25.Read_SemanticTokensClientCapabilities;

   procedure Read_Hover
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Hover) renames
     Part_20.Read_Hover;

   procedure Read_InlineValueWorkspaceClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures
        .InlineValueWorkspaceClientCapabilities) renames
     Part_30.Read_InlineValueWorkspaceClientCapabilities;

   procedure Read_AlsSearchKind
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.AlsSearchKind) renames
     Part_14.Read_AlsSearchKind;

   procedure Read_ResourceOperation
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ResourceOperation) renames
     Part_27.Read_ResourceOperation;

   procedure Read_PublishDiagnosticsClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value : out LSP.Structures.PublishDiagnosticsClientCapabilities) renames
     Part_24.Read_PublishDiagnosticsClientCapabilities;

   procedure Read_DocumentHighlight_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentHighlight_Vector) renames
     Part_13.Read_DocumentHighlight_Vector;

   procedure Read_SemanticTokensOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SemanticTokensOptions) renames
     Part_24.Read_SemanticTokensOptions;

   procedure Read_DocumentHighlightOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentHighlightOptions) renames
     Part_11.Read_DocumentHighlightOptions;

   procedure Read_Integer_Or_Virtual_String
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Integer_Or_Virtual_String) renames
     Part_22.Read_Integer_Or_Virtual_String;

   procedure Read_FoldingRangeParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.FoldingRangeParams) renames
     Part_24.Read_FoldingRangeParams;

   procedure Read_FoldingRangeOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.FoldingRangeOptions) renames
     Part_12.Read_FoldingRangeOptions;

   procedure Read_Integer_Or_Null
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Integer_Or_Null) renames
     Part_6.Read_Integer_Or_Null;

   procedure Read_RegistrationParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.RegistrationParams) renames
     Part_25.Read_RegistrationParams;

   procedure Read_SignatureHelpParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SignatureHelpParams) renames
     Part_13.Read_SignatureHelpParams;

   procedure Read_TextEdit_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TextEdit_Vector) renames
     Part_19.Read_TextEdit_Vector;

   procedure Read_ShowMessageRequestClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value : out LSP.Structures.ShowMessageRequestClientCapabilities) renames
     Part_4.Read_ShowMessageRequestClientCapabilities;

   procedure Read_SemanticTokensEdit_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SemanticTokensEdit_Vector) renames
     Part_14.Read_SemanticTokensEdit_Vector;

   procedure Read_Position
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Position) renames
     Part_13.Read_Position;

   procedure Read_TextDocumentSaveRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TextDocumentSaveRegistrationOptions) renames
     Part_24.Read_TextDocumentSaveRegistrationOptions;

   procedure Read_CodeActionParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CodeActionParams) renames
     Part_27.Read_CodeActionParams;

   procedure Read_Command
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Command) renames
     Part_3.Read_Command;

   procedure Read_TextDocumentFilter
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TextDocumentFilter) renames
     Part_8.Read_TextDocumentFilter;

   procedure Read_FoldingRange_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.FoldingRange_Vector) renames
     Part_14.Read_FoldingRange_Vector;

   procedure Read_SelectionRange_Vector_Or_Null
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SelectionRange_Vector_Or_Null) renames
     Part_11.Read_SelectionRange_Vector_Or_Null;

   procedure Read_ExecuteCommandRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ExecuteCommandRegistrationOptions) renames
     Part_13.Read_ExecuteCommandRegistrationOptions;

   procedure Read_UniquenessLevel
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.UniquenessLevel) renames
     Part_28.Read_UniquenessLevel;

   procedure Read_TextDocumentEdit
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TextDocumentEdit) renames
     Part_12.Read_TextDocumentEdit;

   procedure Read_CompletionItemLabelDetails
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CompletionItemLabelDetails) renames
     Part_17.Read_CompletionItemLabelDetails;

   procedure Read_CompletionOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CompletionOptions) renames
     Part_2.Read_CompletionOptions;

   procedure Read_Diagnostic
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Diagnostic) renames
     Part_3.Read_Diagnostic;

   procedure Read_LinkedEditingRangeParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.LinkedEditingRangeParams) renames
     Part_28.Read_LinkedEditingRangeParams;

   procedure Read_TextDocumentSyncOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TextDocumentSyncOptions) renames
     Part_17.Read_TextDocumentSyncOptions;

   procedure Read_ImplementationParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ImplementationParams) renames
     Part_28.Read_ImplementationParams;

   procedure Read_OptionalVersionedTextDocumentIdentifier
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures
        .OptionalVersionedTextDocumentIdentifier) renames
     Part_18.Read_OptionalVersionedTextDocumentIdentifier;

   procedure Read_SemanticTokensLegend
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SemanticTokensLegend) renames
     Part_7.Read_SemanticTokensLegend;

   procedure Read_WorkspaceDocumentDiagnosticReport_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures
        .WorkspaceDocumentDiagnosticReport_Vector) renames
     Part_1.Read_WorkspaceDocumentDiagnosticReport_Vector;

   procedure Read_ExecuteCommandOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ExecuteCommandOptions) renames
     Part_14.Read_ExecuteCommandOptions;

   procedure Read_TextDocumentSyncClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TextDocumentSyncClientCapabilities) renames
     Part_16.Read_TextDocumentSyncClientCapabilities;

   procedure Read_MarkupKind
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.MarkupKind) renames
     Part_14.Read_MarkupKind;

   procedure Read_AlsReferenceKind_Set
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.AlsReferenceKind_Set) renames
     Part_22.Read_AlsReferenceKind_Set;

   procedure Read_CallHierarchyPrepareParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CallHierarchyPrepareParams) renames
     Part_14.Read_CallHierarchyPrepareParams;

   procedure Read_LSPErrorCodes
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.LSPErrorCodes) renames
     Part_13.Read_LSPErrorCodes;

   procedure Read_ReferenceRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ReferenceRegistrationOptions) renames
     Part_16.Read_ReferenceRegistrationOptions;

   procedure Read_LinkedEditingRangeOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.LinkedEditingRangeOptions) renames
     Part_8.Read_LinkedEditingRangeOptions;

end LSP.Inputs;
