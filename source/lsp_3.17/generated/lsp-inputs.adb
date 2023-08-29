--
--  Copyright (C) <YEAR>, <COPYRIGHT HOLDER>
--
--  SPDX-License-Identifier: MIT
--
--  DON'T EDIT THIS FILE! It was generated from metaModel.json.
--

pragma Ada_2022;
with Interfaces;
with LSP.Input_Tools;
with VSS.Strings;
with VSS.JSON.Pull_Readers.Buffered;
with Minimal_Perfect_Hash;

package body LSP.Inputs is

   pragma Warnings (Off, "is not referenced");
   use type Interfaces.Integer_64;

   procedure Read_ClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ClientCapabilities);

   procedure Read_FileChangeType
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.FileChangeType);

   procedure Read_Virtual_String
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Virtual_String);

   procedure Read_Declaration
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Declaration);

   procedure Read_FileOperationRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.FileOperationRegistrationOptions);

   procedure Read_WorkspaceFolder_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.WorkspaceFolder_Vector);

   procedure Read_TextDocumentContentChangeEvent_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TextDocumentContentChangeEvent_Vector);

   procedure Read_FoldingRangeClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.FoldingRangeClientCapabilities);

   procedure Read_CreateFile
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CreateFile);

   procedure Read_CompletionContext
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CompletionContext);

   procedure Read_NotebookCell_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.NotebookCell_Vector);

   procedure Read_CodeActionKind
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.CodeActionKind);

   procedure Read_RelatedUnchangedDocumentDiagnosticReport
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.RelatedUnchangedDocumentDiagnosticReport);

   procedure Read_DidChangeWatchedFilesClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DidChangeWatchedFilesClientCapabilities);

   procedure Read_SemanticTokenTypes
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.SemanticTokenTypes);

   procedure Read_AnnotatedTextEdit
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.AnnotatedTextEdit);

   procedure Read_DeclarationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DeclarationOptions);

   procedure Read_SignatureInformation
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SignatureInformation);

   procedure Read_TextDocumentSaveReason
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.TextDocumentSaveReason);

   procedure Read_SemanticTokens
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SemanticTokens);

   procedure Read_TokenFormat
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.TokenFormat);

   procedure Read_TextDocumentRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TextDocumentRegistrationOptions);

   procedure Read_URI
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.URI);

   procedure Read_SemanticTokenModifiers
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.SemanticTokenModifiers);

   procedure Read_ConfigurationItem
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ConfigurationItem);

   procedure Read_DocumentOnTypeFormattingClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentOnTypeFormattingClientCapabilities);

   procedure Read_SymbolKind
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.SymbolKind);

   procedure Read_SelectionRangeClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SelectionRangeClientCapabilities);

   procedure Read_Range_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Range_Vector);

   procedure Read_Definition
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Definition);

   procedure Read_NotebookDocumentFilter
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.NotebookDocumentFilter);

   procedure Read_Color
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Color);

   procedure Read_FileOperationPattern
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.FileOperationPattern);

   procedure Read_LinkedEditingRanges
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.LinkedEditingRanges);

   procedure Read_Virtual_String_Or_MarkupContent
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Virtual_String_Or_MarkupContent);

   procedure Read_ServerCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ServerCapabilities);

   procedure Read_DidChangeWatchedFilesRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DidChangeWatchedFilesRegistrationOptions);

   procedure Read_CallHierarchyOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CallHierarchyOptions);

   procedure Read_ImplementationRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ImplementationRegistrationOptions);

   procedure Read_FoldingRangeRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.FoldingRangeRegistrationOptions);

   procedure Read_InlineValueContext
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.InlineValueContext);

   procedure Read_TextDocumentIdentifier_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TextDocumentIdentifier_Vector);

   procedure Read_DefinitionLink
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DefinitionLink);

   procedure Read_DocumentFormattingOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentFormattingOptions);

   procedure Read_SymbolInformation_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SymbolInformation_Vector);

   procedure Read_RenameFile
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.RenameFile);

   procedure Read_DidChangeConfigurationRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DidChangeConfigurationRegistrationOptions);

   procedure Read_CallHierarchyItem
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CallHierarchyItem);

   procedure Read_SymbolTag_Set
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SymbolTag_Set);

   procedure Read_RelativePattern
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.RelativePattern);

   procedure Read_tagSupport_OfWorkspaceSymbolClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures
        .tagSupport_OfWorkspaceSymbolClientCapabilities);

   procedure Read_ReferenceOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ReferenceOptions);

   procedure Read_TextDocumentClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TextDocumentClientCapabilities);

   procedure Read_DocumentSymbol_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentSymbol_Vector);

   procedure Read_GlobPattern
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.GlobPattern);

   procedure Read_NotebookDocumentIdentifier
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.NotebookDocumentIdentifier);

   procedure Read_InsertReplaceEdit
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.InsertReplaceEdit);

   procedure Read_InlineValueVariableLookup
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.InlineValueVariableLookup);

   procedure Read_FileEvent
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.FileEvent);

   procedure Read_SignatureHelpClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SignatureHelpClientCapabilities);

   procedure Read_NotebookDocumentSyncClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.NotebookDocumentSyncClientCapabilities);

   procedure Read_DiagnosticOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DiagnosticOptions);

   procedure Read_WindowClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.WindowClientCapabilities);

   procedure Read_GeneralClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.GeneralClientCapabilities);

   procedure Read_SemanticTokensWorkspaceClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SemanticTokensWorkspaceClientCapabilities);

   procedure Read_LinkedEditingRangeClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.LinkedEditingRangeClientCapabilities);

   procedure Read_SignatureHelp
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SignatureHelp);

   procedure Read_Diagnostic_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Diagnostic_Vector);

   procedure Read_DocumentHighlightKind
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.DocumentHighlightKind);

   procedure Read_ResourceOperationKind
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.ResourceOperationKind);

   procedure Read_InlineValueClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.InlineValueClientCapabilities);

   procedure Read_PrepareRenameResult
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.PrepareRenameResult);

   procedure Read_DocumentLinkClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentLinkClientCapabilities);

   procedure Read_MarkupKind_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.MarkupKind_Vector);

   procedure Read_CodeActionContext
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CodeActionContext);

   procedure Read_InlayHintRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.InlayHintRegistrationOptions);

   procedure Read_Natural_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Natural_Vector);

   procedure Read_CodeDescription
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CodeDescription);

   procedure Read_Virtual_String_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Virtual_String_Vector);

   procedure Read_LSPObject
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.LSPObject);

   procedure Read_TextDocumentPositionParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TextDocumentPositionParams);

   procedure Read_CodeLensRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CodeLensRegistrationOptions);

   procedure Read_SelectionRange
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SelectionRange);

   procedure Read_NotebookCellTextDocumentFilter
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.NotebookCellTextDocumentFilter);

   procedure Read_WorkspaceFoldersInitializeParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.WorkspaceFoldersInitializeParams);

   procedure Read_FileSystemWatcher
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.FileSystemWatcher);

   procedure Read_CodeActionKind_Set
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CodeActionKind_Set);

   procedure Read_NotebookDocumentSyncOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.NotebookDocumentSyncOptions);

   procedure Read_CodeActionRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CodeActionRegistrationOptions);

   procedure Read_NotebookDocumentSyncRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.NotebookDocumentSyncRegistrationOptions);

   procedure Read_WorkspaceUnchangedDocumentDiagnosticReport
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.WorkspaceUnchangedDocumentDiagnosticReport);

   procedure Read_HoverClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.HoverClientCapabilities);

   procedure Read_RenameClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.RenameClientCapabilities);

   procedure Read_DefinitionOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DefinitionOptions);

   procedure Read_InlayHintKind
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.InlayHintKind);

   procedure Read_symbolKind_OfWorkspaceSymbolClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures
        .symbolKind_OfWorkspaceSymbolClientCapabilities);

   procedure Read_FailureHandlingKind
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.FailureHandlingKind);

   procedure Read_FileOperationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.FileOperationOptions);

   procedure Read_DocumentSelector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentSelector);

   procedure Read_CompletionClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CompletionClientCapabilities);

   procedure Read_CodeLensWorkspaceClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CodeLensWorkspaceClientCapabilities);

   procedure Read_RenameOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.RenameOptions);

   procedure Read_SymbolKind_Set
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SymbolKind_Set);

   procedure Read_DidChangeConfigurationClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DidChangeConfigurationClientCapabilities);

   procedure Read_DocumentHighlightRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentHighlightRegistrationOptions);

   procedure Read_DeclarationLink
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DeclarationLink);

   procedure Read_T
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.T);

   procedure Read_FullDocumentDiagnosticReport
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.FullDocumentDiagnosticReport);

   procedure Read_DocumentSymbolRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentSymbolRegistrationOptions);

   procedure Read_DeclarationRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DeclarationRegistrationOptions);

   procedure Read_DeleteFileOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DeleteFileOptions);

   procedure Read_WatchKind
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.WatchKind);

   procedure Read_TypeHierarchyRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TypeHierarchyRegistrationOptions);

   procedure Read_DiagnosticSeverity
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.DiagnosticSeverity);

   procedure Read_CodeActionTriggerKind
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.CodeActionTriggerKind);

   procedure Read_CodeActionClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CodeActionClientCapabilities);

   procedure Read_MarkdownClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.MarkdownClientCapabilities);

   procedure Read_DefinitionLink_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DefinitionLink_Vector);

   procedure Read_ImplementationClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ImplementationClientCapabilities);

   procedure Read_TextDocumentItem
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TextDocumentItem);

   procedure Read_InsertTextFormat
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.InsertTextFormat);

   procedure Read_relatedDocuments_OfDocumentDiagnosticReportPartialResult_Item
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures
        .relatedDocuments_OfDocumentDiagnosticReportPartialResult_Item);

   procedure Read_ExecuteCommandClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ExecuteCommandClientCapabilities);

   procedure Read_CallHierarchyItem_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CallHierarchyItem_Vector);

   procedure Read_DeleteFile
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DeleteFile);

   procedure Read_SaveOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SaveOptions);

   procedure Read_NotebookDocument
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.NotebookDocument);

   procedure Read_RelatedFullDocumentDiagnosticReport
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.RelatedFullDocumentDiagnosticReport);

   procedure Read_CodeLensClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CodeLensClientCapabilities);

   procedure Read_DocumentSymbolOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentSymbolOptions);

   procedure Read_CompletionRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CompletionRegistrationOptions);

   procedure Read_InlayHintClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.InlayHintClientCapabilities);

   procedure Read_ReferenceContext
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ReferenceContext);

   procedure Read_WorkspaceEditClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.WorkspaceEditClientCapabilities);

   procedure Read_WorkspaceSymbolOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.WorkspaceSymbolOptions);

   procedure Read_ChangeAnnotation
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ChangeAnnotation);

   procedure Read_FileOperationPatternOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.FileOperationPatternOptions);

   procedure Read_VersionedTextDocumentIdentifier
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.VersionedTextDocumentIdentifier);

   procedure Read_SemanticTokensDelta
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SemanticTokensDelta);

   procedure Read_ShowDocumentClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ShowDocumentClientCapabilities);

   procedure Read_InlayHintLabelPart
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.InlayHintLabelPart);

   procedure Read_SelectionRangeOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SelectionRangeOptions);

   procedure Read_LocationLink
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.LocationLink);

   procedure Read_CompletionList
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CompletionList);

   procedure Read_DiagnosticRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DiagnosticRegistrationOptions);

   procedure Read_SignatureHelpTriggerKind
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.SignatureHelpTriggerKind);

   procedure Read_TypeHierarchyClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TypeHierarchyClientCapabilities);

   procedure Read_DocumentOnTypeFormattingOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentOnTypeFormattingOptions);

   procedure Read_WorkspaceSymbolRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.WorkspaceSymbolRegistrationOptions);

   procedure Read_FileDelete
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.FileDelete);

   procedure Read_DocumentFormattingRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentFormattingRegistrationOptions);

   procedure Read_ColorPresentation
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ColorPresentation);

   procedure Read_TypeHierarchyItem
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TypeHierarchyItem);

   procedure Read_Boolean_Or_Any
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Boolean_Or_Any);

   procedure Read_Location
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Location);

   procedure Read_RenameRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.RenameRegistrationOptions);

   procedure Read_ParameterInformation
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ParameterInformation);

   procedure Read_PositionEncodingKind
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.PositionEncodingKind);

   procedure Read_MessageType
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.MessageType);

   procedure Read_An_InitializeParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.An_InitializeParams);

   procedure Read_FileOperationFilter
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.FileOperationFilter);

   procedure Read_MonikerOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.MonikerOptions);

   procedure Read_InlineValue
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.InlineValue);

   procedure Read_Virtual_String_Or_NotebookDocumentFilter
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Virtual_String_Or_NotebookDocumentFilter);

   procedure Read_MonikerKind
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.MonikerKind);

   procedure Read_NotebookCellKind
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.NotebookCellKind);

   procedure Read_HoverRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.HoverRegistrationOptions);

   procedure Read_CompletionItemTag
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.CompletionItemTag);

   procedure Read_DefinitionRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DefinitionRegistrationOptions);

   procedure Read_clientInfo_Of_InitializeParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.clientInfo_Of_InitializeParams);

   procedure Read_DiagnosticTag
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.DiagnosticTag);

   procedure Read_DiagnosticClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DiagnosticClientCapabilities);

   procedure Read_MonikerRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.MonikerRegistrationOptions);

   procedure Read_ExecutionSummary
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ExecutionSummary);

   procedure Read_PreviousResultId
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.PreviousResultId);

   procedure Read_FoldingRangeKind
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.FoldingRangeKind);

   procedure Read_DocumentFilter
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentFilter);

   procedure Read_CallHierarchyIncomingCall
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CallHierarchyIncomingCall);

   procedure Read_TextDocumentChangeRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TextDocumentChangeRegistrationOptions);

   procedure Read_DeclarationLink_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DeclarationLink_Vector);

   procedure Read_DocumentFormattingClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentFormattingClientCapabilities);

   procedure Read_NotebookDocumentChangeEvent
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.NotebookDocumentChangeEvent);

   procedure Read_FormattingOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.FormattingOptions);

   procedure Read_resolveSupport_OfWorkspaceSymbolClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures
        .resolveSupport_OfWorkspaceSymbolClientCapabilities);

   procedure Read_WorkspaceSymbol_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.WorkspaceSymbol_Vector);

   procedure Read_TypeDefinitionRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TypeDefinitionRegistrationOptions);

   procedure Read_CodeLensOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CodeLensOptions);

   procedure Read_FileRename
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.FileRename);

   procedure Read_DocumentRangeFormattingClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentRangeFormattingClientCapabilities);

   procedure Read_Moniker
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Moniker);

   procedure Read_DiagnosticWorkspaceClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DiagnosticWorkspaceClientCapabilities);

   procedure Read_DefinitionClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DefinitionClientCapabilities);

   procedure Read_SignatureHelpRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SignatureHelpRegistrationOptions);

   procedure Read_TypeDefinitionOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TypeDefinitionOptions);

   procedure Read_MarkupContent
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.MarkupContent);

   procedure Read_WorkspaceEdit
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.WorkspaceEdit);

   procedure Read_DocumentHighlight
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentHighlight);

   procedure Read_MessageActionItem
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.MessageActionItem);

   procedure Read_TextDocumentItem_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TextDocumentItem_Vector);

   procedure Read_InlineValueRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.InlineValueRegistrationOptions);

   procedure Read_VersionedNotebookDocumentIdentifier
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.VersionedNotebookDocumentIdentifier);

   procedure Read_BaseSymbolInformation
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.BaseSymbolInformation);

   procedure Read_TextEdit
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TextEdit);

   procedure Read_DeclarationClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DeclarationClientCapabilities);

   procedure Read_SignatureHelpOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SignatureHelpOptions);

   procedure Read_SemanticTokensEdit
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SemanticTokensEdit);

   procedure Read_FileCreate
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.FileCreate);

   procedure Read_WorkspaceFullDocumentDiagnosticReport
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.WorkspaceFullDocumentDiagnosticReport);

   procedure Read_FoldingRange
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.FoldingRange);

   procedure Read_SemanticTokensDeltaPartialResult
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SemanticTokensDeltaPartialResult);

   procedure Read_NotebookDocumentClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.NotebookDocumentClientCapabilities);

   procedure Read_WorkspaceSymbolClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.WorkspaceSymbolClientCapabilities);

   procedure Read_Boolean_Or_Something
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Boolean_Or_Something);

   procedure Read_TypeHierarchyOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TypeHierarchyOptions);

   procedure Read_SelectionRangeRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SelectionRangeRegistrationOptions);

   procedure Read_RegularExpressionsClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.RegularExpressionsClientCapabilities);

   procedure Read_WorkspaceDocumentDiagnosticReport
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.WorkspaceDocumentDiagnosticReport);

   procedure Read_WorkspaceClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.WorkspaceClientCapabilities);

   procedure Read_TextDocumentSyncKind
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.TextDocumentSyncKind);

   procedure Read_InlayHintOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.InlayHintOptions);

   procedure Read_InlineValueOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.InlineValueOptions);

   procedure Read_WorkspaceFoldersChangeEvent
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.WorkspaceFoldersChangeEvent);

   procedure Read_TypeDefinitionClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TypeDefinitionClientCapabilities);

   procedure Read_CreateFileOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CreateFileOptions);

   procedure Read_DiagnosticRelatedInformation
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DiagnosticRelatedInformation);

   procedure Read_InsertTextMode
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.InsertTextMode);

   procedure Read_ImplementationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ImplementationOptions);

   procedure Read_DocumentColorRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentColorRegistrationOptions);

   procedure Read_CodeActionOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CodeActionOptions);

   procedure Read_CompletionItemKind
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.CompletionItemKind);

   procedure Read_Command_Or_CodeAction
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Command_Or_CodeAction);

   procedure Read_ChangeAnnotationIdentifier
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ChangeAnnotationIdentifier);

   procedure Read_SignatureHelpContext
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SignatureHelpContext);

   procedure Read_CallHierarchyOutgoingCall
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CallHierarchyOutgoingCall);

   procedure Read_TextDocumentContentChangeEvent
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TextDocumentContentChangeEvent);

   procedure Read_WorkspaceFolder
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.WorkspaceFolder);

   procedure Read_MarkedString
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.MarkedString);

   procedure Read_NotebookCell
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.NotebookCell);

   procedure Read_WorkspaceFoldersServerCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.WorkspaceFoldersServerCapabilities);

   procedure Read_SemanticTokensRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SemanticTokensRegistrationOptions);

   procedure Read_CompletionTriggerKind
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.CompletionTriggerKind);

   procedure Read_CallHierarchyRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CallHierarchyRegistrationOptions);

   procedure Read_CallHierarchyClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CallHierarchyClientCapabilities);

   procedure Read_DocumentRangeFormattingOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentRangeFormattingOptions);

   procedure Read_InlayHintWorkspaceClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.InlayHintWorkspaceClientCapabilities);

   procedure Read_DiagnosticServerCancellationData
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DiagnosticServerCancellationData);

   procedure Read_DocumentColorOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentColorOptions);

   procedure Read_FileOperationPatternKind
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.FileOperationPatternKind);

   procedure Read_Unregistration
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Unregistration);

   procedure Read_HoverOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.HoverOptions);

   procedure Read_DocumentOnTypeFormattingRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value : out LSP.Structures.DocumentOnTypeFormattingRegistrationOptions);

   procedure Read_relatedDocuments_OfDocumentDiagnosticReportPartialResult
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures
        .relatedDocuments_OfDocumentDiagnosticReportPartialResult);

   procedure Read_ReferenceClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ReferenceClientCapabilities);

   procedure Read_MonikerClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.MonikerClientCapabilities);

   procedure Read_CompletionItemTag_Set
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CompletionItemTag_Set);

   procedure Read_ColorInformation
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ColorInformation);

   procedure Read_A_Range
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.A_Range);

   procedure Read_DocumentColorClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentColorClientCapabilities);

   procedure Read_InlineValueEvaluatableExpression
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.InlineValueEvaluatableExpression);

   procedure Read_Pattern
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Pattern);

   procedure Read_DiagnosticTag_Set
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DiagnosticTag_Set);

   procedure Read_NotebookCellArrayChange
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.NotebookCellArrayChange);

   procedure Read_DocumentLinkRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentLinkRegistrationOptions);

   procedure Read_DocumentLinkOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentLinkOptions);

   procedure Read_PrepareSupportDefaultBehavior
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.PrepareSupportDefaultBehavior);

   procedure Read_TextDocumentIdentifier
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TextDocumentIdentifier);

   procedure Read_TraceValues
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.TraceValues);

   procedure Read_SymbolInformation
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SymbolInformation);

   procedure Read_FileOperationClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.FileOperationClientCapabilities);

   procedure Read_DocumentHighlightClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentHighlightClientCapabilities);

   procedure Read_InlineValueText
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.InlineValueText);

   procedure Read_UnchangedDocumentDiagnosticReport
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.UnchangedDocumentDiagnosticReport);

   procedure Read_SymbolTag
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.SymbolTag);

   procedure Read_Registration
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Registration);

   procedure Read_DocumentRangeFormattingRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentRangeFormattingRegistrationOptions);

   procedure Read_RenameFileOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.RenameFileOptions);

   procedure Read_DocumentSymbol
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentSymbol);

   procedure Read_DocumentSymbolClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentSymbolClientCapabilities);

   procedure Read_InitializeError
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.InitializeError);

   procedure Read_LinkedEditingRangeRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.LinkedEditingRangeRegistrationOptions);

   procedure Read_SemanticTokensClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SemanticTokensClientCapabilities);

   procedure Read_Hover
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Hover);

   procedure Read_InlineValueWorkspaceClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.InlineValueWorkspaceClientCapabilities);

   procedure Read_ResourceOperation
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ResourceOperation);

   procedure Read_PublishDiagnosticsClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.PublishDiagnosticsClientCapabilities);

   procedure Read_SemanticTokensOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SemanticTokensOptions);

   procedure Read_DocumentHighlightOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentHighlightOptions);

   procedure Read_FoldingRangeOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.FoldingRangeOptions);

   procedure Read_Integer_Or_Null
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Integer_Or_Null);

   procedure Read_TextEdit_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TextEdit_Vector);

   procedure Read_ShowMessageRequestClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ShowMessageRequestClientCapabilities);

   procedure Read_SemanticTokensEdit_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SemanticTokensEdit_Vector);

   procedure Read_Position
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Position);

   procedure Read_TextDocumentSaveRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TextDocumentSaveRegistrationOptions);

   procedure Read_Command
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Command);

   procedure Read_TextDocumentFilter
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TextDocumentFilter);

   procedure Read_ExecuteCommandRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ExecuteCommandRegistrationOptions);

   procedure Read_UniquenessLevel
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.UniquenessLevel);

   procedure Read_TextDocumentEdit
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TextDocumentEdit);

   procedure Read_CompletionItemLabelDetails
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CompletionItemLabelDetails);

   procedure Read_CompletionOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CompletionOptions);

   procedure Read_Diagnostic
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Diagnostic);

   procedure Read_TextDocumentSyncOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TextDocumentSyncOptions);

   procedure Read_OptionalVersionedTextDocumentIdentifier
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.OptionalVersionedTextDocumentIdentifier);

   procedure Read_SemanticTokensLegend
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SemanticTokensLegend);

   procedure Read_WorkspaceDocumentDiagnosticReport_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.WorkspaceDocumentDiagnosticReport_Vector);

   procedure Read_ExecuteCommandOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ExecuteCommandOptions);

   procedure Read_TextDocumentSyncClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TextDocumentSyncClientCapabilities);

   procedure Read_MarkupKind
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.MarkupKind);

   procedure Read_LSPErrorCodes
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.LSPErrorCodes);

   procedure Read_ReferenceRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ReferenceRegistrationOptions);

   procedure Read_LinkedEditingRangeOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.LinkedEditingRangeOptions);

   package ClientCapabilities_Scope is
      package ClientCapabilities_Map is new Minimal_Perfect_Hash
        (["workspace",
         "textDocument",
         "notebookDocument",
         "window",
         "general",
         "experimental"]);

   end ClientCapabilities_Scope;

   procedure Read_ClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ClientCapabilities) is
      use ClientCapabilities_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case ClientCapabilities_Map.Get_Index (Key) is
               when 1 =>  --  workspace
                  Value.workspace :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_WorkspaceClientCapabilities
                    (Handler, Value.workspace.Value);
               when 2 =>  --  textDocument
                  Value.textDocument :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_TextDocumentClientCapabilities
                    (Handler, Value.textDocument.Value);
               when 3 =>  --  notebookDocument
                  Value.notebookDocument :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_NotebookDocumentClientCapabilities
                    (Handler, Value.notebookDocument.Value);
               when 4 =>  --  window
                  Value.window :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_WindowClientCapabilities (Handler, Value.window.Value);
               when 5 =>  --  general
                  Value.general :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_GeneralClientCapabilities
                    (Handler, Value.general.Value);
               when 6 =>  --  experimental
                  Read_LSPAny (Handler, Value.experimental);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_ClientCapabilities;

   procedure Read_FileChangeType
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.FileChangeType) is
   begin
      Value :=
        LSP.Enumerations.FileChangeType'Val
          (Handler.Number_Value.Integer_Value - 1);
      Handler.Read_Next;
   end Read_FileChangeType;

   package InitializeParams_Scope is
      package InitializeParams_Map is new Minimal_Perfect_Hash
        (["workDoneToken",
         "processId",
         "clientInfo",
         "locale",
         "rootPath",
         "rootUri",
         "capabilities",
         "initializationOptions",
         "trace",
         "workspaceFolders"]);

      package trace_Of_InitializeParams_Scope is
         package trace_Of_InitializeParams_Map is new Minimal_Perfect_Hash
           (["off",
            "messages",
            "compact",
            "verbose"]);

      end trace_Of_InitializeParams_Scope;

   end InitializeParams_Scope;

   procedure Read_InitializeParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.InitializeParams) is
      use InitializeParams_Scope;
      procedure Read_trace_Of_InitializeParams
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.trace_Of_InitializeParams);

      procedure Read_Virtual_String_Or_Null
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.Virtual_String_Or_Null);

      procedure Read_DocumentUri_Or_Null
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.DocumentUri_Or_Null);

      procedure Read_trace_Of_InitializeParams
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.trace_Of_InitializeParams) is
         use trace_Of_InitializeParams_Scope;
      begin
         Value :=
           LSP.Structures.trace_Of_InitializeParams'Val
             (trace_Of_InitializeParams_Map.Get_Index (Handler.String_Value) -
              1);
         Handler.Read_Next;
      end Read_trace_Of_InitializeParams;

      procedure Read_Virtual_String_Or_Null
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.Virtual_String_Or_Null) is
      begin
         if Handler.Is_Null_Value then
            Handler.Read_Next;
         else
            Value :=
              (Is_Null => False,
               Value   => <>);
            Value.Value.Clear;
            Value.Value.Append (Handler.String_Value);
            Handler.Read_Next;
         end if;
      end Read_Virtual_String_Or_Null;

      procedure Read_DocumentUri_Or_Null
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.DocumentUri_Or_Null) is
      begin
         if Handler.Is_Null_Value then
            Handler.Read_Next;
         else
            Value       :=
              (Is_Null => False,
               Value   => <>);
            Value.Value := (Handler.String_Value with null record);
            Handler.Read_Next;
         end if;
      end Read_DocumentUri_Or_Null;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case InitializeParams_Map.Get_Index (Key) is
               when 1 =>  --  workDoneToken
                  Value.workDoneToken :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_ProgressToken (Handler, Value.workDoneToken.Value);
               when 2 =>  --  processId
                  Read_Integer_Or_Null (Handler, Value.processId);
               when 3 =>  --  clientInfo
                  Value.clientInfo :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_clientInfo_Of_InitializeParams
                    (Handler, Value.clientInfo.Value);
               when 4 =>  --  locale
                  Value.locale.Clear;
                  Value.locale.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 5 =>  --  rootPath
                  Value.rootPath :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_Virtual_String_Or_Null (Handler, Value.rootPath.Value);
               when 6 =>  --  rootUri
                  Read_DocumentUri_Or_Null (Handler, Value.rootUri);
               when 7 =>  --  capabilities
                  Read_ClientCapabilities (Handler, Value.capabilities);
               when 8 =>  --  initializationOptions
                  Read_LSPAny (Handler, Value.initializationOptions);
               when 9 =>  --  trace
                  Value.trace :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_trace_Of_InitializeParams (Handler, Value.trace.Value);
               when 10 =>  --  workspaceFolders
                  Value.Parent.workspaceFolders :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_WorkspaceFolder_Vector_Or_Null
                    (Handler, Value.Parent.workspaceFolders.Value);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_InitializeParams;

   procedure Read_DocumentLink_Vector_Or_Null
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentLink_Vector_Or_Null) is
   begin
      if Handler.Is_Null_Value then
         Handler.Read_Next;
      else
         Value.Clear;
         Read_DocumentLink_Vector (Handler, Value);
      end if;
   end Read_DocumentLink_Vector_Or_Null;

   procedure Read_Command_Or_CodeAction_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Command_Or_CodeAction_Vector) is
   begin
      pragma Assert (Handler.Is_Start_Array);
      Handler.Read_Next;

      declare
         Set   : LSP.Structures.Command_Or_CodeAction_Vector renames Value;
         Value : LSP.Structures.Command_Or_CodeAction;
      begin
         Set.Clear;
         while not Handler.Is_End_Array loop
            Read_Command_Or_CodeAction (Handler, Value);
            Set.Append (Value);
         end loop;
      end;

      Handler.Read_Next;
   end Read_Command_Or_CodeAction_Vector;

   procedure Read_Virtual_String
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Virtual_String) is
   begin
      Value.Clear;
      Value.Append (Handler.String_Value);
      Handler.Read_Next;
   end Read_Virtual_String;

   procedure Read_Declaration
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Declaration) is
   begin
      declare
         Set   : LSP.Structures.Declaration renames Value;
         Value : LSP.Structures.Location;
      begin
         Set.Clear;
         if Handler.Is_Start_Array then
            Handler.Read_Next;
            while not Handler.Is_End_Array loop
               Read_Location (Handler, Value);
               Set.Append (Value);
            end loop;
            Handler.Read_Next;

         else
            Read_Location (Handler, Value);
            Set.Append (Value);
         end if;
      end;

   end Read_Declaration;

   procedure Read_Null_Record
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Null_Record) is
   begin
      null;  --  #null_value
      Handler.Read_Next;
   end Read_Null_Record;

   package FileOperationRegistrationOptions_Scope is
      package FileOperationRegistrationOptions_Map is new Minimal_Perfect_Hash
        (["filters"]);

   end FileOperationRegistrationOptions_Scope;

   procedure Read_FileOperationRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.FileOperationRegistrationOptions) is
      use FileOperationRegistrationOptions_Scope;
      procedure Read_FileOperationFilter_Vector
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.FileOperationFilter_Vector);

      procedure Read_FileOperationFilter_Vector
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.FileOperationFilter_Vector) is
      begin
         pragma Assert (Handler.Is_Start_Array);
         Handler.Read_Next;

         declare
            Set   : LSP.Structures.FileOperationFilter_Vector renames Value;
            Value : LSP.Structures.FileOperationFilter;
         begin
            Set.Clear;
            while not Handler.Is_End_Array loop
               Read_FileOperationFilter (Handler, Value);
               Set.Append (Value);
            end loop;
         end;

         Handler.Read_Next;
      end Read_FileOperationFilter_Vector;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case FileOperationRegistrationOptions_Map.Get_Index (Key) is
               when 1 =>  --  filters
                  Read_FileOperationFilter_Vector (Handler, Value.filters);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_FileOperationRegistrationOptions;

   procedure Read_CallHierarchyOutgoingCall_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CallHierarchyOutgoingCall_Vector) is
   begin
      pragma Assert (Handler.Is_Start_Array);
      Handler.Read_Next;

      declare
         Set   : LSP.Structures.CallHierarchyOutgoingCall_Vector renames Value;
         Value : LSP.Structures.CallHierarchyOutgoingCall;
      begin
         Set.Clear;
         while not Handler.Is_End_Array loop
            Read_CallHierarchyOutgoingCall (Handler, Value);
            Set.Append (Value);
         end loop;
      end;

      Handler.Read_Next;
   end Read_CallHierarchyOutgoingCall_Vector;

   procedure Read_WorkspaceFolder_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.WorkspaceFolder_Vector) is
   begin
      pragma Assert (Handler.Is_Start_Array);
      Handler.Read_Next;

      declare
         Set   : LSP.Structures.WorkspaceFolder_Vector renames Value;
         Value : LSP.Structures.WorkspaceFolder;
      begin
         Set.Clear;
         while not Handler.Is_End_Array loop
            Read_WorkspaceFolder (Handler, Value);
            Set.Append (Value);
         end loop;
      end;

      Handler.Read_Next;
   end Read_WorkspaceFolder_Vector;

   procedure Read_TextDocumentContentChangeEvent_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TextDocumentContentChangeEvent_Vector) is
   begin
      pragma Assert (Handler.Is_Start_Array);
      Handler.Read_Next;

      declare
         Set   :
           LSP.Structures.TextDocumentContentChangeEvent_Vector renames Value;
         Value : LSP.Structures.TextDocumentContentChangeEvent;
      begin
         Set.Clear;
         while not Handler.Is_End_Array loop
            Read_TextDocumentContentChangeEvent (Handler, Value);
            Set.Append (Value);
         end loop;
      end;

      Handler.Read_Next;
   end Read_TextDocumentContentChangeEvent_Vector;

   package FoldingRangeClientCapabilities_Scope is
      package FoldingRangeClientCapabilities_Map is new Minimal_Perfect_Hash
        (["dynamicRegistration",
         "rangeLimit",
         "lineFoldingOnly",
         "foldingRangeKind",
         "foldingRange"]);

      package foldingRange_OfFoldingRangeClientCapabilities_Scope is
         package foldingRange_OfFoldingRangeClientCapabilities_Map is new Minimal_Perfect_Hash
           (["collapsedText"]);

      end foldingRange_OfFoldingRangeClientCapabilities_Scope;

      package foldingRangeKind_OfFoldingRangeClientCapabilities_Scope is
         package foldingRangeKind_OfFoldingRangeClientCapabilities_Map is new Minimal_Perfect_Hash
           (["valueSet"]);

      end foldingRangeKind_OfFoldingRangeClientCapabilities_Scope;

   end FoldingRangeClientCapabilities_Scope;

   procedure Read_FoldingRangeClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.FoldingRangeClientCapabilities) is
      use FoldingRangeClientCapabilities_Scope;
      procedure Read_foldingRange_OfFoldingRangeClientCapabilities
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .foldingRange_OfFoldingRangeClientCapabilities);

      procedure Read_FoldingRangeKind_Set
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.FoldingRangeKind_Set);

      procedure Read_foldingRangeKind_OfFoldingRangeClientCapabilities
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .foldingRangeKind_OfFoldingRangeClientCapabilities);

      procedure Read_foldingRange_OfFoldingRangeClientCapabilities
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .foldingRange_OfFoldingRangeClientCapabilities) is
      begin
         pragma Assert (Handler.Is_Start_Object);
         Handler.Read_Next;

         while Handler.Is_Key_Name loop
            declare
               use foldingRange_OfFoldingRangeClientCapabilities_Scope;
               Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
            begin
               Handler.Read_Next;
               case foldingRange_OfFoldingRangeClientCapabilities_Map.Get_Index
                 (Key) is
                  when 1 =>  --  collapsedText
                     Value.collapsedText       :=
                       (Is_Set => True,
                        Value  => <>);
                     Value.collapsedText.Value := Handler.Boolean_Value;
                     Handler.Read_Next;
                  when others =>
                     Handler.Skip_Current_Value;
               end case;
            end;
         end loop;

         Handler.Read_Next;
      end Read_foldingRange_OfFoldingRangeClientCapabilities;

      procedure Read_FoldingRangeKind_Set
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.FoldingRangeKind_Set) is
      begin
         pragma Assert (Handler.Is_Start_Array);
         Handler.Read_Next;

         declare
            Set   : LSP.Structures.FoldingRangeKind_Set renames Value;
            Value : LSP.Enumerations.FoldingRangeKind;
         begin
            Set := (others => False);
            while not Handler.Is_End_Array loop
               Read_FoldingRangeKind (Handler, Value);
               Set (Value) := True;
            end loop;
         end;

         Handler.Read_Next;
      end Read_FoldingRangeKind_Set;

      procedure Read_foldingRangeKind_OfFoldingRangeClientCapabilities
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .foldingRangeKind_OfFoldingRangeClientCapabilities) is
      begin
         pragma Assert (Handler.Is_Start_Object);
         Handler.Read_Next;

         while Handler.Is_Key_Name loop
            declare
               use foldingRangeKind_OfFoldingRangeClientCapabilities_Scope;
               Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
            begin
               Handler.Read_Next;
               case foldingRangeKind_OfFoldingRangeClientCapabilities_Map
                 .Get_Index
                 (Key) is
                  when 1 =>  --  valueSet
                     Read_FoldingRangeKind_Set (Handler, Value.valueSet);
                  when others =>
                     Handler.Skip_Current_Value;
               end case;
            end;
         end loop;

         Handler.Read_Next;
      end Read_foldingRangeKind_OfFoldingRangeClientCapabilities;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case FoldingRangeClientCapabilities_Map.Get_Index (Key) is
               when 1 =>  --  dynamicRegistration
                  Value.dynamicRegistration       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.dynamicRegistration.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 2 =>  --  rangeLimit
                  Value.rangeLimit       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.rangeLimit.Value :=
                    Integer (Handler.Number_Value.Integer_Value);
                  Handler.Read_Next;
               when 3 =>  --  lineFoldingOnly
                  Value.lineFoldingOnly       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.lineFoldingOnly.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 4 =>  --  foldingRangeKind
                  Value.foldingRangeKind :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_foldingRangeKind_OfFoldingRangeClientCapabilities
                    (Handler, Value.foldingRangeKind.Value);
               when 5 =>  --  foldingRange
                  Value.foldingRange :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_foldingRange_OfFoldingRangeClientCapabilities
                    (Handler, Value.foldingRange.Value);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_FoldingRangeClientCapabilities;

   package CreateFile_Scope is
      package CreateFile_Map is new Minimal_Perfect_Hash
        (["kind",
         "annotationId",
         "uri",
         "options"]);

   end CreateFile_Scope;

   procedure Read_CreateFile
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CreateFile) is
      use CreateFile_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case CreateFile_Map.Get_Index (Key) is
               when 2 =>  --  annotationId
                  Value.annotationId :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_ChangeAnnotationIdentifier
                    (Handler, Value.annotationId.Value);
               when 1 =>  --  kind
                  Handler.Read_Next;  --  Skip string literal: create
               when 3 =>  --  uri
                  Value.uri := (Handler.String_Value with null record);
                  Handler.Read_Next;
               when 4 =>  --  options
                  Value.options :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_CreateFileOptions (Handler, Value.options.Value);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_CreateFile;

   package CompletionContext_Scope is
      package CompletionContext_Map is new Minimal_Perfect_Hash
        (["triggerKind",
         "triggerCharacter"]);

   end CompletionContext_Scope;

   procedure Read_CompletionContext
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CompletionContext) is
      use CompletionContext_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case CompletionContext_Map.Get_Index (Key) is
               when 1 =>  --  triggerKind
                  Read_CompletionTriggerKind (Handler, Value.triggerKind);
               when 2 =>  --  triggerCharacter
                  Value.triggerCharacter.Clear;
                  Value.triggerCharacter.Append (Handler.String_Value);
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_CompletionContext;

   procedure Read_NotebookCell_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.NotebookCell_Vector) is
   begin
      pragma Assert (Handler.Is_Start_Array);
      Handler.Read_Next;

      declare
         Set   : LSP.Structures.NotebookCell_Vector renames Value;
         Value : LSP.Structures.NotebookCell;
      begin
         Set.Clear;
         while not Handler.Is_End_Array loop
            Read_NotebookCell (Handler, Value);
            Set.Append (Value);
         end loop;
      end;

      Handler.Read_Next;
   end Read_NotebookCell_Vector;

   package DocumentDiagnosticParams_Scope is
      package DocumentDiagnosticParams_Map is new Minimal_Perfect_Hash
        (["workDoneToken",
         "partialResultToken",
         "textDocument",
         "identifier",
         "previousResultId"]);

   end DocumentDiagnosticParams_Scope;

   procedure Read_DocumentDiagnosticParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentDiagnosticParams) is
      use DocumentDiagnosticParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case DocumentDiagnosticParams_Map.Get_Index (Key) is
               when 1 =>  --  workDoneToken
                  Value.workDoneToken :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_ProgressToken (Handler, Value.workDoneToken.Value);
               when 2 =>  --  partialResultToken
                  Value.partialResultToken :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_ProgressToken (Handler, Value.partialResultToken.Value);
               when 3 =>  --  textDocument
                  Read_TextDocumentIdentifier (Handler, Value.textDocument);
               when 4 =>  --  identifier
                  Value.identifier.Clear;
                  Value.identifier.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 5 =>  --  previousResultId
                  Value.previousResultId.Clear;
                  Value.previousResultId.Append (Handler.String_Value);
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_DocumentDiagnosticParams;

   procedure Read_DocumentLink_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentLink_Vector) is
   begin
      pragma Assert (Handler.Is_Start_Array);
      Handler.Read_Next;

      declare
         Set   : LSP.Structures.DocumentLink_Vector renames Value;
         Value : LSP.Structures.DocumentLink;
      begin
         Set.Clear;
         while not Handler.Is_End_Array loop
            Read_DocumentLink (Handler, Value);
            Set.Append (Value);
         end loop;
      end;

      Handler.Read_Next;
   end Read_DocumentLink_Vector;

   package CodeActionKind_Map is new Minimal_Perfect_Hash
     (["",
      "quickfix",
      "refactor",
      "refactor.extract",
      "refactor.inline",
      "refactor.rewrite",
      "source",
      "source.organizeImports",
      "source.fixAll"]);

   procedure Read_CodeActionKind
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.CodeActionKind) is
   begin
      Value :=
        LSP.Enumerations.CodeActionKind'Val
          (CodeActionKind_Map.Get_Index (Handler.String_Value) - 1);
      Handler.Read_Next;
   end Read_CodeActionKind;

   package RelatedUnchangedDocumentDiagnosticReport_Scope is
      package RelatedUnchangedDocumentDiagnosticReport_Map is new Minimal_Perfect_Hash
        (["kind",
         "resultId",
         "relatedDocuments"]);

   end RelatedUnchangedDocumentDiagnosticReport_Scope;

   procedure Read_RelatedUnchangedDocumentDiagnosticReport
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.RelatedUnchangedDocumentDiagnosticReport) is
      use RelatedUnchangedDocumentDiagnosticReport_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case RelatedUnchangedDocumentDiagnosticReport_Map.Get_Index
              (Key) is
               when 1 =>  --  kind
                  Handler.Read_Next;  --  Skip string literal: unchanged
               when 2 =>  --  resultId
                  Value.resultId.Clear;
                  Value.resultId.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 3 =>  --  relatedDocuments
                  pragma Assert (Handler.Is_Start_Object);
                  Handler.Read_Next;

                  while not Handler.Is_End_Object loop
                     declare
                        Map   :
                          LSP.Structures
                            .relatedDocuments_OfDocumentDiagnosticReportPartialResult renames
                          Value.relatedDocuments;
                        Key   : LSP.Structures.DocumentUri;
                        Value :
                          LSP.Structures
                            .relatedDocuments_OfDocumentDiagnosticReportPartialResult_Item;
                     begin
                        Key := (Handler.Key_Name with null record);
                        Handler.Read_Next;
                        Read_relatedDocuments_OfDocumentDiagnosticReportPartialResult_Item
                          (Handler, Value);
                        Map.Insert (Key, Value);
                     end;
                  end loop;

                  Handler.Read_Next;

               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_RelatedUnchangedDocumentDiagnosticReport;

   package DidChangeWatchedFilesClientCapabilities_Scope is
      package DidChangeWatchedFilesClientCapabilities_Map is new Minimal_Perfect_Hash
        (["dynamicRegistration",
         "relativePatternSupport"]);

   end DidChangeWatchedFilesClientCapabilities_Scope;

   procedure Read_DidChangeWatchedFilesClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DidChangeWatchedFilesClientCapabilities) is
      use DidChangeWatchedFilesClientCapabilities_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case DidChangeWatchedFilesClientCapabilities_Map.Get_Index (Key) is
               when 1 =>  --  dynamicRegistration
                  Value.dynamicRegistration       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.dynamicRegistration.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 2 =>  --  relativePatternSupport
                  Value.relativePatternSupport       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.relativePatternSupport.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_DidChangeWatchedFilesClientCapabilities;

   package SemanticTokenTypes_Map is new Minimal_Perfect_Hash
     (["namespace",
      "type",
      "class",
      "enum",
      "interface",
      "struct",
      "typeParameter",
      "parameter",
      "variable",
      "property",
      "enumMember",
      "event",
      "function",
      "method",
      "macro",
      "keyword",
      "modifier",
      "comment",
      "string",
      "number",
      "regexp",
      "operator",
      "decorator"]);

   procedure Read_SemanticTokenTypes
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.SemanticTokenTypes) is
   begin
      Value :=
        LSP.Enumerations.SemanticTokenTypes'Val
          (SemanticTokenTypes_Map.Get_Index (Handler.String_Value) - 1);
      Handler.Read_Next;
   end Read_SemanticTokenTypes;

   package AnnotatedTextEdit_Scope is
      package AnnotatedTextEdit_Map is new Minimal_Perfect_Hash
        (["range",
         "newText",
         "annotationId"]);

   end AnnotatedTextEdit_Scope;

   procedure Read_AnnotatedTextEdit
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.AnnotatedTextEdit) is
      use AnnotatedTextEdit_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case AnnotatedTextEdit_Map.Get_Index (Key) is
               when 1 =>  --  range
                  Read_A_Range (Handler, Value.a_range);
               when 2 =>  --  newText
                  Value.newText.Clear;
                  Value.newText.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 3 =>  --  annotationId
                  Read_ChangeAnnotationIdentifier
                    (Handler, Value.annotationId);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_AnnotatedTextEdit;

   package DeclarationOptions_Scope is
      package DeclarationOptions_Map is new Minimal_Perfect_Hash
        (["workDoneProgress"]);

   end DeclarationOptions_Scope;

   procedure Read_DeclarationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DeclarationOptions) is
      use DeclarationOptions_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case DeclarationOptions_Map.Get_Index (Key) is
               when 1 =>  --  workDoneProgress
                  Value.workDoneProgress       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.workDoneProgress.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_DeclarationOptions;

   package SignatureInformation_Scope is
      package SignatureInformation_Map is new Minimal_Perfect_Hash
        (["label",
         "documentation",
         "parameters",
         "activeParameter"]);

   end SignatureInformation_Scope;

   procedure Read_SignatureInformation
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SignatureInformation) is
      use SignatureInformation_Scope;
      procedure Read_ParameterInformation_Vector
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.ParameterInformation_Vector);

      procedure Read_ParameterInformation_Vector
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.ParameterInformation_Vector) is
      begin
         pragma Assert (Handler.Is_Start_Array);
         Handler.Read_Next;

         declare
            Set   : LSP.Structures.ParameterInformation_Vector renames Value;
            Value : LSP.Structures.ParameterInformation;
         begin
            Set.Clear;
            while not Handler.Is_End_Array loop
               Read_ParameterInformation (Handler, Value);
               Set.Append (Value);
            end loop;
         end;

         Handler.Read_Next;
      end Read_ParameterInformation_Vector;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case SignatureInformation_Map.Get_Index (Key) is
               when 1 =>  --  label
                  Value.label.Clear;
                  Value.label.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 2 =>  --  documentation
                  Value.documentation :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_Virtual_String_Or_MarkupContent
                    (Handler, Value.documentation.Value);
               when 3 =>  --  parameters
                  Read_ParameterInformation_Vector (Handler, Value.parameters);
               when 4 =>  --  activeParameter
                  Value.activeParameter       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.activeParameter.Value :=
                    Integer (Handler.Number_Value.Integer_Value);
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_SignatureInformation;

   procedure Read_TextDocumentSaveReason
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.TextDocumentSaveReason) is
   begin
      Value :=
        LSP.Enumerations.TextDocumentSaveReason'Val
          (Handler.Number_Value.Integer_Value - 1);
      Handler.Read_Next;
   end Read_TextDocumentSaveReason;

   package SemanticTokens_Scope is
      package SemanticTokens_Map is new Minimal_Perfect_Hash
        (["resultId",
         "data"]);

   end SemanticTokens_Scope;

   procedure Read_SemanticTokens
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SemanticTokens) is
      use SemanticTokens_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case SemanticTokens_Map.Get_Index (Key) is
               when 1 =>  --  resultId
                  Value.resultId.Clear;
                  Value.resultId.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 2 =>  --  data
                  Read_Natural_Vector (Handler, Value.data);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_SemanticTokens;

   package InlineValueParams_Scope is
      package InlineValueParams_Map is new Minimal_Perfect_Hash
        (["workDoneToken",
         "textDocument",
         "range",
         "context"]);

   end InlineValueParams_Scope;

   procedure Read_InlineValueParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.InlineValueParams) is
      use InlineValueParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case InlineValueParams_Map.Get_Index (Key) is
               when 1 =>  --  workDoneToken
                  Value.workDoneToken :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_ProgressToken (Handler, Value.workDoneToken.Value);
               when 2 =>  --  textDocument
                  Read_TextDocumentIdentifier (Handler, Value.textDocument);
               when 3 =>  --  range
                  Read_A_Range (Handler, Value.a_range);
               when 4 =>  --  context
                  Read_InlineValueContext (Handler, Value.context);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_InlineValueParams;

   package TokenFormat_Map is new Minimal_Perfect_Hash (["relative"]);

   procedure Read_TokenFormat
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.TokenFormat) is
   begin
      Value :=
        LSP.Enumerations.TokenFormat'Val
          (TokenFormat_Map.Get_Index (Handler.String_Value) - 1);
      Handler.Read_Next;
   end Read_TokenFormat;

   package TextDocumentRegistrationOptions_Scope is
      package TextDocumentRegistrationOptions_Map is new Minimal_Perfect_Hash
        (["documentSelector"]);

   end TextDocumentRegistrationOptions_Scope;

   procedure Read_TextDocumentRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TextDocumentRegistrationOptions) is
      use TextDocumentRegistrationOptions_Scope;
      procedure Read_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.DocumentSelector_Or_Null);

      procedure Read_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.DocumentSelector_Or_Null) is
      begin
         if Handler.Is_Null_Value then
            Handler.Read_Next;
         else
            Value :=
              (Is_Null => False,
               Value   => <>);
            Read_DocumentSelector (Handler, Value.Value);
         end if;
      end Read_DocumentSelector_Or_Null;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case TextDocumentRegistrationOptions_Map.Get_Index (Key) is
               when 1 =>  --  documentSelector
                  Read_DocumentSelector_Or_Null
                    (Handler, Value.documentSelector);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_TextDocumentRegistrationOptions;

   procedure Read_URI
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.URI) is
   begin
      Value.Clear;
      Value.Append (Handler.String_Value);
      Handler.Read_Next;
   end Read_URI;

   package SemanticTokenModifiers_Map is new Minimal_Perfect_Hash
     (["declaration",
      "definition",
      "readonly",
      "static",
      "deprecated",
      "abstract",
      "async",
      "modification",
      "documentation",
      "defaultLibrary"]);

   procedure Read_SemanticTokenModifiers
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.SemanticTokenModifiers) is
   begin
      Value :=
        LSP.Enumerations.SemanticTokenModifiers'Val
          (SemanticTokenModifiers_Map.Get_Index (Handler.String_Value) - 1);
      Handler.Read_Next;
   end Read_SemanticTokenModifiers;

   package ConfigurationItem_Scope is
      package ConfigurationItem_Map is new Minimal_Perfect_Hash
        (["scopeUri",
         "section"]);

   end ConfigurationItem_Scope;

   procedure Read_ConfigurationItem
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ConfigurationItem) is
      use ConfigurationItem_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case ConfigurationItem_Map.Get_Index (Key) is
               when 1 =>  --  scopeUri
                  Value.scopeUri.Clear;
                  Value.scopeUri.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 2 =>  --  section
                  Value.section.Clear;
                  Value.section.Append (Handler.String_Value);
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_ConfigurationItem;

   package DocumentOnTypeFormattingClientCapabilities_Scope is
      package DocumentOnTypeFormattingClientCapabilities_Map is new Minimal_Perfect_Hash
        (["dynamicRegistration"]);

   end DocumentOnTypeFormattingClientCapabilities_Scope;

   procedure Read_DocumentOnTypeFormattingClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value : out LSP.Structures.DocumentOnTypeFormattingClientCapabilities) is
      use DocumentOnTypeFormattingClientCapabilities_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case DocumentOnTypeFormattingClientCapabilities_Map.Get_Index
              (Key) is
               when 1 =>  --  dynamicRegistration
                  Value.dynamicRegistration       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.dynamicRegistration.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_DocumentOnTypeFormattingClientCapabilities;

   procedure Read_SymbolKind
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.SymbolKind) is
   begin
      Value :=
        LSP.Enumerations.SymbolKind'Val
          (Handler.Number_Value.Integer_Value - 1);
      Handler.Read_Next;
   end Read_SymbolKind;

   package DidSaveNotebookDocumentParams_Scope is
      package DidSaveNotebookDocumentParams_Map is new Minimal_Perfect_Hash
        (["notebookDocument"]);

   end DidSaveNotebookDocumentParams_Scope;

   procedure Read_DidSaveNotebookDocumentParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DidSaveNotebookDocumentParams) is
      use DidSaveNotebookDocumentParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case DidSaveNotebookDocumentParams_Map.Get_Index (Key) is
               when 1 =>  --  notebookDocument
                  Read_NotebookDocumentIdentifier
                    (Handler, Value.notebookDocument);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_DidSaveNotebookDocumentParams;

   package Declaration_Result_Scope is
      package Declaration_Result_Map is new Minimal_Perfect_Hash
        (["uri",
         "range"]);

   end Declaration_Result_Scope;

   procedure Read_Declaration_Result
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Declaration_Result) is
      use Declaration_Result_Scope;
   begin
      declare
         Parent  :
           constant not null access VSS.JSON.Pull_Readers.JSON_Pull_Reader'
             Class :=
           Handler'Access;
         Handler :
           VSS.JSON.Pull_Readers.Buffered.JSON_Buffered_Pull_Reader (Parent);
      begin
         Handler.Mark;
         if Handler.Is_Null_Value then
            Value :=
              (Kind   => LSP.Structures.Variant_3,
               others => <>);
         elsif Handler.Is_Start_Object then
            Handler.Read_Next;
            Value :=
              (Kind   => LSP.Structures.Variant_2,
               others => <>);
            while Handler.Is_Key_Name loop
               declare
                  Key   : constant VSS.Strings.Virtual_String :=
                    Handler.Key_Name;
                  Index : constant Natural                    :=
                    Declaration_Result_Map.Get_Index (Key);
               begin
                  Handler.Read_Next;
                  case Index is
                     when 1 =>  --  uri
                        Value :=
                          (Kind   => LSP.Structures.Variant_1,
                           others => <>);
                        exit;
                     when 2 =>  --  range
                        Value :=
                          (Kind   => LSP.Structures.Variant_1,
                           others => <>);
                        exit;
                     when others =>
                        Handler.Skip_Current_Value;
                  end case;
               end;
            end loop;
         else
            raise Program_Error;  --  Unexpected JSON value
         end if;

         Handler.Reset;
         Handler.Unmark;

         case Value.Kind is
            when LSP.Structures.Variant_1 =>
               Read_Declaration (Handler, Value.Variant_1);
            when LSP.Structures.Variant_2 =>
               Read_DeclarationLink_Vector (Handler, Value.Variant_2);
            when LSP.Structures.Variant_3 =>
               null;  --  #null_value
               Handler.Read_Next;
         end case;
      end;
   end Read_Declaration_Result;

   package Definition_Progress_Report_Scope is
      package Definition_Progress_Report_Map is new Minimal_Perfect_Hash
        (["uri",
         "range",
         "originSelectionRange",
         "targetUri",
         "targetRange",
         "targetSelectionRange"]);

   end Definition_Progress_Report_Scope;

   procedure Read_Definition_Progress_Report
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Definition_Progress_Report) is
      use Definition_Progress_Report_Scope;
   begin
      declare
         Parent  :
           constant not null access VSS.JSON.Pull_Readers.JSON_Pull_Reader'
             Class :=
           Handler'Access;
         Handler :
           VSS.JSON.Pull_Readers.Buffered.JSON_Buffered_Pull_Reader (Parent);
      begin
         Handler.Mark;
         if Handler.Is_Start_Array then
            Handler.Read_Next;
         end if;
         if Handler.Is_Start_Object then
            Handler.Read_Next;
            while Handler.Is_Key_Name loop
               declare
                  Key   : constant VSS.Strings.Virtual_String :=
                    Handler.Key_Name;
                  Index : constant Natural                    :=
                    Definition_Progress_Report_Map.Get_Index (Key);
               begin
                  Handler.Read_Next;
                  case Index is
                     when 1 =>  --  uri
                        Value :=
                          (Kind   => LSP.Structures.Variant_1,
                           others => <>);
                        exit;
                     when 2 =>  --  range
                        Value :=
                          (Kind   => LSP.Structures.Variant_1,
                           others => <>);
                        exit;
                     when 3 =>  --  originSelectionRange
                        Value :=
                          (Kind   => LSP.Structures.Variant_2,
                           others => <>);
                        exit;
                     when 4 =>  --  targetUri
                        Value :=
                          (Kind   => LSP.Structures.Variant_2,
                           others => <>);
                        exit;
                     when 5 =>  --  targetRange
                        Value :=
                          (Kind   => LSP.Structures.Variant_2,
                           others => <>);
                        exit;
                     when 6 =>  --  targetSelectionRange
                        Value :=
                          (Kind   => LSP.Structures.Variant_2,
                           others => <>);
                        exit;
                     when others =>
                        Handler.Skip_Current_Value;
                  end case;
               end;
            end loop;
         else
            raise Program_Error;  --  Unexpected JSON value
         end if;

         Handler.Reset;
         Handler.Unmark;

         case Value.Kind is
            when LSP.Structures.Variant_1 =>
               Read_Location_Vector (Handler, Value.Variant_1);
            when LSP.Structures.Variant_2 =>
               Read_DefinitionLink_Vector (Handler, Value.Variant_2);
         end case;
      end;
   end Read_Definition_Progress_Report;

   package SemanticTokensParams_Scope is
      package SemanticTokensParams_Map is new Minimal_Perfect_Hash
        (["workDoneToken",
         "partialResultToken",
         "textDocument"]);

   end SemanticTokensParams_Scope;

   procedure Read_SemanticTokensParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SemanticTokensParams) is
      use SemanticTokensParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case SemanticTokensParams_Map.Get_Index (Key) is
               when 1 =>  --  workDoneToken
                  Value.workDoneToken :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_ProgressToken (Handler, Value.workDoneToken.Value);
               when 2 =>  --  partialResultToken
                  Value.partialResultToken :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_ProgressToken (Handler, Value.partialResultToken.Value);
               when 3 =>  --  textDocument
                  Read_TextDocumentIdentifier (Handler, Value.textDocument);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_SemanticTokensParams;

   procedure Read_ColorInformation_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ColorInformation_Vector) is
   begin
      pragma Assert (Handler.Is_Start_Array);
      Handler.Read_Next;

      declare
         Set   : LSP.Structures.ColorInformation_Vector renames Value;
         Value : LSP.Structures.ColorInformation;
      begin
         Set.Clear;
         while not Handler.Is_End_Array loop
            Read_ColorInformation (Handler, Value);
            Set.Append (Value);
         end loop;
      end;

      Handler.Read_Next;
   end Read_ColorInformation_Vector;

   package SelectionRangeClientCapabilities_Scope is
      package SelectionRangeClientCapabilities_Map is new Minimal_Perfect_Hash
        (["dynamicRegistration"]);

   end SelectionRangeClientCapabilities_Scope;

   procedure Read_SelectionRangeClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SelectionRangeClientCapabilities) is
      use SelectionRangeClientCapabilities_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case SelectionRangeClientCapabilities_Map.Get_Index (Key) is
               when 1 =>  --  dynamicRegistration
                  Value.dynamicRegistration       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.dynamicRegistration.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_SelectionRangeClientCapabilities;

   package DocumentSymbol_Progress_Report_Scope is
      package DocumentSymbol_Progress_Report_Map is new Minimal_Perfect_Hash
        (["location",
         "containerName",
         "detail",
         "range",
         "selectionRange",
         "children"]);

   end DocumentSymbol_Progress_Report_Scope;

   procedure Read_DocumentSymbol_Progress_Report
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentSymbol_Progress_Report) is
      use DocumentSymbol_Progress_Report_Scope;
   begin
      declare
         Parent  :
           constant not null access VSS.JSON.Pull_Readers.JSON_Pull_Reader'
             Class :=
           Handler'Access;
         Handler :
           VSS.JSON.Pull_Readers.Buffered.JSON_Buffered_Pull_Reader (Parent);
      begin
         Handler.Mark;
         if Handler.Is_Start_Array then
            Handler.Read_Next;
         end if;
         if Handler.Is_Start_Object then
            Handler.Read_Next;
            while Handler.Is_Key_Name loop
               declare
                  Key   : constant VSS.Strings.Virtual_String :=
                    Handler.Key_Name;
                  Index : constant Natural                    :=
                    DocumentSymbol_Progress_Report_Map.Get_Index (Key);
               begin
                  Handler.Read_Next;
                  case Index is
                     when 1 =>  --  location
                        Value :=
                          (Kind   => LSP.Structures.Variant_1,
                           others => <>);
                        exit;
                     when 2 =>  --  containerName
                        Value :=
                          (Kind   => LSP.Structures.Variant_1,
                           others => <>);
                        exit;
                     when 3 =>  --  detail
                        Value :=
                          (Kind   => LSP.Structures.Variant_2,
                           others => <>);
                        exit;
                     when 4 =>  --  range
                        Value :=
                          (Kind   => LSP.Structures.Variant_2,
                           others => <>);
                        exit;
                     when 5 =>  --  selectionRange
                        Value :=
                          (Kind   => LSP.Structures.Variant_2,
                           others => <>);
                        exit;
                     when 6 =>  --  children
                        Value :=
                          (Kind   => LSP.Structures.Variant_2,
                           others => <>);
                        exit;
                     when others =>
                        Handler.Skip_Current_Value;
                  end case;
               end;
            end loop;
         else
            raise Program_Error;  --  Unexpected JSON value
         end if;

         Handler.Reset;
         Handler.Unmark;

         case Value.Kind is
            when LSP.Structures.Variant_1 =>
               Read_SymbolInformation_Vector (Handler, Value.Variant_1);
            when LSP.Structures.Variant_2 =>
               Read_DocumentSymbol_Vector (Handler, Value.Variant_2);
         end case;
      end;
   end Read_DocumentSymbol_Progress_Report;

   procedure Read_CallHierarchyIncomingCall_Vector_Or_Null
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CallHierarchyIncomingCall_Vector_Or_Null) is
   begin
      if Handler.Is_Null_Value then
         Handler.Read_Next;
      else
         Value.Clear;
         Read_CallHierarchyIncomingCall_Vector (Handler, Value);
      end if;
   end Read_CallHierarchyIncomingCall_Vector_Or_Null;

   procedure Read_Range_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Range_Vector) is
   begin
      pragma Assert (Handler.Is_Start_Array);
      Handler.Read_Next;

      declare
         Set   : LSP.Structures.Range_Vector renames Value;
         Value : LSP.Structures.A_Range;
      begin
         Set.Clear;
         while not Handler.Is_End_Array loop
            Read_A_Range (Handler, Value);
            Set.Append (Value);
         end loop;
      end;

      Handler.Read_Next;
   end Read_Range_Vector;

   procedure Read_Definition
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Definition) is
   begin
      declare
         Set   : LSP.Structures.Definition renames Value;
         Value : LSP.Structures.Location;
      begin
         Set.Clear;
         if Handler.Is_Start_Array then
            Handler.Read_Next;
            while not Handler.Is_End_Array loop
               Read_Location (Handler, Value);
               Set.Append (Value);
            end loop;
            Handler.Read_Next;

         else
            Read_Location (Handler, Value);
            Set.Append (Value);
         end if;
      end;

   end Read_Definition;

   package NotebookDocumentFilter_Scope is
      package NotebookDocumentFilter_Map is new Minimal_Perfect_Hash
        (["notebookType",
         "scheme",
         "pattern"]);

   end NotebookDocumentFilter_Scope;

   procedure Read_NotebookDocumentFilter
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.NotebookDocumentFilter) is
      use NotebookDocumentFilter_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while Handler.Is_Key_Name loop
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case NotebookDocumentFilter_Map.Get_Index (Key) is
               when 1 =>  --  notebookType
                  Value.notebookType.Clear;
                  Value.notebookType.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 2 =>  --  scheme
                  Value.scheme.Clear;
                  Value.scheme.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 3 =>  --  pattern
                  Value.pattern.Clear;
                  Value.pattern.Append (Handler.String_Value);
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_NotebookDocumentFilter;

   package Color_Scope is
      package Color_Map is new Minimal_Perfect_Hash
        (["red",
         "green",
         "blue",
         "alpha"]);

   end Color_Scope;

   procedure Read_Color
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Color) is
      use Color_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case Color_Map.Get_Index (Key) is
               when 1 =>  --  red
                  Value.red := Float (Handler.Number_Value.Float_Value);
                  Handler.Read_Next;
               when 2 =>  --  green
                  Value.green := Float (Handler.Number_Value.Float_Value);
                  Handler.Read_Next;
               when 3 =>  --  blue
                  Value.blue := Float (Handler.Number_Value.Float_Value);
                  Handler.Read_Next;
               when 4 =>  --  alpha
                  Value.alpha := Float (Handler.Number_Value.Float_Value);
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_Color;

   package FileOperationPattern_Scope is
      package FileOperationPattern_Map is new Minimal_Perfect_Hash
        (["glob",
         "matches",
         "options"]);

   end FileOperationPattern_Scope;

   procedure Read_FileOperationPattern
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.FileOperationPattern) is
      use FileOperationPattern_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case FileOperationPattern_Map.Get_Index (Key) is
               when 1 =>  --  glob
                  Value.glob.Clear;
                  Value.glob.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 2 =>  --  matches
                  Value.matches :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_FileOperationPatternKind (Handler, Value.matches.Value);
               when 3 =>  --  options
                  Value.options :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_FileOperationPatternOptions
                    (Handler, Value.options.Value);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_FileOperationPattern;

   package LinkedEditingRanges_Scope is
      package LinkedEditingRanges_Map is new Minimal_Perfect_Hash
        (["ranges",
         "wordPattern"]);

   end LinkedEditingRanges_Scope;

   procedure Read_LinkedEditingRanges
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.LinkedEditingRanges) is
      use LinkedEditingRanges_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case LinkedEditingRanges_Map.Get_Index (Key) is
               when 1 =>  --  ranges
                  Read_Range_Vector (Handler, Value.ranges);
               when 2 =>  --  wordPattern
                  Value.wordPattern.Clear;
                  Value.wordPattern.Append (Handler.String_Value);
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_LinkedEditingRanges;

   procedure Read_Virtual_String_Or_MarkupContent
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Virtual_String_Or_MarkupContent) is
   begin
      if Handler.Is_String_Value then
         Value :=
           (Is_Virtual_String => True,
            others            => <>);
      else
         Value :=
           (Is_Virtual_String => False,
            others            => <>);
      end if;

      case Value.Is_Virtual_String is
         when True =>
            Value.Virtual_String.Clear;
            Value.Virtual_String.Append (Handler.String_Value);
            Handler.Read_Next;
         when False =>
            Read_MarkupContent (Handler, Value.MarkupContent);
      end case;
   end Read_Virtual_String_Or_MarkupContent;

   procedure Read_CodeLens_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CodeLens_Vector) is
   begin
      pragma Assert (Handler.Is_Start_Array);
      Handler.Read_Next;

      declare
         Set   : LSP.Structures.CodeLens_Vector renames Value;
         Value : LSP.Structures.CodeLens;
      begin
         Set.Clear;
         while not Handler.Is_End_Array loop
            Read_CodeLens (Handler, Value);
            Set.Append (Value);
         end loop;
      end;

      Handler.Read_Next;
   end Read_CodeLens_Vector;

   package ServerCapabilities_Scope is
      package ServerCapabilities_Map is new Minimal_Perfect_Hash
        (["positionEncoding",
         "textDocumentSync",
         "notebookDocumentSync",
         "completionProvider",
         "hoverProvider",
         "signatureHelpProvider",
         "declarationProvider",
         "definitionProvider",
         "typeDefinitionProvider",
         "implementationProvider",
         "referencesProvider",
         "documentHighlightProvider",
         "documentSymbolProvider",
         "codeActionProvider",
         "codeLensProvider",
         "documentLinkProvider",
         "colorProvider",
         "workspaceSymbolProvider",
         "documentFormattingProvider",
         "documentRangeFormattingProvider",
         "documentOnTypeFormattingProvider",
         "renameProvider",
         "foldingRangeProvider",
         "selectionRangeProvider",
         "executeCommandProvider",
         "callHierarchyProvider",
         "linkedEditingRangeProvider",
         "semanticTokensProvider",
         "monikerProvider",
         "typeHierarchyProvider",
         "inlineValueProvider",
         "inlayHintProvider",
         "diagnosticProvider",
         "workspace",
         "experimental"]);

      package NotebookDocumentSyncOptions_Or_NotebookDocumentSyncRegistrationOptions_Scope
      is
         package NotebookDocumentSyncOptions_Or_NotebookDocumentSyncRegistrationOptions_Map is new Minimal_Perfect_Hash
           (["id"]);

      end NotebookDocumentSyncOptions_Or_NotebookDocumentSyncRegistrationOptions_Scope;

      package implementationProvider_OfServerCapabilities_Scope is
         package implementationProvider_OfServerCapabilities_Map is new Minimal_Perfect_Hash
           (["documentSelector",
            "id"]);

      end implementationProvider_OfServerCapabilities_Scope;

      package typeHierarchyProvider_OfServerCapabilities_Scope is
         package typeHierarchyProvider_OfServerCapabilities_Map is new Minimal_Perfect_Hash
           (["documentSelector",
            "id"]);

      end typeHierarchyProvider_OfServerCapabilities_Scope;

      package DiagnosticOptions_Or_DiagnosticRegistrationOptions_Scope is
         package DiagnosticOptions_Or_DiagnosticRegistrationOptions_Map is new Minimal_Perfect_Hash
           (["documentSelector",
            "id"]);

      end DiagnosticOptions_Or_DiagnosticRegistrationOptions_Scope;

      package inlayHintProvider_OfServerCapabilities_Scope is
         package inlayHintProvider_OfServerCapabilities_Map is new Minimal_Perfect_Hash
           (["documentSelector",
            "id"]);

      end inlayHintProvider_OfServerCapabilities_Scope;

      package SemanticTokensOptions_Or_SemanticTokensRegistrationOptions_Scope
      is
         package SemanticTokensOptions_Or_SemanticTokensRegistrationOptions_Map is new Minimal_Perfect_Hash
           (["documentSelector",
            "id"]);

      end SemanticTokensOptions_Or_SemanticTokensRegistrationOptions_Scope;

      package monikerProvider_OfServerCapabilities_Scope is
         package monikerProvider_OfServerCapabilities_Map is new Minimal_Perfect_Hash
           (["documentSelector"]);

      end monikerProvider_OfServerCapabilities_Scope;

      package selectionRangeProvider_OfServerCapabilities_Scope is
         package selectionRangeProvider_OfServerCapabilities_Map is new Minimal_Perfect_Hash
           (["documentSelector",
            "id"]);

      end selectionRangeProvider_OfServerCapabilities_Scope;

      package workspace_OfServerCapabilities_Scope is
         package workspace_OfServerCapabilities_Map is new Minimal_Perfect_Hash
           (["workspaceFolders",
            "fileOperations"]);

      end workspace_OfServerCapabilities_Scope;

      package declarationProvider_OfServerCapabilities_Scope is
         package declarationProvider_OfServerCapabilities_Map is new Minimal_Perfect_Hash
           (["documentSelector",
            "id"]);

      end declarationProvider_OfServerCapabilities_Scope;

      package callHierarchyProvider_OfServerCapabilities_Scope is
         package callHierarchyProvider_OfServerCapabilities_Map is new Minimal_Perfect_Hash
           (["documentSelector",
            "id"]);

      end callHierarchyProvider_OfServerCapabilities_Scope;

      package linkedEditingRangeProvider_OfServerCapabilities_Scope is
         package linkedEditingRangeProvider_OfServerCapabilities_Map is new Minimal_Perfect_Hash
           (["documentSelector",
            "id"]);

      end linkedEditingRangeProvider_OfServerCapabilities_Scope;

      package foldingRangeProvider_OfServerCapabilities_Scope is
         package foldingRangeProvider_OfServerCapabilities_Map is new Minimal_Perfect_Hash
           (["documentSelector",
            "id"]);

      end foldingRangeProvider_OfServerCapabilities_Scope;

      package typeDefinitionProvider_OfServerCapabilities_Scope is
         package typeDefinitionProvider_OfServerCapabilities_Map is new Minimal_Perfect_Hash
           (["documentSelector",
            "id"]);

      end typeDefinitionProvider_OfServerCapabilities_Scope;

      package inlineValueProvider_OfServerCapabilities_Scope is
         package inlineValueProvider_OfServerCapabilities_Map is new Minimal_Perfect_Hash
           (["documentSelector",
            "id"]);

      end inlineValueProvider_OfServerCapabilities_Scope;

      package colorProvider_OfServerCapabilities_Scope is
         package colorProvider_OfServerCapabilities_Map is new Minimal_Perfect_Hash
           (["documentSelector",
            "id"]);

      end colorProvider_OfServerCapabilities_Scope;

   end ServerCapabilities_Scope;

   procedure Read_ServerCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ServerCapabilities) is
      use ServerCapabilities_Scope;
      procedure Read_Boolean_Or_DocumentSymbolOptions
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.Boolean_Or_DocumentSymbolOptions);

      procedure Read_NotebookDocumentSyncOptions_Or_NotebookDocumentSyncRegistrationOptions
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .NotebookDocumentSyncOptions_Or_NotebookDocumentSyncRegistrationOptions);

      procedure Read_Boolean_Or_WorkspaceSymbolOptions
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.Boolean_Or_WorkspaceSymbolOptions);

      procedure Read_implementationProvider_OfServerCapabilities
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .implementationProvider_OfServerCapabilities);

      procedure Read_typeHierarchyProvider_OfServerCapabilities
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .typeHierarchyProvider_OfServerCapabilities);

      procedure Read_DiagnosticOptions_Or_DiagnosticRegistrationOptions
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .DiagnosticOptions_Or_DiagnosticRegistrationOptions);

      procedure Read_inlayHintProvider_OfServerCapabilities
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.inlayHintProvider_OfServerCapabilities);

      procedure Read_SemanticTokensOptions_Or_SemanticTokensRegistrationOptions
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .SemanticTokensOptions_Or_SemanticTokensRegistrationOptions);

      procedure Read_monikerProvider_OfServerCapabilities
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.monikerProvider_OfServerCapabilities);

      procedure Read_Boolean_Or_CodeActionOptions
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.Boolean_Or_CodeActionOptions);

      procedure Read_selectionRangeProvider_OfServerCapabilities
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .selectionRangeProvider_OfServerCapabilities);

      procedure Read_Boolean_Or_DocumentRangeFormattingOptions
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value : out LSP.Structures.Boolean_Or_DocumentRangeFormattingOptions);

      procedure Read_Boolean_Or_HoverOptions
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.Boolean_Or_HoverOptions);

      procedure Read_workspace_OfServerCapabilities
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.workspace_OfServerCapabilities);

      procedure Read_declarationProvider_OfServerCapabilities
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value : out LSP.Structures.declarationProvider_OfServerCapabilities);

      procedure Read_callHierarchyProvider_OfServerCapabilities
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .callHierarchyProvider_OfServerCapabilities);

      procedure Read_Boolean_Or_DocumentHighlightOptions
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.Boolean_Or_DocumentHighlightOptions);

      procedure Read_TextDocumentSyncOptions_Or_TextDocumentSyncKind
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .TextDocumentSyncOptions_Or_TextDocumentSyncKind);

      procedure Read_linkedEditingRangeProvider_OfServerCapabilities
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .linkedEditingRangeProvider_OfServerCapabilities);

      procedure Read_Boolean_Or_DocumentFormattingOptions
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.Boolean_Or_DocumentFormattingOptions);

      procedure Read_Boolean_Or_ReferenceOptions
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.Boolean_Or_ReferenceOptions);

      procedure Read_foldingRangeProvider_OfServerCapabilities
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value : out LSP.Structures.foldingRangeProvider_OfServerCapabilities);

      procedure Read_typeDefinitionProvider_OfServerCapabilities
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .typeDefinitionProvider_OfServerCapabilities);

      procedure Read_inlineValueProvider_OfServerCapabilities
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value : out LSP.Structures.inlineValueProvider_OfServerCapabilities);

      procedure Read_colorProvider_OfServerCapabilities
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.colorProvider_OfServerCapabilities);

      procedure Read_Boolean_Or_DefinitionOptions
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.Boolean_Or_DefinitionOptions);

      procedure Read_Boolean_Or_RenameOptions
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.Boolean_Or_RenameOptions);

      procedure Read_Boolean_Or_DocumentSymbolOptions
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.Boolean_Or_DocumentSymbolOptions) is
      begin
         if Handler.Is_Boolean_Value then
            Value :=
              (Is_Boolean => True,
               others     => <>);
         else
            Value :=
              (Is_Boolean => False,
               others     => <>);
         end if;

         case Value.Is_Boolean is
            when True =>
               Value.Boolean := Handler.Boolean_Value;
               Handler.Read_Next;
            when False =>
               Read_DocumentSymbolOptions
                 (Handler, Value.DocumentSymbolOptions);
         end case;
      end Read_Boolean_Or_DocumentSymbolOptions;

      procedure Read_NotebookDocumentSyncOptions_Or_NotebookDocumentSyncRegistrationOptions
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .NotebookDocumentSyncOptions_Or_NotebookDocumentSyncRegistrationOptions) is
         use
           NotebookDocumentSyncOptions_Or_NotebookDocumentSyncRegistrationOptions_Scope;
      begin
         declare
            Parent  :
              constant not null access VSS.JSON.Pull_Readers.JSON_Pull_Reader'
                Class :=
              Handler'Access;
            Handler :
              VSS.JSON.Pull_Readers.Buffered.JSON_Buffered_Pull_Reader
                (Parent);
         begin
            Handler.Mark;
            if Handler.Is_Start_Object then
               Handler.Read_Next;
               Value :=
                 (Is_NotebookDocumentSyncOptions => True,
                  others                         => <>);
               while Handler.Is_Key_Name loop
                  declare
                     Key   : constant VSS.Strings.Virtual_String :=
                       Handler.Key_Name;
                     Index : constant Natural                    :=
                       NotebookDocumentSyncOptions_Or_NotebookDocumentSyncRegistrationOptions_Map
                         .Get_Index
                         (Key);
                  begin
                     Handler.Read_Next;
                     case Index is
                        when 1 =>  --  id
                           Value :=
                             (Is_NotebookDocumentSyncOptions => False,
                              others                         => <>);
                           exit;
                        when others =>
                           Handler.Skip_Current_Value;
                     end case;
                  end;
               end loop;
            else
               raise Program_Error;  --  Unexpected JSON value
            end if;

            Handler.Reset;
            Handler.Unmark;

            case Value.Is_NotebookDocumentSyncOptions is
               when True =>
                  Read_NotebookDocumentSyncOptions
                    (Handler, Value.NotebookDocumentSyncOptions);
               when False =>
                  Read_NotebookDocumentSyncRegistrationOptions
                    (Handler, Value.NotebookDocumentSyncRegistrationOptions);
            end case;
         end;
      end Read_NotebookDocumentSyncOptions_Or_NotebookDocumentSyncRegistrationOptions;

      procedure Read_Boolean_Or_WorkspaceSymbolOptions
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.Boolean_Or_WorkspaceSymbolOptions) is
      begin
         if Handler.Is_Boolean_Value then
            Value :=
              (Is_Boolean => True,
               others     => <>);
         else
            Value :=
              (Is_Boolean => False,
               others     => <>);
         end if;

         case Value.Is_Boolean is
            when True =>
               Value.Boolean := Handler.Boolean_Value;
               Handler.Read_Next;
            when False =>
               Read_WorkspaceSymbolOptions
                 (Handler, Value.WorkspaceSymbolOptions);
         end case;
      end Read_Boolean_Or_WorkspaceSymbolOptions;

      procedure Read_implementationProvider_OfServerCapabilities
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .implementationProvider_OfServerCapabilities) is
         use implementationProvider_OfServerCapabilities_Scope;
      begin
         declare
            Parent  :
              constant not null access VSS.JSON.Pull_Readers.JSON_Pull_Reader'
                Class :=
              Handler'Access;
            Handler :
              VSS.JSON.Pull_Readers.Buffered.JSON_Buffered_Pull_Reader
                (Parent);
         begin
            Handler.Mark;
            if Handler.Is_Boolean_Value then
               Value :=
                 (Kind   => LSP.Structures.Variant_1,
                  others => <>);
            elsif Handler.Is_Start_Object then
               Handler.Read_Next;
               Value :=
                 (Kind   => LSP.Structures.Variant_2,
                  others => <>);
               while Handler.Is_Key_Name loop
                  declare
                     Key   : constant VSS.Strings.Virtual_String :=
                       Handler.Key_Name;
                     Index : constant Natural                    :=
                       implementationProvider_OfServerCapabilities_Map
                         .Get_Index
                         (Key);
                  begin
                     Handler.Read_Next;
                     case Index is
                        when 1 =>  --  documentSelector
                           Value :=
                             (Kind   => LSP.Structures.Variant_3,
                              others => <>);
                           exit;
                        when 2 =>  --  id
                           Value :=
                             (Kind   => LSP.Structures.Variant_3,
                              others => <>);
                           exit;
                        when others =>
                           Handler.Skip_Current_Value;
                     end case;
                  end;
               end loop;
            else
               raise Program_Error;  --  Unexpected JSON value
            end if;

            Handler.Reset;
            Handler.Unmark;

            case Value.Kind is
               when LSP.Structures.Variant_1 =>
                  Value.Variant_1 := Handler.Boolean_Value;
                  Handler.Read_Next;
               when LSP.Structures.Variant_2 =>
                  Read_ImplementationOptions (Handler, Value.Variant_2);
               when LSP.Structures.Variant_3 =>
                  Read_ImplementationRegistrationOptions
                    (Handler, Value.Variant_3);
            end case;
         end;
      end Read_implementationProvider_OfServerCapabilities;

      procedure Read_typeHierarchyProvider_OfServerCapabilities
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .typeHierarchyProvider_OfServerCapabilities) is
         use typeHierarchyProvider_OfServerCapabilities_Scope;
      begin
         declare
            Parent  :
              constant not null access VSS.JSON.Pull_Readers.JSON_Pull_Reader'
                Class :=
              Handler'Access;
            Handler :
              VSS.JSON.Pull_Readers.Buffered.JSON_Buffered_Pull_Reader
                (Parent);
         begin
            Handler.Mark;
            if Handler.Is_Boolean_Value then
               Value :=
                 (Kind   => LSP.Structures.Variant_1,
                  others => <>);
            elsif Handler.Is_Start_Object then
               Handler.Read_Next;
               Value :=
                 (Kind   => LSP.Structures.Variant_2,
                  others => <>);
               while Handler.Is_Key_Name loop
                  declare
                     Key   : constant VSS.Strings.Virtual_String :=
                       Handler.Key_Name;
                     Index : constant Natural                    :=
                       typeHierarchyProvider_OfServerCapabilities_Map.Get_Index
                         (Key);
                  begin
                     Handler.Read_Next;
                     case Index is
                        when 1 =>  --  documentSelector
                           Value :=
                             (Kind   => LSP.Structures.Variant_3,
                              others => <>);
                           exit;
                        when 2 =>  --  id
                           Value :=
                             (Kind   => LSP.Structures.Variant_3,
                              others => <>);
                           exit;
                        when others =>
                           Handler.Skip_Current_Value;
                     end case;
                  end;
               end loop;
            else
               raise Program_Error;  --  Unexpected JSON value
            end if;

            Handler.Reset;
            Handler.Unmark;

            case Value.Kind is
               when LSP.Structures.Variant_1 =>
                  Value.Variant_1 := Handler.Boolean_Value;
                  Handler.Read_Next;
               when LSP.Structures.Variant_2 =>
                  Read_TypeHierarchyOptions (Handler, Value.Variant_2);
               when LSP.Structures.Variant_3 =>
                  Read_TypeHierarchyRegistrationOptions
                    (Handler, Value.Variant_3);
            end case;
         end;
      end Read_typeHierarchyProvider_OfServerCapabilities;

      procedure Read_DiagnosticOptions_Or_DiagnosticRegistrationOptions
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .DiagnosticOptions_Or_DiagnosticRegistrationOptions) is
         use DiagnosticOptions_Or_DiagnosticRegistrationOptions_Scope;
      begin
         declare
            Parent  :
              constant not null access VSS.JSON.Pull_Readers.JSON_Pull_Reader'
                Class :=
              Handler'Access;
            Handler :
              VSS.JSON.Pull_Readers.Buffered.JSON_Buffered_Pull_Reader
                (Parent);
         begin
            Handler.Mark;
            if Handler.Is_Start_Object then
               Handler.Read_Next;
               Value :=
                 (Is_DiagnosticOptions => True,
                  others               => <>);
               while Handler.Is_Key_Name loop
                  declare
                     Key   : constant VSS.Strings.Virtual_String :=
                       Handler.Key_Name;
                     Index : constant Natural                    :=
                       DiagnosticOptions_Or_DiagnosticRegistrationOptions_Map
                         .Get_Index
                         (Key);
                  begin
                     Handler.Read_Next;
                     case Index is
                        when 1 =>  --  documentSelector
                           Value :=
                             (Is_DiagnosticOptions => False,
                              others               => <>);
                           exit;
                        when 2 =>  --  id
                           Value :=
                             (Is_DiagnosticOptions => False,
                              others               => <>);
                           exit;
                        when others =>
                           Handler.Skip_Current_Value;
                     end case;
                  end;
               end loop;
            else
               raise Program_Error;  --  Unexpected JSON value
            end if;

            Handler.Reset;
            Handler.Unmark;

            case Value.Is_DiagnosticOptions is
               when True =>
                  Read_DiagnosticOptions (Handler, Value.DiagnosticOptions);
               when False =>
                  Read_DiagnosticRegistrationOptions
                    (Handler, Value.DiagnosticRegistrationOptions);
            end case;
         end;
      end Read_DiagnosticOptions_Or_DiagnosticRegistrationOptions;

      procedure Read_inlayHintProvider_OfServerCapabilities
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value : out LSP.Structures.inlayHintProvider_OfServerCapabilities) is
         use inlayHintProvider_OfServerCapabilities_Scope;
      begin
         declare
            Parent  :
              constant not null access VSS.JSON.Pull_Readers.JSON_Pull_Reader'
                Class :=
              Handler'Access;
            Handler :
              VSS.JSON.Pull_Readers.Buffered.JSON_Buffered_Pull_Reader
                (Parent);
         begin
            Handler.Mark;
            if Handler.Is_Boolean_Value then
               Value :=
                 (Kind   => LSP.Structures.Variant_1,
                  others => <>);
            elsif Handler.Is_Start_Object then
               Handler.Read_Next;
               Value :=
                 (Kind   => LSP.Structures.Variant_2,
                  others => <>);
               while Handler.Is_Key_Name loop
                  declare
                     Key   : constant VSS.Strings.Virtual_String :=
                       Handler.Key_Name;
                     Index : constant Natural                    :=
                       inlayHintProvider_OfServerCapabilities_Map.Get_Index
                         (Key);
                  begin
                     Handler.Read_Next;
                     case Index is
                        when 1 =>  --  documentSelector
                           Value :=
                             (Kind   => LSP.Structures.Variant_3,
                              others => <>);
                           exit;
                        when 2 =>  --  id
                           Value :=
                             (Kind   => LSP.Structures.Variant_3,
                              others => <>);
                           exit;
                        when others =>
                           Handler.Skip_Current_Value;
                     end case;
                  end;
               end loop;
            else
               raise Program_Error;  --  Unexpected JSON value
            end if;

            Handler.Reset;
            Handler.Unmark;

            case Value.Kind is
               when LSP.Structures.Variant_1 =>
                  Value.Variant_1 := Handler.Boolean_Value;
                  Handler.Read_Next;
               when LSP.Structures.Variant_2 =>
                  Read_InlayHintOptions (Handler, Value.Variant_2);
               when LSP.Structures.Variant_3 =>
                  Read_InlayHintRegistrationOptions (Handler, Value.Variant_3);
            end case;
         end;
      end Read_inlayHintProvider_OfServerCapabilities;

      procedure Read_SemanticTokensOptions_Or_SemanticTokensRegistrationOptions
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .SemanticTokensOptions_Or_SemanticTokensRegistrationOptions) is
         use SemanticTokensOptions_Or_SemanticTokensRegistrationOptions_Scope;
      begin
         declare
            Parent  :
              constant not null access VSS.JSON.Pull_Readers.JSON_Pull_Reader'
                Class :=
              Handler'Access;
            Handler :
              VSS.JSON.Pull_Readers.Buffered.JSON_Buffered_Pull_Reader
                (Parent);
         begin
            Handler.Mark;
            if Handler.Is_Start_Object then
               Handler.Read_Next;
               Value :=
                 (Is_SemanticTokensOptions => True,
                  others                   => <>);
               while Handler.Is_Key_Name loop
                  declare
                     Key   : constant VSS.Strings.Virtual_String :=
                       Handler.Key_Name;
                     Index : constant Natural                    :=
                       SemanticTokensOptions_Or_SemanticTokensRegistrationOptions_Map
                         .Get_Index
                         (Key);
                  begin
                     Handler.Read_Next;
                     case Index is
                        when 1 =>  --  documentSelector
                           Value :=
                             (Is_SemanticTokensOptions => False,
                              others                   => <>);
                           exit;
                        when 2 =>  --  id
                           Value :=
                             (Is_SemanticTokensOptions => False,
                              others                   => <>);
                           exit;
                        when others =>
                           Handler.Skip_Current_Value;
                     end case;
                  end;
               end loop;
            else
               raise Program_Error;  --  Unexpected JSON value
            end if;

            Handler.Reset;
            Handler.Unmark;

            case Value.Is_SemanticTokensOptions is
               when True =>
                  Read_SemanticTokensOptions
                    (Handler, Value.SemanticTokensOptions);
               when False =>
                  Read_SemanticTokensRegistrationOptions
                    (Handler, Value.SemanticTokensRegistrationOptions);
            end case;
         end;
      end Read_SemanticTokensOptions_Or_SemanticTokensRegistrationOptions;

      procedure Read_monikerProvider_OfServerCapabilities
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.monikerProvider_OfServerCapabilities) is
         use monikerProvider_OfServerCapabilities_Scope;
      begin
         declare
            Parent  :
              constant not null access VSS.JSON.Pull_Readers.JSON_Pull_Reader'
                Class :=
              Handler'Access;
            Handler :
              VSS.JSON.Pull_Readers.Buffered.JSON_Buffered_Pull_Reader
                (Parent);
         begin
            Handler.Mark;
            if Handler.Is_Boolean_Value then
               Value :=
                 (Kind   => LSP.Structures.Variant_1,
                  others => <>);
            elsif Handler.Is_Start_Object then
               Handler.Read_Next;
               Value :=
                 (Kind   => LSP.Structures.Variant_2,
                  others => <>);
               while Handler.Is_Key_Name loop
                  declare
                     Key   : constant VSS.Strings.Virtual_String :=
                       Handler.Key_Name;
                     Index : constant Natural                    :=
                       monikerProvider_OfServerCapabilities_Map.Get_Index
                         (Key);
                  begin
                     Handler.Read_Next;
                     case Index is
                        when 1 =>  --  documentSelector
                           Value :=
                             (Kind   => LSP.Structures.Variant_3,
                              others => <>);
                           exit;
                        when others =>
                           Handler.Skip_Current_Value;
                     end case;
                  end;
               end loop;
            else
               raise Program_Error;  --  Unexpected JSON value
            end if;

            Handler.Reset;
            Handler.Unmark;

            case Value.Kind is
               when LSP.Structures.Variant_1 =>
                  Value.Variant_1 := Handler.Boolean_Value;
                  Handler.Read_Next;
               when LSP.Structures.Variant_2 =>
                  Read_MonikerOptions (Handler, Value.Variant_2);
               when LSP.Structures.Variant_3 =>
                  Read_MonikerRegistrationOptions (Handler, Value.Variant_3);
            end case;
         end;
      end Read_monikerProvider_OfServerCapabilities;

      procedure Read_Boolean_Or_CodeActionOptions
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.Boolean_Or_CodeActionOptions) is
      begin
         if Handler.Is_Boolean_Value then
            Value :=
              (Is_Boolean => True,
               others     => <>);
         else
            Value :=
              (Is_Boolean => False,
               others     => <>);
         end if;

         case Value.Is_Boolean is
            when True =>
               Value.Boolean := Handler.Boolean_Value;
               Handler.Read_Next;
            when False =>
               Read_CodeActionOptions (Handler, Value.CodeActionOptions);
         end case;
      end Read_Boolean_Or_CodeActionOptions;

      procedure Read_selectionRangeProvider_OfServerCapabilities
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .selectionRangeProvider_OfServerCapabilities) is
         use selectionRangeProvider_OfServerCapabilities_Scope;
      begin
         declare
            Parent  :
              constant not null access VSS.JSON.Pull_Readers.JSON_Pull_Reader'
                Class :=
              Handler'Access;
            Handler :
              VSS.JSON.Pull_Readers.Buffered.JSON_Buffered_Pull_Reader
                (Parent);
         begin
            Handler.Mark;
            if Handler.Is_Boolean_Value then
               Value :=
                 (Kind   => LSP.Structures.Variant_1,
                  others => <>);
            elsif Handler.Is_Start_Object then
               Handler.Read_Next;
               Value :=
                 (Kind   => LSP.Structures.Variant_2,
                  others => <>);
               while Handler.Is_Key_Name loop
                  declare
                     Key   : constant VSS.Strings.Virtual_String :=
                       Handler.Key_Name;
                     Index : constant Natural                    :=
                       selectionRangeProvider_OfServerCapabilities_Map
                         .Get_Index
                         (Key);
                  begin
                     Handler.Read_Next;
                     case Index is
                        when 1 =>  --  documentSelector
                           Value :=
                             (Kind   => LSP.Structures.Variant_3,
                              others => <>);
                           exit;
                        when 2 =>  --  id
                           Value :=
                             (Kind   => LSP.Structures.Variant_3,
                              others => <>);
                           exit;
                        when others =>
                           Handler.Skip_Current_Value;
                     end case;
                  end;
               end loop;
            else
               raise Program_Error;  --  Unexpected JSON value
            end if;

            Handler.Reset;
            Handler.Unmark;

            case Value.Kind is
               when LSP.Structures.Variant_1 =>
                  Value.Variant_1 := Handler.Boolean_Value;
                  Handler.Read_Next;
               when LSP.Structures.Variant_2 =>
                  Read_SelectionRangeOptions (Handler, Value.Variant_2);
               when LSP.Structures.Variant_3 =>
                  Read_SelectionRangeRegistrationOptions
                    (Handler, Value.Variant_3);
            end case;
         end;
      end Read_selectionRangeProvider_OfServerCapabilities;

      procedure Read_Boolean_Or_DocumentRangeFormattingOptions
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .Boolean_Or_DocumentRangeFormattingOptions) is
      begin
         if Handler.Is_Boolean_Value then
            Value :=
              (Is_Boolean => True,
               others     => <>);
         else
            Value :=
              (Is_Boolean => False,
               others     => <>);
         end if;

         case Value.Is_Boolean is
            when True =>
               Value.Boolean := Handler.Boolean_Value;
               Handler.Read_Next;
            when False =>
               Read_DocumentRangeFormattingOptions
                 (Handler, Value.DocumentRangeFormattingOptions);
         end case;
      end Read_Boolean_Or_DocumentRangeFormattingOptions;

      procedure Read_Boolean_Or_HoverOptions
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.Boolean_Or_HoverOptions) is
      begin
         if Handler.Is_Boolean_Value then
            Value :=
              (Is_Boolean => True,
               others     => <>);
         else
            Value :=
              (Is_Boolean => False,
               others     => <>);
         end if;

         case Value.Is_Boolean is
            when True =>
               Value.Boolean := Handler.Boolean_Value;
               Handler.Read_Next;
            when False =>
               Read_HoverOptions (Handler, Value.HoverOptions);
         end case;
      end Read_Boolean_Or_HoverOptions;

      procedure Read_workspace_OfServerCapabilities
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.workspace_OfServerCapabilities) is
      begin
         pragma Assert (Handler.Is_Start_Object);
         Handler.Read_Next;

         while Handler.Is_Key_Name loop
            declare
               use workspace_OfServerCapabilities_Scope;
               Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
            begin
               Handler.Read_Next;
               case workspace_OfServerCapabilities_Map.Get_Index (Key) is
                  when 1 =>  --  workspaceFolders
                     Value.workspaceFolders :=
                       (Is_Set => True,
                        Value  => <>);
                     Read_WorkspaceFoldersServerCapabilities
                       (Handler, Value.workspaceFolders.Value);
                  when 2 =>  --  fileOperations
                     Value.fileOperations :=
                       (Is_Set => True,
                        Value  => <>);
                     Read_FileOperationOptions
                       (Handler, Value.fileOperations.Value);
                  when others =>
                     Handler.Skip_Current_Value;
               end case;
            end;
         end loop;

         Handler.Read_Next;
      end Read_workspace_OfServerCapabilities;

      procedure Read_declarationProvider_OfServerCapabilities
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .declarationProvider_OfServerCapabilities) is
         use declarationProvider_OfServerCapabilities_Scope;
      begin
         declare
            Parent  :
              constant not null access VSS.JSON.Pull_Readers.JSON_Pull_Reader'
                Class :=
              Handler'Access;
            Handler :
              VSS.JSON.Pull_Readers.Buffered.JSON_Buffered_Pull_Reader
                (Parent);
         begin
            Handler.Mark;
            if Handler.Is_Boolean_Value then
               Value :=
                 (Kind   => LSP.Structures.Variant_1,
                  others => <>);
            elsif Handler.Is_Start_Object then
               Handler.Read_Next;
               Value :=
                 (Kind   => LSP.Structures.Variant_2,
                  others => <>);
               while Handler.Is_Key_Name loop
                  declare
                     Key   : constant VSS.Strings.Virtual_String :=
                       Handler.Key_Name;
                     Index : constant Natural                    :=
                       declarationProvider_OfServerCapabilities_Map.Get_Index
                         (Key);
                  begin
                     Handler.Read_Next;
                     case Index is
                        when 1 =>  --  documentSelector
                           Value :=
                             (Kind   => LSP.Structures.Variant_3,
                              others => <>);
                           exit;
                        when 2 =>  --  id
                           Value :=
                             (Kind   => LSP.Structures.Variant_3,
                              others => <>);
                           exit;
                        when others =>
                           Handler.Skip_Current_Value;
                     end case;
                  end;
               end loop;
            else
               raise Program_Error;  --  Unexpected JSON value
            end if;

            Handler.Reset;
            Handler.Unmark;

            case Value.Kind is
               when LSP.Structures.Variant_1 =>
                  Value.Variant_1 := Handler.Boolean_Value;
                  Handler.Read_Next;
               when LSP.Structures.Variant_2 =>
                  Read_DeclarationOptions (Handler, Value.Variant_2);
               when LSP.Structures.Variant_3 =>
                  Read_DeclarationRegistrationOptions
                    (Handler, Value.Variant_3);
            end case;
         end;
      end Read_declarationProvider_OfServerCapabilities;

      procedure Read_callHierarchyProvider_OfServerCapabilities
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .callHierarchyProvider_OfServerCapabilities) is
         use callHierarchyProvider_OfServerCapabilities_Scope;
      begin
         declare
            Parent  :
              constant not null access VSS.JSON.Pull_Readers.JSON_Pull_Reader'
                Class :=
              Handler'Access;
            Handler :
              VSS.JSON.Pull_Readers.Buffered.JSON_Buffered_Pull_Reader
                (Parent);
         begin
            Handler.Mark;
            if Handler.Is_Boolean_Value then
               Value :=
                 (Kind   => LSP.Structures.Variant_1,
                  others => <>);
            elsif Handler.Is_Start_Object then
               Handler.Read_Next;
               Value :=
                 (Kind   => LSP.Structures.Variant_2,
                  others => <>);
               while Handler.Is_Key_Name loop
                  declare
                     Key   : constant VSS.Strings.Virtual_String :=
                       Handler.Key_Name;
                     Index : constant Natural                    :=
                       callHierarchyProvider_OfServerCapabilities_Map.Get_Index
                         (Key);
                  begin
                     Handler.Read_Next;
                     case Index is
                        when 1 =>  --  documentSelector
                           Value :=
                             (Kind   => LSP.Structures.Variant_3,
                              others => <>);
                           exit;
                        when 2 =>  --  id
                           Value :=
                             (Kind   => LSP.Structures.Variant_3,
                              others => <>);
                           exit;
                        when others =>
                           Handler.Skip_Current_Value;
                     end case;
                  end;
               end loop;
            else
               raise Program_Error;  --  Unexpected JSON value
            end if;

            Handler.Reset;
            Handler.Unmark;

            case Value.Kind is
               when LSP.Structures.Variant_1 =>
                  Value.Variant_1 := Handler.Boolean_Value;
                  Handler.Read_Next;
               when LSP.Structures.Variant_2 =>
                  Read_CallHierarchyOptions (Handler, Value.Variant_2);
               when LSP.Structures.Variant_3 =>
                  Read_CallHierarchyRegistrationOptions
                    (Handler, Value.Variant_3);
            end case;
         end;
      end Read_callHierarchyProvider_OfServerCapabilities;

      procedure Read_Boolean_Or_DocumentHighlightOptions
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.Boolean_Or_DocumentHighlightOptions) is
      begin
         if Handler.Is_Boolean_Value then
            Value :=
              (Is_Boolean => True,
               others     => <>);
         else
            Value :=
              (Is_Boolean => False,
               others     => <>);
         end if;

         case Value.Is_Boolean is
            when True =>
               Value.Boolean := Handler.Boolean_Value;
               Handler.Read_Next;
            when False =>
               Read_DocumentHighlightOptions
                 (Handler, Value.DocumentHighlightOptions);
         end case;
      end Read_Boolean_Or_DocumentHighlightOptions;

      procedure Read_TextDocumentSyncOptions_Or_TextDocumentSyncKind
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .TextDocumentSyncOptions_Or_TextDocumentSyncKind) is
      begin
         if Handler.Is_Number_Value then
            Value :=
              (Is_TextDocumentSyncOptions => False,
               others                     => <>);
         else
            Value :=
              (Is_TextDocumentSyncOptions => True,
               others                     => <>);
         end if;

         case Value.Is_TextDocumentSyncOptions is
            when True =>
               Read_TextDocumentSyncOptions
                 (Handler, Value.TextDocumentSyncOptions);
            when False =>
               Read_TextDocumentSyncKind (Handler, Value.TextDocumentSyncKind);
         end case;
      end Read_TextDocumentSyncOptions_Or_TextDocumentSyncKind;

      procedure Read_linkedEditingRangeProvider_OfServerCapabilities
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .linkedEditingRangeProvider_OfServerCapabilities) is
         use linkedEditingRangeProvider_OfServerCapabilities_Scope;
      begin
         declare
            Parent  :
              constant not null access VSS.JSON.Pull_Readers.JSON_Pull_Reader'
                Class :=
              Handler'Access;
            Handler :
              VSS.JSON.Pull_Readers.Buffered.JSON_Buffered_Pull_Reader
                (Parent);
         begin
            Handler.Mark;
            if Handler.Is_Boolean_Value then
               Value :=
                 (Kind   => LSP.Structures.Variant_1,
                  others => <>);
            elsif Handler.Is_Start_Object then
               Handler.Read_Next;
               Value :=
                 (Kind   => LSP.Structures.Variant_2,
                  others => <>);
               while Handler.Is_Key_Name loop
                  declare
                     Key   : constant VSS.Strings.Virtual_String :=
                       Handler.Key_Name;
                     Index : constant Natural                    :=
                       linkedEditingRangeProvider_OfServerCapabilities_Map
                         .Get_Index
                         (Key);
                  begin
                     Handler.Read_Next;
                     case Index is
                        when 1 =>  --  documentSelector
                           Value :=
                             (Kind   => LSP.Structures.Variant_3,
                              others => <>);
                           exit;
                        when 2 =>  --  id
                           Value :=
                             (Kind   => LSP.Structures.Variant_3,
                              others => <>);
                           exit;
                        when others =>
                           Handler.Skip_Current_Value;
                     end case;
                  end;
               end loop;
            else
               raise Program_Error;  --  Unexpected JSON value
            end if;

            Handler.Reset;
            Handler.Unmark;

            case Value.Kind is
               when LSP.Structures.Variant_1 =>
                  Value.Variant_1 := Handler.Boolean_Value;
                  Handler.Read_Next;
               when LSP.Structures.Variant_2 =>
                  Read_LinkedEditingRangeOptions (Handler, Value.Variant_2);
               when LSP.Structures.Variant_3 =>
                  Read_LinkedEditingRangeRegistrationOptions
                    (Handler, Value.Variant_3);
            end case;
         end;
      end Read_linkedEditingRangeProvider_OfServerCapabilities;

      procedure Read_Boolean_Or_DocumentFormattingOptions
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.Boolean_Or_DocumentFormattingOptions) is
      begin
         if Handler.Is_Boolean_Value then
            Value :=
              (Is_Boolean => True,
               others     => <>);
         else
            Value :=
              (Is_Boolean => False,
               others     => <>);
         end if;

         case Value.Is_Boolean is
            when True =>
               Value.Boolean := Handler.Boolean_Value;
               Handler.Read_Next;
            when False =>
               Read_DocumentFormattingOptions
                 (Handler, Value.DocumentFormattingOptions);
         end case;
      end Read_Boolean_Or_DocumentFormattingOptions;

      procedure Read_Boolean_Or_ReferenceOptions
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.Boolean_Or_ReferenceOptions) is
      begin
         if Handler.Is_Boolean_Value then
            Value :=
              (Is_Boolean => True,
               others     => <>);
         else
            Value :=
              (Is_Boolean => False,
               others     => <>);
         end if;

         case Value.Is_Boolean is
            when True =>
               Value.Boolean := Handler.Boolean_Value;
               Handler.Read_Next;
            when False =>
               Read_ReferenceOptions (Handler, Value.ReferenceOptions);
         end case;
      end Read_Boolean_Or_ReferenceOptions;

      procedure Read_foldingRangeProvider_OfServerCapabilities
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .foldingRangeProvider_OfServerCapabilities) is
         use foldingRangeProvider_OfServerCapabilities_Scope;
      begin
         declare
            Parent  :
              constant not null access VSS.JSON.Pull_Readers.JSON_Pull_Reader'
                Class :=
              Handler'Access;
            Handler :
              VSS.JSON.Pull_Readers.Buffered.JSON_Buffered_Pull_Reader
                (Parent);
         begin
            Handler.Mark;
            if Handler.Is_Boolean_Value then
               Value :=
                 (Kind   => LSP.Structures.Variant_1,
                  others => <>);
            elsif Handler.Is_Start_Object then
               Handler.Read_Next;
               Value :=
                 (Kind   => LSP.Structures.Variant_2,
                  others => <>);
               while Handler.Is_Key_Name loop
                  declare
                     Key   : constant VSS.Strings.Virtual_String :=
                       Handler.Key_Name;
                     Index : constant Natural                    :=
                       foldingRangeProvider_OfServerCapabilities_Map.Get_Index
                         (Key);
                  begin
                     Handler.Read_Next;
                     case Index is
                        when 1 =>  --  documentSelector
                           Value :=
                             (Kind   => LSP.Structures.Variant_3,
                              others => <>);
                           exit;
                        when 2 =>  --  id
                           Value :=
                             (Kind   => LSP.Structures.Variant_3,
                              others => <>);
                           exit;
                        when others =>
                           Handler.Skip_Current_Value;
                     end case;
                  end;
               end loop;
            else
               raise Program_Error;  --  Unexpected JSON value
            end if;

            Handler.Reset;
            Handler.Unmark;

            case Value.Kind is
               when LSP.Structures.Variant_1 =>
                  Value.Variant_1 := Handler.Boolean_Value;
                  Handler.Read_Next;
               when LSP.Structures.Variant_2 =>
                  Read_FoldingRangeOptions (Handler, Value.Variant_2);
               when LSP.Structures.Variant_3 =>
                  Read_FoldingRangeRegistrationOptions
                    (Handler, Value.Variant_3);
            end case;
         end;
      end Read_foldingRangeProvider_OfServerCapabilities;

      procedure Read_typeDefinitionProvider_OfServerCapabilities
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .typeDefinitionProvider_OfServerCapabilities) is
         use typeDefinitionProvider_OfServerCapabilities_Scope;
      begin
         declare
            Parent  :
              constant not null access VSS.JSON.Pull_Readers.JSON_Pull_Reader'
                Class :=
              Handler'Access;
            Handler :
              VSS.JSON.Pull_Readers.Buffered.JSON_Buffered_Pull_Reader
                (Parent);
         begin
            Handler.Mark;
            if Handler.Is_Boolean_Value then
               Value :=
                 (Kind   => LSP.Structures.Variant_1,
                  others => <>);
            elsif Handler.Is_Start_Object then
               Handler.Read_Next;
               Value :=
                 (Kind   => LSP.Structures.Variant_2,
                  others => <>);
               while Handler.Is_Key_Name loop
                  declare
                     Key   : constant VSS.Strings.Virtual_String :=
                       Handler.Key_Name;
                     Index : constant Natural                    :=
                       typeDefinitionProvider_OfServerCapabilities_Map
                         .Get_Index
                         (Key);
                  begin
                     Handler.Read_Next;
                     case Index is
                        when 1 =>  --  documentSelector
                           Value :=
                             (Kind   => LSP.Structures.Variant_3,
                              others => <>);
                           exit;
                        when 2 =>  --  id
                           Value :=
                             (Kind   => LSP.Structures.Variant_3,
                              others => <>);
                           exit;
                        when others =>
                           Handler.Skip_Current_Value;
                     end case;
                  end;
               end loop;
            else
               raise Program_Error;  --  Unexpected JSON value
            end if;

            Handler.Reset;
            Handler.Unmark;

            case Value.Kind is
               when LSP.Structures.Variant_1 =>
                  Value.Variant_1 := Handler.Boolean_Value;
                  Handler.Read_Next;
               when LSP.Structures.Variant_2 =>
                  Read_TypeDefinitionOptions (Handler, Value.Variant_2);
               when LSP.Structures.Variant_3 =>
                  Read_TypeDefinitionRegistrationOptions
                    (Handler, Value.Variant_3);
            end case;
         end;
      end Read_typeDefinitionProvider_OfServerCapabilities;

      procedure Read_inlineValueProvider_OfServerCapabilities
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .inlineValueProvider_OfServerCapabilities) is
         use inlineValueProvider_OfServerCapabilities_Scope;
      begin
         declare
            Parent  :
              constant not null access VSS.JSON.Pull_Readers.JSON_Pull_Reader'
                Class :=
              Handler'Access;
            Handler :
              VSS.JSON.Pull_Readers.Buffered.JSON_Buffered_Pull_Reader
                (Parent);
         begin
            Handler.Mark;
            if Handler.Is_Boolean_Value then
               Value :=
                 (Kind   => LSP.Structures.Variant_1,
                  others => <>);
            elsif Handler.Is_Start_Object then
               Handler.Read_Next;
               Value :=
                 (Kind   => LSP.Structures.Variant_2,
                  others => <>);
               while Handler.Is_Key_Name loop
                  declare
                     Key   : constant VSS.Strings.Virtual_String :=
                       Handler.Key_Name;
                     Index : constant Natural                    :=
                       inlineValueProvider_OfServerCapabilities_Map.Get_Index
                         (Key);
                  begin
                     Handler.Read_Next;
                     case Index is
                        when 1 =>  --  documentSelector
                           Value :=
                             (Kind   => LSP.Structures.Variant_3,
                              others => <>);
                           exit;
                        when 2 =>  --  id
                           Value :=
                             (Kind   => LSP.Structures.Variant_3,
                              others => <>);
                           exit;
                        when others =>
                           Handler.Skip_Current_Value;
                     end case;
                  end;
               end loop;
            else
               raise Program_Error;  --  Unexpected JSON value
            end if;

            Handler.Reset;
            Handler.Unmark;

            case Value.Kind is
               when LSP.Structures.Variant_1 =>
                  Value.Variant_1 := Handler.Boolean_Value;
                  Handler.Read_Next;
               when LSP.Structures.Variant_2 =>
                  Read_InlineValueOptions (Handler, Value.Variant_2);
               when LSP.Structures.Variant_3 =>
                  Read_InlineValueRegistrationOptions
                    (Handler, Value.Variant_3);
            end case;
         end;
      end Read_inlineValueProvider_OfServerCapabilities;

      procedure Read_colorProvider_OfServerCapabilities
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.colorProvider_OfServerCapabilities) is
         use colorProvider_OfServerCapabilities_Scope;
      begin
         declare
            Parent  :
              constant not null access VSS.JSON.Pull_Readers.JSON_Pull_Reader'
                Class :=
              Handler'Access;
            Handler :
              VSS.JSON.Pull_Readers.Buffered.JSON_Buffered_Pull_Reader
                (Parent);
         begin
            Handler.Mark;
            if Handler.Is_Boolean_Value then
               Value :=
                 (Kind   => LSP.Structures.Variant_1,
                  others => <>);
            elsif Handler.Is_Start_Object then
               Handler.Read_Next;
               Value :=
                 (Kind   => LSP.Structures.Variant_2,
                  others => <>);
               while Handler.Is_Key_Name loop
                  declare
                     Key   : constant VSS.Strings.Virtual_String :=
                       Handler.Key_Name;
                     Index : constant Natural                    :=
                       colorProvider_OfServerCapabilities_Map.Get_Index (Key);
                  begin
                     Handler.Read_Next;
                     case Index is
                        when 1 =>  --  documentSelector
                           Value :=
                             (Kind   => LSP.Structures.Variant_3,
                              others => <>);
                           exit;
                        when 2 =>  --  id
                           Value :=
                             (Kind   => LSP.Structures.Variant_3,
                              others => <>);
                           exit;
                        when others =>
                           Handler.Skip_Current_Value;
                     end case;
                  end;
               end loop;
            else
               raise Program_Error;  --  Unexpected JSON value
            end if;

            Handler.Reset;
            Handler.Unmark;

            case Value.Kind is
               when LSP.Structures.Variant_1 =>
                  Value.Variant_1 := Handler.Boolean_Value;
                  Handler.Read_Next;
               when LSP.Structures.Variant_2 =>
                  Read_DocumentColorOptions (Handler, Value.Variant_2);
               when LSP.Structures.Variant_3 =>
                  Read_DocumentColorRegistrationOptions
                    (Handler, Value.Variant_3);
            end case;
         end;
      end Read_colorProvider_OfServerCapabilities;

      procedure Read_Boolean_Or_DefinitionOptions
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.Boolean_Or_DefinitionOptions) is
      begin
         if Handler.Is_Boolean_Value then
            Value :=
              (Is_Boolean => True,
               others     => <>);
         else
            Value :=
              (Is_Boolean => False,
               others     => <>);
         end if;

         case Value.Is_Boolean is
            when True =>
               Value.Boolean := Handler.Boolean_Value;
               Handler.Read_Next;
            when False =>
               Read_DefinitionOptions (Handler, Value.DefinitionOptions);
         end case;
      end Read_Boolean_Or_DefinitionOptions;

      procedure Read_Boolean_Or_RenameOptions
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.Boolean_Or_RenameOptions) is
      begin
         if Handler.Is_Boolean_Value then
            Value :=
              (Is_Boolean => True,
               others     => <>);
         else
            Value :=
              (Is_Boolean => False,
               others     => <>);
         end if;

         case Value.Is_Boolean is
            when True =>
               Value.Boolean := Handler.Boolean_Value;
               Handler.Read_Next;
            when False =>
               Read_RenameOptions (Handler, Value.RenameOptions);
         end case;
      end Read_Boolean_Or_RenameOptions;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case ServerCapabilities_Map.Get_Index (Key) is
               when 1 =>  --  positionEncoding
                  Value.positionEncoding :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_PositionEncodingKind
                    (Handler, Value.positionEncoding.Value);
               when 2 =>  --  textDocumentSync
                  Value.textDocumentSync :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_TextDocumentSyncOptions_Or_TextDocumentSyncKind
                    (Handler, Value.textDocumentSync.Value);
               when 3 =>  --  notebookDocumentSync
                  Value.notebookDocumentSync :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_NotebookDocumentSyncOptions_Or_NotebookDocumentSyncRegistrationOptions
                    (Handler, Value.notebookDocumentSync.Value);
               when 4 =>  --  completionProvider
                  Value.completionProvider :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_CompletionOptions
                    (Handler, Value.completionProvider.Value);
               when 5 =>  --  hoverProvider
                  Value.hoverProvider :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_Boolean_Or_HoverOptions
                    (Handler, Value.hoverProvider.Value);
               when 6 =>  --  signatureHelpProvider
                  Value.signatureHelpProvider :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_SignatureHelpOptions
                    (Handler, Value.signatureHelpProvider.Value);
               when 7 =>  --  declarationProvider
                  Value.declarationProvider :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_declarationProvider_OfServerCapabilities
                    (Handler, Value.declarationProvider.Value);
               when 8 =>  --  definitionProvider
                  Value.definitionProvider :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_Boolean_Or_DefinitionOptions
                    (Handler, Value.definitionProvider.Value);
               when 9 =>  --  typeDefinitionProvider
                  Value.typeDefinitionProvider :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_typeDefinitionProvider_OfServerCapabilities
                    (Handler, Value.typeDefinitionProvider.Value);
               when 10 =>  --  implementationProvider
                  Value.implementationProvider :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_implementationProvider_OfServerCapabilities
                    (Handler, Value.implementationProvider.Value);
               when 11 =>  --  referencesProvider
                  Value.referencesProvider :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_Boolean_Or_ReferenceOptions
                    (Handler, Value.referencesProvider.Value);
               when 12 =>  --  documentHighlightProvider
                  Value.documentHighlightProvider :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_Boolean_Or_DocumentHighlightOptions
                    (Handler, Value.documentHighlightProvider.Value);
               when 13 =>  --  documentSymbolProvider
                  Value.documentSymbolProvider :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_Boolean_Or_DocumentSymbolOptions
                    (Handler, Value.documentSymbolProvider.Value);
               when 14 =>  --  codeActionProvider
                  Value.codeActionProvider :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_Boolean_Or_CodeActionOptions
                    (Handler, Value.codeActionProvider.Value);
               when 15 =>  --  codeLensProvider
                  Value.codeLensProvider :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_CodeLensOptions (Handler, Value.codeLensProvider.Value);
               when 16 =>  --  documentLinkProvider
                  Value.documentLinkProvider :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_DocumentLinkOptions
                    (Handler, Value.documentLinkProvider.Value);
               when 17 =>  --  colorProvider
                  Value.colorProvider :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_colorProvider_OfServerCapabilities
                    (Handler, Value.colorProvider.Value);
               when 18 =>  --  workspaceSymbolProvider
                  Value.workspaceSymbolProvider :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_Boolean_Or_WorkspaceSymbolOptions
                    (Handler, Value.workspaceSymbolProvider.Value);
               when 19 =>  --  documentFormattingProvider
                  Value.documentFormattingProvider :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_Boolean_Or_DocumentFormattingOptions
                    (Handler, Value.documentFormattingProvider.Value);
               when 20 =>  --  documentRangeFormattingProvider
                  Value.documentRangeFormattingProvider :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_Boolean_Or_DocumentRangeFormattingOptions
                    (Handler, Value.documentRangeFormattingProvider.Value);
               when 21 =>  --  documentOnTypeFormattingProvider
                  Value.documentOnTypeFormattingProvider :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_DocumentOnTypeFormattingOptions
                    (Handler, Value.documentOnTypeFormattingProvider.Value);
               when 22 =>  --  renameProvider
                  Value.renameProvider :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_Boolean_Or_RenameOptions
                    (Handler, Value.renameProvider.Value);
               when 23 =>  --  foldingRangeProvider
                  Value.foldingRangeProvider :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_foldingRangeProvider_OfServerCapabilities
                    (Handler, Value.foldingRangeProvider.Value);
               when 24 =>  --  selectionRangeProvider
                  Value.selectionRangeProvider :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_selectionRangeProvider_OfServerCapabilities
                    (Handler, Value.selectionRangeProvider.Value);
               when 25 =>  --  executeCommandProvider
                  Value.executeCommandProvider :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_ExecuteCommandOptions
                    (Handler, Value.executeCommandProvider.Value);
               when 26 =>  --  callHierarchyProvider
                  Value.callHierarchyProvider :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_callHierarchyProvider_OfServerCapabilities
                    (Handler, Value.callHierarchyProvider.Value);
               when 27 =>  --  linkedEditingRangeProvider
                  Value.linkedEditingRangeProvider :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_linkedEditingRangeProvider_OfServerCapabilities
                    (Handler, Value.linkedEditingRangeProvider.Value);
               when 28 =>  --  semanticTokensProvider
                  Value.semanticTokensProvider :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_SemanticTokensOptions_Or_SemanticTokensRegistrationOptions
                    (Handler, Value.semanticTokensProvider.Value);
               when 29 =>  --  monikerProvider
                  Value.monikerProvider :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_monikerProvider_OfServerCapabilities
                    (Handler, Value.monikerProvider.Value);
               when 30 =>  --  typeHierarchyProvider
                  Value.typeHierarchyProvider :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_typeHierarchyProvider_OfServerCapabilities
                    (Handler, Value.typeHierarchyProvider.Value);
               when 31 =>  --  inlineValueProvider
                  Value.inlineValueProvider :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_inlineValueProvider_OfServerCapabilities
                    (Handler, Value.inlineValueProvider.Value);
               when 32 =>  --  inlayHintProvider
                  Value.inlayHintProvider :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_inlayHintProvider_OfServerCapabilities
                    (Handler, Value.inlayHintProvider.Value);
               when 33 =>  --  diagnosticProvider
                  Value.diagnosticProvider :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_DiagnosticOptions_Or_DiagnosticRegistrationOptions
                    (Handler, Value.diagnosticProvider.Value);
               when 34 =>  --  workspace
                  Value.workspace :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_workspace_OfServerCapabilities
                    (Handler, Value.workspace.Value);
               when 35 =>  --  experimental
                  Value.experimental :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_T (Handler, Value.experimental.Value);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_ServerCapabilities;

   package DidChangeWatchedFilesRegistrationOptions_Scope is
      package DidChangeWatchedFilesRegistrationOptions_Map is new Minimal_Perfect_Hash
        (["watchers"]);

   end DidChangeWatchedFilesRegistrationOptions_Scope;

   procedure Read_DidChangeWatchedFilesRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DidChangeWatchedFilesRegistrationOptions) is
      use DidChangeWatchedFilesRegistrationOptions_Scope;
      procedure Read_FileSystemWatcher_Vector
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.FileSystemWatcher_Vector);

      procedure Read_FileSystemWatcher_Vector
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.FileSystemWatcher_Vector) is
      begin
         pragma Assert (Handler.Is_Start_Array);
         Handler.Read_Next;

         declare
            Set   : LSP.Structures.FileSystemWatcher_Vector renames Value;
            Value : LSP.Structures.FileSystemWatcher;
         begin
            Set.Clear;
            while not Handler.Is_End_Array loop
               Read_FileSystemWatcher (Handler, Value);
               Set.Append (Value);
            end loop;
         end;

         Handler.Read_Next;
      end Read_FileSystemWatcher_Vector;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case DidChangeWatchedFilesRegistrationOptions_Map.Get_Index
              (Key) is
               when 1 =>  --  watchers
                  Read_FileSystemWatcher_Vector (Handler, Value.watchers);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_DidChangeWatchedFilesRegistrationOptions;

   package CallHierarchyOptions_Scope is
      package CallHierarchyOptions_Map is new Minimal_Perfect_Hash
        (["workDoneProgress"]);

   end CallHierarchyOptions_Scope;

   procedure Read_CallHierarchyOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CallHierarchyOptions) is
      use CallHierarchyOptions_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case CallHierarchyOptions_Map.Get_Index (Key) is
               when 1 =>  --  workDoneProgress
                  Value.workDoneProgress       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.workDoneProgress.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_CallHierarchyOptions;

   procedure Read_LSPAny
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.LSPAny) is
   begin
      LSP.Input_Tools.Read_LSPAny_Class (Handler, Value);
   end Read_LSPAny;

   package ImplementationRegistrationOptions_Scope is
      package ImplementationRegistrationOptions_Map is new Minimal_Perfect_Hash
        (["documentSelector",
         "workDoneProgress",
         "id"]);

   end ImplementationRegistrationOptions_Scope;

   procedure Read_ImplementationRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ImplementationRegistrationOptions) is
      use ImplementationRegistrationOptions_Scope;
      procedure Read_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.DocumentSelector_Or_Null);

      procedure Read_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.DocumentSelector_Or_Null) is
      begin
         if Handler.Is_Null_Value then
            Handler.Read_Next;
         else
            Value :=
              (Is_Null => False,
               Value   => <>);
            Read_DocumentSelector (Handler, Value.Value);
         end if;
      end Read_DocumentSelector_Or_Null;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case ImplementationRegistrationOptions_Map.Get_Index (Key) is
               when 1 =>  --  documentSelector
                  Read_DocumentSelector_Or_Null
                    (Handler, Value.Parent.documentSelector);
               when 2 =>  --  workDoneProgress
                  Value.workDoneProgress       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.workDoneProgress.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 3 =>  --  id
                  Value.id.Clear;
                  Value.id.Append (Handler.String_Value);
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_ImplementationRegistrationOptions;

   package LogTraceParams_Scope is
      package LogTraceParams_Map is new Minimal_Perfect_Hash
        (["message",
         "verbose"]);

   end LogTraceParams_Scope;

   procedure Read_LogTraceParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.LogTraceParams) is
      use LogTraceParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case LogTraceParams_Map.Get_Index (Key) is
               when 1 =>  --  message
                  Value.message.Clear;
                  Value.message.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 2 =>  --  verbose
                  Value.verbose.Clear;
                  Value.verbose.Append (Handler.String_Value);
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_LogTraceParams;

   package FoldingRangeRegistrationOptions_Scope is
      package FoldingRangeRegistrationOptions_Map is new Minimal_Perfect_Hash
        (["documentSelector",
         "workDoneProgress",
         "id"]);

   end FoldingRangeRegistrationOptions_Scope;

   procedure Read_FoldingRangeRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.FoldingRangeRegistrationOptions) is
      use FoldingRangeRegistrationOptions_Scope;
      procedure Read_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.DocumentSelector_Or_Null);

      procedure Read_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.DocumentSelector_Or_Null) is
      begin
         if Handler.Is_Null_Value then
            Handler.Read_Next;
         else
            Value :=
              (Is_Null => False,
               Value   => <>);
            Read_DocumentSelector (Handler, Value.Value);
         end if;
      end Read_DocumentSelector_Or_Null;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case FoldingRangeRegistrationOptions_Map.Get_Index (Key) is
               when 1 =>  --  documentSelector
                  Read_DocumentSelector_Or_Null
                    (Handler, Value.Parent.documentSelector);
               when 2 =>  --  workDoneProgress
                  Value.workDoneProgress       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.workDoneProgress.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 3 =>  --  id
                  Value.id.Clear;
                  Value.id.Append (Handler.String_Value);
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_FoldingRangeRegistrationOptions;

   package InlineValueContext_Scope is
      package InlineValueContext_Map is new Minimal_Perfect_Hash
        (["frameId",
         "stoppedLocation"]);

   end InlineValueContext_Scope;

   procedure Read_InlineValueContext
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.InlineValueContext) is
      use InlineValueContext_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case InlineValueContext_Map.Get_Index (Key) is
               when 1 =>  --  frameId
                  Value.frameId :=
                    Integer (Handler.Number_Value.Integer_Value);
                  Handler.Read_Next;
               when 2 =>  --  stoppedLocation
                  Read_A_Range (Handler, Value.stoppedLocation);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_InlineValueContext;

   procedure Read_TextDocumentIdentifier_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TextDocumentIdentifier_Vector) is
   begin
      pragma Assert (Handler.Is_Start_Array);
      Handler.Read_Next;

      declare
         Set   : LSP.Structures.TextDocumentIdentifier_Vector renames Value;
         Value : LSP.Structures.TextDocumentIdentifier;
      begin
         Set.Clear;
         while not Handler.Is_End_Array loop
            Read_TextDocumentIdentifier (Handler, Value);
            Set.Append (Value);
         end loop;
      end;

      Handler.Read_Next;
   end Read_TextDocumentIdentifier_Vector;

   procedure Read_DefinitionLink
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DefinitionLink) renames
     Read_LocationLink;

   package DocumentFormattingOptions_Scope is
      package DocumentFormattingOptions_Map is new Minimal_Perfect_Hash
        (["workDoneProgress"]);

   end DocumentFormattingOptions_Scope;

   procedure Read_DocumentFormattingOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentFormattingOptions) is
      use DocumentFormattingOptions_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case DocumentFormattingOptions_Map.Get_Index (Key) is
               when 1 =>  --  workDoneProgress
                  Value.workDoneProgress       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.workDoneProgress.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_DocumentFormattingOptions;

   procedure Read_SymbolInformation_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SymbolInformation_Vector) is
   begin
      pragma Assert (Handler.Is_Start_Array);
      Handler.Read_Next;

      declare
         Set   : LSP.Structures.SymbolInformation_Vector renames Value;
         Value : LSP.Structures.SymbolInformation;
      begin
         Set.Clear;
         while not Handler.Is_End_Array loop
            Read_SymbolInformation (Handler, Value);
            Set.Append (Value);
         end loop;
      end;

      Handler.Read_Next;
   end Read_SymbolInformation_Vector;

   package RenameFile_Scope is
      package RenameFile_Map is new Minimal_Perfect_Hash
        (["kind",
         "annotationId",
         "oldUri",
         "newUri",
         "options"]);

   end RenameFile_Scope;

   procedure Read_RenameFile
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.RenameFile) is
      use RenameFile_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case RenameFile_Map.Get_Index (Key) is
               when 2 =>  --  annotationId
                  Value.annotationId :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_ChangeAnnotationIdentifier
                    (Handler, Value.annotationId.Value);
               when 1 =>  --  kind
                  Handler.Read_Next;  --  Skip string literal: rename
               when 3 =>  --  oldUri
                  Value.oldUri := (Handler.String_Value with null record);
                  Handler.Read_Next;
               when 4 =>  --  newUri
                  Value.newUri := (Handler.String_Value with null record);
                  Handler.Read_Next;
               when 5 =>  --  options
                  Value.options :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_RenameFileOptions (Handler, Value.options.Value);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_RenameFile;

   procedure Read_PrepareRenameResult_Or_Null
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.PrepareRenameResult_Or_Null) is
   begin
      if Handler.Is_Null_Value then
         Handler.Read_Next;
      else
         Value :=
           (Is_Null => False,
            Value   => <>);
         Read_PrepareRenameResult (Handler, Value.Value);
      end if;
   end Read_PrepareRenameResult_Or_Null;

   package DidChangeConfigurationRegistrationOptions_Scope is
      package DidChangeConfigurationRegistrationOptions_Map is new Minimal_Perfect_Hash
        (["section"]);

   end DidChangeConfigurationRegistrationOptions_Scope;

   procedure Read_DidChangeConfigurationRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value : out LSP.Structures.DidChangeConfigurationRegistrationOptions) is
      use DidChangeConfigurationRegistrationOptions_Scope;
      procedure Read_Virtual_String_Vector
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.Virtual_String_Vector);

      procedure Read_Virtual_String_Vector
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.Virtual_String_Vector) is
      begin
         declare
            Set   : LSP.Structures.Virtual_String_Vector renames Value;
            Value : LSP.Structures.Virtual_String;
         begin
            Set.Clear;
            if Handler.Is_Start_Array then
               Handler.Read_Next;
               while not Handler.Is_End_Array loop
                  Value.Clear;
                  Value.Append (Handler.String_Value);
                  Handler.Read_Next;
                  Set.Append (Value);
               end loop;
               Handler.Read_Next;

            else
               Value.Clear;
               Value.Append (Handler.String_Value);
               Handler.Read_Next;
               Set.Append (Value);
            end if;
         end;

      end Read_Virtual_String_Vector;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case DidChangeConfigurationRegistrationOptions_Map.Get_Index
              (Key) is
               when 1 =>  --  section
                  Value.section :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_Virtual_String_Vector (Handler, Value.section.Value);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_DidChangeConfigurationRegistrationOptions;

   package CallHierarchyItem_Scope is
      package CallHierarchyItem_Map is new Minimal_Perfect_Hash
        (["name",
         "kind",
         "tags",
         "detail",
         "uri",
         "range",
         "selectionRange",
         "data"]);

   end CallHierarchyItem_Scope;

   procedure Read_CallHierarchyItem
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CallHierarchyItem) is
      use CallHierarchyItem_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case CallHierarchyItem_Map.Get_Index (Key) is
               when 1 =>  --  name
                  Value.name.Clear;
                  Value.name.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 2 =>  --  kind
                  Read_SymbolKind (Handler, Value.kind);
               when 3 =>  --  tags
                  Read_SymbolTag_Set (Handler, Value.tags);
               when 4 =>  --  detail
                  Value.detail.Clear;
                  Value.detail.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 5 =>  --  uri
                  Value.uri := (Handler.String_Value with null record);
                  Handler.Read_Next;
               when 6 =>  --  range
                  Read_A_Range (Handler, Value.a_range);
               when 7 =>  --  selectionRange
                  Read_A_Range (Handler, Value.selectionRange);
               when 8 =>  --  data
                  Read_LSPAny (Handler, Value.data);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_CallHierarchyItem;

   procedure Read_SymbolTag_Set
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SymbolTag_Set) is
   begin
      pragma Assert (Handler.Is_Start_Array);
      Handler.Read_Next;

      declare
         Set   : LSP.Structures.SymbolTag_Set renames Value;
         Value : LSP.Enumerations.SymbolTag;
      begin
         Set := (others => False);
         while not Handler.Is_End_Array loop
            Read_SymbolTag (Handler, Value);
            Set (Value) := True;
         end loop;
      end;

      Handler.Read_Next;
   end Read_SymbolTag_Set;

   procedure Read_CompletionItem_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CompletionItem_Vector) is
   begin
      pragma Assert (Handler.Is_Start_Array);
      Handler.Read_Next;

      declare
         Set   : LSP.Structures.CompletionItem_Vector renames Value;
         Value : LSP.Structures.CompletionItem;
      begin
         Set.Clear;
         while not Handler.Is_End_Array loop
            Read_CompletionItem (Handler, Value);
            Set.Append (Value);
         end loop;
      end;

      Handler.Read_Next;
   end Read_CompletionItem_Vector;

   package RelativePattern_Scope is
      package RelativePattern_Map is new Minimal_Perfect_Hash
        (["baseUri",
         "pattern"]);

   end RelativePattern_Scope;

   procedure Read_RelativePattern
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.RelativePattern) is
      use RelativePattern_Scope;
      procedure Read_WorkspaceFolder_Or_URI
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.WorkspaceFolder_Or_URI);

      procedure Read_WorkspaceFolder_Or_URI
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.WorkspaceFolder_Or_URI) is
      begin
         if Handler.Is_String_Value then
            Value :=
              (Is_WorkspaceFolder => False,
               others             => <>);
         else
            Value :=
              (Is_WorkspaceFolder => True,
               others             => <>);
         end if;

         case Value.Is_WorkspaceFolder is
            when True =>
               Read_WorkspaceFolder (Handler, Value.WorkspaceFolder);
            when False =>
               Read_URI (Handler, Value.URI);
         end case;
      end Read_WorkspaceFolder_Or_URI;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case RelativePattern_Map.Get_Index (Key) is
               when 1 =>  --  baseUri
                  Read_WorkspaceFolder_Or_URI (Handler, Value.baseUri);
               when 2 =>  --  pattern
                  Read_Pattern (Handler, Value.pattern);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_RelativePattern;

   procedure Read_Moniker_Vector_Or_Null
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Moniker_Vector_Or_Null) is
   begin
      if Handler.Is_Null_Value then
         Handler.Read_Next;
      else
         Value.Clear;
         Read_Moniker_Vector (Handler, Value);
      end if;
   end Read_Moniker_Vector_Or_Null;

   package WorkDoneProgressReport_Scope is
      package WorkDoneProgressReport_Map is new Minimal_Perfect_Hash
        (["kind",
         "cancellable",
         "message",
         "percentage"]);

   end WorkDoneProgressReport_Scope;

   procedure Read_WorkDoneProgressReport
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.WorkDoneProgressReport) is
      use WorkDoneProgressReport_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case WorkDoneProgressReport_Map.Get_Index (Key) is
               when 1 =>  --  kind
                  Handler.Read_Next;  --  Skip string literal: report
               when 2 =>  --  cancellable
                  Value.cancellable       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.cancellable.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 3 =>  --  message
                  Value.message.Clear;
                  Value.message.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 4 =>  --  percentage
                  Value.percentage       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.percentage.Value :=
                    Integer (Handler.Number_Value.Integer_Value);
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_WorkDoneProgressReport;

   package tagSupport_OfWorkspaceSymbolClientCapabilities_Scope is
      package tagSupport_OfWorkspaceSymbolClientCapabilities_Map is new Minimal_Perfect_Hash
        (["valueSet"]);

   end tagSupport_OfWorkspaceSymbolClientCapabilities_Scope;

   procedure Read_tagSupport_OfWorkspaceSymbolClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures
        .tagSupport_OfWorkspaceSymbolClientCapabilities) is
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while Handler.Is_Key_Name loop
         declare
            use tagSupport_OfWorkspaceSymbolClientCapabilities_Scope;
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case tagSupport_OfWorkspaceSymbolClientCapabilities_Map.Get_Index
              (Key) is
               when 1 =>  --  valueSet
                  Read_SymbolTag_Set (Handler, Value.valueSet);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_tagSupport_OfWorkspaceSymbolClientCapabilities;

   package ReferenceOptions_Scope is
      package ReferenceOptions_Map is new Minimal_Perfect_Hash
        (["workDoneProgress"]);

   end ReferenceOptions_Scope;

   procedure Read_ReferenceOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ReferenceOptions) is
      use ReferenceOptions_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case ReferenceOptions_Map.Get_Index (Key) is
               when 1 =>  --  workDoneProgress
                  Value.workDoneProgress       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.workDoneProgress.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_ReferenceOptions;

   package TextDocumentClientCapabilities_Scope is
      package TextDocumentClientCapabilities_Map is new Minimal_Perfect_Hash
        (["synchronization",
         "completion",
         "hover",
         "signatureHelp",
         "declaration",
         "definition",
         "typeDefinition",
         "implementation",
         "references",
         "documentHighlight",
         "documentSymbol",
         "codeAction",
         "codeLens",
         "documentLink",
         "colorProvider",
         "formatting",
         "rangeFormatting",
         "onTypeFormatting",
         "rename",
         "foldingRange",
         "selectionRange",
         "publishDiagnostics",
         "callHierarchy",
         "semanticTokens",
         "linkedEditingRange",
         "moniker",
         "typeHierarchy",
         "inlineValue",
         "inlayHint",
         "diagnostic"]);

   end TextDocumentClientCapabilities_Scope;

   procedure Read_TextDocumentClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TextDocumentClientCapabilities) is
      use TextDocumentClientCapabilities_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case TextDocumentClientCapabilities_Map.Get_Index (Key) is
               when 1 =>  --  synchronization
                  Value.synchronization :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_TextDocumentSyncClientCapabilities
                    (Handler, Value.synchronization.Value);
               when 2 =>  --  completion
                  Value.completion :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_CompletionClientCapabilities
                    (Handler, Value.completion.Value);
               when 3 =>  --  hover
                  Value.hover :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_HoverClientCapabilities (Handler, Value.hover.Value);
               when 4 =>  --  signatureHelp
                  Value.signatureHelp :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_SignatureHelpClientCapabilities
                    (Handler, Value.signatureHelp.Value);
               when 5 =>  --  declaration
                  Value.declaration :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_DeclarationClientCapabilities
                    (Handler, Value.declaration.Value);
               when 6 =>  --  definition
                  Value.definition :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_DefinitionClientCapabilities
                    (Handler, Value.definition.Value);
               when 7 =>  --  typeDefinition
                  Value.typeDefinition :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_TypeDefinitionClientCapabilities
                    (Handler, Value.typeDefinition.Value);
               when 8 =>  --  implementation
                  Value.implementation :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_ImplementationClientCapabilities
                    (Handler, Value.implementation.Value);
               when 9 =>  --  references
                  Value.references :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_ReferenceClientCapabilities
                    (Handler, Value.references.Value);
               when 10 =>  --  documentHighlight
                  Value.documentHighlight :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_DocumentHighlightClientCapabilities
                    (Handler, Value.documentHighlight.Value);
               when 11 =>  --  documentSymbol
                  Value.documentSymbol :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_DocumentSymbolClientCapabilities
                    (Handler, Value.documentSymbol.Value);
               when 12 =>  --  codeAction
                  Value.codeAction :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_CodeActionClientCapabilities
                    (Handler, Value.codeAction.Value);
               when 13 =>  --  codeLens
                  Value.codeLens :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_CodeLensClientCapabilities
                    (Handler, Value.codeLens.Value);
               when 14 =>  --  documentLink
                  Value.documentLink :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_DocumentLinkClientCapabilities
                    (Handler, Value.documentLink.Value);
               when 15 =>  --  colorProvider
                  Value.colorProvider :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_DocumentColorClientCapabilities
                    (Handler, Value.colorProvider.Value);
               when 16 =>  --  formatting
                  Value.formatting :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_DocumentFormattingClientCapabilities
                    (Handler, Value.formatting.Value);
               when 17 =>  --  rangeFormatting
                  Value.rangeFormatting :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_DocumentRangeFormattingClientCapabilities
                    (Handler, Value.rangeFormatting.Value);
               when 18 =>  --  onTypeFormatting
                  Value.onTypeFormatting :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_DocumentOnTypeFormattingClientCapabilities
                    (Handler, Value.onTypeFormatting.Value);
               when 19 =>  --  rename
                  Value.rename :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_RenameClientCapabilities (Handler, Value.rename.Value);
               when 20 =>  --  foldingRange
                  Value.foldingRange :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_FoldingRangeClientCapabilities
                    (Handler, Value.foldingRange.Value);
               when 21 =>  --  selectionRange
                  Value.selectionRange :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_SelectionRangeClientCapabilities
                    (Handler, Value.selectionRange.Value);
               when 22 =>  --  publishDiagnostics
                  Value.publishDiagnostics :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_PublishDiagnosticsClientCapabilities
                    (Handler, Value.publishDiagnostics.Value);
               when 23 =>  --  callHierarchy
                  Value.callHierarchy :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_CallHierarchyClientCapabilities
                    (Handler, Value.callHierarchy.Value);
               when 24 =>  --  semanticTokens
                  Value.semanticTokens :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_SemanticTokensClientCapabilities
                    (Handler, Value.semanticTokens.Value);
               when 25 =>  --  linkedEditingRange
                  Value.linkedEditingRange :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_LinkedEditingRangeClientCapabilities
                    (Handler, Value.linkedEditingRange.Value);
               when 26 =>  --  moniker
                  Value.moniker :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_MonikerClientCapabilities
                    (Handler, Value.moniker.Value);
               when 27 =>  --  typeHierarchy
                  Value.typeHierarchy :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_TypeHierarchyClientCapabilities
                    (Handler, Value.typeHierarchy.Value);
               when 28 =>  --  inlineValue
                  Value.inlineValue :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_InlineValueClientCapabilities
                    (Handler, Value.inlineValue.Value);
               when 29 =>  --  inlayHint
                  Value.inlayHint :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_InlayHintClientCapabilities
                    (Handler, Value.inlayHint.Value);
               when 30 =>  --  diagnostic
                  Value.diagnostic :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_DiagnosticClientCapabilities
                    (Handler, Value.diagnostic.Value);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_TextDocumentClientCapabilities;

   package WorkspaceSymbolParams_Scope is
      package WorkspaceSymbolParams_Map is new Minimal_Perfect_Hash
        (["workDoneToken",
         "partialResultToken",
         "query"]);

   end WorkspaceSymbolParams_Scope;

   procedure Read_WorkspaceSymbolParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.WorkspaceSymbolParams) is
      use WorkspaceSymbolParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case WorkspaceSymbolParams_Map.Get_Index (Key) is
               when 1 =>  --  workDoneToken
                  Value.workDoneToken :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_ProgressToken (Handler, Value.workDoneToken.Value);
               when 2 =>  --  partialResultToken
                  Value.partialResultToken :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_ProgressToken (Handler, Value.partialResultToken.Value);
               when 3 =>  --  query
                  Value.query.Clear;
                  Value.query.Append (Handler.String_Value);
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_WorkspaceSymbolParams;

   procedure Read_DocumentSymbol_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentSymbol_Vector) is
   begin
      pragma Assert (Handler.Is_Start_Array);
      Handler.Read_Next;

      declare
         Set   : LSP.Structures.DocumentSymbol_Vector renames Value;
         Value : LSP.Structures.DocumentSymbol;
      begin
         Set.Clear;
         while not Handler.Is_End_Array loop
            Read_DocumentSymbol (Handler, Value);
            Set.Append (Value);
         end loop;
      end;

      Handler.Read_Next;
   end Read_DocumentSymbol_Vector;

   procedure Read_GlobPattern
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.GlobPattern) is
   begin
      if Handler.Is_String_Value then
         Value :=
           (Is_Pattern => True,
            others     => <>);
      else
         Value :=
           (Is_Pattern => False,
            others     => <>);
      end if;

      case Value.Is_Pattern is
         when True =>
            Read_Pattern (Handler, Value.Pattern);
         when False =>
            Read_RelativePattern (Handler, Value.RelativePattern);
      end case;
   end Read_GlobPattern;

   package NotebookDocumentIdentifier_Scope is
      package NotebookDocumentIdentifier_Map is new Minimal_Perfect_Hash
        (["uri"]);

   end NotebookDocumentIdentifier_Scope;

   procedure Read_NotebookDocumentIdentifier
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.NotebookDocumentIdentifier) is
      use NotebookDocumentIdentifier_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case NotebookDocumentIdentifier_Map.Get_Index (Key) is
               when 1 =>  --  uri
                  Read_URI (Handler, Value.uri);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_NotebookDocumentIdentifier;

   package InsertReplaceEdit_Scope is
      package InsertReplaceEdit_Map is new Minimal_Perfect_Hash
        (["newText",
         "insert",
         "replace"]);

   end InsertReplaceEdit_Scope;

   procedure Read_InsertReplaceEdit
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.InsertReplaceEdit) is
      use InsertReplaceEdit_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case InsertReplaceEdit_Map.Get_Index (Key) is
               when 1 =>  --  newText
                  Value.newText.Clear;
                  Value.newText.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 2 =>  --  insert
                  Read_A_Range (Handler, Value.insert);
               when 3 =>  --  replace
                  Read_A_Range (Handler, Value.replace);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_InsertReplaceEdit;

   package InitializeResult_Scope is
      package InitializeResult_Map is new Minimal_Perfect_Hash
        (["capabilities",
         "serverInfo"]);

   end InitializeResult_Scope;

   procedure Read_InitializeResult
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.InitializeResult) is
      use InitializeResult_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case InitializeResult_Map.Get_Index (Key) is
               when 1 =>  --  capabilities
                  Read_ServerCapabilities (Handler, Value.capabilities);
               when 2 =>  --  serverInfo
                  Value.serverInfo :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_clientInfo_Of_InitializeParams
                    (Handler, Value.serverInfo.Value);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_InitializeResult;

   package InlineValueVariableLookup_Scope is
      package InlineValueVariableLookup_Map is new Minimal_Perfect_Hash
        (["range",
         "variableName",
         "caseSensitiveLookup"]);

   end InlineValueVariableLookup_Scope;

   procedure Read_InlineValueVariableLookup
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.InlineValueVariableLookup) is
      use InlineValueVariableLookup_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case InlineValueVariableLookup_Map.Get_Index (Key) is
               when 1 =>  --  range
                  Read_A_Range (Handler, Value.a_range);
               when 2 =>  --  variableName
                  Value.variableName.Clear;
                  Value.variableName.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 3 =>  --  caseSensitiveLookup
                  Value.caseSensitiveLookup := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_InlineValueVariableLookup;

   package FileEvent_Scope is
      package FileEvent_Map is new Minimal_Perfect_Hash
        (["uri",
         "type"]);

   end FileEvent_Scope;

   procedure Read_FileEvent
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.FileEvent) is
      use FileEvent_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case FileEvent_Map.Get_Index (Key) is
               when 1 =>  --  uri
                  Value.uri := (Handler.String_Value with null record);
                  Handler.Read_Next;
               when 2 =>  --  type
                  Read_FileChangeType (Handler, Value.a_type);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_FileEvent;

   package SignatureHelpClientCapabilities_Scope is
      package SignatureHelpClientCapabilities_Map is new Minimal_Perfect_Hash
        (["dynamicRegistration",
         "signatureInformation",
         "contextSupport"]);

      package signatureInformation_OfSignatureHelpClientCapabilities_Scope is
         package signatureInformation_OfSignatureHelpClientCapabilities_Map is new Minimal_Perfect_Hash
           (["documentationFormat",
            "parameterInformation",
            "activeParameterSupport"]);

      end signatureInformation_OfSignatureHelpClientCapabilities_Scope;

      package parameterInformation_OfsignatureInformation_OfSignatureHelpClientCapabilities_Scope
      is
         package parameterInformation_OfsignatureInformation_OfSignatureHelpClientCapabilities_Map is new Minimal_Perfect_Hash
           (["labelOffsetSupport"]);

      end parameterInformation_OfsignatureInformation_OfSignatureHelpClientCapabilities_Scope;

   end SignatureHelpClientCapabilities_Scope;

   procedure Read_SignatureHelpClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SignatureHelpClientCapabilities) is
      use SignatureHelpClientCapabilities_Scope;
      procedure Read_signatureInformation_OfSignatureHelpClientCapabilities
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .signatureInformation_OfSignatureHelpClientCapabilities);

      procedure Read_parameterInformation_OfsignatureInformation_OfSignatureHelpClientCapabilities
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .parameterInformation_OfsignatureInformation_OfSignatureHelpClientCapabilities);

      procedure Read_signatureInformation_OfSignatureHelpClientCapabilities
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .signatureInformation_OfSignatureHelpClientCapabilities) is
      begin
         pragma Assert (Handler.Is_Start_Object);
         Handler.Read_Next;

         while Handler.Is_Key_Name loop
            declare
               use
                 signatureInformation_OfSignatureHelpClientCapabilities_Scope;
               Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
            begin
               Handler.Read_Next;
               case signatureInformation_OfSignatureHelpClientCapabilities_Map
                 .Get_Index
                 (Key) is
                  when 1 =>  --  documentationFormat
                     Read_MarkupKind_Vector
                       (Handler, Value.documentationFormat);
                  when 2 =>  --  parameterInformation
                     Value.parameterInformation :=
                       (Is_Set => True,
                        Value  => <>);
                     Read_parameterInformation_OfsignatureInformation_OfSignatureHelpClientCapabilities
                       (Handler, Value.parameterInformation.Value);
                  when 3 =>  --  activeParameterSupport
                     Value.activeParameterSupport       :=
                       (Is_Set => True,
                        Value  => <>);
                     Value.activeParameterSupport.Value :=
                       Handler.Boolean_Value;
                     Handler.Read_Next;
                  when others =>
                     Handler.Skip_Current_Value;
               end case;
            end;
         end loop;

         Handler.Read_Next;
      end Read_signatureInformation_OfSignatureHelpClientCapabilities;

      procedure Read_parameterInformation_OfsignatureInformation_OfSignatureHelpClientCapabilities
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .parameterInformation_OfsignatureInformation_OfSignatureHelpClientCapabilities) is
      begin
         pragma Assert (Handler.Is_Start_Object);
         Handler.Read_Next;

         while Handler.Is_Key_Name loop
            declare
               use
                 parameterInformation_OfsignatureInformation_OfSignatureHelpClientCapabilities_Scope;
               Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
            begin
               Handler.Read_Next;
               case parameterInformation_OfsignatureInformation_OfSignatureHelpClientCapabilities_Map
                 .Get_Index
                 (Key) is
                  when 1 =>  --  labelOffsetSupport
                     Value.labelOffsetSupport       :=
                       (Is_Set => True,
                        Value  => <>);
                     Value.labelOffsetSupport.Value := Handler.Boolean_Value;
                     Handler.Read_Next;
                  when others =>
                     Handler.Skip_Current_Value;
               end case;
            end;
         end loop;

         Handler.Read_Next;
      end Read_parameterInformation_OfsignatureInformation_OfSignatureHelpClientCapabilities;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case SignatureHelpClientCapabilities_Map.Get_Index (Key) is
               when 1 =>  --  dynamicRegistration
                  Value.dynamicRegistration       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.dynamicRegistration.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 2 =>  --  signatureInformation
                  Value.signatureInformation :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_signatureInformation_OfSignatureHelpClientCapabilities
                    (Handler, Value.signatureInformation.Value);
               when 3 =>  --  contextSupport
                  Value.contextSupport       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.contextSupport.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_SignatureHelpClientCapabilities;

   package WorkDoneProgressCancelParams_Scope is
      package WorkDoneProgressCancelParams_Map is new Minimal_Perfect_Hash
        (["token"]);

   end WorkDoneProgressCancelParams_Scope;

   procedure Read_WorkDoneProgressCancelParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.WorkDoneProgressCancelParams) is
      use WorkDoneProgressCancelParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case WorkDoneProgressCancelParams_Map.Get_Index (Key) is
               when 1 =>  --  token
                  Read_ProgressToken (Handler, Value.token);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_WorkDoneProgressCancelParams;

   package NotebookDocumentSyncClientCapabilities_Scope is
      package NotebookDocumentSyncClientCapabilities_Map is new Minimal_Perfect_Hash
        (["dynamicRegistration",
         "executionSummarySupport"]);

   end NotebookDocumentSyncClientCapabilities_Scope;

   procedure Read_NotebookDocumentSyncClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.NotebookDocumentSyncClientCapabilities) is
      use NotebookDocumentSyncClientCapabilities_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case NotebookDocumentSyncClientCapabilities_Map.Get_Index (Key) is
               when 1 =>  --  dynamicRegistration
                  Value.dynamicRegistration       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.dynamicRegistration.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 2 =>  --  executionSummarySupport
                  Value.executionSummarySupport       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.executionSummarySupport.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_NotebookDocumentSyncClientCapabilities;

   procedure Read_ProgressToken
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ProgressToken) is
   begin
      if Handler.Is_Number_Value then
         Value :=
           (Is_Integer => True,
            others     => <>);
      else
         Value :=
           (Is_Integer => False,
            others     => <>);
      end if;

      case Value.Is_Integer is
         when True =>
            Value.Integer := Integer (Handler.Number_Value.Integer_Value);
            Handler.Read_Next;
         when False =>
            Value.Virtual_String.Clear;
            Value.Virtual_String.Append (Handler.String_Value);
            Handler.Read_Next;
      end case;
   end Read_ProgressToken;

   package DiagnosticOptions_Scope is
      package DiagnosticOptions_Map is new Minimal_Perfect_Hash
        (["workDoneProgress",
         "identifier",
         "interFileDependencies",
         "workspaceDiagnostics"]);

   end DiagnosticOptions_Scope;

   procedure Read_DiagnosticOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DiagnosticOptions) is
      use DiagnosticOptions_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case DiagnosticOptions_Map.Get_Index (Key) is
               when 1 =>  --  workDoneProgress
                  Value.workDoneProgress       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.workDoneProgress.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 2 =>  --  identifier
                  Value.identifier.Clear;
                  Value.identifier.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 3 =>  --  interFileDependencies
                  Value.interFileDependencies := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 4 =>  --  workspaceDiagnostics
                  Value.workspaceDiagnostics := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_DiagnosticOptions;

   package WindowClientCapabilities_Scope is
      package WindowClientCapabilities_Map is new Minimal_Perfect_Hash
        (["workDoneProgress",
         "showMessage",
         "showDocument"]);

   end WindowClientCapabilities_Scope;

   procedure Read_WindowClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.WindowClientCapabilities) is
      use WindowClientCapabilities_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case WindowClientCapabilities_Map.Get_Index (Key) is
               when 1 =>  --  workDoneProgress
                  Value.workDoneProgress       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.workDoneProgress.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 2 =>  --  showMessage
                  Value.showMessage :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_ShowMessageRequestClientCapabilities
                    (Handler, Value.showMessage.Value);
               when 3 =>  --  showDocument
                  Value.showDocument :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_ShowDocumentClientCapabilities
                    (Handler, Value.showDocument.Value);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_WindowClientCapabilities;

   package GeneralClientCapabilities_Scope is
      package GeneralClientCapabilities_Map is new Minimal_Perfect_Hash
        (["staleRequestSupport",
         "regularExpressions",
         "markdown",
         "positionEncodings"]);

      package staleRequestSupport_OfGeneralClientCapabilities_Scope is
         package staleRequestSupport_OfGeneralClientCapabilities_Map is new Minimal_Perfect_Hash
           (["cancel",
            "retryOnContentModified"]);

      end staleRequestSupport_OfGeneralClientCapabilities_Scope;

   end GeneralClientCapabilities_Scope;

   procedure Read_GeneralClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.GeneralClientCapabilities) is
      use GeneralClientCapabilities_Scope;
      procedure Read_PositionEncodingKind_Set
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.PositionEncodingKind_Set);

      procedure Read_staleRequestSupport_OfGeneralClientCapabilities
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .staleRequestSupport_OfGeneralClientCapabilities);

      procedure Read_PositionEncodingKind_Set
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.PositionEncodingKind_Set) is
      begin
         pragma Assert (Handler.Is_Start_Array);
         Handler.Read_Next;

         declare
            Set   : LSP.Structures.PositionEncodingKind_Set renames Value;
            Value : LSP.Enumerations.PositionEncodingKind;
         begin
            Set := (others => False);
            while not Handler.Is_End_Array loop
               Read_PositionEncodingKind (Handler, Value);
               Set (Value) := True;
            end loop;
         end;

         Handler.Read_Next;
      end Read_PositionEncodingKind_Set;

      procedure Read_staleRequestSupport_OfGeneralClientCapabilities
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .staleRequestSupport_OfGeneralClientCapabilities) is
      begin
         pragma Assert (Handler.Is_Start_Object);
         Handler.Read_Next;

         while Handler.Is_Key_Name loop
            declare
               use staleRequestSupport_OfGeneralClientCapabilities_Scope;
               Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
            begin
               Handler.Read_Next;
               case staleRequestSupport_OfGeneralClientCapabilities_Map
                 .Get_Index
                 (Key) is
                  when 1 =>  --  cancel
                     Value.cancel := Handler.Boolean_Value;
                     Handler.Read_Next;
                  when 2 =>  --  retryOnContentModified
                     Read_Virtual_String_Vector
                       (Handler, Value.retryOnContentModified);
                  when others =>
                     Handler.Skip_Current_Value;
               end case;
            end;
         end loop;

         Handler.Read_Next;
      end Read_staleRequestSupport_OfGeneralClientCapabilities;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case GeneralClientCapabilities_Map.Get_Index (Key) is
               when 1 =>  --  staleRequestSupport
                  Value.staleRequestSupport :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_staleRequestSupport_OfGeneralClientCapabilities
                    (Handler, Value.staleRequestSupport.Value);
               when 2 =>  --  regularExpressions
                  Value.regularExpressions :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_RegularExpressionsClientCapabilities
                    (Handler, Value.regularExpressions.Value);
               when 3 =>  --  markdown
                  Value.markdown :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_MarkdownClientCapabilities
                    (Handler, Value.markdown.Value);
               when 4 =>  --  positionEncodings
                  Read_PositionEncodingKind_Set
                    (Handler, Value.positionEncodings);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_GeneralClientCapabilities;

   package SemanticTokensWorkspaceClientCapabilities_Scope is
      package SemanticTokensWorkspaceClientCapabilities_Map is new Minimal_Perfect_Hash
        (["refreshSupport"]);

   end SemanticTokensWorkspaceClientCapabilities_Scope;

   procedure Read_SemanticTokensWorkspaceClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value : out LSP.Structures.SemanticTokensWorkspaceClientCapabilities) is
      use SemanticTokensWorkspaceClientCapabilities_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case SemanticTokensWorkspaceClientCapabilities_Map.Get_Index
              (Key) is
               when 1 =>  --  refreshSupport
                  Value.refreshSupport       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.refreshSupport.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_SemanticTokensWorkspaceClientCapabilities;

   package LinkedEditingRangeClientCapabilities_Scope is
      package LinkedEditingRangeClientCapabilities_Map is new Minimal_Perfect_Hash
        (["dynamicRegistration"]);

   end LinkedEditingRangeClientCapabilities_Scope;

   procedure Read_LinkedEditingRangeClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.LinkedEditingRangeClientCapabilities) is
      use LinkedEditingRangeClientCapabilities_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case LinkedEditingRangeClientCapabilities_Map.Get_Index (Key) is
               when 1 =>  --  dynamicRegistration
                  Value.dynamicRegistration       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.dynamicRegistration.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_LinkedEditingRangeClientCapabilities;

   package SignatureHelp_Scope is
      package SignatureHelp_Map is new Minimal_Perfect_Hash
        (["signatures",
         "activeSignature",
         "activeParameter"]);

   end SignatureHelp_Scope;

   procedure Read_SignatureHelp
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SignatureHelp) is
      use SignatureHelp_Scope;
      procedure Read_SignatureInformation_Vector
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.SignatureInformation_Vector);

      procedure Read_SignatureInformation_Vector
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.SignatureInformation_Vector) is
      begin
         pragma Assert (Handler.Is_Start_Array);
         Handler.Read_Next;

         declare
            Set   : LSP.Structures.SignatureInformation_Vector renames Value;
            Value : LSP.Structures.SignatureInformation;
         begin
            Set.Clear;
            while not Handler.Is_End_Array loop
               Read_SignatureInformation (Handler, Value);
               Set.Append (Value);
            end loop;
         end;

         Handler.Read_Next;
      end Read_SignatureInformation_Vector;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case SignatureHelp_Map.Get_Index (Key) is
               when 1 =>  --  signatures
                  Read_SignatureInformation_Vector (Handler, Value.signatures);
               when 2 =>  --  activeSignature
                  Value.activeSignature       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.activeSignature.Value :=
                    Integer (Handler.Number_Value.Integer_Value);
                  Handler.Read_Next;
               when 3 =>  --  activeParameter
                  Value.activeParameter       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.activeParameter.Value :=
                    Integer (Handler.Number_Value.Integer_Value);
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_SignatureHelp;

   package DidChangeWatchedFilesParams_Scope is
      package DidChangeWatchedFilesParams_Map is new Minimal_Perfect_Hash
        (["changes"]);

   end DidChangeWatchedFilesParams_Scope;

   procedure Read_DidChangeWatchedFilesParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DidChangeWatchedFilesParams) is
      use DidChangeWatchedFilesParams_Scope;
      procedure Read_FileEvent_Vector
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.FileEvent_Vector);

      procedure Read_FileEvent_Vector
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.FileEvent_Vector) is
      begin
         pragma Assert (Handler.Is_Start_Array);
         Handler.Read_Next;

         declare
            Set   : LSP.Structures.FileEvent_Vector renames Value;
            Value : LSP.Structures.FileEvent;
         begin
            Set.Clear;
            while not Handler.Is_End_Array loop
               Read_FileEvent (Handler, Value);
               Set.Append (Value);
            end loop;
         end;

         Handler.Read_Next;
      end Read_FileEvent_Vector;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case DidChangeWatchedFilesParams_Map.Get_Index (Key) is
               when 1 =>  --  changes
                  Read_FileEvent_Vector (Handler, Value.changes);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_DidChangeWatchedFilesParams;

   package DidOpenNotebookDocumentParams_Scope is
      package DidOpenNotebookDocumentParams_Map is new Minimal_Perfect_Hash
        (["notebookDocument",
         "cellTextDocuments"]);

   end DidOpenNotebookDocumentParams_Scope;

   procedure Read_DidOpenNotebookDocumentParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DidOpenNotebookDocumentParams) is
      use DidOpenNotebookDocumentParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case DidOpenNotebookDocumentParams_Map.Get_Index (Key) is
               when 1 =>  --  notebookDocument
                  Read_NotebookDocument (Handler, Value.notebookDocument);
               when 2 =>  --  cellTextDocuments
                  Read_TextDocumentItem_Vector
                    (Handler, Value.cellTextDocuments);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_DidOpenNotebookDocumentParams;

   procedure Read_SignatureHelp_Or_Null
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SignatureHelp_Or_Null) is
   begin
      if Handler.Is_Null_Value then
         Handler.Read_Next;
      else
         Value :=
           (Is_Null => False,
            Value   => <>);
         Read_SignatureHelp (Handler, Value.Value);
      end if;
   end Read_SignatureHelp_Or_Null;

   procedure Read_Diagnostic_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Diagnostic_Vector) is
   begin
      pragma Assert (Handler.Is_Start_Array);
      Handler.Read_Next;

      declare
         Set   : LSP.Structures.Diagnostic_Vector renames Value;
         Value : LSP.Structures.Diagnostic;
      begin
         Set.Clear;
         while not Handler.Is_End_Array loop
            Read_Diagnostic (Handler, Value);
            Set.Append (Value);
         end loop;
      end;

      Handler.Read_Next;
   end Read_Diagnostic_Vector;

   procedure Read_DocumentHighlightKind
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.DocumentHighlightKind) is
   begin
      Value :=
        LSP.Enumerations.DocumentHighlightKind'Val
          (Handler.Number_Value.Integer_Value - 1);
      Handler.Read_Next;
   end Read_DocumentHighlightKind;

   procedure Read_CodeLens_Vector_Or_Null
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CodeLens_Vector_Or_Null) is
   begin
      if Handler.Is_Null_Value then
         Handler.Read_Next;
      else
         Value.Clear;
         Read_CodeLens_Vector (Handler, Value);
      end if;
   end Read_CodeLens_Vector_Or_Null;

   package ResourceOperationKind_Map is new Minimal_Perfect_Hash
     (["create",
      "rename",
      "delete"]);

   procedure Read_ResourceOperationKind
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.ResourceOperationKind) is
   begin
      Value :=
        LSP.Enumerations.ResourceOperationKind'Val
          (ResourceOperationKind_Map.Get_Index (Handler.String_Value) - 1);
      Handler.Read_Next;
   end Read_ResourceOperationKind;

   procedure Read_WorkspaceFolder_Vector_Or_Null
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.WorkspaceFolder_Vector_Or_Null) is
   begin
      if Handler.Is_Null_Value then
         Handler.Read_Next;
      else
         Value.Clear;
         Read_WorkspaceFolder_Vector (Handler, Value);
      end if;
   end Read_WorkspaceFolder_Vector_Or_Null;

   package InlineValueClientCapabilities_Scope is
      package InlineValueClientCapabilities_Map is new Minimal_Perfect_Hash
        (["dynamicRegistration"]);

   end InlineValueClientCapabilities_Scope;

   procedure Read_InlineValueClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.InlineValueClientCapabilities) is
      use InlineValueClientCapabilities_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case InlineValueClientCapabilities_Map.Get_Index (Key) is
               when 1 =>  --  dynamicRegistration
                  Value.dynamicRegistration       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.dynamicRegistration.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_InlineValueClientCapabilities;

   package PrepareRenameResult_Scope is
      package PrepareRenameResult_Map is new Minimal_Perfect_Hash
        (["start",
         "end",
         "range",
         "placeholder",
         "defaultBehavior"]);

      package PrepareRenameResult_2_Scope is
         package PrepareRenameResult_2_Map is new Minimal_Perfect_Hash
           (["range",
            "placeholder"]);

      end PrepareRenameResult_2_Scope;

      package PrepareRenameResult_3_Scope is
         package PrepareRenameResult_3_Map is new Minimal_Perfect_Hash
           (["defaultBehavior"]);

      end PrepareRenameResult_3_Scope;

   end PrepareRenameResult_Scope;

   procedure Read_PrepareRenameResult
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.PrepareRenameResult) is
      use PrepareRenameResult_Scope;
      procedure Read_PrepareRenameResult_2
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.PrepareRenameResult_2);

      procedure Read_PrepareRenameResult_3
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.PrepareRenameResult_3);

      procedure Read_PrepareRenameResult_2
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.PrepareRenameResult_2) is
      begin
         pragma Assert (Handler.Is_Start_Object);
         Handler.Read_Next;

         while Handler.Is_Key_Name loop
            declare
               use PrepareRenameResult_2_Scope;
               Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
            begin
               Handler.Read_Next;
               case PrepareRenameResult_2_Map.Get_Index (Key) is
                  when 1 =>  --  range
                     Read_A_Range (Handler, Value.a_range);
                  when 2 =>  --  placeholder
                     Value.placeholder.Clear;
                     Value.placeholder.Append (Handler.String_Value);
                     Handler.Read_Next;
                  when others =>
                     Handler.Skip_Current_Value;
               end case;
            end;
         end loop;

         Handler.Read_Next;
      end Read_PrepareRenameResult_2;

      procedure Read_PrepareRenameResult_3
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.PrepareRenameResult_3) is
      begin
         pragma Assert (Handler.Is_Start_Object);
         Handler.Read_Next;

         while Handler.Is_Key_Name loop
            declare
               use PrepareRenameResult_3_Scope;
               Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
            begin
               Handler.Read_Next;
               case PrepareRenameResult_3_Map.Get_Index (Key) is
                  when 1 =>  --  defaultBehavior
                     Value.defaultBehavior := Handler.Boolean_Value;
                     Handler.Read_Next;
                  when others =>
                     Handler.Skip_Current_Value;
               end case;
            end;
         end loop;

         Handler.Read_Next;
      end Read_PrepareRenameResult_3;

   begin
      declare
         Parent  :
           constant not null access VSS.JSON.Pull_Readers.JSON_Pull_Reader'
             Class :=
           Handler'Access;
         Handler :
           VSS.JSON.Pull_Readers.Buffered.JSON_Buffered_Pull_Reader (Parent);
      begin
         Handler.Mark;
         if Handler.Is_Start_Object then
            Handler.Read_Next;
            while Handler.Is_Key_Name loop
               declare
                  Key   : constant VSS.Strings.Virtual_String :=
                    Handler.Key_Name;
                  Index : constant Natural                    :=
                    PrepareRenameResult_Map.Get_Index (Key);
               begin
                  Handler.Read_Next;
                  case Index is
                     when 1 =>  --  start
                        Value :=
                          (Kind   => LSP.Structures.Variant_1,
                           others => <>);
                        exit;
                     when 2 =>  --  end
                        Value :=
                          (Kind   => LSP.Structures.Variant_1,
                           others => <>);
                        exit;
                     when 3 =>  --  range
                        Value :=
                          (Kind   => LSP.Structures.Variant_2,
                           others => <>);
                        exit;
                     when 4 =>  --  placeholder
                        Value :=
                          (Kind   => LSP.Structures.Variant_2,
                           others => <>);
                        exit;
                     when 5 =>  --  defaultBehavior
                        Value :=
                          (Kind   => LSP.Structures.Variant_3,
                           others => <>);
                        exit;
                     when others =>
                        Handler.Skip_Current_Value;
                  end case;
               end;
            end loop;
         else
            raise Program_Error;  --  Unexpected JSON value
         end if;

         Handler.Reset;
         Handler.Unmark;

         case Value.Kind is
            when LSP.Structures.Variant_1 =>
               Read_A_Range (Handler, Value.Variant_1);
            when LSP.Structures.Variant_2 =>
               Read_PrepareRenameResult_2 (Handler, Value.Variant_2);
            when LSP.Structures.Variant_3 =>
               Read_PrepareRenameResult_3 (Handler, Value.Variant_3);
         end case;
      end;
   end Read_PrepareRenameResult;

   package DocumentLinkParams_Scope is
      package DocumentLinkParams_Map is new Minimal_Perfect_Hash
        (["workDoneToken",
         "partialResultToken",
         "textDocument"]);

   end DocumentLinkParams_Scope;

   procedure Read_DocumentLinkParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentLinkParams) is
      use DocumentLinkParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case DocumentLinkParams_Map.Get_Index (Key) is
               when 1 =>  --  workDoneToken
                  Value.workDoneToken :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_ProgressToken (Handler, Value.workDoneToken.Value);
               when 2 =>  --  partialResultToken
                  Value.partialResultToken :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_ProgressToken (Handler, Value.partialResultToken.Value);
               when 3 =>  --  textDocument
                  Read_TextDocumentIdentifier (Handler, Value.textDocument);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_DocumentLinkParams;

   package DocumentLinkClientCapabilities_Scope is
      package DocumentLinkClientCapabilities_Map is new Minimal_Perfect_Hash
        (["dynamicRegistration",
         "tooltipSupport"]);

   end DocumentLinkClientCapabilities_Scope;

   procedure Read_DocumentLinkClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentLinkClientCapabilities) is
      use DocumentLinkClientCapabilities_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case DocumentLinkClientCapabilities_Map.Get_Index (Key) is
               when 1 =>  --  dynamicRegistration
                  Value.dynamicRegistration       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.dynamicRegistration.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 2 =>  --  tooltipSupport
                  Value.tooltipSupport       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.tooltipSupport.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_DocumentLinkClientCapabilities;

   procedure Read_MarkupKind_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.MarkupKind_Vector) is
   begin
      pragma Assert (Handler.Is_Start_Array);
      Handler.Read_Next;

      declare
         Set   : LSP.Structures.MarkupKind_Vector renames Value;
         Value : LSP.Enumerations.MarkupKind;
      begin
         Set.Clear;
         while not Handler.Is_End_Array loop
            Read_MarkupKind (Handler, Value);
            Set.Append (Value);
         end loop;
      end;

      Handler.Read_Next;
   end Read_MarkupKind_Vector;

   package CodeActionContext_Scope is
      package CodeActionContext_Map is new Minimal_Perfect_Hash
        (["diagnostics",
         "only",
         "triggerKind"]);

   end CodeActionContext_Scope;

   procedure Read_CodeActionContext
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CodeActionContext) is
      use CodeActionContext_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case CodeActionContext_Map.Get_Index (Key) is
               when 1 =>  --  diagnostics
                  Read_Diagnostic_Vector (Handler, Value.diagnostics);
               when 2 =>  --  only
                  Read_CodeActionKind_Set (Handler, Value.only);
               when 3 =>  --  triggerKind
                  Value.triggerKind :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_CodeActionTriggerKind
                    (Handler, Value.triggerKind.Value);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_CodeActionContext;

   package ConfigurationParams_Scope is
      package ConfigurationParams_Map is new Minimal_Perfect_Hash (["items"]);

   end ConfigurationParams_Scope;

   procedure Read_ConfigurationParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ConfigurationParams) is
      use ConfigurationParams_Scope;
      procedure Read_ConfigurationItem_Vector
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.ConfigurationItem_Vector);

      procedure Read_ConfigurationItem_Vector
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.ConfigurationItem_Vector) is
      begin
         pragma Assert (Handler.Is_Start_Array);
         Handler.Read_Next;

         declare
            Set   : LSP.Structures.ConfigurationItem_Vector renames Value;
            Value : LSP.Structures.ConfigurationItem;
         begin
            Set.Clear;
            while not Handler.Is_End_Array loop
               Read_ConfigurationItem (Handler, Value);
               Set.Append (Value);
            end loop;
         end;

         Handler.Read_Next;
      end Read_ConfigurationItem_Vector;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case ConfigurationParams_Map.Get_Index (Key) is
               when 1 =>  --  items
                  Read_ConfigurationItem_Vector (Handler, Value.items);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_ConfigurationParams;

   package DidChangeTextDocumentParams_Scope is
      package DidChangeTextDocumentParams_Map is new Minimal_Perfect_Hash
        (["textDocument",
         "contentChanges"]);

   end DidChangeTextDocumentParams_Scope;

   procedure Read_DidChangeTextDocumentParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DidChangeTextDocumentParams) is
      use DidChangeTextDocumentParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case DidChangeTextDocumentParams_Map.Get_Index (Key) is
               when 1 =>  --  textDocument
                  Read_VersionedTextDocumentIdentifier
                    (Handler, Value.textDocument);
               when 2 =>  --  contentChanges
                  Read_TextDocumentContentChangeEvent_Vector
                    (Handler, Value.contentChanges);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_DidChangeTextDocumentParams;

   package InlayHintRegistrationOptions_Scope is
      package InlayHintRegistrationOptions_Map is new Minimal_Perfect_Hash
        (["workDoneProgress",
         "resolveProvider",
         "documentSelector",
         "id"]);

   end InlayHintRegistrationOptions_Scope;

   procedure Read_InlayHintRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.InlayHintRegistrationOptions) is
      use InlayHintRegistrationOptions_Scope;
      procedure Read_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.DocumentSelector_Or_Null);

      procedure Read_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.DocumentSelector_Or_Null) is
      begin
         if Handler.Is_Null_Value then
            Handler.Read_Next;
         else
            Value :=
              (Is_Null => False,
               Value   => <>);
            Read_DocumentSelector (Handler, Value.Value);
         end if;
      end Read_DocumentSelector_Or_Null;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case InlayHintRegistrationOptions_Map.Get_Index (Key) is
               when 1 =>  --  workDoneProgress
                  Value.workDoneProgress       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.workDoneProgress.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 2 =>  --  resolveProvider
                  Value.resolveProvider       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.resolveProvider.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 3 =>  --  documentSelector
                  Read_DocumentSelector_Or_Null
                    (Handler, Value.Parent.documentSelector);
               when 4 =>  --  id
                  Value.id.Clear;
                  Value.id.Append (Handler.String_Value);
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_InlayHintRegistrationOptions;

   package Declaration_Progress_Report_Scope is
      package Declaration_Progress_Report_Map is new Minimal_Perfect_Hash
        (["uri",
         "range",
         "originSelectionRange",
         "targetUri",
         "targetRange",
         "targetSelectionRange"]);

   end Declaration_Progress_Report_Scope;

   procedure Read_Declaration_Progress_Report
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Declaration_Progress_Report) is
      use Declaration_Progress_Report_Scope;
   begin
      declare
         Parent  :
           constant not null access VSS.JSON.Pull_Readers.JSON_Pull_Reader'
             Class :=
           Handler'Access;
         Handler :
           VSS.JSON.Pull_Readers.Buffered.JSON_Buffered_Pull_Reader (Parent);
      begin
         Handler.Mark;
         if Handler.Is_Start_Array then
            Handler.Read_Next;
         end if;
         if Handler.Is_Start_Object then
            Handler.Read_Next;
            while Handler.Is_Key_Name loop
               declare
                  Key   : constant VSS.Strings.Virtual_String :=
                    Handler.Key_Name;
                  Index : constant Natural                    :=
                    Declaration_Progress_Report_Map.Get_Index (Key);
               begin
                  Handler.Read_Next;
                  case Index is
                     when 1 =>  --  uri
                        Value :=
                          (Kind   => LSP.Structures.Variant_1,
                           others => <>);
                        exit;
                     when 2 =>  --  range
                        Value :=
                          (Kind   => LSP.Structures.Variant_1,
                           others => <>);
                        exit;
                     when 3 =>  --  originSelectionRange
                        Value :=
                          (Kind   => LSP.Structures.Variant_2,
                           others => <>);
                        exit;
                     when 4 =>  --  targetUri
                        Value :=
                          (Kind   => LSP.Structures.Variant_2,
                           others => <>);
                        exit;
                     when 5 =>  --  targetRange
                        Value :=
                          (Kind   => LSP.Structures.Variant_2,
                           others => <>);
                        exit;
                     when 6 =>  --  targetSelectionRange
                        Value :=
                          (Kind   => LSP.Structures.Variant_2,
                           others => <>);
                        exit;
                     when others =>
                        Handler.Skip_Current_Value;
                  end case;
               end;
            end loop;
         else
            raise Program_Error;  --  Unexpected JSON value
         end if;

         Handler.Reset;
         Handler.Unmark;

         case Value.Kind is
            when LSP.Structures.Variant_1 =>
               Read_Location_Vector (Handler, Value.Variant_1);
            when LSP.Structures.Variant_2 =>
               Read_DeclarationLink_Vector (Handler, Value.Variant_2);
         end case;
      end;
   end Read_Declaration_Progress_Report;

   procedure Read_Natural_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Natural_Vector) is
   begin
      pragma Assert (Handler.Is_Start_Array);
      Handler.Read_Next;

      declare
         Set   : LSP.Structures.Natural_Vector renames Value;
         Value : Natural;
      begin
         Set.Clear;
         while not Handler.Is_End_Array loop
            Value := Integer (Handler.Number_Value.Integer_Value);
            Handler.Read_Next;
            Set.Append (Value);
         end loop;
      end;

      Handler.Read_Next;
   end Read_Natural_Vector;

   package CodeDescription_Scope is
      package CodeDescription_Map is new Minimal_Perfect_Hash (["href"]);

   end CodeDescription_Scope;

   procedure Read_CodeDescription
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CodeDescription) is
      use CodeDescription_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case CodeDescription_Map.Get_Index (Key) is
               when 1 =>  --  href
                  Read_URI (Handler, Value.href);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_CodeDescription;

   procedure Read_Virtual_String_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Virtual_String_Vector) is
   begin
      pragma Assert (Handler.Is_Start_Array);
      Handler.Read_Next;

      declare
         Set   : LSP.Structures.Virtual_String_Vector renames Value;
         Value : LSP.Structures.Virtual_String;
      begin
         Set.Clear;
         while not Handler.Is_End_Array loop
            Value.Clear;
            Value.Append (Handler.String_Value);
            Handler.Read_Next;
            Set.Append (Value);
         end loop;
      end;

      Handler.Read_Next;
   end Read_Virtual_String_Vector;

   procedure Read_LSPObject
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.LSPObject) is
   begin
      pragma Assert (Handler.Is_Start_Object);
      LSP.Input_Tools.Read_LSPAny_Class (Handler, Value);

   end Read_LSPObject;

   package TextDocumentPositionParams_Scope is
      package TextDocumentPositionParams_Map is new Minimal_Perfect_Hash
        (["textDocument",
         "position"]);

   end TextDocumentPositionParams_Scope;

   procedure Read_TextDocumentPositionParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TextDocumentPositionParams) is
      use TextDocumentPositionParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case TextDocumentPositionParams_Map.Get_Index (Key) is
               when 1 =>  --  textDocument
                  Read_TextDocumentIdentifier (Handler, Value.textDocument);
               when 2 =>  --  position
                  Read_Position (Handler, Value.position);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_TextDocumentPositionParams;

   package DidSaveTextDocumentParams_Scope is
      package DidSaveTextDocumentParams_Map is new Minimal_Perfect_Hash
        (["textDocument",
         "text"]);

   end DidSaveTextDocumentParams_Scope;

   procedure Read_DidSaveTextDocumentParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DidSaveTextDocumentParams) is
      use DidSaveTextDocumentParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case DidSaveTextDocumentParams_Map.Get_Index (Key) is
               when 1 =>  --  textDocument
                  Read_TextDocumentIdentifier (Handler, Value.textDocument);
               when 2 =>  --  text
                  Value.text.Clear;
                  Value.text.Append (Handler.String_Value);
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_DidSaveTextDocumentParams;

   package CodeLensRegistrationOptions_Scope is
      package CodeLensRegistrationOptions_Map is new Minimal_Perfect_Hash
        (["documentSelector",
         "workDoneProgress",
         "resolveProvider"]);

   end CodeLensRegistrationOptions_Scope;

   procedure Read_CodeLensRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CodeLensRegistrationOptions) is
      use CodeLensRegistrationOptions_Scope;
      procedure Read_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.DocumentSelector_Or_Null);

      procedure Read_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.DocumentSelector_Or_Null) is
      begin
         if Handler.Is_Null_Value then
            Handler.Read_Next;
         else
            Value :=
              (Is_Null => False,
               Value   => <>);
            Read_DocumentSelector (Handler, Value.Value);
         end if;
      end Read_DocumentSelector_Or_Null;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case CodeLensRegistrationOptions_Map.Get_Index (Key) is
               when 1 =>  --  documentSelector
                  Read_DocumentSelector_Or_Null
                    (Handler, Value.Parent.documentSelector);
               when 2 =>  --  workDoneProgress
                  Value.workDoneProgress       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.workDoneProgress.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 3 =>  --  resolveProvider
                  Value.resolveProvider       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.resolveProvider.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_CodeLensRegistrationOptions;

   package SelectionRange_Scope is
      package SelectionRange_Map is new Minimal_Perfect_Hash
        (["range",
         "parent"]);

   end SelectionRange_Scope;

   procedure Read_SelectionRange
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SelectionRange) is
      use SelectionRange_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case SelectionRange_Map.Get_Index (Key) is
               when 1 =>  --  range
                  Read_A_Range (Handler, Value.a_range);
               when 2 =>  --  parent
                  declare
                     Value_parent : LSP.Structures.SelectionRange;
                  begin
                     Read_SelectionRange (Handler, Value_parent);
                     Value.parent.Set (Value_parent);
                  end;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_SelectionRange;

   package NotebookCellTextDocumentFilter_Scope is
      package NotebookCellTextDocumentFilter_Map is new Minimal_Perfect_Hash
        (["notebook",
         "language"]);

   end NotebookCellTextDocumentFilter_Scope;

   procedure Read_NotebookCellTextDocumentFilter
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.NotebookCellTextDocumentFilter) is
      use NotebookCellTextDocumentFilter_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case NotebookCellTextDocumentFilter_Map.Get_Index (Key) is
               when 1 =>  --  notebook
                  Read_Virtual_String_Or_NotebookDocumentFilter
                    (Handler, Value.notebook);
               when 2 =>  --  language
                  Value.language.Clear;
                  Value.language.Append (Handler.String_Value);
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_NotebookCellTextDocumentFilter;

   package WorkspaceFoldersInitializeParams_Scope is
      package WorkspaceFoldersInitializeParams_Map is new Minimal_Perfect_Hash
        (["workspaceFolders"]);

   end WorkspaceFoldersInitializeParams_Scope;

   procedure Read_WorkspaceFoldersInitializeParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.WorkspaceFoldersInitializeParams) is
      use WorkspaceFoldersInitializeParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case WorkspaceFoldersInitializeParams_Map.Get_Index (Key) is
               when 1 =>  --  workspaceFolders
                  Value.workspaceFolders :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_WorkspaceFolder_Vector_Or_Null
                    (Handler, Value.workspaceFolders.Value);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_WorkspaceFoldersInitializeParams;

   package ShowMessageParams_Scope is
      package ShowMessageParams_Map is new Minimal_Perfect_Hash
        (["type",
         "message"]);

   end ShowMessageParams_Scope;

   procedure Read_ShowMessageParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ShowMessageParams) is
      use ShowMessageParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case ShowMessageParams_Map.Get_Index (Key) is
               when 1 =>  --  type
                  Read_MessageType (Handler, Value.a_type);
               when 2 =>  --  message
                  Value.message.Clear;
                  Value.message.Append (Handler.String_Value);
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_ShowMessageParams;

   package FileSystemWatcher_Scope is
      package FileSystemWatcher_Map is new Minimal_Perfect_Hash
        (["globPattern",
         "kind"]);

   end FileSystemWatcher_Scope;

   procedure Read_FileSystemWatcher
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.FileSystemWatcher) is
      use FileSystemWatcher_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case FileSystemWatcher_Map.Get_Index (Key) is
               when 1 =>  --  globPattern
                  Read_GlobPattern (Handler, Value.globPattern);
               when 2 =>  --  kind
                  Value.kind :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_WatchKind (Handler, Value.kind.Value);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_FileSystemWatcher;

   procedure Read_CodeActionKind_Set
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CodeActionKind_Set) is
   begin
      pragma Assert (Handler.Is_Start_Array);
      Handler.Read_Next;

      declare
         Set   : LSP.Structures.CodeActionKind_Set renames Value;
         Value : LSP.Enumerations.CodeActionKind;
      begin
         Set := (others => False);
         while not Handler.Is_End_Array loop
            Read_CodeActionKind (Handler, Value);
            Set (Value) := True;
         end loop;
      end;

      Handler.Read_Next;
   end Read_CodeActionKind_Set;

   package NotebookDocumentSyncOptions_Scope is
      package NotebookDocumentSyncOptions_Map is new Minimal_Perfect_Hash
        (["notebookSelector",
         "save"]);

      package cells_OfnotebookSelector_OfNotebookDocumentSyncOptions_Item_Item_Scope
      is
         package cells_OfnotebookSelector_OfNotebookDocumentSyncOptions_Item_Item_Map is new Minimal_Perfect_Hash
           (["language"]);

      end cells_OfnotebookSelector_OfNotebookDocumentSyncOptions_Item_Item_Scope;

      package notebookSelector_OfNotebookDocumentSyncOptions_Item_Scope is
         package notebookSelector_OfNotebookDocumentSyncOptions_Item_Map is new Minimal_Perfect_Hash
           (["notebook",
            "cells"]);

      end notebookSelector_OfNotebookDocumentSyncOptions_Item_Scope;

   end NotebookDocumentSyncOptions_Scope;

   procedure Read_NotebookDocumentSyncOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.NotebookDocumentSyncOptions) is
      use NotebookDocumentSyncOptions_Scope;
      procedure Read_cells_OfnotebookSelector_OfNotebookDocumentSyncOptions_Item_Item
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .cells_OfnotebookSelector_OfNotebookDocumentSyncOptions_Item_Item);

      procedure Read_notebookSelector_OfNotebookDocumentSyncOptions
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .notebookSelector_OfNotebookDocumentSyncOptions);

      procedure Read_cells_OfnotebookSelector_OfNotebookDocumentSyncOptions_Item
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .cells_OfnotebookSelector_OfNotebookDocumentSyncOptions_Item);

      procedure Read_notebookSelector_OfNotebookDocumentSyncOptions_Item
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .notebookSelector_OfNotebookDocumentSyncOptions_Item);

      procedure Read_cells_OfnotebookSelector_OfNotebookDocumentSyncOptions_Item_Item
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .cells_OfnotebookSelector_OfNotebookDocumentSyncOptions_Item_Item) is
      begin
         pragma Assert (Handler.Is_Start_Object);
         Handler.Read_Next;

         while Handler.Is_Key_Name loop
            declare
               use
                 cells_OfnotebookSelector_OfNotebookDocumentSyncOptions_Item_Item_Scope;
               Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
            begin
               Handler.Read_Next;
               case cells_OfnotebookSelector_OfNotebookDocumentSyncOptions_Item_Item_Map
                 .Get_Index
                 (Key) is
                  when 1 =>  --  language
                     Value.language.Clear;
                     Value.language.Append (Handler.String_Value);
                     Handler.Read_Next;
                  when others =>
                     Handler.Skip_Current_Value;
               end case;
            end;
         end loop;

         Handler.Read_Next;
      end Read_cells_OfnotebookSelector_OfNotebookDocumentSyncOptions_Item_Item;

      procedure Read_notebookSelector_OfNotebookDocumentSyncOptions
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .notebookSelector_OfNotebookDocumentSyncOptions) is
      begin
         pragma Assert (Handler.Is_Start_Array);
         Handler.Read_Next;

         declare
            Set   :
              LSP.Structures
                .notebookSelector_OfNotebookDocumentSyncOptions renames
              Value;
            Value :
              LSP.Structures
                .notebookSelector_OfNotebookDocumentSyncOptions_Item;
         begin
            Set.Clear;
            while not Handler.Is_End_Array loop
               Read_notebookSelector_OfNotebookDocumentSyncOptions_Item
                 (Handler, Value);
               Set.Append (Value);
            end loop;
         end;

         Handler.Read_Next;
      end Read_notebookSelector_OfNotebookDocumentSyncOptions;

      procedure Read_cells_OfnotebookSelector_OfNotebookDocumentSyncOptions_Item
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .cells_OfnotebookSelector_OfNotebookDocumentSyncOptions_Item) is
      begin
         pragma Assert (Handler.Is_Start_Array);
         Handler.Read_Next;

         declare
            Set   :
              LSP.Structures
                .cells_OfnotebookSelector_OfNotebookDocumentSyncOptions_Item renames
              Value;
            Value :
              LSP.Structures
                .cells_OfnotebookSelector_OfNotebookDocumentSyncOptions_Item_Item;
         begin
            Set.Clear;
            while not Handler.Is_End_Array loop
               Read_cells_OfnotebookSelector_OfNotebookDocumentSyncOptions_Item_Item
                 (Handler, Value);
               Set.Append (Value);
            end loop;
         end;

         Handler.Read_Next;
      end Read_cells_OfnotebookSelector_OfNotebookDocumentSyncOptions_Item;

      procedure Read_notebookSelector_OfNotebookDocumentSyncOptions_Item
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .notebookSelector_OfNotebookDocumentSyncOptions_Item) is
         use notebookSelector_OfNotebookDocumentSyncOptions_Item_Scope;
      begin
         pragma Assert (Handler.Is_Start_Object);
         Handler.Read_Next;

         while Handler.Is_Key_Name loop
            declare
               Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
            begin
               Handler.Read_Next;
               case notebookSelector_OfNotebookDocumentSyncOptions_Item_Map
                 .Get_Index
                 (Key) is
                  when 1 =>  --  notebook
                     Value.notebook :=
                       (Is_Set => True,
                        Value  => <>);
                     Read_Virtual_String_Or_NotebookDocumentFilter
                       (Handler, Value.notebook.Value);
                  when 2 =>  --  cells
                     Read_cells_OfnotebookSelector_OfNotebookDocumentSyncOptions_Item
                       (Handler, Value.cells);
                  when others =>
                     Handler.Skip_Current_Value;
               end case;
            end;
         end loop;

         Handler.Read_Next;
      end Read_notebookSelector_OfNotebookDocumentSyncOptions_Item;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case NotebookDocumentSyncOptions_Map.Get_Index (Key) is
               when 1 =>  --  notebookSelector
                  Read_notebookSelector_OfNotebookDocumentSyncOptions
                    (Handler, Value.notebookSelector);
               when 2 =>  --  save
                  Value.save       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.save.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_NotebookDocumentSyncOptions;

   package CodeActionRegistrationOptions_Scope is
      package CodeActionRegistrationOptions_Map is new Minimal_Perfect_Hash
        (["documentSelector",
         "workDoneProgress",
         "codeActionKinds",
         "resolveProvider"]);

   end CodeActionRegistrationOptions_Scope;

   procedure Read_CodeActionRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CodeActionRegistrationOptions) is
      use CodeActionRegistrationOptions_Scope;
      procedure Read_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.DocumentSelector_Or_Null);

      procedure Read_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.DocumentSelector_Or_Null) is
      begin
         if Handler.Is_Null_Value then
            Handler.Read_Next;
         else
            Value :=
              (Is_Null => False,
               Value   => <>);
            Read_DocumentSelector (Handler, Value.Value);
         end if;
      end Read_DocumentSelector_Or_Null;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case CodeActionRegistrationOptions_Map.Get_Index (Key) is
               when 1 =>  --  documentSelector
                  Read_DocumentSelector_Or_Null
                    (Handler, Value.Parent.documentSelector);
               when 2 =>  --  workDoneProgress
                  Value.workDoneProgress       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.workDoneProgress.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 3 =>  --  codeActionKinds
                  Read_CodeActionKind_Set (Handler, Value.codeActionKinds);
               when 4 =>  --  resolveProvider
                  Value.resolveProvider       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.resolveProvider.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_CodeActionRegistrationOptions;

   package NotebookDocumentSyncRegistrationOptions_Scope is
      package NotebookDocumentSyncRegistrationOptions_Map is new Minimal_Perfect_Hash
        (["notebookSelector",
         "save",
         "id"]);

      package cells_OfnotebookSelector_OfNotebookDocumentSyncOptions_Item_Item_Scope
      is
         package cells_OfnotebookSelector_OfNotebookDocumentSyncOptions_Item_Item_Map is new Minimal_Perfect_Hash
           (["language"]);

      end cells_OfnotebookSelector_OfNotebookDocumentSyncOptions_Item_Item_Scope;

      package notebookSelector_OfNotebookDocumentSyncOptions_Item_Scope is
         package notebookSelector_OfNotebookDocumentSyncOptions_Item_Map is new Minimal_Perfect_Hash
           (["notebook",
            "cells"]);

      end notebookSelector_OfNotebookDocumentSyncOptions_Item_Scope;

   end NotebookDocumentSyncRegistrationOptions_Scope;

   procedure Read_NotebookDocumentSyncRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.NotebookDocumentSyncRegistrationOptions) is
      use NotebookDocumentSyncRegistrationOptions_Scope;
      procedure Read_cells_OfnotebookSelector_OfNotebookDocumentSyncOptions_Item_Item
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .cells_OfnotebookSelector_OfNotebookDocumentSyncOptions_Item_Item);

      procedure Read_notebookSelector_OfNotebookDocumentSyncOptions
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .notebookSelector_OfNotebookDocumentSyncOptions);

      procedure Read_cells_OfnotebookSelector_OfNotebookDocumentSyncOptions_Item
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .cells_OfnotebookSelector_OfNotebookDocumentSyncOptions_Item);

      procedure Read_notebookSelector_OfNotebookDocumentSyncOptions_Item
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .notebookSelector_OfNotebookDocumentSyncOptions_Item);

      procedure Read_cells_OfnotebookSelector_OfNotebookDocumentSyncOptions_Item_Item
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .cells_OfnotebookSelector_OfNotebookDocumentSyncOptions_Item_Item) is
      begin
         pragma Assert (Handler.Is_Start_Object);
         Handler.Read_Next;

         while Handler.Is_Key_Name loop
            declare
               use
                 cells_OfnotebookSelector_OfNotebookDocumentSyncOptions_Item_Item_Scope;
               Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
            begin
               Handler.Read_Next;
               case cells_OfnotebookSelector_OfNotebookDocumentSyncOptions_Item_Item_Map
                 .Get_Index
                 (Key) is
                  when 1 =>  --  language
                     Value.language.Clear;
                     Value.language.Append (Handler.String_Value);
                     Handler.Read_Next;
                  when others =>
                     Handler.Skip_Current_Value;
               end case;
            end;
         end loop;

         Handler.Read_Next;
      end Read_cells_OfnotebookSelector_OfNotebookDocumentSyncOptions_Item_Item;

      procedure Read_notebookSelector_OfNotebookDocumentSyncOptions
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .notebookSelector_OfNotebookDocumentSyncOptions) is
      begin
         pragma Assert (Handler.Is_Start_Array);
         Handler.Read_Next;

         declare
            Set   :
              LSP.Structures
                .notebookSelector_OfNotebookDocumentSyncOptions renames
              Value;
            Value :
              LSP.Structures
                .notebookSelector_OfNotebookDocumentSyncOptions_Item;
         begin
            Set.Clear;
            while not Handler.Is_End_Array loop
               Read_notebookSelector_OfNotebookDocumentSyncOptions_Item
                 (Handler, Value);
               Set.Append (Value);
            end loop;
         end;

         Handler.Read_Next;
      end Read_notebookSelector_OfNotebookDocumentSyncOptions;

      procedure Read_cells_OfnotebookSelector_OfNotebookDocumentSyncOptions_Item
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .cells_OfnotebookSelector_OfNotebookDocumentSyncOptions_Item) is
      begin
         pragma Assert (Handler.Is_Start_Array);
         Handler.Read_Next;

         declare
            Set   :
              LSP.Structures
                .cells_OfnotebookSelector_OfNotebookDocumentSyncOptions_Item renames
              Value;
            Value :
              LSP.Structures
                .cells_OfnotebookSelector_OfNotebookDocumentSyncOptions_Item_Item;
         begin
            Set.Clear;
            while not Handler.Is_End_Array loop
               Read_cells_OfnotebookSelector_OfNotebookDocumentSyncOptions_Item_Item
                 (Handler, Value);
               Set.Append (Value);
            end loop;
         end;

         Handler.Read_Next;
      end Read_cells_OfnotebookSelector_OfNotebookDocumentSyncOptions_Item;

      procedure Read_notebookSelector_OfNotebookDocumentSyncOptions_Item
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .notebookSelector_OfNotebookDocumentSyncOptions_Item) is
         use notebookSelector_OfNotebookDocumentSyncOptions_Item_Scope;
      begin
         pragma Assert (Handler.Is_Start_Object);
         Handler.Read_Next;

         while Handler.Is_Key_Name loop
            declare
               Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
            begin
               Handler.Read_Next;
               case notebookSelector_OfNotebookDocumentSyncOptions_Item_Map
                 .Get_Index
                 (Key) is
                  when 1 =>  --  notebook
                     Value.notebook :=
                       (Is_Set => True,
                        Value  => <>);
                     Read_Virtual_String_Or_NotebookDocumentFilter
                       (Handler, Value.notebook.Value);
                  when 2 =>  --  cells
                     Read_cells_OfnotebookSelector_OfNotebookDocumentSyncOptions_Item
                       (Handler, Value.cells);
                  when others =>
                     Handler.Skip_Current_Value;
               end case;
            end;
         end loop;

         Handler.Read_Next;
      end Read_notebookSelector_OfNotebookDocumentSyncOptions_Item;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case NotebookDocumentSyncRegistrationOptions_Map.Get_Index (Key) is
               when 1 =>  --  notebookSelector
                  Read_notebookSelector_OfNotebookDocumentSyncOptions
                    (Handler, Value.notebookSelector);
               when 2 =>  --  save
                  Value.save       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.save.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 3 =>  --  id
                  Value.id.Clear;
                  Value.id.Append (Handler.String_Value);
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_NotebookDocumentSyncRegistrationOptions;

   package WorkspaceUnchangedDocumentDiagnosticReport_Scope is
      package WorkspaceUnchangedDocumentDiagnosticReport_Map is new Minimal_Perfect_Hash
        (["kind",
         "resultId",
         "uri",
         "version"]);

   end WorkspaceUnchangedDocumentDiagnosticReport_Scope;

   procedure Read_WorkspaceUnchangedDocumentDiagnosticReport
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value : out LSP.Structures.WorkspaceUnchangedDocumentDiagnosticReport) is
      use WorkspaceUnchangedDocumentDiagnosticReport_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case WorkspaceUnchangedDocumentDiagnosticReport_Map.Get_Index
              (Key) is
               when 1 =>  --  kind
                  Handler.Read_Next;  --  Skip string literal: unchanged
               when 2 =>  --  resultId
                  Value.resultId.Clear;
                  Value.resultId.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 3 =>  --  uri
                  Value.uri := (Handler.String_Value with null record);
                  Handler.Read_Next;
               when 4 =>  --  version
                  Read_Integer_Or_Null (Handler, Value.version);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_WorkspaceUnchangedDocumentDiagnosticReport;

   package ReferenceParams_Scope is
      package ReferenceParams_Map is new Minimal_Perfect_Hash
        (["textDocument",
         "position",
         "workDoneToken",
         "partialResultToken",
         "context"]);

   end ReferenceParams_Scope;

   procedure Read_ReferenceParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ReferenceParams) is
      use ReferenceParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case ReferenceParams_Map.Get_Index (Key) is
               when 1 =>  --  textDocument
                  Read_TextDocumentIdentifier (Handler, Value.textDocument);
               when 2 =>  --  position
                  Read_Position (Handler, Value.position);
               when 3 =>  --  workDoneToken
                  Value.workDoneToken :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_ProgressToken (Handler, Value.workDoneToken.Value);
               when 4 =>  --  partialResultToken
                  Value.partialResultToken :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_ProgressToken (Handler, Value.partialResultToken.Value);
               when 5 =>  --  context
                  Read_ReferenceContext (Handler, Value.context);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_ReferenceParams;

   package HoverClientCapabilities_Scope is
      package HoverClientCapabilities_Map is new Minimal_Perfect_Hash
        (["dynamicRegistration",
         "contentFormat"]);

   end HoverClientCapabilities_Scope;

   procedure Read_HoverClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.HoverClientCapabilities) is
      use HoverClientCapabilities_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case HoverClientCapabilities_Map.Get_Index (Key) is
               when 1 =>  --  dynamicRegistration
                  Value.dynamicRegistration       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.dynamicRegistration.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 2 =>  --  contentFormat
                  Read_MarkupKind_Vector (Handler, Value.contentFormat);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_HoverClientCapabilities;

   package RenameClientCapabilities_Scope is
      package RenameClientCapabilities_Map is new Minimal_Perfect_Hash
        (["dynamicRegistration",
         "prepareSupport",
         "prepareSupportDefaultBehavior",
         "honorsChangeAnnotations"]);

   end RenameClientCapabilities_Scope;

   procedure Read_RenameClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.RenameClientCapabilities) is
      use RenameClientCapabilities_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case RenameClientCapabilities_Map.Get_Index (Key) is
               when 1 =>  --  dynamicRegistration
                  Value.dynamicRegistration       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.dynamicRegistration.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 2 =>  --  prepareSupport
                  Value.prepareSupport       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.prepareSupport.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 3 =>  --  prepareSupportDefaultBehavior
                  Value.prepareSupportDefaultBehavior :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_PrepareSupportDefaultBehavior
                    (Handler, Value.prepareSupportDefaultBehavior.Value);
               when 4 =>  --  honorsChangeAnnotations
                  Value.honorsChangeAnnotations       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.honorsChangeAnnotations.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_RenameClientCapabilities;

   package DidChangeConfigurationParams_Scope is
      package DidChangeConfigurationParams_Map is new Minimal_Perfect_Hash
        (["settings"]);

   end DidChangeConfigurationParams_Scope;

   procedure Read_DidChangeConfigurationParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DidChangeConfigurationParams) is
      use DidChangeConfigurationParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case DidChangeConfigurationParams_Map.Get_Index (Key) is
               when 1 =>  --  settings
                  Read_LSPAny (Handler, Value.settings);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_DidChangeConfigurationParams;

   package DefinitionOptions_Scope is
      package DefinitionOptions_Map is new Minimal_Perfect_Hash
        (["workDoneProgress"]);

   end DefinitionOptions_Scope;

   procedure Read_DefinitionOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DefinitionOptions) is
      use DefinitionOptions_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case DefinitionOptions_Map.Get_Index (Key) is
               when 1 =>  --  workDoneProgress
                  Value.workDoneProgress       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.workDoneProgress.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_DefinitionOptions;

   procedure Read_Hover_Or_Null
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Hover_Or_Null) is
   begin
      if Handler.Is_Null_Value then
         Handler.Read_Next;
      else
         Value :=
           (Is_Null => False,
            Value   => <>);
         Read_Hover (Handler, Value.Value);
      end if;
   end Read_Hover_Or_Null;

   procedure Read_InlayHintKind
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.InlayHintKind) is
   begin
      Value :=
        LSP.Enumerations.InlayHintKind'Val
          (Handler.Number_Value.Integer_Value - 1);
      Handler.Read_Next;
   end Read_InlayHintKind;

   package symbolKind_OfWorkspaceSymbolClientCapabilities_Scope is
      package symbolKind_OfWorkspaceSymbolClientCapabilities_Map is new Minimal_Perfect_Hash
        (["valueSet"]);

   end symbolKind_OfWorkspaceSymbolClientCapabilities_Scope;

   procedure Read_symbolKind_OfWorkspaceSymbolClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures
        .symbolKind_OfWorkspaceSymbolClientCapabilities) is
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while Handler.Is_Key_Name loop
         declare
            use symbolKind_OfWorkspaceSymbolClientCapabilities_Scope;
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case symbolKind_OfWorkspaceSymbolClientCapabilities_Map.Get_Index
              (Key) is
               when 1 =>  --  valueSet
                  Read_SymbolKind_Set (Handler, Value.valueSet);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_symbolKind_OfWorkspaceSymbolClientCapabilities;

   procedure Read_CallHierarchyOutgoingCall_Vector_Or_Null
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CallHierarchyOutgoingCall_Vector_Or_Null) is
   begin
      if Handler.Is_Null_Value then
         Handler.Read_Next;
      else
         Value.Clear;
         Read_CallHierarchyOutgoingCall_Vector (Handler, Value);
      end if;
   end Read_CallHierarchyOutgoingCall_Vector_Or_Null;

   package FailureHandlingKind_Map is new Minimal_Perfect_Hash
     (["abort",
      "transactional",
      "textOnlyTransactional",
      "undo"]);

   procedure Read_FailureHandlingKind
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.FailureHandlingKind) is
   begin
      Value :=
        LSP.Enumerations.FailureHandlingKind'Val
          (FailureHandlingKind_Map.Get_Index (Handler.String_Value) - 1);
      Handler.Read_Next;
   end Read_FailureHandlingKind;

   package FileOperationOptions_Scope is
      package FileOperationOptions_Map is new Minimal_Perfect_Hash
        (["didCreate",
         "willCreate",
         "didRename",
         "willRename",
         "didDelete",
         "willDelete"]);

   end FileOperationOptions_Scope;

   procedure Read_FileOperationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.FileOperationOptions) is
      use FileOperationOptions_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case FileOperationOptions_Map.Get_Index (Key) is
               when 1 =>  --  didCreate
                  Value.didCreate :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_FileOperationRegistrationOptions
                    (Handler, Value.didCreate.Value);
               when 2 =>  --  willCreate
                  Value.willCreate :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_FileOperationRegistrationOptions
                    (Handler, Value.willCreate.Value);
               when 3 =>  --  didRename
                  Value.didRename :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_FileOperationRegistrationOptions
                    (Handler, Value.didRename.Value);
               when 4 =>  --  willRename
                  Value.willRename :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_FileOperationRegistrationOptions
                    (Handler, Value.willRename.Value);
               when 5 =>  --  didDelete
                  Value.didDelete :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_FileOperationRegistrationOptions
                    (Handler, Value.didDelete.Value);
               when 6 =>  --  willDelete
                  Value.willDelete :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_FileOperationRegistrationOptions
                    (Handler, Value.willDelete.Value);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_FileOperationOptions;

   package ShowDocumentResult_Scope is
      package ShowDocumentResult_Map is new Minimal_Perfect_Hash (["success"]);

   end ShowDocumentResult_Scope;

   procedure Read_ShowDocumentResult
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ShowDocumentResult) is
      use ShowDocumentResult_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case ShowDocumentResult_Map.Get_Index (Key) is
               when 1 =>  --  success
                  Value.success := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_ShowDocumentResult;

   package TypeDefinitionParams_Scope is
      package TypeDefinitionParams_Map is new Minimal_Perfect_Hash
        (["textDocument",
         "position",
         "workDoneToken",
         "partialResultToken"]);

   end TypeDefinitionParams_Scope;

   procedure Read_TypeDefinitionParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TypeDefinitionParams) is
      use TypeDefinitionParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case TypeDefinitionParams_Map.Get_Index (Key) is
               when 1 =>  --  textDocument
                  Read_TextDocumentIdentifier (Handler, Value.textDocument);
               when 2 =>  --  position
                  Read_Position (Handler, Value.position);
               when 3 =>  --  workDoneToken
                  Value.workDoneToken :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_ProgressToken (Handler, Value.workDoneToken.Value);
               when 4 =>  --  partialResultToken
                  Value.partialResultToken :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_ProgressToken (Handler, Value.partialResultToken.Value);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_TypeDefinitionParams;

   procedure Read_DocumentSelector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentSelector) is
      procedure Read_Virtual_String_Or_DocumentFilter
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.Virtual_String_Or_DocumentFilter);

      procedure Read_Virtual_String_Or_DocumentFilter
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.Virtual_String_Or_DocumentFilter) is
      begin
         if Handler.Is_String_Value then
            Value :=
              (Is_Virtual_String => True,
               others            => <>);
         else
            Value :=
              (Is_Virtual_String => False,
               others            => <>);
         end if;

         case Value.Is_Virtual_String is
            when True =>
               Value.Virtual_String.Clear;
               Value.Virtual_String.Append (Handler.String_Value);
               Handler.Read_Next;
            when False =>
               Read_DocumentFilter (Handler, Value.DocumentFilter);
         end case;
      end Read_Virtual_String_Or_DocumentFilter;

   begin
      pragma Assert (Handler.Is_Start_Array);
      Handler.Read_Next;

      declare
         Set   : LSP.Structures.DocumentSelector renames Value;
         Value : LSP.Structures.Virtual_String_Or_DocumentFilter;
      begin
         Set.Clear;
         while not Handler.Is_End_Array loop
            Read_Virtual_String_Or_DocumentFilter (Handler, Value);
            Set.Append (Value);
         end loop;
      end;

      Handler.Read_Next;
   end Read_DocumentSelector;

   package CompletionClientCapabilities_Scope is
      package CompletionClientCapabilities_Map is new Minimal_Perfect_Hash
        (["dynamicRegistration",
         "completionItem",
         "completionItemKind",
         "insertTextMode",
         "contextSupport",
         "completionList"]);

      package completionItem_OfCompletionClientCapabilities_Scope is
         package completionItem_OfCompletionClientCapabilities_Map is new Minimal_Perfect_Hash
           (["snippetSupport",
            "commitCharactersSupport",
            "documentationFormat",
            "deprecatedSupport",
            "preselectSupport",
            "tagSupport",
            "insertReplaceSupport",
            "resolveSupport",
            "insertTextModeSupport",
            "labelDetailsSupport"]);

      end completionItem_OfCompletionClientCapabilities_Scope;

      package completionList_OfCompletionClientCapabilities_Scope is
         package completionList_OfCompletionClientCapabilities_Map is new Minimal_Perfect_Hash
           (["itemDefaults"]);

      end completionList_OfCompletionClientCapabilities_Scope;

      package tagSupport_OfcompletionItem_OfCompletionClientCapabilities_Scope
      is
         package tagSupport_OfcompletionItem_OfCompletionClientCapabilities_Map is new Minimal_Perfect_Hash
           (["valueSet"]);

      end tagSupport_OfcompletionItem_OfCompletionClientCapabilities_Scope;

      package completionItemKind_OfCompletionClientCapabilities_Scope is
         package completionItemKind_OfCompletionClientCapabilities_Map is new Minimal_Perfect_Hash
           (["valueSet"]);

      end completionItemKind_OfCompletionClientCapabilities_Scope;

      package insertTextModeSupport_OfcompletionItem_OfCompletionClientCapabilities_Scope
      is
         package insertTextModeSupport_OfcompletionItem_OfCompletionClientCapabilities_Map is new Minimal_Perfect_Hash
           (["valueSet"]);

      end insertTextModeSupport_OfcompletionItem_OfCompletionClientCapabilities_Scope;

   end CompletionClientCapabilities_Scope;

   procedure Read_CompletionClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CompletionClientCapabilities) is
      use CompletionClientCapabilities_Scope;
      procedure Read_CompletionItemKind_Set
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.CompletionItemKind_Set);

      procedure Read_completionItem_OfCompletionClientCapabilities
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .completionItem_OfCompletionClientCapabilities);

      procedure Read_completionList_OfCompletionClientCapabilities
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .completionList_OfCompletionClientCapabilities);

      procedure Read_tagSupport_OfcompletionItem_OfCompletionClientCapabilities
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .tagSupport_OfcompletionItem_OfCompletionClientCapabilities);

      procedure Read_InsertTextMode_Set
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.InsertTextMode_Set);

      procedure Read_completionItemKind_OfCompletionClientCapabilities
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .completionItemKind_OfCompletionClientCapabilities);

      procedure Read_insertTextModeSupport_OfcompletionItem_OfCompletionClientCapabilities
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .insertTextModeSupport_OfcompletionItem_OfCompletionClientCapabilities);

      procedure Read_CompletionItemKind_Set
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.CompletionItemKind_Set) is
      begin
         pragma Assert (Handler.Is_Start_Array);
         Handler.Read_Next;

         declare
            Set   : LSP.Structures.CompletionItemKind_Set renames Value;
            Value : LSP.Enumerations.CompletionItemKind;
         begin
            Set := (others => False);
            while not Handler.Is_End_Array loop
               Read_CompletionItemKind (Handler, Value);
               Set (Value) := True;
            end loop;
         end;

         Handler.Read_Next;
      end Read_CompletionItemKind_Set;

      procedure Read_completionItem_OfCompletionClientCapabilities
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .completionItem_OfCompletionClientCapabilities) is
      begin
         pragma Assert (Handler.Is_Start_Object);
         Handler.Read_Next;

         while Handler.Is_Key_Name loop
            declare
               use completionItem_OfCompletionClientCapabilities_Scope;
               Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
            begin
               Handler.Read_Next;
               case completionItem_OfCompletionClientCapabilities_Map.Get_Index
                 (Key) is
                  when 1 =>  --  snippetSupport
                     Value.snippetSupport       :=
                       (Is_Set => True,
                        Value  => <>);
                     Value.snippetSupport.Value := Handler.Boolean_Value;
                     Handler.Read_Next;
                  when 2 =>  --  commitCharactersSupport
                     Value.commitCharactersSupport       :=
                       (Is_Set => True,
                        Value  => <>);
                     Value.commitCharactersSupport.Value :=
                       Handler.Boolean_Value;
                     Handler.Read_Next;
                  when 3 =>  --  documentationFormat
                     Read_MarkupKind_Vector
                       (Handler, Value.documentationFormat);
                  when 4 =>  --  deprecatedSupport
                     Value.deprecatedSupport       :=
                       (Is_Set => True,
                        Value  => <>);
                     Value.deprecatedSupport.Value := Handler.Boolean_Value;
                     Handler.Read_Next;
                  when 5 =>  --  preselectSupport
                     Value.preselectSupport       :=
                       (Is_Set => True,
                        Value  => <>);
                     Value.preselectSupport.Value := Handler.Boolean_Value;
                     Handler.Read_Next;
                  when 6 =>  --  tagSupport
                     Value.tagSupport :=
                       (Is_Set => True,
                        Value  => <>);
                     Read_tagSupport_OfcompletionItem_OfCompletionClientCapabilities
                       (Handler, Value.tagSupport.Value);
                  when 7 =>  --  insertReplaceSupport
                     Value.insertReplaceSupport       :=
                       (Is_Set => True,
                        Value  => <>);
                     Value.insertReplaceSupport.Value := Handler.Boolean_Value;
                     Handler.Read_Next;
                  when 8 =>  --  resolveSupport
                     Value.resolveSupport :=
                       (Is_Set => True,
                        Value  => <>);
                     Read_resolveSupport_OfWorkspaceSymbolClientCapabilities
                       (Handler, Value.resolveSupport.Value);
                  when 9 =>  --  insertTextModeSupport
                     Value.insertTextModeSupport :=
                       (Is_Set => True,
                        Value  => <>);
                     Read_insertTextModeSupport_OfcompletionItem_OfCompletionClientCapabilities
                       (Handler, Value.insertTextModeSupport.Value);
                  when 10 =>  --  labelDetailsSupport
                     Value.labelDetailsSupport       :=
                       (Is_Set => True,
                        Value  => <>);
                     Value.labelDetailsSupport.Value := Handler.Boolean_Value;
                     Handler.Read_Next;
                  when others =>
                     Handler.Skip_Current_Value;
               end case;
            end;
         end loop;

         Handler.Read_Next;
      end Read_completionItem_OfCompletionClientCapabilities;

      procedure Read_completionList_OfCompletionClientCapabilities
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .completionList_OfCompletionClientCapabilities) is
      begin
         pragma Assert (Handler.Is_Start_Object);
         Handler.Read_Next;

         while Handler.Is_Key_Name loop
            declare
               use completionList_OfCompletionClientCapabilities_Scope;
               Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
            begin
               Handler.Read_Next;
               case completionList_OfCompletionClientCapabilities_Map.Get_Index
                 (Key) is
                  when 1 =>  --  itemDefaults
                     Read_Virtual_String_Vector (Handler, Value.itemDefaults);
                  when others =>
                     Handler.Skip_Current_Value;
               end case;
            end;
         end loop;

         Handler.Read_Next;
      end Read_completionList_OfCompletionClientCapabilities;

      procedure Read_tagSupport_OfcompletionItem_OfCompletionClientCapabilities
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .tagSupport_OfcompletionItem_OfCompletionClientCapabilities) is
      begin
         pragma Assert (Handler.Is_Start_Object);
         Handler.Read_Next;

         while Handler.Is_Key_Name loop
            declare
               use
                 tagSupport_OfcompletionItem_OfCompletionClientCapabilities_Scope;
               Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
            begin
               Handler.Read_Next;
               case tagSupport_OfcompletionItem_OfCompletionClientCapabilities_Map
                 .Get_Index
                 (Key) is
                  when 1 =>  --  valueSet
                     Read_CompletionItemTag_Set (Handler, Value.valueSet);
                  when others =>
                     Handler.Skip_Current_Value;
               end case;
            end;
         end loop;

         Handler.Read_Next;
      end Read_tagSupport_OfcompletionItem_OfCompletionClientCapabilities;

      procedure Read_InsertTextMode_Set
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.InsertTextMode_Set) is
      begin
         pragma Assert (Handler.Is_Start_Array);
         Handler.Read_Next;

         declare
            Set   : LSP.Structures.InsertTextMode_Set renames Value;
            Value : LSP.Enumerations.InsertTextMode;
         begin
            Set := (others => False);
            while not Handler.Is_End_Array loop
               Read_InsertTextMode (Handler, Value);
               Set (Value) := True;
            end loop;
         end;

         Handler.Read_Next;
      end Read_InsertTextMode_Set;

      procedure Read_completionItemKind_OfCompletionClientCapabilities
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .completionItemKind_OfCompletionClientCapabilities) is
      begin
         pragma Assert (Handler.Is_Start_Object);
         Handler.Read_Next;

         while Handler.Is_Key_Name loop
            declare
               use completionItemKind_OfCompletionClientCapabilities_Scope;
               Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
            begin
               Handler.Read_Next;
               case completionItemKind_OfCompletionClientCapabilities_Map
                 .Get_Index
                 (Key) is
                  when 1 =>  --  valueSet
                     Read_CompletionItemKind_Set (Handler, Value.valueSet);
                  when others =>
                     Handler.Skip_Current_Value;
               end case;
            end;
         end loop;

         Handler.Read_Next;
      end Read_completionItemKind_OfCompletionClientCapabilities;

      procedure Read_insertTextModeSupport_OfcompletionItem_OfCompletionClientCapabilities
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .insertTextModeSupport_OfcompletionItem_OfCompletionClientCapabilities) is
      begin
         pragma Assert (Handler.Is_Start_Object);
         Handler.Read_Next;

         while Handler.Is_Key_Name loop
            declare
               use
                 insertTextModeSupport_OfcompletionItem_OfCompletionClientCapabilities_Scope;
               Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
            begin
               Handler.Read_Next;
               case insertTextModeSupport_OfcompletionItem_OfCompletionClientCapabilities_Map
                 .Get_Index
                 (Key) is
                  when 1 =>  --  valueSet
                     Read_InsertTextMode_Set (Handler, Value.valueSet);
                  when others =>
                     Handler.Skip_Current_Value;
               end case;
            end;
         end loop;

         Handler.Read_Next;
      end Read_insertTextModeSupport_OfcompletionItem_OfCompletionClientCapabilities;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case CompletionClientCapabilities_Map.Get_Index (Key) is
               when 1 =>  --  dynamicRegistration
                  Value.dynamicRegistration       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.dynamicRegistration.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 2 =>  --  completionItem
                  Value.completionItem :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_completionItem_OfCompletionClientCapabilities
                    (Handler, Value.completionItem.Value);
               when 3 =>  --  completionItemKind
                  Value.completionItemKind :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_completionItemKind_OfCompletionClientCapabilities
                    (Handler, Value.completionItemKind.Value);
               when 4 =>  --  insertTextMode
                  Value.insertTextMode :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_InsertTextMode (Handler, Value.insertTextMode.Value);
               when 5 =>  --  contextSupport
                  Value.contextSupport       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.contextSupport.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 6 =>  --  completionList
                  Value.completionList :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_completionList_OfCompletionClientCapabilities
                    (Handler, Value.completionList.Value);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_CompletionClientCapabilities;

   package CodeLensWorkspaceClientCapabilities_Scope is
      package CodeLensWorkspaceClientCapabilities_Map is new Minimal_Perfect_Hash
        (["refreshSupport"]);

   end CodeLensWorkspaceClientCapabilities_Scope;

   procedure Read_CodeLensWorkspaceClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CodeLensWorkspaceClientCapabilities) is
      use CodeLensWorkspaceClientCapabilities_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case CodeLensWorkspaceClientCapabilities_Map.Get_Index (Key) is
               when 1 =>  --  refreshSupport
                  Value.refreshSupport       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.refreshSupport.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_CodeLensWorkspaceClientCapabilities;

   package RenameOptions_Scope is
      package RenameOptions_Map is new Minimal_Perfect_Hash
        (["workDoneProgress",
         "prepareProvider"]);

   end RenameOptions_Scope;

   procedure Read_RenameOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.RenameOptions) is
      use RenameOptions_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case RenameOptions_Map.Get_Index (Key) is
               when 1 =>  --  workDoneProgress
                  Value.workDoneProgress       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.workDoneProgress.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 2 =>  --  prepareProvider
                  Value.prepareProvider       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.prepareProvider.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_RenameOptions;

   procedure Read_SymbolKind_Set
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SymbolKind_Set) is
   begin
      pragma Assert (Handler.Is_Start_Array);
      Handler.Read_Next;

      declare
         Set   : LSP.Structures.SymbolKind_Set renames Value;
         Value : LSP.Enumerations.SymbolKind;
      begin
         Set := (others => False);
         while not Handler.Is_End_Array loop
            Read_SymbolKind (Handler, Value);
            Set (Value) := True;
         end loop;
      end;

      Handler.Read_Next;
   end Read_SymbolKind_Set;

   package DidChangeConfigurationClientCapabilities_Scope is
      package DidChangeConfigurationClientCapabilities_Map is new Minimal_Perfect_Hash
        (["dynamicRegistration"]);

   end DidChangeConfigurationClientCapabilities_Scope;

   procedure Read_DidChangeConfigurationClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DidChangeConfigurationClientCapabilities) is
      use DidChangeConfigurationClientCapabilities_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case DidChangeConfigurationClientCapabilities_Map.Get_Index
              (Key) is
               when 1 =>  --  dynamicRegistration
                  Value.dynamicRegistration       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.dynamicRegistration.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_DidChangeConfigurationClientCapabilities;

   package DocumentHighlightRegistrationOptions_Scope is
      package DocumentHighlightRegistrationOptions_Map is new Minimal_Perfect_Hash
        (["documentSelector",
         "workDoneProgress"]);

   end DocumentHighlightRegistrationOptions_Scope;

   procedure Read_DocumentHighlightRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentHighlightRegistrationOptions) is
      use DocumentHighlightRegistrationOptions_Scope;
      procedure Read_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.DocumentSelector_Or_Null);

      procedure Read_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.DocumentSelector_Or_Null) is
      begin
         if Handler.Is_Null_Value then
            Handler.Read_Next;
         else
            Value :=
              (Is_Null => False,
               Value   => <>);
            Read_DocumentSelector (Handler, Value.Value);
         end if;
      end Read_DocumentSelector_Or_Null;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case DocumentHighlightRegistrationOptions_Map.Get_Index (Key) is
               when 1 =>  --  documentSelector
                  Read_DocumentSelector_Or_Null
                    (Handler, Value.Parent.documentSelector);
               when 2 =>  --  workDoneProgress
                  Value.workDoneProgress       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.workDoneProgress.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_DocumentHighlightRegistrationOptions;

   procedure Read_DeclarationLink
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DeclarationLink) renames
     Read_LocationLink;

   procedure Read_T
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.T) is
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            Handler.Skip_Current_Value;
         end;
      end loop;

      Handler.Read_Next;
   end Read_T;

   package FullDocumentDiagnosticReport_Scope is
      package FullDocumentDiagnosticReport_Map is new Minimal_Perfect_Hash
        (["kind",
         "resultId",
         "items"]);

   end FullDocumentDiagnosticReport_Scope;

   procedure Read_FullDocumentDiagnosticReport
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.FullDocumentDiagnosticReport) is
      use FullDocumentDiagnosticReport_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case FullDocumentDiagnosticReport_Map.Get_Index (Key) is
               when 1 =>  --  kind
                  Handler.Read_Next;  --  Skip string literal: full
               when 2 =>  --  resultId
                  Value.resultId.Clear;
                  Value.resultId.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 3 =>  --  items
                  Read_Diagnostic_Vector (Handler, Value.items);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_FullDocumentDiagnosticReport;

   package RenameParams_Scope is
      package RenameParams_Map is new Minimal_Perfect_Hash
        (["workDoneToken",
         "textDocument",
         "position",
         "newName"]);

   end RenameParams_Scope;

   procedure Read_RenameParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.RenameParams) is
      use RenameParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case RenameParams_Map.Get_Index (Key) is
               when 1 =>  --  workDoneToken
                  Value.workDoneToken :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_ProgressToken (Handler, Value.workDoneToken.Value);
               when 2 =>  --  textDocument
                  Read_TextDocumentIdentifier (Handler, Value.textDocument);
               when 3 =>  --  position
                  Read_Position (Handler, Value.position);
               when 4 =>  --  newName
                  Value.newName.Clear;
                  Value.newName.Append (Handler.String_Value);
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_RenameParams;

   package DocumentSymbolRegistrationOptions_Scope is
      package DocumentSymbolRegistrationOptions_Map is new Minimal_Perfect_Hash
        (["documentSelector",
         "workDoneProgress",
         "label"]);

   end DocumentSymbolRegistrationOptions_Scope;

   procedure Read_DocumentSymbolRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentSymbolRegistrationOptions) is
      use DocumentSymbolRegistrationOptions_Scope;
      procedure Read_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.DocumentSelector_Or_Null);

      procedure Read_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.DocumentSelector_Or_Null) is
      begin
         if Handler.Is_Null_Value then
            Handler.Read_Next;
         else
            Value :=
              (Is_Null => False,
               Value   => <>);
            Read_DocumentSelector (Handler, Value.Value);
         end if;
      end Read_DocumentSelector_Or_Null;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case DocumentSymbolRegistrationOptions_Map.Get_Index (Key) is
               when 1 =>  --  documentSelector
                  Read_DocumentSelector_Or_Null
                    (Handler, Value.Parent.documentSelector);
               when 2 =>  --  workDoneProgress
                  Value.workDoneProgress       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.workDoneProgress.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 3 =>  --  label
                  Value.label.Clear;
                  Value.label.Append (Handler.String_Value);
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_DocumentSymbolRegistrationOptions;

   procedure Read_LSPAny_Or_Null
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.LSPAny_Or_Null) is
   begin
      if Handler.Is_Null_Value then
         Handler.Read_Next;
      else
         Value :=
           (Is_Null => False,
            Value   => <>);
         Read_LSPAny (Handler, Value.Value);
      end if;
   end Read_LSPAny_Or_Null;

   package DeclarationRegistrationOptions_Scope is
      package DeclarationRegistrationOptions_Map is new Minimal_Perfect_Hash
        (["workDoneProgress",
         "documentSelector",
         "id"]);

   end DeclarationRegistrationOptions_Scope;

   procedure Read_DeclarationRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DeclarationRegistrationOptions) is
      use DeclarationRegistrationOptions_Scope;
      procedure Read_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.DocumentSelector_Or_Null);

      procedure Read_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.DocumentSelector_Or_Null) is
      begin
         if Handler.Is_Null_Value then
            Handler.Read_Next;
         else
            Value :=
              (Is_Null => False,
               Value   => <>);
            Read_DocumentSelector (Handler, Value.Value);
         end if;
      end Read_DocumentSelector_Or_Null;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case DeclarationRegistrationOptions_Map.Get_Index (Key) is
               when 1 =>  --  workDoneProgress
                  Value.workDoneProgress       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.workDoneProgress.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 2 =>  --  documentSelector
                  Read_DocumentSelector_Or_Null
                    (Handler, Value.Parent.documentSelector);
               when 3 =>  --  id
                  Value.id.Clear;
                  Value.id.Append (Handler.String_Value);
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_DeclarationRegistrationOptions;

   package WorkspaceSymbol_Scope is
      package WorkspaceSymbol_Map is new Minimal_Perfect_Hash
        (["name",
         "kind",
         "tags",
         "containerName",
         "location",
         "data"]);

      package Location_Or_Something_Scope is
         package Location_Or_Something_Map is new Minimal_Perfect_Hash
           (["range",
            "uri"]);

      end Location_Or_Something_Scope;

   end WorkspaceSymbol_Scope;

   procedure Read_WorkspaceSymbol
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.WorkspaceSymbol) is
      use WorkspaceSymbol_Scope;
      procedure Read_Location_Or_Something
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.Location_Or_Something);

      procedure Read_Location_Or_Something
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.Location_Or_Something) is
         use Location_Or_Something_Scope;
      begin
         declare
            Parent  :
              constant not null access VSS.JSON.Pull_Readers.JSON_Pull_Reader'
                Class :=
              Handler'Access;
            Handler :
              VSS.JSON.Pull_Readers.Buffered.JSON_Buffered_Pull_Reader
                (Parent);
         begin
            Handler.Mark;
            if Handler.Is_Start_Object then
               Handler.Read_Next;
               Value :=
                 (Is_Location => False,
                  others      => <>);
               while Handler.Is_Key_Name loop
                  declare
                     Key   : constant VSS.Strings.Virtual_String :=
                       Handler.Key_Name;
                     Index : constant Natural                    :=
                       Location_Or_Something_Map.Get_Index (Key);
                  begin
                     Handler.Read_Next;
                     case Index is
                        when 1 =>  --  range
                           Value :=
                             (Is_Location => True,
                              others      => <>);
                           exit;
                        when others =>
                           Handler.Skip_Current_Value;
                     end case;
                  end;
               end loop;
            else
               raise Program_Error;  --  Unexpected JSON value
            end if;

            Handler.Reset;
            Handler.Unmark;

            case Value.Is_Location is
               when True =>
                  Read_Location (Handler, Value.Location);
               when False =>
                  pragma Assert (Handler.Is_Start_Object);
                  Handler.Read_Next;

                  while Handler.Is_Key_Name loop
                     declare
                        Key : constant VSS.Strings.Virtual_String :=
                          Handler.Key_Name;
                     begin
                        Handler.Read_Next;
                        case Location_Or_Something_Map.Get_Index (Key) is
                           when 2 =>  --  uri
                              Value.uri :=
                                (Handler.String_Value with null record);
                              Handler.Read_Next;
                           when others =>
                              Handler.Skip_Current_Value;
                        end case;
                     end;
                  end loop;

                  Handler.Read_Next;
            end case;
         end;
      end Read_Location_Or_Something;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case WorkspaceSymbol_Map.Get_Index (Key) is
               when 1 =>  --  name
                  Value.name.Clear;
                  Value.name.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 2 =>  --  kind
                  Read_SymbolKind (Handler, Value.kind);
               when 3 =>  --  tags
                  Read_SymbolTag_Set (Handler, Value.tags);
               when 4 =>  --  containerName
                  Value.containerName.Clear;
                  Value.containerName.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 5 =>  --  location
                  Read_Location_Or_Something (Handler, Value.location);
               when 6 =>  --  data
                  Read_LSPAny (Handler, Value.data);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_WorkspaceSymbol;

   package DeleteFileOptions_Scope is
      package DeleteFileOptions_Map is new Minimal_Perfect_Hash
        (["recursive",
         "ignoreIfNotExists"]);

   end DeleteFileOptions_Scope;

   procedure Read_DeleteFileOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DeleteFileOptions) is
      use DeleteFileOptions_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case DeleteFileOptions_Map.Get_Index (Key) is
               when 1 =>  --  recursive
                  Value.recursive       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.recursive.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 2 =>  --  ignoreIfNotExists
                  Value.ignoreIfNotExists       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.ignoreIfNotExists.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_DeleteFileOptions;

   procedure Read_WatchKind
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.WatchKind) is
   begin
      Value :=
        LSP.Enumerations.WatchKind'Val
          (Handler.Number_Value.Integer_Value - 1);
      Handler.Read_Next;
   end Read_WatchKind;

   package TypeHierarchyRegistrationOptions_Scope is
      package TypeHierarchyRegistrationOptions_Map is new Minimal_Perfect_Hash
        (["documentSelector",
         "workDoneProgress",
         "id"]);

   end TypeHierarchyRegistrationOptions_Scope;

   procedure Read_TypeHierarchyRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TypeHierarchyRegistrationOptions) is
      use TypeHierarchyRegistrationOptions_Scope;
      procedure Read_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.DocumentSelector_Or_Null);

      procedure Read_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.DocumentSelector_Or_Null) is
      begin
         if Handler.Is_Null_Value then
            Handler.Read_Next;
         else
            Value :=
              (Is_Null => False,
               Value   => <>);
            Read_DocumentSelector (Handler, Value.Value);
         end if;
      end Read_DocumentSelector_Or_Null;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case TypeHierarchyRegistrationOptions_Map.Get_Index (Key) is
               when 1 =>  --  documentSelector
                  Read_DocumentSelector_Or_Null
                    (Handler, Value.Parent.documentSelector);
               when 2 =>  --  workDoneProgress
                  Value.workDoneProgress       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.workDoneProgress.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 3 =>  --  id
                  Value.id.Clear;
                  Value.id.Append (Handler.String_Value);
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_TypeHierarchyRegistrationOptions;

   procedure Read_DiagnosticSeverity
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.DiagnosticSeverity) is
   begin
      Value :=
        LSP.Enumerations.DiagnosticSeverity'Val
          (Handler.Number_Value.Integer_Value - 1);
      Handler.Read_Next;
   end Read_DiagnosticSeverity;

   procedure Read_LSPAny_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.LSPAny_Vector) is
   begin
      LSP.Input_Tools.Read_LSPAny_Class (Handler, Value);
   end Read_LSPAny_Vector;

   package PrepareRenameParams_Scope is
      package PrepareRenameParams_Map is new Minimal_Perfect_Hash
        (["textDocument",
         "position",
         "workDoneToken"]);

   end PrepareRenameParams_Scope;

   procedure Read_PrepareRenameParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.PrepareRenameParams) is
      use PrepareRenameParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case PrepareRenameParams_Map.Get_Index (Key) is
               when 1 =>  --  textDocument
                  Read_TextDocumentIdentifier (Handler, Value.textDocument);
               when 2 =>  --  position
                  Read_Position (Handler, Value.position);
               when 3 =>  --  workDoneToken
                  Value.workDoneToken :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_ProgressToken (Handler, Value.workDoneToken.Value);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_PrepareRenameParams;

   procedure Read_CodeActionTriggerKind
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.CodeActionTriggerKind) is
   begin
      Value :=
        LSP.Enumerations.CodeActionTriggerKind'Val
          (Handler.Number_Value.Integer_Value - 1);
      Handler.Read_Next;
   end Read_CodeActionTriggerKind;

   package CodeActionClientCapabilities_Scope is
      package CodeActionClientCapabilities_Map is new Minimal_Perfect_Hash
        (["dynamicRegistration",
         "codeActionLiteralSupport",
         "isPreferredSupport",
         "disabledSupport",
         "dataSupport",
         "resolveSupport",
         "honorsChangeAnnotations"]);

      package codeActionKind_OfcodeActionLiteralSupport_OfCodeActionClientCapabilities_Scope
      is
         package codeActionKind_OfcodeActionLiteralSupport_OfCodeActionClientCapabilities_Map is new Minimal_Perfect_Hash
           (["valueSet"]);

      end codeActionKind_OfcodeActionLiteralSupport_OfCodeActionClientCapabilities_Scope;

      package codeActionLiteralSupport_OfCodeActionClientCapabilities_Scope is
         package codeActionLiteralSupport_OfCodeActionClientCapabilities_Map is new Minimal_Perfect_Hash
           (["codeActionKind"]);

      end codeActionLiteralSupport_OfCodeActionClientCapabilities_Scope;

   end CodeActionClientCapabilities_Scope;

   procedure Read_CodeActionClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CodeActionClientCapabilities) is
      use CodeActionClientCapabilities_Scope;
      procedure Read_codeActionKind_OfcodeActionLiteralSupport_OfCodeActionClientCapabilities
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .codeActionKind_OfcodeActionLiteralSupport_OfCodeActionClientCapabilities);

      procedure Read_codeActionLiteralSupport_OfCodeActionClientCapabilities
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .codeActionLiteralSupport_OfCodeActionClientCapabilities);

      procedure Read_codeActionKind_OfcodeActionLiteralSupport_OfCodeActionClientCapabilities
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .codeActionKind_OfcodeActionLiteralSupport_OfCodeActionClientCapabilities) is
      begin
         pragma Assert (Handler.Is_Start_Object);
         Handler.Read_Next;

         while Handler.Is_Key_Name loop
            declare
               use
                 codeActionKind_OfcodeActionLiteralSupport_OfCodeActionClientCapabilities_Scope;
               Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
            begin
               Handler.Read_Next;
               case codeActionKind_OfcodeActionLiteralSupport_OfCodeActionClientCapabilities_Map
                 .Get_Index
                 (Key) is
                  when 1 =>  --  valueSet
                     Read_CodeActionKind_Set (Handler, Value.valueSet);
                  when others =>
                     Handler.Skip_Current_Value;
               end case;
            end;
         end loop;

         Handler.Read_Next;
      end Read_codeActionKind_OfcodeActionLiteralSupport_OfCodeActionClientCapabilities;

      procedure Read_codeActionLiteralSupport_OfCodeActionClientCapabilities
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .codeActionLiteralSupport_OfCodeActionClientCapabilities) is
      begin
         pragma Assert (Handler.Is_Start_Object);
         Handler.Read_Next;

         while Handler.Is_Key_Name loop
            declare
               use
                 codeActionLiteralSupport_OfCodeActionClientCapabilities_Scope;
               Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
            begin
               Handler.Read_Next;
               case codeActionLiteralSupport_OfCodeActionClientCapabilities_Map
                 .Get_Index
                 (Key) is
                  when 1 =>  --  codeActionKind
                     Read_codeActionKind_OfcodeActionLiteralSupport_OfCodeActionClientCapabilities
                       (Handler, Value.codeActionKind);
                  when others =>
                     Handler.Skip_Current_Value;
               end case;
            end;
         end loop;

         Handler.Read_Next;
      end Read_codeActionLiteralSupport_OfCodeActionClientCapabilities;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case CodeActionClientCapabilities_Map.Get_Index (Key) is
               when 1 =>  --  dynamicRegistration
                  Value.dynamicRegistration       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.dynamicRegistration.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 2 =>  --  codeActionLiteralSupport
                  Value.codeActionLiteralSupport :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_codeActionLiteralSupport_OfCodeActionClientCapabilities
                    (Handler, Value.codeActionLiteralSupport.Value);
               when 3 =>  --  isPreferredSupport
                  Value.isPreferredSupport       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.isPreferredSupport.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 4 =>  --  disabledSupport
                  Value.disabledSupport       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.disabledSupport.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 5 =>  --  dataSupport
                  Value.dataSupport       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.dataSupport.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 6 =>  --  resolveSupport
                  Value.resolveSupport :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_resolveSupport_OfWorkspaceSymbolClientCapabilities
                    (Handler, Value.resolveSupport.Value);
               when 7 =>  --  honorsChangeAnnotations
                  Value.honorsChangeAnnotations       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.honorsChangeAnnotations.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_CodeActionClientCapabilities;

   package MarkdownClientCapabilities_Scope is
      package MarkdownClientCapabilities_Map is new Minimal_Perfect_Hash
        (["parser",
         "version",
         "allowedTags"]);

   end MarkdownClientCapabilities_Scope;

   procedure Read_MarkdownClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.MarkdownClientCapabilities) is
      use MarkdownClientCapabilities_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case MarkdownClientCapabilities_Map.Get_Index (Key) is
               when 1 =>  --  parser
                  Value.parser.Clear;
                  Value.parser.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 2 =>  --  version
                  Value.version.Clear;
                  Value.version.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 3 =>  --  allowedTags
                  Read_Virtual_String_Vector (Handler, Value.allowedTags);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_MarkdownClientCapabilities;

   package CancelParams_Scope is
      package CancelParams_Map is new Minimal_Perfect_Hash (["id"]);

   end CancelParams_Scope;

   procedure Read_CancelParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CancelParams) is
      use CancelParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case CancelParams_Map.Get_Index (Key) is
               when 1 =>  --  id
                  Read_Integer_Or_Virtual_String (Handler, Value.id);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_CancelParams;

   procedure Read_DefinitionLink_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DefinitionLink_Vector) is
   begin
      pragma Assert (Handler.Is_Start_Array);
      Handler.Read_Next;

      declare
         Set   : LSP.Structures.DefinitionLink_Vector renames Value;
         Value : LSP.Structures.DefinitionLink;
      begin
         Set.Clear;
         while not Handler.Is_End_Array loop
            Read_DefinitionLink (Handler, Value);
            Set.Append (Value);
         end loop;
      end;

      Handler.Read_Next;
   end Read_DefinitionLink_Vector;

   package DefinitionParams_Scope is
      package DefinitionParams_Map is new Minimal_Perfect_Hash
        (["textDocument",
         "position",
         "workDoneToken",
         "partialResultToken"]);

   end DefinitionParams_Scope;

   procedure Read_DefinitionParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DefinitionParams) is
      use DefinitionParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case DefinitionParams_Map.Get_Index (Key) is
               when 1 =>  --  textDocument
                  Read_TextDocumentIdentifier (Handler, Value.textDocument);
               when 2 =>  --  position
                  Read_Position (Handler, Value.position);
               when 3 =>  --  workDoneToken
                  Value.workDoneToken :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_ProgressToken (Handler, Value.workDoneToken.Value);
               when 4 =>  --  partialResultToken
                  Value.partialResultToken :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_ProgressToken (Handler, Value.partialResultToken.Value);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_DefinitionParams;

   package ImplementationClientCapabilities_Scope is
      package ImplementationClientCapabilities_Map is new Minimal_Perfect_Hash
        (["dynamicRegistration",
         "linkSupport"]);

   end ImplementationClientCapabilities_Scope;

   procedure Read_ImplementationClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ImplementationClientCapabilities) is
      use ImplementationClientCapabilities_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case ImplementationClientCapabilities_Map.Get_Index (Key) is
               when 1 =>  --  dynamicRegistration
                  Value.dynamicRegistration       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.dynamicRegistration.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 2 =>  --  linkSupport
                  Value.linkSupport       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.linkSupport.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_ImplementationClientCapabilities;

   package TextDocumentItem_Scope is
      package TextDocumentItem_Map is new Minimal_Perfect_Hash
        (["uri",
         "languageId",
         "version",
         "text"]);

   end TextDocumentItem_Scope;

   procedure Read_TextDocumentItem
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TextDocumentItem) is
      use TextDocumentItem_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case TextDocumentItem_Map.Get_Index (Key) is
               when 1 =>  --  uri
                  Value.uri := (Handler.String_Value with null record);
                  Handler.Read_Next;
               when 2 =>  --  languageId
                  Value.languageId.Clear;
                  Value.languageId.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 3 =>  --  version
                  Value.version :=
                    Integer (Handler.Number_Value.Integer_Value);
                  Handler.Read_Next;
               when 4 =>  --  text
                  Value.text.Clear;
                  Value.text.Append (Handler.String_Value);
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_TextDocumentItem;

   package ColorPresentationParams_Scope is
      package ColorPresentationParams_Map is new Minimal_Perfect_Hash
        (["workDoneToken",
         "partialResultToken",
         "textDocument",
         "color",
         "range"]);

   end ColorPresentationParams_Scope;

   procedure Read_ColorPresentationParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ColorPresentationParams) is
      use ColorPresentationParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case ColorPresentationParams_Map.Get_Index (Key) is
               when 1 =>  --  workDoneToken
                  Value.workDoneToken :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_ProgressToken (Handler, Value.workDoneToken.Value);
               when 2 =>  --  partialResultToken
                  Value.partialResultToken :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_ProgressToken (Handler, Value.partialResultToken.Value);
               when 3 =>  --  textDocument
                  Read_TextDocumentIdentifier (Handler, Value.textDocument);
               when 4 =>  --  color
                  Read_Color (Handler, Value.color);
               when 5 =>  --  range
                  Read_A_Range (Handler, Value.a_range);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_ColorPresentationParams;

   procedure Read_ErrorCodes
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.ErrorCodes) is
   begin
      Value :=
        LSP.Enumerations.ErrorCodes'Val
          (Handler.Number_Value.Integer_Value + 32_700);
      Handler.Read_Next;
   end Read_ErrorCodes;

   procedure Read_InsertTextFormat
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.InsertTextFormat) is
   begin
      Value :=
        LSP.Enumerations.InsertTextFormat'Val
          (Handler.Number_Value.Integer_Value - 1);
      Handler.Read_Next;
   end Read_InsertTextFormat;

   package relatedDocuments_OfDocumentDiagnosticReportPartialResult_Item_Scope
   is
      package relatedDocuments_OfDocumentDiagnosticReportPartialResult_Item_Map is new Minimal_Perfect_Hash
        (["full",
         "unchanged"]);

   end relatedDocuments_OfDocumentDiagnosticReportPartialResult_Item_Scope;

   procedure Read_relatedDocuments_OfDocumentDiagnosticReportPartialResult_Item
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures
        .relatedDocuments_OfDocumentDiagnosticReportPartialResult_Item) is
      use relatedDocuments_OfDocumentDiagnosticReportPartialResult_Item_Scope;
   begin
      declare
         Parent  :
           constant not null access VSS.JSON.Pull_Readers.JSON_Pull_Reader'
             Class :=
           Handler'Access;
         Handler :
           VSS.JSON.Pull_Readers.Buffered.JSON_Buffered_Pull_Reader (Parent);
         Kind    : Natural;
      begin
         Handler.Mark;
         pragma Assert (Handler.Is_Start_Object);
         Handler.Read_Next;
         while Handler.Is_Key_Name loop
            declare
               use type VSS.Strings.Virtual_String;
               Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
            begin
               Handler.Read_Next;
               if Key = "kind" then
                  pragma Assert (Handler.Is_String_Value);
                  Kind :=
                    relatedDocuments_OfDocumentDiagnosticReportPartialResult_Item_Map
                      .Get_Index
                      (Handler.String_Value);
                  case Kind is
                     when 1 =>  --  full
                        Value :=
                          (Kind   => LSP.Structures.full,
                           others => <>);
                     when 2 =>  --  unchanged
                        Value :=
                          (Kind   => LSP.Structures.unchanged,
                           others => <>);
                     when others =>
                        raise Constraint_Error;
                  end case;
                  exit;
               else
                  Handler.Skip_Current_Value;
               end if;
            end;
         end loop;

         Handler.Reset;
         Handler.Unmark;

         case Value.Kind is
            when LSP.Structures.full =>
               Read_FullDocumentDiagnosticReport (Handler, Value.full);
            when LSP.Structures.unchanged =>
               Read_UnchangedDocumentDiagnosticReport
                 (Handler, Value.unchanged);
         end case;
      end;
   end Read_relatedDocuments_OfDocumentDiagnosticReportPartialResult_Item;

   package ExecuteCommandClientCapabilities_Scope is
      package ExecuteCommandClientCapabilities_Map is new Minimal_Perfect_Hash
        (["dynamicRegistration"]);

   end ExecuteCommandClientCapabilities_Scope;

   procedure Read_ExecuteCommandClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ExecuteCommandClientCapabilities) is
      use ExecuteCommandClientCapabilities_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case ExecuteCommandClientCapabilities_Map.Get_Index (Key) is
               when 1 =>  --  dynamicRegistration
                  Value.dynamicRegistration       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.dynamicRegistration.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_ExecuteCommandClientCapabilities;

   procedure Read_FoldingRange_Vector_Or_Null
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.FoldingRange_Vector_Or_Null) is
   begin
      if Handler.Is_Null_Value then
         Handler.Read_Next;
      else
         Value.Clear;
         Read_FoldingRange_Vector (Handler, Value);
      end if;
   end Read_FoldingRange_Vector_Or_Null;

   procedure Read_CallHierarchyItem_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CallHierarchyItem_Vector) is
   begin
      pragma Assert (Handler.Is_Start_Array);
      Handler.Read_Next;

      declare
         Set   : LSP.Structures.CallHierarchyItem_Vector renames Value;
         Value : LSP.Structures.CallHierarchyItem;
      begin
         Set.Clear;
         while not Handler.Is_End_Array loop
            Read_CallHierarchyItem (Handler, Value);
            Set.Append (Value);
         end loop;
      end;

      Handler.Read_Next;
   end Read_CallHierarchyItem_Vector;

   package DeleteFile_Scope is
      package DeleteFile_Map is new Minimal_Perfect_Hash
        (["kind",
         "annotationId",
         "uri",
         "options"]);

   end DeleteFile_Scope;

   procedure Read_DeleteFile
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DeleteFile) is
      use DeleteFile_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case DeleteFile_Map.Get_Index (Key) is
               when 2 =>  --  annotationId
                  Value.annotationId :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_ChangeAnnotationIdentifier
                    (Handler, Value.annotationId.Value);
               when 1 =>  --  kind
                  Handler.Read_Next;  --  Skip string literal: delete
               when 3 =>  --  uri
                  Value.uri := (Handler.String_Value with null record);
                  Handler.Read_Next;
               when 4 =>  --  options
                  Value.options :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_DeleteFileOptions (Handler, Value.options.Value);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_DeleteFile;

   package SaveOptions_Scope is
      package SaveOptions_Map is new Minimal_Perfect_Hash (["includeText"]);

   end SaveOptions_Scope;

   procedure Read_SaveOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SaveOptions) is
      use SaveOptions_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case SaveOptions_Map.Get_Index (Key) is
               when 1 =>  --  includeText
                  Value.includeText       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.includeText.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_SaveOptions;

   package NotebookDocument_Scope is
      package NotebookDocument_Map is new Minimal_Perfect_Hash
        (["uri",
         "notebookType",
         "version",
         "metadata",
         "cells"]);

   end NotebookDocument_Scope;

   procedure Read_NotebookDocument
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.NotebookDocument) is
      use NotebookDocument_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case NotebookDocument_Map.Get_Index (Key) is
               when 1 =>  --  uri
                  Read_URI (Handler, Value.uri);
               when 2 =>  --  notebookType
                  Value.notebookType.Clear;
                  Value.notebookType.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 3 =>  --  version
                  Value.version :=
                    Integer (Handler.Number_Value.Integer_Value);
                  Handler.Read_Next;
               when 4 =>  --  metadata
                  Value.metadata :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_LSPObject (Handler, Value.metadata.Value);
               when 5 =>  --  cells
                  Read_NotebookCell_Vector (Handler, Value.cells);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_NotebookDocument;

   package RelatedFullDocumentDiagnosticReport_Scope is
      package RelatedFullDocumentDiagnosticReport_Map is new Minimal_Perfect_Hash
        (["kind",
         "resultId",
         "items",
         "relatedDocuments"]);

   end RelatedFullDocumentDiagnosticReport_Scope;

   procedure Read_RelatedFullDocumentDiagnosticReport
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.RelatedFullDocumentDiagnosticReport) is
      use RelatedFullDocumentDiagnosticReport_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case RelatedFullDocumentDiagnosticReport_Map.Get_Index (Key) is
               when 1 =>  --  kind
                  Handler.Read_Next;  --  Skip string literal: full
               when 2 =>  --  resultId
                  Value.resultId.Clear;
                  Value.resultId.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 3 =>  --  items
                  Read_Diagnostic_Vector (Handler, Value.items);
               when 4 =>  --  relatedDocuments
                  pragma Assert (Handler.Is_Start_Object);
                  Handler.Read_Next;

                  while not Handler.Is_End_Object loop
                     declare
                        Map   :
                          LSP.Structures
                            .relatedDocuments_OfDocumentDiagnosticReportPartialResult renames
                          Value.relatedDocuments;
                        Key   : LSP.Structures.DocumentUri;
                        Value :
                          LSP.Structures
                            .relatedDocuments_OfDocumentDiagnosticReportPartialResult_Item;
                     begin
                        Key := (Handler.Key_Name with null record);
                        Handler.Read_Next;
                        Read_relatedDocuments_OfDocumentDiagnosticReportPartialResult_Item
                          (Handler, Value);
                        Map.Insert (Key, Value);
                     end;
                  end loop;

                  Handler.Read_Next;

               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_RelatedFullDocumentDiagnosticReport;

   package CodeLensClientCapabilities_Scope is
      package CodeLensClientCapabilities_Map is new Minimal_Perfect_Hash
        (["dynamicRegistration"]);

   end CodeLensClientCapabilities_Scope;

   procedure Read_CodeLensClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CodeLensClientCapabilities) is
      use CodeLensClientCapabilities_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case CodeLensClientCapabilities_Map.Get_Index (Key) is
               when 1 =>  --  dynamicRegistration
                  Value.dynamicRegistration       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.dynamicRegistration.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_CodeLensClientCapabilities;

   package DocumentSymbolOptions_Scope is
      package DocumentSymbolOptions_Map is new Minimal_Perfect_Hash
        (["workDoneProgress",
         "label"]);

   end DocumentSymbolOptions_Scope;

   procedure Read_DocumentSymbolOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentSymbolOptions) is
      use DocumentSymbolOptions_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case DocumentSymbolOptions_Map.Get_Index (Key) is
               when 1 =>  --  workDoneProgress
                  Value.workDoneProgress       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.workDoneProgress.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 2 =>  --  label
                  Value.label.Clear;
                  Value.label.Append (Handler.String_Value);
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_DocumentSymbolOptions;

   package CompletionRegistrationOptions_Scope is
      package CompletionRegistrationOptions_Map is new Minimal_Perfect_Hash
        (["documentSelector",
         "workDoneProgress",
         "triggerCharacters",
         "allCommitCharacters",
         "resolveProvider",
         "completionItem"]);

      package completionItem_OfCompletionOptions_Scope is
         package completionItem_OfCompletionOptions_Map is new Minimal_Perfect_Hash
           (["labelDetailsSupport"]);

      end completionItem_OfCompletionOptions_Scope;

   end CompletionRegistrationOptions_Scope;

   procedure Read_CompletionRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CompletionRegistrationOptions) is
      use CompletionRegistrationOptions_Scope;
      procedure Read_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.DocumentSelector_Or_Null);

      procedure Read_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.DocumentSelector_Or_Null) is
      begin
         if Handler.Is_Null_Value then
            Handler.Read_Next;
         else
            Value :=
              (Is_Null => False,
               Value   => <>);
            Read_DocumentSelector (Handler, Value.Value);
         end if;
      end Read_DocumentSelector_Or_Null;

      procedure Read_completionItem_OfCompletionOptions
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.completionItem_OfCompletionOptions);

      procedure Read_completionItem_OfCompletionOptions
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.completionItem_OfCompletionOptions) is
      begin
         pragma Assert (Handler.Is_Start_Object);
         Handler.Read_Next;

         while Handler.Is_Key_Name loop
            declare
               use completionItem_OfCompletionOptions_Scope;
               Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
            begin
               Handler.Read_Next;
               case completionItem_OfCompletionOptions_Map.Get_Index (Key) is
                  when 1 =>  --  labelDetailsSupport
                     Value.labelDetailsSupport       :=
                       (Is_Set => True,
                        Value  => <>);
                     Value.labelDetailsSupport.Value := Handler.Boolean_Value;
                     Handler.Read_Next;
                  when others =>
                     Handler.Skip_Current_Value;
               end case;
            end;
         end loop;

         Handler.Read_Next;
      end Read_completionItem_OfCompletionOptions;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case CompletionRegistrationOptions_Map.Get_Index (Key) is
               when 1 =>  --  documentSelector
                  Read_DocumentSelector_Or_Null
                    (Handler, Value.Parent.documentSelector);
               when 2 =>  --  workDoneProgress
                  Value.workDoneProgress       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.workDoneProgress.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 3 =>  --  triggerCharacters
                  Read_Virtual_String_Vector
                    (Handler, Value.triggerCharacters);
               when 4 =>  --  allCommitCharacters
                  Read_Virtual_String_Vector
                    (Handler, Value.allCommitCharacters);
               when 5 =>  --  resolveProvider
                  Value.resolveProvider       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.resolveProvider.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 6 =>  --  completionItem
                  Value.completionItem :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_completionItem_OfCompletionOptions
                    (Handler, Value.completionItem.Value);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_CompletionRegistrationOptions;

   package InlayHintClientCapabilities_Scope is
      package InlayHintClientCapabilities_Map is new Minimal_Perfect_Hash
        (["dynamicRegistration",
         "resolveSupport"]);

   end InlayHintClientCapabilities_Scope;

   procedure Read_InlayHintClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.InlayHintClientCapabilities) is
      use InlayHintClientCapabilities_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case InlayHintClientCapabilities_Map.Get_Index (Key) is
               when 1 =>  --  dynamicRegistration
                  Value.dynamicRegistration       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.dynamicRegistration.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 2 =>  --  resolveSupport
                  Value.resolveSupport :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_resolveSupport_OfWorkspaceSymbolClientCapabilities
                    (Handler, Value.resolveSupport.Value);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_InlayHintClientCapabilities;

   package ReferenceContext_Scope is
      package ReferenceContext_Map is new Minimal_Perfect_Hash
        (["includeDeclaration"]);

   end ReferenceContext_Scope;

   procedure Read_ReferenceContext
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ReferenceContext) is
      use ReferenceContext_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case ReferenceContext_Map.Get_Index (Key) is
               when 1 =>  --  includeDeclaration
                  Value.includeDeclaration := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_ReferenceContext;

   package WorkspaceEditClientCapabilities_Scope is
      package WorkspaceEditClientCapabilities_Map is new Minimal_Perfect_Hash
        (["documentChanges",
         "resourceOperations",
         "failureHandling",
         "normalizesLineEndings",
         "changeAnnotationSupport"]);

      package changeAnnotationSupport_OfWorkspaceEditClientCapabilities_Scope
      is
         package changeAnnotationSupport_OfWorkspaceEditClientCapabilities_Map is new Minimal_Perfect_Hash
           (["groupsOnLabel"]);

      end changeAnnotationSupport_OfWorkspaceEditClientCapabilities_Scope;

   end WorkspaceEditClientCapabilities_Scope;

   procedure Read_WorkspaceEditClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.WorkspaceEditClientCapabilities) is
      use WorkspaceEditClientCapabilities_Scope;
      procedure Read_ResourceOperationKind_Set
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.ResourceOperationKind_Set);

      procedure Read_changeAnnotationSupport_OfWorkspaceEditClientCapabilities
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .changeAnnotationSupport_OfWorkspaceEditClientCapabilities);

      procedure Read_ResourceOperationKind_Set
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.ResourceOperationKind_Set) is
      begin
         pragma Assert (Handler.Is_Start_Array);
         Handler.Read_Next;

         declare
            Set   : LSP.Structures.ResourceOperationKind_Set renames Value;
            Value : LSP.Enumerations.ResourceOperationKind;
         begin
            Set := (others => False);
            while not Handler.Is_End_Array loop
               Read_ResourceOperationKind (Handler, Value);
               Set (Value) := True;
            end loop;
         end;

         Handler.Read_Next;
      end Read_ResourceOperationKind_Set;

      procedure Read_changeAnnotationSupport_OfWorkspaceEditClientCapabilities
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .changeAnnotationSupport_OfWorkspaceEditClientCapabilities) is
      begin
         pragma Assert (Handler.Is_Start_Object);
         Handler.Read_Next;

         while Handler.Is_Key_Name loop
            declare
               use
                 changeAnnotationSupport_OfWorkspaceEditClientCapabilities_Scope;
               Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
            begin
               Handler.Read_Next;
               case changeAnnotationSupport_OfWorkspaceEditClientCapabilities_Map
                 .Get_Index
                 (Key) is
                  when 1 =>  --  groupsOnLabel
                     Value.groupsOnLabel       :=
                       (Is_Set => True,
                        Value  => <>);
                     Value.groupsOnLabel.Value := Handler.Boolean_Value;
                     Handler.Read_Next;
                  when others =>
                     Handler.Skip_Current_Value;
               end case;
            end;
         end loop;

         Handler.Read_Next;
      end Read_changeAnnotationSupport_OfWorkspaceEditClientCapabilities;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case WorkspaceEditClientCapabilities_Map.Get_Index (Key) is
               when 1 =>  --  documentChanges
                  Value.documentChanges       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.documentChanges.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 2 =>  --  resourceOperations
                  Read_ResourceOperationKind_Set
                    (Handler, Value.resourceOperations);
               when 3 =>  --  failureHandling
                  Value.failureHandling :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_FailureHandlingKind
                    (Handler, Value.failureHandling.Value);
               when 4 =>  --  normalizesLineEndings
                  Value.normalizesLineEndings       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.normalizesLineEndings.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 5 =>  --  changeAnnotationSupport
                  Value.changeAnnotationSupport :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_changeAnnotationSupport_OfWorkspaceEditClientCapabilities
                    (Handler, Value.changeAnnotationSupport.Value);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_WorkspaceEditClientCapabilities;

   package WorkspaceSymbolOptions_Scope is
      package WorkspaceSymbolOptions_Map is new Minimal_Perfect_Hash
        (["workDoneProgress",
         "resolveProvider"]);

   end WorkspaceSymbolOptions_Scope;

   procedure Read_WorkspaceSymbolOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.WorkspaceSymbolOptions) is
      use WorkspaceSymbolOptions_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case WorkspaceSymbolOptions_Map.Get_Index (Key) is
               when 1 =>  --  workDoneProgress
                  Value.workDoneProgress       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.workDoneProgress.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 2 =>  --  resolveProvider
                  Value.resolveProvider       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.resolveProvider.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_WorkspaceSymbolOptions;

   procedure Read_MessageActionItem_Or_Null
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.MessageActionItem_Or_Null) is
   begin
      if Handler.Is_Null_Value then
         Handler.Read_Next;
      else
         Value :=
           (Is_Null => False,
            Value   => <>);
         Read_MessageActionItem (Handler, Value.Value);
      end if;
   end Read_MessageActionItem_Or_Null;

   package ChangeAnnotation_Scope is
      package ChangeAnnotation_Map is new Minimal_Perfect_Hash
        (["label",
         "needsConfirmation",
         "description"]);

   end ChangeAnnotation_Scope;

   procedure Read_ChangeAnnotation
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ChangeAnnotation) is
      use ChangeAnnotation_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case ChangeAnnotation_Map.Get_Index (Key) is
               when 1 =>  --  label
                  Value.label.Clear;
                  Value.label.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 2 =>  --  needsConfirmation
                  Value.needsConfirmation       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.needsConfirmation.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 3 =>  --  description
                  Value.description.Clear;
                  Value.description.Append (Handler.String_Value);
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_ChangeAnnotation;

   package FileOperationPatternOptions_Scope is
      package FileOperationPatternOptions_Map is new Minimal_Perfect_Hash
        (["ignoreCase"]);

   end FileOperationPatternOptions_Scope;

   procedure Read_FileOperationPatternOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.FileOperationPatternOptions) is
      use FileOperationPatternOptions_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case FileOperationPatternOptions_Map.Get_Index (Key) is
               when 1 =>  --  ignoreCase
                  Value.ignoreCase       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.ignoreCase.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_FileOperationPatternOptions;

   package VersionedTextDocumentIdentifier_Scope is
      package VersionedTextDocumentIdentifier_Map is new Minimal_Perfect_Hash
        (["uri",
         "version"]);

   end VersionedTextDocumentIdentifier_Scope;

   procedure Read_VersionedTextDocumentIdentifier
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.VersionedTextDocumentIdentifier) is
      use VersionedTextDocumentIdentifier_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case VersionedTextDocumentIdentifier_Map.Get_Index (Key) is
               when 1 =>  --  uri
                  Value.uri := (Handler.String_Value with null record);
                  Handler.Read_Next;
               when 2 =>  --  version
                  Value.version :=
                    Integer (Handler.Number_Value.Integer_Value);
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_VersionedTextDocumentIdentifier;

   package SemanticTokensDelta_Scope is
      package SemanticTokensDelta_Map is new Minimal_Perfect_Hash
        (["resultId",
         "edits"]);

   end SemanticTokensDelta_Scope;

   procedure Read_SemanticTokensDelta
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SemanticTokensDelta) is
      use SemanticTokensDelta_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case SemanticTokensDelta_Map.Get_Index (Key) is
               when 1 =>  --  resultId
                  Value.resultId.Clear;
                  Value.resultId.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 2 =>  --  edits
                  Read_SemanticTokensEdit_Vector (Handler, Value.edits);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_SemanticTokensDelta;

   package ShowDocumentClientCapabilities_Scope is
      package ShowDocumentClientCapabilities_Map is new Minimal_Perfect_Hash
        (["support"]);

   end ShowDocumentClientCapabilities_Scope;

   procedure Read_ShowDocumentClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ShowDocumentClientCapabilities) is
      use ShowDocumentClientCapabilities_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case ShowDocumentClientCapabilities_Map.Get_Index (Key) is
               when 1 =>  --  support
                  Value.support := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_ShowDocumentClientCapabilities;

   package ApplyWorkspaceEditParams_Scope is
      package ApplyWorkspaceEditParams_Map is new Minimal_Perfect_Hash
        (["label",
         "edit"]);

   end ApplyWorkspaceEditParams_Scope;

   procedure Read_ApplyWorkspaceEditParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ApplyWorkspaceEditParams) is
      use ApplyWorkspaceEditParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case ApplyWorkspaceEditParams_Map.Get_Index (Key) is
               when 1 =>  --  label
                  Value.label.Clear;
                  Value.label.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 2 =>  --  edit
                  Read_WorkspaceEdit (Handler, Value.edit);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_ApplyWorkspaceEditParams;

   package InlayHintLabelPart_Scope is
      package InlayHintLabelPart_Map is new Minimal_Perfect_Hash
        (["value",
         "tooltip",
         "location",
         "command"]);

   end InlayHintLabelPart_Scope;

   procedure Read_InlayHintLabelPart
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.InlayHintLabelPart) is
      use InlayHintLabelPart_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case InlayHintLabelPart_Map.Get_Index (Key) is
               when 1 =>  --  value
                  Value.value.Clear;
                  Value.value.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 2 =>  --  tooltip
                  Value.tooltip :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_Virtual_String_Or_MarkupContent
                    (Handler, Value.tooltip.Value);
               when 3 =>  --  location
                  Value.location :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_Location (Handler, Value.location.Value);
               when 4 =>  --  command
                  Value.command :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_Command (Handler, Value.command.Value);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_InlayHintLabelPart;

   package SelectionRangeOptions_Scope is
      package SelectionRangeOptions_Map is new Minimal_Perfect_Hash
        (["workDoneProgress"]);

   end SelectionRangeOptions_Scope;

   procedure Read_SelectionRangeOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SelectionRangeOptions) is
      use SelectionRangeOptions_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case SelectionRangeOptions_Map.Get_Index (Key) is
               when 1 =>  --  workDoneProgress
                  Value.workDoneProgress       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.workDoneProgress.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_SelectionRangeOptions;

   package LocationLink_Scope is
      package LocationLink_Map is new Minimal_Perfect_Hash
        (["originSelectionRange",
         "targetUri",
         "targetRange",
         "targetSelectionRange"]);

   end LocationLink_Scope;

   procedure Read_LocationLink
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.LocationLink) is
      use LocationLink_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case LocationLink_Map.Get_Index (Key) is
               when 1 =>  --  originSelectionRange
                  Value.originSelectionRange :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_A_Range (Handler, Value.originSelectionRange.Value);
               when 2 =>  --  targetUri
                  Value.targetUri := (Handler.String_Value with null record);
                  Handler.Read_Next;
               when 3 =>  --  targetRange
                  Read_A_Range (Handler, Value.targetRange);
               when 4 =>  --  targetSelectionRange
                  Read_A_Range (Handler, Value.targetSelectionRange);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_LocationLink;

   package CompletionList_Scope is
      package CompletionList_Map is new Minimal_Perfect_Hash
        (["isIncomplete",
         "itemDefaults",
         "items"]);

      package itemDefaults_OfCompletionList_Scope is
         package itemDefaults_OfCompletionList_Map is new Minimal_Perfect_Hash
           (["commitCharacters",
            "editRange",
            "insertTextFormat",
            "insertTextMode",
            "data"]);

      end itemDefaults_OfCompletionList_Scope;

      package Range_Or_Something_Scope is
         package Range_Or_Something_Map is new Minimal_Perfect_Hash
           (["start",
            "end",
            "insert",
            "replace"]);

      end Range_Or_Something_Scope;

   end CompletionList_Scope;

   procedure Read_CompletionList
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CompletionList) is
      use CompletionList_Scope;
      procedure Read_itemDefaults_OfCompletionList
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.itemDefaults_OfCompletionList);

      procedure Read_Range_Or_Something
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.Range_Or_Something);

      procedure Read_itemDefaults_OfCompletionList
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.itemDefaults_OfCompletionList) is
      begin
         pragma Assert (Handler.Is_Start_Object);
         Handler.Read_Next;

         while Handler.Is_Key_Name loop
            declare
               use itemDefaults_OfCompletionList_Scope;
               Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
            begin
               Handler.Read_Next;
               case itemDefaults_OfCompletionList_Map.Get_Index (Key) is
                  when 1 =>  --  commitCharacters
                     Read_Virtual_String_Vector
                       (Handler, Value.commitCharacters);
                  when 2 =>  --  editRange
                     Value.editRange :=
                       (Is_Set => True,
                        Value  => <>);
                     Read_Range_Or_Something (Handler, Value.editRange.Value);
                  when 3 =>  --  insertTextFormat
                     Value.insertTextFormat :=
                       (Is_Set => True,
                        Value  => <>);
                     Read_InsertTextFormat
                       (Handler, Value.insertTextFormat.Value);
                  when 4 =>  --  insertTextMode
                     Value.insertTextMode :=
                       (Is_Set => True,
                        Value  => <>);
                     Read_InsertTextMode (Handler, Value.insertTextMode.Value);
                  when 5 =>  --  data
                     Read_LSPAny (Handler, Value.data);
                  when others =>
                     Handler.Skip_Current_Value;
               end case;
            end;
         end loop;

         Handler.Read_Next;
      end Read_itemDefaults_OfCompletionList;

      procedure Read_Range_Or_Something
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.Range_Or_Something) is
         use Range_Or_Something_Scope;
      begin
         declare
            Parent  :
              constant not null access VSS.JSON.Pull_Readers.JSON_Pull_Reader'
                Class :=
              Handler'Access;
            Handler :
              VSS.JSON.Pull_Readers.Buffered.JSON_Buffered_Pull_Reader
                (Parent);
         begin
            Handler.Mark;
            if Handler.Is_Start_Object then
               Handler.Read_Next;
               while Handler.Is_Key_Name loop
                  declare
                     Key   : constant VSS.Strings.Virtual_String :=
                       Handler.Key_Name;
                     Index : constant Natural                    :=
                       Range_Or_Something_Map.Get_Index (Key);
                  begin
                     Handler.Read_Next;
                     case Index is
                        when 1 =>  --  start
                           Value :=
                             (Is_A_Range => True,
                              others     => <>);
                           exit;
                        when 2 =>  --  end
                           Value :=
                             (Is_A_Range => True,
                              others     => <>);
                           exit;
                        when 3 =>  --  insert
                           Value :=
                             (Is_A_Range => False,
                              others     => <>);
                           exit;
                        when 4 =>  --  replace
                           Value :=
                             (Is_A_Range => False,
                              others     => <>);
                           exit;
                        when others =>
                           Handler.Skip_Current_Value;
                     end case;
                  end;
               end loop;
            else
               raise Program_Error;  --  Unexpected JSON value
            end if;

            Handler.Reset;
            Handler.Unmark;

            case Value.Is_A_Range is
               when True =>
                  Read_A_Range (Handler, Value.A_Range);
               when False =>
                  pragma Assert (Handler.Is_Start_Object);
                  Handler.Read_Next;

                  while Handler.Is_Key_Name loop
                     declare
                        Key : constant VSS.Strings.Virtual_String :=
                          Handler.Key_Name;
                     begin
                        Handler.Read_Next;
                        case Range_Or_Something_Map.Get_Index (Key) is
                           when 3 =>  --  insert
                              Read_A_Range (Handler, Value.insert);
                           when 4 =>  --  replace
                              Read_A_Range (Handler, Value.replace);
                           when others =>
                              Handler.Skip_Current_Value;
                        end case;
                     end;
                  end loop;

                  Handler.Read_Next;
            end case;
         end;
      end Read_Range_Or_Something;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case CompletionList_Map.Get_Index (Key) is
               when 1 =>  --  isIncomplete
                  Value.isIncomplete := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 2 =>  --  itemDefaults
                  Value.itemDefaults :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_itemDefaults_OfCompletionList
                    (Handler, Value.itemDefaults.Value);
               when 3 =>  --  items
                  Read_CompletionItem_Vector (Handler, Value.items);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_CompletionList;

   package DiagnosticRegistrationOptions_Scope is
      package DiagnosticRegistrationOptions_Map is new Minimal_Perfect_Hash
        (["documentSelector",
         "workDoneProgress",
         "identifier",
         "interFileDependencies",
         "workspaceDiagnostics",
         "id"]);

   end DiagnosticRegistrationOptions_Scope;

   procedure Read_DiagnosticRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DiagnosticRegistrationOptions) is
      use DiagnosticRegistrationOptions_Scope;
      procedure Read_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.DocumentSelector_Or_Null);

      procedure Read_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.DocumentSelector_Or_Null) is
      begin
         if Handler.Is_Null_Value then
            Handler.Read_Next;
         else
            Value :=
              (Is_Null => False,
               Value   => <>);
            Read_DocumentSelector (Handler, Value.Value);
         end if;
      end Read_DocumentSelector_Or_Null;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case DiagnosticRegistrationOptions_Map.Get_Index (Key) is
               when 1 =>  --  documentSelector
                  Read_DocumentSelector_Or_Null
                    (Handler, Value.Parent.documentSelector);
               when 2 =>  --  workDoneProgress
                  Value.workDoneProgress       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.workDoneProgress.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 3 =>  --  identifier
                  Value.identifier.Clear;
                  Value.identifier.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 4 =>  --  interFileDependencies
                  Value.interFileDependencies := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 5 =>  --  workspaceDiagnostics
                  Value.workspaceDiagnostics := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 6 =>  --  id
                  Value.id.Clear;
                  Value.id.Append (Handler.String_Value);
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_DiagnosticRegistrationOptions;

   procedure Read_SignatureHelpTriggerKind
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.SignatureHelpTriggerKind) is
   begin
      Value :=
        LSP.Enumerations.SignatureHelpTriggerKind'Val
          (Handler.Number_Value.Integer_Value - 1);
      Handler.Read_Next;
   end Read_SignatureHelpTriggerKind;

   package CodeAction_Scope is
      package CodeAction_Map is new Minimal_Perfect_Hash
        (["title",
         "kind",
         "diagnostics",
         "isPreferred",
         "disabled",
         "edit",
         "command",
         "data"]);

      package disabled_OfCodeAction_Scope is
         package disabled_OfCodeAction_Map is new Minimal_Perfect_Hash
           (["reason"]);

      end disabled_OfCodeAction_Scope;

   end CodeAction_Scope;

   procedure Read_CodeAction
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CodeAction) is
      use CodeAction_Scope;
      procedure Read_disabled_OfCodeAction
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.disabled_OfCodeAction);

      procedure Read_disabled_OfCodeAction
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.disabled_OfCodeAction) is
      begin
         pragma Assert (Handler.Is_Start_Object);
         Handler.Read_Next;

         while Handler.Is_Key_Name loop
            declare
               use disabled_OfCodeAction_Scope;
               Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
            begin
               Handler.Read_Next;
               case disabled_OfCodeAction_Map.Get_Index (Key) is
                  when 1 =>  --  reason
                     Value.reason.Clear;
                     Value.reason.Append (Handler.String_Value);
                     Handler.Read_Next;
                  when others =>
                     Handler.Skip_Current_Value;
               end case;
            end;
         end loop;

         Handler.Read_Next;
      end Read_disabled_OfCodeAction;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case CodeAction_Map.Get_Index (Key) is
               when 1 =>  --  title
                  Value.title.Clear;
                  Value.title.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 2 =>  --  kind
                  Value.kind :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_CodeActionKind (Handler, Value.kind.Value);
               when 3 =>  --  diagnostics
                  Read_Diagnostic_Vector (Handler, Value.diagnostics);
               when 4 =>  --  isPreferred
                  Value.isPreferred       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.isPreferred.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 5 =>  --  disabled
                  Value.disabled :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_disabled_OfCodeAction (Handler, Value.disabled.Value);
               when 6 =>  --  edit
                  Value.edit :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_WorkspaceEdit (Handler, Value.edit.Value);
               when 7 =>  --  command
                  Value.command :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_Command (Handler, Value.command.Value);
               when 8 =>  --  data
                  Read_LSPAny (Handler, Value.data);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_CodeAction;

   procedure Read_InlayHint_Vector_Or_Null
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.InlayHint_Vector_Or_Null) is
   begin
      if Handler.Is_Null_Value then
         Handler.Read_Next;
      else
         Value.Clear;
         Read_InlayHint_Vector (Handler, Value);
      end if;
   end Read_InlayHint_Vector_Or_Null;

   package DeclarationParams_Scope is
      package DeclarationParams_Map is new Minimal_Perfect_Hash
        (["textDocument",
         "position",
         "workDoneToken",
         "partialResultToken"]);

   end DeclarationParams_Scope;

   procedure Read_DeclarationParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DeclarationParams) is
      use DeclarationParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case DeclarationParams_Map.Get_Index (Key) is
               when 1 =>  --  textDocument
                  Read_TextDocumentIdentifier (Handler, Value.textDocument);
               when 2 =>  --  position
                  Read_Position (Handler, Value.position);
               when 3 =>  --  workDoneToken
                  Value.workDoneToken :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_ProgressToken (Handler, Value.workDoneToken.Value);
               when 4 =>  --  partialResultToken
                  Value.partialResultToken :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_ProgressToken (Handler, Value.partialResultToken.Value);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_DeclarationParams;

   package DocumentRangeFormattingParams_Scope is
      package DocumentRangeFormattingParams_Map is new Minimal_Perfect_Hash
        (["workDoneToken",
         "textDocument",
         "range",
         "options"]);

   end DocumentRangeFormattingParams_Scope;

   procedure Read_DocumentRangeFormattingParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentRangeFormattingParams) is
      use DocumentRangeFormattingParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case DocumentRangeFormattingParams_Map.Get_Index (Key) is
               when 1 =>  --  workDoneToken
                  Value.workDoneToken :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_ProgressToken (Handler, Value.workDoneToken.Value);
               when 2 =>  --  textDocument
                  Read_TextDocumentIdentifier (Handler, Value.textDocument);
               when 3 =>  --  range
                  Read_A_Range (Handler, Value.a_range);
               when 4 =>  --  options
                  Read_FormattingOptions (Handler, Value.options);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_DocumentRangeFormattingParams;

   package TypeHierarchyClientCapabilities_Scope is
      package TypeHierarchyClientCapabilities_Map is new Minimal_Perfect_Hash
        (["dynamicRegistration"]);

   end TypeHierarchyClientCapabilities_Scope;

   procedure Read_TypeHierarchyClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TypeHierarchyClientCapabilities) is
      use TypeHierarchyClientCapabilities_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case TypeHierarchyClientCapabilities_Map.Get_Index (Key) is
               when 1 =>  --  dynamicRegistration
                  Value.dynamicRegistration       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.dynamicRegistration.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_TypeHierarchyClientCapabilities;

   package DocumentOnTypeFormattingOptions_Scope is
      package DocumentOnTypeFormattingOptions_Map is new Minimal_Perfect_Hash
        (["firstTriggerCharacter",
         "moreTriggerCharacter"]);

   end DocumentOnTypeFormattingOptions_Scope;

   procedure Read_DocumentOnTypeFormattingOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentOnTypeFormattingOptions) is
      use DocumentOnTypeFormattingOptions_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case DocumentOnTypeFormattingOptions_Map.Get_Index (Key) is
               when 1 =>  --  firstTriggerCharacter
                  Value.firstTriggerCharacter.Clear;
                  Value.firstTriggerCharacter.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 2 =>  --  moreTriggerCharacter
                  Read_Virtual_String_Vector
                    (Handler, Value.moreTriggerCharacter);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_DocumentOnTypeFormattingOptions;

   package WorkspaceSymbolRegistrationOptions_Scope is
      package WorkspaceSymbolRegistrationOptions_Map is new Minimal_Perfect_Hash
        (["workDoneProgress",
         "resolveProvider"]);

   end WorkspaceSymbolRegistrationOptions_Scope;

   procedure Read_WorkspaceSymbolRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.WorkspaceSymbolRegistrationOptions) is
      use WorkspaceSymbolRegistrationOptions_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case WorkspaceSymbolRegistrationOptions_Map.Get_Index (Key) is
               when 1 =>  --  workDoneProgress
                  Value.workDoneProgress       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.workDoneProgress.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 2 =>  --  resolveProvider
                  Value.resolveProvider       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.resolveProvider.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_WorkspaceSymbolRegistrationOptions;

   package MonikerParams_Scope is
      package MonikerParams_Map is new Minimal_Perfect_Hash
        (["textDocument",
         "position",
         "workDoneToken",
         "partialResultToken"]);

   end MonikerParams_Scope;

   procedure Read_MonikerParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.MonikerParams) is
      use MonikerParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case MonikerParams_Map.Get_Index (Key) is
               when 1 =>  --  textDocument
                  Read_TextDocumentIdentifier (Handler, Value.textDocument);
               when 2 =>  --  position
                  Read_Position (Handler, Value.position);
               when 3 =>  --  workDoneToken
                  Value.workDoneToken :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_ProgressToken (Handler, Value.workDoneToken.Value);
               when 4 =>  --  partialResultToken
                  Value.partialResultToken :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_ProgressToken (Handler, Value.partialResultToken.Value);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_MonikerParams;

   package InlayHint_Scope is
      package InlayHint_Map is new Minimal_Perfect_Hash
        (["position",
         "label",
         "kind",
         "textEdits",
         "tooltip",
         "paddingLeft",
         "paddingRight",
         "data"]);

   end InlayHint_Scope;

   procedure Read_InlayHint
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.InlayHint) is
      use InlayHint_Scope;
      procedure Read_Virtual_String_Or_InlayHintLabelPart_Vector
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .Virtual_String_Or_InlayHintLabelPart_Vector);

      procedure Read_InlayHintLabelPart_Vector
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.InlayHintLabelPart_Vector);

      procedure Read_Virtual_String_Or_InlayHintLabelPart_Vector
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .Virtual_String_Or_InlayHintLabelPart_Vector) is
      begin
         if Handler.Is_String_Value then
            Value :=
              (Is_Virtual_String => True,
               others            => <>);
         else
            Value :=
              (Is_Virtual_String => False,
               others            => <>);
         end if;

         case Value.Is_Virtual_String is
            when True =>
               Value.Virtual_String.Clear;
               Value.Virtual_String.Append (Handler.String_Value);
               Handler.Read_Next;
            when False =>
               Read_InlayHintLabelPart_Vector
                 (Handler, Value.InlayHintLabelPart_Vector);
         end case;
      end Read_Virtual_String_Or_InlayHintLabelPart_Vector;

      procedure Read_InlayHintLabelPart_Vector
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.InlayHintLabelPart_Vector) is
      begin
         pragma Assert (Handler.Is_Start_Array);
         Handler.Read_Next;

         declare
            Set   : LSP.Structures.InlayHintLabelPart_Vector renames Value;
            Value : LSP.Structures.InlayHintLabelPart;
         begin
            Set.Clear;
            while not Handler.Is_End_Array loop
               Read_InlayHintLabelPart (Handler, Value);
               Set.Append (Value);
            end loop;
         end;

         Handler.Read_Next;
      end Read_InlayHintLabelPart_Vector;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case InlayHint_Map.Get_Index (Key) is
               when 1 =>  --  position
                  Read_Position (Handler, Value.position);
               when 2 =>  --  label
                  Read_Virtual_String_Or_InlayHintLabelPart_Vector
                    (Handler, Value.label);
               when 3 =>  --  kind
                  Value.kind :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_InlayHintKind (Handler, Value.kind.Value);
               when 4 =>  --  textEdits
                  Read_TextEdit_Vector (Handler, Value.textEdits);
               when 5 =>  --  tooltip
                  Value.tooltip :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_Virtual_String_Or_MarkupContent
                    (Handler, Value.tooltip.Value);
               when 6 =>  --  paddingLeft
                  Value.paddingLeft       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.paddingLeft.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 7 =>  --  paddingRight
                  Value.paddingRight       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.paddingRight.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 8 =>  --  data
                  Read_LSPAny (Handler, Value.data);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_InlayHint;

   package FileDelete_Scope is
      package FileDelete_Map is new Minimal_Perfect_Hash (["uri"]);

   end FileDelete_Scope;

   procedure Read_FileDelete
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.FileDelete) is
      use FileDelete_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case FileDelete_Map.Get_Index (Key) is
               when 1 =>  --  uri
                  Value.uri.Clear;
                  Value.uri.Append (Handler.String_Value);
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_FileDelete;

   package DocumentFormattingRegistrationOptions_Scope is
      package DocumentFormattingRegistrationOptions_Map is new Minimal_Perfect_Hash
        (["documentSelector",
         "workDoneProgress"]);

   end DocumentFormattingRegistrationOptions_Scope;

   procedure Read_DocumentFormattingRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentFormattingRegistrationOptions) is
      use DocumentFormattingRegistrationOptions_Scope;
      procedure Read_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.DocumentSelector_Or_Null);

      procedure Read_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.DocumentSelector_Or_Null) is
      begin
         if Handler.Is_Null_Value then
            Handler.Read_Next;
         else
            Value :=
              (Is_Null => False,
               Value   => <>);
            Read_DocumentSelector (Handler, Value.Value);
         end if;
      end Read_DocumentSelector_Or_Null;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case DocumentFormattingRegistrationOptions_Map.Get_Index (Key) is
               when 1 =>  --  documentSelector
                  Read_DocumentSelector_Or_Null
                    (Handler, Value.Parent.documentSelector);
               when 2 =>  --  workDoneProgress
                  Value.workDoneProgress       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.workDoneProgress.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_DocumentFormattingRegistrationOptions;

   package ColorPresentation_Scope is
      package ColorPresentation_Map is new Minimal_Perfect_Hash
        (["label",
         "textEdit",
         "additionalTextEdits"]);

   end ColorPresentation_Scope;

   procedure Read_ColorPresentation
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ColorPresentation) is
      use ColorPresentation_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case ColorPresentation_Map.Get_Index (Key) is
               when 1 =>  --  label
                  Value.label.Clear;
                  Value.label.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 2 =>  --  textEdit
                  Value.textEdit :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_TextEdit (Handler, Value.textEdit.Value);
               when 3 =>  --  additionalTextEdits
                  Read_TextEdit_Vector (Handler, Value.additionalTextEdits);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_ColorPresentation;

   package UnregistrationParams_Scope is
      package UnregistrationParams_Map is new Minimal_Perfect_Hash
        (["unregisterations"]);

   end UnregistrationParams_Scope;

   procedure Read_UnregistrationParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.UnregistrationParams) is
      use UnregistrationParams_Scope;
      procedure Read_Unregistration_Vector
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.Unregistration_Vector);

      procedure Read_Unregistration_Vector
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.Unregistration_Vector) is
      begin
         pragma Assert (Handler.Is_Start_Array);
         Handler.Read_Next;

         declare
            Set   : LSP.Structures.Unregistration_Vector renames Value;
            Value : LSP.Structures.Unregistration;
         begin
            Set.Clear;
            while not Handler.Is_End_Array loop
               Read_Unregistration (Handler, Value);
               Set.Append (Value);
            end loop;
         end;

         Handler.Read_Next;
      end Read_Unregistration_Vector;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case UnregistrationParams_Map.Get_Index (Key) is
               when 1 =>  --  unregisterations
                  Read_Unregistration_Vector (Handler, Value.unregisterations);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_UnregistrationParams;

   package SelectionRangeParams_Scope is
      package SelectionRangeParams_Map is new Minimal_Perfect_Hash
        (["workDoneToken",
         "partialResultToken",
         "textDocument",
         "positions"]);

   end SelectionRangeParams_Scope;

   procedure Read_SelectionRangeParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SelectionRangeParams) is
      use SelectionRangeParams_Scope;
      procedure Read_Position_Vector
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.Position_Vector);

      procedure Read_Position_Vector
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.Position_Vector) is
      begin
         pragma Assert (Handler.Is_Start_Array);
         Handler.Read_Next;

         declare
            Set   : LSP.Structures.Position_Vector renames Value;
            Value : LSP.Structures.Position;
         begin
            Set.Clear;
            while not Handler.Is_End_Array loop
               Read_Position (Handler, Value);
               Set.Append (Value);
            end loop;
         end;

         Handler.Read_Next;
      end Read_Position_Vector;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case SelectionRangeParams_Map.Get_Index (Key) is
               when 1 =>  --  workDoneToken
                  Value.workDoneToken :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_ProgressToken (Handler, Value.workDoneToken.Value);
               when 2 =>  --  partialResultToken
                  Value.partialResultToken :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_ProgressToken (Handler, Value.partialResultToken.Value);
               when 3 =>  --  textDocument
                  Read_TextDocumentIdentifier (Handler, Value.textDocument);
               when 4 =>  --  positions
                  Read_Position_Vector (Handler, Value.positions);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_SelectionRangeParams;

   package Tokens_Delta_Result_Scope is
      package Tokens_Delta_Result_Map is new Minimal_Perfect_Hash
        (["data",
         "edits"]);

   end Tokens_Delta_Result_Scope;

   procedure Read_Tokens_Delta_Result
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Tokens_Delta_Result) is
      use Tokens_Delta_Result_Scope;
   begin
      declare
         Parent  :
           constant not null access VSS.JSON.Pull_Readers.JSON_Pull_Reader'
             Class :=
           Handler'Access;
         Handler :
           VSS.JSON.Pull_Readers.Buffered.JSON_Buffered_Pull_Reader (Parent);
      begin
         Handler.Mark;
         if Handler.Is_Null_Value then
            Value :=
              (Kind   => LSP.Structures.Variant_3,
               others => <>);
         elsif Handler.Is_Start_Object then
            Handler.Read_Next;
            while Handler.Is_Key_Name loop
               declare
                  Key   : constant VSS.Strings.Virtual_String :=
                    Handler.Key_Name;
                  Index : constant Natural                    :=
                    Tokens_Delta_Result_Map.Get_Index (Key);
               begin
                  Handler.Read_Next;
                  case Index is
                     when 1 =>  --  data
                        Value :=
                          (Kind   => LSP.Structures.Variant_1,
                           others => <>);
                        exit;
                     when 2 =>  --  edits
                        Value :=
                          (Kind   => LSP.Structures.Variant_2,
                           others => <>);
                        exit;
                     when others =>
                        Handler.Skip_Current_Value;
                  end case;
               end;
            end loop;
         else
            raise Program_Error;  --  Unexpected JSON value
         end if;

         Handler.Reset;
         Handler.Unmark;

         case Value.Kind is
            when LSP.Structures.Variant_1 =>
               Read_SemanticTokens (Handler, Value.Variant_1);
            when LSP.Structures.Variant_2 =>
               Read_SemanticTokensDelta (Handler, Value.Variant_2);
            when LSP.Structures.Variant_3 =>
               null;  --  #null_value
               Handler.Read_Next;
         end case;
      end;
   end Read_Tokens_Delta_Result;

   package RenameFilesParams_Scope is
      package RenameFilesParams_Map is new Minimal_Perfect_Hash (["files"]);

   end RenameFilesParams_Scope;

   procedure Read_RenameFilesParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.RenameFilesParams) is
      use RenameFilesParams_Scope;
      procedure Read_FileRename_Vector
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.FileRename_Vector);

      procedure Read_FileRename_Vector
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.FileRename_Vector) is
      begin
         pragma Assert (Handler.Is_Start_Array);
         Handler.Read_Next;

         declare
            Set   : LSP.Structures.FileRename_Vector renames Value;
            Value : LSP.Structures.FileRename;
         begin
            Set.Clear;
            while not Handler.Is_End_Array loop
               Read_FileRename (Handler, Value);
               Set.Append (Value);
            end loop;
         end;

         Handler.Read_Next;
      end Read_FileRename_Vector;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case RenameFilesParams_Map.Get_Index (Key) is
               when 1 =>  --  files
                  Read_FileRename_Vector (Handler, Value.files);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_RenameFilesParams;

   package TypeHierarchyItem_Scope is
      package TypeHierarchyItem_Map is new Minimal_Perfect_Hash
        (["name",
         "kind",
         "tags",
         "detail",
         "uri",
         "range",
         "selectionRange",
         "data"]);

   end TypeHierarchyItem_Scope;

   procedure Read_TypeHierarchyItem
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TypeHierarchyItem) is
      use TypeHierarchyItem_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case TypeHierarchyItem_Map.Get_Index (Key) is
               when 1 =>  --  name
                  Value.name.Clear;
                  Value.name.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 2 =>  --  kind
                  Read_SymbolKind (Handler, Value.kind);
               when 3 =>  --  tags
                  Read_SymbolTag_Set (Handler, Value.tags);
               when 4 =>  --  detail
                  Value.detail.Clear;
                  Value.detail.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 5 =>  --  uri
                  Value.uri := (Handler.String_Value with null record);
                  Handler.Read_Next;
               when 6 =>  --  range
                  Read_A_Range (Handler, Value.a_range);
               when 7 =>  --  selectionRange
                  Read_A_Range (Handler, Value.selectionRange);
               when 8 =>  --  data
                  Read_LSPAny (Handler, Value.data);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_TypeHierarchyItem;

   procedure Read_Boolean_Or_Any
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Boolean_Or_Any) is
   begin
      LSP.Input_Tools.Read_LSPAny_Class (Handler, Value);
   end Read_Boolean_Or_Any;

   package Location_Scope is
      package Location_Map is new Minimal_Perfect_Hash
        (["uri",
         "range"]);

   end Location_Scope;

   procedure Read_Location
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Location) is
      use Location_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case Location_Map.Get_Index (Key) is
               when 1 =>  --  uri
                  Value.uri := (Handler.String_Value with null record);
                  Handler.Read_Next;
               when 2 =>  --  range
                  Read_A_Range (Handler, Value.a_range);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_Location;

   package RenameRegistrationOptions_Scope is
      package RenameRegistrationOptions_Map is new Minimal_Perfect_Hash
        (["documentSelector",
         "workDoneProgress",
         "prepareProvider"]);

   end RenameRegistrationOptions_Scope;

   procedure Read_RenameRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.RenameRegistrationOptions) is
      use RenameRegistrationOptions_Scope;
      procedure Read_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.DocumentSelector_Or_Null);

      procedure Read_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.DocumentSelector_Or_Null) is
      begin
         if Handler.Is_Null_Value then
            Handler.Read_Next;
         else
            Value :=
              (Is_Null => False,
               Value   => <>);
            Read_DocumentSelector (Handler, Value.Value);
         end if;
      end Read_DocumentSelector_Or_Null;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case RenameRegistrationOptions_Map.Get_Index (Key) is
               when 1 =>  --  documentSelector
                  Read_DocumentSelector_Or_Null
                    (Handler, Value.Parent.documentSelector);
               when 2 =>  --  workDoneProgress
                  Value.workDoneProgress       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.workDoneProgress.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 3 =>  --  prepareProvider
                  Value.prepareProvider       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.prepareProvider.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_RenameRegistrationOptions;

   package ParameterInformation_Scope is
      package ParameterInformation_Map is new Minimal_Perfect_Hash
        (["label",
         "documentation"]);

   end ParameterInformation_Scope;

   procedure Read_ParameterInformation
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ParameterInformation) is
      use ParameterInformation_Scope;
      procedure Read_Natural_Tuple
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.Natural_Tuple);

      procedure Read_String_Or_Natural_Tuple
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.String_Or_Natural_Tuple);

      procedure Read_Natural_Tuple
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.Natural_Tuple) is
      begin
         pragma Assert (Handler.Is_Start_Array);
         Handler.Read_Next;
         for J in Value'Range loop
            Value (J) := Integer (Handler.Number_Value.Integer_Value);
            Handler.Read_Next;
         end loop;
         pragma Assert (Handler.Is_End_Array);
         Handler.Read_Next;
      end Read_Natural_Tuple;

      procedure Read_String_Or_Natural_Tuple
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.String_Or_Natural_Tuple) is
      begin
         if Handler.Is_String_Value then
            Value :=
              (Is_Virtual_String => True,
               others            => <>);
         else
            Value :=
              (Is_Virtual_String => False,
               others            => <>);
         end if;

         case Value.Is_Virtual_String is
            when True =>
               Value.Virtual_String.Clear;
               Value.Virtual_String.Append (Handler.String_Value);
               Handler.Read_Next;
            when False =>
               Read_Natural_Tuple (Handler, Value.Natural_Tuple);
         end case;
      end Read_String_Or_Natural_Tuple;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case ParameterInformation_Map.Get_Index (Key) is
               when 1 =>  --  label
                  Read_String_Or_Natural_Tuple (Handler, Value.label);
               when 2 =>  --  documentation
                  Value.documentation :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_Virtual_String_Or_MarkupContent
                    (Handler, Value.documentation.Value);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_ParameterInformation;

   package PositionEncodingKind_Map is new Minimal_Perfect_Hash
     (["utf-8",
      "utf-16",
      "utf-32"]);

   procedure Read_PositionEncodingKind
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.PositionEncodingKind) is
   begin
      Value :=
        LSP.Enumerations.PositionEncodingKind'Val
          (PositionEncodingKind_Map.Get_Index (Handler.String_Value) - 1);
      Handler.Read_Next;
   end Read_PositionEncodingKind;

   package Symbol_Progress_Report_Scope is
      package Symbol_Progress_Report_Map is new Minimal_Perfect_Hash
        (["deprecated",
         "data"]);

   end Symbol_Progress_Report_Scope;

   procedure Read_Symbol_Progress_Report
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Symbol_Progress_Report) is
      use Symbol_Progress_Report_Scope;
   begin
      declare
         Parent  :
           constant not null access VSS.JSON.Pull_Readers.JSON_Pull_Reader'
             Class :=
           Handler'Access;
         Handler :
           VSS.JSON.Pull_Readers.Buffered.JSON_Buffered_Pull_Reader (Parent);
      begin
         Handler.Mark;
         if Handler.Is_Start_Array then
            Handler.Read_Next;
         end if;
         if Handler.Is_Start_Object then
            Handler.Read_Next;
            while Handler.Is_Key_Name loop
               declare
                  Key   : constant VSS.Strings.Virtual_String :=
                    Handler.Key_Name;
                  Index : constant Natural                    :=
                    Symbol_Progress_Report_Map.Get_Index (Key);
               begin
                  Handler.Read_Next;
                  case Index is
                     when 1 =>  --  deprecated
                        Value :=
                          (Kind   => LSP.Structures.Variant_1,
                           others => <>);
                        exit;
                     when 2 =>  --  data
                        Value :=
                          (Kind   => LSP.Structures.Variant_2,
                           others => <>);
                        exit;
                     when others =>
                        Handler.Skip_Current_Value;
                  end case;
               end;
            end loop;
         else
            raise Program_Error;  --  Unexpected JSON value
         end if;

         Handler.Reset;
         Handler.Unmark;

         case Value.Kind is
            when LSP.Structures.Variant_1 =>
               Read_SymbolInformation_Vector (Handler, Value.Variant_1);
            when LSP.Structures.Variant_2 =>
               Read_WorkspaceSymbol_Vector (Handler, Value.Variant_2);
         end case;
      end;
   end Read_Symbol_Progress_Report;

   procedure Read_MessageType
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.MessageType) is
   begin
      Value :=
        LSP.Enumerations.MessageType'Val
          (Handler.Number_Value.Integer_Value - 1);
      Handler.Read_Next;
   end Read_MessageType;

   package An_InitializeParams_Scope is
      package An_InitializeParams_Map is new Minimal_Perfect_Hash
        (["workDoneToken",
         "processId",
         "clientInfo",
         "locale",
         "rootPath",
         "rootUri",
         "capabilities",
         "initializationOptions",
         "trace"]);

      package trace_Of_InitializeParams_Scope is
         package trace_Of_InitializeParams_Map is new Minimal_Perfect_Hash
           (["off",
            "messages",
            "compact",
            "verbose"]);

      end trace_Of_InitializeParams_Scope;

   end An_InitializeParams_Scope;

   procedure Read_An_InitializeParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.An_InitializeParams) is
      use An_InitializeParams_Scope;
      procedure Read_trace_Of_InitializeParams
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.trace_Of_InitializeParams);

      procedure Read_Virtual_String_Or_Null
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.Virtual_String_Or_Null);

      procedure Read_DocumentUri_Or_Null
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.DocumentUri_Or_Null);

      procedure Read_trace_Of_InitializeParams
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.trace_Of_InitializeParams) is
         use trace_Of_InitializeParams_Scope;
      begin
         Value :=
           LSP.Structures.trace_Of_InitializeParams'Val
             (trace_Of_InitializeParams_Map.Get_Index (Handler.String_Value) -
              1);
         Handler.Read_Next;
      end Read_trace_Of_InitializeParams;

      procedure Read_Virtual_String_Or_Null
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.Virtual_String_Or_Null) is
      begin
         if Handler.Is_Null_Value then
            Handler.Read_Next;
         else
            Value :=
              (Is_Null => False,
               Value   => <>);
            Value.Value.Clear;
            Value.Value.Append (Handler.String_Value);
            Handler.Read_Next;
         end if;
      end Read_Virtual_String_Or_Null;

      procedure Read_DocumentUri_Or_Null
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.DocumentUri_Or_Null) is
      begin
         if Handler.Is_Null_Value then
            Handler.Read_Next;
         else
            Value       :=
              (Is_Null => False,
               Value   => <>);
            Value.Value := (Handler.String_Value with null record);
            Handler.Read_Next;
         end if;
      end Read_DocumentUri_Or_Null;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case An_InitializeParams_Map.Get_Index (Key) is
               when 1 =>  --  workDoneToken
                  Value.workDoneToken :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_ProgressToken (Handler, Value.workDoneToken.Value);
               when 2 =>  --  processId
                  Read_Integer_Or_Null (Handler, Value.processId);
               when 3 =>  --  clientInfo
                  Value.clientInfo :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_clientInfo_Of_InitializeParams
                    (Handler, Value.clientInfo.Value);
               when 4 =>  --  locale
                  Value.locale.Clear;
                  Value.locale.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 5 =>  --  rootPath
                  Value.rootPath :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_Virtual_String_Or_Null (Handler, Value.rootPath.Value);
               when 6 =>  --  rootUri
                  Read_DocumentUri_Or_Null (Handler, Value.rootUri);
               when 7 =>  --  capabilities
                  Read_ClientCapabilities (Handler, Value.capabilities);
               when 8 =>  --  initializationOptions
                  Read_LSPAny (Handler, Value.initializationOptions);
               when 9 =>  --  trace
                  Value.trace :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_trace_Of_InitializeParams (Handler, Value.trace.Value);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_An_InitializeParams;

   package TypeHierarchySubtypesParams_Scope is
      package TypeHierarchySubtypesParams_Map is new Minimal_Perfect_Hash
        (["workDoneToken",
         "partialResultToken",
         "item"]);

   end TypeHierarchySubtypesParams_Scope;

   procedure Read_TypeHierarchySubtypesParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TypeHierarchySubtypesParams) is
      use TypeHierarchySubtypesParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case TypeHierarchySubtypesParams_Map.Get_Index (Key) is
               when 1 =>  --  workDoneToken
                  Value.workDoneToken :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_ProgressToken (Handler, Value.workDoneToken.Value);
               when 2 =>  --  partialResultToken
                  Value.partialResultToken :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_ProgressToken (Handler, Value.partialResultToken.Value);
               when 3 =>  --  item
                  Read_TypeHierarchyItem (Handler, Value.item);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_TypeHierarchySubtypesParams;

   package FileOperationFilter_Scope is
      package FileOperationFilter_Map is new Minimal_Perfect_Hash
        (["scheme",
         "pattern"]);

   end FileOperationFilter_Scope;

   procedure Read_FileOperationFilter
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.FileOperationFilter) is
      use FileOperationFilter_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case FileOperationFilter_Map.Get_Index (Key) is
               when 1 =>  --  scheme
                  Value.scheme.Clear;
                  Value.scheme.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 2 =>  --  pattern
                  Read_FileOperationPattern (Handler, Value.pattern);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_FileOperationFilter;

   package ApplyWorkspaceEditResult_Scope is
      package ApplyWorkspaceEditResult_Map is new Minimal_Perfect_Hash
        (["applied",
         "failureReason",
         "failedChange"]);

   end ApplyWorkspaceEditResult_Scope;

   procedure Read_ApplyWorkspaceEditResult
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ApplyWorkspaceEditResult) is
      use ApplyWorkspaceEditResult_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case ApplyWorkspaceEditResult_Map.Get_Index (Key) is
               when 1 =>  --  applied
                  Value.applied := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 2 =>  --  failureReason
                  Value.failureReason.Clear;
                  Value.failureReason.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 3 =>  --  failedChange
                  Value.failedChange       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.failedChange.Value :=
                    Integer (Handler.Number_Value.Integer_Value);
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_ApplyWorkspaceEditResult;

   package SemanticTokensPartialResult_Scope is
      package SemanticTokensPartialResult_Map is new Minimal_Perfect_Hash
        (["data"]);

   end SemanticTokensPartialResult_Scope;

   procedure Read_SemanticTokensPartialResult
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SemanticTokensPartialResult) is
      use SemanticTokensPartialResult_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case SemanticTokensPartialResult_Map.Get_Index (Key) is
               when 1 =>  --  data
                  Read_Natural_Vector (Handler, Value.data);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_SemanticTokensPartialResult;

   package Completion_Result_Scope is
      package Completion_Result_Map is new Minimal_Perfect_Hash
        (["isIncomplete",
         "itemDefaults",
         "items"]);

   end Completion_Result_Scope;

   procedure Read_Completion_Result
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Completion_Result) is
      use Completion_Result_Scope;
   begin
      declare
         Parent  :
           constant not null access VSS.JSON.Pull_Readers.JSON_Pull_Reader'
             Class :=
           Handler'Access;
         Handler :
           VSS.JSON.Pull_Readers.Buffered.JSON_Buffered_Pull_Reader (Parent);
      begin
         Handler.Mark;
         if Handler.Is_Null_Value then
            Value :=
              (Kind   => LSP.Structures.Variant_3,
               others => <>);
         elsif Handler.Is_Start_Object then
            Handler.Read_Next;
            Value :=
              (Kind   => LSP.Structures.Variant_1,
               others => <>);
            while Handler.Is_Key_Name loop
               declare
                  Key   : constant VSS.Strings.Virtual_String :=
                    Handler.Key_Name;
                  Index : constant Natural                    :=
                    Completion_Result_Map.Get_Index (Key);
               begin
                  Handler.Read_Next;
                  case Index is
                     when 1 =>  --  isIncomplete
                        Value :=
                          (Kind   => LSP.Structures.Variant_2,
                           others => <>);
                        exit;
                     when 2 =>  --  itemDefaults
                        Value :=
                          (Kind   => LSP.Structures.Variant_2,
                           others => <>);
                        exit;
                     when 3 =>  --  items
                        Value :=
                          (Kind   => LSP.Structures.Variant_2,
                           others => <>);
                        exit;
                     when others =>
                        Handler.Skip_Current_Value;
                  end case;
               end;
            end loop;
         else
            raise Program_Error;  --  Unexpected JSON value
         end if;

         Handler.Reset;
         Handler.Unmark;

         case Value.Kind is
            when LSP.Structures.Variant_1 =>
               Read_CompletionItem_Vector (Handler, Value.Variant_1);
            when LSP.Structures.Variant_2 =>
               Read_CompletionList (Handler, Value.Variant_2);
            when LSP.Structures.Variant_3 =>
               null;  --  #null_value
               Handler.Read_Next;
         end case;
      end;
   end Read_Completion_Result;

   package MonikerOptions_Scope is
      package MonikerOptions_Map is new Minimal_Perfect_Hash
        (["workDoneProgress"]);

   end MonikerOptions_Scope;

   procedure Read_MonikerOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.MonikerOptions) is
      use MonikerOptions_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case MonikerOptions_Map.Get_Index (Key) is
               when 1 =>  --  workDoneProgress
                  Value.workDoneProgress       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.workDoneProgress.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_MonikerOptions;

   package InlineValue_Scope is
      package InlineValue_Map is new Minimal_Perfect_Hash
        (["text",
         "variableName",
         "caseSensitiveLookup",
         "expression"]);

   end InlineValue_Scope;

   procedure Read_InlineValue
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.InlineValue) is
      use InlineValue_Scope;
   begin
      declare
         Parent  :
           constant not null access VSS.JSON.Pull_Readers.JSON_Pull_Reader'
             Class :=
           Handler'Access;
         Handler :
           VSS.JSON.Pull_Readers.Buffered.JSON_Buffered_Pull_Reader (Parent);
      begin
         Handler.Mark;
         if Handler.Is_Start_Object then
            Handler.Read_Next;
            while Handler.Is_Key_Name loop
               declare
                  Key   : constant VSS.Strings.Virtual_String :=
                    Handler.Key_Name;
                  Index : constant Natural := InlineValue_Map.Get_Index (Key);
               begin
                  Handler.Read_Next;
                  case Index is
                     when 1 =>  --  text
                        Value :=
                          (Kind   => LSP.Structures.Variant_1,
                           others => <>);
                        exit;
                     when 2 =>  --  variableName
                        Value :=
                          (Kind   => LSP.Structures.Variant_2,
                           others => <>);
                        exit;
                     when 3 =>  --  caseSensitiveLookup
                        Value :=
                          (Kind   => LSP.Structures.Variant_2,
                           others => <>);
                        exit;
                     when 4 =>  --  expression
                        Value :=
                          (Kind   => LSP.Structures.Variant_3,
                           others => <>);
                        exit;
                     when others =>
                        Handler.Skip_Current_Value;
                  end case;
               end;
            end loop;
         else
            raise Program_Error;  --  Unexpected JSON value
         end if;

         Handler.Reset;
         Handler.Unmark;

         case Value.Kind is
            when LSP.Structures.Variant_1 =>
               Read_InlineValueText (Handler, Value.Variant_1);
            when LSP.Structures.Variant_2 =>
               Read_InlineValueVariableLookup (Handler, Value.Variant_2);
            when LSP.Structures.Variant_3 =>
               Read_InlineValueEvaluatableExpression
                 (Handler, Value.Variant_3);
         end case;
      end;
   end Read_InlineValue;

   procedure Read_Virtual_String_Or_NotebookDocumentFilter
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Virtual_String_Or_NotebookDocumentFilter) is
   begin
      if Handler.Is_String_Value then
         Value :=
           (Is_Virtual_String => True,
            others            => <>);
      else
         Value :=
           (Is_Virtual_String => False,
            others            => <>);
      end if;

      case Value.Is_Virtual_String is
         when True =>
            Value.Virtual_String.Clear;
            Value.Virtual_String.Append (Handler.String_Value);
            Handler.Read_Next;
         when False =>
            Read_NotebookDocumentFilter
              (Handler, Value.NotebookDocumentFilter);
      end case;
   end Read_Virtual_String_Or_NotebookDocumentFilter;

   package MonikerKind_Map is new Minimal_Perfect_Hash
     (["import",
      "export",
      "local"]);

   procedure Read_MonikerKind
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.MonikerKind) is
   begin
      Value :=
        LSP.Enumerations.MonikerKind'Val
          (MonikerKind_Map.Get_Index (Handler.String_Value) - 1);
      Handler.Read_Next;
   end Read_MonikerKind;

   procedure Read_NotebookCellKind
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.NotebookCellKind) is
   begin
      Value :=
        LSP.Enumerations.NotebookCellKind'Val
          (Handler.Number_Value.Integer_Value - 1);
      Handler.Read_Next;
   end Read_NotebookCellKind;

   procedure Read_CallHierarchyItem_Vector_Or_Null
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CallHierarchyItem_Vector_Or_Null) is
   begin
      if Handler.Is_Null_Value then
         Handler.Read_Next;
      else
         Value.Clear;
         Read_CallHierarchyItem_Vector (Handler, Value);
      end if;
   end Read_CallHierarchyItem_Vector_Or_Null;

   package DidCloseTextDocumentParams_Scope is
      package DidCloseTextDocumentParams_Map is new Minimal_Perfect_Hash
        (["textDocument"]);

   end DidCloseTextDocumentParams_Scope;

   procedure Read_DidCloseTextDocumentParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DidCloseTextDocumentParams) is
      use DidCloseTextDocumentParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case DidCloseTextDocumentParams_Map.Get_Index (Key) is
               when 1 =>  --  textDocument
                  Read_TextDocumentIdentifier (Handler, Value.textDocument);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_DidCloseTextDocumentParams;

   package CallHierarchyOutgoingCallsParams_Scope is
      package CallHierarchyOutgoingCallsParams_Map is new Minimal_Perfect_Hash
        (["workDoneToken",
         "partialResultToken",
         "item"]);

   end CallHierarchyOutgoingCallsParams_Scope;

   procedure Read_CallHierarchyOutgoingCallsParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CallHierarchyOutgoingCallsParams) is
      use CallHierarchyOutgoingCallsParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case CallHierarchyOutgoingCallsParams_Map.Get_Index (Key) is
               when 1 =>  --  workDoneToken
                  Value.workDoneToken :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_ProgressToken (Handler, Value.workDoneToken.Value);
               when 2 =>  --  partialResultToken
                  Value.partialResultToken :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_ProgressToken (Handler, Value.partialResultToken.Value);
               when 3 =>  --  item
                  Read_CallHierarchyItem (Handler, Value.item);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_CallHierarchyOutgoingCallsParams;

   procedure Read_SelectionRange_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SelectionRange_Vector) is
   begin
      pragma Assert (Handler.Is_Start_Array);
      Handler.Read_Next;

      declare
         Set   : LSP.Structures.SelectionRange_Vector renames Value;
         Value : LSP.Structures.SelectionRange;
      begin
         Set.Clear;
         while not Handler.Is_End_Array loop
            Read_SelectionRange (Handler, Value);
            Set.Append (Value);
         end loop;
      end;

      Handler.Read_Next;
   end Read_SelectionRange_Vector;

   package HoverRegistrationOptions_Scope is
      package HoverRegistrationOptions_Map is new Minimal_Perfect_Hash
        (["documentSelector",
         "workDoneProgress"]);

   end HoverRegistrationOptions_Scope;

   procedure Read_HoverRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.HoverRegistrationOptions) is
      use HoverRegistrationOptions_Scope;
      procedure Read_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.DocumentSelector_Or_Null);

      procedure Read_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.DocumentSelector_Or_Null) is
      begin
         if Handler.Is_Null_Value then
            Handler.Read_Next;
         else
            Value :=
              (Is_Null => False,
               Value   => <>);
            Read_DocumentSelector (Handler, Value.Value);
         end if;
      end Read_DocumentSelector_Or_Null;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case HoverRegistrationOptions_Map.Get_Index (Key) is
               when 1 =>  --  documentSelector
                  Read_DocumentSelector_Or_Null
                    (Handler, Value.Parent.documentSelector);
               when 2 =>  --  workDoneProgress
                  Value.workDoneProgress       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.workDoneProgress.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_HoverRegistrationOptions;

   procedure Read_CompletionItemTag
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.CompletionItemTag) is
   begin
      Value :=
        LSP.Enumerations.CompletionItemTag'Val
          (Handler.Number_Value.Integer_Value - 1);
      Handler.Read_Next;
   end Read_CompletionItemTag;

   package SemanticTokensDeltaParams_Scope is
      package SemanticTokensDeltaParams_Map is new Minimal_Perfect_Hash
        (["workDoneToken",
         "partialResultToken",
         "textDocument",
         "previousResultId"]);

   end SemanticTokensDeltaParams_Scope;

   procedure Read_SemanticTokensDeltaParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SemanticTokensDeltaParams) is
      use SemanticTokensDeltaParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case SemanticTokensDeltaParams_Map.Get_Index (Key) is
               when 1 =>  --  workDoneToken
                  Value.workDoneToken :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_ProgressToken (Handler, Value.workDoneToken.Value);
               when 2 =>  --  partialResultToken
                  Value.partialResultToken :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_ProgressToken (Handler, Value.partialResultToken.Value);
               when 3 =>  --  textDocument
                  Read_TextDocumentIdentifier (Handler, Value.textDocument);
               when 4 =>  --  previousResultId
                  Value.previousResultId.Clear;
                  Value.previousResultId.Append (Handler.String_Value);
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_SemanticTokensDeltaParams;

   package DidChangeWorkspaceFoldersParams_Scope is
      package DidChangeWorkspaceFoldersParams_Map is new Minimal_Perfect_Hash
        (["event"]);

   end DidChangeWorkspaceFoldersParams_Scope;

   procedure Read_DidChangeWorkspaceFoldersParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DidChangeWorkspaceFoldersParams) is
      use DidChangeWorkspaceFoldersParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case DidChangeWorkspaceFoldersParams_Map.Get_Index (Key) is
               when 1 =>  --  event
                  Read_WorkspaceFoldersChangeEvent (Handler, Value.event);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_DidChangeWorkspaceFoldersParams;

   package DefinitionRegistrationOptions_Scope is
      package DefinitionRegistrationOptions_Map is new Minimal_Perfect_Hash
        (["documentSelector",
         "workDoneProgress"]);

   end DefinitionRegistrationOptions_Scope;

   procedure Read_DefinitionRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DefinitionRegistrationOptions) is
      use DefinitionRegistrationOptions_Scope;
      procedure Read_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.DocumentSelector_Or_Null);

      procedure Read_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.DocumentSelector_Or_Null) is
      begin
         if Handler.Is_Null_Value then
            Handler.Read_Next;
         else
            Value :=
              (Is_Null => False,
               Value   => <>);
            Read_DocumentSelector (Handler, Value.Value);
         end if;
      end Read_DocumentSelector_Or_Null;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case DefinitionRegistrationOptions_Map.Get_Index (Key) is
               when 1 =>  --  documentSelector
                  Read_DocumentSelector_Or_Null
                    (Handler, Value.Parent.documentSelector);
               when 2 =>  --  workDoneProgress
                  Value.workDoneProgress       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.workDoneProgress.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_DefinitionRegistrationOptions;

   package clientInfo_Of_InitializeParams_Scope is
      package clientInfo_Of_InitializeParams_Map is new Minimal_Perfect_Hash
        (["name",
         "version"]);

   end clientInfo_Of_InitializeParams_Scope;

   procedure Read_clientInfo_Of_InitializeParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.clientInfo_Of_InitializeParams) is
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while Handler.Is_Key_Name loop
         declare
            use clientInfo_Of_InitializeParams_Scope;
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case clientInfo_Of_InitializeParams_Map.Get_Index (Key) is
               when 1 =>  --  name
                  Value.name.Clear;
                  Value.name.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 2 =>  --  version
                  Value.version.Clear;
                  Value.version.Append (Handler.String_Value);
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_clientInfo_Of_InitializeParams;

   package CodeLensParams_Scope is
      package CodeLensParams_Map is new Minimal_Perfect_Hash
        (["workDoneToken",
         "partialResultToken",
         "textDocument"]);

   end CodeLensParams_Scope;

   procedure Read_CodeLensParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CodeLensParams) is
      use CodeLensParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case CodeLensParams_Map.Get_Index (Key) is
               when 1 =>  --  workDoneToken
                  Value.workDoneToken :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_ProgressToken (Handler, Value.workDoneToken.Value);
               when 2 =>  --  partialResultToken
                  Value.partialResultToken :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_ProgressToken (Handler, Value.partialResultToken.Value);
               when 3 =>  --  textDocument
                  Read_TextDocumentIdentifier (Handler, Value.textDocument);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_CodeLensParams;

   procedure Read_DiagnosticTag
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.DiagnosticTag) is
   begin
      Value :=
        LSP.Enumerations.DiagnosticTag'Val
          (Handler.Number_Value.Integer_Value - 1);
      Handler.Read_Next;
   end Read_DiagnosticTag;

   procedure Read_TypeHierarchyItem_Vector_Or_Null
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TypeHierarchyItem_Vector_Or_Null) is
   begin
      if Handler.Is_Null_Value then
         Handler.Read_Next;
      else
         Value.Clear;
         Read_TypeHierarchyItem_Vector (Handler, Value);
      end if;
   end Read_TypeHierarchyItem_Vector_Or_Null;

   package Symbol_Result_Scope is
      package Symbol_Result_Map is new Minimal_Perfect_Hash
        (["deprecated",
         "data"]);

   end Symbol_Result_Scope;

   procedure Read_Symbol_Result
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Symbol_Result) is
      use Symbol_Result_Scope;
   begin
      declare
         Parent  :
           constant not null access VSS.JSON.Pull_Readers.JSON_Pull_Reader'
             Class :=
           Handler'Access;
         Handler :
           VSS.JSON.Pull_Readers.Buffered.JSON_Buffered_Pull_Reader (Parent);
      begin
         Handler.Mark;
         if Handler.Is_Start_Array then
            Handler.Read_Next;
         end if;
         if Handler.Is_Null_Value then
            Value :=
              (Kind   => LSP.Structures.Variant_3,
               others => <>);
         elsif Handler.Is_Start_Object then
            Handler.Read_Next;
            while Handler.Is_Key_Name loop
               declare
                  Key   : constant VSS.Strings.Virtual_String :=
                    Handler.Key_Name;
                  Index : constant Natural                    :=
                    Symbol_Result_Map.Get_Index (Key);
               begin
                  Handler.Read_Next;
                  case Index is
                     when 1 =>  --  deprecated
                        Value :=
                          (Kind   => LSP.Structures.Variant_1,
                           others => <>);
                        exit;
                     when 2 =>  --  data
                        Value :=
                          (Kind   => LSP.Structures.Variant_2,
                           others => <>);
                        exit;
                     when others =>
                        Handler.Skip_Current_Value;
                  end case;
               end;
            end loop;
         else
            raise Program_Error;  --  Unexpected JSON value
         end if;

         Handler.Reset;
         Handler.Unmark;

         case Value.Kind is
            when LSP.Structures.Variant_1 =>
               Read_SymbolInformation_Vector (Handler, Value.Variant_1);
            when LSP.Structures.Variant_2 =>
               Read_WorkspaceSymbol_Vector (Handler, Value.Variant_2);
            when LSP.Structures.Variant_3 =>
               null;  --  #null_value
               Handler.Read_Next;
         end case;
      end;
   end Read_Symbol_Result;

   package DiagnosticClientCapabilities_Scope is
      package DiagnosticClientCapabilities_Map is new Minimal_Perfect_Hash
        (["dynamicRegistration",
         "relatedDocumentSupport"]);

   end DiagnosticClientCapabilities_Scope;

   procedure Read_DiagnosticClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DiagnosticClientCapabilities) is
      use DiagnosticClientCapabilities_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case DiagnosticClientCapabilities_Map.Get_Index (Key) is
               when 1 =>  --  dynamicRegistration
                  Value.dynamicRegistration       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.dynamicRegistration.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 2 =>  --  relatedDocumentSupport
                  Value.relatedDocumentSupport       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.relatedDocumentSupport.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_DiagnosticClientCapabilities;

   package MonikerRegistrationOptions_Scope is
      package MonikerRegistrationOptions_Map is new Minimal_Perfect_Hash
        (["documentSelector",
         "workDoneProgress"]);

   end MonikerRegistrationOptions_Scope;

   procedure Read_MonikerRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.MonikerRegistrationOptions) is
      use MonikerRegistrationOptions_Scope;
      procedure Read_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.DocumentSelector_Or_Null);

      procedure Read_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.DocumentSelector_Or_Null) is
      begin
         if Handler.Is_Null_Value then
            Handler.Read_Next;
         else
            Value :=
              (Is_Null => False,
               Value   => <>);
            Read_DocumentSelector (Handler, Value.Value);
         end if;
      end Read_DocumentSelector_Or_Null;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case MonikerRegistrationOptions_Map.Get_Index (Key) is
               when 1 =>  --  documentSelector
                  Read_DocumentSelector_Or_Null
                    (Handler, Value.Parent.documentSelector);
               when 2 =>  --  workDoneProgress
                  Value.workDoneProgress       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.workDoneProgress.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_MonikerRegistrationOptions;

   package ExecutionSummary_Scope is
      package ExecutionSummary_Map is new Minimal_Perfect_Hash
        (["executionOrder",
         "success"]);

   end ExecutionSummary_Scope;

   procedure Read_ExecutionSummary
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ExecutionSummary) is
      use ExecutionSummary_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case ExecutionSummary_Map.Get_Index (Key) is
               when 1 =>  --  executionOrder
                  Value.executionOrder :=
                    Integer (Handler.Number_Value.Integer_Value);
                  Handler.Read_Next;
               when 2 =>  --  success
                  Value.success       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.success.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_ExecutionSummary;

   package TypeHierarchySupertypesParams_Scope is
      package TypeHierarchySupertypesParams_Map is new Minimal_Perfect_Hash
        (["workDoneToken",
         "partialResultToken",
         "item"]);

   end TypeHierarchySupertypesParams_Scope;

   procedure Read_TypeHierarchySupertypesParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TypeHierarchySupertypesParams) is
      use TypeHierarchySupertypesParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case TypeHierarchySupertypesParams_Map.Get_Index (Key) is
               when 1 =>  --  workDoneToken
                  Value.workDoneToken :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_ProgressToken (Handler, Value.workDoneToken.Value);
               when 2 =>  --  partialResultToken
                  Value.partialResultToken :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_ProgressToken (Handler, Value.partialResultToken.Value);
               when 3 =>  --  item
                  Read_TypeHierarchyItem (Handler, Value.item);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_TypeHierarchySupertypesParams;

   package PreviousResultId_Scope is
      package PreviousResultId_Map is new Minimal_Perfect_Hash
        (["uri",
         "value"]);

   end PreviousResultId_Scope;

   procedure Read_PreviousResultId
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.PreviousResultId) is
      use PreviousResultId_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case PreviousResultId_Map.Get_Index (Key) is
               when 1 =>  --  uri
                  Value.uri := (Handler.String_Value with null record);
                  Handler.Read_Next;
               when 2 =>  --  value
                  Value.value.Clear;
                  Value.value.Append (Handler.String_Value);
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_PreviousResultId;

   package FoldingRangeKind_Map is new Minimal_Perfect_Hash
     (["comment",
      "imports",
      "region"]);

   procedure Read_FoldingRangeKind
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.FoldingRangeKind) is
   begin
      Value :=
        LSP.Enumerations.FoldingRangeKind'Val
          (FoldingRangeKind_Map.Get_Index (Handler.String_Value) - 1);
      Handler.Read_Next;
   end Read_FoldingRangeKind;

   package PublishDiagnosticsParams_Scope is
      package PublishDiagnosticsParams_Map is new Minimal_Perfect_Hash
        (["uri",
         "version",
         "diagnostics"]);

   end PublishDiagnosticsParams_Scope;

   procedure Read_PublishDiagnosticsParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.PublishDiagnosticsParams) is
      use PublishDiagnosticsParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case PublishDiagnosticsParams_Map.Get_Index (Key) is
               when 1 =>  --  uri
                  Value.uri := (Handler.String_Value with null record);
                  Handler.Read_Next;
               when 2 =>  --  version
                  Value.version       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.version.Value :=
                    Integer (Handler.Number_Value.Integer_Value);
                  Handler.Read_Next;
               when 3 =>  --  diagnostics
                  Read_Diagnostic_Vector (Handler, Value.diagnostics);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_PublishDiagnosticsParams;

   package DocumentFilter_Scope is
      package DocumentFilter_Map is new Minimal_Perfect_Hash (["notebook"]);

   end DocumentFilter_Scope;

   procedure Read_DocumentFilter
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentFilter) is
      use DocumentFilter_Scope;
   begin
      declare
         Parent  :
           constant not null access VSS.JSON.Pull_Readers.JSON_Pull_Reader'
             Class :=
           Handler'Access;
         Handler :
           VSS.JSON.Pull_Readers.Buffered.JSON_Buffered_Pull_Reader (Parent);
      begin
         Handler.Mark;
         if Handler.Is_Start_Object then
            Handler.Read_Next;
            Value :=
              (Is_TextDocumentFilter => True,
               others                => <>);
            while Handler.Is_Key_Name loop
               declare
                  Key   : constant VSS.Strings.Virtual_String :=
                    Handler.Key_Name;
                  Index : constant Natural                    :=
                    DocumentFilter_Map.Get_Index (Key);
               begin
                  Handler.Read_Next;
                  case Index is
                     when 1 =>  --  notebook
                        Value :=
                          (Is_TextDocumentFilter => False,
                           others                => <>);
                        exit;
                     when others =>
                        Handler.Skip_Current_Value;
                  end case;
               end;
            end loop;
         else
            raise Program_Error;  --  Unexpected JSON value
         end if;

         Handler.Reset;
         Handler.Unmark;

         case Value.Is_TextDocumentFilter is
            when True =>
               Read_TextDocumentFilter (Handler, Value.TextDocumentFilter);
            when False =>
               Read_NotebookCellTextDocumentFilter
                 (Handler, Value.NotebookCellTextDocumentFilter);
         end case;
      end;
   end Read_DocumentFilter;

   package WorkspaceDiagnosticReport_Scope is
      package WorkspaceDiagnosticReport_Map is new Minimal_Perfect_Hash
        (["items"]);

   end WorkspaceDiagnosticReport_Scope;

   procedure Read_WorkspaceDiagnosticReport
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.WorkspaceDiagnosticReport) is
      use WorkspaceDiagnosticReport_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case WorkspaceDiagnosticReport_Map.Get_Index (Key) is
               when 1 =>  --  items
                  Read_WorkspaceDocumentDiagnosticReport_Vector
                    (Handler, Value.items);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_WorkspaceDiagnosticReport;

   package CallHierarchyIncomingCall_Scope is
      package CallHierarchyIncomingCall_Map is new Minimal_Perfect_Hash
        (["from",
         "fromRanges"]);

   end CallHierarchyIncomingCall_Scope;

   procedure Read_CallHierarchyIncomingCall
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CallHierarchyIncomingCall) is
      use CallHierarchyIncomingCall_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case CallHierarchyIncomingCall_Map.Get_Index (Key) is
               when 1 =>  --  from
                  Read_CallHierarchyItem (Handler, Value.from);
               when 2 =>  --  fromRanges
                  Read_Range_Vector (Handler, Value.fromRanges);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_CallHierarchyIncomingCall;

   procedure Read_TextEdit_Vector_Or_Null
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TextEdit_Vector_Or_Null) is
   begin
      if Handler.Is_Null_Value then
         Handler.Read_Next;
      else
         Value.Clear;
         Read_TextEdit_Vector (Handler, Value);
      end if;
   end Read_TextEdit_Vector_Or_Null;

   procedure Read_LinkedEditingRanges_Or_Null
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.LinkedEditingRanges_Or_Null) is
   begin
      if Handler.Is_Null_Value then
         Handler.Read_Next;
      else
         Value :=
           (Is_Null => False,
            Value   => <>);
         Read_LinkedEditingRanges (Handler, Value.Value);
      end if;
   end Read_LinkedEditingRanges_Or_Null;

   package TextDocumentChangeRegistrationOptions_Scope is
      package TextDocumentChangeRegistrationOptions_Map is new Minimal_Perfect_Hash
        (["documentSelector",
         "syncKind"]);

   end TextDocumentChangeRegistrationOptions_Scope;

   procedure Read_TextDocumentChangeRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TextDocumentChangeRegistrationOptions) is
      use TextDocumentChangeRegistrationOptions_Scope;
      procedure Read_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.DocumentSelector_Or_Null);

      procedure Read_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.DocumentSelector_Or_Null) is
      begin
         if Handler.Is_Null_Value then
            Handler.Read_Next;
         else
            Value :=
              (Is_Null => False,
               Value   => <>);
            Read_DocumentSelector (Handler, Value.Value);
         end if;
      end Read_DocumentSelector_Or_Null;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case TextDocumentChangeRegistrationOptions_Map.Get_Index (Key) is
               when 1 =>  --  documentSelector
                  Read_DocumentSelector_Or_Null
                    (Handler, Value.documentSelector);
               when 2 =>  --  syncKind
                  Read_TextDocumentSyncKind (Handler, Value.syncKind);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_TextDocumentChangeRegistrationOptions;

   procedure Read_DeclarationLink_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DeclarationLink_Vector) is
   begin
      pragma Assert (Handler.Is_Start_Array);
      Handler.Read_Next;

      declare
         Set   : LSP.Structures.DeclarationLink_Vector renames Value;
         Value : LSP.Structures.DeclarationLink;
      begin
         Set.Clear;
         while not Handler.Is_End_Array loop
            Read_DeclarationLink (Handler, Value);
            Set.Append (Value);
         end loop;
      end;

      Handler.Read_Next;
   end Read_DeclarationLink_Vector;

   package SetTraceParams_Scope is
      package SetTraceParams_Map is new Minimal_Perfect_Hash (["value"]);

   end SetTraceParams_Scope;

   procedure Read_SetTraceParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SetTraceParams) is
      use SetTraceParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case SetTraceParams_Map.Get_Index (Key) is
               when 1 =>  --  value
                  Read_TraceValues (Handler, Value.value);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_SetTraceParams;

   package DocumentFormattingClientCapabilities_Scope is
      package DocumentFormattingClientCapabilities_Map is new Minimal_Perfect_Hash
        (["dynamicRegistration"]);

   end DocumentFormattingClientCapabilities_Scope;

   procedure Read_DocumentFormattingClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentFormattingClientCapabilities) is
      use DocumentFormattingClientCapabilities_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case DocumentFormattingClientCapabilities_Map.Get_Index (Key) is
               when 1 =>  --  dynamicRegistration
                  Value.dynamicRegistration       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.dynamicRegistration.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_DocumentFormattingClientCapabilities;

   package NotebookDocumentChangeEvent_Scope is
      package NotebookDocumentChangeEvent_Map is new Minimal_Perfect_Hash
        (["metadata",
         "cells"]);

      package textContent_Ofcells_OfNotebookDocumentChangeEvent_Item_Scope is
         package textContent_Ofcells_OfNotebookDocumentChangeEvent_Item_Map is new Minimal_Perfect_Hash
           (["document",
            "changes"]);

      end textContent_Ofcells_OfNotebookDocumentChangeEvent_Item_Scope;

      package cells_OfNotebookDocumentChangeEvent_Scope is
         package cells_OfNotebookDocumentChangeEvent_Map is new Minimal_Perfect_Hash
           (["structure",
            "data",
            "textContent"]);

      end cells_OfNotebookDocumentChangeEvent_Scope;

      package structure_Ofcells_OfNotebookDocumentChangeEvent_Scope is
         package structure_Ofcells_OfNotebookDocumentChangeEvent_Map is new Minimal_Perfect_Hash
           (["array",
            "didOpen",
            "didClose"]);

      end structure_Ofcells_OfNotebookDocumentChangeEvent_Scope;

   end NotebookDocumentChangeEvent_Scope;

   procedure Read_NotebookDocumentChangeEvent
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.NotebookDocumentChangeEvent) is
      use NotebookDocumentChangeEvent_Scope;
      procedure Read_textContent_Ofcells_OfNotebookDocumentChangeEvent_Item
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .textContent_Ofcells_OfNotebookDocumentChangeEvent_Item);

      procedure Read_cells_OfNotebookDocumentChangeEvent
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.cells_OfNotebookDocumentChangeEvent);

      procedure Read_textContent_Ofcells_OfNotebookDocumentChangeEvent
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .textContent_Ofcells_OfNotebookDocumentChangeEvent);

      procedure Read_structure_Ofcells_OfNotebookDocumentChangeEvent
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .structure_Ofcells_OfNotebookDocumentChangeEvent);

      procedure Read_textContent_Ofcells_OfNotebookDocumentChangeEvent_Item
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .textContent_Ofcells_OfNotebookDocumentChangeEvent_Item) is
      begin
         pragma Assert (Handler.Is_Start_Object);
         Handler.Read_Next;

         while Handler.Is_Key_Name loop
            declare
               use
                 textContent_Ofcells_OfNotebookDocumentChangeEvent_Item_Scope;
               Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
            begin
               Handler.Read_Next;
               case textContent_Ofcells_OfNotebookDocumentChangeEvent_Item_Map
                 .Get_Index
                 (Key) is
                  when 1 =>  --  document
                     Read_VersionedTextDocumentIdentifier
                       (Handler, Value.document);
                  when 2 =>  --  changes
                     Read_TextDocumentContentChangeEvent_Vector
                       (Handler, Value.changes);
                  when others =>
                     Handler.Skip_Current_Value;
               end case;
            end;
         end loop;

         Handler.Read_Next;
      end Read_textContent_Ofcells_OfNotebookDocumentChangeEvent_Item;

      procedure Read_cells_OfNotebookDocumentChangeEvent
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.cells_OfNotebookDocumentChangeEvent) is
      begin
         pragma Assert (Handler.Is_Start_Object);
         Handler.Read_Next;

         while Handler.Is_Key_Name loop
            declare
               use cells_OfNotebookDocumentChangeEvent_Scope;
               Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
            begin
               Handler.Read_Next;
               case cells_OfNotebookDocumentChangeEvent_Map.Get_Index (Key) is
                  when 1 =>  --  structure
                     Value.structure :=
                       (Is_Set => True,
                        Value  => <>);
                     Read_structure_Ofcells_OfNotebookDocumentChangeEvent
                       (Handler, Value.structure.Value);
                  when 2 =>  --  data
                     Read_NotebookCell_Vector (Handler, Value.data);
                  when 3 =>  --  textContent
                     Read_textContent_Ofcells_OfNotebookDocumentChangeEvent
                       (Handler, Value.textContent);
                  when others =>
                     Handler.Skip_Current_Value;
               end case;
            end;
         end loop;

         Handler.Read_Next;
      end Read_cells_OfNotebookDocumentChangeEvent;

      procedure Read_textContent_Ofcells_OfNotebookDocumentChangeEvent
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .textContent_Ofcells_OfNotebookDocumentChangeEvent) is
      begin
         pragma Assert (Handler.Is_Start_Array);
         Handler.Read_Next;

         declare
            Set   :
              LSP.Structures
                .textContent_Ofcells_OfNotebookDocumentChangeEvent renames
              Value;
            Value :
              LSP.Structures
                .textContent_Ofcells_OfNotebookDocumentChangeEvent_Item;
         begin
            Set.Clear;
            while not Handler.Is_End_Array loop
               Read_textContent_Ofcells_OfNotebookDocumentChangeEvent_Item
                 (Handler, Value);
               Set.Append (Value);
            end loop;
         end;

         Handler.Read_Next;
      end Read_textContent_Ofcells_OfNotebookDocumentChangeEvent;

      procedure Read_structure_Ofcells_OfNotebookDocumentChangeEvent
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .structure_Ofcells_OfNotebookDocumentChangeEvent) is
      begin
         pragma Assert (Handler.Is_Start_Object);
         Handler.Read_Next;

         while Handler.Is_Key_Name loop
            declare
               use structure_Ofcells_OfNotebookDocumentChangeEvent_Scope;
               Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
            begin
               Handler.Read_Next;
               case structure_Ofcells_OfNotebookDocumentChangeEvent_Map
                 .Get_Index
                 (Key) is
                  when 1 =>  --  array
                     Read_NotebookCellArrayChange (Handler, Value.an_array);
                  when 2 =>  --  didOpen
                     Read_TextDocumentItem_Vector (Handler, Value.didOpen);
                  when 3 =>  --  didClose
                     Read_TextDocumentIdentifier_Vector
                       (Handler, Value.didClose);
                  when others =>
                     Handler.Skip_Current_Value;
               end case;
            end;
         end loop;

         Handler.Read_Next;
      end Read_structure_Ofcells_OfNotebookDocumentChangeEvent;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case NotebookDocumentChangeEvent_Map.Get_Index (Key) is
               when 1 =>  --  metadata
                  Value.metadata :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_LSPObject (Handler, Value.metadata.Value);
               when 2 =>  --  cells
                  Value.cells :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_cells_OfNotebookDocumentChangeEvent
                    (Handler, Value.cells.Value);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_NotebookDocumentChangeEvent;

   package FormattingOptions_Scope is
      package FormattingOptions_Map is new Minimal_Perfect_Hash
        (["tabSize",
         "insertSpaces",
         "trimTrailingWhitespace",
         "insertFinalNewline",
         "trimFinalNewlines"]);

   end FormattingOptions_Scope;

   procedure Read_FormattingOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.FormattingOptions) is
      use FormattingOptions_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case FormattingOptions_Map.Get_Index (Key) is
               when 1 =>  --  tabSize
                  Value.tabSize :=
                    Integer (Handler.Number_Value.Integer_Value);
                  Handler.Read_Next;
               when 2 =>  --  insertSpaces
                  Value.insertSpaces := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 3 =>  --  trimTrailingWhitespace
                  Value.trimTrailingWhitespace       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.trimTrailingWhitespace.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 4 =>  --  insertFinalNewline
                  Value.insertFinalNewline       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.insertFinalNewline.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 5 =>  --  trimFinalNewlines
                  Value.trimFinalNewlines       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.trimFinalNewlines.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_FormattingOptions;

   package WorkspaceDiagnosticReportPartialResult_Scope is
      package WorkspaceDiagnosticReportPartialResult_Map is new Minimal_Perfect_Hash
        (["items"]);

   end WorkspaceDiagnosticReportPartialResult_Scope;

   procedure Read_WorkspaceDiagnosticReportPartialResult
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.WorkspaceDiagnosticReportPartialResult) is
      use WorkspaceDiagnosticReportPartialResult_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case WorkspaceDiagnosticReportPartialResult_Map.Get_Index (Key) is
               when 1 =>  --  items
                  Read_WorkspaceDocumentDiagnosticReport_Vector
                    (Handler, Value.items);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_WorkspaceDiagnosticReportPartialResult;

   package resolveSupport_OfWorkspaceSymbolClientCapabilities_Scope is
      package resolveSupport_OfWorkspaceSymbolClientCapabilities_Map is new Minimal_Perfect_Hash
        (["properties"]);

   end resolveSupport_OfWorkspaceSymbolClientCapabilities_Scope;

   procedure Read_resolveSupport_OfWorkspaceSymbolClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures
        .resolveSupport_OfWorkspaceSymbolClientCapabilities) is
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while Handler.Is_Key_Name loop
         declare
            use resolveSupport_OfWorkspaceSymbolClientCapabilities_Scope;
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case resolveSupport_OfWorkspaceSymbolClientCapabilities_Map
              .Get_Index
              (Key) is
               when 1 =>  --  properties
                  Read_Virtual_String_Vector (Handler, Value.properties);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_resolveSupport_OfWorkspaceSymbolClientCapabilities;

   package DidOpenTextDocumentParams_Scope is
      package DidOpenTextDocumentParams_Map is new Minimal_Perfect_Hash
        (["textDocument"]);

   end DidOpenTextDocumentParams_Scope;

   procedure Read_DidOpenTextDocumentParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DidOpenTextDocumentParams) is
      use DidOpenTextDocumentParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case DidOpenTextDocumentParams_Map.Get_Index (Key) is
               when 1 =>  --  textDocument
                  Read_TextDocumentItem (Handler, Value.textDocument);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_DidOpenTextDocumentParams;

   procedure Read_WorkspaceSymbol_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.WorkspaceSymbol_Vector) is
   begin
      pragma Assert (Handler.Is_Start_Array);
      Handler.Read_Next;

      declare
         Set   : LSP.Structures.WorkspaceSymbol_Vector renames Value;
         Value : LSP.Structures.WorkspaceSymbol;
      begin
         Set.Clear;
         while not Handler.Is_End_Array loop
            Read_WorkspaceSymbol (Handler, Value);
            Set.Append (Value);
         end loop;
      end;

      Handler.Read_Next;
   end Read_WorkspaceSymbol_Vector;

   package TypeDefinitionRegistrationOptions_Scope is
      package TypeDefinitionRegistrationOptions_Map is new Minimal_Perfect_Hash
        (["documentSelector",
         "workDoneProgress",
         "id"]);

   end TypeDefinitionRegistrationOptions_Scope;

   procedure Read_TypeDefinitionRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TypeDefinitionRegistrationOptions) is
      use TypeDefinitionRegistrationOptions_Scope;
      procedure Read_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.DocumentSelector_Or_Null);

      procedure Read_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.DocumentSelector_Or_Null) is
      begin
         if Handler.Is_Null_Value then
            Handler.Read_Next;
         else
            Value :=
              (Is_Null => False,
               Value   => <>);
            Read_DocumentSelector (Handler, Value.Value);
         end if;
      end Read_DocumentSelector_Or_Null;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case TypeDefinitionRegistrationOptions_Map.Get_Index (Key) is
               when 1 =>  --  documentSelector
                  Read_DocumentSelector_Or_Null
                    (Handler, Value.Parent.documentSelector);
               when 2 =>  --  workDoneProgress
                  Value.workDoneProgress       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.workDoneProgress.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 3 =>  --  id
                  Value.id.Clear;
                  Value.id.Append (Handler.String_Value);
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_TypeDefinitionRegistrationOptions;

   package CodeLensOptions_Scope is
      package CodeLensOptions_Map is new Minimal_Perfect_Hash
        (["workDoneProgress",
         "resolveProvider"]);

   end CodeLensOptions_Scope;

   procedure Read_CodeLensOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CodeLensOptions) is
      use CodeLensOptions_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case CodeLensOptions_Map.Get_Index (Key) is
               when 1 =>  --  workDoneProgress
                  Value.workDoneProgress       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.workDoneProgress.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 2 =>  --  resolveProvider
                  Value.resolveProvider       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.resolveProvider.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_CodeLensOptions;

   package FileRename_Scope is
      package FileRename_Map is new Minimal_Perfect_Hash
        (["oldUri",
         "newUri"]);

   end FileRename_Scope;

   procedure Read_FileRename
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.FileRename) is
      use FileRename_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case FileRename_Map.Get_Index (Key) is
               when 1 =>  --  oldUri
                  Value.oldUri.Clear;
                  Value.oldUri.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 2 =>  --  newUri
                  Value.newUri.Clear;
                  Value.newUri.Append (Handler.String_Value);
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_FileRename;

   package SemanticTokensPartialResult_Or_SemanticTokensDeltaPartialResult_Scope
   is
      package SemanticTokensPartialResult_Or_SemanticTokensDeltaPartialResult_Map is new Minimal_Perfect_Hash
        (["data",
         "edits"]);

   end SemanticTokensPartialResult_Or_SemanticTokensDeltaPartialResult_Scope;

   procedure Read_SemanticTokensPartialResult_Or_SemanticTokensDeltaPartialResult
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures
        .SemanticTokensPartialResult_Or_SemanticTokensDeltaPartialResult) is
      use
        SemanticTokensPartialResult_Or_SemanticTokensDeltaPartialResult_Scope;
   begin
      declare
         Parent  :
           constant not null access VSS.JSON.Pull_Readers.JSON_Pull_Reader'
             Class :=
           Handler'Access;
         Handler :
           VSS.JSON.Pull_Readers.Buffered.JSON_Buffered_Pull_Reader (Parent);
      begin
         Handler.Mark;
         if Handler.Is_Start_Object then
            Handler.Read_Next;
            while Handler.Is_Key_Name loop
               declare
                  Key   : constant VSS.Strings.Virtual_String :=
                    Handler.Key_Name;
                  Index : constant Natural                    :=
                    SemanticTokensPartialResult_Or_SemanticTokensDeltaPartialResult_Map
                      .Get_Index
                      (Key);
               begin
                  Handler.Read_Next;
                  case Index is
                     when 1 =>  --  data
                        Value :=
                          (Is_SemanticTokensPartialResult => True,
                           others                         => <>);
                        exit;
                     when 2 =>  --  edits
                        Value :=
                          (Is_SemanticTokensPartialResult => False,
                           others                         => <>);
                        exit;
                     when others =>
                        Handler.Skip_Current_Value;
                  end case;
               end;
            end loop;
         else
            raise Program_Error;  --  Unexpected JSON value
         end if;

         Handler.Reset;
         Handler.Unmark;

         case Value.Is_SemanticTokensPartialResult is
            when True =>
               Read_SemanticTokensPartialResult
                 (Handler, Value.SemanticTokensPartialResult);
            when False =>
               Read_SemanticTokensDeltaPartialResult
                 (Handler, Value.SemanticTokensDeltaPartialResult);
         end case;
      end;
   end Read_SemanticTokensPartialResult_Or_SemanticTokensDeltaPartialResult;

   package DocumentDiagnosticReport_Scope is
      package DocumentDiagnosticReport_Map is new Minimal_Perfect_Hash
        (["full",
         "unchanged"]);

   end DocumentDiagnosticReport_Scope;

   procedure Read_DocumentDiagnosticReport
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentDiagnosticReport) is
      use DocumentDiagnosticReport_Scope;
   begin
      declare
         Parent  :
           constant not null access VSS.JSON.Pull_Readers.JSON_Pull_Reader'
             Class :=
           Handler'Access;
         Handler :
           VSS.JSON.Pull_Readers.Buffered.JSON_Buffered_Pull_Reader (Parent);
         Kind    : Natural;
      begin
         Handler.Mark;
         pragma Assert (Handler.Is_Start_Object);
         Handler.Read_Next;
         while Handler.Is_Key_Name loop
            declare
               use type VSS.Strings.Virtual_String;
               Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
            begin
               Handler.Read_Next;
               if Key = "kind" then
                  pragma Assert (Handler.Is_String_Value);
                  Kind :=
                    DocumentDiagnosticReport_Map.Get_Index
                      (Handler.String_Value);
                  case Kind is
                     when 1 =>  --  full
                        Value :=
                          (Kind   => LSP.Structures.full,
                           others => <>);
                     when 2 =>  --  unchanged
                        Value :=
                          (Kind   => LSP.Structures.unchanged,
                           others => <>);
                     when others =>
                        raise Constraint_Error;
                  end case;
                  exit;
               else
                  Handler.Skip_Current_Value;
               end if;
            end;
         end loop;

         Handler.Reset;
         Handler.Unmark;

         case Value.Kind is
            when LSP.Structures.full =>
               Read_RelatedFullDocumentDiagnosticReport (Handler, Value.full);
            when LSP.Structures.unchanged =>
               Read_RelatedUnchangedDocumentDiagnosticReport
                 (Handler, Value.unchanged);
         end case;
      end;
   end Read_DocumentDiagnosticReport;

   package DocumentRangeFormattingClientCapabilities_Scope is
      package DocumentRangeFormattingClientCapabilities_Map is new Minimal_Perfect_Hash
        (["dynamicRegistration"]);

   end DocumentRangeFormattingClientCapabilities_Scope;

   procedure Read_DocumentRangeFormattingClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value : out LSP.Structures.DocumentRangeFormattingClientCapabilities) is
      use DocumentRangeFormattingClientCapabilities_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case DocumentRangeFormattingClientCapabilities_Map.Get_Index
              (Key) is
               when 1 =>  --  dynamicRegistration
                  Value.dynamicRegistration       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.dynamicRegistration.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_DocumentRangeFormattingClientCapabilities;

   package Moniker_Scope is
      package Moniker_Map is new Minimal_Perfect_Hash
        (["scheme",
         "identifier",
         "unique",
         "kind"]);

   end Moniker_Scope;

   procedure Read_Moniker
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Moniker) is
      use Moniker_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case Moniker_Map.Get_Index (Key) is
               when 1 =>  --  scheme
                  Value.scheme.Clear;
                  Value.scheme.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 2 =>  --  identifier
                  Value.identifier.Clear;
                  Value.identifier.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 3 =>  --  unique
                  Read_UniquenessLevel (Handler, Value.unique);
               when 4 =>  --  kind
                  Value.kind :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_MonikerKind (Handler, Value.kind.Value);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_Moniker;

   package ExecuteCommandParams_Scope is
      package ExecuteCommandParams_Map is new Minimal_Perfect_Hash
        (["workDoneToken",
         "command",
         "arguments"]);

   end ExecuteCommandParams_Scope;

   procedure Read_ExecuteCommandParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ExecuteCommandParams) is
      use ExecuteCommandParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case ExecuteCommandParams_Map.Get_Index (Key) is
               when 1 =>  --  workDoneToken
                  Value.workDoneToken :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_ProgressToken (Handler, Value.workDoneToken.Value);
               when 2 =>  --  command
                  Value.command.Clear;
                  Value.command.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 3 =>  --  arguments
                  Read_LSPAny_Vector (Handler, Value.arguments);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_ExecuteCommandParams;

   package WorkDoneProgressBegin_Scope is
      package WorkDoneProgressBegin_Map is new Minimal_Perfect_Hash
        (["kind",
         "title",
         "cancellable",
         "message",
         "percentage"]);

   end WorkDoneProgressBegin_Scope;

   procedure Read_WorkDoneProgressBegin
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.WorkDoneProgressBegin) is
      use WorkDoneProgressBegin_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case WorkDoneProgressBegin_Map.Get_Index (Key) is
               when 1 =>  --  kind
                  Handler.Read_Next;  --  Skip string literal: begin
               when 2 =>  --  title
                  Value.title.Clear;
                  Value.title.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 3 =>  --  cancellable
                  Value.cancellable       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.cancellable.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 4 =>  --  message
                  Value.message.Clear;
                  Value.message.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 5 =>  --  percentage
                  Value.percentage       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.percentage.Value :=
                    Integer (Handler.Number_Value.Integer_Value);
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_WorkDoneProgressBegin;

   package DiagnosticWorkspaceClientCapabilities_Scope is
      package DiagnosticWorkspaceClientCapabilities_Map is new Minimal_Perfect_Hash
        (["refreshSupport"]);

   end DiagnosticWorkspaceClientCapabilities_Scope;

   procedure Read_DiagnosticWorkspaceClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DiagnosticWorkspaceClientCapabilities) is
      use DiagnosticWorkspaceClientCapabilities_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case DiagnosticWorkspaceClientCapabilities_Map.Get_Index (Key) is
               when 1 =>  --  refreshSupport
                  Value.refreshSupport       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.refreshSupport.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_DiagnosticWorkspaceClientCapabilities;

   package DefinitionClientCapabilities_Scope is
      package DefinitionClientCapabilities_Map is new Minimal_Perfect_Hash
        (["dynamicRegistration",
         "linkSupport"]);

   end DefinitionClientCapabilities_Scope;

   procedure Read_DefinitionClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DefinitionClientCapabilities) is
      use DefinitionClientCapabilities_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case DefinitionClientCapabilities_Map.Get_Index (Key) is
               when 1 =>  --  dynamicRegistration
                  Value.dynamicRegistration       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.dynamicRegistration.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 2 =>  --  linkSupport
                  Value.linkSupport       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.linkSupport.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_DefinitionClientCapabilities;

   package SignatureHelpRegistrationOptions_Scope is
      package SignatureHelpRegistrationOptions_Map is new Minimal_Perfect_Hash
        (["documentSelector",
         "workDoneProgress",
         "triggerCharacters",
         "retriggerCharacters"]);

   end SignatureHelpRegistrationOptions_Scope;

   procedure Read_SignatureHelpRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SignatureHelpRegistrationOptions) is
      use SignatureHelpRegistrationOptions_Scope;
      procedure Read_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.DocumentSelector_Or_Null);

      procedure Read_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.DocumentSelector_Or_Null) is
      begin
         if Handler.Is_Null_Value then
            Handler.Read_Next;
         else
            Value :=
              (Is_Null => False,
               Value   => <>);
            Read_DocumentSelector (Handler, Value.Value);
         end if;
      end Read_DocumentSelector_Or_Null;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case SignatureHelpRegistrationOptions_Map.Get_Index (Key) is
               when 1 =>  --  documentSelector
                  Read_DocumentSelector_Or_Null
                    (Handler, Value.Parent.documentSelector);
               when 2 =>  --  workDoneProgress
                  Value.workDoneProgress       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.workDoneProgress.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 3 =>  --  triggerCharacters
                  Read_Virtual_String_Vector
                    (Handler, Value.triggerCharacters);
               when 4 =>  --  retriggerCharacters
                  Read_Virtual_String_Vector
                    (Handler, Value.retriggerCharacters);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_SignatureHelpRegistrationOptions;

   package TypeDefinitionOptions_Scope is
      package TypeDefinitionOptions_Map is new Minimal_Perfect_Hash
        (["workDoneProgress"]);

   end TypeDefinitionOptions_Scope;

   procedure Read_TypeDefinitionOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TypeDefinitionOptions) is
      use TypeDefinitionOptions_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case TypeDefinitionOptions_Map.Get_Index (Key) is
               when 1 =>  --  workDoneProgress
                  Value.workDoneProgress       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.workDoneProgress.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_TypeDefinitionOptions;

   package MarkupContent_Scope is
      package MarkupContent_Map is new Minimal_Perfect_Hash
        (["kind",
         "value"]);

   end MarkupContent_Scope;

   procedure Read_MarkupContent
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.MarkupContent) is
      use MarkupContent_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case MarkupContent_Map.Get_Index (Key) is
               when 1 =>  --  kind
                  Read_MarkupKind (Handler, Value.kind);
               when 2 =>  --  value
                  Value.value.Clear;
                  Value.value.Append (Handler.String_Value);
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_MarkupContent;

   package WorkspaceEdit_Scope is
      package WorkspaceEdit_Map is new Minimal_Perfect_Hash
        (["changes",
         "documentChanges",
         "changeAnnotations"]);

      package documentChanges_OfWorkspaceEdit_Item_Scope is
         package documentChanges_OfWorkspaceEdit_Item_Map is new Minimal_Perfect_Hash
           (["create",
            "rename",
            "delete"]);

      end documentChanges_OfWorkspaceEdit_Item_Scope;

   end WorkspaceEdit_Scope;

   procedure Read_WorkspaceEdit
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.WorkspaceEdit) is
      use WorkspaceEdit_Scope;
      procedure Read_changes_OfWorkspaceEdit
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.changes_OfWorkspaceEdit);

      procedure Read_documentChanges_OfWorkspaceEdit_Item
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.documentChanges_OfWorkspaceEdit_Item);

      procedure Read_changeAnnotations_OfWorkspaceEdit
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.changeAnnotations_OfWorkspaceEdit);

      procedure Read_documentChanges_OfWorkspaceEdit
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.documentChanges_OfWorkspaceEdit);

      procedure Read_changes_OfWorkspaceEdit
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.changes_OfWorkspaceEdit) is
      begin
         pragma Assert (Handler.Is_Start_Object);
         Handler.Read_Next;

         while not Handler.Is_End_Object loop
            declare
               Map   : LSP.Structures.changes_OfWorkspaceEdit renames Value;
               Key   : LSP.Structures.DocumentUri;
               Value : LSP.Structures.TextEdit_Vector;
            begin
               Key := (Handler.Key_Name with null record);
               Handler.Read_Next;
               Read_TextEdit_Vector (Handler, Value);
               Map.Insert (Key, Value);
            end;
         end loop;

         Handler.Read_Next;

      end Read_changes_OfWorkspaceEdit;

      procedure Read_documentChanges_OfWorkspaceEdit_Item
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.documentChanges_OfWorkspaceEdit_Item) is
         use documentChanges_OfWorkspaceEdit_Item_Scope;
      begin
         declare
            Parent  :
              constant not null access VSS.JSON.Pull_Readers.JSON_Pull_Reader'
                Class :=
              Handler'Access;
            Handler :
              VSS.JSON.Pull_Readers.Buffered.JSON_Buffered_Pull_Reader
                (Parent);
            Kind    : Natural;
         begin
            Handler.Mark;
            pragma Assert (Handler.Is_Start_Object);
            Handler.Read_Next;
            while Handler.Is_Key_Name loop
               declare
                  use type VSS.Strings.Virtual_String;
                  Key : constant VSS.Strings.Virtual_String :=
                    Handler.Key_Name;
               begin
                  Handler.Read_Next;
                  if Key = "kind" then
                     pragma Assert (Handler.Is_String_Value);
                     Kind :=
                       documentChanges_OfWorkspaceEdit_Item_Map.Get_Index
                         (Handler.String_Value);
                     case Kind is
                        when 1 =>  --  create
                           Value :=
                             (Kind   => LSP.Structures.create,
                              others => <>);
                        when 2 =>  --  rename
                           Value :=
                             (Kind   => LSP.Structures.rename,
                              others => <>);
                        when 3 =>  --  delete
                           Value :=
                             (Kind   => LSP.Structures.delete,
                              others => <>);
                        when others =>
                           raise Constraint_Error;
                     end case;
                     exit;
                  else
                     Handler.Skip_Current_Value;
                  end if;
               end;
            end loop;

            Handler.Reset;
            Handler.Unmark;

            case Value.Kind is
               when LSP.Structures.Variant_1 =>
                  Read_TextDocumentEdit (Handler, Value.Variant_1);
               when LSP.Structures.create =>
                  Read_CreateFile (Handler, Value.create);
               when LSP.Structures.rename =>
                  Read_RenameFile (Handler, Value.rename);
               when LSP.Structures.delete =>
                  Read_DeleteFile (Handler, Value.delete);
            end case;
         end;
      end Read_documentChanges_OfWorkspaceEdit_Item;

      procedure Read_changeAnnotations_OfWorkspaceEdit
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.changeAnnotations_OfWorkspaceEdit) is
      begin
         pragma Assert (Handler.Is_Start_Object);
         Handler.Read_Next;

         while not Handler.Is_End_Object loop
            declare
               Map   :
                 LSP.Structures.changeAnnotations_OfWorkspaceEdit renames
                 Value;
               Key   : LSP.Structures.ChangeAnnotationIdentifier;
               Value : LSP.Structures.ChangeAnnotation;
            begin
               Key := (Handler.Key_Name with null record);
               Handler.Read_Next;
               Read_ChangeAnnotation (Handler, Value);
               Map.Insert (Key, Value);
            end;
         end loop;

         Handler.Read_Next;

      end Read_changeAnnotations_OfWorkspaceEdit;

      procedure Read_documentChanges_OfWorkspaceEdit
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.documentChanges_OfWorkspaceEdit) is
      begin
         pragma Assert (Handler.Is_Start_Array);
         Handler.Read_Next;

         declare
            Set : LSP.Structures.documentChanges_OfWorkspaceEdit renames Value;
            Value : LSP.Structures.documentChanges_OfWorkspaceEdit_Item;
         begin
            Set.Clear;
            while not Handler.Is_End_Array loop
               Read_documentChanges_OfWorkspaceEdit_Item (Handler, Value);
               Set.Append (Value);
            end loop;
         end;

         Handler.Read_Next;
      end Read_documentChanges_OfWorkspaceEdit;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case WorkspaceEdit_Map.Get_Index (Key) is
               when 1 =>  --  changes
                  pragma Assert (Handler.Is_Start_Object);
                  Handler.Read_Next;

                  while not Handler.Is_End_Object loop
                     declare
                        Map   :
                          LSP.Structures.changes_OfWorkspaceEdit renames
                          Value.changes;
                        Key   : LSP.Structures.DocumentUri;
                        Value : LSP.Structures.TextEdit_Vector;
                     begin
                        Key := (Handler.Key_Name with null record);
                        Handler.Read_Next;
                        Read_TextEdit_Vector (Handler, Value);
                        Map.Insert (Key, Value);
                     end;
                  end loop;

                  Handler.Read_Next;

               when 2 =>  --  documentChanges
                  Read_documentChanges_OfWorkspaceEdit
                    (Handler, Value.documentChanges);
               when 3 =>  --  changeAnnotations
                  pragma Assert (Handler.Is_Start_Object);
                  Handler.Read_Next;

                  while not Handler.Is_End_Object loop
                     declare
                        Map   :
                          LSP.Structures
                            .changeAnnotations_OfWorkspaceEdit renames
                          Value.changeAnnotations;
                        Key   : LSP.Structures.ChangeAnnotationIdentifier;
                        Value : LSP.Structures.ChangeAnnotation;
                     begin
                        Key := (Handler.Key_Name with null record);
                        Handler.Read_Next;
                        Read_ChangeAnnotation (Handler, Value);
                        Map.Insert (Key, Value);
                     end;
                  end loop;

                  Handler.Read_Next;

               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_WorkspaceEdit;

   package DocumentHighlight_Scope is
      package DocumentHighlight_Map is new Minimal_Perfect_Hash
        (["range",
         "kind"]);

   end DocumentHighlight_Scope;

   procedure Read_DocumentHighlight
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentHighlight) is
      use DocumentHighlight_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case DocumentHighlight_Map.Get_Index (Key) is
               when 1 =>  --  range
                  Read_A_Range (Handler, Value.a_range);
               when 2 =>  --  kind
                  Value.kind :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_DocumentHighlightKind (Handler, Value.kind.Value);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_DocumentHighlight;

   package MessageActionItem_Scope is
      package MessageActionItem_Map is new Minimal_Perfect_Hash (["title"]);

   end MessageActionItem_Scope;

   procedure Read_MessageActionItem
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.MessageActionItem) is
      use MessageActionItem_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case MessageActionItem_Map.Get_Index (Key) is
               when 1 =>  --  title
                  Value.title.Clear;
                  Value.title.Append (Handler.String_Value);
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_MessageActionItem;

   procedure Read_TextDocumentItem_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TextDocumentItem_Vector) is
   begin
      pragma Assert (Handler.Is_Start_Array);
      Handler.Read_Next;

      declare
         Set   : LSP.Structures.TextDocumentItem_Vector renames Value;
         Value : LSP.Structures.TextDocumentItem;
      begin
         Set.Clear;
         while not Handler.Is_End_Array loop
            Read_TextDocumentItem (Handler, Value);
            Set.Append (Value);
         end loop;
      end;

      Handler.Read_Next;
   end Read_TextDocumentItem_Vector;

   package InlineValueRegistrationOptions_Scope is
      package InlineValueRegistrationOptions_Map is new Minimal_Perfect_Hash
        (["workDoneProgress",
         "documentSelector",
         "id"]);

   end InlineValueRegistrationOptions_Scope;

   procedure Read_InlineValueRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.InlineValueRegistrationOptions) is
      use InlineValueRegistrationOptions_Scope;
      procedure Read_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.DocumentSelector_Or_Null);

      procedure Read_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.DocumentSelector_Or_Null) is
      begin
         if Handler.Is_Null_Value then
            Handler.Read_Next;
         else
            Value :=
              (Is_Null => False,
               Value   => <>);
            Read_DocumentSelector (Handler, Value.Value);
         end if;
      end Read_DocumentSelector_Or_Null;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case InlineValueRegistrationOptions_Map.Get_Index (Key) is
               when 1 =>  --  workDoneProgress
                  Value.workDoneProgress       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.workDoneProgress.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 2 =>  --  documentSelector
                  Read_DocumentSelector_Or_Null
                    (Handler, Value.Parent.documentSelector);
               when 3 =>  --  id
                  Value.id.Clear;
                  Value.id.Append (Handler.String_Value);
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_InlineValueRegistrationOptions;

   package VersionedNotebookDocumentIdentifier_Scope is
      package VersionedNotebookDocumentIdentifier_Map is new Minimal_Perfect_Hash
        (["version",
         "uri"]);

   end VersionedNotebookDocumentIdentifier_Scope;

   procedure Read_VersionedNotebookDocumentIdentifier
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.VersionedNotebookDocumentIdentifier) is
      use VersionedNotebookDocumentIdentifier_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case VersionedNotebookDocumentIdentifier_Map.Get_Index (Key) is
               when 1 =>  --  version
                  Value.version :=
                    Integer (Handler.Number_Value.Integer_Value);
                  Handler.Read_Next;
               when 2 =>  --  uri
                  Read_URI (Handler, Value.uri);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_VersionedNotebookDocumentIdentifier;

   package BaseSymbolInformation_Scope is
      package BaseSymbolInformation_Map is new Minimal_Perfect_Hash
        (["name",
         "kind",
         "tags",
         "containerName"]);

   end BaseSymbolInformation_Scope;

   procedure Read_BaseSymbolInformation
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.BaseSymbolInformation) is
      use BaseSymbolInformation_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case BaseSymbolInformation_Map.Get_Index (Key) is
               when 1 =>  --  name
                  Value.name.Clear;
                  Value.name.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 2 =>  --  kind
                  Read_SymbolKind (Handler, Value.kind);
               when 3 =>  --  tags
                  Read_SymbolTag_Set (Handler, Value.tags);
               when 4 =>  --  containerName
                  Value.containerName.Clear;
                  Value.containerName.Append (Handler.String_Value);
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_BaseSymbolInformation;

   package InlayHintParams_Scope is
      package InlayHintParams_Map is new Minimal_Perfect_Hash
        (["workDoneToken",
         "textDocument",
         "range"]);

   end InlayHintParams_Scope;

   procedure Read_InlayHintParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.InlayHintParams) is
      use InlayHintParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case InlayHintParams_Map.Get_Index (Key) is
               when 1 =>  --  workDoneToken
                  Value.workDoneToken :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_ProgressToken (Handler, Value.workDoneToken.Value);
               when 2 =>  --  textDocument
                  Read_TextDocumentIdentifier (Handler, Value.textDocument);
               when 3 =>  --  range
                  Read_A_Range (Handler, Value.a_range);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_InlayHintParams;

   package WorkDoneProgressCreateParams_Scope is
      package WorkDoneProgressCreateParams_Map is new Minimal_Perfect_Hash
        (["token"]);

   end WorkDoneProgressCreateParams_Scope;

   procedure Read_WorkDoneProgressCreateParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.WorkDoneProgressCreateParams) is
      use WorkDoneProgressCreateParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case WorkDoneProgressCreateParams_Map.Get_Index (Key) is
               when 1 =>  --  token
                  Read_ProgressToken (Handler, Value.token);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_WorkDoneProgressCreateParams;

   package TextEdit_Scope is
      package TextEdit_Map is new Minimal_Perfect_Hash
        (["range",
         "newText"]);

   end TextEdit_Scope;

   procedure Read_TextEdit
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TextEdit) is
      use TextEdit_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case TextEdit_Map.Get_Index (Key) is
               when 1 =>  --  range
                  Read_A_Range (Handler, Value.a_range);
               when 2 =>  --  newText
                  Value.newText.Clear;
                  Value.newText.Append (Handler.String_Value);
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_TextEdit;

   package DeclarationClientCapabilities_Scope is
      package DeclarationClientCapabilities_Map is new Minimal_Perfect_Hash
        (["dynamicRegistration",
         "linkSupport"]);

   end DeclarationClientCapabilities_Scope;

   procedure Read_DeclarationClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DeclarationClientCapabilities) is
      use DeclarationClientCapabilities_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case DeclarationClientCapabilities_Map.Get_Index (Key) is
               when 1 =>  --  dynamicRegistration
                  Value.dynamicRegistration       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.dynamicRegistration.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 2 =>  --  linkSupport
                  Value.linkSupport       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.linkSupport.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_DeclarationClientCapabilities;

   package SignatureHelpOptions_Scope is
      package SignatureHelpOptions_Map is new Minimal_Perfect_Hash
        (["workDoneProgress",
         "triggerCharacters",
         "retriggerCharacters"]);

   end SignatureHelpOptions_Scope;

   procedure Read_SignatureHelpOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SignatureHelpOptions) is
      use SignatureHelpOptions_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case SignatureHelpOptions_Map.Get_Index (Key) is
               when 1 =>  --  workDoneProgress
                  Value.workDoneProgress       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.workDoneProgress.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 2 =>  --  triggerCharacters
                  Read_Virtual_String_Vector
                    (Handler, Value.triggerCharacters);
               when 3 =>  --  retriggerCharacters
                  Read_Virtual_String_Vector
                    (Handler, Value.retriggerCharacters);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_SignatureHelpOptions;

   package SemanticTokensEdit_Scope is
      package SemanticTokensEdit_Map is new Minimal_Perfect_Hash
        (["start",
         "deleteCount",
         "data"]);

   end SemanticTokensEdit_Scope;

   procedure Read_SemanticTokensEdit
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SemanticTokensEdit) is
      use SemanticTokensEdit_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case SemanticTokensEdit_Map.Get_Index (Key) is
               when 1 =>  --  start
                  Value.start := Integer (Handler.Number_Value.Integer_Value);
                  Handler.Read_Next;
               when 2 =>  --  deleteCount
                  Value.deleteCount :=
                    Integer (Handler.Number_Value.Integer_Value);
                  Handler.Read_Next;
               when 3 =>  --  data
                  Read_Natural_Vector (Handler, Value.data);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_SemanticTokensEdit;

   package DocumentHighlightParams_Scope is
      package DocumentHighlightParams_Map is new Minimal_Perfect_Hash
        (["textDocument",
         "position",
         "workDoneToken",
         "partialResultToken"]);

   end DocumentHighlightParams_Scope;

   procedure Read_DocumentHighlightParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentHighlightParams) is
      use DocumentHighlightParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case DocumentHighlightParams_Map.Get_Index (Key) is
               when 1 =>  --  textDocument
                  Read_TextDocumentIdentifier (Handler, Value.textDocument);
               when 2 =>  --  position
                  Read_Position (Handler, Value.position);
               when 3 =>  --  workDoneToken
                  Value.workDoneToken :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_ProgressToken (Handler, Value.workDoneToken.Value);
               when 4 =>  --  partialResultToken
                  Value.partialResultToken :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_ProgressToken (Handler, Value.partialResultToken.Value);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_DocumentHighlightParams;

   package FileCreate_Scope is
      package FileCreate_Map is new Minimal_Perfect_Hash (["uri"]);

   end FileCreate_Scope;

   procedure Read_FileCreate
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.FileCreate) is
      use FileCreate_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case FileCreate_Map.Get_Index (Key) is
               when 1 =>  --  uri
                  Value.uri.Clear;
                  Value.uri.Append (Handler.String_Value);
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_FileCreate;

   package WorkspaceFullDocumentDiagnosticReport_Scope is
      package WorkspaceFullDocumentDiagnosticReport_Map is new Minimal_Perfect_Hash
        (["kind",
         "resultId",
         "items",
         "uri",
         "version"]);

   end WorkspaceFullDocumentDiagnosticReport_Scope;

   procedure Read_WorkspaceFullDocumentDiagnosticReport
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.WorkspaceFullDocumentDiagnosticReport) is
      use WorkspaceFullDocumentDiagnosticReport_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case WorkspaceFullDocumentDiagnosticReport_Map.Get_Index (Key) is
               when 1 =>  --  kind
                  Handler.Read_Next;  --  Skip string literal: full
               when 2 =>  --  resultId
                  Value.resultId.Clear;
                  Value.resultId.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 3 =>  --  items
                  Read_Diagnostic_Vector (Handler, Value.items);
               when 4 =>  --  uri
                  Value.uri := (Handler.String_Value with null record);
                  Handler.Read_Next;
               when 5 =>  --  version
                  Read_Integer_Or_Null (Handler, Value.version);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_WorkspaceFullDocumentDiagnosticReport;

   procedure Read_SemanticTokens_Or_Null
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SemanticTokens_Or_Null) is
   begin
      if Handler.Is_Null_Value then
         Handler.Read_Next;
      else
         Value :=
           (Is_Null => False,
            Value   => <>);
         Read_SemanticTokens (Handler, Value.Value);
      end if;
   end Read_SemanticTokens_Or_Null;

   package SemanticTokensRangeParams_Scope is
      package SemanticTokensRangeParams_Map is new Minimal_Perfect_Hash
        (["workDoneToken",
         "partialResultToken",
         "textDocument",
         "range"]);

   end SemanticTokensRangeParams_Scope;

   procedure Read_SemanticTokensRangeParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SemanticTokensRangeParams) is
      use SemanticTokensRangeParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case SemanticTokensRangeParams_Map.Get_Index (Key) is
               when 1 =>  --  workDoneToken
                  Value.workDoneToken :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_ProgressToken (Handler, Value.workDoneToken.Value);
               when 2 =>  --  partialResultToken
                  Value.partialResultToken :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_ProgressToken (Handler, Value.partialResultToken.Value);
               when 3 =>  --  textDocument
                  Read_TextDocumentIdentifier (Handler, Value.textDocument);
               when 4 =>  --  range
                  Read_A_Range (Handler, Value.a_range);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_SemanticTokensRangeParams;

   package FoldingRange_Scope is
      package FoldingRange_Map is new Minimal_Perfect_Hash
        (["startLine",
         "startCharacter",
         "endLine",
         "endCharacter",
         "kind",
         "collapsedText"]);

   end FoldingRange_Scope;

   procedure Read_FoldingRange
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.FoldingRange) is
      use FoldingRange_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case FoldingRange_Map.Get_Index (Key) is
               when 1 =>  --  startLine
                  Value.startLine :=
                    Integer (Handler.Number_Value.Integer_Value);
                  Handler.Read_Next;
               when 2 =>  --  startCharacter
                  Value.startCharacter       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.startCharacter.Value :=
                    Integer (Handler.Number_Value.Integer_Value);
                  Handler.Read_Next;
               when 3 =>  --  endLine
                  Value.endLine :=
                    Integer (Handler.Number_Value.Integer_Value);
                  Handler.Read_Next;
               when 4 =>  --  endCharacter
                  Value.endCharacter       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.endCharacter.Value :=
                    Integer (Handler.Number_Value.Integer_Value);
                  Handler.Read_Next;
               when 5 =>  --  kind
                  Value.kind :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_FoldingRangeKind (Handler, Value.kind.Value);
               when 6 =>  --  collapsedText
                  Value.collapsedText.Clear;
                  Value.collapsedText.Append (Handler.String_Value);
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_FoldingRange;

   package SemanticTokensDeltaPartialResult_Scope is
      package SemanticTokensDeltaPartialResult_Map is new Minimal_Perfect_Hash
        (["edits"]);

   end SemanticTokensDeltaPartialResult_Scope;

   procedure Read_SemanticTokensDeltaPartialResult
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SemanticTokensDeltaPartialResult) is
      use SemanticTokensDeltaPartialResult_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case SemanticTokensDeltaPartialResult_Map.Get_Index (Key) is
               when 1 =>  --  edits
                  Read_SemanticTokensEdit_Vector (Handler, Value.edits);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_SemanticTokensDeltaPartialResult;

   package NotebookDocumentClientCapabilities_Scope is
      package NotebookDocumentClientCapabilities_Map is new Minimal_Perfect_Hash
        (["synchronization"]);

   end NotebookDocumentClientCapabilities_Scope;

   procedure Read_NotebookDocumentClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.NotebookDocumentClientCapabilities) is
      use NotebookDocumentClientCapabilities_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case NotebookDocumentClientCapabilities_Map.Get_Index (Key) is
               when 1 =>  --  synchronization
                  Read_NotebookDocumentSyncClientCapabilities
                    (Handler, Value.synchronization);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_NotebookDocumentClientCapabilities;

   package CompletionParams_Scope is
      package CompletionParams_Map is new Minimal_Perfect_Hash
        (["textDocument",
         "position",
         "workDoneToken",
         "partialResultToken",
         "context"]);

   end CompletionParams_Scope;

   procedure Read_CompletionParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CompletionParams) is
      use CompletionParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case CompletionParams_Map.Get_Index (Key) is
               when 1 =>  --  textDocument
                  Read_TextDocumentIdentifier (Handler, Value.textDocument);
               when 2 =>  --  position
                  Read_Position (Handler, Value.position);
               when 3 =>  --  workDoneToken
                  Value.workDoneToken :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_ProgressToken (Handler, Value.workDoneToken.Value);
               when 4 =>  --  partialResultToken
                  Value.partialResultToken :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_ProgressToken (Handler, Value.partialResultToken.Value);
               when 5 =>  --  context
                  Value.context :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_CompletionContext (Handler, Value.context.Value);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_CompletionParams;

   package CallHierarchyIncomingCallsParams_Scope is
      package CallHierarchyIncomingCallsParams_Map is new Minimal_Perfect_Hash
        (["workDoneToken",
         "partialResultToken",
         "item"]);

   end CallHierarchyIncomingCallsParams_Scope;

   procedure Read_CallHierarchyIncomingCallsParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CallHierarchyIncomingCallsParams) is
      use CallHierarchyIncomingCallsParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case CallHierarchyIncomingCallsParams_Map.Get_Index (Key) is
               when 1 =>  --  workDoneToken
                  Value.workDoneToken :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_ProgressToken (Handler, Value.workDoneToken.Value);
               when 2 =>  --  partialResultToken
                  Value.partialResultToken :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_ProgressToken (Handler, Value.partialResultToken.Value);
               when 3 =>  --  item
                  Read_CallHierarchyItem (Handler, Value.item);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_CallHierarchyIncomingCallsParams;

   procedure Read_Location_Vector_Or_Null
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Location_Vector_Or_Null) is
   begin
      if Handler.Is_Null_Value then
         Handler.Read_Next;
      else
         Value.Clear;
         Read_Location_Vector (Handler, Value);
      end if;
   end Read_Location_Vector_Or_Null;

   package WorkspaceSymbolClientCapabilities_Scope is
      package WorkspaceSymbolClientCapabilities_Map is new Minimal_Perfect_Hash
        (["dynamicRegistration",
         "symbolKind",
         "tagSupport",
         "resolveSupport"]);

   end WorkspaceSymbolClientCapabilities_Scope;

   procedure Read_WorkspaceSymbolClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.WorkspaceSymbolClientCapabilities) is
      use WorkspaceSymbolClientCapabilities_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case WorkspaceSymbolClientCapabilities_Map.Get_Index (Key) is
               when 1 =>  --  dynamicRegistration
                  Value.dynamicRegistration       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.dynamicRegistration.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 2 =>  --  symbolKind
                  Value.symbolKind :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_symbolKind_OfWorkspaceSymbolClientCapabilities
                    (Handler, Value.symbolKind.Value);
               when 3 =>  --  tagSupport
                  Value.tagSupport :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_tagSupport_OfWorkspaceSymbolClientCapabilities
                    (Handler, Value.tagSupport.Value);
               when 4 =>  --  resolveSupport
                  Value.resolveSupport :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_resolveSupport_OfWorkspaceSymbolClientCapabilities
                    (Handler, Value.resolveSupport.Value);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_WorkspaceSymbolClientCapabilities;

   package Boolean_Or_Something_Scope is
      package Boolean_Or_Something_Map is new Minimal_Perfect_Hash (["delta"]);

   end Boolean_Or_Something_Scope;

   procedure Read_Boolean_Or_Something
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Boolean_Or_Something) is
      use Boolean_Or_Something_Scope;
   begin
      if Handler.Is_Boolean_Value then
         Value :=
           (Is_Boolean => True,
            others     => <>);
      else
         Value :=
           (Is_Boolean => False,
            others     => <>);
      end if;

      case Value.Is_Boolean is
         when True =>
            Value.Boolean := Handler.Boolean_Value;
            Handler.Read_Next;
         when False =>
            pragma Assert (Handler.Is_Start_Object);
            Handler.Read_Next;

            while Handler.Is_Key_Name loop
               declare
                  Key : constant VSS.Strings.Virtual_String :=
                    Handler.Key_Name;
               begin
                  Handler.Read_Next;
                  case Boolean_Or_Something_Map.Get_Index (Key) is
                     when 1 =>  --  delta
                        Value.a_delta       :=
                          (Is_Set => True,
                           Value  => <>);
                        Value.a_delta.Value := Handler.Boolean_Value;
                        Handler.Read_Next;
                     when others =>
                        Handler.Skip_Current_Value;
                  end case;
               end;
            end loop;

            Handler.Read_Next;
      end case;
   end Read_Boolean_Or_Something;

   package TypeHierarchyOptions_Scope is
      package TypeHierarchyOptions_Map is new Minimal_Perfect_Hash
        (["workDoneProgress"]);

   end TypeHierarchyOptions_Scope;

   procedure Read_TypeHierarchyOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TypeHierarchyOptions) is
      use TypeHierarchyOptions_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case TypeHierarchyOptions_Map.Get_Index (Key) is
               when 1 =>  --  workDoneProgress
                  Value.workDoneProgress       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.workDoneProgress.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_TypeHierarchyOptions;

   package SelectionRangeRegistrationOptions_Scope is
      package SelectionRangeRegistrationOptions_Map is new Minimal_Perfect_Hash
        (["workDoneProgress",
         "documentSelector",
         "id"]);

   end SelectionRangeRegistrationOptions_Scope;

   procedure Read_SelectionRangeRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SelectionRangeRegistrationOptions) is
      use SelectionRangeRegistrationOptions_Scope;
      procedure Read_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.DocumentSelector_Or_Null);

      procedure Read_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.DocumentSelector_Or_Null) is
      begin
         if Handler.Is_Null_Value then
            Handler.Read_Next;
         else
            Value :=
              (Is_Null => False,
               Value   => <>);
            Read_DocumentSelector (Handler, Value.Value);
         end if;
      end Read_DocumentSelector_Or_Null;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case SelectionRangeRegistrationOptions_Map.Get_Index (Key) is
               when 1 =>  --  workDoneProgress
                  Value.workDoneProgress       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.workDoneProgress.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 2 =>  --  documentSelector
                  Read_DocumentSelector_Or_Null
                    (Handler, Value.Parent.documentSelector);
               when 3 =>  --  id
                  Value.id.Clear;
                  Value.id.Append (Handler.String_Value);
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_SelectionRangeRegistrationOptions;

   package DidChangeNotebookDocumentParams_Scope is
      package DidChangeNotebookDocumentParams_Map is new Minimal_Perfect_Hash
        (["notebookDocument",
         "change"]);

   end DidChangeNotebookDocumentParams_Scope;

   procedure Read_DidChangeNotebookDocumentParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DidChangeNotebookDocumentParams) is
      use DidChangeNotebookDocumentParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case DidChangeNotebookDocumentParams_Map.Get_Index (Key) is
               when 1 =>  --  notebookDocument
                  Read_VersionedNotebookDocumentIdentifier
                    (Handler, Value.notebookDocument);
               when 2 =>  --  change
                  Read_NotebookDocumentChangeEvent (Handler, Value.change);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_DidChangeNotebookDocumentParams;

   package RegularExpressionsClientCapabilities_Scope is
      package RegularExpressionsClientCapabilities_Map is new Minimal_Perfect_Hash
        (["engine",
         "version"]);

   end RegularExpressionsClientCapabilities_Scope;

   procedure Read_RegularExpressionsClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.RegularExpressionsClientCapabilities) is
      use RegularExpressionsClientCapabilities_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case RegularExpressionsClientCapabilities_Map.Get_Index (Key) is
               when 1 =>  --  engine
                  Value.engine.Clear;
                  Value.engine.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 2 =>  --  version
                  Value.version.Clear;
                  Value.version.Append (Handler.String_Value);
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_RegularExpressionsClientCapabilities;

   package WorkspaceDocumentDiagnosticReport_Scope is
      package WorkspaceDocumentDiagnosticReport_Map is new Minimal_Perfect_Hash
        (["full",
         "unchanged"]);

   end WorkspaceDocumentDiagnosticReport_Scope;

   procedure Read_WorkspaceDocumentDiagnosticReport
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.WorkspaceDocumentDiagnosticReport) is
      use WorkspaceDocumentDiagnosticReport_Scope;
   begin
      declare
         Parent  :
           constant not null access VSS.JSON.Pull_Readers.JSON_Pull_Reader'
             Class :=
           Handler'Access;
         Handler :
           VSS.JSON.Pull_Readers.Buffered.JSON_Buffered_Pull_Reader (Parent);
         Kind    : Natural;
      begin
         Handler.Mark;
         pragma Assert (Handler.Is_Start_Object);
         Handler.Read_Next;
         while Handler.Is_Key_Name loop
            declare
               use type VSS.Strings.Virtual_String;
               Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
            begin
               Handler.Read_Next;
               if Key = "kind" then
                  pragma Assert (Handler.Is_String_Value);
                  Kind :=
                    WorkspaceDocumentDiagnosticReport_Map.Get_Index
                      (Handler.String_Value);
                  case Kind is
                     when 1 =>  --  full
                        Value :=
                          (Kind   => LSP.Structures.full,
                           others => <>);
                     when 2 =>  --  unchanged
                        Value :=
                          (Kind   => LSP.Structures.unchanged,
                           others => <>);
                     when others =>
                        raise Constraint_Error;
                  end case;
                  exit;
               else
                  Handler.Skip_Current_Value;
               end if;
            end;
         end loop;

         Handler.Reset;
         Handler.Unmark;

         case Value.Kind is
            when LSP.Structures.full =>
               Read_WorkspaceFullDocumentDiagnosticReport
                 (Handler, Value.full);
            when LSP.Structures.unchanged =>
               Read_WorkspaceUnchangedDocumentDiagnosticReport
                 (Handler, Value.unchanged);
         end case;
      end;
   end Read_WorkspaceDocumentDiagnosticReport;

   package WorkspaceClientCapabilities_Scope is
      package WorkspaceClientCapabilities_Map is new Minimal_Perfect_Hash
        (["applyEdit",
         "workspaceEdit",
         "didChangeConfiguration",
         "didChangeWatchedFiles",
         "symbol",
         "executeCommand",
         "workspaceFolders",
         "configuration",
         "semanticTokens",
         "codeLens",
         "fileOperations",
         "inlineValue",
         "inlayHint",
         "diagnostics"]);

   end WorkspaceClientCapabilities_Scope;

   procedure Read_WorkspaceClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.WorkspaceClientCapabilities) is
      use WorkspaceClientCapabilities_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case WorkspaceClientCapabilities_Map.Get_Index (Key) is
               when 1 =>  --  applyEdit
                  Value.applyEdit       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.applyEdit.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 2 =>  --  workspaceEdit
                  Value.workspaceEdit :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_WorkspaceEditClientCapabilities
                    (Handler, Value.workspaceEdit.Value);
               when 3 =>  --  didChangeConfiguration
                  Value.didChangeConfiguration :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_DidChangeConfigurationClientCapabilities
                    (Handler, Value.didChangeConfiguration.Value);
               when 4 =>  --  didChangeWatchedFiles
                  Value.didChangeWatchedFiles :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_DidChangeWatchedFilesClientCapabilities
                    (Handler, Value.didChangeWatchedFiles.Value);
               when 5 =>  --  symbol
                  Value.symbol :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_WorkspaceSymbolClientCapabilities
                    (Handler, Value.symbol.Value);
               when 6 =>  --  executeCommand
                  Value.executeCommand :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_ExecuteCommandClientCapabilities
                    (Handler, Value.executeCommand.Value);
               when 7 =>  --  workspaceFolders
                  Value.workspaceFolders       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.workspaceFolders.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 8 =>  --  configuration
                  Value.configuration       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.configuration.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 9 =>  --  semanticTokens
                  Value.semanticTokens :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_SemanticTokensWorkspaceClientCapabilities
                    (Handler, Value.semanticTokens.Value);
               when 10 =>  --  codeLens
                  Value.codeLens :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_CodeLensWorkspaceClientCapabilities
                    (Handler, Value.codeLens.Value);
               when 11 =>  --  fileOperations
                  Value.fileOperations :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_FileOperationClientCapabilities
                    (Handler, Value.fileOperations.Value);
               when 12 =>  --  inlineValue
                  Value.inlineValue :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_InlineValueWorkspaceClientCapabilities
                    (Handler, Value.inlineValue.Value);
               when 13 =>  --  inlayHint
                  Value.inlayHint :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_InlayHintWorkspaceClientCapabilities
                    (Handler, Value.inlayHint.Value);
               when 14 =>  --  diagnostics
                  Value.diagnostics :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_DiagnosticWorkspaceClientCapabilities
                    (Handler, Value.diagnostics.Value);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_WorkspaceClientCapabilities;

   package WorkspaceDiagnosticParams_Scope is
      package WorkspaceDiagnosticParams_Map is new Minimal_Perfect_Hash
        (["workDoneToken",
         "partialResultToken",
         "identifier",
         "previousResultIds"]);

   end WorkspaceDiagnosticParams_Scope;

   procedure Read_WorkspaceDiagnosticParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.WorkspaceDiagnosticParams) is
      use WorkspaceDiagnosticParams_Scope;
      procedure Read_PreviousResultId_Vector
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.PreviousResultId_Vector);

      procedure Read_PreviousResultId_Vector
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.PreviousResultId_Vector) is
      begin
         pragma Assert (Handler.Is_Start_Array);
         Handler.Read_Next;

         declare
            Set   : LSP.Structures.PreviousResultId_Vector renames Value;
            Value : LSP.Structures.PreviousResultId;
         begin
            Set.Clear;
            while not Handler.Is_End_Array loop
               Read_PreviousResultId (Handler, Value);
               Set.Append (Value);
            end loop;
         end;

         Handler.Read_Next;
      end Read_PreviousResultId_Vector;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case WorkspaceDiagnosticParams_Map.Get_Index (Key) is
               when 1 =>  --  workDoneToken
                  Value.workDoneToken :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_ProgressToken (Handler, Value.workDoneToken.Value);
               when 2 =>  --  partialResultToken
                  Value.partialResultToken :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_ProgressToken (Handler, Value.partialResultToken.Value);
               when 3 =>  --  identifier
                  Value.identifier.Clear;
                  Value.identifier.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 4 =>  --  previousResultIds
                  Read_PreviousResultId_Vector
                    (Handler, Value.previousResultIds);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_WorkspaceDiagnosticParams;

   procedure Read_TextDocumentSyncKind
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.TextDocumentSyncKind) is
   begin
      Value :=
        LSP.Enumerations.TextDocumentSyncKind'Val
          (Handler.Number_Value.Integer_Value + 0);
      Handler.Read_Next;
   end Read_TextDocumentSyncKind;

   package InlayHintOptions_Scope is
      package InlayHintOptions_Map is new Minimal_Perfect_Hash
        (["workDoneProgress",
         "resolveProvider"]);

   end InlayHintOptions_Scope;

   procedure Read_InlayHintOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.InlayHintOptions) is
      use InlayHintOptions_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case InlayHintOptions_Map.Get_Index (Key) is
               when 1 =>  --  workDoneProgress
                  Value.workDoneProgress       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.workDoneProgress.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 2 =>  --  resolveProvider
                  Value.resolveProvider       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.resolveProvider.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_InlayHintOptions;

   package InlineValueOptions_Scope is
      package InlineValueOptions_Map is new Minimal_Perfect_Hash
        (["workDoneProgress"]);

   end InlineValueOptions_Scope;

   procedure Read_InlineValueOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.InlineValueOptions) is
      use InlineValueOptions_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case InlineValueOptions_Map.Get_Index (Key) is
               when 1 =>  --  workDoneProgress
                  Value.workDoneProgress       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.workDoneProgress.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_InlineValueOptions;

   package WorkspaceFoldersChangeEvent_Scope is
      package WorkspaceFoldersChangeEvent_Map is new Minimal_Perfect_Hash
        (["added",
         "removed"]);

   end WorkspaceFoldersChangeEvent_Scope;

   procedure Read_WorkspaceFoldersChangeEvent
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.WorkspaceFoldersChangeEvent) is
      use WorkspaceFoldersChangeEvent_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case WorkspaceFoldersChangeEvent_Map.Get_Index (Key) is
               when 1 =>  --  added
                  Read_WorkspaceFolder_Vector (Handler, Value.added);
               when 2 =>  --  removed
                  Read_WorkspaceFolder_Vector (Handler, Value.removed);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_WorkspaceFoldersChangeEvent;

   package TypeDefinitionClientCapabilities_Scope is
      package TypeDefinitionClientCapabilities_Map is new Minimal_Perfect_Hash
        (["dynamicRegistration",
         "linkSupport"]);

   end TypeDefinitionClientCapabilities_Scope;

   procedure Read_TypeDefinitionClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TypeDefinitionClientCapabilities) is
      use TypeDefinitionClientCapabilities_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case TypeDefinitionClientCapabilities_Map.Get_Index (Key) is
               when 1 =>  --  dynamicRegistration
                  Value.dynamicRegistration       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.dynamicRegistration.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 2 =>  --  linkSupport
                  Value.linkSupport       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.linkSupport.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_TypeDefinitionClientCapabilities;

   package CreateFileOptions_Scope is
      package CreateFileOptions_Map is new Minimal_Perfect_Hash
        (["overwrite",
         "ignoreIfExists"]);

   end CreateFileOptions_Scope;

   procedure Read_CreateFileOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CreateFileOptions) is
      use CreateFileOptions_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case CreateFileOptions_Map.Get_Index (Key) is
               when 1 =>  --  overwrite
                  Value.overwrite       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.overwrite.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 2 =>  --  ignoreIfExists
                  Value.ignoreIfExists       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.ignoreIfExists.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_CreateFileOptions;

   package DiagnosticRelatedInformation_Scope is
      package DiagnosticRelatedInformation_Map is new Minimal_Perfect_Hash
        (["location",
         "message"]);

   end DiagnosticRelatedInformation_Scope;

   procedure Read_DiagnosticRelatedInformation
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DiagnosticRelatedInformation) is
      use DiagnosticRelatedInformation_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case DiagnosticRelatedInformation_Map.Get_Index (Key) is
               when 1 =>  --  location
                  Read_Location (Handler, Value.location);
               when 2 =>  --  message
                  Value.message.Clear;
                  Value.message.Append (Handler.String_Value);
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_DiagnosticRelatedInformation;

   procedure Read_InsertTextMode
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.InsertTextMode) is
   begin
      Value :=
        LSP.Enumerations.InsertTextMode'Val
          (Handler.Number_Value.Integer_Value - 1);
      Handler.Read_Next;
   end Read_InsertTextMode;

   procedure Read_InlineValue_Vector_Or_Null
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.InlineValue_Vector_Or_Null) is
   begin
      if Handler.Is_Null_Value then
         Handler.Read_Next;
      else
         Value.Clear;
         Read_InlineValue_Vector (Handler, Value);
      end if;
   end Read_InlineValue_Vector_Or_Null;

   package DidCloseNotebookDocumentParams_Scope is
      package DidCloseNotebookDocumentParams_Map is new Minimal_Perfect_Hash
        (["notebookDocument",
         "cellTextDocuments"]);

   end DidCloseNotebookDocumentParams_Scope;

   procedure Read_DidCloseNotebookDocumentParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DidCloseNotebookDocumentParams) is
      use DidCloseNotebookDocumentParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case DidCloseNotebookDocumentParams_Map.Get_Index (Key) is
               when 1 =>  --  notebookDocument
                  Read_NotebookDocumentIdentifier
                    (Handler, Value.notebookDocument);
               when 2 =>  --  cellTextDocuments
                  Read_TextDocumentIdentifier_Vector
                    (Handler, Value.cellTextDocuments);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_DidCloseNotebookDocumentParams;

   package ImplementationOptions_Scope is
      package ImplementationOptions_Map is new Minimal_Perfect_Hash
        (["workDoneProgress"]);

   end ImplementationOptions_Scope;

   procedure Read_ImplementationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ImplementationOptions) is
      use ImplementationOptions_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case ImplementationOptions_Map.Get_Index (Key) is
               when 1 =>  --  workDoneProgress
                  Value.workDoneProgress       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.workDoneProgress.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_ImplementationOptions;

   procedure Read_InlayHint_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.InlayHint_Vector) is
   begin
      pragma Assert (Handler.Is_Start_Array);
      Handler.Read_Next;

      declare
         Set   : LSP.Structures.InlayHint_Vector renames Value;
         Value : LSP.Structures.InlayHint;
      begin
         Set.Clear;
         while not Handler.Is_End_Array loop
            Read_InlayHint (Handler, Value);
            Set.Append (Value);
         end loop;
      end;

      Handler.Read_Next;
   end Read_InlayHint_Vector;

   package DocumentColorRegistrationOptions_Scope is
      package DocumentColorRegistrationOptions_Map is new Minimal_Perfect_Hash
        (["documentSelector",
         "workDoneProgress",
         "id"]);

   end DocumentColorRegistrationOptions_Scope;

   procedure Read_DocumentColorRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentColorRegistrationOptions) is
      use DocumentColorRegistrationOptions_Scope;
      procedure Read_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.DocumentSelector_Or_Null);

      procedure Read_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.DocumentSelector_Or_Null) is
      begin
         if Handler.Is_Null_Value then
            Handler.Read_Next;
         else
            Value :=
              (Is_Null => False,
               Value   => <>);
            Read_DocumentSelector (Handler, Value.Value);
         end if;
      end Read_DocumentSelector_Or_Null;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case DocumentColorRegistrationOptions_Map.Get_Index (Key) is
               when 1 =>  --  documentSelector
                  Read_DocumentSelector_Or_Null
                    (Handler, Value.Parent.documentSelector);
               when 2 =>  --  workDoneProgress
                  Value.workDoneProgress       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.workDoneProgress.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 3 =>  --  id
                  Value.id.Clear;
                  Value.id.Append (Handler.String_Value);
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_DocumentColorRegistrationOptions;

   package CodeActionOptions_Scope is
      package CodeActionOptions_Map is new Minimal_Perfect_Hash
        (["workDoneProgress",
         "codeActionKinds",
         "resolveProvider"]);

   end CodeActionOptions_Scope;

   procedure Read_CodeActionOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CodeActionOptions) is
      use CodeActionOptions_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case CodeActionOptions_Map.Get_Index (Key) is
               when 1 =>  --  workDoneProgress
                  Value.workDoneProgress       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.workDoneProgress.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 2 =>  --  codeActionKinds
                  Read_CodeActionKind_Set (Handler, Value.codeActionKinds);
               when 3 =>  --  resolveProvider
                  Value.resolveProvider       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.resolveProvider.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_CodeActionOptions;

   procedure Read_CompletionItemKind
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.CompletionItemKind) is
   begin
      Value :=
        LSP.Enumerations.CompletionItemKind'Val
          (Handler.Number_Value.Integer_Value - 1);
      Handler.Read_Next;
   end Read_CompletionItemKind;

   package ShowDocumentParams_Scope is
      package ShowDocumentParams_Map is new Minimal_Perfect_Hash
        (["uri",
         "external",
         "takeFocus",
         "selection"]);

   end ShowDocumentParams_Scope;

   procedure Read_ShowDocumentParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ShowDocumentParams) is
      use ShowDocumentParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case ShowDocumentParams_Map.Get_Index (Key) is
               when 1 =>  --  uri
                  Read_URI (Handler, Value.uri);
               when 2 =>  --  external
                  Value.external       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.external.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 3 =>  --  takeFocus
                  Value.takeFocus       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.takeFocus.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 4 =>  --  selection
                  Value.selection :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_A_Range (Handler, Value.selection.Value);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_ShowDocumentParams;

   package Command_Or_CodeAction_Scope is
      package Command_Or_CodeAction_Map is new Minimal_Perfect_Hash
        (["arguments",
         "kind",
         "diagnostics",
         "isPreferred",
         "disabled",
         "edit",
         "data"]);

   end Command_Or_CodeAction_Scope;

   procedure Read_Command_Or_CodeAction
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Command_Or_CodeAction) is
      use Command_Or_CodeAction_Scope;
   begin
      declare
         Parent  :
           constant not null access VSS.JSON.Pull_Readers.JSON_Pull_Reader'
             Class :=
           Handler'Access;
         Handler :
           VSS.JSON.Pull_Readers.Buffered.JSON_Buffered_Pull_Reader (Parent);
      begin
         Handler.Mark;
         if Handler.Is_Start_Object then
            Handler.Read_Next;
            while Handler.Is_Key_Name loop
               declare
                  Key   : constant VSS.Strings.Virtual_String :=
                    Handler.Key_Name;
                  Index : constant Natural                    :=
                    Command_Or_CodeAction_Map.Get_Index (Key);
               begin
                  Handler.Read_Next;
                  case Index is
                     when 1 =>  --  arguments
                        Value :=
                          (Is_Command => True,
                           others     => <>);
                        exit;
                     when 2 =>  --  kind
                        Value :=
                          (Is_Command => False,
                           others     => <>);
                        exit;
                     when 3 =>  --  diagnostics
                        Value :=
                          (Is_Command => False,
                           others     => <>);
                        exit;
                     when 4 =>  --  isPreferred
                        Value :=
                          (Is_Command => False,
                           others     => <>);
                        exit;
                     when 5 =>  --  disabled
                        Value :=
                          (Is_Command => False,
                           others     => <>);
                        exit;
                     when 6 =>  --  edit
                        Value :=
                          (Is_Command => False,
                           others     => <>);
                        exit;
                     when 7 =>  --  data
                        Value :=
                          (Is_Command => False,
                           others     => <>);
                        exit;
                     when others =>
                        Handler.Skip_Current_Value;
                  end case;
               end;
            end loop;
         else
            raise Program_Error;  --  Unexpected JSON value
         end if;

         Handler.Reset;
         Handler.Unmark;

         case Value.Is_Command is
            when True =>
               Read_Command (Handler, Value.Command);
            when False =>
               Read_CodeAction (Handler, Value.CodeAction);
         end case;
      end;
   end Read_Command_Or_CodeAction;

   package CreateFilesParams_Scope is
      package CreateFilesParams_Map is new Minimal_Perfect_Hash (["files"]);

   end CreateFilesParams_Scope;

   procedure Read_CreateFilesParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CreateFilesParams) is
      use CreateFilesParams_Scope;
      procedure Read_FileCreate_Vector
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.FileCreate_Vector);

      procedure Read_FileCreate_Vector
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.FileCreate_Vector) is
      begin
         pragma Assert (Handler.Is_Start_Array);
         Handler.Read_Next;

         declare
            Set   : LSP.Structures.FileCreate_Vector renames Value;
            Value : LSP.Structures.FileCreate;
         begin
            Set.Clear;
            while not Handler.Is_End_Array loop
               Read_FileCreate (Handler, Value);
               Set.Append (Value);
            end loop;
         end;

         Handler.Read_Next;
      end Read_FileCreate_Vector;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case CreateFilesParams_Map.Get_Index (Key) is
               when 1 =>  --  files
                  Read_FileCreate_Vector (Handler, Value.files);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_CreateFilesParams;

   procedure Read_ColorPresentation_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ColorPresentation_Vector) is
   begin
      pragma Assert (Handler.Is_Start_Array);
      Handler.Read_Next;

      declare
         Set   : LSP.Structures.ColorPresentation_Vector renames Value;
         Value : LSP.Structures.ColorPresentation;
      begin
         Set.Clear;
         while not Handler.Is_End_Array loop
            Read_ColorPresentation (Handler, Value);
            Set.Append (Value);
         end loop;
      end;

      Handler.Read_Next;
   end Read_ColorPresentation_Vector;

   procedure Read_ChangeAnnotationIdentifier
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ChangeAnnotationIdentifier) is
   begin
      Value.Clear;
      Value.Append (Handler.String_Value);
      Handler.Read_Next;
   end Read_ChangeAnnotationIdentifier;

   package DocumentFormattingParams_Scope is
      package DocumentFormattingParams_Map is new Minimal_Perfect_Hash
        (["workDoneToken",
         "textDocument",
         "options"]);

   end DocumentFormattingParams_Scope;

   procedure Read_DocumentFormattingParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentFormattingParams) is
      use DocumentFormattingParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case DocumentFormattingParams_Map.Get_Index (Key) is
               when 1 =>  --  workDoneToken
                  Value.workDoneToken :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_ProgressToken (Handler, Value.workDoneToken.Value);
               when 2 =>  --  textDocument
                  Read_TextDocumentIdentifier (Handler, Value.textDocument);
               when 3 =>  --  options
                  Read_FormattingOptions (Handler, Value.options);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_DocumentFormattingParams;

   package SignatureHelpContext_Scope is
      package SignatureHelpContext_Map is new Minimal_Perfect_Hash
        (["triggerKind",
         "triggerCharacter",
         "isRetrigger",
         "activeSignatureHelp"]);

   end SignatureHelpContext_Scope;

   procedure Read_SignatureHelpContext
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SignatureHelpContext) is
      use SignatureHelpContext_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case SignatureHelpContext_Map.Get_Index (Key) is
               when 1 =>  --  triggerKind
                  Read_SignatureHelpTriggerKind (Handler, Value.triggerKind);
               when 2 =>  --  triggerCharacter
                  Value.triggerCharacter.Clear;
                  Value.triggerCharacter.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 3 =>  --  isRetrigger
                  Value.isRetrigger := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 4 =>  --  activeSignatureHelp
                  Value.activeSignatureHelp :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_SignatureHelp
                    (Handler, Value.activeSignatureHelp.Value);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_SignatureHelpContext;

   package DeleteFilesParams_Scope is
      package DeleteFilesParams_Map is new Minimal_Perfect_Hash (["files"]);

   end DeleteFilesParams_Scope;

   procedure Read_DeleteFilesParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DeleteFilesParams) is
      use DeleteFilesParams_Scope;
      procedure Read_FileDelete_Vector
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.FileDelete_Vector);

      procedure Read_FileDelete_Vector
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.FileDelete_Vector) is
      begin
         pragma Assert (Handler.Is_Start_Array);
         Handler.Read_Next;

         declare
            Set   : LSP.Structures.FileDelete_Vector renames Value;
            Value : LSP.Structures.FileDelete;
         begin
            Set.Clear;
            while not Handler.Is_End_Array loop
               Read_FileDelete (Handler, Value);
               Set.Append (Value);
            end loop;
         end;

         Handler.Read_Next;
      end Read_FileDelete_Vector;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case DeleteFilesParams_Map.Get_Index (Key) is
               when 1 =>  --  files
                  Read_FileDelete_Vector (Handler, Value.files);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_DeleteFilesParams;

   procedure Read_TypeHierarchyItem_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TypeHierarchyItem_Vector) is
   begin
      pragma Assert (Handler.Is_Start_Array);
      Handler.Read_Next;

      declare
         Set   : LSP.Structures.TypeHierarchyItem_Vector renames Value;
         Value : LSP.Structures.TypeHierarchyItem;
      begin
         Set.Clear;
         while not Handler.Is_End_Array loop
            Read_TypeHierarchyItem (Handler, Value);
            Set.Append (Value);
         end loop;
      end;

      Handler.Read_Next;
   end Read_TypeHierarchyItem_Vector;

   package DocumentOnTypeFormattingParams_Scope is
      package DocumentOnTypeFormattingParams_Map is new Minimal_Perfect_Hash
        (["textDocument",
         "position",
         "ch",
         "options"]);

   end DocumentOnTypeFormattingParams_Scope;

   procedure Read_DocumentOnTypeFormattingParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentOnTypeFormattingParams) is
      use DocumentOnTypeFormattingParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case DocumentOnTypeFormattingParams_Map.Get_Index (Key) is
               when 1 =>  --  textDocument
                  Read_TextDocumentIdentifier (Handler, Value.textDocument);
               when 2 =>  --  position
                  Read_Position (Handler, Value.position);
               when 3 =>  --  ch
                  Value.ch.Clear;
                  Value.ch.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 4 =>  --  options
                  Read_FormattingOptions (Handler, Value.options);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_DocumentOnTypeFormattingParams;

   package CallHierarchyOutgoingCall_Scope is
      package CallHierarchyOutgoingCall_Map is new Minimal_Perfect_Hash
        (["to",
         "fromRanges"]);

   end CallHierarchyOutgoingCall_Scope;

   procedure Read_CallHierarchyOutgoingCall
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CallHierarchyOutgoingCall) is
      use CallHierarchyOutgoingCall_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case CallHierarchyOutgoingCall_Map.Get_Index (Key) is
               when 1 =>  --  to
                  Read_CallHierarchyItem (Handler, Value.to);
               when 2 =>  --  fromRanges
                  Read_Range_Vector (Handler, Value.fromRanges);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_CallHierarchyOutgoingCall;

   procedure Read_Location_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Location_Vector) is
   begin
      pragma Assert (Handler.Is_Start_Array);
      Handler.Read_Next;

      declare
         Set   : LSP.Structures.Location_Vector renames Value;
         Value : LSP.Structures.Location;
      begin
         Set.Clear;
         while not Handler.Is_End_Array loop
            Read_Location (Handler, Value);
            Set.Append (Value);
         end loop;
      end;

      Handler.Read_Next;
   end Read_Location_Vector;

   package TextDocumentContentChangeEvent_Scope is
      package TextDocumentContentChangeEvent_Map is new Minimal_Perfect_Hash
        (["range",
         "rangeLength",
         "text"]);

   end TextDocumentContentChangeEvent_Scope;

   procedure Read_TextDocumentContentChangeEvent
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TextDocumentContentChangeEvent) is
      use TextDocumentContentChangeEvent_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while Handler.Is_Key_Name loop
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case TextDocumentContentChangeEvent_Map.Get_Index (Key) is
               when 1 =>  --  range
                  Value.a_range :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_A_Range (Handler, Value.a_range.Value);
               when 2 =>  --  rangeLength
                  Value.rangeLength       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.rangeLength.Value :=
                    Integer (Handler.Number_Value.Integer_Value);
                  Handler.Read_Next;
               when 3 =>  --  text
                  Value.text.Clear;
                  Value.text.Append (Handler.String_Value);
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_TextDocumentContentChangeEvent;

   package DocumentColorParams_Scope is
      package DocumentColorParams_Map is new Minimal_Perfect_Hash
        (["workDoneToken",
         "partialResultToken",
         "textDocument"]);

   end DocumentColorParams_Scope;

   procedure Read_DocumentColorParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentColorParams) is
      use DocumentColorParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case DocumentColorParams_Map.Get_Index (Key) is
               when 1 =>  --  workDoneToken
                  Value.workDoneToken :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_ProgressToken (Handler, Value.workDoneToken.Value);
               when 2 =>  --  partialResultToken
                  Value.partialResultToken :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_ProgressToken (Handler, Value.partialResultToken.Value);
               when 3 =>  --  textDocument
                  Read_TextDocumentIdentifier (Handler, Value.textDocument);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_DocumentColorParams;

   package WorkspaceFolder_Scope is
      package WorkspaceFolder_Map is new Minimal_Perfect_Hash
        (["uri",
         "name"]);

   end WorkspaceFolder_Scope;

   procedure Read_WorkspaceFolder
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.WorkspaceFolder) is
      use WorkspaceFolder_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case WorkspaceFolder_Map.Get_Index (Key) is
               when 1 =>  --  uri
                  Read_URI (Handler, Value.uri);
               when 2 =>  --  name
                  Value.name.Clear;
                  Value.name.Append (Handler.String_Value);
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_WorkspaceFolder;

   package MarkedString_Scope is
      package MarkedString_Map is new Minimal_Perfect_Hash
        (["language",
         "value"]);

   end MarkedString_Scope;

   procedure Read_MarkedString
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.MarkedString) is
      use MarkedString_Scope;
   begin
      if Handler.Is_String_Value then
         Value :=
           (Is_Virtual_String => True,
            others            => <>);
      else
         Value :=
           (Is_Virtual_String => False,
            others            => <>);
      end if;

      case Value.Is_Virtual_String is
         when True =>
            Value.Virtual_String.Clear;
            Value.Virtual_String.Append (Handler.String_Value);
            Handler.Read_Next;
         when False =>
            pragma Assert (Handler.Is_Start_Object);
            Handler.Read_Next;

            while Handler.Is_Key_Name loop
               declare
                  Key : constant VSS.Strings.Virtual_String :=
                    Handler.Key_Name;
               begin
                  Handler.Read_Next;
                  case MarkedString_Map.Get_Index (Key) is
                     when 1 =>  --  language
                        Value.language.Clear;
                        Value.language.Append (Handler.String_Value);
                        Handler.Read_Next;
                     when 2 =>  --  value
                        Value.value.Clear;
                        Value.value.Append (Handler.String_Value);
                        Handler.Read_Next;
                     when others =>
                        Handler.Skip_Current_Value;
                  end case;
               end;
            end loop;

            Handler.Read_Next;
      end case;
   end Read_MarkedString;

   package NotebookCell_Scope is
      package NotebookCell_Map is new Minimal_Perfect_Hash
        (["kind",
         "document",
         "metadata",
         "executionSummary"]);

   end NotebookCell_Scope;

   procedure Read_NotebookCell
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.NotebookCell) is
      use NotebookCell_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case NotebookCell_Map.Get_Index (Key) is
               when 1 =>  --  kind
                  Read_NotebookCellKind (Handler, Value.kind);
               when 2 =>  --  document
                  Value.document := (Handler.String_Value with null record);
                  Handler.Read_Next;
               when 3 =>  --  metadata
                  Value.metadata :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_LSPObject (Handler, Value.metadata.Value);
               when 4 =>  --  executionSummary
                  Value.executionSummary :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_ExecutionSummary
                    (Handler, Value.executionSummary.Value);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_NotebookCell;

   package DocumentLink_Scope is
      package DocumentLink_Map is new Minimal_Perfect_Hash
        (["range",
         "target",
         "tooltip",
         "data"]);

   end DocumentLink_Scope;

   procedure Read_DocumentLink
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentLink) is
      use DocumentLink_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case DocumentLink_Map.Get_Index (Key) is
               when 1 =>  --  range
                  Read_A_Range (Handler, Value.a_range);
               when 2 =>  --  target
                  Value.target.Clear;
                  Value.target.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 3 =>  --  tooltip
                  Value.tooltip.Clear;
                  Value.tooltip.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 4 =>  --  data
                  Read_LSPAny (Handler, Value.data);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_DocumentLink;

   package WorkspaceFoldersServerCapabilities_Scope is
      package WorkspaceFoldersServerCapabilities_Map is new Minimal_Perfect_Hash
        (["supported",
         "changeNotifications"]);

   end WorkspaceFoldersServerCapabilities_Scope;

   procedure Read_WorkspaceFoldersServerCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.WorkspaceFoldersServerCapabilities) is
      use WorkspaceFoldersServerCapabilities_Scope;
      procedure Read_Virtual_String_Or_Boolean
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.Virtual_String_Or_Boolean);

      procedure Read_Virtual_String_Or_Boolean
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.Virtual_String_Or_Boolean) is
      begin
         if Handler.Is_String_Value then
            Value :=
              (Is_Virtual_String => True,
               others            => <>);
         else
            Value :=
              (Is_Virtual_String => False,
               others            => <>);
         end if;

         case Value.Is_Virtual_String is
            when True =>
               Value.Virtual_String.Clear;
               Value.Virtual_String.Append (Handler.String_Value);
               Handler.Read_Next;
            when False =>
               Value.Boolean := Handler.Boolean_Value;
               Handler.Read_Next;
         end case;
      end Read_Virtual_String_Or_Boolean;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case WorkspaceFoldersServerCapabilities_Map.Get_Index (Key) is
               when 1 =>  --  supported
                  Value.supported       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.supported.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 2 =>  --  changeNotifications
                  Value.changeNotifications :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_Virtual_String_Or_Boolean
                    (Handler, Value.changeNotifications.Value);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_WorkspaceFoldersServerCapabilities;

   package WorkDoneProgressEnd_Scope is
      package WorkDoneProgressEnd_Map is new Minimal_Perfect_Hash
        (["kind",
         "message"]);

   end WorkDoneProgressEnd_Scope;

   procedure Read_WorkDoneProgressEnd
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.WorkDoneProgressEnd) is
      use WorkDoneProgressEnd_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case WorkDoneProgressEnd_Map.Get_Index (Key) is
               when 1 =>  --  kind
                  Handler.Read_Next;  --  Skip string literal: end
               when 2 =>  --  message
                  Value.message.Clear;
                  Value.message.Append (Handler.String_Value);
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_WorkDoneProgressEnd;

   package SemanticTokensRegistrationOptions_Scope is
      package SemanticTokensRegistrationOptions_Map is new Minimal_Perfect_Hash
        (["documentSelector",
         "workDoneProgress",
         "legend",
         "range",
         "full",
         "id"]);

   end SemanticTokensRegistrationOptions_Scope;

   procedure Read_SemanticTokensRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SemanticTokensRegistrationOptions) is
      use SemanticTokensRegistrationOptions_Scope;
      procedure Read_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.DocumentSelector_Or_Null);

      procedure Read_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.DocumentSelector_Or_Null) is
      begin
         if Handler.Is_Null_Value then
            Handler.Read_Next;
         else
            Value :=
              (Is_Null => False,
               Value   => <>);
            Read_DocumentSelector (Handler, Value.Value);
         end if;
      end Read_DocumentSelector_Or_Null;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case SemanticTokensRegistrationOptions_Map.Get_Index (Key) is
               when 1 =>  --  documentSelector
                  Read_DocumentSelector_Or_Null
                    (Handler, Value.Parent.documentSelector);
               when 2 =>  --  workDoneProgress
                  Value.workDoneProgress       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.workDoneProgress.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 3 =>  --  legend
                  Read_SemanticTokensLegend (Handler, Value.legend);
               when 4 =>  --  range
                  Value.a_range :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_Boolean_Or_Any (Handler, Value.a_range.Value);
               when 5 =>  --  full
                  Value.full :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_Boolean_Or_Something (Handler, Value.full.Value);
               when 6 =>  --  id
                  Value.id.Clear;
                  Value.id.Append (Handler.String_Value);
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_SemanticTokensRegistrationOptions;

   procedure Read_CompletionTriggerKind
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.CompletionTriggerKind) is
   begin
      Value :=
        LSP.Enumerations.CompletionTriggerKind'Val
          (Handler.Number_Value.Integer_Value - 1);
      Handler.Read_Next;
   end Read_CompletionTriggerKind;

   package CallHierarchyRegistrationOptions_Scope is
      package CallHierarchyRegistrationOptions_Map is new Minimal_Perfect_Hash
        (["documentSelector",
         "workDoneProgress",
         "id"]);

   end CallHierarchyRegistrationOptions_Scope;

   procedure Read_CallHierarchyRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CallHierarchyRegistrationOptions) is
      use CallHierarchyRegistrationOptions_Scope;
      procedure Read_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.DocumentSelector_Or_Null);

      procedure Read_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.DocumentSelector_Or_Null) is
      begin
         if Handler.Is_Null_Value then
            Handler.Read_Next;
         else
            Value :=
              (Is_Null => False,
               Value   => <>);
            Read_DocumentSelector (Handler, Value.Value);
         end if;
      end Read_DocumentSelector_Or_Null;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case CallHierarchyRegistrationOptions_Map.Get_Index (Key) is
               when 1 =>  --  documentSelector
                  Read_DocumentSelector_Or_Null
                    (Handler, Value.Parent.documentSelector);
               when 2 =>  --  workDoneProgress
                  Value.workDoneProgress       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.workDoneProgress.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 3 =>  --  id
                  Value.id.Clear;
                  Value.id.Append (Handler.String_Value);
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_CallHierarchyRegistrationOptions;

   package CallHierarchyClientCapabilities_Scope is
      package CallHierarchyClientCapabilities_Map is new Minimal_Perfect_Hash
        (["dynamicRegistration"]);

   end CallHierarchyClientCapabilities_Scope;

   procedure Read_CallHierarchyClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CallHierarchyClientCapabilities) is
      use CallHierarchyClientCapabilities_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case CallHierarchyClientCapabilities_Map.Get_Index (Key) is
               when 1 =>  --  dynamicRegistration
                  Value.dynamicRegistration       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.dynamicRegistration.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_CallHierarchyClientCapabilities;

   package DocumentSymbolParams_Scope is
      package DocumentSymbolParams_Map is new Minimal_Perfect_Hash
        (["workDoneToken",
         "partialResultToken",
         "textDocument"]);

   end DocumentSymbolParams_Scope;

   procedure Read_DocumentSymbolParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentSymbolParams) is
      use DocumentSymbolParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case DocumentSymbolParams_Map.Get_Index (Key) is
               when 1 =>  --  workDoneToken
                  Value.workDoneToken :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_ProgressToken (Handler, Value.workDoneToken.Value);
               when 2 =>  --  partialResultToken
                  Value.partialResultToken :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_ProgressToken (Handler, Value.partialResultToken.Value);
               when 3 =>  --  textDocument
                  Read_TextDocumentIdentifier (Handler, Value.textDocument);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_DocumentSymbolParams;

   package DocumentRangeFormattingOptions_Scope is
      package DocumentRangeFormattingOptions_Map is new Minimal_Perfect_Hash
        (["workDoneProgress"]);

   end DocumentRangeFormattingOptions_Scope;

   procedure Read_DocumentRangeFormattingOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentRangeFormattingOptions) is
      use DocumentRangeFormattingOptions_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case DocumentRangeFormattingOptions_Map.Get_Index (Key) is
               when 1 =>  --  workDoneProgress
                  Value.workDoneProgress       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.workDoneProgress.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_DocumentRangeFormattingOptions;

   package InlayHintWorkspaceClientCapabilities_Scope is
      package InlayHintWorkspaceClientCapabilities_Map is new Minimal_Perfect_Hash
        (["refreshSupport"]);

   end InlayHintWorkspaceClientCapabilities_Scope;

   procedure Read_InlayHintWorkspaceClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.InlayHintWorkspaceClientCapabilities) is
      use InlayHintWorkspaceClientCapabilities_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case InlayHintWorkspaceClientCapabilities_Map.Get_Index (Key) is
               when 1 =>  --  refreshSupport
                  Value.refreshSupport       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.refreshSupport.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_InlayHintWorkspaceClientCapabilities;

   procedure Read_InlineValue_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.InlineValue_Vector) is
   begin
      pragma Assert (Handler.Is_Start_Array);
      Handler.Read_Next;

      declare
         Set   : LSP.Structures.InlineValue_Vector renames Value;
         Value : LSP.Structures.InlineValue;
      begin
         Set.Clear;
         while not Handler.Is_End_Array loop
            Read_InlineValue (Handler, Value);
            Set.Append (Value);
         end loop;
      end;

      Handler.Read_Next;
   end Read_InlineValue_Vector;

   package DiagnosticServerCancellationData_Scope is
      package DiagnosticServerCancellationData_Map is new Minimal_Perfect_Hash
        (["retriggerRequest"]);

   end DiagnosticServerCancellationData_Scope;

   procedure Read_DiagnosticServerCancellationData
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DiagnosticServerCancellationData) is
      use DiagnosticServerCancellationData_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case DiagnosticServerCancellationData_Map.Get_Index (Key) is
               when 1 =>  --  retriggerRequest
                  Value.retriggerRequest := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_DiagnosticServerCancellationData;

   package DocumentColorOptions_Scope is
      package DocumentColorOptions_Map is new Minimal_Perfect_Hash
        (["workDoneProgress"]);

   end DocumentColorOptions_Scope;

   procedure Read_DocumentColorOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentColorOptions) is
      use DocumentColorOptions_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case DocumentColorOptions_Map.Get_Index (Key) is
               when 1 =>  --  workDoneProgress
                  Value.workDoneProgress       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.workDoneProgress.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_DocumentColorOptions;

   procedure Read_InitializedParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.InitializedParams) is
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            Handler.Skip_Current_Value;
         end;
      end loop;

      Handler.Read_Next;
   end Read_InitializedParams;

   package FileOperationPatternKind_Map is new Minimal_Perfect_Hash
     (["file",
      "folder"]);

   procedure Read_FileOperationPatternKind
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.FileOperationPatternKind) is
   begin
      Value :=
        LSP.Enumerations.FileOperationPatternKind'Val
          (FileOperationPatternKind_Map.Get_Index (Handler.String_Value) - 1);
      Handler.Read_Next;
   end Read_FileOperationPatternKind;

   package Unregistration_Scope is
      package Unregistration_Map is new Minimal_Perfect_Hash
        (["id",
         "method"]);

   end Unregistration_Scope;

   procedure Read_Unregistration
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Unregistration) is
      use Unregistration_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case Unregistration_Map.Get_Index (Key) is
               when 1 =>  --  id
                  Value.id.Clear;
                  Value.id.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 2 =>  --  method
                  Value.method.Clear;
                  Value.method.Append (Handler.String_Value);
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_Unregistration;

   package HoverOptions_Scope is
      package HoverOptions_Map is new Minimal_Perfect_Hash
        (["workDoneProgress"]);

   end HoverOptions_Scope;

   procedure Read_HoverOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.HoverOptions) is
      use HoverOptions_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case HoverOptions_Map.Get_Index (Key) is
               when 1 =>  --  workDoneProgress
                  Value.workDoneProgress       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.workDoneProgress.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_HoverOptions;

   package DocumentOnTypeFormattingRegistrationOptions_Scope is
      package DocumentOnTypeFormattingRegistrationOptions_Map is new Minimal_Perfect_Hash
        (["documentSelector",
         "firstTriggerCharacter",
         "moreTriggerCharacter"]);

   end DocumentOnTypeFormattingRegistrationOptions_Scope;

   procedure Read_DocumentOnTypeFormattingRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures
        .DocumentOnTypeFormattingRegistrationOptions) is
      use DocumentOnTypeFormattingRegistrationOptions_Scope;
      procedure Read_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.DocumentSelector_Or_Null);

      procedure Read_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.DocumentSelector_Or_Null) is
      begin
         if Handler.Is_Null_Value then
            Handler.Read_Next;
         else
            Value :=
              (Is_Null => False,
               Value   => <>);
            Read_DocumentSelector (Handler, Value.Value);
         end if;
      end Read_DocumentSelector_Or_Null;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case DocumentOnTypeFormattingRegistrationOptions_Map.Get_Index
              (Key) is
               when 1 =>  --  documentSelector
                  Read_DocumentSelector_Or_Null
                    (Handler, Value.Parent.documentSelector);
               when 2 =>  --  firstTriggerCharacter
                  Value.firstTriggerCharacter.Clear;
                  Value.firstTriggerCharacter.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 3 =>  --  moreTriggerCharacter
                  Read_Virtual_String_Vector
                    (Handler, Value.moreTriggerCharacter);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_DocumentOnTypeFormattingRegistrationOptions;

   procedure Read_relatedDocuments_OfDocumentDiagnosticReportPartialResult
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures
        .relatedDocuments_OfDocumentDiagnosticReportPartialResult) is
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         declare
            Map   :
              LSP.Structures
                .relatedDocuments_OfDocumentDiagnosticReportPartialResult renames
              Value;
            Key   : LSP.Structures.DocumentUri;
            Value :
              LSP.Structures
                .relatedDocuments_OfDocumentDiagnosticReportPartialResult_Item;
         begin
            Key := (Handler.Key_Name with null record);
            Handler.Read_Next;
            Read_relatedDocuments_OfDocumentDiagnosticReportPartialResult_Item
              (Handler, Value);
            Map.Insert (Key, Value);
         end;
      end loop;

      Handler.Read_Next;

   end Read_relatedDocuments_OfDocumentDiagnosticReportPartialResult;

   package ReferenceClientCapabilities_Scope is
      package ReferenceClientCapabilities_Map is new Minimal_Perfect_Hash
        (["dynamicRegistration"]);

   end ReferenceClientCapabilities_Scope;

   procedure Read_ReferenceClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ReferenceClientCapabilities) is
      use ReferenceClientCapabilities_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case ReferenceClientCapabilities_Map.Get_Index (Key) is
               when 1 =>  --  dynamicRegistration
                  Value.dynamicRegistration       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.dynamicRegistration.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_ReferenceClientCapabilities;

   package MonikerClientCapabilities_Scope is
      package MonikerClientCapabilities_Map is new Minimal_Perfect_Hash
        (["dynamicRegistration"]);

   end MonikerClientCapabilities_Scope;

   procedure Read_MonikerClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.MonikerClientCapabilities) is
      use MonikerClientCapabilities_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case MonikerClientCapabilities_Map.Get_Index (Key) is
               when 1 =>  --  dynamicRegistration
                  Value.dynamicRegistration       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.dynamicRegistration.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_MonikerClientCapabilities;

   procedure Read_CompletionItemTag_Set
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CompletionItemTag_Set) is
   begin
      pragma Assert (Handler.Is_Start_Array);
      Handler.Read_Next;

      declare
         Set   : LSP.Structures.CompletionItemTag_Set renames Value;
         Value : LSP.Enumerations.CompletionItemTag;
      begin
         Set := (others => False);
         while not Handler.Is_End_Array loop
            Read_CompletionItemTag (Handler, Value);
            Set (Value) := True;
         end loop;
      end;

      Handler.Read_Next;
   end Read_CompletionItemTag_Set;

   package ColorInformation_Scope is
      package ColorInformation_Map is new Minimal_Perfect_Hash
        (["range",
         "color"]);

   end ColorInformation_Scope;

   procedure Read_ColorInformation
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ColorInformation) is
      use ColorInformation_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case ColorInformation_Map.Get_Index (Key) is
               when 1 =>  --  range
                  Read_A_Range (Handler, Value.a_range);
               when 2 =>  --  color
                  Read_Color (Handler, Value.color);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_ColorInformation;

   package A_Range_Scope is
      package A_Range_Map is new Minimal_Perfect_Hash
        (["start",
         "end"]);

   end A_Range_Scope;

   procedure Read_A_Range
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.A_Range) is
      use A_Range_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case A_Range_Map.Get_Index (Key) is
               when 1 =>  --  start
                  Read_Position (Handler, Value.start);
               when 2 =>  --  end
                  Read_Position (Handler, Value.an_end);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_A_Range;

   package ShowMessageRequestParams_Scope is
      package ShowMessageRequestParams_Map is new Minimal_Perfect_Hash
        (["type",
         "message",
         "actions"]);

   end ShowMessageRequestParams_Scope;

   procedure Read_ShowMessageRequestParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ShowMessageRequestParams) is
      use ShowMessageRequestParams_Scope;
      procedure Read_MessageActionItem_Vector
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.MessageActionItem_Vector);

      procedure Read_MessageActionItem_Vector
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.MessageActionItem_Vector) is
      begin
         pragma Assert (Handler.Is_Start_Array);
         Handler.Read_Next;

         declare
            Set   : LSP.Structures.MessageActionItem_Vector renames Value;
            Value : LSP.Structures.MessageActionItem;
         begin
            Set.Clear;
            while not Handler.Is_End_Array loop
               Read_MessageActionItem (Handler, Value);
               Set.Append (Value);
            end loop;
         end;

         Handler.Read_Next;
      end Read_MessageActionItem_Vector;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case ShowMessageRequestParams_Map.Get_Index (Key) is
               when 1 =>  --  type
                  Read_MessageType (Handler, Value.a_type);
               when 2 =>  --  message
                  Value.message.Clear;
                  Value.message.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 3 =>  --  actions
                  Read_MessageActionItem_Vector (Handler, Value.actions);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_ShowMessageRequestParams;

   package DocumentColorClientCapabilities_Scope is
      package DocumentColorClientCapabilities_Map is new Minimal_Perfect_Hash
        (["dynamicRegistration"]);

   end DocumentColorClientCapabilities_Scope;

   procedure Read_DocumentColorClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentColorClientCapabilities) is
      use DocumentColorClientCapabilities_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case DocumentColorClientCapabilities_Map.Get_Index (Key) is
               when 1 =>  --  dynamicRegistration
                  Value.dynamicRegistration       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.dynamicRegistration.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_DocumentColorClientCapabilities;

   procedure Read_WorkspaceEdit_Or_Null
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.WorkspaceEdit_Or_Null) is
   begin
      if Handler.Is_Null_Value then
         Handler.Read_Next;
      else
         Value :=
           (Is_Null => False,
            Value   => <>);
         Read_WorkspaceEdit (Handler, Value.Value);
      end if;
   end Read_WorkspaceEdit_Or_Null;

   package InlineValueEvaluatableExpression_Scope is
      package InlineValueEvaluatableExpression_Map is new Minimal_Perfect_Hash
        (["range",
         "expression"]);

   end InlineValueEvaluatableExpression_Scope;

   procedure Read_InlineValueEvaluatableExpression
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.InlineValueEvaluatableExpression) is
      use InlineValueEvaluatableExpression_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case InlineValueEvaluatableExpression_Map.Get_Index (Key) is
               when 1 =>  --  range
                  Read_A_Range (Handler, Value.a_range);
               when 2 =>  --  expression
                  Value.expression.Clear;
                  Value.expression.Append (Handler.String_Value);
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_InlineValueEvaluatableExpression;

   procedure Read_Pattern
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Pattern) is
   begin
      Value.Clear;
      Value.Append (Handler.String_Value);
      Handler.Read_Next;
   end Read_Pattern;

   procedure Read_DiagnosticTag_Set
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DiagnosticTag_Set) is
   begin
      pragma Assert (Handler.Is_Start_Array);
      Handler.Read_Next;

      declare
         Set   : LSP.Structures.DiagnosticTag_Set renames Value;
         Value : LSP.Enumerations.DiagnosticTag;
      begin
         Set := (others => False);
         while not Handler.Is_End_Array loop
            Read_DiagnosticTag (Handler, Value);
            Set (Value) := True;
         end loop;
      end;

      Handler.Read_Next;
   end Read_DiagnosticTag_Set;

   package NotebookCellArrayChange_Scope is
      package NotebookCellArrayChange_Map is new Minimal_Perfect_Hash
        (["start",
         "deleteCount",
         "cells"]);

   end NotebookCellArrayChange_Scope;

   procedure Read_NotebookCellArrayChange
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.NotebookCellArrayChange) is
      use NotebookCellArrayChange_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case NotebookCellArrayChange_Map.Get_Index (Key) is
               when 1 =>  --  start
                  Value.start := Integer (Handler.Number_Value.Integer_Value);
                  Handler.Read_Next;
               when 2 =>  --  deleteCount
                  Value.deleteCount :=
                    Integer (Handler.Number_Value.Integer_Value);
                  Handler.Read_Next;
               when 3 =>  --  cells
                  Read_NotebookCell_Vector (Handler, Value.cells);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_NotebookCellArrayChange;

   package DocumentLinkRegistrationOptions_Scope is
      package DocumentLinkRegistrationOptions_Map is new Minimal_Perfect_Hash
        (["documentSelector",
         "workDoneProgress",
         "resolveProvider"]);

   end DocumentLinkRegistrationOptions_Scope;

   procedure Read_DocumentLinkRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentLinkRegistrationOptions) is
      use DocumentLinkRegistrationOptions_Scope;
      procedure Read_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.DocumentSelector_Or_Null);

      procedure Read_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.DocumentSelector_Or_Null) is
      begin
         if Handler.Is_Null_Value then
            Handler.Read_Next;
         else
            Value :=
              (Is_Null => False,
               Value   => <>);
            Read_DocumentSelector (Handler, Value.Value);
         end if;
      end Read_DocumentSelector_Or_Null;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case DocumentLinkRegistrationOptions_Map.Get_Index (Key) is
               when 1 =>  --  documentSelector
                  Read_DocumentSelector_Or_Null
                    (Handler, Value.Parent.documentSelector);
               when 2 =>  --  workDoneProgress
                  Value.workDoneProgress       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.workDoneProgress.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 3 =>  --  resolveProvider
                  Value.resolveProvider       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.resolveProvider.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_DocumentLinkRegistrationOptions;

   procedure Read_DocumentHighlight_Vector_Or_Null
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentHighlight_Vector_Or_Null) is
   begin
      if Handler.Is_Null_Value then
         Handler.Read_Next;
      else
         Value.Clear;
         Read_DocumentHighlight_Vector (Handler, Value);
      end if;
   end Read_DocumentHighlight_Vector_Or_Null;

   package LogMessageParams_Scope is
      package LogMessageParams_Map is new Minimal_Perfect_Hash
        (["type",
         "message"]);

   end LogMessageParams_Scope;

   procedure Read_LogMessageParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.LogMessageParams) is
      use LogMessageParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case LogMessageParams_Map.Get_Index (Key) is
               when 1 =>  --  type
                  Read_MessageType (Handler, Value.a_type);
               when 2 =>  --  message
                  Value.message.Clear;
                  Value.message.Append (Handler.String_Value);
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_LogMessageParams;

   package CodeLens_Scope is
      package CodeLens_Map is new Minimal_Perfect_Hash
        (["range",
         "command",
         "data"]);

   end CodeLens_Scope;

   procedure Read_CodeLens
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CodeLens) is
      use CodeLens_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case CodeLens_Map.Get_Index (Key) is
               when 1 =>  --  range
                  Read_A_Range (Handler, Value.a_range);
               when 2 =>  --  command
                  Value.command :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_Command (Handler, Value.command.Value);
               when 3 =>  --  data
                  Read_LSPAny (Handler, Value.data);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_CodeLens;

   package DocumentDiagnosticReportPartialResult_Scope is
      package DocumentDiagnosticReportPartialResult_Map is new Minimal_Perfect_Hash
        (["relatedDocuments"]);

   end DocumentDiagnosticReportPartialResult_Scope;

   procedure Read_DocumentDiagnosticReportPartialResult
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentDiagnosticReportPartialResult) is
      use DocumentDiagnosticReportPartialResult_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case DocumentDiagnosticReportPartialResult_Map.Get_Index (Key) is
               when 1 =>  --  relatedDocuments
                  pragma Assert (Handler.Is_Start_Object);
                  Handler.Read_Next;

                  while not Handler.Is_End_Object loop
                     declare
                        Map   :
                          LSP.Structures
                            .relatedDocuments_OfDocumentDiagnosticReportPartialResult renames
                          Value.relatedDocuments;
                        Key   : LSP.Structures.DocumentUri;
                        Value :
                          LSP.Structures
                            .relatedDocuments_OfDocumentDiagnosticReportPartialResult_Item;
                     begin
                        Key := (Handler.Key_Name with null record);
                        Handler.Read_Next;
                        Read_relatedDocuments_OfDocumentDiagnosticReportPartialResult_Item
                          (Handler, Value);
                        Map.Insert (Key, Value);
                     end;
                  end loop;

                  Handler.Read_Next;

               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_DocumentDiagnosticReportPartialResult;

   procedure Read_CallHierarchyIncomingCall_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CallHierarchyIncomingCall_Vector) is
   begin
      pragma Assert (Handler.Is_Start_Array);
      Handler.Read_Next;

      declare
         Set   : LSP.Structures.CallHierarchyIncomingCall_Vector renames Value;
         Value : LSP.Structures.CallHierarchyIncomingCall;
      begin
         Set.Clear;
         while not Handler.Is_End_Array loop
            Read_CallHierarchyIncomingCall (Handler, Value);
            Set.Append (Value);
         end loop;
      end;

      Handler.Read_Next;
   end Read_CallHierarchyIncomingCall_Vector;

   package ProgressParams_Scope is
      package ProgressParams_Map is new Minimal_Perfect_Hash
        (["token",
         "value"]);

   end ProgressParams_Scope;

   procedure Read_ProgressParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ProgressParams) is
      use ProgressParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case ProgressParams_Map.Get_Index (Key) is
               when 1 =>  --  token
                  Read_ProgressToken (Handler, Value.token);
               when 2 =>  --  value
                  Read_LSPAny (Handler, Value.value);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_ProgressParams;

   package DocumentLinkOptions_Scope is
      package DocumentLinkOptions_Map is new Minimal_Perfect_Hash
        (["workDoneProgress",
         "resolveProvider"]);

   end DocumentLinkOptions_Scope;

   procedure Read_DocumentLinkOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentLinkOptions) is
      use DocumentLinkOptions_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case DocumentLinkOptions_Map.Get_Index (Key) is
               when 1 =>  --  workDoneProgress
                  Value.workDoneProgress       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.workDoneProgress.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 2 =>  --  resolveProvider
                  Value.resolveProvider       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.resolveProvider.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_DocumentLinkOptions;

   procedure Read_PrepareSupportDefaultBehavior
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.PrepareSupportDefaultBehavior) is
   begin
      Value :=
        LSP.Enumerations.PrepareSupportDefaultBehavior'Val
          (Handler.Number_Value.Integer_Value - 1);
      Handler.Read_Next;
   end Read_PrepareSupportDefaultBehavior;

   package TextDocumentIdentifier_Scope is
      package TextDocumentIdentifier_Map is new Minimal_Perfect_Hash (["uri"]);

   end TextDocumentIdentifier_Scope;

   procedure Read_TextDocumentIdentifier
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TextDocumentIdentifier) is
      use TextDocumentIdentifier_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case TextDocumentIdentifier_Map.Get_Index (Key) is
               when 1 =>  --  uri
                  Value.uri := (Handler.String_Value with null record);
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_TextDocumentIdentifier;

   package DocumentSymbol_Result_Scope is
      package DocumentSymbol_Result_Map is new Minimal_Perfect_Hash
        (["location",
         "containerName",
         "detail",
         "range",
         "selectionRange",
         "children"]);

   end DocumentSymbol_Result_Scope;

   procedure Read_DocumentSymbol_Result
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentSymbol_Result) is
      use DocumentSymbol_Result_Scope;
   begin
      declare
         Parent  :
           constant not null access VSS.JSON.Pull_Readers.JSON_Pull_Reader'
             Class :=
           Handler'Access;
         Handler :
           VSS.JSON.Pull_Readers.Buffered.JSON_Buffered_Pull_Reader (Parent);
      begin
         Handler.Mark;
         if Handler.Is_Start_Array then
            Handler.Read_Next;
         end if;
         if Handler.Is_Null_Value then
            Value :=
              (Kind   => LSP.Structures.Variant_3,
               others => <>);
         elsif Handler.Is_Start_Object then
            Handler.Read_Next;
            while Handler.Is_Key_Name loop
               declare
                  Key   : constant VSS.Strings.Virtual_String :=
                    Handler.Key_Name;
                  Index : constant Natural                    :=
                    DocumentSymbol_Result_Map.Get_Index (Key);
               begin
                  Handler.Read_Next;
                  case Index is
                     when 1 =>  --  location
                        Value :=
                          (Kind   => LSP.Structures.Variant_1,
                           others => <>);
                        exit;
                     when 2 =>  --  containerName
                        Value :=
                          (Kind   => LSP.Structures.Variant_1,
                           others => <>);
                        exit;
                     when 3 =>  --  detail
                        Value :=
                          (Kind   => LSP.Structures.Variant_2,
                           others => <>);
                        exit;
                     when 4 =>  --  range
                        Value :=
                          (Kind   => LSP.Structures.Variant_2,
                           others => <>);
                        exit;
                     when 5 =>  --  selectionRange
                        Value :=
                          (Kind   => LSP.Structures.Variant_2,
                           others => <>);
                        exit;
                     when 6 =>  --  children
                        Value :=
                          (Kind   => LSP.Structures.Variant_2,
                           others => <>);
                        exit;
                     when others =>
                        Handler.Skip_Current_Value;
                  end case;
               end;
            end loop;
         else
            raise Program_Error;  --  Unexpected JSON value
         end if;

         Handler.Reset;
         Handler.Unmark;

         case Value.Kind is
            when LSP.Structures.Variant_1 =>
               Read_SymbolInformation_Vector (Handler, Value.Variant_1);
            when LSP.Structures.Variant_2 =>
               Read_DocumentSymbol_Vector (Handler, Value.Variant_2);
            when LSP.Structures.Variant_3 =>
               null;  --  #null_value
               Handler.Read_Next;
         end case;
      end;
   end Read_DocumentSymbol_Result;

   package TraceValues_Map is new Minimal_Perfect_Hash
     (["off",
      "messages",
      "verbose"]);

   procedure Read_TraceValues
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.TraceValues) is
   begin
      Value :=
        LSP.Enumerations.TraceValues'Val
          (TraceValues_Map.Get_Index (Handler.String_Value) - 1);
      Handler.Read_Next;
   end Read_TraceValues;

   package TypeHierarchyPrepareParams_Scope is
      package TypeHierarchyPrepareParams_Map is new Minimal_Perfect_Hash
        (["textDocument",
         "position",
         "workDoneToken"]);

   end TypeHierarchyPrepareParams_Scope;

   procedure Read_TypeHierarchyPrepareParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TypeHierarchyPrepareParams) is
      use TypeHierarchyPrepareParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case TypeHierarchyPrepareParams_Map.Get_Index (Key) is
               when 1 =>  --  textDocument
                  Read_TextDocumentIdentifier (Handler, Value.textDocument);
               when 2 =>  --  position
                  Read_Position (Handler, Value.position);
               when 3 =>  --  workDoneToken
                  Value.workDoneToken :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_ProgressToken (Handler, Value.workDoneToken.Value);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_TypeHierarchyPrepareParams;

   package SymbolInformation_Scope is
      package SymbolInformation_Map is new Minimal_Perfect_Hash
        (["name",
         "kind",
         "tags",
         "containerName",
         "deprecated",
         "location"]);

   end SymbolInformation_Scope;

   procedure Read_SymbolInformation
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SymbolInformation) is
      use SymbolInformation_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case SymbolInformation_Map.Get_Index (Key) is
               when 1 =>  --  name
                  Value.name.Clear;
                  Value.name.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 2 =>  --  kind
                  Read_SymbolKind (Handler, Value.kind);
               when 3 =>  --  tags
                  Read_SymbolTag_Set (Handler, Value.tags);
               when 4 =>  --  containerName
                  Value.containerName.Clear;
                  Value.containerName.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 5 =>  --  deprecated
                  Value.deprecated       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.deprecated.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 6 =>  --  location
                  Read_Location (Handler, Value.location);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_SymbolInformation;

   package WillSaveTextDocumentParams_Scope is
      package WillSaveTextDocumentParams_Map is new Minimal_Perfect_Hash
        (["textDocument",
         "reason"]);

   end WillSaveTextDocumentParams_Scope;

   procedure Read_WillSaveTextDocumentParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.WillSaveTextDocumentParams) is
      use WillSaveTextDocumentParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case WillSaveTextDocumentParams_Map.Get_Index (Key) is
               when 1 =>  --  textDocument
                  Read_TextDocumentIdentifier (Handler, Value.textDocument);
               when 2 =>  --  reason
                  Read_TextDocumentSaveReason (Handler, Value.reason);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_WillSaveTextDocumentParams;

   procedure Read_Command_Or_CodeAction_Vector_Or_Null
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Command_Or_CodeAction_Vector_Or_Null) is
   begin
      if Handler.Is_Null_Value then
         Handler.Read_Next;
      else
         Value.Clear;
         Read_Command_Or_CodeAction_Vector (Handler, Value);
      end if;
   end Read_Command_Or_CodeAction_Vector_Or_Null;

   package FileOperationClientCapabilities_Scope is
      package FileOperationClientCapabilities_Map is new Minimal_Perfect_Hash
        (["dynamicRegistration",
         "didCreate",
         "willCreate",
         "didRename",
         "willRename",
         "didDelete",
         "willDelete"]);

   end FileOperationClientCapabilities_Scope;

   procedure Read_FileOperationClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.FileOperationClientCapabilities) is
      use FileOperationClientCapabilities_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case FileOperationClientCapabilities_Map.Get_Index (Key) is
               when 1 =>  --  dynamicRegistration
                  Value.dynamicRegistration       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.dynamicRegistration.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 2 =>  --  didCreate
                  Value.didCreate       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.didCreate.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 3 =>  --  willCreate
                  Value.willCreate       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.willCreate.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 4 =>  --  didRename
                  Value.didRename       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.didRename.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 5 =>  --  willRename
                  Value.willRename       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.willRename.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 6 =>  --  didDelete
                  Value.didDelete       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.didDelete.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 7 =>  --  willDelete
                  Value.willDelete       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.willDelete.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_FileOperationClientCapabilities;

   package Definition_Result_Scope is
      package Definition_Result_Map is new Minimal_Perfect_Hash
        (["uri",
         "range"]);

   end Definition_Result_Scope;

   procedure Read_Definition_Result
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Definition_Result) is
      use Definition_Result_Scope;
   begin
      declare
         Parent  :
           constant not null access VSS.JSON.Pull_Readers.JSON_Pull_Reader'
             Class :=
           Handler'Access;
         Handler :
           VSS.JSON.Pull_Readers.Buffered.JSON_Buffered_Pull_Reader (Parent);
      begin
         Handler.Mark;
         if Handler.Is_Null_Value then
            Value :=
              (Kind   => LSP.Structures.Variant_3,
               others => <>);
         elsif Handler.Is_Start_Object then
            Handler.Read_Next;
            Value :=
              (Kind   => LSP.Structures.Variant_2,
               others => <>);
            while Handler.Is_Key_Name loop
               declare
                  Key   : constant VSS.Strings.Virtual_String :=
                    Handler.Key_Name;
                  Index : constant Natural                    :=
                    Definition_Result_Map.Get_Index (Key);
               begin
                  Handler.Read_Next;
                  case Index is
                     when 1 =>  --  uri
                        Value :=
                          (Kind   => LSP.Structures.Variant_1,
                           others => <>);
                        exit;
                     when 2 =>  --  range
                        Value :=
                          (Kind   => LSP.Structures.Variant_1,
                           others => <>);
                        exit;
                     when others =>
                        Handler.Skip_Current_Value;
                  end case;
               end;
            end loop;
         else
            raise Program_Error;  --  Unexpected JSON value
         end if;

         Handler.Reset;
         Handler.Unmark;

         case Value.Kind is
            when LSP.Structures.Variant_1 =>
               Read_Definition (Handler, Value.Variant_1);
            when LSP.Structures.Variant_2 =>
               Read_DefinitionLink_Vector (Handler, Value.Variant_2);
            when LSP.Structures.Variant_3 =>
               null;  --  #null_value
               Handler.Read_Next;
         end case;
      end;
   end Read_Definition_Result;

   package DocumentHighlightClientCapabilities_Scope is
      package DocumentHighlightClientCapabilities_Map is new Minimal_Perfect_Hash
        (["dynamicRegistration"]);

   end DocumentHighlightClientCapabilities_Scope;

   procedure Read_DocumentHighlightClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentHighlightClientCapabilities) is
      use DocumentHighlightClientCapabilities_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case DocumentHighlightClientCapabilities_Map.Get_Index (Key) is
               when 1 =>  --  dynamicRegistration
                  Value.dynamicRegistration       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.dynamicRegistration.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_DocumentHighlightClientCapabilities;

   package InlineValueText_Scope is
      package InlineValueText_Map is new Minimal_Perfect_Hash
        (["range",
         "text"]);

   end InlineValueText_Scope;

   procedure Read_InlineValueText
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.InlineValueText) is
      use InlineValueText_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case InlineValueText_Map.Get_Index (Key) is
               when 1 =>  --  range
                  Read_A_Range (Handler, Value.a_range);
               when 2 =>  --  text
                  Value.text.Clear;
                  Value.text.Append (Handler.String_Value);
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_InlineValueText;

   package UnchangedDocumentDiagnosticReport_Scope is
      package UnchangedDocumentDiagnosticReport_Map is new Minimal_Perfect_Hash
        (["kind",
         "resultId"]);

   end UnchangedDocumentDiagnosticReport_Scope;

   procedure Read_UnchangedDocumentDiagnosticReport
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.UnchangedDocumentDiagnosticReport) is
      use UnchangedDocumentDiagnosticReport_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case UnchangedDocumentDiagnosticReport_Map.Get_Index (Key) is
               when 1 =>  --  kind
                  Handler.Read_Next;  --  Skip string literal: unchanged
               when 2 =>  --  resultId
                  Value.resultId.Clear;
                  Value.resultId.Append (Handler.String_Value);
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_UnchangedDocumentDiagnosticReport;

   procedure Read_SymbolTag
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.SymbolTag) is
   begin
      Value :=
        LSP.Enumerations.SymbolTag'Val
          (Handler.Number_Value.Integer_Value - 1);
      Handler.Read_Next;
   end Read_SymbolTag;

   package Registration_Scope is
      package Registration_Map is new Minimal_Perfect_Hash
        (["id",
         "method",
         "registerOptions"]);

   end Registration_Scope;

   procedure Read_Registration
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Registration) is
      use Registration_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case Registration_Map.Get_Index (Key) is
               when 1 =>  --  id
                  Value.id.Clear;
                  Value.id.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 2 =>  --  method
                  Value.method.Clear;
                  Value.method.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 3 =>  --  registerOptions
                  Read_LSPAny (Handler, Value.registerOptions);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_Registration;

   package CompletionItem_Scope is
      package CompletionItem_Map is new Minimal_Perfect_Hash
        (["label",
         "labelDetails",
         "kind",
         "tags",
         "detail",
         "documentation",
         "deprecated",
         "preselect",
         "sortText",
         "filterText",
         "insertText",
         "insertTextFormat",
         "insertTextMode",
         "textEdit",
         "textEditText",
         "additionalTextEdits",
         "commitCharacters",
         "command",
         "data"]);

      package TextEdit_Or_InsertReplaceEdit_Scope is
         package TextEdit_Or_InsertReplaceEdit_Map is new Minimal_Perfect_Hash
           (["range",
            "insert",
            "replace"]);

      end TextEdit_Or_InsertReplaceEdit_Scope;

   end CompletionItem_Scope;

   procedure Read_CompletionItem
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CompletionItem) is
      use CompletionItem_Scope;
      procedure Read_TextEdit_Or_InsertReplaceEdit
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.TextEdit_Or_InsertReplaceEdit);

      procedure Read_TextEdit_Or_InsertReplaceEdit
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.TextEdit_Or_InsertReplaceEdit) is
         use TextEdit_Or_InsertReplaceEdit_Scope;
      begin
         declare
            Parent  :
              constant not null access VSS.JSON.Pull_Readers.JSON_Pull_Reader'
                Class :=
              Handler'Access;
            Handler :
              VSS.JSON.Pull_Readers.Buffered.JSON_Buffered_Pull_Reader
                (Parent);
         begin
            Handler.Mark;
            if Handler.Is_Start_Object then
               Handler.Read_Next;
               while Handler.Is_Key_Name loop
                  declare
                     Key   : constant VSS.Strings.Virtual_String :=
                       Handler.Key_Name;
                     Index : constant Natural                    :=
                       TextEdit_Or_InsertReplaceEdit_Map.Get_Index (Key);
                  begin
                     Handler.Read_Next;
                     case Index is
                        when 1 =>  --  range
                           Value :=
                             (Is_TextEdit => True,
                              others      => <>);
                           exit;
                        when 2 =>  --  insert
                           Value :=
                             (Is_TextEdit => False,
                              others      => <>);
                           exit;
                        when 3 =>  --  replace
                           Value :=
                             (Is_TextEdit => False,
                              others      => <>);
                           exit;
                        when others =>
                           Handler.Skip_Current_Value;
                     end case;
                  end;
               end loop;
            else
               raise Program_Error;  --  Unexpected JSON value
            end if;

            Handler.Reset;
            Handler.Unmark;

            case Value.Is_TextEdit is
               when True =>
                  Read_TextEdit (Handler, Value.TextEdit);
               when False =>
                  Read_InsertReplaceEdit (Handler, Value.InsertReplaceEdit);
            end case;
         end;
      end Read_TextEdit_Or_InsertReplaceEdit;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case CompletionItem_Map.Get_Index (Key) is
               when 1 =>  --  label
                  Value.label.Clear;
                  Value.label.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 2 =>  --  labelDetails
                  Value.labelDetails :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_CompletionItemLabelDetails
                    (Handler, Value.labelDetails.Value);
               when 3 =>  --  kind
                  Value.kind :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_CompletionItemKind (Handler, Value.kind.Value);
               when 4 =>  --  tags
                  Read_CompletionItemTag_Set (Handler, Value.tags);
               when 5 =>  --  detail
                  Value.detail.Clear;
                  Value.detail.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 6 =>  --  documentation
                  Value.documentation :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_Virtual_String_Or_MarkupContent
                    (Handler, Value.documentation.Value);
               when 7 =>  --  deprecated
                  Value.deprecated       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.deprecated.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 8 =>  --  preselect
                  Value.preselect       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.preselect.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 9 =>  --  sortText
                  Value.sortText.Clear;
                  Value.sortText.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 10 =>  --  filterText
                  Value.filterText.Clear;
                  Value.filterText.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 11 =>  --  insertText
                  Value.insertText.Clear;
                  Value.insertText.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 12 =>  --  insertTextFormat
                  Value.insertTextFormat :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_InsertTextFormat
                    (Handler, Value.insertTextFormat.Value);
               when 13 =>  --  insertTextMode
                  Value.insertTextMode :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_InsertTextMode (Handler, Value.insertTextMode.Value);
               when 14 =>  --  textEdit
                  Value.textEdit :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_TextEdit_Or_InsertReplaceEdit
                    (Handler, Value.textEdit.Value);
               when 15 =>  --  textEditText
                  Value.textEditText.Clear;
                  Value.textEditText.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 16 =>  --  additionalTextEdits
                  Read_TextEdit_Vector (Handler, Value.additionalTextEdits);
               when 17 =>  --  commitCharacters
                  Read_Virtual_String_Vector (Handler, Value.commitCharacters);
               when 18 =>  --  command
                  Value.command :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_Command (Handler, Value.command.Value);
               when 19 =>  --  data
                  Read_LSPAny (Handler, Value.data);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_CompletionItem;

   package DocumentRangeFormattingRegistrationOptions_Scope is
      package DocumentRangeFormattingRegistrationOptions_Map is new Minimal_Perfect_Hash
        (["documentSelector",
         "workDoneProgress"]);

   end DocumentRangeFormattingRegistrationOptions_Scope;

   procedure Read_DocumentRangeFormattingRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value : out LSP.Structures.DocumentRangeFormattingRegistrationOptions) is
      use DocumentRangeFormattingRegistrationOptions_Scope;
      procedure Read_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.DocumentSelector_Or_Null);

      procedure Read_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.DocumentSelector_Or_Null) is
      begin
         if Handler.Is_Null_Value then
            Handler.Read_Next;
         else
            Value :=
              (Is_Null => False,
               Value   => <>);
            Read_DocumentSelector (Handler, Value.Value);
         end if;
      end Read_DocumentSelector_Or_Null;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case DocumentRangeFormattingRegistrationOptions_Map.Get_Index
              (Key) is
               when 1 =>  --  documentSelector
                  Read_DocumentSelector_Or_Null
                    (Handler, Value.Parent.documentSelector);
               when 2 =>  --  workDoneProgress
                  Value.workDoneProgress       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.workDoneProgress.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_DocumentRangeFormattingRegistrationOptions;

   procedure Read_Moniker_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Moniker_Vector) is
   begin
      pragma Assert (Handler.Is_Start_Array);
      Handler.Read_Next;

      declare
         Set   : LSP.Structures.Moniker_Vector renames Value;
         Value : LSP.Structures.Moniker;
      begin
         Set.Clear;
         while not Handler.Is_End_Array loop
            Read_Moniker (Handler, Value);
            Set.Append (Value);
         end loop;
      end;

      Handler.Read_Next;
   end Read_Moniker_Vector;

   package RenameFileOptions_Scope is
      package RenameFileOptions_Map is new Minimal_Perfect_Hash
        (["overwrite",
         "ignoreIfExists"]);

   end RenameFileOptions_Scope;

   procedure Read_RenameFileOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.RenameFileOptions) is
      use RenameFileOptions_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case RenameFileOptions_Map.Get_Index (Key) is
               when 1 =>  --  overwrite
                  Value.overwrite       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.overwrite.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 2 =>  --  ignoreIfExists
                  Value.ignoreIfExists       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.ignoreIfExists.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_RenameFileOptions;

   package HoverParams_Scope is
      package HoverParams_Map is new Minimal_Perfect_Hash
        (["textDocument",
         "position",
         "workDoneToken"]);

   end HoverParams_Scope;

   procedure Read_HoverParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.HoverParams) is
      use HoverParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case HoverParams_Map.Get_Index (Key) is
               when 1 =>  --  textDocument
                  Read_TextDocumentIdentifier (Handler, Value.textDocument);
               when 2 =>  --  position
                  Read_Position (Handler, Value.position);
               when 3 =>  --  workDoneToken
                  Value.workDoneToken :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_ProgressToken (Handler, Value.workDoneToken.Value);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_HoverParams;

   package DocumentSymbol_Scope is
      package DocumentSymbol_Map is new Minimal_Perfect_Hash
        (["name",
         "detail",
         "kind",
         "tags",
         "deprecated",
         "range",
         "selectionRange",
         "children"]);

   end DocumentSymbol_Scope;

   procedure Read_DocumentSymbol
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentSymbol) is
      use DocumentSymbol_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case DocumentSymbol_Map.Get_Index (Key) is
               when 1 =>  --  name
                  Value.name.Clear;
                  Value.name.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 2 =>  --  detail
                  Value.detail.Clear;
                  Value.detail.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 3 =>  --  kind
                  Read_SymbolKind (Handler, Value.kind);
               when 4 =>  --  tags
                  Read_SymbolTag_Set (Handler, Value.tags);
               when 5 =>  --  deprecated
                  Value.deprecated       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.deprecated.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 6 =>  --  range
                  Read_A_Range (Handler, Value.a_range);
               when 7 =>  --  selectionRange
                  Read_A_Range (Handler, Value.selectionRange);
               when 8 =>  --  children
                  Read_DocumentSymbol_Vector (Handler, Value.children);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_DocumentSymbol;

   package DocumentSymbolClientCapabilities_Scope is
      package DocumentSymbolClientCapabilities_Map is new Minimal_Perfect_Hash
        (["dynamicRegistration",
         "symbolKind",
         "hierarchicalDocumentSymbolSupport",
         "tagSupport",
         "labelSupport"]);

   end DocumentSymbolClientCapabilities_Scope;

   procedure Read_DocumentSymbolClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentSymbolClientCapabilities) is
      use DocumentSymbolClientCapabilities_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case DocumentSymbolClientCapabilities_Map.Get_Index (Key) is
               when 1 =>  --  dynamicRegistration
                  Value.dynamicRegistration       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.dynamicRegistration.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 2 =>  --  symbolKind
                  Value.symbolKind :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_symbolKind_OfWorkspaceSymbolClientCapabilities
                    (Handler, Value.symbolKind.Value);
               when 3 =>  --  hierarchicalDocumentSymbolSupport
                  Value.hierarchicalDocumentSymbolSupport       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.hierarchicalDocumentSymbolSupport.Value :=
                    Handler.Boolean_Value;
                  Handler.Read_Next;
               when 4 =>  --  tagSupport
                  Value.tagSupport :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_tagSupport_OfWorkspaceSymbolClientCapabilities
                    (Handler, Value.tagSupport.Value);
               when 5 =>  --  labelSupport
                  Value.labelSupport       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.labelSupport.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_DocumentSymbolClientCapabilities;

   package InitializeError_Scope is
      package InitializeError_Map is new Minimal_Perfect_Hash (["retry"]);

   end InitializeError_Scope;

   procedure Read_InitializeError
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.InitializeError) is
      use InitializeError_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case InitializeError_Map.Get_Index (Key) is
               when 1 =>  --  retry
                  Value.retry := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_InitializeError;

   package LinkedEditingRangeRegistrationOptions_Scope is
      package LinkedEditingRangeRegistrationOptions_Map is new Minimal_Perfect_Hash
        (["documentSelector",
         "workDoneProgress",
         "id"]);

   end LinkedEditingRangeRegistrationOptions_Scope;

   procedure Read_LinkedEditingRangeRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.LinkedEditingRangeRegistrationOptions) is
      use LinkedEditingRangeRegistrationOptions_Scope;
      procedure Read_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.DocumentSelector_Or_Null);

      procedure Read_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.DocumentSelector_Or_Null) is
      begin
         if Handler.Is_Null_Value then
            Handler.Read_Next;
         else
            Value :=
              (Is_Null => False,
               Value   => <>);
            Read_DocumentSelector (Handler, Value.Value);
         end if;
      end Read_DocumentSelector_Or_Null;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case LinkedEditingRangeRegistrationOptions_Map.Get_Index (Key) is
               when 1 =>  --  documentSelector
                  Read_DocumentSelector_Or_Null
                    (Handler, Value.Parent.documentSelector);
               when 2 =>  --  workDoneProgress
                  Value.workDoneProgress       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.workDoneProgress.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 3 =>  --  id
                  Value.id.Clear;
                  Value.id.Append (Handler.String_Value);
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_LinkedEditingRangeRegistrationOptions;

   package SemanticTokensClientCapabilities_Scope is
      package SemanticTokensClientCapabilities_Map is new Minimal_Perfect_Hash
        (["dynamicRegistration",
         "requests",
         "tokenTypes",
         "tokenModifiers",
         "formats",
         "overlappingTokenSupport",
         "multilineTokenSupport",
         "serverCancelSupport",
         "augmentsSyntaxTokens"]);

      package requests_OfSemanticTokensClientCapabilities_Scope is
         package requests_OfSemanticTokensClientCapabilities_Map is new Minimal_Perfect_Hash
           (["range",
            "full"]);

      end requests_OfSemanticTokensClientCapabilities_Scope;

   end SemanticTokensClientCapabilities_Scope;

   procedure Read_SemanticTokensClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SemanticTokensClientCapabilities) is
      use SemanticTokensClientCapabilities_Scope;
      procedure Read_requests_OfSemanticTokensClientCapabilities
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .requests_OfSemanticTokensClientCapabilities);

      procedure Read_TokenFormat_Set
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.TokenFormat_Set);

      procedure Read_requests_OfSemanticTokensClientCapabilities
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .requests_OfSemanticTokensClientCapabilities) is
      begin
         pragma Assert (Handler.Is_Start_Object);
         Handler.Read_Next;

         while Handler.Is_Key_Name loop
            declare
               use requests_OfSemanticTokensClientCapabilities_Scope;
               Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
            begin
               Handler.Read_Next;
               case requests_OfSemanticTokensClientCapabilities_Map.Get_Index
                 (Key) is
                  when 1 =>  --  range
                     Value.a_range :=
                       (Is_Set => True,
                        Value  => <>);
                     Read_Boolean_Or_Any (Handler, Value.a_range.Value);
                  when 2 =>  --  full
                     Value.full :=
                       (Is_Set => True,
                        Value  => <>);
                     Read_Boolean_Or_Something (Handler, Value.full.Value);
                  when others =>
                     Handler.Skip_Current_Value;
               end case;
            end;
         end loop;

         Handler.Read_Next;
      end Read_requests_OfSemanticTokensClientCapabilities;

      procedure Read_TokenFormat_Set
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.TokenFormat_Set) is
      begin
         pragma Assert (Handler.Is_Start_Array);
         Handler.Read_Next;

         declare
            Set   : LSP.Structures.TokenFormat_Set renames Value;
            Value : LSP.Enumerations.TokenFormat;
         begin
            Set := (others => False);
            while not Handler.Is_End_Array loop
               Read_TokenFormat (Handler, Value);
               Set (Value) := True;
            end loop;
         end;

         Handler.Read_Next;
      end Read_TokenFormat_Set;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case SemanticTokensClientCapabilities_Map.Get_Index (Key) is
               when 1 =>  --  dynamicRegistration
                  Value.dynamicRegistration       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.dynamicRegistration.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 2 =>  --  requests
                  Read_requests_OfSemanticTokensClientCapabilities
                    (Handler, Value.requests);
               when 3 =>  --  tokenTypes
                  Read_Virtual_String_Vector (Handler, Value.tokenTypes);
               when 4 =>  --  tokenModifiers
                  Read_Virtual_String_Vector (Handler, Value.tokenModifiers);
               when 5 =>  --  formats
                  Read_TokenFormat_Set (Handler, Value.formats);
               when 6 =>  --  overlappingTokenSupport
                  Value.overlappingTokenSupport       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.overlappingTokenSupport.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 7 =>  --  multilineTokenSupport
                  Value.multilineTokenSupport       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.multilineTokenSupport.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 8 =>  --  serverCancelSupport
                  Value.serverCancelSupport       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.serverCancelSupport.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 9 =>  --  augmentsSyntaxTokens
                  Value.augmentsSyntaxTokens       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.augmentsSyntaxTokens.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_SemanticTokensClientCapabilities;

   package Hover_Scope is
      package Hover_Map is new Minimal_Perfect_Hash
        (["contents",
         "range"]);

      package MarkupContent_Or_MarkedString_Vector_Scope is
         package MarkupContent_Or_MarkedString_Vector_Map is new Minimal_Perfect_Hash
           (["kind",
            "language"]);

      end MarkupContent_Or_MarkedString_Vector_Scope;

   end Hover_Scope;

   procedure Read_Hover
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Hover) is
      use Hover_Scope;
      procedure Read_MarkedString_Vector
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.MarkedString_Vector);

      procedure Read_MarkupContent_Or_MarkedString_Vector
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.MarkupContent_Or_MarkedString_Vector);

      procedure Read_MarkedString_Vector
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.MarkedString_Vector) is
      begin
         pragma Assert (Handler.Is_Start_Array);
         Handler.Read_Next;

         declare
            Set   : LSP.Structures.MarkedString_Vector renames Value;
            Value : LSP.Structures.MarkedString;
         begin
            Set.Clear;
            while not Handler.Is_End_Array loop
               Read_MarkedString (Handler, Value);
               Set.Append (Value);
            end loop;
         end;

         Handler.Read_Next;
      end Read_MarkedString_Vector;

      procedure Read_MarkupContent_Or_MarkedString_Vector
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.MarkupContent_Or_MarkedString_Vector) is
         --  use MarkupContent_Or_MarkedString_Vector_Scope;
      begin
         declare
            Parent  :
              constant not null access VSS.JSON.Pull_Readers.JSON_Pull_Reader'
                Class :=
              Handler'Access;
            Handler :
              VSS.JSON.Pull_Readers.Buffered.JSON_Buffered_Pull_Reader
                (Parent);
         begin
            Handler.Mark;
            LSP.Input_Tools.Look_For_MarkupContent_Or_MarkedString_Vector
              (Handler, Value);
            Handler.Reset;
            Handler.Unmark;
            if not Value.Is_MarkupContent and not Handler.Is_Start_Array then
               Read_MarkedString (Handler, Value.MarkedString_Vector (1));
               return;
            end if;

            case Value.Is_MarkupContent is
               when True =>
                  Read_MarkupContent (Handler, Value.MarkupContent);
               when False =>
                  Read_MarkedString_Vector
                    (Handler, Value.MarkedString_Vector);
            end case;
         end;
      end Read_MarkupContent_Or_MarkedString_Vector;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case Hover_Map.Get_Index (Key) is
               when 1 =>  --  contents
                  Read_MarkupContent_Or_MarkedString_Vector
                    (Handler, Value.contents);
               when 2 =>  --  range
                  Value.a_range :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_A_Range (Handler, Value.a_range.Value);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_Hover;

   package InlineValueWorkspaceClientCapabilities_Scope is
      package InlineValueWorkspaceClientCapabilities_Map is new Minimal_Perfect_Hash
        (["refreshSupport"]);

   end InlineValueWorkspaceClientCapabilities_Scope;

   procedure Read_InlineValueWorkspaceClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.InlineValueWorkspaceClientCapabilities) is
      use InlineValueWorkspaceClientCapabilities_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case InlineValueWorkspaceClientCapabilities_Map.Get_Index (Key) is
               when 1 =>  --  refreshSupport
                  Value.refreshSupport       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.refreshSupport.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_InlineValueWorkspaceClientCapabilities;

   package ResourceOperation_Scope is
      package ResourceOperation_Map is new Minimal_Perfect_Hash
        (["kind",
         "annotationId"]);

   end ResourceOperation_Scope;

   procedure Read_ResourceOperation
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ResourceOperation) is
      use ResourceOperation_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case ResourceOperation_Map.Get_Index (Key) is
               when 1 =>  --  kind
                  Value.kind.Clear;
                  Value.kind.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 2 =>  --  annotationId
                  Value.annotationId :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_ChangeAnnotationIdentifier
                    (Handler, Value.annotationId.Value);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_ResourceOperation;

   package PublishDiagnosticsClientCapabilities_Scope is
      package PublishDiagnosticsClientCapabilities_Map is new Minimal_Perfect_Hash
        (["relatedInformation",
         "tagSupport",
         "versionSupport",
         "codeDescriptionSupport",
         "dataSupport"]);

      package tagSupport_OfPublishDiagnosticsClientCapabilities_Scope is
         package tagSupport_OfPublishDiagnosticsClientCapabilities_Map is new Minimal_Perfect_Hash
           (["valueSet"]);

      end tagSupport_OfPublishDiagnosticsClientCapabilities_Scope;

   end PublishDiagnosticsClientCapabilities_Scope;

   procedure Read_PublishDiagnosticsClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.PublishDiagnosticsClientCapabilities) is
      use PublishDiagnosticsClientCapabilities_Scope;
      procedure Read_tagSupport_OfPublishDiagnosticsClientCapabilities
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .tagSupport_OfPublishDiagnosticsClientCapabilities);

      procedure Read_tagSupport_OfPublishDiagnosticsClientCapabilities
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .tagSupport_OfPublishDiagnosticsClientCapabilities) is
      begin
         pragma Assert (Handler.Is_Start_Object);
         Handler.Read_Next;

         while Handler.Is_Key_Name loop
            declare
               use tagSupport_OfPublishDiagnosticsClientCapabilities_Scope;
               Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
            begin
               Handler.Read_Next;
               case tagSupport_OfPublishDiagnosticsClientCapabilities_Map
                 .Get_Index
                 (Key) is
                  when 1 =>  --  valueSet
                     Read_DiagnosticTag_Set (Handler, Value.valueSet);
                  when others =>
                     Handler.Skip_Current_Value;
               end case;
            end;
         end loop;

         Handler.Read_Next;
      end Read_tagSupport_OfPublishDiagnosticsClientCapabilities;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case PublishDiagnosticsClientCapabilities_Map.Get_Index (Key) is
               when 1 =>  --  relatedInformation
                  Value.relatedInformation       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.relatedInformation.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 2 =>  --  tagSupport
                  Value.tagSupport :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_tagSupport_OfPublishDiagnosticsClientCapabilities
                    (Handler, Value.tagSupport.Value);
               when 3 =>  --  versionSupport
                  Value.versionSupport       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.versionSupport.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 4 =>  --  codeDescriptionSupport
                  Value.codeDescriptionSupport       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.codeDescriptionSupport.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 5 =>  --  dataSupport
                  Value.dataSupport       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.dataSupport.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_PublishDiagnosticsClientCapabilities;

   procedure Read_DocumentHighlight_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentHighlight_Vector) is
   begin
      pragma Assert (Handler.Is_Start_Array);
      Handler.Read_Next;

      declare
         Set   : LSP.Structures.DocumentHighlight_Vector renames Value;
         Value : LSP.Structures.DocumentHighlight;
      begin
         Set.Clear;
         while not Handler.Is_End_Array loop
            Read_DocumentHighlight (Handler, Value);
            Set.Append (Value);
         end loop;
      end;

      Handler.Read_Next;
   end Read_DocumentHighlight_Vector;

   package SemanticTokensOptions_Scope is
      package SemanticTokensOptions_Map is new Minimal_Perfect_Hash
        (["workDoneProgress",
         "legend",
         "range",
         "full"]);

   end SemanticTokensOptions_Scope;

   procedure Read_SemanticTokensOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SemanticTokensOptions) is
      use SemanticTokensOptions_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case SemanticTokensOptions_Map.Get_Index (Key) is
               when 1 =>  --  workDoneProgress
                  Value.workDoneProgress       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.workDoneProgress.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 2 =>  --  legend
                  Read_SemanticTokensLegend (Handler, Value.legend);
               when 3 =>  --  range
                  Value.a_range :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_Boolean_Or_Any (Handler, Value.a_range.Value);
               when 4 =>  --  full
                  Value.full :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_Boolean_Or_Something (Handler, Value.full.Value);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_SemanticTokensOptions;

   package DocumentHighlightOptions_Scope is
      package DocumentHighlightOptions_Map is new Minimal_Perfect_Hash
        (["workDoneProgress"]);

   end DocumentHighlightOptions_Scope;

   procedure Read_DocumentHighlightOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.DocumentHighlightOptions) is
      use DocumentHighlightOptions_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case DocumentHighlightOptions_Map.Get_Index (Key) is
               when 1 =>  --  workDoneProgress
                  Value.workDoneProgress       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.workDoneProgress.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_DocumentHighlightOptions;

   procedure Read_Integer_Or_Virtual_String
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Integer_Or_Virtual_String) is
   begin
      if Handler.Is_Number_Value then
         Value :=
           (Is_Integer => True,
            others     => <>);
      else
         Value :=
           (Is_Integer => False,
            others     => <>);
      end if;

      case Value.Is_Integer is
         when True =>
            Value.Integer := Integer (Handler.Number_Value.Integer_Value);
            Handler.Read_Next;
         when False =>
            Value.Virtual_String.Clear;
            Value.Virtual_String.Append (Handler.String_Value);
            Handler.Read_Next;
      end case;
   end Read_Integer_Or_Virtual_String;

   package FoldingRangeParams_Scope is
      package FoldingRangeParams_Map is new Minimal_Perfect_Hash
        (["workDoneToken",
         "partialResultToken",
         "textDocument"]);

   end FoldingRangeParams_Scope;

   procedure Read_FoldingRangeParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.FoldingRangeParams) is
      use FoldingRangeParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case FoldingRangeParams_Map.Get_Index (Key) is
               when 1 =>  --  workDoneToken
                  Value.workDoneToken :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_ProgressToken (Handler, Value.workDoneToken.Value);
               when 2 =>  --  partialResultToken
                  Value.partialResultToken :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_ProgressToken (Handler, Value.partialResultToken.Value);
               when 3 =>  --  textDocument
                  Read_TextDocumentIdentifier (Handler, Value.textDocument);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_FoldingRangeParams;

   package FoldingRangeOptions_Scope is
      package FoldingRangeOptions_Map is new Minimal_Perfect_Hash
        (["workDoneProgress"]);

   end FoldingRangeOptions_Scope;

   procedure Read_FoldingRangeOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.FoldingRangeOptions) is
      use FoldingRangeOptions_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case FoldingRangeOptions_Map.Get_Index (Key) is
               when 1 =>  --  workDoneProgress
                  Value.workDoneProgress       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.workDoneProgress.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_FoldingRangeOptions;

   procedure Read_Integer_Or_Null
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Integer_Or_Null) is
   begin
      if Handler.Is_Null_Value then
         Handler.Read_Next;
      else
         Value       :=
           (Is_Null => False,
            Value   => <>);
         Value.Value := Integer (Handler.Number_Value.Integer_Value);
         Handler.Read_Next;
      end if;
   end Read_Integer_Or_Null;

   package RegistrationParams_Scope is
      package RegistrationParams_Map is new Minimal_Perfect_Hash
        (["registrations"]);

   end RegistrationParams_Scope;

   procedure Read_RegistrationParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.RegistrationParams) is
      use RegistrationParams_Scope;
      procedure Read_Registration_Vector
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.Registration_Vector);

      procedure Read_Registration_Vector
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.Registration_Vector) is
      begin
         pragma Assert (Handler.Is_Start_Array);
         Handler.Read_Next;

         declare
            Set   : LSP.Structures.Registration_Vector renames Value;
            Value : LSP.Structures.Registration;
         begin
            Set.Clear;
            while not Handler.Is_End_Array loop
               Read_Registration (Handler, Value);
               Set.Append (Value);
            end loop;
         end;

         Handler.Read_Next;
      end Read_Registration_Vector;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case RegistrationParams_Map.Get_Index (Key) is
               when 1 =>  --  registrations
                  Read_Registration_Vector (Handler, Value.registrations);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_RegistrationParams;

   package SignatureHelpParams_Scope is
      package SignatureHelpParams_Map is new Minimal_Perfect_Hash
        (["textDocument",
         "position",
         "workDoneToken",
         "context"]);

   end SignatureHelpParams_Scope;

   procedure Read_SignatureHelpParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SignatureHelpParams) is
      use SignatureHelpParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case SignatureHelpParams_Map.Get_Index (Key) is
               when 1 =>  --  textDocument
                  Read_TextDocumentIdentifier (Handler, Value.textDocument);
               when 2 =>  --  position
                  Read_Position (Handler, Value.position);
               when 3 =>  --  workDoneToken
                  Value.workDoneToken :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_ProgressToken (Handler, Value.workDoneToken.Value);
               when 4 =>  --  context
                  Value.context :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_SignatureHelpContext (Handler, Value.context.Value);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_SignatureHelpParams;

   procedure Read_TextEdit_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TextEdit_Vector) is
   begin
      pragma Assert (Handler.Is_Start_Array);
      Handler.Read_Next;

      declare
         Set   : LSP.Structures.TextEdit_Vector renames Value;
         Value : LSP.Structures.TextEdit;
      begin
         Set.Clear;
         while not Handler.Is_End_Array loop
            Read_TextEdit (Handler, Value);
            Set.Append (Value);
         end loop;
      end;

      Handler.Read_Next;
   end Read_TextEdit_Vector;

   package ShowMessageRequestClientCapabilities_Scope is
      package ShowMessageRequestClientCapabilities_Map is new Minimal_Perfect_Hash
        (["messageActionItem"]);

      package messageActionItem_OfShowMessageRequestClientCapabilities_Scope is
         package messageActionItem_OfShowMessageRequestClientCapabilities_Map is new Minimal_Perfect_Hash
           (["additionalPropertiesSupport"]);

      end messageActionItem_OfShowMessageRequestClientCapabilities_Scope;

   end ShowMessageRequestClientCapabilities_Scope;

   procedure Read_ShowMessageRequestClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ShowMessageRequestClientCapabilities) is
      use ShowMessageRequestClientCapabilities_Scope;
      procedure Read_messageActionItem_OfShowMessageRequestClientCapabilities
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .messageActionItem_OfShowMessageRequestClientCapabilities);

      procedure Read_messageActionItem_OfShowMessageRequestClientCapabilities
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures
           .messageActionItem_OfShowMessageRequestClientCapabilities) is
      begin
         pragma Assert (Handler.Is_Start_Object);
         Handler.Read_Next;

         while Handler.Is_Key_Name loop
            declare
               use
                 messageActionItem_OfShowMessageRequestClientCapabilities_Scope;
               Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
            begin
               Handler.Read_Next;
               case messageActionItem_OfShowMessageRequestClientCapabilities_Map
                 .Get_Index
                 (Key) is
                  when 1 =>  --  additionalPropertiesSupport
                     Value.additionalPropertiesSupport       :=
                       (Is_Set => True,
                        Value  => <>);
                     Value.additionalPropertiesSupport.Value :=
                       Handler.Boolean_Value;
                     Handler.Read_Next;
                  when others =>
                     Handler.Skip_Current_Value;
               end case;
            end;
         end loop;

         Handler.Read_Next;
      end Read_messageActionItem_OfShowMessageRequestClientCapabilities;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case ShowMessageRequestClientCapabilities_Map.Get_Index (Key) is
               when 1 =>  --  messageActionItem
                  Value.messageActionItem :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_messageActionItem_OfShowMessageRequestClientCapabilities
                    (Handler, Value.messageActionItem.Value);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_ShowMessageRequestClientCapabilities;

   procedure Read_SemanticTokensEdit_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SemanticTokensEdit_Vector) is
   begin
      pragma Assert (Handler.Is_Start_Array);
      Handler.Read_Next;

      declare
         Set   : LSP.Structures.SemanticTokensEdit_Vector renames Value;
         Value : LSP.Structures.SemanticTokensEdit;
      begin
         Set.Clear;
         while not Handler.Is_End_Array loop
            Read_SemanticTokensEdit (Handler, Value);
            Set.Append (Value);
         end loop;
      end;

      Handler.Read_Next;
   end Read_SemanticTokensEdit_Vector;

   package Position_Scope is
      package Position_Map is new Minimal_Perfect_Hash
        (["line",
         "character"]);

   end Position_Scope;

   procedure Read_Position
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Position) is
      use Position_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case Position_Map.Get_Index (Key) is
               when 1 =>  --  line
                  Value.line := Integer (Handler.Number_Value.Integer_Value);
                  Handler.Read_Next;
               when 2 =>  --  character
                  Value.character :=
                    Integer (Handler.Number_Value.Integer_Value);
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_Position;

   package TextDocumentSaveRegistrationOptions_Scope is
      package TextDocumentSaveRegistrationOptions_Map is new Minimal_Perfect_Hash
        (["documentSelector",
         "includeText"]);

   end TextDocumentSaveRegistrationOptions_Scope;

   procedure Read_TextDocumentSaveRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TextDocumentSaveRegistrationOptions) is
      use TextDocumentSaveRegistrationOptions_Scope;
      procedure Read_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.DocumentSelector_Or_Null);

      procedure Read_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.DocumentSelector_Or_Null) is
      begin
         if Handler.Is_Null_Value then
            Handler.Read_Next;
         else
            Value :=
              (Is_Null => False,
               Value   => <>);
            Read_DocumentSelector (Handler, Value.Value);
         end if;
      end Read_DocumentSelector_Or_Null;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case TextDocumentSaveRegistrationOptions_Map.Get_Index (Key) is
               when 1 =>  --  documentSelector
                  Read_DocumentSelector_Or_Null
                    (Handler, Value.Parent.documentSelector);
               when 2 =>  --  includeText
                  Value.includeText       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.includeText.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_TextDocumentSaveRegistrationOptions;

   package CodeActionParams_Scope is
      package CodeActionParams_Map is new Minimal_Perfect_Hash
        (["workDoneToken",
         "partialResultToken",
         "textDocument",
         "range",
         "context"]);

   end CodeActionParams_Scope;

   procedure Read_CodeActionParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CodeActionParams) is
      use CodeActionParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case CodeActionParams_Map.Get_Index (Key) is
               when 1 =>  --  workDoneToken
                  Value.workDoneToken :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_ProgressToken (Handler, Value.workDoneToken.Value);
               when 2 =>  --  partialResultToken
                  Value.partialResultToken :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_ProgressToken (Handler, Value.partialResultToken.Value);
               when 3 =>  --  textDocument
                  Read_TextDocumentIdentifier (Handler, Value.textDocument);
               when 4 =>  --  range
                  Read_A_Range (Handler, Value.a_range);
               when 5 =>  --  context
                  Read_CodeActionContext (Handler, Value.context);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_CodeActionParams;

   package Command_Scope is
      package Command_Map is new Minimal_Perfect_Hash
        (["title",
         "command",
         "arguments"]);

   end Command_Scope;

   procedure Read_Command
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Command) is
      use Command_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case Command_Map.Get_Index (Key) is
               when 1 =>  --  title
                  Value.title.Clear;
                  Value.title.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 2 =>  --  command
                  Value.command.Clear;
                  Value.command.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 3 =>  --  arguments
                  Read_LSPAny_Vector (Handler, Value.arguments);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_Command;

   package TextDocumentFilter_Scope is
      package TextDocumentFilter_Map is new Minimal_Perfect_Hash
        (["language",
         "scheme",
         "pattern"]);

   end TextDocumentFilter_Scope;

   procedure Read_TextDocumentFilter
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TextDocumentFilter) is
      use TextDocumentFilter_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while Handler.Is_Key_Name loop
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case TextDocumentFilter_Map.Get_Index (Key) is
               when 1 =>  --  language
                  Value.language.Clear;
                  Value.language.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 2 =>  --  scheme
                  Value.scheme.Clear;
                  Value.scheme.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 3 =>  --  pattern
                  Value.pattern.Clear;
                  Value.pattern.Append (Handler.String_Value);
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_TextDocumentFilter;

   procedure Read_FoldingRange_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.FoldingRange_Vector) is
   begin
      pragma Assert (Handler.Is_Start_Array);
      Handler.Read_Next;

      declare
         Set   : LSP.Structures.FoldingRange_Vector renames Value;
         Value : LSP.Structures.FoldingRange;
      begin
         Set.Clear;
         while not Handler.Is_End_Array loop
            Read_FoldingRange (Handler, Value);
            Set.Append (Value);
         end loop;
      end;

      Handler.Read_Next;
   end Read_FoldingRange_Vector;

   procedure Read_SelectionRange_Vector_Or_Null
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SelectionRange_Vector_Or_Null) is
   begin
      if Handler.Is_Null_Value then
         Handler.Read_Next;
      else
         Value.Clear;
         Read_SelectionRange_Vector (Handler, Value);
      end if;
   end Read_SelectionRange_Vector_Or_Null;

   package ExecuteCommandRegistrationOptions_Scope is
      package ExecuteCommandRegistrationOptions_Map is new Minimal_Perfect_Hash
        (["workDoneProgress",
         "commands"]);

   end ExecuteCommandRegistrationOptions_Scope;

   procedure Read_ExecuteCommandRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ExecuteCommandRegistrationOptions) is
      use ExecuteCommandRegistrationOptions_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case ExecuteCommandRegistrationOptions_Map.Get_Index (Key) is
               when 1 =>  --  workDoneProgress
                  Value.workDoneProgress       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.workDoneProgress.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 2 =>  --  commands
                  Read_Virtual_String_Vector (Handler, Value.commands);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_ExecuteCommandRegistrationOptions;

   package UniquenessLevel_Map is new Minimal_Perfect_Hash
     (["document",
      "project",
      "group",
      "scheme",
      "global"]);

   procedure Read_UniquenessLevel
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.UniquenessLevel) is
   begin
      Value :=
        LSP.Enumerations.UniquenessLevel'Val
          (UniquenessLevel_Map.Get_Index (Handler.String_Value) - 1);
      Handler.Read_Next;
   end Read_UniquenessLevel;

   package TextDocumentEdit_Scope is
      package TextDocumentEdit_Map is new Minimal_Perfect_Hash
        (["textDocument",
         "edits"]);

      package TextEdit_Or_AnnotatedTextEdit_Scope is
         package TextEdit_Or_AnnotatedTextEdit_Map is new Minimal_Perfect_Hash
           (["annotationId"]);

      end TextEdit_Or_AnnotatedTextEdit_Scope;

   end TextDocumentEdit_Scope;

   procedure Read_TextDocumentEdit
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TextDocumentEdit) is
      use TextDocumentEdit_Scope;
      procedure Read_TextEdit_Or_AnnotatedTextEdit
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.TextEdit_Or_AnnotatedTextEdit);

      procedure Read_TextEdit_Or_AnnotatedTextEdit_Vector
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.TextEdit_Or_AnnotatedTextEdit_Vector);

      procedure Read_TextEdit_Or_AnnotatedTextEdit
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.TextEdit_Or_AnnotatedTextEdit) is
         use TextEdit_Or_AnnotatedTextEdit_Scope;
      begin
         declare
            Parent  :
              constant not null access VSS.JSON.Pull_Readers.JSON_Pull_Reader'
                Class :=
              Handler'Access;
            Handler :
              VSS.JSON.Pull_Readers.Buffered.JSON_Buffered_Pull_Reader
                (Parent);
         begin
            Handler.Mark;
            if Handler.Is_Start_Object then
               Handler.Read_Next;
               Value :=
                 (Is_TextEdit => True,
                  others      => <>);
               while Handler.Is_Key_Name loop
                  declare
                     Key   : constant VSS.Strings.Virtual_String :=
                       Handler.Key_Name;
                     Index : constant Natural                    :=
                       TextEdit_Or_AnnotatedTextEdit_Map.Get_Index (Key);
                  begin
                     Handler.Read_Next;
                     case Index is
                        when 1 =>  --  annotationId
                           Value :=
                             (Is_TextEdit => False,
                              others      => <>);
                           exit;
                        when others =>
                           Handler.Skip_Current_Value;
                     end case;
                  end;
               end loop;
            else
               raise Program_Error;  --  Unexpected JSON value
            end if;

            Handler.Reset;
            Handler.Unmark;

            case Value.Is_TextEdit is
               when True =>
                  Read_TextEdit (Handler, Value.TextEdit);
               when False =>
                  Read_AnnotatedTextEdit (Handler, Value.AnnotatedTextEdit);
            end case;
         end;
      end Read_TextEdit_Or_AnnotatedTextEdit;

      procedure Read_TextEdit_Or_AnnotatedTextEdit_Vector
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.TextEdit_Or_AnnotatedTextEdit_Vector) is
      begin
         pragma Assert (Handler.Is_Start_Array);
         Handler.Read_Next;

         declare
            Set   :
              LSP.Structures.TextEdit_Or_AnnotatedTextEdit_Vector renames
              Value;
            Value : LSP.Structures.TextEdit_Or_AnnotatedTextEdit;
         begin
            Set.Clear;
            while not Handler.Is_End_Array loop
               Read_TextEdit_Or_AnnotatedTextEdit (Handler, Value);
               Set.Append (Value);
            end loop;
         end;

         Handler.Read_Next;
      end Read_TextEdit_Or_AnnotatedTextEdit_Vector;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case TextDocumentEdit_Map.Get_Index (Key) is
               when 1 =>  --  textDocument
                  Read_OptionalVersionedTextDocumentIdentifier
                    (Handler, Value.textDocument);
               when 2 =>  --  edits
                  Read_TextEdit_Or_AnnotatedTextEdit_Vector
                    (Handler, Value.edits);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_TextDocumentEdit;

   package CompletionItemLabelDetails_Scope is
      package CompletionItemLabelDetails_Map is new Minimal_Perfect_Hash
        (["detail",
         "description"]);

   end CompletionItemLabelDetails_Scope;

   procedure Read_CompletionItemLabelDetails
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CompletionItemLabelDetails) is
      use CompletionItemLabelDetails_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case CompletionItemLabelDetails_Map.Get_Index (Key) is
               when 1 =>  --  detail
                  Value.detail.Clear;
                  Value.detail.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 2 =>  --  description
                  Value.description.Clear;
                  Value.description.Append (Handler.String_Value);
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_CompletionItemLabelDetails;

   package CompletionOptions_Scope is
      package CompletionOptions_Map is new Minimal_Perfect_Hash
        (["workDoneProgress",
         "triggerCharacters",
         "allCommitCharacters",
         "resolveProvider",
         "completionItem"]);

      package completionItem_OfCompletionOptions_Scope is
         package completionItem_OfCompletionOptions_Map is new Minimal_Perfect_Hash
           (["labelDetailsSupport"]);

      end completionItem_OfCompletionOptions_Scope;

   end CompletionOptions_Scope;

   procedure Read_CompletionOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CompletionOptions) is
      use CompletionOptions_Scope;
      procedure Read_completionItem_OfCompletionOptions
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.completionItem_OfCompletionOptions);

      procedure Read_completionItem_OfCompletionOptions
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.completionItem_OfCompletionOptions) is
      begin
         pragma Assert (Handler.Is_Start_Object);
         Handler.Read_Next;

         while Handler.Is_Key_Name loop
            declare
               use completionItem_OfCompletionOptions_Scope;
               Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
            begin
               Handler.Read_Next;
               case completionItem_OfCompletionOptions_Map.Get_Index (Key) is
                  when 1 =>  --  labelDetailsSupport
                     Value.labelDetailsSupport       :=
                       (Is_Set => True,
                        Value  => <>);
                     Value.labelDetailsSupport.Value := Handler.Boolean_Value;
                     Handler.Read_Next;
                  when others =>
                     Handler.Skip_Current_Value;
               end case;
            end;
         end loop;

         Handler.Read_Next;
      end Read_completionItem_OfCompletionOptions;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case CompletionOptions_Map.Get_Index (Key) is
               when 1 =>  --  workDoneProgress
                  Value.workDoneProgress       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.workDoneProgress.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 2 =>  --  triggerCharacters
                  Read_Virtual_String_Vector
                    (Handler, Value.triggerCharacters);
               when 3 =>  --  allCommitCharacters
                  Read_Virtual_String_Vector
                    (Handler, Value.allCommitCharacters);
               when 4 =>  --  resolveProvider
                  Value.resolveProvider       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.resolveProvider.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 5 =>  --  completionItem
                  Value.completionItem :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_completionItem_OfCompletionOptions
                    (Handler, Value.completionItem.Value);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_CompletionOptions;

   package Diagnostic_Scope is
      package Diagnostic_Map is new Minimal_Perfect_Hash
        (["range",
         "severity",
         "code",
         "codeDescription",
         "source",
         "message",
         "tags",
         "relatedInformation",
         "data"]);

   end Diagnostic_Scope;

   procedure Read_Diagnostic
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.Diagnostic) is
      use Diagnostic_Scope;
      procedure Read_DiagnosticRelatedInformation_Vector
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.DiagnosticRelatedInformation_Vector);

      procedure Read_DiagnosticRelatedInformation_Vector
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.DiagnosticRelatedInformation_Vector) is
      begin
         pragma Assert (Handler.Is_Start_Array);
         Handler.Read_Next;

         declare
            Set   :
              LSP.Structures.DiagnosticRelatedInformation_Vector renames Value;
            Value : LSP.Structures.DiagnosticRelatedInformation;
         begin
            Set.Clear;
            while not Handler.Is_End_Array loop
               Read_DiagnosticRelatedInformation (Handler, Value);
               Set.Append (Value);
            end loop;
         end;

         Handler.Read_Next;
      end Read_DiagnosticRelatedInformation_Vector;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case Diagnostic_Map.Get_Index (Key) is
               when 1 =>  --  range
                  Read_A_Range (Handler, Value.a_range);
               when 2 =>  --  severity
                  Value.severity :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_DiagnosticSeverity (Handler, Value.severity.Value);
               when 3 =>  --  code
                  Value.code :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_Integer_Or_Virtual_String (Handler, Value.code.Value);
               when 4 =>  --  codeDescription
                  Value.codeDescription :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_CodeDescription (Handler, Value.codeDescription.Value);
               when 5 =>  --  source
                  Value.source.Clear;
                  Value.source.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 6 =>  --  message
                  Value.message.Clear;
                  Value.message.Append (Handler.String_Value);
                  Handler.Read_Next;
               when 7 =>  --  tags
                  Read_DiagnosticTag_Set (Handler, Value.tags);
               when 8 =>  --  relatedInformation
                  Read_DiagnosticRelatedInformation_Vector
                    (Handler, Value.relatedInformation);
               when 9 =>  --  data
                  Read_LSPAny (Handler, Value.data);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_Diagnostic;

   package LinkedEditingRangeParams_Scope is
      package LinkedEditingRangeParams_Map is new Minimal_Perfect_Hash
        (["textDocument",
         "position",
         "workDoneToken"]);

   end LinkedEditingRangeParams_Scope;

   procedure Read_LinkedEditingRangeParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.LinkedEditingRangeParams) is
      use LinkedEditingRangeParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case LinkedEditingRangeParams_Map.Get_Index (Key) is
               when 1 =>  --  textDocument
                  Read_TextDocumentIdentifier (Handler, Value.textDocument);
               when 2 =>  --  position
                  Read_Position (Handler, Value.position);
               when 3 =>  --  workDoneToken
                  Value.workDoneToken :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_ProgressToken (Handler, Value.workDoneToken.Value);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_LinkedEditingRangeParams;

   package TextDocumentSyncOptions_Scope is
      package TextDocumentSyncOptions_Map is new Minimal_Perfect_Hash
        (["openClose",
         "change",
         "willSave",
         "willSaveWaitUntil",
         "save"]);

   end TextDocumentSyncOptions_Scope;

   procedure Read_TextDocumentSyncOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TextDocumentSyncOptions) is
      use TextDocumentSyncOptions_Scope;
      procedure Read_Boolean_Or_SaveOptions
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.Boolean_Or_SaveOptions);

      procedure Read_Boolean_Or_SaveOptions
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.Boolean_Or_SaveOptions) is
      begin
         if Handler.Is_Boolean_Value then
            Value :=
              (Is_Boolean => True,
               others     => <>);
         else
            Value :=
              (Is_Boolean => False,
               others     => <>);
         end if;

         case Value.Is_Boolean is
            when True =>
               Value.Boolean := Handler.Boolean_Value;
               Handler.Read_Next;
            when False =>
               Read_SaveOptions (Handler, Value.SaveOptions);
         end case;
      end Read_Boolean_Or_SaveOptions;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case TextDocumentSyncOptions_Map.Get_Index (Key) is
               when 1 =>  --  openClose
                  Value.openClose       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.openClose.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 2 =>  --  change
                  Value.change :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_TextDocumentSyncKind (Handler, Value.change.Value);
               when 3 =>  --  willSave
                  Value.willSave       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.willSave.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 4 =>  --  willSaveWaitUntil
                  Value.willSaveWaitUntil       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.willSaveWaitUntil.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 5 =>  --  save
                  Value.save :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_Boolean_Or_SaveOptions (Handler, Value.save.Value);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_TextDocumentSyncOptions;

   package ImplementationParams_Scope is
      package ImplementationParams_Map is new Minimal_Perfect_Hash
        (["textDocument",
         "position",
         "workDoneToken",
         "partialResultToken"]);

   end ImplementationParams_Scope;

   procedure Read_ImplementationParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ImplementationParams) is
      use ImplementationParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case ImplementationParams_Map.Get_Index (Key) is
               when 1 =>  --  textDocument
                  Read_TextDocumentIdentifier (Handler, Value.textDocument);
               when 2 =>  --  position
                  Read_Position (Handler, Value.position);
               when 3 =>  --  workDoneToken
                  Value.workDoneToken :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_ProgressToken (Handler, Value.workDoneToken.Value);
               when 4 =>  --  partialResultToken
                  Value.partialResultToken :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_ProgressToken (Handler, Value.partialResultToken.Value);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_ImplementationParams;

   package OptionalVersionedTextDocumentIdentifier_Scope is
      package OptionalVersionedTextDocumentIdentifier_Map is new Minimal_Perfect_Hash
        (["uri",
         "version"]);

   end OptionalVersionedTextDocumentIdentifier_Scope;

   procedure Read_OptionalVersionedTextDocumentIdentifier
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.OptionalVersionedTextDocumentIdentifier) is
      use OptionalVersionedTextDocumentIdentifier_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case OptionalVersionedTextDocumentIdentifier_Map.Get_Index (Key) is
               when 1 =>  --  uri
                  Value.uri := (Handler.String_Value with null record);
                  Handler.Read_Next;
               when 2 =>  --  version
                  Read_Integer_Or_Null (Handler, Value.version);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_OptionalVersionedTextDocumentIdentifier;

   package SemanticTokensLegend_Scope is
      package SemanticTokensLegend_Map is new Minimal_Perfect_Hash
        (["tokenTypes",
         "tokenModifiers"]);

   end SemanticTokensLegend_Scope;

   procedure Read_SemanticTokensLegend
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.SemanticTokensLegend) is
      use SemanticTokensLegend_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case SemanticTokensLegend_Map.Get_Index (Key) is
               when 1 =>  --  tokenTypes
                  Read_Virtual_String_Vector (Handler, Value.tokenTypes);
               when 2 =>  --  tokenModifiers
                  Read_Virtual_String_Vector (Handler, Value.tokenModifiers);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_SemanticTokensLegend;

   procedure Read_WorkspaceDocumentDiagnosticReport_Vector
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.WorkspaceDocumentDiagnosticReport_Vector) is
   begin
      pragma Assert (Handler.Is_Start_Array);
      Handler.Read_Next;

      declare
         Set   :
           LSP.Structures.WorkspaceDocumentDiagnosticReport_Vector renames
           Value;
         Value : LSP.Structures.WorkspaceDocumentDiagnosticReport;
      begin
         Set.Clear;
         while not Handler.Is_End_Array loop
            Read_WorkspaceDocumentDiagnosticReport (Handler, Value);
            Set.Append (Value);
         end loop;
      end;

      Handler.Read_Next;
   end Read_WorkspaceDocumentDiagnosticReport_Vector;

   package ExecuteCommandOptions_Scope is
      package ExecuteCommandOptions_Map is new Minimal_Perfect_Hash
        (["workDoneProgress",
         "commands"]);

   end ExecuteCommandOptions_Scope;

   procedure Read_ExecuteCommandOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ExecuteCommandOptions) is
      use ExecuteCommandOptions_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case ExecuteCommandOptions_Map.Get_Index (Key) is
               when 1 =>  --  workDoneProgress
                  Value.workDoneProgress       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.workDoneProgress.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 2 =>  --  commands
                  Read_Virtual_String_Vector (Handler, Value.commands);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_ExecuteCommandOptions;

   package TextDocumentSyncClientCapabilities_Scope is
      package TextDocumentSyncClientCapabilities_Map is new Minimal_Perfect_Hash
        (["dynamicRegistration",
         "willSave",
         "willSaveWaitUntil",
         "didSave"]);

   end TextDocumentSyncClientCapabilities_Scope;

   procedure Read_TextDocumentSyncClientCapabilities
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.TextDocumentSyncClientCapabilities) is
      use TextDocumentSyncClientCapabilities_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case TextDocumentSyncClientCapabilities_Map.Get_Index (Key) is
               when 1 =>  --  dynamicRegistration
                  Value.dynamicRegistration       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.dynamicRegistration.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 2 =>  --  willSave
                  Value.willSave       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.willSave.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 3 =>  --  willSaveWaitUntil
                  Value.willSaveWaitUntil       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.willSaveWaitUntil.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when 4 =>  --  didSave
                  Value.didSave       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.didSave.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_TextDocumentSyncClientCapabilities;

   package MarkupKind_Map is new Minimal_Perfect_Hash
     (["plaintext",
      "markdown"]);

   procedure Read_MarkupKind
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.MarkupKind) is
   begin
      Value :=
        LSP.Enumerations.MarkupKind'Val
          (MarkupKind_Map.Get_Index (Handler.String_Value) - 1);
      Handler.Read_Next;
   end Read_MarkupKind;

   package CallHierarchyPrepareParams_Scope is
      package CallHierarchyPrepareParams_Map is new Minimal_Perfect_Hash
        (["textDocument",
         "position",
         "workDoneToken"]);

   end CallHierarchyPrepareParams_Scope;

   procedure Read_CallHierarchyPrepareParams
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.CallHierarchyPrepareParams) is
      use CallHierarchyPrepareParams_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case CallHierarchyPrepareParams_Map.Get_Index (Key) is
               when 1 =>  --  textDocument
                  Read_TextDocumentIdentifier (Handler, Value.textDocument);
               when 2 =>  --  position
                  Read_Position (Handler, Value.position);
               when 3 =>  --  workDoneToken
                  Value.workDoneToken :=
                    (Is_Set => True,
                     Value  => <>);
                  Read_ProgressToken (Handler, Value.workDoneToken.Value);
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_CallHierarchyPrepareParams;

   procedure Read_LSPErrorCodes
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Enumerations.LSPErrorCodes) is
   begin
      Value :=
        LSP.Enumerations.LSPErrorCodes'Val
          (Handler.Number_Value.Integer_Value + 32_899);
      Handler.Read_Next;
   end Read_LSPErrorCodes;

   package ReferenceRegistrationOptions_Scope is
      package ReferenceRegistrationOptions_Map is new Minimal_Perfect_Hash
        (["documentSelector",
         "workDoneProgress"]);

   end ReferenceRegistrationOptions_Scope;

   procedure Read_ReferenceRegistrationOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.ReferenceRegistrationOptions) is
      use ReferenceRegistrationOptions_Scope;
      procedure Read_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.DocumentSelector_Or_Null);

      procedure Read_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
         Value   : out LSP.Structures.DocumentSelector_Or_Null) is
      begin
         if Handler.Is_Null_Value then
            Handler.Read_Next;
         else
            Value :=
              (Is_Null => False,
               Value   => <>);
            Read_DocumentSelector (Handler, Value.Value);
         end if;
      end Read_DocumentSelector_Or_Null;

   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case ReferenceRegistrationOptions_Map.Get_Index (Key) is
               when 1 =>  --  documentSelector
                  Read_DocumentSelector_Or_Null
                    (Handler, Value.Parent.documentSelector);
               when 2 =>  --  workDoneProgress
                  Value.workDoneProgress       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.workDoneProgress.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_ReferenceRegistrationOptions;

   package LinkedEditingRangeOptions_Scope is
      package LinkedEditingRangeOptions_Map is new Minimal_Perfect_Hash
        (["workDoneProgress"]);

   end LinkedEditingRangeOptions_Scope;

   procedure Read_LinkedEditingRangeOptions
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Value   : out LSP.Structures.LinkedEditingRangeOptions) is
      use LinkedEditingRangeOptions_Scope;
   begin
      pragma Assert (Handler.Is_Start_Object);
      Handler.Read_Next;

      while not Handler.Is_End_Object loop
         pragma Assert (Handler.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
         begin
            Handler.Read_Next;
            case LinkedEditingRangeOptions_Map.Get_Index (Key) is
               when 1 =>  --  workDoneProgress
                  Value.workDoneProgress       :=
                    (Is_Set => True,
                     Value  => <>);
                  Value.workDoneProgress.Value := Handler.Boolean_Value;
                  Handler.Read_Next;
               when others =>
                  Handler.Skip_Current_Value;
            end case;
         end;
      end loop;

      Handler.Read_Next;
   end Read_LinkedEditingRangeOptions;

end LSP.Inputs;
