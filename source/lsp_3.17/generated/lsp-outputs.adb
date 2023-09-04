--
--  Copyright (C) <YEAR>, <COPYRIGHT HOLDER>
--
--  SPDX-License-Identifier: MIT
--
--  DON'T EDIT THIS FILE! It was generated from metaModel.json.
--

with Ada.Containers;
with Interfaces;
with LSP.Output_Tools;

package body LSP.Outputs is

   pragma Warnings (Off, "is not referenced");
   use type Interfaces.Integer_64;

   use type Ada.Containers.Count_Type;

   procedure Write_ClientCapabilities
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.ClientCapabilities);

   procedure Write_FileChangeType
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Enumerations.FileChangeType);

   procedure Write_Virtual_String
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.Virtual_String);

   procedure Write_Declaration
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.Declaration);

   procedure Write_FileOperationRegistrationOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.FileOperationRegistrationOptions);

   procedure Write_WorkspaceFolder_Vector
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.WorkspaceFolder_Vector);

   procedure Write_TextDocumentContentChangeEvent_Vector
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.TextDocumentContentChangeEvent_Vector);

   procedure Write_FoldingRangeClientCapabilities
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.FoldingRangeClientCapabilities);

   procedure Write_CreateFile
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.CreateFile);

   procedure Write_CompletionContext
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.CompletionContext);

   procedure Write_NotebookCell_Vector
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.NotebookCell_Vector);

   procedure Write_CodeActionKind
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Enumerations.CodeActionKind);

   procedure Write_RelatedUnchangedDocumentDiagnosticReport
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.RelatedUnchangedDocumentDiagnosticReport);

   procedure Write_DidChangeWatchedFilesClientCapabilities
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DidChangeWatchedFilesClientCapabilities);

   procedure Write_SemanticTokenTypes
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Enumerations.SemanticTokenTypes);

   procedure Write_AnnotatedTextEdit
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.AnnotatedTextEdit);

   procedure Write_DeclarationOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DeclarationOptions);

   procedure Write_SignatureInformation
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.SignatureInformation);

   procedure Write_TextDocumentSaveReason
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Enumerations.TextDocumentSaveReason);

   procedure Write_SemanticTokens
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.SemanticTokens);

   procedure Write_TokenFormat
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Enumerations.TokenFormat);

   procedure Write_TextDocumentRegistrationOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.TextDocumentRegistrationOptions);

   procedure Write_URI
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.URI);

   procedure Write_SemanticTokenModifiers
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Enumerations.SemanticTokenModifiers);

   procedure Write_ConfigurationItem
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.ConfigurationItem);

   procedure Write_DocumentOnTypeFormattingClientCapabilities
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DocumentOnTypeFormattingClientCapabilities);

   procedure Write_SymbolKind
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Enumerations.SymbolKind);

   procedure Write_SelectionRangeClientCapabilities
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.SelectionRangeClientCapabilities);

   procedure Write_Range_Vector
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.Range_Vector);

   procedure Write_Definition
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.Definition);

   procedure Write_NotebookDocumentFilter
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.NotebookDocumentFilter);

   procedure Write_Color
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.Color);

   procedure Write_FileOperationPattern
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.FileOperationPattern);

   procedure Write_LinkedEditingRanges
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.LinkedEditingRanges);

   procedure Write_Virtual_String_Or_MarkupContent
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.Virtual_String_Or_MarkupContent);

   procedure Write_ServerCapabilities
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.ServerCapabilities);

   procedure Write_DidChangeWatchedFilesRegistrationOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DidChangeWatchedFilesRegistrationOptions);

   procedure Write_CallHierarchyOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.CallHierarchyOptions);

   procedure Write_ImplementationRegistrationOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.ImplementationRegistrationOptions);

   procedure Write_FoldingRangeRegistrationOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.FoldingRangeRegistrationOptions);

   procedure Write_InlineValueContext
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.InlineValueContext);

   procedure Write_TextDocumentIdentifier_Vector
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.TextDocumentIdentifier_Vector);

   procedure Write_DefinitionLink
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DefinitionLink);

   procedure Write_DocumentFormattingOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DocumentFormattingOptions);

   procedure Write_SymbolInformation_Vector
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.SymbolInformation_Vector);

   procedure Write_RenameFile
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.RenameFile);

   procedure Write_DidChangeConfigurationRegistrationOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DidChangeConfigurationRegistrationOptions);

   procedure Write_CallHierarchyItem
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.CallHierarchyItem);

   procedure Write_SymbolTag_Set
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.SymbolTag_Set);

   procedure Write_RelativePattern
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.RelativePattern);

   procedure Write_ReferenceOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.ReferenceOptions);

   procedure Write_tagSupport_OfWorkspaceSymbolClientCapabilities
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.tagSupport_OfWorkspaceSymbolClientCapabilities);

   procedure Write_TextDocumentClientCapabilities
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.TextDocumentClientCapabilities);

   procedure Write_DocumentSymbol_Vector
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DocumentSymbol_Vector);

   procedure Write_GlobPattern
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.GlobPattern);

   procedure Write_NotebookDocumentIdentifier
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.NotebookDocumentIdentifier);

   procedure Write_InsertReplaceEdit
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.InsertReplaceEdit);

   procedure Write_InlineValueVariableLookup
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.InlineValueVariableLookup);

   procedure Write_FileEvent
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.FileEvent);

   procedure Write_SignatureHelpClientCapabilities
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.SignatureHelpClientCapabilities);

   procedure Write_NotebookDocumentSyncClientCapabilities
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.NotebookDocumentSyncClientCapabilities);

   procedure Write_DiagnosticOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DiagnosticOptions);

   procedure Write_WindowClientCapabilities
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.WindowClientCapabilities);

   procedure Write_GeneralClientCapabilities
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.GeneralClientCapabilities);

   procedure Write_SemanticTokensWorkspaceClientCapabilities
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.SemanticTokensWorkspaceClientCapabilities);

   procedure Write_LinkedEditingRangeClientCapabilities
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.LinkedEditingRangeClientCapabilities);

   procedure Write_SignatureHelp
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.SignatureHelp);

   procedure Write_Diagnostic_Vector
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.Diagnostic_Vector);

   procedure Write_DocumentHighlightKind
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Enumerations.DocumentHighlightKind);

   procedure Write_ResourceOperationKind
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Enumerations.ResourceOperationKind);

   procedure Write_InlineValueClientCapabilities
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.InlineValueClientCapabilities);

   procedure Write_PrepareRenameResult
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.PrepareRenameResult);

   procedure Write_DocumentLinkClientCapabilities
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DocumentLinkClientCapabilities);

   procedure Write_MarkupKind_Vector
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.MarkupKind_Vector);

   procedure Write_CodeActionContext
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.CodeActionContext);

   procedure Write_InlayHintRegistrationOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.InlayHintRegistrationOptions);

   procedure Write_Natural_Vector
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.Natural_Vector);

   procedure Write_CodeDescription
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.CodeDescription);

   procedure Write_Virtual_String_Vector
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.Virtual_String_Vector);

   procedure Write_LSPObject
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.LSPObject);

   procedure Write_TextDocumentPositionParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.TextDocumentPositionParams);

   procedure Write_CodeLensRegistrationOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.CodeLensRegistrationOptions);

   procedure Write_SelectionRange
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.SelectionRange);

   procedure Write_NotebookCellTextDocumentFilter
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.NotebookCellTextDocumentFilter);

   procedure Write_WorkspaceFoldersInitializeParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.WorkspaceFoldersInitializeParams);

   procedure Write_FileSystemWatcher
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.FileSystemWatcher);

   procedure Write_CodeActionKind_Set
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.CodeActionKind_Set);

   procedure Write_NotebookDocumentSyncOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.NotebookDocumentSyncOptions);

   procedure Write_CodeActionRegistrationOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.CodeActionRegistrationOptions);

   procedure Write_NotebookDocumentSyncRegistrationOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.NotebookDocumentSyncRegistrationOptions);

   procedure Write_WorkspaceUnchangedDocumentDiagnosticReport
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.WorkspaceUnchangedDocumentDiagnosticReport);

   procedure Write_HoverClientCapabilities
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.HoverClientCapabilities);

   procedure Write_RenameClientCapabilities
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.RenameClientCapabilities);

   procedure Write_DefinitionOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DefinitionOptions);

   procedure Write_InlayHintKind
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Enumerations.InlayHintKind);

   procedure Write_symbolKind_OfWorkspaceSymbolClientCapabilities
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.symbolKind_OfWorkspaceSymbolClientCapabilities);

   procedure Write_FailureHandlingKind
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Enumerations.FailureHandlingKind);

   procedure Write_FileOperationOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.FileOperationOptions);

   procedure Write_DocumentSelector
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DocumentSelector);

   procedure Write_CompletionClientCapabilities
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.CompletionClientCapabilities);

   procedure Write_CodeLensWorkspaceClientCapabilities
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.CodeLensWorkspaceClientCapabilities);

   procedure Write_RenameOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.RenameOptions);

   procedure Write_SymbolKind_Set
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.SymbolKind_Set);

   procedure Write_DidChangeConfigurationClientCapabilities
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DidChangeConfigurationClientCapabilities);

   procedure Write_DocumentHighlightRegistrationOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DocumentHighlightRegistrationOptions);

   procedure Write_DeclarationLink
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DeclarationLink);

   procedure Write_T
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.T);

   procedure Write_FullDocumentDiagnosticReport
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.FullDocumentDiagnosticReport);

   procedure Write_DocumentSymbolRegistrationOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DocumentSymbolRegistrationOptions);

   procedure Write_DeclarationRegistrationOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DeclarationRegistrationOptions);

   procedure Write_TypeHierarchyRegistrationOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.TypeHierarchyRegistrationOptions);

   procedure Write_DeleteFileOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DeleteFileOptions);

   procedure Write_WatchKind
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Enumerations.WatchKind);

   procedure Write_DiagnosticSeverity
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Enumerations.DiagnosticSeverity);

   procedure Write_CodeActionTriggerKind
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Enumerations.CodeActionTriggerKind);

   procedure Write_CodeActionClientCapabilities
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.CodeActionClientCapabilities);

   procedure Write_MarkdownClientCapabilities
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.MarkdownClientCapabilities);

   procedure Write_DefinitionLink_Vector
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DefinitionLink_Vector);

   procedure Write_ImplementationClientCapabilities
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.ImplementationClientCapabilities);

   procedure Write_TextDocumentItem
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.TextDocumentItem);

   procedure Write_InsertTextFormat
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Enumerations.InsertTextFormat);

   procedure Write_relatedDocuments_OfDocumentDiagnosticReportPartialResult_Item
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures
        .relatedDocuments_OfDocumentDiagnosticReportPartialResult_Item);

   procedure Write_ExecuteCommandClientCapabilities
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.ExecuteCommandClientCapabilities);

   procedure Write_CallHierarchyItem_Vector
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.CallHierarchyItem_Vector);

   procedure Write_DeleteFile
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DeleteFile);

   procedure Write_SaveOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.SaveOptions);

   procedure Write_NotebookDocument
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.NotebookDocument);

   procedure Write_RelatedFullDocumentDiagnosticReport
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.RelatedFullDocumentDiagnosticReport);

   procedure Write_CodeLensClientCapabilities
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.CodeLensClientCapabilities);

   procedure Write_DocumentSymbolOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DocumentSymbolOptions);

   procedure Write_CompletionRegistrationOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.CompletionRegistrationOptions);

   procedure Write_InlayHintClientCapabilities
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.InlayHintClientCapabilities);

   procedure Write_ReferenceContext
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.ReferenceContext);

   procedure Write_WorkspaceEditClientCapabilities
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.WorkspaceEditClientCapabilities);

   procedure Write_WorkspaceSymbolOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.WorkspaceSymbolOptions);

   procedure Write_ChangeAnnotation
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.ChangeAnnotation);

   procedure Write_FileOperationPatternOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.FileOperationPatternOptions);

   procedure Write_VersionedTextDocumentIdentifier
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.VersionedTextDocumentIdentifier);

   procedure Write_SemanticTokensDelta
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.SemanticTokensDelta);

   procedure Write_ShowDocumentClientCapabilities
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.ShowDocumentClientCapabilities);

   procedure Write_InlayHintLabelPart
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.InlayHintLabelPart);

   procedure Write_SelectionRangeOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.SelectionRangeOptions);

   procedure Write_LocationLink
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.LocationLink);

   procedure Write_CompletionList
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.CompletionList);

   procedure Write_DiagnosticRegistrationOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DiagnosticRegistrationOptions);

   procedure Write_SignatureHelpTriggerKind
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Enumerations.SignatureHelpTriggerKind);

   procedure Write_TypeHierarchyClientCapabilities
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.TypeHierarchyClientCapabilities);

   procedure Write_DocumentOnTypeFormattingOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DocumentOnTypeFormattingOptions);

   procedure Write_WorkspaceSymbolRegistrationOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.WorkspaceSymbolRegistrationOptions);

   procedure Write_FileDelete
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.FileDelete);

   procedure Write_DocumentFormattingRegistrationOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DocumentFormattingRegistrationOptions);

   procedure Write_ColorPresentation
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.ColorPresentation);

   procedure Write_TypeHierarchyItem
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.TypeHierarchyItem);

   procedure Write_Boolean_Or_Any
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.Boolean_Or_Any);

   procedure Write_Location
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.Location);

   procedure Write_RenameRegistrationOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.RenameRegistrationOptions);

   procedure Write_ParameterInformation
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.ParameterInformation);

   procedure Write_PositionEncodingKind
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Enumerations.PositionEncodingKind);

   procedure Write_MessageType
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Enumerations.MessageType);

   procedure Write_An_InitializeParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.An_InitializeParams);

   procedure Write_FileOperationFilter
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.FileOperationFilter);

   procedure Write_MonikerOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.MonikerOptions);

   procedure Write_InlineValue
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.InlineValue);

   procedure Write_Virtual_String_Or_NotebookDocumentFilter
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.Virtual_String_Or_NotebookDocumentFilter);

   procedure Write_MonikerKind
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Enumerations.MonikerKind);

   procedure Write_NotebookCellKind
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Enumerations.NotebookCellKind);

   procedure Write_HoverRegistrationOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.HoverRegistrationOptions);

   procedure Write_CompletionItemTag
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Enumerations.CompletionItemTag);

   procedure Write_DefinitionRegistrationOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DefinitionRegistrationOptions);

   procedure Write_clientInfo_Of_InitializeParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.clientInfo_Of_InitializeParams);

   procedure Write_DiagnosticTag
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Enumerations.DiagnosticTag);

   procedure Write_DiagnosticClientCapabilities
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DiagnosticClientCapabilities);

   procedure Write_MonikerRegistrationOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.MonikerRegistrationOptions);

   procedure Write_ExecutionSummary
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.ExecutionSummary);

   procedure Write_PreviousResultId
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.PreviousResultId);

   procedure Write_FoldingRangeKind
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Enumerations.FoldingRangeKind);

   procedure Write_DocumentFilter
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DocumentFilter);

   procedure Write_CallHierarchyIncomingCall
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.CallHierarchyIncomingCall);

   procedure Write_TextDocumentChangeRegistrationOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.TextDocumentChangeRegistrationOptions);

   procedure Write_DeclarationLink_Vector
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DeclarationLink_Vector);

   procedure Write_DocumentFormattingClientCapabilities
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DocumentFormattingClientCapabilities);

   procedure Write_NotebookDocumentChangeEvent
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.NotebookDocumentChangeEvent);

   procedure Write_FormattingOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.FormattingOptions);

   procedure Write_resolveSupport_OfWorkspaceSymbolClientCapabilities
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures
        .resolveSupport_OfWorkspaceSymbolClientCapabilities);

   procedure Write_WorkspaceSymbol_Vector
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.WorkspaceSymbol_Vector);

   procedure Write_TypeDefinitionRegistrationOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.TypeDefinitionRegistrationOptions);

   procedure Write_CodeLensOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.CodeLensOptions);

   procedure Write_FileRename
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.FileRename);

   procedure Write_DocumentRangeFormattingClientCapabilities
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DocumentRangeFormattingClientCapabilities);

   procedure Write_Moniker
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.Moniker);

   procedure Write_DiagnosticWorkspaceClientCapabilities
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DiagnosticWorkspaceClientCapabilities);

   procedure Write_DefinitionClientCapabilities
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DefinitionClientCapabilities);

   procedure Write_SignatureHelpRegistrationOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.SignatureHelpRegistrationOptions);

   procedure Write_TypeDefinitionOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.TypeDefinitionOptions);

   procedure Write_MarkupContent
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.MarkupContent);

   procedure Write_WorkspaceEdit
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.WorkspaceEdit);

   procedure Write_DocumentHighlight
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DocumentHighlight);

   procedure Write_MessageActionItem
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.MessageActionItem);

   procedure Write_TextDocumentItem_Vector
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.TextDocumentItem_Vector);

   procedure Write_InlineValueRegistrationOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.InlineValueRegistrationOptions);

   procedure Write_VersionedNotebookDocumentIdentifier
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.VersionedNotebookDocumentIdentifier);

   procedure Write_BaseSymbolInformation
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.BaseSymbolInformation);

   procedure Write_TextEdit
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.TextEdit);

   procedure Write_DeclarationClientCapabilities
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DeclarationClientCapabilities);

   procedure Write_SignatureHelpOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.SignatureHelpOptions);

   procedure Write_SemanticTokensEdit
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.SemanticTokensEdit);

   procedure Write_FileCreate
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.FileCreate);

   procedure Write_WorkspaceFullDocumentDiagnosticReport
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.WorkspaceFullDocumentDiagnosticReport);

   procedure Write_FoldingRange
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.FoldingRange);

   procedure Write_SemanticTokensDeltaPartialResult
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.SemanticTokensDeltaPartialResult);

   procedure Write_NotebookDocumentClientCapabilities
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.NotebookDocumentClientCapabilities);

   procedure Write_WorkspaceSymbolClientCapabilities
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.WorkspaceSymbolClientCapabilities);

   procedure Write_Boolean_Or_Something
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.Boolean_Or_Something);

   procedure Write_TypeHierarchyOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.TypeHierarchyOptions);

   procedure Write_SelectionRangeRegistrationOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.SelectionRangeRegistrationOptions);

   procedure Write_RegularExpressionsClientCapabilities
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.RegularExpressionsClientCapabilities);

   procedure Write_WorkspaceDocumentDiagnosticReport
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.WorkspaceDocumentDiagnosticReport);

   procedure Write_WorkspaceClientCapabilities
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.WorkspaceClientCapabilities);

   procedure Write_TextDocumentSyncKind
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Enumerations.TextDocumentSyncKind);

   procedure Write_InlayHintOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.InlayHintOptions);

   procedure Write_InlineValueOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.InlineValueOptions);

   procedure Write_WorkspaceFoldersChangeEvent
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.WorkspaceFoldersChangeEvent);

   procedure Write_TypeDefinitionClientCapabilities
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.TypeDefinitionClientCapabilities);

   procedure Write_CreateFileOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.CreateFileOptions);

   procedure Write_AlsReferenceKind
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Enumerations.AlsReferenceKind);

   procedure Write_DiagnosticRelatedInformation
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DiagnosticRelatedInformation);

   procedure Write_InsertTextMode
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Enumerations.InsertTextMode);

   procedure Write_ImplementationOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.ImplementationOptions);

   procedure Write_DocumentColorRegistrationOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DocumentColorRegistrationOptions);

   procedure Write_CodeActionOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.CodeActionOptions);

   procedure Write_CompletionItemKind
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Enumerations.CompletionItemKind);

   procedure Write_Command_Or_CodeAction
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.Command_Or_CodeAction);

   procedure Write_ChangeAnnotationIdentifier
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.ChangeAnnotationIdentifier);

   procedure Write_SignatureHelpContext
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.SignatureHelpContext);

   procedure Write_CallHierarchyOutgoingCall
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.CallHierarchyOutgoingCall);

   procedure Write_TextDocumentContentChangeEvent
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.TextDocumentContentChangeEvent);

   procedure Write_WorkspaceFolder
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.WorkspaceFolder);

   procedure Write_MarkedString
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.MarkedString);

   procedure Write_NotebookCell
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.NotebookCell);

   procedure Write_WorkspaceFoldersServerCapabilities
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.WorkspaceFoldersServerCapabilities);

   procedure Write_SemanticTokensRegistrationOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.SemanticTokensRegistrationOptions);

   procedure Write_CompletionTriggerKind
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Enumerations.CompletionTriggerKind);

   procedure Write_CallHierarchyRegistrationOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.CallHierarchyRegistrationOptions);

   procedure Write_CallHierarchyClientCapabilities
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.CallHierarchyClientCapabilities);

   procedure Write_DocumentRangeFormattingOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DocumentRangeFormattingOptions);

   procedure Write_InlayHintWorkspaceClientCapabilities
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.InlayHintWorkspaceClientCapabilities);

   procedure Write_DiagnosticServerCancellationData
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DiagnosticServerCancellationData);

   procedure Write_DocumentColorOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DocumentColorOptions);

   procedure Write_FileOperationPatternKind
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Enumerations.FileOperationPatternKind);

   procedure Write_Unregistration
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.Unregistration);

   procedure Write_HoverOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.HoverOptions);

   procedure Write_DocumentOnTypeFormattingRegistrationOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DocumentOnTypeFormattingRegistrationOptions);

   procedure Write_relatedDocuments_OfDocumentDiagnosticReportPartialResult
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures
        .relatedDocuments_OfDocumentDiagnosticReportPartialResult);

   procedure Write_ReferenceClientCapabilities
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.ReferenceClientCapabilities);

   procedure Write_MonikerClientCapabilities
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.MonikerClientCapabilities);

   procedure Write_CompletionItemTag_Set
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.CompletionItemTag_Set);

   procedure Write_ColorInformation
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.ColorInformation);

   procedure Write_A_Range
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.A_Range);

   procedure Write_DocumentColorClientCapabilities
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DocumentColorClientCapabilities);

   procedure Write_InlineValueEvaluatableExpression
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.InlineValueEvaluatableExpression);

   procedure Write_Pattern
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.Pattern);

   procedure Write_DiagnosticTag_Set
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DiagnosticTag_Set);

   procedure Write_NotebookCellArrayChange
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.NotebookCellArrayChange);

   procedure Write_DocumentLinkRegistrationOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DocumentLinkRegistrationOptions);

   procedure Write_DocumentLinkOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DocumentLinkOptions);

   procedure Write_PrepareSupportDefaultBehavior
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Enumerations.PrepareSupportDefaultBehavior);

   procedure Write_TextDocumentIdentifier
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.TextDocumentIdentifier);

   procedure Write_TraceValues
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Enumerations.TraceValues);

   procedure Write_SymbolInformation
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.SymbolInformation);

   procedure Write_FileOperationClientCapabilities
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.FileOperationClientCapabilities);

   procedure Write_DocumentHighlightClientCapabilities
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DocumentHighlightClientCapabilities);

   procedure Write_InlineValueText
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.InlineValueText);

   procedure Write_UnchangedDocumentDiagnosticReport
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.UnchangedDocumentDiagnosticReport);

   procedure Write_SymbolTag
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Enumerations.SymbolTag);

   procedure Write_Registration
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.Registration);

   procedure Write_DocumentRangeFormattingRegistrationOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DocumentRangeFormattingRegistrationOptions);

   procedure Write_RenameFileOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.RenameFileOptions);

   procedure Write_DocumentSymbol
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DocumentSymbol);

   procedure Write_DocumentSymbolClientCapabilities
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DocumentSymbolClientCapabilities);

   procedure Write_InitializeError
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.InitializeError);

   procedure Write_LinkedEditingRangeRegistrationOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.LinkedEditingRangeRegistrationOptions);

   procedure Write_SemanticTokensClientCapabilities
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.SemanticTokensClientCapabilities);

   procedure Write_Hover
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.Hover);

   procedure Write_InlineValueWorkspaceClientCapabilities
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.InlineValueWorkspaceClientCapabilities);

   procedure Write_ResourceOperation
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.ResourceOperation);

   procedure Write_PublishDiagnosticsClientCapabilities
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.PublishDiagnosticsClientCapabilities);

   procedure Write_SemanticTokensOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.SemanticTokensOptions);

   procedure Write_DocumentHighlightOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DocumentHighlightOptions);

   procedure Write_FoldingRangeOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.FoldingRangeOptions);

   procedure Write_Integer_Or_Null
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.Integer_Or_Null);

   procedure Write_TextEdit_Vector
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.TextEdit_Vector);

   procedure Write_ShowMessageRequestClientCapabilities
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.ShowMessageRequestClientCapabilities);

   procedure Write_SemanticTokensEdit_Vector
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.SemanticTokensEdit_Vector);

   procedure Write_Position
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.Position);

   procedure Write_TextDocumentSaveRegistrationOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.TextDocumentSaveRegistrationOptions);

   procedure Write_Command
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.Command);

   procedure Write_TextDocumentFilter
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.TextDocumentFilter);

   procedure Write_ExecuteCommandRegistrationOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.ExecuteCommandRegistrationOptions);

   procedure Write_UniquenessLevel
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Enumerations.UniquenessLevel);

   procedure Write_TextDocumentEdit
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.TextDocumentEdit);

   procedure Write_CompletionItemLabelDetails
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.CompletionItemLabelDetails);

   procedure Write_CompletionOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.CompletionOptions);

   procedure Write_Diagnostic
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.Diagnostic);

   procedure Write_TextDocumentSyncOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.TextDocumentSyncOptions);

   procedure Write_OptionalVersionedTextDocumentIdentifier
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.OptionalVersionedTextDocumentIdentifier);

   procedure Write_SemanticTokensLegend
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.SemanticTokensLegend);

   procedure Write_WorkspaceDocumentDiagnosticReport_Vector
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.WorkspaceDocumentDiagnosticReport_Vector);

   procedure Write_ExecuteCommandOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.ExecuteCommandOptions);

   procedure Write_TextDocumentSyncClientCapabilities
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.TextDocumentSyncClientCapabilities);

   procedure Write_MarkupKind
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Enumerations.MarkupKind);

   procedure Write_AlsReferenceKind_Set
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.AlsReferenceKind_Set);

   procedure Write_LSPErrorCodes
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Enumerations.LSPErrorCodes);

   procedure Write_ReferenceRegistrationOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.ReferenceRegistrationOptions);

   procedure Write_LinkedEditingRangeOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.LinkedEditingRangeOptions);

   procedure Write_ClientCapabilities
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.ClientCapabilities) is
   begin
      Handler.Start_Object;
      if Value.workspace.Is_Set then
         Handler.Key_Name ("workspace");
         Write_WorkspaceClientCapabilities (Handler, Value.workspace.Value);
      end if;
      if Value.textDocument.Is_Set then
         Handler.Key_Name ("textDocument");
         Write_TextDocumentClientCapabilities
           (Handler, Value.textDocument.Value);
      end if;
      if Value.notebookDocument.Is_Set then
         Handler.Key_Name ("notebookDocument");
         Write_NotebookDocumentClientCapabilities
           (Handler, Value.notebookDocument.Value);
      end if;
      if Value.window.Is_Set then
         Handler.Key_Name ("window");
         Write_WindowClientCapabilities (Handler, Value.window.Value);
      end if;
      if Value.general.Is_Set then
         Handler.Key_Name ("general");
         Write_GeneralClientCapabilities (Handler, Value.general.Value);
      end if;
      if not Value.experimental.Is_Empty then
         Handler.Key_Name ("experimental");
         Write_LSPAny (Handler, Value.experimental);
      end if;
      Handler.End_Object;
   end Write_ClientCapabilities;

   procedure Write_FileChangeType
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Enumerations.FileChangeType) is
   begin
      case Value is
         when LSP.Enumerations.Created =>
            Handler.Integer_Value (1);
         when LSP.Enumerations.Changed =>
            Handler.Integer_Value (2);
         when LSP.Enumerations.Deleted =>
            Handler.Integer_Value (3);
      end case;
   end Write_FileChangeType;

   procedure Write_InitializeParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.InitializeParams) is
      procedure Write_trace_Of_InitializeParams
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.trace_Of_InitializeParams);

      procedure Write_Virtual_String_Or_Null
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.Virtual_String_Or_Null);

      procedure Write_DocumentUri_Or_Null
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.DocumentUri_Or_Null);

      procedure Write_trace_Of_InitializeParams
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.trace_Of_InitializeParams) is
      begin
         case Value is
            when LSP.Structures.off =>
               Handler.String_Value ("off");
            when LSP.Structures.messages =>
               Handler.String_Value ("messages");
            when LSP.Structures.compact =>
               Handler.String_Value ("compact");
            when LSP.Structures.verbose =>
               Handler.String_Value ("verbose");
         end case;
      end Write_trace_Of_InitializeParams;

      procedure Write_Virtual_String_Or_Null
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.Virtual_String_Or_Null) is
      begin
         if Value.Is_Null then
            Handler.Null_Value;
         else
            Handler.String_Value (Value.Value);
         end if;
      end Write_Virtual_String_Or_Null;

      procedure Write_DocumentUri_Or_Null
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.DocumentUri_Or_Null) is
      begin
         if Value.Is_Null then
            Handler.Null_Value;
         else
            Handler.String_Value (Value.Value);
         end if;
      end Write_DocumentUri_Or_Null;

   begin
      Handler.Start_Object;
      if Value.workDoneToken.Is_Set then
         Handler.Key_Name ("workDoneToken");
         Write_ProgressToken (Handler, Value.workDoneToken.Value);
      end if;
      Handler.Key_Name ("processId");
      Write_Integer_Or_Null (Handler, Value.processId);
      if Value.clientInfo.Is_Set then
         Handler.Key_Name ("clientInfo");
         Write_clientInfo_Of_InitializeParams
           (Handler, Value.clientInfo.Value);
      end if;
      if not Value.locale.Is_Null then
         Handler.Key_Name ("locale");
         Handler.String_Value (Value.locale);
      end if;
      if Value.rootPath.Is_Set then
         Handler.Key_Name ("rootPath");
         Write_Virtual_String_Or_Null (Handler, Value.rootPath.Value);
      end if;
      Handler.Key_Name ("rootUri");
      Write_DocumentUri_Or_Null (Handler, Value.rootUri);
      Handler.Key_Name ("capabilities");
      Write_ClientCapabilities (Handler, Value.capabilities);
      if not Value.initializationOptions.Is_Empty then
         Handler.Key_Name ("initializationOptions");
         Write_LSPAny (Handler, Value.initializationOptions);
      end if;
      if Value.trace.Is_Set then
         Handler.Key_Name ("trace");
         Write_trace_Of_InitializeParams (Handler, Value.trace.Value);
      end if;
      if Value.Parent.workspaceFolders.Is_Set then
         Handler.Key_Name ("workspaceFolders");
         Write_WorkspaceFolder_Vector_Or_Null
           (Handler, Value.Parent.workspaceFolders.Value);
      end if;
      Handler.End_Object;
   end Write_InitializeParams;

   procedure Write_DocumentLink_Vector_Or_Null
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DocumentLink_Vector_Or_Null) is
   begin
      if Value.Is_Empty then
         Handler.Null_Value;
      else
         Write_DocumentLink_Vector (Handler, Value);
      end if;
   end Write_DocumentLink_Vector_Or_Null;

   procedure Write_Command_Or_CodeAction_Vector
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.Command_Or_CodeAction_Vector) is
   begin
      Handler.Start_Array;
      for J in Value.First_Index .. Value.Last_Index loop
         Write_Command_Or_CodeAction (Handler, Value (J));
      end loop;
      Handler.End_Array;
   end Write_Command_Or_CodeAction_Vector;

   procedure Write_Virtual_String
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.Virtual_String) is
   begin
      Handler.String_Value (Value);
   end Write_Virtual_String;

   procedure Write_Declaration
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.Declaration) is
   begin
      if Value.Length = 1 then
         Write_Location (Handler, Value (1));

      else
         Handler.Start_Array;
         for J in 1 .. Value.Last_Index loop
            Write_Location (Handler, Value (J));

         end loop;
         Handler.End_Array;
      end if;
   end Write_Declaration;

   procedure Write_Null_Record
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.Null_Record) is
   begin
      Handler.Null_Value;
   end Write_Null_Record;

   procedure Write_FileOperationRegistrationOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.FileOperationRegistrationOptions) is
      procedure Write_FileOperationFilter_Vector
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.FileOperationFilter_Vector);

      procedure Write_FileOperationFilter_Vector
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.FileOperationFilter_Vector) is
      begin
         Handler.Start_Array;
         for J in Value.First_Index .. Value.Last_Index loop
            Write_FileOperationFilter (Handler, Value (J));
         end loop;
         Handler.End_Array;
      end Write_FileOperationFilter_Vector;

   begin
      Handler.Start_Object;
      Handler.Key_Name ("filters");
      Write_FileOperationFilter_Vector (Handler, Value.filters);
      Handler.End_Object;
   end Write_FileOperationRegistrationOptions;

   procedure Write_CallHierarchyOutgoingCall_Vector
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.CallHierarchyOutgoingCall_Vector) is
   begin
      Handler.Start_Array;
      for J in Value.First_Index .. Value.Last_Index loop
         Write_CallHierarchyOutgoingCall (Handler, Value (J));
      end loop;
      Handler.End_Array;
   end Write_CallHierarchyOutgoingCall_Vector;

   procedure Write_WorkspaceFolder_Vector
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.WorkspaceFolder_Vector) is
   begin
      Handler.Start_Array;
      for J in Value.First_Index .. Value.Last_Index loop
         Write_WorkspaceFolder (Handler, Value (J));
      end loop;
      Handler.End_Array;
   end Write_WorkspaceFolder_Vector;

   procedure Write_TextDocumentContentChangeEvent_Vector
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.TextDocumentContentChangeEvent_Vector) is
   begin
      Handler.Start_Array;
      for J in Value.First_Index .. Value.Last_Index loop
         Write_TextDocumentContentChangeEvent (Handler, Value (J));
      end loop;
      Handler.End_Array;
   end Write_TextDocumentContentChangeEvent_Vector;

   procedure Write_FoldingRangeClientCapabilities
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.FoldingRangeClientCapabilities) is
      procedure Write_foldingRange_OfFoldingRangeClientCapabilities
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value : LSP.Structures.foldingRange_OfFoldingRangeClientCapabilities);

      procedure Write_FoldingRangeKind_Set
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.FoldingRangeKind_Set);

      procedure Write_foldingRangeKind_OfFoldingRangeClientCapabilities
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures
           .foldingRangeKind_OfFoldingRangeClientCapabilities);

      procedure Write_foldingRange_OfFoldingRangeClientCapabilities
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures
           .foldingRange_OfFoldingRangeClientCapabilities) is
      begin
         Handler.Start_Object;
         if Value.collapsedText.Is_Set then
            Handler.Key_Name ("collapsedText");
            Handler.Boolean_Value (Value.collapsedText.Value);
         end if;
         Handler.End_Object;
      end Write_foldingRange_OfFoldingRangeClientCapabilities;

      procedure Write_FoldingRangeKind_Set
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.FoldingRangeKind_Set) is
      begin
         Handler.Start_Array;
         declare
            Set : LSP.Structures.FoldingRangeKind_Set renames Value;
         begin
            for Value in Set'Range loop
               if Set (Value) then
                  Write_FoldingRangeKind (Handler, Value);
               end if;
            end loop;
         end;
         Handler.End_Array;
      end Write_FoldingRangeKind_Set;

      procedure Write_foldingRangeKind_OfFoldingRangeClientCapabilities
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures
           .foldingRangeKind_OfFoldingRangeClientCapabilities) is
      begin
         Handler.Start_Object;
         if (for some Item of Value.valueSet => Item) then
            Handler.Key_Name ("valueSet");
            Write_FoldingRangeKind_Set (Handler, Value.valueSet);
         end if;
         Handler.End_Object;
      end Write_foldingRangeKind_OfFoldingRangeClientCapabilities;

   begin
      Handler.Start_Object;
      if Value.dynamicRegistration.Is_Set then
         Handler.Key_Name ("dynamicRegistration");
         Handler.Boolean_Value (Value.dynamicRegistration.Value);
      end if;
      if Value.rangeLimit.Is_Set then
         Handler.Key_Name ("rangeLimit");
         Handler.Integer_Value (Integer'Pos (Value.rangeLimit.Value));
      end if;
      if Value.lineFoldingOnly.Is_Set then
         Handler.Key_Name ("lineFoldingOnly");
         Handler.Boolean_Value (Value.lineFoldingOnly.Value);
      end if;
      if Value.foldingRangeKind.Is_Set then
         Handler.Key_Name ("foldingRangeKind");
         Write_foldingRangeKind_OfFoldingRangeClientCapabilities
           (Handler, Value.foldingRangeKind.Value);
      end if;
      if Value.foldingRange.Is_Set then
         Handler.Key_Name ("foldingRange");
         Write_foldingRange_OfFoldingRangeClientCapabilities
           (Handler, Value.foldingRange.Value);
      end if;
      Handler.End_Object;
   end Write_FoldingRangeClientCapabilities;

   procedure Write_CreateFile
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.CreateFile) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("kind");
      Handler.String_Value ("create");
      if Value.annotationId.Is_Set then
         Handler.Key_Name ("annotationId");
         Write_ChangeAnnotationIdentifier (Handler, Value.annotationId.Value);
      end if;
      Handler.Key_Name ("uri");
      Handler.String_Value (Value.uri);
      if Value.options.Is_Set then
         Handler.Key_Name ("options");
         Write_CreateFileOptions (Handler, Value.options.Value);
      end if;
      Handler.End_Object;
   end Write_CreateFile;

   procedure Write_CompletionContext
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.CompletionContext) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("triggerKind");
      Write_CompletionTriggerKind (Handler, Value.triggerKind);
      if not Value.triggerCharacter.Is_Null then
         Handler.Key_Name ("triggerCharacter");
         Handler.String_Value (Value.triggerCharacter);
      end if;
      Handler.End_Object;
   end Write_CompletionContext;

   procedure Write_NotebookCell_Vector
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.NotebookCell_Vector) is
   begin
      Handler.Start_Array;
      for J in Value.First_Index .. Value.Last_Index loop
         Write_NotebookCell (Handler, Value (J));
      end loop;
      Handler.End_Array;
   end Write_NotebookCell_Vector;

   procedure Write_DocumentDiagnosticParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DocumentDiagnosticParams) is
   begin
      Handler.Start_Object;
      if Value.workDoneToken.Is_Set then
         Handler.Key_Name ("workDoneToken");
         Write_ProgressToken (Handler, Value.workDoneToken.Value);
      end if;
      if Value.partialResultToken.Is_Set then
         Handler.Key_Name ("partialResultToken");
         Write_ProgressToken (Handler, Value.partialResultToken.Value);
      end if;
      Handler.Key_Name ("textDocument");
      Write_TextDocumentIdentifier (Handler, Value.textDocument);
      if not Value.identifier.Is_Null then
         Handler.Key_Name ("identifier");
         Handler.String_Value (Value.identifier);
      end if;
      if not Value.previousResultId.Is_Null then
         Handler.Key_Name ("previousResultId");
         Handler.String_Value (Value.previousResultId);
      end if;
      Handler.End_Object;
   end Write_DocumentDiagnosticParams;

   procedure Write_DocumentLink_Vector
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DocumentLink_Vector) is
   begin
      Handler.Start_Array;
      for J in Value.First_Index .. Value.Last_Index loop
         Write_DocumentLink (Handler, Value (J));
      end loop;
      Handler.End_Array;
   end Write_DocumentLink_Vector;

   procedure Write_CodeActionKind
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Enumerations.CodeActionKind) is
   begin
      case Value is
         when LSP.Enumerations.Empty =>
            Handler.String_Value ("");
         when LSP.Enumerations.QuickFix =>
            Handler.String_Value ("quickfix");
         when LSP.Enumerations.Refactor =>
            Handler.String_Value ("refactor");
         when LSP.Enumerations.RefactorExtract =>
            Handler.String_Value ("refactor.extract");
         when LSP.Enumerations.RefactorInline =>
            Handler.String_Value ("refactor.inline");
         when LSP.Enumerations.RefactorRewrite =>
            Handler.String_Value ("refactor.rewrite");
         when LSP.Enumerations.Source =>
            Handler.String_Value ("source");
         when LSP.Enumerations.SourceOrganizeImports =>
            Handler.String_Value ("source.organizeImports");
         when LSP.Enumerations.SourceFixAll =>
            Handler.String_Value ("source.fixAll");
      end case;
   end Write_CodeActionKind;

   procedure Write_RelatedUnchangedDocumentDiagnosticReport
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.RelatedUnchangedDocumentDiagnosticReport) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("kind");
      Handler.String_Value ("unchanged");
      Handler.Key_Name ("resultId");
      Handler.String_Value (Value.resultId);
      if not Value.relatedDocuments.Is_Empty then
         Handler.Key_Name ("relatedDocuments");
         declare
            use
              LSP.Structures
                .relatedDocuments_OfDocumentDiagnosticReportPartialResult_Item_Maps;
         begin
            Handler.Start_Object;
            for Cursor in Value.relatedDocuments.Iterate loop
               Handler.Key_Name (Key (Cursor));
               Write_relatedDocuments_OfDocumentDiagnosticReportPartialResult_Item
                 (Handler, Value.relatedDocuments (Cursor));
            end loop;
            Handler.End_Object;
         end;
      end if;
      Handler.End_Object;
   end Write_RelatedUnchangedDocumentDiagnosticReport;

   procedure Write_DidChangeWatchedFilesClientCapabilities
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DidChangeWatchedFilesClientCapabilities) is
   begin
      Handler.Start_Object;
      if Value.dynamicRegistration.Is_Set then
         Handler.Key_Name ("dynamicRegistration");
         Handler.Boolean_Value (Value.dynamicRegistration.Value);
      end if;
      if Value.relativePatternSupport.Is_Set then
         Handler.Key_Name ("relativePatternSupport");
         Handler.Boolean_Value (Value.relativePatternSupport.Value);
      end if;
      Handler.End_Object;
   end Write_DidChangeWatchedFilesClientCapabilities;

   procedure Write_SemanticTokenTypes
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Enumerations.SemanticTokenTypes) is
   begin
      case Value is
         when LSP.Enumerations.namespace =>
            Handler.String_Value ("namespace");
         when LSP.Enumerations.a_type =>
            Handler.String_Value ("type");
         when LSP.Enumerations.class =>
            Handler.String_Value ("class");
         when LSP.Enumerations.enum =>
            Handler.String_Value ("enum");
         when LSP.Enumerations.an_interface =>
            Handler.String_Value ("interface");
         when LSP.Enumerations.struct =>
            Handler.String_Value ("struct");
         when LSP.Enumerations.typeParameter =>
            Handler.String_Value ("typeParameter");
         when LSP.Enumerations.parameter =>
            Handler.String_Value ("parameter");
         when LSP.Enumerations.variable =>
            Handler.String_Value ("variable");
         when LSP.Enumerations.property =>
            Handler.String_Value ("property");
         when LSP.Enumerations.enumMember =>
            Handler.String_Value ("enumMember");
         when LSP.Enumerations.event =>
            Handler.String_Value ("event");
         when LSP.Enumerations.a_function =>
            Handler.String_Value ("function");
         when LSP.Enumerations.method =>
            Handler.String_Value ("method");
         when LSP.Enumerations.macro =>
            Handler.String_Value ("macro");
         when LSP.Enumerations.keyword =>
            Handler.String_Value ("keyword");
         when LSP.Enumerations.modifier =>
            Handler.String_Value ("modifier");
         when LSP.Enumerations.comment =>
            Handler.String_Value ("comment");
         when LSP.Enumerations.string =>
            Handler.String_Value ("string");
         when LSP.Enumerations.number =>
            Handler.String_Value ("number");
         when LSP.Enumerations.regexp =>
            Handler.String_Value ("regexp");
         when LSP.Enumerations.operator =>
            Handler.String_Value ("operator");
         when LSP.Enumerations.decorator =>
            Handler.String_Value ("decorator");
      end case;
   end Write_SemanticTokenTypes;

   procedure Write_AnnotatedTextEdit
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.AnnotatedTextEdit) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("range");
      Write_A_Range (Handler, Value.a_range);
      Handler.Key_Name ("newText");
      Handler.String_Value (Value.newText);
      Handler.Key_Name ("annotationId");
      Write_ChangeAnnotationIdentifier (Handler, Value.annotationId);
      Handler.End_Object;
   end Write_AnnotatedTextEdit;

   procedure Write_DeclarationOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DeclarationOptions) is
   begin
      Handler.Start_Object;
      if Value.workDoneProgress.Is_Set then
         Handler.Key_Name ("workDoneProgress");
         Handler.Boolean_Value (Value.workDoneProgress.Value);
      end if;
      Handler.End_Object;
   end Write_DeclarationOptions;

   procedure Write_SignatureInformation
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.SignatureInformation) is
      procedure Write_ParameterInformation_Vector
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.ParameterInformation_Vector);

      procedure Write_ParameterInformation_Vector
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.ParameterInformation_Vector) is
      begin
         Handler.Start_Array;
         for J in Value.First_Index .. Value.Last_Index loop
            Write_ParameterInformation (Handler, Value (J));
         end loop;
         Handler.End_Array;
      end Write_ParameterInformation_Vector;

   begin
      Handler.Start_Object;
      Handler.Key_Name ("label");
      Handler.String_Value (Value.label);
      if Value.documentation.Is_Set then
         Handler.Key_Name ("documentation");
         Write_Virtual_String_Or_MarkupContent
           (Handler, Value.documentation.Value);
      end if;
      if not Value.parameters.Is_Empty then
         Handler.Key_Name ("parameters");
         Write_ParameterInformation_Vector (Handler, Value.parameters);
      end if;
      if Value.activeParameter.Is_Set then
         Handler.Key_Name ("activeParameter");
         Handler.Integer_Value (Integer'Pos (Value.activeParameter.Value));
      end if;
      Handler.End_Object;
   end Write_SignatureInformation;

   procedure Write_TextDocumentSaveReason
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Enumerations.TextDocumentSaveReason) is
   begin
      case Value is
         when LSP.Enumerations.Manual =>
            Handler.Integer_Value (1);
         when LSP.Enumerations.AfterDelay =>
            Handler.Integer_Value (2);
         when LSP.Enumerations.FocusOut =>
            Handler.Integer_Value (3);
      end case;
   end Write_TextDocumentSaveReason;

   procedure Write_SemanticTokens
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.SemanticTokens) is
   begin
      Handler.Start_Object;
      if not Value.resultId.Is_Null then
         Handler.Key_Name ("resultId");
         Handler.String_Value (Value.resultId);
      end if;
      Handler.Key_Name ("data");
      Write_Natural_Vector (Handler, Value.data);
      Handler.End_Object;
   end Write_SemanticTokens;

   procedure Write_InlineValueParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.InlineValueParams) is
   begin
      Handler.Start_Object;
      if Value.workDoneToken.Is_Set then
         Handler.Key_Name ("workDoneToken");
         Write_ProgressToken (Handler, Value.workDoneToken.Value);
      end if;
      Handler.Key_Name ("textDocument");
      Write_TextDocumentIdentifier (Handler, Value.textDocument);
      Handler.Key_Name ("range");
      Write_A_Range (Handler, Value.a_range);
      Handler.Key_Name ("context");
      Write_InlineValueContext (Handler, Value.context);
      Handler.End_Object;
   end Write_InlineValueParams;

   procedure Write_TokenFormat
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Enumerations.TokenFormat) is
   begin
      case Value is
         when LSP.Enumerations.Relative =>
            Handler.String_Value ("relative");
      end case;
   end Write_TokenFormat;

   procedure Write_TextDocumentRegistrationOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.TextDocumentRegistrationOptions) is
      procedure Write_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.DocumentSelector_Or_Null);

      procedure Write_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.DocumentSelector_Or_Null) is
      begin
         if Value.Is_Null then
            Handler.Null_Value;
         else
            Write_DocumentSelector (Handler, Value.Value);
         end if;
      end Write_DocumentSelector_Or_Null;

   begin
      Handler.Start_Object;
      Handler.Key_Name ("documentSelector");
      Write_DocumentSelector_Or_Null (Handler, Value.documentSelector);
      Handler.End_Object;
   end Write_TextDocumentRegistrationOptions;

   procedure Write_URI
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.URI) is
   begin
      Handler.String_Value (Value);
   end Write_URI;

   procedure Write_SemanticTokenModifiers
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Enumerations.SemanticTokenModifiers) is
   begin
      case Value is
         when LSP.Enumerations.declaration =>
            Handler.String_Value ("declaration");
         when LSP.Enumerations.definition =>
            Handler.String_Value ("definition");
         when LSP.Enumerations.readonly =>
            Handler.String_Value ("readonly");
         when LSP.Enumerations.static =>
            Handler.String_Value ("static");
         when LSP.Enumerations.deprecated =>
            Handler.String_Value ("deprecated");
         when LSP.Enumerations.an_abstract =>
            Handler.String_Value ("abstract");
         when LSP.Enumerations.async =>
            Handler.String_Value ("async");
         when LSP.Enumerations.modification =>
            Handler.String_Value ("modification");
         when LSP.Enumerations.documentation =>
            Handler.String_Value ("documentation");
         when LSP.Enumerations.defaultLibrary =>
            Handler.String_Value ("defaultLibrary");
      end case;
   end Write_SemanticTokenModifiers;

   procedure Write_ConfigurationItem
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.ConfigurationItem) is
   begin
      Handler.Start_Object;
      if not Value.scopeUri.Is_Null then
         Handler.Key_Name ("scopeUri");
         Handler.String_Value (Value.scopeUri);
      end if;
      if not Value.section.Is_Null then
         Handler.Key_Name ("section");
         Handler.String_Value (Value.section);
      end if;
      Handler.End_Object;
   end Write_ConfigurationItem;

   procedure Write_DocumentOnTypeFormattingClientCapabilities
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DocumentOnTypeFormattingClientCapabilities) is
   begin
      Handler.Start_Object;
      if Value.dynamicRegistration.Is_Set then
         Handler.Key_Name ("dynamicRegistration");
         Handler.Boolean_Value (Value.dynamicRegistration.Value);
      end if;
      Handler.End_Object;
   end Write_DocumentOnTypeFormattingClientCapabilities;

   procedure Write_SymbolKind
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Enumerations.SymbolKind) is
   begin
      case Value is
         when LSP.Enumerations.File =>
            Handler.Integer_Value (1);
         when LSP.Enumerations.Module =>
            Handler.Integer_Value (2);
         when LSP.Enumerations.Namespace =>
            Handler.Integer_Value (3);
         when LSP.Enumerations.A_Package =>
            Handler.Integer_Value (4);
         when LSP.Enumerations.Class =>
            Handler.Integer_Value (5);
         when LSP.Enumerations.Method =>
            Handler.Integer_Value (6);
         when LSP.Enumerations.Property =>
            Handler.Integer_Value (7);
         when LSP.Enumerations.Field =>
            Handler.Integer_Value (8);
         when LSP.Enumerations.Constructor =>
            Handler.Integer_Value (9);
         when LSP.Enumerations.Enum =>
            Handler.Integer_Value (10);
         when LSP.Enumerations.An_Interface =>
            Handler.Integer_Value (11);
         when LSP.Enumerations.A_Function =>
            Handler.Integer_Value (12);
         when LSP.Enumerations.Variable =>
            Handler.Integer_Value (13);
         when LSP.Enumerations.A_Constant =>
            Handler.Integer_Value (14);
         when LSP.Enumerations.String =>
            Handler.Integer_Value (15);
         when LSP.Enumerations.Number =>
            Handler.Integer_Value (16);
         when LSP.Enumerations.Boolean =>
            Handler.Integer_Value (17);
         when LSP.Enumerations.An_Array =>
            Handler.Integer_Value (18);
         when LSP.Enumerations.Object =>
            Handler.Integer_Value (19);
         when LSP.Enumerations.Key =>
            Handler.Integer_Value (20);
         when LSP.Enumerations.A_Null =>
            Handler.Integer_Value (21);
         when LSP.Enumerations.EnumMember =>
            Handler.Integer_Value (22);
         when LSP.Enumerations.Struct =>
            Handler.Integer_Value (23);
         when LSP.Enumerations.Event =>
            Handler.Integer_Value (24);
         when LSP.Enumerations.Operator =>
            Handler.Integer_Value (25);
         when LSP.Enumerations.TypeParameter =>
            Handler.Integer_Value (26);
      end case;
   end Write_SymbolKind;

   procedure Write_DidSaveNotebookDocumentParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DidSaveNotebookDocumentParams) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("notebookDocument");
      Write_NotebookDocumentIdentifier (Handler, Value.notebookDocument);
      Handler.End_Object;
   end Write_DidSaveNotebookDocumentParams;

   procedure Write_Declaration_Result
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.Declaration_Result) is
   begin
      case Value.Kind is
         when LSP.Structures.Variant_1 =>
            Write_Declaration (Handler, Value.Variant_1);
         when LSP.Structures.Variant_2 =>
            Write_DeclarationLink_Vector (Handler, Value.Variant_2);
         when LSP.Structures.Variant_3 =>
            Handler.Null_Value;
      end case;
   end Write_Declaration_Result;

   procedure Write_Definition_Progress_Report
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.Definition_Progress_Report) is
   begin
      case Value.Kind is
         when LSP.Structures.Variant_1 =>
            Write_Location_Vector (Handler, Value.Variant_1);
         when LSP.Structures.Variant_2 =>
            Write_DefinitionLink_Vector (Handler, Value.Variant_2);
      end case;
   end Write_Definition_Progress_Report;

   procedure Write_SemanticTokensParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.SemanticTokensParams) is
   begin
      Handler.Start_Object;
      if Value.workDoneToken.Is_Set then
         Handler.Key_Name ("workDoneToken");
         Write_ProgressToken (Handler, Value.workDoneToken.Value);
      end if;
      if Value.partialResultToken.Is_Set then
         Handler.Key_Name ("partialResultToken");
         Write_ProgressToken (Handler, Value.partialResultToken.Value);
      end if;
      Handler.Key_Name ("textDocument");
      Write_TextDocumentIdentifier (Handler, Value.textDocument);
      Handler.End_Object;
   end Write_SemanticTokensParams;

   procedure Write_ColorInformation_Vector
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.ColorInformation_Vector) is
   begin
      Handler.Start_Array;
      for J in Value.First_Index .. Value.Last_Index loop
         Write_ColorInformation (Handler, Value (J));
      end loop;
      Handler.End_Array;
   end Write_ColorInformation_Vector;

   procedure Write_SelectionRangeClientCapabilities
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.SelectionRangeClientCapabilities) is
   begin
      Handler.Start_Object;
      if Value.dynamicRegistration.Is_Set then
         Handler.Key_Name ("dynamicRegistration");
         Handler.Boolean_Value (Value.dynamicRegistration.Value);
      end if;
      Handler.End_Object;
   end Write_SelectionRangeClientCapabilities;

   procedure Write_DocumentSymbol_Progress_Report
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DocumentSymbol_Progress_Report) is
   begin
      case Value.Kind is
         when LSP.Structures.Variant_1 =>
            Write_SymbolInformation_Vector (Handler, Value.Variant_1);
         when LSP.Structures.Variant_2 =>
            Write_DocumentSymbol_Vector (Handler, Value.Variant_2);
      end case;
   end Write_DocumentSymbol_Progress_Report;

   procedure Write_CallHierarchyIncomingCall_Vector_Or_Null
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.CallHierarchyIncomingCall_Vector_Or_Null) is
   begin
      if Value.Is_Empty then
         Handler.Null_Value;
      else
         Write_CallHierarchyIncomingCall_Vector (Handler, Value);
      end if;
   end Write_CallHierarchyIncomingCall_Vector_Or_Null;

   procedure Write_Range_Vector
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.Range_Vector) is
   begin
      Handler.Start_Array;
      for J in Value.First_Index .. Value.Last_Index loop
         Write_A_Range (Handler, Value (J));
      end loop;
      Handler.End_Array;
   end Write_Range_Vector;

   procedure Write_Definition
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.Definition) is
   begin
      if Value.Length = 1 then
         Write_Location (Handler, Value (1));

      else
         Handler.Start_Array;
         for J in 1 .. Value.Last_Index loop
            Write_Location (Handler, Value (J));

         end loop;
         Handler.End_Array;
      end if;
   end Write_Definition;

   procedure Write_NotebookDocumentFilter
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.NotebookDocumentFilter) is
   begin
      Handler.Start_Object;
      if not Value.notebookType.Is_Null then
         Handler.Key_Name ("notebookType");
         Handler.String_Value (Value.notebookType);
      end if;
      if not Value.scheme.Is_Null then
         Handler.Key_Name ("scheme");
         Handler.String_Value (Value.scheme);
      end if;
      if not Value.pattern.Is_Null then
         Handler.Key_Name ("pattern");
         Handler.String_Value (Value.pattern);
      end if;
      Handler.End_Object;
   end Write_NotebookDocumentFilter;

   procedure Write_Color
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.Color) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("red");
      Handler.Float_Value (Interfaces.IEEE_Float_64 (Value.red));
      Handler.Key_Name ("green");
      Handler.Float_Value (Interfaces.IEEE_Float_64 (Value.green));
      Handler.Key_Name ("blue");
      Handler.Float_Value (Interfaces.IEEE_Float_64 (Value.blue));
      Handler.Key_Name ("alpha");
      Handler.Float_Value (Interfaces.IEEE_Float_64 (Value.alpha));
      Handler.End_Object;
   end Write_Color;

   procedure Write_FileOperationPattern
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.FileOperationPattern) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("glob");
      Handler.String_Value (Value.glob);
      if Value.matches.Is_Set then
         Handler.Key_Name ("matches");
         Write_FileOperationPatternKind (Handler, Value.matches.Value);
      end if;
      if Value.options.Is_Set then
         Handler.Key_Name ("options");
         Write_FileOperationPatternOptions (Handler, Value.options.Value);
      end if;
      Handler.End_Object;
   end Write_FileOperationPattern;

   procedure Write_LinkedEditingRanges
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.LinkedEditingRanges) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("ranges");
      Write_Range_Vector (Handler, Value.ranges);
      if not Value.wordPattern.Is_Null then
         Handler.Key_Name ("wordPattern");
         Handler.String_Value (Value.wordPattern);
      end if;
      Handler.End_Object;
   end Write_LinkedEditingRanges;

   procedure Write_Virtual_String_Or_MarkupContent
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.Virtual_String_Or_MarkupContent) is
   begin
      case Value.Is_Virtual_String is
         when True =>
            Handler.String_Value (Value.Virtual_String);
         when False =>
            Write_MarkupContent (Handler, Value.MarkupContent);
      end case;
   end Write_Virtual_String_Or_MarkupContent;

   procedure Write_CodeLens_Vector
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.CodeLens_Vector) is
   begin
      Handler.Start_Array;
      for J in Value.First_Index .. Value.Last_Index loop
         Write_CodeLens (Handler, Value (J));
      end loop;
      Handler.End_Array;
   end Write_CodeLens_Vector;

   procedure Write_ServerCapabilities
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.ServerCapabilities) is
      procedure Write_Boolean_Or_DocumentSymbolOptions
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.Boolean_Or_DocumentSymbolOptions);

      procedure Write_NotebookDocumentSyncOptions_Or_NotebookDocumentSyncRegistrationOptions
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures
           .NotebookDocumentSyncOptions_Or_NotebookDocumentSyncRegistrationOptions);

      procedure Write_Boolean_Or_WorkspaceSymbolOptions
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.Boolean_Or_WorkspaceSymbolOptions);

      procedure Write_implementationProvider_OfServerCapabilities
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.implementationProvider_OfServerCapabilities);

      procedure Write_typeHierarchyProvider_OfServerCapabilities
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.typeHierarchyProvider_OfServerCapabilities);

      procedure Write_DiagnosticOptions_Or_DiagnosticRegistrationOptions
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures
           .DiagnosticOptions_Or_DiagnosticRegistrationOptions);

      procedure Write_inlayHintProvider_OfServerCapabilities
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.inlayHintProvider_OfServerCapabilities);

      procedure Write_SemanticTokensOptions_Or_SemanticTokensRegistrationOptions
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures
           .SemanticTokensOptions_Or_SemanticTokensRegistrationOptions);

      procedure Write_monikerProvider_OfServerCapabilities
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.monikerProvider_OfServerCapabilities);

      procedure Write_Boolean_Or_CodeActionOptions
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.Boolean_Or_CodeActionOptions);

      procedure Write_selectionRangeProvider_OfServerCapabilities
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.selectionRangeProvider_OfServerCapabilities);

      procedure Write_Boolean_Or_DocumentRangeFormattingOptions
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.Boolean_Or_DocumentRangeFormattingOptions);

      procedure Write_Boolean_Or_HoverOptions
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.Boolean_Or_HoverOptions);

      procedure Write_workspace_OfServerCapabilities
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.workspace_OfServerCapabilities);

      procedure Write_declarationProvider_OfServerCapabilities
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.declarationProvider_OfServerCapabilities);

      procedure Write_callHierarchyProvider_OfServerCapabilities
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.callHierarchyProvider_OfServerCapabilities);

      procedure Write_Boolean_Or_DocumentHighlightOptions
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.Boolean_Or_DocumentHighlightOptions);

      procedure Write_TextDocumentSyncOptions_Or_TextDocumentSyncKind
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures
           .TextDocumentSyncOptions_Or_TextDocumentSyncKind);

      procedure Write_linkedEditingRangeProvider_OfServerCapabilities
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures
           .linkedEditingRangeProvider_OfServerCapabilities);

      procedure Write_Boolean_Or_DocumentFormattingOptions
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.Boolean_Or_DocumentFormattingOptions);

      procedure Write_Boolean_Or_ReferenceOptions
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.Boolean_Or_ReferenceOptions);

      procedure Write_foldingRangeProvider_OfServerCapabilities
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.foldingRangeProvider_OfServerCapabilities);

      procedure Write_typeDefinitionProvider_OfServerCapabilities
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.typeDefinitionProvider_OfServerCapabilities);

      procedure Write_inlineValueProvider_OfServerCapabilities
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.inlineValueProvider_OfServerCapabilities);

      procedure Write_colorProvider_OfServerCapabilities
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.colorProvider_OfServerCapabilities);

      procedure Write_Boolean_Or_DefinitionOptions
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.Boolean_Or_DefinitionOptions);

      procedure Write_Boolean_Or_RenameOptions
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.Boolean_Or_RenameOptions);

      procedure Write_Boolean_Or_DocumentSymbolOptions
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.Boolean_Or_DocumentSymbolOptions) is
      begin
         case Value.Is_Boolean is
            when True =>
               Handler.Boolean_Value (Value.Boolean);
            when False =>
               Write_DocumentSymbolOptions
                 (Handler, Value.DocumentSymbolOptions);
         end case;
      end Write_Boolean_Or_DocumentSymbolOptions;

      procedure Write_NotebookDocumentSyncOptions_Or_NotebookDocumentSyncRegistrationOptions
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures
           .NotebookDocumentSyncOptions_Or_NotebookDocumentSyncRegistrationOptions) is
      begin
         case Value.Is_NotebookDocumentSyncOptions is
            when True =>
               Write_NotebookDocumentSyncOptions
                 (Handler, Value.NotebookDocumentSyncOptions);
            when False =>
               Write_NotebookDocumentSyncRegistrationOptions
                 (Handler, Value.NotebookDocumentSyncRegistrationOptions);
         end case;
      end Write_NotebookDocumentSyncOptions_Or_NotebookDocumentSyncRegistrationOptions;

      procedure Write_Boolean_Or_WorkspaceSymbolOptions
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.Boolean_Or_WorkspaceSymbolOptions) is
      begin
         case Value.Is_Boolean is
            when True =>
               Handler.Boolean_Value (Value.Boolean);
            when False =>
               Write_WorkspaceSymbolOptions
                 (Handler, Value.WorkspaceSymbolOptions);
         end case;
      end Write_Boolean_Or_WorkspaceSymbolOptions;

      procedure Write_implementationProvider_OfServerCapabilities
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value : LSP.Structures.implementationProvider_OfServerCapabilities) is
      begin
         case Value.Kind is
            when LSP.Structures.Variant_1 =>
               Handler.Boolean_Value (Value.Variant_1);
            when LSP.Structures.Variant_2 =>
               Write_ImplementationOptions (Handler, Value.Variant_2);
            when LSP.Structures.Variant_3 =>
               Write_ImplementationRegistrationOptions
                 (Handler, Value.Variant_3);
         end case;
      end Write_implementationProvider_OfServerCapabilities;

      procedure Write_typeHierarchyProvider_OfServerCapabilities
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value : LSP.Structures.typeHierarchyProvider_OfServerCapabilities) is
      begin
         case Value.Kind is
            when LSP.Structures.Variant_1 =>
               Handler.Boolean_Value (Value.Variant_1);
            when LSP.Structures.Variant_2 =>
               Write_TypeHierarchyOptions (Handler, Value.Variant_2);
            when LSP.Structures.Variant_3 =>
               Write_TypeHierarchyRegistrationOptions
                 (Handler, Value.Variant_3);
         end case;
      end Write_typeHierarchyProvider_OfServerCapabilities;

      procedure Write_DiagnosticOptions_Or_DiagnosticRegistrationOptions
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures
           .DiagnosticOptions_Or_DiagnosticRegistrationOptions) is
      begin
         case Value.Is_DiagnosticOptions is
            when True =>
               Write_DiagnosticOptions (Handler, Value.DiagnosticOptions);
            when False =>
               Write_DiagnosticRegistrationOptions
                 (Handler, Value.DiagnosticRegistrationOptions);
         end case;
      end Write_DiagnosticOptions_Or_DiagnosticRegistrationOptions;

      procedure Write_inlayHintProvider_OfServerCapabilities
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.inlayHintProvider_OfServerCapabilities) is
      begin
         case Value.Kind is
            when LSP.Structures.Variant_1 =>
               Handler.Boolean_Value (Value.Variant_1);
            when LSP.Structures.Variant_2 =>
               Write_InlayHintOptions (Handler, Value.Variant_2);
            when LSP.Structures.Variant_3 =>
               Write_InlayHintRegistrationOptions (Handler, Value.Variant_3);
         end case;
      end Write_inlayHintProvider_OfServerCapabilities;

      procedure Write_SemanticTokensOptions_Or_SemanticTokensRegistrationOptions
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures
           .SemanticTokensOptions_Or_SemanticTokensRegistrationOptions) is
      begin
         case Value.Is_SemanticTokensOptions is
            when True =>
               Write_SemanticTokensOptions
                 (Handler, Value.SemanticTokensOptions);
            when False =>
               Write_SemanticTokensRegistrationOptions
                 (Handler, Value.SemanticTokensRegistrationOptions);
         end case;
      end Write_SemanticTokensOptions_Or_SemanticTokensRegistrationOptions;

      procedure Write_monikerProvider_OfServerCapabilities
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.monikerProvider_OfServerCapabilities) is
      begin
         case Value.Kind is
            when LSP.Structures.Variant_1 =>
               Handler.Boolean_Value (Value.Variant_1);
            when LSP.Structures.Variant_2 =>
               Write_MonikerOptions (Handler, Value.Variant_2);
            when LSP.Structures.Variant_3 =>
               Write_MonikerRegistrationOptions (Handler, Value.Variant_3);
         end case;
      end Write_monikerProvider_OfServerCapabilities;

      procedure Write_Boolean_Or_CodeActionOptions
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.Boolean_Or_CodeActionOptions) is
      begin
         case Value.Is_Boolean is
            when True =>
               Handler.Boolean_Value (Value.Boolean);
            when False =>
               Write_CodeActionOptions (Handler, Value.CodeActionOptions);
         end case;
      end Write_Boolean_Or_CodeActionOptions;

      procedure Write_selectionRangeProvider_OfServerCapabilities
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value : LSP.Structures.selectionRangeProvider_OfServerCapabilities) is
      begin
         case Value.Kind is
            when LSP.Structures.Variant_1 =>
               Handler.Boolean_Value (Value.Variant_1);
            when LSP.Structures.Variant_2 =>
               Write_SelectionRangeOptions (Handler, Value.Variant_2);
            when LSP.Structures.Variant_3 =>
               Write_SelectionRangeRegistrationOptions
                 (Handler, Value.Variant_3);
         end case;
      end Write_selectionRangeProvider_OfServerCapabilities;

      procedure Write_Boolean_Or_DocumentRangeFormattingOptions
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.Boolean_Or_DocumentRangeFormattingOptions) is
      begin
         case Value.Is_Boolean is
            when True =>
               Handler.Boolean_Value (Value.Boolean);
            when False =>
               Write_DocumentRangeFormattingOptions
                 (Handler, Value.DocumentRangeFormattingOptions);
         end case;
      end Write_Boolean_Or_DocumentRangeFormattingOptions;

      procedure Write_Boolean_Or_HoverOptions
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.Boolean_Or_HoverOptions) is
      begin
         case Value.Is_Boolean is
            when True =>
               Handler.Boolean_Value (Value.Boolean);
            when False =>
               Write_HoverOptions (Handler, Value.HoverOptions);
         end case;
      end Write_Boolean_Or_HoverOptions;

      procedure Write_workspace_OfServerCapabilities
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.workspace_OfServerCapabilities) is
      begin
         Handler.Start_Object;
         if Value.workspaceFolders.Is_Set then
            Handler.Key_Name ("workspaceFolders");
            Write_WorkspaceFoldersServerCapabilities
              (Handler, Value.workspaceFolders.Value);
         end if;
         if Value.fileOperations.Is_Set then
            Handler.Key_Name ("fileOperations");
            Write_FileOperationOptions (Handler, Value.fileOperations.Value);
         end if;
         Handler.End_Object;
      end Write_workspace_OfServerCapabilities;

      procedure Write_declarationProvider_OfServerCapabilities
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.declarationProvider_OfServerCapabilities) is
      begin
         case Value.Kind is
            when LSP.Structures.Variant_1 =>
               Handler.Boolean_Value (Value.Variant_1);
            when LSP.Structures.Variant_2 =>
               Write_DeclarationOptions (Handler, Value.Variant_2);
            when LSP.Structures.Variant_3 =>
               Write_DeclarationRegistrationOptions (Handler, Value.Variant_3);
         end case;
      end Write_declarationProvider_OfServerCapabilities;

      procedure Write_callHierarchyProvider_OfServerCapabilities
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value : LSP.Structures.callHierarchyProvider_OfServerCapabilities) is
      begin
         case Value.Kind is
            when LSP.Structures.Variant_1 =>
               Handler.Boolean_Value (Value.Variant_1);
            when LSP.Structures.Variant_2 =>
               Write_CallHierarchyOptions (Handler, Value.Variant_2);
            when LSP.Structures.Variant_3 =>
               Write_CallHierarchyRegistrationOptions
                 (Handler, Value.Variant_3);
         end case;
      end Write_callHierarchyProvider_OfServerCapabilities;

      procedure Write_Boolean_Or_DocumentHighlightOptions
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.Boolean_Or_DocumentHighlightOptions) is
      begin
         case Value.Is_Boolean is
            when True =>
               Handler.Boolean_Value (Value.Boolean);
            when False =>
               Write_DocumentHighlightOptions
                 (Handler, Value.DocumentHighlightOptions);
         end case;
      end Write_Boolean_Or_DocumentHighlightOptions;

      procedure Write_TextDocumentSyncOptions_Or_TextDocumentSyncKind
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures
           .TextDocumentSyncOptions_Or_TextDocumentSyncKind) is
      begin
         case Value.Is_TextDocumentSyncOptions is
            when True =>
               Write_TextDocumentSyncOptions
                 (Handler, Value.TextDocumentSyncOptions);
            when False =>
               Write_TextDocumentSyncKind
                 (Handler, Value.TextDocumentSyncKind);
         end case;
      end Write_TextDocumentSyncOptions_Or_TextDocumentSyncKind;

      procedure Write_linkedEditingRangeProvider_OfServerCapabilities
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures
           .linkedEditingRangeProvider_OfServerCapabilities) is
      begin
         case Value.Kind is
            when LSP.Structures.Variant_1 =>
               Handler.Boolean_Value (Value.Variant_1);
            when LSP.Structures.Variant_2 =>
               Write_LinkedEditingRangeOptions (Handler, Value.Variant_2);
            when LSP.Structures.Variant_3 =>
               Write_LinkedEditingRangeRegistrationOptions
                 (Handler, Value.Variant_3);
         end case;
      end Write_linkedEditingRangeProvider_OfServerCapabilities;

      procedure Write_Boolean_Or_DocumentFormattingOptions
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.Boolean_Or_DocumentFormattingOptions) is
      begin
         case Value.Is_Boolean is
            when True =>
               Handler.Boolean_Value (Value.Boolean);
            when False =>
               Write_DocumentFormattingOptions
                 (Handler, Value.DocumentFormattingOptions);
         end case;
      end Write_Boolean_Or_DocumentFormattingOptions;

      procedure Write_Boolean_Or_ReferenceOptions
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.Boolean_Or_ReferenceOptions) is
      begin
         case Value.Is_Boolean is
            when True =>
               Handler.Boolean_Value (Value.Boolean);
            when False =>
               Write_ReferenceOptions (Handler, Value.ReferenceOptions);
         end case;
      end Write_Boolean_Or_ReferenceOptions;

      procedure Write_foldingRangeProvider_OfServerCapabilities
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.foldingRangeProvider_OfServerCapabilities) is
      begin
         case Value.Kind is
            when LSP.Structures.Variant_1 =>
               Handler.Boolean_Value (Value.Variant_1);
            when LSP.Structures.Variant_2 =>
               Write_FoldingRangeOptions (Handler, Value.Variant_2);
            when LSP.Structures.Variant_3 =>
               Write_FoldingRangeRegistrationOptions
                 (Handler, Value.Variant_3);
         end case;
      end Write_foldingRangeProvider_OfServerCapabilities;

      procedure Write_typeDefinitionProvider_OfServerCapabilities
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value : LSP.Structures.typeDefinitionProvider_OfServerCapabilities) is
      begin
         case Value.Kind is
            when LSP.Structures.Variant_1 =>
               Handler.Boolean_Value (Value.Variant_1);
            when LSP.Structures.Variant_2 =>
               Write_TypeDefinitionOptions (Handler, Value.Variant_2);
            when LSP.Structures.Variant_3 =>
               Write_TypeDefinitionRegistrationOptions
                 (Handler, Value.Variant_3);
         end case;
      end Write_typeDefinitionProvider_OfServerCapabilities;

      procedure Write_inlineValueProvider_OfServerCapabilities
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.inlineValueProvider_OfServerCapabilities) is
      begin
         case Value.Kind is
            when LSP.Structures.Variant_1 =>
               Handler.Boolean_Value (Value.Variant_1);
            when LSP.Structures.Variant_2 =>
               Write_InlineValueOptions (Handler, Value.Variant_2);
            when LSP.Structures.Variant_3 =>
               Write_InlineValueRegistrationOptions (Handler, Value.Variant_3);
         end case;
      end Write_inlineValueProvider_OfServerCapabilities;

      procedure Write_colorProvider_OfServerCapabilities
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.colorProvider_OfServerCapabilities) is
      begin
         case Value.Kind is
            when LSP.Structures.Variant_1 =>
               Handler.Boolean_Value (Value.Variant_1);
            when LSP.Structures.Variant_2 =>
               Write_DocumentColorOptions (Handler, Value.Variant_2);
            when LSP.Structures.Variant_3 =>
               Write_DocumentColorRegistrationOptions
                 (Handler, Value.Variant_3);
         end case;
      end Write_colorProvider_OfServerCapabilities;

      procedure Write_Boolean_Or_DefinitionOptions
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.Boolean_Or_DefinitionOptions) is
      begin
         case Value.Is_Boolean is
            when True =>
               Handler.Boolean_Value (Value.Boolean);
            when False =>
               Write_DefinitionOptions (Handler, Value.DefinitionOptions);
         end case;
      end Write_Boolean_Or_DefinitionOptions;

      procedure Write_Boolean_Or_RenameOptions
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.Boolean_Or_RenameOptions) is
      begin
         case Value.Is_Boolean is
            when True =>
               Handler.Boolean_Value (Value.Boolean);
            when False =>
               Write_RenameOptions (Handler, Value.RenameOptions);
         end case;
      end Write_Boolean_Or_RenameOptions;

   begin
      Handler.Start_Object;
      if Value.positionEncoding.Is_Set then
         Handler.Key_Name ("positionEncoding");
         Write_PositionEncodingKind (Handler, Value.positionEncoding.Value);
      end if;
      if Value.textDocumentSync.Is_Set then
         Handler.Key_Name ("textDocumentSync");
         Write_TextDocumentSyncOptions_Or_TextDocumentSyncKind
           (Handler, Value.textDocumentSync.Value);
      end if;
      if Value.notebookDocumentSync.Is_Set then
         Handler.Key_Name ("notebookDocumentSync");
         Write_NotebookDocumentSyncOptions_Or_NotebookDocumentSyncRegistrationOptions
           (Handler, Value.notebookDocumentSync.Value);
      end if;
      if Value.completionProvider.Is_Set then
         Handler.Key_Name ("completionProvider");
         Write_CompletionOptions (Handler, Value.completionProvider.Value);
      end if;
      if Value.hoverProvider.Is_Set then
         Handler.Key_Name ("hoverProvider");
         Write_Boolean_Or_HoverOptions (Handler, Value.hoverProvider.Value);
      end if;
      if Value.signatureHelpProvider.Is_Set then
         Handler.Key_Name ("signatureHelpProvider");
         Write_SignatureHelpOptions
           (Handler, Value.signatureHelpProvider.Value);
      end if;
      if Value.declarationProvider.Is_Set then
         Handler.Key_Name ("declarationProvider");
         Write_declarationProvider_OfServerCapabilities
           (Handler, Value.declarationProvider.Value);
      end if;
      if Value.definitionProvider.Is_Set then
         Handler.Key_Name ("definitionProvider");
         Write_Boolean_Or_DefinitionOptions
           (Handler, Value.definitionProvider.Value);
      end if;
      if Value.typeDefinitionProvider.Is_Set then
         Handler.Key_Name ("typeDefinitionProvider");
         Write_typeDefinitionProvider_OfServerCapabilities
           (Handler, Value.typeDefinitionProvider.Value);
      end if;
      if Value.implementationProvider.Is_Set then
         Handler.Key_Name ("implementationProvider");
         Write_implementationProvider_OfServerCapabilities
           (Handler, Value.implementationProvider.Value);
      end if;
      if Value.referencesProvider.Is_Set then
         Handler.Key_Name ("referencesProvider");
         Write_Boolean_Or_ReferenceOptions
           (Handler, Value.referencesProvider.Value);
      end if;
      if Value.documentHighlightProvider.Is_Set then
         Handler.Key_Name ("documentHighlightProvider");
         Write_Boolean_Or_DocumentHighlightOptions
           (Handler, Value.documentHighlightProvider.Value);
      end if;
      if Value.documentSymbolProvider.Is_Set then
         Handler.Key_Name ("documentSymbolProvider");
         Write_Boolean_Or_DocumentSymbolOptions
           (Handler, Value.documentSymbolProvider.Value);
      end if;
      if Value.codeActionProvider.Is_Set then
         Handler.Key_Name ("codeActionProvider");
         Write_Boolean_Or_CodeActionOptions
           (Handler, Value.codeActionProvider.Value);
      end if;
      if Value.codeLensProvider.Is_Set then
         Handler.Key_Name ("codeLensProvider");
         Write_CodeLensOptions (Handler, Value.codeLensProvider.Value);
      end if;
      if Value.documentLinkProvider.Is_Set then
         Handler.Key_Name ("documentLinkProvider");
         Write_DocumentLinkOptions (Handler, Value.documentLinkProvider.Value);
      end if;
      if Value.colorProvider.Is_Set then
         Handler.Key_Name ("colorProvider");
         Write_colorProvider_OfServerCapabilities
           (Handler, Value.colorProvider.Value);
      end if;
      if Value.workspaceSymbolProvider.Is_Set then
         Handler.Key_Name ("workspaceSymbolProvider");
         Write_Boolean_Or_WorkspaceSymbolOptions
           (Handler, Value.workspaceSymbolProvider.Value);
      end if;
      if Value.documentFormattingProvider.Is_Set then
         Handler.Key_Name ("documentFormattingProvider");
         Write_Boolean_Or_DocumentFormattingOptions
           (Handler, Value.documentFormattingProvider.Value);
      end if;
      if Value.documentRangeFormattingProvider.Is_Set then
         Handler.Key_Name ("documentRangeFormattingProvider");
         Write_Boolean_Or_DocumentRangeFormattingOptions
           (Handler, Value.documentRangeFormattingProvider.Value);
      end if;
      if Value.documentOnTypeFormattingProvider.Is_Set then
         Handler.Key_Name ("documentOnTypeFormattingProvider");
         Write_DocumentOnTypeFormattingOptions
           (Handler, Value.documentOnTypeFormattingProvider.Value);
      end if;
      if Value.renameProvider.Is_Set then
         Handler.Key_Name ("renameProvider");
         Write_Boolean_Or_RenameOptions (Handler, Value.renameProvider.Value);
      end if;
      if Value.foldingRangeProvider.Is_Set then
         Handler.Key_Name ("foldingRangeProvider");
         Write_foldingRangeProvider_OfServerCapabilities
           (Handler, Value.foldingRangeProvider.Value);
      end if;
      if Value.selectionRangeProvider.Is_Set then
         Handler.Key_Name ("selectionRangeProvider");
         Write_selectionRangeProvider_OfServerCapabilities
           (Handler, Value.selectionRangeProvider.Value);
      end if;
      if Value.executeCommandProvider.Is_Set then
         Handler.Key_Name ("executeCommandProvider");
         Write_ExecuteCommandOptions
           (Handler, Value.executeCommandProvider.Value);
      end if;
      if Value.callHierarchyProvider.Is_Set then
         Handler.Key_Name ("callHierarchyProvider");
         Write_callHierarchyProvider_OfServerCapabilities
           (Handler, Value.callHierarchyProvider.Value);
      end if;
      if Value.linkedEditingRangeProvider.Is_Set then
         Handler.Key_Name ("linkedEditingRangeProvider");
         Write_linkedEditingRangeProvider_OfServerCapabilities
           (Handler, Value.linkedEditingRangeProvider.Value);
      end if;
      if Value.semanticTokensProvider.Is_Set then
         Handler.Key_Name ("semanticTokensProvider");
         Write_SemanticTokensOptions_Or_SemanticTokensRegistrationOptions
           (Handler, Value.semanticTokensProvider.Value);
      end if;
      if Value.monikerProvider.Is_Set then
         Handler.Key_Name ("monikerProvider");
         Write_monikerProvider_OfServerCapabilities
           (Handler, Value.monikerProvider.Value);
      end if;
      if Value.typeHierarchyProvider.Is_Set then
         Handler.Key_Name ("typeHierarchyProvider");
         Write_typeHierarchyProvider_OfServerCapabilities
           (Handler, Value.typeHierarchyProvider.Value);
      end if;
      if Value.inlineValueProvider.Is_Set then
         Handler.Key_Name ("inlineValueProvider");
         Write_inlineValueProvider_OfServerCapabilities
           (Handler, Value.inlineValueProvider.Value);
      end if;
      if Value.inlayHintProvider.Is_Set then
         Handler.Key_Name ("inlayHintProvider");
         Write_inlayHintProvider_OfServerCapabilities
           (Handler, Value.inlayHintProvider.Value);
      end if;
      if Value.diagnosticProvider.Is_Set then
         Handler.Key_Name ("diagnosticProvider");
         Write_DiagnosticOptions_Or_DiagnosticRegistrationOptions
           (Handler, Value.diagnosticProvider.Value);
      end if;
      if Value.workspace.Is_Set then
         Handler.Key_Name ("workspace");
         Write_workspace_OfServerCapabilities (Handler, Value.workspace.Value);
      end if;
      if Value.experimental.Is_Set then
         Handler.Key_Name ("experimental");
         Write_T (Handler, Value.experimental.Value);
      end if;
      if (for some Item of Value.alsReferenceKinds => Item) then
         Handler.Key_Name ("alsReferenceKinds");
         Write_AlsReferenceKind_Set (Handler, Value.alsReferenceKinds);
      end if;
      Handler.End_Object;
   end Write_ServerCapabilities;

   procedure Write_DidChangeWatchedFilesRegistrationOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DidChangeWatchedFilesRegistrationOptions) is
      procedure Write_FileSystemWatcher_Vector
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.FileSystemWatcher_Vector);

      procedure Write_FileSystemWatcher_Vector
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.FileSystemWatcher_Vector) is
      begin
         Handler.Start_Array;
         for J in Value.First_Index .. Value.Last_Index loop
            Write_FileSystemWatcher (Handler, Value (J));
         end loop;
         Handler.End_Array;
      end Write_FileSystemWatcher_Vector;

   begin
      Handler.Start_Object;
      Handler.Key_Name ("watchers");
      Write_FileSystemWatcher_Vector (Handler, Value.watchers);
      Handler.End_Object;
   end Write_DidChangeWatchedFilesRegistrationOptions;

   procedure Write_CallHierarchyOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.CallHierarchyOptions) is
   begin
      Handler.Start_Object;
      if Value.workDoneProgress.Is_Set then
         Handler.Key_Name ("workDoneProgress");
         Handler.Boolean_Value (Value.workDoneProgress.Value);
      end if;
      Handler.End_Object;
   end Write_CallHierarchyOptions;

   procedure Write_LSPAny
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.LSPAny) renames
     LSP.Output_Tools.Write_LSPAny;

   procedure Write_ImplementationRegistrationOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.ImplementationRegistrationOptions) is
      procedure Write_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.DocumentSelector_Or_Null);

      procedure Write_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.DocumentSelector_Or_Null) is
      begin
         if Value.Is_Null then
            Handler.Null_Value;
         else
            Write_DocumentSelector (Handler, Value.Value);
         end if;
      end Write_DocumentSelector_Or_Null;

   begin
      Handler.Start_Object;
      Handler.Key_Name ("documentSelector");
      Write_DocumentSelector_Or_Null (Handler, Value.Parent.documentSelector);
      if Value.workDoneProgress.Is_Set then
         Handler.Key_Name ("workDoneProgress");
         Handler.Boolean_Value (Value.workDoneProgress.Value);
      end if;
      if not Value.id.Is_Null then
         Handler.Key_Name ("id");
         Handler.String_Value (Value.id);
      end if;
      Handler.End_Object;
   end Write_ImplementationRegistrationOptions;

   procedure Write_LogTraceParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.LogTraceParams) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("message");
      Handler.String_Value (Value.message);
      if not Value.verbose.Is_Null then
         Handler.Key_Name ("verbose");
         Handler.String_Value (Value.verbose);
      end if;
      Handler.End_Object;
   end Write_LogTraceParams;

   procedure Write_FoldingRangeRegistrationOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.FoldingRangeRegistrationOptions) is
      procedure Write_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.DocumentSelector_Or_Null);

      procedure Write_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.DocumentSelector_Or_Null) is
      begin
         if Value.Is_Null then
            Handler.Null_Value;
         else
            Write_DocumentSelector (Handler, Value.Value);
         end if;
      end Write_DocumentSelector_Or_Null;

   begin
      Handler.Start_Object;
      Handler.Key_Name ("documentSelector");
      Write_DocumentSelector_Or_Null (Handler, Value.Parent.documentSelector);
      if Value.workDoneProgress.Is_Set then
         Handler.Key_Name ("workDoneProgress");
         Handler.Boolean_Value (Value.workDoneProgress.Value);
      end if;
      if not Value.id.Is_Null then
         Handler.Key_Name ("id");
         Handler.String_Value (Value.id);
      end if;
      Handler.End_Object;
   end Write_FoldingRangeRegistrationOptions;

   procedure Write_InlineValueContext
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.InlineValueContext) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("frameId");
      Handler.Integer_Value (Integer'Pos (Value.frameId));
      Handler.Key_Name ("stoppedLocation");
      Write_A_Range (Handler, Value.stoppedLocation);
      Handler.End_Object;
   end Write_InlineValueContext;

   procedure Write_TextDocumentIdentifier_Vector
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.TextDocumentIdentifier_Vector) is
   begin
      Handler.Start_Array;
      for J in Value.First_Index .. Value.Last_Index loop
         Write_TextDocumentIdentifier (Handler, Value (J));
      end loop;
      Handler.End_Array;
   end Write_TextDocumentIdentifier_Vector;

   procedure Write_DefinitionLink
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DefinitionLink) renames
     Write_LocationLink;

   procedure Write_DocumentFormattingOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DocumentFormattingOptions) is
   begin
      Handler.Start_Object;
      if Value.workDoneProgress.Is_Set then
         Handler.Key_Name ("workDoneProgress");
         Handler.Boolean_Value (Value.workDoneProgress.Value);
      end if;
      Handler.End_Object;
   end Write_DocumentFormattingOptions;

   procedure Write_SymbolInformation_Vector
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.SymbolInformation_Vector) is
   begin
      Handler.Start_Array;
      for J in Value.First_Index .. Value.Last_Index loop
         Write_SymbolInformation (Handler, Value (J));
      end loop;
      Handler.End_Array;
   end Write_SymbolInformation_Vector;

   procedure Write_RenameFile
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.RenameFile) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("kind");
      Handler.String_Value ("rename");
      if Value.annotationId.Is_Set then
         Handler.Key_Name ("annotationId");
         Write_ChangeAnnotationIdentifier (Handler, Value.annotationId.Value);
      end if;
      Handler.Key_Name ("oldUri");
      Handler.String_Value (Value.oldUri);
      Handler.Key_Name ("newUri");
      Handler.String_Value (Value.newUri);
      if Value.options.Is_Set then
         Handler.Key_Name ("options");
         Write_RenameFileOptions (Handler, Value.options.Value);
      end if;
      Handler.End_Object;
   end Write_RenameFile;

   procedure Write_PrepareRenameResult_Or_Null
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.PrepareRenameResult_Or_Null) is
   begin
      if Value.Is_Null then
         Handler.Null_Value;
      else
         Write_PrepareRenameResult (Handler, Value.Value);
      end if;
   end Write_PrepareRenameResult_Or_Null;

   procedure Write_DidChangeConfigurationRegistrationOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DidChangeConfigurationRegistrationOptions) is
      procedure Write_Virtual_String_Vector
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.Virtual_String_Vector);

      procedure Write_Virtual_String_Vector
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.Virtual_String_Vector) is
      begin
         if Value.Length = 1 then
            Handler.String_Value (Value (1));

         else
            Handler.Start_Array;
            for J in 1 .. Value.Length loop
               Handler.String_Value (Value (J));

            end loop;
            Handler.End_Array;
         end if;
      end Write_Virtual_String_Vector;

   begin
      Handler.Start_Object;
      if Value.section.Is_Set then
         Handler.Key_Name ("section");
         Write_Virtual_String_Vector (Handler, Value.section.Value);
      end if;
      Handler.End_Object;
   end Write_DidChangeConfigurationRegistrationOptions;

   procedure Write_CallHierarchyItem
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.CallHierarchyItem) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("name");
      Handler.String_Value (Value.name);
      Handler.Key_Name ("kind");
      Write_SymbolKind (Handler, Value.kind);
      if (for some Item of Value.tags => Item) then
         Handler.Key_Name ("tags");
         Write_SymbolTag_Set (Handler, Value.tags);
      end if;
      if not Value.detail.Is_Null then
         Handler.Key_Name ("detail");
         Handler.String_Value (Value.detail);
      end if;
      Handler.Key_Name ("uri");
      Handler.String_Value (Value.uri);
      Handler.Key_Name ("range");
      Write_A_Range (Handler, Value.a_range);
      Handler.Key_Name ("selectionRange");
      Write_A_Range (Handler, Value.selectionRange);
      if not Value.data.Is_Empty then
         Handler.Key_Name ("data");
         Write_LSPAny (Handler, Value.data);
      end if;
      Handler.End_Object;
   end Write_CallHierarchyItem;

   procedure Write_SymbolTag_Set
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.SymbolTag_Set) is
   begin
      Handler.Start_Array;
      declare
         Set : LSP.Structures.SymbolTag_Set renames Value;
      begin
         for Value in Set'Range loop
            if Set (Value) then
               Write_SymbolTag (Handler, Value);
            end if;
         end loop;
      end;
      Handler.End_Array;
   end Write_SymbolTag_Set;

   procedure Write_CompletionItem_Vector
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.CompletionItem_Vector) is
   begin
      Handler.Start_Array;
      for J in Value.First_Index .. Value.Last_Index loop
         Write_CompletionItem (Handler, Value (J));
      end loop;
      Handler.End_Array;
   end Write_CompletionItem_Vector;

   procedure Write_RelativePattern
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.RelativePattern) is
      procedure Write_WorkspaceFolder_Or_URI
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.WorkspaceFolder_Or_URI);

      procedure Write_WorkspaceFolder_Or_URI
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.WorkspaceFolder_Or_URI) is
      begin
         case Value.Is_WorkspaceFolder is
            when True =>
               Write_WorkspaceFolder (Handler, Value.WorkspaceFolder);
            when False =>
               Write_URI (Handler, Value.URI);
         end case;
      end Write_WorkspaceFolder_Or_URI;

   begin
      Handler.Start_Object;
      Handler.Key_Name ("baseUri");
      Write_WorkspaceFolder_Or_URI (Handler, Value.baseUri);
      Handler.Key_Name ("pattern");
      Write_Pattern (Handler, Value.pattern);
      Handler.End_Object;
   end Write_RelativePattern;

   procedure Write_Moniker_Vector_Or_Null
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.Moniker_Vector_Or_Null) is
   begin
      if Value.Is_Empty then
         Handler.Null_Value;
      else
         Write_Moniker_Vector (Handler, Value);
      end if;
   end Write_Moniker_Vector_Or_Null;

   procedure Write_WorkDoneProgressReport
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.WorkDoneProgressReport) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("kind");
      Handler.String_Value ("report");
      if Value.cancellable.Is_Set then
         Handler.Key_Name ("cancellable");
         Handler.Boolean_Value (Value.cancellable.Value);
      end if;
      if not Value.message.Is_Null then
         Handler.Key_Name ("message");
         Handler.String_Value (Value.message);
      end if;
      if Value.percentage.Is_Set then
         Handler.Key_Name ("percentage");
         Handler.Integer_Value (Integer'Pos (Value.percentage.Value));
      end if;
      Handler.End_Object;
   end Write_WorkDoneProgressReport;

   procedure Write_ReferenceOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.ReferenceOptions) is
   begin
      Handler.Start_Object;
      if Value.workDoneProgress.Is_Set then
         Handler.Key_Name ("workDoneProgress");
         Handler.Boolean_Value (Value.workDoneProgress.Value);
      end if;
      Handler.End_Object;
   end Write_ReferenceOptions;

   procedure Write_tagSupport_OfWorkspaceSymbolClientCapabilities
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value : LSP.Structures.tagSupport_OfWorkspaceSymbolClientCapabilities) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("valueSet");
      Write_SymbolTag_Set (Handler, Value.valueSet);
      Handler.End_Object;
   end Write_tagSupport_OfWorkspaceSymbolClientCapabilities;

   procedure Write_TextDocumentClientCapabilities
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.TextDocumentClientCapabilities) is
   begin
      Handler.Start_Object;
      if Value.synchronization.Is_Set then
         Handler.Key_Name ("synchronization");
         Write_TextDocumentSyncClientCapabilities
           (Handler, Value.synchronization.Value);
      end if;
      if Value.completion.Is_Set then
         Handler.Key_Name ("completion");
         Write_CompletionClientCapabilities (Handler, Value.completion.Value);
      end if;
      if Value.hover.Is_Set then
         Handler.Key_Name ("hover");
         Write_HoverClientCapabilities (Handler, Value.hover.Value);
      end if;
      if Value.signatureHelp.Is_Set then
         Handler.Key_Name ("signatureHelp");
         Write_SignatureHelpClientCapabilities
           (Handler, Value.signatureHelp.Value);
      end if;
      if Value.declaration.Is_Set then
         Handler.Key_Name ("declaration");
         Write_DeclarationClientCapabilities
           (Handler, Value.declaration.Value);
      end if;
      if Value.definition.Is_Set then
         Handler.Key_Name ("definition");
         Write_DefinitionClientCapabilities (Handler, Value.definition.Value);
      end if;
      if Value.typeDefinition.Is_Set then
         Handler.Key_Name ("typeDefinition");
         Write_TypeDefinitionClientCapabilities
           (Handler, Value.typeDefinition.Value);
      end if;
      if Value.implementation.Is_Set then
         Handler.Key_Name ("implementation");
         Write_ImplementationClientCapabilities
           (Handler, Value.implementation.Value);
      end if;
      if Value.references.Is_Set then
         Handler.Key_Name ("references");
         Write_ReferenceClientCapabilities (Handler, Value.references.Value);
      end if;
      if Value.documentHighlight.Is_Set then
         Handler.Key_Name ("documentHighlight");
         Write_DocumentHighlightClientCapabilities
           (Handler, Value.documentHighlight.Value);
      end if;
      if Value.documentSymbol.Is_Set then
         Handler.Key_Name ("documentSymbol");
         Write_DocumentSymbolClientCapabilities
           (Handler, Value.documentSymbol.Value);
      end if;
      if Value.codeAction.Is_Set then
         Handler.Key_Name ("codeAction");
         Write_CodeActionClientCapabilities (Handler, Value.codeAction.Value);
      end if;
      if Value.codeLens.Is_Set then
         Handler.Key_Name ("codeLens");
         Write_CodeLensClientCapabilities (Handler, Value.codeLens.Value);
      end if;
      if Value.documentLink.Is_Set then
         Handler.Key_Name ("documentLink");
         Write_DocumentLinkClientCapabilities
           (Handler, Value.documentLink.Value);
      end if;
      if Value.colorProvider.Is_Set then
         Handler.Key_Name ("colorProvider");
         Write_DocumentColorClientCapabilities
           (Handler, Value.colorProvider.Value);
      end if;
      if Value.formatting.Is_Set then
         Handler.Key_Name ("formatting");
         Write_DocumentFormattingClientCapabilities
           (Handler, Value.formatting.Value);
      end if;
      if Value.rangeFormatting.Is_Set then
         Handler.Key_Name ("rangeFormatting");
         Write_DocumentRangeFormattingClientCapabilities
           (Handler, Value.rangeFormatting.Value);
      end if;
      if Value.onTypeFormatting.Is_Set then
         Handler.Key_Name ("onTypeFormatting");
         Write_DocumentOnTypeFormattingClientCapabilities
           (Handler, Value.onTypeFormatting.Value);
      end if;
      if Value.rename.Is_Set then
         Handler.Key_Name ("rename");
         Write_RenameClientCapabilities (Handler, Value.rename.Value);
      end if;
      if Value.foldingRange.Is_Set then
         Handler.Key_Name ("foldingRange");
         Write_FoldingRangeClientCapabilities
           (Handler, Value.foldingRange.Value);
      end if;
      if Value.selectionRange.Is_Set then
         Handler.Key_Name ("selectionRange");
         Write_SelectionRangeClientCapabilities
           (Handler, Value.selectionRange.Value);
      end if;
      if Value.publishDiagnostics.Is_Set then
         Handler.Key_Name ("publishDiagnostics");
         Write_PublishDiagnosticsClientCapabilities
           (Handler, Value.publishDiagnostics.Value);
      end if;
      if Value.callHierarchy.Is_Set then
         Handler.Key_Name ("callHierarchy");
         Write_CallHierarchyClientCapabilities
           (Handler, Value.callHierarchy.Value);
      end if;
      if Value.semanticTokens.Is_Set then
         Handler.Key_Name ("semanticTokens");
         Write_SemanticTokensClientCapabilities
           (Handler, Value.semanticTokens.Value);
      end if;
      if Value.linkedEditingRange.Is_Set then
         Handler.Key_Name ("linkedEditingRange");
         Write_LinkedEditingRangeClientCapabilities
           (Handler, Value.linkedEditingRange.Value);
      end if;
      if Value.moniker.Is_Set then
         Handler.Key_Name ("moniker");
         Write_MonikerClientCapabilities (Handler, Value.moniker.Value);
      end if;
      if Value.typeHierarchy.Is_Set then
         Handler.Key_Name ("typeHierarchy");
         Write_TypeHierarchyClientCapabilities
           (Handler, Value.typeHierarchy.Value);
      end if;
      if Value.inlineValue.Is_Set then
         Handler.Key_Name ("inlineValue");
         Write_InlineValueClientCapabilities
           (Handler, Value.inlineValue.Value);
      end if;
      if Value.inlayHint.Is_Set then
         Handler.Key_Name ("inlayHint");
         Write_InlayHintClientCapabilities (Handler, Value.inlayHint.Value);
      end if;
      if Value.diagnostic.Is_Set then
         Handler.Key_Name ("diagnostic");
         Write_DiagnosticClientCapabilities (Handler, Value.diagnostic.Value);
      end if;
      Handler.End_Object;
   end Write_TextDocumentClientCapabilities;

   procedure Write_WorkspaceSymbolParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.WorkspaceSymbolParams) is
   begin
      Handler.Start_Object;
      if Value.workDoneToken.Is_Set then
         Handler.Key_Name ("workDoneToken");
         Write_ProgressToken (Handler, Value.workDoneToken.Value);
      end if;
      if Value.partialResultToken.Is_Set then
         Handler.Key_Name ("partialResultToken");
         Write_ProgressToken (Handler, Value.partialResultToken.Value);
      end if;
      Handler.Key_Name ("query");
      Handler.String_Value (Value.query);
      Handler.End_Object;
   end Write_WorkspaceSymbolParams;

   procedure Write_DocumentSymbol_Vector
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DocumentSymbol_Vector) is
   begin
      Handler.Start_Array;
      for J in 1 .. Value.Length loop
         Write_DocumentSymbol (Handler, Value (J));
      end loop;
      Handler.End_Array;
   end Write_DocumentSymbol_Vector;

   procedure Write_GlobPattern
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.GlobPattern) is
   begin
      case Value.Is_Pattern is
         when True =>
            Write_Pattern (Handler, Value.Pattern);
         when False =>
            Write_RelativePattern (Handler, Value.RelativePattern);
      end case;
   end Write_GlobPattern;

   procedure Write_NotebookDocumentIdentifier
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.NotebookDocumentIdentifier) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("uri");
      Write_URI (Handler, Value.uri);
      Handler.End_Object;
   end Write_NotebookDocumentIdentifier;

   procedure Write_InsertReplaceEdit
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.InsertReplaceEdit) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("newText");
      Handler.String_Value (Value.newText);
      Handler.Key_Name ("insert");
      Write_A_Range (Handler, Value.insert);
      Handler.Key_Name ("replace");
      Write_A_Range (Handler, Value.replace);
      Handler.End_Object;
   end Write_InsertReplaceEdit;

   procedure Write_InitializeResult
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.InitializeResult) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("capabilities");
      Write_ServerCapabilities (Handler, Value.capabilities);
      if Value.serverInfo.Is_Set then
         Handler.Key_Name ("serverInfo");
         Write_clientInfo_Of_InitializeParams
           (Handler, Value.serverInfo.Value);
      end if;
      Handler.End_Object;
   end Write_InitializeResult;

   procedure Write_InlineValueVariableLookup
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.InlineValueVariableLookup) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("range");
      Write_A_Range (Handler, Value.a_range);
      if not Value.variableName.Is_Null then
         Handler.Key_Name ("variableName");
         Handler.String_Value (Value.variableName);
      end if;
      Handler.Key_Name ("caseSensitiveLookup");
      Handler.Boolean_Value (Value.caseSensitiveLookup);
      Handler.End_Object;
   end Write_InlineValueVariableLookup;

   procedure Write_FileEvent
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.FileEvent) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("uri");
      Handler.String_Value (Value.uri);
      Handler.Key_Name ("type");
      Write_FileChangeType (Handler, Value.a_type);
      Handler.End_Object;
   end Write_FileEvent;

   procedure Write_SignatureHelpClientCapabilities
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.SignatureHelpClientCapabilities) is
      procedure Write_signatureInformation_OfSignatureHelpClientCapabilities
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures
           .signatureInformation_OfSignatureHelpClientCapabilities);

      procedure Write_parameterInformation_OfsignatureInformation_OfSignatureHelpClientCapabilities
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures
           .parameterInformation_OfsignatureInformation_OfSignatureHelpClientCapabilities);

      procedure Write_signatureInformation_OfSignatureHelpClientCapabilities
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures
           .signatureInformation_OfSignatureHelpClientCapabilities) is
      begin
         Handler.Start_Object;
         if not Value.documentationFormat.Is_Empty then
            Handler.Key_Name ("documentationFormat");
            Write_MarkupKind_Vector (Handler, Value.documentationFormat);
         end if;
         if Value.parameterInformation.Is_Set then
            Handler.Key_Name ("parameterInformation");
            Write_parameterInformation_OfsignatureInformation_OfSignatureHelpClientCapabilities
              (Handler, Value.parameterInformation.Value);
         end if;
         if Value.activeParameterSupport.Is_Set then
            Handler.Key_Name ("activeParameterSupport");
            Handler.Boolean_Value (Value.activeParameterSupport.Value);
         end if;
         Handler.End_Object;
      end Write_signatureInformation_OfSignatureHelpClientCapabilities;

      procedure Write_parameterInformation_OfsignatureInformation_OfSignatureHelpClientCapabilities
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures
           .parameterInformation_OfsignatureInformation_OfSignatureHelpClientCapabilities) is
      begin
         Handler.Start_Object;
         if Value.labelOffsetSupport.Is_Set then
            Handler.Key_Name ("labelOffsetSupport");
            Handler.Boolean_Value (Value.labelOffsetSupport.Value);
         end if;
         Handler.End_Object;
      end Write_parameterInformation_OfsignatureInformation_OfSignatureHelpClientCapabilities;

   begin
      Handler.Start_Object;
      if Value.dynamicRegistration.Is_Set then
         Handler.Key_Name ("dynamicRegistration");
         Handler.Boolean_Value (Value.dynamicRegistration.Value);
      end if;
      if Value.signatureInformation.Is_Set then
         Handler.Key_Name ("signatureInformation");
         Write_signatureInformation_OfSignatureHelpClientCapabilities
           (Handler, Value.signatureInformation.Value);
      end if;
      if Value.contextSupport.Is_Set then
         Handler.Key_Name ("contextSupport");
         Handler.Boolean_Value (Value.contextSupport.Value);
      end if;
      Handler.End_Object;
   end Write_SignatureHelpClientCapabilities;

   procedure Write_WorkDoneProgressCancelParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.WorkDoneProgressCancelParams) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("token");
      Write_ProgressToken (Handler, Value.token);
      Handler.End_Object;
   end Write_WorkDoneProgressCancelParams;

   procedure Write_NotebookDocumentSyncClientCapabilities
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.NotebookDocumentSyncClientCapabilities) is
   begin
      Handler.Start_Object;
      if Value.dynamicRegistration.Is_Set then
         Handler.Key_Name ("dynamicRegistration");
         Handler.Boolean_Value (Value.dynamicRegistration.Value);
      end if;
      if Value.executionSummarySupport.Is_Set then
         Handler.Key_Name ("executionSummarySupport");
         Handler.Boolean_Value (Value.executionSummarySupport.Value);
      end if;
      Handler.End_Object;
   end Write_NotebookDocumentSyncClientCapabilities;

   procedure Write_ProgressToken
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.ProgressToken) is
   begin
      case Value.Is_Integer is
         when True =>
            Handler.Integer_Value (Integer'Pos (Value.Integer));
         when False =>
            Handler.String_Value (Value.Virtual_String);
      end case;
   end Write_ProgressToken;

   procedure Write_DiagnosticOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DiagnosticOptions) is
   begin
      Handler.Start_Object;
      if Value.workDoneProgress.Is_Set then
         Handler.Key_Name ("workDoneProgress");
         Handler.Boolean_Value (Value.workDoneProgress.Value);
      end if;
      if not Value.identifier.Is_Null then
         Handler.Key_Name ("identifier");
         Handler.String_Value (Value.identifier);
      end if;
      Handler.Key_Name ("interFileDependencies");
      Handler.Boolean_Value (Value.interFileDependencies);
      Handler.Key_Name ("workspaceDiagnostics");
      Handler.Boolean_Value (Value.workspaceDiagnostics);
      Handler.End_Object;
   end Write_DiagnosticOptions;

   procedure Write_WindowClientCapabilities
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.WindowClientCapabilities) is
   begin
      Handler.Start_Object;
      if Value.workDoneProgress.Is_Set then
         Handler.Key_Name ("workDoneProgress");
         Handler.Boolean_Value (Value.workDoneProgress.Value);
      end if;
      if Value.showMessage.Is_Set then
         Handler.Key_Name ("showMessage");
         Write_ShowMessageRequestClientCapabilities
           (Handler, Value.showMessage.Value);
      end if;
      if Value.showDocument.Is_Set then
         Handler.Key_Name ("showDocument");
         Write_ShowDocumentClientCapabilities
           (Handler, Value.showDocument.Value);
      end if;
      Handler.End_Object;
   end Write_WindowClientCapabilities;

   procedure Write_GeneralClientCapabilities
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.GeneralClientCapabilities) is
      procedure Write_PositionEncodingKind_Set
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.PositionEncodingKind_Set);

      procedure Write_staleRequestSupport_OfGeneralClientCapabilities
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures
           .staleRequestSupport_OfGeneralClientCapabilities);

      procedure Write_PositionEncodingKind_Set
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.PositionEncodingKind_Set) is
      begin
         Handler.Start_Array;
         declare
            Set : LSP.Structures.PositionEncodingKind_Set renames Value;
         begin
            for Value in Set'Range loop
               if Set (Value) then
                  Write_PositionEncodingKind (Handler, Value);
               end if;
            end loop;
         end;
         Handler.End_Array;
      end Write_PositionEncodingKind_Set;

      procedure Write_staleRequestSupport_OfGeneralClientCapabilities
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures
           .staleRequestSupport_OfGeneralClientCapabilities) is
      begin
         Handler.Start_Object;
         Handler.Key_Name ("cancel");
         Handler.Boolean_Value (Value.cancel);
         Handler.Key_Name ("retryOnContentModified");
         Write_Virtual_String_Vector (Handler, Value.retryOnContentModified);
         Handler.End_Object;
      end Write_staleRequestSupport_OfGeneralClientCapabilities;

   begin
      Handler.Start_Object;
      if Value.staleRequestSupport.Is_Set then
         Handler.Key_Name ("staleRequestSupport");
         Write_staleRequestSupport_OfGeneralClientCapabilities
           (Handler, Value.staleRequestSupport.Value);
      end if;
      if Value.regularExpressions.Is_Set then
         Handler.Key_Name ("regularExpressions");
         Write_RegularExpressionsClientCapabilities
           (Handler, Value.regularExpressions.Value);
      end if;
      if Value.markdown.Is_Set then
         Handler.Key_Name ("markdown");
         Write_MarkdownClientCapabilities (Handler, Value.markdown.Value);
      end if;
      if (for some Item of Value.positionEncodings => Item) then
         Handler.Key_Name ("positionEncodings");
         Write_PositionEncodingKind_Set (Handler, Value.positionEncodings);
      end if;
      Handler.End_Object;
   end Write_GeneralClientCapabilities;

   procedure Write_SemanticTokensWorkspaceClientCapabilities
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.SemanticTokensWorkspaceClientCapabilities) is
   begin
      Handler.Start_Object;
      if Value.refreshSupport.Is_Set then
         Handler.Key_Name ("refreshSupport");
         Handler.Boolean_Value (Value.refreshSupport.Value);
      end if;
      Handler.End_Object;
   end Write_SemanticTokensWorkspaceClientCapabilities;

   procedure Write_LinkedEditingRangeClientCapabilities
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.LinkedEditingRangeClientCapabilities) is
   begin
      Handler.Start_Object;
      if Value.dynamicRegistration.Is_Set then
         Handler.Key_Name ("dynamicRegistration");
         Handler.Boolean_Value (Value.dynamicRegistration.Value);
      end if;
      Handler.End_Object;
   end Write_LinkedEditingRangeClientCapabilities;

   procedure Write_SignatureHelp
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.SignatureHelp) is
      procedure Write_SignatureInformation_Vector
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.SignatureInformation_Vector);

      procedure Write_SignatureInformation_Vector
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.SignatureInformation_Vector) is
      begin
         Handler.Start_Array;
         for J in Value.First_Index .. Value.Last_Index loop
            Write_SignatureInformation (Handler, Value (J));
         end loop;
         Handler.End_Array;
      end Write_SignatureInformation_Vector;

   begin
      Handler.Start_Object;
      Handler.Key_Name ("signatures");
      Write_SignatureInformation_Vector (Handler, Value.signatures);
      if Value.activeSignature.Is_Set then
         Handler.Key_Name ("activeSignature");
         Handler.Integer_Value (Integer'Pos (Value.activeSignature.Value));
      end if;
      if Value.activeParameter.Is_Set then
         Handler.Key_Name ("activeParameter");
         Handler.Integer_Value (Integer'Pos (Value.activeParameter.Value));
      end if;
      Handler.End_Object;
   end Write_SignatureHelp;

   procedure Write_DidChangeWatchedFilesParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DidChangeWatchedFilesParams) is
      procedure Write_FileEvent_Vector
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.FileEvent_Vector);

      procedure Write_FileEvent_Vector
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.FileEvent_Vector) is
      begin
         Handler.Start_Array;
         for J in Value.First_Index .. Value.Last_Index loop
            Write_FileEvent (Handler, Value (J));
         end loop;
         Handler.End_Array;
      end Write_FileEvent_Vector;

   begin
      Handler.Start_Object;
      Handler.Key_Name ("changes");
      Write_FileEvent_Vector (Handler, Value.changes);
      Handler.End_Object;
   end Write_DidChangeWatchedFilesParams;

   procedure Write_DidOpenNotebookDocumentParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DidOpenNotebookDocumentParams) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("notebookDocument");
      Write_NotebookDocument (Handler, Value.notebookDocument);
      Handler.Key_Name ("cellTextDocuments");
      Write_TextDocumentItem_Vector (Handler, Value.cellTextDocuments);
      Handler.End_Object;
   end Write_DidOpenNotebookDocumentParams;

   procedure Write_SignatureHelp_Or_Null
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.SignatureHelp_Or_Null) is
   begin
      if Value.Is_Null then
         Handler.Null_Value;
      else
         Write_SignatureHelp (Handler, Value.Value);
      end if;
   end Write_SignatureHelp_Or_Null;

   procedure Write_Diagnostic_Vector
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.Diagnostic_Vector) is
   begin
      Handler.Start_Array;
      for J in Value.First_Index .. Value.Last_Index loop
         Write_Diagnostic (Handler, Value (J));
      end loop;
      Handler.End_Array;
   end Write_Diagnostic_Vector;

   procedure Write_DocumentHighlightKind
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Enumerations.DocumentHighlightKind) is
   begin
      case Value is
         when LSP.Enumerations.Text =>
            Handler.Integer_Value (1);
         when LSP.Enumerations.Read =>
            Handler.Integer_Value (2);
         when LSP.Enumerations.Write =>
            Handler.Integer_Value (3);
      end case;
   end Write_DocumentHighlightKind;

   procedure Write_CodeLens_Vector_Or_Null
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.CodeLens_Vector_Or_Null) is
   begin
      if Value.Is_Empty then
         Handler.Null_Value;
      else
         Write_CodeLens_Vector (Handler, Value);
      end if;
   end Write_CodeLens_Vector_Or_Null;

   procedure Write_ResourceOperationKind
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Enumerations.ResourceOperationKind) is
   begin
      case Value is
         when LSP.Enumerations.Create =>
            Handler.String_Value ("create");
         when LSP.Enumerations.Rename =>
            Handler.String_Value ("rename");
         when LSP.Enumerations.Delete =>
            Handler.String_Value ("delete");
      end case;
   end Write_ResourceOperationKind;

   procedure Write_WorkspaceFolder_Vector_Or_Null
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.WorkspaceFolder_Vector_Or_Null) is
   begin
      if Value.Is_Empty then
         Handler.Null_Value;
      else
         Write_WorkspaceFolder_Vector (Handler, Value);
      end if;
   end Write_WorkspaceFolder_Vector_Or_Null;

   procedure Write_InlineValueClientCapabilities
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.InlineValueClientCapabilities) is
   begin
      Handler.Start_Object;
      if Value.dynamicRegistration.Is_Set then
         Handler.Key_Name ("dynamicRegistration");
         Handler.Boolean_Value (Value.dynamicRegistration.Value);
      end if;
      Handler.End_Object;
   end Write_InlineValueClientCapabilities;

   procedure Write_PrepareRenameResult
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.PrepareRenameResult) is
      procedure Write_PrepareRenameResult_2
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.PrepareRenameResult_2);

      procedure Write_PrepareRenameResult_3
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.PrepareRenameResult_3);

      procedure Write_PrepareRenameResult_2
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.PrepareRenameResult_2) is
      begin
         Handler.Start_Object;
         Handler.Key_Name ("range");
         Write_A_Range (Handler, Value.a_range);
         Handler.Key_Name ("placeholder");
         Handler.String_Value (Value.placeholder);
         Handler.End_Object;
      end Write_PrepareRenameResult_2;

      procedure Write_PrepareRenameResult_3
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.PrepareRenameResult_3) is
      begin
         Handler.Start_Object;
         Handler.Key_Name ("defaultBehavior");
         Handler.Boolean_Value (Value.defaultBehavior);
         Handler.End_Object;
      end Write_PrepareRenameResult_3;

   begin
      case Value.Kind is
         when LSP.Structures.Variant_1 =>
            Write_A_Range (Handler, Value.Variant_1);
         when LSP.Structures.Variant_2 =>
            Write_PrepareRenameResult_2 (Handler, Value.Variant_2);
         when LSP.Structures.Variant_3 =>
            Write_PrepareRenameResult_3 (Handler, Value.Variant_3);
      end case;
   end Write_PrepareRenameResult;

   procedure Write_DocumentLinkParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DocumentLinkParams) is
   begin
      Handler.Start_Object;
      if Value.workDoneToken.Is_Set then
         Handler.Key_Name ("workDoneToken");
         Write_ProgressToken (Handler, Value.workDoneToken.Value);
      end if;
      if Value.partialResultToken.Is_Set then
         Handler.Key_Name ("partialResultToken");
         Write_ProgressToken (Handler, Value.partialResultToken.Value);
      end if;
      Handler.Key_Name ("textDocument");
      Write_TextDocumentIdentifier (Handler, Value.textDocument);
      Handler.End_Object;
   end Write_DocumentLinkParams;

   procedure Write_DocumentLinkClientCapabilities
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DocumentLinkClientCapabilities) is
   begin
      Handler.Start_Object;
      if Value.dynamicRegistration.Is_Set then
         Handler.Key_Name ("dynamicRegistration");
         Handler.Boolean_Value (Value.dynamicRegistration.Value);
      end if;
      if Value.tooltipSupport.Is_Set then
         Handler.Key_Name ("tooltipSupport");
         Handler.Boolean_Value (Value.tooltipSupport.Value);
      end if;
      Handler.End_Object;
   end Write_DocumentLinkClientCapabilities;

   procedure Write_MarkupKind_Vector
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.MarkupKind_Vector) is
   begin
      Handler.Start_Array;
      for J in Value.First_Index .. Value.Last_Index loop
         Write_MarkupKind (Handler, Value (J));
      end loop;
      Handler.End_Array;
   end Write_MarkupKind_Vector;

   procedure Write_CodeActionContext
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.CodeActionContext) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("diagnostics");
      Write_Diagnostic_Vector (Handler, Value.diagnostics);
      if (for some Item of Value.only => Item) then
         Handler.Key_Name ("only");
         Write_CodeActionKind_Set (Handler, Value.only);
      end if;
      if Value.triggerKind.Is_Set then
         Handler.Key_Name ("triggerKind");
         Write_CodeActionTriggerKind (Handler, Value.triggerKind.Value);
      end if;
      Handler.End_Object;
   end Write_CodeActionContext;

   procedure Write_ConfigurationParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.ConfigurationParams) is
      procedure Write_ConfigurationItem_Vector
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.ConfigurationItem_Vector);

      procedure Write_ConfigurationItem_Vector
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.ConfigurationItem_Vector) is
      begin
         Handler.Start_Array;
         for J in Value.First_Index .. Value.Last_Index loop
            Write_ConfigurationItem (Handler, Value (J));
         end loop;
         Handler.End_Array;
      end Write_ConfigurationItem_Vector;

   begin
      Handler.Start_Object;
      Handler.Key_Name ("items");
      Write_ConfigurationItem_Vector (Handler, Value.items);
      Handler.End_Object;
   end Write_ConfigurationParams;

   procedure Write_DidChangeTextDocumentParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DidChangeTextDocumentParams) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("textDocument");
      Write_VersionedTextDocumentIdentifier (Handler, Value.textDocument);
      Handler.Key_Name ("contentChanges");
      Write_TextDocumentContentChangeEvent_Vector
        (Handler, Value.contentChanges);
      Handler.End_Object;
   end Write_DidChangeTextDocumentParams;

   procedure Write_InlayHintRegistrationOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.InlayHintRegistrationOptions) is
      procedure Write_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.DocumentSelector_Or_Null);

      procedure Write_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.DocumentSelector_Or_Null) is
      begin
         if Value.Is_Null then
            Handler.Null_Value;
         else
            Write_DocumentSelector (Handler, Value.Value);
         end if;
      end Write_DocumentSelector_Or_Null;

   begin
      Handler.Start_Object;
      if Value.workDoneProgress.Is_Set then
         Handler.Key_Name ("workDoneProgress");
         Handler.Boolean_Value (Value.workDoneProgress.Value);
      end if;
      if Value.resolveProvider.Is_Set then
         Handler.Key_Name ("resolveProvider");
         Handler.Boolean_Value (Value.resolveProvider.Value);
      end if;
      Handler.Key_Name ("documentSelector");
      Write_DocumentSelector_Or_Null (Handler, Value.Parent.documentSelector);
      if not Value.id.Is_Null then
         Handler.Key_Name ("id");
         Handler.String_Value (Value.id);
      end if;
      Handler.End_Object;
   end Write_InlayHintRegistrationOptions;

   procedure Write_Declaration_Progress_Report
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.Declaration_Progress_Report) is
   begin
      case Value.Kind is
         when LSP.Structures.Variant_1 =>
            Write_Location_Vector (Handler, Value.Variant_1);
         when LSP.Structures.Variant_2 =>
            Write_DeclarationLink_Vector (Handler, Value.Variant_2);
      end case;
   end Write_Declaration_Progress_Report;

   procedure Write_Natural_Vector
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.Natural_Vector) is
   begin
      Handler.Start_Array;
      for J in Value.First_Index .. Value.Last_Index loop
         Handler.Integer_Value (Integer'Pos (Value (J)));
      end loop;
      Handler.End_Array;
   end Write_Natural_Vector;

   procedure Write_CodeDescription
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.CodeDescription) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("href");
      Write_URI (Handler, Value.href);
      Handler.End_Object;
   end Write_CodeDescription;

   procedure Write_Virtual_String_Vector
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.Virtual_String_Vector) is
   begin
      Handler.Start_Array;
      for J in 1 .. Value.Length loop
         Handler.String_Value (Value (J));
      end loop;
      Handler.End_Array;
   end Write_Virtual_String_Vector;

   procedure Write_LSPObject
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.LSPObject) is
   begin
      Handler.Start_Object;
      LSP.Output_Tools.Write_LSPAny (Handler, LSP.Structures.LSPAny (Value));
      Handler.End_Object;
   end Write_LSPObject;

   procedure Write_TextDocumentPositionParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.TextDocumentPositionParams) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("textDocument");
      Write_TextDocumentIdentifier (Handler, Value.textDocument);
      Handler.Key_Name ("position");
      Write_Position (Handler, Value.position);
      Handler.End_Object;
   end Write_TextDocumentPositionParams;

   procedure Write_DidSaveTextDocumentParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DidSaveTextDocumentParams) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("textDocument");
      Write_TextDocumentIdentifier (Handler, Value.textDocument);
      if not Value.text.Is_Null then
         Handler.Key_Name ("text");
         Handler.String_Value (Value.text);
      end if;
      Handler.End_Object;
   end Write_DidSaveTextDocumentParams;

   procedure Write_CodeLensRegistrationOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.CodeLensRegistrationOptions) is
      procedure Write_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.DocumentSelector_Or_Null);

      procedure Write_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.DocumentSelector_Or_Null) is
      begin
         if Value.Is_Null then
            Handler.Null_Value;
         else
            Write_DocumentSelector (Handler, Value.Value);
         end if;
      end Write_DocumentSelector_Or_Null;

   begin
      Handler.Start_Object;
      Handler.Key_Name ("documentSelector");
      Write_DocumentSelector_Or_Null (Handler, Value.Parent.documentSelector);
      if Value.workDoneProgress.Is_Set then
         Handler.Key_Name ("workDoneProgress");
         Handler.Boolean_Value (Value.workDoneProgress.Value);
      end if;
      if Value.resolveProvider.Is_Set then
         Handler.Key_Name ("resolveProvider");
         Handler.Boolean_Value (Value.resolveProvider.Value);
      end if;
      Handler.End_Object;
   end Write_CodeLensRegistrationOptions;

   procedure Write_SelectionRange
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.SelectionRange) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("range");
      Write_A_Range (Handler, Value.a_range);
      if Value.parent.Is_Set then
         Handler.Key_Name ("parent");
         Write_SelectionRange (Handler, Value.parent.Value);
      end if;
      Handler.End_Object;
   end Write_SelectionRange;

   procedure Write_NotebookCellTextDocumentFilter
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.NotebookCellTextDocumentFilter) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("notebook");
      Write_Virtual_String_Or_NotebookDocumentFilter (Handler, Value.notebook);
      if not Value.language.Is_Null then
         Handler.Key_Name ("language");
         Handler.String_Value (Value.language);
      end if;
      Handler.End_Object;
   end Write_NotebookCellTextDocumentFilter;

   procedure Write_WorkspaceFoldersInitializeParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.WorkspaceFoldersInitializeParams) is
   begin
      Handler.Start_Object;
      if Value.workspaceFolders.Is_Set then
         Handler.Key_Name ("workspaceFolders");
         Write_WorkspaceFolder_Vector_Or_Null
           (Handler, Value.workspaceFolders.Value);
      end if;
      Handler.End_Object;
   end Write_WorkspaceFoldersInitializeParams;

   procedure Write_ShowMessageParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.ShowMessageParams) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("type");
      Write_MessageType (Handler, Value.a_type);
      Handler.Key_Name ("message");
      Handler.String_Value (Value.message);
      Handler.End_Object;
   end Write_ShowMessageParams;

   procedure Write_FileSystemWatcher
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.FileSystemWatcher) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("globPattern");
      Write_GlobPattern (Handler, Value.globPattern);
      if Value.kind.Is_Set then
         Handler.Key_Name ("kind");
         Write_WatchKind (Handler, Value.kind.Value);
      end if;
      Handler.End_Object;
   end Write_FileSystemWatcher;

   procedure Write_CodeActionKind_Set
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.CodeActionKind_Set) is
   begin
      Handler.Start_Array;
      declare
         Set : LSP.Structures.CodeActionKind_Set renames Value;
      begin
         for Value in Set'Range loop
            if Set (Value) then
               Write_CodeActionKind (Handler, Value);
            end if;
         end loop;
      end;
      Handler.End_Array;
   end Write_CodeActionKind_Set;

   procedure Write_NotebookDocumentSyncOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.NotebookDocumentSyncOptions) is
      procedure Write_cells_OfnotebookSelector_OfNotebookDocumentSyncOptions_Item_Item
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures
           .cells_OfnotebookSelector_OfNotebookDocumentSyncOptions_Item_Item);

      procedure Write_notebookSelector_OfNotebookDocumentSyncOptions
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures
           .notebookSelector_OfNotebookDocumentSyncOptions);

      procedure Write_cells_OfnotebookSelector_OfNotebookDocumentSyncOptions_Item
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures
           .cells_OfnotebookSelector_OfNotebookDocumentSyncOptions_Item);

      procedure Write_notebookSelector_OfNotebookDocumentSyncOptions_Item
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures
           .notebookSelector_OfNotebookDocumentSyncOptions_Item);

      procedure Write_cells_OfnotebookSelector_OfNotebookDocumentSyncOptions_Item_Item
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures
           .cells_OfnotebookSelector_OfNotebookDocumentSyncOptions_Item_Item) is
      begin
         Handler.Start_Object;
         Handler.Key_Name ("language");
         Handler.String_Value (Value.language);
         Handler.End_Object;
      end Write_cells_OfnotebookSelector_OfNotebookDocumentSyncOptions_Item_Item;

      procedure Write_notebookSelector_OfNotebookDocumentSyncOptions
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures
           .notebookSelector_OfNotebookDocumentSyncOptions) is
      begin
         Handler.Start_Array;
         for J in Value.First_Index .. Value.Last_Index loop
            Write_notebookSelector_OfNotebookDocumentSyncOptions_Item
              (Handler, Value (J));
         end loop;
         Handler.End_Array;
      end Write_notebookSelector_OfNotebookDocumentSyncOptions;

      procedure Write_cells_OfnotebookSelector_OfNotebookDocumentSyncOptions_Item
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures
           .cells_OfnotebookSelector_OfNotebookDocumentSyncOptions_Item) is
      begin
         Handler.Start_Array;
         for J in Value.First_Index .. Value.Last_Index loop
            Write_cells_OfnotebookSelector_OfNotebookDocumentSyncOptions_Item_Item
              (Handler, Value (J));
         end loop;
         Handler.End_Array;
      end Write_cells_OfnotebookSelector_OfNotebookDocumentSyncOptions_Item;

      procedure Write_notebookSelector_OfNotebookDocumentSyncOptions_Item
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures
           .notebookSelector_OfNotebookDocumentSyncOptions_Item) is
      begin
         Handler.Start_Object;
         if Value.notebook.Is_Set then
            Handler.Key_Name ("notebook");
            Write_Virtual_String_Or_NotebookDocumentFilter
              (Handler, Value.notebook.Value);
         end if;
         if not Value.cells.Is_Empty then
            Handler.Key_Name ("cells");
            Write_cells_OfnotebookSelector_OfNotebookDocumentSyncOptions_Item
              (Handler, Value.cells);
         end if;
         Handler.End_Object;
      end Write_notebookSelector_OfNotebookDocumentSyncOptions_Item;

   begin
      Handler.Start_Object;
      Handler.Key_Name ("notebookSelector");
      Write_notebookSelector_OfNotebookDocumentSyncOptions
        (Handler, Value.notebookSelector);
      if Value.save.Is_Set then
         Handler.Key_Name ("save");
         Handler.Boolean_Value (Value.save.Value);
      end if;
      Handler.End_Object;
   end Write_NotebookDocumentSyncOptions;

   procedure Write_CodeActionRegistrationOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.CodeActionRegistrationOptions) is
      procedure Write_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.DocumentSelector_Or_Null);

      procedure Write_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.DocumentSelector_Or_Null) is
      begin
         if Value.Is_Null then
            Handler.Null_Value;
         else
            Write_DocumentSelector (Handler, Value.Value);
         end if;
      end Write_DocumentSelector_Or_Null;

   begin
      Handler.Start_Object;
      Handler.Key_Name ("documentSelector");
      Write_DocumentSelector_Or_Null (Handler, Value.Parent.documentSelector);
      if Value.workDoneProgress.Is_Set then
         Handler.Key_Name ("workDoneProgress");
         Handler.Boolean_Value (Value.workDoneProgress.Value);
      end if;
      if (for some Item of Value.codeActionKinds => Item) then
         Handler.Key_Name ("codeActionKinds");
         Write_CodeActionKind_Set (Handler, Value.codeActionKinds);
      end if;
      if Value.resolveProvider.Is_Set then
         Handler.Key_Name ("resolveProvider");
         Handler.Boolean_Value (Value.resolveProvider.Value);
      end if;
      Handler.End_Object;
   end Write_CodeActionRegistrationOptions;

   procedure Write_NotebookDocumentSyncRegistrationOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.NotebookDocumentSyncRegistrationOptions) is
      procedure Write_cells_OfnotebookSelector_OfNotebookDocumentSyncOptions_Item_Item
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures
           .cells_OfnotebookSelector_OfNotebookDocumentSyncOptions_Item_Item);

      procedure Write_notebookSelector_OfNotebookDocumentSyncOptions
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures
           .notebookSelector_OfNotebookDocumentSyncOptions);

      procedure Write_cells_OfnotebookSelector_OfNotebookDocumentSyncOptions_Item
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures
           .cells_OfnotebookSelector_OfNotebookDocumentSyncOptions_Item);

      procedure Write_notebookSelector_OfNotebookDocumentSyncOptions_Item
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures
           .notebookSelector_OfNotebookDocumentSyncOptions_Item);

      procedure Write_cells_OfnotebookSelector_OfNotebookDocumentSyncOptions_Item_Item
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures
           .cells_OfnotebookSelector_OfNotebookDocumentSyncOptions_Item_Item) is
      begin
         Handler.Start_Object;
         Handler.Key_Name ("language");
         Handler.String_Value (Value.language);
         Handler.End_Object;
      end Write_cells_OfnotebookSelector_OfNotebookDocumentSyncOptions_Item_Item;

      procedure Write_notebookSelector_OfNotebookDocumentSyncOptions
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures
           .notebookSelector_OfNotebookDocumentSyncOptions) is
      begin
         Handler.Start_Array;
         for J in Value.First_Index .. Value.Last_Index loop
            Write_notebookSelector_OfNotebookDocumentSyncOptions_Item
              (Handler, Value (J));
         end loop;
         Handler.End_Array;
      end Write_notebookSelector_OfNotebookDocumentSyncOptions;

      procedure Write_cells_OfnotebookSelector_OfNotebookDocumentSyncOptions_Item
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures
           .cells_OfnotebookSelector_OfNotebookDocumentSyncOptions_Item) is
      begin
         Handler.Start_Array;
         for J in Value.First_Index .. Value.Last_Index loop
            Write_cells_OfnotebookSelector_OfNotebookDocumentSyncOptions_Item_Item
              (Handler, Value (J));
         end loop;
         Handler.End_Array;
      end Write_cells_OfnotebookSelector_OfNotebookDocumentSyncOptions_Item;

      procedure Write_notebookSelector_OfNotebookDocumentSyncOptions_Item
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures
           .notebookSelector_OfNotebookDocumentSyncOptions_Item) is
      begin
         Handler.Start_Object;
         if Value.notebook.Is_Set then
            Handler.Key_Name ("notebook");
            Write_Virtual_String_Or_NotebookDocumentFilter
              (Handler, Value.notebook.Value);
         end if;
         if not Value.cells.Is_Empty then
            Handler.Key_Name ("cells");
            Write_cells_OfnotebookSelector_OfNotebookDocumentSyncOptions_Item
              (Handler, Value.cells);
         end if;
         Handler.End_Object;
      end Write_notebookSelector_OfNotebookDocumentSyncOptions_Item;

   begin
      Handler.Start_Object;
      Handler.Key_Name ("notebookSelector");
      Write_notebookSelector_OfNotebookDocumentSyncOptions
        (Handler, Value.notebookSelector);
      if Value.save.Is_Set then
         Handler.Key_Name ("save");
         Handler.Boolean_Value (Value.save.Value);
      end if;
      if not Value.id.Is_Null then
         Handler.Key_Name ("id");
         Handler.String_Value (Value.id);
      end if;
      Handler.End_Object;
   end Write_NotebookDocumentSyncRegistrationOptions;

   procedure Write_WorkspaceUnchangedDocumentDiagnosticReport
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.WorkspaceUnchangedDocumentDiagnosticReport) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("kind");
      Handler.String_Value ("unchanged");
      Handler.Key_Name ("resultId");
      Handler.String_Value (Value.resultId);
      Handler.Key_Name ("uri");
      Handler.String_Value (Value.uri);
      Handler.Key_Name ("version");
      Write_Integer_Or_Null (Handler, Value.version);
      Handler.End_Object;
   end Write_WorkspaceUnchangedDocumentDiagnosticReport;

   procedure Write_ReferenceParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.ReferenceParams) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("textDocument");
      Write_TextDocumentIdentifier (Handler, Value.textDocument);
      Handler.Key_Name ("position");
      Write_Position (Handler, Value.position);
      if Value.workDoneToken.Is_Set then
         Handler.Key_Name ("workDoneToken");
         Write_ProgressToken (Handler, Value.workDoneToken.Value);
      end if;
      if Value.partialResultToken.Is_Set then
         Handler.Key_Name ("partialResultToken");
         Write_ProgressToken (Handler, Value.partialResultToken.Value);
      end if;
      Handler.Key_Name ("context");
      Write_ReferenceContext (Handler, Value.context);
      Handler.End_Object;
   end Write_ReferenceParams;

   procedure Write_HoverClientCapabilities
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.HoverClientCapabilities) is
   begin
      Handler.Start_Object;
      if Value.dynamicRegistration.Is_Set then
         Handler.Key_Name ("dynamicRegistration");
         Handler.Boolean_Value (Value.dynamicRegistration.Value);
      end if;
      if not Value.contentFormat.Is_Empty then
         Handler.Key_Name ("contentFormat");
         Write_MarkupKind_Vector (Handler, Value.contentFormat);
      end if;
      Handler.End_Object;
   end Write_HoverClientCapabilities;

   procedure Write_RenameClientCapabilities
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.RenameClientCapabilities) is
   begin
      Handler.Start_Object;
      if Value.dynamicRegistration.Is_Set then
         Handler.Key_Name ("dynamicRegistration");
         Handler.Boolean_Value (Value.dynamicRegistration.Value);
      end if;
      if Value.prepareSupport.Is_Set then
         Handler.Key_Name ("prepareSupport");
         Handler.Boolean_Value (Value.prepareSupport.Value);
      end if;
      if Value.prepareSupportDefaultBehavior.Is_Set then
         Handler.Key_Name ("prepareSupportDefaultBehavior");
         Write_PrepareSupportDefaultBehavior
           (Handler, Value.prepareSupportDefaultBehavior.Value);
      end if;
      if Value.honorsChangeAnnotations.Is_Set then
         Handler.Key_Name ("honorsChangeAnnotations");
         Handler.Boolean_Value (Value.honorsChangeAnnotations.Value);
      end if;
      Handler.End_Object;
   end Write_RenameClientCapabilities;

   procedure Write_DidChangeConfigurationParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DidChangeConfigurationParams) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("settings");
      Write_LSPAny (Handler, Value.settings);
      Handler.End_Object;
   end Write_DidChangeConfigurationParams;

   procedure Write_DefinitionOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DefinitionOptions) is
   begin
      Handler.Start_Object;
      if Value.workDoneProgress.Is_Set then
         Handler.Key_Name ("workDoneProgress");
         Handler.Boolean_Value (Value.workDoneProgress.Value);
      end if;
      Handler.End_Object;
   end Write_DefinitionOptions;

   procedure Write_Hover_Or_Null
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.Hover_Or_Null) is
   begin
      if Value.Is_Null then
         Handler.Null_Value;
      else
         Write_Hover (Handler, Value.Value);
      end if;
   end Write_Hover_Or_Null;

   procedure Write_InlayHintKind
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Enumerations.InlayHintKind) is
   begin
      case Value is
         when LSP.Enumerations.A_Type =>
            Handler.Integer_Value (1);
         when LSP.Enumerations.Parameter =>
            Handler.Integer_Value (2);
      end case;
   end Write_InlayHintKind;

   procedure Write_symbolKind_OfWorkspaceSymbolClientCapabilities
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value : LSP.Structures.symbolKind_OfWorkspaceSymbolClientCapabilities) is
   begin
      Handler.Start_Object;
      if (for some Item of Value.valueSet => Item) then
         Handler.Key_Name ("valueSet");
         Write_SymbolKind_Set (Handler, Value.valueSet);
      end if;
      Handler.End_Object;
   end Write_symbolKind_OfWorkspaceSymbolClientCapabilities;

   procedure Write_CallHierarchyOutgoingCall_Vector_Or_Null
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.CallHierarchyOutgoingCall_Vector_Or_Null) is
   begin
      if Value.Is_Empty then
         Handler.Null_Value;
      else
         Write_CallHierarchyOutgoingCall_Vector (Handler, Value);
      end if;
   end Write_CallHierarchyOutgoingCall_Vector_Or_Null;

   procedure Write_FailureHandlingKind
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Enumerations.FailureHandlingKind) is
   begin
      case Value is
         when LSP.Enumerations.An_Abort =>
            Handler.String_Value ("abort");
         when LSP.Enumerations.Transactional =>
            Handler.String_Value ("transactional");
         when LSP.Enumerations.TextOnlyTransactional =>
            Handler.String_Value ("textOnlyTransactional");
         when LSP.Enumerations.Undo =>
            Handler.String_Value ("undo");
      end case;
   end Write_FailureHandlingKind;

   procedure Write_FileOperationOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.FileOperationOptions) is
   begin
      Handler.Start_Object;
      if Value.didCreate.Is_Set then
         Handler.Key_Name ("didCreate");
         Write_FileOperationRegistrationOptions
           (Handler, Value.didCreate.Value);
      end if;
      if Value.willCreate.Is_Set then
         Handler.Key_Name ("willCreate");
         Write_FileOperationRegistrationOptions
           (Handler, Value.willCreate.Value);
      end if;
      if Value.didRename.Is_Set then
         Handler.Key_Name ("didRename");
         Write_FileOperationRegistrationOptions
           (Handler, Value.didRename.Value);
      end if;
      if Value.willRename.Is_Set then
         Handler.Key_Name ("willRename");
         Write_FileOperationRegistrationOptions
           (Handler, Value.willRename.Value);
      end if;
      if Value.didDelete.Is_Set then
         Handler.Key_Name ("didDelete");
         Write_FileOperationRegistrationOptions
           (Handler, Value.didDelete.Value);
      end if;
      if Value.willDelete.Is_Set then
         Handler.Key_Name ("willDelete");
         Write_FileOperationRegistrationOptions
           (Handler, Value.willDelete.Value);
      end if;
      Handler.End_Object;
   end Write_FileOperationOptions;

   procedure Write_ShowDocumentResult
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.ShowDocumentResult) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("success");
      Handler.Boolean_Value (Value.success);
      Handler.End_Object;
   end Write_ShowDocumentResult;

   procedure Write_TypeDefinitionParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.TypeDefinitionParams) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("textDocument");
      Write_TextDocumentIdentifier (Handler, Value.textDocument);
      Handler.Key_Name ("position");
      Write_Position (Handler, Value.position);
      if Value.workDoneToken.Is_Set then
         Handler.Key_Name ("workDoneToken");
         Write_ProgressToken (Handler, Value.workDoneToken.Value);
      end if;
      if Value.partialResultToken.Is_Set then
         Handler.Key_Name ("partialResultToken");
         Write_ProgressToken (Handler, Value.partialResultToken.Value);
      end if;
      Handler.End_Object;
   end Write_TypeDefinitionParams;

   procedure Write_DocumentSelector
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DocumentSelector) is
      procedure Write_Virtual_String_Or_DocumentFilter
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.Virtual_String_Or_DocumentFilter);

      procedure Write_Virtual_String_Or_DocumentFilter
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.Virtual_String_Or_DocumentFilter) is
      begin
         case Value.Is_Virtual_String is
            when True =>
               Handler.String_Value (Value.Virtual_String);
            when False =>
               Write_DocumentFilter (Handler, Value.DocumentFilter);
         end case;
      end Write_Virtual_String_Or_DocumentFilter;

   begin
      Handler.Start_Array;
      for J in Value.First_Index .. Value.Last_Index loop
         Write_Virtual_String_Or_DocumentFilter (Handler, Value (J));
      end loop;
      Handler.End_Array;
   end Write_DocumentSelector;

   procedure Write_CompletionClientCapabilities
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.CompletionClientCapabilities) is
      procedure Write_CompletionItemKind_Set
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.CompletionItemKind_Set);

      procedure Write_completionItem_OfCompletionClientCapabilities
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value : LSP.Structures.completionItem_OfCompletionClientCapabilities);

      procedure Write_completionList_OfCompletionClientCapabilities
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value : LSP.Structures.completionList_OfCompletionClientCapabilities);

      procedure Write_tagSupport_OfcompletionItem_OfCompletionClientCapabilities
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures
           .tagSupport_OfcompletionItem_OfCompletionClientCapabilities);

      procedure Write_InsertTextMode_Set
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.InsertTextMode_Set);

      procedure Write_completionItemKind_OfCompletionClientCapabilities
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures
           .completionItemKind_OfCompletionClientCapabilities);

      procedure Write_insertTextModeSupport_OfcompletionItem_OfCompletionClientCapabilities
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures
           .insertTextModeSupport_OfcompletionItem_OfCompletionClientCapabilities);

      procedure Write_CompletionItemKind_Set
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.CompletionItemKind_Set) is
      begin
         Handler.Start_Array;
         declare
            Set : LSP.Structures.CompletionItemKind_Set renames Value;
         begin
            for Value in Set'Range loop
               if Set (Value) then
                  Write_CompletionItemKind (Handler, Value);
               end if;
            end loop;
         end;
         Handler.End_Array;
      end Write_CompletionItemKind_Set;

      procedure Write_completionItem_OfCompletionClientCapabilities
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures
           .completionItem_OfCompletionClientCapabilities) is
      begin
         Handler.Start_Object;
         if Value.snippetSupport.Is_Set then
            Handler.Key_Name ("snippetSupport");
            Handler.Boolean_Value (Value.snippetSupport.Value);
         end if;
         if Value.commitCharactersSupport.Is_Set then
            Handler.Key_Name ("commitCharactersSupport");
            Handler.Boolean_Value (Value.commitCharactersSupport.Value);
         end if;
         if not Value.documentationFormat.Is_Empty then
            Handler.Key_Name ("documentationFormat");
            Write_MarkupKind_Vector (Handler, Value.documentationFormat);
         end if;
         if Value.deprecatedSupport.Is_Set then
            Handler.Key_Name ("deprecatedSupport");
            Handler.Boolean_Value (Value.deprecatedSupport.Value);
         end if;
         if Value.preselectSupport.Is_Set then
            Handler.Key_Name ("preselectSupport");
            Handler.Boolean_Value (Value.preselectSupport.Value);
         end if;
         if Value.tagSupport.Is_Set then
            Handler.Key_Name ("tagSupport");
            Write_tagSupport_OfcompletionItem_OfCompletionClientCapabilities
              (Handler, Value.tagSupport.Value);
         end if;
         if Value.insertReplaceSupport.Is_Set then
            Handler.Key_Name ("insertReplaceSupport");
            Handler.Boolean_Value (Value.insertReplaceSupport.Value);
         end if;
         if Value.resolveSupport.Is_Set then
            Handler.Key_Name ("resolveSupport");
            Write_resolveSupport_OfWorkspaceSymbolClientCapabilities
              (Handler, Value.resolveSupport.Value);
         end if;
         if Value.insertTextModeSupport.Is_Set then
            Handler.Key_Name ("insertTextModeSupport");
            Write_insertTextModeSupport_OfcompletionItem_OfCompletionClientCapabilities
              (Handler, Value.insertTextModeSupport.Value);
         end if;
         if Value.labelDetailsSupport.Is_Set then
            Handler.Key_Name ("labelDetailsSupport");
            Handler.Boolean_Value (Value.labelDetailsSupport.Value);
         end if;
         Handler.End_Object;
      end Write_completionItem_OfCompletionClientCapabilities;

      procedure Write_completionList_OfCompletionClientCapabilities
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures
           .completionList_OfCompletionClientCapabilities) is
      begin
         Handler.Start_Object;
         if not Value.itemDefaults.Is_Empty then
            Handler.Key_Name ("itemDefaults");
            Write_Virtual_String_Vector (Handler, Value.itemDefaults);
         end if;
         Handler.End_Object;
      end Write_completionList_OfCompletionClientCapabilities;

      procedure Write_tagSupport_OfcompletionItem_OfCompletionClientCapabilities
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures
           .tagSupport_OfcompletionItem_OfCompletionClientCapabilities) is
      begin
         Handler.Start_Object;
         Handler.Key_Name ("valueSet");
         Write_CompletionItemTag_Set (Handler, Value.valueSet);
         Handler.End_Object;
      end Write_tagSupport_OfcompletionItem_OfCompletionClientCapabilities;

      procedure Write_InsertTextMode_Set
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.InsertTextMode_Set) is
      begin
         Handler.Start_Array;
         declare
            Set : LSP.Structures.InsertTextMode_Set renames Value;
         begin
            for Value in Set'Range loop
               if Set (Value) then
                  Write_InsertTextMode (Handler, Value);
               end if;
            end loop;
         end;
         Handler.End_Array;
      end Write_InsertTextMode_Set;

      procedure Write_completionItemKind_OfCompletionClientCapabilities
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures
           .completionItemKind_OfCompletionClientCapabilities) is
      begin
         Handler.Start_Object;
         if (for some Item of Value.valueSet => Item) then
            Handler.Key_Name ("valueSet");
            Write_CompletionItemKind_Set (Handler, Value.valueSet);
         end if;
         Handler.End_Object;
      end Write_completionItemKind_OfCompletionClientCapabilities;

      procedure Write_insertTextModeSupport_OfcompletionItem_OfCompletionClientCapabilities
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures
           .insertTextModeSupport_OfcompletionItem_OfCompletionClientCapabilities) is
      begin
         Handler.Start_Object;
         Handler.Key_Name ("valueSet");
         Write_InsertTextMode_Set (Handler, Value.valueSet);
         Handler.End_Object;
      end Write_insertTextModeSupport_OfcompletionItem_OfCompletionClientCapabilities;

   begin
      Handler.Start_Object;
      if Value.dynamicRegistration.Is_Set then
         Handler.Key_Name ("dynamicRegistration");
         Handler.Boolean_Value (Value.dynamicRegistration.Value);
      end if;
      if Value.completionItem.Is_Set then
         Handler.Key_Name ("completionItem");
         Write_completionItem_OfCompletionClientCapabilities
           (Handler, Value.completionItem.Value);
      end if;
      if Value.completionItemKind.Is_Set then
         Handler.Key_Name ("completionItemKind");
         Write_completionItemKind_OfCompletionClientCapabilities
           (Handler, Value.completionItemKind.Value);
      end if;
      if Value.insertTextMode.Is_Set then
         Handler.Key_Name ("insertTextMode");
         Write_InsertTextMode (Handler, Value.insertTextMode.Value);
      end if;
      if Value.contextSupport.Is_Set then
         Handler.Key_Name ("contextSupport");
         Handler.Boolean_Value (Value.contextSupport.Value);
      end if;
      if Value.completionList.Is_Set then
         Handler.Key_Name ("completionList");
         Write_completionList_OfCompletionClientCapabilities
           (Handler, Value.completionList.Value);
      end if;
      Handler.End_Object;
   end Write_CompletionClientCapabilities;

   procedure Write_CodeLensWorkspaceClientCapabilities
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.CodeLensWorkspaceClientCapabilities) is
   begin
      Handler.Start_Object;
      if Value.refreshSupport.Is_Set then
         Handler.Key_Name ("refreshSupport");
         Handler.Boolean_Value (Value.refreshSupport.Value);
      end if;
      Handler.End_Object;
   end Write_CodeLensWorkspaceClientCapabilities;

   procedure Write_RenameOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.RenameOptions) is
   begin
      Handler.Start_Object;
      if Value.workDoneProgress.Is_Set then
         Handler.Key_Name ("workDoneProgress");
         Handler.Boolean_Value (Value.workDoneProgress.Value);
      end if;
      if Value.prepareProvider.Is_Set then
         Handler.Key_Name ("prepareProvider");
         Handler.Boolean_Value (Value.prepareProvider.Value);
      end if;
      Handler.End_Object;
   end Write_RenameOptions;

   procedure Write_SymbolKind_Set
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.SymbolKind_Set) is
   begin
      Handler.Start_Array;
      declare
         Set : LSP.Structures.SymbolKind_Set renames Value;
      begin
         for Value in Set'Range loop
            if Set (Value) then
               Write_SymbolKind (Handler, Value);
            end if;
         end loop;
      end;
      Handler.End_Array;
   end Write_SymbolKind_Set;

   procedure Write_DidChangeConfigurationClientCapabilities
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DidChangeConfigurationClientCapabilities) is
   begin
      Handler.Start_Object;
      if Value.dynamicRegistration.Is_Set then
         Handler.Key_Name ("dynamicRegistration");
         Handler.Boolean_Value (Value.dynamicRegistration.Value);
      end if;
      Handler.End_Object;
   end Write_DidChangeConfigurationClientCapabilities;

   procedure Write_DocumentHighlightRegistrationOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DocumentHighlightRegistrationOptions) is
      procedure Write_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.DocumentSelector_Or_Null);

      procedure Write_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.DocumentSelector_Or_Null) is
      begin
         if Value.Is_Null then
            Handler.Null_Value;
         else
            Write_DocumentSelector (Handler, Value.Value);
         end if;
      end Write_DocumentSelector_Or_Null;

   begin
      Handler.Start_Object;
      Handler.Key_Name ("documentSelector");
      Write_DocumentSelector_Or_Null (Handler, Value.Parent.documentSelector);
      if Value.workDoneProgress.Is_Set then
         Handler.Key_Name ("workDoneProgress");
         Handler.Boolean_Value (Value.workDoneProgress.Value);
      end if;
      Handler.End_Object;
   end Write_DocumentHighlightRegistrationOptions;

   procedure Write_DeclarationLink
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DeclarationLink) renames
     Write_LocationLink;

   procedure Write_T
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.T) is
   begin
      Handler.Start_Object;
      null;
      Handler.End_Object;
   end Write_T;

   procedure Write_FullDocumentDiagnosticReport
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.FullDocumentDiagnosticReport) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("kind");
      Handler.String_Value ("full");
      if not Value.resultId.Is_Null then
         Handler.Key_Name ("resultId");
         Handler.String_Value (Value.resultId);
      end if;
      Handler.Key_Name ("items");
      Write_Diagnostic_Vector (Handler, Value.items);
      Handler.End_Object;
   end Write_FullDocumentDiagnosticReport;

   procedure Write_RenameParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.RenameParams) is
   begin
      Handler.Start_Object;
      if Value.workDoneToken.Is_Set then
         Handler.Key_Name ("workDoneToken");
         Write_ProgressToken (Handler, Value.workDoneToken.Value);
      end if;
      Handler.Key_Name ("textDocument");
      Write_TextDocumentIdentifier (Handler, Value.textDocument);
      Handler.Key_Name ("position");
      Write_Position (Handler, Value.position);
      Handler.Key_Name ("newName");
      Handler.String_Value (Value.newName);
      Handler.End_Object;
   end Write_RenameParams;

   procedure Write_DocumentSymbolRegistrationOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DocumentSymbolRegistrationOptions) is
      procedure Write_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.DocumentSelector_Or_Null);

      procedure Write_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.DocumentSelector_Or_Null) is
      begin
         if Value.Is_Null then
            Handler.Null_Value;
         else
            Write_DocumentSelector (Handler, Value.Value);
         end if;
      end Write_DocumentSelector_Or_Null;

   begin
      Handler.Start_Object;
      Handler.Key_Name ("documentSelector");
      Write_DocumentSelector_Or_Null (Handler, Value.Parent.documentSelector);
      if Value.workDoneProgress.Is_Set then
         Handler.Key_Name ("workDoneProgress");
         Handler.Boolean_Value (Value.workDoneProgress.Value);
      end if;
      if not Value.label.Is_Null then
         Handler.Key_Name ("label");
         Handler.String_Value (Value.label);
      end if;
      Handler.End_Object;
   end Write_DocumentSymbolRegistrationOptions;

   procedure Write_LSPAny_Or_Null
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.LSPAny_Or_Null) is
   begin
      if Value.Is_Null then
         Handler.Null_Value;
      else
         Write_LSPAny (Handler, Value.Value);
      end if;
   end Write_LSPAny_Or_Null;

   procedure Write_DeclarationRegistrationOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DeclarationRegistrationOptions) is
      procedure Write_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.DocumentSelector_Or_Null);

      procedure Write_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.DocumentSelector_Or_Null) is
      begin
         if Value.Is_Null then
            Handler.Null_Value;
         else
            Write_DocumentSelector (Handler, Value.Value);
         end if;
      end Write_DocumentSelector_Or_Null;

   begin
      Handler.Start_Object;
      if Value.workDoneProgress.Is_Set then
         Handler.Key_Name ("workDoneProgress");
         Handler.Boolean_Value (Value.workDoneProgress.Value);
      end if;
      Handler.Key_Name ("documentSelector");
      Write_DocumentSelector_Or_Null (Handler, Value.Parent.documentSelector);
      if not Value.id.Is_Null then
         Handler.Key_Name ("id");
         Handler.String_Value (Value.id);
      end if;
      Handler.End_Object;
   end Write_DeclarationRegistrationOptions;

   procedure Write_WorkspaceSymbol
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.WorkspaceSymbol) is
      procedure Write_Location_Or_Something
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.Location_Or_Something);

      procedure Write_Location_Or_Something
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.Location_Or_Something) is
      begin
         case Value.Is_Location is
            when True =>
               Write_Location (Handler, Value.Location);
            when False =>
               Handler.Start_Object;
               Handler.Key_Name ("uri");
               Handler.String_Value (Value.uri);
               Handler.End_Object;
         end case;
      end Write_Location_Or_Something;

   begin
      Handler.Start_Object;
      Handler.Key_Name ("name");
      Handler.String_Value (Value.name);
      Handler.Key_Name ("kind");
      Write_SymbolKind (Handler, Value.kind);
      if (for some Item of Value.tags => Item) then
         Handler.Key_Name ("tags");
         Write_SymbolTag_Set (Handler, Value.tags);
      end if;
      if not Value.containerName.Is_Null then
         Handler.Key_Name ("containerName");
         Handler.String_Value (Value.containerName);
      end if;
      Handler.Key_Name ("location");
      Write_Location_Or_Something (Handler, Value.location);
      if not Value.data.Is_Empty then
         Handler.Key_Name ("data");
         Write_LSPAny (Handler, Value.data);
      end if;
      Handler.End_Object;
   end Write_WorkspaceSymbol;

   procedure Write_TypeHierarchyRegistrationOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.TypeHierarchyRegistrationOptions) is
      procedure Write_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.DocumentSelector_Or_Null);

      procedure Write_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.DocumentSelector_Or_Null) is
      begin
         if Value.Is_Null then
            Handler.Null_Value;
         else
            Write_DocumentSelector (Handler, Value.Value);
         end if;
      end Write_DocumentSelector_Or_Null;

   begin
      Handler.Start_Object;
      Handler.Key_Name ("documentSelector");
      Write_DocumentSelector_Or_Null (Handler, Value.Parent.documentSelector);
      if Value.workDoneProgress.Is_Set then
         Handler.Key_Name ("workDoneProgress");
         Handler.Boolean_Value (Value.workDoneProgress.Value);
      end if;
      if not Value.id.Is_Null then
         Handler.Key_Name ("id");
         Handler.String_Value (Value.id);
      end if;
      Handler.End_Object;
   end Write_TypeHierarchyRegistrationOptions;

   procedure Write_DeleteFileOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DeleteFileOptions) is
   begin
      Handler.Start_Object;
      if Value.recursive.Is_Set then
         Handler.Key_Name ("recursive");
         Handler.Boolean_Value (Value.recursive.Value);
      end if;
      if Value.ignoreIfNotExists.Is_Set then
         Handler.Key_Name ("ignoreIfNotExists");
         Handler.Boolean_Value (Value.ignoreIfNotExists.Value);
      end if;
      Handler.End_Object;
   end Write_DeleteFileOptions;

   procedure Write_WatchKind
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Enumerations.WatchKind) is
   begin
      case Value is
         when LSP.Enumerations.Create =>
            Handler.Integer_Value (1);
         when LSP.Enumerations.Change =>
            Handler.Integer_Value (2);
         when LSP.Enumerations.Delete =>
            Handler.Integer_Value (4);
      end case;
   end Write_WatchKind;

   procedure Write_DiagnosticSeverity
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Enumerations.DiagnosticSeverity) is
   begin
      case Value is
         when LSP.Enumerations.Error =>
            Handler.Integer_Value (1);
         when LSP.Enumerations.Warning =>
            Handler.Integer_Value (2);
         when LSP.Enumerations.Information =>
            Handler.Integer_Value (3);
         when LSP.Enumerations.Hint =>
            Handler.Integer_Value (4);
      end case;
   end Write_DiagnosticSeverity;

   procedure Write_LSPAny_Vector
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.LSPAny_Vector) renames
     LSP.Output_Tools.Write_LSPAny;

   procedure Write_PrepareRenameParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.PrepareRenameParams) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("textDocument");
      Write_TextDocumentIdentifier (Handler, Value.textDocument);
      Handler.Key_Name ("position");
      Write_Position (Handler, Value.position);
      if Value.workDoneToken.Is_Set then
         Handler.Key_Name ("workDoneToken");
         Write_ProgressToken (Handler, Value.workDoneToken.Value);
      end if;
      Handler.End_Object;
   end Write_PrepareRenameParams;

   procedure Write_CodeActionTriggerKind
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Enumerations.CodeActionTriggerKind) is
   begin
      case Value is
         when LSP.Enumerations.Invoked =>
            Handler.Integer_Value (1);
         when LSP.Enumerations.Automatic =>
            Handler.Integer_Value (2);
      end case;
   end Write_CodeActionTriggerKind;

   procedure Write_CodeActionClientCapabilities
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.CodeActionClientCapabilities) is
      procedure Write_codeActionKind_OfcodeActionLiteralSupport_OfCodeActionClientCapabilities
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures
           .codeActionKind_OfcodeActionLiteralSupport_OfCodeActionClientCapabilities);

      procedure Write_codeActionLiteralSupport_OfCodeActionClientCapabilities
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures
           .codeActionLiteralSupport_OfCodeActionClientCapabilities);

      procedure Write_codeActionKind_OfcodeActionLiteralSupport_OfCodeActionClientCapabilities
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures
           .codeActionKind_OfcodeActionLiteralSupport_OfCodeActionClientCapabilities) is
      begin
         Handler.Start_Object;
         Handler.Key_Name ("valueSet");
         Write_CodeActionKind_Set (Handler, Value.valueSet);
         Handler.End_Object;
      end Write_codeActionKind_OfcodeActionLiteralSupport_OfCodeActionClientCapabilities;

      procedure Write_codeActionLiteralSupport_OfCodeActionClientCapabilities
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures
           .codeActionLiteralSupport_OfCodeActionClientCapabilities) is
      begin
         Handler.Start_Object;
         Handler.Key_Name ("codeActionKind");
         Write_codeActionKind_OfcodeActionLiteralSupport_OfCodeActionClientCapabilities
           (Handler, Value.codeActionKind);
         Handler.End_Object;
      end Write_codeActionLiteralSupport_OfCodeActionClientCapabilities;

   begin
      Handler.Start_Object;
      if Value.dynamicRegistration.Is_Set then
         Handler.Key_Name ("dynamicRegistration");
         Handler.Boolean_Value (Value.dynamicRegistration.Value);
      end if;
      if Value.codeActionLiteralSupport.Is_Set then
         Handler.Key_Name ("codeActionLiteralSupport");
         Write_codeActionLiteralSupport_OfCodeActionClientCapabilities
           (Handler, Value.codeActionLiteralSupport.Value);
      end if;
      if Value.isPreferredSupport.Is_Set then
         Handler.Key_Name ("isPreferredSupport");
         Handler.Boolean_Value (Value.isPreferredSupport.Value);
      end if;
      if Value.disabledSupport.Is_Set then
         Handler.Key_Name ("disabledSupport");
         Handler.Boolean_Value (Value.disabledSupport.Value);
      end if;
      if Value.dataSupport.Is_Set then
         Handler.Key_Name ("dataSupport");
         Handler.Boolean_Value (Value.dataSupport.Value);
      end if;
      if Value.resolveSupport.Is_Set then
         Handler.Key_Name ("resolveSupport");
         Write_resolveSupport_OfWorkspaceSymbolClientCapabilities
           (Handler, Value.resolveSupport.Value);
      end if;
      if Value.honorsChangeAnnotations.Is_Set then
         Handler.Key_Name ("honorsChangeAnnotations");
         Handler.Boolean_Value (Value.honorsChangeAnnotations.Value);
      end if;
      Handler.End_Object;
   end Write_CodeActionClientCapabilities;

   procedure Write_MarkdownClientCapabilities
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.MarkdownClientCapabilities) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("parser");
      Handler.String_Value (Value.parser);
      if not Value.version.Is_Null then
         Handler.Key_Name ("version");
         Handler.String_Value (Value.version);
      end if;
      if not Value.allowedTags.Is_Empty then
         Handler.Key_Name ("allowedTags");
         Write_Virtual_String_Vector (Handler, Value.allowedTags);
      end if;
      Handler.End_Object;
   end Write_MarkdownClientCapabilities;

   procedure Write_CancelParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.CancelParams) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("id");
      Write_Integer_Or_Virtual_String (Handler, Value.id);
      Handler.End_Object;
   end Write_CancelParams;

   procedure Write_DefinitionLink_Vector
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DefinitionLink_Vector) is
   begin
      Handler.Start_Array;
      for J in Value.First_Index .. Value.Last_Index loop
         Write_DefinitionLink (Handler, Value (J));
      end loop;
      Handler.End_Array;
   end Write_DefinitionLink_Vector;

   procedure Write_DefinitionParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DefinitionParams) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("textDocument");
      Write_TextDocumentIdentifier (Handler, Value.textDocument);
      Handler.Key_Name ("position");
      Write_Position (Handler, Value.position);
      if Value.workDoneToken.Is_Set then
         Handler.Key_Name ("workDoneToken");
         Write_ProgressToken (Handler, Value.workDoneToken.Value);
      end if;
      if Value.partialResultToken.Is_Set then
         Handler.Key_Name ("partialResultToken");
         Write_ProgressToken (Handler, Value.partialResultToken.Value);
      end if;
      Handler.End_Object;
   end Write_DefinitionParams;

   procedure Write_ImplementationClientCapabilities
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.ImplementationClientCapabilities) is
   begin
      Handler.Start_Object;
      if Value.dynamicRegistration.Is_Set then
         Handler.Key_Name ("dynamicRegistration");
         Handler.Boolean_Value (Value.dynamicRegistration.Value);
      end if;
      if Value.linkSupport.Is_Set then
         Handler.Key_Name ("linkSupport");
         Handler.Boolean_Value (Value.linkSupport.Value);
      end if;
      Handler.End_Object;
   end Write_ImplementationClientCapabilities;

   procedure Write_TextDocumentItem
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.TextDocumentItem) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("uri");
      Handler.String_Value (Value.uri);
      Handler.Key_Name ("languageId");
      Handler.String_Value (Value.languageId);
      Handler.Key_Name ("version");
      Handler.Integer_Value (Integer'Pos (Value.version));
      Handler.Key_Name ("text");
      Handler.String_Value (Value.text);
      Handler.End_Object;
   end Write_TextDocumentItem;

   procedure Write_ColorPresentationParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.ColorPresentationParams) is
   begin
      Handler.Start_Object;
      if Value.workDoneToken.Is_Set then
         Handler.Key_Name ("workDoneToken");
         Write_ProgressToken (Handler, Value.workDoneToken.Value);
      end if;
      if Value.partialResultToken.Is_Set then
         Handler.Key_Name ("partialResultToken");
         Write_ProgressToken (Handler, Value.partialResultToken.Value);
      end if;
      Handler.Key_Name ("textDocument");
      Write_TextDocumentIdentifier (Handler, Value.textDocument);
      Handler.Key_Name ("color");
      Write_Color (Handler, Value.color);
      Handler.Key_Name ("range");
      Write_A_Range (Handler, Value.a_range);
      Handler.End_Object;
   end Write_ColorPresentationParams;

   procedure Write_ErrorCodes
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Enumerations.ErrorCodes) is
   begin
      case Value is
         when LSP.Enumerations.ParseError =>
            Handler.Integer_Value (-32_700);
         when LSP.Enumerations.InvalidRequest =>
            Handler.Integer_Value (-32_600);
         when LSP.Enumerations.MethodNotFound =>
            Handler.Integer_Value (-32_601);
         when LSP.Enumerations.InvalidParams =>
            Handler.Integer_Value (-32_602);
         when LSP.Enumerations.InternalError =>
            Handler.Integer_Value (-32_603);
         when LSP.Enumerations.jsonrpcReservedErrorRangeStart =>
            Handler.Integer_Value (-32_099);
         when LSP.Enumerations.serverErrorStart =>
            Handler.Integer_Value (-32_099);
         when LSP.Enumerations.ServerNotInitialized =>
            Handler.Integer_Value (-32_002);
         when LSP.Enumerations.UnknownErrorCode =>
            Handler.Integer_Value (-32_001);
         when LSP.Enumerations.jsonrpcReservedErrorRangeEnd =>
            Handler.Integer_Value (-32_000);
         when LSP.Enumerations.serverErrorEnd =>
            Handler.Integer_Value (-32_000);
      end case;
   end Write_ErrorCodes;

   procedure Write_InsertTextFormat
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Enumerations.InsertTextFormat) is
   begin
      case Value is
         when LSP.Enumerations.PlainText =>
            Handler.Integer_Value (1);
         when LSP.Enumerations.Snippet =>
            Handler.Integer_Value (2);
      end case;
   end Write_InsertTextFormat;

   procedure Write_relatedDocuments_OfDocumentDiagnosticReportPartialResult_Item
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures
        .relatedDocuments_OfDocumentDiagnosticReportPartialResult_Item) is
   begin
      case Value.Kind is
         when LSP.Structures.full =>
            Write_FullDocumentDiagnosticReport (Handler, Value.full);
         when LSP.Structures.unchanged =>
            Write_UnchangedDocumentDiagnosticReport (Handler, Value.unchanged);
      end case;
   end Write_relatedDocuments_OfDocumentDiagnosticReportPartialResult_Item;

   procedure Write_ExecuteCommandClientCapabilities
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.ExecuteCommandClientCapabilities) is
   begin
      Handler.Start_Object;
      if Value.dynamicRegistration.Is_Set then
         Handler.Key_Name ("dynamicRegistration");
         Handler.Boolean_Value (Value.dynamicRegistration.Value);
      end if;
      Handler.End_Object;
   end Write_ExecuteCommandClientCapabilities;

   procedure Write_FoldingRange_Vector_Or_Null
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.FoldingRange_Vector_Or_Null) is
   begin
      if Value.Is_Empty then
         Handler.Null_Value;
      else
         Write_FoldingRange_Vector (Handler, Value);
      end if;
   end Write_FoldingRange_Vector_Or_Null;

   procedure Write_CallHierarchyItem_Vector
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.CallHierarchyItem_Vector) is
   begin
      Handler.Start_Array;
      for J in Value.First_Index .. Value.Last_Index loop
         Write_CallHierarchyItem (Handler, Value (J));
      end loop;
      Handler.End_Array;
   end Write_CallHierarchyItem_Vector;

   procedure Write_DeleteFile
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DeleteFile) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("kind");
      Handler.String_Value ("delete");
      if Value.annotationId.Is_Set then
         Handler.Key_Name ("annotationId");
         Write_ChangeAnnotationIdentifier (Handler, Value.annotationId.Value);
      end if;
      Handler.Key_Name ("uri");
      Handler.String_Value (Value.uri);
      if Value.options.Is_Set then
         Handler.Key_Name ("options");
         Write_DeleteFileOptions (Handler, Value.options.Value);
      end if;
      Handler.End_Object;
   end Write_DeleteFile;

   procedure Write_SaveOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.SaveOptions) is
   begin
      Handler.Start_Object;
      if Value.includeText.Is_Set then
         Handler.Key_Name ("includeText");
         Handler.Boolean_Value (Value.includeText.Value);
      end if;
      Handler.End_Object;
   end Write_SaveOptions;

   procedure Write_NotebookDocument
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.NotebookDocument) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("uri");
      Write_URI (Handler, Value.uri);
      Handler.Key_Name ("notebookType");
      Handler.String_Value (Value.notebookType);
      Handler.Key_Name ("version");
      Handler.Integer_Value (Integer'Pos (Value.version));
      if Value.metadata.Is_Set then
         Handler.Key_Name ("metadata");
         Write_LSPObject (Handler, Value.metadata.Value);
      end if;
      Handler.Key_Name ("cells");
      Write_NotebookCell_Vector (Handler, Value.cells);
      Handler.End_Object;
   end Write_NotebookDocument;

   procedure Write_RelatedFullDocumentDiagnosticReport
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.RelatedFullDocumentDiagnosticReport) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("kind");
      Handler.String_Value ("full");
      if not Value.resultId.Is_Null then
         Handler.Key_Name ("resultId");
         Handler.String_Value (Value.resultId);
      end if;
      Handler.Key_Name ("items");
      Write_Diagnostic_Vector (Handler, Value.items);
      if not Value.relatedDocuments.Is_Empty then
         Handler.Key_Name ("relatedDocuments");
         declare
            use
              LSP.Structures
                .relatedDocuments_OfDocumentDiagnosticReportPartialResult_Item_Maps;
         begin
            Handler.Start_Object;
            for Cursor in Value.relatedDocuments.Iterate loop
               Handler.Key_Name (Key (Cursor));
               Write_relatedDocuments_OfDocumentDiagnosticReportPartialResult_Item
                 (Handler, Value.relatedDocuments (Cursor));
            end loop;
            Handler.End_Object;
         end;
      end if;
      Handler.End_Object;
   end Write_RelatedFullDocumentDiagnosticReport;

   procedure Write_CodeLensClientCapabilities
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.CodeLensClientCapabilities) is
   begin
      Handler.Start_Object;
      if Value.dynamicRegistration.Is_Set then
         Handler.Key_Name ("dynamicRegistration");
         Handler.Boolean_Value (Value.dynamicRegistration.Value);
      end if;
      Handler.End_Object;
   end Write_CodeLensClientCapabilities;

   procedure Write_DocumentSymbolOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DocumentSymbolOptions) is
   begin
      Handler.Start_Object;
      if Value.workDoneProgress.Is_Set then
         Handler.Key_Name ("workDoneProgress");
         Handler.Boolean_Value (Value.workDoneProgress.Value);
      end if;
      if not Value.label.Is_Null then
         Handler.Key_Name ("label");
         Handler.String_Value (Value.label);
      end if;
      Handler.End_Object;
   end Write_DocumentSymbolOptions;

   procedure Write_CompletionRegistrationOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.CompletionRegistrationOptions) is
      procedure Write_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.DocumentSelector_Or_Null);

      procedure Write_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.DocumentSelector_Or_Null) is
      begin
         if Value.Is_Null then
            Handler.Null_Value;
         else
            Write_DocumentSelector (Handler, Value.Value);
         end if;
      end Write_DocumentSelector_Or_Null;

      procedure Write_completionItem_OfCompletionOptions
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.completionItem_OfCompletionOptions);

      procedure Write_completionItem_OfCompletionOptions
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.completionItem_OfCompletionOptions) is
      begin
         Handler.Start_Object;
         if Value.labelDetailsSupport.Is_Set then
            Handler.Key_Name ("labelDetailsSupport");
            Handler.Boolean_Value (Value.labelDetailsSupport.Value);
         end if;
         Handler.End_Object;
      end Write_completionItem_OfCompletionOptions;

   begin
      Handler.Start_Object;
      Handler.Key_Name ("documentSelector");
      Write_DocumentSelector_Or_Null (Handler, Value.Parent.documentSelector);
      if Value.workDoneProgress.Is_Set then
         Handler.Key_Name ("workDoneProgress");
         Handler.Boolean_Value (Value.workDoneProgress.Value);
      end if;
      if not Value.triggerCharacters.Is_Empty then
         Handler.Key_Name ("triggerCharacters");
         Write_Virtual_String_Vector (Handler, Value.triggerCharacters);
      end if;
      if not Value.allCommitCharacters.Is_Empty then
         Handler.Key_Name ("allCommitCharacters");
         Write_Virtual_String_Vector (Handler, Value.allCommitCharacters);
      end if;
      if Value.resolveProvider.Is_Set then
         Handler.Key_Name ("resolveProvider");
         Handler.Boolean_Value (Value.resolveProvider.Value);
      end if;
      if Value.completionItem.Is_Set then
         Handler.Key_Name ("completionItem");
         Write_completionItem_OfCompletionOptions
           (Handler, Value.completionItem.Value);
      end if;
      Handler.End_Object;
   end Write_CompletionRegistrationOptions;

   procedure Write_InlayHintClientCapabilities
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.InlayHintClientCapabilities) is
   begin
      Handler.Start_Object;
      if Value.dynamicRegistration.Is_Set then
         Handler.Key_Name ("dynamicRegistration");
         Handler.Boolean_Value (Value.dynamicRegistration.Value);
      end if;
      if Value.resolveSupport.Is_Set then
         Handler.Key_Name ("resolveSupport");
         Write_resolveSupport_OfWorkspaceSymbolClientCapabilities
           (Handler, Value.resolveSupport.Value);
      end if;
      Handler.End_Object;
   end Write_InlayHintClientCapabilities;

   procedure Write_ReferenceContext
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.ReferenceContext) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("includeDeclaration");
      Handler.Boolean_Value (Value.includeDeclaration);
      Handler.End_Object;
   end Write_ReferenceContext;

   procedure Write_WorkspaceEditClientCapabilities
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.WorkspaceEditClientCapabilities) is
      procedure Write_ResourceOperationKind_Set
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.ResourceOperationKind_Set);

      procedure Write_changeAnnotationSupport_OfWorkspaceEditClientCapabilities
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures
           .changeAnnotationSupport_OfWorkspaceEditClientCapabilities);

      procedure Write_ResourceOperationKind_Set
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.ResourceOperationKind_Set) is
      begin
         Handler.Start_Array;
         declare
            Set : LSP.Structures.ResourceOperationKind_Set renames Value;
         begin
            for Value in Set'Range loop
               if Set (Value) then
                  Write_ResourceOperationKind (Handler, Value);
               end if;
            end loop;
         end;
         Handler.End_Array;
      end Write_ResourceOperationKind_Set;

      procedure Write_changeAnnotationSupport_OfWorkspaceEditClientCapabilities
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures
           .changeAnnotationSupport_OfWorkspaceEditClientCapabilities) is
      begin
         Handler.Start_Object;
         if Value.groupsOnLabel.Is_Set then
            Handler.Key_Name ("groupsOnLabel");
            Handler.Boolean_Value (Value.groupsOnLabel.Value);
         end if;
         Handler.End_Object;
      end Write_changeAnnotationSupport_OfWorkspaceEditClientCapabilities;

   begin
      Handler.Start_Object;
      if Value.documentChanges.Is_Set then
         Handler.Key_Name ("documentChanges");
         Handler.Boolean_Value (Value.documentChanges.Value);
      end if;
      if (for some Item of Value.resourceOperations => Item) then
         Handler.Key_Name ("resourceOperations");
         Write_ResourceOperationKind_Set (Handler, Value.resourceOperations);
      end if;
      if Value.failureHandling.Is_Set then
         Handler.Key_Name ("failureHandling");
         Write_FailureHandlingKind (Handler, Value.failureHandling.Value);
      end if;
      if Value.normalizesLineEndings.Is_Set then
         Handler.Key_Name ("normalizesLineEndings");
         Handler.Boolean_Value (Value.normalizesLineEndings.Value);
      end if;
      if Value.changeAnnotationSupport.Is_Set then
         Handler.Key_Name ("changeAnnotationSupport");
         Write_changeAnnotationSupport_OfWorkspaceEditClientCapabilities
           (Handler, Value.changeAnnotationSupport.Value);
      end if;
      Handler.End_Object;
   end Write_WorkspaceEditClientCapabilities;

   procedure Write_WorkspaceSymbolOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.WorkspaceSymbolOptions) is
   begin
      Handler.Start_Object;
      if Value.workDoneProgress.Is_Set then
         Handler.Key_Name ("workDoneProgress");
         Handler.Boolean_Value (Value.workDoneProgress.Value);
      end if;
      if Value.resolveProvider.Is_Set then
         Handler.Key_Name ("resolveProvider");
         Handler.Boolean_Value (Value.resolveProvider.Value);
      end if;
      Handler.End_Object;
   end Write_WorkspaceSymbolOptions;

   procedure Write_MessageActionItem_Or_Null
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.MessageActionItem_Or_Null) is
   begin
      if Value.Is_Null then
         Handler.Null_Value;
      else
         Write_MessageActionItem (Handler, Value.Value);
      end if;
   end Write_MessageActionItem_Or_Null;

   procedure Write_ChangeAnnotation
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.ChangeAnnotation) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("label");
      Handler.String_Value (Value.label);
      if Value.needsConfirmation.Is_Set then
         Handler.Key_Name ("needsConfirmation");
         Handler.Boolean_Value (Value.needsConfirmation.Value);
      end if;
      if not Value.description.Is_Null then
         Handler.Key_Name ("description");
         Handler.String_Value (Value.description);
      end if;
      Handler.End_Object;
   end Write_ChangeAnnotation;

   procedure Write_FileOperationPatternOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.FileOperationPatternOptions) is
   begin
      Handler.Start_Object;
      if Value.ignoreCase.Is_Set then
         Handler.Key_Name ("ignoreCase");
         Handler.Boolean_Value (Value.ignoreCase.Value);
      end if;
      Handler.End_Object;
   end Write_FileOperationPatternOptions;

   procedure Write_VersionedTextDocumentIdentifier
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.VersionedTextDocumentIdentifier) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("uri");
      Handler.String_Value (Value.uri);
      Handler.Key_Name ("version");
      Handler.Integer_Value (Integer'Pos (Value.version));
      Handler.End_Object;
   end Write_VersionedTextDocumentIdentifier;

   procedure Write_SemanticTokensDelta
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.SemanticTokensDelta) is
   begin
      Handler.Start_Object;
      if not Value.resultId.Is_Null then
         Handler.Key_Name ("resultId");
         Handler.String_Value (Value.resultId);
      end if;
      Handler.Key_Name ("edits");
      Write_SemanticTokensEdit_Vector (Handler, Value.edits);
      Handler.End_Object;
   end Write_SemanticTokensDelta;

   procedure Write_ShowDocumentClientCapabilities
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.ShowDocumentClientCapabilities) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("support");
      Handler.Boolean_Value (Value.support);
      Handler.End_Object;
   end Write_ShowDocumentClientCapabilities;

   procedure Write_ApplyWorkspaceEditParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.ApplyWorkspaceEditParams) is
   begin
      Handler.Start_Object;
      if not Value.label.Is_Null then
         Handler.Key_Name ("label");
         Handler.String_Value (Value.label);
      end if;
      Handler.Key_Name ("edit");
      Write_WorkspaceEdit (Handler, Value.edit);
      Handler.End_Object;
   end Write_ApplyWorkspaceEditParams;

   procedure Write_InlayHintLabelPart
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.InlayHintLabelPart) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("value");
      Handler.String_Value (Value.value);
      if Value.tooltip.Is_Set then
         Handler.Key_Name ("tooltip");
         Write_Virtual_String_Or_MarkupContent (Handler, Value.tooltip.Value);
      end if;
      if Value.location.Is_Set then
         Handler.Key_Name ("location");
         Write_Location (Handler, Value.location.Value);
      end if;
      if Value.command.Is_Set then
         Handler.Key_Name ("command");
         Write_Command (Handler, Value.command.Value);
      end if;
      Handler.End_Object;
   end Write_InlayHintLabelPart;

   procedure Write_SelectionRangeOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.SelectionRangeOptions) is
   begin
      Handler.Start_Object;
      if Value.workDoneProgress.Is_Set then
         Handler.Key_Name ("workDoneProgress");
         Handler.Boolean_Value (Value.workDoneProgress.Value);
      end if;
      Handler.End_Object;
   end Write_SelectionRangeOptions;

   procedure Write_LocationLink
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.LocationLink) is
   begin
      Handler.Start_Object;
      if Value.originSelectionRange.Is_Set then
         Handler.Key_Name ("originSelectionRange");
         Write_A_Range (Handler, Value.originSelectionRange.Value);
      end if;
      Handler.Key_Name ("targetUri");
      Handler.String_Value (Value.targetUri);
      Handler.Key_Name ("targetRange");
      Write_A_Range (Handler, Value.targetRange);
      Handler.Key_Name ("targetSelectionRange");
      Write_A_Range (Handler, Value.targetSelectionRange);
      Handler.End_Object;
   end Write_LocationLink;

   procedure Write_CompletionList
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.CompletionList) is
      procedure Write_itemDefaults_OfCompletionList
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.itemDefaults_OfCompletionList);

      procedure Write_Range_Or_Something
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.Range_Or_Something);

      procedure Write_itemDefaults_OfCompletionList
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.itemDefaults_OfCompletionList) is
      begin
         Handler.Start_Object;
         if not Value.commitCharacters.Is_Empty then
            Handler.Key_Name ("commitCharacters");
            Write_Virtual_String_Vector (Handler, Value.commitCharacters);
         end if;
         if Value.editRange.Is_Set then
            Handler.Key_Name ("editRange");
            Write_Range_Or_Something (Handler, Value.editRange.Value);
         end if;
         if Value.insertTextFormat.Is_Set then
            Handler.Key_Name ("insertTextFormat");
            Write_InsertTextFormat (Handler, Value.insertTextFormat.Value);
         end if;
         if Value.insertTextMode.Is_Set then
            Handler.Key_Name ("insertTextMode");
            Write_InsertTextMode (Handler, Value.insertTextMode.Value);
         end if;
         if not Value.data.Is_Empty then
            Handler.Key_Name ("data");
            Write_LSPAny (Handler, Value.data);
         end if;
         Handler.End_Object;
      end Write_itemDefaults_OfCompletionList;

      procedure Write_Range_Or_Something
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.Range_Or_Something) is
      begin
         case Value.Is_A_Range is
            when True =>
               Write_A_Range (Handler, Value.A_Range);
            when False =>
               Handler.Start_Object;
               Handler.Key_Name ("insert");
               Write_A_Range (Handler, Value.insert);
               Handler.Key_Name ("replace");
               Write_A_Range (Handler, Value.replace);
               Handler.End_Object;
         end case;
      end Write_Range_Or_Something;

   begin
      Handler.Start_Object;
      Handler.Key_Name ("isIncomplete");
      Handler.Boolean_Value (Value.isIncomplete);
      if Value.itemDefaults.Is_Set then
         Handler.Key_Name ("itemDefaults");
         Write_itemDefaults_OfCompletionList
           (Handler, Value.itemDefaults.Value);
      end if;
      Handler.Key_Name ("items");
      Write_CompletionItem_Vector (Handler, Value.items);
      Handler.End_Object;
   end Write_CompletionList;

   procedure Write_DiagnosticRegistrationOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DiagnosticRegistrationOptions) is
      procedure Write_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.DocumentSelector_Or_Null);

      procedure Write_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.DocumentSelector_Or_Null) is
      begin
         if Value.Is_Null then
            Handler.Null_Value;
         else
            Write_DocumentSelector (Handler, Value.Value);
         end if;
      end Write_DocumentSelector_Or_Null;

   begin
      Handler.Start_Object;
      Handler.Key_Name ("documentSelector");
      Write_DocumentSelector_Or_Null (Handler, Value.Parent.documentSelector);
      if Value.workDoneProgress.Is_Set then
         Handler.Key_Name ("workDoneProgress");
         Handler.Boolean_Value (Value.workDoneProgress.Value);
      end if;
      if not Value.identifier.Is_Null then
         Handler.Key_Name ("identifier");
         Handler.String_Value (Value.identifier);
      end if;
      Handler.Key_Name ("interFileDependencies");
      Handler.Boolean_Value (Value.interFileDependencies);
      Handler.Key_Name ("workspaceDiagnostics");
      Handler.Boolean_Value (Value.workspaceDiagnostics);
      if not Value.id.Is_Null then
         Handler.Key_Name ("id");
         Handler.String_Value (Value.id);
      end if;
      Handler.End_Object;
   end Write_DiagnosticRegistrationOptions;

   procedure Write_SignatureHelpTriggerKind
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Enumerations.SignatureHelpTriggerKind) is
   begin
      case Value is
         when LSP.Enumerations.Invoked =>
            Handler.Integer_Value (1);
         when LSP.Enumerations.TriggerCharacter =>
            Handler.Integer_Value (2);
         when LSP.Enumerations.ContentChange =>
            Handler.Integer_Value (3);
      end case;
   end Write_SignatureHelpTriggerKind;

   procedure Write_CodeAction
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.CodeAction) is
      procedure Write_disabled_OfCodeAction
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.disabled_OfCodeAction);

      procedure Write_disabled_OfCodeAction
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.disabled_OfCodeAction) is
      begin
         Handler.Start_Object;
         Handler.Key_Name ("reason");
         Handler.String_Value (Value.reason);
         Handler.End_Object;
      end Write_disabled_OfCodeAction;

   begin
      Handler.Start_Object;
      Handler.Key_Name ("title");
      Handler.String_Value (Value.title);
      if Value.kind.Is_Set then
         Handler.Key_Name ("kind");
         Write_CodeActionKind (Handler, Value.kind.Value);
      end if;
      if not Value.diagnostics.Is_Empty then
         Handler.Key_Name ("diagnostics");
         Write_Diagnostic_Vector (Handler, Value.diagnostics);
      end if;
      if Value.isPreferred.Is_Set then
         Handler.Key_Name ("isPreferred");
         Handler.Boolean_Value (Value.isPreferred.Value);
      end if;
      if Value.disabled.Is_Set then
         Handler.Key_Name ("disabled");
         Write_disabled_OfCodeAction (Handler, Value.disabled.Value);
      end if;
      if Value.edit.Is_Set then
         Handler.Key_Name ("edit");
         Write_WorkspaceEdit (Handler, Value.edit.Value);
      end if;
      if Value.command.Is_Set then
         Handler.Key_Name ("command");
         Write_Command (Handler, Value.command.Value);
      end if;
      if not Value.data.Is_Empty then
         Handler.Key_Name ("data");
         Write_LSPAny (Handler, Value.data);
      end if;
      Handler.End_Object;
   end Write_CodeAction;

   procedure Write_InlayHint_Vector_Or_Null
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.InlayHint_Vector_Or_Null) is
   begin
      if Value.Is_Empty then
         Handler.Null_Value;
      else
         Write_InlayHint_Vector (Handler, Value);
      end if;
   end Write_InlayHint_Vector_Or_Null;

   procedure Write_DeclarationParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DeclarationParams) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("textDocument");
      Write_TextDocumentIdentifier (Handler, Value.textDocument);
      Handler.Key_Name ("position");
      Write_Position (Handler, Value.position);
      if Value.workDoneToken.Is_Set then
         Handler.Key_Name ("workDoneToken");
         Write_ProgressToken (Handler, Value.workDoneToken.Value);
      end if;
      if Value.partialResultToken.Is_Set then
         Handler.Key_Name ("partialResultToken");
         Write_ProgressToken (Handler, Value.partialResultToken.Value);
      end if;
      Handler.End_Object;
   end Write_DeclarationParams;

   procedure Write_DocumentRangeFormattingParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DocumentRangeFormattingParams) is
   begin
      Handler.Start_Object;
      if Value.workDoneToken.Is_Set then
         Handler.Key_Name ("workDoneToken");
         Write_ProgressToken (Handler, Value.workDoneToken.Value);
      end if;
      Handler.Key_Name ("textDocument");
      Write_TextDocumentIdentifier (Handler, Value.textDocument);
      Handler.Key_Name ("range");
      Write_A_Range (Handler, Value.a_range);
      Handler.Key_Name ("options");
      Write_FormattingOptions (Handler, Value.options);
      Handler.End_Object;
   end Write_DocumentRangeFormattingParams;

   procedure Write_TypeHierarchyClientCapabilities
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.TypeHierarchyClientCapabilities) is
   begin
      Handler.Start_Object;
      if Value.dynamicRegistration.Is_Set then
         Handler.Key_Name ("dynamicRegistration");
         Handler.Boolean_Value (Value.dynamicRegistration.Value);
      end if;
      Handler.End_Object;
   end Write_TypeHierarchyClientCapabilities;

   procedure Write_DocumentOnTypeFormattingOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DocumentOnTypeFormattingOptions) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("firstTriggerCharacter");
      Handler.String_Value (Value.firstTriggerCharacter);
      if not Value.moreTriggerCharacter.Is_Empty then
         Handler.Key_Name ("moreTriggerCharacter");
         Write_Virtual_String_Vector (Handler, Value.moreTriggerCharacter);
      end if;
      Handler.End_Object;
   end Write_DocumentOnTypeFormattingOptions;

   procedure Write_WorkspaceSymbolRegistrationOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.WorkspaceSymbolRegistrationOptions) is
   begin
      Handler.Start_Object;
      if Value.workDoneProgress.Is_Set then
         Handler.Key_Name ("workDoneProgress");
         Handler.Boolean_Value (Value.workDoneProgress.Value);
      end if;
      if Value.resolveProvider.Is_Set then
         Handler.Key_Name ("resolveProvider");
         Handler.Boolean_Value (Value.resolveProvider.Value);
      end if;
      Handler.End_Object;
   end Write_WorkspaceSymbolRegistrationOptions;

   procedure Write_MonikerParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.MonikerParams) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("textDocument");
      Write_TextDocumentIdentifier (Handler, Value.textDocument);
      Handler.Key_Name ("position");
      Write_Position (Handler, Value.position);
      if Value.workDoneToken.Is_Set then
         Handler.Key_Name ("workDoneToken");
         Write_ProgressToken (Handler, Value.workDoneToken.Value);
      end if;
      if Value.partialResultToken.Is_Set then
         Handler.Key_Name ("partialResultToken");
         Write_ProgressToken (Handler, Value.partialResultToken.Value);
      end if;
      Handler.End_Object;
   end Write_MonikerParams;

   procedure Write_InlayHint
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.InlayHint) is
      procedure Write_Virtual_String_Or_InlayHintLabelPart_Vector
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.Virtual_String_Or_InlayHintLabelPart_Vector);

      procedure Write_InlayHintLabelPart_Vector
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.InlayHintLabelPart_Vector);

      procedure Write_Virtual_String_Or_InlayHintLabelPart_Vector
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value : LSP.Structures.Virtual_String_Or_InlayHintLabelPart_Vector) is
      begin
         case Value.Is_Virtual_String is
            when True =>
               Handler.String_Value (Value.Virtual_String);
            when False =>
               Write_InlayHintLabelPart_Vector
                 (Handler, Value.InlayHintLabelPart_Vector);
         end case;
      end Write_Virtual_String_Or_InlayHintLabelPart_Vector;

      procedure Write_InlayHintLabelPart_Vector
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.InlayHintLabelPart_Vector) is
      begin
         Handler.Start_Array;
         for J in Value.First_Index .. Value.Last_Index loop
            Write_InlayHintLabelPart (Handler, Value (J));
         end loop;
         Handler.End_Array;
      end Write_InlayHintLabelPart_Vector;

   begin
      Handler.Start_Object;
      Handler.Key_Name ("position");
      Write_Position (Handler, Value.position);
      Handler.Key_Name ("label");
      Write_Virtual_String_Or_InlayHintLabelPart_Vector (Handler, Value.label);
      if Value.kind.Is_Set then
         Handler.Key_Name ("kind");
         Write_InlayHintKind (Handler, Value.kind.Value);
      end if;
      if not Value.textEdits.Is_Empty then
         Handler.Key_Name ("textEdits");
         Write_TextEdit_Vector (Handler, Value.textEdits);
      end if;
      if Value.tooltip.Is_Set then
         Handler.Key_Name ("tooltip");
         Write_Virtual_String_Or_MarkupContent (Handler, Value.tooltip.Value);
      end if;
      if Value.paddingLeft.Is_Set then
         Handler.Key_Name ("paddingLeft");
         Handler.Boolean_Value (Value.paddingLeft.Value);
      end if;
      if Value.paddingRight.Is_Set then
         Handler.Key_Name ("paddingRight");
         Handler.Boolean_Value (Value.paddingRight.Value);
      end if;
      if not Value.data.Is_Empty then
         Handler.Key_Name ("data");
         Write_LSPAny (Handler, Value.data);
      end if;
      Handler.End_Object;
   end Write_InlayHint;

   procedure Write_FileDelete
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.FileDelete) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("uri");
      Handler.String_Value (Value.uri);
      Handler.End_Object;
   end Write_FileDelete;

   procedure Write_DocumentFormattingRegistrationOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DocumentFormattingRegistrationOptions) is
      procedure Write_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.DocumentSelector_Or_Null);

      procedure Write_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.DocumentSelector_Or_Null) is
      begin
         if Value.Is_Null then
            Handler.Null_Value;
         else
            Write_DocumentSelector (Handler, Value.Value);
         end if;
      end Write_DocumentSelector_Or_Null;

   begin
      Handler.Start_Object;
      Handler.Key_Name ("documentSelector");
      Write_DocumentSelector_Or_Null (Handler, Value.Parent.documentSelector);
      if Value.workDoneProgress.Is_Set then
         Handler.Key_Name ("workDoneProgress");
         Handler.Boolean_Value (Value.workDoneProgress.Value);
      end if;
      Handler.End_Object;
   end Write_DocumentFormattingRegistrationOptions;

   procedure Write_ColorPresentation
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.ColorPresentation) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("label");
      Handler.String_Value (Value.label);
      if Value.textEdit.Is_Set then
         Handler.Key_Name ("textEdit");
         Write_TextEdit (Handler, Value.textEdit.Value);
      end if;
      if not Value.additionalTextEdits.Is_Empty then
         Handler.Key_Name ("additionalTextEdits");
         Write_TextEdit_Vector (Handler, Value.additionalTextEdits);
      end if;
      Handler.End_Object;
   end Write_ColorPresentation;

   procedure Write_UnregistrationParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.UnregistrationParams) is
      procedure Write_Unregistration_Vector
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.Unregistration_Vector);

      procedure Write_Unregistration_Vector
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.Unregistration_Vector) is
      begin
         Handler.Start_Array;
         for J in Value.First_Index .. Value.Last_Index loop
            Write_Unregistration (Handler, Value (J));
         end loop;
         Handler.End_Array;
      end Write_Unregistration_Vector;

   begin
      Handler.Start_Object;
      Handler.Key_Name ("unregisterations");
      Write_Unregistration_Vector (Handler, Value.unregisterations);
      Handler.End_Object;
   end Write_UnregistrationParams;

   procedure Write_SelectionRangeParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.SelectionRangeParams) is
      procedure Write_Position_Vector
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.Position_Vector);

      procedure Write_Position_Vector
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.Position_Vector) is
      begin
         Handler.Start_Array;
         for J in Value.First_Index .. Value.Last_Index loop
            Write_Position (Handler, Value (J));
         end loop;
         Handler.End_Array;
      end Write_Position_Vector;

   begin
      Handler.Start_Object;
      if Value.workDoneToken.Is_Set then
         Handler.Key_Name ("workDoneToken");
         Write_ProgressToken (Handler, Value.workDoneToken.Value);
      end if;
      if Value.partialResultToken.Is_Set then
         Handler.Key_Name ("partialResultToken");
         Write_ProgressToken (Handler, Value.partialResultToken.Value);
      end if;
      Handler.Key_Name ("textDocument");
      Write_TextDocumentIdentifier (Handler, Value.textDocument);
      Handler.Key_Name ("positions");
      Write_Position_Vector (Handler, Value.positions);
      Handler.End_Object;
   end Write_SelectionRangeParams;

   procedure Write_Tokens_Delta_Result
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.Tokens_Delta_Result) is
   begin
      case Value.Kind is
         when LSP.Structures.Variant_1 =>
            Write_SemanticTokens (Handler, Value.Variant_1);
         when LSP.Structures.Variant_2 =>
            Write_SemanticTokensDelta (Handler, Value.Variant_2);
         when LSP.Structures.Variant_3 =>
            Handler.Null_Value;
      end case;
   end Write_Tokens_Delta_Result;

   procedure Write_RenameFilesParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.RenameFilesParams) is
      procedure Write_FileRename_Vector
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.FileRename_Vector);

      procedure Write_FileRename_Vector
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.FileRename_Vector) is
      begin
         Handler.Start_Array;
         for J in Value.First_Index .. Value.Last_Index loop
            Write_FileRename (Handler, Value (J));
         end loop;
         Handler.End_Array;
      end Write_FileRename_Vector;

   begin
      Handler.Start_Object;
      Handler.Key_Name ("files");
      Write_FileRename_Vector (Handler, Value.files);
      Handler.End_Object;
   end Write_RenameFilesParams;

   procedure Write_TypeHierarchyItem
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.TypeHierarchyItem) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("name");
      Handler.String_Value (Value.name);
      Handler.Key_Name ("kind");
      Write_SymbolKind (Handler, Value.kind);
      if (for some Item of Value.tags => Item) then
         Handler.Key_Name ("tags");
         Write_SymbolTag_Set (Handler, Value.tags);
      end if;
      if not Value.detail.Is_Null then
         Handler.Key_Name ("detail");
         Handler.String_Value (Value.detail);
      end if;
      Handler.Key_Name ("uri");
      Handler.String_Value (Value.uri);
      Handler.Key_Name ("range");
      Write_A_Range (Handler, Value.a_range);
      Handler.Key_Name ("selectionRange");
      Write_A_Range (Handler, Value.selectionRange);
      if not Value.data.Is_Empty then
         Handler.Key_Name ("data");
         Write_LSPAny (Handler, Value.data);
      end if;
      Handler.End_Object;
   end Write_TypeHierarchyItem;

   procedure Write_Boolean_Or_Any
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.Boolean_Or_Any) is
   begin
      LSP.Output_Tools.Write_LSPAny (Handler, (Value with null record));
   end Write_Boolean_Or_Any;

   procedure Write_Location
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.Location) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("uri");
      Handler.String_Value (Value.uri);
      Handler.Key_Name ("range");
      Write_A_Range (Handler, Value.a_range);
      if (for some Item of Value.alsKind => Item) then
         Handler.Key_Name ("alsKind");
         Write_AlsReferenceKind_Set (Handler, Value.alsKind);
      end if;
      Handler.End_Object;
   end Write_Location;

   procedure Write_RenameRegistrationOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.RenameRegistrationOptions) is
      procedure Write_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.DocumentSelector_Or_Null);

      procedure Write_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.DocumentSelector_Or_Null) is
      begin
         if Value.Is_Null then
            Handler.Null_Value;
         else
            Write_DocumentSelector (Handler, Value.Value);
         end if;
      end Write_DocumentSelector_Or_Null;

   begin
      Handler.Start_Object;
      Handler.Key_Name ("documentSelector");
      Write_DocumentSelector_Or_Null (Handler, Value.Parent.documentSelector);
      if Value.workDoneProgress.Is_Set then
         Handler.Key_Name ("workDoneProgress");
         Handler.Boolean_Value (Value.workDoneProgress.Value);
      end if;
      if Value.prepareProvider.Is_Set then
         Handler.Key_Name ("prepareProvider");
         Handler.Boolean_Value (Value.prepareProvider.Value);
      end if;
      Handler.End_Object;
   end Write_RenameRegistrationOptions;

   procedure Write_ParameterInformation
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.ParameterInformation) is
      procedure Write_Natural_Tuple
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.Natural_Tuple);

      procedure Write_String_Or_Natural_Tuple
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.String_Or_Natural_Tuple);

      procedure Write_Natural_Tuple
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.Natural_Tuple) is
      begin
         Handler.Start_Array;
         for J in Value'Range loop
            Handler.Integer_Value (Integer'Pos (Value (J)));
         end loop;
         Handler.End_Array;
      end Write_Natural_Tuple;

      procedure Write_String_Or_Natural_Tuple
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.String_Or_Natural_Tuple) is
      begin
         case Value.Is_Virtual_String is
            when True =>
               Handler.String_Value (Value.Virtual_String);
            when False =>
               Write_Natural_Tuple (Handler, Value.Natural_Tuple);
         end case;
      end Write_String_Or_Natural_Tuple;

   begin
      Handler.Start_Object;
      Handler.Key_Name ("label");
      Write_String_Or_Natural_Tuple (Handler, Value.label);
      if Value.documentation.Is_Set then
         Handler.Key_Name ("documentation");
         Write_Virtual_String_Or_MarkupContent
           (Handler, Value.documentation.Value);
      end if;
      Handler.End_Object;
   end Write_ParameterInformation;

   procedure Write_PositionEncodingKind
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Enumerations.PositionEncodingKind) is
   begin
      case Value is
         when LSP.Enumerations.UTF8 =>
            Handler.String_Value ("utf-8");
         when LSP.Enumerations.UTF16 =>
            Handler.String_Value ("utf-16");
         when LSP.Enumerations.UTF32 =>
            Handler.String_Value ("utf-32");
      end case;
   end Write_PositionEncodingKind;

   procedure Write_Symbol_Progress_Report
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.Symbol_Progress_Report) is
   begin
      case Value.Kind is
         when LSP.Structures.Variant_1 =>
            Write_SymbolInformation_Vector (Handler, Value.Variant_1);
         when LSP.Structures.Variant_2 =>
            Write_WorkspaceSymbol_Vector (Handler, Value.Variant_2);
      end case;
   end Write_Symbol_Progress_Report;

   procedure Write_MessageType
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Enumerations.MessageType) is
   begin
      case Value is
         when LSP.Enumerations.Error =>
            Handler.Integer_Value (1);
         when LSP.Enumerations.Warning =>
            Handler.Integer_Value (2);
         when LSP.Enumerations.Info =>
            Handler.Integer_Value (3);
         when LSP.Enumerations.Log =>
            Handler.Integer_Value (4);
      end case;
   end Write_MessageType;

   procedure Write_An_InitializeParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.An_InitializeParams) is
      procedure Write_trace_Of_InitializeParams
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.trace_Of_InitializeParams);

      procedure Write_Virtual_String_Or_Null
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.Virtual_String_Or_Null);

      procedure Write_DocumentUri_Or_Null
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.DocumentUri_Or_Null);

      procedure Write_trace_Of_InitializeParams
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.trace_Of_InitializeParams) is
      begin
         case Value is
            when LSP.Structures.off =>
               Handler.String_Value ("off");
            when LSP.Structures.messages =>
               Handler.String_Value ("messages");
            when LSP.Structures.compact =>
               Handler.String_Value ("compact");
            when LSP.Structures.verbose =>
               Handler.String_Value ("verbose");
         end case;
      end Write_trace_Of_InitializeParams;

      procedure Write_Virtual_String_Or_Null
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.Virtual_String_Or_Null) is
      begin
         if Value.Is_Null then
            Handler.Null_Value;
         else
            Handler.String_Value (Value.Value);
         end if;
      end Write_Virtual_String_Or_Null;

      procedure Write_DocumentUri_Or_Null
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.DocumentUri_Or_Null) is
      begin
         if Value.Is_Null then
            Handler.Null_Value;
         else
            Handler.String_Value (Value.Value);
         end if;
      end Write_DocumentUri_Or_Null;

   begin
      Handler.Start_Object;
      if Value.workDoneToken.Is_Set then
         Handler.Key_Name ("workDoneToken");
         Write_ProgressToken (Handler, Value.workDoneToken.Value);
      end if;
      Handler.Key_Name ("processId");
      Write_Integer_Or_Null (Handler, Value.processId);
      if Value.clientInfo.Is_Set then
         Handler.Key_Name ("clientInfo");
         Write_clientInfo_Of_InitializeParams
           (Handler, Value.clientInfo.Value);
      end if;
      if not Value.locale.Is_Null then
         Handler.Key_Name ("locale");
         Handler.String_Value (Value.locale);
      end if;
      if Value.rootPath.Is_Set then
         Handler.Key_Name ("rootPath");
         Write_Virtual_String_Or_Null (Handler, Value.rootPath.Value);
      end if;
      Handler.Key_Name ("rootUri");
      Write_DocumentUri_Or_Null (Handler, Value.rootUri);
      Handler.Key_Name ("capabilities");
      Write_ClientCapabilities (Handler, Value.capabilities);
      if not Value.initializationOptions.Is_Empty then
         Handler.Key_Name ("initializationOptions");
         Write_LSPAny (Handler, Value.initializationOptions);
      end if;
      if Value.trace.Is_Set then
         Handler.Key_Name ("trace");
         Write_trace_Of_InitializeParams (Handler, Value.trace.Value);
      end if;
      Handler.End_Object;
   end Write_An_InitializeParams;

   procedure Write_TypeHierarchySubtypesParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.TypeHierarchySubtypesParams) is
   begin
      Handler.Start_Object;
      if Value.workDoneToken.Is_Set then
         Handler.Key_Name ("workDoneToken");
         Write_ProgressToken (Handler, Value.workDoneToken.Value);
      end if;
      if Value.partialResultToken.Is_Set then
         Handler.Key_Name ("partialResultToken");
         Write_ProgressToken (Handler, Value.partialResultToken.Value);
      end if;
      Handler.Key_Name ("item");
      Write_TypeHierarchyItem (Handler, Value.item);
      Handler.End_Object;
   end Write_TypeHierarchySubtypesParams;

   procedure Write_FileOperationFilter
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.FileOperationFilter) is
   begin
      Handler.Start_Object;
      if not Value.scheme.Is_Null then
         Handler.Key_Name ("scheme");
         Handler.String_Value (Value.scheme);
      end if;
      Handler.Key_Name ("pattern");
      Write_FileOperationPattern (Handler, Value.pattern);
      Handler.End_Object;
   end Write_FileOperationFilter;

   procedure Write_ApplyWorkspaceEditResult
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.ApplyWorkspaceEditResult) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("applied");
      Handler.Boolean_Value (Value.applied);
      if not Value.failureReason.Is_Null then
         Handler.Key_Name ("failureReason");
         Handler.String_Value (Value.failureReason);
      end if;
      if Value.failedChange.Is_Set then
         Handler.Key_Name ("failedChange");
         Handler.Integer_Value (Integer'Pos (Value.failedChange.Value));
      end if;
      Handler.End_Object;
   end Write_ApplyWorkspaceEditResult;

   procedure Write_SemanticTokensPartialResult
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.SemanticTokensPartialResult) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("data");
      Write_Natural_Vector (Handler, Value.data);
      Handler.End_Object;
   end Write_SemanticTokensPartialResult;

   procedure Write_Completion_Result
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.Completion_Result) is
   begin
      case Value.Kind is
         when LSP.Structures.Variant_1 =>
            Write_CompletionItem_Vector (Handler, Value.Variant_1);
         when LSP.Structures.Variant_2 =>
            Write_CompletionList (Handler, Value.Variant_2);
         when LSP.Structures.Variant_3 =>
            Handler.Null_Value;
      end case;
   end Write_Completion_Result;

   procedure Write_MonikerOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.MonikerOptions) is
   begin
      Handler.Start_Object;
      if Value.workDoneProgress.Is_Set then
         Handler.Key_Name ("workDoneProgress");
         Handler.Boolean_Value (Value.workDoneProgress.Value);
      end if;
      Handler.End_Object;
   end Write_MonikerOptions;

   procedure Write_InlineValue
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.InlineValue) is
   begin
      case Value.Kind is
         when LSP.Structures.Variant_1 =>
            Write_InlineValueText (Handler, Value.Variant_1);
         when LSP.Structures.Variant_2 =>
            Write_InlineValueVariableLookup (Handler, Value.Variant_2);
         when LSP.Structures.Variant_3 =>
            Write_InlineValueEvaluatableExpression (Handler, Value.Variant_3);
      end case;
   end Write_InlineValue;

   procedure Write_Virtual_String_Or_NotebookDocumentFilter
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.Virtual_String_Or_NotebookDocumentFilter) is
   begin
      case Value.Is_Virtual_String is
         when True =>
            Handler.String_Value (Value.Virtual_String);
         when False =>
            Write_NotebookDocumentFilter
              (Handler, Value.NotebookDocumentFilter);
      end case;
   end Write_Virtual_String_Or_NotebookDocumentFilter;

   procedure Write_MonikerKind
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Enumerations.MonikerKind) is
   begin
      case Value is
         when LSP.Enumerations.import =>
            Handler.String_Value ("import");
         when LSP.Enumerations.export =>
            Handler.String_Value ("export");
         when LSP.Enumerations.local =>
            Handler.String_Value ("local");
      end case;
   end Write_MonikerKind;

   procedure Write_NotebookCellKind
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Enumerations.NotebookCellKind) is
   begin
      case Value is
         when LSP.Enumerations.Markup =>
            Handler.Integer_Value (1);
         when LSP.Enumerations.Code =>
            Handler.Integer_Value (2);
      end case;
   end Write_NotebookCellKind;

   procedure Write_CallHierarchyItem_Vector_Or_Null
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.CallHierarchyItem_Vector_Or_Null) is
   begin
      if Value.Is_Empty then
         Handler.Null_Value;
      else
         Write_CallHierarchyItem_Vector (Handler, Value);
      end if;
   end Write_CallHierarchyItem_Vector_Or_Null;

   procedure Write_DidCloseTextDocumentParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DidCloseTextDocumentParams) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("textDocument");
      Write_TextDocumentIdentifier (Handler, Value.textDocument);
      Handler.End_Object;
   end Write_DidCloseTextDocumentParams;

   procedure Write_CallHierarchyOutgoingCallsParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.CallHierarchyOutgoingCallsParams) is
   begin
      Handler.Start_Object;
      if Value.workDoneToken.Is_Set then
         Handler.Key_Name ("workDoneToken");
         Write_ProgressToken (Handler, Value.workDoneToken.Value);
      end if;
      if Value.partialResultToken.Is_Set then
         Handler.Key_Name ("partialResultToken");
         Write_ProgressToken (Handler, Value.partialResultToken.Value);
      end if;
      Handler.Key_Name ("item");
      Write_CallHierarchyItem (Handler, Value.item);
      Handler.End_Object;
   end Write_CallHierarchyOutgoingCallsParams;

   procedure Write_SelectionRange_Vector
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.SelectionRange_Vector) is
   begin
      Handler.Start_Array;
      for J in 1 .. Value.Length loop
         Write_SelectionRange (Handler, Value (J));
      end loop;
      Handler.End_Array;
   end Write_SelectionRange_Vector;

   procedure Write_HoverRegistrationOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.HoverRegistrationOptions) is
      procedure Write_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.DocumentSelector_Or_Null);

      procedure Write_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.DocumentSelector_Or_Null) is
      begin
         if Value.Is_Null then
            Handler.Null_Value;
         else
            Write_DocumentSelector (Handler, Value.Value);
         end if;
      end Write_DocumentSelector_Or_Null;

   begin
      Handler.Start_Object;
      Handler.Key_Name ("documentSelector");
      Write_DocumentSelector_Or_Null (Handler, Value.Parent.documentSelector);
      if Value.workDoneProgress.Is_Set then
         Handler.Key_Name ("workDoneProgress");
         Handler.Boolean_Value (Value.workDoneProgress.Value);
      end if;
      Handler.End_Object;
   end Write_HoverRegistrationOptions;

   procedure Write_CompletionItemTag
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Enumerations.CompletionItemTag) is
   begin
      case Value is
         when LSP.Enumerations.Deprecated =>
            Handler.Integer_Value (1);
      end case;
   end Write_CompletionItemTag;

   procedure Write_SemanticTokensDeltaParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.SemanticTokensDeltaParams) is
   begin
      Handler.Start_Object;
      if Value.workDoneToken.Is_Set then
         Handler.Key_Name ("workDoneToken");
         Write_ProgressToken (Handler, Value.workDoneToken.Value);
      end if;
      if Value.partialResultToken.Is_Set then
         Handler.Key_Name ("partialResultToken");
         Write_ProgressToken (Handler, Value.partialResultToken.Value);
      end if;
      Handler.Key_Name ("textDocument");
      Write_TextDocumentIdentifier (Handler, Value.textDocument);
      Handler.Key_Name ("previousResultId");
      Handler.String_Value (Value.previousResultId);
      Handler.End_Object;
   end Write_SemanticTokensDeltaParams;

   procedure Write_DidChangeWorkspaceFoldersParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DidChangeWorkspaceFoldersParams) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("event");
      Write_WorkspaceFoldersChangeEvent (Handler, Value.event);
      Handler.End_Object;
   end Write_DidChangeWorkspaceFoldersParams;

   procedure Write_DefinitionRegistrationOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DefinitionRegistrationOptions) is
      procedure Write_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.DocumentSelector_Or_Null);

      procedure Write_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.DocumentSelector_Or_Null) is
      begin
         if Value.Is_Null then
            Handler.Null_Value;
         else
            Write_DocumentSelector (Handler, Value.Value);
         end if;
      end Write_DocumentSelector_Or_Null;

   begin
      Handler.Start_Object;
      Handler.Key_Name ("documentSelector");
      Write_DocumentSelector_Or_Null (Handler, Value.Parent.documentSelector);
      if Value.workDoneProgress.Is_Set then
         Handler.Key_Name ("workDoneProgress");
         Handler.Boolean_Value (Value.workDoneProgress.Value);
      end if;
      Handler.End_Object;
   end Write_DefinitionRegistrationOptions;

   procedure Write_clientInfo_Of_InitializeParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.clientInfo_Of_InitializeParams) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("name");
      Handler.String_Value (Value.name);
      if not Value.version.Is_Null then
         Handler.Key_Name ("version");
         Handler.String_Value (Value.version);
      end if;
      Handler.End_Object;
   end Write_clientInfo_Of_InitializeParams;

   procedure Write_CodeLensParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.CodeLensParams) is
   begin
      Handler.Start_Object;
      if Value.workDoneToken.Is_Set then
         Handler.Key_Name ("workDoneToken");
         Write_ProgressToken (Handler, Value.workDoneToken.Value);
      end if;
      if Value.partialResultToken.Is_Set then
         Handler.Key_Name ("partialResultToken");
         Write_ProgressToken (Handler, Value.partialResultToken.Value);
      end if;
      Handler.Key_Name ("textDocument");
      Write_TextDocumentIdentifier (Handler, Value.textDocument);
      Handler.End_Object;
   end Write_CodeLensParams;

   procedure Write_DiagnosticTag
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Enumerations.DiagnosticTag) is
   begin
      case Value is
         when LSP.Enumerations.Unnecessary =>
            Handler.Integer_Value (1);
         when LSP.Enumerations.Deprecated =>
            Handler.Integer_Value (2);
      end case;
   end Write_DiagnosticTag;

   procedure Write_TypeHierarchyItem_Vector_Or_Null
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.TypeHierarchyItem_Vector_Or_Null) is
   begin
      if Value.Is_Empty then
         Handler.Null_Value;
      else
         Write_TypeHierarchyItem_Vector (Handler, Value);
      end if;
   end Write_TypeHierarchyItem_Vector_Or_Null;

   procedure Write_Symbol_Result
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.Symbol_Result) is
   begin
      case Value.Kind is
         when LSP.Structures.Variant_1 =>
            Write_SymbolInformation_Vector (Handler, Value.Variant_1);
         when LSP.Structures.Variant_2 =>
            Write_WorkspaceSymbol_Vector (Handler, Value.Variant_2);
         when LSP.Structures.Variant_3 =>
            Handler.Null_Value;
      end case;
   end Write_Symbol_Result;

   procedure Write_DiagnosticClientCapabilities
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DiagnosticClientCapabilities) is
   begin
      Handler.Start_Object;
      if Value.dynamicRegistration.Is_Set then
         Handler.Key_Name ("dynamicRegistration");
         Handler.Boolean_Value (Value.dynamicRegistration.Value);
      end if;
      if Value.relatedDocumentSupport.Is_Set then
         Handler.Key_Name ("relatedDocumentSupport");
         Handler.Boolean_Value (Value.relatedDocumentSupport.Value);
      end if;
      Handler.End_Object;
   end Write_DiagnosticClientCapabilities;

   procedure Write_MonikerRegistrationOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.MonikerRegistrationOptions) is
      procedure Write_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.DocumentSelector_Or_Null);

      procedure Write_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.DocumentSelector_Or_Null) is
      begin
         if Value.Is_Null then
            Handler.Null_Value;
         else
            Write_DocumentSelector (Handler, Value.Value);
         end if;
      end Write_DocumentSelector_Or_Null;

   begin
      Handler.Start_Object;
      Handler.Key_Name ("documentSelector");
      Write_DocumentSelector_Or_Null (Handler, Value.Parent.documentSelector);
      if Value.workDoneProgress.Is_Set then
         Handler.Key_Name ("workDoneProgress");
         Handler.Boolean_Value (Value.workDoneProgress.Value);
      end if;
      Handler.End_Object;
   end Write_MonikerRegistrationOptions;

   procedure Write_ExecutionSummary
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.ExecutionSummary) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("executionOrder");
      Handler.Integer_Value (Integer'Pos (Value.executionOrder));
      if Value.success.Is_Set then
         Handler.Key_Name ("success");
         Handler.Boolean_Value (Value.success.Value);
      end if;
      Handler.End_Object;
   end Write_ExecutionSummary;

   procedure Write_TypeHierarchySupertypesParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.TypeHierarchySupertypesParams) is
   begin
      Handler.Start_Object;
      if Value.workDoneToken.Is_Set then
         Handler.Key_Name ("workDoneToken");
         Write_ProgressToken (Handler, Value.workDoneToken.Value);
      end if;
      if Value.partialResultToken.Is_Set then
         Handler.Key_Name ("partialResultToken");
         Write_ProgressToken (Handler, Value.partialResultToken.Value);
      end if;
      Handler.Key_Name ("item");
      Write_TypeHierarchyItem (Handler, Value.item);
      Handler.End_Object;
   end Write_TypeHierarchySupertypesParams;

   procedure Write_PreviousResultId
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.PreviousResultId) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("uri");
      Handler.String_Value (Value.uri);
      Handler.Key_Name ("value");
      Handler.String_Value (Value.value);
      Handler.End_Object;
   end Write_PreviousResultId;

   procedure Write_FoldingRangeKind
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Enumerations.FoldingRangeKind) is
   begin
      case Value is
         when LSP.Enumerations.Comment =>
            Handler.String_Value ("comment");
         when LSP.Enumerations.Imports =>
            Handler.String_Value ("imports");
         when LSP.Enumerations.Region =>
            Handler.String_Value ("region");
      end case;
   end Write_FoldingRangeKind;

   procedure Write_PublishDiagnosticsParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.PublishDiagnosticsParams) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("uri");
      Handler.String_Value (Value.uri);
      if Value.version.Is_Set then
         Handler.Key_Name ("version");
         Handler.Integer_Value (Integer'Pos (Value.version.Value));
      end if;
      Handler.Key_Name ("diagnostics");
      Write_Diagnostic_Vector (Handler, Value.diagnostics);
      Handler.End_Object;
   end Write_PublishDiagnosticsParams;

   procedure Write_DocumentFilter
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DocumentFilter) is
   begin
      case Value.Is_TextDocumentFilter is
         when True =>
            Write_TextDocumentFilter (Handler, Value.TextDocumentFilter);
         when False =>
            Write_NotebookCellTextDocumentFilter
              (Handler, Value.NotebookCellTextDocumentFilter);
      end case;
   end Write_DocumentFilter;

   procedure Write_WorkspaceDiagnosticReport
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.WorkspaceDiagnosticReport) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("items");
      Write_WorkspaceDocumentDiagnosticReport_Vector (Handler, Value.items);
      Handler.End_Object;
   end Write_WorkspaceDiagnosticReport;

   procedure Write_CallHierarchyIncomingCall
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.CallHierarchyIncomingCall) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("from");
      Write_CallHierarchyItem (Handler, Value.from);
      Handler.Key_Name ("fromRanges");
      Write_Range_Vector (Handler, Value.fromRanges);
      Handler.End_Object;
   end Write_CallHierarchyIncomingCall;

   procedure Write_TextEdit_Vector_Or_Null
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.TextEdit_Vector_Or_Null) is
   begin
      if Value.Is_Empty then
         Handler.Null_Value;
      else
         Write_TextEdit_Vector (Handler, Value);
      end if;
   end Write_TextEdit_Vector_Or_Null;

   procedure Write_LinkedEditingRanges_Or_Null
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.LinkedEditingRanges_Or_Null) is
   begin
      if Value.Is_Null then
         Handler.Null_Value;
      else
         Write_LinkedEditingRanges (Handler, Value.Value);
      end if;
   end Write_LinkedEditingRanges_Or_Null;

   procedure Write_TextDocumentChangeRegistrationOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.TextDocumentChangeRegistrationOptions) is
      procedure Write_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.DocumentSelector_Or_Null);

      procedure Write_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.DocumentSelector_Or_Null) is
      begin
         if Value.Is_Null then
            Handler.Null_Value;
         else
            Write_DocumentSelector (Handler, Value.Value);
         end if;
      end Write_DocumentSelector_Or_Null;

   begin
      Handler.Start_Object;
      Handler.Key_Name ("documentSelector");
      Write_DocumentSelector_Or_Null (Handler, Value.documentSelector);
      Handler.Key_Name ("syncKind");
      Write_TextDocumentSyncKind (Handler, Value.syncKind);
      Handler.End_Object;
   end Write_TextDocumentChangeRegistrationOptions;

   procedure Write_DeclarationLink_Vector
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DeclarationLink_Vector) is
   begin
      Handler.Start_Array;
      for J in Value.First_Index .. Value.Last_Index loop
         Write_DeclarationLink (Handler, Value (J));
      end loop;
      Handler.End_Array;
   end Write_DeclarationLink_Vector;

   procedure Write_SetTraceParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.SetTraceParams) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("value");
      Write_TraceValues (Handler, Value.value);
      Handler.End_Object;
   end Write_SetTraceParams;

   procedure Write_DocumentFormattingClientCapabilities
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DocumentFormattingClientCapabilities) is
   begin
      Handler.Start_Object;
      if Value.dynamicRegistration.Is_Set then
         Handler.Key_Name ("dynamicRegistration");
         Handler.Boolean_Value (Value.dynamicRegistration.Value);
      end if;
      Handler.End_Object;
   end Write_DocumentFormattingClientCapabilities;

   procedure Write_NotebookDocumentChangeEvent
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.NotebookDocumentChangeEvent) is
      procedure Write_textContent_Ofcells_OfNotebookDocumentChangeEvent_Item
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures
           .textContent_Ofcells_OfNotebookDocumentChangeEvent_Item);

      procedure Write_cells_OfNotebookDocumentChangeEvent
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.cells_OfNotebookDocumentChangeEvent);

      procedure Write_textContent_Ofcells_OfNotebookDocumentChangeEvent
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures
           .textContent_Ofcells_OfNotebookDocumentChangeEvent);

      procedure Write_structure_Ofcells_OfNotebookDocumentChangeEvent
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures
           .structure_Ofcells_OfNotebookDocumentChangeEvent);

      procedure Write_textContent_Ofcells_OfNotebookDocumentChangeEvent_Item
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures
           .textContent_Ofcells_OfNotebookDocumentChangeEvent_Item) is
      begin
         Handler.Start_Object;
         Handler.Key_Name ("document");
         Write_VersionedTextDocumentIdentifier (Handler, Value.document);
         Handler.Key_Name ("changes");
         Write_TextDocumentContentChangeEvent_Vector (Handler, Value.changes);
         Handler.End_Object;
      end Write_textContent_Ofcells_OfNotebookDocumentChangeEvent_Item;

      procedure Write_cells_OfNotebookDocumentChangeEvent
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.cells_OfNotebookDocumentChangeEvent) is
      begin
         Handler.Start_Object;
         if Value.structure.Is_Set then
            Handler.Key_Name ("structure");
            Write_structure_Ofcells_OfNotebookDocumentChangeEvent
              (Handler, Value.structure.Value);
         end if;
         if not Value.data.Is_Empty then
            Handler.Key_Name ("data");
            Write_NotebookCell_Vector (Handler, Value.data);
         end if;
         if not Value.textContent.Is_Empty then
            Handler.Key_Name ("textContent");
            Write_textContent_Ofcells_OfNotebookDocumentChangeEvent
              (Handler, Value.textContent);
         end if;
         Handler.End_Object;
      end Write_cells_OfNotebookDocumentChangeEvent;

      procedure Write_textContent_Ofcells_OfNotebookDocumentChangeEvent
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures
           .textContent_Ofcells_OfNotebookDocumentChangeEvent) is
      begin
         Handler.Start_Array;
         for J in Value.First_Index .. Value.Last_Index loop
            Write_textContent_Ofcells_OfNotebookDocumentChangeEvent_Item
              (Handler, Value (J));
         end loop;
         Handler.End_Array;
      end Write_textContent_Ofcells_OfNotebookDocumentChangeEvent;

      procedure Write_structure_Ofcells_OfNotebookDocumentChangeEvent
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures
           .structure_Ofcells_OfNotebookDocumentChangeEvent) is
      begin
         Handler.Start_Object;
         Handler.Key_Name ("array");
         Write_NotebookCellArrayChange (Handler, Value.an_array);
         if not Value.didOpen.Is_Empty then
            Handler.Key_Name ("didOpen");
            Write_TextDocumentItem_Vector (Handler, Value.didOpen);
         end if;
         if not Value.didClose.Is_Empty then
            Handler.Key_Name ("didClose");
            Write_TextDocumentIdentifier_Vector (Handler, Value.didClose);
         end if;
         Handler.End_Object;
      end Write_structure_Ofcells_OfNotebookDocumentChangeEvent;

   begin
      Handler.Start_Object;
      if Value.metadata.Is_Set then
         Handler.Key_Name ("metadata");
         Write_LSPObject (Handler, Value.metadata.Value);
      end if;
      if Value.cells.Is_Set then
         Handler.Key_Name ("cells");
         Write_cells_OfNotebookDocumentChangeEvent
           (Handler, Value.cells.Value);
      end if;
      Handler.End_Object;
   end Write_NotebookDocumentChangeEvent;

   procedure Write_FormattingOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.FormattingOptions) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("tabSize");
      Handler.Integer_Value (Integer'Pos (Value.tabSize));
      Handler.Key_Name ("insertSpaces");
      Handler.Boolean_Value (Value.insertSpaces);
      if Value.trimTrailingWhitespace.Is_Set then
         Handler.Key_Name ("trimTrailingWhitespace");
         Handler.Boolean_Value (Value.trimTrailingWhitespace.Value);
      end if;
      if Value.insertFinalNewline.Is_Set then
         Handler.Key_Name ("insertFinalNewline");
         Handler.Boolean_Value (Value.insertFinalNewline.Value);
      end if;
      if Value.trimFinalNewlines.Is_Set then
         Handler.Key_Name ("trimFinalNewlines");
         Handler.Boolean_Value (Value.trimFinalNewlines.Value);
      end if;
      Handler.End_Object;
   end Write_FormattingOptions;

   procedure Write_WorkspaceDiagnosticReportPartialResult
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.WorkspaceDiagnosticReportPartialResult) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("items");
      Write_WorkspaceDocumentDiagnosticReport_Vector (Handler, Value.items);
      Handler.End_Object;
   end Write_WorkspaceDiagnosticReportPartialResult;

   procedure Write_resolveSupport_OfWorkspaceSymbolClientCapabilities
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures
        .resolveSupport_OfWorkspaceSymbolClientCapabilities) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("properties");
      Write_Virtual_String_Vector (Handler, Value.properties);
      Handler.End_Object;
   end Write_resolveSupport_OfWorkspaceSymbolClientCapabilities;

   procedure Write_DidOpenTextDocumentParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DidOpenTextDocumentParams) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("textDocument");
      Write_TextDocumentItem (Handler, Value.textDocument);
      Handler.End_Object;
   end Write_DidOpenTextDocumentParams;

   procedure Write_WorkspaceSymbol_Vector
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.WorkspaceSymbol_Vector) is
   begin
      Handler.Start_Array;
      for J in Value.First_Index .. Value.Last_Index loop
         Write_WorkspaceSymbol (Handler, Value (J));
      end loop;
      Handler.End_Array;
   end Write_WorkspaceSymbol_Vector;

   procedure Write_TypeDefinitionRegistrationOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.TypeDefinitionRegistrationOptions) is
      procedure Write_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.DocumentSelector_Or_Null);

      procedure Write_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.DocumentSelector_Or_Null) is
      begin
         if Value.Is_Null then
            Handler.Null_Value;
         else
            Write_DocumentSelector (Handler, Value.Value);
         end if;
      end Write_DocumentSelector_Or_Null;

   begin
      Handler.Start_Object;
      Handler.Key_Name ("documentSelector");
      Write_DocumentSelector_Or_Null (Handler, Value.Parent.documentSelector);
      if Value.workDoneProgress.Is_Set then
         Handler.Key_Name ("workDoneProgress");
         Handler.Boolean_Value (Value.workDoneProgress.Value);
      end if;
      if not Value.id.Is_Null then
         Handler.Key_Name ("id");
         Handler.String_Value (Value.id);
      end if;
      Handler.End_Object;
   end Write_TypeDefinitionRegistrationOptions;

   procedure Write_CodeLensOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.CodeLensOptions) is
   begin
      Handler.Start_Object;
      if Value.workDoneProgress.Is_Set then
         Handler.Key_Name ("workDoneProgress");
         Handler.Boolean_Value (Value.workDoneProgress.Value);
      end if;
      if Value.resolveProvider.Is_Set then
         Handler.Key_Name ("resolveProvider");
         Handler.Boolean_Value (Value.resolveProvider.Value);
      end if;
      Handler.End_Object;
   end Write_CodeLensOptions;

   procedure Write_FileRename
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.FileRename) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("oldUri");
      Handler.String_Value (Value.oldUri);
      Handler.Key_Name ("newUri");
      Handler.String_Value (Value.newUri);
      Handler.End_Object;
   end Write_FileRename;

   procedure Write_SemanticTokensPartialResult_Or_SemanticTokensDeltaPartialResult
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures
        .SemanticTokensPartialResult_Or_SemanticTokensDeltaPartialResult) is
   begin
      case Value.Is_SemanticTokensPartialResult is
         when True =>
            Write_SemanticTokensPartialResult
              (Handler, Value.SemanticTokensPartialResult);
         when False =>
            Write_SemanticTokensDeltaPartialResult
              (Handler, Value.SemanticTokensDeltaPartialResult);
      end case;
   end Write_SemanticTokensPartialResult_Or_SemanticTokensDeltaPartialResult;

   procedure Write_DocumentDiagnosticReport
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DocumentDiagnosticReport) is
   begin
      case Value.Kind is
         when LSP.Structures.full =>
            Write_RelatedFullDocumentDiagnosticReport (Handler, Value.full);
         when LSP.Structures.unchanged =>
            Write_RelatedUnchangedDocumentDiagnosticReport
              (Handler, Value.unchanged);
      end case;
   end Write_DocumentDiagnosticReport;

   procedure Write_DocumentRangeFormattingClientCapabilities
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DocumentRangeFormattingClientCapabilities) is
   begin
      Handler.Start_Object;
      if Value.dynamicRegistration.Is_Set then
         Handler.Key_Name ("dynamicRegistration");
         Handler.Boolean_Value (Value.dynamicRegistration.Value);
      end if;
      Handler.End_Object;
   end Write_DocumentRangeFormattingClientCapabilities;

   procedure Write_Moniker
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.Moniker) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("scheme");
      Handler.String_Value (Value.scheme);
      Handler.Key_Name ("identifier");
      Handler.String_Value (Value.identifier);
      Handler.Key_Name ("unique");
      Write_UniquenessLevel (Handler, Value.unique);
      if Value.kind.Is_Set then
         Handler.Key_Name ("kind");
         Write_MonikerKind (Handler, Value.kind.Value);
      end if;
      Handler.End_Object;
   end Write_Moniker;

   procedure Write_ExecuteCommandParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.ExecuteCommandParams) is
   begin
      Handler.Start_Object;
      if Value.workDoneToken.Is_Set then
         Handler.Key_Name ("workDoneToken");
         Write_ProgressToken (Handler, Value.workDoneToken.Value);
      end if;
      Handler.Key_Name ("command");
      Handler.String_Value (Value.command);
      if not Value.arguments.Is_Empty then
         Handler.Key_Name ("arguments");
         Write_LSPAny_Vector (Handler, Value.arguments);
      end if;
      Handler.End_Object;
   end Write_ExecuteCommandParams;

   procedure Write_WorkDoneProgressBegin
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.WorkDoneProgressBegin) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("kind");
      Handler.String_Value ("begin");
      Handler.Key_Name ("title");
      Handler.String_Value (Value.title);
      if Value.cancellable.Is_Set then
         Handler.Key_Name ("cancellable");
         Handler.Boolean_Value (Value.cancellable.Value);
      end if;
      if not Value.message.Is_Null then
         Handler.Key_Name ("message");
         Handler.String_Value (Value.message);
      end if;
      if Value.percentage.Is_Set then
         Handler.Key_Name ("percentage");
         Handler.Integer_Value (Integer'Pos (Value.percentage.Value));
      end if;
      Handler.End_Object;
   end Write_WorkDoneProgressBegin;

   procedure Write_DiagnosticWorkspaceClientCapabilities
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DiagnosticWorkspaceClientCapabilities) is
   begin
      Handler.Start_Object;
      if Value.refreshSupport.Is_Set then
         Handler.Key_Name ("refreshSupport");
         Handler.Boolean_Value (Value.refreshSupport.Value);
      end if;
      Handler.End_Object;
   end Write_DiagnosticWorkspaceClientCapabilities;

   procedure Write_DefinitionClientCapabilities
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DefinitionClientCapabilities) is
   begin
      Handler.Start_Object;
      if Value.dynamicRegistration.Is_Set then
         Handler.Key_Name ("dynamicRegistration");
         Handler.Boolean_Value (Value.dynamicRegistration.Value);
      end if;
      if Value.linkSupport.Is_Set then
         Handler.Key_Name ("linkSupport");
         Handler.Boolean_Value (Value.linkSupport.Value);
      end if;
      Handler.End_Object;
   end Write_DefinitionClientCapabilities;

   procedure Write_SignatureHelpRegistrationOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.SignatureHelpRegistrationOptions) is
      procedure Write_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.DocumentSelector_Or_Null);

      procedure Write_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.DocumentSelector_Or_Null) is
      begin
         if Value.Is_Null then
            Handler.Null_Value;
         else
            Write_DocumentSelector (Handler, Value.Value);
         end if;
      end Write_DocumentSelector_Or_Null;

   begin
      Handler.Start_Object;
      Handler.Key_Name ("documentSelector");
      Write_DocumentSelector_Or_Null (Handler, Value.Parent.documentSelector);
      if Value.workDoneProgress.Is_Set then
         Handler.Key_Name ("workDoneProgress");
         Handler.Boolean_Value (Value.workDoneProgress.Value);
      end if;
      if not Value.triggerCharacters.Is_Empty then
         Handler.Key_Name ("triggerCharacters");
         Write_Virtual_String_Vector (Handler, Value.triggerCharacters);
      end if;
      if not Value.retriggerCharacters.Is_Empty then
         Handler.Key_Name ("retriggerCharacters");
         Write_Virtual_String_Vector (Handler, Value.retriggerCharacters);
      end if;
      Handler.End_Object;
   end Write_SignatureHelpRegistrationOptions;

   procedure Write_TypeDefinitionOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.TypeDefinitionOptions) is
   begin
      Handler.Start_Object;
      if Value.workDoneProgress.Is_Set then
         Handler.Key_Name ("workDoneProgress");
         Handler.Boolean_Value (Value.workDoneProgress.Value);
      end if;
      Handler.End_Object;
   end Write_TypeDefinitionOptions;

   procedure Write_MarkupContent
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.MarkupContent) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("kind");
      Write_MarkupKind (Handler, Value.kind);
      Handler.Key_Name ("value");
      Handler.String_Value (Value.value);
      Handler.End_Object;
   end Write_MarkupContent;

   procedure Write_WorkspaceEdit
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.WorkspaceEdit) is
      procedure Write_changes_OfWorkspaceEdit
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.changes_OfWorkspaceEdit);

      procedure Write_documentChanges_OfWorkspaceEdit_Item
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.documentChanges_OfWorkspaceEdit_Item);

      procedure Write_changeAnnotations_OfWorkspaceEdit
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.changeAnnotations_OfWorkspaceEdit);

      procedure Write_documentChanges_OfWorkspaceEdit
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.documentChanges_OfWorkspaceEdit);

      procedure Write_changes_OfWorkspaceEdit
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.changes_OfWorkspaceEdit) is
      begin
         declare
            use LSP.Structures.TextEdit_Vector_Maps;
         begin
            Handler.Start_Object;
            for Cursor in Value.Iterate loop
               Handler.Key_Name (Key (Cursor));
               Write_TextEdit_Vector (Handler, Value (Cursor));
            end loop;
            Handler.End_Object;
         end;
      end Write_changes_OfWorkspaceEdit;

      procedure Write_documentChanges_OfWorkspaceEdit_Item
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.documentChanges_OfWorkspaceEdit_Item) is
      begin
         case Value.Kind is
            when LSP.Structures.Variant_1 =>
               Write_TextDocumentEdit (Handler, Value.Variant_1);
            when LSP.Structures.create =>
               Write_CreateFile (Handler, Value.create);
            when LSP.Structures.rename =>
               Write_RenameFile (Handler, Value.rename);
            when LSP.Structures.delete =>
               Write_DeleteFile (Handler, Value.delete);
         end case;
      end Write_documentChanges_OfWorkspaceEdit_Item;

      procedure Write_changeAnnotations_OfWorkspaceEdit
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.changeAnnotations_OfWorkspaceEdit) is
      begin
         declare
            use LSP.Structures.ChangeAnnotation_Maps;
         begin
            Handler.Start_Object;
            for Cursor in Value.Iterate loop
               Handler.Key_Name (Key (Cursor));
               Write_ChangeAnnotation (Handler, Value (Cursor));
            end loop;
            Handler.End_Object;
         end;
      end Write_changeAnnotations_OfWorkspaceEdit;

      procedure Write_documentChanges_OfWorkspaceEdit
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.documentChanges_OfWorkspaceEdit) is
      begin
         Handler.Start_Array;
         for J in Value.First_Index .. Value.Last_Index loop
            Write_documentChanges_OfWorkspaceEdit_Item (Handler, Value (J));
         end loop;
         Handler.End_Array;
      end Write_documentChanges_OfWorkspaceEdit;

   begin
      Handler.Start_Object;
      if not Value.changes.Is_Empty then
         Handler.Key_Name ("changes");
         declare
            use LSP.Structures.TextEdit_Vector_Maps;
         begin
            Handler.Start_Object;
            for Cursor in Value.changes.Iterate loop
               Handler.Key_Name (Key (Cursor));
               Write_TextEdit_Vector (Handler, Value.changes (Cursor));
            end loop;
            Handler.End_Object;
         end;
      end if;
      if not Value.documentChanges.Is_Empty then
         Handler.Key_Name ("documentChanges");
         Write_documentChanges_OfWorkspaceEdit
           (Handler, Value.documentChanges);
      end if;
      if not Value.changeAnnotations.Is_Empty then
         Handler.Key_Name ("changeAnnotations");
         declare
            use LSP.Structures.ChangeAnnotation_Maps;
         begin
            Handler.Start_Object;
            for Cursor in Value.changeAnnotations.Iterate loop
               Handler.Key_Name (Key (Cursor));
               Write_ChangeAnnotation
                 (Handler, Value.changeAnnotations (Cursor));
            end loop;
            Handler.End_Object;
         end;
      end if;
      Handler.End_Object;
   end Write_WorkspaceEdit;

   procedure Write_DocumentHighlight
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DocumentHighlight) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("range");
      Write_A_Range (Handler, Value.a_range);
      if Value.kind.Is_Set then
         Handler.Key_Name ("kind");
         Write_DocumentHighlightKind (Handler, Value.kind.Value);
      end if;
      Handler.End_Object;
   end Write_DocumentHighlight;

   procedure Write_MessageActionItem
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.MessageActionItem) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("title");
      Handler.String_Value (Value.title);
      Handler.End_Object;
   end Write_MessageActionItem;

   procedure Write_TextDocumentItem_Vector
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.TextDocumentItem_Vector) is
   begin
      Handler.Start_Array;
      for J in Value.First_Index .. Value.Last_Index loop
         Write_TextDocumentItem (Handler, Value (J));
      end loop;
      Handler.End_Array;
   end Write_TextDocumentItem_Vector;

   procedure Write_InlineValueRegistrationOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.InlineValueRegistrationOptions) is
      procedure Write_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.DocumentSelector_Or_Null);

      procedure Write_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.DocumentSelector_Or_Null) is
      begin
         if Value.Is_Null then
            Handler.Null_Value;
         else
            Write_DocumentSelector (Handler, Value.Value);
         end if;
      end Write_DocumentSelector_Or_Null;

   begin
      Handler.Start_Object;
      if Value.workDoneProgress.Is_Set then
         Handler.Key_Name ("workDoneProgress");
         Handler.Boolean_Value (Value.workDoneProgress.Value);
      end if;
      Handler.Key_Name ("documentSelector");
      Write_DocumentSelector_Or_Null (Handler, Value.Parent.documentSelector);
      if not Value.id.Is_Null then
         Handler.Key_Name ("id");
         Handler.String_Value (Value.id);
      end if;
      Handler.End_Object;
   end Write_InlineValueRegistrationOptions;

   procedure Write_VersionedNotebookDocumentIdentifier
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.VersionedNotebookDocumentIdentifier) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("version");
      Handler.Integer_Value (Integer'Pos (Value.version));
      Handler.Key_Name ("uri");
      Write_URI (Handler, Value.uri);
      Handler.End_Object;
   end Write_VersionedNotebookDocumentIdentifier;

   procedure Write_BaseSymbolInformation
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.BaseSymbolInformation) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("name");
      Handler.String_Value (Value.name);
      Handler.Key_Name ("kind");
      Write_SymbolKind (Handler, Value.kind);
      if (for some Item of Value.tags => Item) then
         Handler.Key_Name ("tags");
         Write_SymbolTag_Set (Handler, Value.tags);
      end if;
      if not Value.containerName.Is_Null then
         Handler.Key_Name ("containerName");
         Handler.String_Value (Value.containerName);
      end if;
      Handler.End_Object;
   end Write_BaseSymbolInformation;

   procedure Write_InlayHintParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.InlayHintParams) is
   begin
      Handler.Start_Object;
      if Value.workDoneToken.Is_Set then
         Handler.Key_Name ("workDoneToken");
         Write_ProgressToken (Handler, Value.workDoneToken.Value);
      end if;
      Handler.Key_Name ("textDocument");
      Write_TextDocumentIdentifier (Handler, Value.textDocument);
      Handler.Key_Name ("range");
      Write_A_Range (Handler, Value.a_range);
      Handler.End_Object;
   end Write_InlayHintParams;

   procedure Write_WorkDoneProgressCreateParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.WorkDoneProgressCreateParams) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("token");
      Write_ProgressToken (Handler, Value.token);
      Handler.End_Object;
   end Write_WorkDoneProgressCreateParams;

   procedure Write_TextEdit
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.TextEdit) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("range");
      Write_A_Range (Handler, Value.a_range);
      Handler.Key_Name ("newText");
      Handler.String_Value (Value.newText);
      Handler.End_Object;
   end Write_TextEdit;

   procedure Write_DeclarationClientCapabilities
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DeclarationClientCapabilities) is
   begin
      Handler.Start_Object;
      if Value.dynamicRegistration.Is_Set then
         Handler.Key_Name ("dynamicRegistration");
         Handler.Boolean_Value (Value.dynamicRegistration.Value);
      end if;
      if Value.linkSupport.Is_Set then
         Handler.Key_Name ("linkSupport");
         Handler.Boolean_Value (Value.linkSupport.Value);
      end if;
      Handler.End_Object;
   end Write_DeclarationClientCapabilities;

   procedure Write_SignatureHelpOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.SignatureHelpOptions) is
   begin
      Handler.Start_Object;
      if Value.workDoneProgress.Is_Set then
         Handler.Key_Name ("workDoneProgress");
         Handler.Boolean_Value (Value.workDoneProgress.Value);
      end if;
      if not Value.triggerCharacters.Is_Empty then
         Handler.Key_Name ("triggerCharacters");
         Write_Virtual_String_Vector (Handler, Value.triggerCharacters);
      end if;
      if not Value.retriggerCharacters.Is_Empty then
         Handler.Key_Name ("retriggerCharacters");
         Write_Virtual_String_Vector (Handler, Value.retriggerCharacters);
      end if;
      Handler.End_Object;
   end Write_SignatureHelpOptions;

   procedure Write_SemanticTokensEdit
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.SemanticTokensEdit) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("start");
      Handler.Integer_Value (Integer'Pos (Value.start));
      Handler.Key_Name ("deleteCount");
      Handler.Integer_Value (Integer'Pos (Value.deleteCount));
      if not Value.data.Is_Empty then
         Handler.Key_Name ("data");
         Write_Natural_Vector (Handler, Value.data);
      end if;
      Handler.End_Object;
   end Write_SemanticTokensEdit;

   procedure Write_DocumentHighlightParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DocumentHighlightParams) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("textDocument");
      Write_TextDocumentIdentifier (Handler, Value.textDocument);
      Handler.Key_Name ("position");
      Write_Position (Handler, Value.position);
      if Value.workDoneToken.Is_Set then
         Handler.Key_Name ("workDoneToken");
         Write_ProgressToken (Handler, Value.workDoneToken.Value);
      end if;
      if Value.partialResultToken.Is_Set then
         Handler.Key_Name ("partialResultToken");
         Write_ProgressToken (Handler, Value.partialResultToken.Value);
      end if;
      Handler.End_Object;
   end Write_DocumentHighlightParams;

   procedure Write_FileCreate
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.FileCreate) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("uri");
      Handler.String_Value (Value.uri);
      Handler.End_Object;
   end Write_FileCreate;

   procedure Write_WorkspaceFullDocumentDiagnosticReport
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.WorkspaceFullDocumentDiagnosticReport) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("kind");
      Handler.String_Value ("full");
      if not Value.resultId.Is_Null then
         Handler.Key_Name ("resultId");
         Handler.String_Value (Value.resultId);
      end if;
      Handler.Key_Name ("items");
      Write_Diagnostic_Vector (Handler, Value.items);
      Handler.Key_Name ("uri");
      Handler.String_Value (Value.uri);
      Handler.Key_Name ("version");
      Write_Integer_Or_Null (Handler, Value.version);
      Handler.End_Object;
   end Write_WorkspaceFullDocumentDiagnosticReport;

   procedure Write_SemanticTokens_Or_Null
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.SemanticTokens_Or_Null) is
   begin
      if Value.Is_Null then
         Handler.Null_Value;
      else
         Write_SemanticTokens (Handler, Value.Value);
      end if;
   end Write_SemanticTokens_Or_Null;

   procedure Write_SemanticTokensRangeParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.SemanticTokensRangeParams) is
   begin
      Handler.Start_Object;
      if Value.workDoneToken.Is_Set then
         Handler.Key_Name ("workDoneToken");
         Write_ProgressToken (Handler, Value.workDoneToken.Value);
      end if;
      if Value.partialResultToken.Is_Set then
         Handler.Key_Name ("partialResultToken");
         Write_ProgressToken (Handler, Value.partialResultToken.Value);
      end if;
      Handler.Key_Name ("textDocument");
      Write_TextDocumentIdentifier (Handler, Value.textDocument);
      Handler.Key_Name ("range");
      Write_A_Range (Handler, Value.a_range);
      Handler.End_Object;
   end Write_SemanticTokensRangeParams;

   procedure Write_FoldingRange
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.FoldingRange) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("startLine");
      Handler.Integer_Value (Integer'Pos (Value.startLine));
      if Value.startCharacter.Is_Set then
         Handler.Key_Name ("startCharacter");
         Handler.Integer_Value (Integer'Pos (Value.startCharacter.Value));
      end if;
      Handler.Key_Name ("endLine");
      Handler.Integer_Value (Integer'Pos (Value.endLine));
      if Value.endCharacter.Is_Set then
         Handler.Key_Name ("endCharacter");
         Handler.Integer_Value (Integer'Pos (Value.endCharacter.Value));
      end if;
      if Value.kind.Is_Set then
         Handler.Key_Name ("kind");
         Write_FoldingRangeKind (Handler, Value.kind.Value);
      end if;
      if not Value.collapsedText.Is_Null then
         Handler.Key_Name ("collapsedText");
         Handler.String_Value (Value.collapsedText);
      end if;
      Handler.End_Object;
   end Write_FoldingRange;

   procedure Write_SemanticTokensDeltaPartialResult
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.SemanticTokensDeltaPartialResult) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("edits");
      Write_SemanticTokensEdit_Vector (Handler, Value.edits);
      Handler.End_Object;
   end Write_SemanticTokensDeltaPartialResult;

   procedure Write_NotebookDocumentClientCapabilities
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.NotebookDocumentClientCapabilities) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("synchronization");
      Write_NotebookDocumentSyncClientCapabilities
        (Handler, Value.synchronization);
      Handler.End_Object;
   end Write_NotebookDocumentClientCapabilities;

   procedure Write_CompletionParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.CompletionParams) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("textDocument");
      Write_TextDocumentIdentifier (Handler, Value.textDocument);
      Handler.Key_Name ("position");
      Write_Position (Handler, Value.position);
      if Value.workDoneToken.Is_Set then
         Handler.Key_Name ("workDoneToken");
         Write_ProgressToken (Handler, Value.workDoneToken.Value);
      end if;
      if Value.partialResultToken.Is_Set then
         Handler.Key_Name ("partialResultToken");
         Write_ProgressToken (Handler, Value.partialResultToken.Value);
      end if;
      if Value.context.Is_Set then
         Handler.Key_Name ("context");
         Write_CompletionContext (Handler, Value.context.Value);
      end if;
      Handler.End_Object;
   end Write_CompletionParams;

   procedure Write_CallHierarchyIncomingCallsParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.CallHierarchyIncomingCallsParams) is
   begin
      Handler.Start_Object;
      if Value.workDoneToken.Is_Set then
         Handler.Key_Name ("workDoneToken");
         Write_ProgressToken (Handler, Value.workDoneToken.Value);
      end if;
      if Value.partialResultToken.Is_Set then
         Handler.Key_Name ("partialResultToken");
         Write_ProgressToken (Handler, Value.partialResultToken.Value);
      end if;
      Handler.Key_Name ("item");
      Write_CallHierarchyItem (Handler, Value.item);
      Handler.End_Object;
   end Write_CallHierarchyIncomingCallsParams;

   procedure Write_Location_Vector_Or_Null
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.Location_Vector_Or_Null) is
   begin
      if Value.Is_Empty then
         Handler.Null_Value;
      else
         Write_Location_Vector (Handler, Value);
      end if;
   end Write_Location_Vector_Or_Null;

   procedure Write_WorkspaceSymbolClientCapabilities
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.WorkspaceSymbolClientCapabilities) is
   begin
      Handler.Start_Object;
      if Value.dynamicRegistration.Is_Set then
         Handler.Key_Name ("dynamicRegistration");
         Handler.Boolean_Value (Value.dynamicRegistration.Value);
      end if;
      if Value.symbolKind.Is_Set then
         Handler.Key_Name ("symbolKind");
         Write_symbolKind_OfWorkspaceSymbolClientCapabilities
           (Handler, Value.symbolKind.Value);
      end if;
      if Value.tagSupport.Is_Set then
         Handler.Key_Name ("tagSupport");
         Write_tagSupport_OfWorkspaceSymbolClientCapabilities
           (Handler, Value.tagSupport.Value);
      end if;
      if Value.resolveSupport.Is_Set then
         Handler.Key_Name ("resolveSupport");
         Write_resolveSupport_OfWorkspaceSymbolClientCapabilities
           (Handler, Value.resolveSupport.Value);
      end if;
      Handler.End_Object;
   end Write_WorkspaceSymbolClientCapabilities;

   procedure Write_Boolean_Or_Something
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.Boolean_Or_Something) is
   begin
      case Value.Is_Boolean is
         when True =>
            Handler.Boolean_Value (Value.Boolean);
         when False =>
            Handler.Start_Object;
            if Value.a_delta.Is_Set then
               Handler.Key_Name ("delta");
               Handler.Boolean_Value (Value.a_delta.Value);
            end if;
            Handler.End_Object;
      end case;
   end Write_Boolean_Or_Something;

   procedure Write_TypeHierarchyOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.TypeHierarchyOptions) is
   begin
      Handler.Start_Object;
      if Value.workDoneProgress.Is_Set then
         Handler.Key_Name ("workDoneProgress");
         Handler.Boolean_Value (Value.workDoneProgress.Value);
      end if;
      Handler.End_Object;
   end Write_TypeHierarchyOptions;

   procedure Write_SelectionRangeRegistrationOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.SelectionRangeRegistrationOptions) is
      procedure Write_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.DocumentSelector_Or_Null);

      procedure Write_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.DocumentSelector_Or_Null) is
      begin
         if Value.Is_Null then
            Handler.Null_Value;
         else
            Write_DocumentSelector (Handler, Value.Value);
         end if;
      end Write_DocumentSelector_Or_Null;

   begin
      Handler.Start_Object;
      if Value.workDoneProgress.Is_Set then
         Handler.Key_Name ("workDoneProgress");
         Handler.Boolean_Value (Value.workDoneProgress.Value);
      end if;
      Handler.Key_Name ("documentSelector");
      Write_DocumentSelector_Or_Null (Handler, Value.Parent.documentSelector);
      if not Value.id.Is_Null then
         Handler.Key_Name ("id");
         Handler.String_Value (Value.id);
      end if;
      Handler.End_Object;
   end Write_SelectionRangeRegistrationOptions;

   procedure Write_DidChangeNotebookDocumentParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DidChangeNotebookDocumentParams) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("notebookDocument");
      Write_VersionedNotebookDocumentIdentifier
        (Handler, Value.notebookDocument);
      Handler.Key_Name ("change");
      Write_NotebookDocumentChangeEvent (Handler, Value.change);
      Handler.End_Object;
   end Write_DidChangeNotebookDocumentParams;

   procedure Write_RegularExpressionsClientCapabilities
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.RegularExpressionsClientCapabilities) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("engine");
      Handler.String_Value (Value.engine);
      if not Value.version.Is_Null then
         Handler.Key_Name ("version");
         Handler.String_Value (Value.version);
      end if;
      Handler.End_Object;
   end Write_RegularExpressionsClientCapabilities;

   procedure Write_WorkspaceDocumentDiagnosticReport
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.WorkspaceDocumentDiagnosticReport) is
   begin
      case Value.Kind is
         when LSP.Structures.full =>
            Write_WorkspaceFullDocumentDiagnosticReport (Handler, Value.full);
         when LSP.Structures.unchanged =>
            Write_WorkspaceUnchangedDocumentDiagnosticReport
              (Handler, Value.unchanged);
      end case;
   end Write_WorkspaceDocumentDiagnosticReport;

   procedure Write_WorkspaceClientCapabilities
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.WorkspaceClientCapabilities) is
   begin
      Handler.Start_Object;
      if Value.applyEdit.Is_Set then
         Handler.Key_Name ("applyEdit");
         Handler.Boolean_Value (Value.applyEdit.Value);
      end if;
      if Value.workspaceEdit.Is_Set then
         Handler.Key_Name ("workspaceEdit");
         Write_WorkspaceEditClientCapabilities
           (Handler, Value.workspaceEdit.Value);
      end if;
      if Value.didChangeConfiguration.Is_Set then
         Handler.Key_Name ("didChangeConfiguration");
         Write_DidChangeConfigurationClientCapabilities
           (Handler, Value.didChangeConfiguration.Value);
      end if;
      if Value.didChangeWatchedFiles.Is_Set then
         Handler.Key_Name ("didChangeWatchedFiles");
         Write_DidChangeWatchedFilesClientCapabilities
           (Handler, Value.didChangeWatchedFiles.Value);
      end if;
      if Value.symbol.Is_Set then
         Handler.Key_Name ("symbol");
         Write_WorkspaceSymbolClientCapabilities (Handler, Value.symbol.Value);
      end if;
      if Value.executeCommand.Is_Set then
         Handler.Key_Name ("executeCommand");
         Write_ExecuteCommandClientCapabilities
           (Handler, Value.executeCommand.Value);
      end if;
      if Value.workspaceFolders.Is_Set then
         Handler.Key_Name ("workspaceFolders");
         Handler.Boolean_Value (Value.workspaceFolders.Value);
      end if;
      if Value.configuration.Is_Set then
         Handler.Key_Name ("configuration");
         Handler.Boolean_Value (Value.configuration.Value);
      end if;
      if Value.semanticTokens.Is_Set then
         Handler.Key_Name ("semanticTokens");
         Write_SemanticTokensWorkspaceClientCapabilities
           (Handler, Value.semanticTokens.Value);
      end if;
      if Value.codeLens.Is_Set then
         Handler.Key_Name ("codeLens");
         Write_CodeLensWorkspaceClientCapabilities
           (Handler, Value.codeLens.Value);
      end if;
      if Value.fileOperations.Is_Set then
         Handler.Key_Name ("fileOperations");
         Write_FileOperationClientCapabilities
           (Handler, Value.fileOperations.Value);
      end if;
      if Value.inlineValue.Is_Set then
         Handler.Key_Name ("inlineValue");
         Write_InlineValueWorkspaceClientCapabilities
           (Handler, Value.inlineValue.Value);
      end if;
      if Value.inlayHint.Is_Set then
         Handler.Key_Name ("inlayHint");
         Write_InlayHintWorkspaceClientCapabilities
           (Handler, Value.inlayHint.Value);
      end if;
      if Value.diagnostics.Is_Set then
         Handler.Key_Name ("diagnostics");
         Write_DiagnosticWorkspaceClientCapabilities
           (Handler, Value.diagnostics.Value);
      end if;
      Handler.End_Object;
   end Write_WorkspaceClientCapabilities;

   procedure Write_WorkspaceDiagnosticParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.WorkspaceDiagnosticParams) is
      procedure Write_PreviousResultId_Vector
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.PreviousResultId_Vector);

      procedure Write_PreviousResultId_Vector
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.PreviousResultId_Vector) is
      begin
         Handler.Start_Array;
         for J in Value.First_Index .. Value.Last_Index loop
            Write_PreviousResultId (Handler, Value (J));
         end loop;
         Handler.End_Array;
      end Write_PreviousResultId_Vector;

   begin
      Handler.Start_Object;
      if Value.workDoneToken.Is_Set then
         Handler.Key_Name ("workDoneToken");
         Write_ProgressToken (Handler, Value.workDoneToken.Value);
      end if;
      if Value.partialResultToken.Is_Set then
         Handler.Key_Name ("partialResultToken");
         Write_ProgressToken (Handler, Value.partialResultToken.Value);
      end if;
      if not Value.identifier.Is_Null then
         Handler.Key_Name ("identifier");
         Handler.String_Value (Value.identifier);
      end if;
      Handler.Key_Name ("previousResultIds");
      Write_PreviousResultId_Vector (Handler, Value.previousResultIds);
      Handler.End_Object;
   end Write_WorkspaceDiagnosticParams;

   procedure Write_TextDocumentSyncKind
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Enumerations.TextDocumentSyncKind) is
   begin
      case Value is
         when LSP.Enumerations.None =>
            Handler.Integer_Value (0);
         when LSP.Enumerations.Full =>
            Handler.Integer_Value (1);
         when LSP.Enumerations.Incremental =>
            Handler.Integer_Value (2);
      end case;
   end Write_TextDocumentSyncKind;

   procedure Write_InlayHintOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.InlayHintOptions) is
   begin
      Handler.Start_Object;
      if Value.workDoneProgress.Is_Set then
         Handler.Key_Name ("workDoneProgress");
         Handler.Boolean_Value (Value.workDoneProgress.Value);
      end if;
      if Value.resolveProvider.Is_Set then
         Handler.Key_Name ("resolveProvider");
         Handler.Boolean_Value (Value.resolveProvider.Value);
      end if;
      Handler.End_Object;
   end Write_InlayHintOptions;

   procedure Write_InlineValueOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.InlineValueOptions) is
   begin
      Handler.Start_Object;
      if Value.workDoneProgress.Is_Set then
         Handler.Key_Name ("workDoneProgress");
         Handler.Boolean_Value (Value.workDoneProgress.Value);
      end if;
      Handler.End_Object;
   end Write_InlineValueOptions;

   procedure Write_WorkspaceFoldersChangeEvent
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.WorkspaceFoldersChangeEvent) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("added");
      Write_WorkspaceFolder_Vector (Handler, Value.added);
      Handler.Key_Name ("removed");
      Write_WorkspaceFolder_Vector (Handler, Value.removed);
      Handler.End_Object;
   end Write_WorkspaceFoldersChangeEvent;

   procedure Write_TypeDefinitionClientCapabilities
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.TypeDefinitionClientCapabilities) is
   begin
      Handler.Start_Object;
      if Value.dynamicRegistration.Is_Set then
         Handler.Key_Name ("dynamicRegistration");
         Handler.Boolean_Value (Value.dynamicRegistration.Value);
      end if;
      if Value.linkSupport.Is_Set then
         Handler.Key_Name ("linkSupport");
         Handler.Boolean_Value (Value.linkSupport.Value);
      end if;
      Handler.End_Object;
   end Write_TypeDefinitionClientCapabilities;

   procedure Write_CreateFileOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.CreateFileOptions) is
   begin
      Handler.Start_Object;
      if Value.overwrite.Is_Set then
         Handler.Key_Name ("overwrite");
         Handler.Boolean_Value (Value.overwrite.Value);
      end if;
      if Value.ignoreIfExists.Is_Set then
         Handler.Key_Name ("ignoreIfExists");
         Handler.Boolean_Value (Value.ignoreIfExists.Value);
      end if;
      Handler.End_Object;
   end Write_CreateFileOptions;

   procedure Write_AlsReferenceKind
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Enumerations.AlsReferenceKind) is
   begin
      case Value is
         when LSP.Enumerations.reference =>
            Handler.String_Value ("reference");
         when LSP.Enumerations.an_access =>
            Handler.String_Value ("access");
         when LSP.Enumerations.write =>
            Handler.String_Value ("write");
         when LSP.Enumerations.call =>
            Handler.String_Value ("call");
         when LSP.Enumerations.dispatching_call =>
            Handler.String_Value ("dispatching call");
         when LSP.Enumerations.parent =>
            Handler.String_Value ("parent");
         when LSP.Enumerations.child =>
            Handler.String_Value ("child");
         when LSP.Enumerations.an_overriding =>
            Handler.String_Value ("overriding");
      end case;
   end Write_AlsReferenceKind;

   procedure Write_DiagnosticRelatedInformation
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DiagnosticRelatedInformation) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("location");
      Write_Location (Handler, Value.location);
      Handler.Key_Name ("message");
      Handler.String_Value (Value.message);
      Handler.End_Object;
   end Write_DiagnosticRelatedInformation;

   procedure Write_InsertTextMode
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Enumerations.InsertTextMode) is
   begin
      case Value is
         when LSP.Enumerations.asIs =>
            Handler.Integer_Value (1);
         when LSP.Enumerations.adjustIndentation =>
            Handler.Integer_Value (2);
      end case;
   end Write_InsertTextMode;

   procedure Write_InlineValue_Vector_Or_Null
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.InlineValue_Vector_Or_Null) is
   begin
      if Value.Is_Empty then
         Handler.Null_Value;
      else
         Write_InlineValue_Vector (Handler, Value);
      end if;
   end Write_InlineValue_Vector_Or_Null;

   procedure Write_DidCloseNotebookDocumentParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DidCloseNotebookDocumentParams) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("notebookDocument");
      Write_NotebookDocumentIdentifier (Handler, Value.notebookDocument);
      Handler.Key_Name ("cellTextDocuments");
      Write_TextDocumentIdentifier_Vector (Handler, Value.cellTextDocuments);
      Handler.End_Object;
   end Write_DidCloseNotebookDocumentParams;

   procedure Write_ImplementationOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.ImplementationOptions) is
   begin
      Handler.Start_Object;
      if Value.workDoneProgress.Is_Set then
         Handler.Key_Name ("workDoneProgress");
         Handler.Boolean_Value (Value.workDoneProgress.Value);
      end if;
      Handler.End_Object;
   end Write_ImplementationOptions;

   procedure Write_InlayHint_Vector
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.InlayHint_Vector) is
   begin
      Handler.Start_Array;
      for J in Value.First_Index .. Value.Last_Index loop
         Write_InlayHint (Handler, Value (J));
      end loop;
      Handler.End_Array;
   end Write_InlayHint_Vector;

   procedure Write_DocumentColorRegistrationOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DocumentColorRegistrationOptions) is
      procedure Write_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.DocumentSelector_Or_Null);

      procedure Write_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.DocumentSelector_Or_Null) is
      begin
         if Value.Is_Null then
            Handler.Null_Value;
         else
            Write_DocumentSelector (Handler, Value.Value);
         end if;
      end Write_DocumentSelector_Or_Null;

   begin
      Handler.Start_Object;
      Handler.Key_Name ("documentSelector");
      Write_DocumentSelector_Or_Null (Handler, Value.Parent.documentSelector);
      if Value.workDoneProgress.Is_Set then
         Handler.Key_Name ("workDoneProgress");
         Handler.Boolean_Value (Value.workDoneProgress.Value);
      end if;
      if not Value.id.Is_Null then
         Handler.Key_Name ("id");
         Handler.String_Value (Value.id);
      end if;
      Handler.End_Object;
   end Write_DocumentColorRegistrationOptions;

   procedure Write_CodeActionOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.CodeActionOptions) is
   begin
      Handler.Start_Object;
      if Value.workDoneProgress.Is_Set then
         Handler.Key_Name ("workDoneProgress");
         Handler.Boolean_Value (Value.workDoneProgress.Value);
      end if;
      if (for some Item of Value.codeActionKinds => Item) then
         Handler.Key_Name ("codeActionKinds");
         Write_CodeActionKind_Set (Handler, Value.codeActionKinds);
      end if;
      if Value.resolveProvider.Is_Set then
         Handler.Key_Name ("resolveProvider");
         Handler.Boolean_Value (Value.resolveProvider.Value);
      end if;
      Handler.End_Object;
   end Write_CodeActionOptions;

   procedure Write_CompletionItemKind
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Enumerations.CompletionItemKind) is
   begin
      case Value is
         when LSP.Enumerations.Text =>
            Handler.Integer_Value (1);
         when LSP.Enumerations.Method =>
            Handler.Integer_Value (2);
         when LSP.Enumerations.A_Function =>
            Handler.Integer_Value (3);
         when LSP.Enumerations.Constructor =>
            Handler.Integer_Value (4);
         when LSP.Enumerations.Field =>
            Handler.Integer_Value (5);
         when LSP.Enumerations.Variable =>
            Handler.Integer_Value (6);
         when LSP.Enumerations.Class =>
            Handler.Integer_Value (7);
         when LSP.Enumerations.An_Interface =>
            Handler.Integer_Value (8);
         when LSP.Enumerations.Module =>
            Handler.Integer_Value (9);
         when LSP.Enumerations.Property =>
            Handler.Integer_Value (10);
         when LSP.Enumerations.Unit =>
            Handler.Integer_Value (11);
         when LSP.Enumerations.Value =>
            Handler.Integer_Value (12);
         when LSP.Enumerations.Enum =>
            Handler.Integer_Value (13);
         when LSP.Enumerations.Keyword =>
            Handler.Integer_Value (14);
         when LSP.Enumerations.Snippet =>
            Handler.Integer_Value (15);
         when LSP.Enumerations.Color =>
            Handler.Integer_Value (16);
         when LSP.Enumerations.File =>
            Handler.Integer_Value (17);
         when LSP.Enumerations.Reference =>
            Handler.Integer_Value (18);
         when LSP.Enumerations.Folder =>
            Handler.Integer_Value (19);
         when LSP.Enumerations.EnumMember =>
            Handler.Integer_Value (20);
         when LSP.Enumerations.A_Constant =>
            Handler.Integer_Value (21);
         when LSP.Enumerations.Struct =>
            Handler.Integer_Value (22);
         when LSP.Enumerations.Event =>
            Handler.Integer_Value (23);
         when LSP.Enumerations.Operator =>
            Handler.Integer_Value (24);
         when LSP.Enumerations.TypeParameter =>
            Handler.Integer_Value (25);
      end case;
   end Write_CompletionItemKind;

   procedure Write_ShowDocumentParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.ShowDocumentParams) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("uri");
      Write_URI (Handler, Value.uri);
      if Value.external.Is_Set then
         Handler.Key_Name ("external");
         Handler.Boolean_Value (Value.external.Value);
      end if;
      if Value.takeFocus.Is_Set then
         Handler.Key_Name ("takeFocus");
         Handler.Boolean_Value (Value.takeFocus.Value);
      end if;
      if Value.selection.Is_Set then
         Handler.Key_Name ("selection");
         Write_A_Range (Handler, Value.selection.Value);
      end if;
      Handler.End_Object;
   end Write_ShowDocumentParams;

   procedure Write_Command_Or_CodeAction
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.Command_Or_CodeAction) is
   begin
      case Value.Is_Command is
         when True =>
            Write_Command (Handler, Value.Command);
         when False =>
            Write_CodeAction (Handler, Value.CodeAction);
      end case;
   end Write_Command_Or_CodeAction;

   procedure Write_CreateFilesParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.CreateFilesParams) is
      procedure Write_FileCreate_Vector
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.FileCreate_Vector);

      procedure Write_FileCreate_Vector
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.FileCreate_Vector) is
      begin
         Handler.Start_Array;
         for J in Value.First_Index .. Value.Last_Index loop
            Write_FileCreate (Handler, Value (J));
         end loop;
         Handler.End_Array;
      end Write_FileCreate_Vector;

   begin
      Handler.Start_Object;
      Handler.Key_Name ("files");
      Write_FileCreate_Vector (Handler, Value.files);
      Handler.End_Object;
   end Write_CreateFilesParams;

   procedure Write_ColorPresentation_Vector
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.ColorPresentation_Vector) is
   begin
      Handler.Start_Array;
      for J in Value.First_Index .. Value.Last_Index loop
         Write_ColorPresentation (Handler, Value (J));
      end loop;
      Handler.End_Array;
   end Write_ColorPresentation_Vector;

   procedure Write_ChangeAnnotationIdentifier
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.ChangeAnnotationIdentifier) is
   begin
      Handler.String_Value (Value);
   end Write_ChangeAnnotationIdentifier;

   procedure Write_DocumentFormattingParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DocumentFormattingParams) is
   begin
      Handler.Start_Object;
      if Value.workDoneToken.Is_Set then
         Handler.Key_Name ("workDoneToken");
         Write_ProgressToken (Handler, Value.workDoneToken.Value);
      end if;
      Handler.Key_Name ("textDocument");
      Write_TextDocumentIdentifier (Handler, Value.textDocument);
      Handler.Key_Name ("options");
      Write_FormattingOptions (Handler, Value.options);
      Handler.End_Object;
   end Write_DocumentFormattingParams;

   procedure Write_SignatureHelpContext
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.SignatureHelpContext) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("triggerKind");
      Write_SignatureHelpTriggerKind (Handler, Value.triggerKind);
      if not Value.triggerCharacter.Is_Null then
         Handler.Key_Name ("triggerCharacter");
         Handler.String_Value (Value.triggerCharacter);
      end if;
      Handler.Key_Name ("isRetrigger");
      Handler.Boolean_Value (Value.isRetrigger);
      if Value.activeSignatureHelp.Is_Set then
         Handler.Key_Name ("activeSignatureHelp");
         Write_SignatureHelp (Handler, Value.activeSignatureHelp.Value);
      end if;
      Handler.End_Object;
   end Write_SignatureHelpContext;

   procedure Write_DeleteFilesParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DeleteFilesParams) is
      procedure Write_FileDelete_Vector
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.FileDelete_Vector);

      procedure Write_FileDelete_Vector
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.FileDelete_Vector) is
      begin
         Handler.Start_Array;
         for J in Value.First_Index .. Value.Last_Index loop
            Write_FileDelete (Handler, Value (J));
         end loop;
         Handler.End_Array;
      end Write_FileDelete_Vector;

   begin
      Handler.Start_Object;
      Handler.Key_Name ("files");
      Write_FileDelete_Vector (Handler, Value.files);
      Handler.End_Object;
   end Write_DeleteFilesParams;

   procedure Write_TypeHierarchyItem_Vector
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.TypeHierarchyItem_Vector) is
   begin
      Handler.Start_Array;
      for J in Value.First_Index .. Value.Last_Index loop
         Write_TypeHierarchyItem (Handler, Value (J));
      end loop;
      Handler.End_Array;
   end Write_TypeHierarchyItem_Vector;

   procedure Write_DocumentOnTypeFormattingParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DocumentOnTypeFormattingParams) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("textDocument");
      Write_TextDocumentIdentifier (Handler, Value.textDocument);
      Handler.Key_Name ("position");
      Write_Position (Handler, Value.position);
      Handler.Key_Name ("ch");
      Handler.String_Value (Value.ch);
      Handler.Key_Name ("options");
      Write_FormattingOptions (Handler, Value.options);
      Handler.End_Object;
   end Write_DocumentOnTypeFormattingParams;

   procedure Write_CallHierarchyOutgoingCall
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.CallHierarchyOutgoingCall) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("to");
      Write_CallHierarchyItem (Handler, Value.to);
      Handler.Key_Name ("fromRanges");
      Write_Range_Vector (Handler, Value.fromRanges);
      Handler.End_Object;
   end Write_CallHierarchyOutgoingCall;

   procedure Write_Location_Vector
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.Location_Vector) is
   begin
      Handler.Start_Array;
      for J in Value.First_Index .. Value.Last_Index loop
         Write_Location (Handler, Value (J));
      end loop;
      Handler.End_Array;
   end Write_Location_Vector;

   procedure Write_TextDocumentContentChangeEvent
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.TextDocumentContentChangeEvent) is
   begin
      Handler.Start_Object;
      if Value.a_range.Is_Set then
         Handler.Key_Name ("range");
         Write_A_Range (Handler, Value.a_range.Value);
      end if;
      if Value.rangeLength.Is_Set then
         Handler.Key_Name ("rangeLength");
         Handler.Integer_Value (Integer'Pos (Value.rangeLength.Value));
      end if;
      Handler.Key_Name ("text");
      Handler.String_Value (Value.text);
      Handler.End_Object;
   end Write_TextDocumentContentChangeEvent;

   procedure Write_DocumentColorParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DocumentColorParams) is
   begin
      Handler.Start_Object;
      if Value.workDoneToken.Is_Set then
         Handler.Key_Name ("workDoneToken");
         Write_ProgressToken (Handler, Value.workDoneToken.Value);
      end if;
      if Value.partialResultToken.Is_Set then
         Handler.Key_Name ("partialResultToken");
         Write_ProgressToken (Handler, Value.partialResultToken.Value);
      end if;
      Handler.Key_Name ("textDocument");
      Write_TextDocumentIdentifier (Handler, Value.textDocument);
      Handler.End_Object;
   end Write_DocumentColorParams;

   procedure Write_WorkspaceFolder
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.WorkspaceFolder) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("uri");
      Write_URI (Handler, Value.uri);
      Handler.Key_Name ("name");
      Handler.String_Value (Value.name);
      Handler.End_Object;
   end Write_WorkspaceFolder;

   procedure Write_MarkedString
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.MarkedString) is
   begin
      case Value.Is_Virtual_String is
         when True =>
            Handler.String_Value (Value.Virtual_String);
         when False =>
            Handler.Start_Object;
            Handler.Key_Name ("language");
            Handler.String_Value (Value.language);
            Handler.Key_Name ("value");
            Handler.String_Value (Value.value);
            Handler.End_Object;
      end case;
   end Write_MarkedString;

   procedure Write_NotebookCell
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.NotebookCell) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("kind");
      Write_NotebookCellKind (Handler, Value.kind);
      Handler.Key_Name ("document");
      Handler.String_Value (Value.document);
      if Value.metadata.Is_Set then
         Handler.Key_Name ("metadata");
         Write_LSPObject (Handler, Value.metadata.Value);
      end if;
      if Value.executionSummary.Is_Set then
         Handler.Key_Name ("executionSummary");
         Write_ExecutionSummary (Handler, Value.executionSummary.Value);
      end if;
      Handler.End_Object;
   end Write_NotebookCell;

   procedure Write_DocumentLink
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DocumentLink) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("range");
      Write_A_Range (Handler, Value.a_range);
      if not Value.target.Is_Null then
         Handler.Key_Name ("target");
         Handler.String_Value (Value.target);
      end if;
      if not Value.tooltip.Is_Null then
         Handler.Key_Name ("tooltip");
         Handler.String_Value (Value.tooltip);
      end if;
      if not Value.data.Is_Empty then
         Handler.Key_Name ("data");
         Write_LSPAny (Handler, Value.data);
      end if;
      Handler.End_Object;
   end Write_DocumentLink;

   procedure Write_WorkspaceFoldersServerCapabilities
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.WorkspaceFoldersServerCapabilities) is
      procedure Write_Virtual_String_Or_Boolean
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.Virtual_String_Or_Boolean);

      procedure Write_Virtual_String_Or_Boolean
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.Virtual_String_Or_Boolean) is
      begin
         case Value.Is_Virtual_String is
            when True =>
               Handler.String_Value (Value.Virtual_String);
            when False =>
               Handler.Boolean_Value (Value.Boolean);
         end case;
      end Write_Virtual_String_Or_Boolean;

   begin
      Handler.Start_Object;
      if Value.supported.Is_Set then
         Handler.Key_Name ("supported");
         Handler.Boolean_Value (Value.supported.Value);
      end if;
      if Value.changeNotifications.Is_Set then
         Handler.Key_Name ("changeNotifications");
         Write_Virtual_String_Or_Boolean
           (Handler, Value.changeNotifications.Value);
      end if;
      Handler.End_Object;
   end Write_WorkspaceFoldersServerCapabilities;

   procedure Write_WorkDoneProgressEnd
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.WorkDoneProgressEnd) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("kind");
      Handler.String_Value ("end");
      if not Value.message.Is_Null then
         Handler.Key_Name ("message");
         Handler.String_Value (Value.message);
      end if;
      Handler.End_Object;
   end Write_WorkDoneProgressEnd;

   procedure Write_SemanticTokensRegistrationOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.SemanticTokensRegistrationOptions) is
      procedure Write_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.DocumentSelector_Or_Null);

      procedure Write_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.DocumentSelector_Or_Null) is
      begin
         if Value.Is_Null then
            Handler.Null_Value;
         else
            Write_DocumentSelector (Handler, Value.Value);
         end if;
      end Write_DocumentSelector_Or_Null;

   begin
      Handler.Start_Object;
      Handler.Key_Name ("documentSelector");
      Write_DocumentSelector_Or_Null (Handler, Value.Parent.documentSelector);
      if Value.workDoneProgress.Is_Set then
         Handler.Key_Name ("workDoneProgress");
         Handler.Boolean_Value (Value.workDoneProgress.Value);
      end if;
      Handler.Key_Name ("legend");
      Write_SemanticTokensLegend (Handler, Value.legend);
      if Value.a_range.Is_Set then
         Handler.Key_Name ("range");
         Write_Boolean_Or_Any (Handler, Value.a_range.Value);
      end if;
      if Value.full.Is_Set then
         Handler.Key_Name ("full");
         Write_Boolean_Or_Something (Handler, Value.full.Value);
      end if;
      if not Value.id.Is_Null then
         Handler.Key_Name ("id");
         Handler.String_Value (Value.id);
      end if;
      Handler.End_Object;
   end Write_SemanticTokensRegistrationOptions;

   procedure Write_CompletionTriggerKind
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Enumerations.CompletionTriggerKind) is
   begin
      case Value is
         when LSP.Enumerations.Invoked =>
            Handler.Integer_Value (1);
         when LSP.Enumerations.TriggerCharacter =>
            Handler.Integer_Value (2);
         when LSP.Enumerations.TriggerForIncompleteCompletions =>
            Handler.Integer_Value (3);
      end case;
   end Write_CompletionTriggerKind;

   procedure Write_CallHierarchyRegistrationOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.CallHierarchyRegistrationOptions) is
      procedure Write_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.DocumentSelector_Or_Null);

      procedure Write_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.DocumentSelector_Or_Null) is
      begin
         if Value.Is_Null then
            Handler.Null_Value;
         else
            Write_DocumentSelector (Handler, Value.Value);
         end if;
      end Write_DocumentSelector_Or_Null;

   begin
      Handler.Start_Object;
      Handler.Key_Name ("documentSelector");
      Write_DocumentSelector_Or_Null (Handler, Value.Parent.documentSelector);
      if Value.workDoneProgress.Is_Set then
         Handler.Key_Name ("workDoneProgress");
         Handler.Boolean_Value (Value.workDoneProgress.Value);
      end if;
      if not Value.id.Is_Null then
         Handler.Key_Name ("id");
         Handler.String_Value (Value.id);
      end if;
      Handler.End_Object;
   end Write_CallHierarchyRegistrationOptions;

   procedure Write_CallHierarchyClientCapabilities
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.CallHierarchyClientCapabilities) is
   begin
      Handler.Start_Object;
      if Value.dynamicRegistration.Is_Set then
         Handler.Key_Name ("dynamicRegistration");
         Handler.Boolean_Value (Value.dynamicRegistration.Value);
      end if;
      Handler.End_Object;
   end Write_CallHierarchyClientCapabilities;

   procedure Write_DocumentSymbolParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DocumentSymbolParams) is
   begin
      Handler.Start_Object;
      if Value.workDoneToken.Is_Set then
         Handler.Key_Name ("workDoneToken");
         Write_ProgressToken (Handler, Value.workDoneToken.Value);
      end if;
      if Value.partialResultToken.Is_Set then
         Handler.Key_Name ("partialResultToken");
         Write_ProgressToken (Handler, Value.partialResultToken.Value);
      end if;
      Handler.Key_Name ("textDocument");
      Write_TextDocumentIdentifier (Handler, Value.textDocument);
      Handler.End_Object;
   end Write_DocumentSymbolParams;

   procedure Write_DocumentRangeFormattingOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DocumentRangeFormattingOptions) is
   begin
      Handler.Start_Object;
      if Value.workDoneProgress.Is_Set then
         Handler.Key_Name ("workDoneProgress");
         Handler.Boolean_Value (Value.workDoneProgress.Value);
      end if;
      Handler.End_Object;
   end Write_DocumentRangeFormattingOptions;

   procedure Write_InlayHintWorkspaceClientCapabilities
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.InlayHintWorkspaceClientCapabilities) is
   begin
      Handler.Start_Object;
      if Value.refreshSupport.Is_Set then
         Handler.Key_Name ("refreshSupport");
         Handler.Boolean_Value (Value.refreshSupport.Value);
      end if;
      Handler.End_Object;
   end Write_InlayHintWorkspaceClientCapabilities;

   procedure Write_InlineValue_Vector
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.InlineValue_Vector) is
   begin
      Handler.Start_Array;
      for J in Value.First_Index .. Value.Last_Index loop
         Write_InlineValue (Handler, Value (J));
      end loop;
      Handler.End_Array;
   end Write_InlineValue_Vector;

   procedure Write_DiagnosticServerCancellationData
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DiagnosticServerCancellationData) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("retriggerRequest");
      Handler.Boolean_Value (Value.retriggerRequest);
      Handler.End_Object;
   end Write_DiagnosticServerCancellationData;

   procedure Write_DocumentColorOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DocumentColorOptions) is
   begin
      Handler.Start_Object;
      if Value.workDoneProgress.Is_Set then
         Handler.Key_Name ("workDoneProgress");
         Handler.Boolean_Value (Value.workDoneProgress.Value);
      end if;
      Handler.End_Object;
   end Write_DocumentColorOptions;

   procedure Write_InitializedParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.InitializedParams) is
   begin
      Handler.Start_Object;
      null;
      Handler.End_Object;
   end Write_InitializedParams;

   procedure Write_FileOperationPatternKind
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Enumerations.FileOperationPatternKind) is
   begin
      case Value is
         when LSP.Enumerations.file =>
            Handler.String_Value ("file");
         when LSP.Enumerations.folder =>
            Handler.String_Value ("folder");
      end case;
   end Write_FileOperationPatternKind;

   procedure Write_Unregistration
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.Unregistration) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("id");
      Handler.String_Value (Value.id);
      Handler.Key_Name ("method");
      Handler.String_Value (Value.method);
      Handler.End_Object;
   end Write_Unregistration;

   procedure Write_HoverOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.HoverOptions) is
   begin
      Handler.Start_Object;
      if Value.workDoneProgress.Is_Set then
         Handler.Key_Name ("workDoneProgress");
         Handler.Boolean_Value (Value.workDoneProgress.Value);
      end if;
      Handler.End_Object;
   end Write_HoverOptions;

   procedure Write_DocumentOnTypeFormattingRegistrationOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DocumentOnTypeFormattingRegistrationOptions) is
      procedure Write_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.DocumentSelector_Or_Null);

      procedure Write_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.DocumentSelector_Or_Null) is
      begin
         if Value.Is_Null then
            Handler.Null_Value;
         else
            Write_DocumentSelector (Handler, Value.Value);
         end if;
      end Write_DocumentSelector_Or_Null;

   begin
      Handler.Start_Object;
      Handler.Key_Name ("documentSelector");
      Write_DocumentSelector_Or_Null (Handler, Value.Parent.documentSelector);
      Handler.Key_Name ("firstTriggerCharacter");
      Handler.String_Value (Value.firstTriggerCharacter);
      if not Value.moreTriggerCharacter.Is_Empty then
         Handler.Key_Name ("moreTriggerCharacter");
         Write_Virtual_String_Vector (Handler, Value.moreTriggerCharacter);
      end if;
      Handler.End_Object;
   end Write_DocumentOnTypeFormattingRegistrationOptions;

   procedure Write_relatedDocuments_OfDocumentDiagnosticReportPartialResult
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures
        .relatedDocuments_OfDocumentDiagnosticReportPartialResult) is
   begin
      declare
         use
           LSP.Structures
             .relatedDocuments_OfDocumentDiagnosticReportPartialResult_Item_Maps;
      begin
         Handler.Start_Object;
         for Cursor in Value.Iterate loop
            Handler.Key_Name (Key (Cursor));
            Write_relatedDocuments_OfDocumentDiagnosticReportPartialResult_Item
              (Handler, Value (Cursor));
         end loop;
         Handler.End_Object;
      end;
   end Write_relatedDocuments_OfDocumentDiagnosticReportPartialResult;

   procedure Write_ReferenceClientCapabilities
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.ReferenceClientCapabilities) is
   begin
      Handler.Start_Object;
      if Value.dynamicRegistration.Is_Set then
         Handler.Key_Name ("dynamicRegistration");
         Handler.Boolean_Value (Value.dynamicRegistration.Value);
      end if;
      Handler.End_Object;
   end Write_ReferenceClientCapabilities;

   procedure Write_MonikerClientCapabilities
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.MonikerClientCapabilities) is
   begin
      Handler.Start_Object;
      if Value.dynamicRegistration.Is_Set then
         Handler.Key_Name ("dynamicRegistration");
         Handler.Boolean_Value (Value.dynamicRegistration.Value);
      end if;
      Handler.End_Object;
   end Write_MonikerClientCapabilities;

   procedure Write_CompletionItemTag_Set
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.CompletionItemTag_Set) is
   begin
      Handler.Start_Array;
      declare
         Set : LSP.Structures.CompletionItemTag_Set renames Value;
      begin
         for Value in Set'Range loop
            if Set (Value) then
               Write_CompletionItemTag (Handler, Value);
            end if;
         end loop;
      end;
      Handler.End_Array;
   end Write_CompletionItemTag_Set;

   procedure Write_ColorInformation
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.ColorInformation) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("range");
      Write_A_Range (Handler, Value.a_range);
      Handler.Key_Name ("color");
      Write_Color (Handler, Value.color);
      Handler.End_Object;
   end Write_ColorInformation;

   procedure Write_A_Range
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.A_Range) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("start");
      Write_Position (Handler, Value.start);
      Handler.Key_Name ("end");
      Write_Position (Handler, Value.an_end);
      Handler.End_Object;
   end Write_A_Range;

   procedure Write_ShowMessageRequestParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.ShowMessageRequestParams) is
      procedure Write_MessageActionItem_Vector
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.MessageActionItem_Vector);

      procedure Write_MessageActionItem_Vector
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.MessageActionItem_Vector) is
      begin
         Handler.Start_Array;
         for J in Value.First_Index .. Value.Last_Index loop
            Write_MessageActionItem (Handler, Value (J));
         end loop;
         Handler.End_Array;
      end Write_MessageActionItem_Vector;

   begin
      Handler.Start_Object;
      Handler.Key_Name ("type");
      Write_MessageType (Handler, Value.a_type);
      Handler.Key_Name ("message");
      Handler.String_Value (Value.message);
      if not Value.actions.Is_Empty then
         Handler.Key_Name ("actions");
         Write_MessageActionItem_Vector (Handler, Value.actions);
      end if;
      Handler.End_Object;
   end Write_ShowMessageRequestParams;

   procedure Write_DocumentColorClientCapabilities
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DocumentColorClientCapabilities) is
   begin
      Handler.Start_Object;
      if Value.dynamicRegistration.Is_Set then
         Handler.Key_Name ("dynamicRegistration");
         Handler.Boolean_Value (Value.dynamicRegistration.Value);
      end if;
      Handler.End_Object;
   end Write_DocumentColorClientCapabilities;

   procedure Write_WorkspaceEdit_Or_Null
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.WorkspaceEdit_Or_Null) is
   begin
      if Value.Is_Null then
         Handler.Null_Value;
      else
         Write_WorkspaceEdit (Handler, Value.Value);
      end if;
   end Write_WorkspaceEdit_Or_Null;

   procedure Write_InlineValueEvaluatableExpression
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.InlineValueEvaluatableExpression) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("range");
      Write_A_Range (Handler, Value.a_range);
      if not Value.expression.Is_Null then
         Handler.Key_Name ("expression");
         Handler.String_Value (Value.expression);
      end if;
      Handler.End_Object;
   end Write_InlineValueEvaluatableExpression;

   procedure Write_Pattern
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.Pattern) is
   begin
      Handler.String_Value (Value);
   end Write_Pattern;

   procedure Write_DiagnosticTag_Set
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DiagnosticTag_Set) is
   begin
      Handler.Start_Array;
      declare
         Set : LSP.Structures.DiagnosticTag_Set renames Value;
      begin
         for Value in Set'Range loop
            if Set (Value) then
               Write_DiagnosticTag (Handler, Value);
            end if;
         end loop;
      end;
      Handler.End_Array;
   end Write_DiagnosticTag_Set;

   procedure Write_NotebookCellArrayChange
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.NotebookCellArrayChange) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("start");
      Handler.Integer_Value (Integer'Pos (Value.start));
      Handler.Key_Name ("deleteCount");
      Handler.Integer_Value (Integer'Pos (Value.deleteCount));
      if not Value.cells.Is_Empty then
         Handler.Key_Name ("cells");
         Write_NotebookCell_Vector (Handler, Value.cells);
      end if;
      Handler.End_Object;
   end Write_NotebookCellArrayChange;

   procedure Write_DocumentLinkRegistrationOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DocumentLinkRegistrationOptions) is
      procedure Write_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.DocumentSelector_Or_Null);

      procedure Write_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.DocumentSelector_Or_Null) is
      begin
         if Value.Is_Null then
            Handler.Null_Value;
         else
            Write_DocumentSelector (Handler, Value.Value);
         end if;
      end Write_DocumentSelector_Or_Null;

   begin
      Handler.Start_Object;
      Handler.Key_Name ("documentSelector");
      Write_DocumentSelector_Or_Null (Handler, Value.Parent.documentSelector);
      if Value.workDoneProgress.Is_Set then
         Handler.Key_Name ("workDoneProgress");
         Handler.Boolean_Value (Value.workDoneProgress.Value);
      end if;
      if Value.resolveProvider.Is_Set then
         Handler.Key_Name ("resolveProvider");
         Handler.Boolean_Value (Value.resolveProvider.Value);
      end if;
      Handler.End_Object;
   end Write_DocumentLinkRegistrationOptions;

   procedure Write_DocumentHighlight_Vector_Or_Null
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DocumentHighlight_Vector_Or_Null) is
   begin
      if Value.Is_Empty then
         Handler.Null_Value;
      else
         Write_DocumentHighlight_Vector (Handler, Value);
      end if;
   end Write_DocumentHighlight_Vector_Or_Null;

   procedure Write_LogMessageParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.LogMessageParams) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("type");
      Write_MessageType (Handler, Value.a_type);
      Handler.Key_Name ("message");
      Handler.String_Value (Value.message);
      Handler.End_Object;
   end Write_LogMessageParams;

   procedure Write_CodeLens
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.CodeLens) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("range");
      Write_A_Range (Handler, Value.a_range);
      if Value.command.Is_Set then
         Handler.Key_Name ("command");
         Write_Command (Handler, Value.command.Value);
      end if;
      if not Value.data.Is_Empty then
         Handler.Key_Name ("data");
         Write_LSPAny (Handler, Value.data);
      end if;
      Handler.End_Object;
   end Write_CodeLens;

   procedure Write_DocumentDiagnosticReportPartialResult
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DocumentDiagnosticReportPartialResult) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("relatedDocuments");
      declare
         use
           LSP.Structures
             .relatedDocuments_OfDocumentDiagnosticReportPartialResult_Item_Maps;
      begin
         Handler.Start_Object;
         for Cursor in Value.relatedDocuments.Iterate loop
            Handler.Key_Name (Key (Cursor));
            Write_relatedDocuments_OfDocumentDiagnosticReportPartialResult_Item
              (Handler, Value.relatedDocuments (Cursor));
         end loop;
         Handler.End_Object;
      end;
      Handler.End_Object;
   end Write_DocumentDiagnosticReportPartialResult;

   procedure Write_CallHierarchyIncomingCall_Vector
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.CallHierarchyIncomingCall_Vector) is
   begin
      Handler.Start_Array;
      for J in Value.First_Index .. Value.Last_Index loop
         Write_CallHierarchyIncomingCall (Handler, Value (J));
      end loop;
      Handler.End_Array;
   end Write_CallHierarchyIncomingCall_Vector;

   procedure Write_ProgressParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.ProgressParams) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("token");
      Write_ProgressToken (Handler, Value.token);
      Handler.Key_Name ("value");
      Write_LSPAny (Handler, Value.value);
      Handler.End_Object;
   end Write_ProgressParams;

   procedure Write_DocumentLinkOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DocumentLinkOptions) is
   begin
      Handler.Start_Object;
      if Value.workDoneProgress.Is_Set then
         Handler.Key_Name ("workDoneProgress");
         Handler.Boolean_Value (Value.workDoneProgress.Value);
      end if;
      if Value.resolveProvider.Is_Set then
         Handler.Key_Name ("resolveProvider");
         Handler.Boolean_Value (Value.resolveProvider.Value);
      end if;
      Handler.End_Object;
   end Write_DocumentLinkOptions;

   procedure Write_PrepareSupportDefaultBehavior
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Enumerations.PrepareSupportDefaultBehavior) is
   begin
      case Value is
         when LSP.Enumerations.Identifier =>
            Handler.Integer_Value (1);
      end case;
   end Write_PrepareSupportDefaultBehavior;

   procedure Write_TextDocumentIdentifier
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.TextDocumentIdentifier) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("uri");
      Handler.String_Value (Value.uri);
      Handler.End_Object;
   end Write_TextDocumentIdentifier;

   procedure Write_DocumentSymbol_Result
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DocumentSymbol_Result) is
   begin
      case Value.Kind is
         when LSP.Structures.Variant_1 =>
            Write_SymbolInformation_Vector (Handler, Value.Variant_1);
         when LSP.Structures.Variant_2 =>
            Write_DocumentSymbol_Vector (Handler, Value.Variant_2);
         when LSP.Structures.Variant_3 =>
            Handler.Null_Value;
      end case;
   end Write_DocumentSymbol_Result;

   procedure Write_TraceValues
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Enumerations.TraceValues) is
   begin
      case Value is
         when LSP.Enumerations.Off =>
            Handler.String_Value ("off");
         when LSP.Enumerations.Messages =>
            Handler.String_Value ("messages");
         when LSP.Enumerations.Verbose =>
            Handler.String_Value ("verbose");
      end case;
   end Write_TraceValues;

   procedure Write_TypeHierarchyPrepareParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.TypeHierarchyPrepareParams) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("textDocument");
      Write_TextDocumentIdentifier (Handler, Value.textDocument);
      Handler.Key_Name ("position");
      Write_Position (Handler, Value.position);
      if Value.workDoneToken.Is_Set then
         Handler.Key_Name ("workDoneToken");
         Write_ProgressToken (Handler, Value.workDoneToken.Value);
      end if;
      Handler.End_Object;
   end Write_TypeHierarchyPrepareParams;

   procedure Write_SymbolInformation
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.SymbolInformation) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("name");
      Handler.String_Value (Value.name);
      Handler.Key_Name ("kind");
      Write_SymbolKind (Handler, Value.kind);
      if (for some Item of Value.tags => Item) then
         Handler.Key_Name ("tags");
         Write_SymbolTag_Set (Handler, Value.tags);
      end if;
      if not Value.containerName.Is_Null then
         Handler.Key_Name ("containerName");
         Handler.String_Value (Value.containerName);
      end if;
      if Value.deprecated.Is_Set then
         Handler.Key_Name ("deprecated");
         Handler.Boolean_Value (Value.deprecated.Value);
      end if;
      Handler.Key_Name ("location");
      Write_Location (Handler, Value.location);
      Handler.End_Object;
   end Write_SymbolInformation;

   procedure Write_WillSaveTextDocumentParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.WillSaveTextDocumentParams) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("textDocument");
      Write_TextDocumentIdentifier (Handler, Value.textDocument);
      Handler.Key_Name ("reason");
      Write_TextDocumentSaveReason (Handler, Value.reason);
      Handler.End_Object;
   end Write_WillSaveTextDocumentParams;

   procedure Write_Command_Or_CodeAction_Vector_Or_Null
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.Command_Or_CodeAction_Vector_Or_Null) is
   begin
      if Value.Is_Empty then
         Handler.Null_Value;
      else
         Write_Command_Or_CodeAction_Vector (Handler, Value);
      end if;
   end Write_Command_Or_CodeAction_Vector_Or_Null;

   procedure Write_FileOperationClientCapabilities
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.FileOperationClientCapabilities) is
   begin
      Handler.Start_Object;
      if Value.dynamicRegistration.Is_Set then
         Handler.Key_Name ("dynamicRegistration");
         Handler.Boolean_Value (Value.dynamicRegistration.Value);
      end if;
      if Value.didCreate.Is_Set then
         Handler.Key_Name ("didCreate");
         Handler.Boolean_Value (Value.didCreate.Value);
      end if;
      if Value.willCreate.Is_Set then
         Handler.Key_Name ("willCreate");
         Handler.Boolean_Value (Value.willCreate.Value);
      end if;
      if Value.didRename.Is_Set then
         Handler.Key_Name ("didRename");
         Handler.Boolean_Value (Value.didRename.Value);
      end if;
      if Value.willRename.Is_Set then
         Handler.Key_Name ("willRename");
         Handler.Boolean_Value (Value.willRename.Value);
      end if;
      if Value.didDelete.Is_Set then
         Handler.Key_Name ("didDelete");
         Handler.Boolean_Value (Value.didDelete.Value);
      end if;
      if Value.willDelete.Is_Set then
         Handler.Key_Name ("willDelete");
         Handler.Boolean_Value (Value.willDelete.Value);
      end if;
      Handler.End_Object;
   end Write_FileOperationClientCapabilities;

   procedure Write_Definition_Result
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.Definition_Result) is
   begin
      case Value.Kind is
         when LSP.Structures.Variant_1 =>
            Write_Definition (Handler, Value.Variant_1);
         when LSP.Structures.Variant_2 =>
            Write_DefinitionLink_Vector (Handler, Value.Variant_2);
         when LSP.Structures.Variant_3 =>
            Handler.Null_Value;
      end case;
   end Write_Definition_Result;

   procedure Write_DocumentHighlightClientCapabilities
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DocumentHighlightClientCapabilities) is
   begin
      Handler.Start_Object;
      if Value.dynamicRegistration.Is_Set then
         Handler.Key_Name ("dynamicRegistration");
         Handler.Boolean_Value (Value.dynamicRegistration.Value);
      end if;
      Handler.End_Object;
   end Write_DocumentHighlightClientCapabilities;

   procedure Write_InlineValueText
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.InlineValueText) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("range");
      Write_A_Range (Handler, Value.a_range);
      Handler.Key_Name ("text");
      Handler.String_Value (Value.text);
      Handler.End_Object;
   end Write_InlineValueText;

   procedure Write_UnchangedDocumentDiagnosticReport
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.UnchangedDocumentDiagnosticReport) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("kind");
      Handler.String_Value ("unchanged");
      Handler.Key_Name ("resultId");
      Handler.String_Value (Value.resultId);
      Handler.End_Object;
   end Write_UnchangedDocumentDiagnosticReport;

   procedure Write_SymbolTag
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Enumerations.SymbolTag) is
   begin
      case Value is
         when LSP.Enumerations.Deprecated =>
            Handler.Integer_Value (1);
      end case;
   end Write_SymbolTag;

   procedure Write_Registration
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.Registration) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("id");
      Handler.String_Value (Value.id);
      Handler.Key_Name ("method");
      Handler.String_Value (Value.method);
      if not Value.registerOptions.Is_Empty then
         Handler.Key_Name ("registerOptions");
         Write_LSPAny (Handler, Value.registerOptions);
      end if;
      Handler.End_Object;
   end Write_Registration;

   procedure Write_CompletionItem
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.CompletionItem) is
      procedure Write_TextEdit_Or_InsertReplaceEdit
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.TextEdit_Or_InsertReplaceEdit);

      procedure Write_TextEdit_Or_InsertReplaceEdit
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.TextEdit_Or_InsertReplaceEdit) is
      begin
         case Value.Is_TextEdit is
            when True =>
               Write_TextEdit (Handler, Value.TextEdit);
            when False =>
               Write_InsertReplaceEdit (Handler, Value.InsertReplaceEdit);
         end case;
      end Write_TextEdit_Or_InsertReplaceEdit;

   begin
      Handler.Start_Object;
      Handler.Key_Name ("label");
      Handler.String_Value (Value.label);
      if Value.labelDetails.Is_Set then
         Handler.Key_Name ("labelDetails");
         Write_CompletionItemLabelDetails (Handler, Value.labelDetails.Value);
      end if;
      if Value.kind.Is_Set then
         Handler.Key_Name ("kind");
         Write_CompletionItemKind (Handler, Value.kind.Value);
      end if;
      if (for some Item of Value.tags => Item) then
         Handler.Key_Name ("tags");
         Write_CompletionItemTag_Set (Handler, Value.tags);
      end if;
      if not Value.detail.Is_Null then
         Handler.Key_Name ("detail");
         Handler.String_Value (Value.detail);
      end if;
      if Value.documentation.Is_Set then
         Handler.Key_Name ("documentation");
         Write_Virtual_String_Or_MarkupContent
           (Handler, Value.documentation.Value);
      end if;
      if Value.deprecated.Is_Set then
         Handler.Key_Name ("deprecated");
         Handler.Boolean_Value (Value.deprecated.Value);
      end if;
      if Value.preselect.Is_Set then
         Handler.Key_Name ("preselect");
         Handler.Boolean_Value (Value.preselect.Value);
      end if;
      if not Value.sortText.Is_Null then
         Handler.Key_Name ("sortText");
         Handler.String_Value (Value.sortText);
      end if;
      if not Value.filterText.Is_Null then
         Handler.Key_Name ("filterText");
         Handler.String_Value (Value.filterText);
      end if;
      if not Value.insertText.Is_Null then
         Handler.Key_Name ("insertText");
         Handler.String_Value (Value.insertText);
      end if;
      if Value.insertTextFormat.Is_Set then
         Handler.Key_Name ("insertTextFormat");
         Write_InsertTextFormat (Handler, Value.insertTextFormat.Value);
      end if;
      if Value.insertTextMode.Is_Set then
         Handler.Key_Name ("insertTextMode");
         Write_InsertTextMode (Handler, Value.insertTextMode.Value);
      end if;
      if Value.textEdit.Is_Set then
         Handler.Key_Name ("textEdit");
         Write_TextEdit_Or_InsertReplaceEdit (Handler, Value.textEdit.Value);
      end if;
      if not Value.textEditText.Is_Null then
         Handler.Key_Name ("textEditText");
         Handler.String_Value (Value.textEditText);
      end if;
      if not Value.additionalTextEdits.Is_Empty then
         Handler.Key_Name ("additionalTextEdits");
         Write_TextEdit_Vector (Handler, Value.additionalTextEdits);
      end if;
      if not Value.commitCharacters.Is_Empty then
         Handler.Key_Name ("commitCharacters");
         Write_Virtual_String_Vector (Handler, Value.commitCharacters);
      end if;
      if Value.command.Is_Set then
         Handler.Key_Name ("command");
         Write_Command (Handler, Value.command.Value);
      end if;
      if not Value.data.Is_Empty then
         Handler.Key_Name ("data");
         Write_LSPAny (Handler, Value.data);
      end if;
      Handler.End_Object;
   end Write_CompletionItem;

   procedure Write_DocumentRangeFormattingRegistrationOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DocumentRangeFormattingRegistrationOptions) is
      procedure Write_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.DocumentSelector_Or_Null);

      procedure Write_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.DocumentSelector_Or_Null) is
      begin
         if Value.Is_Null then
            Handler.Null_Value;
         else
            Write_DocumentSelector (Handler, Value.Value);
         end if;
      end Write_DocumentSelector_Or_Null;

   begin
      Handler.Start_Object;
      Handler.Key_Name ("documentSelector");
      Write_DocumentSelector_Or_Null (Handler, Value.Parent.documentSelector);
      if Value.workDoneProgress.Is_Set then
         Handler.Key_Name ("workDoneProgress");
         Handler.Boolean_Value (Value.workDoneProgress.Value);
      end if;
      Handler.End_Object;
   end Write_DocumentRangeFormattingRegistrationOptions;

   procedure Write_Moniker_Vector
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.Moniker_Vector) is
   begin
      Handler.Start_Array;
      for J in Value.First_Index .. Value.Last_Index loop
         Write_Moniker (Handler, Value (J));
      end loop;
      Handler.End_Array;
   end Write_Moniker_Vector;

   procedure Write_RenameFileOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.RenameFileOptions) is
   begin
      Handler.Start_Object;
      if Value.overwrite.Is_Set then
         Handler.Key_Name ("overwrite");
         Handler.Boolean_Value (Value.overwrite.Value);
      end if;
      if Value.ignoreIfExists.Is_Set then
         Handler.Key_Name ("ignoreIfExists");
         Handler.Boolean_Value (Value.ignoreIfExists.Value);
      end if;
      Handler.End_Object;
   end Write_RenameFileOptions;

   procedure Write_HoverParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.HoverParams) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("textDocument");
      Write_TextDocumentIdentifier (Handler, Value.textDocument);
      Handler.Key_Name ("position");
      Write_Position (Handler, Value.position);
      if Value.workDoneToken.Is_Set then
         Handler.Key_Name ("workDoneToken");
         Write_ProgressToken (Handler, Value.workDoneToken.Value);
      end if;
      Handler.End_Object;
   end Write_HoverParams;

   procedure Write_DocumentSymbol
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DocumentSymbol) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("name");
      Handler.String_Value (Value.name);
      if not Value.detail.Is_Null then
         Handler.Key_Name ("detail");
         Handler.String_Value (Value.detail);
      end if;
      Handler.Key_Name ("kind");
      Write_SymbolKind (Handler, Value.kind);
      if (for some Item of Value.tags => Item) then
         Handler.Key_Name ("tags");
         Write_SymbolTag_Set (Handler, Value.tags);
      end if;
      if Value.deprecated.Is_Set then
         Handler.Key_Name ("deprecated");
         Handler.Boolean_Value (Value.deprecated.Value);
      end if;
      Handler.Key_Name ("range");
      Write_A_Range (Handler, Value.a_range);
      Handler.Key_Name ("selectionRange");
      Write_A_Range (Handler, Value.selectionRange);
      if Value.children.Length > 0 then
         Handler.Key_Name ("children");
         Write_DocumentSymbol_Vector (Handler, Value.children);
      end if;
      Handler.End_Object;
   end Write_DocumentSymbol;

   procedure Write_DocumentSymbolClientCapabilities
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DocumentSymbolClientCapabilities) is
   begin
      Handler.Start_Object;
      if Value.dynamicRegistration.Is_Set then
         Handler.Key_Name ("dynamicRegistration");
         Handler.Boolean_Value (Value.dynamicRegistration.Value);
      end if;
      if Value.symbolKind.Is_Set then
         Handler.Key_Name ("symbolKind");
         Write_symbolKind_OfWorkspaceSymbolClientCapabilities
           (Handler, Value.symbolKind.Value);
      end if;
      if Value.hierarchicalDocumentSymbolSupport.Is_Set then
         Handler.Key_Name ("hierarchicalDocumentSymbolSupport");
         Handler.Boolean_Value (Value.hierarchicalDocumentSymbolSupport.Value);
      end if;
      if Value.tagSupport.Is_Set then
         Handler.Key_Name ("tagSupport");
         Write_tagSupport_OfWorkspaceSymbolClientCapabilities
           (Handler, Value.tagSupport.Value);
      end if;
      if Value.labelSupport.Is_Set then
         Handler.Key_Name ("labelSupport");
         Handler.Boolean_Value (Value.labelSupport.Value);
      end if;
      Handler.End_Object;
   end Write_DocumentSymbolClientCapabilities;

   procedure Write_InitializeError
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.InitializeError) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("retry");
      Handler.Boolean_Value (Value.retry);
      Handler.End_Object;
   end Write_InitializeError;

   procedure Write_LinkedEditingRangeRegistrationOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.LinkedEditingRangeRegistrationOptions) is
      procedure Write_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.DocumentSelector_Or_Null);

      procedure Write_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.DocumentSelector_Or_Null) is
      begin
         if Value.Is_Null then
            Handler.Null_Value;
         else
            Write_DocumentSelector (Handler, Value.Value);
         end if;
      end Write_DocumentSelector_Or_Null;

   begin
      Handler.Start_Object;
      Handler.Key_Name ("documentSelector");
      Write_DocumentSelector_Or_Null (Handler, Value.Parent.documentSelector);
      if Value.workDoneProgress.Is_Set then
         Handler.Key_Name ("workDoneProgress");
         Handler.Boolean_Value (Value.workDoneProgress.Value);
      end if;
      if not Value.id.Is_Null then
         Handler.Key_Name ("id");
         Handler.String_Value (Value.id);
      end if;
      Handler.End_Object;
   end Write_LinkedEditingRangeRegistrationOptions;

   procedure Write_SemanticTokensClientCapabilities
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.SemanticTokensClientCapabilities) is
      procedure Write_requests_OfSemanticTokensClientCapabilities
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.requests_OfSemanticTokensClientCapabilities);

      procedure Write_TokenFormat_Set
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.TokenFormat_Set);

      procedure Write_requests_OfSemanticTokensClientCapabilities
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value : LSP.Structures.requests_OfSemanticTokensClientCapabilities) is
      begin
         Handler.Start_Object;
         if Value.a_range.Is_Set then
            Handler.Key_Name ("range");
            Write_Boolean_Or_Any (Handler, Value.a_range.Value);
         end if;
         if Value.full.Is_Set then
            Handler.Key_Name ("full");
            Write_Boolean_Or_Something (Handler, Value.full.Value);
         end if;
         Handler.End_Object;
      end Write_requests_OfSemanticTokensClientCapabilities;

      procedure Write_TokenFormat_Set
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.TokenFormat_Set) is
      begin
         Handler.Start_Array;
         declare
            Set : LSP.Structures.TokenFormat_Set renames Value;
         begin
            for Value in Set'Range loop
               if Set (Value) then
                  Write_TokenFormat (Handler, Value);
               end if;
            end loop;
         end;
         Handler.End_Array;
      end Write_TokenFormat_Set;

   begin
      Handler.Start_Object;
      if Value.dynamicRegistration.Is_Set then
         Handler.Key_Name ("dynamicRegistration");
         Handler.Boolean_Value (Value.dynamicRegistration.Value);
      end if;
      Handler.Key_Name ("requests");
      Write_requests_OfSemanticTokensClientCapabilities
        (Handler, Value.requests);
      Handler.Key_Name ("tokenTypes");
      Write_Virtual_String_Vector (Handler, Value.tokenTypes);
      Handler.Key_Name ("tokenModifiers");
      Write_Virtual_String_Vector (Handler, Value.tokenModifiers);
      Handler.Key_Name ("formats");
      Write_TokenFormat_Set (Handler, Value.formats);
      if Value.overlappingTokenSupport.Is_Set then
         Handler.Key_Name ("overlappingTokenSupport");
         Handler.Boolean_Value (Value.overlappingTokenSupport.Value);
      end if;
      if Value.multilineTokenSupport.Is_Set then
         Handler.Key_Name ("multilineTokenSupport");
         Handler.Boolean_Value (Value.multilineTokenSupport.Value);
      end if;
      if Value.serverCancelSupport.Is_Set then
         Handler.Key_Name ("serverCancelSupport");
         Handler.Boolean_Value (Value.serverCancelSupport.Value);
      end if;
      if Value.augmentsSyntaxTokens.Is_Set then
         Handler.Key_Name ("augmentsSyntaxTokens");
         Handler.Boolean_Value (Value.augmentsSyntaxTokens.Value);
      end if;
      Handler.End_Object;
   end Write_SemanticTokensClientCapabilities;

   procedure Write_Hover
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.Hover) is
      procedure Write_MarkedString_Vector
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.MarkedString_Vector);

      procedure Write_MarkupContent_Or_MarkedString_Vector
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.MarkupContent_Or_MarkedString_Vector);

      procedure Write_MarkedString_Vector
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.MarkedString_Vector) is
      begin
         Handler.Start_Array;
         for J in Value.First_Index .. Value.Last_Index loop
            Write_MarkedString (Handler, Value (J));
         end loop;
         Handler.End_Array;
      end Write_MarkedString_Vector;

      procedure Write_MarkupContent_Or_MarkedString_Vector
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.MarkupContent_Or_MarkedString_Vector) is
      begin
         case Value.Is_MarkupContent is
            when True =>
               Write_MarkupContent (Handler, Value.MarkupContent);
            when False =>
               Write_MarkedString_Vector (Handler, Value.MarkedString_Vector);
         end case;
      end Write_MarkupContent_Or_MarkedString_Vector;

   begin
      Handler.Start_Object;
      Handler.Key_Name ("contents");
      Write_MarkupContent_Or_MarkedString_Vector (Handler, Value.contents);
      if Value.a_range.Is_Set then
         Handler.Key_Name ("range");
         Write_A_Range (Handler, Value.a_range.Value);
      end if;
      Handler.End_Object;
   end Write_Hover;

   procedure Write_InlineValueWorkspaceClientCapabilities
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.InlineValueWorkspaceClientCapabilities) is
   begin
      Handler.Start_Object;
      if Value.refreshSupport.Is_Set then
         Handler.Key_Name ("refreshSupport");
         Handler.Boolean_Value (Value.refreshSupport.Value);
      end if;
      Handler.End_Object;
   end Write_InlineValueWorkspaceClientCapabilities;

   procedure Write_ResourceOperation
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.ResourceOperation) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("kind");
      Handler.String_Value (Value.kind);
      if Value.annotationId.Is_Set then
         Handler.Key_Name ("annotationId");
         Write_ChangeAnnotationIdentifier (Handler, Value.annotationId.Value);
      end if;
      Handler.End_Object;
   end Write_ResourceOperation;

   procedure Write_PublishDiagnosticsClientCapabilities
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.PublishDiagnosticsClientCapabilities) is
      procedure Write_tagSupport_OfPublishDiagnosticsClientCapabilities
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures
           .tagSupport_OfPublishDiagnosticsClientCapabilities);

      procedure Write_tagSupport_OfPublishDiagnosticsClientCapabilities
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures
           .tagSupport_OfPublishDiagnosticsClientCapabilities) is
      begin
         Handler.Start_Object;
         Handler.Key_Name ("valueSet");
         Write_DiagnosticTag_Set (Handler, Value.valueSet);
         Handler.End_Object;
      end Write_tagSupport_OfPublishDiagnosticsClientCapabilities;

   begin
      Handler.Start_Object;
      if Value.relatedInformation.Is_Set then
         Handler.Key_Name ("relatedInformation");
         Handler.Boolean_Value (Value.relatedInformation.Value);
      end if;
      if Value.tagSupport.Is_Set then
         Handler.Key_Name ("tagSupport");
         Write_tagSupport_OfPublishDiagnosticsClientCapabilities
           (Handler, Value.tagSupport.Value);
      end if;
      if Value.versionSupport.Is_Set then
         Handler.Key_Name ("versionSupport");
         Handler.Boolean_Value (Value.versionSupport.Value);
      end if;
      if Value.codeDescriptionSupport.Is_Set then
         Handler.Key_Name ("codeDescriptionSupport");
         Handler.Boolean_Value (Value.codeDescriptionSupport.Value);
      end if;
      if Value.dataSupport.Is_Set then
         Handler.Key_Name ("dataSupport");
         Handler.Boolean_Value (Value.dataSupport.Value);
      end if;
      Handler.End_Object;
   end Write_PublishDiagnosticsClientCapabilities;

   procedure Write_DocumentHighlight_Vector
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DocumentHighlight_Vector) is
   begin
      Handler.Start_Array;
      for J in Value.First_Index .. Value.Last_Index loop
         Write_DocumentHighlight (Handler, Value (J));
      end loop;
      Handler.End_Array;
   end Write_DocumentHighlight_Vector;

   procedure Write_SemanticTokensOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.SemanticTokensOptions) is
   begin
      Handler.Start_Object;
      if Value.workDoneProgress.Is_Set then
         Handler.Key_Name ("workDoneProgress");
         Handler.Boolean_Value (Value.workDoneProgress.Value);
      end if;
      Handler.Key_Name ("legend");
      Write_SemanticTokensLegend (Handler, Value.legend);
      if Value.a_range.Is_Set then
         Handler.Key_Name ("range");
         Write_Boolean_Or_Any (Handler, Value.a_range.Value);
      end if;
      if Value.full.Is_Set then
         Handler.Key_Name ("full");
         Write_Boolean_Or_Something (Handler, Value.full.Value);
      end if;
      Handler.End_Object;
   end Write_SemanticTokensOptions;

   procedure Write_DocumentHighlightOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.DocumentHighlightOptions) is
   begin
      Handler.Start_Object;
      if Value.workDoneProgress.Is_Set then
         Handler.Key_Name ("workDoneProgress");
         Handler.Boolean_Value (Value.workDoneProgress.Value);
      end if;
      Handler.End_Object;
   end Write_DocumentHighlightOptions;

   procedure Write_Integer_Or_Virtual_String
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.Integer_Or_Virtual_String) is
   begin
      case Value.Is_Integer is
         when True =>
            Handler.Integer_Value (Integer'Pos (Value.Integer));
         when False =>
            Handler.String_Value (Value.Virtual_String);
      end case;
   end Write_Integer_Or_Virtual_String;

   procedure Write_FoldingRangeParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.FoldingRangeParams) is
   begin
      Handler.Start_Object;
      if Value.workDoneToken.Is_Set then
         Handler.Key_Name ("workDoneToken");
         Write_ProgressToken (Handler, Value.workDoneToken.Value);
      end if;
      if Value.partialResultToken.Is_Set then
         Handler.Key_Name ("partialResultToken");
         Write_ProgressToken (Handler, Value.partialResultToken.Value);
      end if;
      Handler.Key_Name ("textDocument");
      Write_TextDocumentIdentifier (Handler, Value.textDocument);
      Handler.End_Object;
   end Write_FoldingRangeParams;

   procedure Write_FoldingRangeOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.FoldingRangeOptions) is
   begin
      Handler.Start_Object;
      if Value.workDoneProgress.Is_Set then
         Handler.Key_Name ("workDoneProgress");
         Handler.Boolean_Value (Value.workDoneProgress.Value);
      end if;
      Handler.End_Object;
   end Write_FoldingRangeOptions;

   procedure Write_Integer_Or_Null
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.Integer_Or_Null) is
   begin
      if Value.Is_Null then
         Handler.Null_Value;
      else
         Handler.Integer_Value (Integer'Pos (Value.Value));
      end if;
   end Write_Integer_Or_Null;

   procedure Write_RegistrationParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.RegistrationParams) is
      procedure Write_Registration_Vector
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.Registration_Vector);

      procedure Write_Registration_Vector
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.Registration_Vector) is
      begin
         Handler.Start_Array;
         for J in Value.First_Index .. Value.Last_Index loop
            Write_Registration (Handler, Value (J));
         end loop;
         Handler.End_Array;
      end Write_Registration_Vector;

   begin
      Handler.Start_Object;
      Handler.Key_Name ("registrations");
      Write_Registration_Vector (Handler, Value.registrations);
      Handler.End_Object;
   end Write_RegistrationParams;

   procedure Write_SignatureHelpParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.SignatureHelpParams) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("textDocument");
      Write_TextDocumentIdentifier (Handler, Value.textDocument);
      Handler.Key_Name ("position");
      Write_Position (Handler, Value.position);
      if Value.workDoneToken.Is_Set then
         Handler.Key_Name ("workDoneToken");
         Write_ProgressToken (Handler, Value.workDoneToken.Value);
      end if;
      if Value.context.Is_Set then
         Handler.Key_Name ("context");
         Write_SignatureHelpContext (Handler, Value.context.Value);
      end if;
      Handler.End_Object;
   end Write_SignatureHelpParams;

   procedure Write_TextEdit_Vector
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.TextEdit_Vector) is
   begin
      Handler.Start_Array;
      for J in Value.First_Index .. Value.Last_Index loop
         Write_TextEdit (Handler, Value (J));
      end loop;
      Handler.End_Array;
   end Write_TextEdit_Vector;

   procedure Write_ShowMessageRequestClientCapabilities
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.ShowMessageRequestClientCapabilities) is
      procedure Write_messageActionItem_OfShowMessageRequestClientCapabilities
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures
           .messageActionItem_OfShowMessageRequestClientCapabilities);

      procedure Write_messageActionItem_OfShowMessageRequestClientCapabilities
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures
           .messageActionItem_OfShowMessageRequestClientCapabilities) is
      begin
         Handler.Start_Object;
         if Value.additionalPropertiesSupport.Is_Set then
            Handler.Key_Name ("additionalPropertiesSupport");
            Handler.Boolean_Value (Value.additionalPropertiesSupport.Value);
         end if;
         Handler.End_Object;
      end Write_messageActionItem_OfShowMessageRequestClientCapabilities;

   begin
      Handler.Start_Object;
      if Value.messageActionItem.Is_Set then
         Handler.Key_Name ("messageActionItem");
         Write_messageActionItem_OfShowMessageRequestClientCapabilities
           (Handler, Value.messageActionItem.Value);
      end if;
      Handler.End_Object;
   end Write_ShowMessageRequestClientCapabilities;

   procedure Write_SemanticTokensEdit_Vector
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.SemanticTokensEdit_Vector) is
   begin
      Handler.Start_Array;
      for J in Value.First_Index .. Value.Last_Index loop
         Write_SemanticTokensEdit (Handler, Value (J));
      end loop;
      Handler.End_Array;
   end Write_SemanticTokensEdit_Vector;

   procedure Write_Position
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.Position) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("line");
      Handler.Integer_Value (Integer'Pos (Value.line));
      Handler.Key_Name ("character");
      Handler.Integer_Value (Integer'Pos (Value.character));
      Handler.End_Object;
   end Write_Position;

   procedure Write_TextDocumentSaveRegistrationOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.TextDocumentSaveRegistrationOptions) is
      procedure Write_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.DocumentSelector_Or_Null);

      procedure Write_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.DocumentSelector_Or_Null) is
      begin
         if Value.Is_Null then
            Handler.Null_Value;
         else
            Write_DocumentSelector (Handler, Value.Value);
         end if;
      end Write_DocumentSelector_Or_Null;

   begin
      Handler.Start_Object;
      Handler.Key_Name ("documentSelector");
      Write_DocumentSelector_Or_Null (Handler, Value.Parent.documentSelector);
      if Value.includeText.Is_Set then
         Handler.Key_Name ("includeText");
         Handler.Boolean_Value (Value.includeText.Value);
      end if;
      Handler.End_Object;
   end Write_TextDocumentSaveRegistrationOptions;

   procedure Write_CodeActionParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.CodeActionParams) is
   begin
      Handler.Start_Object;
      if Value.workDoneToken.Is_Set then
         Handler.Key_Name ("workDoneToken");
         Write_ProgressToken (Handler, Value.workDoneToken.Value);
      end if;
      if Value.partialResultToken.Is_Set then
         Handler.Key_Name ("partialResultToken");
         Write_ProgressToken (Handler, Value.partialResultToken.Value);
      end if;
      Handler.Key_Name ("textDocument");
      Write_TextDocumentIdentifier (Handler, Value.textDocument);
      Handler.Key_Name ("range");
      Write_A_Range (Handler, Value.a_range);
      Handler.Key_Name ("context");
      Write_CodeActionContext (Handler, Value.context);
      Handler.End_Object;
   end Write_CodeActionParams;

   procedure Write_Command
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.Command) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("title");
      Handler.String_Value (Value.title);
      Handler.Key_Name ("command");
      Handler.String_Value (Value.command);
      if not Value.arguments.Is_Empty then
         Handler.Key_Name ("arguments");
         Write_LSPAny_Vector (Handler, Value.arguments);
      end if;
      Handler.End_Object;
   end Write_Command;

   procedure Write_TextDocumentFilter
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.TextDocumentFilter) is
   begin
      Handler.Start_Object;
      if not Value.language.Is_Null then
         Handler.Key_Name ("language");
         Handler.String_Value (Value.language);
      end if;
      if not Value.scheme.Is_Null then
         Handler.Key_Name ("scheme");
         Handler.String_Value (Value.scheme);
      end if;
      if not Value.pattern.Is_Null then
         Handler.Key_Name ("pattern");
         Handler.String_Value (Value.pattern);
      end if;
      Handler.End_Object;
   end Write_TextDocumentFilter;

   procedure Write_FoldingRange_Vector
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.FoldingRange_Vector) is
   begin
      Handler.Start_Array;
      for J in Value.First_Index .. Value.Last_Index loop
         Write_FoldingRange (Handler, Value (J));
      end loop;
      Handler.End_Array;
   end Write_FoldingRange_Vector;

   procedure Write_SelectionRange_Vector_Or_Null
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.SelectionRange_Vector_Or_Null) is
   begin
      if Value.Length = 0 then
         Handler.Null_Value;
      else
         Write_SelectionRange_Vector (Handler, Value);
      end if;
   end Write_SelectionRange_Vector_Or_Null;

   procedure Write_ExecuteCommandRegistrationOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.ExecuteCommandRegistrationOptions) is
   begin
      Handler.Start_Object;
      if Value.workDoneProgress.Is_Set then
         Handler.Key_Name ("workDoneProgress");
         Handler.Boolean_Value (Value.workDoneProgress.Value);
      end if;
      Handler.Key_Name ("commands");
      Write_Virtual_String_Vector (Handler, Value.commands);
      Handler.End_Object;
   end Write_ExecuteCommandRegistrationOptions;

   procedure Write_UniquenessLevel
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Enumerations.UniquenessLevel) is
   begin
      case Value is
         when LSP.Enumerations.document =>
            Handler.String_Value ("document");
         when LSP.Enumerations.project =>
            Handler.String_Value ("project");
         when LSP.Enumerations.group =>
            Handler.String_Value ("group");
         when LSP.Enumerations.scheme =>
            Handler.String_Value ("scheme");
         when LSP.Enumerations.global =>
            Handler.String_Value ("global");
      end case;
   end Write_UniquenessLevel;

   procedure Write_TextDocumentEdit
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.TextDocumentEdit) is
      procedure Write_TextEdit_Or_AnnotatedTextEdit
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.TextEdit_Or_AnnotatedTextEdit);

      procedure Write_TextEdit_Or_AnnotatedTextEdit_Vector
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.TextEdit_Or_AnnotatedTextEdit_Vector);

      procedure Write_TextEdit_Or_AnnotatedTextEdit
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.TextEdit_Or_AnnotatedTextEdit) is
      begin
         case Value.Is_TextEdit is
            when True =>
               Write_TextEdit (Handler, Value.TextEdit);
            when False =>
               Write_AnnotatedTextEdit (Handler, Value.AnnotatedTextEdit);
         end case;
      end Write_TextEdit_Or_AnnotatedTextEdit;

      procedure Write_TextEdit_Or_AnnotatedTextEdit_Vector
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.TextEdit_Or_AnnotatedTextEdit_Vector) is
      begin
         Handler.Start_Array;
         for J in Value.First_Index .. Value.Last_Index loop
            Write_TextEdit_Or_AnnotatedTextEdit (Handler, Value (J));
         end loop;
         Handler.End_Array;
      end Write_TextEdit_Or_AnnotatedTextEdit_Vector;

   begin
      Handler.Start_Object;
      Handler.Key_Name ("textDocument");
      Write_OptionalVersionedTextDocumentIdentifier
        (Handler, Value.textDocument);
      Handler.Key_Name ("edits");
      Write_TextEdit_Or_AnnotatedTextEdit_Vector (Handler, Value.edits);
      Handler.End_Object;
   end Write_TextDocumentEdit;

   procedure Write_CompletionItemLabelDetails
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.CompletionItemLabelDetails) is
   begin
      Handler.Start_Object;
      if not Value.detail.Is_Null then
         Handler.Key_Name ("detail");
         Handler.String_Value (Value.detail);
      end if;
      if not Value.description.Is_Null then
         Handler.Key_Name ("description");
         Handler.String_Value (Value.description);
      end if;
      Handler.End_Object;
   end Write_CompletionItemLabelDetails;

   procedure Write_CompletionOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.CompletionOptions) is
      procedure Write_completionItem_OfCompletionOptions
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.completionItem_OfCompletionOptions);

      procedure Write_completionItem_OfCompletionOptions
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.completionItem_OfCompletionOptions) is
      begin
         Handler.Start_Object;
         if Value.labelDetailsSupport.Is_Set then
            Handler.Key_Name ("labelDetailsSupport");
            Handler.Boolean_Value (Value.labelDetailsSupport.Value);
         end if;
         Handler.End_Object;
      end Write_completionItem_OfCompletionOptions;

   begin
      Handler.Start_Object;
      if Value.workDoneProgress.Is_Set then
         Handler.Key_Name ("workDoneProgress");
         Handler.Boolean_Value (Value.workDoneProgress.Value);
      end if;
      if not Value.triggerCharacters.Is_Empty then
         Handler.Key_Name ("triggerCharacters");
         Write_Virtual_String_Vector (Handler, Value.triggerCharacters);
      end if;
      if not Value.allCommitCharacters.Is_Empty then
         Handler.Key_Name ("allCommitCharacters");
         Write_Virtual_String_Vector (Handler, Value.allCommitCharacters);
      end if;
      if Value.resolveProvider.Is_Set then
         Handler.Key_Name ("resolveProvider");
         Handler.Boolean_Value (Value.resolveProvider.Value);
      end if;
      if Value.completionItem.Is_Set then
         Handler.Key_Name ("completionItem");
         Write_completionItem_OfCompletionOptions
           (Handler, Value.completionItem.Value);
      end if;
      Handler.End_Object;
   end Write_CompletionOptions;

   procedure Write_Diagnostic
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.Diagnostic) is
      procedure Write_DiagnosticRelatedInformation_Vector
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.DiagnosticRelatedInformation_Vector);

      procedure Write_DiagnosticRelatedInformation_Vector
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.DiagnosticRelatedInformation_Vector) is
      begin
         Handler.Start_Array;
         for J in Value.First_Index .. Value.Last_Index loop
            Write_DiagnosticRelatedInformation (Handler, Value (J));
         end loop;
         Handler.End_Array;
      end Write_DiagnosticRelatedInformation_Vector;

   begin
      Handler.Start_Object;
      Handler.Key_Name ("range");
      Write_A_Range (Handler, Value.a_range);
      if Value.severity.Is_Set then
         Handler.Key_Name ("severity");
         Write_DiagnosticSeverity (Handler, Value.severity.Value);
      end if;
      if Value.code.Is_Set then
         Handler.Key_Name ("code");
         Write_Integer_Or_Virtual_String (Handler, Value.code.Value);
      end if;
      if Value.codeDescription.Is_Set then
         Handler.Key_Name ("codeDescription");
         Write_CodeDescription (Handler, Value.codeDescription.Value);
      end if;
      if not Value.source.Is_Null then
         Handler.Key_Name ("source");
         Handler.String_Value (Value.source);
      end if;
      Handler.Key_Name ("message");
      Handler.String_Value (Value.message);
      if (for some Item of Value.tags => Item) then
         Handler.Key_Name ("tags");
         Write_DiagnosticTag_Set (Handler, Value.tags);
      end if;
      if not Value.relatedInformation.Is_Empty then
         Handler.Key_Name ("relatedInformation");
         Write_DiagnosticRelatedInformation_Vector
           (Handler, Value.relatedInformation);
      end if;
      if not Value.data.Is_Empty then
         Handler.Key_Name ("data");
         Write_LSPAny (Handler, Value.data);
      end if;
      Handler.End_Object;
   end Write_Diagnostic;

   procedure Write_LinkedEditingRangeParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.LinkedEditingRangeParams) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("textDocument");
      Write_TextDocumentIdentifier (Handler, Value.textDocument);
      Handler.Key_Name ("position");
      Write_Position (Handler, Value.position);
      if Value.workDoneToken.Is_Set then
         Handler.Key_Name ("workDoneToken");
         Write_ProgressToken (Handler, Value.workDoneToken.Value);
      end if;
      Handler.End_Object;
   end Write_LinkedEditingRangeParams;

   procedure Write_TextDocumentSyncOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.TextDocumentSyncOptions) is
      procedure Write_Boolean_Or_SaveOptions
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.Boolean_Or_SaveOptions);

      procedure Write_Boolean_Or_SaveOptions
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.Boolean_Or_SaveOptions) is
      begin
         case Value.Is_Boolean is
            when True =>
               Handler.Boolean_Value (Value.Boolean);
            when False =>
               Write_SaveOptions (Handler, Value.SaveOptions);
         end case;
      end Write_Boolean_Or_SaveOptions;

   begin
      Handler.Start_Object;
      if Value.openClose.Is_Set then
         Handler.Key_Name ("openClose");
         Handler.Boolean_Value (Value.openClose.Value);
      end if;
      if Value.change.Is_Set then
         Handler.Key_Name ("change");
         Write_TextDocumentSyncKind (Handler, Value.change.Value);
      end if;
      if Value.willSave.Is_Set then
         Handler.Key_Name ("willSave");
         Handler.Boolean_Value (Value.willSave.Value);
      end if;
      if Value.willSaveWaitUntil.Is_Set then
         Handler.Key_Name ("willSaveWaitUntil");
         Handler.Boolean_Value (Value.willSaveWaitUntil.Value);
      end if;
      if Value.save.Is_Set then
         Handler.Key_Name ("save");
         Write_Boolean_Or_SaveOptions (Handler, Value.save.Value);
      end if;
      Handler.End_Object;
   end Write_TextDocumentSyncOptions;

   procedure Write_ImplementationParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.ImplementationParams) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("textDocument");
      Write_TextDocumentIdentifier (Handler, Value.textDocument);
      Handler.Key_Name ("position");
      Write_Position (Handler, Value.position);
      if Value.workDoneToken.Is_Set then
         Handler.Key_Name ("workDoneToken");
         Write_ProgressToken (Handler, Value.workDoneToken.Value);
      end if;
      if Value.partialResultToken.Is_Set then
         Handler.Key_Name ("partialResultToken");
         Write_ProgressToken (Handler, Value.partialResultToken.Value);
      end if;
      Handler.End_Object;
   end Write_ImplementationParams;

   procedure Write_OptionalVersionedTextDocumentIdentifier
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.OptionalVersionedTextDocumentIdentifier) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("uri");
      Handler.String_Value (Value.uri);
      Handler.Key_Name ("version");
      Write_Integer_Or_Null (Handler, Value.version);
      Handler.End_Object;
   end Write_OptionalVersionedTextDocumentIdentifier;

   procedure Write_SemanticTokensLegend
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.SemanticTokensLegend) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("tokenTypes");
      Write_Virtual_String_Vector (Handler, Value.tokenTypes);
      Handler.Key_Name ("tokenModifiers");
      Write_Virtual_String_Vector (Handler, Value.tokenModifiers);
      Handler.End_Object;
   end Write_SemanticTokensLegend;

   procedure Write_WorkspaceDocumentDiagnosticReport_Vector
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.WorkspaceDocumentDiagnosticReport_Vector) is
   begin
      Handler.Start_Array;
      for J in Value.First_Index .. Value.Last_Index loop
         Write_WorkspaceDocumentDiagnosticReport (Handler, Value (J));
      end loop;
      Handler.End_Array;
   end Write_WorkspaceDocumentDiagnosticReport_Vector;

   procedure Write_ExecuteCommandOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.ExecuteCommandOptions) is
   begin
      Handler.Start_Object;
      if Value.workDoneProgress.Is_Set then
         Handler.Key_Name ("workDoneProgress");
         Handler.Boolean_Value (Value.workDoneProgress.Value);
      end if;
      Handler.Key_Name ("commands");
      Write_Virtual_String_Vector (Handler, Value.commands);
      Handler.End_Object;
   end Write_ExecuteCommandOptions;

   procedure Write_TextDocumentSyncClientCapabilities
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.TextDocumentSyncClientCapabilities) is
   begin
      Handler.Start_Object;
      if Value.dynamicRegistration.Is_Set then
         Handler.Key_Name ("dynamicRegistration");
         Handler.Boolean_Value (Value.dynamicRegistration.Value);
      end if;
      if Value.willSave.Is_Set then
         Handler.Key_Name ("willSave");
         Handler.Boolean_Value (Value.willSave.Value);
      end if;
      if Value.willSaveWaitUntil.Is_Set then
         Handler.Key_Name ("willSaveWaitUntil");
         Handler.Boolean_Value (Value.willSaveWaitUntil.Value);
      end if;
      if Value.didSave.Is_Set then
         Handler.Key_Name ("didSave");
         Handler.Boolean_Value (Value.didSave.Value);
      end if;
      Handler.End_Object;
   end Write_TextDocumentSyncClientCapabilities;

   procedure Write_MarkupKind
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Enumerations.MarkupKind) is
   begin
      case Value is
         when LSP.Enumerations.PlainText =>
            Handler.String_Value ("plaintext");
         when LSP.Enumerations.Markdown =>
            Handler.String_Value ("markdown");
      end case;
   end Write_MarkupKind;

   procedure Write_AlsReferenceKind_Set
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.AlsReferenceKind_Set) is
   begin
      Handler.Start_Array;
      declare
         Set : LSP.Structures.AlsReferenceKind_Set renames Value;
      begin
         for Value in Set'Range loop
            if Set (Value) then
               Write_AlsReferenceKind (Handler, Value);
            end if;
         end loop;
      end;
      Handler.End_Array;
   end Write_AlsReferenceKind_Set;

   procedure Write_CallHierarchyPrepareParams
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.CallHierarchyPrepareParams) is
   begin
      Handler.Start_Object;
      Handler.Key_Name ("textDocument");
      Write_TextDocumentIdentifier (Handler, Value.textDocument);
      Handler.Key_Name ("position");
      Write_Position (Handler, Value.position);
      if Value.workDoneToken.Is_Set then
         Handler.Key_Name ("workDoneToken");
         Write_ProgressToken (Handler, Value.workDoneToken.Value);
      end if;
      Handler.End_Object;
   end Write_CallHierarchyPrepareParams;

   procedure Write_LSPErrorCodes
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Enumerations.LSPErrorCodes) is
   begin
      case Value is
         when LSP.Enumerations.lspReservedErrorRangeStart =>
            Handler.Integer_Value (-32_899);
         when LSP.Enumerations.RequestFailed =>
            Handler.Integer_Value (-32_803);
         when LSP.Enumerations.ServerCancelled =>
            Handler.Integer_Value (-32_802);
         when LSP.Enumerations.ContentModified =>
            Handler.Integer_Value (-32_801);
         when LSP.Enumerations.RequestCancelled =>
            Handler.Integer_Value (-32_800);
         when LSP.Enumerations.lspReservedErrorRangeEnd =>
            Handler.Integer_Value (-32_800);
      end case;
   end Write_LSPErrorCodes;

   procedure Write_ReferenceRegistrationOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.ReferenceRegistrationOptions) is
      procedure Write_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.DocumentSelector_Or_Null);

      procedure Write_DocumentSelector_Or_Null
        (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
         Value   : LSP.Structures.DocumentSelector_Or_Null) is
      begin
         if Value.Is_Null then
            Handler.Null_Value;
         else
            Write_DocumentSelector (Handler, Value.Value);
         end if;
      end Write_DocumentSelector_Or_Null;

   begin
      Handler.Start_Object;
      Handler.Key_Name ("documentSelector");
      Write_DocumentSelector_Or_Null (Handler, Value.Parent.documentSelector);
      if Value.workDoneProgress.Is_Set then
         Handler.Key_Name ("workDoneProgress");
         Handler.Boolean_Value (Value.workDoneProgress.Value);
      end if;
      Handler.End_Object;
   end Write_ReferenceRegistrationOptions;

   procedure Write_LinkedEditingRangeOptions
     (Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;
      Value   : LSP.Structures.LinkedEditingRangeOptions) is
   begin
      Handler.Start_Object;
      if Value.workDoneProgress.Is_Set then
         Handler.Key_Name ("workDoneProgress");
         Handler.Boolean_Value (Value.workDoneProgress.Value);
      end if;
      Handler.End_Object;
   end Write_LinkedEditingRangeOptions;

end LSP.Outputs;
