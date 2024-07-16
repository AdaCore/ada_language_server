--  Automatically generated, do not edit.
with Ada.Streams;
with LSP.Messages;

package LSP.Message_IO is

   procedure Read_RequestMessage
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.RequestMessage);

   procedure Write_RequestMessage
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.RequestMessage);

   procedure Read_NotificationMessage
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.NotificationMessage);

   procedure Write_NotificationMessage
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.NotificationMessage);

   procedure Read_CancelParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.CancelParams);

   procedure Write_CancelParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.CancelParams);

   procedure Read_Position
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.Position);

   procedure Write_Position
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.Position);

   procedure Read_Span
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.Span);

   procedure Write_Span
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.Span);

   procedure Read_CodeActionKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.CodeActionKind);

   procedure Write_CodeActionKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.CodeActionKind);

   procedure Read_AlsReferenceKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.AlsReferenceKind);

   procedure Write_AlsReferenceKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.AlsReferenceKind);

   procedure Read_AlsDisplayMethodAncestryOnNavigationPolicy
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.AlsDisplayMethodAncestryOnNavigationPolicy);

   procedure Write_AlsDisplayMethodAncestryOnNavigationPolicy
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.AlsDisplayMethodAncestryOnNavigationPolicy);

   procedure Read_Location
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.Location);

   procedure Write_Location
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.Location);

   procedure Read_LocationLink
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.LocationLink);

   procedure Write_LocationLink
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.LocationLink);

   procedure Read_DiagnosticSeverity
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.DiagnosticSeverity);

   procedure Write_DiagnosticSeverity
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.DiagnosticSeverity);

   procedure Read_DiagnosticTag
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.DiagnosticTag);

   procedure Write_DiagnosticTag
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.DiagnosticTag);

   procedure Read_CodeDescription
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.CodeDescription);

   procedure Write_CodeDescription
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.CodeDescription);

   procedure Read_DiagnosticRelatedInformation
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.DiagnosticRelatedInformation);

   procedure Write_DiagnosticRelatedInformation
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.DiagnosticRelatedInformation);

   procedure Read_Diagnostic
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.Diagnostic);

   procedure Write_Diagnostic
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.Diagnostic);

   procedure Read_TextEdit
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.TextEdit);

   procedure Write_TextEdit
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.TextEdit);

   procedure Read_AnnotatedTextEdit
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.AnnotatedTextEdit);

   procedure Write_AnnotatedTextEdit
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.AnnotatedTextEdit);

   procedure Read_TextDocumentIdentifier
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.TextDocumentIdentifier);

   procedure Write_TextDocumentIdentifier
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.TextDocumentIdentifier);

   procedure Read_VersionedTextDocumentIdentifier
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.VersionedTextDocumentIdentifier);

   procedure Write_VersionedTextDocumentIdentifier
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.VersionedTextDocumentIdentifier);

   procedure Read_OptionalVersionedTextDocumentIdentifier
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.OptionalVersionedTextDocumentIdentifier);

   procedure Write_OptionalVersionedTextDocumentIdentifier
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.OptionalVersionedTextDocumentIdentifier);

   procedure Read_TextDocumentEdit
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.TextDocumentEdit);

   procedure Write_TextDocumentEdit
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.TextDocumentEdit);

   procedure Read_FileResourceChangeKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.FileResourceChangeKind);

   procedure Write_FileResourceChangeKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.FileResourceChangeKind);

   procedure Read_CreateFileOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.CreateFileOptions);

   procedure Write_CreateFileOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.CreateFileOptions);

   procedure Read_CreateFile
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.CreateFile);

   procedure Write_CreateFile
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.CreateFile);

   procedure Read_RenameFileOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.RenameFileOptions);

   procedure Write_RenameFileOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.RenameFileOptions);

   procedure Read_RenameFile
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.RenameFile);

   procedure Write_RenameFile
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.RenameFile);

   procedure Read_DeleteFileOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.DeleteFileOptions);

   procedure Write_DeleteFileOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.DeleteFileOptions);

   procedure Read_DeleteFile
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.DeleteFile);

   procedure Write_DeleteFile
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.DeleteFile);

   procedure Read_ChangeAnnotation
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.ChangeAnnotation);

   procedure Write_ChangeAnnotation
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.ChangeAnnotation);

   procedure Read_TextDocumentItem
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.TextDocumentItem);

   procedure Write_TextDocumentItem
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.TextDocumentItem);

   procedure Read_TextDocumentPositionParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.TextDocumentPositionParams);

   procedure Write_TextDocumentPositionParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.TextDocumentPositionParams);

   procedure Read_dynamicRegistration
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.dynamicRegistration);

   procedure Write_dynamicRegistration
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.dynamicRegistration);

   procedure Read_ResourceOperationKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.ResourceOperationKind);

   procedure Write_ResourceOperationKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.ResourceOperationKind);

   procedure Read_FailureHandlingKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.FailureHandlingKind);

   procedure Write_FailureHandlingKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.FailureHandlingKind);

   procedure Read_AnnotationSupport
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.AnnotationSupport);

   procedure Write_AnnotationSupport
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.AnnotationSupport);

   procedure Read_WorkspaceEditClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.WorkspaceEditClientCapabilities);

   procedure Write_WorkspaceEditClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.WorkspaceEditClientCapabilities);

   procedure Read_SymbolKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.SymbolKind);

   procedure Write_SymbolKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.SymbolKind);

   procedure Read_symbolKindCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.symbolKindCapabilities);

   procedure Write_symbolKindCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.symbolKindCapabilities);

   procedure Read_SymbolTag
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.SymbolTag);

   procedure Write_SymbolTag
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.SymbolTag);

   procedure Read_tagSupportCapability
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.tagSupportCapability);

   procedure Write_tagSupportCapability
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.tagSupportCapability);

   procedure Read_Als_Visibility
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.Als_Visibility);

   procedure Write_Als_Visibility
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.Als_Visibility);

   procedure Read_WorkspaceSymbolClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.WorkspaceSymbolClientCapabilities);

   procedure Write_WorkspaceSymbolClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.WorkspaceSymbolClientCapabilities);

   procedure Read_SemanticTokensWorkspaceClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.SemanticTokensWorkspaceClientCapabilities);

   procedure Write_SemanticTokensWorkspaceClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.SemanticTokensWorkspaceClientCapabilities);

   procedure Read_CodeLensWorkspaceClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.CodeLensWorkspaceClientCapabilities);

   procedure Write_CodeLensWorkspaceClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.CodeLensWorkspaceClientCapabilities);

   procedure Read_FileOperationsClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.FileOperationsClientCapabilities);

   procedure Write_FileOperationsClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.FileOperationsClientCapabilities);

   procedure Read_WorkspaceClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.WorkspaceClientCapabilities);

   procedure Write_WorkspaceClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.WorkspaceClientCapabilities);

   procedure Read_MarkupKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.MarkupKind);

   procedure Write_MarkupKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.MarkupKind);

   procedure Read_MarkupContent
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.MarkupContent);

   procedure Write_MarkupContent
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.MarkupContent);

   procedure Read_SaveOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.SaveOptions);

   procedure Write_SaveOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.SaveOptions);

   procedure Read_TextDocumentSyncKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.TextDocumentSyncKind);

   procedure Write_TextDocumentSyncKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.TextDocumentSyncKind);

   procedure Read_TextDocumentSyncOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.TextDocumentSyncOptions);

   procedure Write_TextDocumentSyncOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.TextDocumentSyncOptions);

   procedure Read_TextDocumentSyncClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.TextDocumentSyncClientCapabilities);

   procedure Write_TextDocumentSyncClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.TextDocumentSyncClientCapabilities);

   procedure Read_CompletionItemTag
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.CompletionItemTag);

   procedure Write_CompletionItemTag
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.CompletionItemTag);

   procedure Read_CompletionItemTagSupport
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.CompletionItemTagSupport);

   procedure Write_CompletionItemTagSupport
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.CompletionItemTagSupport);

   procedure Read_resolveSupportCapability
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.resolveSupportCapability);

   procedure Write_resolveSupportCapability
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.resolveSupportCapability);

   procedure Read_InsertTextMode
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.InsertTextMode);

   procedure Write_InsertTextMode
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.InsertTextMode);

   procedure Read_insertTextModeSupportCapability
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.insertTextModeSupportCapability);

   procedure Write_insertTextModeSupportCapability
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.insertTextModeSupportCapability);

   procedure Read_completionItemCapability
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.completionItemCapability);

   procedure Write_completionItemCapability
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.completionItemCapability);

   procedure Read_CompletionItemKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.CompletionItemKind);

   procedure Write_CompletionItemKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.CompletionItemKind);

   procedure Read_CompletionItemKindSetCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.CompletionItemKindSetCapabilities);

   procedure Write_CompletionItemKindSetCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.CompletionItemKindSetCapabilities);

   procedure Read_CompletionClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.CompletionClientCapabilities);

   procedure Write_CompletionClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.CompletionClientCapabilities);

   procedure Read_HoverClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.HoverClientCapabilities);

   procedure Write_HoverClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.HoverClientCapabilities);

   procedure Read_parameterInformation_Capability
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.parameterInformation_Capability);

   procedure Write_parameterInformation_Capability
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.parameterInformation_Capability);

   procedure Read_signatureInformation_Capability
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.signatureInformation_Capability);

   procedure Write_signatureInformation_Capability
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.signatureInformation_Capability);

   procedure Read_SignatureHelpClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.SignatureHelpClientCapabilities);

   procedure Write_SignatureHelpClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.SignatureHelpClientCapabilities);

   procedure Read_DocumentSymbolClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.DocumentSymbolClientCapabilities);

   procedure Write_DocumentSymbolClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.DocumentSymbolClientCapabilities);

   procedure Read_DeclarationClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.DeclarationClientCapabilities);

   procedure Write_DeclarationClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.DeclarationClientCapabilities);

   procedure Read_codeActionKindCapability
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.codeActionKindCapability);

   procedure Write_codeActionKindCapability
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.codeActionKindCapability);

   procedure Read_codeActionLiteralSupport_Capability
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.codeActionLiteralSupport_Capability);

   procedure Write_codeActionLiteralSupport_Capability
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.codeActionLiteralSupport_Capability);

   procedure Read_CodeActionClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.CodeActionClientCapabilities);

   procedure Write_CodeActionClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.CodeActionClientCapabilities);

   procedure Read_DocumentLinkClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.DocumentLinkClientCapabilities);

   procedure Write_DocumentLinkClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.DocumentLinkClientCapabilities);

   procedure Read_PrepareSupportDefaultBehavior
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.PrepareSupportDefaultBehavior);

   procedure Write_PrepareSupportDefaultBehavior
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.PrepareSupportDefaultBehavior);

   procedure Read_RenameClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.RenameClientCapabilities);

   procedure Write_RenameClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.RenameClientCapabilities);

   procedure Read_DiagnosticTagSupport
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.DiagnosticTagSupport);

   procedure Write_DiagnosticTagSupport
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.DiagnosticTagSupport);

   procedure Read_PublishDiagnosticsClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.PublishDiagnosticsClientCapabilities);

   procedure Write_PublishDiagnosticsClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.PublishDiagnosticsClientCapabilities);

   procedure Read_FoldingRangeClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.FoldingRangeClientCapabilities);

   procedure Write_FoldingRangeClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.FoldingRangeClientCapabilities);

   procedure Read_SemanticTokenTypes
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.SemanticTokenTypes);

   procedure Write_SemanticTokenTypes
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.SemanticTokenTypes);

   procedure Read_SemanticTokenModifiers
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.SemanticTokenModifiers);

   procedure Write_SemanticTokenModifiers
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.SemanticTokenModifiers);

   procedure Read_TokenFormat
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.TokenFormat);

   procedure Write_TokenFormat
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.TokenFormat);

   procedure Read_SemanticTokensFullCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.SemanticTokensFullCapabilities);

   procedure Write_SemanticTokensFullCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.SemanticTokensFullCapabilities);

   procedure Read_SemanticTokensRequestCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.SemanticTokensRequestCapabilities);

   procedure Write_SemanticTokensRequestCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.SemanticTokensRequestCapabilities);

   procedure Read_SemanticTokensClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.SemanticTokensClientCapabilities);

   procedure Write_SemanticTokensClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.SemanticTokensClientCapabilities);

   procedure Read_TextDocumentClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.TextDocumentClientCapabilities);

   procedure Write_TextDocumentClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.TextDocumentClientCapabilities);

   procedure Read_ShowDocumentClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.ShowDocumentClientCapabilities);

   procedure Write_ShowDocumentClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.ShowDocumentClientCapabilities);

   procedure Read_MessageActionItemCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.MessageActionItemCapabilities);

   procedure Write_MessageActionItemCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.MessageActionItemCapabilities);

   procedure Read_ShowMessageRequestClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.ShowMessageRequestClientCapabilities);

   procedure Write_ShowMessageRequestClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.ShowMessageRequestClientCapabilities);

   procedure Read_WindowClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.WindowClientCapabilities);

   procedure Write_WindowClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.WindowClientCapabilities);

   procedure Read_MarkdownClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.MarkdownClientCapabilities);

   procedure Write_MarkdownClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.MarkdownClientCapabilities);

   procedure Read_RegularExpressionsClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.RegularExpressionsClientCapabilities);

   procedure Write_RegularExpressionsClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.RegularExpressionsClientCapabilities);

   procedure Read_GeneralClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.GeneralClientCapabilities);

   procedure Write_GeneralClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.GeneralClientCapabilities);

   procedure Read_ClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.ClientCapabilities);

   procedure Write_ClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.ClientCapabilities);

   procedure Read_WorkspaceFolder
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.WorkspaceFolder);

   procedure Write_WorkspaceFolder
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.WorkspaceFolder);

   procedure Read_WorkDoneProgressCreateParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.WorkDoneProgressCreateParams);

   procedure Write_WorkDoneProgressCreateParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.WorkDoneProgressCreateParams);

   procedure Read_Text_Progress_Params
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.Text_Progress_Params);

   procedure Write_Text_Progress_Params
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.Text_Progress_Params);

   procedure Read_ProgramInfo
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.ProgramInfo);

   procedure Write_ProgramInfo
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.ProgramInfo);

   procedure Read_TraceValue
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.TraceValue);

   procedure Write_TraceValue
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.TraceValue);

   procedure Read_InitializeParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.InitializeParams);

   procedure Write_InitializeParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.InitializeParams);

   procedure Read_CompletionOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.CompletionOptions);

   procedure Write_CompletionOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.CompletionOptions);

   procedure Read_SignatureHelpOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.SignatureHelpOptions);

   procedure Write_SignatureHelpOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.SignatureHelpOptions);

   procedure Read_TextDocumentRegistrationOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.TextDocumentRegistrationOptions);

   procedure Write_TextDocumentRegistrationOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.TextDocumentRegistrationOptions);

   procedure Read_TSW_RegistrationOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.TSW_RegistrationOptions);

   procedure Write_TSW_RegistrationOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.TSW_RegistrationOptions);

   procedure Read_DocumentSymbolOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.DocumentSymbolOptions);

   procedure Write_DocumentSymbolOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.DocumentSymbolOptions);

   procedure Read_CodeActionOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.CodeActionOptions);

   procedure Write_CodeActionOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.CodeActionOptions);

   procedure Read_CodeLensOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.CodeLensOptions);

   procedure Write_CodeLensOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.CodeLensOptions);

   procedure Read_DocumentOnTypeFormattingOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.DocumentOnTypeFormattingOptions);

   procedure Write_DocumentOnTypeFormattingOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.DocumentOnTypeFormattingOptions);

   procedure Read_RenameOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.RenameOptions);

   procedure Write_RenameOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.RenameOptions);

   procedure Read_DocumentLinkOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.DocumentLinkOptions);

   procedure Write_DocumentLinkOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.DocumentLinkOptions);

   procedure Read_ExecuteCommandOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.ExecuteCommandOptions);

   procedure Write_ExecuteCommandOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.ExecuteCommandOptions);

   procedure Read_WorkspaceFoldersServerCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.WorkspaceFoldersServerCapabilities);

   procedure Write_WorkspaceFoldersServerCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.WorkspaceFoldersServerCapabilities);

   procedure Read_FileOperationPatternKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.FileOperationPatternKind);

   procedure Write_FileOperationPatternKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.FileOperationPatternKind);

   procedure Read_FileOperationPatternOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.FileOperationPatternOptions);

   procedure Write_FileOperationPatternOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.FileOperationPatternOptions);

   procedure Read_FileOperationPattern
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.FileOperationPattern);

   procedure Write_FileOperationPattern
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.FileOperationPattern);

   procedure Read_FileOperationFilter
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.FileOperationFilter);

   procedure Write_FileOperationFilter
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.FileOperationFilter);

   procedure Read_FileOperationRegistrationOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.FileOperationRegistrationOptions);

   procedure Write_FileOperationRegistrationOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.FileOperationRegistrationOptions);

   procedure Read_FileOperationsServerCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.FileOperationsServerCapabilities);

   procedure Write_FileOperationsServerCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.FileOperationsServerCapabilities);

   procedure Read_workspace_Options
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.workspace_Options);

   procedure Write_workspace_Options
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.workspace_Options);

   procedure Read_SemanticTokensLegend
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.SemanticTokensLegend);

   procedure Write_SemanticTokensLegend
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.SemanticTokensLegend);

   procedure Read_SemanticTokensOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.SemanticTokensOptions);

   procedure Write_SemanticTokensOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.SemanticTokensOptions);

   procedure Read_ServerCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.ServerCapabilities);

   procedure Write_ServerCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.ServerCapabilities);

   procedure Read_InitializeResult
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.InitializeResult);

   procedure Write_InitializeResult
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.InitializeResult);

   procedure Read_InitializedParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.InitializedParams);

   procedure Write_InitializedParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.InitializedParams);

   procedure Read_MessageType
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.MessageType);

   procedure Write_MessageType
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.MessageType);

   procedure Read_ShowMessageParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.ShowMessageParams);

   procedure Write_ShowMessageParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.ShowMessageParams);

   procedure Read_ShowMessageRequestParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.ShowMessageRequestParams);

   procedure Write_ShowMessageRequestParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.ShowMessageRequestParams);

   procedure Read_LogMessageParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.LogMessageParams);

   procedure Write_LogMessageParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.LogMessageParams);

   procedure Read_TextDocumentChangeRegistrationOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.TextDocumentChangeRegistrationOptions);

   procedure Write_TextDocumentChangeRegistrationOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.TextDocumentChangeRegistrationOptions);

   procedure Read_TextDocumentSaveRegistrationOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.TextDocumentSaveRegistrationOptions);

   procedure Write_TextDocumentSaveRegistrationOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.TextDocumentSaveRegistrationOptions);

   procedure Read_CompletionRegistrationOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.CompletionRegistrationOptions);

   procedure Write_CompletionRegistrationOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.CompletionRegistrationOptions);

   procedure Read_SignatureHelpRegistrationOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.SignatureHelpRegistrationOptions);

   procedure Write_SignatureHelpRegistrationOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.SignatureHelpRegistrationOptions);

   procedure Read_CodeLensRegistrationOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.CodeLensRegistrationOptions);

   procedure Write_CodeLensRegistrationOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.CodeLensRegistrationOptions);

   procedure Read_DocumentOnTypeFormattingRegistrationOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.DocumentOnTypeFormattingRegistrationOptions);

   procedure Write_DocumentOnTypeFormattingRegistrationOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.DocumentOnTypeFormattingRegistrationOptions);

   procedure Read_ExecuteCommandRegistrationOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.ExecuteCommandRegistrationOptions);

   procedure Write_ExecuteCommandRegistrationOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.ExecuteCommandRegistrationOptions);

   procedure Read_FileSystemWatcher
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.FileSystemWatcher);

   procedure Write_FileSystemWatcher
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.FileSystemWatcher);

   procedure Read_DidChangeWatchedFilesRegistrationOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.DidChangeWatchedFilesRegistrationOptions);

   procedure Write_DidChangeWatchedFilesRegistrationOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.DidChangeWatchedFilesRegistrationOptions);

   procedure Read_CodeActionRegistrationOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.CodeActionRegistrationOptions);

   procedure Write_CodeActionRegistrationOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.CodeActionRegistrationOptions);

   procedure Read_RenameRegistrationOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.RenameRegistrationOptions);

   procedure Write_RenameRegistrationOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.RenameRegistrationOptions);

   procedure Read_Registration
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.Registration);

   procedure Write_Registration
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.Registration);

   procedure Read_RegistrationParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.RegistrationParams);

   procedure Write_RegistrationParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.RegistrationParams);

   procedure Read_Unregistration
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.Unregistration);

   procedure Write_Unregistration
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.Unregistration);

   procedure Read_UnregistrationParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.UnregistrationParams);

   procedure Write_UnregistrationParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.UnregistrationParams);

   procedure Read_DidChangeConfigurationParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.DidChangeConfigurationParams);

   procedure Write_DidChangeConfigurationParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.DidChangeConfigurationParams);

   procedure Read_DidOpenTextDocumentParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.DidOpenTextDocumentParams);

   procedure Write_DidOpenTextDocumentParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.DidOpenTextDocumentParams);

   procedure Read_TextDocumentContentChangeEvent
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.TextDocumentContentChangeEvent);

   procedure Write_TextDocumentContentChangeEvent
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.TextDocumentContentChangeEvent);

   procedure Read_DidChangeTextDocumentParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.DidChangeTextDocumentParams);

   procedure Write_DidChangeTextDocumentParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.DidChangeTextDocumentParams);

   procedure Read_TextDocumentSaveReason
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.TextDocumentSaveReason);

   procedure Write_TextDocumentSaveReason
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.TextDocumentSaveReason);

   procedure Read_DidSaveTextDocumentParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.DidSaveTextDocumentParams);

   procedure Write_DidSaveTextDocumentParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.DidSaveTextDocumentParams);

   procedure Read_DidCloseTextDocumentParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.DidCloseTextDocumentParams);

   procedure Write_DidCloseTextDocumentParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.DidCloseTextDocumentParams);

   procedure Read_FileChangeType
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.FileChangeType);

   procedure Write_FileChangeType
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.FileChangeType);

   procedure Read_FileEvent
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.FileEvent);

   procedure Write_FileEvent
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.FileEvent);

   procedure Read_DidChangeWatchedFilesParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.DidChangeWatchedFilesParams);

   procedure Write_DidChangeWatchedFilesParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.DidChangeWatchedFilesParams);

   procedure Read_PublishDiagnosticsParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.PublishDiagnosticsParams);

   procedure Write_PublishDiagnosticsParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.PublishDiagnosticsParams);

   procedure Read_InsertTextFormat
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.InsertTextFormat);

   procedure Write_InsertTextFormat
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.InsertTextFormat);

   procedure Read_InsertReplaceEdit
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.InsertReplaceEdit);

   procedure Write_InsertReplaceEdit
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.InsertReplaceEdit);

   procedure Read_CompletionItem
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.CompletionItem);

   procedure Write_CompletionItem
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.CompletionItem);

   procedure Read_CompletionList
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.CompletionList);

   procedure Write_CompletionList
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.CompletionList);

   procedure Read_Hover
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.Hover);

   procedure Write_Hover
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.Hover);

   procedure Read_ParameterInformation
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.ParameterInformation);

   procedure Write_ParameterInformation
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.ParameterInformation);

   procedure Read_SignatureInformation
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.SignatureInformation);

   procedure Write_SignatureInformation
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.SignatureInformation);

   procedure Read_SignatureHelp
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.SignatureHelp);

   procedure Write_SignatureHelp
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.SignatureHelp);

   procedure Read_ReferenceContext
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.ReferenceContext);

   procedure Write_ReferenceContext
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.ReferenceContext);

   procedure Read_ReferenceParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.ReferenceParams);

   procedure Write_ReferenceParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.ReferenceParams);

   procedure Read_DocumentHighlightKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.DocumentHighlightKind);

   procedure Write_DocumentHighlightKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.DocumentHighlightKind);

   procedure Read_DocumentHighlight
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.DocumentHighlight);

   procedure Write_DocumentHighlight
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.DocumentHighlight);

   procedure Read_Search_Kind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.Search_Kind);

   procedure Write_Search_Kind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.Search_Kind);

   procedure Read_DocumentSymbolParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.DocumentSymbolParams);

   procedure Write_DocumentSymbolParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.DocumentSymbolParams);

   procedure Read_SymbolInformation
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.SymbolInformation);

   procedure Write_SymbolInformation
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.SymbolInformation);

   procedure Read_WorkspaceSymbolParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.WorkspaceSymbolParams);

   procedure Write_WorkspaceSymbolParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.WorkspaceSymbolParams);

   procedure Read_CodeActionContext
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.CodeActionContext);

   procedure Write_CodeActionContext
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.CodeActionContext);

   procedure Read_CodeActionParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.CodeActionParams);

   procedure Write_CodeActionParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.CodeActionParams);

   procedure Read_FormattingOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.FormattingOptions);

   procedure Write_FormattingOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.FormattingOptions);

   procedure Read_DocumentFormattingParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.DocumentFormattingParams);

   procedure Write_DocumentFormattingParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.DocumentFormattingParams);

   procedure Read_DocumentRangeFormattingParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.DocumentRangeFormattingParams);

   procedure Write_DocumentRangeFormattingParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.DocumentRangeFormattingParams);

   procedure Read_DocumentOnTypeFormattingParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.DocumentOnTypeFormattingParams);

   procedure Write_DocumentOnTypeFormattingParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.DocumentOnTypeFormattingParams);

   procedure Read_RenameParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.RenameParams);

   procedure Write_RenameParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.RenameParams);

   procedure Read_ApplyWorkspaceEditParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.ApplyWorkspaceEditParams);

   procedure Write_ApplyWorkspaceEditParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.ApplyWorkspaceEditParams);

   procedure Read_ApplyWorkspaceEditResult
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.ApplyWorkspaceEditResult);

   procedure Write_ApplyWorkspaceEditResult
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.ApplyWorkspaceEditResult);

   procedure Read_WorkDoneProgressBegin
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.WorkDoneProgressBegin);

   procedure Write_WorkDoneProgressBegin
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.WorkDoneProgressBegin);

   procedure Read_WorkDoneProgressReport
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.WorkDoneProgressReport);

   procedure Write_WorkDoneProgressReport
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.WorkDoneProgressReport);

   procedure Read_WorkDoneProgressEnd
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.WorkDoneProgressEnd);

   procedure Write_WorkDoneProgressEnd
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.WorkDoneProgressEnd);

   procedure Read_WorkspaceFoldersChangeEvent
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.WorkspaceFoldersChangeEvent);

   procedure Write_WorkspaceFoldersChangeEvent
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.WorkspaceFoldersChangeEvent);

   procedure Read_DidChangeWorkspaceFoldersParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.DidChangeWorkspaceFoldersParams);

   procedure Write_DidChangeWorkspaceFoldersParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.DidChangeWorkspaceFoldersParams);

   procedure Read_ConfigurationItem
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.ConfigurationItem);

   procedure Write_ConfigurationItem
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.ConfigurationItem);

   procedure Read_ConfigurationParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.ConfigurationParams);

   procedure Write_ConfigurationParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.ConfigurationParams);

   procedure Read_CompletionTriggerKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.CompletionTriggerKind);

   procedure Write_CompletionTriggerKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.CompletionTriggerKind);

   procedure Read_CompletionContext
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.CompletionContext);

   procedure Write_CompletionContext
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.CompletionContext);

   procedure Read_CompletionParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.CompletionParams);

   procedure Write_CompletionParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.CompletionParams);

   procedure Read_Disable_Reason
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.Disable_Reason);

   procedure Write_Disable_Reason
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.Disable_Reason);

   procedure Read_RGBA_Color
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.RGBA_Color);

   procedure Write_RGBA_Color
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.RGBA_Color);

   procedure Read_ColorInformation
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.ColorInformation);

   procedure Write_ColorInformation
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.ColorInformation);

   procedure Read_ColorPresentationParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.ColorPresentationParams);

   procedure Write_ColorPresentationParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.ColorPresentationParams);

   procedure Read_ColorPresentation
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.ColorPresentation);

   procedure Write_ColorPresentation
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.ColorPresentation);

   procedure Read_FoldingRangeParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.FoldingRangeParams);

   procedure Write_FoldingRangeParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.FoldingRangeParams);

   procedure Read_FoldingRange
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.FoldingRange);

   procedure Write_FoldingRange
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.FoldingRange);

   procedure Read_DocumentColorParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.DocumentColorParams);

   procedure Write_DocumentColorParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.DocumentColorParams);

   procedure Read_SignatureHelpTriggerKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.SignatureHelpTriggerKind);

   procedure Write_SignatureHelpTriggerKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.SignatureHelpTriggerKind);

   procedure Read_SignatureHelpContext
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.SignatureHelpContext);

   procedure Write_SignatureHelpContext
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.SignatureHelpContext);

   procedure Read_SignatureHelpParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.SignatureHelpParams);

   procedure Write_SignatureHelpParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.SignatureHelpParams);

   procedure Read_NavigationRequestParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.NavigationRequestParams);

   procedure Write_NavigationRequestParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.NavigationRequestParams);

   procedure Read_SelectionRangeParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.SelectionRangeParams);

   procedure Write_SelectionRangeParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.SelectionRangeParams);

   procedure Read_SelectionRange
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.SelectionRange);

   procedure Write_SelectionRange
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.SelectionRange);

   procedure Read_LinkedEditingRanges
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.LinkedEditingRanges);

   procedure Write_LinkedEditingRanges
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.LinkedEditingRanges);

   procedure Read_CallHierarchyItem
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.CallHierarchyItem);

   procedure Write_CallHierarchyItem
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.CallHierarchyItem);

   procedure Read_CallHierarchyIncomingCallsParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.CallHierarchyIncomingCallsParams);

   procedure Write_CallHierarchyIncomingCallsParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.CallHierarchyIncomingCallsParams);

   procedure Read_CallHierarchyIncomingCall
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.CallHierarchyIncomingCall);

   procedure Write_CallHierarchyIncomingCall
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.CallHierarchyIncomingCall);

   procedure Read_CallHierarchyOutgoingCall
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.CallHierarchyOutgoingCall);

   procedure Write_CallHierarchyOutgoingCall
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.CallHierarchyOutgoingCall);

   procedure Read_SemanticTokensParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.SemanticTokensParams);

   procedure Write_SemanticTokensParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.SemanticTokensParams);

   procedure Read_SemanticTokens
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.SemanticTokens);

   procedure Write_SemanticTokens
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.SemanticTokens);

   procedure Read_SemanticTokensPartialResult
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.SemanticTokensPartialResult);

   procedure Write_SemanticTokensPartialResult
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.SemanticTokensPartialResult);

   procedure Read_SemanticTokensDeltaParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.SemanticTokensDeltaParams);

   procedure Write_SemanticTokensDeltaParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.SemanticTokensDeltaParams);

   procedure Read_SemanticTokensEdit
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.SemanticTokensEdit);

   procedure Write_SemanticTokensEdit
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.SemanticTokensEdit);

   procedure Read_SemanticTokensDelta
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.SemanticTokensDelta);

   procedure Write_SemanticTokensDelta
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.SemanticTokensDelta);

   procedure Read_SemanticTokensDeltaPartialResult
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.SemanticTokensDeltaPartialResult);

   procedure Write_SemanticTokensDeltaPartialResult
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.SemanticTokensDeltaPartialResult);

   procedure Read_SemanticTokensRangeParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.SemanticTokensRangeParams);

   procedure Write_SemanticTokensRangeParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.SemanticTokensRangeParams);

   procedure Read_UniquenessLevel
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.UniquenessLevel);

   procedure Write_UniquenessLevel
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.UniquenessLevel);

   procedure Read_MonikerKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.MonikerKind);

   procedure Write_MonikerKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.MonikerKind);

   procedure Read_Moniker
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.Moniker);

   procedure Write_Moniker
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.Moniker);

   procedure Read_ShowDocumentParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.ShowDocumentParams);

   procedure Write_ShowDocumentParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.ShowDocumentParams);

   procedure Read_ShowDocumentResult
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.ShowDocumentResult);

   procedure Write_ShowDocumentResult
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.ShowDocumentResult);

   procedure Read_FileCreate
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.FileCreate);

   procedure Write_FileCreate
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.FileCreate);

   procedure Read_CreateFilesParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.CreateFilesParams);

   procedure Write_CreateFilesParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.CreateFilesParams);

   procedure Read_FileRename
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.FileRename);

   procedure Write_FileRename
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.FileRename);

   procedure Read_RenameFilesParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.RenameFilesParams);

   procedure Write_RenameFilesParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.RenameFilesParams);

   procedure Read_FileDelete
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.FileDelete);

   procedure Write_FileDelete
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.FileDelete);

   procedure Read_DeleteFilesParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.DeleteFilesParams);

   procedure Write_DeleteFilesParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.DeleteFilesParams);

   procedure Read_LogTraceParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.LogTraceParams);

   procedure Write_LogTraceParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.LogTraceParams);

   procedure Read_SetTraceParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.SetTraceParams);

   procedure Write_SetTraceParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.SetTraceParams);

   procedure Read_ALS_Subprogram_And_References
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.ALS_Subprogram_And_References);

   procedure Write_ALS_Subprogram_And_References
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.ALS_Subprogram_And_References);

   procedure Read_ALS_Source_Dir_Description
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.ALS_Source_Dir_Description);

   procedure Write_ALS_Source_Dir_Description
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.ALS_Source_Dir_Description);

   procedure Read_ALS_ShowDependenciesKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.ALS_ShowDependenciesKind);

   procedure Write_ALS_ShowDependenciesKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.ALS_ShowDependenciesKind);

   procedure Read_ALS_ShowDependenciesParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.ALS_ShowDependenciesParams);

   procedure Write_ALS_ShowDependenciesParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.ALS_ShowDependenciesParams);

   procedure Read_ALS_Unit_Description
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.ALS_Unit_Description);

   procedure Write_ALS_Unit_Description
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.ALS_Unit_Description);

end LSP.Message_IO;
