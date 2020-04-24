--  Automatically generated, do not edit.
with LSP.JSON_Streams;
with LSP.Messages;                 use LSP.Messages;
with LSP.Types;                    use LSP.Types;

package body LSP.Message_IO is
   pragma Style_Checks ("M175");

   procedure Write_Span
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Span)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("start");
      Position'Write (S, V.first);
      JS.Key ("end");
      Position'Write (S, V.last);
      JS.End_Object;
   end Write_Span;

   procedure Write_Location
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Location)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("uri");
      LSP.Types.Write (S, V.uri);
      JS.Key ("range");
      LSP.Messages.Span'Write (S, V.span);
      JS.Key ("alsKind");
      AlsReferenceKind_Set'Write (S, V.alsKind);
      JS.End_Object;
   end Write_Location;

   procedure Write_LocationLink
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LocationLink)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("originSelectionRange");
      Optional_Span'Write (S, V.originSelectionRange);
      JS.Key ("targetUri");
      LSP.Types.Write (S, V.targetUri);
      JS.Key ("targetRange");
      Span'Write (S, V.targetRange);
      JS.Key ("targetSelectionRange");
      Span'Write (S, V.targetSelectionRange);
      JS.Key ("alsKind");
      AlsReferenceKind_Set'Write (S, V.alsKind);
      JS.End_Object;
   end Write_LocationLink;

   procedure Write_DiagnosticRelatedInformation
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DiagnosticRelatedInformation)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("location");
      LSP.Messages.Location'Write (S, V.location);
      JS.Key ("message");
      LSP.Types.Write (S, V.message);
      JS.End_Object;
   end Write_DiagnosticRelatedInformation;

   procedure Write_TextEdit
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : TextEdit)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("range");
      LSP.Messages.Span'Write (S, V.span);
      JS.Key ("newText");
      LSP.Types.Write (S, V.newText);
      JS.End_Object;
   end Write_TextEdit;

   procedure Write_TextDocumentIdentifier
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : TextDocumentIdentifier)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("uri");
      LSP.Types.Write (S, V.uri);
      JS.End_Object;
   end Write_TextDocumentIdentifier;

   procedure Write_TextDocumentEdit
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : TextDocumentEdit)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("textDocument");
      VersionedTextDocumentIdentifier'Write (S, V.textDocument);
      JS.Key ("edits");
      TextEdit_Vector'Write (S, V.edits);
      JS.End_Object;
   end Write_TextDocumentEdit;

   procedure Write_TextDocumentItem
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : TextDocumentItem)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("uri");
      LSP.Types.Write (S, V.uri);
      JS.Key ("languageId");
      LSP.Types.Write (S, V.languageId);
      Write_Number (JS, +"version", LSP.Types.LSP_Number (V.version));
      JS.Key ("text");
      LSP.Types.Write (S, V.text);
      JS.End_Object;
   end Write_TextDocumentItem;

   procedure Write_TextDocumentPositionParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : TextDocumentPositionParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("textDocument");
      TextDocumentIdentifier'Write (S, V.textDocument);
      JS.Key ("position");
      LSP.Messages.Position'Write (S, V.position);
      JS.End_Object;
   end Write_TextDocumentPositionParams;

   procedure Write_WorkspaceEditClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : WorkspaceEditClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("documentChanges");
      Optional_Boolean'Write (S, V.documentChanges);
      JS.Key ("resourceOperations");
      Optional_ResourceOperationKindSet'Write (S, V.resourceOperations);
      JS.Key ("failureHandling");
      Optional_FailureHandlingKind'Write (S, V.failureHandling);
      JS.End_Object;
   end Write_WorkspaceEditClientCapabilities;

   procedure Write_WorkspaceClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : WorkspaceClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("applyEdit");
      Optional_Boolean'Write (S, V.applyEdit);
      JS.Key ("workspaceEdit");
      WorkspaceEditClientCapabilities'Write (S, V.workspaceEdit);
      JS.Key ("didChangeConfiguration");
      DidChangeConfigurationClientCapabilities'Write (S, V.didChangeConfiguration);
      JS.Key ("didChangeWatchedFiles");
      DidChangeWatchedFilesClientCapabilities'Write (S, V.didChangeWatchedFiles);
      JS.Key ("symbol");
      Optional_WorkspaceSymbolClientCapabilities'Write (S, V.symbol);
      JS.Key ("executeCommand");
      ExecuteCommandClientCapabilities'Write (S, V.executeCommand);
      JS.Key ("workspaceFolders");
      Optional_Boolean'Write (S, V.workspaceFolders);
      JS.Key ("configuration");
      Optional_Boolean'Write (S, V.configuration);
      JS.End_Object;
   end Write_WorkspaceClientCapabilities;

   procedure Write_MarkupContent
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : MarkupContent)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("kind");
      MarkupKind'Write (S, V.kind);
      JS.Key ("value");
      LSP.Types.Write (S, V.value);
      JS.End_Object;
   end Write_MarkupContent;

   procedure Write_SaveOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SaveOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("includeText");
      Optional_Boolean'Write (S, V.includeText);
      JS.End_Object;
   end Write_SaveOptions;

   procedure Write_TextDocumentSyncClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : TextDocumentSyncClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("dynamicRegistration");
      Optional_Boolean'Write (S, V.dynamicRegistration);
      JS.Key ("willSave");
      Optional_Boolean'Write (S, V.willSave);
      JS.Key ("willSaveWaitUntil");
      Optional_Boolean'Write (S, V.willSaveWaitUntil);
      JS.Key ("didSave");
      Optional_Boolean'Write (S, V.didSave);
      JS.End_Object;
   end Write_TextDocumentSyncClientCapabilities;

   procedure Write_CompletionItemTagSupport
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CompletionItemTagSupport)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("valueSet");
      CompletionItemTagSet'Write (S, V.valueSet);
      JS.End_Object;
   end Write_CompletionItemTagSupport;

   procedure Write_completionItemCapability
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : completionItemCapability)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("snippetSupport");
      Optional_Boolean'Write (S, V.snippetSupport);
      JS.Key ("commitCharactersSupport");
      Optional_Boolean'Write (S, V.commitCharactersSupport);
      JS.Key ("documentationFormat");
      MarkupKind_Vector'Write (S, V.documentationFormat);
      JS.Key ("deprecatedSupport");
      Optional_Boolean'Write (S, V.deprecatedSupport);
      JS.Key ("preselectSupport");
      Optional_Boolean'Write (S, V.preselectSupport);
      JS.Key ("tagSupport");
      Optional_CompletionItemTagSupport'Write (S, V.tagSupport);
      JS.End_Object;
   end Write_completionItemCapability;

   procedure Write_HoverClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : HoverClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("dynamicRegistration");
      Optional_Boolean'Write (S, V.dynamicRegistration);
      JS.Key ("contentFormat");
      Optional_MarkupKind_Vector'Write (S, V.contentFormat);
      JS.End_Object;
   end Write_HoverClientCapabilities;

   procedure Write_parameterInformation_Capability
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : parameterInformation_Capability)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("labelOffsetSupport");
      Optional_Boolean'Write (S, V.labelOffsetSupport);
      JS.End_Object;
   end Write_parameterInformation_Capability;

   procedure Write_signatureInformation_Capability
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : signatureInformation_Capability)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("documentationFormat");
      Optional_MarkupKind_Vector'Write (S, V.documentationFormat);
      JS.Key ("parameterInformation");
      Optional_parameterInformation_Capability'Write (S, V.parameterInformation);
      JS.End_Object;
   end Write_signatureInformation_Capability;

   procedure Write_SignatureHelpClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SignatureHelpClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("dynamicRegistration");
      Optional_Boolean'Write (S, V.dynamicRegistration);
      JS.Key ("signatureInformation");
      Optional_signatureInformation_Capability'Write (S, V.signatureInformation);
      JS.Key ("contextSupport");
      Optional_Boolean'Write (S, V.contextSupport);
      JS.End_Object;
   end Write_SignatureHelpClientCapabilities;

   procedure Write_DeclarationClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DeclarationClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("dynamicRegistration");
      Optional_Boolean'Write (S, V.dynamicRegistration);
      JS.Key ("linkSupport");
      Optional_Boolean'Write (S, V.linkSupport);
      JS.End_Object;
   end Write_DeclarationClientCapabilities;

   procedure Write_CodeActionClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CodeActionClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("dynamicRegistration");
      Optional_Boolean'Write (S, V.dynamicRegistration);
      JS.Key ("codeActionLiteralSupport");
      Optional_codeActionLiteralSupport_Capability'Write (S, V.codeActionLiteralSupport);
      JS.Key ("isPreferredSupport");
      Optional_Boolean'Write (S, V.isPreferredSupport);
      JS.End_Object;
   end Write_CodeActionClientCapabilities;

   procedure Write_DocumentLinkClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DocumentLinkClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("dynamicRegistration");
      Optional_Boolean'Write (S, V.dynamicRegistration);
      JS.Key ("tooltipSupport");
      Optional_Boolean'Write (S, V.tooltipSupport);
      JS.End_Object;
   end Write_DocumentLinkClientCapabilities;

   procedure Write_RenameClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : RenameClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("dynamicRegistration");
      Optional_Boolean'Write (S, V.dynamicRegistration);
      JS.Key ("prepareSupport");
      Optional_Boolean'Write (S, V.prepareSupport);
      JS.End_Object;
   end Write_RenameClientCapabilities;

   procedure Write_DiagnosticTagSupport
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DiagnosticTagSupport)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("valueSet");
      DiagnosticTagSet'Write (S, V.valueSet);
      JS.End_Object;
   end Write_DiagnosticTagSupport;

   procedure Write_PublishDiagnosticsClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : PublishDiagnosticsClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("relatedInformation");
      Optional_Boolean'Write (S, V.relatedInformation);
      JS.Key ("tagSupport");
      Optional_DiagnosticTagSupport'Write (S, V.tagSupport);
      JS.Key ("versionSupport");
      Optional_Boolean'Write (S, V.versionSupport);
      JS.End_Object;
   end Write_PublishDiagnosticsClientCapabilities;

   procedure Write_FoldingRangeClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : FoldingRangeClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("dynamicRegistration");
      Optional_Boolean'Write (S, V.dynamicRegistration);
      JS.Key ("rangeLimit");
      Optional_Number'Write (S, V.rangeLimit);
      JS.Key ("lineFoldingOnly");
      Optional_Boolean'Write (S, V.lineFoldingOnly);
      JS.End_Object;
   end Write_FoldingRangeClientCapabilities;

   procedure Write_TextDocumentClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : TextDocumentClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("synchronization");
      TextDocumentSyncClientCapabilities'Write (S, V.synchronization);
      JS.Key ("completion");
      CompletionClientCapabilities'Write (S, V.completion);
      JS.Key ("hover");
      Optional_HoverClientCapabilities'Write (S, V.hover);
      JS.Key ("signatureHelp");
      Optional_SignatureHelpClientCapabilities'Write (S, V.signatureHelp);
      JS.Key ("declaration");
      Optional_DeclarationClientCapabilities'Write (S, V.declaration);
      JS.Key ("definition");
      Optional_DefinitionClientCapabilities'Write (S, V.definition);
      JS.Key ("typeDefinition");
      Optional_TypeDefinitionClientCapabilities'Write (S, V.typeDefinition);
      JS.Key ("implementation");
      Optional_ImplementationClientCapabilities'Write (S, V.implementation);
      JS.Key ("references");
      ReferenceClientCapabilities'Write (S, V.references);
      JS.Key ("documentHighlight");
      DocumentHighlightClientCapabilities'Write (S, V.documentHighlight);
      JS.Key ("documentSymbol");
      Optional_DocumentSymbolClientCapabilities'Write (S, V.documentSymbol);
      JS.Key ("codeAction");
      Optional_CodeActionClientCapabilities'Write (S, V.codeAction);
      JS.Key ("codeLens");
      CodeLensClientCapabilities'Write (S, V.codeLens);
      JS.Key ("documentLink");
      Optional_DocumentLinkClientCapabilities'Write (S, V.documentLink);
      JS.Key ("colorProvider");
      DocumentColorClientCapabilities'Write (S, V.colorProvider);
      JS.Key ("formatting");
      DocumentFormattingClientCapabilities'Write (S, V.formatting);
      JS.Key ("rangeFormatting");
      DocumentRangeFormattingClientCapabilities'Write (S, V.rangeFormatting);
      JS.Key ("onTypeFormatting");
      DocumentOnTypeFormattingClientCapabilities'Write (S, V.onTypeFormatting);
      JS.Key ("rename");
      Optional_RenameClientCapabilities'Write (S, V.rename);
      JS.Key ("publishDiagnostics");
      Optional_PublishDiagnosticsClientCapabilities'Write (S, V.publishDiagnostics);
      JS.Key ("foldingRange");
      Optional_FoldingRangeClientCapabilities'Write (S, V.foldingRange);
      JS.Key ("selectionRange");
      SelectionRangeClientCapabilities'Write (S, V.selectionRange);
      JS.End_Object;
   end Write_TextDocumentClientCapabilities;

   procedure Write_WindowClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : WindowClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("workDoneProgress");
      Optional_Boolean'Write (S, V.workDoneProgress);
      JS.End_Object;
   end Write_WindowClientCapabilities;

   procedure Write_ClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ClientCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("workspace");
      WorkspaceClientCapabilities'Write (S, V.workspace);
      JS.Key ("textDocument");
      TextDocumentClientCapabilities'Write (S, V.textDocument);
      JS.Key ("window");
      Optional_WindowClientCapabilities'Write (S, V.window);
      JS.End_Object;
   end Write_ClientCapabilities;

   procedure Write_WorkspaceFolder
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : WorkspaceFolder)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("uri");
      LSP.Types.Write (S, V.uri);
      JS.Key ("name");
      LSP.Types.Write (S, V.name);
      JS.End_Object;
   end Write_WorkspaceFolder;

   procedure Write_ProgramInfo
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ProgramInfo)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("name");
      LSP.Types.Write (S, V.name);
      JS.Key ("version");
      Optional_String'Write (S, V.version);
      JS.End_Object;
   end Write_ProgramInfo;

   procedure Write_TextDocumentSyncOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : TextDocumentSyncOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("openClose");
      Optional_Boolean'Write (S, V.openClose);
      JS.Key ("change");
      Optional_TextDocumentSyncKind'Write (S, V.change);
      JS.Key ("willSave");
      Optional_Boolean'Write (S, V.willSave);
      JS.Key ("willSaveWaitUntil");
      Optional_Boolean'Write (S, V.willSaveWaitUntil);
      JS.Key ("save");
      Optional_SaveOptions'Write (S, V.save);
      JS.End_Object;
   end Write_TextDocumentSyncOptions;

   procedure Write_WorkspaceFoldersServerCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : WorkspaceFoldersServerCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("supported");
      Optional_Boolean'Write (S, V.supported);
      JS.Key ("changeNotifications");
      Optional_Boolean_Or_String'Write (S, V.changeNotifications);
      JS.End_Object;
   end Write_WorkspaceFoldersServerCapabilities;

   procedure Write_workspace_Options
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : workspace_Options)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("workspaceFolders");
      Optional_WorkspaceFoldersServerCapabilities'Write (S, V.workspaceFolders);
      JS.End_Object;
   end Write_workspace_Options;

   procedure Write_ServerCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ServerCapabilities)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("textDocumentSync");
      Optional_TextDocumentSyncOptions'Write (S, V.textDocumentSync);
      JS.Key ("completionProvider");
      Optional_CompletionOptions'Write (S, V.completionProvider);
      JS.Key ("hoverProvider");
      HoverOptions'Write (S, V.hoverProvider);
      JS.Key ("signatureHelpProvider");
      Optional_SignatureHelpOptions'Write (S, V.signatureHelpProvider);
      JS.Key ("declarationProvider");
      DeclarationOptions'Write (S, V.declarationProvider);
      JS.Key ("definitionProvider");
      DefinitionOptions'Write (S, V.definitionProvider);
      JS.Key ("typeDefinitionProvider");
      TypeDefinitionOptions'Write (S, V.typeDefinitionProvider);
      JS.Key ("implementationProvider");
      ImplementationOptions'Write (S, V.implementationProvider);
      JS.Key ("referencesProvider");
      ReferenceOptions'Write (S, V.referencesProvider);
      JS.Key ("documentHighlightProvider");
      DocumentHighlightOptions'Write (S, V.documentHighlightProvider);
      JS.Key ("documentSymbolProvider");
      DocumentSymbolOptions'Write (S, V.documentSymbolProvider);
      JS.Key ("codeActionProvider");
      Optional_CodeActionOptions'Write (S, V.codeActionProvider);
      JS.Key ("codeLensProvider");
      Optional_CodeLensOptions'Write (S, V.codeLensProvider);
      JS.Key ("documentLinkProvider");
      Optional_DocumentLinkOptions'Write (S, V.documentLinkProvider);
      JS.Key ("colorProvider");
      DocumentColorOptions'Write (S, V.colorProvider);
      JS.Key ("documentFormattingProvider");
      DocumentFormattingOptions'Write (S, V.documentFormattingProvider);
      JS.Key ("documentRangeFormattingProvider");
      DocumentRangeFormattingOptions'Write (S, V.documentRangeFormattingProvider);
      JS.Key ("documentOnTypeFormattingProvider");
      Optional_DocumentOnTypeFormattingOptions'Write (S, V.documentOnTypeFormattingProvider);
      JS.Key ("renameProvider");
      Optional_RenameOptions'Write (S, V.renameProvider);
      JS.Key ("foldingRangeProvider");
      FoldingRangeOptions'Write (S, V.foldingRangeProvider);
      JS.Key ("executeCommandProvider");
      Optional_ExecuteCommandOptions'Write (S, V.executeCommandProvider);
      JS.Key ("selectionRangeProvider");
      SelectionRangeOptions'Write (S, V.selectionRangeProvider);
      JS.Key ("workspaceSymbolProvider");
      WorkspaceSymbolOptions'Write (S, V.workspaceSymbolProvider);
      JS.Key ("workspace");
      Optional_workspace_Options'Write (S, V.workspace);
      JS.Key ("alsCalledByProvider");
      Optional_Boolean'Write (S, V.alsCalledByProvider);
      JS.Key ("alsReferenceKinds");
      Optional_AlsReferenceKind_Set'Write (S, V.alsReferenceKinds);
      JS.End_Object;
   end Write_ServerCapabilities;

   procedure Write_InitializeResult
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : InitializeResult)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("capabilities");
      ServerCapabilities'Write (S, V.capabilities);
      JS.Key ("serverInfo");
      Optional_ProgramInfo'Write (S, V.serverInfo);
      JS.End_Object;
   end Write_InitializeResult;

   procedure Write_InitializedParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : InitializedParams)
   is
      pragma Unreferenced (V);

      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.End_Object;
   end Write_InitializedParams;

   procedure Write_DidChangeConfigurationParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DidChangeConfigurationParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("settings");
      LSP.Types.LSP_Any'Write (S, V.settings);
      JS.End_Object;
   end Write_DidChangeConfigurationParams;

   procedure Write_DidOpenTextDocumentParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DidOpenTextDocumentParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("textDocument");
      TextDocumentItem'Write (S, V.textDocument);
      JS.End_Object;
   end Write_DidOpenTextDocumentParams;

   procedure Write_TextDocumentContentChangeEvent
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : TextDocumentContentChangeEvent)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("range");
      Optional_Span'Write (S, V.span);
      JS.Key ("rangeLength");
      LSP.Types.Optional_Number'Write (S, V.rangeLength);
      JS.Key ("text");
      LSP.Types.Write (S, V.text);
      JS.End_Object;
   end Write_TextDocumentContentChangeEvent;

   procedure Write_DidChangeTextDocumentParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DidChangeTextDocumentParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("textDocument");
      VersionedTextDocumentIdentifier'Write (S, V.textDocument);
      JS.Key ("contentChanges");
      TextDocumentContentChangeEvent_Vector'Write (S, V.contentChanges);
      JS.End_Object;
   end Write_DidChangeTextDocumentParams;

   procedure Write_DidSaveTextDocumentParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DidSaveTextDocumentParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("textDocument");
      TextDocumentIdentifier'Write (S, V.textDocument);
      JS.Key ("text");
      Optional_String'Write (S, V.text);
      JS.End_Object;
   end Write_DidSaveTextDocumentParams;

   procedure Write_DidCloseTextDocumentParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DidCloseTextDocumentParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("textDocument");
      TextDocumentIdentifier'Write (S, V.textDocument);
      JS.End_Object;
   end Write_DidCloseTextDocumentParams;

   procedure Write_CompletionList
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CompletionList)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Write_Boolean (JS, +"isIncomplete", V.isIncomplete);
      JS.Key ("items");
      CompletionItem_Vector'Write (S, V.items);
      JS.End_Object;
   end Write_CompletionList;

   procedure Write_Hover
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Hover)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("contents");
      MarkupContent_Or_MarkedString_Vector'Write (S, V.contents);
      JS.Key ("range");
      Optional_Span'Write (S, V.Span);
      JS.End_Object;
   end Write_Hover;

   procedure Write_ParameterInformation
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ParameterInformation)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("label");
      Parameter_Label'Write (S, V.label);
      JS.Key ("documentation");
      Optional_String_Or_MarkupContent'Write (S, V.documentation);
      JS.End_Object;
   end Write_ParameterInformation;

   procedure Write_SignatureInformation
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SignatureInformation)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("label");
      LSP.Types.Write (S, V.label);
      JS.Key ("documentation");
      Optional_String_Or_MarkupContent'Write (S, V.documentation);
      JS.Key ("parameters");
      ParameterInformation_Vector'Write (S, V.parameters);
      JS.End_Object;
   end Write_SignatureInformation;

   procedure Write_ReferenceContext
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ReferenceContext)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Write_Boolean (JS, +"includeDeclaration", V.includeDeclaration);
      JS.End_Object;
   end Write_ReferenceContext;

   procedure Write_DocumentHighlight
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DocumentHighlight)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("range");
      LSP.Messages.Span'Write (S, V.span);
      JS.Key ("kind");
      Optional_DocumentHighlightKind'Write (S, V.kind);
      JS.End_Object;
   end Write_DocumentHighlight;

   procedure Write_SymbolInformation
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SymbolInformation)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("name");
      LSP.Types.Write (S, V.name);
      JS.Key ("kind");
      SymbolKind'Write (S, V.kind);
      JS.Key ("alsIsAdaProcedure");
      Optional_Boolean'Write (S, V.alsIsAdaProcedure);
      JS.Key ("deprecated");
      Optional_Boolean'Write (S, V.deprecated);
      JS.Key ("location");
      LSP.Messages.Location'Write (S, V.location);
      JS.Key ("containerName");
      Optional_String'Write (S, V.containerName);
      JS.End_Object;
   end Write_SymbolInformation;

   procedure Write_CodeActionContext
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CodeActionContext)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("diagnostics");
      Diagnostic_Vector'Write (S, V.diagnostics);
      JS.Key ("only");
      Optional_CodeActionKindSet'Write (S, V.only);
      JS.End_Object;
   end Write_CodeActionContext;

   procedure Write_ApplyWorkspaceEditResult
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ApplyWorkspaceEditResult)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Write_Boolean (JS, +"applied", V.applied);
      JS.Key ("failureReason");
      Optional_String'Write (S, V.failureReason);
      JS.End_Object;
   end Write_ApplyWorkspaceEditResult;

   procedure Write_WorkspaceFoldersChangeEvent
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : WorkspaceFoldersChangeEvent)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("added");
      WorkspaceFolder_Vector'Write (S, V.added);
      JS.Key ("removed");
      WorkspaceFolder_Vector'Write (S, V.removed);
      JS.End_Object;
   end Write_WorkspaceFoldersChangeEvent;

   procedure Write_DidChangeWorkspaceFoldersParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DidChangeWorkspaceFoldersParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("event");
      WorkspaceFoldersChangeEvent'Write (S, V.event);
      JS.End_Object;
   end Write_DidChangeWorkspaceFoldersParams;

   procedure Write_ConfigurationItem
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ConfigurationItem)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("scopeUri");
      Optional_String'Write (S, V.scopeUri);
      JS.Key ("section");
      Optional_String'Write (S, V.section);
      JS.End_Object;
   end Write_ConfigurationItem;

   procedure Write_ConfigurationParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ConfigurationParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("items");
      ConfigurationItem_Vector'Write (S, V.items);
      JS.End_Object;
   end Write_ConfigurationParams;

   procedure Write_FileSystemWatcher
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : FileSystemWatcher)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("globPattern");
      LSP.Types.Write (S, V.globPattern);
      JS.Key ("kind");
      WatchKind_Set'Write (S, V.kind);
      JS.End_Object;
   end Write_FileSystemWatcher;

   procedure Write_DidChangeWatchedFilesRegistrationOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DidChangeWatchedFilesRegistrationOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("watchers");
      FileSystemWatcher_Vector'Write (S, V.watchers);
      JS.End_Object;
   end Write_DidChangeWatchedFilesRegistrationOptions;

   procedure Write_RGBA_Color
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : RGBA_Color)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Write_Number (JS, +"red", V.red);
      Write_Number (JS, +"green", V.green);
      Write_Number (JS, +"blue", V.blue);
      Write_Number (JS, +"alpha", V.alpha);
      JS.End_Object;
   end Write_RGBA_Color;

   procedure Write_ColorInformation
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ColorInformation)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("range");
      LSP.Messages.Span'Write (S, V.span);
      JS.Key ("color");
      RGBA_Color'Write (S, V.color);
      JS.End_Object;
   end Write_ColorInformation;

   procedure Write_FoldingRange
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : FoldingRange)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Write_Number (JS, +"startLine", V.startLine);
      JS.Key ("startCharacter");
      Optional_Number'Write (S, V.startCharacter);
      Write_Number (JS, +"endLine", V.endLine);
      JS.Key ("endCharacter");
      Optional_Number'Write (S, V.endCharacter);
      JS.Key ("kind");
      Optional_String'Write (S, V.kind);
      JS.End_Object;
   end Write_FoldingRange;

   procedure Write_SelectionRange
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SelectionRange)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("range");
      LSP.Messages.Span'Write (S, V.span);
      JS.End_Object;
   end Write_SelectionRange;

   procedure Write_ALS_Subprogram_And_References
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ALS_Subprogram_And_References)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("location");
      Location'Write (S, V.loc);
      JS.Key ("name");
      LSP.Types.Write (S, V.name);
      JS.Key ("refs");
      Location_Vector'Write (S, V.refs);
      JS.End_Object;
   end Write_ALS_Subprogram_And_References;

end LSP.Message_IO;
