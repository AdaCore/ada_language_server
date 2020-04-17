--  Automatically generated, do not edit.
with LSP.JSON_Streams;
with LSP.Messages;                 use LSP.Messages;
with LSP.Types;

package body LSP.Message_IO is
   pragma Style_Checks ("M175");

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

end LSP.Message_IO;
