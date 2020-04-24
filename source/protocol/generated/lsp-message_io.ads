--  Automatically generated, do not edit.
with Ada.Streams;
with LSP.Messages;

package LSP.Message_IO is

   procedure Write_Span
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.Span);

   procedure Write_TextDocumentEdit
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.TextDocumentEdit);

   procedure Write_TextDocumentPositionParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.TextDocumentPositionParams);

   procedure Write_WorkspaceEditClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.WorkspaceEditClientCapabilities);

   procedure Write_WorkspaceClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.WorkspaceClientCapabilities);

   procedure Write_SaveOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.SaveOptions);

   procedure Write_TextDocumentSyncClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.TextDocumentSyncClientCapabilities);

   procedure Write_CompletionItemTagSupport
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.CompletionItemTagSupport);

   procedure Write_completionItemCapability
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.completionItemCapability);

   procedure Write_HoverClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.HoverClientCapabilities);

   procedure Write_parameterInformation_Capability
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.parameterInformation_Capability);

   procedure Write_signatureInformation_Capability
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.signatureInformation_Capability);

   procedure Write_SignatureHelpClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.SignatureHelpClientCapabilities);

   procedure Write_DeclarationClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.DeclarationClientCapabilities);

   procedure Write_CodeActionClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.CodeActionClientCapabilities);

   procedure Write_DocumentLinkClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.DocumentLinkClientCapabilities);

   procedure Write_RenameClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.RenameClientCapabilities);

   procedure Write_DiagnosticTagSupport
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.DiagnosticTagSupport);

   procedure Write_PublishDiagnosticsClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.PublishDiagnosticsClientCapabilities);

   procedure Write_TextDocumentClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.TextDocumentClientCapabilities);

   procedure Write_WindowClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.WindowClientCapabilities);

   procedure Write_ClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.ClientCapabilities);

   procedure Write_TextDocumentSyncOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.TextDocumentSyncOptions);

   procedure Write_WorkspaceFoldersServerCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.WorkspaceFoldersServerCapabilities);

   procedure Write_workspace_Options
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.workspace_Options);

   procedure Write_ServerCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.ServerCapabilities);

   procedure Write_InitializeResult
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.InitializeResult);

   procedure Write_DidChangeConfigurationParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.DidChangeConfigurationParams);

   procedure Write_DidOpenTextDocumentParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.DidOpenTextDocumentParams);

   procedure Write_DidChangeTextDocumentParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.DidChangeTextDocumentParams);

   procedure Write_DidCloseTextDocumentParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.DidCloseTextDocumentParams);

   procedure Write_ParameterInformation
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.ParameterInformation);

   procedure Write_CodeActionContext
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.CodeActionContext);

   procedure Write_WorkspaceFoldersChangeEvent
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.WorkspaceFoldersChangeEvent);

   procedure Write_DidChangeWorkspaceFoldersParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.DidChangeWorkspaceFoldersParams);

   procedure Write_ConfigurationParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.ConfigurationParams);

   procedure Write_DidChangeWatchedFilesRegistrationOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.DidChangeWatchedFilesRegistrationOptions);

end LSP.Message_IO;
