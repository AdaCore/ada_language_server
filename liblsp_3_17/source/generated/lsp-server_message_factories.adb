--
--  Copyright (C) <YEAR>, <COPYRIGHT HOLDER>
--
--  SPDX-License-Identifier: MIT
--
--  DON'T EDIT THIS FILE! It was generated from metaModel.json.
--

with LSP.Server_Responses.Errors;
with LSP.Server_Notifications.SetTrace;
with LSP.Server_Notifications.Exits;
with LSP.Server_Notifications.Initialized;
with LSP.Server_Notifications.DidChangeNotebook;
with LSP.Server_Notifications.DidCloseNotebook;
with LSP.Server_Notifications.DidOpenNotebook;
with LSP.Server_Notifications.DidSaveNotebook;
with LSP.Server_Notifications.DidChange;
with LSP.Server_Notifications.DidClose;
with LSP.Server_Notifications.DidOpen;
with LSP.Server_Notifications.DidSave;
with LSP.Server_Notifications.WillSave;
with LSP.Server_Notifications.Cancel;
with LSP.Server_Notifications.DidChangeConfiguration;
with LSP.Server_Notifications.DidChangeWatchedFiles;
with LSP.Server_Notifications.DidChangeWorkspaceFolders;
with LSP.Server_Notifications.DidCreateFiles;
with LSP.Server_Notifications.DidDeleteFiles;
with LSP.Server_Notifications.DidRenameFiles;
with LSP.Server_Requests.AlsCheckSyntax;
with LSP.Server_Requests.IncomingCalls;
with LSP.Server_Requests.OutgoingCalls;
with LSP.Server_Requests.Code_Action_Resolve;
with LSP.Server_Requests.Code_Lens_Resolve;
with LSP.Server_Requests.Completion_Resolve;
with LSP.Server_Requests.Link_Resolve;
with LSP.Server_Requests.Initialize;
with LSP.Server_Requests.Inlay_Resolve;
with LSP.Server_Requests.Shutdown;
with LSP.Server_Requests.CodeAction;
with LSP.Server_Requests.CodeLens;
with LSP.Server_Requests.ColorPresentation;
with LSP.Server_Requests.Completion;
with LSP.Server_Requests.Declaration;
with LSP.Server_Requests.Definition;
with LSP.Server_Requests.Diagnostic;
with LSP.Server_Requests.DocumentColor;
with LSP.Server_Requests.DocumentHighlight;
with LSP.Server_Requests.DocumentLink;
with LSP.Server_Requests.DocumentSymbol;
with LSP.Server_Requests.FoldingRange;
with LSP.Server_Requests.Formatting;
with LSP.Server_Requests.Hover;
with LSP.Server_Requests.Implementation;
with LSP.Server_Requests.InlayHint;
with LSP.Server_Requests.InlineValue;
with LSP.Server_Requests.LinkedEditingRange;
with LSP.Server_Requests.Moniker;
with LSP.Server_Requests.OnTypeFormatting;
with LSP.Server_Requests.PrepareCallHierarchy;
with LSP.Server_Requests.PrepareRename;
with LSP.Server_Requests.PrepareTypeHierarchy;
with LSP.Server_Requests.RangeFormatting;
with LSP.Server_Requests.References;
with LSP.Server_Requests.Rename;
with LSP.Server_Requests.SelectionRange;
with LSP.Server_Requests.Tokens_Full;
with LSP.Server_Requests.Tokens_Delta;
with LSP.Server_Requests.Tokens_Range;
with LSP.Server_Requests.SignatureHelp;
with LSP.Server_Requests.TypeDefinition;
with LSP.Server_Requests.WillSaveWaitUntil;
with LSP.Server_Requests.Subtypes;
with LSP.Server_Requests.Supertypes;
with LSP.Server_Requests.Workspace_Diagnostic;
with LSP.Server_Requests.ExecuteCommand;
with LSP.Server_Requests.Symbol;
with LSP.Server_Requests.WillCreateFiles;
with LSP.Server_Requests.WillDeleteFiles;
with LSP.Server_Requests.WillRenameFiles;
with LSP.Server_Requests.Symbol_Resolve;
with LSP.Server_Responses.RegisterCapability;
with LSP.Server_Responses.UnregisterCapability;
with LSP.Server_Responses.ShowDocument;
with LSP.Server_Responses.ShowMessageRequest;
with LSP.Server_Responses.Progress_Create;
with LSP.Server_Responses.ApplyEdit;
with LSP.Server_Responses.Code_Lens_Refresh;
with LSP.Server_Responses.Configuration;
with LSP.Server_Responses.Diagnostic_Refresh;
with LSP.Server_Responses.Inlay_Refresh;
with LSP.Server_Responses.Inline_Refresh;
with LSP.Server_Responses.Tokens_Refresh;
with LSP.Server_Responses.WorkspaceFolders;

package body LSP.Server_Message_Factories is

   overriding procedure On_Error_Response
     (Self  : in out Server_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Errors.ResponseError) is
   begin
      Self.Consumer.On_Message (new LSP.Server_Responses.Errors.Response'
           (Id    => Id,
            Error => Value));
   end On_Error_Response;

   overriding procedure On_SetTrace_Notification
     (Self   : in out Server_Message_Factory;
      Params : LSP.Structures.SetTraceParams) is
   begin
      Self.Consumer.On_Message
        (new LSP.Server_Notifications.SetTrace.Notification'
           (Params => Params));
   end On_SetTrace_Notification;

   overriding procedure On_Exits_Notification
     (Self : in out Server_Message_Factory) is
   begin
      Self.Consumer.On_Message
        (new LSP.Server_Notifications.Exits.Notification);
   end On_Exits_Notification;

   overriding procedure On_Initialized_Notification
     (Self   : in out Server_Message_Factory;
      Params : LSP.Structures.InitializedParams) is
   begin
      Self.Consumer.On_Message
        (new LSP.Server_Notifications.Initialized.Notification'
           (Params => Params));
   end On_Initialized_Notification;

   overriding procedure On_DidChangeNotebook_Notification
     (Self   : in out Server_Message_Factory;
      Params : LSP.Structures.DidChangeNotebookDocumentParams) is
   begin
      Self.Consumer.On_Message
        (new LSP.Server_Notifications.DidChangeNotebook.Notification'
           (Params => Params));
   end On_DidChangeNotebook_Notification;

   overriding procedure On_DidCloseNotebook_Notification
     (Self   : in out Server_Message_Factory;
      Params : LSP.Structures.DidCloseNotebookDocumentParams) is
   begin
      Self.Consumer.On_Message
        (new LSP.Server_Notifications.DidCloseNotebook.Notification'
           (Params => Params));
   end On_DidCloseNotebook_Notification;

   overriding procedure On_DidOpenNotebook_Notification
     (Self   : in out Server_Message_Factory;
      Params : LSP.Structures.DidOpenNotebookDocumentParams) is
   begin
      Self.Consumer.On_Message
        (new LSP.Server_Notifications.DidOpenNotebook.Notification'
           (Params => Params));
   end On_DidOpenNotebook_Notification;

   overriding procedure On_DidSaveNotebook_Notification
     (Self   : in out Server_Message_Factory;
      Params : LSP.Structures.DidSaveNotebookDocumentParams) is
   begin
      Self.Consumer.On_Message
        (new LSP.Server_Notifications.DidSaveNotebook.Notification'
           (Params => Params));
   end On_DidSaveNotebook_Notification;

   overriding procedure On_DidChange_Notification
     (Self   : in out Server_Message_Factory;
      Params : LSP.Structures.DidChangeTextDocumentParams) is
   begin
      Self.Consumer.On_Message
        (new LSP.Server_Notifications.DidChange.Notification'
           (Params => Params));
   end On_DidChange_Notification;

   overriding procedure On_DidClose_Notification
     (Self   : in out Server_Message_Factory;
      Params : LSP.Structures.DidCloseTextDocumentParams) is
   begin
      Self.Consumer.On_Message
        (new LSP.Server_Notifications.DidClose.Notification'
           (Params => Params));
   end On_DidClose_Notification;

   overriding procedure On_DidOpen_Notification
     (Self   : in out Server_Message_Factory;
      Params : LSP.Structures.DidOpenTextDocumentParams) is
   begin
      Self.Consumer.On_Message
        (new LSP.Server_Notifications.DidOpen.Notification'(Params => Params));
   end On_DidOpen_Notification;

   overriding procedure On_DidSave_Notification
     (Self   : in out Server_Message_Factory;
      Params : LSP.Structures.DidSaveTextDocumentParams) is
   begin
      Self.Consumer.On_Message
        (new LSP.Server_Notifications.DidSave.Notification'(Params => Params));
   end On_DidSave_Notification;

   overriding procedure On_WillSave_Notification
     (Self   : in out Server_Message_Factory;
      Params : LSP.Structures.WillSaveTextDocumentParams) is
   begin
      Self.Consumer.On_Message
        (new LSP.Server_Notifications.WillSave.Notification'
           (Params => Params));
   end On_WillSave_Notification;

   overriding procedure On_Cancel_Notification
     (Self   : in out Server_Message_Factory;
      Params : LSP.Structures.WorkDoneProgressCancelParams) is
   begin
      Self.Consumer.On_Message
        (new LSP.Server_Notifications.Cancel.Notification'(Params => Params));
   end On_Cancel_Notification;

   overriding procedure On_DidChangeConfiguration_Notification
     (Self   : in out Server_Message_Factory;
      Params : LSP.Structures.DidChangeConfigurationParams) is
   begin
      Self.Consumer.On_Message
        (new LSP.Server_Notifications.DidChangeConfiguration.Notification'
           (Params => Params));
   end On_DidChangeConfiguration_Notification;

   overriding procedure On_DidChangeWatchedFiles_Notification
     (Self   : in out Server_Message_Factory;
      Params : LSP.Structures.DidChangeWatchedFilesParams) is
   begin
      Self.Consumer.On_Message
        (new LSP.Server_Notifications.DidChangeWatchedFiles.Notification'
           (Params => Params));
   end On_DidChangeWatchedFiles_Notification;

   overriding procedure On_DidChangeWorkspaceFolders_Notification
     (Self   : in out Server_Message_Factory;
      Params : LSP.Structures.DidChangeWorkspaceFoldersParams) is
   begin
      Self.Consumer.On_Message
        (new LSP.Server_Notifications.DidChangeWorkspaceFolders.Notification'
           (Params => Params));
   end On_DidChangeWorkspaceFolders_Notification;

   overriding procedure On_DidCreateFiles_Notification
     (Self   : in out Server_Message_Factory;
      Params : LSP.Structures.CreateFilesParams) is
   begin
      Self.Consumer.On_Message
        (new LSP.Server_Notifications.DidCreateFiles.Notification'
           (Params => Params));
   end On_DidCreateFiles_Notification;

   overriding procedure On_DidDeleteFiles_Notification
     (Self   : in out Server_Message_Factory;
      Params : LSP.Structures.DeleteFilesParams) is
   begin
      Self.Consumer.On_Message
        (new LSP.Server_Notifications.DidDeleteFiles.Notification'
           (Params => Params));
   end On_DidDeleteFiles_Notification;

   overriding procedure On_DidRenameFiles_Notification
     (Self   : in out Server_Message_Factory;
      Params : LSP.Structures.RenameFilesParams) is
   begin
      Self.Consumer.On_Message
        (new LSP.Server_Notifications.DidRenameFiles.Notification'
           (Params => Params));
   end On_DidRenameFiles_Notification;

   overriding procedure On_AlsCheckSyntax_Request
     (Self  : in out Server_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.AlsCheckSyntaxParams) is
   begin
      Self.Consumer.On_Message (new LSP.Server_Requests.AlsCheckSyntax.Request'
           (Id       => Id,
            Params   => Value,
            Canceled => False));
   end On_AlsCheckSyntax_Request;

   overriding procedure On_IncomingCalls_Request
     (Self  : in out Server_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.CallHierarchyIncomingCallsParams) is
   begin
      Self.Consumer.On_Message (new LSP.Server_Requests.IncomingCalls.Request'
           (Id       => Id,
            Params   => Value,
            Canceled => False));
   end On_IncomingCalls_Request;

   overriding procedure On_OutgoingCalls_Request
     (Self  : in out Server_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.CallHierarchyOutgoingCallsParams) is
   begin
      Self.Consumer.On_Message (new LSP.Server_Requests.OutgoingCalls.Request'
           (Id       => Id,
            Params   => Value,
            Canceled => False));
   end On_OutgoingCalls_Request;

   overriding procedure On_RegisterCapability_Response
     (Self  : in out Server_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Null_Record) is
   begin
      Self.Consumer.On_Message
        (new LSP.Server_Responses.RegisterCapability.Response'
           (Id     => Id,
            Result => Value));
   end On_RegisterCapability_Response;

   overriding procedure On_UnregisterCapability_Response
     (Self  : in out Server_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Null_Record) is
   begin
      Self.Consumer.On_Message
        (new LSP.Server_Responses.UnregisterCapability.Response'
           (Id     => Id,
            Result => Value));
   end On_UnregisterCapability_Response;

   overriding procedure On_Code_Action_Resolve_Request
     (Self  : in out Server_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.CodeAction) is
   begin
      Self.Consumer.On_Message
        (new LSP.Server_Requests.Code_Action_Resolve.Request'
           (Id       => Id,
            Params   => Value,
            Canceled => False));
   end On_Code_Action_Resolve_Request;

   overriding procedure On_Code_Lens_Resolve_Request
     (Self  : in out Server_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.CodeLens) is
   begin
      Self.Consumer.On_Message
        (new LSP.Server_Requests.Code_Lens_Resolve.Request'
           (Id       => Id,
            Params   => Value,
            Canceled => False));
   end On_Code_Lens_Resolve_Request;

   overriding procedure On_Completion_Resolve_Request
     (Self  : in out Server_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.CompletionItem) is
   begin
      Self.Consumer.On_Message
        (new LSP.Server_Requests.Completion_Resolve.Request'
           (Id       => Id,
            Params   => Value,
            Canceled => False));
   end On_Completion_Resolve_Request;

   overriding procedure On_Link_Resolve_Request
     (Self  : in out Server_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.DocumentLink) is
   begin
      Self.Consumer.On_Message (new LSP.Server_Requests.Link_Resolve.Request'
           (Id       => Id,
            Params   => Value,
            Canceled => False));
   end On_Link_Resolve_Request;

   overriding procedure On_Initialize_Request
     (Self  : in out Server_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.InitializeParams) is
   begin
      Self.Consumer.On_Message (new LSP.Server_Requests.Initialize.Request'
           (Id       => Id,
            Params   => Value,
            Canceled => False));
   end On_Initialize_Request;

   overriding procedure On_Inlay_Resolve_Request
     (Self  : in out Server_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.InlayHint) is
   begin
      Self.Consumer.On_Message (new LSP.Server_Requests.Inlay_Resolve.Request'
           (Id       => Id,
            Params   => Value,
            Canceled => False));
   end On_Inlay_Resolve_Request;

   overriding procedure On_Shutdown_Request
     (Self : in out Server_Message_Factory;
      Id   : LSP.Structures.Integer_Or_Virtual_String) is
   begin
      Self.Consumer.On_Message (new LSP.Server_Requests.Shutdown.Request'
           (Id       => Id,
            Canceled => False));
   end On_Shutdown_Request;

   overriding procedure On_CodeAction_Request
     (Self  : in out Server_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.CodeActionParams) is
   begin
      Self.Consumer.On_Message (new LSP.Server_Requests.CodeAction.Request'
           (Id       => Id,
            Params   => Value,
            Canceled => False));
   end On_CodeAction_Request;

   overriding procedure On_CodeLens_Request
     (Self  : in out Server_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.CodeLensParams) is
   begin
      Self.Consumer.On_Message (new LSP.Server_Requests.CodeLens.Request'
           (Id       => Id,
            Params   => Value,
            Canceled => False));
   end On_CodeLens_Request;

   overriding procedure On_ColorPresentation_Request
     (Self  : in out Server_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.ColorPresentationParams) is
   begin
      Self.Consumer.On_Message
        (new LSP.Server_Requests.ColorPresentation.Request'
           (Id       => Id,
            Params   => Value,
            Canceled => False));
   end On_ColorPresentation_Request;

   overriding procedure On_Completion_Request
     (Self  : in out Server_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.CompletionParams) is
   begin
      Self.Consumer.On_Message (new LSP.Server_Requests.Completion.Request'
           (Id       => Id,
            Params   => Value,
            Canceled => False));
   end On_Completion_Request;

   overriding procedure On_Declaration_Request
     (Self  : in out Server_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.DeclarationParams) is
   begin
      Self.Consumer.On_Message (new LSP.Server_Requests.Declaration.Request'
           (Id       => Id,
            Params   => Value,
            Canceled => False));
   end On_Declaration_Request;

   overriding procedure On_Definition_Request
     (Self  : in out Server_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.DefinitionParams) is
   begin
      Self.Consumer.On_Message (new LSP.Server_Requests.Definition.Request'
           (Id       => Id,
            Params   => Value,
            Canceled => False));
   end On_Definition_Request;

   overriding procedure On_Diagnostic_Request
     (Self  : in out Server_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.DocumentDiagnosticParams) is
   begin
      Self.Consumer.On_Message (new LSP.Server_Requests.Diagnostic.Request'
           (Id       => Id,
            Params   => Value,
            Canceled => False));
   end On_Diagnostic_Request;

   overriding procedure On_DocumentColor_Request
     (Self  : in out Server_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.DocumentColorParams) is
   begin
      Self.Consumer.On_Message (new LSP.Server_Requests.DocumentColor.Request'
           (Id       => Id,
            Params   => Value,
            Canceled => False));
   end On_DocumentColor_Request;

   overriding procedure On_DocumentHighlight_Request
     (Self  : in out Server_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.DocumentHighlightParams) is
   begin
      Self.Consumer.On_Message
        (new LSP.Server_Requests.DocumentHighlight.Request'
           (Id       => Id,
            Params   => Value,
            Canceled => False));
   end On_DocumentHighlight_Request;

   overriding procedure On_DocumentLink_Request
     (Self  : in out Server_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.DocumentLinkParams) is
   begin
      Self.Consumer.On_Message (new LSP.Server_Requests.DocumentLink.Request'
           (Id       => Id,
            Params   => Value,
            Canceled => False));
   end On_DocumentLink_Request;

   overriding procedure On_DocumentSymbol_Request
     (Self  : in out Server_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.DocumentSymbolParams) is
   begin
      Self.Consumer.On_Message (new LSP.Server_Requests.DocumentSymbol.Request'
           (Id       => Id,
            Params   => Value,
            Canceled => False));
   end On_DocumentSymbol_Request;

   overriding procedure On_FoldingRange_Request
     (Self  : in out Server_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.FoldingRangeParams) is
   begin
      Self.Consumer.On_Message (new LSP.Server_Requests.FoldingRange.Request'
           (Id       => Id,
            Params   => Value,
            Canceled => False));
   end On_FoldingRange_Request;

   overriding procedure On_Formatting_Request
     (Self  : in out Server_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.DocumentFormattingParams) is
   begin
      Self.Consumer.On_Message (new LSP.Server_Requests.Formatting.Request'
           (Id       => Id,
            Params   => Value,
            Canceled => False));
   end On_Formatting_Request;

   overriding procedure On_Hover_Request
     (Self  : in out Server_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.HoverParams) is
   begin
      Self.Consumer.On_Message (new LSP.Server_Requests.Hover.Request'
           (Id       => Id,
            Params   => Value,
            Canceled => False));
   end On_Hover_Request;

   overriding procedure On_Implementation_Request
     (Self  : in out Server_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.ImplementationParams) is
   begin
      Self.Consumer.On_Message (new LSP.Server_Requests.Implementation.Request'
           (Id       => Id,
            Params   => Value,
            Canceled => False));
   end On_Implementation_Request;

   overriding procedure On_InlayHint_Request
     (Self  : in out Server_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.InlayHintParams) is
   begin
      Self.Consumer.On_Message (new LSP.Server_Requests.InlayHint.Request'
           (Id       => Id,
            Params   => Value,
            Canceled => False));
   end On_InlayHint_Request;

   overriding procedure On_InlineValue_Request
     (Self  : in out Server_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.InlineValueParams) is
   begin
      Self.Consumer.On_Message (new LSP.Server_Requests.InlineValue.Request'
           (Id       => Id,
            Params   => Value,
            Canceled => False));
   end On_InlineValue_Request;

   overriding procedure On_LinkedEditingRange_Request
     (Self  : in out Server_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.LinkedEditingRangeParams) is
   begin
      Self.Consumer.On_Message
        (new LSP.Server_Requests.LinkedEditingRange.Request'
           (Id       => Id,
            Params   => Value,
            Canceled => False));
   end On_LinkedEditingRange_Request;

   overriding procedure On_Moniker_Request
     (Self  : in out Server_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.MonikerParams) is
   begin
      Self.Consumer.On_Message (new LSP.Server_Requests.Moniker.Request'
           (Id       => Id,
            Params   => Value,
            Canceled => False));
   end On_Moniker_Request;

   overriding procedure On_OnTypeFormatting_Request
     (Self  : in out Server_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.DocumentOnTypeFormattingParams) is
   begin
      Self.Consumer.On_Message
        (new LSP.Server_Requests.OnTypeFormatting.Request'
           (Id       => Id,
            Params   => Value,
            Canceled => False));
   end On_OnTypeFormatting_Request;

   overriding procedure On_PrepareCallHierarchy_Request
     (Self  : in out Server_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.CallHierarchyPrepareParams) is
   begin
      Self.Consumer.On_Message
        (new LSP.Server_Requests.PrepareCallHierarchy.Request'
           (Id       => Id,
            Params   => Value,
            Canceled => False));
   end On_PrepareCallHierarchy_Request;

   overriding procedure On_PrepareRename_Request
     (Self  : in out Server_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.PrepareRenameParams) is
   begin
      Self.Consumer.On_Message (new LSP.Server_Requests.PrepareRename.Request'
           (Id       => Id,
            Params   => Value,
            Canceled => False));
   end On_PrepareRename_Request;

   overriding procedure On_PrepareTypeHierarchy_Request
     (Self  : in out Server_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.TypeHierarchyPrepareParams) is
   begin
      Self.Consumer.On_Message
        (new LSP.Server_Requests.PrepareTypeHierarchy.Request'
           (Id       => Id,
            Params   => Value,
            Canceled => False));
   end On_PrepareTypeHierarchy_Request;

   overriding procedure On_RangeFormatting_Request
     (Self  : in out Server_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.DocumentRangeFormattingParams) is
   begin
      Self.Consumer.On_Message
        (new LSP.Server_Requests.RangeFormatting.Request'
           (Id       => Id,
            Params   => Value,
            Canceled => False));
   end On_RangeFormatting_Request;

   overriding procedure On_References_Request
     (Self  : in out Server_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.ReferenceParams) is
   begin
      Self.Consumer.On_Message (new LSP.Server_Requests.References.Request'
           (Id       => Id,
            Params   => Value,
            Canceled => False));
   end On_References_Request;

   overriding procedure On_Rename_Request
     (Self  : in out Server_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.RenameParams) is
   begin
      Self.Consumer.On_Message (new LSP.Server_Requests.Rename.Request'
           (Id       => Id,
            Params   => Value,
            Canceled => False));
   end On_Rename_Request;

   overriding procedure On_SelectionRange_Request
     (Self  : in out Server_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.SelectionRangeParams) is
   begin
      Self.Consumer.On_Message (new LSP.Server_Requests.SelectionRange.Request'
           (Id       => Id,
            Params   => Value,
            Canceled => False));
   end On_SelectionRange_Request;

   overriding procedure On_Tokens_Full_Request
     (Self  : in out Server_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.SemanticTokensParams) is
   begin
      Self.Consumer.On_Message (new LSP.Server_Requests.Tokens_Full.Request'
           (Id       => Id,
            Params   => Value,
            Canceled => False));
   end On_Tokens_Full_Request;

   overriding procedure On_Tokens_Delta_Request
     (Self  : in out Server_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.SemanticTokensDeltaParams) is
   begin
      Self.Consumer.On_Message (new LSP.Server_Requests.Tokens_Delta.Request'
           (Id       => Id,
            Params   => Value,
            Canceled => False));
   end On_Tokens_Delta_Request;

   overriding procedure On_Tokens_Range_Request
     (Self  : in out Server_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.SemanticTokensRangeParams) is
   begin
      Self.Consumer.On_Message (new LSP.Server_Requests.Tokens_Range.Request'
           (Id       => Id,
            Params   => Value,
            Canceled => False));
   end On_Tokens_Range_Request;

   overriding procedure On_SignatureHelp_Request
     (Self  : in out Server_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.SignatureHelpParams) is
   begin
      Self.Consumer.On_Message (new LSP.Server_Requests.SignatureHelp.Request'
           (Id       => Id,
            Params   => Value,
            Canceled => False));
   end On_SignatureHelp_Request;

   overriding procedure On_TypeDefinition_Request
     (Self  : in out Server_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.TypeDefinitionParams) is
   begin
      Self.Consumer.On_Message (new LSP.Server_Requests.TypeDefinition.Request'
           (Id       => Id,
            Params   => Value,
            Canceled => False));
   end On_TypeDefinition_Request;

   overriding procedure On_WillSaveWaitUntil_Request
     (Self  : in out Server_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.WillSaveTextDocumentParams) is
   begin
      Self.Consumer.On_Message
        (new LSP.Server_Requests.WillSaveWaitUntil.Request'
           (Id       => Id,
            Params   => Value,
            Canceled => False));
   end On_WillSaveWaitUntil_Request;

   overriding procedure On_Subtypes_Request
     (Self  : in out Server_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.TypeHierarchySubtypesParams) is
   begin
      Self.Consumer.On_Message (new LSP.Server_Requests.Subtypes.Request'
           (Id       => Id,
            Params   => Value,
            Canceled => False));
   end On_Subtypes_Request;

   overriding procedure On_Supertypes_Request
     (Self  : in out Server_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.TypeHierarchySupertypesParams) is
   begin
      Self.Consumer.On_Message (new LSP.Server_Requests.Supertypes.Request'
           (Id       => Id,
            Params   => Value,
            Canceled => False));
   end On_Supertypes_Request;

   overriding procedure On_ShowDocument_Response
     (Self  : in out Server_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.ShowDocumentResult) is
   begin
      Self.Consumer.On_Message (new LSP.Server_Responses.ShowDocument.Response'
           (Id     => Id,
            Result => Value));
   end On_ShowDocument_Response;

   overriding procedure On_ShowMessageRequest_Response
     (Self  : in out Server_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.MessageActionItem_Or_Null) is
   begin
      Self.Consumer.On_Message
        (new LSP.Server_Responses.ShowMessageRequest.Response'
           (Id     => Id,
            Result => Value));
   end On_ShowMessageRequest_Response;

   overriding procedure On_Progress_Create_Response
     (Self  : in out Server_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Null_Record) is
   begin
      Self.Consumer.On_Message
        (new LSP.Server_Responses.Progress_Create.Response'
           (Id     => Id,
            Result => Value));
   end On_Progress_Create_Response;

   overriding procedure On_ApplyEdit_Response
     (Self  : in out Server_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.ApplyWorkspaceEditResult) is
   begin
      Self.Consumer.On_Message (new LSP.Server_Responses.ApplyEdit.Response'
           (Id     => Id,
            Result => Value));
   end On_ApplyEdit_Response;

   overriding procedure On_Code_Lens_Refresh_Response
     (Self  : in out Server_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Null_Record) is
   begin
      Self.Consumer.On_Message
        (new LSP.Server_Responses.Code_Lens_Refresh.Response'
           (Id     => Id,
            Result => Value));
   end On_Code_Lens_Refresh_Response;

   overriding procedure On_Configuration_Response
     (Self  : in out Server_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.LSPAny_Vector) is
   begin
      Self.Consumer.On_Message
        (new LSP.Server_Responses.Configuration.Response'
           (Id     => Id,
            Result => Value));
   end On_Configuration_Response;

   overriding procedure On_Workspace_Diagnostic_Request
     (Self  : in out Server_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.WorkspaceDiagnosticParams) is
   begin
      Self.Consumer.On_Message
        (new LSP.Server_Requests.Workspace_Diagnostic.Request'
           (Id       => Id,
            Params   => Value,
            Canceled => False));
   end On_Workspace_Diagnostic_Request;

   overriding procedure On_Diagnostic_Refresh_Response
     (Self  : in out Server_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Null_Record) is
   begin
      Self.Consumer.On_Message
        (new LSP.Server_Responses.Diagnostic_Refresh.Response'
           (Id     => Id,
            Result => Value));
   end On_Diagnostic_Refresh_Response;

   overriding procedure On_ExecuteCommand_Request
     (Self  : in out Server_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.ExecuteCommandParams) is
   begin
      Self.Consumer.On_Message (new LSP.Server_Requests.ExecuteCommand.Request'
           (Id       => Id,
            Params   => Value,
            Canceled => False));
   end On_ExecuteCommand_Request;

   overriding procedure On_Inlay_Refresh_Response
     (Self  : in out Server_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Null_Record) is
   begin
      Self.Consumer.On_Message
        (new LSP.Server_Responses.Inlay_Refresh.Response'
           (Id     => Id,
            Result => Value));
   end On_Inlay_Refresh_Response;

   overriding procedure On_Inline_Refresh_Response
     (Self  : in out Server_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Null_Record) is
   begin
      Self.Consumer.On_Message
        (new LSP.Server_Responses.Inline_Refresh.Response'
           (Id     => Id,
            Result => Value));
   end On_Inline_Refresh_Response;

   overriding procedure On_Tokens_Refresh_Response
     (Self  : in out Server_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Null_Record) is
   begin
      Self.Consumer.On_Message
        (new LSP.Server_Responses.Tokens_Refresh.Response'
           (Id     => Id,
            Result => Value));
   end On_Tokens_Refresh_Response;

   overriding procedure On_Symbol_Request
     (Self  : in out Server_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.WorkspaceSymbolParams) is
   begin
      Self.Consumer.On_Message (new LSP.Server_Requests.Symbol.Request'
           (Id       => Id,
            Params   => Value,
            Canceled => False));
   end On_Symbol_Request;

   overriding procedure On_WillCreateFiles_Request
     (Self  : in out Server_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.CreateFilesParams) is
   begin
      Self.Consumer.On_Message
        (new LSP.Server_Requests.WillCreateFiles.Request'
           (Id       => Id,
            Params   => Value,
            Canceled => False));
   end On_WillCreateFiles_Request;

   overriding procedure On_WillDeleteFiles_Request
     (Self  : in out Server_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.DeleteFilesParams) is
   begin
      Self.Consumer.On_Message
        (new LSP.Server_Requests.WillDeleteFiles.Request'
           (Id       => Id,
            Params   => Value,
            Canceled => False));
   end On_WillDeleteFiles_Request;

   overriding procedure On_WillRenameFiles_Request
     (Self  : in out Server_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.RenameFilesParams) is
   begin
      Self.Consumer.On_Message
        (new LSP.Server_Requests.WillRenameFiles.Request'
           (Id       => Id,
            Params   => Value,
            Canceled => False));
   end On_WillRenameFiles_Request;

   overriding procedure On_WorkspaceFolders_Response
     (Self  : in out Server_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.WorkspaceFolder_Vector_Or_Null) is
   begin
      Self.Consumer.On_Message
        (new LSP.Server_Responses.WorkspaceFolders.Response'
           (Id     => Id,
            Result => Value));
   end On_WorkspaceFolders_Response;

   overriding procedure On_Symbol_Resolve_Request
     (Self  : in out Server_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.WorkspaceSymbol) is
   begin
      Self.Consumer.On_Message (new LSP.Server_Requests.Symbol_Resolve.Request'
           (Id       => Id,
            Params   => Value,
            Canceled => False));
   end On_Symbol_Resolve_Request;

end LSP.Server_Message_Factories;
