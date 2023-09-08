--
--  Copyright (C) <YEAR>, <COPYRIGHT HOLDER>
--
--  SPDX-License-Identifier: MIT
--
--  DON'T EDIT THIS FILE! It was generated from metaModel.json.
--

with LSP.Client_Responses.Errors;
with LSP.Client_Notifications.LogTrace;
with LSP.Client_Notifications.Event;
with LSP.Client_Notifications.PublishDiagnostics;
with LSP.Client_Notifications.LogMessage;
with LSP.Client_Notifications.ShowMessage;
with LSP.Client_Requests.RegisterCapability;
with LSP.Client_Requests.UnregisterCapability;
with LSP.Client_Requests.ShowDocument;
with LSP.Client_Requests.ShowMessageRequest;
with LSP.Client_Requests.Progress_Create;
with LSP.Client_Requests.ApplyEdit;
with LSP.Client_Requests.Code_Lens_Refresh;
with LSP.Client_Requests.Configuration;
with LSP.Client_Requests.Diagnostic_Refresh;
with LSP.Client_Requests.Inlay_Refresh;
with LSP.Client_Requests.Inline_Refresh;
with LSP.Client_Requests.Tokens_Refresh;
with LSP.Client_Requests.WorkspaceFolders;
with LSP.Client_Responses.AlsCheckSyntax;
with LSP.Client_Responses.IncomingCalls;
with LSP.Client_Responses.OutgoingCalls;
with LSP.Client_Responses.Code_Action_Resolve;
with LSP.Client_Responses.Code_Lens_Resolve;
with LSP.Client_Responses.Completion_Resolve;
with LSP.Client_Responses.Link_Resolve;
with LSP.Client_Responses.Initialize;
with LSP.Client_Responses.Inlay_Resolve;
with LSP.Client_Responses.Shutdown;
with LSP.Client_Responses.CodeAction;
with LSP.Client_Responses.CodeLens;
with LSP.Client_Responses.ColorPresentation;
with LSP.Client_Responses.Completion;
with LSP.Client_Responses.Declaration;
with LSP.Client_Responses.Definition;
with LSP.Client_Responses.Diagnostic;
with LSP.Client_Responses.DocumentColor;
with LSP.Client_Responses.DocumentHighlight;
with LSP.Client_Responses.DocumentLink;
with LSP.Client_Responses.DocumentSymbol;
with LSP.Client_Responses.FoldingRange;
with LSP.Client_Responses.Formatting;
with LSP.Client_Responses.Hover;
with LSP.Client_Responses.Implementation;
with LSP.Client_Responses.InlayHint;
with LSP.Client_Responses.InlineValue;
with LSP.Client_Responses.LinkedEditingRange;
with LSP.Client_Responses.Moniker;
with LSP.Client_Responses.OnTypeFormatting;
with LSP.Client_Responses.PrepareCallHierarchy;
with LSP.Client_Responses.PrepareRename;
with LSP.Client_Responses.PrepareTypeHierarchy;
with LSP.Client_Responses.RangeFormatting;
with LSP.Client_Responses.References;
with LSP.Client_Responses.Rename;
with LSP.Client_Responses.SelectionRange;
with LSP.Client_Responses.Tokens_Full;
with LSP.Client_Responses.Tokens_Delta;
with LSP.Client_Responses.Tokens_Range;
with LSP.Client_Responses.SignatureHelp;
with LSP.Client_Responses.TypeDefinition;
with LSP.Client_Responses.WillSaveWaitUntil;
with LSP.Client_Responses.Subtypes;
with LSP.Client_Responses.Supertypes;
with LSP.Client_Responses.Workspace_Diagnostic;
with LSP.Client_Responses.ExecuteCommand;
with LSP.Client_Responses.Symbol;
with LSP.Client_Responses.WillCreateFiles;
with LSP.Client_Responses.WillDeleteFiles;
with LSP.Client_Responses.WillRenameFiles;
with LSP.Client_Responses.Symbol_Resolve;
with LSP.Progress_Reports.IncomingCalls;
with LSP.Progress_Reports.OutgoingCalls;
with LSP.Progress_Reports.CodeAction;
with LSP.Progress_Reports.CodeLens;
with LSP.Progress_Reports.ColorPresentation;
with LSP.Progress_Reports.Completion;
with LSP.Progress_Reports.Declaration;
with LSP.Progress_Reports.Definition;
with LSP.Progress_Reports.Diagnostic;
with LSP.Progress_Reports.DocumentColor;
with LSP.Progress_Reports.DocumentHighlight;
with LSP.Progress_Reports.DocumentLink;
with LSP.Progress_Reports.DocumentSymbol;
with LSP.Progress_Reports.FoldingRange;
with LSP.Progress_Reports.Implementation;
with LSP.Progress_Reports.InlayHint;
with LSP.Progress_Reports.InlineValue;
with LSP.Progress_Reports.Moniker;
with LSP.Progress_Reports.References;
with LSP.Progress_Reports.SelectionRange;
with LSP.Progress_Reports.Tokens_Full;
with LSP.Progress_Reports.Tokens_Delta;
with LSP.Progress_Reports.Tokens_Range;
with LSP.Progress_Reports.TypeDefinition;
with LSP.Progress_Reports.Subtypes;
with LSP.Progress_Reports.Supertypes;
with LSP.Progress_Reports.Workspace_Diagnostic;
with LSP.Progress_Reports.Symbol;
with LSP.Progress_Reports.ProgressBegin;

with LSP.Progress_Reports.ProgressReport;

with LSP.Progress_Reports.ProgressEnd;

package body LSP.Client_Message_Factories is

   overriding procedure On_Error_Response
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Errors.ResponseError) is
   begin
      Client_Message_Factory'Class (Self).On_Message
        (new LSP.Client_Responses.Errors.Response'
           (Id    => Id,
            Error => Value));
   end On_Error_Response;

   overriding procedure On_LogTrace_Notification
     (Self  : in out Client_Message_Factory;
      Value : LSP.Structures.LogTraceParams) is
   begin
      Client_Message_Factory'Class (Self).On_Message
        (new LSP.Client_Notifications.LogTrace.Notification'(Params => Value));
   end On_LogTrace_Notification;

   overriding procedure On_Event_Notification
     (Self : in out Client_Message_Factory; Value : LSP.Structures.LSPAny) is
   begin
      Client_Message_Factory'Class (Self).On_Message
        (new LSP.Client_Notifications.Event.Notification'(Params => Value));
   end On_Event_Notification;

   overriding procedure On_PublishDiagnostics_Notification
     (Self  : in out Client_Message_Factory;
      Value : LSP.Structures.PublishDiagnosticsParams) is
   begin
      Client_Message_Factory'Class (Self).On_Message
        (new LSP.Client_Notifications.PublishDiagnostics.Notification'
           (Params => Value));
   end On_PublishDiagnostics_Notification;

   overriding procedure On_LogMessage_Notification
     (Self  : in out Client_Message_Factory;
      Value : LSP.Structures.LogMessageParams) is
   begin
      Client_Message_Factory'Class (Self).On_Message
        (new LSP.Client_Notifications.LogMessage.Notification'
           (Params => Value));
   end On_LogMessage_Notification;

   overriding procedure On_ShowMessage_Notification
     (Self  : in out Client_Message_Factory;
      Value : LSP.Structures.ShowMessageParams) is
   begin
      Client_Message_Factory'Class (Self).On_Message
        (new LSP.Client_Notifications.ShowMessage.Notification'
           (Params => Value));
   end On_ShowMessage_Notification;

   overriding procedure On_RegisterCapability_Request
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.RegistrationParams) is
   begin
      Client_Message_Factory'Class (Self).On_Message
        (new LSP.Client_Requests.RegisterCapability.Request'
           (Id     => Id,
            Params => Value));
   end On_RegisterCapability_Request;

   overriding procedure On_UnregisterCapability_Request
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.UnregistrationParams) is
   begin
      Client_Message_Factory'Class (Self).On_Message
        (new LSP.Client_Requests.UnregisterCapability.Request'
           (Id     => Id,
            Params => Value));
   end On_UnregisterCapability_Request;

   overriding procedure On_ShowDocument_Request
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.ShowDocumentParams) is
   begin
      Client_Message_Factory'Class (Self).On_Message
        (new LSP.Client_Requests.ShowDocument.Request'
           (Id     => Id,
            Params => Value));
   end On_ShowDocument_Request;

   overriding procedure On_ShowMessageRequest_Request
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.ShowMessageRequestParams) is
   begin
      Client_Message_Factory'Class (Self).On_Message
        (new LSP.Client_Requests.ShowMessageRequest.Request'
           (Id     => Id,
            Params => Value));
   end On_ShowMessageRequest_Request;

   overriding procedure On_Progress_Create_Request
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.WorkDoneProgressCreateParams) is
   begin
      Client_Message_Factory'Class (Self).On_Message
        (new LSP.Client_Requests.Progress_Create.Request'
           (Id     => Id,
            Params => Value));
   end On_Progress_Create_Request;

   overriding procedure On_ApplyEdit_Request
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.ApplyWorkspaceEditParams) is
   begin
      Client_Message_Factory'Class (Self).On_Message
        (new LSP.Client_Requests.ApplyEdit.Request'
           (Id     => Id,
            Params => Value));
   end On_ApplyEdit_Request;

   overriding procedure On_Code_Lens_Refresh_Request
     (Self : in out Client_Message_Factory;
      Id   : LSP.Structures.Integer_Or_Virtual_String) is
   begin
      Client_Message_Factory'Class (Self).On_Message
        (new LSP.Client_Requests.Code_Lens_Refresh.Request'(Id => Id));
   end On_Code_Lens_Refresh_Request;

   overriding procedure On_Configuration_Request
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.ConfigurationParams) is
   begin
      Client_Message_Factory'Class (Self).On_Message
        (new LSP.Client_Requests.Configuration.Request'
           (Id     => Id,
            Params => Value));
   end On_Configuration_Request;

   overriding procedure On_Diagnostic_Refresh_Request
     (Self : in out Client_Message_Factory;
      Id   : LSP.Structures.Integer_Or_Virtual_String) is
   begin
      Client_Message_Factory'Class (Self).On_Message
        (new LSP.Client_Requests.Diagnostic_Refresh.Request'(Id => Id));
   end On_Diagnostic_Refresh_Request;

   overriding procedure On_Inlay_Refresh_Request
     (Self : in out Client_Message_Factory;
      Id   : LSP.Structures.Integer_Or_Virtual_String) is
   begin
      Client_Message_Factory'Class (Self).On_Message
        (new LSP.Client_Requests.Inlay_Refresh.Request'(Id => Id));
   end On_Inlay_Refresh_Request;

   overriding procedure On_Inline_Refresh_Request
     (Self : in out Client_Message_Factory;
      Id   : LSP.Structures.Integer_Or_Virtual_String) is
   begin
      Client_Message_Factory'Class (Self).On_Message
        (new LSP.Client_Requests.Inline_Refresh.Request'(Id => Id));
   end On_Inline_Refresh_Request;

   overriding procedure On_Tokens_Refresh_Request
     (Self : in out Client_Message_Factory;
      Id   : LSP.Structures.Integer_Or_Virtual_String) is
   begin
      Client_Message_Factory'Class (Self).On_Message
        (new LSP.Client_Requests.Tokens_Refresh.Request'(Id => Id));
   end On_Tokens_Refresh_Request;

   overriding procedure On_WorkspaceFolders_Request
     (Self : in out Client_Message_Factory;
      Id   : LSP.Structures.Integer_Or_Virtual_String) is
   begin
      Client_Message_Factory'Class (Self).On_Message
        (new LSP.Client_Requests.WorkspaceFolders.Request'(Id => Id));
   end On_WorkspaceFolders_Request;

   overriding procedure On_AlsCheckSyntax_Response
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.AlsCheckSyntaxResult) is
   begin
      Client_Message_Factory'Class (Self).On_Message
        (new LSP.Client_Responses.AlsCheckSyntax.Response'
           (Id     => Id,
            Result => Value));
   end On_AlsCheckSyntax_Response;

   overriding procedure On_IncomingCalls_Response
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.CallHierarchyIncomingCall_Vector_Or_Null) is
   begin
      Client_Message_Factory'Class (Self).On_Message
        (new LSP.Client_Responses.IncomingCalls.Response'
           (Id     => Id,
            Result => Value));
   end On_IncomingCalls_Response;

   overriding procedure On_OutgoingCalls_Response
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.CallHierarchyOutgoingCall_Vector_Or_Null) is
   begin
      Client_Message_Factory'Class (Self).On_Message
        (new LSP.Client_Responses.OutgoingCalls.Response'
           (Id     => Id,
            Result => Value));
   end On_OutgoingCalls_Response;

   overriding procedure On_Code_Action_Resolve_Response
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.CodeAction) is
   begin
      Client_Message_Factory'Class (Self).On_Message
        (new LSP.Client_Responses.Code_Action_Resolve.Response'
           (Id     => Id,
            Result => Value));
   end On_Code_Action_Resolve_Response;

   overriding procedure On_Code_Lens_Resolve_Response
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.CodeLens) is
   begin
      Client_Message_Factory'Class (Self).On_Message
        (new LSP.Client_Responses.Code_Lens_Resolve.Response'
           (Id     => Id,
            Result => Value));
   end On_Code_Lens_Resolve_Response;

   overriding procedure On_Completion_Resolve_Response
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.CompletionItem) is
   begin
      Client_Message_Factory'Class (Self).On_Message
        (new LSP.Client_Responses.Completion_Resolve.Response'
           (Id     => Id,
            Result => Value));
   end On_Completion_Resolve_Response;

   overriding procedure On_Link_Resolve_Response
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.DocumentLink) is
   begin
      Client_Message_Factory'Class (Self).On_Message
        (new LSP.Client_Responses.Link_Resolve.Response'
           (Id     => Id,
            Result => Value));
   end On_Link_Resolve_Response;

   overriding procedure On_Initialize_Response
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.InitializeResult) is
   begin
      Client_Message_Factory'Class (Self).On_Message
        (new LSP.Client_Responses.Initialize.Response'
           (Id     => Id,
            Result => Value));
   end On_Initialize_Response;

   overriding procedure On_Inlay_Resolve_Response
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.InlayHint) is
   begin
      Client_Message_Factory'Class (Self).On_Message
        (new LSP.Client_Responses.Inlay_Resolve.Response'
           (Id     => Id,
            Result => Value));
   end On_Inlay_Resolve_Response;

   overriding procedure On_Shutdown_Response
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Null_Record) is
   begin
      Client_Message_Factory'Class (Self).On_Message
        (new LSP.Client_Responses.Shutdown.Response'
           (Id     => Id,
            Result => Value));
   end On_Shutdown_Response;

   overriding procedure On_CodeAction_Response
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Command_Or_CodeAction_Vector_Or_Null) is
   begin
      Client_Message_Factory'Class (Self).On_Message
        (new LSP.Client_Responses.CodeAction.Response'
           (Id     => Id,
            Result => Value));
   end On_CodeAction_Response;

   overriding procedure On_CodeLens_Response
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.CodeLens_Vector_Or_Null) is
   begin
      Client_Message_Factory'Class (Self).On_Message
        (new LSP.Client_Responses.CodeLens.Response'
           (Id     => Id,
            Result => Value));
   end On_CodeLens_Response;

   overriding procedure On_ColorPresentation_Response
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.ColorPresentation_Vector) is
   begin
      Client_Message_Factory'Class (Self).On_Message
        (new LSP.Client_Responses.ColorPresentation.Response'
           (Id     => Id,
            Result => Value));
   end On_ColorPresentation_Response;

   overriding procedure On_Completion_Response
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Completion_Result) is
   begin
      Client_Message_Factory'Class (Self).On_Message
        (new LSP.Client_Responses.Completion.Response'
           (Id     => Id,
            Result => Value));
   end On_Completion_Response;

   overriding procedure On_Declaration_Response
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Declaration_Result) is
   begin
      Client_Message_Factory'Class (Self).On_Message
        (new LSP.Client_Responses.Declaration.Response'
           (Id     => Id,
            Result => Value));
   end On_Declaration_Response;

   overriding procedure On_Definition_Response
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Definition_Result) is
   begin
      Client_Message_Factory'Class (Self).On_Message
        (new LSP.Client_Responses.Definition.Response'
           (Id     => Id,
            Result => Value));
   end On_Definition_Response;

   overriding procedure On_Diagnostic_Response
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.DocumentDiagnosticReport) is
   begin
      Client_Message_Factory'Class (Self).On_Message
        (new LSP.Client_Responses.Diagnostic.Response'
           (Id     => Id,
            Result => Value));
   end On_Diagnostic_Response;

   overriding procedure On_DocumentColor_Response
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.ColorInformation_Vector) is
   begin
      Client_Message_Factory'Class (Self).On_Message
        (new LSP.Client_Responses.DocumentColor.Response'
           (Id     => Id,
            Result => Value));
   end On_DocumentColor_Response;

   overriding procedure On_DocumentHighlight_Response
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.DocumentHighlight_Vector_Or_Null) is
   begin
      Client_Message_Factory'Class (Self).On_Message
        (new LSP.Client_Responses.DocumentHighlight.Response'
           (Id     => Id,
            Result => Value));
   end On_DocumentHighlight_Response;

   overriding procedure On_DocumentLink_Response
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.DocumentLink_Vector_Or_Null) is
   begin
      Client_Message_Factory'Class (Self).On_Message
        (new LSP.Client_Responses.DocumentLink.Response'
           (Id     => Id,
            Result => Value));
   end On_DocumentLink_Response;

   overriding procedure On_DocumentSymbol_Response
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.DocumentSymbol_Result) is
   begin
      Client_Message_Factory'Class (Self).On_Message
        (new LSP.Client_Responses.DocumentSymbol.Response'
           (Id     => Id,
            Result => Value));
   end On_DocumentSymbol_Response;

   overriding procedure On_FoldingRange_Response
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.FoldingRange_Vector_Or_Null) is
   begin
      Client_Message_Factory'Class (Self).On_Message
        (new LSP.Client_Responses.FoldingRange.Response'
           (Id     => Id,
            Result => Value));
   end On_FoldingRange_Response;

   overriding procedure On_Formatting_Response
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.TextEdit_Vector_Or_Null) is
   begin
      Client_Message_Factory'Class (Self).On_Message
        (new LSP.Client_Responses.Formatting.Response'
           (Id     => Id,
            Result => Value));
   end On_Formatting_Response;

   overriding procedure On_Hover_Response
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Hover_Or_Null) is
   begin
      Client_Message_Factory'Class (Self).On_Message
        (new LSP.Client_Responses.Hover.Response'
           (Id     => Id,
            Result => Value));
   end On_Hover_Response;

   overriding procedure On_Implementation_Response
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Definition_Result) is
   begin
      Client_Message_Factory'Class (Self).On_Message
        (new LSP.Client_Responses.Implementation.Response'
           (Id     => Id,
            Result => Value));
   end On_Implementation_Response;

   overriding procedure On_InlayHint_Response
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.InlayHint_Vector_Or_Null) is
   begin
      Client_Message_Factory'Class (Self).On_Message
        (new LSP.Client_Responses.InlayHint.Response'
           (Id     => Id,
            Result => Value));
   end On_InlayHint_Response;

   overriding procedure On_InlineValue_Response
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.InlineValue_Vector_Or_Null) is
   begin
      Client_Message_Factory'Class (Self).On_Message
        (new LSP.Client_Responses.InlineValue.Response'
           (Id     => Id,
            Result => Value));
   end On_InlineValue_Response;

   overriding procedure On_LinkedEditingRange_Response
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.LinkedEditingRanges_Or_Null) is
   begin
      Client_Message_Factory'Class (Self).On_Message
        (new LSP.Client_Responses.LinkedEditingRange.Response'
           (Id     => Id,
            Result => Value));
   end On_LinkedEditingRange_Response;

   overriding procedure On_Moniker_Response
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Moniker_Vector_Or_Null) is
   begin
      Client_Message_Factory'Class (Self).On_Message
        (new LSP.Client_Responses.Moniker.Response'
           (Id     => Id,
            Result => Value));
   end On_Moniker_Response;

   overriding procedure On_OnTypeFormatting_Response
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.TextEdit_Vector_Or_Null) is
   begin
      Client_Message_Factory'Class (Self).On_Message
        (new LSP.Client_Responses.OnTypeFormatting.Response'
           (Id     => Id,
            Result => Value));
   end On_OnTypeFormatting_Response;

   overriding procedure On_PrepareCallHierarchy_Response
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.CallHierarchyItem_Vector_Or_Null) is
   begin
      Client_Message_Factory'Class (Self).On_Message
        (new LSP.Client_Responses.PrepareCallHierarchy.Response'
           (Id     => Id,
            Result => Value));
   end On_PrepareCallHierarchy_Response;

   overriding procedure On_PrepareRename_Response
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.PrepareRenameResult_Or_Null) is
   begin
      Client_Message_Factory'Class (Self).On_Message
        (new LSP.Client_Responses.PrepareRename.Response'
           (Id     => Id,
            Result => Value));
   end On_PrepareRename_Response;

   overriding procedure On_PrepareTypeHierarchy_Response
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.TypeHierarchyItem_Vector_Or_Null) is
   begin
      Client_Message_Factory'Class (Self).On_Message
        (new LSP.Client_Responses.PrepareTypeHierarchy.Response'
           (Id     => Id,
            Result => Value));
   end On_PrepareTypeHierarchy_Response;

   overriding procedure On_RangeFormatting_Response
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.TextEdit_Vector_Or_Null) is
   begin
      Client_Message_Factory'Class (Self).On_Message
        (new LSP.Client_Responses.RangeFormatting.Response'
           (Id     => Id,
            Result => Value));
   end On_RangeFormatting_Response;

   overriding procedure On_References_Response
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Location_Vector_Or_Null) is
   begin
      Client_Message_Factory'Class (Self).On_Message
        (new LSP.Client_Responses.References.Response'
           (Id     => Id,
            Result => Value));
   end On_References_Response;

   overriding procedure On_Rename_Response
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.WorkspaceEdit_Or_Null) is
   begin
      Client_Message_Factory'Class (Self).On_Message
        (new LSP.Client_Responses.Rename.Response'
           (Id     => Id,
            Result => Value));
   end On_Rename_Response;

   overriding procedure On_SelectionRange_Response
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.SelectionRange_Vector_Or_Null) is
   begin
      Client_Message_Factory'Class (Self).On_Message
        (new LSP.Client_Responses.SelectionRange.Response'
           (Id     => Id,
            Result => Value));
   end On_SelectionRange_Response;

   overriding procedure On_Tokens_Full_Response
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.SemanticTokens_Or_Null) is
   begin
      Client_Message_Factory'Class (Self).On_Message
        (new LSP.Client_Responses.Tokens_Full.Response'
           (Id     => Id,
            Result => Value));
   end On_Tokens_Full_Response;

   overriding procedure On_Tokens_Delta_Response
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Tokens_Delta_Result) is
   begin
      Client_Message_Factory'Class (Self).On_Message
        (new LSP.Client_Responses.Tokens_Delta.Response'
           (Id     => Id,
            Result => Value));
   end On_Tokens_Delta_Response;

   overriding procedure On_Tokens_Range_Response
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.SemanticTokens_Or_Null) is
   begin
      Client_Message_Factory'Class (Self).On_Message
        (new LSP.Client_Responses.Tokens_Range.Response'
           (Id     => Id,
            Result => Value));
   end On_Tokens_Range_Response;

   overriding procedure On_SignatureHelp_Response
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.SignatureHelp_Or_Null) is
   begin
      Client_Message_Factory'Class (Self).On_Message
        (new LSP.Client_Responses.SignatureHelp.Response'
           (Id     => Id,
            Result => Value));
   end On_SignatureHelp_Response;

   overriding procedure On_TypeDefinition_Response
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Definition_Result) is
   begin
      Client_Message_Factory'Class (Self).On_Message
        (new LSP.Client_Responses.TypeDefinition.Response'
           (Id     => Id,
            Result => Value));
   end On_TypeDefinition_Response;

   overriding procedure On_WillSaveWaitUntil_Response
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.TextEdit_Vector_Or_Null) is
   begin
      Client_Message_Factory'Class (Self).On_Message
        (new LSP.Client_Responses.WillSaveWaitUntil.Response'
           (Id     => Id,
            Result => Value));
   end On_WillSaveWaitUntil_Response;

   overriding procedure On_Subtypes_Response
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.TypeHierarchyItem_Vector_Or_Null) is
   begin
      Client_Message_Factory'Class (Self).On_Message
        (new LSP.Client_Responses.Subtypes.Response'
           (Id     => Id,
            Result => Value));
   end On_Subtypes_Response;

   overriding procedure On_Supertypes_Response
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.TypeHierarchyItem_Vector_Or_Null) is
   begin
      Client_Message_Factory'Class (Self).On_Message
        (new LSP.Client_Responses.Supertypes.Response'
           (Id     => Id,
            Result => Value));
   end On_Supertypes_Response;

   overriding procedure On_Workspace_Diagnostic_Response
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.WorkspaceDiagnosticReport) is
   begin
      Client_Message_Factory'Class (Self).On_Message
        (new LSP.Client_Responses.Workspace_Diagnostic.Response'
           (Id     => Id,
            Result => Value));
   end On_Workspace_Diagnostic_Response;

   overriding procedure On_ExecuteCommand_Response
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.LSPAny_Or_Null) is
   begin
      Client_Message_Factory'Class (Self).On_Message
        (new LSP.Client_Responses.ExecuteCommand.Response'
           (Id     => Id,
            Result => Value));
   end On_ExecuteCommand_Response;

   overriding procedure On_Symbol_Response
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.Symbol_Result) is
   begin
      Client_Message_Factory'Class (Self).On_Message
        (new LSP.Client_Responses.Symbol.Response'
           (Id     => Id,
            Result => Value));
   end On_Symbol_Response;

   overriding procedure On_WillCreateFiles_Response
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.WorkspaceEdit_Or_Null) is
   begin
      Client_Message_Factory'Class (Self).On_Message
        (new LSP.Client_Responses.WillCreateFiles.Response'
           (Id     => Id,
            Result => Value));
   end On_WillCreateFiles_Response;

   overriding procedure On_WillDeleteFiles_Response
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.WorkspaceEdit_Or_Null) is
   begin
      Client_Message_Factory'Class (Self).On_Message
        (new LSP.Client_Responses.WillDeleteFiles.Response'
           (Id     => Id,
            Result => Value));
   end On_WillDeleteFiles_Response;

   overriding procedure On_WillRenameFiles_Response
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.WorkspaceEdit_Or_Null) is
   begin
      Client_Message_Factory'Class (Self).On_Message
        (new LSP.Client_Responses.WillRenameFiles.Response'
           (Id     => Id,
            Result => Value));
   end On_WillRenameFiles_Response;

   overriding procedure On_Symbol_Resolve_Response
     (Self  : in out Client_Message_Factory;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.WorkspaceSymbol) is
   begin
      Client_Message_Factory'Class (Self).On_Message
        (new LSP.Client_Responses.Symbol_Resolve.Response'
           (Id     => Id,
            Result => Value));
   end On_Symbol_Resolve_Response;

   overriding procedure On_IncomingCalls_Partial_Result
     (Self  : in out Client_Message_Factory;
      Token : LSP.Structures.ProgressToken;
      Value : LSP.Structures.CallHierarchyIncomingCall_Vector) is
   begin
      Client_Message_Factory'Class (Self).On_Message
        (new LSP.Progress_Reports.IncomingCalls.Partial_Result'
           (Token  => Token,
            Params => Value));
   end On_IncomingCalls_Partial_Result;

   overriding procedure On_OutgoingCalls_Partial_Result
     (Self  : in out Client_Message_Factory;
      Token : LSP.Structures.ProgressToken;
      Value : LSP.Structures.CallHierarchyOutgoingCall_Vector) is
   begin
      Client_Message_Factory'Class (Self).On_Message
        (new LSP.Progress_Reports.OutgoingCalls.Partial_Result'
           (Token  => Token,
            Params => Value));
   end On_OutgoingCalls_Partial_Result;

   overriding procedure On_CodeAction_Partial_Result
     (Self  : in out Client_Message_Factory;
      Token : LSP.Structures.ProgressToken;
      Value : LSP.Structures.Command_Or_CodeAction_Vector) is
   begin
      Client_Message_Factory'Class (Self).On_Message
        (new LSP.Progress_Reports.CodeAction.Partial_Result'
           (Token  => Token,
            Params => Value));
   end On_CodeAction_Partial_Result;

   overriding procedure On_CodeLens_Partial_Result
     (Self  : in out Client_Message_Factory;
      Token : LSP.Structures.ProgressToken;
      Value : LSP.Structures.CodeLens_Vector) is
   begin
      Client_Message_Factory'Class (Self).On_Message
        (new LSP.Progress_Reports.CodeLens.Partial_Result'
           (Token  => Token,
            Params => Value));
   end On_CodeLens_Partial_Result;

   overriding procedure On_ColorPresentation_Partial_Result
     (Self  : in out Client_Message_Factory;
      Token : LSP.Structures.ProgressToken;
      Value : LSP.Structures.ColorPresentation_Vector) is
   begin
      Client_Message_Factory'Class (Self).On_Message
        (new LSP.Progress_Reports.ColorPresentation.Partial_Result'
           (Token  => Token,
            Params => Value));
   end On_ColorPresentation_Partial_Result;

   overriding procedure On_Completion_Partial_Result
     (Self  : in out Client_Message_Factory;
      Token : LSP.Structures.ProgressToken;
      Value : LSP.Structures.CompletionItem_Vector) is
   begin
      Client_Message_Factory'Class (Self).On_Message
        (new LSP.Progress_Reports.Completion.Partial_Result'
           (Token  => Token,
            Params => Value));
   end On_Completion_Partial_Result;

   overriding procedure On_Declaration_Partial_Result
     (Self  : in out Client_Message_Factory;
      Token : LSP.Structures.ProgressToken;
      Value : LSP.Structures.Declaration_Progress_Report) is
   begin
      Client_Message_Factory'Class (Self).On_Message
        (new LSP.Progress_Reports.Declaration.Partial_Result'
           (Token  => Token,
            Params => Value));
   end On_Declaration_Partial_Result;

   overriding procedure On_Definition_Partial_Result
     (Self  : in out Client_Message_Factory;
      Token : LSP.Structures.ProgressToken;
      Value : LSP.Structures.Definition_Progress_Report) is
   begin
      Client_Message_Factory'Class (Self).On_Message
        (new LSP.Progress_Reports.Definition.Partial_Result'
           (Token  => Token,
            Params => Value));
   end On_Definition_Partial_Result;

   overriding procedure On_Diagnostic_Partial_Result
     (Self  : in out Client_Message_Factory;
      Token : LSP.Structures.ProgressToken;
      Value : LSP.Structures.DocumentDiagnosticReportPartialResult) is
   begin
      Client_Message_Factory'Class (Self).On_Message
        (new LSP.Progress_Reports.Diagnostic.Partial_Result'
           (Token  => Token,
            Params => Value));
   end On_Diagnostic_Partial_Result;

   overriding procedure On_DocumentColor_Partial_Result
     (Self  : in out Client_Message_Factory;
      Token : LSP.Structures.ProgressToken;
      Value : LSP.Structures.ColorInformation_Vector) is
   begin
      Client_Message_Factory'Class (Self).On_Message
        (new LSP.Progress_Reports.DocumentColor.Partial_Result'
           (Token  => Token,
            Params => Value));
   end On_DocumentColor_Partial_Result;

   overriding procedure On_DocumentHighlight_Partial_Result
     (Self  : in out Client_Message_Factory;
      Token : LSP.Structures.ProgressToken;
      Value : LSP.Structures.DocumentHighlight_Vector) is
   begin
      Client_Message_Factory'Class (Self).On_Message
        (new LSP.Progress_Reports.DocumentHighlight.Partial_Result'
           (Token  => Token,
            Params => Value));
   end On_DocumentHighlight_Partial_Result;

   overriding procedure On_DocumentLink_Partial_Result
     (Self  : in out Client_Message_Factory;
      Token : LSP.Structures.ProgressToken;
      Value : LSP.Structures.DocumentLink_Vector) is
   begin
      Client_Message_Factory'Class (Self).On_Message
        (new LSP.Progress_Reports.DocumentLink.Partial_Result'
           (Token  => Token,
            Params => Value));
   end On_DocumentLink_Partial_Result;

   overriding procedure On_DocumentSymbol_Partial_Result
     (Self  : in out Client_Message_Factory;
      Token : LSP.Structures.ProgressToken;
      Value : LSP.Structures.DocumentSymbol_Progress_Report) is
   begin
      Client_Message_Factory'Class (Self).On_Message
        (new LSP.Progress_Reports.DocumentSymbol.Partial_Result'
           (Token  => Token,
            Params => Value));
   end On_DocumentSymbol_Partial_Result;

   overriding procedure On_FoldingRange_Partial_Result
     (Self  : in out Client_Message_Factory;
      Token : LSP.Structures.ProgressToken;
      Value : LSP.Structures.FoldingRange_Vector) is
   begin
      Client_Message_Factory'Class (Self).On_Message
        (new LSP.Progress_Reports.FoldingRange.Partial_Result'
           (Token  => Token,
            Params => Value));
   end On_FoldingRange_Partial_Result;

   overriding procedure On_Implementation_Partial_Result
     (Self  : in out Client_Message_Factory;
      Token : LSP.Structures.ProgressToken;
      Value : LSP.Structures.Definition_Progress_Report) is
   begin
      Client_Message_Factory'Class (Self).On_Message
        (new LSP.Progress_Reports.Implementation.Partial_Result'
           (Token  => Token,
            Params => Value));
   end On_Implementation_Partial_Result;

   overriding procedure On_InlayHint_Partial_Result
     (Self  : in out Client_Message_Factory;
      Token : LSP.Structures.ProgressToken;
      Value : LSP.Structures.InlayHint_Vector) is
   begin
      Client_Message_Factory'Class (Self).On_Message
        (new LSP.Progress_Reports.InlayHint.Partial_Result'
           (Token  => Token,
            Params => Value));
   end On_InlayHint_Partial_Result;

   overriding procedure On_InlineValue_Partial_Result
     (Self  : in out Client_Message_Factory;
      Token : LSP.Structures.ProgressToken;
      Value : LSP.Structures.InlineValue_Vector) is
   begin
      Client_Message_Factory'Class (Self).On_Message
        (new LSP.Progress_Reports.InlineValue.Partial_Result'
           (Token  => Token,
            Params => Value));
   end On_InlineValue_Partial_Result;

   overriding procedure On_Moniker_Partial_Result
     (Self  : in out Client_Message_Factory;
      Token : LSP.Structures.ProgressToken;
      Value : LSP.Structures.Moniker_Vector) is
   begin
      Client_Message_Factory'Class (Self).On_Message
        (new LSP.Progress_Reports.Moniker.Partial_Result'
           (Token  => Token,
            Params => Value));
   end On_Moniker_Partial_Result;

   overriding procedure On_References_Partial_Result
     (Self  : in out Client_Message_Factory;
      Token : LSP.Structures.ProgressToken;
      Value : LSP.Structures.Location_Vector) is
   begin
      Client_Message_Factory'Class (Self).On_Message
        (new LSP.Progress_Reports.References.Partial_Result'
           (Token  => Token,
            Params => Value));
   end On_References_Partial_Result;

   overriding procedure On_SelectionRange_Partial_Result
     (Self  : in out Client_Message_Factory;
      Token : LSP.Structures.ProgressToken;
      Value : LSP.Structures.SelectionRange_Vector) is
   begin
      Client_Message_Factory'Class (Self).On_Message
        (new LSP.Progress_Reports.SelectionRange.Partial_Result'
           (Token  => Token,
            Params => Value));
   end On_SelectionRange_Partial_Result;

   overriding procedure On_Tokens_Full_Partial_Result
     (Self  : in out Client_Message_Factory;
      Token : LSP.Structures.ProgressToken;
      Value : LSP.Structures.SemanticTokensPartialResult) is
   begin
      Client_Message_Factory'Class (Self).On_Message
        (new LSP.Progress_Reports.Tokens_Full.Partial_Result'
           (Token  => Token,
            Params => Value));
   end On_Tokens_Full_Partial_Result;

   overriding procedure On_Tokens_Delta_Partial_Result
     (Self  : in out Client_Message_Factory;
      Token : LSP.Structures.ProgressToken;
      Value : LSP.Structures
        .SemanticTokensPartialResult_Or_SemanticTokensDeltaPartialResult) is
   begin
      Client_Message_Factory'Class (Self).On_Message
        (new LSP.Progress_Reports.Tokens_Delta.Partial_Result'
           (Token  => Token,
            Params => Value));
   end On_Tokens_Delta_Partial_Result;

   overriding procedure On_Tokens_Range_Partial_Result
     (Self  : in out Client_Message_Factory;
      Token : LSP.Structures.ProgressToken;
      Value : LSP.Structures.SemanticTokensPartialResult) is
   begin
      Client_Message_Factory'Class (Self).On_Message
        (new LSP.Progress_Reports.Tokens_Range.Partial_Result'
           (Token  => Token,
            Params => Value));
   end On_Tokens_Range_Partial_Result;

   overriding procedure On_TypeDefinition_Partial_Result
     (Self  : in out Client_Message_Factory;
      Token : LSP.Structures.ProgressToken;
      Value : LSP.Structures.Definition_Progress_Report) is
   begin
      Client_Message_Factory'Class (Self).On_Message
        (new LSP.Progress_Reports.TypeDefinition.Partial_Result'
           (Token  => Token,
            Params => Value));
   end On_TypeDefinition_Partial_Result;

   overriding procedure On_Subtypes_Partial_Result
     (Self  : in out Client_Message_Factory;
      Token : LSP.Structures.ProgressToken;
      Value : LSP.Structures.TypeHierarchyItem_Vector) is
   begin
      Client_Message_Factory'Class (Self).On_Message
        (new LSP.Progress_Reports.Subtypes.Partial_Result'
           (Token  => Token,
            Params => Value));
   end On_Subtypes_Partial_Result;

   overriding procedure On_Supertypes_Partial_Result
     (Self  : in out Client_Message_Factory;
      Token : LSP.Structures.ProgressToken;
      Value : LSP.Structures.TypeHierarchyItem_Vector) is
   begin
      Client_Message_Factory'Class (Self).On_Message
        (new LSP.Progress_Reports.Supertypes.Partial_Result'
           (Token  => Token,
            Params => Value));
   end On_Supertypes_Partial_Result;

   overriding procedure On_Workspace_Diagnostic_Partial_Result
     (Self  : in out Client_Message_Factory;
      Token : LSP.Structures.ProgressToken;
      Value : LSP.Structures.WorkspaceDiagnosticReportPartialResult) is
   begin
      Client_Message_Factory'Class (Self).On_Message
        (new LSP.Progress_Reports.Workspace_Diagnostic.Partial_Result'
           (Token  => Token,
            Params => Value));
   end On_Workspace_Diagnostic_Partial_Result;

   overriding procedure On_Symbol_Partial_Result
     (Self  : in out Client_Message_Factory;
      Token : LSP.Structures.ProgressToken;
      Value : LSP.Structures.Symbol_Progress_Report) is
   begin
      Client_Message_Factory'Class (Self).On_Message
        (new LSP.Progress_Reports.Symbol.Partial_Result'
           (Token  => Token,
            Params => Value));
   end On_Symbol_Partial_Result;

   overriding procedure On_ProgressBegin_Work_Done
     (Self  : in out Client_Message_Factory;
      Token : LSP.Structures.ProgressToken;
      Value : LSP.Structures.WorkDoneProgressBegin) is
   begin
      Client_Message_Factory'Class (Self).On_Message
        (new LSP.Progress_Reports.ProgressBegin.Work_Done'
           (Token  => Token,
            Params => Value));
   end On_ProgressBegin_Work_Done;

   overriding procedure On_ProgressReport_Work_Done
     (Self  : in out Client_Message_Factory;
      Token : LSP.Structures.ProgressToken;
      Value : LSP.Structures.WorkDoneProgressReport) is
   begin
      Client_Message_Factory'Class (Self).On_Message
        (new LSP.Progress_Reports.ProgressReport.Work_Done'
           (Token  => Token,
            Params => Value));
   end On_ProgressReport_Work_Done;

   overriding procedure On_ProgressEnd_Work_Done
     (Self  : in out Client_Message_Factory;
      Token : LSP.Structures.ProgressToken;
      Value : LSP.Structures.WorkDoneProgressEnd) is
   begin
      Client_Message_Factory'Class (Self).On_Message
        (new LSP.Progress_Reports.ProgressEnd.Work_Done'
           (Token  => Token,
            Params => Value));
   end On_ProgressEnd_Work_Done;

end LSP.Client_Message_Factories;
