------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                        Copyright (C) 2024, AdaCore                       --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

with Ada.Tags;

with LSP.Server_Notifications.Cancel;
with LSP.Server_Notifications.CancelRequest;
with LSP.Server_Notifications.DidChange;
with LSP.Server_Notifications.DidChangeConfiguration;
with LSP.Server_Notifications.DidChangeNotebook;
with LSP.Server_Notifications.DidChangeWatchedFiles;
with LSP.Server_Notifications.DidChangeWorkspaceFolders;
with LSP.Server_Notifications.DidClose;
with LSP.Server_Notifications.DidCloseNotebook;
with LSP.Server_Notifications.DidCreateFiles;
with LSP.Server_Notifications.DidDeleteFiles;
with LSP.Server_Notifications.DidOpen;
with LSP.Server_Notifications.DidOpenNotebook;
with LSP.Server_Notifications.DidRenameFiles;
with LSP.Server_Notifications.DidSave;
with LSP.Server_Notifications.DidSaveNotebook;
with LSP.Server_Notifications.Exits;
with LSP.Server_Notifications.Initialized;
with LSP.Server_Notifications.Progress;
with LSP.Server_Notifications.SetTrace;
with LSP.Server_Notifications.WillSave;
with LSP.Server_Requests.AlsCheckSyntax;
with LSP.Server_Requests.Code_Action_Resolve;
with LSP.Server_Requests.Code_Lens_Resolve;
with LSP.Server_Requests.CodeAction;
with LSP.Server_Requests.CodeLens;
with LSP.Server_Requests.ColorPresentation;
with LSP.Server_Requests.Completion;
with LSP.Server_Requests.Completion_Resolve;
with LSP.Server_Requests.Declaration;
with LSP.Server_Requests.Definition;
with LSP.Server_Requests.Diagnostic;
with LSP.Server_Requests.DocumentColor;
with LSP.Server_Requests.DocumentHighlight;
with LSP.Server_Requests.DocumentLink;
with LSP.Server_Requests.DocumentSymbol;
with LSP.Server_Requests.ExecuteCommand;
with LSP.Server_Requests.FoldingRange;
with LSP.Server_Requests.Formatting;
with LSP.Server_Requests.Hover;
with LSP.Server_Requests.Implementation;
with LSP.Server_Requests.IncomingCalls;
with LSP.Server_Requests.Initialize;
with LSP.Server_Requests.Inlay_Resolve;
with LSP.Server_Requests.InlayHint;
with LSP.Server_Requests.InlineValue;
with LSP.Server_Requests.Link_Resolve;
with LSP.Server_Requests.LinkedEditingRange;
with LSP.Server_Requests.Moniker;
with LSP.Server_Requests.OnTypeFormatting;
with LSP.Server_Requests.OutgoingCalls;
with LSP.Server_Requests.PrepareCallHierarchy;
with LSP.Server_Requests.PrepareRename;
with LSP.Server_Requests.PrepareTypeHierarchy;
with LSP.Server_Requests.RangeFormatting;
with LSP.Server_Requests.References;
with LSP.Server_Requests.Rename;
with LSP.Server_Requests.SelectionRange;
with LSP.Server_Requests.Shutdown;
with LSP.Server_Requests.SignatureHelp;
with LSP.Server_Requests.Subtypes;
with LSP.Server_Requests.Supertypes;
with LSP.Server_Requests.Symbol;
with LSP.Server_Requests.Symbol_Resolve;
with LSP.Server_Requests.Tokens_Delta;
with LSP.Server_Requests.Tokens_Full;
with LSP.Server_Requests.Tokens_Range;
with LSP.Server_Requests.TypeDefinition;
with LSP.Server_Requests.WillCreateFiles;
with LSP.Server_Requests.WillDeleteFiles;
with LSP.Server_Requests.WillRenameFiles;
with LSP.Server_Requests.WillSaveWaitUntil;
with LSP.Server_Requests.Workspace_Diagnostic;
with LSP.Server_Responses.ApplyEdit;
with LSP.Server_Responses.Code_Lens_Refresh;
with LSP.Server_Responses.Configuration;
with LSP.Server_Responses.Diagnostic_Refresh;
with LSP.Server_Responses.Inlay_Refresh;
with LSP.Server_Responses.Inline_Refresh;
with LSP.Server_Responses.Progress_Create;
with LSP.Server_Responses.RegisterCapability;
with LSP.Server_Responses.ShowDocument;
with LSP.Server_Responses.ShowMessageRequest;
with LSP.Server_Responses.Tokens_Refresh;
with LSP.Server_Responses.UnregisterCapability;
with LSP.Server_Responses.WorkspaceFolders;

with LSP.Ada_Indexing;  --  FIX ME: avoid dependency on Ada server

procedure LSP.Register_Fallbacks
  (Self    : in out LSP.Job_Schedulers.Job_Scheduler'Class;
   Handler : LSP.Server_Message_Handlers.Server_Message_Handler_Access)
is
   procedure Add (Tag : Ada.Tags.Tag);

   ---------
   -- Add --
   ---------

   procedure Add (Tag : Ada.Tags.Tag) is
   begin
      Self.Register_Handler (Tag, Handler);
   end Add;

begin
   Add (LSP.Server_Notifications.Cancel.Notification'Tag);
   Add (LSP.Server_Notifications.CancelRequest.Notification'Tag);
   Add (LSP.Server_Notifications.DidChange.Notification'Tag);
   Add (LSP.Server_Notifications.DidChangeConfiguration.Notification'Tag);
   Add (LSP.Server_Notifications.DidChangeNotebook.Notification'Tag);
   Add (LSP.Server_Notifications.DidChangeWatchedFiles.Notification'Tag);
   Add (LSP.Server_Notifications.DidChangeWorkspaceFolders.Notification'Tag);
   Add (LSP.Server_Notifications.DidClose.Notification'Tag);
   Add (LSP.Server_Notifications.DidCloseNotebook.Notification'Tag);
   Add (LSP.Server_Notifications.DidCreateFiles.Notification'Tag);
   Add (LSP.Server_Notifications.DidDeleteFiles.Notification'Tag);
   Add (LSP.Server_Notifications.DidOpen.Notification'Tag);
   Add (LSP.Server_Notifications.DidOpenNotebook.Notification'Tag);
   Add (LSP.Server_Notifications.DidRenameFiles.Notification'Tag);
   Add (LSP.Server_Notifications.DidSave.Notification'Tag);
   Add (LSP.Server_Notifications.DidSaveNotebook.Notification'Tag);
   Add (LSP.Server_Notifications.Exits.Notification'Tag);
   Add (LSP.Server_Notifications.Initialized.Notification'Tag);
   Add (LSP.Server_Notifications.Progress.Notification'Tag);
   Add (LSP.Server_Notifications.SetTrace.Notification'Tag);
   Add (LSP.Server_Notifications.WillSave.Notification'Tag);
   Add (LSP.Server_Requests.AlsCheckSyntax.Request'Tag);
   Add (LSP.Server_Requests.Code_Action_Resolve.Request'Tag);
   Add (LSP.Server_Requests.Code_Lens_Resolve.Request'Tag);
   Add (LSP.Server_Requests.CodeAction.Request'Tag);
   Add (LSP.Server_Requests.CodeLens.Request'Tag);
   Add (LSP.Server_Requests.ColorPresentation.Request'Tag);
   Add (LSP.Server_Requests.Completion.Request'Tag);
   Add (LSP.Server_Requests.Completion_Resolve.Request'Tag);
   Add (LSP.Server_Requests.Declaration.Request'Tag);
   Add (LSP.Server_Requests.Definition.Request'Tag);
   Add (LSP.Server_Requests.Diagnostic.Request'Tag);
   Add (LSP.Server_Requests.DocumentColor.Request'Tag);
   Add (LSP.Server_Requests.DocumentHighlight.Request'Tag);
   Add (LSP.Server_Requests.DocumentLink.Request'Tag);
   Add (LSP.Server_Requests.DocumentSymbol.Request'Tag);
   Add (LSP.Server_Requests.ExecuteCommand.Request'Tag);
   Add (LSP.Server_Requests.FoldingRange.Request'Tag);
   Add (LSP.Server_Requests.Formatting.Request'Tag);
   Add (LSP.Server_Requests.Hover.Request'Tag);
   Add (LSP.Server_Requests.Implementation.Request'Tag);
   Add (LSP.Server_Requests.IncomingCalls.Request'Tag);
   Add (LSP.Server_Requests.Initialize.Request'Tag);
   Add (LSP.Server_Requests.Inlay_Resolve.Request'Tag);
   Add (LSP.Server_Requests.InlayHint.Request'Tag);
   Add (LSP.Server_Requests.InlineValue.Request'Tag);
   Add (LSP.Server_Requests.Link_Resolve.Request'Tag);
   Add (LSP.Server_Requests.LinkedEditingRange.Request'Tag);
   Add (LSP.Server_Requests.Moniker.Request'Tag);
   Add (LSP.Server_Requests.OnTypeFormatting.Request'Tag);
   Add (LSP.Server_Requests.OutgoingCalls.Request'Tag);
   Add (LSP.Server_Requests.PrepareCallHierarchy.Request'Tag);
   Add (LSP.Server_Requests.PrepareRename.Request'Tag);
   Add (LSP.Server_Requests.PrepareTypeHierarchy.Request'Tag);
   Add (LSP.Server_Requests.RangeFormatting.Request'Tag);
   Add (LSP.Server_Requests.References.Request'Tag);
   Add (LSP.Server_Requests.Rename.Request'Tag);
   Add (LSP.Server_Requests.SelectionRange.Request'Tag);
   Add (LSP.Server_Requests.Shutdown.Request'Tag);
   Add (LSP.Server_Requests.SignatureHelp.Request'Tag);
   Add (LSP.Server_Requests.Subtypes.Request'Tag);
   Add (LSP.Server_Requests.Supertypes.Request'Tag);
   Add (LSP.Server_Requests.Symbol.Request'Tag);
   Add (LSP.Server_Requests.Symbol_Resolve.Request'Tag);
   Add (LSP.Server_Requests.Tokens_Delta.Request'Tag);
   Add (LSP.Server_Requests.Tokens_Full.Request'Tag);
   Add (LSP.Server_Requests.Tokens_Range.Request'Tag);
   Add (LSP.Server_Requests.TypeDefinition.Request'Tag);
   Add (LSP.Server_Requests.WillCreateFiles.Request'Tag);
   Add (LSP.Server_Requests.WillDeleteFiles.Request'Tag);
   Add (LSP.Server_Requests.WillRenameFiles.Request'Tag);
   Add (LSP.Server_Requests.WillSaveWaitUntil.Request'Tag);
   Add (LSP.Server_Requests.Workspace_Diagnostic.Request'Tag);
   Add (LSP.Server_Responses.ApplyEdit.Response'Tag);
   Add (LSP.Server_Responses.Code_Lens_Refresh.Response'Tag);
   Add (LSP.Server_Responses.Configuration.Response'Tag);
   Add (LSP.Server_Responses.Diagnostic_Refresh.Response'Tag);
   Add (LSP.Server_Responses.Inlay_Refresh.Response'Tag);
   Add (LSP.Server_Responses.Inline_Refresh.Response'Tag);
   Add (LSP.Server_Responses.Progress_Create.Response'Tag);
   Add (LSP.Server_Responses.RegisterCapability.Response'Tag);
   Add (LSP.Server_Responses.ShowDocument.Response'Tag);
   Add (LSP.Server_Responses.ShowMessageRequest.Response'Tag);
   Add (LSP.Server_Responses.Tokens_Refresh.Response'Tag);
   Add (LSP.Server_Responses.UnregisterCapability.Response'Tag);
   Add (LSP.Server_Responses.WorkspaceFolders.Response'Tag);

   Add (LSP.Ada_Indexing.Indexing_Job'Tag);
end LSP.Register_Fallbacks;
