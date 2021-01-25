------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2018-2020, AdaCore                     --
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

with Ada.Tags;  use Ada.Tags;
with Ada.Tags.Generic_Dispatching_Constructor;
with Ada.Strings.Wide_Unbounded;

with Interfaces;

with VSS.JSON.Streams.Readers;
with VSS.Strings.Conversions;
with VSS.JSON.Streams.Readers.Look_Ahead;

with LSP.JSON_Streams;
with LSP.Message_IO;

package body LSP.Messages is

   function Create_Command is new Ada.Tags.Generic_Dispatching_Constructor
     (T           => LSP.Commands.Command,
      Parameters  => LSP.JSON_Streams.JSON_Stream'Class,
      Constructor => LSP.Commands.Create);

   procedure Read_Tag
     (JS  : in out LSP.JSON_Streams.JSON_Stream'Class;
      Tag : out Ada.Tags.Tag);
   --  Read `Command` LSP object from JS stream, look for `command` Tag
   --  and find corresponding Ada.Tags.Tag.

   procedure Read_RequestMessage
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.RequestMessage)
      renames LSP.Message_IO.Read_RequestMessage;

   procedure Read_NotificationMessage
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.NotificationMessage)
      renames LSP.Message_IO.Read_NotificationMessage;

   procedure Read_CallHierarchyIncomingCall
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CallHierarchyIncomingCall)
      renames LSP.Message_IO.Read_CallHierarchyIncomingCall;

   procedure Read_CallHierarchyIncomingCallsParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CallHierarchyIncomingCallsParams)
      renames LSP.Message_IO.Read_CallHierarchyIncomingCallsParams;

   procedure Read_CallHierarchyItem
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CallHierarchyItem)
      renames LSP.Message_IO.Read_CallHierarchyItem;

   procedure Read_CallHierarchyOutgoingCall
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CallHierarchyOutgoingCall)
      renames LSP.Message_IO.Read_CallHierarchyOutgoingCall;

   procedure Read_CancelParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.CancelParams)
      renames LSP.Message_IO.Read_CancelParams;

   procedure Read_Position
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.Position)
      renames LSP.Message_IO.Read_Position;

   procedure Read_Span
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.Span)
      renames LSP.Message_IO.Read_Span;

   procedure Read_CodeActionKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.CodeActionKind)
      renames LSP.Message_IO.Read_CodeActionKind;

   procedure Read_AlsReferenceKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.AlsReferenceKind)
      renames LSP.Message_IO.Read_AlsReferenceKind;

   procedure Read_AlsDisplayMethodAncestryOnNavigationPolicy
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.AlsDisplayMethodAncestryOnNavigationPolicy)
      renames LSP.Message_IO.Read_AlsDisplayMethodAncestryOnNavigationPolicy;

   procedure Read_Location
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.Location)
      renames LSP.Message_IO.Read_Location;

   procedure Read_LocationLink
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.LocationLink)
      renames LSP.Message_IO.Read_LocationLink;

   procedure Read_DiagnosticSeverity
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.DiagnosticSeverity)
      renames LSP.Message_IO.Read_DiagnosticSeverity;

   procedure Read_DiagnosticTag
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.DiagnosticTag)
      renames LSP.Message_IO.Read_DiagnosticTag;

   procedure Read_DiagnosticRelatedInformation
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.DiagnosticRelatedInformation)
      renames LSP.Message_IO.Read_DiagnosticRelatedInformation;

   procedure Read_Diagnostic
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.Diagnostic)
      renames LSP.Message_IO.Read_Diagnostic;

   procedure Read_TextEdit
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.TextEdit)
      renames LSP.Message_IO.Read_TextEdit;

   procedure Read_TextDocumentIdentifier
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.TextDocumentIdentifier)
      renames LSP.Message_IO.Read_TextDocumentIdentifier;

   procedure Read_TextDocumentEdit
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.TextDocumentEdit)
      renames LSP.Message_IO.Read_TextDocumentEdit;

   procedure Read_TextDocumentItem
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.TextDocumentItem)
      renames LSP.Message_IO.Read_TextDocumentItem;

   procedure Read_TextDocumentPositionParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.TextDocumentPositionParams)
      renames LSP.Message_IO.Read_TextDocumentPositionParams;

   procedure Read_dynamicRegistration
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.dynamicRegistration)
      renames LSP.Message_IO.Read_dynamicRegistration;

   procedure Read_ResourceOperationKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.ResourceOperationKind)
      renames LSP.Message_IO.Read_ResourceOperationKind;

   procedure Read_FailureHandlingKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.FailureHandlingKind)
      renames LSP.Message_IO.Read_FailureHandlingKind;

   procedure Read_WorkspaceEditClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.WorkspaceEditClientCapabilities)
      renames LSP.Message_IO.Read_WorkspaceEditClientCapabilities;

   procedure Read_SymbolKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.SymbolKind)
      renames LSP.Message_IO.Read_SymbolKind;

   procedure Read_symbolKindCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.symbolKindCapabilities)
      renames LSP.Message_IO.Read_symbolKindCapabilities;

   procedure Read_Als_Visibility
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.Als_Visibility)
      renames LSP.Message_IO.Read_Als_Visibility;

   procedure Read_WorkspaceSymbolClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.WorkspaceSymbolClientCapabilities)
      renames LSP.Message_IO.Read_WorkspaceSymbolClientCapabilities;

   procedure Read_WorkspaceClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.WorkspaceClientCapabilities)
      renames LSP.Message_IO.Read_WorkspaceClientCapabilities;

   procedure Read_MarkupKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.MarkupKind)
      renames LSP.Message_IO.Read_MarkupKind;

   procedure Read_MarkupContent
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.MarkupContent)
      renames LSP.Message_IO.Read_MarkupContent;

   procedure Read_SaveOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.SaveOptions)
      renames LSP.Message_IO.Read_SaveOptions;

   procedure Read_TextDocumentSyncClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.TextDocumentSyncClientCapabilities)
      renames LSP.Message_IO.Read_TextDocumentSyncClientCapabilities;

   procedure Read_CompletionItemTag
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.CompletionItemTag)
      renames LSP.Message_IO.Read_CompletionItemTag;

   procedure Read_CompletionItemTagSupport
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.CompletionItemTagSupport)
      renames LSP.Message_IO.Read_CompletionItemTagSupport;

   procedure Read_completionItemCapability
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.completionItemCapability)
      renames LSP.Message_IO.Read_completionItemCapability;

   procedure Read_CompletionItemKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.CompletionItemKind)
      renames LSP.Message_IO.Read_CompletionItemKind;

   procedure Read_CompletionItemKindSetCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.CompletionItemKindSetCapabilities)
      renames LSP.Message_IO.Read_CompletionItemKindSetCapabilities;

   procedure Read_CompletionClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.CompletionClientCapabilities)
      renames LSP.Message_IO.Read_CompletionClientCapabilities;

   procedure Read_HoverClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.HoverClientCapabilities)
      renames LSP.Message_IO.Read_HoverClientCapabilities;

   procedure Read_parameterInformation_Capability
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.parameterInformation_Capability)
      renames LSP.Message_IO.Read_parameterInformation_Capability;

   procedure Read_signatureInformation_Capability
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.signatureInformation_Capability)
      renames LSP.Message_IO.Read_signatureInformation_Capability;

   procedure Read_SignatureHelpClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.SignatureHelpClientCapabilities)
      renames LSP.Message_IO.Read_SignatureHelpClientCapabilities;

   procedure Read_DocumentSymbolClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.DocumentSymbolClientCapabilities)
      renames LSP.Message_IO.Read_DocumentSymbolClientCapabilities;

   procedure Read_DeclarationClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.DeclarationClientCapabilities)
      renames LSP.Message_IO.Read_DeclarationClientCapabilities;

   procedure Read_codeActionKindCapability
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.codeActionKindCapability)
      renames LSP.Message_IO.Read_codeActionKindCapability;

   procedure Read_codeActionLiteralSupport_Capability
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.codeActionLiteralSupport_Capability)
      renames LSP.Message_IO.Read_codeActionLiteralSupport_Capability;

   procedure Read_CodeActionClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.CodeActionClientCapabilities)
      renames LSP.Message_IO.Read_CodeActionClientCapabilities;

   procedure Read_DocumentLinkClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.DocumentLinkClientCapabilities)
      renames LSP.Message_IO.Read_DocumentLinkClientCapabilities;

   procedure Read_RenameClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.RenameClientCapabilities)
      renames LSP.Message_IO.Read_RenameClientCapabilities;

   procedure Read_DiagnosticTagSupport
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.DiagnosticTagSupport)
      renames LSP.Message_IO.Read_DiagnosticTagSupport;

   procedure Read_PublishDiagnosticsClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.PublishDiagnosticsClientCapabilities)
      renames LSP.Message_IO.Read_PublishDiagnosticsClientCapabilities;

   procedure Read_FoldingRangeClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.FoldingRangeClientCapabilities)
      renames LSP.Message_IO.Read_FoldingRangeClientCapabilities;

   procedure Read_Text_Progress_Params
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.Text_Progress_Params)
      renames LSP.Message_IO.Read_Text_Progress_Params;

   procedure Read_TextDocumentClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.TextDocumentClientCapabilities)
      renames LSP.Message_IO.Read_TextDocumentClientCapabilities;

   procedure Read_WindowClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.WindowClientCapabilities)
      renames LSP.Message_IO.Read_WindowClientCapabilities;

   procedure Read_ClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.ClientCapabilities)
      renames LSP.Message_IO.Read_ClientCapabilities;

   procedure Read_WorkspaceFolder
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.WorkspaceFolder)
      renames LSP.Message_IO.Read_WorkspaceFolder;

   procedure Read_WorkDoneProgressCreateParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.WorkDoneProgressCreateParams)
      renames LSP.Message_IO.Read_WorkDoneProgressCreateParams;

   procedure Read_ProgramInfo
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.ProgramInfo)
      renames LSP.Message_IO.Read_ProgramInfo;

   procedure Read_Trace_Kind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.Trace_Kind)
      renames LSP.Message_IO.Read_Trace_Kind;

   procedure Read_TextDocumentSyncKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.TextDocumentSyncKind)
      renames LSP.Message_IO.Read_TextDocumentSyncKind;

   procedure Read_TextDocumentSyncOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.TextDocumentSyncOptions)
      renames LSP.Message_IO.Read_TextDocumentSyncOptions;

   procedure Read_CompletionOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.CompletionOptions)
      renames LSP.Message_IO.Read_CompletionOptions;

   procedure Read_SignatureHelpOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.SignatureHelpOptions)
      renames LSP.Message_IO.Read_SignatureHelpOptions;

   procedure Read_TSW_RegistrationOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.TSW_RegistrationOptions)
      renames LSP.Message_IO.Read_TSW_RegistrationOptions;

   procedure Read_CodeActionOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.CodeActionOptions)
      renames LSP.Message_IO.Read_CodeActionOptions;

   procedure Read_CodeLensOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.CodeLensOptions)
      renames LSP.Message_IO.Read_CodeLensOptions;

   procedure Read_DocumentOnTypeFormattingOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.DocumentOnTypeFormattingOptions)
      renames LSP.Message_IO.Read_DocumentOnTypeFormattingOptions;

   procedure Read_RenameOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.RenameOptions)
      renames LSP.Message_IO.Read_RenameOptions;

   procedure Read_DocumentLinkOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.DocumentLinkOptions)
      renames LSP.Message_IO.Read_DocumentLinkOptions;

   procedure Read_ExecuteCommandOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.ExecuteCommandOptions)
      renames LSP.Message_IO.Read_ExecuteCommandOptions;

   procedure Read_WorkspaceFoldersServerCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.WorkspaceFoldersServerCapabilities)
      renames LSP.Message_IO.Read_WorkspaceFoldersServerCapabilities;

   procedure Read_workspace_Options
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.workspace_Options)
      renames LSP.Message_IO.Read_workspace_Options;

   procedure Read_ServerCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.ServerCapabilities)
      renames LSP.Message_IO.Read_ServerCapabilities;

   procedure Read_InitializeResult
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.InitializeResult)
      renames LSP.Message_IO.Read_InitializeResult;

   procedure Read_InitializedParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.InitializedParams)
      renames LSP.Message_IO.Read_InitializedParams;

   procedure Read_MessageType
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.MessageType)
      renames LSP.Message_IO.Read_MessageType;

   procedure Read_ShowMessageParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.ShowMessageParams)
      renames LSP.Message_IO.Read_ShowMessageParams;

   procedure Read_ShowMessageRequestParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.ShowMessageRequestParams)
      renames LSP.Message_IO.Read_ShowMessageRequestParams;

   procedure Read_LogMessageParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.LogMessageParams)
      renames LSP.Message_IO.Read_LogMessageParams;

   procedure Read_DidChangeConfigurationParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.DidChangeConfigurationParams)
      renames LSP.Message_IO.Read_DidChangeConfigurationParams;

   procedure Read_DidOpenTextDocumentParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.DidOpenTextDocumentParams)
      renames LSP.Message_IO.Read_DidOpenTextDocumentParams;

   procedure Read_TextDocumentContentChangeEvent
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.TextDocumentContentChangeEvent)
      renames LSP.Message_IO.Read_TextDocumentContentChangeEvent;

   procedure Read_DidChangeTextDocumentParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.DidChangeTextDocumentParams)
      renames LSP.Message_IO.Read_DidChangeTextDocumentParams;

   procedure Read_TextDocumentSaveReason
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.TextDocumentSaveReason)
      renames LSP.Message_IO.Read_TextDocumentSaveReason;

   procedure Read_DidSaveTextDocumentParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.DidSaveTextDocumentParams)
      renames LSP.Message_IO.Read_DidSaveTextDocumentParams;

   procedure Read_DidCloseTextDocumentParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.DidCloseTextDocumentParams)
      renames LSP.Message_IO.Read_DidCloseTextDocumentParams;

   procedure Read_FileChangeType
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.FileChangeType)
      renames LSP.Message_IO.Read_FileChangeType;

   procedure Read_PublishDiagnosticsParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.PublishDiagnosticsParams)
      renames LSP.Message_IO.Read_PublishDiagnosticsParams;

   procedure Read_InsertTextFormat
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.InsertTextFormat)
      renames LSP.Message_IO.Read_InsertTextFormat;

   procedure Read_CompletionItem
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.CompletionItem)
      renames LSP.Message_IO.Read_CompletionItem;

   procedure Read_CompletionList
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.CompletionList)
      renames LSP.Message_IO.Read_CompletionList;

   procedure Read_Hover
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.Hover)
      renames LSP.Message_IO.Read_Hover;

   procedure Read_ParameterInformation
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.ParameterInformation)
      renames LSP.Message_IO.Read_ParameterInformation;

   procedure Read_SignatureInformation
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.SignatureInformation)
      renames LSP.Message_IO.Read_SignatureInformation;

   procedure Read_SignatureHelp
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.SignatureHelp)
      renames LSP.Message_IO.Read_SignatureHelp;

   procedure Read_ReferenceContext
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.ReferenceContext)
      renames LSP.Message_IO.Read_ReferenceContext;

   procedure Read_ReferenceParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.ReferenceParams)
      renames LSP.Message_IO.Read_ReferenceParams;

   procedure Read_DocumentHighlightKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.DocumentHighlightKind)
      renames LSP.Message_IO.Read_DocumentHighlightKind;

   procedure Read_DocumentHighlight
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.DocumentHighlight)
      renames LSP.Message_IO.Read_DocumentHighlight;

   procedure Read_DocumentSymbolParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.DocumentSymbolParams)
      renames LSP.Message_IO.Read_DocumentSymbolParams;

   procedure Read_SymbolInformation
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.SymbolInformation)
      renames LSP.Message_IO.Read_SymbolInformation;

   procedure Read_WorkspaceSymbolParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.WorkspaceSymbolParams)
      renames LSP.Message_IO.Read_WorkspaceSymbolParams;

   procedure Read_CodeActionContext
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.CodeActionContext)
      renames LSP.Message_IO.Read_CodeActionContext;

   procedure Read_CodeActionParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.CodeActionParams)
      renames LSP.Message_IO.Read_CodeActionParams;

   procedure Read_FormattingOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.FormattingOptions)
      renames LSP.Message_IO.Read_FormattingOptions;

   procedure Read_DocumentFormattingParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.DocumentFormattingParams)
      renames LSP.Message_IO.Read_DocumentFormattingParams;

   procedure Read_NavigationRequestParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out NavigationRequestParams)
     renames LSP.Message_IO.Read_NavigationRequestParams;

   procedure Read_DocumentRangeFormattingParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.DocumentRangeFormattingParams)
      renames LSP.Message_IO.Read_DocumentRangeFormattingParams;

   procedure Read_DocumentOnTypeFormattingParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.DocumentOnTypeFormattingParams)
      renames LSP.Message_IO.Read_DocumentOnTypeFormattingParams;

   procedure Read_RenameParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.RenameParams)
      renames LSP.Message_IO.Read_RenameParams;

   procedure Read_ApplyWorkspaceEditParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.ApplyWorkspaceEditParams)
      renames LSP.Message_IO.Read_ApplyWorkspaceEditParams;

   procedure Read_ApplyWorkspaceEditResult
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.ApplyWorkspaceEditResult)
      renames LSP.Message_IO.Read_ApplyWorkspaceEditResult;

   procedure Read_WorkDoneProgressBegin
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.WorkDoneProgressBegin)
      renames LSP.Message_IO.Read_WorkDoneProgressBegin;

   procedure Read_WorkDoneProgressReport
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.WorkDoneProgressReport)
      renames LSP.Message_IO.Read_WorkDoneProgressReport;

   procedure Read_WorkDoneProgressEnd
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.WorkDoneProgressEnd)
      renames LSP.Message_IO.Read_WorkDoneProgressEnd;

   procedure Read_WorkspaceFoldersChangeEvent
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.WorkspaceFoldersChangeEvent)
      renames LSP.Message_IO.Read_WorkspaceFoldersChangeEvent;

   procedure Read_DidChangeWorkspaceFoldersParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.DidChangeWorkspaceFoldersParams)
      renames LSP.Message_IO.Read_DidChangeWorkspaceFoldersParams;

   procedure Read_ConfigurationItem
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.ConfigurationItem)
      renames LSP.Message_IO.Read_ConfigurationItem;

   procedure Read_ConfigurationParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.ConfigurationParams)
      renames LSP.Message_IO.Read_ConfigurationParams;

   procedure Read_FileSystemWatcher
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.FileSystemWatcher)
      renames LSP.Message_IO.Read_FileSystemWatcher;

   procedure Read_DidChangeWatchedFilesRegistrationOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.DidChangeWatchedFilesRegistrationOptions)
      renames LSP.Message_IO.Read_DidChangeWatchedFilesRegistrationOptions;

   procedure Read_CompletionTriggerKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.CompletionTriggerKind)
      renames LSP.Message_IO.Read_CompletionTriggerKind;

   procedure Read_CompletionContext
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.CompletionContext)
      renames LSP.Message_IO.Read_CompletionContext;

   procedure Read_CompletionParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.CompletionParams)
      renames LSP.Message_IO.Read_CompletionParams;

   procedure Read_RGBA_Color
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.RGBA_Color)
      renames LSP.Message_IO.Read_RGBA_Color;

   procedure Read_ColorInformation
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.ColorInformation)
      renames LSP.Message_IO.Read_ColorInformation;

   procedure Read_ColorPresentationParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.ColorPresentationParams)
      renames LSP.Message_IO.Read_ColorPresentationParams;

   procedure Read_ColorPresentation
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.ColorPresentation)
      renames LSP.Message_IO.Read_ColorPresentation;

   procedure Read_FoldingRangeParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.FoldingRangeParams)
      renames LSP.Message_IO.Read_FoldingRangeParams;

   procedure Read_FoldingRange
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.FoldingRange)
      renames LSP.Message_IO.Read_FoldingRange;

   procedure Read_DocumentColorParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.DocumentColorParams)
      renames LSP.Message_IO.Read_DocumentColorParams;

   procedure Read_SelectionRangeParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.SelectionRangeParams)
      renames LSP.Message_IO.Read_SelectionRangeParams;

   procedure Read_SelectionRange
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.SelectionRange)
      renames LSP.Message_IO.Read_SelectionRange;

   procedure Read_ALS_Subprogram_And_References
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.ALS_Subprogram_And_References)
      renames LSP.Message_IO.Read_ALS_Subprogram_And_References;

   procedure Read_ALS_Unit_Description
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.ALS_Unit_Description)
      renames LSP.Message_IO.Read_ALS_Unit_Description;

   procedure Read_ALS_ShowDependenciesKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ALS_ShowDependenciesKind)
     renames LSP.Message_IO.Read_ALS_ShowDependenciesKind;

   procedure Write_ALS_ShowDependenciesKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ALS_ShowDependenciesKind)
     renames LSP.Message_IO.Write_ALS_ShowDependenciesKind;

   procedure Read_ALS_ShowDependenciesParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ALS_ShowDependenciesParams)
     renames LSP.Message_IO.Read_ALS_ShowDependenciesParams;

   procedure Write_ALS_ShowDependenciesParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ALS_ShowDependenciesParams)
   renames LSP.Message_IO.Write_ALS_ShowDependenciesParams;

   procedure Write_RequestMessage
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.RequestMessage)
      renames LSP.Message_IO.Write_RequestMessage;

   procedure Write_NotificationMessage
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.NotificationMessage)
      renames LSP.Message_IO.Write_NotificationMessage;

   procedure Write_CallHierarchyIncomingCall
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CallHierarchyIncomingCall)
      renames LSP.Message_IO.Write_CallHierarchyIncomingCall;

   procedure Write_CallHierarchyIncomingCallsParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CallHierarchyIncomingCallsParams)
      renames LSP.Message_IO.Write_CallHierarchyIncomingCallsParams;

   procedure Write_CallHierarchyItem
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CallHierarchyItem)
      renames LSP.Message_IO.Write_CallHierarchyItem;

   procedure Write_CallHierarchyOutgoingCall
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CallHierarchyOutgoingCall)
      renames LSP.Message_IO.Write_CallHierarchyOutgoingCall;

   procedure Write_CancelParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.CancelParams)
      renames LSP.Message_IO.Write_CancelParams;

   procedure Write_Position
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.Position)
      renames LSP.Message_IO.Write_Position;

   procedure Write_Span
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.Span)
      renames LSP.Message_IO.Write_Span;

   procedure Write_CodeActionKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.CodeActionKind)
      renames LSP.Message_IO.Write_CodeActionKind;

   procedure Write_AlsReferenceKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.AlsReferenceKind)
      renames LSP.Message_IO.Write_AlsReferenceKind;

   procedure Write_AlsDisplayMethodAncestryOnNavigationPolicy
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.AlsDisplayMethodAncestryOnNavigationPolicy)
      renames LSP.Message_IO.Write_AlsDisplayMethodAncestryOnNavigationPolicy;

   procedure Write_Location
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.Location)
      renames LSP.Message_IO.Write_Location;

   procedure Write_LocationLink
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.LocationLink)
      renames LSP.Message_IO.Write_LocationLink;

   procedure Write_DiagnosticSeverity
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.DiagnosticSeverity)
      renames LSP.Message_IO.Write_DiagnosticSeverity;

   procedure Write_DiagnosticTag
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.DiagnosticTag)
      renames LSP.Message_IO.Write_DiagnosticTag;

   procedure Write_DiagnosticRelatedInformation
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.DiagnosticRelatedInformation)
      renames LSP.Message_IO.Write_DiagnosticRelatedInformation;

   procedure Write_Diagnostic
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.Diagnostic)
      renames LSP.Message_IO.Write_Diagnostic;

   procedure Write_TextEdit
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.TextEdit)
      renames LSP.Message_IO.Write_TextEdit;

   procedure Write_TextDocumentIdentifier
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.TextDocumentIdentifier)
      renames LSP.Message_IO.Write_TextDocumentIdentifier;

   procedure Write_TextDocumentEdit
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.TextDocumentEdit)
      renames LSP.Message_IO.Write_TextDocumentEdit;

   procedure Write_TextDocumentItem
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.TextDocumentItem)
      renames LSP.Message_IO.Write_TextDocumentItem;

   procedure Write_TextDocumentPositionParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.TextDocumentPositionParams)
      renames LSP.Message_IO.Write_TextDocumentPositionParams;

   procedure Write_dynamicRegistration
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.dynamicRegistration)
      renames LSP.Message_IO.Write_dynamicRegistration;

   procedure Write_ResourceOperationKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.ResourceOperationKind)
      renames LSP.Message_IO.Write_ResourceOperationKind;

   procedure Write_FailureHandlingKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.FailureHandlingKind)
      renames LSP.Message_IO.Write_FailureHandlingKind;

   procedure Write_WorkspaceEditClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.WorkspaceEditClientCapabilities)
      renames LSP.Message_IO.Write_WorkspaceEditClientCapabilities;

   procedure Write_SymbolKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.SymbolKind)
      renames LSP.Message_IO.Write_SymbolKind;

   procedure Write_symbolKindCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.symbolKindCapabilities)
      renames LSP.Message_IO.Write_symbolKindCapabilities;

   procedure Write_Als_Visibility
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.Als_Visibility)
      renames LSP.Message_IO.Write_Als_Visibility;

   procedure Write_WorkspaceSymbolClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.WorkspaceSymbolClientCapabilities)
      renames LSP.Message_IO.Write_WorkspaceSymbolClientCapabilities;

   procedure Write_WorkspaceClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.WorkspaceClientCapabilities)
      renames LSP.Message_IO.Write_WorkspaceClientCapabilities;

   procedure Write_MarkupKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.MarkupKind)
      renames LSP.Message_IO.Write_MarkupKind;

   procedure Write_MarkupContent
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.MarkupContent)
      renames LSP.Message_IO.Write_MarkupContent;

   procedure Write_SaveOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.SaveOptions)
      renames LSP.Message_IO.Write_SaveOptions;

   procedure Write_TextDocumentSyncClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.TextDocumentSyncClientCapabilities)
      renames LSP.Message_IO.Write_TextDocumentSyncClientCapabilities;

   procedure Write_CompletionItemTag
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.CompletionItemTag)
      renames LSP.Message_IO.Write_CompletionItemTag;

   procedure Write_CompletionItemTagSupport
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.CompletionItemTagSupport)
      renames LSP.Message_IO.Write_CompletionItemTagSupport;

   procedure Write_completionItemCapability
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.completionItemCapability)
      renames LSP.Message_IO.Write_completionItemCapability;

   procedure Write_CompletionItemKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.CompletionItemKind)
      renames LSP.Message_IO.Write_CompletionItemKind;

   procedure Write_CompletionItemKindSetCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.CompletionItemKindSetCapabilities)
      renames LSP.Message_IO.Write_CompletionItemKindSetCapabilities;

   procedure Write_CompletionClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.CompletionClientCapabilities)
      renames LSP.Message_IO.Write_CompletionClientCapabilities;

   procedure Write_HoverClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.HoverClientCapabilities)
      renames LSP.Message_IO.Write_HoverClientCapabilities;

   procedure Write_parameterInformation_Capability
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.parameterInformation_Capability)
      renames LSP.Message_IO.Write_parameterInformation_Capability;

   procedure Write_signatureInformation_Capability
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.signatureInformation_Capability)
      renames LSP.Message_IO.Write_signatureInformation_Capability;

   procedure Write_SignatureHelpClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.SignatureHelpClientCapabilities)
      renames LSP.Message_IO.Write_SignatureHelpClientCapabilities;

   procedure Write_DocumentSymbolClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.DocumentSymbolClientCapabilities)
      renames LSP.Message_IO.Write_DocumentSymbolClientCapabilities;

   procedure Write_DeclarationClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.DeclarationClientCapabilities)
      renames LSP.Message_IO.Write_DeclarationClientCapabilities;

   procedure Write_codeActionKindCapability
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.codeActionKindCapability)
      renames LSP.Message_IO.Write_codeActionKindCapability;

   procedure Write_codeActionLiteralSupport_Capability
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.codeActionLiteralSupport_Capability)
      renames LSP.Message_IO.Write_codeActionLiteralSupport_Capability;

   procedure Write_CodeActionClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.CodeActionClientCapabilities)
      renames LSP.Message_IO.Write_CodeActionClientCapabilities;

   procedure Write_DocumentLinkClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.DocumentLinkClientCapabilities)
      renames LSP.Message_IO.Write_DocumentLinkClientCapabilities;

   procedure Write_RenameClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.RenameClientCapabilities)
      renames LSP.Message_IO.Write_RenameClientCapabilities;

   procedure Write_DiagnosticTagSupport
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.DiagnosticTagSupport)
      renames LSP.Message_IO.Write_DiagnosticTagSupport;

   procedure Write_PublishDiagnosticsClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.PublishDiagnosticsClientCapabilities)
      renames LSP.Message_IO.Write_PublishDiagnosticsClientCapabilities;

   procedure Write_FoldingRangeClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.FoldingRangeClientCapabilities)
      renames LSP.Message_IO.Write_FoldingRangeClientCapabilities;

   procedure Write_Text_Progress_Params
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.Text_Progress_Params)
      renames LSP.Message_IO.Write_Text_Progress_Params;

   procedure Write_TextDocumentClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.TextDocumentClientCapabilities)
      renames LSP.Message_IO.Write_TextDocumentClientCapabilities;

   procedure Write_WindowClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.WindowClientCapabilities)
      renames LSP.Message_IO.Write_WindowClientCapabilities;

   procedure Write_ClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.ClientCapabilities)
      renames LSP.Message_IO.Write_ClientCapabilities;

   procedure Write_WorkspaceFolder
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.WorkspaceFolder)
      renames LSP.Message_IO.Write_WorkspaceFolder;

   procedure Write_WorkDoneProgressCreateParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.WorkDoneProgressCreateParams)
      renames LSP.Message_IO.Write_WorkDoneProgressCreateParams;

   procedure Write_ProgramInfo
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.ProgramInfo)
      renames LSP.Message_IO.Write_ProgramInfo;

   procedure Write_Trace_Kind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.Trace_Kind)
      renames LSP.Message_IO.Write_Trace_Kind;

   procedure Write_TextDocumentSyncKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.TextDocumentSyncKind)
      renames LSP.Message_IO.Write_TextDocumentSyncKind;

   procedure Write_TextDocumentSyncOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.TextDocumentSyncOptions)
      renames LSP.Message_IO.Write_TextDocumentSyncOptions;

   procedure Write_CompletionOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.CompletionOptions)
      renames LSP.Message_IO.Write_CompletionOptions;

   procedure Write_SignatureHelpOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.SignatureHelpOptions)
      renames LSP.Message_IO.Write_SignatureHelpOptions;

   procedure Write_TSW_RegistrationOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.TSW_RegistrationOptions)
      renames LSP.Message_IO.Write_TSW_RegistrationOptions;

   procedure Write_CodeActionOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.CodeActionOptions)
      renames LSP.Message_IO.Write_CodeActionOptions;

   procedure Write_CodeLensOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.CodeLensOptions)
      renames LSP.Message_IO.Write_CodeLensOptions;

   procedure Write_DocumentOnTypeFormattingOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.DocumentOnTypeFormattingOptions)
      renames LSP.Message_IO.Write_DocumentOnTypeFormattingOptions;

   procedure Write_RenameOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.RenameOptions)
      renames LSP.Message_IO.Write_RenameOptions;

   procedure Write_DocumentLinkOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.DocumentLinkOptions)
      renames LSP.Message_IO.Write_DocumentLinkOptions;

   procedure Write_ExecuteCommandOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.ExecuteCommandOptions)
      renames LSP.Message_IO.Write_ExecuteCommandOptions;

   procedure Write_WorkspaceFoldersServerCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.WorkspaceFoldersServerCapabilities)
      renames LSP.Message_IO.Write_WorkspaceFoldersServerCapabilities;

   procedure Write_workspace_Options
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.workspace_Options)
      renames LSP.Message_IO.Write_workspace_Options;

   procedure Write_ServerCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.ServerCapabilities)
      renames LSP.Message_IO.Write_ServerCapabilities;

   procedure Write_InitializeResult
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.InitializeResult)
      renames LSP.Message_IO.Write_InitializeResult;

   procedure Write_InitializedParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.InitializedParams)
      renames LSP.Message_IO.Write_InitializedParams;

   procedure Write_MessageType
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.MessageType)
      renames LSP.Message_IO.Write_MessageType;

   procedure Write_ShowMessageParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.ShowMessageParams)
      renames LSP.Message_IO.Write_ShowMessageParams;

   procedure Write_ShowMessageRequestParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.ShowMessageRequestParams)
      renames LSP.Message_IO.Write_ShowMessageRequestParams;

   procedure Write_LogMessageParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.LogMessageParams)
      renames LSP.Message_IO.Write_LogMessageParams;

   procedure Write_DidChangeConfigurationParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.DidChangeConfigurationParams)
      renames LSP.Message_IO.Write_DidChangeConfigurationParams;

   procedure Write_DidOpenTextDocumentParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.DidOpenTextDocumentParams)
      renames LSP.Message_IO.Write_DidOpenTextDocumentParams;

   procedure Write_TextDocumentContentChangeEvent
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.TextDocumentContentChangeEvent)
      renames LSP.Message_IO.Write_TextDocumentContentChangeEvent;

   procedure Write_DidChangeTextDocumentParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.DidChangeTextDocumentParams)
      renames LSP.Message_IO.Write_DidChangeTextDocumentParams;

   procedure Write_TextDocumentSaveReason
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.TextDocumentSaveReason)
      renames LSP.Message_IO.Write_TextDocumentSaveReason;

   procedure Write_DidSaveTextDocumentParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.DidSaveTextDocumentParams)
      renames LSP.Message_IO.Write_DidSaveTextDocumentParams;

   procedure Write_DidCloseTextDocumentParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.DidCloseTextDocumentParams)
      renames LSP.Message_IO.Write_DidCloseTextDocumentParams;

   procedure Write_FileChangeType
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.FileChangeType)
      renames LSP.Message_IO.Write_FileChangeType;

   procedure Write_PublishDiagnosticsParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.PublishDiagnosticsParams)
      renames LSP.Message_IO.Write_PublishDiagnosticsParams;

   procedure Write_InsertTextFormat
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.InsertTextFormat)
      renames LSP.Message_IO.Write_InsertTextFormat;

   procedure Write_CompletionItem
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.CompletionItem)
      renames LSP.Message_IO.Write_CompletionItem;

   procedure Write_CompletionList
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.CompletionList)
      renames LSP.Message_IO.Write_CompletionList;

   procedure Write_Hover
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.Hover)
      renames LSP.Message_IO.Write_Hover;

   procedure Write_ParameterInformation
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.ParameterInformation)
      renames LSP.Message_IO.Write_ParameterInformation;

   procedure Write_SignatureInformation
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.SignatureInformation)
      renames LSP.Message_IO.Write_SignatureInformation;

   procedure Write_SignatureHelp
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.SignatureHelp)
      renames LSP.Message_IO.Write_SignatureHelp;

   procedure Write_ReferenceContext
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.ReferenceContext)
      renames LSP.Message_IO.Write_ReferenceContext;

   procedure Write_ReferenceParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.ReferenceParams)
      renames LSP.Message_IO.Write_ReferenceParams;

   procedure Write_DocumentHighlightKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.DocumentHighlightKind)
      renames LSP.Message_IO.Write_DocumentHighlightKind;

   procedure Write_DocumentHighlight
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.DocumentHighlight)
      renames LSP.Message_IO.Write_DocumentHighlight;

   procedure Write_DocumentSymbolParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.DocumentSymbolParams)
      renames LSP.Message_IO.Write_DocumentSymbolParams;

   procedure Write_SymbolInformation
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.SymbolInformation)
      renames LSP.Message_IO.Write_SymbolInformation;

   procedure Write_WorkspaceSymbolParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.WorkspaceSymbolParams)
      renames LSP.Message_IO.Write_WorkspaceSymbolParams;

   procedure Write_CodeActionContext
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.CodeActionContext)
      renames LSP.Message_IO.Write_CodeActionContext;

   procedure Write_CodeActionParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.CodeActionParams)
      renames LSP.Message_IO.Write_CodeActionParams;

   procedure Write_FormattingOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.FormattingOptions)
      renames LSP.Message_IO.Write_FormattingOptions;

   procedure Write_DocumentFormattingParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.DocumentFormattingParams)
      renames LSP.Message_IO.Write_DocumentFormattingParams;

   procedure Write_NavigationRequestParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : NavigationRequestParams)
     renames LSP.Message_IO.Write_NavigationRequestParams;

   procedure Write_DocumentRangeFormattingParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.DocumentRangeFormattingParams)
      renames LSP.Message_IO.Write_DocumentRangeFormattingParams;

   procedure Write_DocumentOnTypeFormattingParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.DocumentOnTypeFormattingParams)
      renames LSP.Message_IO.Write_DocumentOnTypeFormattingParams;

   procedure Write_RenameParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.RenameParams)
      renames LSP.Message_IO.Write_RenameParams;

   procedure Write_ApplyWorkspaceEditParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.ApplyWorkspaceEditParams)
      renames LSP.Message_IO.Write_ApplyWorkspaceEditParams;

   procedure Write_ApplyWorkspaceEditResult
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.ApplyWorkspaceEditResult)
      renames LSP.Message_IO.Write_ApplyWorkspaceEditResult;

   procedure Write_WorkDoneProgressBegin
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.WorkDoneProgressBegin)
      renames LSP.Message_IO.Write_WorkDoneProgressBegin;

   procedure Write_WorkDoneProgressReport
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.WorkDoneProgressReport)
      renames LSP.Message_IO.Write_WorkDoneProgressReport;

   procedure Write_WorkDoneProgressEnd
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.WorkDoneProgressEnd)
      renames LSP.Message_IO.Write_WorkDoneProgressEnd;

   procedure Write_WorkspaceFoldersChangeEvent
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.WorkspaceFoldersChangeEvent)
      renames LSP.Message_IO.Write_WorkspaceFoldersChangeEvent;

   procedure Write_DidChangeWorkspaceFoldersParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.DidChangeWorkspaceFoldersParams)
      renames LSP.Message_IO.Write_DidChangeWorkspaceFoldersParams;

   procedure Write_ConfigurationItem
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.ConfigurationItem)
      renames LSP.Message_IO.Write_ConfigurationItem;

   procedure Write_ConfigurationParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.ConfigurationParams)
      renames LSP.Message_IO.Write_ConfigurationParams;

   procedure Write_FileSystemWatcher
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.FileSystemWatcher)
      renames LSP.Message_IO.Write_FileSystemWatcher;

   procedure Write_DidChangeWatchedFilesRegistrationOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.DidChangeWatchedFilesRegistrationOptions)
      renames LSP.Message_IO.Write_DidChangeWatchedFilesRegistrationOptions;

   procedure Write_CompletionTriggerKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.CompletionTriggerKind)
      renames LSP.Message_IO.Write_CompletionTriggerKind;

   procedure Write_CompletionContext
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.CompletionContext)
      renames LSP.Message_IO.Write_CompletionContext;

   procedure Write_CompletionParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.CompletionParams)
      renames LSP.Message_IO.Write_CompletionParams;

   procedure Write_RGBA_Color
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.RGBA_Color)
      renames LSP.Message_IO.Write_RGBA_Color;

   procedure Write_ColorInformation
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.ColorInformation)
      renames LSP.Message_IO.Write_ColorInformation;

   procedure Write_ColorPresentationParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.ColorPresentationParams)
      renames LSP.Message_IO.Write_ColorPresentationParams;

   procedure Write_ColorPresentation
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.ColorPresentation)
      renames LSP.Message_IO.Write_ColorPresentation;

   procedure Write_FoldingRangeParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.FoldingRangeParams)
      renames LSP.Message_IO.Write_FoldingRangeParams;

   procedure Write_FoldingRange
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.FoldingRange)
      renames LSP.Message_IO.Write_FoldingRange;

   procedure Write_DocumentColorParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.DocumentColorParams)
      renames LSP.Message_IO.Write_DocumentColorParams;

   procedure Write_SelectionRangeParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.SelectionRangeParams)
      renames LSP.Message_IO.Write_SelectionRangeParams;

   procedure Write_SelectionRange
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.SelectionRange)
      renames LSP.Message_IO.Write_SelectionRange;

   procedure Write_ALS_Subprogram_And_References
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.ALS_Subprogram_And_References)
      renames LSP.Message_IO.Write_ALS_Subprogram_And_References;

   procedure Write_ALS_Unit_Description
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.ALS_Unit_Description)
      renames LSP.Message_IO.Write_ALS_Unit_Description;

   -------------------
   -- Method_To_Tag --
   -------------------

   function Method_To_Tag
     (Map    : Maps.Map;
      Method : LSP.Types.LSP_String) return Ada.Tags.Tag
   is
      Cursor : constant Maps.Cursor := Map.Find (Method);
   begin
      if Maps.Has_Element (Cursor) then
         return Maps.Element (Cursor);
      else
         return Ada.Tags.No_Tag;
      end if;
   end Method_To_Tag;

   -------------------------------
   -- Read_AlsReferenceKind_Set --
   -------------------------------

   procedure Read_AlsReferenceKind_Set
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out AlsReferenceKind_Set)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

   begin
      if JS.Is_Server_Side then
         V := (Is_Server_Side => True, others => <>);
      else
         V := (Is_Server_Side => False, others => <>);
      end if;

      pragma Assert (JS.R.Is_Start_Array);
      JS.R.Read_Next;

      while not JS.R.Is_End_Array loop
         if V.Is_Server_Side then
            declare
               Kind : AlsReferenceKind;
            begin
               AlsReferenceKind'Read (S, Kind);
               V.As_Flags (Kind) := True;
            end;
         else
            declare
               Item : LSP_String;
            begin
               LSP.Types.Read (S, Item);
               V.As_Strings.Append (Item);
            end;
         end if;
      end loop;

      JS.R.Read_Next;
   end Read_AlsReferenceKind_Set;

   --------------------------------
   -- Write_AlsReferenceKind_Set --
   --------------------------------

   procedure Write_AlsReferenceKind_Set
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : AlsReferenceKind_Set)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      if V /= Empty_Set then
         if V.Is_Server_Side then
            JS.Start_Array;

            for J in V.As_Flags'Range loop
               if V.As_Flags (J) then
                  AlsReferenceKind'Write (S, J);
               end if;
            end loop;

            JS.End_Array;
         else
            LSP_String_Vector'Write (S, V.As_Strings);
         end if;

      end if;
   end Write_AlsReferenceKind_Set;

   ---------------------
   -- Read_CodeAction --
   ---------------------

   procedure Read_CodeAction
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CodeAction)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
      Is_CodeAction : Boolean := False;
      --  This is True if we are reading CodeAction (not a Command object).
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant Ada.Strings.UTF_Encoding.UTF_8_String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;

            if Key = "command" then
               --  if "command" property is a string then we are reading
               --  a "Command" object, so we just set V.command property
               if JS.R.Is_String_Value then
                  if not V.command.Is_Set then
                     V.command := (Is_Set => True, Value => <>);
                     V.command.Value.title := V.title;
                     V.title := Empty_LSP_String;
                  end if;
                  LSP.Types.Read (S, V.command.Value.command);
               else
                  Optional_Command'Read (S, V.command);
                  Is_CodeAction := True;
               end if;
            elsif Key = "title" then
               if V.command.Is_Set and not Is_CodeAction then
                  LSP.Types.Read (S, V.command.Value.title);
               else
                  LSP.Types.Read (S, V.title);
               end if;
            elsif Key = "arguments" then
               --  "arguments" field is part of Command
               if not V.command.Is_Set then
                  V.command := (Is_Set => True, Value => <>);
                  V.command.Value.title := V.title;
                  V.title := Empty_LSP_String;
               end if;
               Optional_Any_Vector'Read (S, V.command.Value.arguments);
            elsif Key = "kind" then
               Optional_CodeActionKind'Read (S, V.kind);
               Is_CodeAction := True;
            elsif Key = "diagnostics" then
               Optional_Diagnostic_Vector'Read (S, V.diagnostics);
               Is_CodeAction := True;
            elsif Key = "isPreferred" then
               Optional_Boolean'Read (S, V.isPreferred);
               Is_CodeAction := True;
            elsif Key = "edit" then
               Optional_WorkspaceEdit'Read (S, V.edit);
               Is_CodeAction := True;
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;

      JS.R.Read_Next;
   end Read_CodeAction;

   ------------------
   -- Read_Command --
   ------------------

   procedure Read_Command
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Command)
   is
      Tag : Ada.Tags.Tag;
      JS  : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

      Look_Ahead : aliased VSS.JSON.Streams.Readers.Look_Ahead
        .JSON_Look_Ahead_Reader (JS.R);
      Nested     : aliased LSP.JSON_Streams.JSON_Stream
        (JS.Is_Server_Side, Look_Ahead'Unchecked_Access);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;
      Read_Tag (Nested, Tag);
      Look_Ahead.Rewind;  --  Rewind stream just after Start_Object

      while Look_Ahead.Is_Key_Name loop
         pragma Assert (Look_Ahead.Is_Key_Name);
         declare
            Key : constant Ada.Strings.UTF_Encoding.UTF_8_String :=
              VSS.Strings.Conversions.To_UTF_8_String
                (Look_Ahead.Key_Name);
         begin
            Look_Ahead.Read_Next;

            if Key = "command" then
               LSP.Types.Read (Nested'Access, V.command);
            elsif Key = "title" then
               LSP.Types.Read (Nested'Access, V.title);
            elsif Key = "arguments" then
               if Tag in Ada.Tags.No_Tag then
                  Optional_Any_Vector'Read (Nested'Access, V.arguments);
               else
                  V :=
                    (Is_Unknown => False,
                     title      => V.title,
                     Custom     => <>);

                  pragma Assert (Look_Ahead.Is_Start_Array);
                  Look_Ahead.Read_Next;

                  V.Custom.Set (Create_Command (Tag, Nested'Access));

                  pragma Assert (Look_Ahead.Is_End_Array);
                  Look_Ahead.Read_Next;
               end if;
            else
               Nested.Skip_Value;
            end if;
         end;
      end loop;

      JS.R.Read_Next;
   end Read_Command;

   ------------------------------
   -- Read_DocumentSymbol_Tree --
   ------------------------------

   procedure Read_DocumentSymbol_Tree
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DocumentSymbol_Tree)
   is
      procedure Read_Array (Parent : DocumentSymbol_Trees.Cursor);
      procedure Read_Object (Next : DocumentSymbol_Trees.Cursor);

      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

      ----------------
      -- Read_Array --
      ----------------

      procedure Read_Array (Parent : DocumentSymbol_Trees.Cursor) is
      begin
         pragma Assert (JS.R.Is_Start_Array);
         JS.R.Read_Next;

         while not JS.R.Is_End_Array loop
            declare
               Next : DocumentSymbol_Trees.Cursor;
            begin
               V.Insert_Child
                 (Parent   => Parent,
                  Before   => DocumentSymbol_Trees.No_Element,
                  New_Item => (children => False, others => <>),
                  Position => Next);

               Read_Object (Next);
            end;
         end loop;
         JS.R.Read_Next;
      end Read_Array;

      -----------------
      -- Read_Object --
      -----------------

      procedure Read_Object (Next : DocumentSymbol_Trees.Cursor) is
         Item : DocumentSymbol := V (Next);
      begin
         pragma Assert (JS.R.Is_Start_Object);
         JS.R.Read_Next;

         while not JS.R.Is_End_Object loop
            pragma Assert (JS.R.Is_Key_Name);
            declare
               Key : constant Ada.Strings.UTF_Encoding.UTF_8_String :=
                 VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
            begin
               JS.R.Read_Next;

               if Key = "name" then
                  LSP.Types.Read (S, Item.name);
               elsif Key = "detail" then
                  Optional_String'Read (S, Item.detail);
               elsif Key = "kind" then
                  SymbolKind'Read (S, Item.kind);
               elsif Key = "deprecated" then
                  Optional_Boolean'Read (S, Item.deprecated);
               elsif Key = "range" then
                  Span'Read (S, Item.span);
               elsif Key = "selectionRange" then
                  Span'Read (S, Item.selectionRange);
               elsif Key = "alsIsDeclaration" then
                  Optional_Boolean'Read (S, Item.alsIsDeclaration);
               elsif Key = "alsIsAdaProcedure" then
                  Optional_Boolean'Read (S, Item.alsIsAdaProcedure);
               elsif Key = "alsVisibility" then
                  Optional_Als_Visibility'Read (S, Item.alsVisibility);
               elsif Key = "children" then
                  if JS.R.Is_Start_Array then
                     Item.children := True;
                     Read_Array (Next);
                  else
                     JS.Skip_Value;
                  end if;
               else
                  JS.Skip_Value;
               end if;
            end;
         end loop;

         JS.R.Read_Next;
         V (Next) := Item;
      end Read_Object;
   begin
      V.Clear;
      Read_Array (V.Root);
   end Read_DocumentSymbol_Tree;

   -------------------------------
   -- Read_ExecuteCommandParams --
   -------------------------------

   procedure Read_ExecuteCommandParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ExecuteCommandParams)
   is
      Tag : Ada.Tags.Tag;
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

      Look_Ahead : aliased VSS.JSON.Streams.Readers.Look_Ahead
        .JSON_Look_Ahead_Reader (JS.R);
      Nested     : aliased LSP.JSON_Streams.JSON_Stream
        (JS.Is_Server_Side, Look_Ahead'Unchecked_Access);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      Read_Tag (Nested, Tag);
      Look_Ahead.Rewind;  --  Rewind stream just after Start_Object

      while not Look_Ahead.Is_End_Object loop
         pragma Assert (Look_Ahead.Is_Key_Name);
         declare
            Key : constant Ada.Strings.UTF_Encoding.UTF_8_String :=
              VSS.Strings.Conversions.To_UTF_8_String (Look_Ahead.Key_Name);
         begin
            Look_Ahead.Read_Next;

            if Key = "workDoneToken" then
               Optional_ProgressToken'Read
                 (Nested'Access, V.Base.workDoneToken);
            elsif Key = "command" then
               LSP.Types.Read (Nested'Access, V.command);
            elsif Key = "arguments" then
               if Tag in Ada.Tags.No_Tag then
                  Optional_Any_Vector'Read (Nested'Access, V.arguments);
               else
                  --  Overwrite discriminant with Is_Unknown => False
                  V :=
                    (Is_Unknown => False,
                     Base       => V.Base,
                     command    => V.command,
                     Custom     => <>);

                  pragma Assert (Look_Ahead.Is_Start_Array);
                  Look_Ahead.Read_Next;

                  V.Custom.Set (Create_Command (Tag, Nested'Access));

                  pragma Assert (Look_Ahead.Is_End_Array);
                  Look_Ahead.Read_Next;
               end if;
            else
               Nested.Skip_Value;
            end if;
         end;
      end loop;

      JS.R.Read_Next;
   end Read_ExecuteCommandParams;

   ---------------------------
   -- Read_InitializeParams --
   ---------------------------

   procedure Read_InitializeParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out InitializeParams)
      renames LSP.Message_IO.Read_InitializeParams;

   ----------------------------------
   -- Read_Location_Or_Link_Vector --
   ----------------------------------

   procedure Read_Location_Or_Link_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Location_Or_Link_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
      Look_Ahead : aliased VSS.JSON.Streams.Readers.Look_Ahead
        .JSON_Look_Ahead_Reader (JS.R);
      Nested     : aliased LSP.JSON_Streams.JSON_Stream
        (JS.Is_Server_Side, Look_Ahead'Unchecked_Access);
   begin
      pragma Assert (Look_Ahead.Is_Start_Array);
      Look_Ahead.Read_Next;

      if Look_Ahead.Is_End_Array then
         Look_Ahead.Read_Next;
         V := (Kind => Empty_Vector_Kind);
         return;
      end if;

      pragma Assert (Look_Ahead.Is_Start_Object);
      Look_Ahead.Read_Next;

      while Look_Ahead.Is_Key_Name loop
         declare
            Key : constant Ada.Strings.UTF_Encoding.UTF_8_String :=
              VSS.Strings.Conversions.To_UTF_8_String (Look_Ahead.Key_Name);
         begin
            Look_Ahead.Read_Next;

            if Key in "originSelectionRange" | "targetUri" | "targetRange"
              | "targetSelectionRange"
            then
               V := (Kind => LocationLink_Vector_Kind, LocationLinks => <>);
               Look_Ahead.Rewind;  --  Rewind to Start_Array and read
               LocationLink_Vector'Read
                 (Nested'Unchecked_Access, V.LocationLinks);

               return;
            elsif Key in "uri" | "range" then
               V := (Kind => Location_Vector_Kind, Locations => <>);
               Look_Ahead.Rewind;  --  Rewind to Start_Array and read
               Location_Vector'Read (Nested'Unchecked_Access, V.Locations);

               return;
            else
               --  Go to next field and try again
               Nested.Skip_Value;
            end if;
         end;
      end loop;

      --  We have read a first element of JSON array, but no known filed was
      --  found. We are unable to select if this is a vector of Location or
      --  vector of LocationLink. So just raise an error.

      raise Constraint_Error with "Unexpected JSON object";
   end Read_Location_Or_Link_Vector;

   -----------------------
   -- Read_MarkedString --
   -----------------------

   procedure Read_MarkedString
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out MarkedString)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      case JS.R.Event_Kind is
         when VSS.JSON.Streams.Readers.String_Value =>
            V := (Is_String => True,
                  Value     => To_LSP_String (JS.R.String_Value));

            JS.R.Read_Next;
         when VSS.JSON.Streams.Readers.Start_Object =>
            V := (Is_String => False, others => <>);

            JS.R.Read_Next;

            while not JS.R.Is_End_Object loop
               pragma Assert (JS.R.Is_Key_Name);
               declare
                  Key : constant Ada.Strings.UTF_Encoding.UTF_8_String :=
                    VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
               begin
                  JS.R.Read_Next;

                  if Key = "language" then
                     LSP.Types.Read (S, V.language);
                  elsif Key = "value" then
                     LSP.Types.Read (S, V.value);
                  else
                     JS.Skip_Value;
                  end if;
               end;
            end loop;
            JS.R.Read_Next;

         when others =>
            --  Unexpected JSON event
            V := (Is_String => True, Value => Empty_LSP_String);
            JS.Skip_Value;
      end case;
   end Read_MarkedString;

   -----------------------------------------------
   -- Read_MarkupContent_Or_MarkedString_Vector --
   -----------------------------------------------

   procedure Read_MarkupContent_Or_MarkedString_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out MarkupContent_Or_MarkedString_Vector)
   is
      procedure Read_Object (JS : LSP.JSON_Streams.JSON_Stream'Class);

      -----------------
      -- Read_Object --
      -----------------

      procedure Read_Object (JS : LSP.JSON_Streams.JSON_Stream'Class) is
         Look_Ahead : aliased VSS.JSON.Streams.Readers.Look_Ahead
           .JSON_Look_Ahead_Reader (JS.R);
         Nested     : aliased LSP.JSON_Streams.JSON_Stream
           (JS.Is_Server_Side, Look_Ahead'Unchecked_Access);
      begin
         pragma Assert (Look_Ahead.Is_Start_Object);
         Look_Ahead.Read_Next;

         while Look_Ahead.Is_Key_Name loop
            declare
               Key : constant Ada.Strings.UTF_Encoding.UTF_8_String :=
                 VSS.Strings.Conversions.To_UTF_8_String
                   (Look_Ahead.Key_Name);
            begin
               Look_Ahead.Read_Next;

               if Key = "kind" then
                  V := (Is_MarkupContent => True,
                        MarkupContent    => <>);
                  Look_Ahead.Rewind;  --  Rewind to Start_Object and read
                  MarkupContent'Read
                    (Nested'Unchecked_Access, V.MarkupContent);

                  return;
               elsif Key = "language" then
                  declare
                     Item : MarkedString;
                  begin
                     V := (Is_MarkupContent => False,
                           Vector           => <>);
                     Look_Ahead.Rewind;  --  Rewind to Start_Object and read
                     MarkedString'Read (Nested'Unchecked_Access, Item);
                     V.Vector.Append (Item);

                     return;
                  end;
               else
                  --  Go to next field and try again
                  Nested.Skip_Value;
               end if;
            end;
         end loop;
      end Read_Object;

      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      case JS.R.Event_Kind is
         when VSS.JSON.Streams.Readers.String_Value =>

            V := (Is_MarkupContent => False,
                  Vector           => <>);
            V.Vector.Append
              (MarkedString'(Is_String => True,
                             value     => To_LSP_String (JS.R.String_Value)));
            JS.R.Read_Next;
         when VSS.JSON.Streams.Readers.Start_Array =>
            V := (Is_MarkupContent => False,
                  Vector           => <>);
            MarkedString_Vector'Read (S, V.Vector);
         when VSS.JSON.Streams.Readers.Start_Object =>
            Read_Object (JS);
         when others =>
            JS.Skip_Value;
      end case;
   end Read_MarkupContent_Or_MarkedString_Vector;

   -------------------------------------------
   -- Read_Optional_TextDocumentSyncOptions --
   -------------------------------------------

   procedure Read_Optional_TextDocumentSyncOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Optional_TextDocumentSyncOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      case JS.R.Event_Kind is
         when VSS.JSON.Streams.Readers.Null_Value =>
            V := (False, False);
            JS.R.Read_Next;
         when VSS.JSON.Streams.Readers.Start_Object =>
            V := (True, False, others => <>);
            TextDocumentSyncOptions'Read (S, V.Options);
         when VSS.JSON.Streams.Readers.Number_Value =>
            V := (True, True, others => <>);
            TextDocumentSyncKind'Read (S, V.Value);
         when others =>
            JS.Skip_Value;
      end case;
   end Read_Optional_TextDocumentSyncOptions;

   --------------------------
   -- Read_Parameter_Label --
   --------------------------

   procedure Read_Parameter_Label
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Parameter_Label)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      case JS.R.Event_Kind is
         when VSS.JSON.Streams.Readers.String_Value =>
            V := (Is_String => True,
                  String    => To_LSP_String (JS.R.String_Value));
            JS.R.Read_Next;
         when VSS.JSON.Streams.Readers.Start_Array =>
            JS.R.Read_Next;
            UTF_16_Index'Read (S, V.From);
            UTF_16_Index'Read (S, V.Till);
            pragma Assert (JS.R.Is_End_Array);
            JS.R.Read_Next;
         when others =>
            JS.Skip_Value;
      end case;
   end Read_Parameter_Label;

   ---------------------------
   -- Read_Provider_Options --
   ---------------------------

   procedure Read_Provider_Options
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Provider_Options)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      case JS.R.Event_Kind is
         when VSS.JSON.Streams.Readers.Boolean_Value =>
            V := (Is_Boolean => True,
                  Bool       => JS.R.Boolean_Value);
            JS.R.Read_Next;
         when VSS.JSON.Streams.Readers.Start_Object =>
            V := (Is_Boolean => False,
                  Options    => (Is_Set => True, Value => <>));

            Optional_TSW_RegistrationOptions'Read (S, V.Options);
         when others =>
            JS.Skip_Value;
            V := (Is_Boolean => False,
                  Options    => (Is_Set => False));
      end case;
   end Read_Provider_Options;

   ----------------------------------
   -- Read_String_Or_MarkupContent --
   ----------------------------------

   procedure Read_String_Or_MarkupContent
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out String_Or_MarkupContent)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      case JS.R.Event_Kind is
         when VSS.JSON.Streams.Readers.String_Value =>
            V := (Is_String => True,
                  String    => To_LSP_String (JS.R.String_Value));
            JS.R.Read_Next;

         when VSS.JSON.Streams.Readers.Start_Object =>
            V := (Is_String => False, Content => <>);
            MarkupContent'Read (S, V.Content);
         when others =>
            V := (Is_String => False, Content => <>);
            JS.Skip_Value;
      end case;
   end Read_String_Or_MarkupContent;

   --------------------------
   -- Read_Document_Change --
   --------------------------

   procedure Read_Document_Change
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Document_Change) is
   begin
      --  FIXME: rewrite reading procedure
      TextDocumentEdit'Read (S, V.Text_Document_Edit);
   end Read_Document_Change;

   ------------------------
   -- Read_Symbol_Vector --
   ------------------------

   procedure Read_Symbol_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Symbol_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
      Look_Ahead : aliased VSS.JSON.Streams.Readers.Look_Ahead
        .JSON_Look_Ahead_Reader (JS.R);
      Nested     : aliased LSP.JSON_Streams.JSON_Stream
        (JS.Is_Server_Side, Look_Ahead'Unchecked_Access);
   begin
      if not JS.R.Is_Start_Array then
         V := (Is_Tree => False, Vector => <>);
         JS.Skip_Value;
      end if;

      Look_Ahead.Read_Next;
      if Look_Ahead.Is_End_Array then
         Look_Ahead.Read_Next;
         V := (Is_Tree => False, Vector => <>);
         return;
      end if;

      pragma Assert (Look_Ahead.Is_Start_Object);
      Look_Ahead.Read_Next;

      while Look_Ahead.Is_Key_Name loop
         declare
            Key : constant Ada.Strings.UTF_Encoding.UTF_8_String :=
              VSS.Strings.Conversions.To_UTF_8_String (Look_Ahead.Key_Name);
         begin
            Look_Ahead.Read_Next;

            if Key in "detail" | "range" | "selectionRange" | "children" then
               V := (Is_Tree => True, Tree => <>);
               Look_Ahead.Rewind;  --  Rewind to Start_Array and read
               DocumentSymbol_Tree'Read (Nested'Unchecked_Access, V.Tree);

               return;
            elsif Key in "location" | "containerName" then
               V := (Is_Tree => False, Vector => <>);
               Look_Ahead.Rewind;  --  Rewind to Start_Array and read
               SymbolInformation_Vector'Read
                 (Nested'Unchecked_Access, V.Vector);

               return;
            else
               --  Go to next field and try again
               Nested.Skip_Value;
            end if;
         end;
      end loop;

      --  We have read a first element of JSON array, but no known filed was
      --  found. We are unable to select if this is a vector of DocumentSymbol
      --  or vector of SymbolInformation. So just raise an error.

      raise Constraint_Error with "Unexpected JSON object";
   end Read_Symbol_Vector;

   --------------
   -- Read_Tag --
   --------------

   procedure Read_Tag
     (JS  : in out LSP.JSON_Streams.JSON_Stream'Class;
      Tag : out Ada.Tags.Tag)
   is
      Command : LSP_String;
   begin
      Tag := Ada.Tags.No_Tag;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant Ada.Strings.UTF_Encoding.UTF_8_String :=
              VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;

            if Key = "command" then
               LSP.Types.Read (JS'Access, Command);
               if JS.Is_Server_Side then
                  Tag := Ada.Tags.Internal_Tag
                    (LSP.Types.To_UTF_8_String (Command));
               else
                  --  There is a discrepancy between server code and client
                  --  code when decoding a "command": the server has a
                  --  hierarchy of tagged types that represent command
                  --  objects, but this is an opaque type to the client,
                  --  which does not have tagged types with the named tags.
                  Tag := Ada.Tags.No_Tag;
               end if;

               exit;
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
   end Read_Tag;

   ------------------------------------
   -- Get_TextDocumentPositionParams --
   ------------------------------------

   procedure Get_TextDocumentPositionParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out TextDocumentPositionParams'Class)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Key ("textDocument");
      TextDocumentIdentifier'Read (S, V.textDocument);
      JS.Key ("position");
      Position'Read (S, V.position);
   end Get_TextDocumentPositionParams;

   ------------------------------------------
   -- Read_VersionedTextDocumentIdentifier --
   ------------------------------------------

   procedure Read_VersionedTextDocumentIdentifier
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out VersionedTextDocumentIdentifier)
      renames LSP.Message_IO.Read_VersionedTextDocumentIdentifier;

   ------------------------
   -- Read_WatchKind_Set --
   ------------------------

   procedure Read_WatchKind_Set
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out WatchKind_Set)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
      Result : Integer;
      Mask   : Integer := 4;
   begin
      case JS.R.Event_Kind is
         when VSS.JSON.Streams.Readers.Number_Value =>
            Result := Integer (JS.R.Number_Value.Integer_Value);

            for J in reverse WatchKind loop
               if Result >= Mask then
                  V (J) := True;
                  Result := Result - Mask;
               end if;

               Mask := Mask / 2;
            end loop;
            JS.R.Read_Next;
         when others =>
            V := Default_WatchKind_Set;
            JS.Skip_Value;
      end case;
   end Read_WatchKind_Set;

   ----------------------------------
   -- Read_WorkDoneProgressOptions --
   ----------------------------------

   procedure Read_WorkDoneProgressOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out WorkDoneProgressOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant Ada.Strings.UTF_Encoding.UTF_8_String :=
               VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;

            if Key = "workDoneProgress" then
               Optional_Boolean'Read (S, V.workDoneProgress);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_WorkDoneProgressOptions;

   ------------------------
   -- Read_WorkspaceEdit --
   ------------------------

   procedure Read_WorkspaceEdit
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out WorkspaceEdit)
   is
      procedure Each (Name : VSS.Strings.Virtual_String);

      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

      ----------
      -- Each --
      ----------

      procedure Each (Name : VSS.Strings.Virtual_String) is
         Key : constant Ada.Strings.UTF_Encoding.UTF_8_String :=
           VSS.Strings.Conversions.To_UTF_8_String (Name);
         Vector : TextEdit_Vector;
      begin
         JS.R.Read_Next;  --  Skip Key
         pragma Assert (JS.R.Is_Start_Array);
         JS.R.Read_Next;

         while not JS.R.Is_End_Array loop
            declare
               Item : TextEdit;
            begin
               TextEdit'Read (S, Item);
               Vector.Append (Item);
            end;
         end loop;

         JS.R.Read_Next;

         V.changes.Insert (To_LSP_String (Key), Vector);
      end Each;

   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant Ada.Strings.UTF_Encoding.UTF_8_String :=
               VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;

            if Key = "changes" then
               pragma Assert (JS.R.Is_Start_Object);
               JS.R.Read_Next;
               while not JS.R.Is_End_Object loop
                  pragma Assert (JS.R.Is_Key_Name);
                  Each (JS.R.String_Value);
               end loop;
               JS.R.Read_Next;

            elsif Key = "documentChanges" then
               Document_Change_Vector'Read (S, V.documentChanges);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_WorkspaceEdit;

   ----------------------
   -- Write_CodeAction --
   ----------------------

   procedure Write_CodeAction
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CodeAction)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      if LSP.Types.Is_Empty (V.title) and then V.command.Is_Set then
         Optional_Command'Write (S, V.command);
      else
         JS.Start_Object;
         Write_String (JS, +"title", V.title);
         JS.Key ("kind");
         Optional_CodeActionKind'Write (S, V.kind);
         JS.Key ("diagnostics");
         Optional_Diagnostic_Vector'Write (S, V.diagnostics);
         Write_Optional_Boolean (JS, +"isPreferred", V.isPreferred);
         JS.Key ("edit");
         Optional_WorkspaceEdit'Write (S, V.edit);
         JS.Key ("command");
         Optional_Command'Write (S, V.command);
         JS.End_Object;
      end if;
   end Write_CodeAction;

   -------------------
   -- Write_Command --
   -------------------

   procedure Write_Command
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Command)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
      Temp : LSP.Commands.Command_Access;
   begin
      if V.Is_Unknown and then Is_Empty (V.command) then
         return;
      end if;

      JS.Start_Object;
      Write_String (JS, +"title", V.title);

      if V.Is_Unknown then
         Write_String (JS, +"command", V.command);
         JS.Key ("arguments");
         Optional_Any_Vector'Write (S, V.arguments);
      else
         Write_String
           (JS,
            +"command",
            +Ada.Tags.External_Tag (V.Custom.Unchecked_Get'Tag));
         JS.Key ("arguments");
         JS.Start_Array;
         Temp := LSP.Commands.Command_Access (V.Custom.Unchecked_Get);
         --  This Temp variable prevents compiler from a crash.
         LSP.Commands.Command'Class'Write (S, Temp.all);
         JS.End_Array;
      end if;

      JS.End_Object;
   end Write_Command;

   -------------------------------
   -- Write_DocumentSymbol_Tree --
   -------------------------------

   procedure Write_DocumentSymbol_Tree
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DocumentSymbol_Tree)
   is
      procedure Write_Array (Parent : DocumentSymbol_Trees.Cursor);

      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

      -----------------
      -- Write_Array --
      -----------------

      procedure Write_Array (Parent : DocumentSymbol_Trees.Cursor) is
      begin
         JS.Start_Array;

         for J in V.Iterate_Children (Parent) loop
            declare
               Item : DocumentSymbol renames DocumentSymbol_Trees.Element (J);
            begin
               JS.Start_Object;
               Write_String (JS, +"name", Item.name);
               JS.Key ("detail");
               Optional_String'Write (S, Item.detail);
               JS.Key ("kind");
               SymbolKind'Write (S, Item.kind);
               Write_Optional_Boolean (JS, +"deprecated", Item.deprecated);
               JS.Key ("range");
               Span'Write (S, Item.span);
               JS.Key ("selectionRange");
               Span'Write (S, Item.selectionRange);
               Write_Optional_Boolean
                 (JS, +"alsIsDeclaration", Item.alsIsDeclaration);
               Write_Optional_Boolean
                   (JS, +"alsIsAdaProcedure", Item.alsIsAdaProcedure);
               JS.Key ("alsVisibility");
               Optional_Als_Visibility'Write (S, Item.alsVisibility);

               if Item.children then
                  JS.Key ("children");
                  Write_Array (J);
               end if;

               JS.End_Object;
            end;
         end loop;

         JS.End_Array;
      end Write_Array;
   begin
      Write_Array (V.Root);
   end Write_DocumentSymbol_Tree;

   --------------------------------
   -- Write_ExecuteCommandParams --
   --------------------------------

   procedure Write_ExecuteCommandParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ExecuteCommandParams)
   is
      Temp : LSP.Commands.Command_Access;
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("workDoneToken");
      Optional_ProgressToken'Write (S, V.Base.workDoneToken);
      JS.Key ("arguments");

      if V.Is_Unknown then
         Optional_Any_Vector'Write (S, V.arguments);
         Write_String (JS, +"command", V.command);
      else
         JS.Start_Array;
         Temp := LSP.Commands.Command_Access (V.Custom.Unchecked_Get);
         --  This Temp variable prevents compiler from a crash.
         LSP.Commands.Command'Class'Write (S, Temp.all);
         JS.End_Array;
         Write_String (JS, +"command", V.command);
      end if;

      JS.End_Object;
   end Write_ExecuteCommandParams;

   ----------------------------
   -- Write_InitializeParams --
   ----------------------------

   procedure Write_InitializeParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : InitializeParams)
      renames LSP.Message_IO.Write_InitializeParams;

   -----------------------------------
   -- Write_Location_Or_Link_Vector --
   -----------------------------------

   procedure Write_Location_Or_Link_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Location_Or_Link_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      case V.Kind is
         when Empty_Vector_Kind =>
            JS.Write_Null;
         when Location_Vector_Kind =>
            Location_Vector'Write (S, V.Locations);
         when LocationLink_Vector_Kind =>
            LocationLink_Vector'Write (S, V.LocationLinks);
      end case;
   end Write_Location_Or_Link_Vector;

   ------------------------
   -- Write_MarkedString --
   ------------------------

   procedure Write_MarkedString
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : MarkedString)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      if V.Is_String then
         JS.Write_String (V.value);
      else
         JS.Start_Object;
         Write_String (JS, +"language", V.language);
         Write_String (JS, +"value", V.value);
         JS.End_Object;
      end if;
   end Write_MarkedString;

   ------------------------------------------------
   -- Write_MarkupContent_Or_MarkedString_Vector --
   ------------------------------------------------

   procedure Write_MarkupContent_Or_MarkedString_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : MarkupContent_Or_MarkedString_Vector) is
   begin
      if V.Is_MarkupContent then
         MarkupContent'Write (S, V.MarkupContent);
      elsif V.Vector.Last_Index = 1 then
         MarkedString'Write (S, V.Vector.First_Element);
      else
         MarkedString_Vector'Write (S, V.Vector);
      end if;
   end Write_MarkupContent_Or_MarkedString_Vector;

   --------------------------------------------
   -- Write_Optional_TextDocumentSyncOptions --
   --------------------------------------------

   procedure Write_Optional_TextDocumentSyncOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Optional_TextDocumentSyncOptions) is
   begin
      if not V.Is_Set then
         return;
      elsif V.Is_Number then
         TextDocumentSyncKind'Write (S, V.Value);
      else
         TextDocumentSyncOptions'Write (S, V.Options);
      end if;
   end Write_Optional_TextDocumentSyncOptions;

   ---------------------------
   -- Write_Parameter_Label --
   ---------------------------

   procedure Write_Parameter_Label
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Parameter_Label)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      if V.Is_String then
         JS.Write_String (V.String);
      else
         JS.Start_Array;
         JS.Write_Integer (Interfaces.Integer_64 (V.From));
         JS.Write_Integer (Interfaces.Integer_64 (V.Till));
         JS.End_Array;
      end if;
   end Write_Parameter_Label;

   ----------------------------
   -- Write_Provider_Options --
   ----------------------------

   procedure Write_Provider_Options
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Provider_Options)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      if V.Is_Boolean then
         JS.Write_Boolean (V.Bool);
      elsif V.Options.Is_Set then
         Optional_TSW_RegistrationOptions'Write (S, V.Options);
      else
         JS.Start_Object;  --  Write {}
         JS.End_Object;
      end if;
   end Write_Provider_Options;

   --------------------
   -- Write_Response --
   --------------------

   procedure Write_Response_Prefix
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.ResponseMessage'Class)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      Write_String (JS, +"jsonrpc", V.jsonrpc);
      Write_Number_Or_String (JS, +"id", V.id);
      JS.Key ("error");
      Optional_ResponseError'Write (S, V.error);
   end Write_Response_Prefix;

   ---------------------------
   -- Write_ResponseMessage --
   ---------------------------

   procedure Write_ResponseMessage
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ResponseMessage)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Write_Response_Prefix (S, V);

      if not V.Is_Error then
         JS.Key ("result");
         JS.Write_Null;
      end if;

      JS.End_Object;
   end Write_ResponseMessage;

   -----------------------------------
   -- Write_String_Or_MarkupContent --
   -----------------------------------

   procedure Write_String_Or_MarkupContent
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : String_Or_MarkupContent)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      case V.Is_String is
         when True =>
            JS.Write_String (V.String);
         when False =>
            MarkupContent'Write (S, V.Content);
      end case;
   end Write_String_Or_MarkupContent;

   -------------------------
   -- Write_Symbol_Vector --
   -------------------------

   procedure Write_Symbol_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Symbol_Vector) is
   begin
      if V.Is_Tree then
         DocumentSymbol_Tree'Write (S, V.Tree);
      else
         SymbolInformation_Vector'Write (S, V.Vector);
      end if;
   end Write_Symbol_Vector;

   ---------------------------
   -- Write_Document_Change --
   ---------------------------

   procedure Write_Document_Change
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Document_Change) is
   begin
      case V.Kind is
         when Text_Document_Edit =>
            TextDocumentEdit'Write (S, V.Text_Document_Edit);
         when Create_File =>
            CreateFile'Write (S, V.Create_File);
         when Rename_File =>
            RenameFile'Write (S, V.Rename_File);
         when Delete_File =>
            DeleteFile'Write (S, V.Delete_File);
      end case;
   end Write_Document_Change;

   -------------------------------------------
   -- Write_VersionedTextDocumentIdentifier --
   -------------------------------------------

   procedure Write_VersionedTextDocumentIdentifier
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : VersionedTextDocumentIdentifier)
      renames LSP.Message_IO.Write_VersionedTextDocumentIdentifier;

   -------------------------
   -- Write_WatchKind_Set --
   -------------------------

   procedure Write_WatchKind_Set
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : WatchKind_Set)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
      Result : Integer := 0;
      Mask   : Integer := 1;
   begin
      if V /= Default_WatchKind_Set then
         for J in WatchKind loop
            if V (J) then
               Result := Result + Mask;
            end if;

            Mask := Mask * 2;
         end loop;

         JS.Write_Integer (Interfaces.Integer_64 (Result));
      end if;
   end Write_WatchKind_Set;

   -------------------------
   -- Write_WorkspaceEdit --
   -------------------------

   procedure Write_WorkspaceEdit
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : WorkspaceEdit)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      if V.documentChanges.Is_Empty then
         JS.Key ("changes");

         JS.Start_Object;
         for Cursor in V.changes.Iterate loop
            JS.Key
              (Ada.Strings.Wide_Unbounded.Unbounded_Wide_String
                 (TextDocumentEdit_Maps.Key (Cursor)));
            JS.Start_Array;
            for Edit of V.changes (Cursor) loop
               TextEdit'Write (S, Edit);
            end loop;
            JS.End_Array;
         end loop;
         JS.End_Object;
      else
         JS.Key ("documentChanges");
         Document_Change_Vector'Write (S, V.documentChanges);
      end if;
      JS.End_Object;
   end Write_WorkspaceEdit;

   -------------------------
   -- Read_ALSDebugParams --
   -------------------------

   procedure Read_ALSDebugParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ALSDebugParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant Ada.Strings.UTF_Encoding.UTF_8_String :=
               VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;

            if Key = "inputQueueLength" then
               LSP_Number'Read (S, V.inputQueueLength);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_ALSDebugParams;

   --------------------------
   -- Write_ALSDebugParams --
   --------------------------

   procedure Write_ALSDebugParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ALSDebugParams)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Write_Number (JS, +"inputQueueLength", V.inputQueueLength);
      JS.End_Object;
   end Write_ALSDebugParams;

   -----------------------------------
   -- Write_WorkDoneProgressOptions --
   -----------------------------------

   procedure Write_WorkDoneProgressOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : WorkDoneProgressOptions)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      --  In case we don't have workDoneProgress property let's write a boolean
      --  value instead of an empty object to be compatible with older protocol
      --  readers.

      if not V.workDoneProgress.Is_Set then
         JS.Write_Boolean (True);
      else
         JS.Start_Object;
         Write_Optional_Boolean (JS, +"workDoneProgress", V.workDoneProgress);
         JS.End_Object;
      end if;
   end Write_WorkDoneProgressOptions;

   --------------------------
   -- Read_Progress_Params --
   --------------------------

   procedure Read_Progress_Params
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Progress_Params)
   is
      procedure Read_Value;
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

      kind        : LSP_String;
      title       : LSP_String;
      cancellable : Optional_Boolean;
      message     : Optional_String;
      percentage  : Optional_Number;
      token       : LSP_Number_Or_String;

      procedure Read_Value is
      begin
         pragma Assert (JS.R.Is_Start_Object);
         JS.R.Read_Next;

         while not JS.R.Is_End_Object loop
            pragma Assert (JS.R.Is_Key_Name);
            declare
               Key : constant Ada.Strings.UTF_Encoding.UTF_8_String :=
                 VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
            begin
               JS.R.Read_Next;

               if Key = "kind" then
                  LSP.Types.Read (S, kind);
               elsif Key = "title" then
                  LSP.Types.Read (S, title);
               elsif Key = "cancellable" then
                  Optional_Boolean'Read (S, cancellable);
               elsif Key = "message" then
                  Optional_String'Read (S, message);
               elsif Key = "percentage" then
                  Optional_Number'Read (S, percentage);
               else
                  JS.Skip_Value;
               end if;
            end;
         end loop;
         JS.R.Read_Next;
      end Read_Value;

   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant Ada.Strings.UTF_Encoding.UTF_8_String :=
               VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;

            if Key = "token" then
               LSP_Number_Or_String'Read (S, token);
            elsif Key = "value" then
               Read_Value;
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;

      if kind = +"begin" then
         V := (Progress_Begin,
               (token, (kind, title, cancellable, message, percentage)));
      elsif kind = +"report" then
         V := (Progress_Report,
               (token, (kind, cancellable, message, percentage)));
      elsif kind = +"end" then
         V := (Progress_End,
               (token, (kind, message)));
      else
         --  Not implemented
         raise Program_Error;
      end if;
   end Read_Progress_Params;

   ---------------------------
   -- Write_Progress_Params --
   ---------------------------

   procedure Write_Progress_Params
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Progress_Params)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      case V.Kind is
         when Progress_Begin =>
            Write_Number_Or_String (JS, +"token", V.Begin_Param.token);
            JS.Key ("value");
            JS.Start_Object;
            Write_String (JS, +"kind", +"begin");
            Write_String (JS, +"title", V.Begin_Param.value.title);
            Write_Optional_Boolean (JS, +"cancellable",
                                    V.Begin_Param.value.cancellable);
            JS.Key ("message");
            Optional_String'Write (S, V.Begin_Param.value.message);
            JS.Key ("percentage");
            Optional_Number'Write (S, V.Begin_Param.value.percentage);
            JS.End_Object;
         when Progress_Report =>
            Write_Number_Or_String (JS, +"token", V.Report_Param.token);
            JS.Key ("value");
            JS.Start_Object;
            Write_String (JS, +"kind", +"report");
            Write_Optional_Boolean (JS, +"cancellable",
                                    V.Report_Param.value.cancellable);
            JS.Key ("message");
            Optional_String'Write (S, V.Report_Param.value.message);
            JS.Key ("percentage");
            Optional_Number'Write (S, V.Report_Param.value.percentage);
            JS.End_Object;
         when Progress_End =>
            Write_Number_Or_String (JS, +"token", V.End_Param.token);
            JS.Key ("value");
            JS.Start_Object;
            Write_String (JS, +"kind", +"end");
            JS.Key ("message");
            Optional_String'Write (S, V.End_Param.value.message);
            JS.End_Object;
      end case;

      JS.End_Object;
   end Write_Progress_Params;

end LSP.Messages;
