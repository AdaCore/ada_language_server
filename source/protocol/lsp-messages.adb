------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2018-2021, AdaCore                     --
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

with Ada.Strings.UTF_Encoding;
with Ada.Tags;  use Ada.Tags;
with Ada.Tags.Generic_Dispatching_Constructor;

with Interfaces;

with VSS.JSON.Pull_Readers;
with VSS.Strings.Conversions;
with VSS.JSON.Pull_Readers.Look_Ahead;

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

   procedure Read_CreateFile
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.CreateFile)
      renames LSP.Message_IO.Read_CreateFile;

   procedure Read_CreateFileOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.CreateFileOptions)
      renames LSP.Message_IO.Read_CreateFileOptions;

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

   procedure Read_LinkedEditingRanges
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LinkedEditingRanges)
      renames LSP.Message_IO.Read_LinkedEditingRanges;

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

   procedure Read_resolveSupportCapability
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out resolveSupportCapability)
      renames LSP.Message_IO.Read_resolveSupportCapability;

   procedure Read_ResourceOperationKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.ResourceOperationKind)
      renames LSP.Message_IO.Read_ResourceOperationKind;

   procedure Read_FileResourceChangeKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.FileResourceChangeKind)
      renames LSP.Message_IO.Read_FileResourceChangeKind;

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

   procedure Read_SymbolTag
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out SymbolTag)
      renames LSP.Message_IO.Read_SymbolTag;

   procedure Read_tagSupportCapability
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out tagSupportCapability)
      renames LSP.Message_IO.Read_tagSupportCapability;

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

   procedure Read_MarkdownClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out MarkdownClientCapabilities)
      renames LSP.Message_IO.Read_MarkdownClientCapabilities;

   procedure Read_GeneralClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out GeneralClientCapabilities)
      renames LSP.Message_IO.Read_GeneralClientCapabilities;

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

   procedure Read_RenameFile
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.RenameFile)
      renames LSP.Message_IO.Read_RenameFile;

   procedure Read_RenameFileOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.RenameFileOptions)
      renames LSP.Message_IO.Read_RenameFileOptions;

   procedure Read_RegularExpressionsClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out RegularExpressionsClientCapabilities)
      renames LSP.Message_IO.Read_RegularExpressionsClientCapabilities;

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

   procedure Read_ChangeAnnotation
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ChangeAnnotation)
      renames LSP.Message_IO.Read_ChangeAnnotation;

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

   procedure Read_PrepareSupportDefaultBehavior
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out PrepareSupportDefaultBehavior)
      renames LSP.Message_IO.Read_PrepareSupportDefaultBehavior;

   procedure Read_ProgramInfo
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.ProgramInfo)
      renames LSP.Message_IO.Read_ProgramInfo;

   procedure Read_TraceValue
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.TraceValue)
      renames LSP.Message_IO.Read_TraceValue;

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

   procedure Read_CodeDescription
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CodeDescription)
      renames LSP.Message_IO.Read_CodeDescription;

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

   procedure Read_SemanticTokensFullCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out SemanticTokensFullCapabilities)
      renames LSP.Message_IO.Read_SemanticTokensFullCapabilities;

   procedure Read_SemanticTokensRequestCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out SemanticTokensRequestCapabilities)
      renames LSP.Message_IO.Read_SemanticTokensRequestCapabilities;

   procedure Read_SemanticTokenTypes
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out SemanticTokenTypes)
      renames LSP.Message_IO.Read_SemanticTokenTypes;

   procedure Read_SemanticTokenModifiers
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out SemanticTokenModifiers)
      renames LSP.Message_IO.Read_SemanticTokenModifiers;

   procedure Read_SemanticTokensWorkspaceClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out SemanticTokensWorkspaceClientCapabilities)
      renames LSP.Message_IO.Read_SemanticTokensWorkspaceClientCapabilities;

   procedure Read_TokenFormat
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out TokenFormat)
      renames LSP.Message_IO.Read_TokenFormat;

   procedure Read_SemanticTokensClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out SemanticTokensClientCapabilities)
      renames LSP.Message_IO.Read_SemanticTokensClientCapabilities;

   procedure Read_SemanticTokensLegend
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out SemanticTokensLegend)
      renames LSP.Message_IO.Read_SemanticTokensLegend;

   procedure Read_SemanticTokensOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out SemanticTokensOptions)
      renames LSP.Message_IO.Read_SemanticTokensOptions;

   procedure Read_SemanticTokensParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out SemanticTokensParams)
      renames LSP.Message_IO.Read_SemanticTokensParams;

   procedure Read_SemanticTokens
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out SemanticTokens)
      renames LSP.Message_IO.Read_SemanticTokens;

   procedure Read_SemanticTokensPartialResult
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out SemanticTokensPartialResult)
      renames LSP.Message_IO.Read_SemanticTokensPartialResult;

   procedure Read_SemanticTokensDeltaParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out SemanticTokensDeltaParams)
      renames LSP.Message_IO.Read_SemanticTokensDeltaParams;

   procedure Read_SemanticTokensEdit
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out SemanticTokensEdit)
      renames LSP.Message_IO.Read_SemanticTokensEdit;

   procedure Read_SemanticTokensDelta
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out SemanticTokensDelta)
      renames LSP.Message_IO.Read_SemanticTokensDelta;

   procedure Read_SemanticTokensDeltaPartialResult
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out SemanticTokensDeltaPartialResult)
      renames LSP.Message_IO.Read_SemanticTokensDeltaPartialResult;

   procedure Read_SemanticTokensRangeParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out SemanticTokensRangeParams)
      renames LSP.Message_IO.Read_SemanticTokensRangeParams;

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

   procedure Read_Moniker
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Moniker)
      renames LSP.Message_IO.Read_Moniker;

   procedure Read_MonikerKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out MonikerKind)
      renames LSP.Message_IO.Read_MonikerKind;

   procedure Read_ShowDocumentClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ShowDocumentClientCapabilities)
      renames LSP.Message_IO.Read_ShowDocumentClientCapabilities;

   procedure Read_ShowDocumentParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ShowDocumentParams)
      renames LSP.Message_IO.Read_ShowDocumentParams;

   procedure Read_ShowDocumentResult
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ShowDocumentResult)
      renames LSP.Message_IO.Read_ShowDocumentResult;

   procedure Read_MessageActionItemCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out MessageActionItemCapabilities)
      renames LSP.Message_IO.Read_MessageActionItemCapabilities;

   procedure Read_ShowMessageParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.ShowMessageParams)
      renames LSP.Message_IO.Read_ShowMessageParams;

   procedure Read_ShowMessageRequestParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.ShowMessageRequestParams)
      renames LSP.Message_IO.Read_ShowMessageRequestParams;

   procedure Read_ShowMessageRequestClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ShowMessageRequestClientCapabilities)
      renames LSP.Message_IO.Read_ShowMessageRequestClientCapabilities;

   procedure Read_LogMessageParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.LogMessageParams)
      renames LSP.Message_IO.Read_LogMessageParams;

   procedure Read_LogTraceParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LogTraceParams)
      renames LSP.Message_IO.Read_LogTraceParams;

   procedure Read_SetTraceParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out SetTraceParams)
      renames LSP.Message_IO.Read_SetTraceParams;

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

   procedure Read_Disable_Reason
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Disable_Reason)
      renames LSP.Message_IO.Read_Disable_Reason;

   procedure Read_FileChangeType
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.FileChangeType)
      renames LSP.Message_IO.Read_FileChangeType;

   procedure Read_FileOperationsClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out FileOperationsClientCapabilities)
      renames LSP.Message_IO.Read_FileOperationsClientCapabilities;

   procedure Read_FileOperationPatternKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out FileOperationPatternKind)
      renames LSP.Message_IO.Read_FileOperationPatternKind;

   procedure Read_FileOperationPatternOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out FileOperationPatternOptions)
      renames LSP.Message_IO.Read_FileOperationPatternOptions;

   procedure Read_FileOperationPattern
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out FileOperationPattern)
      renames LSP.Message_IO.Read_FileOperationPattern;

   procedure Read_FileOperationFilter
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out FileOperationFilter)
      renames LSP.Message_IO.Read_FileOperationFilter;

   procedure Read_FileOperationRegistrationOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out FileOperationRegistrationOptions)
      renames LSP.Message_IO.Read_FileOperationRegistrationOptions;

   procedure Read_FileOperationsServerCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out FileOperationsServerCapabilities)
      renames LSP.Message_IO.Read_FileOperationsServerCapabilities;

   procedure Read_FileCreate
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out FileCreate)
      renames LSP.Message_IO.Read_FileCreate;

   procedure Read_CreateFilesParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CreateFilesParams)
      renames LSP.Message_IO.Read_CreateFilesParams;

   procedure Read_FileRename
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out FileRename)
      renames LSP.Message_IO.Read_FileRename;

   procedure Read_RenameFilesParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out RenameFilesParams)
      renames LSP.Message_IO.Read_RenameFilesParams;

   procedure Read_FileDelete
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out FileDelete)
      renames LSP.Message_IO.Read_FileDelete;

   procedure Read_DeleteFilesParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DeleteFilesParams)
      renames LSP.Message_IO.Read_DeleteFilesParams;

   procedure Read_DeleteFile
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.DeleteFile)
   renames LSP.Message_IO.Read_DeleteFile;

   procedure Read_DeleteFileOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.DeleteFileOptions)
   renames LSP.Message_IO.Read_DeleteFileOptions;

   procedure Read_FileEvent
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out FileEvent)
      renames LSP.Message_IO.Read_FileEvent;

   procedure Read_DidChangeWatchedFilesParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DidChangeWatchedFilesParams)
      renames LSP.Message_IO.Read_DidChangeWatchedFilesParams;

   procedure Read_PublishDiagnosticsParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.PublishDiagnosticsParams)
      renames LSP.Message_IO.Read_PublishDiagnosticsParams;

   procedure Read_InsertReplaceEdit
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out InsertReplaceEdit)
      renames LSP.Message_IO.Read_InsertReplaceEdit;

   procedure Read_InsertTextFormat
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.InsertTextFormat)
      renames LSP.Message_IO.Read_InsertTextFormat;

   procedure Read_InsertTextMode
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out InsertTextMode)
      renames LSP.Message_IO.Read_InsertTextMode;

   procedure Read_insertTextModeSupportCapability
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out insertTextModeSupportCapability)
      renames LSP.Message_IO.Read_insertTextModeSupportCapability;

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

   procedure Read_RegistrationParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out RegistrationParams)
      renames LSP.Message_IO.Read_RegistrationParams;

   procedure Read_DocumentHighlightKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.DocumentHighlightKind)
      renames LSP.Message_IO.Read_DocumentHighlightKind;

   procedure Read_DocumentHighlight
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.DocumentHighlight)
      renames LSP.Message_IO.Read_DocumentHighlight;

   procedure Read_DocumentSymbolOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DocumentSymbolOptions)
      renames LSP.Message_IO.Read_DocumentSymbolOptions;

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

   procedure Read_UniquenessLevel
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out UniquenessLevel)
      renames LSP.Message_IO.Read_UniquenessLevel;

   procedure Read_Unregistration
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Unregistration)
      renames LSP.Message_IO.Read_Unregistration;

   procedure Read_UnregistrationParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out UnregistrationParams)
      renames LSP.Message_IO.Read_UnregistrationParams;

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

   procedure Read_SignatureHelpTriggerKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out SignatureHelpTriggerKind)
      renames LSP.Message_IO.Read_SignatureHelpTriggerKind;

   procedure Read_SignatureHelpContext
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out SignatureHelpContext)
      renames LSP.Message_IO.Read_SignatureHelpContext;

   procedure Read_SignatureHelpParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out SignatureHelpParams)
      renames LSP.Message_IO.Read_SignatureHelpParams;

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

   procedure Read_AnnotationSupport
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out AnnotationSupport)
      renames LSP.Message_IO.Read_AnnotationSupport;

   procedure Read_AnnotatedTextEdit
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out AnnotatedTextEdit)
      renames LSP.Message_IO.Read_AnnotatedTextEdit;

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

   procedure Read_ALS_Source_Dir_Description
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ALS_Source_Dir_Description)
      renames LSP.Message_IO.Read_ALS_Source_Dir_Description;

   procedure Read_ALS_Unit_Description
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.ALS_Unit_Description)
      renames LSP.Message_IO.Read_ALS_Unit_Description;

   procedure Read_ALS_ShowDependenciesKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ALS_ShowDependenciesKind)
     renames LSP.Message_IO.Read_ALS_ShowDependenciesKind;

   procedure Read_TextDocumentRegistrationOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out TextDocumentRegistrationOptions)
     renames LSP.Message_IO.Read_TextDocumentRegistrationOptions;

   procedure Read_TextDocumentChangeRegistrationOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out TextDocumentChangeRegistrationOptions)
     renames LSP.Message_IO.Read_TextDocumentChangeRegistrationOptions;

   procedure Read_TextDocumentSaveRegistrationOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out TextDocumentSaveRegistrationOptions)
     renames LSP.Message_IO.Read_TextDocumentSaveRegistrationOptions;

   procedure Read_CompletionRegistrationOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CompletionRegistrationOptions)
     renames LSP.Message_IO.Read_CompletionRegistrationOptions;

   procedure Read_SignatureHelpRegistrationOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out SignatureHelpRegistrationOptions)
     renames LSP.Message_IO.Read_SignatureHelpRegistrationOptions;

   procedure Read_CodeLensRegistrationOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CodeLensRegistrationOptions)
     renames LSP.Message_IO.Read_CodeLensRegistrationOptions;

   procedure Read_CodeLensWorkspaceClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CodeLensWorkspaceClientCapabilities)
     renames LSP.Message_IO.Read_CodeLensWorkspaceClientCapabilities;

   procedure Read_DocumentOnTypeFormattingRegistrationOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DocumentOnTypeFormattingRegistrationOptions)
     renames LSP.Message_IO.Read_DocumentOnTypeFormattingRegistrationOptions;

   procedure Read_ExecuteCommandRegistrationOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ExecuteCommandRegistrationOptions)
     renames LSP.Message_IO.Read_ExecuteCommandRegistrationOptions;

   procedure Read_CodeActionRegistrationOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CodeActionRegistrationOptions)
     renames LSP.Message_IO.Read_CodeActionRegistrationOptions;

   procedure Read_RenameRegistrationOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out RenameRegistrationOptions)
     renames LSP.Message_IO.Read_RenameRegistrationOptions;

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

   procedure Write_CreateFile
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.CreateFile)
     renames LSP.Message_IO.Write_CreateFile;

   procedure Write_CreateFileOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.CreateFileOptions)
      renames LSP.Message_IO.Write_CreateFileOptions;

   procedure Write_DeleteFile
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.DeleteFile)
     renames LSP.Message_IO.Write_DeleteFile;

   procedure Write_DeleteFileOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.DeleteFileOptions)
      renames LSP.Message_IO.Write_DeleteFileOptions;

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

   procedure Write_LinkedEditingRanges
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LinkedEditingRanges)
      renames LSP.Message_IO.Write_LinkedEditingRanges;

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

   procedure Write_RenameFile
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.RenameFile)
      renames LSP.Message_IO.Write_RenameFile;

   procedure Write_RenameFileOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.RenameFileOptions)
      renames LSP.Message_IO.Write_RenameFileOptions;

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

   procedure Write_resolveSupportCapability
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : resolveSupportCapability)
      renames LSP.Message_IO.Write_resolveSupportCapability;

   procedure Write_ResourceOperationKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.ResourceOperationKind)
      renames LSP.Message_IO.Write_ResourceOperationKind;

   procedure Write_FileResourceChangeKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.FileResourceChangeKind)
      renames LSP.Message_IO.Write_FileResourceChangeKind;

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

   procedure Write_SymbolTag
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SymbolTag)
      renames LSP.Message_IO.Write_SymbolTag;

   procedure Write_tagSupportCapability
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : tagSupportCapability)
      renames LSP.Message_IO.Write_tagSupportCapability;

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

   procedure Write_MarkdownClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : MarkdownClientCapabilities)
      renames LSP.Message_IO.Write_MarkdownClientCapabilities;

   procedure Write_GeneralClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : GeneralClientCapabilities)
      renames LSP.Message_IO.Write_GeneralClientCapabilities;

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

   procedure Write_RegularExpressionsClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : RegularExpressionsClientCapabilities)
      renames LSP.Message_IO.Write_RegularExpressionsClientCapabilities;

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

   procedure Write_ChangeAnnotation
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ChangeAnnotation)
      renames LSP.Message_IO.Write_ChangeAnnotation;

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

   procedure Write_PrepareSupportDefaultBehavior
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : PrepareSupportDefaultBehavior)
      renames LSP.Message_IO.Write_PrepareSupportDefaultBehavior;

   procedure Write_ProgramInfo
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.ProgramInfo)
      renames LSP.Message_IO.Write_ProgramInfo;

   procedure Write_TraceValue
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.TraceValue)
      renames LSP.Message_IO.Write_TraceValue;

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

   procedure Write_CodeDescription
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CodeDescription)
      renames LSP.Message_IO.Write_CodeDescription;

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

   procedure Write_SemanticTokensFullCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SemanticTokensFullCapabilities)
      renames LSP.Message_IO.Write_SemanticTokensFullCapabilities;

   procedure Write_SemanticTokensRequestCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SemanticTokensRequestCapabilities)
      renames LSP.Message_IO.Write_SemanticTokensRequestCapabilities;

   procedure Write_SemanticTokenTypes
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SemanticTokenTypes)
      renames LSP.Message_IO.Write_SemanticTokenTypes;

   procedure Write_SemanticTokenModifiers
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SemanticTokenModifiers)
      renames LSP.Message_IO.Write_SemanticTokenModifiers;

   procedure Write_SemanticTokensWorkspaceClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SemanticTokensWorkspaceClientCapabilities)
      renames LSP.Message_IO.Write_SemanticTokensWorkspaceClientCapabilities;

   procedure Write_TokenFormat
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : TokenFormat)
      renames LSP.Message_IO.Write_TokenFormat;

   procedure Write_SemanticTokensClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SemanticTokensClientCapabilities)
      renames LSP.Message_IO.Write_SemanticTokensClientCapabilities;

   procedure Write_SemanticTokensLegend
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SemanticTokensLegend)
      renames LSP.Message_IO.Write_SemanticTokensLegend;

   procedure Write_SemanticTokensOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SemanticTokensOptions)
      renames LSP.Message_IO.Write_SemanticTokensOptions;

   procedure Write_SemanticTokensParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SemanticTokensParams)
      renames LSP.Message_IO.Write_SemanticTokensParams;

   procedure Write_SemanticTokens
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SemanticTokens)
      renames LSP.Message_IO.Write_SemanticTokens;

   procedure Write_SemanticTokensPartialResult
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SemanticTokensPartialResult)
      renames LSP.Message_IO.Write_SemanticTokensPartialResult;

   procedure Write_SemanticTokensDeltaParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SemanticTokensDeltaParams)
      renames LSP.Message_IO.Write_SemanticTokensDeltaParams;

   procedure Write_SemanticTokensEdit
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SemanticTokensEdit)
      renames LSP.Message_IO.Write_SemanticTokensEdit;

   procedure Write_SemanticTokensDelta
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SemanticTokensDelta)
      renames LSP.Message_IO.Write_SemanticTokensDelta;

   procedure Write_SemanticTokensDeltaPartialResult
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SemanticTokensDeltaPartialResult)
      renames LSP.Message_IO.Write_SemanticTokensDeltaPartialResult;

   procedure Write_SemanticTokensRangeParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SemanticTokensRangeParams)
      renames LSP.Message_IO.Write_SemanticTokensRangeParams;

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

   procedure Write_Moniker
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Moniker)
      renames LSP.Message_IO.Write_Moniker;

   procedure Write_MonikerKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : MonikerKind)
      renames LSP.Message_IO.Write_MonikerKind;

   procedure Write_ShowDocumentClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ShowDocumentClientCapabilities)
      renames LSP.Message_IO.Write_ShowDocumentClientCapabilities;

   procedure Write_ShowDocumentParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ShowDocumentParams)
      renames LSP.Message_IO.Write_ShowDocumentParams;

   procedure Write_ShowDocumentResult
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ShowDocumentResult)
      renames LSP.Message_IO.Write_ShowDocumentResult;

   procedure Write_MessageActionItemCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : MessageActionItemCapabilities)
      renames LSP.Message_IO.Write_MessageActionItemCapabilities;

   procedure Write_ShowMessageParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.ShowMessageParams)
      renames LSP.Message_IO.Write_ShowMessageParams;

   procedure Write_ShowMessageRequestParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.ShowMessageRequestParams)
      renames LSP.Message_IO.Write_ShowMessageRequestParams;

   procedure Write_ShowMessageRequestClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ShowMessageRequestClientCapabilities)
      renames LSP.Message_IO.Write_ShowMessageRequestClientCapabilities;

   procedure Write_LogMessageParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.LogMessageParams)
      renames LSP.Message_IO.Write_LogMessageParams;

   procedure Write_LogTraceParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LogTraceParams)
      renames LSP.Message_IO.Write_LogTraceParams;

   procedure Write_SetTraceParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SetTraceParams)
      renames LSP.Message_IO.Write_SetTraceParams;

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

   procedure Write_Disable_Reason
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Disable_Reason)
      renames LSP.Message_IO.Write_Disable_Reason;

   procedure Write_FileChangeType
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.FileChangeType)
      renames LSP.Message_IO.Write_FileChangeType;

   procedure Write_FileOperationPatternKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : FileOperationPatternKind)
      renames LSP.Message_IO.Write_FileOperationPatternKind;

   procedure Write_FileOperationPatternOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : FileOperationPatternOptions)
      renames LSP.Message_IO.Write_FileOperationPatternOptions;

   procedure Write_FileOperationPattern
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : FileOperationPattern)
      renames LSP.Message_IO.Write_FileOperationPattern;

   procedure Write_FileOperationFilter
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : FileOperationFilter)
      renames LSP.Message_IO.Write_FileOperationFilter;

   procedure Write_FileOperationRegistrationOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : FileOperationRegistrationOptions)
      renames LSP.Message_IO.Write_FileOperationRegistrationOptions;

   procedure Write_FileOperationsServerCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : FileOperationsServerCapabilities)
      renames LSP.Message_IO.Write_FileOperationsServerCapabilities;

   procedure Write_FileOperationsClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : FileOperationsClientCapabilities)
      renames LSP.Message_IO.Write_FileOperationsClientCapabilities;

   procedure Write_FileCreate
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : FileCreate)
      renames LSP.Message_IO.Write_FileCreate;

   procedure Write_CreateFilesParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CreateFilesParams)
      renames LSP.Message_IO.Write_CreateFilesParams;

   procedure Write_FileRename
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : FileRename)
      renames LSP.Message_IO.Write_FileRename;

   procedure Write_RenameFilesParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : RenameFilesParams)
      renames LSP.Message_IO.Write_RenameFilesParams;

   procedure Write_FileDelete
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : FileDelete)
      renames LSP.Message_IO.Write_FileDelete;

   procedure Write_DeleteFilesParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DeleteFilesParams)
      renames LSP.Message_IO.Write_DeleteFilesParams;

   procedure Write_FileEvent
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : FileEvent)
      renames LSP.Message_IO.Write_FileEvent;

   procedure Write_DidChangeWatchedFilesParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DidChangeWatchedFilesParams)
      renames LSP.Message_IO.Write_DidChangeWatchedFilesParams;

   procedure Write_PublishDiagnosticsParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.PublishDiagnosticsParams)
      renames LSP.Message_IO.Write_PublishDiagnosticsParams;

   procedure Write_InsertReplaceEdit
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : InsertReplaceEdit)
      renames LSP.Message_IO.Write_InsertReplaceEdit;

   procedure Write_InsertTextFormat
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.InsertTextFormat)
      renames LSP.Message_IO.Write_InsertTextFormat;

   procedure Write_InsertTextMode
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : InsertTextMode)
      renames LSP.Message_IO.Write_InsertTextMode;

   procedure Write_insertTextModeSupportCapability
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : insertTextModeSupportCapability)
      renames LSP.Message_IO.Write_insertTextModeSupportCapability;

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

   procedure Write_Registration
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Registration)
      renames LSP.Message_IO.Write_Registration;

   procedure Write_RegistrationParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : RegistrationParams)
      renames LSP.Message_IO.Write_RegistrationParams;

   procedure Write_DocumentHighlightKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.DocumentHighlightKind)
      renames LSP.Message_IO.Write_DocumentHighlightKind;

   procedure Write_DocumentHighlight
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.DocumentHighlight)
      renames LSP.Message_IO.Write_DocumentHighlight;

   procedure Write_DocumentSymbolOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DocumentSymbolOptions)
      renames LSP.Message_IO.Write_DocumentSymbolOptions;

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

   procedure Write_SignatureHelpTriggerKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SignatureHelpTriggerKind)
      renames LSP.Message_IO.Write_SignatureHelpTriggerKind;

   procedure Write_SignatureHelpContext
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SignatureHelpContext)
      renames LSP.Message_IO.Write_SignatureHelpContext;

   procedure Write_SignatureHelpParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SignatureHelpParams)
     renames LSP.Message_IO.Write_SignatureHelpParams;

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

   procedure Write_AnnotationSupport
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : AnnotationSupport)
      renames LSP.Message_IO.Write_AnnotationSupport;

   procedure Write_AnnotatedTextEdit
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : AnnotatedTextEdit)
      renames LSP.Message_IO.Write_AnnotatedTextEdit;

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

   procedure Write_UniquenessLevel
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : UniquenessLevel)
      renames LSP.Message_IO.Write_UniquenessLevel;

   procedure Write_Unregistration
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Unregistration)
      renames LSP.Message_IO.Write_Unregistration;

   procedure Write_UnregistrationParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : UnregistrationParams)
      renames LSP.Message_IO.Write_UnregistrationParams;

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

   procedure Write_ALS_Source_Dir_Description
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ALS_Source_Dir_Description)
     renames LSP.Message_IO.Write_ALS_Source_Dir_Description;

   procedure Write_ALS_Unit_Description
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.ALS_Unit_Description)
      renames LSP.Message_IO.Write_ALS_Unit_Description;

   procedure Write_TextDocumentRegistrationOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : TextDocumentRegistrationOptions)
      renames LSP.Message_IO.Write_TextDocumentRegistrationOptions;

   procedure Write_TextDocumentChangeRegistrationOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : TextDocumentChangeRegistrationOptions)
      renames LSP.Message_IO.Write_TextDocumentChangeRegistrationOptions;

   procedure Write_TextDocumentSaveRegistrationOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : TextDocumentSaveRegistrationOptions)
      renames LSP.Message_IO.Write_TextDocumentSaveRegistrationOptions;

   procedure Write_CompletionRegistrationOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CompletionRegistrationOptions)
      renames LSP.Message_IO.Write_CompletionRegistrationOptions;

   procedure Write_SignatureHelpRegistrationOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SignatureHelpRegistrationOptions)
      renames LSP.Message_IO.Write_SignatureHelpRegistrationOptions;

   procedure Write_CodeLensRegistrationOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CodeLensRegistrationOptions)
      renames LSP.Message_IO.Write_CodeLensRegistrationOptions;

   procedure Write_CodeLensWorkspaceClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CodeLensWorkspaceClientCapabilities)
      renames LSP.Message_IO.Write_CodeLensWorkspaceClientCapabilities;

   procedure Write_DocumentOnTypeFormattingRegistrationOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DocumentOnTypeFormattingRegistrationOptions)
      renames LSP.Message_IO.Write_DocumentOnTypeFormattingRegistrationOptions;

   procedure Write_ExecuteCommandRegistrationOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ExecuteCommandRegistrationOptions)
      renames LSP.Message_IO.Write_ExecuteCommandRegistrationOptions;

   procedure Write_CodeActionRegistrationOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CodeActionRegistrationOptions)
      renames LSP.Message_IO.Write_CodeActionRegistrationOptions;

   procedure Write_RenameRegistrationOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : RenameRegistrationOptions)
      renames LSP.Message_IO.Write_RenameRegistrationOptions;

   procedure Read_Search_Kind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Search_Kind)
      renames LSP.Message_IO.Read_Search_Kind;

   procedure Write_Search_Kind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Search_Kind)
     renames LSP.Message_IO.Write_Search_Kind;

   ---------
   -- "=" --
   ---------

   overriding function "=" (Left, Right : Location) return Boolean is
   begin
      return
        LSP.Types.Equal (Left.uri, Right.uri)
        and Left.span = Right.span
        and Left.alsKind = Right.alsKind;
   end "=";

   -------------------
   -- Method_To_Tag --
   -------------------

   function Method_To_Tag
     (Map    : Maps.Map;
      Method : VSS.Strings.Virtual_String) return Ada.Tags.Tag
   is
      Cursor : constant Maps.Cursor := Map.Find (Method);

   begin
      if Maps.Has_Element (Cursor) then
         return Maps.Element (Cursor);
      else
         return Ada.Tags.No_Tag;
      end if;
   end Method_To_Tag;

   function Empty_Set return AlsReferenceKind_Set is
      (Is_Server_Side => True, As_Flags => (others => False));

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
               Item : VSS.Strings.Virtual_String;

            begin
               LSP.Types.Read_String (S, Item);
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
            LSP.Types.Write_String_Vector (S, V.As_Strings);
         end if;

      end if;
   end Write_AlsReferenceKind_Set;

   ----------------------------------------
   -- Read_TextEdit_Or_InsertReplaceEdit --
   ----------------------------------------

   procedure Read_TextEdit_Or_InsertReplaceEdit
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out TextEdit_Or_InsertReplaceEdit)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

      Found   : Boolean := False;  --  True as we know the actual type
      newText : VSS.Strings.Virtual_String;  --  newText value if not Found

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
            if Key = "newText" then
               if not Found then
                  LSP.Types.Read_String (S, newText);
               elsif V.Is_TextEdit then
                  LSP.Types.Read_String (S, V.TextEdit.newText);
               else
                  LSP.Types.Read_String (S, V.InsertReplaceEdit.newText);
               end if;
            elsif Key = "insert" then
               if not Found then
                  Found := True;

                  V := (Is_TextEdit       => False,
                        InsertReplaceEdit =>
                          (newText     => newText,
                           others      => <>));
               end if;

               Span'Read (S, V.InsertReplaceEdit.insert);
            elsif Key = "replace" then
               if not Found then
                  Found := True;

                  V := (Is_TextEdit       => False,
                        InsertReplaceEdit =>
                          (newText     => newText,
                           others      => <>));
               end if;

               Span'Read (S, V.InsertReplaceEdit.replace);
            elsif Key = "range" then
               if not Found then
                  Found := True;

                  V := (Is_TextEdit => True,
                        TextEdit    =>
                          (newText     => newText,
                           others      => <>));
               end if;

               Span'Read (S, V.TextEdit.span);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_TextEdit_Or_InsertReplaceEdit;

   -----------------------------------------
   -- Write_TextEdit_Or_InsertReplaceEdit --
   -----------------------------------------

   procedure Write_TextEdit_Or_InsertReplaceEdit
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : TextEdit_Or_InsertReplaceEdit) is
   begin
      if V.Is_TextEdit then
         TextEdit'Write (S, V.TextEdit);
      else
         InsertReplaceEdit'Write (S, V.InsertReplaceEdit);
      end if;
   end Write_TextEdit_Or_InsertReplaceEdit;

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
                     V.title := VSS.Strings.Empty_Virtual_String;
                  end if;
                  LSP.Types.Read_String (S, V.command.Value.command);
               else
                  Optional_Command'Read (S, V.command);
                  Is_CodeAction := True;
               end if;
            elsif Key = "title" then
               if V.command.Is_Set and not Is_CodeAction then
                  LSP.Types.Read_String (S, V.command.Value.title);
               else
                  LSP.Types.Read_String (S, V.title);
               end if;
            elsif Key = "arguments" then
               --  "arguments" field is part of Command
               if not V.command.Is_Set then
                  V.command := (Is_Set => True, Value => <>);
                  V.command.Value.title := V.title;
                  V.title := VSS.Strings.Empty_Virtual_String;
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
            elsif Key = "disabled" then
               Optional_Disable_Reason'Read (S, V.disabled);
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

      Look_Ahead : aliased
        VSS.JSON.Pull_Readers.Look_Ahead.JSON_Look_Ahead_Reader (JS.R);
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
               LSP.Types.Read_String (Nested'Access, V.command);
            elsif Key = "title" then
               LSP.Types.Read_String (Nested'Access, V.title);
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
                  LSP.Types.Read_String (S, Item.name);
               elsif Key = "detail" then
                  Optional_Virtual_String'Read (S, Item.detail);
               elsif Key = "kind" then
                  SymbolKind'Read (S, Item.kind);
               elsif Key = "tags" then
                  SymbolTagSet'Read (S, Item.tags);
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

      Look_Ahead : aliased
        VSS.JSON.Pull_Readers.Look_Ahead.JSON_Look_Ahead_Reader (JS.R);
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
               LSP.Types.Read_String (Nested'Access, V.command);
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
      Look_Ahead : aliased
        VSS.JSON.Pull_Readers.Look_Ahead.JSON_Look_Ahead_Reader (JS.R);
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
         when VSS.JSON.Pull_Readers.String_Value =>
            V := (Is_String => True,
                  value     => JS.R.String_Value);

            JS.R.Read_Next;
         when VSS.JSON.Pull_Readers.Start_Object =>
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
                     LSP.Types.Read_String (S, V.language);
                  elsif Key = "value" then
                     LSP.Types.Read_String (S, V.value);
                  else
                     JS.Skip_Value;
                  end if;
               end;
            end loop;
            JS.R.Read_Next;

         when others =>
            --  Unexpected JSON event
            V := (Is_String => True, value => <>);
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
         Look_Ahead : aliased
           VSS.JSON.Pull_Readers.Look_Ahead.JSON_Look_Ahead_Reader (JS.R);
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
         when VSS.JSON.Pull_Readers.String_Value =>

            V := (Is_MarkupContent => False,
                  Vector           => <>);
            V.Vector.Append
              (MarkedString'(Is_String => True,
                             value     => JS.R.String_Value));
            JS.R.Read_Next;
         when VSS.JSON.Pull_Readers.Start_Array =>
            V := (Is_MarkupContent => False,
                  Vector           => <>);
            MarkedString_Vector'Read (S, V.Vector);
         when VSS.JSON.Pull_Readers.Start_Object =>
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
         when VSS.JSON.Pull_Readers.Null_Value =>
            V := (False, False);
            JS.R.Read_Next;
         when VSS.JSON.Pull_Readers.Start_Object =>
            V := (True, False, others => <>);
            TextDocumentSyncOptions'Read (S, V.Options);
         when VSS.JSON.Pull_Readers.Number_Value =>
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
         when VSS.JSON.Pull_Readers.String_Value =>
            V := (Is_String => True,
                  String    => JS.R.String_Value);
            JS.R.Read_Next;
         when VSS.JSON.Pull_Readers.Start_Array =>
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
         when VSS.JSON.Pull_Readers.Boolean_Value =>
            V := (Is_Boolean => True,
                  Bool       => JS.R.Boolean_Value);
            JS.R.Read_Next;
         when VSS.JSON.Pull_Readers.Start_Object =>
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
         when VSS.JSON.Pull_Readers.String_Value =>
            V := (Is_String => True,
                  String    => JS.R.String_Value);
            JS.R.Read_Next;

         when VSS.JSON.Pull_Readers.Start_Object =>
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
      V : out Document_Change)
   is
      JS         : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
      Look_Ahead : aliased
        VSS.JSON.Pull_Readers.Look_Ahead.JSON_Look_Ahead_Reader (JS.R);
      Nested     : aliased LSP.JSON_Streams.JSON_Stream
        (JS.Is_Server_Side, Look_Ahead'Unchecked_Access);

   begin
      pragma Assert (Look_Ahead.Is_Start_Object);
      Look_Ahead.Read_Next;

      while Look_Ahead.Is_Key_Name loop
         declare
            use type VSS.Strings.Virtual_String;

            Key   : constant Ada.Strings.UTF_Encoding.UTF_8_String :=
              VSS.Strings.Conversions.To_UTF_8_String (Look_Ahead.Key_Name);
            Value : VSS.Strings.Virtual_String;

         begin
            Look_Ahead.Read_Next;

            if Key = "kind" then
               --  CreateFile, RenameFile or DeleteFile

               LSP.Types.Read_String (Nested'Unchecked_Access, Value);

               if Value = "create" then
                  V := (Kind => Create_File, others => <>);
                  Look_Ahead.Rewind;
                  CreateFile'Read (Nested'Unchecked_Access, V.Create_File);

                  return;

               elsif Value = "rename" then
                  V := (Kind => Rename_File, others => <>);
                  Look_Ahead.Rewind;
                  RenameFile'Read (Nested'Unchecked_Access, V.Rename_File);

                  return;

               elsif Value = "delete" then
                  V := (Kind => Delete_File, others => <>);
                  Look_Ahead.Rewind;
                  DeleteFile'Read (Nested'Unchecked_Access, V.Delete_File);

                  return;

               else
                  raise Constraint_Error;
               end if;

            else
               Nested.Skip_Value;
            end if;
         end;
      end loop;

      --  There is no "kind" field found, object represents TextDocumentEdit.

      V := (Text_Document_Edit, others => <>);
      Look_Ahead.Rewind;
      TextDocumentEdit'Read (Nested'Unchecked_Access, V.Text_Document_Edit);
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
      Look_Ahead : aliased
        VSS.JSON.Pull_Readers.Look_Ahead.JSON_Look_Ahead_Reader (JS.R);
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
      Command : VSS.Strings.Virtual_String;

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
               LSP.Types.Read_String (JS'Access, Command);

               if JS.Is_Server_Side then
                  Tag := Ada.Tags.Internal_Tag
                    (VSS.Strings.Conversions.To_UTF_8_String (Command));

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

   procedure Read_OptionalVersionedTextDocumentIdentifier
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out OptionalVersionedTextDocumentIdentifier)
      renames LSP.Message_IO.Read_OptionalVersionedTextDocumentIdentifier;

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
         when VSS.JSON.Pull_Readers.Number_Value =>
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
      procedure Each_Annotation (Name : VSS.Strings.Virtual_String);

      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

      ----------
      -- Each --
      ----------

      procedure Each (Name : VSS.Strings.Virtual_String) is
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

         V.changes.Insert (LSP.Types.To_LSP_URI (Name), Vector);
      end Each;

      ---------------------
      -- Each_Annotation --
      ---------------------

      procedure Each_Annotation (Name : VSS.Strings.Virtual_String) is
         Item : ChangeAnnotation;

      begin
         JS.R.Read_Next;  --  Skip Key
         ChangeAnnotation'Read (S, Item);
         V.changeAnnotations.Insert (Name, Item);
      end Each_Annotation;

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

            elsif Key = "changeAnnotations" then
               pragma Assert (JS.R.Is_Start_Object);
               JS.R.Read_Next;

               while not JS.R.Is_End_Object loop
                  pragma Assert (JS.R.Is_Key_Name);
                  Each_Annotation (JS.R.String_Value);
               end loop;

               JS.R.Read_Next;

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
      if V.title.Is_Empty and then V.command.Is_Set then
         Optional_Command'Write (S, V.command);
      else
         JS.Start_Object;
         Write_String (JS, "title", V.title);
         JS.Key ("kind");
         Optional_CodeActionKind'Write (S, V.kind);
         JS.Key ("diagnostics");
         Optional_Diagnostic_Vector'Write (S, V.diagnostics);
         Write_Optional_Boolean (JS, "isPreferred", V.isPreferred);
         JS.Key ("disabled");
         Optional_Disable_Reason'Write (S, V.disabled);
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
      if V.Is_Unknown and then V.command.Is_Empty then
         return;
      end if;

      JS.Start_Object;
      Write_String (JS, "title", V.title);

      if V.Is_Unknown then
         Write_String (JS, "command", V.command);
         JS.Key ("arguments");
         Optional_Any_Vector'Write (S, V.arguments);
      else
         Write_String
           (JS,
            "command",
            VSS.Strings.Conversions.To_Virtual_String
              (Ada.Tags.External_Tag (V.Custom.Unchecked_Get'Tag)));
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
               Write_String (JS, "name", Item.name);
               JS.Key ("detail");
               Optional_Virtual_String'Write (S, Item.detail);
               JS.Key ("kind");
               SymbolKind'Write (S, Item.kind);
               JS.Key ("tags");
               SymbolTagSet'Write (S, Item.tags);
               Write_Optional_Boolean (JS, "deprecated", Item.deprecated);
               JS.Key ("range");
               Span'Write (S, Item.span);
               JS.Key ("selectionRange");
               Span'Write (S, Item.selectionRange);
               Write_Optional_Boolean
                 (JS, "alsIsDeclaration", Item.alsIsDeclaration);
               Write_Optional_Boolean
                   (JS, "alsIsAdaProcedure", Item.alsIsAdaProcedure);
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
         Write_String (JS, "command", V.command);
      else
         JS.Start_Array;
         Temp := LSP.Commands.Command_Access (V.Custom.Unchecked_Get);
         --  This Temp variable prevents compiler from a crash.
         LSP.Commands.Command'Class'Write (S, Temp.all);
         JS.End_Array;
         Write_String (JS, "command", V.command);
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
         Write_String (JS, "language", V.language);
         Write_String (JS, "value", V.value);
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
      Write_String (JS, "jsonrpc", V.jsonrpc);
      Write_Number_Or_String (JS, "id", V.id);
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

   procedure Write_OptionalVersionedTextDocumentIdentifier
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : OptionalVersionedTextDocumentIdentifier)
      renames LSP.Message_IO.Write_OptionalVersionedTextDocumentIdentifier;

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
              (LSP.Types.To_Virtual_String
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

      if not V.changeAnnotations.Is_Empty then
         JS.Key ("changeAnnotations");
         JS.Start_Object;

         for J in V.changeAnnotations.Iterate loop
            JS.Key (ChangeAnnotation_Maps.Key (J));
            ChangeAnnotation'Write (S, ChangeAnnotation_Maps.Element (J));
         end loop;

         JS.End_Object;
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
      Write_Number (JS, "inputQueueLength", V.inputQueueLength);
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
         Write_Optional_Boolean (JS, "workDoneProgress", V.workDoneProgress);
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
      use type VSS.Strings.Virtual_String;

      procedure Read_Value;
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

      kind        : VSS.Strings.Virtual_String;
      title       : VSS.Strings.Virtual_String;
      cancellable : Optional_Boolean;
      message     : Optional_Virtual_String;
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
                  LSP.Types.Read_String (S, kind);
               elsif Key = "title" then
                  LSP.Types.Read_String (S, title);
               elsif Key = "cancellable" then
                  Optional_Boolean'Read (S, cancellable);
               elsif Key = "message" then
                  Optional_Virtual_String'Read (S, message);
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

      if kind = "begin" then
         V := (Progress_Begin,
               (token, (kind, title, cancellable, message, percentage)));
      elsif kind = "report" then
         V := (Progress_Report,
               (token, (kind, cancellable, message, percentage)));
      elsif kind = "end" then
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
            Write_Number_Or_String (JS, "token", V.Begin_Param.token);
            JS.Key ("value");
            JS.Start_Object;
            Write_String (JS, "kind", "begin");
            Write_String (JS, "title", V.Begin_Param.value.title);
            Write_Optional_Boolean (JS, "cancellable",
                                    V.Begin_Param.value.cancellable);
            JS.Key ("message");
            Optional_Virtual_String'Write (S, V.Begin_Param.value.message);
            JS.Key ("percentage");
            Optional_Number'Write (S, V.Begin_Param.value.percentage);
            JS.End_Object;
         when Progress_Report =>
            Write_Number_Or_String (JS, "token", V.Report_Param.token);
            JS.Key ("value");
            JS.Start_Object;
            Write_String (JS, "kind", "report");
            Write_Optional_Boolean (JS, "cancellable",
                                    V.Report_Param.value.cancellable);
            JS.Key ("message");
            Optional_Virtual_String'Write (S, V.Report_Param.value.message);
            JS.Key ("percentage");
            Optional_Number'Write (S, V.Report_Param.value.percentage);
            JS.End_Object;
         when Progress_End =>
            Write_Number_Or_String (JS, "token", V.End_Param.token);
            JS.Key ("value");
            JS.Start_Object;
            Write_String (JS, "kind", "end");
            JS.Key ("message");
            Optional_Virtual_String'Write (S, V.End_Param.value.message);
            JS.End_Object;
      end case;

      JS.End_Object;
   end Write_Progress_Params;

   --------------------------------------------
   -- Read_Progress_SymbolInformation_Vector --
   --------------------------------------------

   procedure Read_Progress_SymbolInformation_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Progress_SymbolInformation_Vector)
   is
      use type VSS.Strings.Virtual_String;

      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);

         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "token" then
               LSP.Types.Read_LSP_Number_Or_String (S, V.token);
            elsif Key = "value" then
               SymbolInformation_Vector'Read (S, V.value);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_Progress_SymbolInformation_Vector;

   ---------------------------------------------
   -- Write_Progress_SymbolInformation_Vector --
   ---------------------------------------------

   procedure Write_Progress_SymbolInformation_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Progress_SymbolInformation_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("token");
      LSP.Types.Write_LSP_Number_Or_String (S, V.token);
      JS.Key ("value");
      SymbolInformation_Vector'Write (S, V.value);
      JS.End_Object;
   end Write_Progress_SymbolInformation_Vector;

   ------------------------------
   -- Read_Registration_Option --
   ------------------------------

   procedure Read_Registration_Option
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Registration_Option) is
   begin
      case V.Kind is
         when Absent =>
            null;
         when Text_Document_Registration_Option =>
            TextDocumentRegistrationOptions'Read (S, V.Text_Document);
         when Text_Document_Change_Registration_Option =>
            TextDocumentChangeRegistrationOptions'Read
              (S, V.Text_Document_Change);
         when Text_Document_Save_Registration_Option =>
            TextDocumentSaveRegistrationOptions'Read (S, V.Text_Document_Save);
         when Completion_Registration_Option =>
            CompletionRegistrationOptions'Read (S, V.Completion);
         when Signature_Help_Registration_Option =>
            SignatureHelpRegistrationOptions'Read (S, V.SignatureHelp);
         when Code_Lens_Registration_Option =>
            CodeLensRegistrationOptions'Read (S, V.CodeLens);
         when Document_Link_Registration_Option =>
            DocumentLinkRegistrationOptions'Read (S, V.DocumentLink);
         when Document_On_Type_Formatting_Registration_Option =>
            DocumentOnTypeFormattingRegistrationOptions'Read
              (S, V.DocumentOnTypeFormatting);
         when Execute_Command_Registration_Option =>
            ExecuteCommandRegistrationOptions'Read (S, V.ExecuteCommand);
         when Did_Change_Watched_Files_Registration_Option =>
            DidChangeWatchedFilesRegistrationOptions'Read
              (S, V.DidChangeWatchedFiles);
         when Code_Action_Registration_Option =>
            CodeActionRegistrationOptions'Read (S, V.CodeAction);
         when Rename_Registration_Option =>
            RenameRegistrationOptions'Read (S, V.Rename);
         when File_Operation_Registration_Option =>
            FileOperationRegistrationOptions'Read (S, V.FileOperation);
      end case;
   end Read_Registration_Option;

   procedure Write_Registration_Option
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Registration_Option) is
   begin
      case V.Kind is
         when Absent =>
            null;
         when Text_Document_Registration_Option =>
            TextDocumentRegistrationOptions'Write (S, V.Text_Document);
         when Text_Document_Change_Registration_Option =>
            TextDocumentChangeRegistrationOptions'Write
              (S, V.Text_Document_Change);
         when Text_Document_Save_Registration_Option =>
            TextDocumentSaveRegistrationOptions'Write
              (S, V.Text_Document_Save);
         when Completion_Registration_Option =>
            CompletionRegistrationOptions'Write (S, V.Completion);
         when Signature_Help_Registration_Option =>
            SignatureHelpRegistrationOptions'Write (S, V.SignatureHelp);
         when Code_Lens_Registration_Option =>
            CodeLensRegistrationOptions'Write (S, V.CodeLens);
         when Document_Link_Registration_Option =>
            DocumentLinkRegistrationOptions'Write (S, V.DocumentLink);
         when Document_On_Type_Formatting_Registration_Option =>
            DocumentOnTypeFormattingRegistrationOptions'Write
              (S, V.DocumentOnTypeFormatting);
         when Execute_Command_Registration_Option =>
            ExecuteCommandRegistrationOptions'Write (S, V.ExecuteCommand);
         when Did_Change_Watched_Files_Registration_Option =>
            DidChangeWatchedFilesRegistrationOptions'Write
              (S, V.DidChangeWatchedFiles);
         when Code_Action_Registration_Option =>
            CodeActionRegistrationOptions'Write (S, V.CodeAction);
         when Rename_Registration_Option =>
            RenameRegistrationOptions'Write (S, V.Rename);
         when File_Operation_Registration_Option =>
            FileOperationRegistrationOptions'Write (S, V.FileOperation);
      end case;
   end Write_Registration_Option;

   -----------------------
   -- Read_Registration --
   -----------------------

   procedure Read_Registration
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Registration)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            use type VSS.Strings.Virtual_String;

            Key : constant Ada.Strings.UTF_Encoding.UTF_8_String :=
               VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            if Key = "id" then
               LSP.Types.Read_String (S, V.id);
            elsif Key = "method" then
               LSP.Types.Read_String (S, V.method);

               --  Now set V.registerOptions.Kind according to the "method"

               if V.method = "workspace/didChangeWatchedFiles" then
                  V.registerOptions :=
                    (Kind => Did_Change_Watched_Files_Registration_Option,
                     others => <>);
               elsif V.method = "workspace/symbol" then
                  V.registerOptions := (Kind => Absent);
               elsif V.method = "workspace/executeCommand" then
                  V.registerOptions :=
                    (Kind => Execute_Command_Registration_Option,
                     others => <>);
               elsif V.method = "textDocument/rename" then
                  V.registerOptions :=
                    (Kind => Rename_Registration_Option,
                     others => <>);
               elsif V.method = "textDocument/onTypeFormatting" then
                  V.registerOptions :=
                    (Kind => Document_On_Type_Formatting_Registration_Option,
                     others => <>);
               elsif V.method = "textDocument/signatureHelp" then
                  V.registerOptions :=
                    (Kind => Signature_Help_Registration_Option, others => <>);
               elsif V.method = "textDocument/documentLink" then
                  V.registerOptions :=
                    (Kind => Document_Link_Registration_Option, others => <>);
               elsif V.method = "textDocument/codeLens" then
                  V.registerOptions :=
                    (Kind => Code_Lens_Registration_Option, others => <>);
               elsif V.method = "textDocument/codeAction" then
                  V.registerOptions :=
                    (Kind => Code_Action_Registration_Option, others => <>);
               elsif V.method = "textDocument/completion" then
                  V.registerOptions :=
                    (Kind => Completion_Registration_Option, others => <>);
               elsif V.method = "textDocument/didChange" then
                  V.registerOptions :=
                    (Kind => Text_Document_Change_Registration_Option,
                     others => <>);
               elsif V.method = "textDocument/didSave" then
                  V.registerOptions :=
                    (Kind => Text_Document_Save_Registration_Option,
                     others => <>);
               elsif V.method = "textDocument/didOpen"
                 or else V.method = "textDocument/didClose"
                 or else V.method = "textDocument/willSave"
                 or else V.method = "textDocument/willSaveWaitUntil"
               then
                  V.registerOptions :=
                    (Kind => Text_Document_Registration_Option,
                     others => <>);
               end if;
               --  TBD workspace/symbol, textDocument/hover,
               --  TBD textDocument/declaration, textDocument/definition
               --  TBD textDocument/typeDefinition, textDocument/implementation
               --  TBD textDocument/references, textDocument/documentHighlight
               --  TBD textDocument/documentSymbol, textDocument/documentColor
               --  TBD textDocument/formatting, textDocument/rangeFormatting
               --  TBD textDocument/foldingRange, textDocument/selectionRange
            elsif Key = "registerOptions" then
               Registration_Option'Read (S, V.registerOptions);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_Registration;

   ----------------------------------
   -- Read_ALS_Check_Syntax_Params --
   ----------------------------------

   procedure Read_ALS_Check_Syntax_Params
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ALS_Check_Syntax_Params)
   is
      use type VSS.Strings.Virtual_String;

      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "input" then
               LSP.Types.Read_String (S, V.Input);
            elsif Key = "rules" then
               LSP.Types.Read_String_Vector (S, V.Rules);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_ALS_Check_Syntax_Params;

   -----------------------------------
   -- Write_ALS_Check_Syntax_Params --
   -----------------------------------

   procedure Write_ALS_Check_Syntax_Params
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ALS_Check_Syntax_Params)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      JS.Key ("input");
      LSP.Types.Write_String (S, V.Input);
      JS.Key ("rules");
      LSP.Types.Write_String_Vector (S, V.Rules);
      JS.End_Object;
   end Write_ALS_Check_Syntax_Params;

   ----------------------------------
   -- Read_ALS_Check_Syntax_Result --
   ----------------------------------

   procedure Read_ALS_Check_Syntax_Result
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ALS_Check_Syntax_Result)
   is
      use type VSS.Strings.Virtual_String;

      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
         begin
            JS.R.Read_Next;
            if Key = "diagnostic" then
               V := (Is_Set => True, Value => <>);
               Read_String (S, V.Value);
            else
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
   end Read_ALS_Check_Syntax_Result;

   -----------------------------------
   -- Write_ALS_Check_Syntax_Result --
   -----------------------------------

   procedure Write_ALS_Check_Syntax_Result
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ALS_Check_Syntax_Result)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      if V.Is_Set then
         JS.Key ("diagnostic");
         Write_String (S, V.Value);
      end if;
      JS.End_Object;
   end Write_ALS_Check_Syntax_Result;

end LSP.Messages;
