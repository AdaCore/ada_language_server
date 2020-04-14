import libadalang as lal

types_to_print = {
    #  'Message',
    #  'RequestMessage',
    #  'ResponseMessage',
    #  'NotificationMessage',
    #  'CancelParams',
    #  'Position',
    #  'Position_Vector',
    #  'Span',
    #  'Optional_Span',
    #  'CodeActionKind',
    #  'CodeActionKindSet',
    #  'Optional_CodeActionKindSet',
    #  'Optional_CodeActionKind',
    #  'AlsReferenceKind',
    #  'AlsReferenceKind_Array',
    #  'AlsReferenceKind_Set',
    #  'Optional_AlsReferenceKind_Set',
    #  'Location',
    #  'Location_Vector',
    #  'LocationLink',
    #  'LocationLink_Vector',
    #  'Location_Or_Link_Kind',
    #  'Location_Or_Link_Vector',
    #  'DiagnosticSeverity',
    #  'Optional_DiagnosticSeverity',
    #  'DiagnosticTag',
    #  'DiagnosticTagSet',
    #  'Optional_DiagnosticTagSet',
    #  'DiagnosticRelatedInformation',
    #  'DiagnosticRelatedInformation_Vector',
    #  'Diagnostic',
    #  'Diagnostic_Vector',
    #  'Optional_Diagnostic_Vector',
    #  'Any_Vector',
    #  'Optional_Any_Vector',
    #  'Command',
    #  'Command_Vector',
    #  'TextEdit',
    #  'Optional_TextEdit',
    #  'TextEdit_Vector',
    #  'TextDocumentIdentifier',
    #  'VersionedTextDocumentIdentifier',
    'TextDocumentEdit',
    #  'CreateFileOptions',
    #  'CreateFile',
    #  'RenameFileOptions',
    #  'RenameFile',
    #  'DeleteFileOptions',
    #  'DeleteFile',
    #  'Document_Change_Kind',
    #  'Document_Change',
    #  'Document_Change_Vector',
    #  'WorkspaceEdit',
    #  'Optional_WorkspaceEdit',
    #  'TextDocumentItem',
    #  'TextDocumentPositionParams',
    #  'DocumentFilter',
    #  'DocumentSelector',
    #  'dynamicRegistration',
    #  'ResourceOperationKind',
    #  'ResourceOperationKindSet',
    #  'Optional_ResourceOperationKindSet',
    #  'FailureHandlingKind',
    #  'Optional_FailureHandlingKind',
    #  'WorkspaceEditClientCapabilities',
    #  'SymbolKind',
    #  'SymbolKindSet',
    #  'Optional_SymbolKindSet',
    #  'Als_Visibility',
    #  'Optional_Als_Visibility',
    #  'WorkspaceSymbolClientCapabilities',
    #  'Optional_WorkspaceSymbolClientCapabilities',
    #  'WorkspaceClientCapabilities',
    #  'MarkupKind',
    #  'MarkupKind_Vector',
    #  'Optional_MarkupKind_Vector',
    #  'MarkupContent',
    #  'String_Or_MarkupContent',
    #  'Optional_String_Or_MarkupContent',
    #  'SaveOptions',
    #  'Optional_SaveOptions',
    #  'TextDocumentSyncClientCapabilities',
    #  'CompletionItemTag',
    #  'CompletionItemTagSet',
    #  'Optional_CompletionItemTagSet',
    #  'CompletionItemTagSupport',
    #  'Optional_CompletionItemTagSupport',
    #  'completionItemCapability',
    #  'Optional_completionItemCapability',
    #  'CompletionItemKind',
    #  'CompletionItemKindSet',
    #  'Optional_CompletionItemKindSet',
    #  'CompletionClientCapabilities',
    #  'HoverClientCapabilities',
    #  'Optional_HoverClientCapabilities',
    #  'parameterInformation_Capability',
    #  'Optional_parameterInformation_Capability',
    #  'signatureInformation_Capability',
    #  'Optional_signatureInformation_Capability',
    #  'SignatureHelpClientCapabilities',
    #  'Optional_SignatureHelpClientCapabilities',
    #  'DocumentSymbolClientCapabilities',
    #  'Optional_DocumentSymbolClientCapabilities',
    #  'DeclarationClientCapabilities',
    #  'Optional_DeclarationClientCapabilities',
    #  'codeActionLiteralSupport_Capability',
    #  'Optional_codeActionLiteralSupport_Capability',
    #  'CodeActionClientCapabilities',
    #  'Optional_CodeActionClientCapabilities',
    #  'DocumentLinkClientCapabilities',
    #  'Optional_DocumentLinkClientCapabilities',
    #  'RenameClientCapabilities',
    #  'Optional_RenameClientCapabilities',
    #  'DiagnosticTagSupport',
    #  'Optional_DiagnosticTagSupport',
    #  'PublishDiagnosticsClientCapabilities',
    #  'Optional_PublishDiagnosticsClientCapabilities',
    #  'FoldingRangeClientCapabilities',
    #  'Optional_FoldingRangeClientCapabilities',
    #  'TextDocumentClientCapabilities',
    #  'WindowClientCapabilities',
    #  'Optional_WindowClientCapabilities',
    #  'ClientCapabilities',
    #  'WorkspaceFolder',
    #  'WorkspaceFolder_Vector',
    #  'Optional_WorkspaceFolder_Vector',
    #  'Optional_ProgressToken',
    #  'T',
    #  'ProgressParam',
    #  'WorkDoneProgressCreateParams',
    #  'WorkDoneProgressParams',
    #  'PartialResultParams',
    #  'Progress_Partial_Params',
    #  'Text_Progress_Partial_Params',
    #  'Text_Progress_Params',
    #  'ProgramInfo',
    #  'Optional_ProgramInfo',
    #  'InitializeParams',
    #  'WorkDoneProgressOptions',
    #  'Optional_WorkDoneProgressOptions',
    #  'TextDocumentSyncKind',
    #  'Optional_TextDocumentSyncKind',
    #  'TextDocumentSyncOptions',
    #  'Optional_TextDocumentSyncOptions',
    #  'CompletionOptions',
    #  'Optional_CompletionOptions',
    #  'SignatureHelpOptions',
    #  'Optional_SignatureHelpOptions',
    #  'TextDocumentRegistrationOptions',
    #  'TSW_RegistrationOptions',
    #  'Optional_TSW_RegistrationOptions',
    #  'Provider_Options',
    #  'Optional_Provider_Options',
    #  'CodeActionOptions',
    #  'Optional_CodeActionOptions',
    #  'CodeLensOptions',
    #  'Optional_CodeLensOptions',
    #  'DocumentOnTypeFormattingOptions',
    #  'Optional_DocumentOnTypeFormattingOptions',
    #  'RenameOptions',
    #  'Optional_RenameOptions',
    #  'DocumentLinkOptions',
    #  'Optional_DocumentLinkOptions',
    #  'ExecuteCommandOptions',
    #  'Optional_ExecuteCommandOptions',
    #  'Optional_Boolean_Or_String',
    #  'WorkspaceFoldersServerCapabilities',
    #  'Optional_WorkspaceFoldersServerCapabilities',
    #  'workspace_Options',
    #  'Optional_workspace_Options',
    #  'ServerCapabilities',
    #  'InitializeResult',
    #  'InitializedParams',
    #  'InitializeError',
    #  'MessageType',
    #  'ShowMessageParams',
    #  'ShowMessageRequestParams',
    #  'LogMessageParams',
    #  'TextDocumentChangeRegistrationOptions',
    #  'TextDocumentSaveRegistrationOptions',
    #  'CompletionRegistrationOptions',
    #  'SignatureHelpRegistrationOptions',
    #  'CodeLensRegistrationOptions',
    #  'DocumentLinkRegistrationOptions',
    #  'DocumentOnTypeFormattingRegistrationOptions',
    #  'ExecuteCommandRegistrationOptions',
    #  'Registration_Option',
    #  'Registration',
    #  'Registration_Array',
    #  'RegistrationParams',
    #  'Unregistration',
    #  'UnregistrationParams',
    #  'DidChangeConfigurationParams',
    #  'DidOpenTextDocumentParams',
    #  'TextDocumentContentChangeEvent',
    #  'TextDocumentContentChangeEvent_Vector',
    #  'DidChangeTextDocumentParams',
    #  'TextDocumentSaveReason',
    #  'WillSaveTextDocumentParams',
    #  'DidSaveTextDocumentParams',
    #  'DidCloseTextDocumentParams',
    #  'FileChangeType',
    #  'FileEvent',
    #  'FileEvent_Vector',
    #  'DidChangeWatchedFilesParams',
    #  'PublishDiagnosticsParams',
    #  'InsertTextFormat',
    #  'Optional_InsertTextFormat',
    #  'Optional_CompletionItemKind',
    #  'Optional_Command',
    #  'CompletionItem',
    #  'CompletionItem_Vector',
    #  'CompletionList',
    #  'MarkedString',
    #  'MarkedString_Vector',
    #  'MarkupContent_Or_MarkedString_Vector',
    #  'Hover',
    #  'Parameter_Label',
    #  'ParameterInformation',
    #  'ParameterInformation_Vector',
    #  'SignatureInformation',
    #  'SignatureInformation_Vector',
    #  'SignatureHelp',
    #  'ReferenceContext',
    #  'ReferenceParams',
    #  'DocumentHighlightKind',
    #  'Optional_DocumentHighlightKind',
    #  'DocumentHighlight',
    #  'DocumentHighlight_Vector',
    #  'DocumentSymbolParams',
    #  'DocumentSymbol',
    #  'DocumentSymbol_Tree',
    #  'SymbolInformation',
    #  'SymbolInformation_Vector',
    #  'Symbol_Vector',
    #  'WorkspaceSymbolParams',
    #  'CodeActionContext',
    #  'CodeActionParams',
    #  'CodeLensParams',
    #  'CodeLens',
    #  'DocumentLinkParams',
    #  'DocumentLink',
    #  'DocumentLink_Vector',
    #  'FormattingOptions',
    #  'DocumentFormattingParams',
    #  'DocumentRangeFormattingParams',
    #  'DocumentOnTypeFormattingParams',
    #  'RenameParams',
    #  'ExecuteCommandParams',
    #  'ApplyWorkspaceEditParams',
    #  'ApplyWorkspaceEditResult',
    #  'WorkDoneProgressBegin',
    #  'WorkDoneProgressReport',
    #  'WorkDoneProgressEnd',
    #  'Progress_Kind',
    #  'Progress_Params',
    #  'WorkspaceFoldersChangeEvent',
    #  'DidChangeWorkspaceFoldersParams',
    #  'ConfigurationItem',
    #  'ConfigurationItem_Vector',
    #  'ConfigurationParams',
    #  'WatchKind',
    #  'WatchKind_Set',
    #  'FileSystemWatcher',
    #  'FileSystemWatcher_Vector',
    #  'DidChangeWatchedFilesRegistrationOptions',
    #  'CompletionTriggerKind',
    #  'CompletionContext',
    #  'Optional_CompletionContext',
    #  'CompletionParams',
    #  'CodeAction',
    #  'CodeAction_Vector',
    #  'CodeActionRegistrationOptions',
    #  'RGBA_Color',
    #  'ColorInformation',
    #  'ColorInformation_Vector',
    #  'ColorPresentationParams',
    #  'ColorPresentation',
    #  'ColorPresentation_Vector',
    #  'RenameRegistrationOptions',
    #  'FoldingRangeParams',
    #  'FoldingRange',
    #  'FoldingRange_Vector',
    #  'DocumentColorParams',
    #  'HoverParams',
    #  'SignatureHelpParams',
    #  'DeclarationParams',
    #  'DefinitionParams',
    #  'TypeDefinitionParams',
    #  'ImplementationParams',
    #  'DocumentHighlightParams',
    #  'SelectionRangeParams',
    #  'SelectionRange',
    #  'SelectionRange_Vector',
    #  'ALS_Subprogram_And_References',
    #  'ALS_Subprogram_And_References_Vector',
    #  'ALS_Debug_Kinds',
    #  'ALSDebugParams',
    }

spec_header = """--  Automatically generated, do not edit.
with Ada.Streams;
with LSP.Messages;

package LSP.Message_IO is
"""

body_header = """--  Automatically generated, do not edit.
with LSP.JSON_Streams;
with LSP.Messages;                 use LSP.Messages;

package body LSP.Message_IO is
"""

file_footer = """
end LSP.Message_IO;
"""

write_spec = """
   procedure Write_{type}
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.{type});
"""

write_header = """
   procedure Write_{type}
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : {type})
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
"""

write_footer = """\
      JS.End_Object;
   end Write_{type};
"""

write_component = """\
      JS.Key ("{name}");
      {type}'Write (S, V.{name});
"""


def filter(x):
    return isinstance(x, lal.TypeDecl) \
        and not isinstance(x, lal.AnonymousTypeDecl) \
        and x.p_defining_name.token_start.text in types_to_print


def print_spec(file, node):
    print node.p_defining_name.token_start.text
    file.write(write_spec.format(type=node.p_defining_name.token_start.text))
    # print write_body.format(type=x.p_defining_name.token_start.text)


def print_components(file, node):
    for x in node.finditer(lal.ComponentDecl):
        name = x.p_defining_name.token_start.text
        tp = x.f_component_def.f_type_expr.f_name.full_name
        file.write(write_component.format(name=name, type=tp))


def print_body(file, node):
    file.write(write_header.format(type=node.p_defining_name.token_start.text))
    print_components(file, node.f_type_def)
    file.write(write_footer.format(type=node.p_defining_name.token_start.text))


def print_writes():
    up = lal.UnitProvider.for_project("gnat/lsp.gpr")
    ctx = lal.AnalysisContext(unit_provider=up)
    unit = ctx.get_from_file("source/protocol/lsp-messages.ads")
    ads = open("source/protocol/generated/lsp-message_io.ads", 'wb')
    ads.write(spec_header)
    adb = open("source/protocol/generated/lsp-message_io.adb", 'wb')
    adb.write(body_header)

    for x in unit.root.finditer(filter):
        print_spec(ads, x)
        print_body(adb, x)

    ads.write(file_footer)
    adb.write(file_footer)

if __name__ == '__main__':
    print_writes()
