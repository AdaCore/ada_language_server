import libadalang as lal

types_to_print = {
    #  'Message',
    #  'RequestMessage',
    #  'ResponseMessage',
    #  'NotificationMessage',
    #  'CancelParams',
    #  'Position',
    #  'Span',
    #  'CodeActionKind',
    #  'AlsReferenceKind',
    #  'AlsReferenceKind_Array',
    #  'AlsReferenceKind_Set',
    #  'Location',
    #  'LocationLink',
    #  'Location_Or_Link_Kind',
    #  'Location_Or_Link_Vector',
    #  'DiagnosticSeverity',
    #  'DiagnosticTag',
    #  'DiagnosticRelatedInformation',
    #  'Diagnostic',
    #  'Command',
    #  'TextEdit',
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
    #  'WorkspaceEdit',
    #  'TextDocumentItem',
    'TextDocumentPositionParams',
    #  'DocumentFilter',
    #  'DocumentSelector',
    #  'dynamicRegistration',
    #  'ResourceOperationKind',
    #  'ResourceOperationKindSet',
    #  'FailureHandlingKind',
    #  'WorkspaceEditClientCapabilities',
    #  'SymbolKind',
    #  'Als_Visibility',
    #  'WorkspaceSymbolClientCapabilities',
    #  'WorkspaceClientCapabilities',
    #  'MarkupKind',
    #  'MarkupContent',
    #  'String_Or_MarkupContent',
    #  'SaveOptions',
    #  'TextDocumentSyncClientCapabilities',
    #  'CompletionItemTag',
    'CompletionItemTagSupport',
    #  'completionItemCapability',
    #  'CompletionItemKind',
    #  'CompletionClientCapabilities',
    #  'HoverClientCapabilities',
    #  'parameterInformation_Capability',
    'signatureInformation_Capability',
    #  'SignatureHelpClientCapabilities',
    #  'DocumentSymbolClientCapabilities',
    #  'DeclarationClientCapabilities',
    #  'codeActionLiteralSupport_Capability',
    #  'CodeActionClientCapabilities',
    #  'DocumentLinkClientCapabilities',
    #  'RenameClientCapabilities',
    'DiagnosticTagSupport',
    #  'PublishDiagnosticsClientCapabilities',
    #  'FoldingRangeClientCapabilities',
    'TextDocumentClientCapabilities',
    #  'WindowClientCapabilities',
    'ClientCapabilities',
    #  'WorkspaceFolder',
    #  'ProgressParam',
    #  'WorkDoneProgressCreateParams',
    #  'WorkDoneProgressParams',
    #  'PartialResultParams',
    #  'Progress_Partial_Params',
    #  'Text_Progress_Partial_Params',
    #  'Text_Progress_Params',
    #  'ProgramInfo',
    #  'InitializeParams',
    #  'WorkDoneProgressOptions',
    #  'TextDocumentSyncKind',
    #  'TextDocumentSyncOptions',
    #  'Optional_TextDocumentSyncOptions',
    #  'CompletionOptions',
    #  'SignatureHelpOptions',
    #  'TextDocumentRegistrationOptions',
    #  'TSW_RegistrationOptions',
    #  'Provider_Options',
    #  'CodeActionOptions',
    #  'CodeLensOptions',
    #  'DocumentOnTypeFormattingOptions',
    #  'RenameOptions',
    #  'DocumentLinkOptions',
    #  'ExecuteCommandOptions',
    #  'WorkspaceFoldersServerCapabilities',
    'workspace_Options',
    #  'ServerCapabilities',
    'InitializeResult',
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
    'DidChangeConfigurationParams',
    'DidOpenTextDocumentParams',
    #  'TextDocumentContentChangeEvent',
    #  'TextDocumentContentChangeEvent_Vector',
    'DidChangeTextDocumentParams',
    #  'TextDocumentSaveReason',
    #  'WillSaveTextDocumentParams',
    #  'DidSaveTextDocumentParams',
    'DidCloseTextDocumentParams',
    #  'FileChangeType',
    #  'FileEvent',
    #  'FileEvent_Vector',
    #  'DidChangeWatchedFilesParams',
    #  'PublishDiagnosticsParams',
    #  'InsertTextFormat',
    #  'CompletionItem',
    #  'CompletionList',
    #  'MarkedString',
    #  'MarkupContent_Or_MarkedString_Vector',
    #  'Hover',
    #  'Parameter_Label',
    'ParameterInformation',
    #  'SignatureInformation',
    #  'SignatureHelp',
    #  'ReferenceContext',
    #  'ReferenceParams',
    #  'DocumentHighlightKind',
    #  'DocumentHighlight',
    #  'DocumentSymbolParams',
    #  'DocumentSymbol',
    #  'DocumentSymbol_Tree',
    #  'SymbolInformation',
    #  'Symbol_Vector',
    #  'WorkspaceSymbolParams',
    'CodeActionContext',
    #  'CodeActionParams',
    #  'CodeLensParams',
    #  'CodeLens',
    #  'DocumentLinkParams',
    #  'DocumentLink',
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
    'WorkspaceFoldersChangeEvent',
    'DidChangeWorkspaceFoldersParams',
    #  'ConfigurationItem',
    'ConfigurationParams',
    #  'WatchKind',
    #  'WatchKind_Set',
    #  'FileSystemWatcher',
    'DidChangeWatchedFilesRegistrationOptions',
    #  'CompletionTriggerKind',
    #  'CompletionContext',
    #  'CompletionParams',
    #  'CodeAction',
    #  'CodeActionRegistrationOptions',
    #  'RGBA_Color',
    #  'ColorInformation',
    #  'ColorPresentationParams',
    #  'ColorPresentation',
    #  'RenameRegistrationOptions',
    #  'FoldingRangeParams',
    #  'FoldingRange',
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
with LSP.Types;

package body LSP.Message_IO is
   pragma Style_Checks ("M175");
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
