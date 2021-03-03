import libadalang as lal

# The list of types to generate Write procedures:
types_to_print = {
    #  'Message',
    'RequestMessage',
    #  'ResponseMessage',
    'NotificationMessage',
    'CancelParams',
    'Position',
    'Span',
    'CodeActionKind',
    'AlsReferenceKind',
    'AlsDisplayMethodAncestryOnNavigationPolicy',
    #  'AlsReferenceKind_Array',
    #  'AlsReferenceKind_Set',
    'Location',
    'LocationLink',
    #  'Location_Or_Link_Kind',
    #  'Location_Or_Link_Vector',
    'DiagnosticSeverity',
    'DiagnosticTag',
    'CodeDescription',
    'DiagnosticRelatedInformation',
    'Diagnostic',
    #  'Command',
    'TextEdit',
    'AnnotatedTextEdit',
    'TextDocumentIdentifier',
    'VersionedTextDocumentIdentifier',
    'OptionalVersionedTextDocumentIdentifier',
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
    'ChangeAnnotation',
    'TextDocumentItem',
    'TextDocumentPositionParams',
    #  'DocumentFilter',
    #  'DocumentSelector',
    'dynamicRegistration',
    'ResourceOperationKind',
    'FailureHandlingKind',
    'AnnotationSupport',
    'WorkspaceEditClientCapabilities',
    'SymbolKind',
    'symbolKindCapabilities',
    'Als_Visibility',
    'WorkspaceSymbolClientCapabilities',
    'WorkspaceClientCapabilities',
    'MarkupKind',
    'MarkupContent',
    #  'String_Or_MarkupContent',
    'SaveOptions',
    'TextDocumentSyncClientCapabilities',
    'CompletionItemTag',
    'CompletionItemTagSupport',
    'completionItemCapability',
    'CompletionItemKind',
    'CompletionItemKindSetCapabilities',
    'CompletionClientCapabilities',
    'HoverClientCapabilities',
    'parameterInformation_Capability',
    'signatureInformation_Capability',
    'SignatureHelpClientCapabilities',
    'DocumentSymbolClientCapabilities',
    'DeclarationClientCapabilities',
    'codeActionKindCapability',
    'codeActionLiteralSupport_Capability',
    'CodeActionClientCapabilities',
    'DocumentLinkClientCapabilities',
    'RenameClientCapabilities',
    'DiagnosticTagSupport',
    'PublishDiagnosticsClientCapabilities',
    'FoldingRangeClientCapabilities',
    'SemanticTokensFullCapabilities',
    'SemanticTokensRequestCapabilities',
    'TokenFormat',
    'SemanticTokensClientCapabilities',
    'TextDocumentClientCapabilities',
    'WindowClientCapabilities',
    'MarkdownClientCapabilities',
    'RegularExpressionsClientCapabilities',
    'GeneralClientCapabilities',
    'ClientCapabilities',
    'WorkspaceFolder',
    #  'ProgressParam',
    'WorkDoneProgressCreateParams',
    #  'WorkDoneProgressParams',
    #  'PartialResultParams',
    #  'Progress_Partial_Params',
    #  'Text_Progress_Partial_Params',
    'Text_Progress_Params',
    'ProgramInfo',
    'TraceValue',
    'InitializeParams',
    #  'WorkDoneProgressOptions',
    'TextDocumentSyncKind',
    'TextDocumentSyncOptions',
    #  'Optional_TextDocumentSyncOptions',
    'CompletionOptions',
    'SignatureHelpOptions',
    'TextDocumentRegistrationOptions',
    'TSW_RegistrationOptions',
    #  'Provider_Options',
    'CodeActionOptions',
    'CodeLensOptions',
    'DocumentOnTypeFormattingOptions',
    'RenameOptions',
    'DocumentLinkOptions',
    'ExecuteCommandOptions',
    'WorkspaceFoldersServerCapabilities',
    'workspace_Options',
    'SemanticTokenTypes',
    'SemanticTokenModifiers',
    'SemanticTokensLegend',
    'SemanticTokensOptions',
    'ServerCapabilities',
    'InitializeResult',
    'InitializedParams',
    #  'InitializeError',
    'MessageType',
    'ShowMessageParams',
    'ShowMessageRequestParams',
    'LogMessageParams',
    'TextDocumentChangeRegistrationOptions',
    'TextDocumentSaveRegistrationOptions',
    'CompletionRegistrationOptions',
    'SignatureHelpRegistrationOptions',
    'CodeLensRegistrationOptions',
    'DocumentLinkRegistrationOptions',
    'DocumentOnTypeFormattingRegistrationOptions',
    'ExecuteCommandRegistrationOptions',
    #  'Registration_Option',
    'Registration',
    'RegistrationParams',
    'Unregistration',
    'UnregistrationParams',
    'DidChangeConfigurationParams',
    'DidOpenTextDocumentParams',
    'TextDocumentContentChangeEvent',
    'DidChangeTextDocumentParams',
    'TextDocumentSaveReason',
    #  'WillSaveTextDocumentParams',
    'DidSaveTextDocumentParams',
    'DidCloseTextDocumentParams',
    'FileChangeType',
    'FileEvent',
    'DidChangeWatchedFilesParams',
    'PublishDiagnosticsParams',
    'InsertTextFormat',
    'CompletionItem',
    'CompletionList',
    #  'MarkedString',
    #  'MarkupContent_Or_MarkedString_Vector',
    'Hover',
    #  'Parameter_Label',
    'ParameterInformation',
    'SignatureInformation',
    'SignatureHelp',
    'ReferenceContext',
    'ReferenceParams',
    'DocumentHighlightKind',
    'DocumentHighlight',
    'DocumentSymbolParams',
    #  'DocumentSymbol',
    #  'DocumentSymbol_Tree',
    'SymbolInformation',
    #  'Symbol_Vector',
    'WorkspaceSymbolParams',
    'CodeActionContext',
    'CodeActionParams',
    #  'CodeLensParams',
    #  'CodeLens',
    #  'DocumentLinkParams',
    #  'DocumentLink',
    'FormattingOptions',
    'DocumentFormattingParams',
    'DocumentRangeFormattingParams',
    'DocumentOnTypeFormattingParams',
    'RenameParams',
    #  'ExecuteCommandParams',
    'ApplyWorkspaceEditParams',
    'ApplyWorkspaceEditResult',
    'WorkDoneProgressBegin',
    'WorkDoneProgressReport',
    'WorkDoneProgressEnd',
    #  'Progress_Kind',
    #  'Progress_Params',
    'WorkspaceFoldersChangeEvent',
    'DidChangeWorkspaceFoldersParams',
    'ConfigurationItem',
    'ConfigurationParams',
    #  'WatchKind',
    #  'WatchKind_Set',
    'FileSystemWatcher',
    'DidChangeWatchedFilesRegistrationOptions',
    'CompletionTriggerKind',
    'CompletionContext',
    'CompletionParams',
    #  'CodeAction',
    'CodeActionRegistrationOptions',
    'RGBA_Color',
    'ColorInformation',
    'ColorPresentationParams',
    'ColorPresentation',
    'RenameRegistrationOptions',
    'FoldingRangeParams',
    'FoldingRange',
    'DocumentColorParams',
    #  'HoverParams',
    #  'SignatureHelpParams',
    #  'DeclarationParams',
    'NavigationRequestParams',
    #  'DefinitionParams',
    #  'TypeDefinitionParams',
    #  'ImplementationParams',
    #  'DocumentHighlightParams',
    'SelectionRangeParams',
    'SelectionRange',
    'LinkedEditingRanges',
    'CallHierarchyItem',
    'CallHierarchyIncomingCallsParams',
    'CallHierarchyIncomingCall',
    'CallHierarchyOutgoingCall',
    'SemanticTokens',
    'SemanticTokensPartialResult',
    'SemanticTokensDeltaParams',
    'SemanticTokensEdit',
    'SemanticTokensDelta',
    'SemanticTokensDeltaPartialResult',
    'SemanticTokensRangeParams',
    'SemanticTokensWorkspaceClientCapabilities',
    'UniquenessLevel',
    'MonikerKind',
    'Moniker',
    'ALS_Subprogram_And_References',
    'ALS_ShowDependenciesKind',
    'ALS_Unit_Description',
    'ALS_ShowDependenciesParams',
    #  'ALS_Debug_Kinds',
    #  'ALSDebugParams',
    }

spec_header = """--  Automatically generated, do not edit.
with Ada.Streams;
with LSP.Messages;

package LSP.Message_IO is
"""

body_header = """--  Automatically generated, do not edit.
with Ada.Strings.UTF_Encoding;
with Interfaces;

with VSS.JSON.Streams.Readers;
with VSS.Strings.Conversions;

with LSP.JSON_Streams;
with LSP.Messages;                 use LSP.Messages;
with LSP.Types;                    use LSP.Types;

package body LSP.Message_IO is
   pragma Style_Checks ("M175");
   use type Interfaces.Integer_64;
"""

file_footer = """
end LSP.Message_IO;
"""

io_spec = """
   procedure {kind}_{type}
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : {out}LSP.Messages.{type});
"""

io_header = """
   procedure {kind}_{type}
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : {out}{type})
   is
{unref}      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
"""

io_footer = """\
   end {kind}_{type};
"""

write_component = {
    "LSP_String": """\
      JS.Key ("{key}");
      LSP.Types.Write (S, V.{name});
""",
    "DocumentUri": """\
      JS.Key ("{key}");
      LSP.Types.Write (S, V.{name});
""",
    "Boolean": """\
      {kind}_Boolean (JS, +"{key}", V.{name});
""",
    "UTF_16_Index": """\
      JS.Key ("{key}");
      LSP.Types.Write_UTF16_Code_Unit_Count (JS, V.{name});
""",
    "": """\
      JS.Key ("{key}");
      {type}'{kind} (S, V.{name});
"""
}

read_component = {
    "LSP_String": """if Key = "{key}" then
               LSP.Types.Read (S, V.{name});
            els""",
    "DocumentUri": """if Key = "{key}" then
               LSP.Types.Read (S, V.{name});
            els""",
    "Boolean": """if Key = "{key}" then
               LSP.Types.Read_Boolean (JS, V.{name});
            els""",
    "UTF_16_Index": """if Key = "{key}" then
               LSP.Types.Read_UTF16_Code_Unit_Count (JS, V.{name});
            els""",
    "": """if Key = "{key}" then
               {type}'Read (S, V.{name});
            els"""
}

io_component = {'Read': read_component, 'Write': write_component}

io_pos_enum = {
    'Read': """\
      V := {type}'Val (JS.R.Number_Value.Integer_Value - {offset});
      JS.R.Read_Next;
""",
    'Write': """\
      JS.Write_Integer (({type}'Pos (V)) + {offset});
"""
}

io_string_enum_header = {
    'Read': """
      Text : constant Standard.String :=
        VSS.Strings.Conversions.To_UTF_8_String (JS.R.String_Value);
   begin
      JS.R.Read_Next;
      """,
    'Write': """
      function To_String
        (Value : {type})
         return Ada.Strings.UTF_Encoding.UTF_8_String;

      function To_String
        (Value : {type})
         return Ada.Strings.UTF_Encoding.UTF_8_String is
      begin
         case Value is
"""
}

io_string_enum_case = {
    'Read': """if Text = "{key}" then
         V := {name};
      els""",
    'Write': """\
            when {name} =>
               return "{key}";
"""
}

io_string_enum_footer = {
    'Read': """e
         V := {type}'First;
      end if;
""",
    'Write': """\
         end case;
      end To_String;

   begin
      JS.Write_String (To_String (V));
"""
}

read_prolog = {
    'Read': """   begin
      pragma Assert (JS.R.Is_Start_Object);
      JS.R.Read_Next;

      while not JS.R.Is_End_Object loop
         pragma Assert (JS.R.Is_Key_Name);
         declare
            Key : constant String :=
               VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);
         begin
            JS.R.Read_Next;
            """,
    'Write': """   begin
      JS.Start_Object;
"""
}


read_epilog = {
    'Read': """e
               JS.Skip_Value;
            end if;
         end;
      end loop;
      JS.R.Read_Next;
""",
    'Write': """      JS.End_Object;
"""
}


# The list of enumeration type represented as strings
enum_as_string = [
    'AlsReferenceKind',
    'AlsDisplayMethodAncestryOnNavigationPolicy',
    'CodeActionKind',
    'ResourceOperationKind',
    'FailureHandlingKind',
    'MarkupKind',
    'TokenFormat',
    'SemanticTokenTypes',
    'SemanticTokenModifiers',
    'UniquenessLevel',
    'MonikerKind',
    'TraceValue']

# The map to substitute words reserved in Ada:
reserver_named = \
   {
    "abortapplying": "abort",
    "access_ref": "access",
    "first": "start",
    "last": "end",
    "loc": "location",    # for ALS_Subprogram_And_References
    "span": "range",
    "a_type": "type",
    "simple": "reference",  # For AlsReferenceKind
    "write": "write",
    "static_call": "call",
    "dispatching_call": "dispatching call",
    "parent": "parent",
    "child": "child",
    "empty": "",            # For CodeActionKind
    "quickfix": "quickfix",
    "refactor": "refactor",
    "refactorextract": "refactor.extract",
    "refactorinline": "refactor.inline",
    "refactorrewrite": "refactor.rewrite",
    "source": "source",
    "sourceorganizeimports": "source.organizeImports",
    "messages_trace": "message",  # for TraceValue
    "diff": "delta", # for SemanticTokens
    "an_abstract": "abstract", # for SemanticTokenTypes
    "an_interface": "interface",
    "a_function": "function",
    "a_string": "string",
    }


def get_key(field):
    lower = field.lower()
    if lower in reserver_named:
        return reserver_named[lower]
    else:
        return field


def write_format(type, kind):
    if type in io_component[kind]:
        return io_component[kind][type]
    else:
        return io_component[kind][""]


def filter(x):
    return isinstance(x, lal.TypeDecl) \
        and not isinstance(x, lal.AnonymousTypeDecl) \
        and x.p_defining_name.token_start.text in types_to_print


def print_spec(file, node):
    name = node.p_defining_name.token_start.text
    print name
    file.write(io_spec.format(type=name, kind='Read', out='out '))
    file.write(io_spec.format(type=name, kind='Write', out=''))


def get_components(node):
    if isinstance(node, lal.DerivedTypeDef):
        parent = node.f_subtype_indication.p_designated_type_decl
        result = get_components(parent.f_type_def)
    else:
        result = []

    result += list(node.finditer(lal.ComponentDecl))

    return result


def print_components(file, kind, node, no_components):
    file.write(read_prolog[kind])

    if kind == 'Read' and no_components:
        #  Write something compilable
        file.write("""if Key = "" then
               null;
            els""")

    for x in get_components(node):
        name = x.p_defining_name.token_start.text
        tp = x.f_component_def.f_type_expr.f_name.text
        txt = write_format(tp, kind).format(key=get_key(name),
                                            kind=kind,
                                            name=name,
                                            type=tp)
        file.write(txt)

    file.write(read_epilog[kind])


def print_enums(file, kind, type, node):
    if type not in enum_as_string:
        offset = 0 if type == 'TextDocumentSyncKind' else 1
        file.write('   begin\n')
        txt = io_pos_enum[kind].format(type=type, offset=offset)
    else:
        txt = io_string_enum_header[kind].format(type=type)
        for x in node.finditer(lal.EnumLiteralDecl):
            name = x.p_defining_name.token_start.text
            txt += io_string_enum_case[kind].format(name=name,
                                                    key=get_key(name))
        txt += io_string_enum_footer[kind].format(type=type)

    file.write(txt)


def print_body(file, node):
    name = node.p_defining_name.token_start.text

    unref = '      pragma Unreferenced (V);\n\n' if \
        len(list(node.finditer(lal.NullRecordDef))) else ''

    def print_decls_and_statements(kind):
        file.write(io_header.format(type=name,
                                    kind=kind,
                                    out='' if kind == 'Write' else 'out ',
                                    unref=unref))
        if isinstance(node.f_type_def, lal.EnumTypeDef):
            print_enums(file, kind, name, node.f_type_def)
        else:
            print_components(file, kind, node.f_type_def, unref)

        file.write(io_footer.format(type=node.p_defining_name.token_start.text,
                                    kind=kind))
    print_decls_and_statements('Read')
    print_decls_and_statements('Write')


def print_io():
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
    print_io()
