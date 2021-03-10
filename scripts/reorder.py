# This script extracts typescripts comments from lsp-messages.ads
# and reorders them in an order that corresponds to LSP specification.
# Use the following command to run the script:
#
#  cd ada_language_server
#  python3 ./scripts/reorder.py > file_to_compare
#  

import re


def read_snippets(file):
    """Take as parameter the name of a lsp-messages.ads file

       file: path to the lsp-messages.ads file

       return: a list representing the test
    """

    # 1. Collect all snippets into a list
    list = []
    snippet = ''
    with open(file, 'r') as f:
        start_re = re.compile(r'^   -- ?```typescript')
        stop_re = re.compile(r'^   -- ?```')
        for x in f.readlines():
            line = x[:-1]
            if snippet:
                if stop_re.match(line):
                    snippet = f'{snippet}\n{line}'
                    list.append(snippet)
                    snippet = ''
                else:
                    snippet = f'{snippet}\n{line}'
            elif start_re.match(line):
                snippet = line
    
    return list


def emit(list, pattern):
    """Take list of code snippets and a pattern, find the first snippet
       containing the pattern and print it.

       list: list of strings.
       pattern: a string to search
    """
    for snippet in list:
        if snippet.find(pattern) >= 0:
            print(snippet)
            return


order = [
    'type integer ',
    'type uinteger ',
    'type decimal ',
    'interface Message ',
    'interface RequestMessage ',
    'interface ResponseMessage ',
    'interface NotificationMessage ',
    'interface CancelParams ',
    'type ProgressToken ',
    'type DocumentUri ',
    'type URI ',
    'interface RegularExpressionsClientCapabilities ',
    'const EOL',
    'interface Position ',
    'start: { line: 5, character: 23 ',
    'interface Range ',
    'interface Location ',
    'interface LocationLink ',
    'interface Diagnostic ',
    'namespace DiagnosticSeverity ',
    'interface DiagnosticRelatedInformation ',
    'interface CodeDescription ',
    'interface Command ',
    'interface TextEdit ',
    'interface ChangeAnnotation ',
    'interface AnnotatedTextEdit ',
    'interface TextDocumentEdit ',
    'interface CreateFileOptions ',
    'interface WorkspaceEdit ',
    'interface WorkspaceEditClientCapabilities ',
    'interface TextDocumentIdentifier ',
    'interface TextDocumentItem ',
    'interface VersionedTextDocumentIdentifier ',
    'interface OptionalVersionedTextDocumentIdentifier ',
    'interface TextDocumentPositionParams ',
    '{ language: ',
    'interface DocumentFilter ',
    'type DocumentSelector ',
    'interface StaticRegistrationOptions ',
    'interface TextDocumentRegistrationOptions ',
    'namespace MarkupKind ',
    'interface MarkdownClientCapabilities ',
    'interface WorkDoneProgressBegin ',
    'interface WorkDoneProgressReport ',
    'interface WorkDoneProgressEnd ',
    'interface WorkDoneProgressParams ',
    'interface WorkDoneProgressOptions ',
    'Window specific client capabilities',
    'interface PartialResultParams ',
    'type TraceValue ',
    'interface InitializeParams ',
    'interface TextDocumentClientCapabilities ',
    'interface ClientCapabilities ',
    'interface InitializeResult ',
    'namespace InitializeError ',
    'interface InitializeError ',
    'interface ServerCapabilities ',
    'interface InitializedParams ',
    'interface LogTraceParams ',
    'interface SetTraceParams ',
    'interface ShowMessageParams ',
    'namespace MessageType ',
    'interface ShowMessageRequestClientCapabilities ',
    'interface ShowMessageRequestParams ',
    'interface MessageActionItem ',
    'interface ShowDocumentClientCapabilities ',
    'interface ShowDocumentParams ',
    'interface ShowDocumentResult ',
    'interface LogMessageParams ',
    'interface WorkDoneProgressCreateParams ',
    'interface WorkDoneProgressCancelParams ',
    'interface Registration ',
    'interface Unregistration ',
    'interface WorkspaceFoldersServerCapabilities ',
    'interface WorkspaceFolder ',
    'interface DidChangeWorkspaceFoldersParams ',
    'interface DidChangeConfigurationClientCapabilities ',
    'interface DidChangeConfigurationParams ',
    'interface ConfigurationParams ',
    'interface DidChangeWatchedFilesClientCapabilities ',
    'interface DidChangeWatchedFilesRegistrationOptions ',
    'interface DidChangeWatchedFilesParams ',
    'interface FileEvent ',
    'interface WorkspaceSymbolClientCapabilities ',
    'interface WorkspaceSymbolOptions ',
    'interface WorkspaceSymbolRegistrationOptions',
    'interface WorkspaceSymbolParams ',
    'interface ExecuteCommandClientCapabilities ',
    'interface ExecuteCommandOptions ',
    'interface ExecuteCommandRegistrationOptions',
    'interface ExecuteCommandParams ',
    'interface ApplyWorkspaceEditParams ',
    'interface ApplyWorkspaceEditResponse ',
    'interface FileOperationRegistrationOptions ',
    'interface CreateFilesParams ',
    'interface RenameFilesParams ',
    'interface DeleteFilesParams ',
    'interface TextDocumentSyncOptions',  #  shorten snippets
    'interface DidOpenTextDocumentParams ',
    'interface TextDocumentChangeRegistrationOptions',
    'interface DidChangeTextDocumentParams ',
    'interface WillSaveTextDocumentParams ',
    'interface SaveOptions ',
    'interface TextDocumentSaveRegistrationOptions',
    'interface DidSaveTextDocumentParams ',
    'interface DidCloseTextDocumentParams ',
    'interface TextDocumentSyncClientCapabilities ',
    'interface PublishDiagnosticsClientCapabilities ',
    'interface PublishDiagnosticsParams ',
    'interface CompletionClientCapabilities ',
    'interface CompletionOptions ',
    'interface CompletionRegistrationOptions',
    'interface CompletionParams ',
    'interface CompletionList ',
    'interface HoverClientCapabilities ',
    'interface HoverOptions ',
    'interface HoverRegistrationOptions',
    'interface HoverParams ',
    'interface Hover ',
    'type MarkedString ',
    'interface SignatureHelpClientCapabilities ',
    'interface SignatureHelpOptions ',
    'interface SignatureHelpRegistrationOptions',
    'interface SignatureHelpParams ',
    'interface SignatureHelp ',
    'interface DeclarationClientCapabilities ',
    'interface DeclarationOptions ',
    'interface DeclarationRegistrationOptions ',
    'interface DeclarationParams ',
    'interface DefinitionClientCapabilities ',
    'interface DefinitionOptions ',
    'interface DefinitionRegistrationOptions ',
    'interface DefinitionParams ',
    'interface TypeDefinitionClientCapabilities ',
    'interface TypeDefinitionOptions ',
    'interface TypeDefinitionRegistrationOptions ',
    'interface TypeDefinitionParams ',
    'interface ImplementationClientCapabilities ',
    'interface ImplementationOptions ',
    'interface ImplementationRegistrationOptions ',
    'interface ImplementationParams ',
    'interface ReferenceClientCapabilities ',
    'interface ReferenceOptions ',
    'interface ReferenceRegistrationOptions ',
    'interface ReferenceParams ',
    'interface DocumentHighlightClientCapabilities ',
    'interface DocumentHighlightOptions ',
    'interface DocumentHighlightRegistrationOptions ',
    'interface DocumentHighlightParams ',
    'interface DocumentHighlight ',
    'interface DocumentSymbolClientCapabilities ',
    'interface DocumentSymbolOptions ',
    'interface DocumentSymbolRegistrationOptions ',
    'interface DocumentSymbolParams ',
    'namespace SymbolKind ',
    'interface CodeActionClientCapabilities ',
    'interface CodeActionOptions ',
    'interface CodeActionRegistrationOptions ',
    'interface CodeActionParams ',
    'interface CodeAction ',
    'textDocument.codeAction.resolveSupport ',
    'Do Foo',
    'interface CodeLensClientCapabilities ',
    'interface CodeLensOptions ',
    'interface CodeLensRegistrationOptions ',
    'interface CodeLensParams ',
    'interface CodeLens ',
    'interface CodeLensWorkspaceClientCapabilities ',
    'interface DocumentLinkClientCapabilities ',
    'interface DocumentLinkOptions ',
    'interface DocumentLinkRegistrationOptions ',
    'interface DocumentLinkParams ',
    'interface DocumentLink ',
    'interface DocumentColorClientCapabilities ',
    'interface DocumentColorOptions ',
    'interface DocumentColorRegistrationOptions ',
    'interface DocumentColorParams ',
    'interface ColorInformation ',
    'interface ColorPresentationParams ',
    'interface ColorPresentation ',
    'interface DocumentFormattingClientCapabilities ',
    'interface DocumentFormattingOptions ',
    'interface DocumentFormattingRegistrationOptions ',
    'interface DocumentFormattingParams ',
    'interface DocumentRangeFormattingClientCapabilities ',
    'interface DocumentRangeFormattingOptions ',
    'interface DocumentRangeFormattingRegistrationOptions ',
    'interface DocumentRangeFormattingParams ',
    'interface DocumentOnTypeFormattingClientCapabilities ',
    'interface DocumentOnTypeFormattingOptions ',
    'interface DocumentOnTypeFormattingRegistrationOptions ',
    'interface DocumentOnTypeFormattingParams ',
    'interface RenameClientCapabilities ',
    'interface RenameOptions ',
    'interface RenameRegistrationOptions ',
    'interface RenameParams ',
    'interface PrepareRenameParams ',
    'interface FoldingRangeClientCapabilities ',
    'interface FoldingRangeOptions ',
    'interface FoldingRangeRegistrationOptions ',
    'interface FoldingRangeParams ',
    'interface FoldingRange ',
    'interface SelectionRangeClientCapabilities ',
    'interface SelectionRangeOptions ',
    'interface SelectionRangeRegistrationOptions ',
    'interface SelectionRangeParams ',
    'interface SelectionRange ',
    'interface CallHierarchyClientCapabilities ',
    'interface CallHierarchyOptions ',
    'interface CallHierarchyRegistrationOptions ',
    'interface CallHierarchyPrepareParams ',
    'interface CallHierarchyItem ',
    'interface CallHierarchyIncomingCallsParams ',
    'interface CallHierarchyIncomingCall ',
    'interface CallHierarchyOutgoingCallsParams ',
    'interface CallHierarchyOutgoingCall ',
    'enum SemanticTokenTypes ',
    'type TokenFormat ',
    'interface SemanticTokensLegend ',
    'line: 2, startChar:  5, length: 3, tokenType: "property"',
    'tokenTypes: [',
    'line: 2, startChar:  5, length: 3, tokenType: 0, tokenModifiers: 3 ',
    'deltaLine: 2, deltaStartChar: 5, length: 3, tokenType: 0, ',
    '2,5,3,0,3,  0,5,4,1,0,  3,2,7,2,0 ',
    'line: 3, startChar:  5, length: 3, tokenType: "property"',
    '3,5,3,0,3,  0,5,4,1,0,  3,2,7,2,0',
    'interface SemanticTokensClientCapabilities ',
    'interface SemanticTokensOptions ',
    'interface SemanticTokensRegistrationOptions ',
    'interface SemanticTokensParams ',
    'interface SemanticTokens ',
    'interface SemanticTokensPartialResult ',
    'interface SemanticTokensDeltaParams ',
    'interface SemanticTokensDelta ',
    'interface SemanticTokensDeltaPartialResult ',
    'interface SemanticTokensRangeParams ',
    'interface SemanticTokensWorkspaceClientCapabilities ',
    'interface LinkedEditingRangeClientCapabilities ',
    'interface LinkedEditingRangeOptions ',
    'interface LinkedEditingRangeRegistrationOptions ',
    'interface LinkedEditingRangeParams ',
    'interface LinkedEditingRanges ',
    'interface MonikerClientCapabilities ',
    'interface MonikerOptions ',
    'interface MonikerRegistrationOptions ',
    'interface MonikerParams ',
    'interface Moniker ',
]

# extract code snippets into a list
list = read_snippets('source/protocol/lsp-messages.ads')

# check if some snippets are not found
rest = [x for x in list if len([p for p in order if x.find(p) >=0 ]) == 0]

if rest:
    print("THESE SNIPPETS ARE NOT MENTIONED")
    for snippet in rest:
        print(snippet)
    print("THESE SNIPPETS ARE NOT MENTIONED")
else:
    # write code snippets in the given order
    for pattern in order:
        emit(list, pattern)

