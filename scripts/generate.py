#!/usr/bin/env python

import os
import os.path
from os.path import join

""" Script to generate the 'generated' parts of the code """

LSP_Messages_Generic_Header = """--  Automatically generated, do not edit.

with Ada.Tags;
with LSP.Generic_{kind}s;
with LSP.JSON_Streams;
with LSP.Server_{kind}_Receivers;
use LSP.Server_{kind}_Receivers;

package LSP.Messages.Server_{kind}s is

   type Server_{kind} is abstract new LSP.Messages.{kind}Message{record};

   function Decode
     (JS : not null access LSP.JSON_Streams.JSON_Stream)
      return Server_{kind} is abstract;

   procedure Visit
     (Self    : Server_{kind};
      Handler : access Server_{kind}_Receiver'Class) is abstract;

   function Method_To_Tag
     (Method : VSS.Strings.Virtual_String) return Ada.Tags.Tag;
   --  For given LSP method return a corresponding message type tag
"""

C_Method_Function_Snippet = """
   function On_{request_name}_Request
     (Self    : access Server_Request_Handler;
      Request : LSP.Messages.Server_Requests.{request_name}_Request)
      return LSP.Messages.Server_Responses.{response_name}
        is abstract;
"""

C_Method_Procedure_Snippet = """
   procedure On_{request_name}_{kind}
     (Self  : access Server_{kind}_Receiver;
      Value : LSP.Messages.{params_name}) is abstract;
"""

C_Method_Procedure_Snippet_Noparam = """
   procedure On_{request_name}_{kind}
     (Self  : access Server_{kind}_Receiver) is abstract;
"""

C_Method_Request_Snippet = """
   procedure On_{request_name}_{kind}
     (Self  : access Server_{kind}_Receiver;
      Value : LSP.Messages.Server_{kind}s.{request_name}_{kind})
        is abstract;
"""

LSP_Messages_Generic_Body_Header = """--  Automatically generated, do not edit.

package body LSP.Messages.Server_{kind}s is

   --  These messages are sent from client to server.

   Map : Maps.Map;

   function Method_To_Tag
     (Method : VSS.Strings.Virtual_String) return Ada.Tags.Tag is
   begin
      return Method_To_Tag (Map, Method);
   end Method_To_Tag;
"""

C_Method_Visit_Body_Snippet = """
   overriding procedure Visit
     (Self    : {request_name}_{kind};
      Handler : access Server_{kind}_Receiver'Class) is
   begin
      Handler.On_{request_name}_{kind} (Self{params});
   end Visit;
"""

C_Method_Visit_Body_Snippet_Noparams = """
   overriding procedure Visit
     (Self    : {request_name}_{kind};
      Handler : access Server_{kind}_Receiver'Class)
   is
      pragma Unreferenced (Self);
   begin
      Handler.On_{request_name}_{kind};
   end Visit;
"""

C_Method_Decode_Body_Snippet = """
   overriding function Decode
     (JS : not null access LSP.JSON_Streams.JSON_Stream)
      return {request_name}_{kind} is
   begin
      return V : {request_name}_{kind} do
         {kind}Message'Read (JS, {kind}Message (V));
      end return;
   end Decode;
"""

C_Handler_Procedure_Body = """--  Automatically generated, do not edit.

with LSP.Messages.Server_Notifications; use LSP.Messages.Server_Notifications;

procedure LSP.Servers.Handle_{kind}
  (Self         : not null LSP.Server_Notification_Handlers
     .Server_Notification_Handler_Access;
   {kind} : LSP.Messages.{kind}Message'Class) is
begin
{handler_snippets}
end LSP.Servers.Handle_{kind};
"""

C_Handler_Function_Body = """--  Automatically generated, do not edit.

with LSP.Messages.Server_Requests; use LSP.Messages.Server_Requests;

function LSP.Servers.Handle_{kind}
  (Self    : not null Server_Request_Handlers
     .Server_Request_Handler_Access;
   {kind} : LSP.Messages.{kind}Message'Class)
      return LSP.Messages.ResponseMessage'Class is
begin
{handler_snippets}
   return LSP.Messages.ResponseMessage'
     (Is_Error => True,
      jsonrpc  => <>,
      id       => <>,
      error    =>
        (Is_Set => True,
         Value  =>
           (code    => LSP.Messages.MethodNotFound,
            message => "The {kind} handler doesn't support this",
            others  => <>)));
end LSP.Servers.Handle_{kind};
"""

C_Handler_Snippet_Function = """
      if {kind} in {request_name}_{kind}'Class then
         declare
            R : LSP.Messages.ResponseMessage'Class :=
               Self.On_{request_name}_{kind}
                  ({request_name}_{kind} ({kind}));
         begin
            R.jsonrpc := "2.0";
            R.id := Request.id;
            return R;
         end;
      end if;
"""

C_Handler_Snippet_Procedure = """
      if {kind} in {request_name}_{kind}'Class then
         Self.On_{request_name}_{kind}
            (({request_name}_{kind} ({kind}).params));
         return;
      end if;
"""

C_Handler_Snippet_Procedure_Noparams = """
      if {kind} in {request_name}_{kind}'Class then
         Self.On_{request_name}_{kind};
         return;
      end if;
"""

LSP_Messages_Body_Begin = """
begin"""

LSP_Messages_Insert = """

   Map.Insert
     ("{protocol_name}",
      {request_name}_{kind}'Tag);"""

LSP_Messages_Generic_Footer = """
end LSP.Messages.Server_{kind}s;
"""

LSP_Messages_Generic_Type_Snippet = """
   package {request_name}_{kind}s is
     new LSP.Generic_{kind}s
       (Server_{kind},
        {params_name},
        Server_{kind}_Receiver'Class);

   type {request_name}_{kind} is
     new {request_name}_{kind}s.{kind} with null record;

   overriding procedure Visit
     (Self    : {request_name}_{kind};
      Handler : access Server_{kind}_Receiver'Class);
"""

LSP_Messages_Generic_Type_Snippet_Noparams = """
   type {request_name}_{kind} is new Server_{kind} with null record;

   overriding function Decode
     (JS : not null access LSP.JSON_Streams.JSON_Stream)
      return {request_name}_{kind};

   overriding procedure Visit
     (Self    : {request_name}_{kind};
      Handler : access Server_{kind}_Receiver'Class);
"""

LSP_Server_Handlers_Header = """--  Automatically generated, do not edit.

pragma Style_Checks (Off);

with LSP.Messages.Server_Requests;
with LSP.Messages.Server_Responses;

package LSP.Server_Request_{handler}s is

   type Server_Request_{handler} is limited interface;
   type Server_Request_{handler}_Access is
     access all Server_Request_{handler}'Class;
   --  A type which represents a handler which supports reacting
   --  to Requests. Clients implementing this interface should override
   --  the *_Request methods, and clients making use of this interface
   --  should simply call Handle_Request when they want to dispatch
   --  a Request to the handler.
"""

LSP_Server_Handlers_Footer = """
end LSP.Server_{kind}_{handler}s;
"""

LSP_Server_Recievers_Header = """--  Automatically generated, do not edit.

limited with LSP.Messages{extra_with};

package LSP.Server_{kind}_{handler}s is

   type Server_{kind}_{handler} is limited interface;
   type Server_{kind}_{handler}_Access is
     access all Server_{kind}_{handler}'Class;
   --  A type which represents a handler which supports reacting
   --  to {kind}s. Clients implementing this interface should override
   --  the *_{kind} methods, and clients making use of this interface
   --  should simply call corresponding method when they want to dispatch
   --  a {kind} to the handler.
"""

basedir = os.path.abspath(os.path.join(os.path.dirname(__file__), '..'))


REQUESTS = [
    ('initialize', 'Initialize', 'InitializeParams', 'Initialize_Response'),
    ('shutdown', 'Shutdown', None, 'Shutdown_Response'),
    ('textDocument/codeAction', 'CodeAction', 'CodeActionParams',
     'CodeAction_Response'),
    ('textDocument/completion', 'Completion', 'TextDocumentPositionParams',
     'Completion_Response'),
    ('completionItem/resolve', 'CompletionItemResolve', 'CompletionItem',
     'CompletionItemResolve_Response'),
    ('textDocument/definition', 'Definition', 'DefinitionParams',
     'Location_Link_Response'),
    ('textDocument/declaration', 'Declaration', 'DeclarationParams',
     'Location_Link_Response'),
    ('textDocument/implementation', 'Implementation',
     'ImplementationParams', 'Location_Link_Response'),
    ('textDocument/typeDefinition', 'Type_Definition',
     'TextDocumentPositionParams', 'Location_Link_Response'),
    ('textDocument/documentHighlight', 'Highlight', 'TextDocumentPositionParams',
     'Highlight_Response'),
    ('textDocument/hover', 'Hover', 'TextDocumentPositionParams',
     'Hover_Response'),
    ('textDocument/documentLink', 'Document_Links',
     'DocumentLinkParams', 'Links_Response'),
    ('textDocument/references', 'References', 'ReferenceParams',
     'Location_Response'),
    ('textDocument/signatureHelp', 'Signature_Help',
     'SignatureHelpParams', 'SignatureHelp_Response'),
    ('textDocument/documentSymbol', 'Document_Symbols',
     'DocumentSymbolParams', 'Symbol_Response'),
    ('textDocument/rename', 'Rename', 'RenameParams', 'Rename_Response'),
    ('textDocument/prepareRename', 'Prepare_Rename',
     'PrepareRenameParams', 'Prepare_Rename_Response'),
    ('textDocument/executeCommand', 'Execute_Command', 'ExecuteCommandParams',
     'ExecuteCommand_Response'),
    ('textDocument/documentColor', 'Document_Color', 'DocumentColorParams',
     'DocumentColor_Response'),
    ('textDocument/colorPresentation', 'Color_Presentation',
     'ColorPresentationParams', 'ColorPresentation_Response'),
    ('textDocument/foldingRange', 'Folding_Range',
     'FoldingRangeParams', 'FoldingRange_Response'),
    ('textDocument/formatting', 'Formatting',
     'DocumentFormattingParams', 'Formatting_Response'),
    ('textDocument/rangeFormatting', 'Range_Formatting',
     'DocumentRangeFormattingParams', 'Range_Formatting_Response'),
    ('textDocument/selectionRange', 'Selection_Range',
     'SelectionRangeParams', 'SelectionRange_Response'),
    ('textDocument/semanticTokens/full', 'Document_Tokens_Full',
     'SemanticTokensParams', 'SemanticTokens_Response'),
    ('textDocument/semanticTokens/range', 'Document_Tokens_Range',
     'SemanticTokensRangeParams', 'SemanticTokens_Response'),
    ('textDocument/prepareCallHierarchy', 'Prepare_Call_Hierarchy',
     'CallHierarchyPrepareParams', 'PrepareCallHierarchy_Response'),
    ('callHierarchy/incomingCalls', 'Incoming_Calls',
     'CallHierarchyIncomingCallsParams', 'IncomingCalls_Response'),
    ('callHierarchy/outgoingCalls', 'Outgoing_Calls',
     'CallHierarchyOutgoingCallsParams', 'OutgoingCalls_Response'),
    ('workspace/symbol', 'Workspace_Symbols', 'WorkspaceSymbolParams',
     'Symbol_Response'),
    ('workspace/executeCommand', 'Workspace_Execute_Command',
     'ExecuteCommandParams', 'ExecuteCommand_Response'),
    ('workspace/willCreateFiles', 'Workspace_Will_Create_Files',
     'CreateFilesParams', 'WillCreateFiles_Response'),
    ('workspace/willRenameFiles', 'Workspace_Will_Rename_Files',
     'RenameFilesParams', 'WillRenameFiles_Response'),
    ('workspace/willDeleteFiles', 'Workspace_Will_Delete_Files',
     'DeleteFilesParams', 'WillDeleteFiles_Response'),

    # ALS-specific requests
    ('textDocument/alsShowDependencies', 'ALS_Show_Dependencies',
     'ALS_ShowDependenciesParams',
     'ALS_ShowDependencies_Response'),
    ('workspace/alsSourceDirs', 'ALS_Source_Dirs',
     None,
     'ALS_SourceDirs_Response'),
    ('$/alsDebug', 'ALS_Debug', 'ALSDebugParams', 'ALS_Debug_Response'),
    ('$/alsCheckSyntax', 'ALS_Check_Syntax', 'ALS_Check_Syntax_Params',
     'ALS_Check_Syntax_Response'),
]
# Names of requests in the form (protocol name, Ada name, parameter name,
#   response name)

NOTIFICATIONS = [
    ('initialized', 'Initialized', None),
    ('exit', 'Exit', None),

    ('workspace/didChangeConfiguration', 'DidChangeConfiguration',
     'DidChangeConfigurationParams'),
    ('workspace/didChangeWorkspaceFolders', 'DidChangeWorkspaceFolders',
     'DidChangeWorkspaceFoldersParams'),
    ('workspace/didChangeWatchedFiles', 'DidChangeWatchedFiles',
     'DidChangeWatchedFilesParams'),
    ('workspace/didCreateFiles', 'DidCreateFiles',
     'CreateFilesParams'),
    ('workspace/didRenameFiles', 'DidRenameFiles',
     'RenameFilesParams'),
    ('workspace/didDeleteFiles', 'DidDeleteFiles',
     'DeleteFilesParams'),

    ('$/cancelRequest', 'Cancel', 'CancelParams'),

    # TODO: rename these to TextDocumentDidOpen/DidChange/DidSave/DidClose?
    ('textDocument/didOpen', 'DidOpenTextDocument',
     'DidOpenTextDocumentParams'),
    ('textDocument/didChange', 'DidChangeTextDocument',
     'DidChangeTextDocumentParams'),
    ('textDocument/didSave', 'DidSaveTextDocument',
     'DidSaveTextDocumentParams'),
    ('textDocument/didClose', 'DidCloseTextDocument',
     'DidCloseTextDocumentParams'),
]


def write_message_types():
    """ Write source/protocol/lsp-messages-request.* """

    def write_package(data_array, kind, record, ads_name, handler_is_procedure):
        """Factorization function"""

        # Write the .ads
        with open(ads_name, 'w') as ads:
            ads.write(LSP_Messages_Generic_Header.format(kind=kind, record=record))

            for l in data_array:
                request_name = l[1]
                params_name = l[2]
                if params_name:
                    ads.write(LSP_Messages_Generic_Type_Snippet.format(
                              request_name=request_name,
                              params_name=params_name,
                              kind=kind))
                else:
                    ads.write(
                        LSP_Messages_Generic_Type_Snippet_Noparams.format(
                            request_name=request_name,
                            kind=kind))

            ads.write(LSP_Messages_Generic_Footer.format(kind=kind))

    def write_body(data_array, kind, adb_name, handler_is_procedure):
        """Factorization function"""

        # Write the .adb
        with open(adb_name, 'w') as adb:
            adb.write(LSP_Messages_Generic_Body_Header.format(kind=kind))

            for l in data_array:
                request_name = l[1]
                params_name = l[2]

                if not params_name:
                    adb.write(
                        C_Method_Decode_Body_Snippet.format(
                            request_name=request_name,
                            kind=kind))

                if params_name or not handler_is_procedure:
                    adb.write(
                        C_Method_Visit_Body_Snippet.format(
                            request_name=request_name,
                            kind=kind,
                            params=".params" if handler_is_procedure else ""))
                else:
                    adb.write(
                        C_Method_Visit_Body_Snippet_Noparams.format(
                            request_name=request_name,
                            kind=kind))

            adb.write(LSP_Messages_Body_Begin.format(kind=kind))

            for l in data_array:
                protocol_name = l[0]
                request_name = l[1]
                params_name = l[2]
                adb.write(LSP_Messages_Insert.format(
                    protocol_name=protocol_name,
                    request_name=request_name,
                    kind=kind))

            adb.write(LSP_Messages_Generic_Footer.format(kind=kind))

    gen_dir = join(basedir, 'source', 'protocol', 'generated')
    write_package(REQUESTS, 'Request',
                  """ with record\n      Canceled : Boolean := False with Atomic;\n   end record""",
                  join(gen_dir, 'lsp-messages-server_requests.ads'),
                  False)
    write_package(NOTIFICATIONS, 'Notification', '\n     with null record',
                  join(gen_dir, 'lsp-messages-server_notifications.ads'),
                  True)
    write_body(NOTIFICATIONS, 'Notification',
               join(gen_dir, 'lsp-messages-server_notifications.adb'),
               True)
    write_body(REQUESTS, 'Request',
               join(gen_dir, 'lsp-messages-server_requests.adb'),
               False)


def write_handle_request():
    def write_package(data_array, kind, adb_name, handler_is_procedure):
        """Factorization function"""

        # Write the .adb
        with open(adb_name, 'w') as adb:
            handler_snippets = ""

            # Generate the snippets
            for l in data_array:
                protocol_name = l[0]
                request_name = l[1]
                if handler_is_procedure:
                    handler_snippets += \
                        C_Handler_Snippet_Procedure.format(
                            request_name=request_name,
                            kind=kind)
                else:
                    handler_snippets += \
                        C_Handler_Snippet_Function.format(
                            request_name=request_name,
                            kind=kind)

            if handler_is_procedure:
                adb.write(C_Handler_Procedure_Body.format(
                              kind=kind,
                              handler_snippets=handler_snippets))
            else:
                adb.write(C_Handler_Function_Body.format(
                              kind=kind,
                              handler_snippets=handler_snippets))

    gen_dir = join(basedir, 'source', 'server', 'generated')
    write_package(REQUESTS, 'Request',
                  join(gen_dir, 'lsp-servers-handle_request.adb'),
                  False)



def write_server_handlers():
    """ Write source/server/lsp-server_{request/notification}_handlers.ads """

    def write_package(data_array, handler_name,
                      ads_name, handler_is_procedure):
        """Factorization function"""

        # Write the .ads
        with open(ads_name, 'w') as ads:

            ads.write(LSP_Server_Handlers_Header.format(
                handler=handler_name))

            for l in data_array:
                ads.write(
                    C_Method_Function_Snippet.format(
                        request_name=l[1],
                        response_name=l[3]))

            ads.write("""
   procedure Handle_Error
     (Self  : access Server_Request_Handler) is null;
   --  This procedure will be called when an unexpected error is raised in the
   --  request processing loop.
""")

            ads.write(LSP_Server_Handlers_Footer.format(
                kind='Request', handler=handler_name))

    gen_dir = join(basedir, 'source', 'server', 'generated')
    write_package(REQUESTS, 'Handler',
                  join(gen_dir, 'lsp-server_request_handlers.ads'),
                  False)


def write_server_receivers():
    """ Write source/server/lsp-server_{request/notification}_handlers.ads """

    def write_package(data_array, kind, handler_name,
                      ads_name, is_request):
        """Factorization function"""

        # Write the .ads
        with open(ads_name, 'w') as ads:
            ads.write(LSP_Server_Recievers_Header.format(
                kind=kind, handler=handler_name,
                extra_with=".Server_Requests" if is_request else""))

            for l in data_array:
                request_name = l[1]
                params_name = l[2]
                if params_name:
                    if is_request:
                        ads.write(
                            C_Method_Request_Snippet.format(
                                request_name=request_name,
                                params_name=params_name,
                                response_name=l[3],
                                kind=kind))
                    else:
                        ads.write(
                            C_Method_Procedure_Snippet.format(
                                request_name=request_name,
                                params_name=params_name,
                                kind=kind))
                else:
                    if is_request:
                        ads.write(
                            C_Method_Request_Snippet.format(
                                request_name=request_name,
                                params_name=params_name,
                                response_name=l[3],
                                kind=kind))
                    else:
                        ads.write(
                            C_Method_Procedure_Snippet_Noparam.format(
                                request_name=request_name,
                                kind=kind))

            ads.write(LSP_Server_Handlers_Footer.format(
                kind=kind, handler=handler_name))

    gen_dir = join(basedir, 'source', 'protocol', 'generated')
    write_package(REQUESTS, 'Request', 'Receiver',
                  join(gen_dir, 'lsp-server_request_receivers.ads'),
                  True)
    write_package(NOTIFICATIONS, 'Notification', 'Receiver',
                  join(gen_dir, 'lsp-server_notification_receivers.ads'),
                  False)


if __name__ == '__main__':
    write_message_types()
    write_handle_request()
    write_server_handlers()
    write_server_receivers()
