#!/usr/bin/env python

import os
import os.path
from os.path import join

""" Script to generate the 'generated' parts of the code """

LSP_Messages_Generic_Header = """--  Automatically generated, do not edit.

with LSP.Generic_{kind}s;
with LSP.Server_{kind}_Receivers;
use LSP.Server_{kind}_Receivers;

package LSP.Messages.Server_{kind}s is

   type Server_{kind} is abstract new LSP.Messages.{kind}Message
     with null record;

   procedure Visit
     (Self    : Server_{kind};
      Handler : access Server_{kind}_Receiver'Class) is abstract;
"""

C_Method_Function_Snippet = """
   function On_{request_name}_{kind}
     (Self  : access Server_{kind}_Handler;
      Value : LSP.Messages.{params_name})
      return LSP.Messages.Server_Responses.{response_name} is abstract;
"""

C_Method_Function_Snippet_Noparam = """
   function On_{request_name}_{kind}
     (Self : access Server_{kind}_Handler)
      return LSP.Messages.Server_Responses.{response_name} is abstract;
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

LSP_Messages_Generic_Body_Header = """--  Automatically generated, do not edit.

package body LSP.Messages.Server_{kind}s is

   --  These messages are sent from client to server.
"""

C_Method_Visit_Body_Snippet = """
   overriding procedure Visit
     (Self    : {request_name}_{kind};
      Handler : access Server_{kind}_Receiver'Class) is
   begin
      Handler.On_{request_name}_{kind} (Self.params);
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

LSP_Servers_Decode_Body = """--  Automatically generated, do not edit.

with Ada.Strings.UTF_Encoding;
with LSP.JSON_Streams;
with LSP.Types; use LSP.Types;
with LSP.Messages.Common_Writers; use LSP.Messages.Common_Writers;
with LSP.Messages.Server_{kind}s; use LSP.Messages.Server_{kind}s;

function LSP.Servers.Decode_{kind}
   (Document : GNATCOLL.JSON.JSON_Value)
    return LSP.Messages.{kind}Message'Class
is
   function "+" (Text : Ada.Strings.UTF_Encoding.UTF_8_String)
      return LSP.Types.LSP_String renames
       LSP.Types.To_LSP_String;

   JS : aliased LSP.JSON_Streams.JSON_Stream;
   JSON_Array : GNATCOLL.JSON.JSON_Array;

   Method     : LSP.Types.LSP_String;

begin
   GNATCOLL.JSON.Append (JSON_Array, Document);
   JS.Set_JSON_Document (JSON_Array);
   JS.Start_Object;

   LSP.Types.Read_String (JS, +"method", Method);
{decode_snippets}
   raise Program_Error; --  {kind} not found
end LSP.Servers.Decode_{kind};
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
with Ada.Strings.UTF_Encoding;

function LSP.Servers.Handle_{kind}
  (Self    : not null Server_Request_Handlers
     .Server_Request_Handler_Access;
   {kind} : LSP.Messages.{kind}Message'Class)
      return LSP.Messages.ResponseMessage'Class
is
   function "+" (Text : Ada.Strings.UTF_Encoding.UTF_8_String)
     return LSP.Types.LSP_String renames LSP.Types.To_LSP_String;
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
            message => +"The {kind} handler doesn't support this",
            others  => <>)));
end LSP.Servers.Handle_{kind};
"""

C_Decode_Snippet = """
   if To_UTF_8_String (Method) = "{protocol_name}" then
      declare
         R : {request_name}_{kind};
      begin
         Set_Common_{kind}_Fields (R, JS);
         JS.Key ("params");
         LSP.Messages.{params_name}'Read (JS'Access, R.params);
         return R;
      end;
   end if;
"""

C_Handler_Snippet_Function = """
      if {kind} in {request_name}_{kind}'Class then
         declare
            R : LSP.Messages.ResponseMessage'Class :=
               Self.On_{request_name}_{kind}
                  ({request_name}_{kind} ({kind}).params);
         begin
            R.jsonrpc := +"2.0";
            R.id := Request.id;
            return R;
         end;
      end if;
"""

C_Handler_Snippet_Function_Noparams = """
      if {kind} in {request_name}_{kind}'Class then
         declare
            R : LSP.Messages.ResponseMessage'Class :=
               Self.On_{request_name}_{kind};
         begin
            R.jsonrpc := +"2.0";
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

C_Decode_Snippet_Noparams = """
      if To_UTF_8_String (Method) = "{protocol_name}" then
         declare
            R : {request_name}_{kind};
         begin
            Set_Common_{kind}_Fields (R, JS);
            return R;
         end;
      end if;
"""

LSP_Messages_Generic_Footer = """
end LSP.Messages.Server_{kind}s;
"""

LSP_Messages_Generic_Type_Snippet = """
   package {request_name}_{kind}s is
     new LSP.Generic_{kind}s
       (Server_{kind},
        {params_name});

   type {request_name}_{kind} is
     new {request_name}_{kind}s.{kind} with null record;

   overriding procedure Visit
     (Self    : {request_name}_{kind};
      Handler : access Server_Notification_Receiver'Class);
"""

LSP_Messages_Generic_Type_Snippet_Noparams = """
   type {request_name}_{kind} is new Server_{kind} with null record;

   overriding procedure Visit
     (Self    : {request_name}_{kind};
      Handler : access Server_Notification_Receiver'Class);
"""

LSP_Server_Handlers_Header = """--  Automatically generated, do not edit.

with LSP.Messages{extra_with};

package LSP.Server_{kind}_{handler}s is

   type Server_{kind}_{handler} is limited interface;
   type Server_{kind}_{handler}_Access is
     access all Server_{kind}_{handler}'Class;
   --  A type which represents a handler which supports reacting
   --  to {kind}s. Clients implementing this interface should override
   --  the *_{kind} methods, and clients making use of this interface
   --  should simply call Handle_{kind} when they want to dispatch
   --  a {kind} to the handler.
"""

LSP_Server_Handlers_Footer = """
end LSP.Server_{kind}_{handler}s;
"""

basedir = os.path.abspath(os.path.join(os.path.dirname(__file__), '..'))


REQUESTS = [
    ('initialize', 'Initialize', 'InitializeParams', 'Initialize_Response'),
    ('shutdown', 'Shutdown', None, 'Shutdown_Response'),
    ('textDocument/codeAction', 'CodeAction', 'CodeActionParams',
     'CodeAction_Response'),
    ('textDocument/completion', 'Completion', 'TextDocumentPositionParams',
     'Completion_Response'),
    ('textDocument/definition', 'Definition', 'TextDocumentPositionParams',
     'Location_Response'),
    ('textDocument/typeDefinition', 'Type_Definition',
     'TextDocumentPositionParams', 'Location_Response'),
    ('textDocument/highight', 'Highlight', 'TextDocumentPositionParams',
     'Highlight_Response'),
    ('textDocument/hover', 'Hover', 'TextDocumentPositionParams',
     'Hover_Response'),
    ('textDocument/references', 'References', 'ReferenceParams',
     'Location_Response'),
    ('textDocument/signatureHelp', 'Signature_Help',
     'TextDocumentPositionParams', 'SignatureHelp_Response'),
    ('textDocument/documentSymbol', 'Document_Symbols',
     'DocumentSymbolParams', 'Symbol_Response'),
    ('textDocument/rename', 'Rename', 'RenameParams', 'Rename_Response'),
    ('textDocument/executeCommand', 'Execute_Command', 'ExecuteCommandParams',
     'ExecuteCommand_Response'),
    ('workspace/symbol', 'Workspace_Symbols', 'WorkspaceSymbolParams',
     'Symbol_Response'),
    ('workspace/executeCommand', 'Workspace_Execute_Command',
     'ExecuteCommandParams', 'ExecuteCommand_Response'),

    # ALS-specific requests
    ('textDocument/alsCalledBy', 'ALS_Called_By', 'TextDocumentPositionParams',
     'ALS_Called_By_Response'),
]
# Names of requests in the form (protocol name, Ada name, parameter name,
#   response name)

NOTIFICATIONS = [
    ('initialized', 'Initialized', None),
    ('exit', 'Exit', None),

    ('workspace/didChangeConfiguration', 'DidChangeConfiguration',
     'DidChangeConfigurationParams'),

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

    def write_package(data_array, kind, ads_name, handler_is_procedure):
        """Factorization function"""

        # Write the .ads
        with open(ads_name, 'wb') as ads:
            ads.write(LSP_Messages_Generic_Header.format(kind=kind))

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

        # Write the .ads
        with open(adb_name, 'wb') as adb:
            adb.write(LSP_Messages_Generic_Body_Header.format(kind=kind))

            for l in data_array:
                request_name = l[1]
                params_name = l[2]
                if params_name:
                    adb.write(
                        C_Method_Visit_Body_Snippet.format(
                            request_name=request_name,
                            kind=kind))
                else:
                    adb.write(
                        C_Method_Visit_Body_Snippet_Noparams.format(
                            request_name=request_name,
                            kind=kind))

            adb.write(LSP_Messages_Generic_Footer.format(kind=kind))

    gen_dir = join(basedir, 'source', 'protocol', 'generated')
    write_package(REQUESTS, 'Request',
                  join(gen_dir, 'lsp-messages-server_requests.ads'),
                  False)
    write_package(NOTIFICATIONS, 'Notification',
                  join(gen_dir, 'lsp-messages-server_notifications.ads'),
                  True)
    write_body(NOTIFICATIONS, 'Notification',
               join(gen_dir, 'lsp-messages-server_notifications.adb'),
               True)


def write_handle_request():
    def write_package(data_array, kind, adb_name, handler_is_procedure):
        """Factorization function"""

        # Write the .adb
        with open(adb_name, 'wb') as adb:
            handler_snippets = ""

            # Generate the snippets
            for l in data_array:
                protocol_name = l[0]
                request_name = l[1]
                params_name = l[2]
                if params_name:
                    if handler_is_procedure:
                        handler_snippets += \
                            C_Handler_Snippet_Procedure.format(
                                request_name=request_name,
                                params_name=params_name,
                                kind=kind)
                    else:
                        handler_snippets += \
                            C_Handler_Snippet_Function.format(
                                request_name=request_name,
                                params_name=params_name,
                                kind=kind)

                else:
                    if handler_is_procedure:
                        handler_snippets += \
                            C_Handler_Snippet_Procedure_Noparams.format(
                                request_name=request_name,
                                params_name=params_name,
                                kind=kind)
                    else:
                        handler_snippets += \
                            C_Handler_Snippet_Function_Noparams.format(
                                request_name=request_name,
                                params_name=params_name,
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
#    write_package(NOTIFICATIONS, 'Notification',
#                  join(gen_dir, 'lsp-servers-handle_notification.adb'),
#                  True)


def write_message_decoders():
    """ Write source/server/lsp-servers-decode_*.adb """

    def write_package(data_array, kind, adb_name,
                      handler_is_procedure):
        """Factorization function"""

        # Write the .adb
        with open(adb_name, 'wb') as adb:
            decode_snippets = ""

            # Generate the snippets
            for l in data_array:
                protocol_name = l[0]
                request_name = l[1]
                params_name = l[2]
                if params_name:
                    decode_snippets += \
                        C_Decode_Snippet.format(
                            protocol_name=protocol_name,
                            request_name=request_name,
                            params_name=params_name,
                            kind=kind)

                else:
                    decode_snippets += \
                        C_Decode_Snippet_Noparams.format(
                            protocol_name=protocol_name,
                            request_name=request_name,
                            kind=kind)

            adb.write(LSP_Servers_Decode_Body.format(
                            decode_snippets=decode_snippets,
                            kind=kind))

    gen_dir = join(basedir, 'source', 'server', 'generated')
    write_package(REQUESTS, 'Request',
                  join(gen_dir, 'lsp-servers-decode_request.adb'),
                  False)
    write_package(NOTIFICATIONS, 'Notification',
                  join(gen_dir, 'lsp-servers-decode_notification.adb'),
                  True)


def write_server_handlers():
    """ Write source/server/lsp-server_{request/notification}_handlers.ads """

    def write_package(data_array, kind, handler_name,
                      ads_name, handler_is_procedure):
        """Factorization function"""

        # Write the .ads
        with open(ads_name, 'wb') as ads:
            extra_with = '' if handler_is_procedure else '.Server_Responses'

            ads.write(LSP_Server_Handlers_Header.format(
                kind=kind, handler=handler_name,
                extra_with=extra_with))

            for l in data_array:
                request_name = l[1]
                params_name = l[2]
                if params_name:
                    if handler_is_procedure:
                        ads.write(
                            C_Method_Procedure_Snippet.format(
                                request_name=request_name,
                                params_name=params_name,
                                kind=kind))
                    else:
                        ads.write(
                            C_Method_Function_Snippet.format(
                                request_name=request_name,
                                params_name=params_name,
                                response_name=l[3],
                                kind=kind))
                else:
                    if handler_is_procedure:
                        ads.write(
                            C_Method_Procedure_Snippet_Noparam.format(
                                request_name=request_name,
                                kind=kind))
                    else:
                        ads.write(
                            C_Method_Function_Snippet_Noparam.format(
                                request_name=request_name,
                                response_name=l[3],
                                kind=kind))

            if not handler_is_procedure:
                ads.write("""
   procedure Handle_Error
     (Self  : access Server_Request_Handler) is null;
   --  This procedure will be called when an unexpected error is raised in the
   --  request processing loop.
""")

            ads.write(LSP_Server_Handlers_Footer.format(
                kind=kind, handler=handler_name))

    gen_dir = join(basedir, 'source', 'protocol', 'generated')
    write_package(REQUESTS, 'Request', 'Handler',
                  join(gen_dir, 'lsp-server_request_handlers.ads'),
                  False)
    write_package(NOTIFICATIONS, 'Notification', 'Receiver',
                  join(gen_dir, 'lsp-server_notification_receivers.ads'),
                  True)


if __name__ == '__main__':
    write_message_types()
    write_handle_request()
    write_message_decoders()
    write_server_handlers()
