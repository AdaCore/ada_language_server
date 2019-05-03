#!/usr/bin/env python

import os
import os.path
from os.path import join

""" Script to generate the 'generated' parts of the code """

LSP_Messages_Generic_Header = """--  Automatically generated, do not edit.

with Ada.Streams;
with GNATCOLL.JSON; use GNATCOLL.JSON;

package LSP.Messages.{kind}s is

   function Decode_{kind}
     (Document : JSON_Value) return {kind}Message'Class;
   --  Decode the request present in the input document. Document is a JSON
   --  representation of the protocol string.
"""

LSP_Messages_Generic_Body_Header = """--  Automatically generated, do not edit.

with Ada.Strings.UTF_Encoding;
with LSP.JSON_Streams;
with LSP.Types; use LSP.Types;
with LSP.Messages.Common_Writers; use LSP.Messages.Common_Writers;

package body LSP.Messages.{kind}s is

   function "+" (Text : Ada.Strings.UTF_Encoding.UTF_8_String)
      return LSP.Types.LSP_String renames
       LSP.Types.To_LSP_String;

   function Decode_{kind}
      (Document : JSON_Value) return {kind}Message'Class
   is
      JS : aliased LSP.JSON_Streams.JSON_Stream;
      JSON_Array : GNATCOLL.JSON.JSON_Array;

      Method     : LSP.Types.LSP_String;

   begin
      GNATCOLL.JSON.Append (JSON_Array, Document);
      JS.Set_JSON_Document (JSON_Array);
      JS.Start_Object;

      LSP.Types.Read_String (JS, +"method", Method);
{snippets}
      raise Program_Error; --  {kind} not found
   end Decode_{kind};
"""

LSP_Message_Generic_Decode_Snippet = """
      if To_UTF_8_String (Method) = "{protocol_name}" then
         declare
            R : {request_name}_{kind};
         begin
            Set_Common_{kind}_Fields (R, JS);
            JS.Key ("params");
            {params_name}'Read (JS'Access, R.params);
            return R;
         end;
      end if;
"""

LSP_Message_Generic_Decode_Snippet_Noparams = """
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
end LSP.Messages.{kind}s;
"""

LSP_Messages_Generic_Type_Snippet = """
   type {request_name}_{kind} is new {kind}Message with
   record
      params : {params_name};
   end record;
"""

LSP_Messages_Generic_Type_Snippet_Noparams = """
   type {request_name}_{kind} is new {kind}Message with null record;
"""

LSP_Messages_Private_Snippet = """
   procedure Read
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out {request_name}_{kind});
   procedure Write
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : {request_name}_{kind});
   for {request_name}_{kind}'Read use Read;
   for {request_name}_{kind}'Write use Write;"""

LSP_Messages_Generic_Write_Snippet = """
   procedure Read
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out {request_name}_{kind})
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Set_Common_{kind}_Fields (V, JS);
      JS.Key ("params");
      {params_name}'Read (S, V.params);
      JS.End_Object;
   end Read;

   procedure Write
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : {request_name}_{kind})
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Write_{kind}_Prefix (S, V);
      JS.Key ("params");
      {params_name}'Write (S, V.params);
      JS.End_Object;
   end Write;
"""

LSP_Messages_Generic_Write_Snippet_Noparams = """
   procedure Read
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out {request_name}_{kind})
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Set_Common_{kind}_Fields (V, JS);
      JS.End_Object;
   end Read;

   procedure Write
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : {request_name}_{kind})
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Write_{kind}_Prefix (S, V);
      JS.End_Object;
   end Write;
"""

basedir = os.path.abspath(os.path.join(os.path.dirname(__file__), '..'))


REQUESTS = [
    ('initialize', 'Initialize', 'InitializeParams'),
    ('shutdown', 'Shutdown', None),
    ('window/showMessageRequest', 'ShowMessage', 'ShowMessageRequestParams'),
    ('textDocument/codeAction', 'CodeAction', 'CodeActionParams'),
    ('textDocument/completion', 'Completion', 'TextDocumentPositionParams'),
    ('textDocument/definition', 'Definition', 'TextDocumentPositionParams'),
    ('textDocument/highight', 'Highlight', 'TextDocumentPositionParams'),
    ('textDocument/hover', 'Hover', 'TextDocumentPositionParams'),
    ('textDocument/references', 'References', 'ReferenceParams'),
    ('textDocument/signatureHelp', 'Signature_Help',
     'TextDocumentPositionParams'),
    ('textDocument/documentSymbol', 'Document_Symbols',
     'DocumentSymbolParams'),
    ('textDocument/executeCommand', 'Execute_Command', 'ExecuteCommandParams'),
    # TODO: rename ApplyWorkspaceEdit to Workspace_Apply_Edit, for consistency
    ('workspace/applyEdit', 'ApplyWorkspaceEdit', 'ApplyWorkspaceEditParams'),
    ('workspace/symbol', 'Workspace_Symbols', 'WorkspaceSymbolParams'),
    ('workspace/executeCommand', 'Workspace_Execute_Command',
     'ExecuteCommandParams'),
]
# Names of requests in the form (protocol name, Ada name, parameter name)

NOTIFICATIONS = [
    ('initialized', 'Initialized', None),
    ('exit', 'Exit', None),

    ('workspace/didChangeConfiguration', 'DidChangeConfiguration', 'DidChangeConfigurationParams'),

    ('window/showMessage', 'ShowMessage', 'ShowMessageParams'),
    ('window/logMessage', 'LogMessage', 'LogMessageParams'),

    ('textDocument/publishDiagnostics', 'PublishDiagnostics',
     'PublishDiagnosticsParams'),

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

    def write_package(data_array, kind, ads_name, adb_name):
        """Factorization function"""

        # Write the .ads
        with open(ads_name, 'wb') as ads:
            ads.write(LSP_Messages_Generic_Header.format(kind=kind))

            for (_, request_name, params_name) in data_array:
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

            ads.write("\nprivate\n")

            for (_, request_name, params_name) in data_array:
                ads.write(LSP_Messages_Private_Snippet.format(
                          request_name=request_name,
                          kind=kind))

            ads.write(LSP_Messages_Generic_Footer.format(kind=kind))

        # Write the .adb
        with open(adb_name, 'wb') as adb:
            snippets = ""
            for (protocol_name, request_name, params_name) in data_array:
                if params_name:
                    snippets += LSP_Message_Generic_Decode_Snippet.format(
                        protocol_name=protocol_name,
                        request_name=request_name,
                        params_name=params_name,
                        kind=kind)
                else:
                    snippets += \
                        LSP_Message_Generic_Decode_Snippet_Noparams.format(
                            protocol_name=protocol_name,
                            request_name=request_name,
                            kind=kind)

            adb.write(LSP_Messages_Generic_Body_Header.format(
                            snippets=snippets,
                            kind=kind))

            for (_, request_name, params_name) in data_array:
                if params_name:
                    adb.write(LSP_Messages_Generic_Write_Snippet.format(
                            request_name=request_name,
                            params_name=params_name,
                            kind=kind))
                else:
                    adb.write(
                        LSP_Messages_Generic_Write_Snippet_Noparams.format(
                            request_name=request_name,
                            kind=kind))

            adb.write(LSP_Messages_Generic_Footer.format(kind=kind))

    gen_dir = join(basedir, 'source', 'protocol', 'generated')
    write_package(REQUESTS, 'Request',
                  join(gen_dir, 'lsp-messages-requests.ads'),
                  join(gen_dir, 'lsp-messages-requests.adb'))
    write_package(NOTIFICATIONS, 'Notification',
                  join(gen_dir, 'lsp-messages-notifications.ads'),
                  join(gen_dir, 'lsp-messages-notifications.adb'))

if __name__ == '__main__':
    write_message_types()
