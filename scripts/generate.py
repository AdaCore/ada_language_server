#!/usr/bin/env python

import os
import os.path
from os.path import join

""" Script to generate the 'generated' parts of the code """

LSP_Messages_Requests_Header = """--  Automatically generated, do not edit.

with Ada.Streams;
with GNATCOLL.JSON; use GNATCOLL.JSON;

package LSP.Messages.Requests is

   function Decode_Request (Document : JSON_Value) return RequestMessage'Class;
   --  Decode the request present in the input document. Document is a JSON
   --  representation of the protocol string.
"""

LSP_Messages_Requests_Body_Header = """--  Automatically generated, do not edit.

with Ada.Strings.UTF_Encoding;
with LSP.JSON_Streams;
with LSP.Types; use LSP.Types;

package body LSP.Messages.Requests is

   function "+" (Text : Ada.Strings.UTF_Encoding.UTF_8_String)
      return LSP.Types.LSP_String renames
       LSP.Types.To_LSP_String;

   procedure Write_Request_Prefix
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.RequestMessage'Class);

   procedure Write_Request_Prefix
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.RequestMessage'Class)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      Write_String (JS, +"jsonrpc", V.jsonrpc);

      if V.id.Is_Number then
         Write_Number (JS, +"id", V.id.Number);
      elsif not Is_Empty (V.id.String) then
         Write_String (JS, +"id", V.id.String);
      end if;

      Write_String (JS, +"method", V.method);
   end Write_Request_Prefix;

   function Decode_Request (Document : JSON_Value) return RequestMessage'Class
   is
      JS : aliased LSP.JSON_Streams.JSON_Stream;
      JSON_Array : GNATCOLL.JSON.JSON_Array;

      Version    : LSP.Types.LSP_String;
      Method     : LSP.Types.LSP_String;
      Request_Id : LSP.Types.LSP_Number_Or_String;

   begin
      GNATCOLL.JSON.Append (JSON_Array, Document);
      JS.Set_JSON_Document (JSON_Array);
      JS.Start_Object;

      LSP.Types.Read_String (JS, +"jsonrpc", Version);
      LSP.Types.Read_String (JS, +"method", Method);
      Read_Number_Or_String (JS, +"id", Request_Id);
{snippets}
      raise Program_Error; --  Request not found
   end Decode_Request;
"""

LSP_Message_Requests_Decode_Snippet = """
      if To_UTF_8_String (Method) = "{protocol_name}" then
         declare
            R : {request_name}_Request;
         begin
            R.jsonrpc := Version;
            R.method := Method;
            R.id := Request_Id;
            JS.Key ("params");
            {params_name}'Read (JS'Access, R.params);
            return R;
         end;
      end if;
"""

LSP_Message_Requests_Decode_Snippet_Noparams = """
      if To_UTF_8_String (Method) = "{protocol_name}" then
         declare
            R : {request_name}_Request;
         begin
            R.jsonrpc := Version;
            R.method := Method;
            R.id := Request_Id;
            return R;
         end;
      end if;
"""

LSP_Messages_Requests_Footer = """
end LSP.Messages.Requests;
"""

LSP_Messages_Request_Type_Snippet = """
   type {request_name}_Request is new RequestMessage with record
      params : {params_name};
   end record;
"""

LSP_Messages_Request_Type_Snippet_Noparams = """
   type {request_name}_Request is new RequestMessage with null record;
"""

LSP_Messages_Private_Snippet = """
   procedure Write
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : {request_name}_Request);
   for {request_name}_Request'Write use Write;"""

LSP_Messages_Request_Write_Snippet = """
   procedure Write
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : {request_name}_Request)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Write_Request_Prefix (S, V);
      JS.Key ("params");
      {params_name}'Write (S, V.params);
      JS.End_Object;
   end Write;
"""

LSP_Messages_Request_Write_Snippet_Noparams = """
   procedure Write
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : {request_name}_Request)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Write_Request_Prefix (S, V);
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


def write_request_types():
    """ Write source/protocol/lsp-messages-request.* """

    ads_name = join(basedir, 'source', 'protocol', 'generated',
                    'lsp-messages-requests.ads')
    adb_name = join(basedir, 'source', 'protocol', 'generated',
                    'lsp-messages-requests.adb')

    # Write the .ads
    with open(ads_name, 'wb') as ads:
        ads.write(LSP_Messages_Requests_Header)

        for (_, request_name, params_name) in REQUESTS:
            if params_name:
                ads.write(LSP_Messages_Request_Type_Snippet.format(
                          request_name=request_name,
                          params_name=params_name))
            else:
                ads.write(LSP_Messages_Request_Type_Snippet_Noparams.format(
                          request_name=request_name))

        ads.write("\nprivate\n")

        for (_, request_name, params_name) in REQUESTS:
            ads.write(LSP_Messages_Private_Snippet.format(
                      request_name=request_name))

        ads.write(LSP_Messages_Requests_Footer)

    # Write the .adb
    with open(adb_name, 'wb') as adb:
        snippets = ""
        for (protocol_name, request_name, params_name) in REQUESTS:
            if params_name:
                snippets += LSP_Message_Requests_Decode_Snippet.format(
                    protocol_name=protocol_name,
                    request_name=request_name,
                    params_name=params_name)
            else:
                snippets += \
                    LSP_Message_Requests_Decode_Snippet_Noparams.format(
                        protocol_name=protocol_name,
                        request_name=request_name)

        adb.write(LSP_Messages_Requests_Body_Header.format(snippets=snippets))

        for (_, request_name, params_name) in REQUESTS:
            if params_name:
                adb.write(LSP_Messages_Request_Write_Snippet.format(
                        request_name=request_name,
                        params_name=params_name))
            else:
                adb.write(LSP_Messages_Request_Write_Snippet_Noparams.format(
                        request_name=request_name))

        adb.write(LSP_Messages_Requests_Footer)

if __name__ == '__main__':
    write_request_types()
