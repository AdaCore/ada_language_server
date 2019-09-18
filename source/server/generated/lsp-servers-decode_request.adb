--  Automatically generated, do not edit.

with Ada.Strings.UTF_Encoding;
with LSP.JSON_Streams;
with LSP.Types; use LSP.Types;
with LSP.Messages.Common_Writers; use LSP.Messages.Common_Writers;
with LSP.Messages.Server_Requests; use LSP.Messages.Server_Requests;

function LSP.Servers.Decode_Request
   (Document : GNATCOLL.JSON.JSON_Value)
    return LSP.Messages.Server_Requests.Server_Request'Class
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

   if To_UTF_8_String (Method) = "initialize" then
      declare
         R : Initialize_Request;
      begin
         Set_Common_Request_Fields (R, JS);
         JS.Key ("params");
         LSP.Messages.InitializeParams'Read (JS'Access, R.params);
         return R;
      end;
   end if;

      if To_UTF_8_String (Method) = "shutdown" then
         declare
            R : Shutdown_Request;
         begin
            Set_Common_Request_Fields (R, JS);
            return R;
         end;
      end if;

   if To_UTF_8_String (Method) = "textDocument/codeAction" then
      declare
         R : CodeAction_Request;
      begin
         Set_Common_Request_Fields (R, JS);
         JS.Key ("params");
         LSP.Messages.CodeActionParams'Read (JS'Access, R.params);
         return R;
      end;
   end if;

   if To_UTF_8_String (Method) = "textDocument/completion" then
      declare
         R : Completion_Request;
      begin
         Set_Common_Request_Fields (R, JS);
         JS.Key ("params");
         LSP.Messages.TextDocumentPositionParams'Read (JS'Access, R.params);
         return R;
      end;
   end if;

   if To_UTF_8_String (Method) = "textDocument/definition" then
      declare
         R : Definition_Request;
      begin
         Set_Common_Request_Fields (R, JS);
         JS.Key ("params");
         LSP.Messages.TextDocumentPositionParams'Read (JS'Access, R.params);
         return R;
      end;
   end if;

   if To_UTF_8_String (Method) = "textDocument/typeDefinition" then
      declare
         R : Type_Definition_Request;
      begin
         Set_Common_Request_Fields (R, JS);
         JS.Key ("params");
         LSP.Messages.TextDocumentPositionParams'Read (JS'Access, R.params);
         return R;
      end;
   end if;

   if To_UTF_8_String (Method) = "textDocument/highight" then
      declare
         R : Highlight_Request;
      begin
         Set_Common_Request_Fields (R, JS);
         JS.Key ("params");
         LSP.Messages.TextDocumentPositionParams'Read (JS'Access, R.params);
         return R;
      end;
   end if;

   if To_UTF_8_String (Method) = "textDocument/hover" then
      declare
         R : Hover_Request;
      begin
         Set_Common_Request_Fields (R, JS);
         JS.Key ("params");
         LSP.Messages.TextDocumentPositionParams'Read (JS'Access, R.params);
         return R;
      end;
   end if;

   if To_UTF_8_String (Method) = "textDocument/references" then
      declare
         R : References_Request;
      begin
         Set_Common_Request_Fields (R, JS);
         JS.Key ("params");
         LSP.Messages.ReferenceParams'Read (JS'Access, R.params);
         return R;
      end;
   end if;

   if To_UTF_8_String (Method) = "textDocument/signatureHelp" then
      declare
         R : Signature_Help_Request;
      begin
         Set_Common_Request_Fields (R, JS);
         JS.Key ("params");
         LSP.Messages.TextDocumentPositionParams'Read (JS'Access, R.params);
         return R;
      end;
   end if;

   if To_UTF_8_String (Method) = "textDocument/documentSymbol" then
      declare
         R : Document_Symbols_Request;
      begin
         Set_Common_Request_Fields (R, JS);
         JS.Key ("params");
         LSP.Messages.DocumentSymbolParams'Read (JS'Access, R.params);
         return R;
      end;
   end if;

   if To_UTF_8_String (Method) = "textDocument/rename" then
      declare
         R : Rename_Request;
      begin
         Set_Common_Request_Fields (R, JS);
         JS.Key ("params");
         LSP.Messages.RenameParams'Read (JS'Access, R.params);
         return R;
      end;
   end if;

   if To_UTF_8_String (Method) = "textDocument/executeCommand" then
      declare
         R : Execute_Command_Request;
      begin
         Set_Common_Request_Fields (R, JS);
         JS.Key ("params");
         LSP.Messages.ExecuteCommandParams'Read (JS'Access, R.params);
         return R;
      end;
   end if;

   if To_UTF_8_String (Method) = "workspace/symbol" then
      declare
         R : Workspace_Symbols_Request;
      begin
         Set_Common_Request_Fields (R, JS);
         JS.Key ("params");
         LSP.Messages.WorkspaceSymbolParams'Read (JS'Access, R.params);
         return R;
      end;
   end if;

   if To_UTF_8_String (Method) = "workspace/executeCommand" then
      declare
         R : Workspace_Execute_Command_Request;
      begin
         Set_Common_Request_Fields (R, JS);
         JS.Key ("params");
         LSP.Messages.ExecuteCommandParams'Read (JS'Access, R.params);
         return R;
      end;
   end if;

   if To_UTF_8_String (Method) = "textDocument/alsCalledBy" then
      declare
         R : ALS_Called_By_Request;
      begin
         Set_Common_Request_Fields (R, JS);
         JS.Key ("params");
         LSP.Messages.TextDocumentPositionParams'Read (JS'Access, R.params);
         return R;
      end;
   end if;

   if To_UTF_8_String (Method) = "$/alsDebug" then
      declare
         R : ALS_Debug_Request;
      begin
         Set_Common_Request_Fields (R, JS);
         JS.Key ("params");
         LSP.Messages.ALSDebugParams'Read (JS'Access, R.params);
         return R;
      end;
   end if;

   raise Program_Error; --  Request not found
end LSP.Servers.Decode_Request;
