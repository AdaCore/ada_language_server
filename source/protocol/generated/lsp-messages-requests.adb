--  Automatically generated, do not edit.

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

      if To_UTF_8_String (Method) = "initialize" then
         declare
            R : Initialize_Request;
         begin
            R.jsonrpc := Version;
            R.method := Method;
            R.id := Request_Id;
            JS.Key ("params");
            InitializeParams'Read (JS'Access, R.params);
            return R;
         end;
      end if;

      if To_UTF_8_String (Method) = "shutdown" then
         declare
            R : Shutdown_Request;
         begin
            R.jsonrpc := Version;
            R.method := Method;
            R.id := Request_Id;
            return R;
         end;
      end if;

      if To_UTF_8_String (Method) = "textDocument/codeAction" then
         declare
            R : CodeAction_Request;
         begin
            R.jsonrpc := Version;
            R.method := Method;
            R.id := Request_Id;
            JS.Key ("params");
            CodeActionParams'Read (JS'Access, R.params);
            return R;
         end;
      end if;

      if To_UTF_8_String (Method) = "textDocument/completion" then
         declare
            R : Completion_Request;
         begin
            R.jsonrpc := Version;
            R.method := Method;
            R.id := Request_Id;
            JS.Key ("params");
            TextDocumentPositionParams'Read (JS'Access, R.params);
            return R;
         end;
      end if;

      if To_UTF_8_String (Method) = "textDocument/definition" then
         declare
            R : Definition_Request;
         begin
            R.jsonrpc := Version;
            R.method := Method;
            R.id := Request_Id;
            JS.Key ("params");
            TextDocumentPositionParams'Read (JS'Access, R.params);
            return R;
         end;
      end if;

      if To_UTF_8_String (Method) = "textDocument/highight" then
         declare
            R : Highlight_Request;
         begin
            R.jsonrpc := Version;
            R.method := Method;
            R.id := Request_Id;
            JS.Key ("params");
            TextDocumentPositionParams'Read (JS'Access, R.params);
            return R;
         end;
      end if;

      if To_UTF_8_String (Method) = "textDocument/hover" then
         declare
            R : Hover_Request;
         begin
            R.jsonrpc := Version;
            R.method := Method;
            R.id := Request_Id;
            JS.Key ("params");
            TextDocumentPositionParams'Read (JS'Access, R.params);
            return R;
         end;
      end if;

      if To_UTF_8_String (Method) = "textDocument/references" then
         declare
            R : References_Request;
         begin
            R.jsonrpc := Version;
            R.method := Method;
            R.id := Request_Id;
            JS.Key ("params");
            ReferenceParams'Read (JS'Access, R.params);
            return R;
         end;
      end if;

      if To_UTF_8_String (Method) = "textDocument/signatureHelp" then
         declare
            R : Signature_Help_Request;
         begin
            R.jsonrpc := Version;
            R.method := Method;
            R.id := Request_Id;
            JS.Key ("params");
            TextDocumentPositionParams'Read (JS'Access, R.params);
            return R;
         end;
      end if;

      if To_UTF_8_String (Method) = "textDocument/documentSymbol" then
         declare
            R : Document_Symbols_Request;
         begin
            R.jsonrpc := Version;
            R.method := Method;
            R.id := Request_Id;
            JS.Key ("params");
            DocumentSymbolParams'Read (JS'Access, R.params);
            return R;
         end;
      end if;

      if To_UTF_8_String (Method) = "textDocument/executeCommand" then
         declare
            R : Execute_Command_Request;
         begin
            R.jsonrpc := Version;
            R.method := Method;
            R.id := Request_Id;
            JS.Key ("params");
            ExecuteCommandParams'Read (JS'Access, R.params);
            return R;
         end;
      end if;

      if To_UTF_8_String (Method) = "workspace/symbol" then
         declare
            R : Workspace_Symbols_Request;
         begin
            R.jsonrpc := Version;
            R.method := Method;
            R.id := Request_Id;
            JS.Key ("params");
            WorkspaceSymbolParams'Read (JS'Access, R.params);
            return R;
         end;
      end if;

      if To_UTF_8_String (Method) = "workspace/executeCommand" then
         declare
            R : Workspace_Execute_Command_Request;
         begin
            R.jsonrpc := Version;
            R.method := Method;
            R.id := Request_Id;
            JS.Key ("params");
            ExecuteCommandParams'Read (JS'Access, R.params);
            return R;
         end;
      end if;

      raise Program_Error; --  Request not found
   end Decode_Request;

   procedure Write
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Initialize_Request)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Write_Request_Prefix (S, V);
      JS.Key ("params");
      InitializeParams'Write (S, V.params);
      JS.End_Object;
   end Write;

   procedure Write
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Shutdown_Request)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Write_Request_Prefix (S, V);
      JS.End_Object;
   end Write;

   procedure Write
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CodeAction_Request)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Write_Request_Prefix (S, V);
      JS.Key ("params");
      CodeActionParams'Write (S, V.params);
      JS.End_Object;
   end Write;

   procedure Write
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Completion_Request)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Write_Request_Prefix (S, V);
      JS.Key ("params");
      TextDocumentPositionParams'Write (S, V.params);
      JS.End_Object;
   end Write;

   procedure Write
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Definition_Request)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Write_Request_Prefix (S, V);
      JS.Key ("params");
      TextDocumentPositionParams'Write (S, V.params);
      JS.End_Object;
   end Write;

   procedure Write
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Highlight_Request)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Write_Request_Prefix (S, V);
      JS.Key ("params");
      TextDocumentPositionParams'Write (S, V.params);
      JS.End_Object;
   end Write;

   procedure Write
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Hover_Request)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Write_Request_Prefix (S, V);
      JS.Key ("params");
      TextDocumentPositionParams'Write (S, V.params);
      JS.End_Object;
   end Write;

   procedure Write
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : References_Request)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Write_Request_Prefix (S, V);
      JS.Key ("params");
      ReferenceParams'Write (S, V.params);
      JS.End_Object;
   end Write;

   procedure Write
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Signature_Help_Request)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Write_Request_Prefix (S, V);
      JS.Key ("params");
      TextDocumentPositionParams'Write (S, V.params);
      JS.End_Object;
   end Write;

   procedure Write
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Document_Symbols_Request)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Write_Request_Prefix (S, V);
      JS.Key ("params");
      DocumentSymbolParams'Write (S, V.params);
      JS.End_Object;
   end Write;

   procedure Write
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Execute_Command_Request)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Write_Request_Prefix (S, V);
      JS.Key ("params");
      ExecuteCommandParams'Write (S, V.params);
      JS.End_Object;
   end Write;

   procedure Write
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Workspace_Symbols_Request)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Write_Request_Prefix (S, V);
      JS.Key ("params");
      WorkspaceSymbolParams'Write (S, V.params);
      JS.End_Object;
   end Write;

   procedure Write
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Workspace_Execute_Command_Request)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Write_Request_Prefix (S, V);
      JS.Key ("params");
      ExecuteCommandParams'Write (S, V.params);
      JS.End_Object;
   end Write;

end LSP.Messages.Requests;
