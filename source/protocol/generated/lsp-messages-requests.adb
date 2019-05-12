--  Automatically generated, do not edit.

with Ada.Strings.UTF_Encoding;
with LSP.JSON_Streams;
with LSP.Types; use LSP.Types;
with LSP.Messages.Common_Writers; use LSP.Messages.Common_Writers;

package body LSP.Messages.Requests is

   function "+" (Text : Ada.Strings.UTF_Encoding.UTF_8_String)
      return LSP.Types.LSP_String renames
       LSP.Types.To_LSP_String;

   function Decode_Request
      (Document : JSON_Value) return RequestMessage'Class
   is
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
            InitializeParams'Read (JS'Access, R.params);
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

      if To_UTF_8_String (Method) = "window/showMessageRequest" then
         declare
            R : ShowMessage_Request;
         begin
            Set_Common_Request_Fields (R, JS);
            JS.Key ("params");
            ShowMessageRequestParams'Read (JS'Access, R.params);
            return R;
         end;
      end if;

      if To_UTF_8_String (Method) = "textDocument/codeAction" then
         declare
            R : CodeAction_Request;
         begin
            Set_Common_Request_Fields (R, JS);
            JS.Key ("params");
            CodeActionParams'Read (JS'Access, R.params);
            return R;
         end;
      end if;

      if To_UTF_8_String (Method) = "textDocument/completion" then
         declare
            R : Completion_Request;
         begin
            Set_Common_Request_Fields (R, JS);
            JS.Key ("params");
            TextDocumentPositionParams'Read (JS'Access, R.params);
            return R;
         end;
      end if;

      if To_UTF_8_String (Method) = "textDocument/definition" then
         declare
            R : Definition_Request;
         begin
            Set_Common_Request_Fields (R, JS);
            JS.Key ("params");
            TextDocumentPositionParams'Read (JS'Access, R.params);
            return R;
         end;
      end if;

      if To_UTF_8_String (Method) = "textDocument/highight" then
         declare
            R : Highlight_Request;
         begin
            Set_Common_Request_Fields (R, JS);
            JS.Key ("params");
            TextDocumentPositionParams'Read (JS'Access, R.params);
            return R;
         end;
      end if;

      if To_UTF_8_String (Method) = "textDocument/hover" then
         declare
            R : Hover_Request;
         begin
            Set_Common_Request_Fields (R, JS);
            JS.Key ("params");
            TextDocumentPositionParams'Read (JS'Access, R.params);
            return R;
         end;
      end if;

      if To_UTF_8_String (Method) = "textDocument/references" then
         declare
            R : References_Request;
         begin
            Set_Common_Request_Fields (R, JS);
            JS.Key ("params");
            ReferenceParams'Read (JS'Access, R.params);
            return R;
         end;
      end if;

      if To_UTF_8_String (Method) = "textDocument/signatureHelp" then
         declare
            R : Signature_Help_Request;
         begin
            Set_Common_Request_Fields (R, JS);
            JS.Key ("params");
            TextDocumentPositionParams'Read (JS'Access, R.params);
            return R;
         end;
      end if;

      if To_UTF_8_String (Method) = "textDocument/documentSymbol" then
         declare
            R : Document_Symbols_Request;
         begin
            Set_Common_Request_Fields (R, JS);
            JS.Key ("params");
            DocumentSymbolParams'Read (JS'Access, R.params);
            return R;
         end;
      end if;

      if To_UTF_8_String (Method) = "textDocument/executeCommand" then
         declare
            R : Execute_Command_Request;
         begin
            Set_Common_Request_Fields (R, JS);
            JS.Key ("params");
            ExecuteCommandParams'Read (JS'Access, R.params);
            return R;
         end;
      end if;

      if To_UTF_8_String (Method) = "workspace/applyEdit" then
         declare
            R : ApplyWorkspaceEdit_Request;
         begin
            Set_Common_Request_Fields (R, JS);
            JS.Key ("params");
            ApplyWorkspaceEditParams'Read (JS'Access, R.params);
            return R;
         end;
      end if;

      if To_UTF_8_String (Method) = "workspace/symbol" then
         declare
            R : Workspace_Symbols_Request;
         begin
            Set_Common_Request_Fields (R, JS);
            JS.Key ("params");
            WorkspaceSymbolParams'Read (JS'Access, R.params);
            return R;
         end;
      end if;

      if To_UTF_8_String (Method) = "workspace/executeCommand" then
         declare
            R : Workspace_Execute_Command_Request;
         begin
            Set_Common_Request_Fields (R, JS);
            JS.Key ("params");
            ExecuteCommandParams'Read (JS'Access, R.params);
            return R;
         end;
      end if;

      raise Program_Error; --  Request not found
   end Decode_Request;

   function Handle_Request
     (Self : access Server_Request_Handler'Class;
      Request : LSP.Messages.RequestMessage'Class)
      return LSP.Messages.ResponseMessage'Class is
   begin

      if Request in Initialize_Request'Class then
         declare
            R : LSP.Messages.ResponseMessage'Class :=
               Self.On_Initialize_Request
                  (Initialize_Request (Request).params);
         begin
            R.jsonrpc := +"2.0";
            R.id := Request.id;
            return R;
         end;
      end if;

      if Request in Shutdown_Request'Class then
         declare
            R : LSP.Messages.ResponseMessage'Class :=
               Self.On_Shutdown_Request;
         begin
            R.jsonrpc := +"2.0";
            R.id := Request.id;
            return R;
         end;
      end if;

      if Request in ShowMessage_Request'Class then
         declare
            R : LSP.Messages.ResponseMessage'Class :=
               Self.On_ShowMessage_Request
                  (ShowMessage_Request (Request).params);
         begin
            R.jsonrpc := +"2.0";
            R.id := Request.id;
            return R;
         end;
      end if;

      if Request in CodeAction_Request'Class then
         declare
            R : LSP.Messages.ResponseMessage'Class :=
               Self.On_CodeAction_Request
                  (CodeAction_Request (Request).params);
         begin
            R.jsonrpc := +"2.0";
            R.id := Request.id;
            return R;
         end;
      end if;

      if Request in Completion_Request'Class then
         declare
            R : LSP.Messages.ResponseMessage'Class :=
               Self.On_Completion_Request
                  (Completion_Request (Request).params);
         begin
            R.jsonrpc := +"2.0";
            R.id := Request.id;
            return R;
         end;
      end if;

      if Request in Definition_Request'Class then
         declare
            R : LSP.Messages.ResponseMessage'Class :=
               Self.On_Definition_Request
                  (Definition_Request (Request).params);
         begin
            R.jsonrpc := +"2.0";
            R.id := Request.id;
            return R;
         end;
      end if;

      if Request in Highlight_Request'Class then
         declare
            R : LSP.Messages.ResponseMessage'Class :=
               Self.On_Highlight_Request
                  (Highlight_Request (Request).params);
         begin
            R.jsonrpc := +"2.0";
            R.id := Request.id;
            return R;
         end;
      end if;

      if Request in Hover_Request'Class then
         declare
            R : LSP.Messages.ResponseMessage'Class :=
               Self.On_Hover_Request
                  (Hover_Request (Request).params);
         begin
            R.jsonrpc := +"2.0";
            R.id := Request.id;
            return R;
         end;
      end if;

      if Request in References_Request'Class then
         declare
            R : LSP.Messages.ResponseMessage'Class :=
               Self.On_References_Request
                  (References_Request (Request).params);
         begin
            R.jsonrpc := +"2.0";
            R.id := Request.id;
            return R;
         end;
      end if;

      if Request in Signature_Help_Request'Class then
         declare
            R : LSP.Messages.ResponseMessage'Class :=
               Self.On_Signature_Help_Request
                  (Signature_Help_Request (Request).params);
         begin
            R.jsonrpc := +"2.0";
            R.id := Request.id;
            return R;
         end;
      end if;

      if Request in Document_Symbols_Request'Class then
         declare
            R : LSP.Messages.ResponseMessage'Class :=
               Self.On_Document_Symbols_Request
                  (Document_Symbols_Request (Request).params);
         begin
            R.jsonrpc := +"2.0";
            R.id := Request.id;
            return R;
         end;
      end if;

      if Request in Execute_Command_Request'Class then
         declare
            R : LSP.Messages.ResponseMessage'Class :=
               Self.On_Execute_Command_Request
                  (Execute_Command_Request (Request).params);
         begin
            R.jsonrpc := +"2.0";
            R.id := Request.id;
            return R;
         end;
      end if;

      if Request in ApplyWorkspaceEdit_Request'Class then
         declare
            R : LSP.Messages.ResponseMessage'Class :=
               Self.On_ApplyWorkspaceEdit_Request
                  (ApplyWorkspaceEdit_Request (Request).params);
         begin
            R.jsonrpc := +"2.0";
            R.id := Request.id;
            return R;
         end;
      end if;

      if Request in Workspace_Symbols_Request'Class then
         declare
            R : LSP.Messages.ResponseMessage'Class :=
               Self.On_Workspace_Symbols_Request
                  (Workspace_Symbols_Request (Request).params);
         begin
            R.jsonrpc := +"2.0";
            R.id := Request.id;
            return R;
         end;
      end if;

      if Request in Workspace_Execute_Command_Request'Class then
         declare
            R : LSP.Messages.ResponseMessage'Class :=
               Self.On_Workspace_Execute_Command_Request
                  (Workspace_Execute_Command_Request (Request).params);
         begin
            R.jsonrpc := +"2.0";
            R.id := Request.id;
            return R;
         end;
      end if;

      return LSP.Messages.ResponseMessage'
        (Is_Error => True,
         jsonrpc  => <>,
         id       => <>,
         error    =>
           (Is_Set => True,
            Value  =>
              (code    => LSP.Messages.MethodNotFound,
               message => +"The Request handler doesn't support this",
               others  => <>)));
   end Handle_Request;

   procedure Read
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Initialize_Request)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Set_Common_Request_Fields (V, JS);
      JS.Key ("params");
      InitializeParams'Read (S, V.params);
      JS.End_Object;
   end Read;

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

   procedure Read
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Shutdown_Request)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Set_Common_Request_Fields (V, JS);
      JS.End_Object;
   end Read;

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

   procedure Read
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ShowMessage_Request)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Set_Common_Request_Fields (V, JS);
      JS.Key ("params");
      ShowMessageRequestParams'Read (S, V.params);
      JS.End_Object;
   end Read;

   procedure Write
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ShowMessage_Request)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Write_Request_Prefix (S, V);
      JS.Key ("params");
      ShowMessageRequestParams'Write (S, V.params);
      JS.End_Object;
   end Write;

   procedure Read
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CodeAction_Request)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Set_Common_Request_Fields (V, JS);
      JS.Key ("params");
      CodeActionParams'Read (S, V.params);
      JS.End_Object;
   end Read;

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

   procedure Read
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Completion_Request)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Set_Common_Request_Fields (V, JS);
      JS.Key ("params");
      TextDocumentPositionParams'Read (S, V.params);
      JS.End_Object;
   end Read;

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

   procedure Read
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Definition_Request)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Set_Common_Request_Fields (V, JS);
      JS.Key ("params");
      TextDocumentPositionParams'Read (S, V.params);
      JS.End_Object;
   end Read;

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

   procedure Read
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Highlight_Request)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Set_Common_Request_Fields (V, JS);
      JS.Key ("params");
      TextDocumentPositionParams'Read (S, V.params);
      JS.End_Object;
   end Read;

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

   procedure Read
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Hover_Request)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Set_Common_Request_Fields (V, JS);
      JS.Key ("params");
      TextDocumentPositionParams'Read (S, V.params);
      JS.End_Object;
   end Read;

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

   procedure Read
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out References_Request)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Set_Common_Request_Fields (V, JS);
      JS.Key ("params");
      ReferenceParams'Read (S, V.params);
      JS.End_Object;
   end Read;

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

   procedure Read
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Signature_Help_Request)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Set_Common_Request_Fields (V, JS);
      JS.Key ("params");
      TextDocumentPositionParams'Read (S, V.params);
      JS.End_Object;
   end Read;

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

   procedure Read
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Document_Symbols_Request)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Set_Common_Request_Fields (V, JS);
      JS.Key ("params");
      DocumentSymbolParams'Read (S, V.params);
      JS.End_Object;
   end Read;

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

   procedure Read
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Execute_Command_Request)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Set_Common_Request_Fields (V, JS);
      JS.Key ("params");
      ExecuteCommandParams'Read (S, V.params);
      JS.End_Object;
   end Read;

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

   procedure Read
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ApplyWorkspaceEdit_Request)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Set_Common_Request_Fields (V, JS);
      JS.Key ("params");
      ApplyWorkspaceEditParams'Read (S, V.params);
      JS.End_Object;
   end Read;

   procedure Write
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ApplyWorkspaceEdit_Request)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Write_Request_Prefix (S, V);
      JS.Key ("params");
      ApplyWorkspaceEditParams'Write (S, V.params);
      JS.End_Object;
   end Write;

   procedure Read
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Workspace_Symbols_Request)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Set_Common_Request_Fields (V, JS);
      JS.Key ("params");
      WorkspaceSymbolParams'Read (S, V.params);
      JS.End_Object;
   end Read;

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

   procedure Read
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Workspace_Execute_Command_Request)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Set_Common_Request_Fields (V, JS);
      JS.Key ("params");
      ExecuteCommandParams'Read (S, V.params);
      JS.End_Object;
   end Read;

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
