--  Automatically generated, do not edit.

with LSP.JSON_Streams;
with LSP.Messages.Common_Writers; use LSP.Messages.Common_Writers;

package body LSP.Messages.Server_Requests is

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
      V : out Type_Definition_Request)
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
      V : Type_Definition_Request)
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
      V : out Rename_Request)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Set_Common_Request_Fields (V, JS);
      JS.Key ("params");
      RenameParams'Read (S, V.params);
      JS.End_Object;
   end Read;

   procedure Write
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Rename_Request)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Write_Request_Prefix (S, V);
      JS.Key ("params");
      RenameParams'Write (S, V.params);
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

   procedure Read
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ALS_Called_By_Request)
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
      V : ALS_Called_By_Request)
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

end LSP.Messages.Server_Requests;
