--  Automatically generated, do not edit.

with LSP.JSON_Streams;
with LSP.Messages.Common_Writers; use LSP.Messages.Common_Writers;

package body LSP.Messages.Notifications is

   procedure Read
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Initialized_Notification)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Set_Common_Notification_Fields (V, JS);
      JS.End_Object;
   end Read;

   procedure Write
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Initialized_Notification)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Write_Notification_Prefix (S, V);
      JS.End_Object;
   end Write;

   procedure Read
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Exit_Notification)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Set_Common_Notification_Fields (V, JS);
      JS.End_Object;
   end Read;

   procedure Write
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Exit_Notification)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Write_Notification_Prefix (S, V);
      JS.End_Object;
   end Write;

   procedure Read
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DidChangeConfiguration_Notification)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Set_Common_Notification_Fields (V, JS);
      JS.Key ("params");
      DidChangeConfigurationParams'Read (S, V.params);
      JS.End_Object;
   end Read;

   procedure Write
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DidChangeConfiguration_Notification)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Write_Notification_Prefix (S, V);
      JS.Key ("params");
      DidChangeConfigurationParams'Write (S, V.params);
      JS.End_Object;
   end Write;

   procedure Read
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DidOpenTextDocument_Notification)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Set_Common_Notification_Fields (V, JS);
      JS.Key ("params");
      DidOpenTextDocumentParams'Read (S, V.params);
      JS.End_Object;
   end Read;

   procedure Write
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DidOpenTextDocument_Notification)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Write_Notification_Prefix (S, V);
      JS.Key ("params");
      DidOpenTextDocumentParams'Write (S, V.params);
      JS.End_Object;
   end Write;

   procedure Read
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DidChangeTextDocument_Notification)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Set_Common_Notification_Fields (V, JS);
      JS.Key ("params");
      DidChangeTextDocumentParams'Read (S, V.params);
      JS.End_Object;
   end Read;

   procedure Write
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DidChangeTextDocument_Notification)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Write_Notification_Prefix (S, V);
      JS.Key ("params");
      DidChangeTextDocumentParams'Write (S, V.params);
      JS.End_Object;
   end Write;

   procedure Read
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DidSaveTextDocument_Notification)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Set_Common_Notification_Fields (V, JS);
      JS.Key ("params");
      DidSaveTextDocumentParams'Read (S, V.params);
      JS.End_Object;
   end Read;

   procedure Write
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DidSaveTextDocument_Notification)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Write_Notification_Prefix (S, V);
      JS.Key ("params");
      DidSaveTextDocumentParams'Write (S, V.params);
      JS.End_Object;
   end Write;

   procedure Read
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DidCloseTextDocument_Notification)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Set_Common_Notification_Fields (V, JS);
      JS.Key ("params");
      DidCloseTextDocumentParams'Read (S, V.params);
      JS.End_Object;
   end Read;

   procedure Write
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DidCloseTextDocument_Notification)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Object;
      Write_Notification_Prefix (S, V);
      JS.Key ("params");
      DidCloseTextDocumentParams'Write (S, V.params);
      JS.End_Object;
   end Write;

end LSP.Messages.Notifications;
