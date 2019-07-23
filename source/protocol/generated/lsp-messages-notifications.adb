--  Automatically generated, do not edit.

with Ada.Strings.UTF_Encoding;
with LSP.JSON_Streams;
with LSP.Types; use LSP.Types;
with LSP.Messages.Common_Writers; use LSP.Messages.Common_Writers;

package body LSP.Messages.Notifications is

   function "+" (Text : Ada.Strings.UTF_Encoding.UTF_8_String)
      return LSP.Types.LSP_String renames
       LSP.Types.To_LSP_String;

   function Decode_Notification
      (Document : JSON_Value) return NotificationMessage'Class
   is
      JS : aliased LSP.JSON_Streams.JSON_Stream;
      JSON_Array : GNATCOLL.JSON.JSON_Array;

      Method     : LSP.Types.LSP_String;

   begin
      GNATCOLL.JSON.Append (JSON_Array, Document);
      JS.Set_JSON_Document (JSON_Array);
      JS.Start_Object;

      LSP.Types.Read_String (JS, +"method", Method);

      if To_UTF_8_String (Method) = "initialized" then
         declare
            R : Initialized_Notification;
         begin
            Set_Common_Notification_Fields (R, JS);
            return R;
         end;
      end if;

      if To_UTF_8_String (Method) = "exit" then
         declare
            R : Exit_Notification;
         begin
            Set_Common_Notification_Fields (R, JS);
            return R;
         end;
      end if;

      if To_UTF_8_String (Method) = "workspace/didChangeConfiguration" then
         declare
            R : DidChangeConfiguration_Notification;
         begin
            Set_Common_Notification_Fields (R, JS);
            JS.Key ("params");
            DidChangeConfigurationParams'Read (JS'Access, R.params);
            return R;
         end;
      end if;

      if To_UTF_8_String (Method) = "textDocument/didOpen" then
         declare
            R : DidOpenTextDocument_Notification;
         begin
            Set_Common_Notification_Fields (R, JS);
            JS.Key ("params");
            DidOpenTextDocumentParams'Read (JS'Access, R.params);
            return R;
         end;
      end if;

      if To_UTF_8_String (Method) = "textDocument/didChange" then
         declare
            R : DidChangeTextDocument_Notification;
         begin
            Set_Common_Notification_Fields (R, JS);
            JS.Key ("params");
            DidChangeTextDocumentParams'Read (JS'Access, R.params);
            return R;
         end;
      end if;

      if To_UTF_8_String (Method) = "textDocument/didSave" then
         declare
            R : DidSaveTextDocument_Notification;
         begin
            Set_Common_Notification_Fields (R, JS);
            JS.Key ("params");
            DidSaveTextDocumentParams'Read (JS'Access, R.params);
            return R;
         end;
      end if;

      if To_UTF_8_String (Method) = "textDocument/didClose" then
         declare
            R : DidCloseTextDocument_Notification;
         begin
            Set_Common_Notification_Fields (R, JS);
            JS.Key ("params");
            DidCloseTextDocumentParams'Read (JS'Access, R.params);
            return R;
         end;
      end if;

      raise Program_Error; --  Notification not found
   end Decode_Notification;

   procedure Handle_Notification
     (Self : access Server_Notification_Handler'Class;
      Notification : LSP.Messages.NotificationMessage'Class) is
   begin

      if Notification in Initialized_Notification'Class then
         Self.On_Initialized_Notification;
         return;
      end if;

      if Notification in Exit_Notification'Class then
         Self.On_Exit_Notification;
         return;
      end if;

      if Notification in DidChangeConfiguration_Notification'Class then
         Self.On_DidChangeConfiguration_Notification
            ((DidChangeConfiguration_Notification (Notification).params));
         return;
      end if;

      if Notification in DidOpenTextDocument_Notification'Class then
         Self.On_DidOpenTextDocument_Notification
            ((DidOpenTextDocument_Notification (Notification).params));
         return;
      end if;

      if Notification in DidChangeTextDocument_Notification'Class then
         Self.On_DidChangeTextDocument_Notification
            ((DidChangeTextDocument_Notification (Notification).params));
         return;
      end if;

      if Notification in DidSaveTextDocument_Notification'Class then
         Self.On_DidSaveTextDocument_Notification
            ((DidSaveTextDocument_Notification (Notification).params));
         return;
      end if;

      if Notification in DidCloseTextDocument_Notification'Class then
         Self.On_DidCloseTextDocument_Notification
            ((DidCloseTextDocument_Notification (Notification).params));
         return;
      end if;

   end Handle_Notification;

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
