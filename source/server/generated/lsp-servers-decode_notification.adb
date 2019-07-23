--  Automatically generated, do not edit.

with Ada.Strings.UTF_Encoding;
with LSP.JSON_Streams;
with LSP.Types; use LSP.Types;
with LSP.Messages.Common_Writers; use LSP.Messages.Common_Writers;
with LSP.Messages.Notifications;

function LSP.Servers.Decode_Notification
   (Document : GNATCOLL.JSON.JSON_Value)
    return LSP.Messages.NotificationMessage'Class
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

      if To_UTF_8_String (Method) = "initialized" then
         declare
            R : LSP.Messages.Notifications.Initialized_Notification;
         begin
            Set_Common_Notification_Fields (R, JS);
            return R;
         end;
      end if;

      if To_UTF_8_String (Method) = "exit" then
         declare
            R : LSP.Messages.Notifications.Exit_Notification;
         begin
            Set_Common_Notification_Fields (R, JS);
            return R;
         end;
      end if;

   if To_UTF_8_String (Method) = "workspace/didChangeConfiguration" then
      declare
         R : LSP.Messages.Notifications.DidChangeConfiguration_Notification;
      begin
         Set_Common_Notification_Fields (R, JS);
         JS.Key ("params");
         LSP.Messages.DidChangeConfigurationParams'Read (JS'Access, R.params);
         return R;
      end;
   end if;

   if To_UTF_8_String (Method) = "textDocument/didOpen" then
      declare
         R : LSP.Messages.Notifications.DidOpenTextDocument_Notification;
      begin
         Set_Common_Notification_Fields (R, JS);
         JS.Key ("params");
         LSP.Messages.DidOpenTextDocumentParams'Read (JS'Access, R.params);
         return R;
      end;
   end if;

   if To_UTF_8_String (Method) = "textDocument/didChange" then
      declare
         R : LSP.Messages.Notifications.DidChangeTextDocument_Notification;
      begin
         Set_Common_Notification_Fields (R, JS);
         JS.Key ("params");
         LSP.Messages.DidChangeTextDocumentParams'Read (JS'Access, R.params);
         return R;
      end;
   end if;

   if To_UTF_8_String (Method) = "textDocument/didSave" then
      declare
         R : LSP.Messages.Notifications.DidSaveTextDocument_Notification;
      begin
         Set_Common_Notification_Fields (R, JS);
         JS.Key ("params");
         LSP.Messages.DidSaveTextDocumentParams'Read (JS'Access, R.params);
         return R;
      end;
   end if;

   if To_UTF_8_String (Method) = "textDocument/didClose" then
      declare
         R : LSP.Messages.Notifications.DidCloseTextDocument_Notification;
      begin
         Set_Common_Notification_Fields (R, JS);
         JS.Key ("params");
         LSP.Messages.DidCloseTextDocumentParams'Read (JS'Access, R.params);
         return R;
      end;
   end if;

   raise Program_Error; --  Notification not found
end LSP.Servers.Decode_Notification;
