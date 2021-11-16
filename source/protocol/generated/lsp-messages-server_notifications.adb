--  Automatically generated, do not edit.

package body LSP.Messages.Server_Notifications is

   --  These messages are sent from client to server.

   Map : Maps.Map;

   function Method_To_Tag
     (Method : VSS.Strings.Virtual_String) return Ada.Tags.Tag is
   begin
      return Method_To_Tag (Map, Method);
   end Method_To_Tag;

   overriding function Decode
     (JS : not null access LSP.JSON_Streams.JSON_Stream)
      return Initialized_Notification is
   begin
      return V : Initialized_Notification do
         NotificationMessage'Read (JS, NotificationMessage (V));
      end return;
   end Decode;

   overriding procedure Visit
     (Self    : Initialized_Notification;
      Handler : access Server_Notification_Receiver'Class)
   is
      pragma Unreferenced (Self);
   begin
      Handler.On_Initialized_Notification;
   end Visit;

   overriding function Decode
     (JS : not null access LSP.JSON_Streams.JSON_Stream)
      return Exit_Notification is
   begin
      return V : Exit_Notification do
         NotificationMessage'Read (JS, NotificationMessage (V));
      end return;
   end Decode;

   overriding procedure Visit
     (Self    : Exit_Notification;
      Handler : access Server_Notification_Receiver'Class)
   is
      pragma Unreferenced (Self);
   begin
      Handler.On_Exit_Notification;
   end Visit;

   overriding procedure Visit
     (Self    : DidChangeConfiguration_Notification;
      Handler : access Server_Notification_Receiver'Class) is
   begin
      Handler.On_DidChangeConfiguration_Notification (Self.params);
   end Visit;

   overriding procedure Visit
     (Self    : DidChangeWorkspaceFolders_Notification;
      Handler : access Server_Notification_Receiver'Class) is
   begin
      Handler.On_DidChangeWorkspaceFolders_Notification (Self.params);
   end Visit;

   overriding procedure Visit
     (Self    : DidChangeWatchedFiles_Notification;
      Handler : access Server_Notification_Receiver'Class) is
   begin
      Handler.On_DidChangeWatchedFiles_Notification (Self.params);
   end Visit;

   overriding procedure Visit
     (Self    : DidCreateFiles_Notification;
      Handler : access Server_Notification_Receiver'Class) is
   begin
      Handler.On_DidCreateFiles_Notification (Self.params);
   end Visit;

   overriding procedure Visit
     (Self    : DidRenameFiles_Notification;
      Handler : access Server_Notification_Receiver'Class) is
   begin
      Handler.On_DidRenameFiles_Notification (Self.params);
   end Visit;

   overriding procedure Visit
     (Self    : DidDeleteFiles_Notification;
      Handler : access Server_Notification_Receiver'Class) is
   begin
      Handler.On_DidDeleteFiles_Notification (Self.params);
   end Visit;

   overriding procedure Visit
     (Self    : Cancel_Notification;
      Handler : access Server_Notification_Receiver'Class) is
   begin
      Handler.On_Cancel_Notification (Self.params);
   end Visit;

   overriding procedure Visit
     (Self    : DidOpenTextDocument_Notification;
      Handler : access Server_Notification_Receiver'Class) is
   begin
      Handler.On_DidOpenTextDocument_Notification (Self.params);
   end Visit;

   overriding procedure Visit
     (Self    : DidChangeTextDocument_Notification;
      Handler : access Server_Notification_Receiver'Class) is
   begin
      Handler.On_DidChangeTextDocument_Notification (Self.params);
   end Visit;

   overriding procedure Visit
     (Self    : DidSaveTextDocument_Notification;
      Handler : access Server_Notification_Receiver'Class) is
   begin
      Handler.On_DidSaveTextDocument_Notification (Self.params);
   end Visit;

   overriding procedure Visit
     (Self    : DidCloseTextDocument_Notification;
      Handler : access Server_Notification_Receiver'Class) is
   begin
      Handler.On_DidCloseTextDocument_Notification (Self.params);
   end Visit;

begin

   Map.Insert
     ("initialized",
      Initialized_Notification'Tag);

   Map.Insert
     ("exit",
      Exit_Notification'Tag);

   Map.Insert
     ("workspace/didChangeConfiguration",
      DidChangeConfiguration_Notification'Tag);

   Map.Insert
     ("workspace/didChangeWorkspaceFolders",
      DidChangeWorkspaceFolders_Notification'Tag);

   Map.Insert
     ("workspace/didChangeWatchedFiles",
      DidChangeWatchedFiles_Notification'Tag);

   Map.Insert
     ("workspace/didCreateFiles",
      DidCreateFiles_Notification'Tag);

   Map.Insert
     ("workspace/didRenameFiles",
      DidRenameFiles_Notification'Tag);

   Map.Insert
     ("workspace/didDeleteFiles",
      DidDeleteFiles_Notification'Tag);

   Map.Insert
     ("$/cancelRequest",
      Cancel_Notification'Tag);

   Map.Insert
     ("textDocument/didOpen",
      DidOpenTextDocument_Notification'Tag);

   Map.Insert
     ("textDocument/didChange",
      DidChangeTextDocument_Notification'Tag);

   Map.Insert
     ("textDocument/didSave",
      DidSaveTextDocument_Notification'Tag);

   Map.Insert
     ("textDocument/didClose",
      DidCloseTextDocument_Notification'Tag);
end LSP.Messages.Server_Notifications;
