--  Automatically generated, do not edit.

with Ada.Tags;
with LSP.Generic_Notifications;
with LSP.JSON_Streams;
with LSP.Server_Notification_Receivers;
use LSP.Server_Notification_Receivers;

package LSP.Messages.Server_Notifications is

   type Server_Notification is abstract new LSP.Messages.NotificationMessage
     with null record;

   function Decode
     (JS : not null access LSP.JSON_Streams.JSON_Stream)
      return Server_Notification is abstract;

   procedure Visit
     (Self    : Server_Notification;
      Handler : access Server_Notification_Receiver'Class) is abstract;

   function Method_To_Tag
     (Method : VSS.Strings.Virtual_String) return Ada.Tags.Tag;
   --  For given LSP method return a corresponding message type tag

   type Initialized_Notification is new Server_Notification with null record;

   overriding function Decode
     (JS : not null access LSP.JSON_Streams.JSON_Stream)
      return Initialized_Notification;

   overriding procedure Visit
     (Self    : Initialized_Notification;
      Handler : access Server_Notification_Receiver'Class);

   type Exit_Notification is new Server_Notification with null record;

   overriding function Decode
     (JS : not null access LSP.JSON_Streams.JSON_Stream)
      return Exit_Notification;

   overriding procedure Visit
     (Self    : Exit_Notification;
      Handler : access Server_Notification_Receiver'Class);

   package DidChangeConfiguration_Notifications is
     new LSP.Generic_Notifications
       (Server_Notification,
        DidChangeConfigurationParams,
        Server_Notification_Receiver'Class);

   type DidChangeConfiguration_Notification is
     new DidChangeConfiguration_Notifications.Notification with null record;

   overriding procedure Visit
     (Self    : DidChangeConfiguration_Notification;
      Handler : access Server_Notification_Receiver'Class);

   package DidChangeWorkspaceFolders_Notifications is
     new LSP.Generic_Notifications
       (Server_Notification,
        DidChangeWorkspaceFoldersParams,
        Server_Notification_Receiver'Class);

   type DidChangeWorkspaceFolders_Notification is
     new DidChangeWorkspaceFolders_Notifications.Notification with null record;

   overriding procedure Visit
     (Self    : DidChangeWorkspaceFolders_Notification;
      Handler : access Server_Notification_Receiver'Class);

   package DidChangeWatchedFiles_Notifications is
     new LSP.Generic_Notifications
       (Server_Notification,
        DidChangeWatchedFilesParams,
        Server_Notification_Receiver'Class);

   type DidChangeWatchedFiles_Notification is
     new DidChangeWatchedFiles_Notifications.Notification with null record;

   overriding procedure Visit
     (Self    : DidChangeWatchedFiles_Notification;
      Handler : access Server_Notification_Receiver'Class);

   package DidCreateFiles_Notifications is
     new LSP.Generic_Notifications
       (Server_Notification,
        CreateFilesParams,
        Server_Notification_Receiver'Class);

   type DidCreateFiles_Notification is
     new DidCreateFiles_Notifications.Notification with null record;

   overriding procedure Visit
     (Self    : DidCreateFiles_Notification;
      Handler : access Server_Notification_Receiver'Class);

   package DidRenameFiles_Notifications is
     new LSP.Generic_Notifications
       (Server_Notification,
        RenameFilesParams,
        Server_Notification_Receiver'Class);

   type DidRenameFiles_Notification is
     new DidRenameFiles_Notifications.Notification with null record;

   overriding procedure Visit
     (Self    : DidRenameFiles_Notification;
      Handler : access Server_Notification_Receiver'Class);

   package DidDeleteFiles_Notifications is
     new LSP.Generic_Notifications
       (Server_Notification,
        DeleteFilesParams,
        Server_Notification_Receiver'Class);

   type DidDeleteFiles_Notification is
     new DidDeleteFiles_Notifications.Notification with null record;

   overriding procedure Visit
     (Self    : DidDeleteFiles_Notification;
      Handler : access Server_Notification_Receiver'Class);

   package Cancel_Notifications is
     new LSP.Generic_Notifications
       (Server_Notification,
        CancelParams,
        Server_Notification_Receiver'Class);

   type Cancel_Notification is
     new Cancel_Notifications.Notification with null record;

   overriding procedure Visit
     (Self    : Cancel_Notification;
      Handler : access Server_Notification_Receiver'Class);

   package DidOpenTextDocument_Notifications is
     new LSP.Generic_Notifications
       (Server_Notification,
        DidOpenTextDocumentParams,
        Server_Notification_Receiver'Class);

   type DidOpenTextDocument_Notification is
     new DidOpenTextDocument_Notifications.Notification with null record;

   overriding procedure Visit
     (Self    : DidOpenTextDocument_Notification;
      Handler : access Server_Notification_Receiver'Class);

   package DidChangeTextDocument_Notifications is
     new LSP.Generic_Notifications
       (Server_Notification,
        DidChangeTextDocumentParams,
        Server_Notification_Receiver'Class);

   type DidChangeTextDocument_Notification is
     new DidChangeTextDocument_Notifications.Notification with null record;

   overriding procedure Visit
     (Self    : DidChangeTextDocument_Notification;
      Handler : access Server_Notification_Receiver'Class);

   package DidSaveTextDocument_Notifications is
     new LSP.Generic_Notifications
       (Server_Notification,
        DidSaveTextDocumentParams,
        Server_Notification_Receiver'Class);

   type DidSaveTextDocument_Notification is
     new DidSaveTextDocument_Notifications.Notification with null record;

   overriding procedure Visit
     (Self    : DidSaveTextDocument_Notification;
      Handler : access Server_Notification_Receiver'Class);

   package DidCloseTextDocument_Notifications is
     new LSP.Generic_Notifications
       (Server_Notification,
        DidCloseTextDocumentParams,
        Server_Notification_Receiver'Class);

   type DidCloseTextDocument_Notification is
     new DidCloseTextDocument_Notifications.Notification with null record;

   overriding procedure Visit
     (Self    : DidCloseTextDocument_Notification;
      Handler : access Server_Notification_Receiver'Class);

end LSP.Messages.Server_Notifications;
