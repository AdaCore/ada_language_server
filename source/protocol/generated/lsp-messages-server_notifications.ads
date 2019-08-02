--  Automatically generated, do not edit.

with LSP.Generic_Notifications;
with LSP.Server_Notification_Receivers;
use LSP.Server_Notification_Receivers;

package LSP.Messages.Server_Notifications is

   type Server_Notification is abstract new LSP.Messages.NotificationMessage
     with null record;

   procedure Visit
     (Self    : Server_Notification;
      Handler : access Server_Notification_Receiver'Class) is abstract;

   type Initialized_Notification is new Server_Notification with null record;

   overriding procedure Visit
     (Self    : Initialized_Notification;
      Handler : access Server_Notification_Receiver'Class);

   type Exit_Notification is new Server_Notification with null record;

   overriding procedure Visit
     (Self    : Exit_Notification;
      Handler : access Server_Notification_Receiver'Class);

   package DidChangeConfiguration_Notifications is
     new LSP.Generic_Notifications
       (Server_Notification,
        DidChangeConfigurationParams);

   type DidChangeConfiguration_Notification is
     new DidChangeConfiguration_Notifications.Notification with null record;

   overriding procedure Visit
     (Self    : DidChangeConfiguration_Notification;
      Handler : access Server_Notification_Receiver'Class);

   package DidOpenTextDocument_Notifications is
     new LSP.Generic_Notifications
       (Server_Notification,
        DidOpenTextDocumentParams);

   type DidOpenTextDocument_Notification is
     new DidOpenTextDocument_Notifications.Notification with null record;

   overriding procedure Visit
     (Self    : DidOpenTextDocument_Notification;
      Handler : access Server_Notification_Receiver'Class);

   package DidChangeTextDocument_Notifications is
     new LSP.Generic_Notifications
       (Server_Notification,
        DidChangeTextDocumentParams);

   type DidChangeTextDocument_Notification is
     new DidChangeTextDocument_Notifications.Notification with null record;

   overriding procedure Visit
     (Self    : DidChangeTextDocument_Notification;
      Handler : access Server_Notification_Receiver'Class);

   package DidSaveTextDocument_Notifications is
     new LSP.Generic_Notifications
       (Server_Notification,
        DidSaveTextDocumentParams);

   type DidSaveTextDocument_Notification is
     new DidSaveTextDocument_Notifications.Notification with null record;

   overriding procedure Visit
     (Self    : DidSaveTextDocument_Notification;
      Handler : access Server_Notification_Receiver'Class);

   package DidCloseTextDocument_Notifications is
     new LSP.Generic_Notifications
       (Server_Notification,
        DidCloseTextDocumentParams);

   type DidCloseTextDocument_Notification is
     new DidCloseTextDocument_Notifications.Notification with null record;

   overriding procedure Visit
     (Self    : DidCloseTextDocument_Notification;
      Handler : access Server_Notification_Receiver'Class);

end LSP.Messages.Server_Notifications;
