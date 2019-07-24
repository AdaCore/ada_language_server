--  Automatically generated, do not edit.

with LSP.Generic_Notifications;

package LSP.Messages.Server_Notifications is

   type Initialized_Notification is new NotificationMessage with null record;

   type Exit_Notification is new NotificationMessage with null record;

   package DidChangeConfiguration_Notifications is
     new LSP.Generic_Notifications (DidChangeConfigurationParams);

   type DidChangeConfiguration_Notification is
     new DidChangeConfiguration_Notifications.Notification with null record;

   package DidOpenTextDocument_Notifications is
     new LSP.Generic_Notifications (DidOpenTextDocumentParams);

   type DidOpenTextDocument_Notification is
     new DidOpenTextDocument_Notifications.Notification with null record;

   package DidChangeTextDocument_Notifications is
     new LSP.Generic_Notifications (DidChangeTextDocumentParams);

   type DidChangeTextDocument_Notification is
     new DidChangeTextDocument_Notifications.Notification with null record;

   package DidSaveTextDocument_Notifications is
     new LSP.Generic_Notifications (DidSaveTextDocumentParams);

   type DidSaveTextDocument_Notification is
     new DidSaveTextDocument_Notifications.Notification with null record;

   package DidCloseTextDocument_Notifications is
     new LSP.Generic_Notifications (DidCloseTextDocumentParams);

   type DidCloseTextDocument_Notification is
     new DidCloseTextDocument_Notifications.Notification with null record;

end LSP.Messages.Server_Notifications;
