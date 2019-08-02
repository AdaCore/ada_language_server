--  Automatically generated, do not edit.

package body LSP.Messages.Server_Notifications is

   --  These messages are sent from client to server.

   overriding procedure Visit
     (Self    : Initialized_Notification;
      Handler : access Server_Notification_Receiver'Class)
   is
      pragma Unreferenced (Self);
   begin
      Handler.On_Initialized_Notification;
   end Visit;

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

end LSP.Messages.Server_Notifications;
