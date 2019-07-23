--  Automatically generated, do not edit.

with LSP.Messages.Notifications; use LSP.Messages.Notifications;

procedure LSP.Servers.Handle_Notification
  (Self         : not null LSP.Server_Notification_Handlers
     .Server_Notification_Handler_Access;
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

end LSP.Servers.Handle_Notification;
