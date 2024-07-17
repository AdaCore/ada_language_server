--
--  Copyright (C) 2022-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with LSP.Server_Message_Visitors;
with LSP.Server_Messages;
with LSP.Server_Notification_Receivers;

package LSP.Server_Notifications is
   pragma Preelaborate;

   type Server_Notification is abstract limited
     new LSP.Server_Messages.Server_Message with null record;

   procedure Visit_Server_Receiver
     (Self  : Server_Notification;
      Value : in out LSP.Server_Notification_Receivers
        .Server_Notification_Receiver'Class)
   is abstract;

   overriding procedure Visit_Server_Message_Visitor
     (Self    : Server_Notification;
      Visitor : in out LSP.Server_Message_Visitors
                   .Server_Message_Visitor'Class);

end LSP.Server_Notifications;
