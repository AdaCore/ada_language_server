--
--  Copyright (C) 2022-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with LSP.Client_Message_Visitors;
with LSP.Client_Messages;
with LSP.Client_Notification_Receivers;

package LSP.Client_Notifications is
   pragma Preelaborate;

   type Client_Notification is abstract limited
     new LSP.Client_Messages.Client_Message with null record;

   procedure Visit_Client_Receiver
     (Self  : Client_Notification;
      Value : in out LSP.Client_Notification_Receivers
        .Client_Notification_Receiver'Class)
   is abstract;

   overriding procedure Visit_Client_Message_Visitor
     (Self    : Client_Notification;
      Visitor : in out LSP.Client_Message_Visitors
                         .Client_Message_Visitor'Class);

end LSP.Client_Notifications;
