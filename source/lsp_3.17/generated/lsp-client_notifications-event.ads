--
--  Copyright (C) <YEAR>, <COPYRIGHT HOLDER>
--
--  SPDX-License-Identifier: MIT
--

with LSP.Structures;

package LSP.Client_Notifications.Event is
   pragma Preelaborate;

   type Notification is
   new LSP.Client_Notifications.Client_Notification with record
      Params : LSP.Structures.LSPAny;
   end record;

   overriding procedure Visit_Client_Receiver
     (Self  : Notification;
      Value : in out LSP.Client_Notification_Receivers
        .Client_Notification_Receiver'
        Class);

end LSP.Client_Notifications.Event;
