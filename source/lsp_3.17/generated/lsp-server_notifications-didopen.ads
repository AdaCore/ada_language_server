--
--  Copyright (C) <YEAR>, <COPYRIGHT HOLDER>
--
--  SPDX-License-Identifier: MIT
--

with LSP.Structures;

package LSP.Server_Notifications.DidOpen is
   pragma Preelaborate;

   type Notification is
   new LSP.Server_Notifications.Server_Notification with record
      Params : LSP.Structures.DidOpenTextDocumentParams;
   end record;

   overriding procedure Visit_Server_Receiver
     (Self  : Notification;
      Value : in out LSP.Server_Notification_Receivers
        .Server_Notification_Receiver'
        Class);

end LSP.Server_Notifications.DidOpen;
