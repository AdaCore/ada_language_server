--
--  Copyright (C) <YEAR>, <COPYRIGHT HOLDER>
--
--  SPDX-License-Identifier: MIT
--

package body LSP.Server_Notifications.DidDeleteFiles is

   overriding procedure Visit_Server_Receiver
     (Self  : Notification;
      Value : in out LSP.Server_Notification_Receivers
        .Server_Notification_Receiver'
        Class) is
   begin
      Value.On_DidDeleteFiles_Notification (Self.Params);
   end Visit_Server_Receiver;

end LSP.Server_Notifications.DidDeleteFiles;
