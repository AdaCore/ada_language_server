--
--  Copyright (C) <YEAR>, <COPYRIGHT HOLDER>
--
--  SPDX-License-Identifier: MIT
--

package body LSP.Client_Notifications.PublishDiagnostics is

   overriding procedure Visit_Client_Receiver
     (Self  : Notification;
      Value : in out LSP.Client_Notification_Receivers
        .Client_Notification_Receiver'
        Class) is
   begin
      Value.On_PublishDiagnostics_Notification (Self.Params);
   end Visit_Client_Receiver;

end LSP.Client_Notifications.PublishDiagnostics;
