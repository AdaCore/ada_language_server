--
--  Copyright (C) <YEAR>, <COPYRIGHT HOLDER>
--
--  SPDX-License-Identifier: MIT
--
--  DON'T EDIT THIS FILE! It was generated from metaModel.json.
--

with LSP.Structures;

package LSP.Client_Notifications.LogTrace is
   pragma Preelaborate;

   type Notification is
   new LSP.Client_Notifications.Client_Notification with record
      Params : LSP.Structures.LogTraceParams;
   end record;

   overriding procedure Visit_Client_Receiver
     (Self  : Notification;
      Value : in out LSP.Client_Notification_Receivers
        .Client_Notification_Receiver'
        Class);

end LSP.Client_Notifications.LogTrace;
