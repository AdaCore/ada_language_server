--
--  Copyright (C) <YEAR>, <COPYRIGHT HOLDER>
--
--  SPDX-License-Identifier: MIT
--

with LSP.Structures;

package LSP.Base_Notification_Receivers is
   pragma Preelaborate;

   type Base_Notification_Receiver is limited interface;

   procedure On_CancelRequest_Notification
     (Self  : in out Base_Notification_Receiver;
      Value : LSP.Structures.CancelParams) is null;

   procedure On_Progress_Notification
     (Self  : in out Base_Notification_Receiver;
      Value : LSP.Structures.ProgressParams) is null;

end LSP.Base_Notification_Receivers;
