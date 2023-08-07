--
--  Copyright (C) <YEAR>, <COPYRIGHT HOLDER>
--
--  SPDX-License-Identifier: MIT
--

with VSS.JSON.Content_Handlers;
with LSP.Structures;
with LSP.Base_Notification_Receivers;

package LSP.Base_Notification_Writers is
   pragma Preelaborate;

   type Base_Notification_Writer
     (Output : access VSS.JSON.Content_Handlers.JSON_Content_Handler'Class) is
   new LSP.Base_Notification_Receivers.Base_Notification_Receiver with
   null record;

   overriding procedure On_CancelRequest_Notification
     (Self  : in out Base_Notification_Writer;
      Value : LSP.Structures.CancelParams);

   overriding procedure On_Progress_Notification
     (Self  : in out Base_Notification_Writer;
      Value : LSP.Structures.ProgressParams);

end LSP.Base_Notification_Writers;
