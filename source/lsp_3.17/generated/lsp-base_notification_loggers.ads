--
--  Copyright (C) <YEAR>, <COPYRIGHT HOLDER>
--
--  SPDX-License-Identifier: MIT
--
--  DON'T EDIT THIS FILE! It was generated from metaModel.json.
--

with VSS.Text_Streams;
with LSP.Structures;
with LSP.Base_Notification_Receivers;

package LSP.Base_Notification_Loggers is
   pragma Preelaborate;

   type Base_Notification_Logger
     (Output : access VSS.Text_Streams.Output_Text_Stream'Class) is
   new LSP.Base_Notification_Receivers.Base_Notification_Receiver with
   null record;

   overriding procedure On_CancelRequest_Notification
     (Self  : in out Base_Notification_Logger;
      Value : LSP.Structures.CancelParams);

   overriding procedure On_Progress_Notification
     (Self  : in out Base_Notification_Logger;
      Value : LSP.Structures.ProgressParams);

end LSP.Base_Notification_Loggers;
