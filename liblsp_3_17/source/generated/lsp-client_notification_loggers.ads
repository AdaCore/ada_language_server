--
--  Copyright (C) <YEAR>, <COPYRIGHT HOLDER>
--
--  SPDX-License-Identifier: MIT
--
--  DON'T EDIT THIS FILE! It was generated from metaModel.json.
--

with LSP.Base_Notification_Loggers;
with LSP.Structures;
with LSP.Client_Notification_Receivers;

package LSP.Client_Notification_Loggers is
   pragma Preelaborate;

   type Client_Notification_Logger is
   new LSP.Base_Notification_Loggers.Base_Notification_Logger and
     LSP.Client_Notification_Receivers.Client_Notification_Receiver with
   null record;

   overriding procedure On_LogTrace_Notification
     (Self  : in out Client_Notification_Logger;
      Value : LSP.Structures.LogTraceParams);

   overriding procedure On_Event_Notification
     (Self : in out Client_Notification_Logger; Value : LSP.Structures.LSPAny);

   overriding procedure On_PublishDiagnostics_Notification
     (Self  : in out Client_Notification_Logger;
      Value : LSP.Structures.PublishDiagnosticsParams);

   overriding procedure On_LogMessage_Notification
     (Self  : in out Client_Notification_Logger;
      Value : LSP.Structures.LogMessageParams);

   overriding procedure On_ShowMessage_Notification
     (Self  : in out Client_Notification_Logger;
      Value : LSP.Structures.ShowMessageParams);

end LSP.Client_Notification_Loggers;
