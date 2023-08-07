--
--  Copyright (C) <YEAR>, <COPYRIGHT HOLDER>
--
--  SPDX-License-Identifier: MIT
--

with LSP.Base_Notification_Writers;
with LSP.Structures;
with LSP.Client_Notification_Receivers;

package LSP.Client_Notification_Writers is
   pragma Preelaborate;

   type Client_Notification_Writer is
   new LSP.Base_Notification_Writers.Base_Notification_Writer and
     LSP.Client_Notification_Receivers.Client_Notification_Receiver with
   null record;

   overriding procedure On_LogTrace_Notification
     (Self  : in out Client_Notification_Writer;
      Value : LSP.Structures.LogTraceParams);

   overriding procedure On_Event_Notification
     (Self : in out Client_Notification_Writer; Value : LSP.Structures.LSPAny);

   overriding procedure On_PublishDiagnostics_Notification
     (Self  : in out Client_Notification_Writer;
      Value : LSP.Structures.PublishDiagnosticsParams);

   overriding procedure On_LogMessage_Notification
     (Self  : in out Client_Notification_Writer;
      Value : LSP.Structures.LogMessageParams);

   overriding procedure On_ShowMessage_Notification
     (Self  : in out Client_Notification_Writer;
      Value : LSP.Structures.ShowMessageParams);

end LSP.Client_Notification_Writers;
