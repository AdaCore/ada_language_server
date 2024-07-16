--
--  Copyright (C) <YEAR>, <COPYRIGHT HOLDER>
--
--  SPDX-License-Identifier: MIT
--
--  DON'T EDIT THIS FILE! It was generated from metaModel.json.
--

with LSP.Base_Notification_Receivers;
with LSP.Structures;

package LSP.Client_Notification_Receivers is
   pragma Preelaborate;

   type Client_Notification_Receiver is
     limited interface and
       LSP.Base_Notification_Receivers.Base_Notification_Receiver;

   procedure On_LogTrace_Notification
     (Self  : in out Client_Notification_Receiver;
      Value : LSP.Structures.LogTraceParams) is null;

   procedure On_Event_Notification
     (Self  : in out Client_Notification_Receiver;
      Value : LSP.Structures.LSPAny) is null;
   --  The telemetry event notification is sent from the server to the client
   --  to ask the client to log telemetry data.

   procedure On_PublishDiagnostics_Notification
     (Self  : in out Client_Notification_Receiver;
      Value : LSP.Structures.PublishDiagnosticsParams) is null;
   --  Diagnostics notification are sent from the server to the client to
   --  signal results of validation runs.

   procedure On_LogMessage_Notification
     (Self  : in out Client_Notification_Receiver;
      Value : LSP.Structures.LogMessageParams) is null;
   --  The log message notification is sent from the server to the client to
   --  ask the client to log a particular message.

   procedure On_ShowMessage_Notification
     (Self  : in out Client_Notification_Receiver;
      Value : LSP.Structures.ShowMessageParams) is null;
   --  The show message notification is sent from a server to a client to ask
   --  the client to display a particular message in the user interface.

end LSP.Client_Notification_Receivers;
