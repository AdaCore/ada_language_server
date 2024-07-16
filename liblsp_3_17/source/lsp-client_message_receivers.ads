--
--  Copyright (C) 2022-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with LSP.Client_Notification_Receivers;
with LSP.Client_Request_Receivers;
with LSP.Client_Response_Receivers;
with LSP.Progress_Report_Receivers;

package LSP.Client_Message_Receivers is
   pragma Preelaborate;

   type Client_Message_Receiver is limited interface and
     LSP.Client_Notification_Receivers.Client_Notification_Receiver and
     LSP.Client_Request_Receivers.Client_Request_Receiver and
     LSP.Client_Response_Receivers.Client_Response_Receiver and
     LSP.Progress_Report_Receivers.Progress_Report_Receiver;

end LSP.Client_Message_Receivers;
