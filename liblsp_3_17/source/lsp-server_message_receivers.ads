--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with LSP.Server_Notification_Receivers;
with LSP.Server_Request_Receivers;
with LSP.Server_Response_Receivers;

package LSP.Server_Message_Receivers is
   pragma Preelaborate;

   type Server_Message_Receiver is limited interface
     and LSP.Server_Notification_Receivers.Server_Notification_Receiver
     and LSP.Server_Request_Receivers.Server_Request_Receiver
     and LSP.Server_Response_Receivers.Server_Response_Receiver;

end LSP.Server_Message_Receivers;
