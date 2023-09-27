--
--  Copyright (C) 2022-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

--  Simple client-to-server message logger that uses 'Image to dump messages.

with VSS.Text_Streams;

with LSP.Server_Message_Visitors;
with LSP.Server_Notification_Loggers;
with LSP.Server_Notifications;
with LSP.Server_Request_Loggers;
with LSP.Server_Requests;
with LSP.Server_Response_Loggers;
with LSP.Server_Responses;

package LSP.Server_Message_Loggers is
   pragma Preelaborate;

   type Server_Message_Logger
     (Output : not null access VSS.Text_Streams.Output_Text_Stream'Class)
   is new LSP.Server_Message_Visitors.Server_Message_Visitor with record
      Request  : LSP.Server_Request_Loggers.Server_Request_Logger (Output);
      Response : LSP.Server_Response_Loggers.Server_Response_Logger (Output);

      Notification :
        LSP.Server_Notification_Loggers.Server_Notification_Logger (Output);
   end record;

   overriding procedure On_Server_Notification
     (Self  : in out Server_Message_Logger;
      Value : LSP.Server_Notifications.Server_Notification'Class);

   overriding procedure On_Server_Request
     (Self  : in out Server_Message_Logger;
      Value : LSP.Server_Requests.Server_Request'Class);

   overriding procedure On_Server_Response
     (Self  : in out Server_Message_Logger;
      Value : LSP.Server_Responses.Server_Response'Class);

end LSP.Server_Message_Loggers;
