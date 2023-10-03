--
--  Copyright (C) 2022-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

--  Simple server-to-client message logger that uses 'Image to dump messages.

with VSS.Text_Streams;

with LSP.Client_Message_Visitors;
with LSP.Client_Notification_Loggers;
with LSP.Client_Notifications;
with LSP.Client_Request_Loggers;
with LSP.Client_Requests;
with LSP.Client_Response_Loggers;
with LSP.Client_Responses;

package LSP.Client_Message_Loggers is
   pragma Preelaborate;

   type Client_Logger
     (Output : not null access VSS.Text_Streams.Output_Text_Stream'Class)
   is new LSP.Client_Message_Visitors.Client_Message_Visitor with record
      Request  : LSP.Client_Request_Loggers.Client_Request_Logger (Output);
      Response : LSP.Client_Response_Loggers.Client_Response_Logger (Output);

      Notification :
        LSP.Client_Notification_Loggers.Client_Notification_Logger (Output);
   end record;

   overriding procedure On_Client_Notification
     (Self  : in out Client_Logger;
      Value : LSP.Client_Notifications.Client_Notification'Class);

   overriding procedure On_Client_Request
     (Self  : in out Client_Logger;
      Value : LSP.Client_Requests.Client_Request'Class);

   overriding procedure On_Client_Response
     (Self  : in out Client_Logger;
      Value : LSP.Client_Responses.Client_Response'Class);

end LSP.Client_Message_Loggers;
