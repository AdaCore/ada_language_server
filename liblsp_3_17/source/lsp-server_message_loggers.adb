--
--  Copyright (C) 2022-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

package body LSP.Server_Message_Loggers is

   ----------------------------
   -- On_Server_Notification --
   ----------------------------

   overriding procedure On_Server_Notification
     (Self  : in out Server_Message_Logger;
      Value : LSP.Server_Notifications.Server_Notification'Class) is
   begin
      Value.Visit_Server_Receiver (Self.Notification);
   end On_Server_Notification;

   -----------------------
   -- On_Server_Request --
   -----------------------

   overriding procedure On_Server_Request
     (Self  : in out Server_Message_Logger;
      Value : LSP.Server_Requests.Server_Request'Class) is
   begin
      Value.Visit_Server_Receiver (Self.Request);
   end On_Server_Request;

   ------------------------
   -- On_Server_Response --
   ------------------------

   overriding procedure On_Server_Response
     (Self  : in out Server_Message_Logger;
      Value : LSP.Server_Responses.Server_Response'Class) is
   begin
      Value.Visit_Server_Receiver (Self.Response);
   end On_Server_Response;

end LSP.Server_Message_Loggers;
