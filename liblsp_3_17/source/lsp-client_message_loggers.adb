--
--  Copyright (C) 2022-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

package body LSP.Client_Message_Loggers is

   ----------------------------
   -- On_Client_Notification --
   ----------------------------

   overriding procedure On_Client_Notification
     (Self  : in out Client_Logger;
      Value : LSP.Client_Notifications.Client_Notification'Class) is
   begin
      Value.Visit_Client_Receiver (Self.Notification);
   end On_Client_Notification;

   -----------------------
   -- On_Client_Request --
   -----------------------

   overriding procedure On_Client_Request
     (Self  : in out Client_Logger;
      Value : LSP.Client_Requests.Client_Request'Class) is
   begin
      Value.Visit_Client_Receiver (Self.Request);
   end On_Client_Request;

   ------------------------
   -- On_Client_Response --
   ------------------------

   overriding procedure On_Client_Response
     (Self  : in out Client_Logger;
      Value : LSP.Client_Responses.Client_Response'Class) is
   begin
      Value.Visit_Client_Receiver (Self.Response);
   end On_Client_Response;

end LSP.Client_Message_Loggers;
