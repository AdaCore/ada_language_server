--
--  Copyright (C) 2022-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

package body LSP.Client_Message_Writers is

   ----------------------------
   -- On_Client_Notification --
   ----------------------------

   overriding procedure On_Client_Notification
     (Self    : in out Client_Message_Writer;
      Message : LSP.Client_Notifications.Client_Notification'Class) is
   begin
      Message.Visit_Client_Receiver (Self.Notification_Writer);
   end On_Client_Notification;

   -----------------------
   -- On_Client_Request --
   -----------------------

   overriding procedure On_Client_Request
     (Self    : in out Client_Message_Writer;
      Message : LSP.Client_Requests.Client_Request'Class) is
   begin
      Message.Visit_Client_Receiver (Self.Request_Writer);
   end On_Client_Request;

   ------------------------
   -- On_Client_Response --
   ------------------------

   overriding procedure On_Client_Response
     (Self    : in out Client_Message_Writer;
      Message : LSP.Client_Responses.Client_Response'Class) is
   begin
      Message.Visit_Client_Receiver (Self.Response_Writer);
   end On_Client_Response;

end LSP.Client_Message_Writers;
