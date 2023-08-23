--
--  Copyright (C) 2022-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with VSS.JSON.Content_Handlers;

with LSP.Client_Message_Visitors;
with LSP.Client_Notification_Writers;
with LSP.Client_Notifications;
with LSP.Client_Request_Writers;
with LSP.Client_Requests;
with LSP.Client_Response_Writers;
with LSP.Client_Responses;
with LSP.Progress_Reports;
with LSP.Progress_Report_Writers;

package LSP.Client_Message_Writers is
   pragma Preelaborate;

   type Client_Message_Writer
     (Output : access VSS.JSON.Content_Handlers.JSON_Content_Handler'Class) is
   limited new LSP.Client_Message_Visitors.Client_Message_Visitor
   with record
      Notification_Writer : LSP.Client_Notification_Writers
        .Client_Notification_Writer (Output);

      Request_Writer      : LSP.Client_Request_Writers
        .Client_Request_Writer (Output);

      Response_Writer     : LSP.Client_Response_Writers
        .Client_Response_Writer (Output);

      Progress_Writer     :
        LSP.Progress_Report_Writers.Progress_Report_Writer (Output);
   end record;

   overriding procedure On_Client_Notification
     (Self    : in out Client_Message_Writer;
      Message : LSP.Client_Notifications.Client_Notification'Class);

   overriding procedure On_Client_Request
     (Self    : in out Client_Message_Writer;
      Message : LSP.Client_Requests.Client_Request'Class);

   overriding procedure On_Client_Response
     (Self    : in out Client_Message_Writer;
      Message : LSP.Client_Responses.Client_Response'Class);

   overriding procedure On_Progress_Report
     (Self    : in out Client_Message_Writer;
      Message : LSP.Progress_Reports.Progress_Report'Class);

end LSP.Client_Message_Writers;
