--
--  Copyright (C) 2022-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

limited with LSP.Client_Notifications;
limited with LSP.Client_Requests;
limited with LSP.Client_Responses;
limited with LSP.Progress_Reports;

package LSP.Client_Message_Visitors is
   pragma Preelaborate;

   type Client_Message_Visitor is limited interface;
   --  Interface to visit messages send by a server to a client

   procedure On_Client_Notification
     (Self  : in out Client_Message_Visitor;
      Value : LSP.Client_Notifications.Client_Notification'Class) is null;

   procedure On_Client_Request
     (Self  : in out Client_Message_Visitor;
      Value : LSP.Client_Requests.Client_Request'Class) is null;

   procedure On_Client_Response
     (Self  : in out Client_Message_Visitor;
      Value : LSP.Client_Responses.Client_Response'Class) is null;

   procedure On_Progress_Report
     (Self  : in out Client_Message_Visitor;
      Value : LSP.Progress_Reports.Progress_Report'Class) is null;

end LSP.Client_Message_Visitors;
