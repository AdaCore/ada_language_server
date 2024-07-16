--
--  Copyright (C) 2022-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

limited with LSP.Server_Notifications;
limited with LSP.Server_Requests;
limited with LSP.Server_Responses;

package LSP.Server_Message_Visitors is
   pragma Preelaborate;

   type Server_Message_Visitor is limited interface;
   --  Interface to visit messages send by a client to a server

   procedure On_Server_Notification
     (Self  : in out Server_Message_Visitor;
      Value : LSP.Server_Notifications.Server_Notification'Class) is null;

   procedure On_Server_Request
     (Self  : in out Server_Message_Visitor;
      Value : LSP.Server_Requests.Server_Request'Class) is null;

   procedure On_Server_Response
     (Self  : in out Server_Message_Visitor;
      Value : LSP.Server_Responses.Server_Response'Class) is null;

end LSP.Server_Message_Visitors;
