------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2018-2019, AdaCore                     --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

with LSP.Messages.Server_Requests;
with LSP.Messages.Server_Responses;
with LSP.Messages.Server_Notifications;
with LSP.Messages.Client_Requests;
with LSP.Messages.Client_Responses;
with LSP.Messages.Client_Notifications;

package LSP.Messages.Visitors is

   type Visitor is limited interface;

   procedure On_Server_Notification
     (Self    : access Visitor;
      Message : LSP.Messages.Server_Notifications.Server_Notification'Class)
        is abstract;
   --  Notification from client to server

   procedure On_Server_Request
     (Self    : access Visitor;
      Message : LSP.Messages.Server_Requests.Server_Request'Class)
        is abstract;
   --  Request from client to server

   procedure On_Server_Response
     (Self    : access Visitor;
      Message : LSP.Messages.Server_Responses.Server_Response'Class)
        is abstract;
   --  Response from server to client

   procedure On_Client_Notification
     (Self    : access Visitor;
      Message : LSP.Messages.Client_Notifications.Client_Notification'Class)
        is abstract;
   --  Notification from server to client

   procedure On_Client_Request
     (Self    : access Visitor;
      Message : LSP.Messages.Client_Requests.Client_Request'Class)
        is abstract;
   --  Request from server to client

   procedure On_Client_Response
     (Self    : access Visitor;
      Message : LSP.Messages.Client_Responses.Client_Response'Class)
        is abstract;
   --  Response from client to server

   procedure Visit
     (Self    : access Visitor'Class;
      Message : LSP.Messages.Message'Class);
   --  Run On_XXX method of Self according to message kind

end LSP.Messages.Visitors;
