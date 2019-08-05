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

package body LSP.Messages.Visitors is

   -----------
   -- Visit --
   -----------

   procedure Visit
     (Self    : access Visitor'Class;
      Message : LSP.Messages.Message'Class)
   is
      use LSP.Messages.Server_Requests;
      use LSP.Messages.Server_Responses;
      use LSP.Messages.Server_Notifications;
      use LSP.Messages.Client_Requests;
      use LSP.Messages.Client_Responses;
      use LSP.Messages.Client_Notifications;
   begin
      if Message in Server_Notification'Class then
         Self.On_Server_Notification (Server_Notification (Message));
      elsif Message in Server_Request'Class then
         Self.On_Server_Request (Server_Request (Message));
      elsif Message in Server_Response'Class then
         Self.On_Server_Response (Server_Response (Message));
      elsif Message in Client_Notification'Class then
         Self.On_Client_Notification (Client_Notification (Message));
      elsif Message in Client_Request'Class then
         Self.On_Client_Request (Client_Request (Message));
      elsif Message in Client_Response'Class then
         Self.On_Client_Response (Client_Response (Message));
      end if;
   end Visit;

end LSP.Messages.Visitors;
