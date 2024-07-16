------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                        Copyright (C) 2020, AdaCore                       --
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
--
--  Interface to process request sent to the client.

with LSP.Client_Request_Receivers;
with LSP.Client_Notification_Receivers;

package LSP.Client_Message_Receivers is

   type Client_Message_Receiver is limited interface
     and LSP.Client_Request_Receivers.Client_Request_Receiver
     and LSP.Client_Notification_Receivers.Client_Notification_Receiver;
   --  An interface to send requests and notification to the client

end LSP.Client_Message_Receivers;
