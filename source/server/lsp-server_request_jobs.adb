------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                        Copyright (C) 2024, AdaCore                       --
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

with LSP.Constants;

package body LSP.Server_Request_Jobs is

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : in out Server_Request_Job;
      Client : in out LSP.Client_Message_Receivers.Client_Message_Receiver'
        Class;
      Status :    out LSP.Server_Jobs.Execution_Status)
   is
   begin
      if Self.Request.Canceled then
         Client.On_Error_Response
           (Self.Request.Id,
            (code    => LSP.Constants.RequestCancelled,
             message => "Request was canceled"));

         Status := LSP.Server_Jobs.Done;
      else
         Execute_Request (Server_Request_Job'Class (Self), Client, Status);
      end if;
   end Execute;

end LSP.Server_Request_Jobs;
