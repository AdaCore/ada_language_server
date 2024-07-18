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

with Ada.Exceptions;

with VSS.Strings.Conversions;

with LSP.Constants;
with LSP.Enumerations;

package body LSP.Server_Request_Jobs is

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : in out Server_Request_Job;
      Client : in out LSP.Client_Message_Receivers.Client_Message_Receiver'
        Class;
      Status : out LSP.Server_Jobs.Execution_Status)
   is
   begin
      if Self.Request.Canceled then
         Client.On_Error_Response
           (Self.Request.Id,
            (code    => LSP.Constants.RequestCancelled,
             message => "Request was canceled"));

         Status := LSP.Server_Jobs.Done;
      else
         Server_Request_Job'Class (Self).Execute_Request (Client, Status);
      end if;

   exception
      when E : others =>
         declare
            Message : constant VSS.Strings.Virtual_String :=
              VSS.Strings.Conversions.To_Virtual_String
                ("Exception: " & Ada.Exceptions.Exception_Information (E));

         begin
            Client.On_Error_Response
              (Self.Request.Id,
               (code    => LSP.Enumerations.InternalError,
                message => Message));

            Status := LSP.Server_Jobs.Done;
         end;
   end Execute;

end LSP.Server_Request_Jobs;
