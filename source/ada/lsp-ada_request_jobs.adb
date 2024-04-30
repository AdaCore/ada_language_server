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

with Langkit_Support.Errors;
with LSP.Ada_Empty_Handlers;

package body LSP.Ada_Request_Jobs is

   ---------------------
   -- Execute_Request --
   ---------------------

   overriding procedure Execute_Request
     (Self   : in out Ada_Request_Job;
      Client :
        in out LSP.Client_Message_Receivers.Client_Message_Receiver'Class;
      Status : out LSP.Server_Jobs.Execution_Status) is
   begin
      Ada_Request_Job'Class (Self).Execute_Ada_Request (Client, Status);
   exception
      when Langkit_Support.Errors.Property_Error =>
         declare
            R : LSP.Ada_Empty_Handlers.Empty_Message_Handler
              (Client'Unchecked_Access);
         begin
            Self.Request.Visit_Server_Receiver (R);
         end;
   end Execute_Request;

end LSP.Ada_Request_Jobs;
