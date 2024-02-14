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

with LSP.Server_Jobs;
with LSP.Server_Message_Handlers;
with LSP.Server_Message_Visitors;
with LSP.Server_Messages;

package LSP.Sequential_Message_Handlers is
   pragma Preelaborate;

   type Sequential_Message_Handler is
     new LSP.Server_Message_Handlers.Server_Message_Handler
       with private;
   --  This message handler processes messages in the order they are received.
   --  TO do this for each message it creates a job with Fence priority.

   procedure Initialize
     (Self    : in out Sequential_Message_Handler'Class;
      Handler : not null access
        LSP.Server_Message_Visitors.Server_Message_Visitor'Class);

private

   type Server_Message_Visitor_Access is access all
     LSP.Server_Message_Visitors.Server_Message_Visitor'Class
       with Storage_Size => 0;

   type Sequential_Message_Handler is
     new LSP.Server_Message_Handlers.Server_Message_Handler
   with record
      Handler : Server_Message_Visitor_Access;
   end record;

   overriding function Create_Job
     (Self    : Sequential_Message_Handler;
      Message : LSP.Server_Messages.Server_Message_Access)
        return LSP.Server_Jobs.Server_Job_Access;

end LSP.Sequential_Message_Handlers;
