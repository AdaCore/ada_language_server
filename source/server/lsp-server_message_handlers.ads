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
with LSP.Server_Messages;

package LSP.Server_Message_Handlers is
   pragma Preelaborate;

   type Server_Message_Handler is limited interface;
   --  Server_Message_Handler construct jobs to process given message.

   function Create_Job
     (Self    : Server_Message_Handler;
      Message : LSP.Server_Messages.Server_Message_Access)
        return LSP.Server_Jobs.Server_Job_Access is abstract;
   --  Create a job to process a server message

   type Server_Message_Handler_Access is
     access all Server_Message_Handler'Class
       with Storage_Size => 0;

end LSP.Server_Message_Handlers;
