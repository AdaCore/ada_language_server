------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2018-2024, AdaCore                     --
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
--  This package provides handler and job type for didChange notifications.

with LSP.GPR_Job_Contexts;
with LSP.Server_Message_Handlers;

private with LSP.Server_Jobs;
private with LSP.Server_Messages;

package LSP.GPR_Did_Change_Document is

   type GPR_Did_Change_Handler
     (Context : not null access LSP.GPR_Job_Contexts.GPR_Job_Context'Class) is
       limited new LSP.Server_Message_Handlers.Server_Message_Handler
         with private;

private

   type GPR_Did_Change_Handler
     (Context : not null access LSP.GPR_Job_Contexts.GPR_Job_Context'Class) is
       limited new LSP.Server_Message_Handlers.Server_Message_Handler
         with null record;

   overriding function Create_Job
     (Self    : GPR_Did_Change_Handler;
      Message : LSP.Server_Messages.Server_Message_Access)
        return LSP.Server_Jobs.Server_Job_Access;

end LSP.GPR_Did_Change_Document;
