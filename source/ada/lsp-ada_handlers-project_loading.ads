------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2018-2023, AdaCore                     --
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

package LSP.Ada_Handlers.Project_Loading is

   procedure Ensure_Project_Loaded (Self : in out Message_Handler'Class);
   --  Search and load a project from the current workspace.
   --  The search will be done in this order:
   --  1- search for an alire crate
   --  2- if there is only one project then load it
   --  3- load the implicit project and warn if there were multiple projects
   --  Do nothing if a project has been loaded.

   procedure Reload_Project (Self : in out Message_Handler'CLass);
   --  Reload the project set in the configuration or Load the project if
   --  none is already yet.

   procedure Release_Contexts_And_Project_Info
     (Self : in out Message_Handler'Class);
   --  Release the memory associated to project information in Self

   procedure Reload_Implicit_Project_Dirs
     (Self : in out Message_Handler'Class);
   --  Reload as project source dirs the directories in
   --  Self.Project_Dirs_Loaded.

end LSP.Ada_Handlers.Project_Loading;
