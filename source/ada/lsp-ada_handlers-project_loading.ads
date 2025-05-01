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
   --  If a project is already loaded, do nothing.
   --
   --  If a project is not already loaded, search and load a project from the
   --  current workspace.
   --
   --  The search for a project file will be done in this order:
   --
   --  1. if ada.projectFile is specified, use it.
   --  2. else if the project is an Alire crate
   --     1. if 'alr' is available on PATH, use Alire to determine the project
   --     2. else continue with the rest of the search
   --  3. else if there is a unique project at the root of the workspace, use it
   --  4. else if there are multiple projects at the root, warn about it and
   --     load the implicit project
   --  5. else load the implicit project
   --
   --  Initially this function guaranteed that the Message_Handler would have
   --  the project loaded on return, but this is no longer the case.

   procedure Reload_Project (Self : in out Message_Handler'CLass);
   --  Clear the current project context and call Ensure_Project_Loaded to
   --  reload a project.

   procedure Reload_Implicit_Project
     (Self : in out Message_Handler'Class);
   --  Reload the implicit project. This is called when the server receives a
   --  onDidOpen notification and the directory containing the opened file is
   --  not part of the implicit project. In that case the notification handler
   --  adds the containing directory to Self.Project_Dirs_Loaded and calls this
   --  subprogram to reload the implicit project.

end LSP.Ada_Handlers.Project_Loading;
