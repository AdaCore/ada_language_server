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
--  Project environment class which attempts to use "codepeer-gnatls" when
--  "<target->gnatls" is not available.

private with GNAT.Expect;
private with GNAT.OS_Lib;

with GNATCOLL.Projects;

package LSP.Ada_Project_Environments is

   type LSP_Project_Environment is
     new GNATCOLL.Projects.Project_Environment with private;

private

   type LSP_Project_Environment is
     new GNATCOLL.Projects.Project_Environment with null record;

   overriding procedure Spawn_Gnatls
     (Self         : LSP_Project_Environment;
      Fd           : out GNAT.Expect.Process_Descriptor_Access;
      Gnatls_Args  : GNAT.OS_Lib.Argument_List_Access;
      Errors       : GNATCOLL.Projects.Error_Report);

end LSP.Ada_Project_Environments;
