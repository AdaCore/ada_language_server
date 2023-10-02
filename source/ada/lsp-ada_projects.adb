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

with GPR2.Project.Registry.Attribute;
with GPR2.Project.Registry.Pack;

package body LSP.Ada_Projects is

begin
   GPR2.Project.Registry.Pack.Add
     (Name     => Pretty_Printer.Pretty_Printer_Id,
      Projects => GPR2.Project.Registry.Pack.Everywhere);

   GPR2.Project.Registry.Attribute.Add
     (Name                 => Pretty_Printer.Switches,
      Index_Type           => GPR2.Project.Registry.Attribute.
                                 FileGlob_Or_Language_Index,
      Index_Optional       => True,
      Value                => GPR2.Project.Registry.Attribute.List,
      Value_Case_Sensitive => True,
      Is_Allowed_In        => GPR2.Project.Registry.Attribute.Everywhere);

   GPR2.Project.Registry.Attribute.Add_Alias
     (Pretty_Printer.Default_Switches, Pretty_Printer.Switches);
end LSP.Ada_Projects;
