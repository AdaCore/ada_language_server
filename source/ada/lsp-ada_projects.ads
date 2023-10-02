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

with GPR2.Project.Attribute_Index;

package LSP.Ada_Projects is

   pragma Elaborate_Body;

   package Pretty_Printer is

      use type GPR2.Package_Id;
      use type GPR2.Optional_Attribute_Id;

      Pretty_Printer_Id : constant GPR2.Package_Id := +"pretty_printer";
      Default_Switches  : constant GPR2.Q_Attribute_Id :=
        (Pretty_Printer_Id, +"default_switches");
      Switches          : constant GPR2.Q_Attribute_Id :=
        (Pretty_Printer_Id, +"switches");
      Ada_Index         : constant GPR2.Project.Attribute_Index.Object :=
        GPR2.Project.Attribute_Index.Create (GPR2.Ada_Language);

   end Pretty_Printer;

end LSP.Ada_Projects;
