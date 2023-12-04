------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2023, AdaCore                          --
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

with Ada.Characters.Conversions;

with Gpr_Parser.Common;

with GPR2.Project.Registry.Attribute.Description;
with GPR2.Project.Registry.Pack.Description;
with VSS.Strings.Conversions;

package body LSP.GPR_Documentation is

   procedure Get_Tooltip_Text
     (Self         : LSP.GPR_Files.File;
      Position     : LSP.Structures.Position;
      Tooltip_Text : out VSS.Strings.Virtual_String) is
      use Gpr_Parser.Common;

      Token  : constant Token_Reference := Self.Token (Position);

      use GPR2.Project.Registry.Attribute.Description;
      use GPR2.Project.Registry.Pack.Description;
      use GPR2;
      use Ada.Characters.Conversions;
   begin

      Tooltip_Text.Clear;

      if Token /= No_Token and then Token.Data.Kind = Gpr_Identifier then
         declare
            Previous : constant Token_Reference :=
                         Token.Previous (Exclude_Trivia => True);
         begin
            if Previous /= No_Token then
               case Previous.Data.Kind is
               when Gpr_Package | Gpr_End =>
                  Tooltip_Text.Append
                    (VSS.Strings.Conversions.To_Virtual_String
                       (Get_Package_Description
                            (Self.Get_Package (Position))));

               when Gpr_For =>
                  Tooltip_Text.Append
                    (VSS.Strings.Conversions.To_Virtual_String
                       (Get_Attribute_Description ((
                        Self.Get_Package (Position),
                        +(Optional_Name_Type (To_String (Token.Text)))))));

               when others =>
                  null;
               end case;
            end if;
         end;
      end if;
   end Get_Tooltip_Text;

end LSP.GPR_Documentation;
