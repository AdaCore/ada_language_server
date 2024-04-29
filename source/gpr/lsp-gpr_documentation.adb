------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                   Copyright (C) 2023-2024, AdaCore                             --
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
with Ada.Characters.Latin_1;
with GPR2.Project.Attribute;
with GPR2.Project.Variable;

with Gpr_Parser.Common;

with GPR2.Project.Registry.Attribute.Description;
with GPR2.Project.Registry.Pack.Description;

with LSP.GPR_Files.References;
with LSP.Text_Documents.Langkit_Documents;

with VSS.Strings.Conversions;

package body LSP.GPR_Documentation is

   procedure Get_Tooltip_Text
     (Self              : LSP.GPR_Files.File_Access;
      URI               : LSP.Structures.DocumentUri;
      Document_Provider : LSP.GPR_Documents.Document_Provider_Access;
      Position          : LSP.Structures.Position;
      Tooltip_Text      : out VSS.Strings.Virtual_String) is
      use Gpr_Parser.Common;

      package LKD renames LSP.Text_Documents.Langkit_Documents;
      package FR renames LSP.GPR_Files.References;

      procedure Append_Value (Reference : FR.Reference);
      --  If gpr parsed without error append to 'Tooltip_Text'
      --  current variable/attribute value.

      Location : constant Gpr_Parser.Slocs.Source_Location :=
                   LSP.GPR_Files.To_Langkit_Location
                     (LKD.To_Source_Location
                        (Line_Text => Self.Get_Line
                           (LKD.To_Source_Line (Position.line)),
                         Position  => Position));

      Token  : constant Token_Reference := Self.Token (Location);

      use GPR2.Project.Registry.Attribute.Description;
      use GPR2.Project.Registry.Pack.Description;
      use GPR2;
      use Ada.Characters.Conversions;

      ------------------
      -- Append_Value --
      ------------------

      procedure Append_Value (Reference : FR.Reference) is
      begin
         if Reference.Is_Variable_Reference
           or else Reference.Is_Attribute_Reference
         then
            declare
               Document : constant LSP.GPR_Documents.Document_Access :=
                            Document_Provider.Get_Open_Document (URI);
               use type LSP.GPR_Documents.Document_Access;
            begin
               if Document /= null and then not Document.Has_Errors
               then
                  if Reference.Is_Variable_Reference then
                     declare
                        Variable : constant GPR2.Project.Variable.Object :=
                                     Document.Get_Variable
                                       (Root_File => Self,
                                        Reference => Reference);
                     begin
                        if Variable.Is_Defined then
                           Tooltip_Text.Append
                             (VSS.Strings.Conversions.To_Virtual_String
                                (GPR2.Project.Variable.Image (Variable)));
                        end if;
                     end;
                  elsif Reference.Is_Attribute_Reference then
                     declare
                        Attribute : constant GPR2.Project.Attribute.Object :=
                                      Document.Get_Attribute
                                        (Root_File => Self,
                                         Reference => Reference);
                     begin
                        if Attribute.Is_Defined then
                           Tooltip_Text.Append
                             (VSS.Strings.Conversions.To_Virtual_String
                                (GPR2.Project.Attribute.Image (Attribute)
                                 & Ada.Characters.Latin_1.CR));
                        end if;
                     end;
                  end if;
               end if;
            end;
         end if;
      end Append_Value;

   begin

      Tooltip_Text.Clear;

      if Token /= No_Token and then Token.Data.Kind = Gpr_Identifier then
         declare
            Reference : constant FR.Reference :=
                          FR.Identifier_Reference
                            (File            => Self,
                             Current_Package => Self.Get_Package (Position),
                             Token           => Token);
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
                  Append_Value (Reference);
                  Tooltip_Text.Append
                    (VSS.Strings.Conversions.To_Virtual_String
                       (Get_Attribute_Description ((
                        Self.Get_Package (Position),
                        +(Optional_Name_Type (To_String (Token.Text)))))));

               when others =>
                  Append_Value (Reference);
                  if Reference.Is_Package_Reference then
                     Tooltip_Text.Append
                       (VSS.Strings.Conversions.To_Virtual_String
                          (Get_Package_Description
                               (Reference.Referenced_Package)));
                  else
                     declare
                        Attribute : constant FR.Attribute_Definition :=
                                      Reference.Referenced_Attribute;

                        use type FR.Attribute_Definition;
                     begin
                        if Attribute /= FR.No_Attribute_Definition then
                           Tooltip_Text.Append
                             (VSS.Strings.Conversions.To_Virtual_String
                                (Get_Attribute_Description
                                     (Attribute.Name)));
                        end if;
                     end;
                  end if;
               end case;
            end if;
         end;
      end if;
   end Get_Tooltip_Text;

end LSP.GPR_Documentation;
