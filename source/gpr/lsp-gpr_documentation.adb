------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                   Copyright (C) 2023-2024, AdaCore                       --
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
with Ada.Characters.Wide_Wide_Latin_1;

with GPR2.Project.Attribute;
with GPR2.Project.Typ;
with GPR2.Project.Variable;
with GPR2.Source_Reference;

with Gpr_Parser.Common;
with Gpr_Parser_Support.Slocs;

with GPR2.Project.Registry.Attribute.Description;
with GPR2.Project.Registry.Pack.Description;

with LSP.GPR_Files.References;
with LSP.Text_Documents.Langkit_Documents;

with VSS.Strings.Conversions;

package body LSP.GPR_Documentation is

   function Get_Documentation
     (Ref   : Gpr_Parser.Common.Token_Reference;
      Style : GNATdoc.Comments.Options.Documentation_Style)
      return VSS.Strings.Virtual_String;
   --  Get variable/type declaration comment.

   -----------------------
   -- Get_Documentation --
   -----------------------

   function Get_Documentation
     (Ref   : Gpr_Parser.Common.Token_Reference;
      Style : GNATdoc.Comments.Options.Documentation_Style)
      return VSS.Strings.Virtual_String
   is
      Documentation : VSS.Strings.Virtual_String;
      Add_LF        : Boolean := False;
      package Slocs renames Gpr_Parser_Support.Slocs;
      use type Slocs.Line_Number;

      Line  : Slocs.Line_Number := Ref.Data.Sloc_Range.Start_Line;
      Token : Gpr_Parser.Common.Token_Reference := Ref;

      use type GNATdoc.Comments.Options.Documentation_Style;

      function Next return Gpr_Parser.Common.Token_Reference is
        (if Style = GNATdoc.Comments.Options.GNAT
         then Token.Next
         else Token.Previous);
      --  Go to next or previous token depending on 'Style' value

      function Valid_Comment return Boolean;
      --  Return True if comment token is still part of 'Ref' comment.

      -------------------
      -- Valid_Comment --
      -------------------

      function Valid_Comment return Boolean is
         Current_Line : constant Slocs.Line_Number :=
                          Token.Data.Sloc_Range.Start_Line;
         Valid        : Boolean := False;
      begin
         case Style is
            when GNATdoc.Comments.Options.GNAT =>
               if Current_Line <= Line + 1 then
                  Valid := True;
               end if;
            when GNATdoc.Comments.Options.Leading =>
               if Current_Line >= Line - 1 then
                  Valid := True;
               end if;
         end case;
         if Valid then
            --  update Line to allow next/previous comment to still be valid
            Line := Current_Line;
         end if;
         return Valid;
      end Valid_Comment;

      use type Gpr_Parser.Common.Token_Reference;
      use type Gpr_Parser.Common.Token_Kind;
   begin
      Token := Next;
      while Token /= Gpr_Parser.Common.No_Token loop
         if Token.Data.Kind = Gpr_Parser.Common.Gpr_Comment
         then
            if Valid_Comment then
               case Style is
               when GNATdoc.Comments.Options.GNAT =>
                  if Add_LF then
                     Documentation.Append
                       (VSS.Strings.To_Virtual_String
                          (Ada.Characters.Wide_Wide_Latin_1.LF & Token.Text));
                  else
                     Documentation.Append
                       (VSS.Strings.To_Virtual_String
                          (Token.Text));
                  end if;
               when GNATdoc.Comments.Options.Leading =>
                  if Add_LF then
                     Documentation.Prepend
                       (VSS.Strings.To_Virtual_String
                          (Token.Text & Ada.Characters.Wide_Wide_Latin_1.LF));
                  else
                     Documentation.Prepend
                       (VSS.Strings.To_Virtual_String
                          (Token.Text));
                  end if;
               end case;
               Add_LF := True;
            else
               exit;
            end if;
         end if;
         Token := Next;
      end loop;
      return Documentation;
   end Get_Documentation;

   ----------------------
   -- Get_Tooltip_Text --
   ----------------------

   procedure Get_Tooltip_Text
     (Self               : LSP.GPR_Files.File_Access;
      URI                : LSP.Structures.DocumentUri;
      Document_Provider  : LSP.GPR_Documents.Document_Provider_Access;
      Position           : LSP.Structures.Position;
      Style              : GNATdoc.Comments.Options.Documentation_Style;
      Declaration_Text   : out VSS.Strings.Virtual_String;
      Documentation_Text : out VSS.Strings.Virtual_String;
      Location_Text      : out VSS.Strings.Virtual_String) is
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
             or else Reference.Is_Type_Reference
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
                           Declaration_Text.Append
                             (VSS.Strings.Conversions.To_Virtual_String
                                (GPR2.Project.Variable.Image (Variable)));
                           Location_Text.Append
                             (VSS.Strings.Conversions.To_Virtual_String
                                (GPR2.Source_Reference.Format
                                     (GPR2.Source_Reference.Object
                                          (Variable))));
                           if Variable.Has_Type then
                              declare
                                 Typ : constant GPR2.Project.Typ.Object :=
                                         Variable.Typ;
                              begin
                                 if Typ.Is_Defined then
                                    Declaration_Text.Prepend
                                      (VSS.Strings.Conversions.To_Virtual_String
                                         (GPR2.Project.Typ.Image (Typ)
                                          & Ada.Characters.Latin_1.CR));
                                 end if;
                              end;
                           end if;
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
                           Declaration_Text.Append
                             (VSS.Strings.Conversions.To_Virtual_String
                                (GPR2.Project.Attribute.Image (Attribute)
                                 & Ada.Characters.Latin_1.CR));
                           Location_Text.Append
                             (VSS.Strings.Conversions.To_Virtual_String
                                (GPR2.Source_Reference.Format
                                     (GPR2.Source_Reference.Object
                                          (Attribute))));
                        end if;

                     end;
                  elsif Reference.Is_Type_Reference then
                     declare
                        Typ : constant GPR2.Project.Typ.Object :=
                                Document.Get_Type

                                  (Root_File => Self,
                                   Reference => Reference);
                     begin
                        if Typ.Is_Defined then
                           Declaration_Text.Append
                             (VSS.Strings.Conversions.To_Virtual_String
                                (GPR2.Project.Typ.Image (Typ)
                                 & Ada.Characters.Latin_1.CR));
                           Location_Text.Append
                             (VSS.Strings.Conversions.To_Virtual_String
                                (GPR2.Source_Reference.Format
                                     (GPR2.Source_Reference.Object
                                          (Typ))));
                        end if;
                     end;

                  end if;
               end if;
            end;
         end if;
      end Append_Value;

   begin

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
                  Documentation_Text.Append
                    (VSS.Strings.Conversions.To_Virtual_String
                       (Get_Package_Description
                            (Self.Get_Package (Position))));

               when Gpr_For =>
                  Append_Value (Reference);
                  Documentation_Text.Append
                    (VSS.Strings.Conversions.To_Virtual_String
                       (Get_Attribute_Description ((
                        Self.Get_Package (Position),
                        +(Optional_Name_Type (To_String (Token.Text)))))));

               when others =>
                  Append_Value (Reference);
                  if Reference.Is_Package_Reference then
                     Documentation_Text.Append
                       (VSS.Strings.Conversions.To_Virtual_String
                          (Get_Package_Description
                               (Reference.Referenced_Package)));
                  elsif Reference.Is_Attribute_Reference then
                     declare
                        Attribute : constant FR.Attribute_Definition :=
                                      Reference.Referenced_Attribute;

                        use type FR.Attribute_Definition;
                     begin
                        if Attribute /= FR.No_Attribute_Definition then
                           Documentation_Text.Append
                             (VSS.Strings.Conversions.To_Virtual_String
                                (Get_Attribute_Description
                                     (Attribute.Name)));
                        end if;
                     end;
                  elsif Reference.Is_Variable_Reference
                    or else Reference.Is_Type_Reference
                  then
                     Documentation_Text.Append
                       (Get_Documentation
                          (LSP.GPR_Files.References.Token_Reference
                               (Self, Position),
                           Style));
                  end if;
               end case;
            end if;
         end;
      end if;
   end Get_Tooltip_Text;

end LSP.GPR_Documentation;
