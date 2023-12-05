------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                        Copyright (C) 2023, AdaCore                       --
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
with Ada.Characters.Handling;

with GPR2.Project.Registry.Attribute;
with GPR2.Project.Registry.Attribute.Description;
with GPR2.Project.Registry.Pack;
with GPR2.Project.Registry.Pack.Description;

with Gpr_Parser.Common;
with Gpr_Parser_Support.Text;

with LSP.Structures.LSPAny_Vectors;

with VSS.String_Vectors;
with VSS.Strings.Conversions;

package body LSP.GPR_Completions is

   use GPR2;

   package GPC renames Gpr_Parser.Common;
   package PRA renames GPR2.Project.Registry.Attribute;
   package PRAD renames GPR2.Project.Registry.Attribute.Description;
   package PRP renames GPR2.Project.Registry.Pack;
   package PRPD renames GPR2.Project.Registry.Pack.Description;

   procedure Fill_Attribute_Completion_Response
     (File     : LSP.GPR_Files.File;
      Position : LSP.Structures.Position;
      Doc      : Boolean;
      Filter   : String;
      Response : in out LSP.Structures.Completion_Result);
      --  Handle completion when cursor after for keyword

   procedure Fill_Package_Completion_Response
     (File     : LSP.GPR_Files.File;
      Doc      : Boolean;
      Filter   : String;
      Response : in out LSP.Structures.Completion_Result);
   --  Handle completion when cursor after package keyword

   ----------------------------------------
   -- Fill_Attribute_Completion_Response --
   ----------------------------------------

   procedure Fill_Attribute_Completion_Response
     (File     : LSP.GPR_Files.File;
      Position : LSP.Structures.Position;
      Doc      : Boolean;
      Filter   : String;
      Response : in out LSP.Structures.Completion_Result) is
      Starts : constant String := Ada.Characters.Handling.To_Lower (Filter);
      Current_Package : constant Package_Id :=
                          File.Get_Package (Position);
   begin
      if Current_Package = Project_Level_Scope
        or else PRP.Is_Allowed_In (Current_Package, File.Kind)
      then
         for Id of PRA.All_Attributes (Current_Package) loop
            declare
               Item : LSP.Structures.CompletionItem;
               Attr : constant String :=
                        Ada.Characters.Handling.To_Lower
                          (String (Name (Id.Attr)));
            begin
               if PRA.Get (Id).Is_Allowed_In (File.Kind)
                 and then
                   (Starts'Length = 0
                    or else (Starts'Length <= Attr'Length
                             and then Filter = Attr
                               (Attr'First .. Attr'First + Starts'Length - 1)))
               then
                  Item.label := VSS.Strings.Conversions.To_Virtual_String
                    (Image (Id.Attr));

                  declare
                     V : VSS.String_Vectors.Virtual_String_Vector;
                  begin
                     V.Append (VSS.Strings.Conversions.
                                 To_Virtual_String (Image (Current_Package)));
                     V.Append (VSS.Strings.Conversions.
                                 To_Virtual_String (Image (Id.Attr)));
                     LSP.Structures.LSPAny_Vectors.To_Any
                       (V, Item.data);
                  end;

                  if Doc then
                     Item.documentation :=
                       (Is_Set => True,
                        Value => (Is_Virtual_String => True,
                                  Virtual_String =>
                                    VSS.Strings.Conversions.To_Virtual_String
                                      (PRAD.Get_Attribute_Description
                                         (Id))));
                  end if;

                  Response.Variant_2.items.Append (Item);
               end if;
            end;
         end loop;
      end if;
   end Fill_Attribute_Completion_Response;

   --------------------------------------
   -- Fill_Package_Completion_Response --
   --------------------------------------

   procedure Fill_Package_Completion_Response
     (File     : LSP.GPR_Files.File;
      Doc      : Boolean;
      Filter   : String;
      Response : in out LSP.Structures.Completion_Result) is
      Starts : constant String := Ada.Characters.Handling.To_Lower (Filter);
      Kind   : constant Project_Kind := File.Kind;
   begin
      for Id of PRP.All_Packages loop
         declare
            Item : LSP.Structures.CompletionItem;
            Pack : constant String :=
                     Ada.Characters.Handling.To_Lower (String (Name (Id)));
         begin
            if not File.In_Packages (Id)
              and then PRP.Is_Allowed_In (Id, Kind)
              and then
                (Starts'Length = 0
                 or else (Starts'Length <= Pack'Length
                          and then Filter = Pack
                            (Pack'First .. Pack'First + Starts'Length - 1)))
            then
               Item.label := VSS.Strings.Conversions.To_Virtual_String
                 (Image (Id));

               declare
                  V : VSS.String_Vectors.Virtual_String_Vector;
               begin
                  V.Append (VSS.Strings.Conversions.To_Virtual_String
                            (Image (Id)));
                  LSP.Structures.LSPAny_Vectors.To_Any
                    (V, Item.data);
               end;

               if Doc then
                  Item.documentation :=
                    (Is_Set => True,
                     Value => (Is_Virtual_String => True,
                               Virtual_String =>
                                 VSS.Strings.Conversions.To_Virtual_String
                                   (PRP.Description.Get_Package_Description
                                      (Id))));
               end if;

               Response.Variant_2.items.Append (Item);
            end if;
         end;
      end loop;
   end Fill_Package_Completion_Response;

   ------------------------------
   -- Fill_Completion_Response --
   ------------------------------

   procedure Fill_Completion_Response
     (File_Provider           : LSP.GPR_Files.File_Provider_Access;
      Value                   : LSP.Structures.CompletionParams;
      Compute_Doc_And_Details : Boolean;
      Response                : in out LSP.Structures.Completion_Result)
   is
      File    : constant LSP.GPR_Files.File_Access :=
                   LSP.GPR_Files.Parse
                     (File_Provider => File_Provider,
                      Path          => File_Provider.To_File
                        (Value.textDocument.uri));
      Current : constant GPC.Token_Reference := File.Token (Value.position);
      In_Comment : constant Boolean :=
                     LSP.GPR_Files.Position_Is_In_Comment
                       (Current, Value.position);
      Previous : constant GPC.Token_Reference := Current.Previous (True);

      function To_String
        (Text : Gpr_Parser_Support.Text.Text_Type) return String is
        (Ada.Characters.Handling.To_Lower
           (Ada.Characters.Conversions.To_String (Text)));

      use type GPC.Token_Reference, GPC.Token_Kind;

   begin
      if not In_Comment then
         if Current.Data.Kind in
           GPC.Gpr_Whitespace | GPC.Gpr_Comment | GPC.Gpr_Termination
             and then Previous /= GPC.No_Token
         then
            if Previous.Data.Kind in GPC.Gpr_For | GPC.Gpr_Package then
               if not LSP.GPR_Files.At_End (Previous, Value.position) then
                  case Previous.Data.Kind is

                  when GPC.Gpr_For =>
                     Fill_Attribute_Completion_Response
                       (File     => File.all,
                        Position => Value.position,
                        Doc      => Compute_Doc_And_Details,
                        Filter   => "",
                        Response => Response);

                  when GPC.Gpr_Package =>
                     Fill_Package_Completion_Response
                       (File     => File.all,
                        Doc      => Compute_Doc_And_Details,
                        Filter   => "",
                        Response => Response);

                  when others =>
                     null;

                  end case;
               end if;

            elsif Previous.Data.Kind = GPC.Gpr_Identifier
              and then LSP.GPR_Files.At_End (Previous, Value.position)
            then
               declare
                  Before_Identifier : constant GPC.Token_Reference :=
                                        Previous.Previous (True);
               begin
                  if Before_Identifier /= GPC.No_Token then
                     case Before_Identifier.Data.Kind is

                     when GPC.Gpr_For =>
                        Fill_Attribute_Completion_Response
                          (File     => File.all,
                           Position => Value.position,
                           Doc      => Compute_Doc_And_Details,
                           Filter   => To_String (Previous.Text),
                           Response => Response);

                     when GPC.Gpr_Package =>
                        Fill_Package_Completion_Response
                          (File     => File.all,
                           Doc      => Compute_Doc_And_Details,
                           Filter   => To_String (Previous.Text),
                           Response => Response);

                     when others =>
                        null;

                     end case;
                  end if;
               end;
            end if;
         end if;
      end if;
   end Fill_Completion_Response;

   procedure Fill_Completion_Resolve_Response
     (Response : in out LSP.Structures.CompletionItem) is
      Pack     : Package_Id;
      Attr     : Q_Optional_Attribute_Id;
      Doc_Text : VSS.Strings.Virtual_String;

      C : LSP.Structures.JSON_Event_Vectors.Cursor :=
            Response.data.First;

      V : constant VSS.String_Vectors.Virtual_String_Vector :=
            LSP.Structures.LSPAny_Vectors.From_Any
              (LSP.Structures.JSON_Event_Vectors.Cursor (C));
   begin
      Pack := +Optional_Name_Type (VSS.Strings.Conversions.To_UTF_8_String
                                   (V.First_Element));
      if V.Length > 1 then
         Attr := (Pack,
                  +Optional_Name_Type (VSS.Strings.Conversions.To_UTF_8_String
                    (V.Last_Element)));

            Doc_Text := VSS.Strings.Conversions.To_Virtual_String
              (PRAD.Get_Attribute_Description (Attr));

      else
         Doc_Text := VSS.Strings.Conversions.To_Virtual_String
           (PRPD.Get_Package_Description (Pack));
      end if;

      Response.documentation :=
        (Is_Set => True,
         Value  => LSP.Structures.Virtual_String_Or_MarkupContent'
           (Is_Virtual_String => True,
            Virtual_String    => Doc_Text));

   end Fill_Completion_Resolve_Response;

end LSP.GPR_Completions;
