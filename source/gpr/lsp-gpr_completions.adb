------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                      Copyright (C) 2023-2024, AdaCore                    --
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
with GPR2.Project.Registry.Attribute;
with GPR2.Project.Registry.Attribute.Description;
with GPR2.Project.Registry.Pack;
with GPR2.Project.Registry.Pack.Description;

with Gpr_Parser.Common;

with LSP.GPR_Files.References;
with LSP.Structures.LSPAny_Vectors;
with LSP.Text_Documents.Langkit_Documents;

with VSS.String_Vectors;
with VSS.Strings.Conversions;
with VSS.Transformers.Casing;

package body LSP.GPR_Completions is

   use GPR2;

   package GPC renames Gpr_Parser.Common;
   package PRA renames GPR2.Project.Registry.Attribute;
   package PRAD renames GPR2.Project.Registry.Attribute.Description;
   package PRP renames GPR2.Project.Registry.Pack;
   package PRPD renames GPR2.Project.Registry.Pack.Description;
   package LKD renames LSP.Text_Documents.Langkit_Documents;

   procedure Fill_Completion_Response
     (File            : LSP.GPR_Files.File_Access;
      Doc             : Boolean;
      Response        : in out LSP.Structures.Completion_Result;
      Reference       : LSP.GPR_Files.References.Reference;
      Prefix          : VSS.Strings.Virtual_String);
   --  Handle completion for specified kind starting from File/Pack

   procedure Fill_Attribute_Completion_Response
     (File            : LSP.GPR_Files.File_Access;
      Current_Package : Package_Id;
      Doc             : Boolean;
      Prefix          : VSS.Strings.Virtual_String;
      Response        : in out LSP.Structures.Completion_Result);
   --  Handle completion when cursor after "for" or "'" keyword

   procedure Fill_Package_Completion_Response
     (File            : LSP.GPR_Files.File_Access;
      Doc             : Boolean;
      Prefix          : VSS.Strings.Virtual_String;
      Unexisting_Only : Boolean;
      Response        : in out LSP.Structures.Completion_Result);
   --  Handle completion when cursor after "package" keyword or after a project
   --  reference.
   --  If Unexisting_Only is returned, only the packages that have not been
   --  defined in the given project file will be returned. Otherwise, only
   --  the package that have been defined will be returned.

   procedure Fill_Type_Completion_Response
     (File     : LSP.GPR_Files.File_Access;
      Prefix   : VSS.Strings.Virtual_String;
      Response : in out LSP.Structures.Completion_Result);
   --  Handle completion when cursor after ':' character

   procedure Fill_Variable_Completion_Response
     (File            : LSP.GPR_Files.File_Access;
      Current_Package : Package_Id;
      Prefix          : VSS.Strings.Virtual_String;
      Response        : in out LSP.Structures.Completion_Result);
   --  Handle completion when cursor after a project/package reference.

   procedure Fill_Left_Part_Completion_Response
     (File            : LSP.GPR_Files.File_Access;
      Current_Package : Package_Id;
      Token_Kind      : GPC.Token_Kind;
      Doc             : Boolean;
      Prefix          : VSS.Strings.Virtual_String;
      Response        : in out LSP.Structures.Completion_Result);
   --  Handle completion when cursor after "use", "renames", "extends", ":=",
   --  '(', ',', '&' tokens.

   procedure Add_Item
     (Name     : VSS.Strings.Virtual_String;
      Prefix   : VSS.Strings.Virtual_String;
      Response : in out LSP.Structures.Completion_Result);
   --  Append 'Name' if it starts with 'Prefix' to 'Response'

   procedure Add_Items
     (Items    : VSS.String_Vectors.Virtual_String_Vector;
      Prefix   : VSS.Strings.Virtual_String;
      Response : in out LSP.Structures.Completion_Result);
   --  Append 'Items' if element starts with 'Prefix' to 'Response'

   function To_Lower
     (S : VSS.Strings.Virtual_String) return VSS.Strings.Virtual_String is
      (S.Transform (VSS.Transformers.Casing.To_Lowercase));

   --------------
   -- Add_Item --
   --------------

   procedure Add_Item
     (Name     : VSS.Strings.Virtual_String;
      Prefix   : VSS.Strings.Virtual_String;
      Response : in out LSP.Structures.Completion_Result) is
      Item       : LSP.Structures.CompletionItem;
   begin
      if VSS.Strings.Starts_With (To_Lower (Name), Prefix) then
         Item.label := Name;
         Response.Variant_2.items.Append (Item);
      end if;
   end Add_Item;

   ---------------
   -- Add_Items --
   ---------------

   procedure Add_Items
     (Items    : VSS.String_Vectors.Virtual_String_Vector;
      Prefix   : VSS.Strings.Virtual_String;
      Response : in out LSP.Structures.Completion_Result) is
   begin
      for Item of Items loop
         Add_Item (Item, Prefix, Response);
      end loop;
   end Add_Items;

   ------------------------------
   -- Fill_Completion_Response --
   ------------------------------

   procedure Fill_Completion_Response
     (File            : LSP.GPR_Files.File_Access;
      Doc             : Boolean;
      Response        : in out LSP.Structures.Completion_Result;
      Reference       : LSP.GPR_Files.References.Reference;
      Prefix          : VSS.Strings.Virtual_String) is
      use LSP.GPR_Files.References;

      Referenced_File : constant LSP.GPR_Files.File_Access :=
                          LSP.GPR_Files.References.Referenced_File
                            (File, Reference);
   begin
      if Is_Project_Reference (Reference) then
         if In_Type_Reference (Reference) then
            Fill_Type_Completion_Response (Referenced_File, Prefix, Response);
         else
            Fill_Variable_Completion_Response
              (Referenced_File, GPR2.Project_Level_Scope, Prefix, Response);
            Fill_Package_Completion_Response
              (File            => File,
               Doc             => Doc,
               Prefix          => Prefix,
               Unexisting_Only => False,
               Response        => Response);
         end if;
      elsif Is_Package_Reference (Reference) then
         Fill_Variable_Completion_Response
           (Referenced_File, Referenced_Package (Reference), Prefix, Response);
      end if;
   end Fill_Completion_Response;

   ----------------------------------------
   -- Fill_Attribute_Completion_Response --
   ----------------------------------------

   procedure Fill_Attribute_Completion_Response
     (File            : LSP.GPR_Files.File_Access;
      Current_Package : Package_Id;
      Doc             : Boolean;
      Prefix          : VSS.Strings.Virtual_String;
      Response        : in out LSP.Structures.Completion_Result) is
   begin
      if Current_Package = Project_Level_Scope
        or else PRP.Is_Allowed_In (Current_Package, File.Kind)
      then
         for Id of PRA.All_Attributes (Current_Package) loop
            declare
               Item : LSP.Structures.CompletionItem;
            begin
               if PRA.Get (Id).Is_Allowed_In (File.Kind)
                 and then VSS.Strings.Starts_With
                   (To_Lower (VSS.Strings.To_Virtual_String
                      (Ada.Characters.Conversions.To_Wide_Wide_String
                         (String (Name (Id.Attr))))),
                    Prefix)
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
     (File            : LSP.GPR_Files.File_Access;
      Doc             : Boolean;
      Prefix          : VSS.Strings.Virtual_String;
      Unexisting_Only : Boolean;
      Response        : in out LSP.Structures.Completion_Result) is
      Kind   : constant Project_Kind := File.Kind;
   begin
      for Id of PRP.All_Packages loop
         declare
            Item : LSP.Structures.CompletionItem;
            Exists_In_File : constant Boolean := File.In_Packages (Id);
         begin
            if (Unexisting_Only xor Exists_In_File)
              and then PRP.Is_Allowed_In (Id, Kind)
              and then VSS.Strings.Starts_With
                (To_Lower (VSS.Strings.To_Virtual_String
                   (Ada.Characters.Conversions.To_Wide_Wide_String
                      (String (Name (Id))))),
                 Prefix)
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

   -----------------------------------
   -- Fill_Type_Completion_Response --
   -----------------------------------

   procedure Fill_Type_Completion_Response
     (File     : LSP.GPR_Files.File_Access;
      Prefix   : VSS.Strings.Virtual_String;
      Response : in out LSP.Structures.Completion_Result) is
   begin
      Add_Items (File.Types, Prefix, Response);
      Add_Items (File.Projects, Prefix, Response);
   end Fill_Type_Completion_Response;

   ---------------------------------------
   -- Fill_Variable_Completion_Response --
   ---------------------------------------

   procedure Fill_Variable_Completion_Response
     (File            : LSP.GPR_Files.File_Access;
      Current_Package : Package_Id;
      Prefix          : VSS.Strings.Virtual_String;
      Response        : in out LSP.Structures.Completion_Result) is
   begin
      Add_Items (File.Variables (Current_Package), Prefix, Response);
   end Fill_Variable_Completion_Response;

   ----------------------------------------
   -- Fill_Left_Part_Completion_Response --
   ----------------------------------------

   procedure Fill_Left_Part_Completion_Response
     (File            : LSP.GPR_Files.File_Access;
      Current_Package : Package_Id;
      Token_Kind      : GPC.Token_Kind;
      Doc             : Boolean;
      Prefix          : VSS.Strings.Virtual_String;
      Response        : in out LSP.Structures.Completion_Result) is

      procedure Fill_Project_Completion_Response
        (File            : LSP.GPR_Files.File_Access;
         Prefix          : VSS.Strings.Virtual_String;
         Response        : in out LSP.Structures.Completion_Result);
      --  Add project identifiers to Response.

      --------------------------------------
      -- Fill_Project_Completion_Response --
      --------------------------------------

      procedure Fill_Project_Completion_Response
        (File            : LSP.GPR_Files.File_Access;
         Prefix          : VSS.Strings.Virtual_String;
         Response        : in out LSP.Structures.Completion_Result) is
      begin
         Add_Item ("project", Prefix, Response);
         Add_Item (File.Name, Prefix, Response);
         Add_Items (File.Projects, Prefix, Response);
      end Fill_Project_Completion_Response;

   begin
      if not (Token_Kind in GPC.Gpr_Renames | GPC.Gpr_Extends) then
         --  Add current_package's variables

         Fill_Variable_Completion_Response
           (File, Current_Package, Prefix, Response);

         --  Add project level's variable if not yet done

         if Current_Package /= GPR2.Project_Level_Scope then
            Fill_Variable_Completion_Response
              (File, GPR2.Project_Level_Scope, Prefix, Response);
         end if;

         --  Add packages

         Fill_Package_Completion_Response
           (File            => File,
            Doc             => Doc,
            Prefix          => Prefix,
            Unexisting_Only => False,
            Response        => Response);
      end if;

      --  Add projects

      Fill_Project_Completion_Response
        (File            => File,
         Prefix          => Prefix,
         Response        => Response);

   end Fill_Left_Part_Completion_Response;

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

      Location : constant Gpr_Parser.Slocs.Source_Location :=
                   LSP.GPR_Files.To_Langkit_Location
                     (LKD.To_Source_Location
                        (Line_Text => File.Get_Line
                           (LKD.To_Source_Line (Value.position.line)),
                         Position  => Value.position));

      Current : constant GPC.Token_Reference := File.Token (Location);

      In_Comment : constant Boolean :=
                     LSP.GPR_Files.Position_Is_In_Comment (Current, Location);

      Previous : GPC.Token_Reference := Current.Previous (True);

      use type GPC.Token_Reference, GPC.Token_Kind;

      procedure Fill_Tick_Completion
        (Tick_Token : GPC.Token_Reference;
         Prefix     : VSS.Strings.Virtual_String);

      procedure Fill_Dot_Completion
        (Dot_Token : GPC.Token_Reference;
         Prefix    : VSS.Strings.Virtual_String);

      -------------------------
      -- Fill_Dot_Completion --
      -------------------------

      procedure Fill_Dot_Completion
        (Dot_Token : GPC.Token_Reference;
         Prefix    : VSS.Strings.Virtual_String)
      is
         Reference : constant LSP.GPR_Files.References.Reference
           := LSP.GPR_Files.References.Identifier_Reference
             (File            => File,
              Current_Package => File.Get_Package
                (Value.position),
              Token           => Dot_Token.Previous (True));

         use type GPR_Files.References.Reference;
      begin

         if Reference /= GPR_Files.References.No_Reference then
            Fill_Completion_Response
              (File            => File,
               Doc             => Compute_Doc_And_Details,
               Prefix          => Prefix,
               Reference       => Reference,
               Response        => Response);
         end if;
      end Fill_Dot_Completion;

      --------------------------
      -- Fill_Tick_Completion --
      --------------------------

      procedure Fill_Tick_Completion
        (Tick_Token : GPC.Token_Reference;
         Prefix     : VSS.Strings.Virtual_String)
      is
         Last_Identifier : constant GPC.Token_Reference :=
                             Tick_Token.Previous (True);
         Reference : constant LSP.GPR_Files.References.Reference
           := LSP.GPR_Files.References.Identifier_Reference
             (File            => File,
              Current_Package => File.Get_Package
                (Value.position),
              Token           => Last_Identifier);

         use type GPR_Files.References.Reference;
      begin

         if Reference /= GPR_Files.References.No_Reference then
            Fill_Attribute_Completion_Response
              (File            => File,
               Current_Package =>
                 GPR_Files.References.Referenced_Package
                   (Reference),
               Doc             => Compute_Doc_And_Details,
               Prefix          => Prefix,
               Response => Response);
         end if;
      end Fill_Tick_Completion;

   begin
      if not In_Comment
        and then Previous /= GPC.No_Token
        and then Current.Data.Kind /= GPC.Gpr_Identifier
      then
         declare
            Identifier_Prefix : constant VSS.Strings.Virtual_String :=
                                  (if Previous.Data.Kind = GPC.Gpr_Identifier
                                   and then LSP.GPR_Files.At_End
                                     (Previous.Data.Sloc_Range, Location)
                                   then To_Lower
                                     (VSS.Strings.To_Virtual_String
                                        (Previous.Text))
                                   else "");
         begin
            if not VSS.Strings.Is_Empty (Identifier_Prefix) then
               Previous := Previous.Previous (True);
            end if;

            if Previous.Data.Kind in
              GPC.Gpr_For | GPC.Gpr_Package | GPC.Gpr_Extends
                | GPC.Gpr_Renames | GPC.Gpr_Use
              and then LSP.GPR_Files.At_End
                (Previous.Data.Sloc_Range, Location)
            then
               --  missing space after 'package', 'renames', 'extends', 'for',
               --  'use' keyword to allow completion
               return;
            end if;
            case Previous.Data.Kind is

               when GPC.Gpr_For =>
                  Fill_Attribute_Completion_Response
                    (File            => File,
                     Current_Package => File.Get_Package (Value.position),
                     Doc             => Compute_Doc_And_Details,
                     Prefix          => Identifier_Prefix,
                     Response => Response);

               when GPC.Gpr_Package =>
                  Fill_Package_Completion_Response
                    (File            => File,
                     Doc             => Compute_Doc_And_Details,
                     Prefix          => Identifier_Prefix,
                     Unexisting_Only => True,
                     Response        => Response);

               when GPC.Gpr_Colon =>
                  Fill_Type_Completion_Response
                    (File     => File,
                     Prefix   => Identifier_Prefix,
                     Response => Response);

               when GPC.Gpr_Dot =>
                  Fill_Dot_Completion (Previous, Identifier_Prefix);

               when GPC.Gpr_Tick =>
                  Fill_Tick_Completion (Previous, Identifier_Prefix);

               when GPC.Gpr_Assign
                  | GPC.Gpr_Par_Open
                  | GPC.Gpr_Comma
                  | GPC.Gpr_Amp
                  | GPC.Gpr_Use
                  | GPC.Gpr_Renames
                  | GPC.Gpr_Extends =>
                  Fill_Left_Part_Completion_Response
                    (File            => File,
                     Current_Package => File.Get_Package (Value.position),
                     Token_Kind      => Previous.Data.Kind,
                     Doc             => Compute_Doc_And_Details,
                     Prefix          => Identifier_Prefix,
                     Response        => Response);

               when others =>
                  null;

            end case;
         end;
      end if;
   end Fill_Completion_Response;

   procedure Fill_Completion_Resolve_Response
     (Response : in out LSP.Structures.CompletionItem)
   is
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
