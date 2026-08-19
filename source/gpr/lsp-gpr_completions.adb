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

with LSP.Enumerations;
with LSP.GPR_Completions.Tools;
with LSP.GPR_Files.References;
with LSP.Structures.LSPAny_Vectors;
with LSP.Text_Documents.Langkit_Documents;
with LSP.Utils; use LSP.Utils;

with VSS.String_Vectors;
with VSS.Strings;
with VSS.Strings.Conversions;

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
      Doc_Formats     : LSP.Structures.MarkupKind_Vector;
      Response        : in out LSP.Structures.Completion_Result;
      Reference       : LSP.GPR_Files.References.Reference;
      Prefix          : VSS.Strings.Virtual_String);
   --  Handle completion for specified kind starting from File/Pack

   procedure Fill_Attribute_Completion_Response
     (File            : LSP.GPR_Files.File_Access;
      Current_Package : Package_Id;
      Doc             : Boolean;
      Doc_Formats     : LSP.Structures.MarkupKind_Vector;
      Prefix          : VSS.Strings.Virtual_String;
      Response        : in out LSP.Structures.Completion_Result);
   --  Handle completion when cursor after "for" or "'" keyword

   procedure Fill_Package_Completion_Response
     (File            : LSP.GPR_Files.File_Access;
      Doc             : Boolean;
      Doc_Formats     : LSP.Structures.MarkupKind_Vector;
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
      Token_Kind      : Gpr_Parser.Common.Token_Kind;
      Doc             : Boolean;
      Doc_Formats     : LSP.Structures.MarkupKind_Vector;
      Prefix          : VSS.Strings.Virtual_String;
      Response        : in out LSP.Structures.Completion_Result;
      Attribute_Token : Gpr_Parser.Common.Token_Reference :=
        Gpr_Parser.Common.No_Token);
   --  Handle completion when cursor after "use", "renames", "extends", ":=",
   --  '(', ',', '&' tokens.
   --  Attribute_Token is the token containing the attribute name when
   --  completing after '(' in an attribute value context.

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

   procedure Add_Keyword
     (Name     : VSS.Strings.Virtual_String;
      Prefix   : VSS.Strings.Virtual_String;
      Response : in out LSP.Structures.Completion_Result);
   --  Append keyword 'Name' if it starts with 'Prefix' to 'Response'

   procedure Fill_Keyword_Completion_Response
     (Previous            : Gpr_Parser.Common.Token_Reference;
      Current_Package     : GPR2.Package_Id;
      In_Import_Partition : Boolean;
      Prefix              : VSS.Strings.Virtual_String;
      Response            : in out LSP.Structures.Completion_Result);
   --  Propose GPR keywords based on the syntactic context

   procedure Fill_Start_Of_File_Keywords
     (Prefix   : VSS.Strings.Virtual_String;
      Response : in out LSP.Structures.Completion_Result);
   --  Propose keywords valid at the start of a GPR file

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

   -----------------
   -- Add_Keyword --
   -----------------

   procedure Add_Keyword
     (Name     : VSS.Strings.Virtual_String;
      Prefix   : VSS.Strings.Virtual_String;
      Response : in out LSP.Structures.Completion_Result) is
      Item : LSP.Structures.CompletionItem;
   begin
      if VSS.Strings.Starts_With (Name, Prefix) then
         Item.label := Name;
         Item.kind  := (Is_Set => True, Value => LSP.Enumerations.Keyword);
         Response.Variant_2.items.Append (Item);
      end if;
   end Add_Keyword;

   ---------------------------------
   -- Fill_Start_Of_File_Keywords --
   ---------------------------------

   procedure Fill_Start_Of_File_Keywords
     (Prefix   : VSS.Strings.Virtual_String;
      Response : in out LSP.Structures.Completion_Result) is
   begin
      Add_Keyword ("with", Prefix, Response);
      Add_Keyword ("limited", Prefix, Response);
      Add_Keyword ("project", Prefix, Response);
      Add_Keyword ("abstract", Prefix, Response);
      Add_Keyword ("standard", Prefix, Response);
      Add_Keyword ("library", Prefix, Response);
      Add_Keyword ("aggregate", Prefix, Response);
      Add_Keyword ("configuration", Prefix, Response);
   end Fill_Start_Of_File_Keywords;

   --------------------------------------
   -- Fill_Keyword_Completion_Response --
   --------------------------------------

   procedure Fill_Keyword_Completion_Response
     (Previous            : Gpr_Parser.Common.Token_Reference;
      Current_Package     : GPR2.Package_Id;
      In_Import_Partition : Boolean;
      Prefix              : VSS.Strings.Virtual_String;
      Response            : in out LSP.Structures.Completion_Result)
   is
      Previous_Kind : constant GPC.Token_Kind := Previous.Data.Kind;

      function Construct_Before_Is return GPC.Token_Kind;
      --  Return the keyword token kind that introduced the 'is' construct
      --  (e.g. Gpr_Case, Gpr_Type, Gpr_Package), or Gpr_Identifier if it
      --  follows a project/package name.

      -------------------------
      -- Construct_Before_Is --
      -------------------------

      function Construct_Before_Is return GPC.Token_Kind is
         use type GPC.Token_Reference;
         T : GPC.Token_Reference := Previous.Previous (True);
      begin
         --  Skip back over the case selector / name.
         --  E.g: "case Prj.V is", "package Compiler is",
         --  or "case A.B'C is" (attribute reference).
         while T /= GPC.No_Token
           and then T.Data.Kind
                    in GPC.Gpr_Identifier | GPC.Gpr_Dot | GPC.Gpr_Tick
         loop
            T := T.Previous (True);
         end loop;

         if T = GPC.No_Token then
            return GPC.Gpr_Identifier;
         end if;

         return T.Data.Kind;
      end Construct_Before_Is;

      function In_Case_Construct return Boolean;
      --  Return True when the cursor lies inside a case construct, i.e. when
      --  a 'case ... is' has been opened before it and not yet closed by an
      --  'end case'.

      -----------------------
      -- In_Case_Construct --
      -----------------------

      function In_Case_Construct return Boolean is
         use type GPC.Token_Reference;
         use type GPC.Token_Kind;
         T     : GPC.Token_Reference := Previous;
         Depth : Natural := 0;
      begin
         --  Walk backwards, balancing every 'end case' against the 'case' it
         --  closes: the first unbalanced 'case' is the one we stand in.
         while T /= GPC.No_Token loop
            if T.Data.Kind = GPC.Gpr_Case then
               declare
                  Before : constant GPC.Token_Reference := T.Previous (True);
               begin
                  if Before /= GPC.No_Token
                    and then Before.Data.Kind = GPC.Gpr_End
                  then
                     Depth := Depth + 1;
                  elsif Depth = 0 then
                     return True;
                  else
                     Depth := Depth - 1;
                  end if;
               end;
            end if;

            T := T.Previous (True);
         end loop;

         return False;
      end In_Case_Construct;

      function In_Project_Header return Boolean;
      --  Return True when the cursor still stands before the project
      --  declaration, the only place where a project qualifier is
      --  admissible. 'project' has no token kind of its own, so look for the
      --  identifier itself: any occurrence before the cursor means the
      --  project declaration is already open.

      -----------------------
      -- In_Project_Header --
      -----------------------

      function In_Project_Header return Boolean is
         use type GPC.Token_Reference;
         use type GPC.Token_Kind;
         use VSS.Strings;
         T : GPC.Token_Reference := Previous;
      begin
         while T /= GPC.No_Token loop
            if T.Data.Kind = GPC.Gpr_Identifier
              and then To_Lower (VSS.Strings.To_Virtual_String (T.Text))
                         = "project"
            then
               return False;
            end if;

            T := T.Previous (True);
         end loop;

         return True;
      end In_Project_Header;

      procedure Add_Body_Keywords (Simple_Only : Boolean := False);
      --  Add keywords valid in a project or package body.
      --  When Simple_Only is set, only simple_declarative_item keywords are
      --  proposed: 'package' and 'type' declarations are not admissible.

      procedure Add_Body_Keywords (Simple_Only : Boolean := False) is
      begin
         Add_Keyword ("for", Prefix, Response);
         Add_Keyword ("case", Prefix, Response);
         Add_Keyword ("end", Prefix, Response);
         Add_Keyword ("null", Prefix, Response);
         --  type and package are only valid at project level, neither inside
         --  a package body nor inside a case item, which admit
         --  simple_declarative_item only (GPR grammar restriction).
         if not Simple_Only
           and then Current_Package = GPR2.Project_Level_Scope
         then
            Add_Keyword ("package", Prefix, Response);
            Add_Keyword ("type", Prefix, Response);
         end if;
      end Add_Body_Keywords;

   begin
      case Previous_Kind is
         when GPC.Gpr_Semicolon =>
            if In_Import_Partition then
               Fill_Start_Of_File_Keywords (Prefix, Response);
            else
               Add_Body_Keywords (Simple_Only => In_Case_Construct);
            end if;

         when GPC.Gpr_Is =>
            case Construct_Before_Is is
               when GPC.Gpr_Case =>
                  Add_Keyword ("when", Prefix, Response);
               when GPC.Gpr_Type =>
                  null;
               when GPC.Gpr_Package =>
                  --  Inside a package body: simple_declarative_item only
                  Add_Body_Keywords (Simple_Only => True);
               when others =>
                  Add_Body_Keywords;
            end case;

         when GPC.Gpr_Arrow =>
            --  Inside a case item: simple_declarative_item only, plus the
            --  'when' opening the next alternative.
            Add_Body_Keywords (Simple_Only => True);
            Add_Keyword ("when", Prefix, Response);

         when GPC.Gpr_When =>
            Add_Keyword ("others", Prefix, Response);

         when GPC.Gpr_Limited =>
            Add_Keyword ("with", Prefix, Response);

         when GPC.Gpr_Abstract =>
            Add_Keyword ("project", Prefix, Response);

         when GPC.Gpr_Identifier =>
            --  Qualifiers library, aggregate, configuration, standard lex
            --  as identifiers; in the project header the next keyword after
            --  any of them is "project" (or "library" for "aggregate library
            --  project"). Elsewhere they are plain names, so a variable
            --  called 'Library' still gets the follow-up keywords below.
            declare
               use VSS.Strings;
               Prev_Text : constant VSS.Strings.Virtual_String :=
                 To_Lower (VSS.Strings.To_Virtual_String (Previous.Text));
               Is_Qualifier : constant Boolean :=
                 (Prev_Text = "library"
                  or else Prev_Text = "configuration"
                  or else Prev_Text = "standard"
                  or else Prev_Text = "aggregate")
                 and then In_Project_Header;
            begin
               if Is_Qualifier then
                  Add_Keyword ("project", Prefix, Response);

                  if Prev_Text = "aggregate" then
                     Add_Keyword ("library", Prefix, Response);
                  end if;
               else
                  Add_Keyword ("is", Prefix, Response);
                  Add_Keyword ("extends", Prefix, Response);
                  Add_Keyword ("renames", Prefix, Response);
                  Add_Keyword ("use", Prefix, Response);
               end if;
            end;

         when GPC.Gpr_Par_Close =>
            Add_Keyword ("use", Prefix, Response);
            Add_Keyword ("is", Prefix, Response);

         when others =>
            null;
      end case;
   end Fill_Keyword_Completion_Response;

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
      Doc_Formats     : LSP.Structures.MarkupKind_Vector;
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
               Doc_Formats     => Doc_Formats,
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
      Doc_Formats     : LSP.Structures.MarkupKind_Vector;
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
                        Value =>
                          LSP.Utils.To_Documentation
                            (VSS.Strings.Conversions.To_Virtual_String
                               (PRAD.Get_Attribute_Description (Id)),
                             Doc_Formats));
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
      Doc_Formats     : LSP.Structures.MarkupKind_Vector;
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
                     Value =>
                       LSP.Utils.To_Documentation
                         (VSS.Strings.Conversions.To_Virtual_String
                            (PRP.Description.Get_Package_Description (Id)),
                          Doc_Formats));
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
      Doc_Formats     : LSP.Structures.MarkupKind_Vector;
      Prefix          : VSS.Strings.Virtual_String;
      Response        : in out LSP.Structures.Completion_Result;
      Attribute_Token : GPC.Token_Reference := GPC.No_Token)
      --  Attribute_Token is the token containing the attribute name when
      --  completing after '(' in an attribute value context.
   is
      use type GPC.Token_Kind;
      use type GPC.Token_Reference;

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
      --  Check if we're completing tool switches via the Default_Switches or
      --  the Switches attribute
      if (Token_Kind = GPC.Gpr_Par_Open or else Token_Kind = GPC.Gpr_Comma or else Token_Kind = GPC.Gpr_Amp)
        and then Attribute_Token /= GPC.No_Token
      then
         declare
            use VSS.Strings;
            Attr_Name : constant VSS.Strings.Virtual_String :=
              To_Lower (VSS.Strings.To_Virtual_String (Attribute_Token.Text));

            --  Check if there's a 'use' keyword between the attribute and
            --  the current position, indicating we're in the value context
            T : GPC.Token_Reference := Attribute_Token.Next (True);
            Has_Use : Boolean := False;
         begin
            --  Look for 'use' keyword after the attribute name
            while T /= GPC.No_Token loop
               if T.Data.Kind = GPC.Gpr_Use then
                  Has_Use := True;
                  exit;
               end if;
               T := T.Next (True);
            end loop;

            if Has_Use
              and then (Attr_Name = "default_switches" or else Attr_Name = "switches")
            then
               declare
                  Left_Par    : constant GPC.Token_Reference :=
                    Attribute_Token.Next (True);
                  Index_Token : GPC.Token_Reference := GPC.No_Token;
                  Index       : VSS.Strings.Virtual_String :=
                    VSS.Strings.Empty_Virtual_String;
               begin
                  if Left_Par /= GPC.No_Token
                    and then Left_Par.Data.Kind = GPC.Gpr_Par_Open
                  then
                     --  If completing after '(', look for an index parameter
                     --  to determine if the tool switches are for a specific
                     --  command (e.g. "gnatsas review")
                     Index_Token := Left_Par.Next (True);

                     if Index_Token /= GPC.No_Token
                       and then Index_Token.Data.Kind = GPC.Gpr_String
                     then
                        declare
                           Raw_Text : constant String :=
                             Ada.Characters.Conversions.To_String
                               (Index_Token.Text);
                        begin
                           Index :=
                             To_Lower
                               (VSS.Strings.Conversions.To_Virtual_String
                                  (Remove_Quote (Raw_Text)));
                        end;
                     end if;
                  end if;

                  LSP.GPR_Completions.Tools.Fill_Tools_Completion_Response
                    (File            => File,
                     Current_Package => Current_Package,
                     Index           => Index,
                     Prefix          => Prefix,
                     Response        => Response);
               end;
               return;
            end if;
         end;
      end if;

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
            Doc_Formats     => Doc_Formats,
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
      Doc_Formats             : LSP.Structures.MarkupKind_Vector;
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
               Doc_Formats     => Doc_Formats,
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
               Doc_Formats     => Doc_Formats,
               Prefix          => Prefix,
               Response => Response);
         end if;
      end Fill_Tick_Completion;

   begin
      if In_Comment then
         return;
      end if;

      --  Don't offer completion if cursor is inside an identifier
      --  (not at the end)
      if Current.Data.Kind = GPC.Gpr_Identifier
        and then not LSP.GPR_Files.At_End
                       (Current.Data.Sloc_Range, Location)
      then
         return;
      end if;

      declare
         First_Token_Prefix : constant VSS.Strings.Virtual_String :=
           (if Previous = GPC.No_Token
              and then Current.Data.Kind = GPC.Gpr_Identifier
              and then LSP.GPR_Files.At_End
                         (Current.Data.Sloc_Range, Location)
            then To_Lower (VSS.Strings.To_Virtual_String (Current.Text))
            else "");

         Identifier_Prefix : constant VSS.Strings.Virtual_String :=
           (if Previous /= GPC.No_Token
              and then Current.Data.Kind /= GPC.Gpr_Identifier
              and then Previous.Data.Kind = GPC.Gpr_Identifier
              and then LSP.GPR_Files.At_End
                         (Previous.Data.Sloc_Range, Location)
            then To_Lower (VSS.Strings.To_Virtual_String (Previous.Text))
            else "");
      begin
         --  Start of file: offer file-start keywords with prefix
         if Previous = GPC.No_Token then
            Fill_Start_Of_File_Keywords
              (First_Token_Prefix, Response);
            return;
         end if;

         if not VSS.Strings.Is_Empty (Identifier_Prefix) then
            Previous := Previous.Previous (True);
         end if;

         --  After consuming prefix, Previous may become No_Token
         if Previous = GPC.No_Token then
            Fill_Start_Of_File_Keywords
              (Identifier_Prefix, Response);
            return;
         end if;

         if Previous.Data.Kind
            in GPC.Gpr_For
             | GPC.Gpr_Package
             | GPC.Gpr_Extends
             | GPC.Gpr_Renames
             | GPC.Gpr_Use
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
                  Doc_Formats     => Doc_Formats,
                  Prefix          => Identifier_Prefix,
                  Response => Response);

            when GPC.Gpr_Package =>
               Fill_Package_Completion_Response
                 (File            => File,
                  Doc             => Compute_Doc_And_Details,
                  Doc_Formats     => Doc_Formats,
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

               declare
                  function Find_Attribute_Token return GPC.Token_Reference;
                  --  Find the attribute name token when completing after
                  --  '(' or ',' in an attribute value context.

                  function Skip_Use_Keyword
                    (T : GPC.Token_Reference;
                     Found_Use : out Boolean) return GPC.Token_Reference;
                  --  Skip past 'use' keyword if present, setting Found_Use
                  --  to indicate whether a 'use' keyword was found.

                  function Skip_To_Opening_Paren
                    (T : GPC.Token_Reference) return GPC.Token_Reference;
                  --  Skip backward through nested expressions to find the
                  --  opening '(' of the attribute value list.

                  ----------------------
                  -- Skip_Use_Keyword --
                  ----------------------

                  function Skip_Use_Keyword
                    (T : GPC.Token_Reference;
                     Found_Use : out Boolean) return GPC.Token_Reference is
                  begin
                     if T /= GPC.No_Token
                       and then T.Data.Kind = GPC.Gpr_Use
                     then
                        Found_Use := True;
                        return T.Previous (True);
                     end if;
                     Found_Use := False;
                     return T;
                  end Skip_Use_Keyword;

                  ---------------------------
                  -- Skip_To_Opening_Paren --
                  ---------------------------

                  function Skip_To_Opening_Paren
                    (T : GPC.Token_Reference) return GPC.Token_Reference
                  is
                     Result      : GPC.Token_Reference := T;
                     Paren_Count : Natural := 0;
                  begin
                     while Result /= GPC.No_Token loop
                        case Result.Data.Kind is
                           when GPC.Gpr_Par_Close =>
                              Paren_Count := Paren_Count + 1;
                           when GPC.Gpr_Par_Open =>
                              if Paren_Count = 0 then
                                 --  Found the opening '(', return previous
                                 return Result.Previous (True);
                              end if;
                              Paren_Count := Paren_Count - 1;
                           when others =>
                              null;
                        end case;
                        Result := Result.Previous (True);
                     end loop;
                     return Result;
                  end Skip_To_Opening_Paren;

                  --------------------------
                  -- Find_Attribute_Token --
                  --------------------------

                  function Find_Attribute_Token return GPC.Token_Reference
                  is
                     T : GPC.Token_Reference := Previous.Previous (True);
                     Found_Use : Boolean;
                  begin
                     --  Pattern: "for" Attr_Name "(" Index ")" "use" "("
                     T := Skip_Use_Keyword (T, Found_Use);

                     --  For comma or ampersand, skip back through the expression list
                     if Previous.Data.Kind in GPC.Gpr_Comma | GPC.Gpr_Amp
                     then
                        --  For ampersand, we might not have parentheses if the value
                        --  is a concatenation of variables. Skip backwards until we
                        --  find either a 'use' keyword or an opening paren.
                        if Previous.Data.Kind = GPC.Gpr_Amp then
                           while T /= GPC.No_Token
                             and then T.Data.Kind
                                      not in GPC.Gpr_Use | GPC.Gpr_Par_Open
                           loop
                              T := T.Previous (True);
                           end loop;
                           T := Skip_Use_Keyword (T, Found_Use);
                        else
                           T := Skip_To_Opening_Paren (T);
                           T := Skip_Use_Keyword (T, Found_Use);
                        end if;
                     end if;

                     if not Found_Use then
                        --  No 'use' keyword found, not in attribute
                        --  value context.
                        return GPC.No_Token;
                     end if;

                     --  Skip past index parameter: ')' Index '('
                     if T /= GPC.No_Token
                       and then T.Data.Kind = GPC.Gpr_Par_Close
                     then
                        T := T.Previous (True);  --  Index value
                        if T /= GPC.No_Token then
                           T := T.Previous (True);  --  '('
                           if T /= GPC.No_Token
                             and then T.Data.Kind = GPC.Gpr_Par_Open
                           then
                              T := T.Previous (True);  --  Attribute name
                           end if;
                        end if;
                     end if;

                     --  Return the attribute name if it's an identifier
                     if T /= GPC.No_Token
                       and then T.Data.Kind = GPC.Gpr_Identifier
                     then
                        return T;
                     end if;

                     return GPC.No_Token;
                  end Find_Attribute_Token;

                  Attribute_Token : constant GPC.Token_Reference :=
                    (if Previous.Data.Kind in GPC.Gpr_Par_Open | GPC.Gpr_Comma | GPC.Gpr_Amp
                     then Find_Attribute_Token
                     else GPC.No_Token);
               begin
                  Fill_Left_Part_Completion_Response
                    (File            => File,
                     Current_Package => File.Get_Package (Value.position),
                     Token_Kind      => Previous.Data.Kind,
                     Doc             => Compute_Doc_And_Details,
                     Doc_Formats     => Doc_Formats,
                     Prefix          => Identifier_Prefix,
                     Response        => Response,
                     Attribute_Token => Attribute_Token);
               end;

            when others =>
               Fill_Keyword_Completion_Response
                 (Previous            => Previous,
                  Current_Package     =>
                    File.Get_Package (Value.position),
                  In_Import_Partition =>
                    File.Token_In_Import_Partition (Previous),
                  Prefix              => Identifier_Prefix,
                  Response            => Response);

         end case;
      end;
   end Fill_Completion_Response;

   procedure Fill_Completion_Resolve_Response
     (Doc_Formats : LSP.Structures.MarkupKind_Vector;
      Response    : in out LSP.Structures.CompletionItem)
   is
      Pack     : Package_Id;
      Attr     : Q_Optional_Attribute_Id;
      Doc_Text : VSS.Strings.Virtual_String;

      C : LSP.Structures.JSON_Event_Vectors.Cursor :=
            Response.data.First;

      V : constant VSS.String_Vectors.Virtual_String_Vector :=
            LSP.Structures.LSPAny_Vectors.From_Any (C);
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
         Value  =>
           LSP.Utils.To_Documentation (Doc_Text, Doc_Formats));

   end Fill_Completion_Resolve_Response;

end LSP.GPR_Completions;
