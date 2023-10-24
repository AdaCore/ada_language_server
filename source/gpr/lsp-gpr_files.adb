------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                       Copyright (C) 2023, AdaCore                        --
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
--
--  This package provides a Gpr file abstraction implementation.

with Ada.Characters.Handling;

with GNATCOLL.Utils;

with GPR2.Environment;
with GPR2.Path_Name.Set;

with Gpr_Parser_Support.Text;

with VSS.Strings.Conversions;
with VSS.Strings.Formatters.Generic_Integers;

pragma Warnings (Off, "* is an internal GNAT unit");
with System.Soft_Links; use System.Soft_Links;
pragma Warnings (On, "* is an internal GNAT unit");

package body LSP.GPR_Files is

   Is_Multitasking : constant Boolean :=
     System.Soft_Links.Lock_Task /=
       System.Soft_Links.Task_Lock_NT'Access;

   function To_Mixed (A : String) return String;

   procedure Reset (Self : in out LSP.GPR_Files.File);

   package GPC renames Gpr_Parser.Common;
   package Convert is new VSS.Strings.Formatters.Generic_Integers (Unit_Index);
   package Conversions renames VSS.Strings.Conversions;

   function Is_Between
     (Position : LSP.Structures.Position;
      Span     : LSP.Structures.A_Range)
            return Boolean
   is ((Position.line = Span.start.line
       and then Position.character >= Span.start.character)
       or else (Position.line = Span.an_end.line
         and then Position.character <= Span.an_end.character)
       or else (Position.line > Span.start.line
         and then Position.line < Span.an_end.line));
   --  Checks if Position is inside Span

   -----------------
   -- Get_Package --
   -----------------

   function Get_Package
     (Self : File; Position : LSP.Structures.Position) return GPR2.Package_Id
   is
   begin
      for P of Self.Packages loop
         if Is_Between (Position, P.Package_Range) then
            return P.Name;
         end if;
      end loop;
      return GPR2.Project_Level_Scope;
   end Get_Package;

   --------
   -- Id --
   --------

   function Id (List : in out Name_List;
                Name : Optional_Name_Type) return Natural
   is
      C          : Name_Maps.Cursor;
      Result     : Natural;
      Value      : constant String :=
        Ada.Characters.Handling.To_Lower (String (Name));
   begin
      if Name'Length = 0 then
         return 0;
      end if;

      --  Note: if we just read the value, the operation is thread-safe.
      --  So let's not add a penalty for the read operation, that should be
      --  the most common operation.
      C := List.Name_To_Id.Find (Value);

      if Name_Maps.Has_Element (C) then
         return Name_Maps.Element (C);
      end if;

      --  We need to add the value: as this operation is not atomic
      --  and the tables are global, we need to ensure the operation
      --  cannot be interrupted.
      begin
         System.Soft_Links.Lock_Task.all;

         if Is_Multitasking then
            --  In a multitasking environment, the value could have been
            --  inserted by someone else since we've checked it above.
            --  So let's retry:
            C := List.Name_To_Id.Find (Value);

            if Name_Maps.Has_Element (C) then
               --  return with the value
               System.Soft_Links.Unlock_Task.all;
               return Name_Maps.Element (C);
            end if;
         end if;

         --  Still not in there, so let's add the value to the list
         List.Id_To_Name.Append
           (VSS.Strings.Conversions.To_Virtual_String
              (To_Unbounded_String (To_Mixed (String (Name)))));
         Result := List.Id_To_Name.Length;
         List.Name_To_Id.Insert (Value, Result);

      exception
         when others =>
            System.Soft_Links.Unlock_Task.all;
            raise;
      end;

      --  Don't need the lock anymore
      System.Soft_Links.Unlock_Task.all;

      return Result;
   end Id;

   --------------
   -- To_Mixed --
   --------------

   function To_Mixed (A : String) return String is
      use Ada.Characters.Handling;
      Ucase  : Boolean := True;
      Result : String (A'Range);
   begin
      for J in A'Range loop
         if Ucase then
            Result (J) := To_Upper (A (J));
         else
            Result (J) := To_Lower (A (J));
         end if;

         Ucase := A (J) in '_' | '.' | ' ';
      end loop;

      return Result;
   end To_Mixed;

   -----------
   -- Image --
   -----------

   function Image (List : Name_List;
                   Id   : Natural) return VSS.Strings.Virtual_String is
   begin
      if Id = 0 then
         return "";
      end if;

      return List.Id_To_Name.Element (Id);
   end Image;

   package Slocs renames Gpr_Parser_Support.Slocs;

   use Gpr_Parser.Common;
   use Gpr_Parser_Support.Token_Data_Handlers;

   function To_Text_Type
     (Ref : GPC.Token_Reference)
      return Gpr_Parser_Support.Text.Text_Type
   is (GPC.Text (Ref));

   function To_Lower_Text_Type
     (Ref : GPC.Token_Reference) return Gpr_Parser_Support.Text.Text_Type
   is (Gpr_Parser_Support.Text.To_Lower (To_Text_Type (Ref)));

   function To_String (Ref : GPC.Token_Reference) return String
   is (Gpr_Parser_Support.Text.To_UTF8 (GPC.Text (Ref)));

   function To_Optional_Name_Type
     (Ref : GPC.Token_Reference) return Optional_Name_Type
   is
     (Optional_Name_Type
        (Gpr_Parser_Support.Text.To_UTF8 (GPC.Text (Ref))));

   function Remove_Quote (S : String) return String is
     (if S'Length >= 2 and then S (S'First) = '"' and then S (S'Last) = '"'
      then S (S'First + 1 .. S'Last - 1)
      else S);

   function To_Lower_String (Ref : GPC.Token_Reference) return String
   is (Gpr_Parser_Support.Text.To_UTF8 (To_Lower_Text_Type (Ref)));

   procedure Internal_Parse (File : in out LSP.GPR_Files.File);

   function Image (Index : Index_Type) return VSS.Strings.Virtual_String;

   ------------
   -- Create --
   ------------

   function Create
     (Text        : Unbounded_String;
      Q_Attribute : GPR2.Q_Attribute_Id;
      At_Pos      : Unit_Index := GPR2.No_Index
     ) return Index_Type is
      Case_Sensitive : constant Boolean :=
        Is_Case_Sensitive (Text, Q_Attribute);
      VS_Text : constant VSS.Strings.Virtual_String :=
        Conversions.To_Virtual_String (Text);
   begin
      return
        (Text      => VS_Text,
         Is_Others => False,
         Compare   => (if Case_Sensitive
                       then VS_Text & "@" & Convert.Image (At_Pos).Name
                       else VSS.Strings.To_Lowercase
                         (VS_Text & "@" & Convert.Image (At_Pos).Name)),
         At_Pos    => At_Pos);
   end Create;

   -----------
   -- Image --
   -----------

   function Image (Index : Index_Type) return VSS.Strings.Virtual_String is
   begin
      if Index /= No_Index then
         if Index = Others_Index then
            return "(others)";
         else
            if Index.At_Pos /= 0 then
               return "(""" & Index.Text & """ at "
                 & Convert.Image (Index.At_Pos).Name & ")";
            else
               return "(""" & Index.Text & """)";
            end if;
         end if;
      else
         return "";
      end if;
   end Image;

   --------------------
   -- Internal_Parse --
   --------------------

   procedure Internal_Parse
     (File : in out LSP.GPR_Files.File) is

      First        : GPC.Token_Reference;
      Index        : GPC.Token_Reference;
      Last         : GPC.Token_Reference;
      Search_Paths : GPR2.Path_Name.Set.Object :=
        GPR2.Project.Default_Search_Paths
          (False, GPR2.Environment.Process_Environment);
      Project_Name : Ada.Strings.Unbounded.Unbounded_String :=
        Ada.Strings.Unbounded.To_Unbounded_String
          (GNATCOLL.Utils.Replace (String (File.Path.Base_Name),
           "-",
           "."));

      Current_Package    : Package_Definition;

      Current_Symbols : Symbol_List;

      type GPR_Token is record
         Ref   : Gpr_Parser.Common.Token_Reference;
         Token : Token_Data_Type;
         Kind  : Gpr_Parser.Common.Token_Kind;
         Index : TDH.Token_Index := No_Token_Index;
      end record;

      function Get_GPR_Token
        (Ref : Gpr_Parser.Common.Token_Reference) return GPR_Token;
      function Get_Referenced_GPR (Token : GPR_Token) return Path_Name.Object;

      function Is_Defined (Token : GPR_Token) return Boolean is
        (Token.Ref /= GPC.No_Token);

      procedure Load;
      --  Extract tokens using UTF-8 and then CP-1252 encoding.
      --  Diags not empty on error

      procedure Add_Symbol
        (Token    : GPR_Token;
         Kind     : Symbol_Kind;
         Name     : VSS.Strings.Virtual_String;
         Children : GPC.Token_Reference);

      procedure Parse_Declarations;
      procedure Parse_Imported_Partition;
      procedure Parse_Project_Declaration;
      procedure Close_Current_Package (Last_Index : GPC.Token_Reference);

      procedure Add_Symbol
        (Token    : GPR_Token;
         Kind     : Symbol_Kind;
         Name     : VSS.Strings.Virtual_String;
         Children : GPC.Token_Reference) is
         Location_Range : constant Slocs.Source_Location_Range :=
           Gpr_Parser.Common.Sloc_Range (Gpr_Parser.Common.Data (Token.Ref));
         New_Symbol     : constant Symbol :=
           (Token.Ref,
            Kind,
            Name,
            (Integer (Location_Range.Start_Line),
             Integer (Location_Range.Start_Column)),
            (Integer (Location_Range.End_Line),
             Integer (Location_Range.End_Column)),
            Children);
      begin
         Current_Symbols.Append (New_Symbol);
      end Add_Symbol;

      function Next_Token
        (Ref : GPC.Token_Reference) return GPC.Token_Reference
      is (Next (Ref, True));

      function Previous_Token
        (Ref : GPC.Token_Reference) return GPC.Token_Reference
      is (Previous (Ref, True));

      ---------------------------
      -- Close_Current_Package --
      ---------------------------

      procedure Close_Current_Package (Last_Index : GPC.Token_Reference) is
      begin
         if Current_Package.Name /= Project_Level_Scope then
            Current_Package.Last := Last_Index;

            --  Initialize Package_Range

            declare
               Location_Range : Slocs.Source_Location_Range :=
                 Gpr_Parser.Common.Sloc_Range
                   (Gpr_Parser.Common.Data (Current_Package.First));
            begin
               Current_Package.Package_Range.start.line :=
                 To_Position_Value (Location_Range.Start_Line);
               Current_Package.Package_Range.start.character :=
                 To_Position_Value (Location_Range.Start_Column);
               Location_Range := Gpr_Parser.Common.Sloc_Range
                 (Gpr_Parser.Common.Data (Current_Package.Last));
               Current_Package.Package_Range.an_end.line :=
                 To_Position_Value (Location_Range.Start_Line);
               Current_Package.Package_Range.an_end.character :=
                 To_Position_Value (Location_Range.Start_Column);
            end;

            if not File.Packages.Contains (Current_Package.Name) then
               File.Packages.Insert (Current_Package.Name, Current_Package);
            end if;
            if not File.Document_Symbols.Children.Contains
              (Current_Package.First)
            then
               File.Document_Symbols.Children.Insert
                 (Key      => Current_Package.First,
                  New_Item => Current_Symbols);
            end if;
            Current_Package := File.Project_Level_Scope_Defs;
            Current_Symbols := File.Document_Symbols.Document_Symbols;
         else
            File.Document_Symbols.Document_Symbols := Current_Symbols;
         end if;
      end Close_Current_Package;

      -------------------
      -- Get_GPR_Token --
      -------------------

      function Get_GPR_Token (Ref : GPC.Token_Reference) return GPR_Token is
         Token : GPR_Token;
      begin
         Token.Ref := Ref;
         Token.Token := GPC.Data (Ref);
         Token.Kind := GPC.Kind (Token.Token);
         Token.Index := Gpr_Parser.Common.Index (Ref);
         return Token;
      end Get_GPR_Token;

      ------------------------
      -- Get_Referenced_GPR --
      ------------------------

      function Get_Referenced_GPR
        (Token : GPR_Token) return Path_Name.Object is
      begin
         return GPR2.Project.Create
           (GPR2.Filename_Type
              (Remove_Quote (To_String (Token.Ref))),
            Search_Paths);
      end Get_Referenced_GPR;

      ----------
      -- Load --
      ----------

      procedure Load is

      begin
         Reset (File);

         File.Unit := Gpr_Parser.Analysis.Get_From_File
           (Context  => Gpr_Parser.Analysis.Create_Context
              ("UTF-8",
               GPR2.File_Readers.Convert (File.File_Provider.Get_File_Reader)),
            Filename => File.Path.Value);
         if Gpr_Parser.Analysis.Root (File.Unit).Is_Null
           or else Gpr_Parser.Analysis.Has_Diagnostics (File.Unit)
         then
            File.Unit := Gpr_Parser.Analysis.Get_From_File
              (Context  => Gpr_Parser.Analysis.Create_Context
                 ("Windows-1252",
                  GPR2.File_Readers.Convert
                    (File.File_Provider.Get_File_Reader)),
               Filename => File.Path.Value);
         end if;
      end Load;

      ------------------------
      -- Parse_Declarations --
      ------------------------

      procedure Parse_Declarations is
         Token : GPR_Token;

         procedure Parse_Attribute;
         procedure Parse_Type;
         procedure Parse_Variable (Variable_Token : GPR_Token);
         procedure Parse_Package;
         procedure Parse_End;

         ---------------------
         -- Parse_Attribute --
         ---------------------

         procedure Parse_Attribute is
            Attribute_Token : GPR_Token;
            Par_Open  : GPR_Token;
            Index_Token : GPR_Token;
            At_Token : GPR_Token;
            Unit_Token : GPR_Token;
            Name      : Optional_Attribute_Id := No_Attribute;
            Attribute : Attribute_Definition;
            Attribute_Index_Map : Attribute_Index_Maps.Map;
            Attr_Index : Index_Type := No_Index;
            At_Pos     : Unit_Index := GPR2.No_Index;
            Index_Text : Unbounded_String;
            Q_Attribute : Q_Optional_Attribute_Id;
         begin
            Index := Next_Token (Index);
            if Index < Last then
               Attribute_Token := Get_GPR_Token (Index);
               if Attribute_Token.Kind /= Gpr_Parser.Common.Gpr_Identifier
               then
                  return;
               end if;
            else
               return;
            end if;
            Name := +To_Optional_Name_Type (Attribute_Token.Ref);
            Q_Attribute := (Current_Package.Name, Name);
            Index := Next_Token (Index);
            Index_Token.Kind := Gpr_Parser.Common.Gpr_Termination;
            if Next_Token (Index) < Last then
               Par_Open := Get_GPR_Token (Index);
               if Par_Open.Kind = Gpr_Parser.Common.Gpr_Par_Open then
                  Index := Next_Token (Index);
                  Index_Token := Get_GPR_Token (Index);
               end if;
            end if;
            if Index_Token.Kind = Gpr_Parser.Common.Gpr_String then
               Index_Text := To_Unbounded_String
                 (Remove_Quote (To_String (Index_Token.Ref)));
               if Next_Token (Next_Token (Index)) < Last then
                  Index := Next_Token (Index);
                  At_Token := Get_GPR_Token (Index);
                  if At_Token.Kind = Gpr_Parser.Common.Gpr_At then
                     Index := Next_Token (Index);
                     Unit_Token := Get_GPR_Token (Index);
                     if Unit_Token.Kind = Gpr_Parser.Common.Gpr_Number then
                        At_Pos := Unit_Index'Value
                          (To_String (Unit_Token.Ref));
                        Index := Next_Token (Index);
                     end if;
                  end if;
               end if;
               Attr_Index := Create (Index_Text, Q_Attribute, At_Pos);
            end if;
            if Index_Token.Kind = Gpr_Parser.Common.Gpr_Others then
               Attr_Index := Others_Index;
            end if;
            declare
               Cursor : constant Attribute_Maps.Cursor :=
                 Current_Package.Attributes.Find (Name);
            begin
               if Attribute_Maps.Has_Element (Cursor) then
                  Attribute_Index_Map := Attribute_Maps.Element (Cursor);
               end if;
               if not Attribute_Index_Map.Contains (Attr_Index) then
                  Attribute.Index := Attr_Index;
                  Attribute.Token := Attribute_Token.Index;
                  Attribute_Index_Map.Insert (Attr_Index, Attribute);

               end if;
               if Attribute_Maps.Has_Element (Cursor) then
                  Current_Package.Attributes.Replace_Element
                    (Cursor, Attribute_Index_Map);
               else
                  Current_Package.Attributes.Insert
                    (Name, Attribute_Index_Map);
               end if;
               Add_Symbol
                 (Token    => Attribute_Token,
                  Kind     => K_Attribute,
                  Name     => Conversions.To_Virtual_String
                    (To_Unbounded_String (Image (Name))) & Image (Attr_Index),
                  Children => GPC.No_Token);
            end;
         end Parse_Attribute;

         ---------------
         -- Parse_End --
         ---------------

         procedure Parse_End is
            Token : GPR_Token;
         begin
            Index := Next_Token (Index);
            if Index < Last then
               Token := Get_GPR_Token (Index);
               if Token.Kind = Gpr_Parser.Common.Gpr_Identifier then
                  Close_Current_Package (Index);
               end if;
            end if;
         end Parse_End;

         -------------------
         -- Parse_Package --
         -------------------

         procedure Parse_Package is
            Token         : GPR_Token;
            Empty_Package : Package_Definition;

            procedure Handle_Package_Reference;

            ------------------------------
            -- Handle_Package_Reference --
            ------------------------------

            procedure Handle_Package_Reference is
               Referenced_Project : GPR_Token;
               Dot                : GPR_Token;
               Referenced_Package : GPR_Token;
               Index1 : constant GPC.Token_Reference := Next_Token (Index);
               Index2 : constant GPC.Token_Reference := Next_Token (Index1);
               Index3 : constant GPC.Token_Reference := Next_Token (Index2);
            begin
               --  TODO add support of root.child.[...].package_name
               --  Currently only prj.pack or pack is supported.

               if Index3 < Last then
                  Referenced_Project := Get_GPR_Token (Index1);
                  Dot := Get_GPR_Token (Index2);
                  Referenced_Package := Get_GPR_Token (Index3);
                  if Referenced_Project.Kind = Gpr_Parser.Common.Gpr_Identifier
                  then
                     if Dot.Kind = Gpr_Parser.Common.Gpr_Dot then
                        if Referenced_Package.Kind =
                          Gpr_Parser.Common.Gpr_Identifier
                        then
                           Index := Index3;
                           Current_Package.Referenced_Project :=
                             +To_String (Referenced_Project.Ref);
                           Current_Package.Referenced_Package :=
                             +To_Optional_Name_Type (Referenced_Package.Ref);
                        end if;
                     else
                        Index := Index1;
                        Current_Package.Referenced_Project := No_Project;
                        Current_Package.Referenced_Package :=
                          +To_Optional_Name_Type (Referenced_Project.Ref);
                     end if;
                  end if;
               end if;
            end Handle_Package_Reference;

         begin
            Close_Current_Package (Index);
            Index := Next_Token (Index);
            if Index < Last then
               Token := Get_GPR_Token (Index);
               if Token.Kind = Gpr_Parser.Common.Gpr_Identifier then
                  if Current_Package.Name = GPR2.Project_Level_Scope then
                     File.Project_Level_Scope_Defs := Current_Package;
                     Current_Package := Empty_Package;
                     Current_Package.Name :=
                       +To_Optional_Name_Type (Token.Ref);
                     Current_Package.First := Index;
                     Add_Symbol
                       (Token    => Token,
                        Kind     => K_Package,
                        Name     => Conversions.To_Virtual_String
                          (To_Unbounded_String (Image (Current_Package.Name))),
                        Children => Current_Package.First);
                     File.Document_Symbols.Document_Symbols := Current_Symbols;
                     Current_Symbols.Clear;
                  end if;
               else
                  return;
               end if;
            else
               return;
            end if;
            Index := Next_Token (Index);
            if Index < Last then
               Token := Get_GPR_Token (Index);
               case Token.Kind is
                  when Gpr_Parser.Common.Gpr_Renames =>
                     Handle_Package_Reference;
                     if Current_Package.Referenced_Package
                       /= GPR2.Project_Level_Scope
                     then
                        Current_Package.Renaming := True;
                     end if;
                     Close_Current_Package (Index);
                  when Gpr_Parser.Common.Gpr_Extends =>
                     Handle_Package_Reference;
                     if Current_Package.Referenced_Package
                       /= GPR2.Project_Level_Scope
                     then
                        Current_Package.Extending := True;
                     end if;
                  when others =>
                     null;
               end case;
            else
               return;
            end if;
         end Parse_Package;

         ----------------
         -- Parse_Type --
         ----------------

         procedure Parse_Type is
            Name   : Type_Id;
            Token : GPR_Token;
         begin
            Close_Current_Package (Index);
            Index := Next_Token (Index);
            if Index < Last then
               Token := Get_GPR_Token (Index);
               if Token.Kind = Gpr_Parser.Common.Gpr_Identifier then
                  Name := +To_String (Token.Ref);
                  if not File.Types.Contains (Name) then
                     declare
                        Typ : Variable_Type;
                     begin
                        Typ.Name := Name;
                        Typ.Token := Index;
                        File.Types.Insert (Name, Typ);
                        Add_Symbol
                          (Token    => Token,
                           Kind     => K_Type,
                           Name     => Image (Typ.Name),
                           Children => GPC.No_Token);
                     end;
                  end if;
                  Index := Next_Token (Index);
               end if;
            end if;
         end Parse_Type;

         --------------------
         -- Parse_Variable --
         --------------------

         procedure Parse_Variable (Variable_Token : GPR_Token) is
            Token : GPR_Token;
            Name  : constant Variable_Id :=
              +To_String (Variable_Token.Ref);
         begin
            if not Current_Package.Variables.Contains (Name) then
               declare
                  Variable : Variable_Definition;
               begin
                  Variable.Name := Name;
                  Variable.Token := Previous_Token (Index);
                  Current_Package.Variables.Insert (Name, Variable);
                  Add_Symbol
                    (Token    => Variable_Token,
                     Kind     => K_Variable,
                     Name     => Image (Variable.Name),
                     Children => GPC.No_Token);
               end;
            end if;
            while Index < Last loop
               Token := Get_GPR_Token (Index);
               case Token.Kind is
               when Gpr_Parser.Common.Gpr_Colon =>
                  null;
               when Gpr_Parser.Common.Gpr_Identifier =>
                  null;
               when Gpr_Parser.Common.Gpr_Dot =>
                  null;
               when Gpr_Parser.Common.Gpr_Assign =>
                  null;
               when others =>
                  exit;
               end case;
               Index := Next_Token (Index);
            end loop;
         end Parse_Variable;

         Previous_Token : GPR_Token;
      begin
         Previous_Token.Kind := Gpr_Parser.Common.Gpr_Termination;
         while Index < Last loop
            Token := Get_GPR_Token (Index);
            case Token.Kind is
               when Gpr_Parser.Common.Gpr_Package =>
                  Parse_Package;
               when Gpr_Parser.Common.Gpr_End =>
                  Parse_End;
               when Gpr_Parser.Common.Gpr_Type =>
                  Parse_Type;
               when Gpr_Parser.Common.Gpr_For =>
                  Parse_Attribute;
               when Gpr_Parser.Common.Gpr_Colon =>
                  if Previous_Token.Kind = Gpr_Parser.Common.Gpr_Identifier
                  then
                     Parse_Variable (Previous_Token);
                  end if;
               when Gpr_Parser.Common.Gpr_Assign =>
                  if Previous_Token.Kind = Gpr_Parser.Common.Gpr_Identifier
                  then
                     Parse_Variable (Previous_Token);
                  end if;
               when others =>
                  null;
            end case;
            Previous_Token := Token;
            Index := Next_Token (Index);
         end loop;
         Close_Current_Package (Last);
         if Current_Package.Name = GPR2.Project_Level_Scope then
            File.Project_Level_Scope_Defs := Current_Package;
            Current_Symbols := File.Document_Symbols.Document_Symbols;
         end if;
      end Parse_Declarations;

      ------------------------------
      -- Parse_Imported_Partition --
      ------------------------------

      procedure Parse_Imported_Partition is
         Limited_Import    : Boolean := False;
      begin
         while Index < Last loop
            declare
               Token : constant GPR_Token := Get_GPR_Token (Index);
            begin
               case Token.Kind is
                  when Gpr_Parser.Common.Gpr_Limited =>
                     Limited_Import := True;

                  when Gpr_Parser.Common.Gpr_With =>
                     null;

                  when Gpr_Parser.Common.Gpr_String =>
                     declare
                        Path     : constant GPR2.Path_Name.Object :=
                          Get_Referenced_GPR (Token);
                        Imported : File_Access;
                     begin
                        if Path.Exists then
                           File.Token_To_File_Map.Insert (Index, Path);
                           Imported := Parse (File.File_Provider, Path);

                           File.Name_To_File_Map.Insert (Imported.Name, Path);
                           File.Limited_Imported.Append (Imported.Name);
                           if not Limited_Import then
                              File.Imported.Append (Imported.Name);
                              Add_Symbol
                                (Token    => Token,
                                 Kind     => K_Imported,
                                 Name     => Image (Imported.Name),
                                 Children => GPC.No_Token);
                           else
                              Add_Symbol
                                (Token    => Token,
                                 Kind     => K_Imported,
                                 Name     => "limited " &
                                               Image (Imported.Name),
                                 Children => GPC.No_Token);
                           end if;
                        end if;
                     end;

                  when Gpr_Parser.Common.Gpr_Comma =>
                     null;

                  when Gpr_Parser.Common.Gpr_Semicolon =>
                     Limited_Import := False;

                  when others =>
                     exit;
               end case;
            end;
            Index := Next_Token (Index);
         end loop;
      end Parse_Imported_Partition;

      -------------------------------
      -- Parse_Project_Declaration --
      -------------------------------

      procedure Parse_Project_Declaration is
         Extends : Boolean := False;
         Extends_All : Boolean := False;
         Extended_Path : Path_Name.Object;
         Extended_Index : GPC.Token_Reference;
         Name : Unbounded_String;
         Project_Token : GPR_Token;
      begin
         while Index < Last loop
            declare
               Token : constant GPR_Token := Get_GPR_Token (Index);
            begin
               case Token.Kind is
                  when Gpr_Parser.Common.Gpr_Identifier =>
                     declare
                        Identifier : constant String :=
                          To_Lower_String (Token.Ref);
                     begin
                        if Identifier = "project" then
                           Project_Token := Token;
                        elsif Identifier = "library" then
                           if File.Kind = GPR2.K_Aggregate then
                              File.Kind := GPR2.K_Aggregate_Library;
                           else
                              File.Kind := GPR2.K_Library;
                           end if;
                        elsif Identifier = "aggregate" then
                           File.Kind := GPR2.K_Aggregate;
                        elsif Identifier = "configuration" then
                           File.Kind := GPR2.K_Configuration;
                        elsif Identifier = "standard" then
                           null;
                        else
                           Ada.Strings.Unbounded.Append
                             (Name, To_String (Token.Ref));
                        end if;
                     end;
                  when Gpr_Parser.Common.Gpr_Abstract =>
                     File.Kind := GPR2.K_Abstract;
                  when Gpr_Parser.Common.Gpr_Dot =>
                     Ada.Strings.Unbounded.Append (Name, ".");
                  when Gpr_Parser.Common.Gpr_Extends =>
                     Extends := True;
                  when Gpr_Parser.Common.Gpr_All =>
                     Extends_All := True;
                  when Gpr_Parser.Common.Gpr_String =>
                     Extended_Index := Index;
                     Extended_Path := Get_Referenced_GPR (Token);
                  when Gpr_Parser.Common.Gpr_Is =>
                     Index := Next_Token (Index);
                     exit;
                     when others =>
                     exit;
               end case;
               Index := Next_Token (Index);
            end;
         end loop;
         Project_Name := Name;
         if Extends and then Extended_Path.Exists then
            declare
               Extended : File_Access;
            begin
               File.Token_To_File_Map.Insert (Extended_Index, Extended_Path);
               Extended := Parse (File.File_Provider, Extended_Path);
               File.Extended := Extended.Name;
               File.Extended_All := Extends_All;
               File.Extended_Path := Extended_Path;
            end;
         end if;

         if Is_Defined (Project_Token) then
            Add_Symbol
              (Token    => Project_Token,
               Kind     => K_Project,
               Name     => Conversions.To_Virtual_String
                             (Project_Name),
               Children => GPC.No_Token);
         end if;
      end Parse_Project_Declaration;

   begin

      Search_Paths.Prepend (File.Path.Containing_Directory);
      File.Name := +To_String (Project_Name);

      Load;

      File.Parsed := True;

      First := File.Unit.First_Token;
      if GPC.Is_Trivia (First) then
         --  skip leading trivias
         First := Next_Token (First);
      end if;

      Last := File.Unit.Last_Token;
      if GPC.Is_Trivia (Last) then
         --  skip trailing trivias
         Last := Previous_Token (Last);
      end if;

      Index := First;

      Parse_Imported_Partition;
      Parse_Project_Declaration;

      --  Update name as guessed name from filename can be different.
      File.Name := +To_String (Project_Name);
      Parse_Declarations;
      Close_Current_Package (Last);

   end Internal_Parse;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self          : in out File;
      Path          : GPR2.Path_Name.Object;
      Tab_Stop      : Positive := Gpr_Parser_Support.Slocs.Default_Tab_Stop;
      File_Provider : File_Provider_Access) is
   begin
      Self.Path := Path;
      Self.Tab_Stop := Tab_Stop;
      Self.File_Provider := File_Provider;
   end Initialize;

   -----------
   -- Reset --
   -----------

   procedure Reset (Self : in out File) is
   begin
      if Self.Parsed then
         Self.Parsed := False;

         Self.Token_To_File_Map.Clear;
         Self.Name_To_File_Map.Clear;
         Self.Imported.Clear;
         Self.Limited_Imported.Clear;
         Self.Extended := No_Project;
         Self.Extended_Path := GPR2.Path_Name.Undefined;
         Self.Extended_All := False;
         Self.Types.Clear;
         Self.Project_Level_Scope_Defs := Empty_Package_Definition;
         Self.Packages.Clear;
         Self.Document_Symbols.Document_Symbols.Clear;
         Self.Document_Symbols.Children.Clear;

         Self.Unit := Gpr_Parser.Analysis.No_Analysis_Unit;
      end if;

   end Reset;

   -----------
   -- Parse --
   -----------

   function Parse
     (File_Provider : File_Provider_Access;
      Path : GPR2.Path_Name.Object) return File_Access is
      File : constant File_Access := File_Provider.Get_Parsed_File (Path);
   begin
      if not File.Parsed then
         Internal_Parse (File.all);
      end if;
      return File;
   end Parse;

   -----------------------------
   -- Parse_Modified_Document --
   -----------------------------

   procedure Parse_Modified_Document
     (File_Provider : File_Provider_Access;
      Path          : GPR2.Path_Name.Object) is
      File : constant File_Access := File_Provider.Get_Parsed_File (Path);
   begin
      if File.Parsed then
         Reset (File.all);
      end if;
      Internal_Parse (File.all);
   end Parse_Modified_Document;

   -------------
   -- Cleanup --
   -------------

   procedure Cleanup (Self : in out File) is
   begin
      if Self.Parsed then
         Reset (Self);
      end if;
   end Cleanup;

   -----------
   -- Token --
   -----------

   function Token
     (Self     : File;
      Position : LSP.Structures.Position)
      return Gpr_Parser.Common.Token_Reference is
      Sloc : constant Source_Location :=
        (To_Line_Number (Position.line),
         To_Column_Number (Position.character));

   begin
      return Self.Unit.Lookup_Token (Sloc);

   end Token;

end LSP.GPR_Files;
