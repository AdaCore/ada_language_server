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

with VSS.String_Vectors;
with VSS.Strings;

with LSP_Gen.Entities;
with LSP_Gen.Mappings;
with LSP_Gen.Puts; use LSP_Gen.Puts;
with LSP_Gen.String_Sets;

package body LSP_Gen.Inputs is

   use type VSS.Strings.Virtual_String;
   use all type LSP_Gen.Entities.Enum.AType_Variant;
   use all type LSP_Gen.Entities.Enum.BaseTypes;

   procedure Write_Subprogram_Definition
     (Name   : VSS.Strings.Virtual_String;
      Prefix : VSS.Strings.Virtual_String);

   procedure Write_Enum
     (Info : LSP_Gen.Entities.Enumeration;
      Spec : Boolean);

   function Has_Nested_Types
     (Done  : LSP_Gen.Dependencies.Dependency_Map;
      Name  : VSS.Strings.Virtual_String) return Boolean is
        (for some Cursor in Done.Iterate => Done (Cursor).Owner = Name);

   procedure Write_Nested_Types
     (Model : LSP_Gen.Meta_Models.Meta_Model;
      Done  : LSP_Gen.Dependencies.Dependency_Map;
      Name  : VSS.Strings.Virtual_String);

   procedure Write_Nested_Types_Maps
     (Model : LSP_Gen.Meta_Models.Meta_Model;
      Done  : LSP_Gen.Dependencies.Dependency_Map;
      Name  : VSS.Strings.Virtual_String);

   procedure Write_Type
     (Model : LSP_Gen.Meta_Models.Meta_Model;
      Done  : LSP_Gen.Dependencies.Dependency_Map;
      Name  : VSS.Strings.Virtual_String;
      Tipe  : LSP_Gen.Entities.AType;
      Spec  : Boolean);

   function Get_Type_Map
     (Model : LSP_Gen.Meta_Models.Meta_Model;
      Tipe  : LSP_Gen.Entities.AType)
      return VSS.String_Vectors.Virtual_String_Vector;

   procedure Write_Type_Map
     (Model : LSP_Gen.Meta_Models.Meta_Model;
      Done  : LSP_Gen.Dependencies.Dependency_Map;
      Tipe  : LSP_Gen.Entities.AType;
      Name  : VSS.Strings.Virtual_String);

   procedure Write_Structure
     (Model : LSP_Gen.Meta_Models.Meta_Model;
      Done  : LSP_Gen.Dependencies.Dependency_Map;
      Name  : VSS.Strings.Virtual_String);

   procedure Write_Call
     (Done   : LSP_Gen.Dependencies.Dependency_Map;
      Tipe   : LSP_Gen.Entities.AType;
      Suffix : VSS.Strings.Virtual_String);

   procedure Write_Subprogram_Name
     (Done : LSP_Gen.Dependencies.Dependency_Map;
      Tipe : LSP_Gen.Entities.AType);

   function All_Properties
     (List : LSP_Gen.Entities.Property_Vector)
     return VSS.String_Vectors.Virtual_String_Vector;

   function All_Properties
     (Model : LSP_Gen.Meta_Models.Meta_Model;
      Tipe  : LSP_Gen.Entities.AType)
      return VSS.String_Vectors.Virtual_String_Vector;

   function All_Properties
     (Model : LSP_Gen.Meta_Models.Meta_Model;
      Tipe  : LSP_Gen.Entities.Structure)
      return VSS.String_Vectors.Virtual_String_Vector;

   Empty_Vector : LSP_Gen.Entities.Property_Vector;

   procedure Write_Properties
     (Model       : LSP_Gen.Meta_Models.Meta_Model;
      Done        : LSP_Gen.Dependencies.Dependency_Map;
      Name_List   : VSS.String_Vectors.Virtual_String_Vector;
      List        : LSP_Gen.Entities.Property_Vector;
      Optional    : VSS.String_Vectors.Virtual_String_Vector :=
        VSS.String_Vectors.Empty_Virtual_String_Vector;
      Suffix      : VSS.Strings.Virtual_String := "";
      Exclude     : LSP_Gen.Entities.Property_Vector := Empty_Vector);

   procedure Write_Property
     (Model       : LSP_Gen.Meta_Models.Meta_Model;
      Done        : LSP_Gen.Dependencies.Dependency_Map;
      Name_List   : VSS.String_Vectors.Virtual_String_Vector;
      Item        : LSP_Gen.Entities.Property;
      Is_Optional : Boolean := False;
      Suffix      : VSS.Strings.Virtual_String := "");

   procedure Write_Look_Ahead
     (Model : LSP_Gen.Meta_Models.Meta_Model;
      Name  : VSS.Strings.Virtual_String;
      List  : LSP_Gen.Entities.AType_Vector;
      Match : access procedure (Index : Positive));

   procedure Write_Unknown_Mapping
     (Model : LSP_Gen.Meta_Models.Meta_Model;
      Done  : LSP_Gen.Dependencies.Dependency_Map;
      Name  : VSS.Strings.Virtual_String;
      List  : LSP_Gen.Entities.AType_Vector);

   procedure Write_Two_Types
     (Model : LSP_Gen.Meta_Models.Meta_Model;
      Done  : LSP_Gen.Dependencies.Dependency_Map;
      Name  : VSS.Strings.Virtual_String;
      Map   : LSP_Gen.Mappings.Or_Mapping;
      List  : LSP_Gen.Entities.AType_Vector);

   procedure Write_Type_Or_Something
     (Model : LSP_Gen.Meta_Models.Meta_Model;
      Done  : LSP_Gen.Dependencies.Dependency_Map;
      Name  : VSS.Strings.Virtual_String;
      Map   : LSP_Gen.Mappings.Or_Mapping;
      List  : LSP_Gen.Entities.AType_Vector;
      Tipe  : LSP_Gen.Entities.AType);

   procedure Write_Literal_Type
     (Model       : LSP_Gen.Meta_Models.Meta_Model;
      Done        : LSP_Gen.Dependencies.Dependency_Map;
      Name        : VSS.Strings.Virtual_String;
      Scope       : VSS.Strings.Virtual_String;
      Name_List   : VSS.String_Vectors.Virtual_String_Vector;
      Tipe        : LSP_Gen.Entities.AType;
      Optional    : VSS.String_Vectors.Virtual_String_Vector :=
        VSS.String_Vectors.Empty_Virtual_String_Vector);

   procedure Write_Union
     (Model : LSP_Gen.Meta_Models.Meta_Model;
      Done  : LSP_Gen.Dependencies.Dependency_Map;
      Name  : VSS.Strings.Virtual_String;
      List  : LSP_Gen.Entities.AType_Vector);

   type String_Vector_Array is array (Positive range <>) of
     VSS.String_Vectors.Virtual_String_Vector;

   procedure Split_Properties
     (Model   : LSP_Gen.Meta_Models.Meta_Model;
      List    : LSP_Gen.Entities.AType_Vector;
      Result  : out String_Vector_Array;
      Overlap : out LSP_Gen.String_Sets.Set);
   --  Populate Result (J) with property names of List (J). Fill Overlap with
   --  non-unique property names.

   procedure Split_Vector_Properties
     (Model   : LSP_Gen.Meta_Models.Meta_Model;
      List    : LSP_Gen.Entities.AType_Vector;
      Result  : out String_Vector_Array;
      Overlap : out LSP_Gen.String_Sets.Set);
   --  THe same, but List could contain arrays

   function Get_Variants
     (Model : LSP_Gen.Meta_Models.Meta_Model;
      List  : LSP_Gen.Entities.AType_Vector)
      return VSS.String_Vectors.Virtual_String_Vector;

   function Find_First
     (List : VSS.String_Vectors.Virtual_String_Vector;
      Item : VSS.Strings.Virtual_String) return Natural;

   function Is_String (Tipe : LSP_Gen.Entities.AType) return Boolean is
     (Tipe.Union.Kind = base
        and then Tipe.Union.base.name = LSP_Gen.Entities.Enum.string);

   function Is_Vector (Tipe : LSP_Gen.Entities.AType) return Boolean is
     (Tipe.Union.Kind = an_array);

   function Is_LSP_Any (Tipe : LSP_Gen.Entities.AType) return Boolean is
     (Tipe.Union.Kind = reference and then
        Tipe.Union.reference.name = "LSPAny");

   function Is_Symbol_Vector (Tipe : LSP_Gen.Entities.AType) return Boolean is
     (Tipe.Union.Kind = an_array and then
      Tipe.Union.an_array.element.Value.Union.Kind = reference and then
      Tipe.Union.an_array.element.Value.Union.reference.name
        = "DocumentSymbol");

   function Is_Set
     (Model : LSP_Gen.Meta_Models.Meta_Model;
      Tipe  : LSP_Gen.Entities.AType) return Boolean is
     (Tipe.Union.Kind = an_array
      and then Tipe.Union.an_array.element.Value.Union.Kind = reference
      and then Model.Is_Enumeration
        (Tipe.Union.an_array.element.Value.Union.reference.name)
      and then not Tipe.Union.an_array.element.Value.Union.reference.name
       .Starts_With ("MarkupKind"));

   function Has_Scope
     (Model : LSP_Gen.Meta_Models.Meta_Model;
      Map : LSP_Gen.Mappings.Or_Mapping) return Boolean is
     (case Map.Kind is
         when LSP_Gen.Mappings.Type_Union |
           LSP_Gen.Mappings.Unknown_Mapping |
           LSP_Gen.Mappings.Type_Or_Something |
           LSP_Gen.Mappings.Option_Combination |
           LSP_Gen.Mappings.Two_Literals |
           LSP_Gen.Mappings.Enumeration
             => True,
         when LSP_Gen.Mappings.Two_Types  =>
            not Model.Is_Base_Type (Map.First) and
            not Model.Is_Base_Type (Map.Second),
         when others => False);

   function Has_Scope
     (Model : LSP_Gen.Meta_Models.Meta_Model;
      Tipe  : LSP_Gen.Entities.AType) return Boolean
   is
      (Tipe.Union.Kind = a_or
       and then Has_Scope
         (Model,
          LSP_Gen.Mappings.Get_Or_Mapping (Model, Tipe.Union.a_or.items)));

   function Get_Base_Type
     (Name : LSP_Gen.Entities.Enum.BaseTypes)
       return VSS.Strings.Virtual_String
   is
     (case Name is
         when LSP_Gen.Entities.Enum.integer | uinteger =>
            VSS.Strings.Virtual_String'("Number"),
         when Uri | DocumentUri | RegExp
           | LSP_Gen.Entities.Enum.string =>
              VSS.Strings.Virtual_String'("String"),
         when a_boolean =>
            VSS.Strings.Virtual_String'("Boolean"),
         when others => raise Program_Error);

   function Contains
     (List : LSP_Gen.Entities.Property_Vector;
      Name : VSS.Strings.Virtual_String) return Boolean is
        (for some J in 1 .. List.Length => List (J).name = Name);

   --------------------
   -- All_Properties --
   --------------------

   function All_Properties
     (List : LSP_Gen.Entities.Property_Vector)
      return VSS.String_Vectors.Virtual_String_Vector is
   begin
      return Result : VSS.String_Vectors.Virtual_String_Vector do
         for J in 1 .. List.Length loop
            Result.Append (List (J).name);
         end loop;
      end return;
   end All_Properties;

   --------------------
   -- All_Properties --
   --------------------

   function All_Properties
     (Model : LSP_Gen.Meta_Models.Meta_Model;
      Tipe  : LSP_Gen.Entities.AType)
      return VSS.String_Vectors.Virtual_String_Vector is
   begin
      case Tipe.Union.Kind is
         when LSP_Gen.Entities.Enum.base =>
            return VSS.String_Vectors.Empty_Virtual_String_Vector;
         when LSP_Gen.Entities.Enum.reference =>
            declare
               Ref : constant LSP_Gen.Meta_Models.Top_Type :=
                 Model.Get (Tipe.Union.reference.name);
            begin
               case Ref.Kind is
                  when LSP_Gen.Meta_Models.Enumeration =>
                     return VSS.String_Vectors.Empty_Virtual_String_Vector;
                  when LSP_Gen.Meta_Models.Structure =>
                     return All_Properties (Model, Ref.Structure);
                  when LSP_Gen.Meta_Models.Type_Alias =>
                     return All_Properties (Model, Ref.Type_Alias.a_type);
               end case;
            end;
         when LSP_Gen.Entities.Enum.an_array =>
            return VSS.String_Vectors.Empty_Virtual_String_Vector;
         when LSP_Gen.Entities.Enum.map =>
            raise Program_Error;
         when LSP_Gen.Entities.Enum.an_and =>
            raise Program_Error;
         when LSP_Gen.Entities.Enum.a_or =>
            return Result : VSS.String_Vectors.Virtual_String_Vector do
               for J in 1 .. Tipe.Union.a_or.items.Length loop
                  Result.Append
                    (All_Properties (Model, Tipe.Union.a_or.items (J)));
               end loop;
            end return;

         when LSP_Gen.Entities.Enum.tuple =>
            raise Program_Error;
         when LSP_Gen.Entities.Enum.literal =>
            return All_Properties (Tipe.Union.literal.value.properties);
         when LSP_Gen.Entities.Enum.stringLiteral =>
            raise Program_Error;
         when LSP_Gen.Entities.Enum.integerLiteral =>
            raise Program_Error;
         when LSP_Gen.Entities.Enum.booleanLiteral =>
            raise Program_Error;
      end case;
   end All_Properties;

   --------------------
   -- All_Properties --
   --------------------

   function All_Properties
     (Model : LSP_Gen.Meta_Models.Meta_Model;
      Tipe  : LSP_Gen.Entities.Structure)
      return VSS.String_Vectors.Virtual_String_Vector is
   begin
      return Result : VSS.String_Vectors.Virtual_String_Vector :=
        All_Properties (Tipe.properties)
      do
         for J in 1 .. Tipe.extends.Length loop
            Result.Append (All_Properties (Model, Tipe.extends (J)));
         end loop;

         for J in 1 .. Tipe.mixins.Length loop
            Result.Append (All_Properties (Model, Tipe.mixins (J)));
         end loop;
      end return;
   end All_Properties;

   ----------------
   -- Find_First --
   ----------------

   function Find_First
     (List : VSS.String_Vectors.Virtual_String_Vector;
      Item : VSS.Strings.Virtual_String) return Natural is
   begin
      for J in 1 .. List.Length loop
         if List (J) = Item then
            return J;
         end if;
      end loop;

      return 0;
   end Find_First;

   ------------------
   -- Get_Type_Map --
   ------------------

   function Get_Type_Map
     (Model : LSP_Gen.Meta_Models.Meta_Model;
      Tipe  : LSP_Gen.Entities.AType)
      return VSS.String_Vectors.Virtual_String_Vector
   is
      Result : VSS.String_Vectors.Virtual_String_Vector;
   begin
      case Tipe.Union.Kind is
         when literal =>
            Result := All_Properties (Tipe.Union.literal.value.properties);

         when a_or =>
            declare
               use all type LSP_Gen.Mappings.Or_Mapping_Kind;

               Map : constant LSP_Gen.Mappings.Or_Mapping :=
                 LSP_Gen.Mappings.Get_Or_Mapping
                   (Model, Tipe.Union.a_or.items);
            begin
               case Map.Kind is
                  when Unknown_Mapping =>
                     declare
                        List  : LSP_Gen.Entities.AType_Vector renames
                          Tipe.Union.a_or.items;
                        Over   : LSP_Gen.String_Sets.Set;
                        Prop   : String_Vector_Array (1 .. List.Length);
                     begin
                        if Is_Vector (List (1)) and Is_Vector (List (2)) then
                           Split_Vector_Properties
                             (Model, Tipe.Union.a_or.items, Prop, Over);
                        else
                           Split_Properties
                             (Model, Tipe.Union.a_or.items, Prop, Over);
                        end if;

                        for List of Prop loop
                           for Item of List loop
                              if not Over.Contains (Item) then
                                 Result.Append (Item);
                              end if;
                           end loop;
                        end loop;
                     end;

                  when Two_Types =>
                     if Has_Scope (Model, Map) then
                        declare
                           Over  : LSP_Gen.String_Sets.Set;
                           Prop  : String_Vector_Array (1 .. 2);
                        begin
                           Split_Properties
                             (Model, Tipe.Union.a_or.items, Prop, Over);

                           for List of Prop loop
                              for Item of List loop
                                 if not Over.Contains (Item) then
                                    Result.Append (Item);
                                 end if;
                              end loop;
                           end loop;
                        end;
                     end if;

                  when Type_Or_Something =>
                     declare
                        Over  : LSP_Gen.String_Sets.Set;
                        Last  : constant Positive range 1 .. 2 :=
                          (if Model.Is_Base_Type (Map.First) then 1 else 2);
                        Prop  : String_Vector_Array (1 .. Last);
                     begin
                        if Last = 1 then
                           Prop (1) := All_Properties
                             (Map.Second.Union.literal.value.properties);
                        else
                           Split_Properties
                             (Model, Tipe.Union.a_or.items, Prop, Over);
                        end if;

                        for List of Prop loop
                           for Item of List loop
                              if not Over.Contains (Item) then
                                 Result.Append (Item);
                              end if;
                           end loop;

                           Over.Clear;  --  Emit all props of List(2)
                        end loop;
                     end;

                  when Option_Combination =>
                     Result := All_Properties (Model, Map.Tipe);

                  when Two_Literals =>
                     Result := All_Properties (Model, Map.Second);

                  when Enumeration =>
                     declare
                        List : LSP_Gen.Entities.AType_Vector renames
                          Map.Items;
                     begin
                        for J in 1 .. List.Length loop
                           Result.Append (List (J).Union.stringLiteral.value);
                        end loop;
                     end;

                  when Type_Union =>
                     declare
                        Variants : constant
                          VSS.String_Vectors.Virtual_String_Vector :=
                            Get_Variants (Model, Map.Items);
                     begin
                        for J in 1 .. Variants.Length loop
                           Result.Append (Variants (J));
                        end loop;
                     end;
                  when others =>
                     null;
               end case;
            end;

         when others =>
            null;
      end case;

      return Result;
   end Get_Type_Map;

   ------------------
   -- Get_Variants --
   ------------------

   function Get_Variants
     (Model : LSP_Gen.Meta_Models.Meta_Model;
      List  : LSP_Gen.Entities.AType_Vector)
      return VSS.String_Vectors.Virtual_String_Vector is
   begin
      return Variants : VSS.String_Vectors.Virtual_String_Vector do
         for J in 1 .. List.Length loop
            declare
               Item : constant VSS.Strings.Virtual_String :=
                 Model.Get_Variant (List (J), J);
            begin
               if not Item.Starts_With ("Variant") then
                  Variants.Append (Item);
               end if;
            end;
         end loop;
      end return;
   end Get_Variants;

   ----------------------
   -- Split_Properties --
   ----------------------

   procedure Split_Properties
     (Model   : LSP_Gen.Meta_Models.Meta_Model;
      List    : LSP_Gen.Entities.AType_Vector;
      Result  : out String_Vector_Array;
      Overlap : out LSP_Gen.String_Sets.Set) is
   begin
      for J in Result'Range loop
         Result (J) := All_Properties (Model, List (J));
      end loop;

      declare
         Seen : LSP_Gen.String_Sets.Set;
      begin
         for List of Result loop
            for Item of List loop
               if Seen.Contains (Item) then
                  Overlap.Include (Item);
               else
                  Seen.Insert (Item);
               end if;
            end loop;
         end loop;
      end;
   end Split_Properties;

   -----------------------------
   -- Split_Vector_Properties --
   -----------------------------

   procedure Split_Vector_Properties
     (Model   : LSP_Gen.Meta_Models.Meta_Model;
      List    : LSP_Gen.Entities.AType_Vector;
      Result  : out String_Vector_Array;
      Overlap : out LSP_Gen.String_Sets.Set) is
   begin
      for J in Result'Range loop
         if Is_Vector (List (J)) then
            Result (J) := All_Properties
              (Model, List (J).Union.an_array.element.Value);
         else
            Result (J) := All_Properties (Model, List (J));
         end if;
      end loop;

      declare
         Seen : LSP_Gen.String_Sets.Set;
      begin
         for List of Result loop
            for Item of List loop
               if Seen.Contains (Item) then
                  Overlap.Include (Item);
               else
                  Seen.Insert (Item);
               end if;
            end loop;
         end loop;
      end;
   end Split_Vector_Properties;

   -----------
   -- Write --
   -----------

   procedure Write
     (Model : LSP_Gen.Meta_Models.Meta_Model;
      Done  : LSP_Gen.Dependencies.Dependency_Map) is
   begin
      Put_Lines (Model.License_Header, "--  ");
      New_Line;
      Put_Line ("with VSS.JSON.Pull_Readers;");
      New_Line;
      Put_Line ("with LSP.Enumerations;");
      Put_Line ("with LSP.Structures;");
      New_Line;
      Put_Line ("package LSP.Inputs is");
      --  Put_Line ("   pragma Preelaborate;"); New_Line;

      for Item of Done loop
         if Item.Is_Message
           or else Item.Short_Name = "ConfigurationParams"
           or else Item.Short_Name = "Integer_Or_Virtual_String"
         then
            Write_Subprogram_Definition
              (Item.Short_Name,
               (if Model.Is_Enumeration (Item.Short_Name)
                then VSS.Strings.To_Virtual_String ("LSP.Enumerations.")
                else VSS.Strings.To_Virtual_String ("LSP.Structures.")));

            Put_Line (";"); New_Line;
         end if;
      end loop;

      Put_Line ("end LSP.Inputs;");

      Put_Lines (Model.License_Header, "--  ");
      New_Line;
      Put_Line ("pragma Ada_2022;");
      Put_Line ("with Interfaces;");
      Put_Line ("with LSP.Input_Tools;");
      Put_Line ("with VSS.Strings;");
      Put_Line ("with VSS.JSON.Pull_Readers.Buffered;");
      Put_Line ("with Minimal_Perfect_Hash;");
      New_Line;

      Put_Line ("package body LSP.Inputs is"); New_Line;
      Put_Line ("pragma Warnings (Off, ""is not referenced"");");
      Put_Line ("use type Interfaces.Integer_64;"); New_Line;

      for Cursor in Done.Iterate loop
         declare
            Tipe : constant LSP_Gen.Entities.AType :=
               LSP_Gen.Dependencies.Dependency_Maps.Key (Cursor);
            Item : constant LSP_Gen.Dependencies.Dependency_Info :=
              Done (Cursor);
         begin
            if not Item.Is_Message
              and then Item.Short_Name /= "ConfigurationParams"
              and then Item.Short_Name /= "Integer_Or_Virtual_String"
              and then Item.Owner.Is_Empty
              and then (Tipe.Union.Kind /= reference or else
                         not Model.Is_Mixin (Tipe.Union.reference.name))
            then
               Write_Type (Model, Done, Item.Short_Name, Tipe, Spec => True);
            end if;
         end;
      end loop;

      for Cursor in Done.Iterate loop
         declare
            Tipe : constant LSP_Gen.Entities.AType :=
               LSP_Gen.Dependencies.Dependency_Maps.Key (Cursor);
            Item : constant LSP_Gen.Dependencies.Dependency_Info :=
              Done (Cursor);
         begin
            if Tipe.Union.Kind = reference then
               declare
                  Value : constant LSP_Gen.Meta_Models.Top_Type :=
                    Model.Get (Tipe.Union.reference.name);
               begin
                  case Value.Kind is
                     when LSP_Gen.Meta_Models.Structure =>
                        if not Model.Is_Mixin (Tipe.Union.reference.name) then
                           Write_Type_Map (Model, Done, Tipe, Item.Short_Name);
                           Write_Structure
                             (Model, Done, Tipe.Union.reference.name);
                        end if;
                     when LSP_Gen.Meta_Models.Type_Alias =>
                        Write_Type_Map
                          (Model,
                           Done,
                           Value.Type_Alias.a_type,
                           Item.Short_Name);

                        Write_Type
                          (Model, Done, Tipe.Union.reference.name,
                           Value.Type_Alias.a_type, Spec => False);
                     when LSP_Gen.Meta_Models.Enumeration =>
                        Write_Enum (Value.Enumeration, Spec => False);
                  end case;
               end;
            elsif Item.Owner.Is_Empty then
               Write_Type_Map (Model, Done, Tipe, Item.Short_Name);
               Write_Type (Model, Done, Item.Short_Name, Tipe, Spec => False);
            end if;
         end;
      end loop;

      Put_Line ("end LSP.Inputs;");
   end Write;

   ----------------
   -- Write_Call --
   ----------------

   procedure Write_Call
     (Done   : LSP_Gen.Dependencies.Dependency_Map;
      Tipe   : LSP_Gen.Entities.AType;
      Suffix : VSS.Strings.Virtual_String)
   is
      Value : constant VSS.Strings.Virtual_String := "Value" & Suffix;
   begin
      case Tipe.Union.Kind is
         when base =>
            case Tipe.Union.base.name is
               when LSP_Gen.Entities.Enum.integer | uinteger =>
                  Put (Value);
                  Put (" := Integer (Handler.Number_Value.Integer_Value);");
                  New_Line;
               when Uri | DocumentUri | RegExp =>
                  Put (Value);
                  Put_Line (" := (Handler.String_Value with null record);");
               when LSP_Gen.Entities.Enum.string =>
                     --  it's hard to recognize typeAliases from string, so
                     --  let's generate universal code for them.
                  Put (Value);
                  Put_Line (".Clear;");
                  Put (Value);
                  Put_Line (".Append (Handler.String_Value);");
               when a_boolean =>
                  Put (Value);
                  Put_Line (" := Handler.Boolean_Value;");
               when decimal =>
                  Put (Value);
                  Put_Line (" := Float (Handler.Number_Value.Float_Value);");
               when a_null =>
                  Put_Line ("null;  --  #null_value");
            end case;
            Put_Line ("Handler.Read_Next;");

         when reference | literal | a_or | an_array | tuple =>
            Write_Subprogram_Name (Done, Tipe);
            Put (" (Handler, ");
            Put (Value);
            Put_Line (");");

         when stringLiteral =>
            Put ("Handler.Read_Next;  --  Skip string literal: ");
            Put_Line (Tipe.Union.stringLiteral.value);

         when map =>
            Put_Line ("pragma Assert (Handler.Is_Start_Object);");
            Put_Line ("Handler.Read_Next;");
            New_Line;
            Put_Line ("while not Handler.Is_End_Object loop");
            Put_Line ("declare");
            Put ("Map : ");
            Put (Done (Tipe).Full_Name);
            Put (" renames ");
            Put (Value);
            Put_Line (";");

            Put ("Key : LSP.Structures.");
            case Tipe.Union.map.key.Union.Kind is
               when LSP_Gen.Entities.Enum.base =>
                  Put ("DocumentUri");
               when LSP_Gen.Entities.Enum.reference =>
                  Put_Id (Tipe.Union.map.key.Union.reference.name);
            end case;

            Put_Line (";");
            Put ("Value : ");
            Put (Done (Tipe.Union.map.value.Value).Full_Name);
            Put_Line (";");
            Put_Line ("begin");
            Put_Line ("Key := (Handler.Key_Name with null record);");
            Put_Line ("Handler.Read_Next;");
            Write_Call (Done, Tipe.Union.map.value.Value, "");
            Put_Line ("Map.Insert (Key, Value);");
            Put_Line ("end;");
            Put_Line ("end loop;");
            New_Line;
            Put_Line ("Handler.Read_Next;");
            New_Line;

         when others =>
            raise Program_Error;
      end case;
   end Write_Call;

   ----------------
   -- Write_Enum --
   ----------------

   procedure Write_Enum
     (Info : LSP_Gen.Entities.Enumeration;
      Spec : Boolean)
   is
      use type LSP_Gen.Entities.Enum.EnumerationType_name;

      Name : constant VSS.Strings.Virtual_String := Info.name;
   begin
      if not Spec and Info.a_type.name = LSP_Gen.Entities.Enum.string then
         Put ("package ");
         Put_Id (Name);
         Put ("_Map is new Minimal_Perfect_Hash ([");

         for J in 1 .. Info.values.Length loop
            if J > 1 then
               Put (", ");
            end if;

            Put ("""");
            Put (Info.values (J).value.String);
            Put_Line ("""");
         end loop;
         Put_Line ("]);");
         New_Line;
      end if;

      Write_Subprogram_Definition (Name, "LSP.Enumerations.");

      if Spec then
         Put_Line (";");
      elsif Info.supportsCustomValues then
         Put_Line (" is");
         Put_Line ("begin");

         case Info.a_type.name is
            when LSP_Gen.Entities.Enum.string =>
               Put_Line ("Value := (Handler.String_Value with null record);");

            when others =>
               Put ("Value := LSP.Enumerations.");
               Put_Id (Name);
               Put_Line (" (Handler.Number_Value.Integer_Value);");
         end case;

         Put_Line ("Handler.Read_Next;");
         Put_Line ("end;");
      else
         Put_Line (" is");
         Put_Line ("begin");
         Put ("Value := LSP.Enumerations.");
         Put_Id (Name);
         Put ("'Val (");

         case Info.a_type.name is
            when LSP_Gen.Entities.Enum.string =>
               Put_Id (Name);
               Put_Line ("_Map.Get_Index (Handler.String_Value) - 1);");

            when others =>
               Put ("Handler.Number_Value.Integer_Value ");

               if Info.values (1).value.Integer > 0 then
                  Put ("- ");
               else
                  Put ("+ ");
               end if;

               Put (abs Info.values (1).value.Integer);
               Put_Line (");");

         end case;

         Put_Line ("Handler.Read_Next;");
         Put ("end Read_");
         Put_Id (Name);
         Put_Line (";");
      end if;

      New_Line;
   end Write_Enum;

   ------------------------
   -- Write_Literal_Type --
   ------------------------

   procedure Write_Literal_Type
     (Model       : LSP_Gen.Meta_Models.Meta_Model;
      Done        : LSP_Gen.Dependencies.Dependency_Map;
      Name        : VSS.Strings.Virtual_String;
      Scope       : VSS.Strings.Virtual_String;
      Name_List   : VSS.String_Vectors.Virtual_String_Vector;
      Tipe        : LSP_Gen.Entities.AType;
      Optional    : VSS.String_Vectors.Virtual_String_Vector :=
        VSS.String_Vectors.Empty_Virtual_String_Vector) is
   begin
      Put_Line ("pragma Assert (Handler.Is_Start_Object);");
      Put_Line ("Handler.Read_Next;");
      New_Line;
      Put_Line ("while Handler.Is_Key_Name loop");
      Put_Line ("declare");

      if not Scope.Is_Empty then
         Put ("use ");
         Put (Scope);
         Put_Line (";");
      end if;

      Put_Line
        ("Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;");
      Put_Line ("begin");
      Put_Line ("Handler.Read_Next;");
      Put ("case ");
      Put_Id (Name);
      Put_Line ("_Map.Get_Index (Key) is");

      Write_Properties
        (Model,
         Done,
         Name_List,
         Tipe.Union.literal.value.properties,
         Optional);

      Put_Line ("   when others =>");
      Put_Line ("Handler.Skip_Current_Value;");
      Put_Line ("end case;");
      Put_Line ("end;");
      Put_Line ("end loop;");
      New_Line;
      Put_Line ("Handler.Read_Next;");
   end Write_Literal_Type;

   ----------------------
   -- Write_Look_Ahead --
   ----------------------

   procedure Write_Look_Ahead
     (Model : LSP_Gen.Meta_Models.Meta_Model;
      Name  : VSS.Strings.Virtual_String;
      List  : LSP_Gen.Entities.AType_Vector;
      Match : access procedure (Index : Positive))
   is
      Index : Natural := 0;
      Over  : LSP_Gen.String_Sets.Set;
      Prop  : String_Vector_Array (1 .. List.Length);
   begin
      Put_Line ("declare");
      Put_Line ("Parent : constant not null access");
      Put ("VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class := ");
      Put_Line ("Handler'Access;");
      Put ("Handler : VSS.JSON.Pull_Readers.");
      Put_Line ("Buffered.JSON_Buffered_Pull_Reader (Parent);");
      Put_Line ("begin");
      Put_Line ("Handler.Mark;");

      if Is_Vector (List (1)) and Is_Vector (List (2)) then
         Split_Vector_Properties (Model, List, Prop, Over);
         Put_Line ("if Handler.Is_Start_Array then");
         Put_Line ("Handler.Read_Next;");
         Put_Line ("end if;");
      else
         Split_Properties (Model, List, Prop, Over);
      end if;

      --  Spot base type variants
      for J in 1 .. List.Length loop
         case List (J).Union.Kind is
            when base =>
               case List (J).Union.base.name is
                  when a_boolean =>
                     Put_Line ("if Handler.Is_Boolean_Value then");
                     Put ("Value := (Kind => LSP.Structures.");
                     Put (Model.Get_Variant (List (J), J));
                     Put_Line (", others => <>);");
                     Put ("els");
                  when a_null =>
                     Put_Line ("if Handler.Is_Null_Value then");
                     Put ("Value := (Kind => LSP.Structures.");
                     Put (Model.Get_Variant (List (J), J));
                     Put_Line (", others => <>);");
                     Put ("els");

                  when others =>
                     raise Program_Error;
               end case;
            when others =>
               null;
         end case;
      end loop;

      --  Then spot object types
      Put_Line ("if Handler.Is_Start_Object then");
      Put_Line ("Handler.Read_Next;");

      --  Set default variant if needed
      for J in 1 .. List.Length loop
         case List (J).Union.Kind is
            when base =>
               null;
            when others =>
               --  If all properties overlap with other:
               if (for all Item of Prop (J) => Over.Contains (Item)) then
                  Index := Index + 1;  --  Count found default values
                  Match (J);
               end if;
         end case;
      end loop;

      pragma Assert (Index <= 1);

      Put_Line ("while Handler.Is_Key_Name loop");
      Put_Line ("declare");
      Put_Line
        ("Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;");
      Put ("Index : constant Natural := ");
      Put (Name);
      Put_Line ("_Map.Get_Index (Key);");
      Put_Line ("begin");
      Put_Line ("Handler.Read_Next;");
      Put ("case Index is");
      Index := 1;

      for J in Prop'Range loop
         for Item of Prop (J) loop
            if not Over.Contains (Item) then
               Put ("   when ");
               Put (Index);
               Put (" =>  --  ");
               Put_Line (Item);
               Index := Index + 1;
               Match (J);
               Put_Line ("exit;");
            end if;
         end loop;
      end loop;

      Put_Line ("   when others =>");
      Put_Line ("Handler.Skip_Current_Value;");
      Put_Line ("end case;");
      Put_Line ("end;");
      Put_Line ("end loop;");
      Put_Line ("else");
      Put_Line ("raise Program_Error;  --  Unexpected JSON value");
      Put_Line ("end if;");
      New_Line;
      Put_Line ("Handler.Reset;");
      Put_Line ("Handler.Unmark;");
      New_Line;
   end Write_Look_Ahead;

   ------------------------
   -- Write_Nested_Types --
   ------------------------

   procedure Write_Nested_Types
     (Model : LSP_Gen.Meta_Models.Meta_Model;
      Done  : LSP_Gen.Dependencies.Dependency_Map;
      Name  : VSS.Strings.Virtual_String) is
   begin
      for Cursor in Done.Iterate loop
         if Done (Cursor).Owner = Name then
            Write_Type
              (Model, Done, Done (Cursor).Short_Name,
               LSP_Gen.Dependencies.Dependency_Maps.Key (Cursor),
               Spec => True);
         end if;
      end loop;

      for Cursor in Done.Iterate loop
         if Done (Cursor).Owner = Name then
            Write_Type
              (Model, Done, Done (Cursor).Short_Name,
               LSP_Gen.Dependencies.Dependency_Maps.Key (Cursor),
               Spec => False);
         end if;
      end loop;
   end Write_Nested_Types;

   -----------------------------
   -- Write_Nested_Types_Maps --
   -----------------------------

   procedure Write_Nested_Types_Maps
     (Model : LSP_Gen.Meta_Models.Meta_Model;
      Done  : LSP_Gen.Dependencies.Dependency_Map;
      Name  : VSS.Strings.Virtual_String)
   is

   begin
      for Cursor in Done.Iterate loop
         if Done (Cursor).Owner = Name then
            declare
               Tipe : constant LSP_Gen.Entities.AType :=
                 LSP_Gen.Dependencies.Dependency_Maps.Key (Cursor);
               Name : constant VSS.Strings.Virtual_String :=
                 Done (Cursor).Short_Name;
            begin
               Write_Type_Map (Model, Done, Tipe, Name);
            end;
         end if;
      end loop;
   end Write_Nested_Types_Maps;

   ----------------------
   -- Write_Properties --
   ----------------------

   procedure Write_Properties
     (Model       : LSP_Gen.Meta_Models.Meta_Model;
      Done        : LSP_Gen.Dependencies.Dependency_Map;
      Name_List   : VSS.String_Vectors.Virtual_String_Vector;
      List        : LSP_Gen.Entities.Property_Vector;
      Optional    : VSS.String_Vectors.Virtual_String_Vector :=
        VSS.String_Vectors.Empty_Virtual_String_Vector;
      Suffix      : VSS.Strings.Virtual_String := "";
      Exclude     : LSP_Gen.Entities.Property_Vector := Empty_Vector) is
   begin
      for J in 1 .. List.Length loop
         if not Contains (Exclude, List (J).name) then
            Write_Property
              (Model,
               Done,
               Name_List,
               List (J),
               Is_Optional => (for some X of Optional => X = List (J).name),
               Suffix      => Suffix);
         end if;
      end loop;
   end Write_Properties;

   --------------------
   -- Write_Property --
   --------------------

   procedure Write_Property
     (Model       : LSP_Gen.Meta_Models.Meta_Model;
      Done        : LSP_Gen.Dependencies.Dependency_Map;
      Name_List   : VSS.String_Vectors.Virtual_String_Vector;
      Item        : LSP_Gen.Entities.Property;
      Is_Optional : Boolean := False;
      Suffix      : VSS.Strings.Virtual_String := "")
   is
      Index : constant Natural := Find_First (Name_List, Item.name);

      Id : VSS.Strings.Virtual_String := LSP_Gen.Mappings.Ada_Id (Item.name);
   begin
      Put ("   when ");
      Put (Index);
      Put (" =>  --  ");
      Put_Line (Item.name);

      if Is_Optional or Item.optional then

         if Done (Item.a_type).Short_Name = "SelectionRange" then
            --  Special case for recursive SelectionRange type
            Put_Line ("declare");
            Put_Line ("Value_parent : LSP.Structures.SelectionRange;");
            Put_Line ("begin");
            Write_Call (Done, Item.a_type, "_parent");
            Put_Line ("Value.parent.Set (Value_parent);");
            Put_Line ("end;");

            return;

         elsif not (Is_Set (Model, Item.a_type)
           or else Is_String (Item.a_type)
           or else Is_Symbol_Vector (Item.a_type)
           or else Is_Vector (Item.a_type)
           or else Is_LSP_Any (Item.a_type)
           or else Item.a_type.Union.Kind = map)
         then
            Put ("Value");
            Put (Suffix);
            Put (".");
            Put (Id);
            Put_Line (" := (Is_Set => True, Value => <>);");
            Id.Append (".Value");
         end if;

      end if;

      Write_Call (Done, Item.a_type, Suffix & "." & Id);
   end Write_Property;

   ---------------------
   -- Write_Structure --
   ---------------------

   procedure Write_Structure
     (Model : LSP_Gen.Meta_Models.Meta_Model;
      Done  : LSP_Gen.Dependencies.Dependency_Map;
      Name  : VSS.Strings.Virtual_String)
   is
      procedure Write_Mixin_Propeties
        (List : VSS.String_Vectors.Virtual_String_Vector;
         Item : LSP_Gen.Entities.Structure);

      procedure Write_Extends_Propeties
        (List : VSS.String_Vectors.Virtual_String_Vector;
         Item : LSP_Gen.Entities.Structure);

      procedure Find_Mixin_Propeties
        (List : in out VSS.String_Vectors.Virtual_String_Vector;
         Item : LSP_Gen.Entities.Structure);

      procedure Find_Extends_Propeties
        (List : in out VSS.String_Vectors.Virtual_String_Vector;
         Item : LSP_Gen.Entities.Structure);

      ----------------------------
      -- Find_Extends_Propeties --
      ----------------------------

      procedure Find_Extends_Propeties
        (List : in out VSS.String_Vectors.Virtual_String_Vector;
         Item : LSP_Gen.Entities.Structure) is
      begin
         for J in 1 .. Item.extends.Length loop
            declare
               Parent : constant LSP_Gen.Entities.Structure :=
                 Model.Structure (Item.extends (J).Union.reference.name);
            begin
               Find_Mixin_Propeties (List, Parent);

               List.Append (All_Properties (Parent.properties));
            end;
         end loop;
      end Find_Extends_Propeties;

      --------------------------
      -- Find_Mixin_Propeties --
      --------------------------

      procedure Find_Mixin_Propeties
        (List : in out VSS.String_Vectors.Virtual_String_Vector;
         Item : LSP_Gen.Entities.Structure) is
      begin
         for J in 1 .. Item.mixins.Length loop
            declare
               Mixin  : constant LSP_Gen.Entities.AType := Item.mixins (J);
               Parent : constant LSP_Gen.Entities.Structure :=
                 Model.Structure (Mixin.Union.reference.name);
            begin
               Find_Mixin_Propeties (List, Parent);
               List.Append (All_Properties (Parent.properties));
            end;
         end loop;
      end Find_Mixin_Propeties;

      -----------------------------
      -- Write_Extends_Propeties --
      -----------------------------

      procedure Write_Extends_Propeties
        (List : VSS.String_Vectors.Virtual_String_Vector;
         Item : LSP_Gen.Entities.Structure)
      is
         Base : constant Positive :=
           (if Item.extends.Length <= 1 then 1
            else Mappings.Base_Index (Item.extends));
      begin
         for J in 1 .. Item.extends.Length loop
            declare
               Parent : constant LSP_Gen.Entities.Structure :=
                 Model.Structure (Item.extends (J).Union.reference.name);
            begin
               Write_Mixin_Propeties (List, Parent);

               Write_Properties
                 (Model, Done, List, Parent.properties,
                  Suffix => (if J /= Base then ".Parent"
                             else VSS.Strings.Empty_Virtual_String),
                  Exclude => Item.properties);
            end;
         end loop;
      end Write_Extends_Propeties;

      ---------------------------
      -- Write_Mixin_Propeties --
      ---------------------------

      procedure Write_Mixin_Propeties
        (List : VSS.String_Vectors.Virtual_String_Vector;
         Item : LSP_Gen.Entities.Structure) is
      begin
         for J in 1 .. Item.mixins.Length loop
            declare
               Mixin  : constant LSP_Gen.Entities.AType := Item.mixins (J);
               Parent : constant LSP_Gen.Entities.Structure :=
                 Model.Structure (Mixin.Union.reference.name);
            begin
               Write_Mixin_Propeties (List, Parent);
               Write_Properties (Model, Done, List, Parent.properties);
            end;
         end loop;
      end Write_Mixin_Propeties;

      Name_List : VSS.String_Vectors.Virtual_String_Vector;

      function Has_Scope return Boolean is
         (not Name_List.Is_Empty or else Has_Nested_Types (Done, Name));

      Item : constant LSP_Gen.Entities.Structure :=
        Model.Structure (Name);
   begin
      Find_Extends_Propeties (Name_List, Item);
      Find_Mixin_Propeties (Name_List, Item);

      for X of All_Properties (Item.properties) loop
         if Find_First (Name_List, X) = 0 then
            Name_List.Append (X);
         end if;
      end loop;

      if Has_Scope then
         Put ("package ");
         Put_Id (Name);
         Put_Line ("_Scope is");
         Put ("package ");
         Put_Id (Name);
         Put ("_Map is new Minimal_Perfect_Hash ([");

         for J in 1 .. Name_List.Length loop
            if J > 1 then
               Put (", ");
            end if;

            Put ("""");
            Put (Name_List (J));
            Put_Line ("""");
         end loop;
         Put_Line ("]);");
         New_Line;

         Write_Nested_Types_Maps (Model, Done, Name);

         for J in 1 .. Item.extends.Length loop
            declare
               Parent : constant LSP_Gen.Entities.Structure :=
                 Model.Structure (Item.extends (J).Union.reference.name);
            begin
               Write_Nested_Types_Maps (Model, Done, Parent.name);
            end;
         end loop;

         Put ("end ");
         Put_Id (Name);
         Put_Line ("_Scope;");
         New_Line;
      end if;

      Write_Subprogram_Definition (Name, "LSP.Structures.");
      Put_Line ("is");

      if Has_Scope then
         Put ("use ");
         Put_Id (Name);
         Put_Line ("_Scope;");
      end if;

      Write_Nested_Types (Model, Done, Name);

      for J in 1 .. Item.extends.Length loop
         Write_Nested_Types
           (Model, Done, Item.extends (J).Union.reference.name);
      end loop;

      Put_Line ("begin");
      Put_Line ("pragma Assert (Handler.Is_Start_Object);");

      if Name = "LSPObject" then

         Put_Line ("LSP.Input_Tools.Read_LSPAny_Class (Handler, Value);");
         New_Line;

         Put ("end Read_");
         Put_Id (Name);
         Put_Line (";");
         New_Line;
         return;
      end if;

      Put_Line ("Handler.Read_Next;");
      New_Line;
      Put_Line ("while not Handler.Is_End_Object loop");
      Put_Line ("pragma Assert (Handler.Is_Key_Name);");
      Put_Line ("declare");
      Put_Line
        ("Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;");
      Put_Line ("begin");
      Put_Line ("Handler.Read_Next;");

      if Name_List.Is_Empty then
         Put_Line ("Handler.Skip_Current_Value;");
      else
         Put ("case ");
         Put_Id (Name);
         Put_Line ("_Map.Get_Index (Key) is");

         Write_Extends_Propeties (Name_List, Item);
         Write_Mixin_Propeties (Name_List, Item);
         Write_Properties (Model, Done, Name_List, Item.properties);

         Put_Line ("   when others =>");
         Put_Line ("Handler.Skip_Current_Value;");
         Put_Line ("end case;");
      end if;

      Put_Line ("end;");
      Put_Line ("end loop;");
      New_Line;
      Put_Line ("Handler.Read_Next;");
      Put ("end Read_");
      Put_Id (Name);
      Put_Line (";");
      New_Line;
   end Write_Structure;

   ---------------------------------
   -- Write_Subprogram_Sefinition --
   ---------------------------------

   procedure Write_Subprogram_Definition
     (Name   : VSS.Strings.Virtual_String;
      Prefix : VSS.Strings.Virtual_String) is
   begin
      Put ("procedure Read_");
      Put_Id (Name);
      Put (" (Handler : in out ");
      Put_Line ("VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;");
      Put ("Value : out ");
      Put (Prefix);
      Put_Id (Name);
      Put (")");
   end Write_Subprogram_Definition;

   ---------------------------
   -- Write_Subprogram_Name --
   ---------------------------

   procedure Write_Subprogram_Name
     (Done   : LSP_Gen.Dependencies.Dependency_Map;
      Tipe   : LSP_Gen.Entities.AType) is
   begin
      Put ("Read_");

      case Tipe.Union.Kind is
         when reference =>
            Put_Id (Tipe.Union.reference.name);
         when literal | a_or | an_array | tuple =>
            Put_Id (Done (Tipe).Short_Name);
         when others =>
            raise Program_Error;
      end case;
   end Write_Subprogram_Name;

   ---------------------
   -- Write_Two_Types --
   ---------------------

   procedure Write_Two_Types
     (Model : LSP_Gen.Meta_Models.Meta_Model;
      Done  : LSP_Gen.Dependencies.Dependency_Map;
      Name  : VSS.Strings.Virtual_String;
      Map   : LSP_Gen.Mappings.Or_Mapping;
      List  : LSP_Gen.Entities.AType_Vector)
   is

      procedure Match (Index : Positive);

      First : constant LSP_Gen.Dependencies.Dependency_Info :=
        Done (Map.First);

      Second : constant LSP_Gen.Dependencies.Dependency_Info :=
        Done (Map.Second);

      function Get_Base_Type (Tipe : LSP_Gen.Entities.AType)
        return VSS.Strings.Virtual_String is
          (if Model.Is_Base_Type (Tipe)
           then Get_Base_Type (Model.Get_Base_Type (Tipe))
           else VSS.Strings.Empty_Virtual_String);

      procedure Match (Index : Positive) is
      begin
         Put ("Value := (Is_");
         Put (First.Short_Name);
         Put (" => ");
         if Index = 1 then
            Put ("True");
         else
            Put ("False");
         end if;
         Put_Line (", others => <>);");
      end Match;

      Base_Type  : VSS.Strings.Virtual_String;
      Base_First : Boolean := True;
   begin
      Base_Type := Get_Base_Type (Map.First);

      if Base_Type.Is_Empty then
         Base_Type := Get_Base_Type (Map.Second);
         Base_First := False;
      end if;

      if not Base_Type.Is_Empty then
         Put ("if Handler.Is_");
         Put (Base_Type);
         Put_Line ("_Value then");
         Put ("Value := (Is_");
         Put (First.Short_Name);
         Put (" => ");
         if Base_First then
            Put ("True");
         else
            Put ("False");
         end if;
         Put_Line (", others => <>);");
         Put_Line ("else");
         Put ("Value := (Is_");
         Put (First.Short_Name);
         Put (" => ");
         if Base_First then
            Put ("False");
         else
            Put ("True");
         end if;
         Put_Line (", others => <>);");
         Put_Line ("end if;");
         New_Line;
      elsif List.Length = 2 then
         Write_Look_Ahead (Model, Name, List, Match'Access);
      else
         --  special case here for type of:
         --  contents: MarkedString | MarkedString[] | MarkupContent;
         Put_Line ("declare");
         Put ("Parent  : constant not null access VSS.JSON.Pull_Readers");
         Put_Line (".JSON_Pull_Reader' Class := Handler'Access;");
         Put ("Handler : VSS.JSON.Pull_Readers.Buffered");
         Put_Line (".JSON_Buffered_Pull_Reader (Parent);");
         Put_Line ("begin");
         Put_Line ("Handler.Mark;");
         Put ("LSP.Input_Tools.Look_For_");
         Put_Line ("MarkupContent_Or_MarkedString_Vector (Handler, Value);");
         Put_Line ("Handler.Reset;");
         Put_Line ("Handler.Unmark;");
         Put ("if not Value.Is_MarkupContent and");
         Put_Line (" not Handler.Is_Start_Array then");
         Put ("Read_MarkedString");
         Put_Line (" (Handler, Value.MarkedString_Vector (1));");
         Put_Line ("return;");
         Put_Line ("end if;");
         New_Line;
      end if;

      Put ("case Value.Is_");
      Put (First.Short_Name);
      Put_Line (" is");
      Put_Line ("   when True =>");

      Write_Call
        (Done,
         Map.First,
         "." & First.Short_Name);

      Put_Line ("   when False =>");

      Write_Call
        (Done,
         Map.Second,
         "." & Second.Short_Name);

      Put_Line ("end case;");

      if Base_Type.Is_Empty then
         Put_Line ("end;");
      end if;
   end Write_Two_Types;

   ----------------
   -- Write_Type --
   ----------------

   procedure Write_Type
     (Model : LSP_Gen.Meta_Models.Meta_Model;
      Done  : LSP_Gen.Dependencies.Dependency_Map;
      Name  : VSS.Strings.Virtual_String;
      Tipe  : LSP_Gen.Entities.AType;
      Spec  : Boolean)
   is
      use all type LSP_Gen.Mappings.Or_Mapping_Kind;

      function Array_Element return LSP_Gen.Entities.AType is
        (Tipe.Union.an_array.element.Value);

   begin
      if Name = "LSPArray" then
         return;  --  TBD
      elsif Tipe.Union.Kind = stringLiteral then
         return;
      elsif Tipe.Union.Kind = base and then
        Tipe.Union.base.name not in LSP_Gen.Entities.Enum.string | a_null
      then
         return;
      end if;

      if Tipe.Union.Kind = reference
        and then Model.Is_Enumeration (Tipe.Union.reference.name)
      then
         Write_Subprogram_Definition (Name, "LSP.Enumerations.");
      else
         Write_Subprogram_Definition (Name, "LSP.Structures.");
      end if;

      if Name = "Declaration" then
         pragma Assert (not VSS.Strings."=" (Name.Hash, 0));
      end if;

      if Spec then
         Put_Line (";");
      elsif Tipe.Union.Kind = reference then
         Put_Line (" renames");
         Write_Subprogram_Name (Done, Tipe);
         Put_Line (";");
      elsif Name = "LSPAny_Vector" or Name = "LSPAny" then
         Put_Line (" is");
         Put_Line ("begin");
         Put_Line ("LSP.Input_Tools.Read_LSPAny_Class (Handler, Value);");
         Put_Line ("end;");
      else
         Put_Line ("is");

         if Has_Scope (Model, Tipe) then
            Put ("use ");
            Put_Id (Name);
            Put_Line ("_Scope;");
         end if;

         Write_Nested_Types (Model, Done, Name);

         Put_Line ("begin");

         case Tipe.Union.Kind is
            when base =>

               Write_Call (Done, Tipe, "");
            when tuple =>

               Put_Line ("pragma Assert (Handler.Is_Start_Array);");
               Put_Line ("Handler.Read_Next;");
               Put_Line ("for J in Value'Range loop");
               Write_Call (Done, Tipe.Union.tuple.items (1), " (J)");
               Put_Line ("end loop;");
               Put_Line ("pragma Assert (Handler.Is_End_Array);");
               Put_Line ("Handler.Read_Next;");
            when an_array =>

               Put_Line ("pragma Assert (Handler.Is_Start_Array);");
               Put_Line ("Handler.Read_Next;");
               New_Line;
               Put_Line ("declare");
               Put ("Set : LSP.Structures.");
               Put (Name);
               Put_Line (" renames Value;");
               Put ("Value : ");
               Put (Done (Tipe.Union.an_array.element.Value).Full_Name);
               Put_Line (";");
               Put_Line ("begin");

               if Name.Ends_With ("_Set")
                 and then
                   (Array_Element.Union.Kind /= reference
                     or else not Model.Is_Custom_Enumeration
                       (Array_Element.Union.reference.name))
               then
                  Put_Line ("Set := (others => False);");
                  Put_Line ("   while not Handler.Is_End_Array loop");
                  Write_Call (Done, Tipe.Union.an_array.element.Value, "");
                  Put_Line ("      Set (Value) := True;");
               else
                  Put_Line ("Set.Clear;");
                  Put_Line ("   while not Handler.Is_End_Array loop");
                  Write_Call (Done, Tipe.Union.an_array.element.Value, "");
                  Put_Line ("      Set.Append (Value);");
               end if;

               Put_Line ("   end loop;");
               Put_Line ("end;");
               New_Line;
               Put_Line ("Handler.Read_Next;");
            when map =>
               Write_Call (Done, Tipe, "");

            when a_or =>
               declare
                  Map : constant LSP_Gen.Mappings.Or_Mapping :=
                    LSP_Gen.Mappings.Get_Or_Mapping
                      (Model, Tipe.Union.a_or.items);
               begin
                  case Map.Kind is
                     when Type_Or_Array =>
                        Put_Line ("declare");
                        Put ("Set : LSP.Structures.");
                        Put (Name);
                        Put_Line (" renames Value;");
                        Put ("Value : ");
                        Put
                          (Done (Map.Array_Type.Union.an_array.element.Value)
                           .Full_Name);
                        Put_Line (";");
                        Put_Line ("begin");
                        Put_Line ("Set.Clear;");

                        Put_Line ("if Handler.Is_Start_Array then");
                        Put_Line ("Handler.Read_Next;");
                        Put_Line ("   while not Handler.Is_End_Array loop");
                        Write_Call
                          (Done,
                           Map.Array_Type.Union.an_array.element.Value,
                           "");
                        Put_Line ("      Set.Append (Value);");
                        Put_Line ("   end loop;");
                        Put_Line ("Handler.Read_Next;");
                        New_Line;

                        Put_Line ("else");
                        Write_Call
                          (Done,
                           Map.Array_Type.Union.an_array.element.Value,
                           "");
                        Put_Line ("Set.Append (Value);");
                        Put_Line ("end if;");
                        Put_Line ("end;");
                        New_Line;

                     when Type_Or_Null =>
                        Put_Line ("if Handler.Is_Null_Value then");
                        Put_Line ("Handler.Read_Next;");
                        Put_Line ("else");
                        Put_Line ("Value := (Is_Null => False, Value => <>);");
                        Write_Call (Done, Map.Tipe, ".Value");
                        Put_Line ("end if;");

                     when Array_Or_Null =>
                        Put_Line ("if Handler.Is_Null_Value then");
                        Put_Line ("Handler.Read_Next;");
                        Put_Line ("else");
                        Put_Line ("Value.Clear;");
                        Write_Call (Done, Map.Array_Type, "");
                        Put_Line ("end if;");

                     when Unknown_Mapping =>
                        Write_Unknown_Mapping
                          (Model, Done, Name, Tipe.Union.a_or.items);
                     when Two_Types | String_Or_Tuple =>
                        Write_Two_Types
                          (Model, Done, Name, Map, Tipe.Union.a_or.items);

                     when Type_Or_Something =>
                        Write_Type_Or_Something
                          (Model,
                           Done,
                           Name,
                           Map,
                           Tipe.Union.a_or.items,
                           Tipe);

                     when Option_Combination =>
                        declare
                           Name_List : constant VSS.String_Vectors
                             .Virtual_String_Vector :=
                               All_Properties (Model, Map.Tipe);
                        begin
                           Write_Literal_Type
                             (Model,
                              Done,
                              Name,
                              "",
                              Name_List,
                              Map.Tipe,
                              Optional => Name_List);
                        end;

                     when Two_Literals =>
                        declare
                           Optional : VSS.String_Vectors.Virtual_String_Vector;

                           Base     : constant VSS.String_Vectors
                             .Virtual_String_Vector :=
                               All_Properties
                                 (Map.First.Union.literal.value.properties);

                           Extend   : constant VSS.String_Vectors
                             .Virtual_String_Vector :=
                               All_Properties
                                 (Map.Second.Union.literal.value.properties);
                        begin
                           for Item of Extend loop
                              if (for all X of Base => Item /= X) then
                                 Optional.Append (Item);
                              end if;
                           end loop;

                           Write_Literal_Type
                             (Model,
                              Done,
                              Name,
                              "",
                              Extend,
                              Map.Second,
                              Optional);
                        end;

                     when Type_Union =>
                        Write_Union (Model, Done, Name, Map.Items);

                     when Boolean_Or_Any =>
                        Put ("LSP.Input_Tools.Read_LSPAny_Class");
                        Put_Line (" (Handler, Value);");

                     when Enumeration =>
                        Put ("Value := LSP.Structures.");
                        Put (Name);
                        Put ("'Val (");
                        Put (Name);
                        Put_Line
                          ("_Map.Get_Index (Handler.String_Value) - 1);");
                        Put_Line ("Handler.Read_Next;");
                  end case;
               end;
            when literal =>

               Write_Literal_Type
                 (Model,
                  Done,
                  Name,
                  Name & "_Scope",
                  All_Properties (Tipe.Union.literal.value.properties),
                  Tipe);

            when others =>
               raise Program_Error;
         end case;
         Put ("end Read_");
         Put_Id (Name);
         Put_Line (";");
      end if;

      New_Line;
   end Write_Type;

   --------------------
   -- Write_Type_Map --
   --------------------

   procedure Write_Type_Map
     (Model : LSP_Gen.Meta_Models.Meta_Model;
      Done  : LSP_Gen.Dependencies.Dependency_Map;
      Tipe  : LSP_Gen.Entities.AType;
      Name  : VSS.Strings.Virtual_String)
   is
      procedure Put_Prolog (Name : VSS.Strings.Virtual_String);
      procedure Put_Epilog (Name : VSS.Strings.Virtual_String);

      procedure Put_Prolog (Name : VSS.Strings.Virtual_String) is
      begin
         Put ("package ");
         Put_Id (Name);
         Put_Line ("_Scope is");
         Put ("package ");
         Put_Id (Name);
         Put ("_Map is new Minimal_Perfect_Hash ([");
      end Put_Prolog;

      procedure Put_Epilog (Name : VSS.Strings.Virtual_String) is
      begin
         Put_Line ("]);");
         New_Line;

         Write_Nested_Types_Maps (Model, Done, Name);

         Put ("end ");
         Put_Id (Name);
         Put_Line ("_Scope;");
         New_Line;
      end Put_Epilog;

      List : constant VSS.String_Vectors.Virtual_String_Vector :=
        Get_Type_Map (Model, Tipe);
   begin
      if not List.Is_Empty then
         Put_Prolog (Name);

         for J in 1 .. List.Length loop
            if J > 1 then
               Put (", ");
            end if;

            Put ("""");
            Put (List (J));
            Put_Line ("""");
         end loop;

         Put_Epilog (Name);

      end if;
   end Write_Type_Map;

   -----------------------------
   -- Write_Type_Or_Something --
   -----------------------------

   procedure Write_Type_Or_Something
     (Model : LSP_Gen.Meta_Models.Meta_Model;
      Done  : LSP_Gen.Dependencies.Dependency_Map;
      Name  : VSS.Strings.Virtual_String;
      Map   : LSP_Gen.Mappings.Or_Mapping;
      List  : LSP_Gen.Entities.AType_Vector;
      Tipe  : LSP_Gen.Entities.AType)
   is

      procedure Match (Index : Positive);

      First : constant VSS.Strings.Virtual_String :=
        Done (Map.First).Short_Name;

      function Base_Type
        return VSS.Strings.Virtual_String is
          (if Model.Is_Base_Type (Map.First)
           then Get_Base_Type (Model.Get_Base_Type (Map.First))
           else VSS.Strings.Empty_Virtual_String);

      procedure Match (Index : Positive) is
      begin
         Put ("Value := (Is_");
         Put (First);
         Put (" => ");
         if Index = 1 then
            Put ("True");
         else
            Put ("False");
         end if;
         Put_Line (", others => <>);");
      end Match;

      Name_List : constant VSS.String_Vectors.Virtual_String_Vector :=
        Get_Type_Map (Model, Tipe);
   begin
      if Map.First.Union.Kind = base then
         Put ("if Handler.Is_");
         Put (Base_Type);
         Put_Line ("_Value then");
         Put ("Value := (Is_");
         Put (First);
         Put (" => True, others => <>);");
         Put_Line ("else");
         Put ("Value := (Is_");
         Put (First);
         Put (" => False");
         Put_Line (", others => <>);");
         Put_Line ("end if;");
         New_Line;
      else
         Write_Look_Ahead (Model, Name, List, Match'Access);
      end if;

      Put ("case Value.Is_");
      Put (First);
      Put_Line (" is");
      Put_Line ("   when True =>");

      Write_Call (Done, Map.First, "." & First);

      Put_Line ("   when False =>");

      Write_Literal_Type (Model, Done, Name, "", Name_List, Map.Second);

      Put_Line ("end case;");

      if Map.First.Union.Kind /= base then
         Put_Line ("end;");
      end if;
   end Write_Type_Or_Something;

   -----------------
   -- Write_Union --
   -----------------

   procedure Write_Union
     (Model : LSP_Gen.Meta_Models.Meta_Model;
      Done  : LSP_Gen.Dependencies.Dependency_Map;
      Name  : VSS.Strings.Virtual_String;
      List  : LSP_Gen.Entities.AType_Vector)
   is
      Variants : constant VSS.String_Vectors.Virtual_String_Vector :=
        Get_Variants (Model, List);
   begin
      Put_Line ("declare");
      Put_Line ("Parent : constant not null access");
      Put ("VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class := ");
      Put_Line ("Handler'Access;");
      Put ("Handler : VSS.JSON.Pull_Readers.");
      Put_Line ("Buffered.JSON_Buffered_Pull_Reader (Parent);");
      Put_Line ("Kind : Natural;");
      Put_Line ("begin");
      Put_Line ("Handler.Mark;");
      Put_Line ("pragma Assert (Handler.Is_Start_Object);");
      Put_Line ("Handler.Read_Next;");
      Put_Line ("while Handler.Is_Key_Name loop");
      Put_Line ("declare");
      Put_Line ("use type VSS.Strings.Virtual_String;");
      Put_Line
        ("Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;");
      Put_Line ("begin");
      Put_Line ("Handler.Read_Next;");
      Put_Line ("if Key = ""kind"" then");
      Put_Line ("pragma Assert (Handler.Is_String_Value);");
      Put ("Kind := ");
      Put_Id (Name);
      Put_Line ("_Map .Get_Index (Handler.String_Value);");
      Put_Line ("case Kind is");
      for J in 1 .. Variants.Length loop
         Put ("   when ");
         Put (J);
         Put (" =>  --  ");
         Put_Line (Variants (J));
         Put ("Value := (Kind => LSP.Structures.");
         Put_Id (Variants (J));
         Put_Line (", others => <>);");
      end loop;
      Put_Line ("   when others =>");
      Put_Line ("raise Constraint_Error;");
      Put_Line ("end case;");
      Put_Line ("exit;");
      Put_Line ("else");
      Put_Line ("Handler.Skip_Current_Value;");
      Put_Line ("end if;");
      Put_Line ("end;");
      Put_Line ("end loop;");
      New_Line;
      Put_Line ("Handler.Reset;");
      Put_Line ("Handler.Unmark;");
      New_Line;
      Put_Line ("case Value.Kind is");

      for J in 1 .. List.Length loop
         Put ("when LSP.Structures.");
         Put_Id (Model.Get_Variant (List (J), J));
         Put_Line (" =>");
         Write_Call (Done, List (J), "." & Model.Get_Variant (List (J), J));
      end loop;

      Put_Line ("end case;");
      Put_Line ("end;");
   end Write_Union;

   ---------------------------
   -- Write_Unknown_Mapping --
   ---------------------------

   procedure Write_Unknown_Mapping
     (Model : LSP_Gen.Meta_Models.Meta_Model;
      Done  : LSP_Gen.Dependencies.Dependency_Map;
      Name  : VSS.Strings.Virtual_String;
      List  : LSP_Gen.Entities.AType_Vector)
   is
      procedure Match (Index : Positive);

      procedure Match (Index : Positive) is
      begin
         Put ("Value := (Kind => LSP.Structures.");
         Put (Model.Get_Variant (List (Index), Index));
         Put_Line (", others => <>);");
      end Match;
   begin
      Write_Look_Ahead (Model, Name, List, Match'Access);

      Put_Line ("case Value.Kind is");

      for J in 1 .. List.Length loop
         Put ("   when LSP.Structures.");
         Put_Id (Model.Get_Variant (List (J), J));
         Put_Line (" =>");

         Write_Call (Done, List (J), "." & Model.Get_Variant (List (J), J));
      end loop;

      Put_Line ("end case;");
      Put_Line ("end;");
   end Write_Unknown_Mapping;

end LSP_Gen.Inputs;
