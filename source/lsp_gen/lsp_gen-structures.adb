------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2022-2023, AdaCore                     --
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

with LSP_Gen.Dependencies; use LSP_Gen.Dependencies;
with LSP_Gen.Entities;
with LSP_Gen.Mappings;
with LSP_Gen.Puts; use LSP_Gen.Puts;
with LSP_Gen.String_Sets;

package body LSP_Gen.Structures is

   use all type LSP_Gen.Entities.Enum.AType_Variant;
   use all type LSP_Gen.Mappings.Or_Mapping_Kind;
   use type LSP_Gen.Entities.Enum.BaseTypes;
   use type VSS.Strings.Virtual_String;

   function Predefined_Equal (L, R : LSP_Gen.Entities.AType) return Boolean
     renames LSP_Gen.Entities."=";

   function "=" (Left, Right : LSP_Gen.Entities.AType) return Boolean;

   procedure Write_Type
     (Model    : LSP_Gen.Meta_Models.Meta_Model;
      Name     : VSS.Strings.Virtual_String;
      Item     : LSP_Gen.Entities.AType;
      Fallback : VSS.Strings.Virtual_String;
      Done     : Dependency_Map);
   procedure Write_Properties
     (List        : LSP_Gen.Entities.Property_Vector;
      Is_Optional : Boolean := False;
      Done        : Dependency_Map);
   --  Force all properties to be optional if Is_Optional
   procedure Write_Property
     (Item        : LSP_Gen.Entities.Property;
      Is_Optional : Boolean := False;
      Done        : Dependency_Map);
   --  Force property to be optional if Is_Optional
   procedure Write_Type_Name
     (Model       : LSP_Gen.Meta_Models.Meta_Model;
      Item        : LSP_Gen.Entities.AType;
      Is_Optional : Boolean);
   procedure Write_Or_Null_Type
     (Name : VSS.Strings.Virtual_String;
      List : LSP_Gen.Entities.AType_Vector);
   procedure Write_Two_Types
     (Model : LSP_Gen.Meta_Models.Meta_Model;
      Done   : Dependency_Map;
      Name  : VSS.Strings.Virtual_String;
      Map    : LSP_Gen.Mappings.Or_Mapping);
   procedure Write_Type_Or_Something
     (Model : LSP_Gen.Meta_Models.Meta_Model;
      Done  : Dependency_Map;
      Name  : VSS.Strings.Virtual_String;
      Map   : LSP_Gen.Mappings.Or_Mapping);
   procedure Write_Two_Literals
     (Name   : VSS.Strings.Virtual_String;
      Base   : LSP_Gen.Entities.AType;
      Extend : LSP_Gen.Entities.AType;
      Done   : Dependency_Map);
   procedure Write_Optional_Type (Name : VSS.Strings.Virtual_String);
   procedure Write_Vector_Type
     (Model : LSP_Gen.Meta_Models.Meta_Model;
      Name  : VSS.Strings.Virtual_String;
      Item  : VSS.Strings.Virtual_String);
   procedure Write_Enumeration
     (Name : VSS.Strings.Virtual_String;
      List : LSP_Gen.Entities.AType_Vector);
   procedure Write_Union
     (Model    : LSP_Gen.Meta_Models.Meta_Model;
      Done     : Dependency_Map;
      Name     : VSS.Strings.Virtual_String;
      List     : LSP_Gen.Entities.AType_Vector);
   procedure Write_Private_Part;
   procedure Emit_Dependence_Type
     (Model     : LSP_Gen.Meta_Models.Meta_Model;
      Item      : LSP_Gen.Entities.AType;
      Skip      : VSS.Strings.Virtual_String;
      Owner     : VSS.Strings.Virtual_String;
      Done      : in out Dependency_Map;
      Fallback  : VSS.Strings.Virtual_String := "";
      Optional  : Boolean := False);
   procedure Emit_Dependence
     (Model     : LSP_Gen.Meta_Models.Meta_Model;
      Item      : LSP_Gen.Entities.AType;
      Skip      : VSS.Strings.Virtual_String;
      Owner     : VSS.Strings.Virtual_String;
      Done      : in out Dependency_Map;
      Fallback  : VSS.Strings.Virtual_String);
   procedure Emit_Dependence
     (Model : LSP_Gen.Meta_Models.Meta_Model;
      Item  : LSP_Gen.Entities.AType_Vector;
      Skip  : VSS.Strings.Virtual_String;
      Owner : VSS.Strings.Virtual_String;
      Done  : in out Dependency_Map);

   procedure Emit_Dependence
     (Model     : LSP_Gen.Meta_Models.Meta_Model;
      Item      : LSP_Gen.Entities.Property_Vector;
      Skip      : VSS.Strings.Virtual_String;
      Owner     : VSS.Strings.Virtual_String;
      Done      : in out Dependency_Map;
      Optional  : Boolean := False;
      Enclosing : VSS.Strings.Virtual_String);

   function Short_Name
     (Model : LSP_Gen.Meta_Models.Meta_Model;
      Item  : LSP_Gen.Entities.AType) return VSS.Strings.Virtual_String;
   --  Return simple defining name for a type

   function Short_Name (Item : LSP_Gen.Entities.MapKeyType)
     return VSS.Strings.Virtual_String;

   function Short_Name
     (Model    : LSP_Gen.Meta_Models.Meta_Model;
      Item     : LSP_Gen.Entities.AType;
      Fallback : VSS.Strings.Virtual_String)
      return VSS.Strings.Virtual_String;
   --  Return simple defining name for a type or Fallback

   function Vector_Name
     (Model : LSP_Gen.Meta_Models.Meta_Model;
      Item  : VSS.Strings.Virtual_String) return VSS.Strings.Virtual_String;
   --  Return Item & "_Vector" (or Item & "_Set" if Item is an enum)

   function Image (Value : Integer) return VSS.Strings.Virtual_String;

   function To_Reference (Name : VSS.Strings.Virtual_String)
     return LSP_Gen.Entities.AType is
       (Union => (reference, (name => Name)));

   function Make_Array
     (Item : LSP_Gen.Entities.AType) return LSP_Gen.Entities.AType;

   function Get_Or_Mapping
     (Model : LSP_Gen.Meta_Models.Meta_Model'Class;
      Items : LSP_Gen.Entities.AType_Vector) return LSP_Gen.Mappings.Or_Mapping
     renames LSP_Gen.Mappings.Get_Or_Mapping;

   Base_Short_Name : constant array (LSP_Gen.Entities.Enum.BaseTypes) of
     VSS.Strings.Virtual_String :=
       (LSP_Gen.Entities.Enum.Uri |
        LSP_Gen.Entities.Enum.DocumentUri => "DocumentUri",
        LSP_Gen.Entities.Enum.integer     => "Integer",
        LSP_Gen.Entities.Enum.uinteger    => "Natural",
        LSP_Gen.Entities.Enum.decimal     => "Float",
        LSP_Gen.Entities.Enum.RegExp |
        LSP_Gen.Entities.Enum.string      => "Virtual_String",
        LSP_Gen.Entities.Enum.a_boolean   => "Boolean",
        LSP_Gen.Entities.Enum.a_null      => "Null_Record");

   Base_Full_Name : constant array (LSP_Gen.Entities.Enum.BaseTypes) of
     VSS.Strings.Virtual_String :=
       (LSP_Gen.Entities.Enum.Uri |
        LSP_Gen.Entities.Enum.DocumentUri => "LSP.Structures.DocumentUri",
        LSP_Gen.Entities.Enum.integer     => "Standard.Integer",
        LSP_Gen.Entities.Enum.uinteger    => "Natural",
        LSP_Gen.Entities.Enum.decimal     => "Float",
        LSP_Gen.Entities.Enum.RegExp |
        LSP_Gen.Entities.Enum.string      => "LSP.Structures.Virtual_String",
        LSP_Gen.Entities.Enum.a_boolean   => "Standard.Boolean",
        LSP_Gen.Entities.Enum.a_null      => "LSP.Structures.Null_Record");

   package Constants is
      Set    : constant VSS.Strings.Virtual_String := "_Set";
      Vector : constant VSS.Strings.Virtual_String := "_Vector";
   end Constants;

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : LSP_Gen.Entities.AType) return Boolean is
      use type LSP_Gen.Entities.MapKeyType;

      function "=" (L, R : LSP_Gen.Entities.AType_Vector) return Boolean;
      function "=" (L, R : LSP_Gen.Entities.Property_Vector) return Boolean;

      ---------
      -- "=" --
      ---------

      function "=" (L, R : LSP_Gen.Entities.AType_Vector) return Boolean is
      begin
         if L.Length /= R.Length then
            return False;
         end if;

         for J in 1 .. L.Length loop
            if L (J) /= R (J) then
               return False;
            end if;
         end loop;

         return True;
      end "=";

      ---------
      -- "=" --
      ---------

      function "=" (L, R : LSP_Gen.Entities.Property_Vector) return Boolean is
      begin
         if L.Length /= R.Length then
            return False;
         end if;

         for J in 1 .. L.Length loop
            if L (J).name /= R (J).name or else
              L (J).optional /= R (J).optional or else
              L (J).a_type /= R (J).a_type
            then
               return False;
            end if;
         end loop;

         return True;
      end "=";
   begin
      if Left.Union.Kind /= Right.Union.Kind then
         return False;
      end if;

      case Left.Union.Kind is
         when base
            | reference
            | stringLiteral
            | integerLiteral
            | booleanLiteral =>

            return Predefined_Equal (Left, Right);
         when an_array =>
            return Left.Union.an_array.element.Value =
              Right.Union.an_array.element.Value;
         when map =>
            return Left.Union.map.key = Right.Union.map.key and then
              Left.Union.map.value.Value = Right.Union.map.value.Value;
         when an_and =>
            return Left.Union.an_and.items = Right.Union.an_and.items;
         when a_or =>
            return Left.Union.a_or.items = Right.Union.a_or.items;
         when tuple =>
            return Left.Union.tuple.items = Right.Union.tuple.items;
         when literal =>
            return Left.Union.literal.value.properties =
              Right.Union.literal.value.properties;
      end case;
   end "=";

   ---------------------
   -- Emit_Dependence --
   ---------------------

   procedure Emit_Dependence
     (Model     : LSP_Gen.Meta_Models.Meta_Model;
      Item      : LSP_Gen.Entities.AType;
      Skip      : VSS.Strings.Virtual_String;
      Owner     : VSS.Strings.Virtual_String;
      Done      : in out Dependency_Map;
      Fallback  : VSS.Strings.Virtual_String)
   is
      Name : constant VSS.Strings.Virtual_String :=
        Short_Name (Model, Item, Fallback);
   begin
      case Item.Union.Kind is
         when base | stringLiteral | reference =>

            null;

         when an_array =>
            if Fallback /= "LSPArray" then
               Emit_Dependence_Type
                 (Model,
                  Item.Union.an_array.element.Value,
                  Skip,
                  Owner,
                  Done,
                  Fallback & "_Item");
            end if;

         when a_or =>
            if Fallback /= "LSPAny" then
               declare
                  Map : constant Mappings.Or_Mapping :=
                    Get_Or_Mapping (Model, Item.Union.a_or.items);
               begin
                  case Map.Kind is
                     when Boolean_Or_Any =>
                        null;

                     when Option_Combination =>
                        Emit_Dependence
                          (Model,
                           Map.Tipe.Union.literal.value.properties,
                           Skip,
                           Owner,
                           Done,
                           Optional => True,
                           Enclosing => Name);

                     when Type_Or_Something =>

                        Emit_Dependence_Type
                          (Model, Map.First, Skip, Owner, Done);

                     when Two_Literals =>
                        null;

                     when Unknown_Mapping =>
                        --  Last resort

                        for J in 1 .. Item.Union.a_or.items.Length loop
                           Emit_Dependence_Type
                             (Model,
                              Item.Union.a_or.items (J),
                              Skip,
                              Owner,
                              Done,
                              Fallback & "_" & Image (J));

                           New_Line;
                        end loop;

                     when others =>
                        Emit_Dependence
                          (Model, Item.Union.a_or.items, Skip, Owner, Done);
                  end case;
               end;
            else
               null;
            end if;

         when literal =>
            Emit_Dependence
              (Model,
               Item.Union.literal.value.properties,
               Skip,
               Owner,
               Done,
               Enclosing => Name);

         when map =>
            Emit_Dependence_Type
              (Model, Item.Union.map.value.Value, Skip, Owner, Done,
               Fallback & "_Item");

         when tuple =>
            Emit_Dependence (Model, Item.Union.tuple.items, Skip, Owner, Done);

         when an_and |
              integerLiteral | booleanLiteral =>
            raise Program_Error;
      end case;
   end Emit_Dependence;

   procedure Emit_Dependence_Type
     (Model     : LSP_Gen.Meta_Models.Meta_Model;
      Item      : LSP_Gen.Entities.AType;
      Skip      : VSS.Strings.Virtual_String;
      Owner     : VSS.Strings.Virtual_String;
      Done      : in out Dependency_Map;
      Fallback  : VSS.Strings.Virtual_String := "";
      Optional  : Boolean := False)
   is
      Name : constant VSS.Strings.Virtual_String :=
        Short_Name (Model, Item, Fallback);
   begin
      Emit_Dependence (Model, Item, Skip, Owner, Done, Fallback);

      if not Done.Contains (Item) then
         Done.Insert
           (Item,
            (Name,
             (if Item.Union.Kind = base
              then Base_Full_Name (Item.Union.base.name)
              else "LSP.Structures." & Name),
             False, False,
             Owner));
         Write_Type (Model, Name, Item, Fallback, Done);
         New_Line;
      elsif Done (Item).Owner /= Owner then
         Done (Item).Owner := "";
      end if;

      if Optional and then
        Item.Union.Kind not in an_array | map and then
        not Done (Item).Has_Option
      then
         Done (Item).Has_Option := True;
         Write_Optional_Type (Name);
      end if;
   end Emit_Dependence_Type;

   ---------------------
   -- Emit_Dependence --
   ---------------------

   procedure Emit_Dependence
     (Model : LSP_Gen.Meta_Models.Meta_Model;
      Item  : LSP_Gen.Entities.AType_Vector;
      Skip  : VSS.Strings.Virtual_String;
      Owner : VSS.Strings.Virtual_String;
      Done  : in out Dependency_Map) is
   begin
      for J in 1 .. Item.Length loop
         Emit_Dependence_Type (Model, Item (J), Skip, Owner, Done);
      end loop;
   end Emit_Dependence;

   ---------------------
   -- Emit_Dependence --
   ---------------------

   procedure Emit_Dependence
     (Model     : LSP_Gen.Meta_Models.Meta_Model;
      Item      : LSP_Gen.Entities.Property_Vector;
      Skip      : VSS.Strings.Virtual_String;
      Owner     : VSS.Strings.Virtual_String;
      Done      : in out Dependency_Map;
      Optional  : Boolean := False;
      Enclosing : VSS.Strings.Virtual_String) is
   begin
      for J in 1 .. Item.Length loop
         Emit_Dependence_Type
           (Model,
            Item (J).a_type,
            Skip,
            Owner,
            Done,
            Fallback => Item (J).name & "_Of" & Enclosing,
            Optional => Item (J).optional or Optional);
      end loop;
   end Emit_Dependence;

   -----------
   -- Image --
   -----------

   function Image (Value : Integer) return VSS.Strings.Virtual_String is
      Result : constant Wide_Wide_String := Value'Wide_Wide_Image;
   begin
      return VSS.Strings.To_Virtual_String
        (if Value < 0 then Result else Result (2 .. Result'Last));
   end Image;

   ----------------
   -- Make_Array --
   ----------------

   function Make_Array
     (Item : LSP_Gen.Entities.AType) return LSP_Gen.Entities.AType is
   begin
      return Result : LSP_Gen.Entities.AType :=
        (Union => (an_array, others => <>))
      do
         Result.Union.an_array.element.Element := Item;
      end return;
   end Make_Array;

   ----------------
   -- Short_Name --
   ----------------

   function Short_Name
     (Model : LSP_Gen.Meta_Models.Meta_Model;
      Item  : LSP_Gen.Entities.AType)
        return VSS.Strings.Virtual_String is
   begin
      case Item.Union.Kind is
         when base =>
            return Base_Short_Name (Item.Union.base.name);
         when reference =>
            return Item.Union.reference.name;
         when an_array =>
            return
              Vector_Name
                (Model, Short_Name (Model, Item.Union.an_array.element.Value));

         when a_or =>
            declare
               List : constant LSP_Gen.Entities.AType_Vector :=
                 Item.Union.a_or.items;

               Mapping : constant Mappings.Or_Mapping :=
                 Get_Or_Mapping (Model, List);
            begin
               case Mapping.Kind is
                  when Type_Or_Null =>
                     return Short_Name (Model, Mapping.Tipe) & "_Or_Null";
                  when Array_Or_Null =>
                     return Short_Name
                       (Model, Mapping.Array_Type) & "_Or_Null";
                  when Type_Or_Array =>
                     return Short_Name (Model, Mapping.Array_Type);
                  when Option_Combination =>
                     return Short_Name (Model, Mapping.Tipe);
                  when Two_Types =>
                     return Short_Name (Model, Mapping.First) & "_Or_" &
                       Short_Name (Model, Mapping.Second);
                  when String_Or_Tuple =>
                     return "String_Or_" & Short_Name (Model, Mapping.Second);
                  when Boolean_Or_Any =>
                     return "Boolean_Or_Any";
                  when Type_Or_Something =>
                     return Short_Name
                       (Model, Mapping.First) & "_Or_Something";
                  when Type_Union
                     | Two_Literals
                     | Enumeration
                     | Unknown_Mapping =>

                     raise Program_Error;
               end case;
            end;
         when tuple =>
            return Short_Name (Model, Item.Union.tuple.items (1)) &
              "_Tuple";
         when others =>
            raise Program_Error;
      end case;
   end Short_Name;

   ----------------
   -- Short_Name --
   ----------------

   function Short_Name
     (Model    : LSP_Gen.Meta_Models.Meta_Model;
      Item     : LSP_Gen.Entities.AType;
      Fallback : VSS.Strings.Virtual_String)
        return VSS.Strings.Virtual_String is
   begin
      case Item.Union.Kind is
         when base | reference =>
            return Short_Name (Model, Item);

         when an_array =>
            declare
               Element_Fallback : constant VSS.Strings.Virtual_String :=
                 Fallback & "_Item";

               Result : constant VSS.Strings.Virtual_String := Short_Name
                 (Model, Item.Union.an_array.element.Value, Element_Fallback);
            begin
               if Result.Starts_With (Element_Fallback) then
                  return Fallback;
               else
                  return Short_Name (Model, Item);
               end if;
            end;
         when a_or =>
            declare
               List : constant LSP_Gen.Entities.AType_Vector :=
                 Item.Union.a_or.items;

               Mapping : constant Mappings.Or_Mapping :=
                 Get_Or_Mapping (Model, List);
            begin
               case Mapping.Kind is
                  when Type_Union
                     | Two_Literals
                     | Enumeration
                     | Unknown_Mapping =>

                     return Fallback;

                  when Option_Combination =>
                     return Short_Name (Model, Mapping.Tipe, Fallback);

                  when others =>
                     return Short_Name (Model, Item);
               end case;
            end;

         when tuple =>
            return Short_Name (Model, Item);

         when literal =>
            return Fallback;

         when others =>
            return Fallback;
      end case;
   end Short_Name;

   ----------------
   -- Short_Name --
   ----------------

   function Short_Name (Item : LSP_Gen.Entities.MapKeyType)
     return VSS.Strings.Virtual_String is
   begin
      case Item.Union.Kind is
         when LSP_Gen.Entities.Enum.base =>
            case Item.Union.base.name is
               when LSP_Gen.Entities.Enum.DocumentUri =>
                  return "DocumentUri";
               when others =>
                  raise Program_Error;
            end case;

         when LSP_Gen.Entities.Enum.reference =>
            return Item.Union.reference.name;
      end case;
   end Short_Name;

   -----------------
   -- Vector_Name --
   -----------------

   function Vector_Name
     (Model : LSP_Gen.Meta_Models.Meta_Model;
      Item  : VSS.Strings.Virtual_String)
     return VSS.Strings.Virtual_String
       is (Item &
           (if Model.Is_Enumeration (Item) and Item /= "MarkupKind"
            then Constants.Set
            else Constants.Vector));

   ----------------------------------
   -- Write_Document_Symbol_Vector --
   ----------------------------------

   procedure Write_Private_Part is
      procedure Write_Vector (Item : VSS.Strings.Virtual_String);
      procedure Write_Vector_Impl (Item : VSS.Strings.Virtual_String);

      ------------------
      -- Write_Vector --
      ------------------

      procedure Write_Vector (Item : VSS.Strings.Virtual_String) is
      begin
         Put ("function Length (Self : ");
         Put (Item);
         Put_Line ("_Vector) return Natural;");
         New_Line;
         Put ("procedure Clear (Self : in out ");
         Put (Item);
         Put_Line ("_Vector);");
         New_Line;

         Put ("procedure Append (Self : in out ");
         Put (Item);
         Put_Line ("_Vector;");
         Put ("Value : ");
         Put (Item);
         Put_Line (");");
         New_Line;

         Put ("type ");
         Put (Item);
         Put ("_Variable_Reference");
         Put (" (Element : not null access ");
         Put (Item);
         Put_Line (")");
         Put_Line ("is null record with Implicit_Dereference => Element;");
         New_Line;

         Put ("function Get_");
         Put (Item);
         Put_Line ("_Variable_Reference");
         Put ("(Self  : aliased in out ");
         Put (Item);
         Put_Line ("_Vector;");
         Put_Line ("Index : Positive)");
         Put ("return ");
         Put (Item);
         Put_Line ("_Variable_Reference with Inline;");
         New_Line;

         Put ("type ");
         Put (Item);
         Put ("_Constant_Reference");
         Put (" (Element : not null access constant ");
         Put (Item);
         Put_Line (")");
         Put_Line ("is null record with Implicit_Dereference => Element;");
         New_Line;

         Put ("function Get_");
         Put (Item);
         Put_Line ("_Constant_Reference");
         Put ("(Self  : aliased ");
         Put (Item);
         Put_Line ("_Vector;");
         Put_Line ("Index : Positive)");
         Put ("return ");
         Put (Item);
         Put_Line ("_Constant_Reference with Inline;");
         New_Line;
      end Write_Vector;

      -----------------------
      -- Write_Vector_Impl --
      -----------------------

      procedure Write_Vector_Impl (Item : VSS.Strings.Virtual_String) is
      begin
         Put ("type ");
         Put (Item);
         Put ("_Array is array (Positive range <>) of aliased ");
         Put (Item);
         Put_Line (";");
         Put ("type ");
         Put (Item);
         Put ("_Array_Access is access all ");
         Put (Item);
         Put_Line ("_Array;");
         New_Line;

         Put ("type ");
         Put (Item);
         Put ("_Vector is ");
         Put_Line ("new Ada.Finalization.Controlled with record");
         Put ("Data : ");
         Put (Item);
         Put_Line ("_Array_Access;");
         Put_Line ("end record;");
         New_Line;

         Put ("overriding procedure Finalize (Self : in out ");
         Put (Item);
         Put_Line ("_Vector);");
         Put ("overriding procedure Adjust (Self : in out ");
         Put (Item);
         Put_Line ("_Vector);");
         New_Line;
      end Write_Vector_Impl;

   begin
      Write_Vector ("DocumentSymbol");
      Write_Vector ("SelectionRange");

      Put_Line
        ("function Is_Set (Self : SelectionRange_Optional) return Boolean;");
      Put ("function Value (Self : SelectionRange_Optional) ");
      Put_Line ("return SelectionRange;");
      Put ("procedure Set (Self : in out SelectionRange_Optional; ");
      Put_Line ("Value : SelectionRange);");
      Put_Line ("procedure Clear (Self : in out SelectionRange_Optional);");
      New_Line;

      Put_Line ("private");
      New_Line;
      Write_Vector_Impl ("DocumentSymbol");
      Write_Vector_Impl ("SelectionRange");

      Put_Line ("type SelectionRange_Access is access all SelectionRange;");

      Put ("type SelectionRange_Optional is ");
      Put_Line ("new Ada.Finalization.Controlled with record");
      Put_Line ("   Value : SelectionRange_Access;");
      Put_Line ("end record;");

      Put ("overriding procedure Finalize (Self : in out ");
      Put_Line ("SelectionRange_Optional);");
      Put ("overriding procedure Adjust (Self : in out ");
      Put_Line ("SelectionRange_Optional);");

   end Write_Private_Part;

   -----------------------
   -- Write_Enumeration --
   -----------------------

   procedure Write_Enumeration
     (Name : VSS.Strings.Virtual_String;
      List : LSP_Gen.Entities.AType_Vector) is
   begin
      Put ("type ");
      Put (Name);
      Put (" is (");

      for J in 1 .. List.Length loop
         if J > 1 then
            Put (", ");
         end if;

         Put (List (J).Union.stringLiteral.value);
      end loop;

      Put_Line (");");
   end Write_Enumeration;

   -------------------------
   -- Write_Optional_Type --
   -------------------------

   procedure Write_Optional_Type (Name : VSS.Strings.Virtual_String) is
   begin
      if Name = "LSPAny" then
         Put_Line ("subtype LSPAny_Optional is LSPAny;");
         New_Line;
      elsif Name = "SelectionRange" then
         null;
      else
         Put ("type ");
         Put_Id (Name);
         Put_Line ("_Optional (Is_Set : Boolean := False) is record");
         Put_Line ("case Is_Set is");
         Put_Line ("when False =>");
         Put_Line ("null;");
         Put_Line ("when True =>");
         Put ("Value : ");
         Put_Id (Name);
         Put_Line (";");
         Put_Line ("end case;");
         Put_Line ("end record;");
         New_Line;
      end if;
   end Write_Optional_Type;

   ------------------------
   -- Write_Or_Null_Type --
   ------------------------

   procedure Write_Or_Null_Type
     (Name : VSS.Strings.Virtual_String;
      List : LSP_Gen.Entities.AType_Vector)
   is
      Last     : constant LSP_Gen.Entities.AType := List (List.Length);
      Has_Null : constant Boolean := Last.Union.Kind = base
        and then Last.Union.base.name = LSP_Gen.Entities.Enum.a_null;
   begin
      if List.Length = 2 and then
        Has_Null and then
        List (1).Union.Kind in
          LSP_Gen.Entities.Enum.base | LSP_Gen.Entities.Enum.reference
      then
         Put ("type ");
         Put (Name);
         Put_Line (" (Is_Null : Boolean := True) is record");
         Put_Line ("case Is_Null is");
         Put_Line ("when True =>");
         Put_Line ("null;");
         Put_Line ("when False =>");
         Put ("Value : ");
         if List (1).Union.Kind in LSP_Gen.Entities.Enum.base then
            Put (Base_Full_Name (List (1).Union.base.name));
         else
            Put_Id (List (1).Union.reference.name);
         end if;
         Put_Line (";");
         Put_Line ("end case;");
         Put_Line ("end record;");
      end if;
   end Write_Or_Null_Type;

   ----------------------
   -- Write_Properties --
   ----------------------

   procedure Write_Properties
     (List        : LSP_Gen.Entities.Property_Vector;
      Is_Optional : Boolean := False;
      Done        : Dependency_Map) is
   begin
      for J in 1 .. List.Length loop
         Write_Property (List (J), Is_Optional, Done);
      end loop;
   end Write_Properties;

   --------------------
   -- Write_Property --
   --------------------

   procedure Write_Property
     (Item        : LSP_Gen.Entities.Property;
      Is_Optional : Boolean := False;
      Done        : Dependency_Map) is
   begin
      if Item.name = "kind" and then
        Item.a_type.Union.Kind = stringLiteral
      then
         --  Don't generate tag for extended types
         return;
      end if;

      Put_Id (Item.name);
      Put (" : ");

      if Item.a_type.Union.Kind not in an_array | map and
        (Item.optional or Is_Optional)
      then
         Put (Done (Item.a_type).Short_Name);
         Put ("_Optional");
      else
         Put (Done (Item.a_type).Full_Name);
      end if;

      Put_Line (";");
      Put_Lines (Item.documentation.Split_Lines, "   --  ");
      New_Line;
   end Write_Property;

   ------------------------
   -- Write_Two_Literals --
   ------------------------

   procedure Write_Two_Literals
     (Name   : VSS.Strings.Virtual_String;
      Base   : LSP_Gen.Entities.AType;
      Extend : LSP_Gen.Entities.AType;
      Done   : Dependency_Map)
   is
      Base_List : constant LSP_Gen.Entities.Property_Vector :=
        Base.Union.literal.value.properties;
      Extend_List : constant LSP_Gen.Entities.Property_Vector :=
        Extend.Union.literal.value.properties;
   begin
      --  This kind of `or` type used just once. In th usage one of literal
      --  extends another one literal. Let's generate it as a single record
      --  type with all added properties are optional.
      Put ("type ");
      Put (Name);
      Put_Line (" is record");

      for J in 1 .. Extend_List.Length loop
         Write_Property
           (Extend_List (J),
            Is_Optional => not
              (for some K in 1 .. Base_List.Length =>
                 Base_List (K).name = Extend_List (J).name),
            Done        => Done);
      end loop;

      Put_Line ("end record;");
   end Write_Two_Literals;

   ---------------------
   -- Write_Two_Types --
   ---------------------

   procedure Write_Two_Types
     (Model : LSP_Gen.Meta_Models.Meta_Model;
      Done  : Dependency_Map;
      Name  : VSS.Strings.Virtual_String;
      Map   : LSP_Gen.Mappings.Or_Mapping)
   is
      First_Name : constant VSS.Strings.Virtual_String :=
        Done (Map.First).Short_Name;
      Second_Name : constant VSS.Strings.Virtual_String :=
        Done (Map.Second).Short_Name;
      Second_Full : constant VSS.Strings.Virtual_String :=
        Done (Map.Second).Full_Name;
   begin
      Put ("type ");
      Put (Name);
      Put (" (Is_");
      Put (First_Name);
      Put_Line (" : Boolean := True) is record");
      Put ("case Is_");
      Put (First_Name);
      Put_Line (" is");
      Put_Line ("   when True =>");
      Put (First_Name);
      Put (": ");
      Write_Type_Name (Model, Map.First, False);
      Put_Line (";");
      Put_Line ("   when False =>");
      Put (Second_Name);
      Put (": ");
      Put (Second_Full);
      Put_Line (";");
      Put_Line ("end case;");
      Put_Line ("end record;");
   end Write_Two_Types;

   ----------------
   -- Write_Type --
   ----------------

   procedure Write_Type
     (Model    : LSP_Gen.Meta_Models.Meta_Model;
      Name     : VSS.Strings.Virtual_String;
      Item     : LSP_Gen.Entities.AType;
      Fallback : VSS.Strings.Virtual_String;
      Done     : Dependency_Map) is
   begin
      case Item.Union.Kind is
         when base =>
            case Item.Union.base.name is
               when LSP_Gen.Entities.Enum.string =>
                  Put ("type ");
                  Put_Id (Name);
                  Put (" is new ");
                  Put_Line ("VSS.Strings.Virtual_String with null record;");
               when LSP_Gen.Entities.Enum.a_null =>
                  Put ("type ");
                  Put_Id (Name);
                  Put_Line (" is null record;");
               when others =>
                  null;
            end case;
         when an_array =>
            declare
               Element : constant VSS.Strings.Virtual_String :=
                 Short_Name
                   (Model,
                    Item.Union.an_array.element.Value,
                    Fallback & "_Item");
            begin
               if Name /= "LSPAny_Vector"
                 and Name /= "Virtual_String_Vector"
               then
                  Write_Vector_Type (Model, Name, Element);
               end if;
            end;
         when map =>
            declare
               Element : constant VSS.Strings.Virtual_String := Short_Name
                 (Model, Item.Union.map.value.Value, Fallback & "_Item");
            begin
               Put ("package ");
               Put (Element);
               Put ("_Maps is new Ada.Containers.Hashed_Maps (");
               --
               Put (Short_Name (Item.Union.map.key));
               Put (", ");
               Put (Element);
               Put (", Get_Hash, ""=""");
               Put_Line (");"); New_Line;

               Put ("type ");
               Put_Id (Name);
               Put (" is new ");
               Put (Element);
               Put_Line ("_Maps.Map with null record;");
            end;
         when a_or =>
            declare
               Map : constant Mappings.Or_Mapping :=
                 Get_Or_Mapping (Model, Item.Union.a_or.items);
            begin
               if Name = "LSPAny" or Name = "LSPArray" then
                  Put ("type ");
                  Put_Id (Name);
                  Put_Line
                    (" is new JSON_Event_Vectors.Vector with null record;");
               else
                  case Map.Kind is
                     when Type_Union =>
                        Write_Union (Model, Done, Name, Map.Items);

                     when Type_Or_Null =>
                        Write_Or_Null_Type (Name, Item.Union.a_or.items);

                     when Type_Or_Something =>

                        Write_Type_Or_Something (Model, Done, Name, Map);

                     when Two_Types
                      | String_Or_Tuple =>

                        Write_Two_Types (Model, Done, Name, Map);

                     when Option_Combination =>
                        Put ("type ");
                        Put_Id (Name);
                        Put_Line (" is record");
                        Write_Properties
                          (Map.Tipe.Union.literal.value.properties,
                           True,
                           Done);
                        Put_Line ("end record;");
                     when Two_Literals =>
                        Write_Two_Literals (Name, Map.First, Map.Second, Done);

                     when Enumeration =>
                        Write_Enumeration (Name, Map.Items);

                     when Array_Or_Null | Type_Or_Array =>
                        if Name /= Short_Name (Model, Map.Array_Type) then
                           Put ("subtype ");
                           Put_Id (Name);
                           Put (" is ");
                           Put (Short_Name (Model, Map.Array_Type));
                           Put_Line (";");
                        end if;

                     when Unknown_Mapping =>
                        Write_Union (Model, Done, Name, Item.Union.a_or.items);

                     when others =>
                        null;
                  end case;
               end if;
            end;
         when tuple =>
            Put ("type ");
            Put (Name);
            Put (" is array (1 .. ");
            Put (Item.Union.tuple.items.Length);
            Put (") of ");
            Put (Short_Name (Model, Item.Union.tuple.items (1)));
            Put_Line (";");

         when literal =>
            Put ("type ");
            Put_Id (Name);
            Put_Line (" is record");
            Write_Properties
              (Item.Union.literal.value.properties, Done => Done);
            Put ("end record;");

         when reference =>
            --  For typeAlias emit subtype as type renaming
            Put ("subtype ");
            Put_Id (Name);
            Put (" is ");
            Put_Id (Item.Union.reference.name);
            Put_Line (";");

         when stringLiteral =>
            null;

         when others =>
            raise Program_Error;
      end case;
   end Write_Type;

   ---------------------
   -- Write_Type_Name --
   ---------------------

   procedure Write_Type_Name
     (Model       : LSP_Gen.Meta_Models.Meta_Model;
      Item        : LSP_Gen.Entities.AType;
      Is_Optional : Boolean) is
   begin
      case Item.Union.Kind is
         when base =>
            if Is_Optional then
               Put (Base_Short_Name (Item.Union.base.name));
               Put ("_Optional");
            else
               Put (Base_Full_Name (Item.Union.base.name));
            end if;
         when reference =>
            if not Model.Is_Enumeration (Item.Union.reference.name)
            then
               Put ("LSP.Structures.");
            elsif not Is_Optional then
               Put ("LSP.Enumerations.");
            end if;

            if Is_Optional then
               Put (Item.Union.reference.name);
               Put ("_Optional");
            else
               Put_Id (Item.Union.reference.name);
            end if;
         when an_array =>
            --  Could be Is_Optional!!!
            Put ("LSP.Structures.");
            Put (Short_Name (Model, Item));
         when map =>
            --  Could be Is_Optional!!!
            Write_Type_Name
              (Model, Item.Union.map.value.Value, False);
            Put ("_Maps.Map");
         when a_or | tuple =>
            Put (Short_Name (Model, Item));
         when others =>
            raise Program_Error;
      end case;
   end Write_Type_Name;

   -----------------------------
   -- Write_Type_Or_Something --
   -----------------------------

   procedure Write_Type_Or_Something
     (Model : LSP_Gen.Meta_Models.Meta_Model;
      Done  : Dependency_Map;
      Name  : VSS.Strings.Virtual_String;
      Map   : LSP_Gen.Mappings.Or_Mapping)
   is
      First_Name : constant VSS.Strings.Virtual_String :=
        Done (Map.First).Short_Name;
      Second_List : constant LSP_Gen.Entities.Property_Vector :=
        Map.Second.Union.literal.value.properties;
   begin
      Put ("type ");
      Put (Name);
      Put (" (Is_");
      Put (First_Name);
      Put_Line (" : Boolean := True) is record");
      Put ("case Is_");
      Put (First_Name);
      Put_Line (" is");
      Put_Line ("   when True =>");
      Put (First_Name);
      Put (": ");
      Write_Type_Name (Model, Map.First, False);
      Put_Line (";");
      Put_Line ("   when False =>");

      Write_Properties (Second_List, False, Done);

      Put_Line ("end case;");
      Put_Line ("end record;");
   end Write_Type_Or_Something;

   -----------------
   -- Write_Types --
   -----------------

   procedure Write_Types
     (Model : LSP_Gen.Meta_Models.Meta_Model;
      Done  : out LSP_Gen.Dependencies.Dependency_Map)
   is

      procedure Write_References
        (List : Meta_Models.Type_Dependency;
         Done : in out Dependency_Map);
      procedure Write_Mixin
        (Item : LSP_Gen.Entities.Structure;
         Done : in out Dependency_Map);
      procedure Write_Structure
        (Name : VSS.Strings.Virtual_String;
         Done : in out Dependency_Map);
      procedure Write_Type_Alias
        (Item : LSP_Gen.Entities.TypeAlias;
         Done : in out Dependency_Map);

      ----------------------
      -- Write_References --
      ----------------------

      procedure Write_References
        (List : Meta_Models.Type_Dependency;
         Done : in out Dependency_Map)
      is
         procedure Write_Reference
           (Name       : VSS.Strings.Virtual_String;
            Has_Option : Boolean);

         ---------------------
         -- Write_Reference --
         ---------------------

         procedure Write_Reference
           (Name       : VSS.Strings.Virtual_String;
            Has_Option : Boolean)
         is
            Ref  : constant LSP_Gen.Entities.AType := To_Reference (Name);
            Item : constant Meta_Models.Top_Type := Model.Get (Name);
         begin
            case Item.Kind is
               when Meta_Models.Structure =>
                  Write_Structure (Name, Done);
               when Meta_Models.Type_Alias =>
                  Write_Type_Alias (Item.Type_Alias, Done);
               when Meta_Models.Enumeration =>
                  null;
            end case;

            if Has_Option and then not Done (Ref).Has_Option then
               Done (Ref).Has_Option := True;
               Write_Optional_Type (Name);
            end if;
         end Write_Reference;

      begin
         for Name of List.Required loop
            Write_Reference (Name, List.Optional.Contains (Name));
         end loop;
      end Write_References;

      -----------------
      -- Write_Mixin --
      -----------------

      procedure Write_Mixin
        (Item : LSP_Gen.Entities.Structure;
         Done : in out Dependency_Map)
      is
         Name : constant VSS.Strings.Virtual_String := Item.name;
         Ref  : constant LSP_Gen.Entities.AType := To_Reference (Name);
      begin
         if Done.Contains (Ref) then
            return;
         end if;

         Done.Insert
           (Ref,
            (Short_Name => Mappings.Ada_Id (Name),
             Full_Name  => "LSP.Structures." & Mappings.Ada_Id (Name),
             Has_Option => False,
             Is_Message => False,
             Owner      => ""));

         Write_References (Model.Dependency (Name), Done);

         Put ("type ");
         Put_Id (Item.name);
         Put_Line (" is interface;");
         Put_Lines (Item.documentation.Split_Lines, "   --  ");
         New_Line;

         for J in 1 .. Item.properties.Length loop
            declare
               Property : constant LSP_Gen.Entities.Property :=
                 Item.properties (J);
            begin
               Put ("function ");
               Put_Id (Property.name);
               Put (" (Self : ");
               Put_Id (Item.name);
               Put (") return ");
               Write_Type_Name (Model, Property.a_type, Property.optional);
               Put_Line (" is abstract;");
               Put_Lines (Property.documentation.Split_Lines, "   --  ");
               New_Line;
            end;
         end loop;
      end Write_Mixin;

      ---------------------
      -- Write_Structure --
      ---------------------

      procedure Write_Structure
        (Name  : VSS.Strings.Virtual_String;
         Done  : in out Dependency_Map)
      is

         procedure Write_Mixins
           (Item   : LSP_Gen.Entities.Structure;
            Prefix : VSS.Strings.Virtual_String);

         procedure Write_Mixin_Propeties (Item : LSP_Gen.Entities.Structure);
         procedure Write_Mixin_Functions (Item : LSP_Gen.Entities.Structure);

         ---------------------------
         -- Write_Mixin_Functions --
         ---------------------------

         procedure Write_Mixin_Functions (Item : LSP_Gen.Entities.Structure) is
         begin
            for K in 1 .. Item.mixins.Length loop
               declare
                  Mixin  : constant LSP_Gen.Entities.AType := Item.mixins (K);
                  Parent : constant LSP_Gen.Entities.Structure :=
                    Model.Structure (Mixin.Union.reference.name);
               begin
                  for J in 1 .. Parent.properties.Length loop
                     declare
                        Property : constant LSP_Gen.Entities.Property :=
                          Parent.properties (J);
                     begin
                        Put ("overriding function ");
                        Put_Id (Property.name);
                        Put (" (Self : ");
                        Put_Id (Item.name);
                        Put (") return ");
                        Write_Type_Name
                          (Model, Property.a_type, Property.optional);
                        Put (" is (Self.");
                        Put_Id (Property.name);
                        Put_Line (");");
                        New_Line;
                     end;
                  end loop;
               end;
            end loop;
         end Write_Mixin_Functions;

         ---------------------------
         -- Write_Mixin_Propeties --
         ---------------------------

         procedure Write_Mixin_Propeties (Item : LSP_Gen.Entities.Structure) is
         begin
            for J in 1 .. Item.mixins.Length loop
               declare
                  Mixin  : constant LSP_Gen.Entities.AType := Item.mixins (J);
                  Parent : constant LSP_Gen.Entities.Structure :=
                    Model.Structure (Mixin.Union.reference.name);
               begin
                  Write_Properties (Parent.properties, False, Done);
               end;
            end loop;
         end Write_Mixin_Propeties;

         ------------------
         -- Write_Mixins --
         ------------------

         procedure Write_Mixins
           (Item   : LSP_Gen.Entities.Structure;
            Prefix : VSS.Strings.Virtual_String) is
         begin
            for J in 1 .. Item.mixins.Length loop
               if J = 1 then
                  Put (Prefix);
               else
                  Put (" and ");
               end if;

               Put_Id (Item.mixins (J).Union.reference.name);
               New_Line;
            end loop;
         end Write_Mixins;

         Item : constant LSP_Gen.Entities.Structure := Model.Structure (Name);
         Ref  : constant LSP_Gen.Entities.AType := To_Reference (Name);

         Ada_Id : constant VSS.Strings.Virtual_String :=
           Mappings.Ada_Id (Name);
      begin
         if Done.Contains (Ref) then
            return;
         end if;

         Done.Insert
           (Ref,
            (Short_Name => Ada_Id,
             Full_Name  => "LSP.Structures." & Ada_Id,
             Has_Option => False,
             Is_Message => (for some X of Work_Done_List =>
                              "WorkDone" & X = Ada_Id),
             Owner      => ""));

         Write_References (Model.Dependency (Name), Done);
         Emit_Dependence
           (Model, Item.properties, Name, Name, Done, Enclosing => Name);

         Put ("type ");
         Put_Id (Name);
         Put_Line (" is ");

         if Name = "LSPObject" then
            Put ("new LSPAny with ");
         elsif Item.extends.Length > 0 then
            Put ("new ");
            Put_Id
              (Item.extends
                 (Mappings.Base_Index (Item.extends)).Union.reference.name);

            Write_Mixins (Item, " and ");
            Put (" with ");
         elsif Item.mixins.Length > 0 then
            Write_Mixins (Item, "new ");
            Put (" with ");
         elsif Model.Is_Tagged (Name) then
            Put ("tagged ");
         end if;

         Put_Line ("record");

         Write_Mixin_Propeties (Item);

         if Item.extends.Length > 1 then
            pragma Assert (Item.extends.Length = 2);

            for J in 1 .. 2 loop
               if J /= Mappings.Base_Index (Item.extends) then
                  Put ("Parent : ");
                  Write_Type_Name (Model, Item.extends (J), False);
                  Put_Line (";");
               end if;
            end loop;

            if Item.properties.Length > 0 then
               Write_Properties (Item.properties, Done => Done);
            end if;
         else
            Write_Properties (Item.properties, Done => Done);

            if Item.properties.Length = 0 and Item.mixins.Length = 0 then
               Put_Line ("null;");
            end if;
         end if;

         Put_Line ("end record;");
         Put_Lines (Item.documentation.Split_Lines, "   --  ");
         New_Line;
         Write_Mixin_Functions (Item);
      end Write_Structure;

      ----------------------
      -- Write_Type_Alias --
      ----------------------

      procedure Write_Type_Alias
        (Item : LSP_Gen.Entities.TypeAlias;
         Done : in out Dependency_Map)
      is
         Name : constant VSS.Strings.Virtual_String := Item.name;
         Ref  : constant LSP_Gen.Entities.AType := To_Reference (Name);
      begin
         if Done.Contains (Ref) then
            return;
         end if;

         Done.Insert
           (Ref,
            (Short_Name => Mappings.Ada_Id (Name),
             Full_Name  => "LSP.Structures." & Mappings.Ada_Id (Name),
             Has_Option => False,
             Is_Message => False,
             Owner      => ""));

         Write_References (Model.Dependency (Name), Done);

         Emit_Dependence (Model, Item.a_type, "", Name, Done, Name);

         Write_Type (Model, Name, Item.a_type, Name, Done);

         Put_Lines (Item.documentation.Split_Lines, "   --  ");
         New_Line;

         if Item.a_type.Union.Kind = base and then
           Item.a_type.Union.base.name = LSP_Gen.Entities.Enum.string
         then
            Put ("function Get_Hash (Self : ");
            Put (Name);
            Put (") return");
            Put_Line (" Ada.Containers.Hash_Type is");
            Put_Line (" (Ada.Containers.Hash_Type'Mod (Self.Hash));");
            New_Line;
         end if;

         if Name = "LSPAny" then
            Put_Line ("subtype LSPAny_Vector is LSPAny;");
            New_Line;
         end if;
      end Write_Type_Alias;

      Enums   : constant VSS.String_Vectors.Virtual_String_Vector :=
        Model.Enumerations;

      List    : constant VSS.String_Vectors.Virtual_String_Vector :=
        Model.Structures;
   begin
      --  Put Enums into Done
      for Name of Enums loop
         Done.Insert
           (To_Reference (Name),
            (LSP_Gen.Mappings.Ada_Id (Name),
             "LSP.Enumerations." & LSP_Gen.Mappings.Ada_Id (Name),
             False, False,
             VSS.Strings.Empty_Virtual_String));
      end loop;

      Put_Lines (Model.License_Header, "--  ");
      New_Line;
      Put_Line ("with Ada.Containers.Hashed_Maps;");
      Put_Line ("with Ada.Containers.Vectors;");
      Put_Line ("with Ada.Finalization;"); New_Line;
      Put_Line ("with VSS.JSON.Streams;");
      Put_Line ("with VSS.Strings;"); New_Line;
      Put_Line ("with VSS.String_Vectors;"); New_Line;
      Put_Line ("with LSP.Enumerations; use LSP.Enumerations;"); New_Line;

      Put_Line ("package LSP.Structures is");
      Put_Line ("   pragma Preelaborate;"); New_Line;

      Put_Line ("subtype Virtual_String is VSS.Strings.Virtual_String;");
      Put_Line
        ("subtype Virtual_String_Optional is VSS.Strings.Virtual_String;");
      Put ("subtype Virtual_String_Vector is");
      Put_Line (" VSS.String_Vectors.Virtual_String_Vector;");
      New_Line;

      Put ("type DocumentUri");
      Put_Line (" is new VSS.Strings.Virtual_String with null record;");
      New_Line;
      Put ("function Get_Hash (Self : DocumentUri) return");
      Put_Line (" Ada.Containers.Hash_Type is");
      Put_Line (" (Ada.Containers.Hash_Type'Mod (Self.Hash));");
      New_Line;

      Put_Line ("package JSON_Event_Vectors is new Ada.Containers.Vectors");
      Put ("(Positive, VSS.JSON.Streams.JSON_Stream_Element, ");
      Put_Line ("VSS.JSON.Streams.""="");");
      New_Line;

      Put_Line ("subtype Boolean_Or_Any is JSON_Event_Vectors.Vector;");
      New_Line;

      Put_Line ("type SelectionRange_Optional is tagged private;");
      New_Line;

      declare
         String : constant LSP_Gen.Entities.AType :=
           (Union => (base, (name => LSP_Gen.Entities.Enum.string)));

         String_Array : constant LSP_Gen.Entities.AType := Make_Array (String);
      begin
         Done.Insert
           (String,
            (Base_Short_Name (LSP_Gen.Entities.Enum.string),
             Base_Full_Name (LSP_Gen.Entities.Enum.string),
             True, False, ""));

         Done.Insert
           (String_Array,
            ("Virtual_String_Vector",
             "LSP.Structures.Virtual_String_Vector",
             True, False, ""));
      end;

      Done.Insert
        ((Union => (reference, (name => "LSPArray"))),
         ("LSPArray",
          "LSP.Structures.LSPArray",
          True, False, ""));

      Done.Insert
        ((Union => (base, (name => LSP_Gen.Entities.Enum.a_boolean))),
         (Base_Short_Name (LSP_Gen.Entities.Enum.a_boolean),
          Base_Full_Name (LSP_Gen.Entities.Enum.a_boolean),
          True, False, ""));
      Done.Insert
        ((Union => (base, (name => LSP_Gen.Entities.Enum.integer))),
         (Base_Short_Name (LSP_Gen.Entities.Enum.integer),
          Base_Full_Name (LSP_Gen.Entities.Enum.integer),
          True, False, ""));
      Done.Insert
        ((Union => (base, (name => LSP_Gen.Entities.Enum.uinteger))),
         (Base_Short_Name (LSP_Gen.Entities.Enum.uinteger),
          Base_Full_Name (LSP_Gen.Entities.Enum.uinteger),
          True, False, ""));

      Write_Optional_Type ("Boolean");
      Write_Optional_Type ("Natural");
      Write_Optional_Type ("Integer");

      for Name of List loop
         if Model.Is_Mixin (Name) then
            Write_Mixin (Model.Structure (Name), Done);
         end if;
      end loop;

      for Name of List loop
         Write_Structure (Name, Done);
      end loop;

      for Name of Model.Type_Aliases loop
         Write_Type_Alias (Model.Type_Alias (Name), Done);
      end loop;

      for Method of Model.Requests loop
         declare
            Request : constant LSP_Gen.Entities.Request :=
              Model.Request (Method);
            Name    : constant VSS.Strings.Virtual_String :=
              Model.Message_Name (Method) & "_Result";
            Partial : constant VSS.Strings.Virtual_String :=
              Model.Message_Name (Method) & "_Progress_Report";
         begin
            Emit_Dependence_Type
              (Model    => Model,
               Item     => Request.result,
               Skip     => VSS.Strings.Empty_Virtual_String,
               Owner    => "",
               Done     => Done,
               Fallback => Name);

            if Request.partialResult.Is_Set then
               Emit_Dependence_Type
                 (Model    => Model,
                  Item     => Request.partialResult.Value,
                  Skip     => VSS.Strings.Empty_Virtual_String,
                  Owner    => "",
                  Done     => Done,
                  Fallback => Partial);

               Done (Request.partialResult.Value).Is_Message := True;
            end if;

            Done (Request.result).Is_Message := True;

            if Request.params.Is_Set
              and then Request.params.Value.Union.Kind /= an_and  --  FIXME?
            then
               Done (Request.params.Value).Is_Message := True;
            end if;
         end;
      end loop;

      for Method of Model.Notifications loop
         declare
            Notification : constant LSP_Gen.Entities.Notification :=
              Model.Notification (Method);
         begin
            if Notification.params.Is_Set then
               Done (Notification.params.Value).Is_Message := True;
            end if;
         end;
      end loop;

      Done (To_Reference ("ErrorCodes")).Is_Message := True;
      Done (To_Reference ("ProgressToken")).Is_Message := True;

      Write_Private_Part;
      Put_Line ("end LSP.Structures;");
   end Write_Types;

   -----------------
   -- Write_Union --
   -----------------

   procedure Write_Union
     (Model    : LSP_Gen.Meta_Models.Meta_Model;
      Done     : Dependency_Map;
      Name     : VSS.Strings.Virtual_String;
      List     : LSP_Gen.Entities.AType_Vector)
   is
      Variants : VSS.String_Vectors.Virtual_String_Vector;
   begin
      for J in 1 .. List.Length loop
         Variants.Append (Model.Get_Variant (List (J), J));
      end loop;

      Put ("type ");
      Put (Name);
      Put ("_Variant is (");

      for J in 1 .. Variants.Length loop
         if J > 1 then
            Put (", ");
         end if;

         Put_Id (Variants (J));
      end loop;
      Put_Line (");");
      New_Line;

      Put ("type ");
      Put (Name);
      Put (" (Kind : ");
      Put (Name);
      Put ("_Variant := ");
      Put (Name);
      Put_Line ("_Variant'First) is record");
      Put_Line ("case Kind is");

      for J in 1 .. Variants.Length loop
         Put ("when ");
         Put_Id (Variants (J));
         Put_Line (" =>");
         Put_Id (Variants (J));
         Put (" : ");
         Put (Done (List (J)).Full_Name);
         Put_Line (";");
      end loop;

      Put_Line ("end case;");
      Put_Line ("end record;");
   end Write_Union;

   -----------------------
   -- Write_Vector_Type --
   -----------------------

   procedure Write_Vector_Type
     (Model : LSP_Gen.Meta_Models.Meta_Model;
      Name  : VSS.Strings.Virtual_String;
      Item  : VSS.Strings.Virtual_String)
   is
      pragma Unreferenced (Model);
   begin
      if Name = "DocumentSymbol_Vector" or Name = "SelectionRange_Vector" then
         declare
            Item : constant VSS.Strings.Virtual_String := Name.Split ('_') (1);
         begin
            Put ("type ");
            Put (Name);
            Put_Line (" is tagged private with");
            Put ("Variable_Indexing => Get_");
            Put (Item);
            Put_Line ("_Variable_Reference,");
            Put ("Constant_Indexing => Get_");
            Put (Item);
            Put_Line ("_Constant_Reference;");
            New_Line;
            return;
         end;
      elsif Name.Ends_With ("_Set") then
         --  It looks like any enum array in LSP is a set. Let's define them
         --  as sets.

         Put ("type ");
         Put (Name);
         Put (" is array (");
         Put (Item);
         Put_Line (") of Boolean");
         Put_Line ("  with Pack, Default_Component_Value => False;");
         New_Line;
         return;
      end if;

      Put ("package ");
      Put (Item);
      Put ("_Vectors is new Ada.Containers.Vectors (Positive, ");
      Put_Id (Item);
      Put_Line (", ""="");");
      New_Line;

      Put ("type ");
      Put (Name);
      Put_Line (" is new ");
      Put (Item);
      Put_Line ("_Vectors.Vector with null record;");
      New_Line;
   end Write_Vector_Type;

end LSP_Gen.Structures;
