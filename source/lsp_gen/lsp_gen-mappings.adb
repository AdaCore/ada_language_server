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

with VSS.Characters;
with VSS.Strings.Cursors.Iterators.Characters;

with LSP_Gen.String_Sets;

package body LSP_Gen.Mappings is
   use all type LSP_Gen.Entities.Enum.AType_Variant;
   use type LSP_Gen.Entities.Enum.BaseTypes;
   use type VSS.Strings.Virtual_String;

   function Predefined_Equal (L, R : LSP_Gen.Entities.AType) return Boolean
     renames LSP_Gen.Entities."=";

   function "=" (Left, Right : LSP_Gen.Entities.AType) return Boolean;

   package String_Sets renames LSP_Gen.String_Sets;

   Keywords : constant String_Sets.Set :=
     ["abort", "abs", "abstract", "accept", "access", "aliased", "all", "and",
      "array", "at", "begin", "body", "case", "constant", "declare", "delay",
      "delta", "digits", "do", "else", "elsif", "end", "entry", "exception",
      "exit", "for", "function", "generic", "goto", "if", "in", "interface",
      "is", "limited", "loop", "mod", "new", "not", "null", "of", "or",
      "others", "out", "overriding", "package", "pragma", "private",
      "procedure", "protected", "raise", "range", "record", "rem", "renames",
      "requeue", "return", "reverse", "select", "separate", "some", "subtype",
      "synchronized", "tagged", "task", "terminate", "then", "type", "until",
      "use", "when", "while", "with", "xor",
      "boolean"];

   Base_Column_Name : constant array (LSP_Gen.Entities.Enum.BaseTypes)
     of VSS.Strings.Virtual_String :=
       (LSP_Gen.Entities.Enum.Uri => "URI",
        LSP_Gen.Entities.Enum.DocumentUri => "DocumentUri",
        LSP_Gen.Entities.Enum.integer => "Integer",
        LSP_Gen.Entities.Enum.uinteger => "Natural",
        LSP_Gen.Entities.Enum.decimal => "Float",
        LSP_Gen.Entities.Enum.RegExp => "???",
        LSP_Gen.Entities.Enum.string => "String",
        LSP_Gen.Entities.Enum.a_boolean => "Boolean",
        LSP_Gen.Entities.Enum.a_null => "Null_Record");

   --  Base_Full_Type_Name : constant array (LSP_Gen.Entities.Enum.BaseTypes)
   --    of VSS.Strings.Virtual_String :=
   --      (LSP_Gen.Entities.Enum.Uri => "LSP.Structures.URI",
   --       LSP_Gen.Entities.Enum.DocumentUri => "LSP.Structures.DocumentUri",
   --       LSP_Gen.Entities.Enum.integer => "Standard.Integer",
   --       LSP_Gen.Entities.Enum.uinteger => "Standard.Natural",
   --       LSP_Gen.Entities.Enum.decimal => "Standard.Float",
   --       LSP_Gen.Entities.Enum.RegExp => "???",
   --       LSP_Gen.Entities.Enum.string => "VSS.Strings.Virtual_String",
   --       LSP_Gen.Entities.Enum.a_boolean => "Standard.Boolean",
   --       LSP_Gen.Entities.Enum.a_null => "??!");

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

   ------------
   -- Ada_Id --
   ------------

   function Ada_Id (Text : VSS.Strings.Virtual_String)
     return VSS.Strings.Virtual_String
   is
      use type VSS.Characters.Virtual_Character;
      Keyword : constant VSS.Strings.Virtual_String := Text.To_Lowercase;
      Result  : VSS.Strings.Virtual_String;
   begin
      if Keywords.Contains (Keyword) then
         if Text.At_First_Character.Element =
           Keyword.At_First_Character.Element
         then
            Result.Append ("a");
         else
            Result.Append ("A");
         end if;

         if Keyword.At_First_Character.Element in
           'a' | 'e' | 'i' | 'o' | '_' --  | 'u'
         then
            Result.Append ("n");
         end if;

         Result.Append ("_");
      elsif Text.Starts_With ("_") then
         Result.Append ("An");
      end if;

      Result.Append (Text);
      return Result;
   end Ada_Id;

   ------------
   -- Ada_Id --
   ------------

   function Ada_Id (Self : LSP_Gen.Entities.EnumerationEntry)
     return VSS.Strings.Virtual_String
       is (Ada_Id (Self.name));

   ----------------
   -- Base_Index --
   ----------------

   function Base_Index
     (List : LSP_Gen.Entities.AType_Vector) return Positive
   is
      First : constant VSS.Strings.Virtual_String :=
        List (1).Union.reference.name;
   begin
      pragma Assert (List.Length in 1 .. 2);

      if List.Length = 1 or First /= "TextDocumentRegistrationOptions" then
         return 1;
      else
         return 2;
      end if;
   end Base_Index;

   -----------------
   -- Column_Name --
   -----------------

   function Column_Name (Self : LSP_Gen.Entities.AType)
     return VSS.Strings.Virtual_String is
   begin
      case Self.Union.Kind is
         when base =>
            return Base_Column_Name (Self.Union.base.name);
         when reference =>
            return Ada_Id (Self.Union.reference.name);
         when others =>
            raise Program_Error;
      end case;
   end Column_Name;

   --------------------
   -- Get_Or_Mapping --
   --------------------

   function Get_Or_Mapping
     (Model : LSP_Gen.Meta_Models.Meta_Model'Class;
      Items : LSP_Gen.Entities.AType_Vector) return Or_Mapping
   is
      function Same_Option
        (Left, Right : LSP_Gen.Entities.AType) return Boolean;
      --  items are the same except `optional` field

      -----------------
      -- Same_Option --
      -----------------

      function Same_Option
        (Left, Right : LSP_Gen.Entities.AType) return Boolean is
      begin
         if Left.Union.Kind /= literal or else
           Right.Union.Kind /= literal or else
           Left.Union.literal.value.properties.Length /=
             Right.Union.literal.value.properties.Length
         then
            return False;
         end if;

         for J in 1 .. Left.Union.literal.value.properties.Length loop
            declare
               L : constant LSP_Gen.Entities.Property :=
                 Left.Union.literal.value.properties (J);
               R : constant LSP_Gen.Entities.Property :=
                 Right.Union.literal.value.properties (J);
            begin
               if L.name /= R.name or else L.a_type /= R.a_type then
                  return False;
               end if;
            end;
         end loop;

         return True;
      end Same_Option;

      Last : constant LSP_Gen.Entities.AType := Items (Items.Length);

      Has_Null : constant Boolean := Last.Union.Kind in base
        and then Last.Union.base.name = LSP_Gen.Entities.Enum.a_null;
   begin
      --  Some base type and `null`
      if Items.Length = 2 and then
        Has_Null and then Items (1).Union.Kind in base | reference
      then
         return (Type_Or_Null, Items (1));
      end if;

      --  Some array type and `null`
      if Items.Length = 2 and then
        Has_Null and then Items (1).Union.Kind in an_array
      then
         return (Array_Or_Null, Items (1));
      end if;

      --  Some base/reference type or an array of it
      if Items.Length = 2 and then
        Items (1).Union.Kind in base | reference and then
        Items (2).Union.Kind in an_array and then
        Items (2).Union.an_array.element.Value = Items (1)
      then
         return (Type_Or_Array, Items (2));
      end if;

      --  Each type (except the first) has `kind` stringLiteral property
      if (for all J in 2 .. Items.Length => Has_Kind (Model, Items (J))) then
         return (Type_Union, Items);
      end if;

      --  All items are the same except `optional` field
      if (for all J in 2 .. Items.Length =>
           Same_Option (Items (1), Items (J)))
      then
         return (Option_Combination, Items (1));
      end if;

      --  A union of two types (base or reference)
      if Items.Length = 2 and then
        (for all J in 1 .. Items.Length => Items (J).Union.Kind in
            reference | base)
      then
         return (Two_Types, Items (1), Items (2));
      end if;

      --  A union of types X, Y, array Y
      if Items.Length = 3 and then
        (for all J in 1 .. 2 => Items (J).Union.Kind = reference) and then
        Items (3).Union.Kind = an_array and then
        Items (3).Union.an_array.element.Value = Items (2)
      then
         return (Two_Types, Items (1), Items (3));
      end if;

      --  A union of string and some array
      if Items.Length = 2 and then
        Items (1).Union.Kind = base and then
        Items (1).Union.base.name = LSP_Gen.Entities.Enum.string and then
        Items (2).Union.Kind = an_array
      then
         return (Two_Types, Items (1), Items (2));
      end if;

      --  A union of string and tuple
      if Items.Length = 2 and then
        Items (1).Union.Kind = base and then
        Items (1).Union.base.name = LSP_Gen.Entities.Enum.string and then
        Items (2).Union.Kind = tuple
      then
         return (String_Or_Tuple, Items (1), Items (2));
      end if;

      --  A union of boolean and an empty object
      if Items.Length = 2 and then
        Items (1).Union.Kind = base and then
        Items (1).Union.base.name = LSP_Gen.Entities.Enum.a_boolean and then
        Items (2).Union.Kind = literal and then
        Items (2).Union.literal.value.properties.Length = 0
      then
         return (Kind => Boolean_Or_Any);
      end if;

      --  All items are stringLiteral
      if (for all J in 1 .. Items.Length =>
            Items (J).Union.Kind = stringLiteral)
      then
         return (Enumeration, Items);
      end if;

      --  A union of a type and some literal
      if Items.Length = 2 and then
        Items (1).Union.Kind in base | reference and then
        Items (2).Union.Kind = literal
      then
         return (Type_Or_Something, Items (1), Items (2));
      end if;

      --  A union of two literals
      if Items.Length = 2 and then
        (for all J in 1 .. Items.Length => Items (J).Union.Kind = literal)
      then
         if Items (1).Union.literal.value.properties.Length <
           Items (2).Union.literal.value.properties.Length
         then
            return (Two_Literals, Items (1), Items (2));
         else
            return (Two_Literals, Items (2), Items (1));
         end if;
      end if;

      return (Kind => Unknown_Mapping);
   end Get_Or_Mapping;

   --------------
   -- Has_Kind --
   --------------

   function Has_Kind
     (Model : LSP_Gen.Meta_Models.Meta_Model'Class;
      Item  : LSP_Gen.Entities.AType) return Boolean
   is
      use type Meta_Models.Top_Type_Kind;

      Value : constant Meta_Models.Top_Type :=
        (if Item.Union.Kind = reference
         then Model.Get (Item.Union.reference.name)
         else (Meta_Models.Enumeration, Enumeration => <>));
   begin
      if Value.Kind = Meta_Models.Structure and then
        Value.Structure.extends.Length > 0 and then
        Has_Kind (Model, Value.Structure.extends (1))
      then
         return True;
      end if;

      return Value.Kind = Meta_Models.Structure
        and then Value.Structure.properties.Length > 0
        and then Value.Structure.properties (1).name = "kind"
        and then Value.Structure.properties (1).a_type.Union.Kind
          = stringLiteral;
   end Has_Kind;

end LSP_Gen.Mappings;
