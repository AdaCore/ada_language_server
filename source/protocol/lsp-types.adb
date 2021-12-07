------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2018-2021, AdaCore                     --
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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Hash;
with Ada.Strings.Wide_Wide_Fixed.Wide_Wide_Hash;
with Interfaces;

with VSS.JSON.Pull_Readers;
with VSS.Strings.Conversions;

with LSP.JSON_Streams;

with URIs;

package body LSP.Types is

   use type VSS.JSON.Pull_Readers.JSON_Event_Kind;

   function No_Any return LSP_Any is
     (GNATCOLL.JSON.JSON_Null with null record);

   function Empty return LSP_Any is
     (GNATCOLL.JSON.Create_Object with null record);

   --------------
   -- Assigned --
   --------------

   function Assigned (Id : LSP_Number_Or_String) return Boolean is
   begin
      return Id.Is_Number or else not Id.String.Is_Empty;
   end Assigned;

   -----------
   -- Equal --
   -----------

   function Equal (Left, Right : LSP_URI) return Boolean is
      use type VSS.Strings.Virtual_String;

   begin
      return Left.URI = Right.URI;
   end Equal;

   -----------------
   -- File_To_URI --
   -----------------

   function File_To_URI (File : String) return LSP.Types.LSP_URI is
      Result : constant URIs.URI_String :=
        URIs.Conversions.From_File (File);
   begin
      return (URI => VSS.Strings.Conversions.To_Virtual_String (Result));
   end File_To_URI;

   function File_To_URI (File : Ada.Strings.Unbounded.Unbounded_String)
     return LSP.Types.LSP_URI is
   begin
      return File_To_URI (Ada.Strings.Unbounded.To_String (File));
   end File_To_URI;

   ----------
   -- Hash --
   ----------

   function Hash
     (Item : VSS.Strings.Virtual_String) return Ada.Containers.Hash_Type is
   begin
      return
        Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Hash
          (VSS.Strings.Conversions.To_Unbounded_Wide_Wide_String (Item));
   end Hash;

   ----------
   -- Hash --
   ----------

   function Hash
     (Item : LSP.Types.LSP_Number_Or_String)
      return Ada.Containers.Hash_Type is
   begin
      if Item.Is_Number then
         return Ada.Containers.Hash_Type'Val (Item.Number);

      else
         return LSP.Types.Hash (Item.String);
      end if;
   end Hash;

   ----------
   -- Hash --
   ----------

   function Hash (Item : LSP_URI) return Ada.Containers.Hash_Type is
     (Ada.Strings.Wide_Wide_Fixed.Wide_Wide_Hash
        (VSS.Strings.Conversions.To_Wide_Wide_String (Item.URI)));

   ------------------
   -- Read_LSP_URI --
   ------------------

   procedure Read_LSP_URI
     (S    : access Ada.Streams.Root_Stream_Type'Class;
      Item : out LSP_URI) is
   begin
      Read_String (S, Item.URI);
   end Read_LSP_URI;

   -----------------
   -- Read_String --
   -----------------

   procedure Read_String
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out VSS.Strings.Virtual_String)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

   begin
      pragma Assert (JS.R.Is_String_Value);
      V := JS.R.String_Value;
      JS.R.Read_Next;
   end Read_String;

   --------------
   -- Read_Any --
   --------------

   procedure Read_Any
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP_Any)
   is
      function Read_Value return GNATCOLL.JSON.JSON_Value;

      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

      ----------------
      -- Read_Value --
      ----------------

      function Read_Value return GNATCOLL.JSON.JSON_Value is
      begin
         case JS.R.Event_Kind is
            when VSS.JSON.Pull_Readers.No_Token |
                 VSS.JSON.Pull_Readers.Invalid |
                 VSS.JSON.Pull_Readers.Start_Document |
                 VSS.JSON.Pull_Readers.End_Document |
                 VSS.JSON.Pull_Readers.End_Array |
                 VSS.JSON.Pull_Readers.End_Object |
                 VSS.JSON.Pull_Readers.Key_Name =>

               raise Program_Error;

            when VSS.JSON.Pull_Readers.Start_Array =>

               return Result : constant GNATCOLL.JSON.JSON_Value :=
                 GNATCOLL.JSON.Create (GNATCOLL.JSON.Empty_Array)
               do
                  JS.R.Read_Next;

                  while not JS.R.Is_End_Array loop
                     GNATCOLL.JSON.Append (Result, Read_Value);
                  end loop;

                  JS.R.Read_Next;
               end return;

            when VSS.JSON.Pull_Readers.Start_Object =>

               return Result : constant GNATCOLL.JSON.JSON_Value :=
                 GNATCOLL.JSON.Create_Object
               do
                  JS.R.Read_Next;

                  while not JS.R.Is_End_Object loop
                     pragma Assert (JS.R.Is_Key_Name);

                     declare
                        Key : constant String :=
                          VSS.Strings.Conversions.To_UTF_8_String
                            (JS.R.Key_Name);
                     begin
                        JS.R.Read_Next;
                        Result.Set_Field (Key, Read_Value);
                     end;
                  end loop;

                  JS.R.Read_Next;
               end return;

            when VSS.JSON.Pull_Readers.String_Value =>

               declare
                  Value : constant String :=
                    VSS.Strings.Conversions.To_UTF_8_String
                      (JS.R.String_Value);
               begin
                  JS.R.Read_Next;

                  return GNATCOLL.JSON.Create (Value);
               end;

            when VSS.JSON.Pull_Readers.Number_Value =>

               declare
                  Value : constant VSS.JSON.JSON_Number :=
                    JS.R.Number_Value;
               begin
                  JS.R.Read_Next;

                  case Value.Kind is
                     when VSS.JSON.JSON_Integer =>
                        return GNATCOLL.JSON.Create
                          (Long_Long_Integer (Value.Integer_Value));
                     when VSS.JSON.JSON_Float =>
                        return GNATCOLL.JSON.Create
                          (Long_Float (Value.Float_Value));
                     when others =>
                        raise Program_Error;
                  end case;
               end;

            when VSS.JSON.Pull_Readers.Boolean_Value =>

               declare
                  Value : constant Boolean := (JS.R.Boolean_Value);
               begin
                  JS.R.Read_Next;

                  return GNATCOLL.JSON.Create (Value);
               end;

            when VSS.JSON.Pull_Readers.Null_Value =>
               JS.R.Read_Next;

               return GNATCOLL.JSON.JSON_Null;

         end case;
      end Read_Value;
   begin
      case JS.R.Event_Kind is
         when VSS.JSON.Pull_Readers.No_Token |
              VSS.JSON.Pull_Readers.Invalid |
              VSS.JSON.Pull_Readers.Start_Document |
              VSS.JSON.Pull_Readers.End_Document |
              VSS.JSON.Pull_Readers.End_Array |
              VSS.JSON.Pull_Readers.End_Object |
              VSS.JSON.Pull_Readers.Key_Name =>

            raise Program_Error;

         when VSS.JSON.Pull_Readers.Start_Array |
              VSS.JSON.Pull_Readers.Start_Object |
              VSS.JSON.Pull_Readers.String_Value |
              VSS.JSON.Pull_Readers.Number_Value |
              VSS.JSON.Pull_Readers.Boolean_Value |
              VSS.JSON.Pull_Readers.Null_Value =>

            V := (Read_Value with null record);
      end case;
   end Read_Any;

   ------------------
   -- Read_Boolean --
   ------------------

   procedure Read_Boolean
    (Stream : in out LSP.JSON_Streams.JSON_Stream'Class;
     Item   : out Boolean) is
   begin
      case Stream.R.Event_Kind is
         when VSS.JSON.Pull_Readers.Boolean_Value =>

            Item := Stream.R.Boolean_Value;
            Stream.R.Read_Next;

         when VSS.JSON.Pull_Readers.Null_Value =>

            Item := False;
            Stream.R.Read_Next;

         when others =>
            Item := True;
            Stream.Skip_Value;
            --  Property of non-boolean type, protocol extension
            --  could provide an object instead of boolean.
      end case;

   end Read_Boolean;

   --------------------------------
   -- Read_LSP_Boolean_Or_String --
   --------------------------------

   procedure Read_LSP_Boolean_Or_String
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP_Boolean_Or_String)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      case JS.R.Event_Kind is
         when VSS.JSON.Pull_Readers.String_Value =>
            V := (Is_Boolean => False,
                  String     => JS.R.String_Value);
            JS.R.Read_Next;

         when VSS.JSON.Pull_Readers.Boolean_Value =>
            V := (Is_Boolean => True,
                  Boolean    => JS.R.Boolean_Value);
            JS.R.Read_Next;

         when others =>
            V := (Is_Boolean => True,
                  Boolean    => True);

            JS.Skip_Value;
      end case;

   end Read_LSP_Boolean_Or_String;

   -------------------------------
   -- Read_LSP_Number_Or_String --
   -------------------------------

   procedure Read_LSP_Number_Or_String
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP_Number_Or_String)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      case JS.R.Event_Kind is
         when VSS.JSON.Pull_Readers.Null_Value =>

            V := (Is_Number => False,
                  String    => <>);

         when VSS.JSON.Pull_Readers.String_Value =>
            V := (Is_Number => False,
                  String    => JS.R.String_Value);

         when VSS.JSON.Pull_Readers.Number_Value =>
            V := (Is_Number => True,
                  Number    => LSP_Number (JS.R.Number_Value.Integer_Value));

         when others =>
            raise Constraint_Error;
      end case;

      JS.R.Read_Next;
   end Read_LSP_Number_Or_String;

   ---------------------------
   -- Read_Optional_Boolean --
   ---------------------------

   procedure Read_Optional_Boolean
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Optional_Boolean)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

   begin
      case JS.R.Event_Kind is
         when VSS.JSON.Pull_Readers.Null_Value =>
            V := (Is_Set => False);
            JS.R.Read_Next;

         when VSS.JSON.Pull_Readers.Boolean_Value =>
            V := (Is_Set => True,
                  Value  => JS.R.Boolean_Value);
            JS.R.Read_Next;

         when others =>
            V := (Is_Set => True,
                  Value  => True);
            JS.Skip_Value;
      end case;

   end Read_Optional_Boolean;

   ----------
   -- Read --
   ----------

   procedure Read
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP_Number)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

   begin
      pragma Assert (JS.R.Is_Number_Value);
      V := LSP_Number (JS.R.Number_Value.Integer_Value);
      JS.R.Read_Next;
   end Read;

   --------------------------
   -- Read_Nullable_String --
   --------------------------

   procedure Read_Nullable_String
     (S    : access Ada.Streams.Root_Stream_Type'Class;
      Item : out Nullable_String)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

   begin
      case JS.R.Event_Kind is
         when VSS.JSON.Pull_Readers.Null_Value =>

            Item := (Is_Set => False);

         when VSS.JSON.Pull_Readers.String_Value =>
            Item := (Is_Set => True,
                     Value  => JS.R.String_Value);

         when others =>
            raise Constraint_Error;
      end case;

      JS.R.Read_Next;
   end Read_Nullable_String;

   ------------------------
   -- Read_String_Vector --
   ------------------------

   procedure Read_String_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out VSS.String_Vectors.Virtual_String_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

   begin
      V.Clear;

      if JS.R.Is_Start_Array then
         JS.R.Read_Next;

         while not JS.R.Is_End_Array loop
            declare
               Item : VSS.Strings.Virtual_String;

            begin
               LSP.Types.Read_String (S, Item);
               V.Append (Item);
            end;
         end loop;

      elsif JS.R.Is_String_Value then
         V.Append (JS.R.String_Value);
      end if;

      JS.R.Read_Next;
   end Read_String_Vector;

   --------------------------------
   -- Read_UTF16_Code_Unit_Count --
   --------------------------------

   procedure Read_UTF16_Code_Unit_Count
    (Stream : in out LSP.JSON_Streams.JSON_Stream'Class;
     Item   : out VSS.Unicode.UTF16_Code_Unit_Count) is
   begin
      pragma Assert (Stream.R.Is_Number_Value);
      Item :=
        VSS.Unicode.UTF16_Code_Unit_Count
          (Stream.R.Number_Value.Integer_Value);
      Stream.R.Read_Next;
   end Read_UTF16_Code_Unit_Count;

   ----------------
   -- To_LSP_URI --
   ----------------

   function To_LSP_URI (Item : VSS.Strings.Virtual_String) return LSP_URI is
   begin
      return (URI => Item);
   end To_LSP_URI;

   -----------------------
   -- To_Virtual_String --
   -----------------------

   function To_Virtual_String (Item : LSP.Types.LSP_Number_Or_String)
      return VSS.Strings.Virtual_String is
   begin
      if Item.Is_Number then
         declare
            Image : constant Wide_Wide_String :=
              LSP_Number'Wide_Wide_Image (Item.Number);
         begin
            return
              VSS.Strings.To_Virtual_String
                (Image (Image'First + 1 .. Image'Last));
         end;
      else
         return Item.String;
      end if;
   end To_Virtual_String;

   -----------------------
   -- To_Virtual_String --
   -----------------------

   function To_Virtual_String
     (Self : LSP_URI) return VSS.Strings.Virtual_String is
   begin
      return Self.URI;
   end To_Virtual_String;

   ---------------------
   -- To_UTF_8_String --
   ---------------------

   function To_UTF_8_String
     (Item : LSP_URI) return Ada.Strings.UTF_Encoding.UTF_8_String is
   begin
      return VSS.Strings.Conversions.To_UTF_8_String (Item.URI);
   end To_UTF_8_String;

   ---------------
   -- Write_Any --
   ---------------

   procedure Write_Any
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP_Any)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

      procedure Write (Value : GNATCOLL.JSON.JSON_Value'Class);
      procedure Write_Field (Key : String; Value : GNATCOLL.JSON.JSON_Value);

      -----------
      -- Write --
      -----------

      procedure Write (Value : GNATCOLL.JSON.JSON_Value'Class) is
      begin
         case Value.Kind is
            when GNATCOLL.JSON.JSON_Null_Type =>
               JS.Write_Null;
            when GNATCOLL.JSON.JSON_Boolean_Type =>
               JS.Write_Boolean (Value.Get);
            when GNATCOLL.JSON.JSON_Int_Type =>
               JS.Write_Integer
                 (Interfaces.Integer_64 (Long_Long_Integer'(Value.Get)));
            when GNATCOLL.JSON.JSON_Float_Type =>
               JS.Write_Integer
                 (Interfaces.Integer_64 (Float'(Value.Get)));
            when GNATCOLL.JSON.JSON_String_Type =>
               JS.Write_String
                 (VSS.Strings.Conversions.To_Virtual_String
                    (Ada.Strings.Unbounded.Unbounded_String'(Value.Get)));
            when GNATCOLL.JSON.JSON_Array_Type =>
               declare
                  Vector : constant GNATCOLL.JSON.JSON_Array := Value.Get;
               begin
                  JS.Start_Array;
                  for J in 1 .. GNATCOLL.JSON.Length (Vector) loop
                     Write (GNATCOLL.JSON.Get (Vector, J));
                  end loop;
                  JS.End_Array;
               end;
            when GNATCOLL.JSON.JSON_Object_Type =>
               JS.Start_Object;
               Value.Map_JSON_Object (Write_Field'Access);
               JS.End_Object;
         end case;
      end Write;

      -----------------
      -- Write_Field --
      -----------------

      procedure Write_Field (Key : String; Value : GNATCOLL.JSON.JSON_Value) is
      begin
         JS.Key (VSS.Strings.Conversions.To_Virtual_String (Key));
         Write (Value);
      end Write_Field;
   begin
      Write (V);
   end Write_Any;

   -------------------
   -- Write_Boolean --
   -------------------

   procedure Write_Boolean
    (Stream : in out LSP.JSON_Streams.JSON_Stream'Class;
     Key    : VSS.Strings.Virtual_String;
     Item   : Boolean) is
   begin
      Stream.Key (Key);
      Stream.Write_Boolean (Item);
   end Write_Boolean;

   ------------------
   -- Write_Number --
   ------------------

   procedure Write_Number
    (Stream : in out LSP.JSON_Streams.JSON_Stream'Class;
     Key    : VSS.Strings.Virtual_String;
     Item   : LSP.Types.LSP_Number) is
   begin
      Stream.Key (Key);
      Stream.Write_Integer (Interfaces.Integer_64 (Item));
   end Write_Number;

   ----------------------------
   -- Write_Optional_Boolean --
   ----------------------------

   procedure Write_Optional_Boolean
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Optional_Boolean)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      if V.Is_Set then
         JS.Write_Boolean (V.Value);
      end if;
   end Write_Optional_Boolean;

   ----------------------------
   -- Write_Optional_Boolean --
   ----------------------------

   procedure Write_Optional_Boolean
    (Stream : in out LSP.JSON_Streams.JSON_Stream'Class;
     Key    : VSS.Strings.Virtual_String;
     Item   : LSP.Types.Optional_Boolean) is
   begin
      if Item.Is_Set then
         Stream.Key (Key);
         Stream.Write_Boolean (Item.Value);
      end if;
   end Write_Optional_Boolean;

   ---------------------------
   -- Write_Nullable_String --
   ---------------------------

   procedure Write_Nullable_String
     (S    : access Ada.Streams.Root_Stream_Type'Class;
      Item : Nullable_String)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      if Item.Is_Set then
         JS.Write_String (Item.Value);
      else
         JS.Write_Null;
      end if;
   end Write_Nullable_String;

   -----------
   -- Write --
   -----------

   procedure Write
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP_Number)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Write_Integer (Interfaces.Integer_64 (V));
   end Write;

   ------------------
   -- Write_String --
   ------------------

   procedure Write_String
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : VSS.Strings.Virtual_String)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Write_String (V);
   end Write_String;

   ------------------
   -- Write_String --
   ------------------

   procedure Write_String
    (Stream : in out LSP.JSON_Streams.JSON_Stream'Class;
     Key    : VSS.Strings.Virtual_String;
     Item   : VSS.Strings.Virtual_String) is
   begin
      Stream.Key (Key);
      Stream.Write_String (Item);
   end Write_String;

   -------------------
   -- Write_LSP_URI --
   -------------------

   procedure Write_LSP_URI
     (S    : access Ada.Streams.Root_Stream_Type'Class;
      Item : LSP_URI) is
   begin
      Write_String (S, Item.URI);
   end Write_LSP_URI;

   --------------------------------
   -- Write_LSP_Number_Or_String --
   --------------------------------

   procedure Write_LSP_Number_Or_String
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP_Number_Or_String) is
   begin
      if V.Is_Number then
         Write (S, V.Number);
      elsif not V.String.Is_Empty then
         Write_String (S, V.String);
      end if;
   end Write_LSP_Number_Or_String;

   ----------------------------
   -- Write_Number_Or_String --
   ----------------------------

   procedure Write_Number_Or_String
    (Stream : in out LSP.JSON_Streams.JSON_Stream'Class;
     Key    : VSS.Strings.Virtual_String;
     Item   : LSP.Types.LSP_Number_Or_String) is
   begin
      if Item.Is_Number then
         Write_Number (Stream, Key, Item.Number);
      elsif not Item.String.Is_Empty then
         Write_String (Stream, Key, Item.String);
      end if;
   end Write_Number_Or_String;

   ---------------------------------
   -- Write_LSP_Boolean_Or_String --
   ---------------------------------

   procedure Write_LSP_Boolean_Or_String
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP_Boolean_Or_String)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      case V.Is_Boolean is
         when True =>
            JS.Write_Boolean (V.Boolean);
         when False =>
            JS.Write_String (V.String);
      end case;
   end Write_LSP_Boolean_Or_String;

   -------------------------
   -- Write_String_Vector --
   -------------------------

   procedure Write_String_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : VSS.String_Vectors.Virtual_String_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

   begin
      JS.Start_Array;

      for J in 1 .. V.Length loop
         JS.Write_String (V.Element (J));
      end loop;

      JS.End_Array;
   end Write_String_Vector;

   ---------------------------------
   -- Write_UTF16_Code_Unit_Count --
   ---------------------------------

   procedure Write_UTF16_Code_Unit_Count
    (Stream : in out LSP.JSON_Streams.JSON_Stream'Class;
     Item   : VSS.Unicode.UTF16_Code_Unit_Count) is
   begin
      Stream.Write_Integer (Interfaces.Integer_64 (Item));
   end Write_UTF16_Code_Unit_Count;

   ----------------------------------
   -- Read_Optional_Virtual_String --
   ----------------------------------

   procedure Read_Optional_Virtual_String
     (S    : access Ada.Streams.Root_Stream_Type'Class;
      Item : out Optional_Virtual_String)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

   begin
      case JS.R.Event_Kind is
         when VSS.JSON.Pull_Readers.Null_Value =>

            Item := (Is_Set => False);

         when VSS.JSON.Pull_Readers.String_Value =>
            Item := (Is_Set => True,
                     Value  => JS.R.String_Value);

         when others =>
            raise Constraint_Error;
      end case;

      JS.R.Read_Next;
   end Read_Optional_Virtual_String;

   -----------------------------------
   -- Write_Optional_Virtual_String --
   -----------------------------------

   procedure Write_Optional_Virtual_String
     (S    : access Ada.Streams.Root_Stream_Type'Class;
      Item : Optional_Virtual_String) is
   begin
      if Item.Is_Set then
         Write_String (S, Item.Value);
      end if;
   end Write_Optional_Virtual_String;

end LSP.Types;
