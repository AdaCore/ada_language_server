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

with Ada.Characters.Handling;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.UTF_Encoding.Wide_Strings;
with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
with Ada.Unchecked_Deallocation;
with Interfaces;

with VSS.JSON.Streams.Readers;
with VSS.Characters;
with VSS.Strings.Conversions;
with VSS.Strings.Character_Iterators;

with LSP.JSON_Streams;

package body LSP.Types is
   use type VSS.JSON.Streams.Readers.JSON_Event_Kind;

   Chunk_Size    : constant := 512;
   --  When processing strings in chunks, this is the size of the chunk

   --------------
   -- Assigned --
   --------------

   function Assigned (Id : LSP_Number_Or_String) return Boolean is
   begin
      return Id.Is_Number or else Length (Id.String) > 0;
   end Assigned;

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

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Text : LSP_String) return Boolean is
   begin
      return Length (Text) = 0;
   end Is_Empty;

   ----------
   -- Read --
   ----------

   procedure Read
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Types.LSP_String)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      pragma Assert (JS.R.Is_String_Value);
      V := To_LSP_String (JS.R.String_Value);
      JS.R.Read_Next;
   end Read;

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
            when VSS.JSON.Streams.Readers.No_Token |
                 VSS.JSON.Streams.Readers.Invalid |
                 VSS.JSON.Streams.Readers.Start_Document |
                 VSS.JSON.Streams.Readers.End_Document |
                 VSS.JSON.Streams.Readers.End_Array |
                 VSS.JSON.Streams.Readers.End_Object |
                 VSS.JSON.Streams.Readers.Key_Name =>

               raise Program_Error;

            when VSS.JSON.Streams.Readers.Start_Array =>

               return Result : constant GNATCOLL.JSON.JSON_Value :=
                 GNATCOLL.JSON.Create (GNATCOLL.JSON.Empty_Array)
               do
                  JS.R.Read_Next;

                  while not JS.R.Is_End_Array loop
                     GNATCOLL.JSON.Append (Result, Read_Value);
                  end loop;

                  JS.R.Read_Next;
               end return;

            when VSS.JSON.Streams.Readers.Start_Object =>

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

            when VSS.JSON.Streams.Readers.String_Value =>

               declare
                  Value : constant String :=
                    VSS.Strings.Conversions.To_UTF_8_String
                      (JS.R.String_Value);
               begin
                  JS.R.Read_Next;

                  return GNATCOLL.JSON.Create (Value);
               end;

            when VSS.JSON.Streams.Readers.Number_Value =>

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

            when VSS.JSON.Streams.Readers.Boolean_Value =>

               declare
                  Value : constant Boolean := (JS.R.Boolean_Value);
               begin
                  JS.R.Read_Next;

                  return GNATCOLL.JSON.Create (Value);
               end;

            when VSS.JSON.Streams.Readers.Null_Value =>
               JS.R.Read_Next;

               return GNATCOLL.JSON.JSON_Null;

         end case;
      end Read_Value;
   begin
      case JS.R.Event_Kind is
         when VSS.JSON.Streams.Readers.No_Token |
              VSS.JSON.Streams.Readers.Invalid |
              VSS.JSON.Streams.Readers.Start_Document |
              VSS.JSON.Streams.Readers.End_Document |
              VSS.JSON.Streams.Readers.End_Array |
              VSS.JSON.Streams.Readers.End_Object |
              VSS.JSON.Streams.Readers.Key_Name =>

            raise Program_Error;

         when VSS.JSON.Streams.Readers.Start_Array |
              VSS.JSON.Streams.Readers.Start_Object |
              VSS.JSON.Streams.Readers.String_Value |
              VSS.JSON.Streams.Readers.Number_Value |
              VSS.JSON.Streams.Readers.Boolean_Value |
              VSS.JSON.Streams.Readers.Null_Value =>

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
         when VSS.JSON.Streams.Readers.Boolean_Value =>

            Item := Stream.R.Boolean_Value;
            Stream.R.Read_Next;

         when VSS.JSON.Streams.Readers.Null_Value =>

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
         when VSS.JSON.Streams.Readers.String_Value =>
            V := (Is_Boolean => False,
                  String     => To_LSP_String (JS.R.String_Value));
            JS.R.Read_Next;

         when VSS.JSON.Streams.Readers.Boolean_Value =>
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
         when VSS.JSON.Streams.Readers.Null_Value =>

            V := (Is_Number => False,
                  String    => Empty_LSP_String);

         when VSS.JSON.Streams.Readers.String_Value =>
            V := (Is_Number => False,
                  String    => To_LSP_String (JS.R.String_Value));

         when VSS.JSON.Streams.Readers.Number_Value =>
            V := (Is_Number => True,
                  Number    => LSP_Number (JS.R.Number_Value.Integer_Value));

         when others =>
            raise Constraint_Error;
      end case;

      JS.R.Read_Next;
   end Read_LSP_Number_Or_String;

   ----------------------------
   -- Read_LSP_String_Vector --
   ----------------------------

   procedure Read_LSP_String_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP_String_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      V.Clear;

      if JS.R.Is_Start_Array then
         JS.R.Read_Next;

         while not JS.R.Is_End_Array loop
            declare
               Item : LSP_String;
            begin
               LSP.Types.Read (S, Item);
               V.Append (Item);
            end;
         end loop;

      elsif JS.R.Is_String_Value then
         V.Append (To_LSP_String (JS.R.String_Value));
      end if;

      JS.R.Read_Next;
   end Read_LSP_String_Vector;

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
         when VSS.JSON.Streams.Readers.Null_Value =>
            V := (Is_Set => False);
            JS.R.Read_Next;

         when VSS.JSON.Streams.Readers.Boolean_Value =>
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
   -- Read_Optional_String --
   --------------------------

   procedure Read_Optional_String
     (S    : access Ada.Streams.Root_Stream_Type'Class;
      Item : out LSP.Types.Optional_String)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

   begin
      case JS.R.Event_Kind is
         when VSS.JSON.Streams.Readers.Null_Value =>

            Item := (Is_Set => False);

         when VSS.JSON.Streams.Readers.String_Value =>
            Item := (Is_Set => True,
                     Value  => To_LSP_String (JS.R.String_Value));

         when others =>
            raise Constraint_Error;
      end case;

      JS.R.Read_Next;
   end Read_Optional_String;

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
         when VSS.JSON.Streams.Readers.Null_Value =>

            Item := (Is_Set => False);

         when VSS.JSON.Streams.Readers.String_Value =>
            Item := (Is_Set => True,
                     Value  => To_LSP_String (JS.R.String_Value));

         when others =>
            raise Constraint_Error;
      end case;

      JS.R.Read_Next;
   end Read_Nullable_String;

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
   -----------------
   -- Starts_With --
   -----------------

   function Starts_With
     (Text           : LSP_String;
      Prefix         : Ada.Strings.UTF_Encoding.UTF_8_String;
      Case_Sensitive : Boolean := True) return Boolean
   is
      use Ada.Characters.Handling;

      Value : constant String := To_UTF_8_String (Text);
   begin
      if Value'Length < Prefix'Length then
         return False;
      end if;

      if Case_Sensitive then
         return Value (1 .. Prefix'Length) = Prefix;
      else
         return To_Lower (Value (1 .. Prefix'Length)) = To_Lower (Prefix);
      end if;
   end Starts_With;

   ---------------
   -- Ends_With --
   ---------------

   function Ends_With
     (Text           : LSP_String;
      Suffix         : Ada.Strings.UTF_Encoding.UTF_8_String;
      Case_Sensitive : Boolean := True) return Boolean
   is
      use Ada.Characters.Handling;

      Value : constant String := To_UTF_8_String (Text);
   begin
      if Value'Length < Suffix'Length then
         return False;
      end if;

      if Case_Sensitive then
         return Value (Value'Last - Suffix'Length + 1 .. Value'Last) = Suffix;
      else
         return To_Lower
           (Value (Value'Last - Suffix'Length + 1 .. Value'Last))
             = To_Lower (Suffix);
      end if;
   end Ends_With;

   -------------------
   -- To_LSP_String --
   -------------------

   function To_LSP_String
     (Item : VSS.Strings.Virtual_String) return LSP_String
   is
      --  It is temporary conversion function to process data without use of
      --  primary stack, thus to prevent crashes on conversion of big data.
      --  It takes into account that content of Virtual_String is always valid,
      --  there are no any checks for invalid ranges of code points here.

      procedure Free is
        new Ada.Unchecked_Deallocation
          (Wide_String, Ada.Strings.Wide_Unbounded.Wide_String_Access);

      High_Surrogate_First : constant := 16#D800#;
      Low_Surrogate_First  : constant := 16#DC00#;

      High_Surrogate_First_Store : constant
        := High_Surrogate_First - 16#1_0000# / 16#400#;
      --  Code point is converted to surrogate pair as:
      --
      --  S (J)     := HB + (C - 0x10000) >> 10
      --  S (J + 1) := LB + (C - 0x10000) & 0x3FF
      --
      --  to optimize implementation they are rewritten as:
      --
      --  S (J + 1) := LB + C & 0x3FF
      --  S (J)     := (HB - 0x10000 >> 10) + C >> 10
      --               ^^^^^^^^^^^^^^^^^^^^
      --  This constant represents constant part of the expression.

      Aux      : Ada.Strings.Wide_Unbounded.Wide_String_Access :=
        new Wide_String (1 .. Natural (Item.Character_Length) * 2);
      --  Abstract character can occupi up to two code units in UTF-16
      --  encoding. Reserve enought space to avoid reallocations.
      Last     : Natural := 0;
      Position : VSS.Strings.Character_Iterators.Character_Iterator :=
        Item.First_Character;

   begin
      if not Position.Has_Element then
         Free (Aux);

         return Empty_LSP_String;
      end if;

      loop
         declare
            use type VSS.Unicode.Code_Point;

            C : constant VSS.Unicode.Code_Point :=
              VSS.Characters.Virtual_Character'Pos (Position.Element);

         begin
            if C <= 16#FFFF# then
               Last := Last + 1;
               Aux (Last) := Wide_Character'Val (C);

            else
               Last := Last + 1;
               Aux (Last) :=
                 Wide_Character'Val
                   (High_Surrogate_First_Store + C / 16#400#);
               Last := Last + 1;
               Aux (Last) :=
                 Wide_Character'Val (Low_Surrogate_First + C mod 16#400#);
            end if;

            exit when not Position.Forward;
         end;
      end loop;

      return Result : LSP_String do
         Set_Unbounded_Wide_String (Result, Aux (1 .. Last));
         Free (Aux);
      end return;

   exception
      when others =>
         Free (Aux);

         raise;
   end To_LSP_String;

   -------------------
   -- To_LSP_String --
   -------------------

   function To_LSP_String (Text : Ada.Strings.UTF_Encoding.UTF_8_String)
     return LSP_String is
      UTF_16 : constant Wide_String :=
        Ada.Strings.UTF_Encoding.Wide_Strings.Decode (Text);
   begin
      return To_Unbounded_Wide_String (UTF_16);
   end To_LSP_String;

   -------------------
   -- To_LSP_String --
   -------------------

   function To_LSP_String
     (Text : GNATCOLL.JSON.UTF8_Unbounded_String) return LSP_String
   is
      Res : LSP_String;
      Len : constant Natural := Length (Text);
      Current_Index : Positive := 1;

      subtype Continuation_Character is Character range
        Character'Val (2#1000_0000#) .. Character'Val (2#1011_1111#);
   begin
      loop
         --  Process the decoding in chunks
         declare
            Bound : Natural := Natural'Min (Current_Index + Chunk_Size, Len);
            Chunk : constant String (Current_Index .. Bound) := Slice
              (Text, Current_Index, Bound);
         begin
            --  We don't want to cut a chunk in the middle of a long
            --  character, so look at the last 4 bytes and cut before
            --  any such long character, and cut if needs be.
            if Bound /= Len then
               for J in reverse 0 .. 3 loop
                  if Chunk (Bound - J) not in Continuation_Character then
                     --  This character is not a continuation character: it's
                     --  OK to cut before it, and start the next chunk with it.
                     Bound := Bound - J - 1;
                     exit;
                  end if;
               end loop;
            end if;
            Append
              (Res, Ada.Strings.UTF_Encoding.Wide_Strings.Decode
                 (Chunk (Current_Index .. Bound)));
            Current_Index := Bound + 1;
            exit when Current_Index > Len;
         end;
      end loop;
      return Res;
   end To_LSP_String;

   -------------------
   -- To_LSP_String --
   -------------------

   function To_LSP_String
     (Text : Wide_Wide_String) return LSP_String
   is
      UTF_16 : constant Wide_String :=
        Ada.Strings.UTF_Encoding.Wide_Wide_Strings.Encode (Text);
   begin
      return To_Unbounded_Wide_String (UTF_16);
   end To_LSP_String;

   ---------------------
   -- To_UTF_8_String --
   ---------------------

   function To_UTF_8_String (Value : LSP_String)
     return Ada.Strings.UTF_Encoding.UTF_8_String
   is
      Wide : constant Wide_String := To_Wide_String (Value);
   begin
      return Ada.Strings.UTF_Encoding.Wide_Strings.Encode (Wide);
   end To_UTF_8_String;

   ---------------------
   -- To_UTF_8_String --
   ---------------------

   function To_UTF_8_String (Item : LSP.Types.LSP_Number_Or_String)
      return Ada.Strings.UTF_Encoding.UTF_8_String is
   begin
      if Item.Is_Number then
         declare
            Image : constant String := LSP_Number'Image (Item.Number);
         begin
            return Image (Image'First + 1 .. Image'Last);
         end;
      else
         return To_UTF_8_String (Item.String);
      end if;
   end To_UTF_8_String;

   -------------------------------
   -- To_UTF_8_Unbounded_String --
   -------------------------------

   function To_UTF_8_Unbounded_String
     (Value : LSP_String) return GNATCOLL.JSON.UTF8_Unbounded_String
   is
      Res  : Ada.Strings.Unbounded.Unbounded_String;
      Len  : constant Natural := Length (Value);
      Current_Index : Natural := 1;
      Next_Index    : Natural;
   begin
      --  Perform the encoding chunk by chunk, so as not to blow the stack
      loop
         Next_Index := Natural'Min (Current_Index + Chunk_Size, Len);
         Ada.Strings.Unbounded.Append
           (Res, Ada.Strings.UTF_Encoding.Wide_Strings.Encode
              (Slice (Value, Current_Index, Next_Index)));
         Current_Index := Next_Index + 1;
         exit when Current_Index > Len;
      end loop;
      return Res;
   end To_UTF_8_Unbounded_String;

   -----------------------
   -- To_Virtual_String --
   -----------------------

   function To_Virtual_String
     (Item : LSP_String) return VSS.Strings.Virtual_String
   is
      High_Surrogate_First  : constant := 16#D800#;
      Low_Surrogate_First   : constant := 16#DC00#;
      Surrogate_Kind_Mask   : constant := 16#FC00#;
      Masked_High_Surrogate : constant := 16#D800#;

      UCS4_Fixup : constant
        := High_Surrogate_First * 16#400# + Low_Surrogate_First - 16#1_0000#;
      --  When code point is encoded as pair of surrogates its value computed
      --  as:
      --
      --    C := (S (J) - HB) << 10 + S (J + 1) - LB + 0x10000
      --
      --  to optimize number of computations this expression is transformed to
      --
      --    C := S (J) << 10 + S (J + 1) - (HB << 10 + LB - 0x10000)
      --                                   ^^^^^^^^^^^^^^^^^^^^^^^^^
      --  This constant represents constant part of the expression.

      Aux   : Ada.Strings.Unbounded.String_Access :=
        new String (1 .. Length (Item) * 4);
      Last  : Natural := 0;
      Index : Positive := 1;

   begin
      while Index <= Length (Item) loop
         declare
            use type VSS.Unicode.Code_Point;

            C : VSS.Unicode.Code_Point :=
              Wide_Character'Pos (Element (Item, Index));

         begin
            Index := Index + 1;

            if (C and Surrogate_Kind_Mask) = Masked_High_Surrogate then
               C :=
                 C * 16#400#
                 + Wide_Character'Pos (Element (Item, Index))
                 - UCS4_Fixup;
               Index := Index + 1;
            end if;

            case C is
               when 16#0000# .. 16#007F# =>
                  Last := Last + 1;
                  Aux (Last) := Character'Val (C);

               when 16#0080# .. 16#07FF# =>
                  Last := Last + 1;
                  Aux (Last) := Character'Val (2#11000000# or (C / 16#40#));
                  Last := Last + 1;
                  Aux (Last) :=
                    Character'Val (2#10000000# or (C and 2#00111111#));

               when 16#0800# .. 16#FFFF# =>
                  Last := Last + 1;
                  Aux (Last) :=
                    Character'Val (2#11100000# or (C / 16#1000#));
                  Last := Last + 1;
                  Aux (Last) :=
                    Character'Val
                      (2#10000000# or ((C / 16#40#) and 2#00111111#));
                  Last := Last + 1;
                  Aux (Last) :=
                    Character'Val (2#10000000# or (C and 2#00111111#));

               when 16#01_0000# .. 16#10_FFFF# =>
                  Last := Last + 1;
                  Aux (Last) :=
                    Character'Val (2#11110000# or (C / 16#4_0000#));
                  Last := Last + 1;
                  Aux (Last) :=
                    Character'Val
                      (2#10000000# or ((C / 16#1000#) and 2#00111111#));
                  Last := Last + 1;
                  Aux (Last) :=
                    Character'Val
                      (2#10000000# or ((C / 16#40#) and 2#00111111#));
                  Last := Last + 1;
                  Aux (Last) :=
                    Character'Val (2#10000000# or (C and 2#00111111#));
            end case;
         end;
      end loop;

      return Result : constant VSS.Strings.Virtual_String :=
        VSS.Strings.Conversions.To_Virtual_String (Aux (1 .. Last))
      do
         Free (Aux);
      end return;

   exception
      when others =>
         Free (Aux);

         raise;
   end To_Virtual_String;

   -----------
   -- Write --
   -----------

   procedure Write
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Types.LSP_String)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Write_String (V);  --  To_UTF_8_Unbounded_String
   end Write;

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
               JS.Write_String (String'(Value.Get));
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
         JS.Key (Ada.Strings.UTF_Encoding.Wide_Strings.Decode (Key));
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
     Key    : LSP.Types.LSP_String;
     Item   : Boolean) is
   begin
      Stream.Key (Ada.Strings.Wide_Unbounded.Unbounded_Wide_String (Key));
      Stream.Write_Boolean (Item);
   end Write_Boolean;

   ------------------
   -- Write_Number --
   ------------------

   procedure Write_Number
    (Stream : in out LSP.JSON_Streams.JSON_Stream'Class;
     Key    : LSP.Types.LSP_String;
     Item   : LSP.Types.LSP_Number) is
   begin
      Stream.Key (Ada.Strings.Wide_Unbounded.Unbounded_Wide_String (Key));
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
     Key    : LSP.Types.LSP_String;
     Item   : LSP.Types.Optional_Boolean) is
   begin
      if Item.Is_Set then
         Stream.Key (Ada.Strings.Wide_Unbounded.Unbounded_Wide_String (Key));
         Stream.Write_Boolean (Item.Value);
      end if;
   end Write_Optional_Boolean;

   ---------------------------
   -- Write_Optional_String --
   ---------------------------

   procedure Write_Optional_String
     (S    : access Ada.Streams.Root_Stream_Type'Class;
      Item : LSP.Types.Optional_String)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      if Item.Is_Set then
         JS.Write_String (Item.Value);
      end if;
   end Write_Optional_String;

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
    (Stream : in out LSP.JSON_Streams.JSON_Stream'Class;
     Key    : LSP.Types.LSP_String;
     Item   : LSP.Types.LSP_String) is
   begin
      Stream.Key (Ada.Strings.Wide_Unbounded.Unbounded_Wide_String (Key));
      Stream.Write_String (Item);
   end Write_String;

   --------------------------------
   -- Write_LSP_Number_Or_String --
   --------------------------------

   procedure Write_LSP_Number_Or_String
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP_Number_Or_String) is
   begin
      if V.Is_Number then
         Write (S, V.Number);
      elsif not Is_Empty (V.String) then
         Write (S, V.String);
      end if;
   end Write_LSP_Number_Or_String;

   -----------------------------
   -- Write_LSP_String_Vector --
   -----------------------------

   procedure Write_LSP_String_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP_String_Vector)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      JS.Start_Array;

      for J in 1 .. V.Last_Index loop
         JS.Write_String (V.Element (J));
      end loop;

      JS.End_Array;
   end Write_LSP_String_Vector;

   ----------------------------
   -- Write_Number_Or_String --
   ----------------------------

   procedure Write_Number_Or_String
    (Stream : in out LSP.JSON_Streams.JSON_Stream'Class;
     Key    : LSP.Types.LSP_String;
     Item   : LSP.Types.LSP_Number_Or_String) is
   begin
      if Item.Is_Number then
         Write_Number (Stream, Key, Item.Number);
      elsif not Is_Empty (Item.String) then
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

   ---------------------------------
   -- Write_UTF16_Code_Unit_Count --
   ---------------------------------

   procedure Write_UTF16_Code_Unit_Count
    (Stream : in out LSP.JSON_Streams.JSON_Stream'Class;
     Item   : VSS.Unicode.UTF16_Code_Unit_Count) is
   begin
      Stream.Write_Integer (Interfaces.Integer_64 (Item));
   end Write_UTF16_Code_Unit_Count;

end LSP.Types;
