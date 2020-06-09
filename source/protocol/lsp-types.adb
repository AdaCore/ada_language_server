------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2018-2020, AdaCore                     --
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
with Interfaces;

with LSP.JSON_Streams;

package body LSP.Types is

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
      V := To_LSP_String (Unbounded_String'(JS.Read.Get));
   end Read;

   --------------
   -- Read_Any --
   --------------

   procedure Read_Any
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP_Any)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);
   begin
      V := LSP_Any'(JS.Read with null record);
   end Read_Any;

   ------------------
   -- Read_Boolean --
   ------------------

   procedure Read_Boolean
    (Stream : in out LSP.JSON_Streams.JSON_Stream'Class;
     Key    : LSP.Types.LSP_String;
     Item   : out Boolean)
   is
      Value : GNATCOLL.JSON.JSON_Value;
   begin
      Stream.Key (Ada.Strings.Wide_Unbounded.Unbounded_Wide_String (Key));
      Value := Stream.Read;

      if Value.Kind in GNATCOLL.JSON.JSON_Null_Type then
         Item := False;  --  No such property
      elsif Value.Kind in GNATCOLL.JSON.JSON_Boolean_Type then
         Item := Value.Get;  --  Property of a boolean type
      else
         Item := True;  --  Property of non-boolean type, protocol extension
         --  could provide an object instead of boolean.
      end if;
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
      Value : constant GNATCOLL.JSON.JSON_Value := JS.Read;
   begin
      if Value.Kind = GNATCOLL.JSON.JSON_String_Type then
         V := (Is_Boolean => False,
               String     => To_LSP_String (Unbounded_String'(Value.Get)));
      elsif Value.Kind = GNATCOLL.JSON.JSON_Boolean_Type then
         V := (Is_Boolean => True,
               Boolean    => GNATCOLL.JSON.Get (Value));
      else
         V := (Is_Boolean => True,
               Boolean    => True);
      end if;
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
      Value : constant GNATCOLL.JSON.JSON_Value := JS.Read;
   begin
      if Value.Is_Empty then
         V := (Is_Number => False,
               String    => Empty_LSP_String);
      elsif Value.Kind in GNATCOLL.JSON.JSON_String_Type then
         V := (Is_Number => False,
               String    => To_LSP_String (Unbounded_String'(Value.Get)));
      else
         V := (Is_Number => True,
               Number    => LSP_Number (Integer'(Value.Get)));
      end if;
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
      JS.Start_Array;

      while not JS.End_Of_Array loop
         V.Append (To_LSP_String (Unbounded_String'(JS.Read.Get)));
      end loop;

      JS.End_Array;
   end Read_LSP_String_Vector;

   ---------------------------
   -- Read_Number_Or_String --
   ---------------------------

   procedure Read_Number_Or_String
     (Stream : in out LSP.JSON_Streams.JSON_Stream'Class;
      Key    : LSP.Types.LSP_String;
      Item   : out LSP.Types.LSP_Number_Or_String) is
   begin
      Stream.Key (Ada.Strings.Wide_Unbounded.Unbounded_Wide_String (Key));
      Read_LSP_Number_Or_String (Stream'Unchecked_Access, Item);
   end Read_Number_Or_String;

   ---------------------------
   -- Read_Optional_Boolean --
   ---------------------------

   procedure Read_Optional_Boolean
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Optional_Boolean)
   is
      JS : LSP.JSON_Streams.JSON_Stream'Class renames
        LSP.JSON_Streams.JSON_Stream'Class (S.all);

      Value : constant GNATCOLL.JSON.JSON_Value := JS.Read;
   begin
      if Value.Is_Empty then
         V := (Is_Set => False);
      elsif Value.Kind in GNATCOLL.JSON.JSON_Boolean_Type then
         V := (Is_Set => True, Value => Value.Get);
      else
         V := (Is_Set => True, Value => True);
      end if;
   end Read_Optional_Boolean;

   ---------------------------
   -- Read_Optional_Boolean --
   ---------------------------

   procedure Read_Optional_Boolean
    (Stream : in out LSP.JSON_Streams.JSON_Stream'Class;
     Key    : LSP.Types.LSP_String;
     Item   : out LSP.Types.Optional_Boolean)
   is
      Value : GNATCOLL.JSON.JSON_Value;
   begin
      Stream.Key (Ada.Strings.Wide_Unbounded.Unbounded_Wide_String (Key));
      Value := Stream.Read;

      if Value.Kind in GNATCOLL.JSON.JSON_Null_Type then
         Item := (Is_Set => False);
      else
         Item := (Is_Set => True, Value => Value.Get);
      end if;
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

      Value : constant Integer := JS.Read.Get;
   begin
      V := LSP_Number (Value);
   end Read;

   --------------------------
   -- Read_Optional_String --
   --------------------------

   procedure Read_Optional_String
     (Stream : in out LSP.JSON_Streams.JSON_Stream'Class;
      Key    : LSP.Types.LSP_String;
      Item   : out LSP.Types.Optional_String)
   is
      Value : GNATCOLL.JSON.JSON_Value;
   begin
      Stream.Key (Ada.Strings.Wide_Unbounded.Unbounded_Wide_String (Key));
      Value := Stream.Read;

      if Value.Kind in GNATCOLL.JSON.JSON_Null_Type then
         Item := (Is_Set => False);
      else
         Item := (Is_Set => True, Value => To_LSP_String
                  (Unbounded_String'(Value.Get)));
      end if;
   end Read_Optional_String;

   -----------------
   -- Read_String --
   -----------------

   procedure Read_String
     (Stream : in out LSP.JSON_Streams.JSON_Stream'Class;
      Key    : LSP.Types.LSP_String;
      Item   : out LSP.Types.LSP_String) is
   begin
      Stream.Key (Ada.Strings.Wide_Unbounded.Unbounded_Wide_String (Key));
      Item := To_LSP_String (Unbounded_String'(Stream.Read.Get));
   end Read_String;

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
   begin
      JS.Write (GNATCOLL.JSON.JSON_Value (V));
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
      Stream.Write (GNATCOLL.JSON.Create (Item));
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
         Stream.Write (GNATCOLL.JSON.Create (Item.Value));
      end if;
   end Write_Optional_Boolean;

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
      JS.Write (GNATCOLL.JSON.Create (Integer (V)));
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
         JS.Write
           (GNATCOLL.JSON.Create (To_UTF_8_String (V.Element (J))));
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

end LSP.Types;
