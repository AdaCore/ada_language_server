------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2017-2018, AdaCore                     --
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

with Ada.Strings.UTF_Encoding.Wide_Strings;

with LSP.JSON_Streams;

package body LSP.Types is
   use Ada.Strings.Wide_Unbounded;

   --------------
   -- Assigned --
   --------------

   function Assigned (Id : LSP_Number_Or_String) return Boolean is
   begin
      return Id.Is_Number or else Length (Id.String) > 0;
   end Assigned;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Text : LSP_String) return Boolean is
   begin
      return Length (Text) = 0;
   end Is_Empty;

   ---------------------------
   -- Read_Number_Or_String --
   ---------------------------

   procedure Read_Number_Or_String
     (Stream : in out LSP.JSON_Streams.JSON_Stream'Class;
      Key    : LSP.Types.LSP_String;
      Item   : out LSP.Types.LSP_Number_Or_String)
   is
      Value : GNATCOLL.JSON.JSON_Value;
   begin
      Stream.Key (Key);
      Value := Stream.Read;

      if Value.Is_Empty then
         Item := (Is_Number => False,
                  String    => Empty_LSP_String);
      elsif Value.Kind in GNATCOLL.JSON.JSON_String_Type then
         Item := (Is_Number => False,
                  String    => To_LSP_String (Value.Get));
      else
         Item := (Is_Number => True, Number => Integer'(Value.Get));
      end if;
   end Read_Number_Or_String;

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
      Stream.Key (Key);
      Value := Stream.Read;

      if Value.Kind in GNATCOLL.JSON.JSON_Null_Type then
         Item := (Is_Set => False);
      else
         Item := (Is_Set => True, Value => To_LSP_String (Value.Get));
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
      Stream.Key (Key);
      Item := To_LSP_String (Stream.Read.Get);
   end Read_String;

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

end LSP.Types;
