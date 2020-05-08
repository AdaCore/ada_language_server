------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2018-2019, AdaCore                     --
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

with Magic.Strings.Conversions;
with LSP.Types;

package body LSP.JSON_Streams is

   function To_UTF_8_String
     (Value : Ada.Strings.Wide_Unbounded.Unbounded_Wide_String)
      return Ada.Strings.UTF_Encoding.UTF_8_String;
   --  Unbounded_Wide_String (UTF-16) to String (UTF-8) conversion.

   procedure Write_Key (Self : in out Write_Stream);

   ---------------
   -- End_Array --
   ---------------

   procedure End_Array (Self : not null access JSON_Stream'Class) is
   begin
      pragma Assert (Self.Writable);
      Self.W.Writer.End_Array;
      Self.W.Key := Magic.Strings.Empty_Magic_String;
   end End_Array;

   ------------------
   -- End_Document --
   ------------------

   procedure End_Document (Self : in out JSON_Stream'Class) is
   begin
      Self.W.Writer.End_Document;
   end End_Document;

   ----------------
   -- End_Object --
   ----------------

   procedure End_Object (Self : not null access JSON_Stream'Class) is
   begin
      pragma Assert (Self.Writable);
      Self.W.Writer.End_Object;
      Self.W.Key := Magic.Strings.Empty_Magic_String;
   end End_Object;

   ---------
   -- Key --
   ---------

   procedure Key
     (Self : not null access JSON_Stream'Class;
      Key  : Ada.Strings.Wide_Unbounded.Unbounded_Wide_String) is
   begin
      case Self.Writable is
         when True =>
            Self.W.Key := Magic.Strings.Conversions.To_Magic_String
              (To_UTF_8_String (Key));
         when False =>
            raise Program_Error;
      end case;
   end Key;

   ---------
   -- Key --
   ---------

   procedure Key
    (Self : not null access JSON_Stream'Class;
     Key  : Wide_String) is
   begin
      Self.Key (Ada.Strings.Wide_Unbounded.To_Unbounded_Wide_String (Key));
   end Key;

   -------
   -- R --
   -------

   function R
     (Self : not null access JSON_Stream'Class)
      return not null access
        Magic.JSON.Streams.Readers.JSON_Stream_Reader'Class is
   begin
      return Self.R'Unchecked_Access;
   end R;

   ----------
   -- Read --
   ----------

   overriding procedure Read
     (Stream : in out JSON_Stream;
      Item   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset) is
   begin
      raise Program_Error;
   end Read;

   -----------------------
   -- Set_JSON_Document --
   -----------------------

   procedure Set_JSON_Document
     (Self  : not null access JSON_Stream'Class;
      Input : not null Magic.Text_Streams.Input_Text_Stream_Access) is
   begin
      Self.Writable := False;  --  Read-only stream
      Self.R.Set_Stream (Input);
   end Set_JSON_Document;

   ----------------
   -- Set_Stream --
   ----------------

   procedure Set_Stream
     (Self   : in out JSON_Stream'Class;
      Stream : not null Magic.Text_Streams.Output_Text_Stream_Access) is
   begin
      Self.W.Writer.Set_Stream (Stream);
      Self.W.Writer.Start_Document;
   end Set_Stream;

   ----------------
   -- Skip_Value --
   ----------------

   procedure Skip_Value (Self : in out JSON_Stream'Class) is
   begin
      case Self.R.Event_Kind is
         when Magic.JSON.Streams.Readers.No_Token |
              Magic.JSON.Streams.Readers.Invalid |
              Magic.JSON.Streams.Readers.Start_Document |
              Magic.JSON.Streams.Readers.End_Document |
              Magic.JSON.Streams.Readers.End_Array |
              Magic.JSON.Streams.Readers.End_Object |
              Magic.JSON.Streams.Readers.Key_Name =>

            raise Program_Error;

         when Magic.JSON.Streams.Readers.Start_Array =>

            Self.R.Read_Next;

            while not Self.R.Is_End_Array loop
               Skip_Value (Self);  --  skip erray element
            end loop;

            Self.R.Read_Next;

         when Magic.JSON.Streams.Readers.Start_Object =>

            Self.R.Read_Next;

            while not Self.R.Is_End_Object loop
               pragma Assert (Self.R.Is_Key_Name);

               Self.R.Read_Next;  --  Skip key
               Skip_Value (Self);  --  Skip corresponding value
            end loop;

            Self.R.Read_Next;

         when Magic.JSON.Streams.Readers.String_Value
            | Magic.JSON.Streams.Readers.Number_Value
            | Magic.JSON.Streams.Readers.Boolean_Value
            | Magic.JSON.Streams.Readers.Null_Value =>

            Self.R.Read_Next;

      end case;
   end Skip_Value;

   -----------------
   -- Start_Array --
   -----------------

   procedure Start_Array (Self : not null access JSON_Stream'Class) is
   begin
      pragma Assert (Self.Writable);
      Write_Key (Self.W);
      Self.W.Writer.Start_Array;
   end Start_Array;

   ------------------
   -- Start_Object --
   ------------------

   procedure Start_Object (Self : not null access JSON_Stream'Class) is
   begin
      pragma Assert (Self.Writable);
      Write_Key (Self.W);
      Self.W.Writer.Start_Object;
   end Start_Object;

   ---------------------
   -- To_UTF_8_String --
   ---------------------

   function To_UTF_8_String
     (Value : Ada.Strings.Wide_Unbounded.Unbounded_Wide_String)
      return Ada.Strings.UTF_Encoding.UTF_8_String is
   begin
      return
        Ada.Strings.UTF_Encoding.Wide_Strings.Encode
          (Ada.Strings.Wide_Unbounded.To_Wide_String (Value));
   end To_UTF_8_String;

   ---------------
   -- Write_Key --
   ---------------

   procedure Write_Key (Self : in out Write_Stream) is
   begin
      if not Self.Key.Is_Empty then
         Self.Writer.Key_Name (Self.Key);
         Self.Key := Magic.Strings.Empty_Magic_String;
      end if;
   end Write_Key;

   -----------
   -- Write --
   -----------

   overriding procedure Write
     (Stream : in out JSON_Stream;
      Item   : Ada.Streams.Stream_Element_Array) is
   begin
      raise Program_Error;
   end Write;

   -------------------
   -- Write_Boolean --
   -------------------

   procedure Write_Boolean
    (Self : in out JSON_Stream'Class;
     Item : Boolean) is
   begin
      Write_Key (Self.W);
      Self.W.Writer.Boolean_Value (Item);
   end Write_Boolean;

   -------------------
   -- Write_Integer --
   -------------------

   procedure Write_Integer
    (Self : in out JSON_Stream'Class;
     Item : Interfaces.Integer_64) is
   begin
      Write_Key (Self.W);
      Self.W.Writer.Integer_Value (Item);
   end Write_Integer;

   ----------------
   -- Write_Null --
   ----------------

   procedure Write_Null (Self : in out JSON_Stream'Class) is
   begin
      Write_Key (Self.W);
      Self.W.Writer.Null_Value;
   end Write_Null;

   ------------------
   -- Write_String --
   ------------------

   procedure Write_String
    (Self : in out JSON_Stream'Class;
     Item : String) is
   begin
      Write_Key (Self.W);
      Self.W.Writer.String_Value
        (Magic.Strings.Conversions.To_Magic_String (Item));
   end Write_String;

   ------------------
   -- Write_String --
   ------------------

   procedure Write_String
    (Self : in out JSON_Stream'Class;
     Item : LSP.Types.LSP_String) is
   begin
      Write_Key (Self.W);
      Self.W.Writer.String_Value
        (Magic.Strings.Conversions.To_Magic_String
           (LSP.Types.To_UTF_8_String (Item)));
   end Write_String;

end LSP.JSON_Streams;
