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

package body LSP.JSON_Streams is

   procedure Write_Key (Self : in out Write_Stream);

   ---------------
   -- End_Array --
   ---------------

   procedure End_Array (Self : not null access JSON_Stream'Class) is
   begin
      pragma Assert (Self.R = null);
      Self.W.Writer.End_Array;
      Self.W.Key := VSS.Strings.Empty_Virtual_String;
   end End_Array;

   ------------------
   -- End_Document --
   ------------------

   procedure End_Document (Self : in out JSON_Stream'Class) is
   begin
      pragma Assert (Self.R = null);
      Self.W.Writer.End_Document;
   end End_Document;

   ----------------
   -- End_Object --
   ----------------

   procedure End_Object (Self : not null access JSON_Stream'Class) is
   begin
      pragma Assert (Self.R = null);
      Self.W.Writer.End_Object;
      Self.W.Key := VSS.Strings.Empty_Virtual_String;
   end End_Object;

   ---------
   -- Key --
   ---------

   procedure Key
     (Self : not null access JSON_Stream'Class;
      Key  : VSS.Strings.Virtual_String) is
   begin
      pragma Assert (Self.R = null);
      Self.W.Key := Key;
   end Key;

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

   ----------------
   -- Set_Stream --
   ----------------

   procedure Set_Stream
     (Self   : in out JSON_Stream'Class;
      Stream : not null VSS.Text_Streams.Output_Text_Stream_Access) is
   begin
      pragma Assert (Self.R = null);
      Self.W.Writer.Set_Stream (Stream);
      Self.W.Writer.Start_Document;
   end Set_Stream;

   ----------------
   -- Skip_Value --
   ----------------

   procedure Skip_Value (Self : in out JSON_Stream'Class) is
   begin
      case Self.R.Event_Kind is
         when VSS.JSON.Pull_Readers.No_Token |
              VSS.JSON.Pull_Readers.Invalid |
              VSS.JSON.Pull_Readers.Start_Document |
              VSS.JSON.Pull_Readers.End_Document |
              VSS.JSON.Pull_Readers.End_Array |
              VSS.JSON.Pull_Readers.End_Object |
              VSS.JSON.Pull_Readers.Key_Name =>

            raise Program_Error;

         when VSS.JSON.Pull_Readers.Start_Array =>

            Self.R.Read_Next;

            while not Self.R.Is_End_Array loop
               Skip_Value (Self);  --  skip erray element
            end loop;

            Self.R.Read_Next;

         when VSS.JSON.Pull_Readers.Start_Object =>

            Self.R.Read_Next;

            while not Self.R.Is_End_Object loop
               pragma Assert (Self.R.Is_Key_Name);

               Self.R.Read_Next;  --  Skip key
               Skip_Value (Self);  --  Skip corresponding value
            end loop;

            Self.R.Read_Next;

         when VSS.JSON.Pull_Readers.String_Value
            | VSS.JSON.Pull_Readers.Number_Value
            | VSS.JSON.Pull_Readers.Boolean_Value
            | VSS.JSON.Pull_Readers.Null_Value =>

            Self.R.Read_Next;

      end case;
   end Skip_Value;

   -----------------
   -- Start_Array --
   -----------------

   procedure Start_Array (Self : not null access JSON_Stream'Class) is
   begin
      pragma Assert (Self.R = null);
      Write_Key (Self.W);
      Self.W.Writer.Start_Array;
   end Start_Array;

   ------------------
   -- Start_Object --
   ------------------

   procedure Start_Object (Self : not null access JSON_Stream'Class) is
   begin
      pragma Assert (Self.R = null);
      Write_Key (Self.W);
      Self.W.Writer.Start_Object;
   end Start_Object;

   ---------------
   -- Write_Key --
   ---------------

   procedure Write_Key (Self : in out Write_Stream) is
   begin
      if not Self.Key.Is_Empty then
         Self.Writer.Key_Name (Self.Key);
         Self.Key := VSS.Strings.Empty_Virtual_String;
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
      pragma Assert (Self.R = null);
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
      pragma Assert (Self.R = null);
      Write_Key (Self.W);
      Self.W.Writer.Integer_Value (Item);
   end Write_Integer;

   ----------------
   -- Write_Null --
   ----------------

   procedure Write_Null (Self : in out JSON_Stream'Class) is
   begin
      pragma Assert (Self.R = null);
      Write_Key (Self.W);
      Self.W.Writer.Null_Value;
   end Write_Null;

   ------------------
   -- Write_String --
   ------------------

   procedure Write_String
    (Self : in out JSON_Stream'Class;
     Item : VSS.Strings.Virtual_String) is
   begin
      pragma Assert (Self.R = null);
      Write_Key (Self.W);
      Self.W.Writer.String_Value (Item);
   end Write_String;

end LSP.JSON_Streams;
