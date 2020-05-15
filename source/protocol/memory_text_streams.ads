------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                       Copyright (C) 2020, AdaCore                        --
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

with Ada.Streams;

with Magic.Characters;
with Magic.Stream_Element_Buffers;
with Magic.Strings;
with Magic.Text_Streams;

package Memory_Text_Streams is

   type Memory_UTF8_Input_Stream is
   limited new Magic.Text_Streams.Input_Text_Stream with record
      Buffer      : Magic.Stream_Element_Buffers.Stream_Element_Buffer;
      Current     : Ada.Streams.Stream_Element_Count := 1;
      Skip        : Boolean := False;
      Incremental : Boolean := False;
      Diagnosis   : Magic.Strings.Magic_String;
   end record;

   overriding procedure Get
     (Self    : in out Memory_UTF8_Input_Stream;
      Item    : out Magic.Characters.Magic_Character;
      Success : in out Boolean);

   overriding function Is_End_Of_Data
     (Self : Memory_UTF8_Input_Stream) return Boolean;

   overriding function Is_End_Of_Stream
     (Self : Memory_UTF8_Input_Stream) return Boolean;

   overriding function Has_Error
     (Self : Memory_UTF8_Input_Stream) return Boolean;

   overriding function Error_Message
     (Self : Memory_UTF8_Input_Stream) return Magic.Strings.Magic_String;

   procedure Set_Incremental
     (Self : in out Memory_UTF8_Input_Stream'Class;
      To   : Boolean);

end Memory_Text_Streams;
