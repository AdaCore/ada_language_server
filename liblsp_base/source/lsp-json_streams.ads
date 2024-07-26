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
--
--  This package provides an Ada stream type to serialize Ada object into JSON.
--

with Ada.Streams;
with Interfaces;

with VSS.JSON.Pull_Readers;
private with VSS.JSON.Push_Writers;
with VSS.Strings;
with VSS.Text_Streams;

package LSP.JSON_Streams is
--   pragma Elaborate_Body;

   type JSON_Stream
     (Is_Server_Side : Boolean := False;
      R : access VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class := null)
       is limited new Ada.Streams.Root_Stream_Type with private;
   --  Stream implemented over JSON document
   --
   --  To support JSON serialization user provides Read/Write streaming
   --  aspects that are aware of JSON_Stream. Simple types use Read/Write
   --  procedures from this package to convert a value to/from simple JSON
   --  value. Compond types use Start_Object/End_Object if they are represented
   --  as JSON object or Start_Array/End_Array if they are implemented as
   --  JSON stream. After starting object or array compound type iterates over
   --  each of its items using corresponding Read/Write aspects for convertion.
   --  To provide property name compound type uses Key procedure before
   --  calling Read/Write. Each item shoud have Read/Write aspect overriden
   --  in the same way.
   --
   --  The Is_Server_Side discriminant can be used to create distinct
   --  representations on client and server sides.

   procedure Start_Object (Self : not null access JSON_Stream'Class);
   --  Start new JSON object during read/write of some compound type

   procedure End_Object (Self : not null access JSON_Stream'Class);
   --  End JSON object during read/write of some compound type

   procedure Start_Array (Self : not null access JSON_Stream'Class);
   --  Start new JSON array during read/write of some compound type

   procedure End_Array (Self : not null access JSON_Stream'Class);
   --  End JSON array during read/write of some compound type

   procedure Key
    (Self : not null access JSON_Stream'Class;
     Key  : VSS.Strings.Virtual_String);
   --  Specify property name before do convertion of an item nested in an JSON
   --  object

   procedure Set_Stream
     (Self   : in out JSON_Stream'Class;
      Stream : not null VSS.Text_Streams.Output_Text_Stream_Access);
   --  Assign output text stream to retrieve resulting JSON document after
   --  writting to the JSON stream

   procedure End_Document (Self : in out JSON_Stream'Class);
   --  Complete writting to the JSON stream

   procedure Skip_Value (Self : in out JSON_Stream'Class);
   --  Read and discard one value from JSON

   procedure Write_String
    (Self : in out JSON_Stream'Class;
     Item : VSS.Strings.Virtual_String);
   --  The same as Write, but optimized for strings.

   procedure Write_Integer
    (Self : in out JSON_Stream'Class;
     Item : Interfaces.Integer_64);
   --  The same as Write, but optimized for integers.

   procedure Write_Boolean
    (Self : in out JSON_Stream'Class;
     Item : Boolean);
   --  The same as Write, but optimized for booleans.

   procedure Write_Null (Self : in out JSON_Stream'Class);
   --  The same as Write, but for null value.

private

   type Write_Stream is limited record
      Writer : VSS.JSON.Push_Writers.JSON_Simple_Push_Writer;
      Key    : VSS.Strings.Virtual_String;
   end record;

   type JSON_Stream
     (Is_Server_Side : Boolean := False;
      R : access VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class := null)
   is limited new Ada.Streams.Root_Stream_Type with
   record
      W : Write_Stream;
   end record;

   overriding procedure Read
     (Stream : in out JSON_Stream;
      Item   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset);

   overriding procedure Write
     (Stream : in out JSON_Stream;
      Item   : Ada.Streams.Stream_Element_Array);

end LSP.JSON_Streams;
