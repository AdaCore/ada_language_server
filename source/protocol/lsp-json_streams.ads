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

private with Ada.Containers.Vectors;
with Ada.Streams;

with LSP.Types;
with GNATCOLL.JSON;

package LSP.JSON_Streams is
--   pragma Preelaborate;

   type JSON_Stream is new Ada.Streams.Root_Stream_Type with private;

   procedure Start_Object (Self : not null access JSON_Stream'Class);

   procedure End_Object (Self : not null access JSON_Stream'Class);

   procedure Start_Array (Self : not null access JSON_Stream'Class);

   procedure End_Array (Self : not null access JSON_Stream'Class);

   function End_Of_Array
    (Self : not null access JSON_Stream'Class) return Boolean;
   --  Returns True when there are no array elements to be read.

   procedure Key
    (Self : not null access JSON_Stream'Class;
     Key  : LSP.Types.LSP_String);

   function Get_JSON_Document
    (Self : not null access JSON_Stream'Class)
       return GNATCOLL.JSON.JSON_Array;

   procedure Set_JSON_Document
    (Self : not null access JSON_Stream'Class;
     Data : GNATCOLL.JSON.JSON_Array);

   function Read
    (Self : in out JSON_Stream'Class)
       return GNATCOLL.JSON.JSON_Value;
   --  Reads current value and updates stream's position.

   procedure Write
    (Self : in out JSON_Stream'Class;
     Item : GNATCOLL.JSON.JSON_Value);
   --  Writes value into the stream and updates stream's position.

private
   type State_Kinds is (Array_State, Object_State);

   type State (Kind : State_Kinds := Array_State) is record
      Modified : Boolean := False;

      case Kind is
         when Array_State =>
            Current_Array : GNATCOLL.JSON.JSON_Array;
            Index         : Positive := 1;

         when Object_State =>
            Current_Object : GNATCOLL.JSON.JSON_Value;
            Key            : LSP.Types.LSP_String;
      end case;
   end record;

   package State_Vectors is new Ada.Containers.Vectors (Positive, State);

   type JSON_Stream is new Ada.Streams.Root_Stream_Type with record
      Current : State;
      Stack   : State_Vectors.Vector;
   end record;

   overriding procedure Read
     (Stream : in out JSON_Stream;
      Item   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset);

   overriding procedure Write
     (Stream : in out JSON_Stream;
      Item   : Ada.Streams.Stream_Element_Array);

end LSP.JSON_Streams;
