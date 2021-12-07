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

--  This package provides generic vector to implement Language Server Protocol.

with Ada.Containers.Vectors;
with Ada.Streams;

generic
   type Element is private;
   Write_Empty : LSP.On_Empty_Array;
   --  How to write an empty array: skip, write `[]` or write `null`

   with function "=" (Left, Right : Element) return Boolean is <>;

package LSP.Generic_Vectors is

   package Element_Vectors is
     new Ada.Containers.Vectors (Positive, Element);

   type Vector is new Element_Vectors.Vector with null record;

   overriding function "=" (Left, Right : Vector) return Boolean is
     (Element_Vectors."="
        (Element_Vectors.Vector (Left),
         Element_Vectors.Vector (Right)));

   procedure Read_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Vector);

   procedure Write_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Vector);

   for Vector'Read use Read_Vector;
   for Vector'Write use Write_Vector;

end LSP.Generic_Vectors;
