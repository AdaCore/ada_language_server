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

--  This package provides generic set to implement Language Server Protocol.

with Ada.Streams;

generic
   type Element is (<>);

   Write_Empty : LSP.On_Empty_Array;
   --  How to write an empty set: skip, write `[]` or write `null`

package LSP.Generic_Sets is

   type Set is private;

   function Empty return Set
     with Inline;
   --  Return an empty set

   function To_Set (From, To : Element) return Set
     with Inline;
   --  Return a set which values in range From .. To

   function Contains (Self : Set; Value : Element) return Boolean
     with Inline;
   --  Check if Value is in the set

   procedure Include (Self : in out Set; Value : Element)
     with Inline;
   --  Append Value to the set

   procedure Remove (Self : in out Set; Value : Element)
     with Inline;
   --  Remove Value to the set

private

   type Set is array (Element) of Boolean
     with Default_Component_Value => False;

   procedure Read_Set
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Set);

   procedure Write_Set
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Set);

   for Set'Read use Read_Set;
   for Set'Write use Write_Set;

end LSP.Generic_Sets;
