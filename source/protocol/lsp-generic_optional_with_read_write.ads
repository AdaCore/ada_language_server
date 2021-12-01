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
--
--  This package provides a template to create optional values together with
--  corresponding read/write aspects.

with Ada.Streams;
generic
   type Element_Type is private;

   --  Value type
   Write_Unset_As_Null : Boolean := False;
   --  Some TypeScript types are expressed as  `TypeX | null`. Set this to
   --  True to represent such types as an optional type. The `Write` procedure
   --  will encode unset value as `null` JSON value.
   Write_Default_As_True : Boolean := False;
   --  Some TypeScript optional properties are expressed as `TypeX | boolean`.
   --  Set this to True to represent such types as an optional type. The
   --  `Write` procedure will encode default value as `true` JSON value.

   with procedure Element_Read
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Element_Type);
   with procedure Element_Write
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Element_Type);

package LSP.Generic_Optional_With_Read_Write is

   type Optional_Type (Is_Set : Boolean := False) is record
      case Is_Set is
         when True =>
            Value : Element_Type;
         when False =>
            null;
      end case;
   end record;

   procedure Read
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Optional_Type);
   --  Read a value from JSON stream

   for Optional_Type'Read use Read;

   procedure Write
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Optional_Type);
   --  Write a value to JSON stream

   for Optional_Type'Write use Write;

end LSP.Generic_Optional_With_Read_Write;
