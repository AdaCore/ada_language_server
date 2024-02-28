------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2018-2023, AdaCore                     --
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

with Ada.Containers.Hashed_Sets;

with LSP.Structures;

package LSP.Locations is

   use type Ada.Containers.Hash_Type;

   Prime : constant := 271;

   function Hash
     (Value : LSP.Structures.A_Range) return Ada.Containers.Hash_Type is
       (Prime * Ada.Containers.Hash_Type'Mod (Value.start.line)
        + Ada.Containers.Hash_Type'Mod (Value.start.character)
        + Prime * Ada.Containers.Hash_Type'Mod (Value.an_end.line)
        + Ada.Containers.Hash_Type'Mod (Value.an_end.character));

   function Hash
     (Value : LSP.Structures.Location) return Ada.Containers.Hash_Type is
       (Value.uri.Get_Hash + Hash (Value.a_range));

   package File_Span_Sets is new Ada.Containers.Hashed_Sets
     (Element_Type        => LSP.Structures.Location,
      Hash                => Hash,
      Equivalent_Elements => LSP.Structures."=",
      "="                 => LSP.Structures."=");

end LSP.Locations;
