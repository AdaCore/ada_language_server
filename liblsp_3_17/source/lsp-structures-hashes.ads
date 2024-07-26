------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2018-2024, AdaCore                     --
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

private with VSS.Strings.Hash;

package LSP.Structures.Hashes is

   function Hash
     (Item : LSP.Structures.Integer_Or_Virtual_String)
      return Ada.Containers.Hash_Type;

private

   function Hash
     (Item : LSP.Structures.Integer_Or_Virtual_String)
      return Ada.Containers.Hash_Type is
        (case Item.Is_Integer is
            when True  => Ada.Containers.Hash_Type'Mod (Item.Integer),
            when False => VSS.Strings.Hash (Item.Virtual_String));

end LSP.Structures.Hashes;
