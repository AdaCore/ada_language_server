------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                        Copyright (C) 2018, AdaCore                       --
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

--  This is a temporary package providing functionality to find all references
--  to a definition in a set of source files. This functionality will later be
--  integrated into Libadalang and this package will be removed.

with GNATCOLL.VFS; use GNATCOLL.VFS;

with Libadalang.Analysis; use Libadalang.Analysis;

package LSP.Ada_Cross_Reference_Services is

   function Find_All_References
     (Definition         : Defining_Name;
      Sources            : File_Array_Access;
      Include_Definition : Boolean := False) return Ada_Node_Array;

end LSP.Ada_Cross_Reference_Services;
