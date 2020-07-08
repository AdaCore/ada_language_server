------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2018-2020, AdaCore                     --
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
--  This package provides a set of files for Ada Language server.

with Ada.Containers.Ordered_Sets;

with GNATCOLL.VFS;

package LSP.Ada_File_Sets is

   package File_Sets is new Ada.Containers.Ordered_Sets
     (Element_Type        => GNATCOLL.VFS.Virtual_File,
      "<"                 => GNATCOLL.VFS."<",
      "="                 => GNATCOLL.VFS."=");

end LSP.Ada_File_Sets;
