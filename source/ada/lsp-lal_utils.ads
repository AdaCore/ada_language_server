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
--  This package provides some Libadalang related utility subprograms.

with Ada.Containers.Ordered_Maps;
with Ada.Containers.Doubly_Linked_Lists;

with GNATCOLL.VFS;

with Libadalang.Analysis; use Libadalang.Analysis;

package LSP.Lal_Utils is

   function Get_Node_As_Name (Node : Ada_Node) return Name;

   function Get_Name_As_Defining (Name_Node : Name) return Defining_Name;

   function Resolve_Name (Name_Node : Name) return Defining_Name;

   function Find_All_References
     (Definition         : Defining_Name;
      Sources            : GNATCOLL.VFS.File_Array_Access;
      Charset            : String;
      Include_Definition : Boolean := False)
         return Ada_Node_Array;
   --  Finds all references to a given defining name in the given list
   --  of units. Charset is the character set to use when loading
   --  files from the disk.
   --  If Include_Definition is True, include the definition as well.

   ---------------
   -- Called_By --
   ---------------

   package References_List is new Ada.Containers.Doubly_Linked_Lists
     (Ada_Node);

   function "<" (Left, Right : Defining_Name) return Boolean is
      (Left.Text < Right.Text);

   package References_By_Subprogram is new Ada.Containers.Ordered_Maps
     (Key_Type     => Defining_Name,
      Element_Type => References_List.List,
      "<"          => "<",
      "="          => References_List."=");

   function Is_Called_By
     (Name_Node : Name;
      Sources   : GNATCOLL.VFS.File_Array_Access;
      Charset   : String)
      return References_By_Subprogram.Map;
   --  Return the list of all the calls made to the subprogram pointed at by
   --  the node given by Name, organized by the subprograms in which these
   --  calls are listed, ordered by the name of these subprograms.
   --  Sources is the list of sources to search in; Charset the encoding
   --  with which to read files from disk.

end LSP.Lal_Utils;
