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

with LSP.Ada_Contexts;

with Libadalang.Analysis; use Libadalang.Analysis;
with Libadalang.Common;

package LSP.Lal_Utils is

   function Get_Node_As_Name (Node : Ada_Node) return Name;

   function Get_Name_As_Defining (Name_Node : Name) return Defining_Name;

   function Resolve_Name
     (Name_Node : Name;
      Imprecise : out Boolean) return Defining_Name;
   --  Return the definition node (canonical part) of the given name.
   --  Imprecise is set to True if LAL's imprecise fallback mechanism has been
   --  used to compute the cross reference.

   ---------------
   -- Called_By --
   ---------------

   package References_List is new Ada.Containers.Doubly_Linked_Lists
     (Base_Id);

   function "<" (Left, Right : Defining_Name) return Boolean is
      (Left.Text < Right.Text);

   package References_By_Subprogram is new Ada.Containers.Ordered_Maps
     (Key_Type     => Defining_Name,
      Element_Type => References_List.List,
      "<"          => "<",
      "="          => References_List."=");

   function Is_Called_By
     (Context           : LSP.Ada_Contexts.Context;
      Definition        : Defining_Name;
      Imprecise_Results : out Boolean)
      return References_By_Subprogram.Map
     with Pre =>
       Definition.P_Basic_Decl.Kind in Libadalang.Common.Ada_Subp_Decl
         | Libadalang.Common.Ada_Subp_Body
         | Libadalang.Common.Ada_Null_Subp_Decl
         | Libadalang.Common.Ada_Entry_Decl
         | Libadalang.Common.Ada_Entry_Body;
   --  Return the list of all the calls made to the subprogram pointed at by
   --  the node given by Definition, organized by the subprograms in which
   --  these calls are listed, ordered by the name of these subprograms.
   --  Imprecise_Results is set to True if we don't know whether the results
   --  are precise.

end LSP.Lal_Utils;
