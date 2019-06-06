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

with Libadalang.Common; use Libadalang.Common;

package body LSP.Lal_Utils is

   ----------------------
   -- Get_Node_As_Name --
   ----------------------

   function Get_Node_As_Name (Node : Ada_Node) return Name is
   begin

      if Node = No_Ada_Node or else Node.Kind not in Ada_Name then
         return No_Name;
      end if;

      return Node.As_Name;

   end Get_Node_As_Name;

   --------------------------
   -- Get_Name_As_Defining --
   --------------------------

   function Get_Name_As_Defining (Name_Node : Name) return Defining_Name is
   begin

      if Name_Node = No_Name or else not Name_Node.P_Is_Defining then
         return No_Defining_Name;
      end if;

      return Name_Node.P_Enclosing_Defining_Name;

   end Get_Name_As_Defining;

   ------------------
   -- Resolve_Name --
   ------------------

   function Resolve_Name (Name_Node : Name) return Defining_Name is
      --  In every case, if the unit has syntax errors, activate
      --  Imprecise_Fallback.
      --  TODO: Should we activate it in any case ?
      Result : constant Defining_Name :=
        Name_Node.P_Xref
          (Imprecise_Fallback => Name_Node.Unit.Has_Diagnostics);
   begin
      if Name_Node.P_Is_Defining and Result = No_Defining_Name then
         --  When Name_Node is part of defining_name and it isn't a completion
         --  of another declaration, then P_Xref returns No_Defining_Name.
         --  In this case we return current defining_name.
         return Name_Node.P_Enclosing_Defining_Name;
      else
         return Result;
      end if;
   end Resolve_Name;

end LSP.Lal_Utils;
