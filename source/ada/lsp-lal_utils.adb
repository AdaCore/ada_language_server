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

with Libadalang.Common; use Libadalang.Common;

package body LSP.Lal_Utils is

   ----------------------------
   -- Get_Definition_In_Node --
   ----------------------------

   function Get_Definition_In_Node (Node : Ada_Node) return Defining_Name is
   begin

      if Node /= No_Ada_Node and then Node.Kind in Ada_Name then
         declare
            Name : constant Libadalang.Analysis.Name := Node.As_Name;
         begin
            if Name.P_Is_Defining then
               return Name.P_Enclosing_Defining_Name;
            end if;
         end;
      end if;

      return No_Defining_Name;

   end Get_Definition_In_Node;

   ------------------
   -- Resolve_Node --
   ------------------

   function Resolve_Node (Node : Ada_Node) return Defining_Name is

      Definition : Defining_Name := Get_Definition_In_Node (Node);

   begin

      if Definition = No_Defining_Name then

         Definition :=
           Node.P_Referenced_Decl.P_Canonical_Part.P_Defining_Name;

         if Definition = No_Defining_Name then
            return No_Defining_Name;
         end if;

      end if;

      return Definition;

   end Resolve_Node;

end LSP.Lal_Utils;
