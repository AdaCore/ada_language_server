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

      Definition : Defining_Name := Get_Name_As_Defining (Name_Node);

   begin

      if Definition = No_Defining_Name then

         declare
            Names : constant Defining_Name_Array :=
              Name_Node.P_Referenced_Decl (Imprecise_Fallback => True)
              .P_Canonical_Part.P_Defining_Names;
         begin

            for I in Names'Range loop

               declare
                  Decl_Name : constant Defining_Name := Names (I);
               begin

                  if P_Name_Matches (Decl_Name, Name_Node) then
                     Definition := Decl_Name;
                     exit;
                  end if;

               end;

            end loop;

         end;

      end if;

      return Definition;

   end Resolve_Name;

end LSP.Lal_Utils;
