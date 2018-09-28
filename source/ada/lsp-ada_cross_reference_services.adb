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

--  Temporary package, see note in corresponding spec file.

with Libadalang.Common;    use Libadalang.Common;
with Libadalang.Iterators; use Libadalang.Iterators;

package body LSP.Ada_Cross_Reference_Services is

   function Find_All_References
     (Definition         : Defining_Name;
      Sources            : File_Array_Access;
      Include_Definition : Boolean := False) return Ref_Vector
   is

      Context    : constant Analysis_Context := Definition.Unit.Context;

      References : Ref_Vector;

   begin

      for N in Sources'Range loop

         declare

            Unit : constant Analysis_Unit := Context.Get_From_File
              (Sources (N).Display_Full_Name);

            Match_Iterator : Traverse_Iterator'Class := Find
              (Unit.Root,
               Kind_Is (Ada_Identifier) and Text_Is (Definition.Text));

            Node : Ada_Node;
            Node_Definition : Defining_Name;

         begin

            while (Match_Iterator.Next (Node)) loop

               Node_Definition := Node.P_Xref;

               if Node_Definition = Definition and then
                 (Include_Definition or else
                  Node.As_Identifier /= Definition.F_Name)
               then
                  References.Append (Node);
               end if;

            end loop;

         end;

      end loop;

      return References;

   end Find_All_References;

end LSP.Ada_Cross_Reference_Services;
