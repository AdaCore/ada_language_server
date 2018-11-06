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

with Libadalang.Iterators; use Libadalang.Iterators;

package body LSP.Ada_Cross_Reference_Services is

   function Find_All_References
     (Definition         : Defining_Name;
      Sources            : File_Array_Access;
      Include_Definition : Boolean := False) return Ada_Node_Array
   is
      Context : constant Analysis_Context := Definition.Unit.Context;
      Source_Units : Analysis_Unit_Array (Sources'Range);
   begin
      if not Definition.Is_Null then
         for N in Sources'Range loop
            Source_Units (N) := Context.Get_From_File
              (Sources (N).Display_Full_Name);
         end loop;

         declare
            References : constant Ada_Node_Array :=
              Definition.P_Find_All_References (Source_Units);
         begin
            if Include_Definition then
               return References & (1 => Definition.As_Ada_Node);
            else
               return References;
            end if;
         end;
      end if;

      return (1 .. 0 => <>);
   end Find_All_References;

end LSP.Ada_Cross_Reference_Services;
