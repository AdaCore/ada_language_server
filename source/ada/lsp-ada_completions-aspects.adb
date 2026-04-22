------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2018-2026, AdaCore                     --
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

with VSS.Strings;

with LSP.Predefined_Completion;

package body LSP.Ada_Completions.Aspects is

   ------------------------
   -- Propose_Completion --
   ------------------------

   overriding procedure Propose_Completion
     (Self   : Aspect_Completion_Provider;
      Sloc   : Langkit_Support.Slocs.Source_Location;
      Token  : Libadalang.Common.Token_Reference;
      Node   : Libadalang.Analysis.Ada_Node;
      Filter : in out LSP.Ada_Completions.Filters.Filter;
      Result : out Ada_Completions.Completion_Result)
   is
      Parent : constant Libadalang.Analysis.Ada_Node :=
        (if Node.Is_Null then Node else Node.Parent);
   begin
      Result := (Ada_Completions.Completion_List, others => <>);

      if Filter.Is_Aspect then
         if not Parent.Is_Null and then
           Parent.Kind in Libadalang.Common.Ada_Aspect_Assoc_Range
         then
            declare
               Prefix : constant VSS.Strings.Virtual_String :=
                 VSS.Strings.To_Virtual_String (Node.Text);

            begin
               LSP.Predefined_Completion.Get_Aspects
                 (Prefix => Prefix,
                  Result => Result.Completion_List);
            end;

         elsif Node.Kind in Libadalang.Common.Ada_Aspect_Spec_Range then
            LSP.Predefined_Completion.Get_Aspects
              (Prefix => VSS.Strings.Empty_Virtual_String,
               Result => Result.Completion_List);
         end if;
      end if;
   end Propose_Completion;

end LSP.Ada_Completions.Aspects;
