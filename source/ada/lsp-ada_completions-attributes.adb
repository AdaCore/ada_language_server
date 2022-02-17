------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2018-2021, AdaCore                     --
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

with LSP.Ada_Completions.Filters;
with LSP.Predefined_Completion;

package body LSP.Ada_Completions.Attributes is

   ------------------------
   -- Propose_Completion --
   ------------------------

   overriding procedure Propose_Completion
     (Self   : Attributes_Completion_Provider;
      Sloc   : Langkit_Support.Slocs.Source_Location;
      Token  : Libadalang.Common.Token_Reference;
      Node   : Libadalang.Analysis.Ada_Node;
      Filter : in out LSP.Ada_Completions.Filters.Filter;
      Names  : in out Ada_Completions.Completion_Maps.Map;
      Result : in out LSP.Messages.CompletionList)
   is
      pragma Unreferenced (Names);

      use Libadalang.Analysis;
      use Libadalang.Common;
   begin
      if Filter.Is_Attribute_Ref then
         declare
            use type VSS.Strings.Virtual_String;
            Token_Kind : constant Libadalang.Common.Token_Kind :=
              Libadalang.Common.Kind (Libadalang.Common.Data (Token));
            Prefix : constant VSS.Strings.Virtual_String :=
              (if Token_Kind = Ada_Tick then
                  VSS.Strings.To_Virtual_String ("'")
               else
                  VSS.Strings.To_Virtual_String (Node.Text));

         begin
            --  If we are right after the "'" we should list all the possible
            --  attributes, so set the prefix to the empty string.
            LSP.Predefined_Completion.Get_Attributes
              (Prefix => (if Prefix /= "'" then Prefix else ""),
               Result => Result.items);
         end;
      end if;
   end Propose_Completion;

end LSP.Ada_Completions.Attributes;
