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

with Libadalang.Common;

with VSS.Strings;

with LSP.Ada_Completions.Filters;
with LSP.Lal_Utils;

package body LSP.Ada_Completions.Keywords is

   ------------------------
   -- Propose_Completion --
   ------------------------

   overriding procedure Propose_Completion
     (Self   : Keyword_Completion_Provider;
      Sloc   : Langkit_Support.Slocs.Source_Location;
      Token  : Libadalang.Common.Token_Reference;
      Node   : Libadalang.Analysis.Ada_Node;
      Filter : in out LSP.Ada_Completions.Filters.Filter;
      Names  : in out Ada_Completions.Completion_Maps.Map;
      Result : in out LSP.Messages.CompletionList)
   is
      pragma Unreferenced (Names);
      Prev   : constant Libadalang.Common.Token_Reference :=
        Libadalang.Common.Previous (Token);

   begin
      if Filter.Is_End_Label or else
        Filter.Is_Numeric_Literal or else
        not Libadalang.Common.Is_Trivia (Prev)
      then
         --  Propose keyword completion if we are not within an end label
         --  not within a numeric literal
         --  and if there is no previous character of if it's a whitespace (we
         --  don't want to propose keywords after typing '(' to feed subprogram
         --  parameters for instance).
         return;
      end if;

      declare
         Item     : LSP.Messages.CompletionItem;
         Prefix   : constant VSS.Strings.Virtual_String :=
           VSS.Strings.To_Virtual_String  (Node.Text);
         Keywords : constant Libadalang.Analysis.Unbounded_Text_Type_Array :=
           Node.P_Valid_Keywords;

      begin
         for Keyword of Keywords loop
            declare
               Label : constant VSS.Strings.Virtual_String :=
                 LSP.Lal_Utils.To_Virtual_String (Keyword);

            begin
               if Label.Starts_With
                    (Prefix, VSS.Strings.Identifier_Caseless)
               then
                  Item.label := Label;
                  Item.insertTextFormat := (True, LSP.Messages.PlainText);
                  Item.insertText := (True, Label);
                  Item.kind := (True, LSP.Messages.Keyword);
                  Result.items.Append (Item);
               end if;
            end;
         end loop;
      end;
   end Propose_Completion;

end LSP.Ada_Completions.Keywords;
