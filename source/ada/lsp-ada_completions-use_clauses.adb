------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2018-2023, AdaCore                     --
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

with VSS.Strings.Conversions;
with Libadalang.Iterators;

with LSP.Ada_Completions.Filters;
with LSP.Enumerations;

package body LSP.Ada_Completions.Use_Clauses is

   ------------------------
   -- Propose_Completion --
   ------------------------

   overriding procedure Propose_Completion
     (Self   : Use_Clause_Completion_Provider;
      Sloc   : Langkit_Support.Slocs.Source_Location;
      Token  : Libadalang.Common.Token_Reference;
      Node   : Libadalang.Analysis.Ada_Node;
      Filter : in out LSP.Ada_Completions.Filters.Filter;
      Names  : in out Ada_Completions.Completion_Maps.Map;
      Result : in out LSP.Structures.CompletionList)
   is
      use Libadalang.Common;
      use Langkit_Support.Slocs;
      use VSS.Strings;

      function Has_With_Clause_Node
        (N : Libadalang.Analysis.Ada_Node) return Boolean;
      --  Returns true if we have a with-clause node on the same line

      --------------------------
      -- Has_With_Clause_Node --
      --------------------------

      function Has_With_Clause_Node
        (N : Libadalang.Analysis.Ada_Node) return Boolean
      is
        (N.Sloc_Range.Start_Line = Sloc.Line
         and then N.Kind = Ada_With_Clause);

      With_Node : constant Libadalang.Analysis.Ada_Node :=
        (if Node.Is_Null then Node
         else Libadalang.Iterators.Find_First
             (Node.Unit.Root, Has_With_Clause_Node'Unrestricted_Access));
   begin
      --  Return immediately if we don't have any with-clause node on the same
      --  line or if we are still within the with-clause node.
      if With_Node.Is_Null
        or else Filter.Is_Comma
        or else
          (not Filter.Is_Semicolon
           and then (Sloc.Line = With_Node.Sloc_Range.End_Line
                     and then Sloc.Column = With_Node.Sloc_Range.End_Column))
      then
         return;
      end if;

      --  Get the package names refered in the with-clause and create the
      --  corresponding use-clause completion item.
      declare
         Packages : constant Libadalang.Analysis.Name_List :=
           With_Node.As_With_Clause.F_Packages;
         Insert_Text : VSS.Strings.Virtual_String :=
           VSS.Strings.Conversions.To_Virtual_String ("use ");
         Doc_Text    : constant VSS.Strings.Virtual_String :=
           VSS.Strings.Conversions.To_Virtual_String
             ("Insert the use-clause corresponding to the with-clause on "
              & "the same line.");
         Item        : LSP.Structures.CompletionItem;
         Count       : Positive := 1;
      begin
         for Package_Name of Packages loop
            Insert_Text := Insert_Text & VSS.Strings.To_Virtual_String
              (Package_Name.Text);

            if Count < Packages.Children_Count then
               Insert_Text := Insert_Text & ", ";
            end if;

            Count := Count + 1;
         end loop;

         Insert_Text := Insert_Text & ";";

         Item :=
           (label               => Insert_Text,
            labelDetails        => (Is_Set => False),
            kind                => (True, LSP.Enumerations.Unit),
            tags                => <>,
            detail              => <>,
            documentation       =>
              (Is_Set => True,
               Value  => LSP.Structures.Virtual_String_Or_MarkupContent'
                 (Is_Virtual_String => True,
                  Virtual_String    => Doc_Text)),
            deprecated          => (Is_Set => False),
            preselect           => (True, False),
            sortText            => "+" & Insert_Text,
            filterText          => <>,
            insertText          => Insert_Text,
            insertTextFormat    => (Is_Set => False),
            insertTextMode      => (Is_Set => False),
            textEdit            => (Is_Set => False),
            textEditText        => <>,
            additionalTextEdits => <>,
            commitCharacters    => <>,
            command             => (Is_Set => False),
            data                => <>);

         Result.items.Append (Item);
      end;
   end Propose_Completion;

end LSP.Ada_Completions.Use_Clauses;
