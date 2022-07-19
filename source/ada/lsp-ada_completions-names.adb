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

with Langkit_Support.Errors;

with VSS.Strings;

with LSP.Ada_Completions.Filters;
with LSP.Lal_Utils;

package body LSP.Ada_Completions.Names is

   ------------------------
   -- Propose_Completion --
   ------------------------

   overriding procedure Propose_Completion
     (Self   : Name_Completion_Provider;
      Sloc   : Langkit_Support.Slocs.Source_Location;
      Token  : Libadalang.Common.Token_Reference;
      Node   : Libadalang.Analysis.Ada_Node;
      Filter : in out LSP.Ada_Completions.Filters.Filter;
      Names  : in out Ada_Completions.Completion_Maps.Map;
      Result : in out LSP.Messages.CompletionList)
   is
      use all type Libadalang.Analysis.Base_Id;
      use all type Libadalang.Common.Ada_Node_Kind_Type;
      use type Libadalang.Common.Token_Kind;

      Parent   : Libadalang.Analysis.Ada_Node;
      --  The parent of the node to complete.

      Sibling  : Libadalang.Analysis.Ada_Node;
      --  The right sibling of the node to complete.

      Dotted_Node : Libadalang.Analysis.Ada_Node := Node;

      Use_Snippets : Boolean := Self.Snippets_Enabled;

      --  Error recovery for Obj.XXX with XXX a keyword => LAL will often
      --  consider it as:
      --  - CallStmt
      --     - Dotted_Name
      --  - ErrorStmt/LoopStmt/etc.
      Error_Dotted_Recovery : constant Boolean :=
        Libadalang.Analysis.Is_Keyword
          (Token   => Token,
           Version => Libadalang.Common.Ada_2012)
        and then
          Libadalang.Common.Kind
            (Libadalang.Common.Data
               (Libadalang.Common.Previous
                  (Token, Exclude_Trivia => True)))
          = Libadalang.Common.Ada_Dot;
   begin

      --  Get the outermost dotted name of which node is a prefix, so that when
      --  completing in a situation such as the following:
      --
      --      Ada.Tex|
      --             ^ Cursor here
      --
      --  we get the DottedName node rather than just the "Tex" BaseId. We want
      --  the DottedName rather than the Id so as to get the proper completions
      --  (all elements in the "Ada" namespace).

      while not Dotted_Node.Is_Null and then Dotted_Node.Kind in
        Libadalang.Common.Ada_Single_Tok_Node | Ada_Dotted_Name
      loop
         if Dotted_Node.Parent.Kind = Ada_Dotted_Name
           and then Dotted_Node.Parent.As_Dotted_Name.F_Suffix = Dotted_Node
         then
            Dotted_Node := Dotted_Node.Parent;
         else
            exit;
         end if;
      end loop;

      --  Return immediately if we are dealing with a null node or if the
      --  node's parent is a Defining_Name, meaning that we are declaring a
      --  new symbol.

      if Dotted_Node.Is_Null or else
        (not Dotted_Node.Parent.Is_Null and then Dotted_Node.Parent.Kind in
           Libadalang.Common.Ada_Defining_Name_Range)
      then
         return;
      end if;

      Parent := Dotted_Node.Parent;
      Sibling := Dotted_Node.Next_Sibling;

      --  Return without asking Libadalang for completion results we are
      --  dealing with a syntax error or with a node list.
      if Dotted_Node.Kind in Libadalang.Common.Ada_Error_Decl_Range
        | Libadalang.Common.Ada_Ada_Node_List_Range
      then
         return;
      end if;

      --  Don't complete numeric literals, attributes nor end labels
      if Filter.Is_Numeric_Literal
        or else Filter.Is_Attribute_Ref
        or else Filter.Is_End_Label
      then
         return;
      end if;

      if not Sibling.Is_Null
        or else
          (not Parent.Is_Null
             and then Parent.Kind in Libadalang.Common.Ada_Param_Assoc_Range)
      then
         --  Snippets should not be used in the following cases:
         --
         --   . The Use_Snippets parameter if set to False
         --
         --   . When the queried node has a sibling: this is to avoid proposing
         --     snippets when a list of parameters is already present on the
         --     right of the completion point for instance.
         --
         --   . When we are providing an actual parameter to a subprogram call

         Use_Snippets := False;

         --  If we are dealing with an end label, just return the corresponding
         --  declaration's name: it's the only valid result in this case.
         if Parent.Kind in Ada_End_Name and then not Parent.Is_Null then
            Names.Include
              (Parent.As_End_Name.P_Basic_Decl.P_Defining_Name,
               (Is_Dot_Call  => False,
                Is_Visible   => True,
                Use_Snippets => Use_Snippets,
                Pos          => 1,
                Weight       => 0));
            return;
         end if;
      end if;

      declare
         use Libadalang.Analysis;

         Prefix              : constant VSS.Strings.Virtual_String :=
           VSS.Strings.To_Virtual_String (Node.Text);
         Raw_Completions     : constant Completion_Item_Iterator :=
           Dotted_Node.P_Complete;

         Item                : Completion_Item;
         BD                  : Basic_Decl;
         Completion_Count    : Natural := Natural (Result.items.Length);
         Name                : VSS.Strings.Virtual_String;
         Underscore          : constant VSS.Strings.Virtual_String := "_";

      begin
         while Next (Raw_Completions, Item) loop
            BD := Decl (Item).As_Basic_Decl;

            if not BD.Is_Null then
               for DN of BD.P_Defining_Names loop
                  Name :=
                    LSP.Lal_Utils.To_Virtual_String (DN.P_Relative_Name.Text);

                  if Name.Ends_With (Underscore) then
                     --  Skip `root_types_` until UB30-020 is fixed.
                     null;

                  --  If we are not completing a dotted name, filter the
                  --  raw completion results by the node's prefix.
                  elsif Dotted_Node.Kind in
                       Libadalang.Common.Ada_Dotted_Name_Range
                    or else Name.Starts_With
                      (Prefix, VSS.Strings.Identifier_Caseless)
                  then
                     Completion_Count := Completion_Count + 1;

                     Names.Include
                       (DN.P_Canonical_Part,
                        (Error_Dotted_Recovery or else Is_Dot_Call (Item),
                         Is_Visible (Item),
                         Use_Snippets,
                         Completion_Count,
                         Weight (Item)));
                  end if;
               end loop;
            end if;
         end loop;
      end;
   exception
      when Langkit_Support.Errors.Property_Error =>
         null;
   end Propose_Completion;

end LSP.Ada_Completions.Names;
