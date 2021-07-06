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

with Ada.Strings.UTF_Encoding;

with Langkit_Support.Text;

with VSS.Strings;

with LSP.Lal_Utils;
with LSP.Types;

package body LSP.Ada_Completions.Names is

   ------------------------
   -- Propose_Completion --
   ------------------------

   overriding procedure Propose_Completion
     (Self   :     Name_Completion_Provider;
      Sloc   :     Langkit_Support.Slocs.Source_Location;
      Token  :     Libadalang.Common.Token_Reference;
      Node   :     Libadalang.Analysis.Ada_Node;
      Names  : out Ada_Completions.Completion_Maps.Map;
      Result : out LSP.Messages.CompletionList)
   is
      pragma Unreferenced (Result);
      use all type Libadalang.Analysis.Base_Id;
      use all type Libadalang.Common.Ada_Node_Kind_Type;

      Parent   : Libadalang.Analysis.Ada_Node;
      --  The parent of the node to complete.

      Sibling  : Libadalang.Analysis.Ada_Node;
      --  The right sibling of the node to complete.

      In_End_Label : Boolean := False;
      --  Set to True if we are completing an end label (e.g: end <Subp_Name>);

      Dotted_Node : Libadalang.Analysis.Ada_Node := Node;

      Use_Snippets : Boolean := Self.Snippets_Enabled;

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

      --  Check if we are completing an end label. If it's the case, we want
      --  to disable snippets since end labels don't expect any parameters.
      In_End_Label := not Parent.Is_Null
        and then Parent.Kind in Libadalang.Common.Ada_End_Name_Range;

      --  Return without asing Libadalang for completion results we are dealing
      --  with a syntax error.
      if Dotted_Node.Kind in Libadalang.Common.Ada_Error_Decl_Range then
         return;
      end if;

--    Should_Use_Names := True;  --  Let's use defining names for completion

      if In_End_Label
        or else not Sibling.Is_Null
        or else
          (not Parent.Is_Null
             and then Parent.Kind in Libadalang.Common.Ada_Param_Assoc_Range)
      then
         --  Snippets should not be used in the following cases:
         --
         --   . The Use_Snippets parameter if set to False
         --
         --   . When the queried node is within an end label
         --
         --   . When the queried node has a sibling: this is to avoid proposing
         --     snippets when a list of parameters is already present on the
         --     right of the completion point for instance.
         --
         --   . When we are providing an actual parameter to a subprogram call

         Use_Snippets := False;
      end if;

      declare
         use Libadalang.Analysis;
         Prefix   : constant Ada.Strings.UTF_Encoding.UTF_8_String :=
           Langkit_Support.Text.To_UTF8 (Node.Text);

         Raw_Completions     : constant Completion_Item_Iterator :=
           Dotted_Node.P_Complete;

         Item                : Completion_Item;
         BD                  : Basic_Decl;
         Completion_Count    : Natural := 0;
         Name                : VSS.Strings.Virtual_String;

      begin
         while Next (Raw_Completions, Item) loop
            BD := Decl (Item).As_Basic_Decl;
            Completion_Count := Completion_Count + 1;

            if not BD.Is_Null then
               for DN of BD.P_Defining_Names loop
                  Name :=
                    LSP.Lal_Utils.To_Virtual_String (DN.P_Relative_Name.Text);

                  --  If we are not completing a dotted name, filter the
                  --  raw completion results by the node's prefix.
                  if Dotted_Node.Kind in
                    Libadalang.Common.Ada_Dotted_Name_Range

                    or else LSP.Types.Starts_With
                      (LSP.Types.LSP_String'(LSP.Types.To_LSP_String (Name)),
                       Prefix         => Prefix,
                       Case_Sensitive => False)
                  then
                     Names.Include
                       (DN, (Is_Dot_Call (Item), True, Use_Snippets));
                  end if;
               end loop;
            end if;
         end loop;
      end;
   end Propose_Completion;

end LSP.Ada_Completions.Names;
