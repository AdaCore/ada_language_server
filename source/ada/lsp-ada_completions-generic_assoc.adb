------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2022, AdaCore                          --
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

with GNATCOLL.Utils;
with LSP.Ada_Documents;
with VSS.Strings.Character_Iterators;
with VSS.Strings.Conversions;

package body LSP.Ada_Completions.Generic_Assoc is

   ------------------------
   -- Propose_Completion --
   ------------------------

   procedure Propose_Completion
     (Self         :
        LSP.Ada_Completions.Parameters.Parameter_Completion_Provider;
      Sloc         : Langkit_Support.Slocs.Source_Location;
      Token        : Libadalang.Common.Token_Reference;
      Node         : Libadalang.Analysis.Ada_Node;
      Limit        : Natural;
      Filter       : in out LSP.Ada_Completions.Filters.Filter;
      Names        : in out Ada_Completions.Completion_Maps.Map;
      Unsorted_Res : in out LSP.Messages.CompletionItem_Vector)
   is
      pragma Unreferenced (Filter, Names);
      use Libadalang.Analysis;
      use Libadalang.Common;
      use LSP.Ada_Completions.Generic_Assoc_Utils;

      Elem_Node         : constant Element := Search_Element (Node);
      Token_Kind        : Libadalang.Common.Token_Kind := Kind (Data (Token));
      Whitespace_Prefix : VSS.Strings.Virtual_String;
      --  Empty if we already have a whitespace before a ","

      Designators       : Laltools.Common.Node_Vectors.Vector;

      Prefix      : VSS.Strings.Virtual_String;
      --  The whole string before the snippet (including whitespaces)

      Column      : Langkit_Support.Slocs.Column_Number;
      --  Use Column as the block indentation

      Prefix_Span : LSP.Messages.Span;
      --  The span covering Prefix.

      function Match_Designators
        (Child  : Laltools.Common.Node_Vectors.Vector;
         Parent : Laltools.Common.Node_Vectors.Vector)
         return Boolean;
      --  Return True if all the designators of Child are also in Parent

      function In_Parent
        (Desg   : Libadalang.Analysis.Ada_Node;
         Parent : Laltools.Common.Node_Vectors.Vector) return Boolean;

      procedure Generate_Snippets
        (Spec_Designators  : Laltools.Common.Node_Vectors.Vector;
         Param_Types       : Param_To_Type_Maps.Map;
         Decl              : Basic_Decl;
         Title             : VSS.Strings.Virtual_String;
         Snippet_Prefix    : VSS.Strings.Virtual_String;
         Completion_Prefix : VSS.Strings.Virtual_String);

      ---------------
      -- In_Parent --
      ---------------

      function In_Parent
        (Desg   : Libadalang.Analysis.Ada_Node;
         Parent : Laltools.Common.Node_Vectors.Vector) return Boolean
      is
         Desg_Text : constant Langkit_Support.Text.Text_Type := Desg.Text;
      begin
         for P of Parent loop
            if P.Text = Desg_Text then
               return True;
            end if;
         end loop;

         return False;
      end In_Parent;

      -----------------------
      -- Match_Designators --
      -----------------------

      function Match_Designators
        (Child  : Laltools.Common.Node_Vectors.Vector;
         Parent : Laltools.Common.Node_Vectors.Vector)
      return Boolean  is
      begin
         for C of Child loop
            if not In_Parent (C, Parent) then
               return False;
            end if;
         end loop;

         return True;
      end Match_Designators;

      -----------------------
      -- Generate_Snippets --
      -----------------------

      procedure Generate_Snippets
        (Spec_Designators  : Laltools.Common.Node_Vectors.Vector;
         Param_Types       : Param_To_Type_Maps.Map;
         Decl              : Basic_Decl;
         Title             : VSS.Strings.Virtual_String;
         Snippet_Prefix    : VSS.Strings.Virtual_String;
         Completion_Prefix : VSS.Strings.Virtual_String)
      is
         Params_Snippet     : VSS.Strings.Virtual_String;
         Snippet_Index      : Integer :=
           Integer (Spec_Designators.Length);
         Use_Named_Notation : constant Boolean :=
           (not Designators.Is_Empty)
           or else (Limit > 0
                    and then (Snippet_Index = 1
                              or else Snippet_Index >= Limit));
      begin
         if Match_Designators (Designators, Spec_Designators) then

            for Desg of reverse Spec_Designators loop
               declare
                  Name      : constant VSS.Strings.Virtual_String :=
                    VSS.Strings.To_Virtual_String (Desg.Text);
                  Item      : LSP.Messages.CompletionItem;
                  Snippet   : VSS.Strings.Virtual_String;
                  Type_Text : constant VSS.Strings.Virtual_String :=
                    (if Param_Types.Contains (Desg)
                     then VSS.Strings.To_Virtual_String
                       (Param_Types (Desg).Node.Text)
                     else "");
                  Doc       : VSS.Strings.Virtual_String;
               begin
                  --  Check if Desg is already present
                  if not In_Parent (Desg, Designators) then
                     --  Add snippet for Desg if it matches the
                     --  current prefix
                     if Token_Kind in Ada_Par_Open | Ada_Comma
                       or else
                         Name.Starts_With
                           (Completion_Prefix,
                            VSS.Strings.Identifier_Caseless)
                     then
                        --  Snippet Format: "Name => "
                        Item.label := Name;
                        Item.insertTextFormat :=
                          (True, LSP.Messages.PlainText);
                        Item.insertText := (True, Value => <>);
                        Item.insertText.Value.Append
                          (Whitespace_Prefix);
                        Item.insertText.Value.Append (Name);
                        Item.insertText.Value.Append (" => ");
                        Item.kind := (True, LSP.Messages.Field);
                        Doc := Item.insertText.Value;

                        if Param_Types (Desg).Is_Value then
                           Item.insertText.Value.Append (Type_Text);
                           Item.label.Append (" => ");
                           Item.label.Append (Type_Text);
                        end if;

                        Item.documentation :=
                          (Is_Set => True,
                           Value  => LSP.Messages.String_Or_MarkupContent'
                             (Is_String => True,
                              String    => Doc));
                        Unsorted_Res.Append (Item);

                     end if;

                     if Use_Named_Notation then
                        --  If Type_Text : "Name => ${idx:Type}"
                        --  Else: "Name => $idx"
                        Snippet.Append (Name);
                        Snippet.Append (" => ");
                        if Param_Types (Desg).Is_Value then
                           Snippet.Append (Type_Text);
                        else
                           Snippet.Append ("$");
                           if not Type_Text.Is_Empty then
                              Snippet.Append ("{");
                           end if;
                           Snippet.Append
                             (VSS.Strings.Conversions.To_Virtual_String
                                (GNATCOLL.Utils.Image
                                     (Snippet_Index, Min_Width => 1)));
                           if not Type_Text.Is_Empty then
                              Snippet.Append (":");
                              Snippet.Append (Type_Text);
                              Snippet.Append ("}");
                           end if;
                        end if;
                     else
                        --  If Type_Text : "${idx:Name : Type}"
                        --  Else: "${idx:Name}"
                        if Param_Types (Desg).Is_Value then
                           Snippet.Append (Type_Text);
                        else
                           Snippet.Append ("${");
                           Snippet.Append
                             (VSS.Strings.Conversions.To_Virtual_String
                                (GNATCOLL.Utils.Image
                                     (Snippet_Index, Min_Width => 1)));
                           Snippet.Append (":");
                           Snippet.Append (Name);
                           if not Type_Text.Is_Empty then
                              Snippet.Append (" : ");
                              Snippet.Append (Type_Text);
                           end if;
                           Snippet.Append ("}");
                        end if;
                     end if;
                     Snippet.Append (", ");
                     Params_Snippet.Prepend (Snippet);
                  end if;
                  Snippet_Index := Snippet_Index - 1;
               end;
            end loop;

            --  If the string is empty => nothing to do
            if not Params_Snippet.Is_Empty
              and then Token_Kind in Ada_Par_Open | Ada_Comma
            then
               declare
                  Last    : VSS.Strings.Character_Iterators.Character_Iterator
                    := Params_Snippet.At_Last_Character;
                  Success : Boolean with Unreferenced;

               begin
                  --  Remove the last 2 characters which are ", " and
                  --  replace it by ")" and the final tab stop
                  Success := Last.Backward;
                  Success := Last.Backward;

                  Params_Snippet :=
                    Params_Snippet.Slice
                      (Params_Snippet.At_First_Character, Last);
                  Params_Snippet.Append (")$0");
               end;

               Params_Snippet.Prepend (Snippet_Prefix);

               declare
                  Item   : LSP.Messages.CompletionItem;
               begin
                  Item.label := Title;
                  Item.insertTextFormat :=
                    (True, LSP.Messages.Snippet);
                  Item.insertText := (True, Value => <>);
                  Item.insertText.Value.Append (Whitespace_Prefix);
                  Item.insertText.Value.Append (Params_Snippet);
                  Item.kind := (True, LSP.Messages.Snippet);
                  LSP.Ada_Documents.Set_Completion_Item_Documentation
                    (Context                 => Self.Context.all,
                     BD                      => Decl,
                     Item                    => Item,
                     Compute_Doc_And_Details =>
                       Self.Compute_Doc_And_Details);
                  Pretty_Print_Snippet
                    (Context => Self.Context.all,
                     Prefix  =>
                       VSS.Strings.Conversions.To_UTF_8_String (Prefix),
                     --  "column = offset - 1"
                     Offset  => Integer (Column) - 1,
                     Span    => Prefix_Span,
                     Rule    => Pretty_Print_Rule,
                     Result  => Item);
                  Unsorted_Res.Append (Item);
               end;
            end if;
         end if;
      end Generate_Snippets;

   begin
      if Elem_Node = Null_Element then
         return;
      end if;

      Prefix_Span :=
        Self.Document.To_LSP_Range
          (Langkit_Support.Slocs.Make_Range
             (Langkit_Support.Slocs.Start_Sloc
                (Get_Prefix_Node (Elem_Node, Column => Column).Sloc_Range),
              Sloc));
      Prefix := Self.Document.Get_Text_At
        (Prefix_Span.first, Prefix_Span.last);

      Designators := Get_Designators (Elem_Node);

      if Token_Kind = Ada_Whitespace then
         Token_Kind := Kind (Data (Previous (Token, Exclude_Trivia => True)));

      elsif Token_Kind = Ada_Comma then
         Whitespace_Prefix.Append (" ");
      end if;

      declare
         Completion_Prefix : constant VSS.Strings.Virtual_String :=
           VSS.Strings.To_Virtual_String (Node.Text);
      begin
         for Spec of Get_Spec_Designators
           (E       => Elem_Node,
            Context => Self.Context)
         loop
            Generate_Snippets
              (Spec_Designators  => Spec.Param_Vector,
               Param_Types       => Spec.Param_Types,
               Decl              => Spec.Decl,
               Title             => Spec.Title,
               Snippet_Prefix    => Spec.Prefix,
               Completion_Prefix => Completion_Prefix);
         end loop;
      end;
   end Propose_Completion;

end LSP.Ada_Completions.Generic_Assoc;
