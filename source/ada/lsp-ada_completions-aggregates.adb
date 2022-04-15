------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2018-2022, AdaCore                     --
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

with VSS.Strings.Character_Iterators;
with VSS.Strings.Conversions;

with LSP.Lal_Utils;

package body LSP.Ada_Completions.Aggregates is

   procedure Get_Aggregate_Completion
     (Node                     : Libadalang.Analysis.Aggregate;
      Named_Notation_Threshold : Natural;
      Result                   : out LSP.Messages.CompletionItem_Vector);

   ------------------------------
   -- Get_Aggregate_Completion --
   ------------------------------

   procedure Get_Aggregate_Completion
     (Node                     : Libadalang.Analysis.Aggregate;
      Named_Notation_Threshold : Natural;
      Result                   : out LSP.Messages.CompletionItem_Vector)
   is
      use Libadalang.Analysis;
      use Libadalang.Common;
      use LSP.Messages;

      Expr_Type : constant Base_Type_Decl := Node.P_Expression_Type;
      Aggr_Type : constant Base_Type_Decl :=
        (if Expr_Type.Is_Null then No_Base_Type_Decl else Expr_Type);

      Use_Named_Notation : Boolean := False;

      function Get_Snippet_For_Component
        (Param              : Base_Formal_Param_Decl;
         Idx                : in out Natural;
         Use_Named_Notation : Boolean) return VSS.Strings.Virtual_String;
      --  Return a snippet for the given component

      function Get_Snippet_For_Discriminant
        (Disc               : Discriminant_Values;
         Idx                : Natural;
         Use_Named_Notation : Boolean) return VSS.Strings.Virtual_String;
      --  Return a snippet for the given discriminant

      function Get_Label_For_Shape
        (Discriminants : Discriminant_Values_Array)
         return VSS.Strings.Virtual_String;
      --  Return a suitable label for the given shape (i.e: a shape corresponds
      --  to one of the various forms that a discriminated variant record can
      --  take).

      -------------------------------
      -- Get_Snippet_For_Component --
      -------------------------------

      function Get_Snippet_For_Component
        (Param              : Base_Formal_Param_Decl;
         Idx                : in out Natural;
         Use_Named_Notation : Boolean) return VSS.Strings.Virtual_String
      is
         Snippet    : VSS.Strings.Virtual_String;
         Param_Ids  : constant Defining_Name_List :=
           (case Param.Kind is
               when Ada_Component_Decl_Range =>
                 As_Component_Decl (Param).F_Ids,
               when Ada_Discriminant_Spec    =>
                 As_Discriminant_Spec (Param).F_Ids,
               when others                   => No_Defining_Name_List);
         Param_Type : constant Type_Expr :=
           (case Param.Kind is
               when Ada_Component_Decl_Range =>
                 As_Component_Decl (Param).F_Component_Def.F_Type_Expr,
               when Ada_Discriminant_Spec    =>
                 As_Discriminant_Spec (Param).F_Type_Expr,
               when others                   => No_Type_Expr);

      begin
         for Id of Param_Ids loop
            if Use_Named_Notation then
               Snippet.Append (VSS.Strings.To_Virtual_String (Id.Text));
               Snippet.Append (" => ");
               Snippet.Append ("${");
               Snippet.Append
                 (VSS.Strings.Conversions.To_Virtual_String
                    (GNATCOLL.Utils.Image (Idx, Min_Width => 1)));
               Snippet.Append (":");
               Snippet.Append
                 (VSS.Strings.To_Virtual_String (Param_Type.Text));
               Snippet.Append ("}, ");
            else
               Snippet.Append ("${");
               Snippet.Append
                 (VSS.Strings.Conversions.To_Virtual_String
                    (GNATCOLL.Utils.Image (Idx, Min_Width => 1)));
               Snippet.Append (":");
               Snippet.Append (VSS.Strings.To_Virtual_String (Id.Text));
               Snippet.Append (" : ");
               Snippet.Append
                 (VSS.Strings.To_Virtual_String (Param_Type.Text));
               Snippet.Append ("}, ");
            end if;

            Idx := Idx + 1;
         end loop;

         Idx := Idx - 1;

         return Snippet;
      end Get_Snippet_For_Component;

      ----------------------------------
      -- Get_Snippet_For_Discriminant --
      ----------------------------------

      function Get_Snippet_For_Discriminant
        (Disc               : Discriminant_Values;
         Idx                : Natural;
         Use_Named_Notation : Boolean) return VSS.Strings.Virtual_String
      is
         Snippet    : VSS.Strings.Virtual_String;
         Values     : constant Alternatives_List'Class :=
           Libadalang.Analysis.Values (Disc);
         Value_Node : Ada_Node;

      begin
         if Use_Named_Notation then
            if Values.Children_Count = 1 then
               Value_Node := Values.Child (Values.First_Child_Index);

               Snippet.Append
                 (VSS.Strings.To_Virtual_String (Discriminant (Disc).Text));
               Snippet.Append (" => ");

               if Value_Node.Kind in Ada_Others_Designator_Range then
                  Snippet.Append  ("${");
                  Snippet.Append
                    (VSS.Strings.Conversions.To_Virtual_String
                       (GNATCOLL.Utils.Image (Idx, Min_Width => 1)));
                  Snippet.Append (":");
                  Snippet.Append
                    (VSS.Strings.To_Virtual_String (Values.Text));
                  Snippet.Append ("}, ");

               else
                  Snippet.Append
                    (VSS.Strings.To_Virtual_String (Values.Text));
                  Snippet.Append (", ");
               end if;

            else
               Snippet.Append
                 (VSS.Strings.To_Virtual_String (Discriminant (Disc).Text));
               Snippet.Append (" => ");
               Snippet.Append ("${");
               Snippet.Append
                 (VSS.Strings.Conversions.To_Virtual_String
                    (GNATCOLL.Utils.Image (Idx, Min_Width => 1)));
               Snippet.Append (":");
               Snippet.Append (VSS.Strings.To_Virtual_String (Values.Text));
               Snippet.Append ("}, ");
            end if;

         else
            if Values.Children_Count = 1 then
               Value_Node := Values.Child (Values.First_Child_Index);

               if Value_Node.Kind in Ada_Others_Designator_Range then
                  Snippet.Append ("${");
                  Snippet.Append
                    (VSS.Strings.Conversions.To_Virtual_String
                       (GNATCOLL.Utils.Image (Idx, Min_Width => 1)));
                  Snippet.Append (":");
                  Snippet.Append
                    (VSS.Strings.To_Virtual_String (Values.Text));
                  Snippet.Append ("}, ");

               else
                  Snippet.Append
                    (VSS.Strings.To_Virtual_String (Values.Text));
                  Snippet.Append (", ");
               end if;

            else
               Snippet.Append ("${");
               Snippet.Append
                 (VSS.Strings.Conversions.To_Virtual_String
                    (GNATCOLL.Utils.Image (Idx, Min_Width => 1)));
               Snippet.Append (":");
               Snippet.Append (VSS.Strings.To_Virtual_String (Values.Text));
               Snippet.Append ("}, ");
            end if;
         end if;

         return Snippet;
      end Get_Snippet_For_Discriminant;

      -------------------------
      -- Get_Label_For_Shape --
      -------------------------

      function Get_Label_For_Shape
        (Discriminants : Discriminant_Values_Array)
         return VSS.Strings.Virtual_String
      is
         Result : VSS.Strings.Virtual_String;
         Length : constant Integer := Discriminants'Length;

      begin
         if Length = 0 then
            return
              LSP.Lal_Utils.To_Virtual_String
                ("Aggregate for " & Aggr_Type.F_Name.Text);
         end if;

         Result := "Aggregate when ";

         for Idx in Discriminants'Range loop
            declare
               Disc_Values : constant Discriminant_Values :=
                 Discriminants (Idx);
            begin
               Result.Append
                 (LSP.Lal_Utils.To_Virtual_String
                    (Discriminant (Disc_Values).Text));
               Result.Append (" => ");

               Result.Append
                 (LSP.Lal_Utils.To_Virtual_String (Values (Disc_Values).Text));

               if Idx < Discriminants'Length then
                  Result.Append (", ");
               end if;
            end;
         end loop;

         return Result;
      end Get_Label_For_Shape;

   begin
      --  If the aggregate node has no type (e.g: representation clauses),
      --  return immediately.
      if Expr_Type.Is_Null then
         return;
      end if;

      if Aggr_Type.Kind in Ada_Type_Decl_Range then
         declare
            Shapes        : constant Libadalang.Analysis.Shape_Array :=
              Aggr_Type.P_Shapes
                (Include_Discriminants => False,
                 Origin                => Node);
            Item          : CompletionItem;
            Idx           : Positive := 1;
            Nb_Components : Natural := 0;
            Insert_Text   : VSS.Strings.Virtual_String;
            Base_Type     : constant Base_Type_Decl := Aggr_Type.P_Base_Type
              (Origin => Node);

         begin
            for Shape of Shapes loop
               declare
                  Discriminants : constant Discriminant_Values_Array :=
                    Discriminants_Values (Shape);
                  Components    : constant Base_Formal_Param_Decl_Array :=
                    Libadalang.Analysis.Components (Shape);
               begin
                  Item.label := Get_Label_For_Shape (Discriminants);
                  Item.kind :=
                    (True, LSP.Messages.Snippet);
                  Item.detail :=
                    (True,
                     LSP.Lal_Utils.Compute_Completion_Detail
                       (Aggr_Type.As_Basic_Decl));
                  Item.insertTextFormat :=
                    Optional_InsertTextFormat'
                      (Is_Set => True,
                       Value  => Snippet);
                  Insert_Text.Clear;

                  --  Compute number of components to know if named notation
                  --  should be used.

                  for Comp of Components loop
                     Nb_Components := Nb_Components
                       + As_Component_Decl (Comp).F_Ids.Children_Count;
                  end loop;

                  Nb_Components := Nb_Components + Discriminants'Length;

                  --  Use the named notation if we need to specify only one
                  --  component (otherwise it won't compile) or if the number
                  --  of components is greater or equal than the theshold.
                  Use_Named_Notation := Named_Notation_Threshold > 0
                    and then
                      (Nb_Components = 1
                          or else Nb_Components >= Named_Notation_Threshold);

                  --  If we are dealing with a derived type that does not have
                  --  access to its parent full view, we should use the
                  --  extension aggregate notation (see RM 4.3.2 for more
                  --  info).
                  if not Base_Type.Is_Null and then Base_Type.P_Is_Private then
                     Insert_Text :=
                       VSS.Strings.To_Virtual_String
                         (Base_Type.F_Name.Text & " with ");

                     declare
                        Base_Discs : constant Base_Formal_Param_Decl_Array :=
                          Base_Type.P_Discriminants_List;
                     begin
                        --  Take into account the base type discriminants to
                        --  know if we should use the named notation or not.

                        Nb_Components := Nb_Components + Base_Discs'Length;

                        Use_Named_Notation := Named_Notation_Threshold > 0
                          and then
                            (Nb_Components = 1
                                    or else Nb_Components
                                    >= Named_Notation_Threshold);

                        for Disc of Base_Discs loop
                           Insert_Text.Append
                             (Get_Snippet_For_Component
                                (Param              => Disc,
                                 Idx                => Idx,
                                 Use_Named_Notation => Use_Named_Notation));

                           Idx := Idx + 1;
                        end loop;
                     end;
                  end if;

                  --  Compute the snippets for the record discriminants, if any
                  for Disc of Discriminants loop
                     Insert_Text.Append
                       (Get_Snippet_For_Discriminant
                          (Disc               => Disc,
                           Idx                => Idx,
                           Use_Named_Notation => Use_Named_Notation));
                     Idx := Idx + 1;
                  end loop;

                  --  Compute the snippets for the record components
                  for Comp of Components loop
                     Insert_Text.Append
                       (Get_Snippet_For_Component
                          (Param              => Comp,
                           Idx                => Idx,
                           Use_Named_Notation => Use_Named_Notation));

                     Idx := Idx + 1;
                  end loop;

                  if Idx > 1 then
                     declare
                        Last     :
                          VSS.Strings.Character_Iterators.Character_Iterator
                            := Insert_Text.At_Last_Character;
                        Success  : Boolean with Unreferenced;

                     begin
                        --  Remove the "}, " substring that has been
                        --  appended in the last loop iteration.

                        Success := Last.Backward;
                        Success := Last.Backward;

                        Insert_Text :=
                          Insert_Text.Slice
                            (Insert_Text.At_First_Character, Last);

                        --  Insert '$0' (i.e: the final tab stop) at the
                        --  end.
                        Insert_Text.Append (")$0");

                        Item.insertText := (True, Insert_Text);
                        Result.Append (Item);
                     end;
                  end if;
               end;
            end loop;
         end;
      end if;
   end Get_Aggregate_Completion;

   ------------------------
   -- Propose_Completion --
   ------------------------

   overriding procedure Propose_Completion
     (Self   :     Aggregate_Completion_Provider;
      Sloc   :     Langkit_Support.Slocs.Source_Location;
      Token  :     Libadalang.Common.Token_Reference;
      Node   :     Libadalang.Analysis.Ada_Node;
      Filter : in out LSP.Ada_Completions.Filters.Filter;
      Names  : in out Ada_Completions.Completion_Maps.Map;
      Result : in out LSP.Messages.CompletionList)
   is
      pragma Unreferenced (Filter);
      pragma Unreferenced (Names);
   begin
      if Node.Kind in Libadalang.Common.Ada_Aggregate_Range then
         Get_Aggregate_Completion
           (Node                     => Node.As_Aggregate,
            Named_Notation_Threshold => Self.Named_Notation_Threshold,
            Result                   => Result.items);
      end if;
   end Propose_Completion;

end LSP.Ada_Completions.Aggregates;
