------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2021, AdaCore                          --
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

with Langkit_Support.Text;       use Langkit_Support.Text;
with Laltools.Common;
with Libadalang.Common;

with VSS.Strings;

with LSP.Ada_Completions.Filters;
with LSP.Ada_Documents;
with LSP.Lal_Utils;
with LSP.Types;                   use LSP.Types;

package body LSP.Ada_Completions.Parameters is

   ------------------------
   -- Propose_Completion --
   ------------------------

   overriding procedure Propose_Completion
     (Self   : Parameter_Completion_Provider;
      Sloc   : Langkit_Support.Slocs.Source_Location;
      Token  : Libadalang.Common.Token_Reference;
      Node   : Libadalang.Analysis.Ada_Node;
      Filter : in out LSP.Ada_Completions.Filters.Filter;
      Names  : in out Ada_Completions.Completion_Maps.Map;
      Result : in out LSP.Messages.CompletionList)
   is
      pragma Unreferenced (Filter);
      use Libadalang.Analysis;
      use Libadalang.Common;

      Call_Expr_Node    : constant Libadalang.Analysis.Call_Expr :=
        LSP.Lal_Utils.Get_Call_Expr (Node);
      Token_Kind        : Libadalang.Common.Token_Kind :=
        Kind (Data (Token));
      Whitespace_Prefix : LSP_String := Empty_LSP_String;
      --  Empty if we already have a whitespace before a ","

      Designators     : Laltools.Common.Node_Vectors.Vector;

      function Is_Present (Id_Text : Text_Type) return Boolean;
      --  Return True if Id_Name match one of the designators

      function Get_Param_Name_List
        (Spec          : Libadalang.Analysis.Base_Subp_Spec;
         Exclude_First : Boolean;
         Length        : out Integer)
         return Laltools.Common.Node_Vectors.Vector;
      --  Return the list of parameters for Spec.

      ----------------
      -- Is_Present --
      ----------------

      function Is_Present (Id_Text : Text_Type) return Boolean is
      begin
         for Desig of Designators loop
            if Id_Text = Desig.Text then
               return True;
            end if;
         end loop;
         return False;
      end Is_Present;

      -------------------------
      -- Get_Param_Name_List --
      -------------------------

      function Get_Param_Name_List
        (Spec          : Libadalang.Analysis.Base_Subp_Spec;
         Exclude_First : Boolean;
         Length        : out Integer)
         return Laltools.Common.Node_Vectors.Vector
      is
         Is_First_Param : Boolean := True;
         Res            : Laltools.Common.Node_Vectors.Vector;
      begin
         for Param of Spec.P_Params loop
            for Id of Param.F_Ids loop
               if not (Is_First_Param and then Exclude_First) then
                  Res.Append (Id.As_Ada_Node);
               end if;
               Is_First_Param := False;
            end loop;
         end loop;

         Length := Integer (Res.Length);
         return Res;
      end Get_Param_Name_List;

   begin
      if Call_Expr_Node = No_Ada_Node then
         return;
      end if;

      Designators := LSP.Lal_Utils.Get_Call_Designators (Call_Expr_Node);

      if Token_Kind = Ada_Whitespace then
         Token_Kind := Kind (Data (Previous (Token, Exclude_Trivia => True)));
      elsif Token_Kind = Ada_Comma then
         Whitespace_Prefix := Whitespace_Prefix & " ";
      end if;

      declare
         Prefix    : constant VSS.Strings.Virtual_String :=
           VSS.Strings.To_Virtual_String (Node.Text);
         Name_Node : constant Libadalang.Analysis.Name :=
           Call_Expr_Node.F_Name;

         Is_Dotted_Name : constant Boolean :=
           (Name_Node.Kind in Ada_Dotted_Name_Range
            and then Name_Node.As_Dotted_Name.P_Is_Dot_Call (True));

         Unsorted_Res   : LSP.Messages.CompletionItem_Vector;
      begin
         for N of reverse Self.Context.Find_All_Env_Elements (Name_Node) loop
            if N.Kind in Ada_Basic_Subp_Decl then
               declare
                  Params_Snippet : LSP_String := Empty_LSP_String;

                  Spec           : constant Libadalang.Analysis.Base_Subp_Spec
                    := N.As_Basic_Decl.P_Subp_Spec_Or_Null;

                  Snippet_Index  : Integer;
               begin
                  if Spec /= Libadalang.Analysis.No_Base_Subp_Spec
                    and then LSP.Lal_Utils.Match_Designators
                      (Spec.P_Params, Designators)
                  then
                     for N of reverse Get_Param_Name_List
                       (Spec          => Spec,
                        Exclude_First => Is_Dotted_Name,
                        Length        => Snippet_Index)
                     loop
                        declare
                           Name_Text : constant Text_Type := N.Text;
                           Name      : constant VSS.Strings.Virtual_String :=
                             VSS.Strings.To_Virtual_String (Name_Text);
                           Item      : LSP.Messages.CompletionItem;

                        begin
                           if not Is_Present (Name_Text) then
                              if Token_Kind in Ada_Par_Open | Ada_Comma
                                or else
                                  Name.Starts_With
                                    (Prefix,
                                     VSS.Strings.Identifier_Caseless)
                              then
                                 Item.label := Name;
                                 Item.insertTextFormat :=
                                   (True, LSP.Messages.PlainText);
                                 Item.insertText :=
                                   (True,
                                    Whitespace_Prefix
                                    & LSP.Types.To_LSP_String (Name)
                                    & " => ");
                                 Item.kind := (True, LSP.Messages.Field);
                                 Unsorted_Res.Append (Item);
                              end if;

                              Params_Snippet :=
                                LSP.Types.To_LSP_String (Name)
                                & " => "
                                & "$"
                                & To_LSP_String
                                (GNATCOLL.Utils.Image
                                   (Snippet_Index, Min_Width => 1))
                                & ", "
                                & Params_Snippet;
                           end if;
                           Snippet_Index := Snippet_Index - 1;
                        end;
                     end loop;

                     --  If the string is empty => nothing to do
                     if Params_Snippet /= Empty_LSP_String
                       and then Token_Kind in Ada_Par_Open | Ada_Comma
                     then
                        --  Remove the last 2 characters which are ", " and
                        --  replace it by ")" and the final tab stop
                        Params_Snippet := Unbounded_Slice
                          (Params_Snippet,
                           1,
                           Length (Params_Snippet) - 2);
                        Params_Snippet := Params_Snippet & ")$0";

                        declare
                           Snippet_Name : constant LSP_String :=
                             "Params of " & To_LSP_String (Name_Node.Text);
                           Item         : LSP.Messages.CompletionItem;
                        begin
                           Item.label := To_Virtual_String
                             (Snippet_Name);
                           Item.insertTextFormat :=
                             (True, LSP.Messages.Snippet);
                           Item.insertText :=
                             (True, Whitespace_Prefix & Params_Snippet);
                           Item.kind := (True, LSP.Messages.Snippet);
                           LSP.Ada_Documents.Set_Completion_Item_Documentation
                             (Context                 => Self.Context.all,
                              BD                      => N.As_Basic_Decl,
                              Item                    => Item,
                              Compute_Doc_And_Details =>
                                Self.Compute_Doc_And_Details);
                           Unsorted_Res.Append (Item);
                        end;
                     end if;
                  end if;
               end;
            end if;
         end loop;

         declare
            Min_Width : constant Natural := Unsorted_Res.Length'Img'Length - 1;
            Cpt       : Natural := 0;
         begin
            for Unsort_Item of reverse Unsorted_Res loop
               --  Use a "+" as the first sorted character to be shown before
               --  the items from the other providers ("+" is lower than
               --  the alphanumeric symbol and "~" in the ASCII table)
               Unsort_Item.sortText :=
                 (True,
                  To_LSP_String
                    ("+"
                     & GNATCOLL.Utils.Image (Cpt, Min_Width => Min_Width)));
               Result.items.Append (Unsort_Item);
               Cpt := Cpt + 1;
            end loop;
         end;
      end;
   end Propose_Completion;

end LSP.Ada_Completions.Parameters;
