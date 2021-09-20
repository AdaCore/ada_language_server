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

with Ada.Strings.UTF_Encoding;

with GNATCOLL.Utils;

with Langkit_Support.Text;       use Langkit_Support.Text;
with Laltools.Common;
with Libadalang.Common;

with LSP.Ada_Completions.Filters;
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
      Names  : out Ada_Completions.Completion_Maps.Map;
      Result : out LSP.Messages.CompletionList)
   is
      pragma Unreferenced (Filter, Names);
      use Libadalang.Analysis;
      use Libadalang.Common;

      Call_Expr_Node : constant Libadalang.Analysis.Call_Expr :=
        LSP.Lal_Utils.Get_Call_Expr (Node);
      Token_Kind     : Libadalang.Common.Token_Kind :=
        Kind (Data (Token));
      Designators : Laltools.Common.Node_Vectors.Vector;

      function Is_Present (Id_Text : Text_Type) return Boolean;
      --  Return True if Id_Name match one of the designators

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

   begin
      if Call_Expr_Node = No_Ada_Node then
         return;
      end if;

      Designators := LSP.Lal_Utils.Get_Call_Designators (Call_Expr_Node);

      if Token_Kind = Ada_Whitespace then
         Token_Kind := Kind (Data (Previous (Token, Exclude_Trivia => True)));
      end if;

      declare
         Item        : LSP.Messages.CompletionItem;
         Prefix      : constant Ada.Strings.UTF_Encoding.UTF_8_String :=
           Langkit_Support.Text.To_UTF8 (Node.Text);
         Name_Node   : constant Libadalang.Analysis.Name :=
           Call_Expr_Node.F_Name;
      begin
         for N of Self.Context.Find_All_Env_Elements (Name_Node) loop
            if N.Kind in Ada_Basic_Subp_Decl then
               declare
                  Params_Snippet : LSP_String := Empty_LSP_String;
                  Snippet_Name   : LSP_String := Empty_LSP_String;
                  --  $0 is used for the final tab stop
                  Index          : Positive := 1;
                  Spec           : constant Libadalang.Analysis.Base_Subp_Spec
                    := N.As_Basic_Decl.P_Subp_Spec_Or_Null;
               begin
                  if Spec /= Libadalang.Analysis.No_Base_Subp_Spec
                    and then LSP.Lal_Utils.Match_Designators
                      (Spec.P_Params, Designators)
                  then
                     for Param of Spec.P_Params loop
                        for Id of Param.F_Ids loop
                           declare
                              Name_Text : constant Text_Type := Id.Text;
                              Name      : constant LSP_String :=
                                To_LSP_String (Name_Text);
                           begin
                              if not Is_Present (Name_Text) then
                                 if Token_Kind in Ada_Par_Open | Ada_Comma
                                   or else
                                     LSP.Types.Starts_With
                                       (Text           => Name,
                                        Prefix         => Prefix,
                                        Case_Sensitive => False)
                                 then
                                    Item.label := To_Virtual_String (Name);
                                    Item.insertTextFormat :=
                                      (True, LSP.Messages.PlainText);
                                    Item.insertText := (True, Name & " => ");
                                    Item.kind := (True, LSP.Messages.Text);
                                    Result.items.Append (Item);
                                 end if;

                                 Params_Snippet := Params_Snippet
                                   & Name
                                   & " => "
                                   & "$"
                                   & To_LSP_String
                                   (GNATCOLL.Utils.Image
                                      (Index, Min_Width => 1))
                                   & ", ";
                                 Index := Index + 1;
                              end if;
                           end;
                        end loop;
                     end loop;

                     --  If the string is empty => nothing to do
                     if Params_Snippet /= Empty_LSP_String then
                        --  Remove the last 2 characters which are ", " and
                        --  replace it by ")" and the final tab stop
                        Params_Snippet := Unbounded_Slice
                          (Params_Snippet,
                           1,
                           Length (Params_Snippet) - 2);
                        Params_Snippet := Params_Snippet & ")$0";

                        Snippet_Name :=
                          Snippet_Name
                          & "Params of "
                          & To_LSP_String (Name_Node.Text);
                        Item.label := To_Virtual_String
                          (Snippet_Name);
                        Item.insertTextFormat :=
                          (True, LSP.Messages.Snippet);
                        Item.insertText := (True, Params_Snippet);
                        Item.kind := (True, LSP.Messages.Snippet);
                        Result.items.Append (Item);
                     end if;
                  end if;
               end;
            end if;
         end loop;
      end;
   end Propose_Completion;

end LSP.Ada_Completions.Parameters;
