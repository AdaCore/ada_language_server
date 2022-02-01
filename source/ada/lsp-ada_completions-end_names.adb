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

with LSP.Lal_Utils;

package body LSP.Ada_Completions.End_Names is
   --  List on Ada 2012 syntax rules with <end> token:
   --  accept_statement
   --  asynchronous_select
   --  block_statement
   --  case_statement
   --  entry_body
   --  extended_return_statement
   --  function_body
   --  generic_package_declaration
   --  if_statement
   --  loop_statement
   --  package_body
   --  package_declaration
   --  procedure_body
   --  protected_body
   --  protected_definition
   --  record_definition
   --  record_representation_clause
   --  selective_accept
   --  task_body
   --  task_definition
   --  variant_part

   ------------------------
   -- Propose_Completion --
   ------------------------

   overriding procedure Propose_Completion
     (Self   :        End_Name_Completion_Provider;
      Sloc   :        Langkit_Support.Slocs.Source_Location;
      Token  :        Libadalang.Common.Token_Reference;
      Node   :        Libadalang.Analysis.Ada_Node;
      Filter : in out LSP.Ada_Completions.Filters.Filter;
      Names  : in out Ada_Completions.Completion_Maps.Map;
      Result : in out LSP.Messages.CompletionList)
   is
      pragma Unreferenced (Filter);
      use type Libadalang.Common.Ada_Node_Kind_Type;
      use type Libadalang.Common.Token_Reference;

      function Get_End_Text
        (End_Token : Libadalang.Common.Token_Data_Type)
          return VSS.Strings.Virtual_String;
      --  Return text after <end> token for given Node

      function Statement_Name (Node : Libadalang.Analysis.Ada_Node)
        return VSS.Strings.Virtual_String;

      function Protected_Name (Node : Libadalang.Analysis.Ada_Node)
        return VSS.Strings.Virtual_String;

      function Task_Name (Node : Libadalang.Analysis.Ada_Node)
        return VSS.Strings.Virtual_String;

      ------------------
      -- Get_End_Text --
      ------------------

      function Get_End_Text
        (End_Token : Libadalang.Common.Token_Data_Type)
          return VSS.Strings.Virtual_String
      is
         use type VSS.Strings.Virtual_String;

         Sloc : constant Langkit_Support.Slocs.Source_Location_Range :=
           Libadalang.Common.Sloc_Range (End_Token);
         From : constant Langkit_Support.Slocs.Source_Location :=
           Langkit_Support.Slocs.Start_Sloc (Sloc);
         Unit : constant Libadalang.Analysis.Analysis_Unit := Node.Unit;
         Node : constant Libadalang.Analysis.Ada_Node :=
           Unit.Root.Lookup (From);
      begin
         case Node.Kind is
            when Libadalang.Common.Ada_Accept_Stmt_With_Stmts_Range =>

               return VSS.Strings.To_Virtual_String
                 (Node.As_Accept_Stmt_With_Stmts.F_Name.Text);

            when Libadalang.Common.Ada_Select_Stmt_Range =>
               return "select";

            when Libadalang.Common.Ada_Begin_Block_Range
               | Libadalang.Common.Ada_Decl_Block_Range =>

               return Statement_Name (Node);

            when Libadalang.Common.Ada_Case_Stmt_Range =>
               return "case";

            when Libadalang.Common.Ada_Entry_Body_Range =>

               return VSS.Strings.To_Virtual_String
                 (Node.As_Entry_Body.F_Entry_Name.Text);

            when Libadalang.Common.Ada_Extended_Return_Stmt_Range =>
               return "return";

            when Libadalang.Common.Ada_Subp_Body_Range =>

               return VSS.Strings.To_Virtual_String
                 (Node.As_Base_Subp_Body.F_Subp_Spec.F_Subp_Name.Text);

            when Libadalang.Common.Ada_Generic_Package_Internal_Range =>

               return VSS.Strings.To_Virtual_String
                 (Node.As_Generic_Package_Internal.F_Package_Name.Text);

            when Libadalang.Common.Ada_If_Stmt_Range =>
               return "if";

            when Libadalang.Common.Ada_Base_Loop_Stmt =>

               declare
                  Name : constant VSS.Strings.Virtual_String :=
                    Statement_Name (Node);
               begin
                  if Name.Is_Empty then
                     return "loop";
                  else
                     return "loop " & Name;
                  end if;
               end;

            when Libadalang.Common.Ada_Package_Body_Range =>

               return VSS.Strings.To_Virtual_String
                 (Node.As_Package_Body.F_Package_Name.Text);

            when Libadalang.Common.Ada_Package_Decl_Range =>

               return VSS.Strings.To_Virtual_String
                 (Node.As_Package_Decl.F_Package_Name.Text);

            when Libadalang.Common.Ada_Protected_Body_Range =>

               return VSS.Strings.To_Virtual_String
                 (Node.As_Protected_Body.F_Name.Text);

            when Libadalang.Common.Ada_Protected_Def_Range =>

               return Protected_Name (Node.Parent);

            when Libadalang.Common.Ada_Record_Def_Range =>
               --  Syntax error recovery fails on this case
               return "record";

            when Libadalang.Common.Ada_Record_Rep_Clause_Range =>
               --  Syntax error recovery fails on this case
               return "record";

            when Libadalang.Common.Ada_Task_Body_Range =>

               return VSS.Strings.To_Virtual_String
                 (Node.As_Task_Body.F_Name.Text);

            when Libadalang.Common.Ada_Task_Def_Range =>

               return Task_Name (Node.Parent);

            when Libadalang.Common.Ada_Variant_Part_Range =>
               --  Syntax error recovery fails on this case
               return "case";

            when others =>
               return VSS.Strings.Empty_Virtual_String;
         end case;
      end Get_End_Text;

      --------------------
      -- Protected_Name --
      --------------------

      function Protected_Name (Node : Libadalang.Analysis.Ada_Node)
        return VSS.Strings.Virtual_String is
      begin
         case Node.Kind is
            when Libadalang.Common.Ada_Single_Protected_Decl_Range =>

               return VSS.Strings.To_Virtual_String
                 (Node.As_Single_Protected_Decl.F_Name.Text);

            when Libadalang.Common.Ada_Protected_Type_Decl_Range =>

               return VSS.Strings.To_Virtual_String
                 (Node.As_Protected_Type_Decl.F_Name.Text);

            when others =>
               return VSS.Strings.Empty_Virtual_String;
         end case;
      end Protected_Name;

      --------------------
      -- Statement_Name --
      --------------------

      function Statement_Name (Node : Libadalang.Analysis.Ada_Node)
        return VSS.Strings.Virtual_String
      is
         Parent : constant Libadalang.Analysis.Ada_Node :=
           Node.Parent;
      begin
         if Parent.Kind in Libadalang.Common.Ada_Named_Stmt_Range then

            return VSS.Strings.To_Virtual_String
              (Parent.As_Named_Stmt.F_Decl.F_Name.Text);

         else
            return "";

         end if;
      end Statement_Name;

      ---------------
      -- Task_Name --
      ---------------

      function Task_Name (Node : Libadalang.Analysis.Ada_Node)
        return VSS.Strings.Virtual_String
      is
      begin
         case Node.Kind is
            when Libadalang.Common.Ada_Task_Type_Decl_Range =>

               return VSS.Strings.To_Virtual_String
                 (Node.As_Task_Type_Decl.F_Name.Text);

            when others =>
               return VSS.Strings.Empty_Virtual_String;
         end case;
      end Task_Name;

      End_Text    : VSS.Strings.Virtual_String;
      Label       : VSS.Strings.Virtual_String;
      Has_Space   : Boolean := Libadalang.Common.Is_Trivia (Token);
      End_Token   : Libadalang.Common.Token_Data_Type;
      Item        : LSP.Messages.CompletionItem;
      Parent      : constant Libadalang.Analysis.Ada_Node :=
        (if Node.Is_Null then Node
         else LSP.Lal_Utils.Skip_Dotted_Names (Node.Parent));
      --  Skip the outermost dotted name enclosing Node.Parent, so
      --  that when completing in a situation such as the following:
      --
      --      end Ada.Tex|
      --                 ^ Cursor here
      --
      --  we get the DottedName node's parent rather than just the "Tex" Id.
      --  We want the DottedName parent rather than the Id so as to get the
      --  proper completions (all elements in the "Ada" namespace).

      Token_Reference : Libadalang.Common.Token_Reference := Token;

   begin
      if Token = Libadalang.Common.No_Token or else Node.Is_Null then
         return;

      elsif not Parent.Is_Null
        and then Parent.Kind = Libadalang.Common.Ada_End_Name
      then
         Token_Reference := Libadalang.Analysis.First_Token
           (Parent.Token_Range);

         Token_Reference := Libadalang.Common.Previous
           (Token_Reference, Exclude_Trivia => True);

         if LSP.Lal_Utils.Is_End_Token (Token_Reference) then
            End_Token := Libadalang.Common.Data (Token_Reference);
            Has_Space := True;  --  Fix for <end Prefix.Name|> case
         else
            return;
         end if;

      elsif LSP.Lal_Utils.Is_End_Token (Token) then
         End_Token := Libadalang.Common.Data (Token);

      else  --  The <end |> case:
         Token_Reference := Libadalang.Common.Previous
           (Token_Reference, Exclude_Trivia => True);

         if LSP.Lal_Utils.Is_End_Token (Token_Reference) then
            End_Token := Libadalang.Common.Data (Token_Reference);
            Has_Space := True;  --  Fix for <end something|> case
         else
            return;
         end if;
      end if;

      End_Text := Get_End_Text (End_Token);

      if End_Text.Is_Null then
         return;
      elsif not Has_Space then

         Label := VSS.Strings.To_Virtual_String
           (Libadalang.Common.Text (Token_Reference));

         if not End_Text.Is_Empty then
            Label.Append (' ');
         end if;
      end if;

      Label.Append (End_Text);
      Label.Append (';');

      Item := (label               => Label,
               kind                => (True, LSP.Messages.Keyword),
               tags                => (Is_Set => False),
               detail              => (Is_Set => False),
               documentation       => (Is_Set => False),
               deprecated          => (Is_Set => False),
               preselect           => (True, Has_Space),
               sortText            => (True, Label),
               filterText          => (Is_Set => False),
               insertText          => (True, Label),
               insertTextFormat    => (Is_Set => False),
               insertTextMode      => (Is_Set => False),
               textEdit            => (Is_Set => False),
               additionalTextEdits => <>,
               commitCharacters    => (Is_Set => False),
               command             => (Is_Set => False),
               data                => (Is_Set => False));

      Result.items.Append (Item);
   end Propose_Completion;

end LSP.Ada_Completions.End_Names;
