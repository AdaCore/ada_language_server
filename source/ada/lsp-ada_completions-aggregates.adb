------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2025-2026, AdaCore                     --
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
pragma Warnings (Off);
with Ada.Containers.Vectors;

with LSP.Ada_Completions.Filters;
with LSP.Ada_Handlers.Format_Range_Commands;
with LSP.Enumerations;
with LSP.Utils;
with VSS.Characters;
with VSS.Characters.Latin;
with VSS.String_Vectors;
with VSS.Strings.Formatters.Integers;
with VSS.Strings.Formatters.Strings;
with VSS.Strings.Templates;

package body LSP.Ada_Completions.Aggregates is

   type Token_Index is new Natural;

   package Token_Index_Vectors is
      new Ada.Containers.Vectors (Positive, Token_Index);

   type Code_Snippet is tagged limited record
      Plain_Text   : VSS.String_Vectors.Virtual_String_Vector := [""];
      --  Complete plain text of the code snippet as vector of lines
      Coded_Text   : VSS.String_Vectors.Virtual_String_Vector := [""];
      --  The same, but with placeholders encoded (like `${3:Foo}`)
      Placeholders : Natural := 0;
      --  Number of used placeholders `${3:Foo}`
      Line_Count   : Positive := 1;
      --  Total number of lines in Text
      Indent       : VSS.Strings.Character_Count := 0;
      --  Current indent in characters
   end record;

   procedure Append_Token
     (Self : in out Code_Snippet'Class;
      Text : VSS.Strings.Virtual_String);
   --  Append a single Ada token to code snippet

   procedure Append_Token
     (Self : in out Code_Snippet'Class;
      Name : Libadalang.Analysis.Ada_Node'Class);
   --  Append an Ada node text to code snippet

   procedure Append_Placeholder_Token
     (Self : in out Code_Snippet'Class;
      Text : VSS.Strings.Virtual_String);
   --  Append a single Ada token to code snippet as a placeholder

   procedure Append_New_Line (Self : in out Code_Snippet'Class);
   --  Append a single line feed

   procedure Append_Value
     (Self            : in out Code_Snippet'Class;
      Aggregate       : Libadalang.Analysis.Aggregate;
      Expression_Type : Libadalang.Analysis.Base_Type_Decl'Class;
      Component_Name  : Wide_Wide_String := "");
   --  Append Ada value of given Expression_Type, use Aggregate as origin

   procedure Append_Discriminant_Value
     (Self            : in out Code_Snippet'Class;
      Aggregate       : Libadalang.Analysis.Aggregate;
      Alternatives    : Libadalang.Analysis.Alternatives_List'Class;
      Expression_Type : Libadalang.Analysis.Base_Type_Decl'Class;
      Component_Name  : Wide_Wide_String);
   --  Append discriminant value taking it from given Alternatives if possible

   procedure Append_Record_Aggregate
     (Self      : in out Code_Snippet'Class;
      Aggregate : Libadalang.Analysis.Aggregate;
      Shape     : Libadalang.Analysis.Shape);
   --  Append a record aggregate text of given Shape

   function To_Text
     (Self         : Code_Snippet'Class;
      Use_Snippets : Boolean) return VSS.Strings.Virtual_String is
        (if Use_Snippets
         then Self.Coded_Text.Join_Lines (VSS.Strings.LF, False)
         else Self.Plain_Text.Join_Lines (VSS.Strings.LF, False));

   procedure Create_Code_Snippet
     (Aggregate      : Libadalang.Analysis.Aggregate;
      Aggregate_Type : Libadalang.Analysis.Base_Type_Decl;
      Result         : out Code_Snippet'Class);

   procedure Create_Code_Snippet
     (Shape     : Libadalang.Analysis.Shape;
      Aggregate : Libadalang.Analysis.Aggregate;
      Result    : out Code_Snippet'Class);

   procedure Create_Completion
     (Self      : Aggregate_Completion_Provider;
      Label     : LSP.Structures.CompletionItemLabelDetails;
      Snippet   : Code_Snippet'Class;
      Aggregate : Libadalang.Analysis.Aggregate;
      Result    : out LSP.Structures.CompletionItem);

   function Format (Self : Aggregate_Completion_Provider'Class)
      return LSP.Structures.InsertTextFormat_Optional
   is
      (if Self.Use_Snippets
       then (True, LSP.Enumerations.Snippet)
       else (Is_Set => False));

   function Get_Shapes
     (Expr_Type : Libadalang.Analysis.Base_Type_Decl'Class;
      Origin    : Libadalang.Analysis.Ada_Node'Class)
      return Libadalang.Analysis.Shape_Array
   is (if Expr_Type.Kind in Libadalang.Common.Ada_Type_Decl
         and then Expr_Type.P_Is_Record_Type
       then
         Expr_Type.P_Shapes
           (Include_Discriminants => True,
            --  This is mandatory to retrieve unbounded discriminants
            Origin                => Origin)
       else []);

   Placeholder_Template : VSS.Strings.Templates.Virtual_String_Template :=
     "{}${{{}:{}}";
     --  Like `[indent]${3:Foo}` with the first `{` escaped

   -------------------------------
   -- Append_Discriminant_Value --
   -------------------------------

   procedure Append_Discriminant_Value
     (Self            : in out Code_Snippet'Class;
      Aggregate       : Libadalang.Analysis.Aggregate;
      Alternatives    : Libadalang.Analysis.Alternatives_List'Class;
      Expression_Type : Libadalang.Analysis.Base_Type_Decl'Class;
      Component_Name  : Wide_Wide_String) is
   begin
      for Item of Alternatives loop
         --  Search for a range Left .. Right and write Left
         if Item.Kind in Libadalang.Common.Ada_Bin_Op then
            Self.Append_Token (Item.As_Bin_Op.F_Left);
            return;
         end if;

         --  Search for a static subtype and write Item'First
         if Item.Kind in Libadalang.Common.Ada_Name
           and then not Item.As_Name.P_Name_Designated_Type.Is_Null
           and then Item.As_Name.P_Is_Static_Subtype
         then
            Self.Append_Token (Item);
            Self.Append_Token ("'First");
            return;
         end if;

         --  Search for a static value in Alternatives
         if Item.Kind in Libadalang.Common.Ada_Expr
           and then Item.As_Expr.P_Is_Static_Expr
         then
            Self.Append_Token (Item);
            return;
         end if;

      end loop;

      --  Fallback to unknown value
      Self.Append_Value (Aggregate, Expression_Type, Component_Name);
   end Append_Discriminant_Value;

   ---------------------
   -- Append_New_Line --
   ---------------------

   procedure Append_New_Line (Self : in out Code_Snippet'Class) is
   begin
      Self.Plain_Text.Append (VSS.Strings.Empty_Virtual_String);
      Self.Coded_Text.Append (VSS.Strings.Empty_Virtual_String);
   end Append_New_Line;

   ------------------------------
   -- Append_Placeholder_Token --
   ------------------------------

   procedure Append_Placeholder_Token
     (Self : in out Code_Snippet'Class;
      Text : VSS.Strings.Virtual_String)
   is
      use VSS.Strings;

      Prefix : constant VSS.Strings.Virtual_String :=
        (if Self.Plain_Text.Length = 1
          or else not Self.Plain_Text.Last_Element.Is_Empty
         then Self.Plain_Text.Last_Element
         else Self.Indent * ' ');

   begin
      Self.Placeholders := @ + 1;
      Self.Plain_Text.Replace (Self.Plain_Text.Last_Index, Prefix & Text);
      Self.Coded_Text.Replace
        (Self.Coded_Text.Last_Index,
         Placeholder_Template.Format
           (VSS.Strings.Formatters.Strings.Image (Prefix),
            VSS.Strings.Formatters.Integers.Image (Self.Placeholders),
            VSS.Strings.Formatters.Strings.Image (Text)));
   end Append_Placeholder_Token;

   -----------------------------
   -- Append_Record_Aggregate --
   -----------------------------

   procedure Append_Record_Aggregate
     (Self      : in out Code_Snippet'Class;
      Aggregate : Libadalang.Analysis.Aggregate;
      Shape     : Libadalang.Analysis.Shape)
   is
      Count : Natural := 0;

      Discriminants : constant Libadalang.Analysis.Discriminant_Values_Array :=
         Libadalang.Analysis.Discriminants_Values (Shape);

      function The_Same_Name
        (Name  : Libadalang.Analysis.Defining_Name;
         Known : Libadalang.Analysis.Discriminant_Values) return Boolean is
          (Name.P_Name_Matches (Known.Discriminant.P_Gnat_Xref));

      function Is_Discriminant
        (Name : Libadalang.Analysis.Defining_Name) return Boolean
      is (for some K of Discriminants => The_Same_Name (Name, K));

   begin
      Self.Append_Token ("(");

      for Component of Shape.Components loop
         for Name of Component.P_Defining_Names loop
            if Count > 0 then
               Self.Append_Token (", ");
            end if;

            Self.Append_Token (Name.F_Name);
            Self.Append_Token (" => ");

            if Is_Discriminant (Name) then
               for K of Discriminants when The_Same_Name (Name, K) loop
                  Self.Append_Discriminant_Value
                    (Aggregate,
                     Libadalang.Analysis.Values (K),
                     Expression_Type =>
                       Component.P_Formal_Type (Origin => Aggregate),
                     Component_Name  => Name.Text);
               end loop;
            else
               Self.Append_Value
                 (Aggregate,
                  Expression_Type =>
                    Component.P_Formal_Type (Origin => Aggregate),
                  Component_Name  => Name.Text);
            end if;

            Count := Count + 1;
         end loop;
      end loop;

      if Count = 0 then
         Self.Append_Token ("null record");
      end if;

      Self.Append_Token (")");
   end Append_Record_Aggregate;

   ------------------
   -- Append_Token --
   ------------------

   procedure Append_Token
     (Self : in out Code_Snippet'Class;
      Text : VSS.Strings.Virtual_String)
   is
      use type VSS.Strings.Virtual_String;

      Line : constant VSS.Strings.Virtual_String :=
        (if Self.Plain_Text.Length = 1
          or else not Self.Plain_Text.Last_Element.Is_Empty
         then Self.Plain_Text.Last_Element
         else Self.Indent * ' ') & Text;
   begin
      Self.Plain_Text.Replace (Self.Plain_Text.Last_Index, Line);
      Self.Coded_Text.Replace (Self.Coded_Text.Last_Index, Line);
   end Append_Token;

   ------------------
   -- Append_Token --
   ------------------

   procedure Append_Token
     (Self : in out Code_Snippet'Class;
      Name : Libadalang.Analysis.Ada_Node'Class) is
   begin
      Self.Append_Token (VSS.Strings.To_Virtual_String (Name.Text));
   end Append_Token;

   ------------------
   -- Append_Value --
   ------------------

   procedure Append_Value
     (Self            : in out Code_Snippet'Class;
      Aggregate       : Libadalang.Analysis.Aggregate;
      Expression_Type : Libadalang.Analysis.Base_Type_Decl'Class;
      Component_Name  : Wide_Wide_String := "") is
   begin
      if Expression_Type.P_Is_Record_Type (Origin => Aggregate) then
         Self.Append_Record_Aggregate
           (Aggregate, Get_Shapes (Expression_Type, Aggregate) (1));
      elsif Expression_Type.P_Is_Access_Type (Origin => Aggregate) then
         Self.Append_Placeholder_Token ("null");
      elsif Expression_Type.P_Is_Int_Type (Origin => Aggregate) then
         Self.Append_Placeholder_Token ("0");
      elsif Expression_Type.P_Is_Real_Type (Origin => Aggregate) then
         Self.Append_Placeholder_Token ("0.0");
      else
         Self.Append_Placeholder_Token ("<>");
      end if;
   end Append_Value;

   -------------------------
   -- Create_Code_Snippet --
   -------------------------

   procedure Create_Code_Snippet
     (Aggregate      : Libadalang.Analysis.Aggregate;
      Aggregate_Type : Libadalang.Analysis.Base_Type_Decl;
      Result         : out Code_Snippet'Class) is
   begin
      Result.Indent :=
        VSS.Strings.Character_Count (Aggregate.Sloc_Range.Start_Column);

      Result.Append_Value (Aggregate, Aggregate_Type);
   end Create_Code_Snippet;

   -------------------------
   -- Create_Code_Snippet --
   -------------------------

   procedure Create_Code_Snippet
     (Shape     : Libadalang.Analysis.Shape;
      Aggregate : Libadalang.Analysis.Aggregate;
      Result    : out Code_Snippet'Class) is
   begin
      Result.Indent :=
        VSS.Strings.Character_Count (Aggregate.Sloc_Range.Start_Column);

      Result.Append_Record_Aggregate (Aggregate, Shape);
   end Create_Code_Snippet;

   -----------------------
   -- Create_Completion --
   -----------------------

   procedure Create_Completion
     (Self      : Aggregate_Completion_Provider;
      Label     : LSP.Structures.CompletionItemLabelDetails;
      Snippet   : Code_Snippet'Class;
      Aggregate : Libadalang.Analysis.Aggregate;
      Result    : out LSP.Structures.CompletionItem)
   is
      function Shift_End_Bound
        (Value : LSP.Structures.Location) return LSP.Structures.Location is
          (uri     => Value.uri,
           alsKind => Value.alsKind,
           hidden  => Value.hidden,
           a_range =>
             (start => Value.a_range.start,
              an_end => (line => Value.a_range.an_end.line,
                         character => Value.a_range.an_end.character + 1)));

      Location : LSP.Structures.Location :=
        Shift_End_Bound (Self.Handler.To_LSP_Location (Aggregate));

      Edit     : LSP.Structures.TextEdit :=
        (a_range => Location.a_range,
         newText => <>);
   begin
      Edit.newText := Snippet.To_Text (Self.Use_Snippets);

      Location.a_range.an_end :=
        (line      => Location.a_range.start.line + 1,
         character => 0);
      --  Fix command's Location end bound to next line.

      Result :=
        (label            => "(Aggregate) ",
         labelDetails     => (True, Label),
         kind             => (True, LSP.Enumerations.Struct),
         insertText       => Edit.newText,
         insertTextFormat => Self.Format,
         textEdit         => (True, (True, Edit)),
         detail           => "<detail TBD>",
         documentation    =>
           (True,
             (True,  --  Is_Virtual_String,
             "A human-readable string that represents a doc-comment")),
         command          =>
           (Is_Set => True,
            Value =>
              LSP.Ada_Handlers.Format_Range_Commands.To_LSP_Command
                (Document => (uri => Location.uri),
                 Span => Location.a_range)),
         others           => <>);
          --  +label : LSP.Structures.Virtual_String;
          --  +labelDetails        => CompletionItemLabelDetails_Optional,
          --  +kind                => CompletionItemKind_Optional,
          --   tags                => LSP.Structures.CompletionItemTag_Set,
          --  +detail              => Virtual_String_Optional,
          --  +documentation       => Virtual_String_Or_MarkupContent_Optional,
          --   deprecated          => Boolean_Optional,
          --   preselect           => Boolean_Optional,
          --   sortText            => Virtual_String_Optional,
          --   filterText          => Virtual_String_Optional,
          --  +insertText          => Virtual_String_Optional,
          --  +insertTextFormat    => InsertTextFormat_Optional,
          --   insertTextMode      => InsertTextMode_Optional,
          --  +textEdit            => TextEdit_Or_InsertReplaceEdit_Optional,
          --   textEditText        => Virtual_String_Optional,
          --   additionalTextEdits => LSP.Structures.TextEdit_Vector,
          --   commitCharacters    => LSP.Structures.Virtual_String_Vector,
          --  +command             => Command_Optional,
          --   data                => LSPAny_Optional);
   end Create_Completion;

   ------------------------
   -- Propose_Completion --
   ------------------------

   overriding
   procedure Propose_Completion
     (Self   : Aggregate_Completion_Provider;
      Sloc   : Langkit_Support.Slocs.Source_Location;
      Token  : Libadalang.Common.Token_Reference;
      Node   : Libadalang.Analysis.Ada_Node;
      Filter : in out LSP.Ada_Completions.Filters.Filter;
      Result : out Ada_Completions.Completion_Result)
   is

      Aggregate : constant Libadalang.Analysis.Aggregate :=
        (if Node.Is_Null
           or else Node.Kind not in Libadalang.Common.Ada_Aggregate_Range
         then Libadalang.Analysis.No_Aggregate
         else Node.As_Aggregate);

      Level : constant Natural :=
         (if not Aggregate.Is_Null and then Aggregate.P_Is_Subaggregate
           then Aggregate.P_Subaggregate_Dimension else 0);

      Aggregate_Type : constant Libadalang.Analysis.Base_Type_Decl :=
        (if Aggregate.Is_Null then Libadalang.Analysis.No_Base_Type_Decl
         elsif Aggregate.P_Is_Subaggregate
         then Aggregate.P_Subaggregate_Array_Type
         else Aggregate.P_Expression_Type);

      function To_Label return LSP.Structures.CompletionItemLabelDetails is
         (VSS.Strings.To_Virtual_String (Aggregate_Type.F_Name.Text),
          LSP.Utils.Node_Location_Image (Aggregate_Type));

      function Label_For_Shape
        (Shape : Libadalang.Analysis.Shape)
          return LSP.Structures.CompletionItemLabelDetails;

      -------------------------
      -- Get_Label_For_Shape --
      -------------------------

      function Label_For_Shape
        (Shape : Libadalang.Analysis.Shape)
          return LSP.Structures.CompletionItemLabelDetails
      is
         Result : VSS.Strings.Virtual_String;
      begin
         for Item of Libadalang.Analysis.Discriminants_Values (Shape) loop
            if not Result.Is_Empty then
               Result.Append (", ");
            end if;

            Result.Append
              (VSS.Strings.To_Virtual_String
                 (Libadalang.Analysis.Discriminant (Item).Text));
            Result.Append (" => ");

            Result.Append
              (VSS.Strings.To_Virtual_String
                 (Libadalang.Analysis.Values (Item).Text));
         end loop;

         return
           (Result,
            LSP.Utils.Node_Location_Image (Aggregate_Type));
      end Label_For_Shape;

      Item : LSP.Structures.CompletionItem;
   begin
      Result := (Ada_Completions.Completion_List, others => <>);

      --  Complete only an empty aggregate just after open parenthesis `(`.
      --  Private, abstract, interface types don't have aggregates
      if not Filter.Is_Open_Parenthesis
        or else Aggregate_Type.Is_Null
        or else Aggregate_Type.P_Is_Private
        or else Aggregate_Type.P_Is_Abstract_Type
        or else Aggregate_Type.P_Is_Interface_Type
      then
         return;
      else
         --  Don't complete if the aggregate has any association
         for Item of Aggregate.F_Assocs loop
            return;
         end loop;
      end if;

      declare
         Shape_List : constant Libadalang.Analysis.Shape_Array :=
           Get_Shapes (Aggregate_Type, Node);
      begin
         if Shape_List'Length <= 1 then
            declare
               Code : Code_Snippet;
            begin
               Create_Code_Snippet (Aggregate, Aggregate_Type, Code);
               Self.Create_Completion (To_Label, Code, Aggregate, Item);
               Result.Completion_List.Append (Item);
            end;
         else
            for Shape of Shape_List loop
               declare
                  Code : Code_Snippet;
               begin
                  Create_Code_Snippet (Shape, Aggregate, Code);

                  Self.Create_Completion
                    (Label_For_Shape (Shape), Code, Aggregate, Item);

                  Result.Completion_List.Append (Item);
               end;
            end loop;
         end if;
      end;
   end Propose_Completion;

end LSP.Ada_Completions.Aggregates;
