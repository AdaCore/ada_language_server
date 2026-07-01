------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2018-2026, AdaCore                     --
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

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Hashed_Sets;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;

with GNATCOLL.VFS;

with Gnatformat;
with Gnatformat.Formatting;

with Langkit_Support.Diagnostics;

with VSS.Characters.Latin;
with VSS.Regular_Expressions;
with VSS.String_Vectors;
with VSS.Strings.Character_Iterators;
with VSS.Strings.Conversions;
with VSS.Strings.Cursors.Iterators.Characters;
with VSS.Strings.Cursors.Markers;
with VSS.Strings.Formatters.Integers;
with VSS.Strings.Formatters.Strings;
with VSS.Strings.Hash;
with VSS.Strings.Templates;
with VSS.Strings;              use VSS.Strings;
with VSS.Transformers.Casing;

with LSP.Ada_Configurations;
with LSP.Ada_Contexts;
with LSP.Ada_Documentation;
with LSP.Ada_Documents;
with LSP.Ada_Handlers.Locations;
with LSP.Ada_Handlers.Refactor.Auto_Import;
with LSP.Enumerations;
with LSP.Formatters.Texts;
with LSP.Structures.LSPAny_Vectors;
with LSP.Utils;

package body LSP.Ada_Completions is

   package Encoding_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => VSS.Strings.Virtual_String,
      Element_Type    => VSS.Strings.Virtual_String,
      Hash            => VSS.Strings.Hash,
      Equivalent_Keys => VSS.Strings."=");

   function To_Completion_Kind (K : LSP.Enumerations.SymbolKind)
     return LSP.Enumerations.CompletionItemKind
   is
     (case K is
        when LSP.Enumerations.A_Function => LSP.Enumerations.A_Function,
        when LSP.Enumerations.Field      => LSP.Enumerations.Field,
        when LSP.Enumerations.Variable   => LSP.Enumerations.Variable,
        when LSP.Enumerations.A_Package  => LSP.Enumerations.Module,
        when LSP.Enumerations.Module     => LSP.Enumerations.Module,
        when LSP.Enumerations.Class      => LSP.Enumerations.Class,
        when LSP.Enumerations.Struct     => LSP.Enumerations.Class,
        when LSP.Enumerations.Number     => LSP.Enumerations.Value,
        when LSP.Enumerations.Enum       => LSP.Enumerations.Enum,
        when LSP.Enumerations.String     => LSP.Enumerations.Value,
        when LSP.Enumerations.A_Constant => LSP.Enumerations.Value,
        when others                      => LSP.Enumerations.Reference);
   --  Convert a SymbolKind to a CompletionItemKind.
   --  TODO: It might be better to have a unified kind, and then convert to
   --  specific kind types, but for the moment this is good enough.

   function Create_Auto_Import_Command
     (Context : LSP.Ada_Contexts.Context;
      Node    : Libadalang.Analysis.Ada_Node;
      Name    : Libadalang.Analysis.Defining_Name;
      Where   : LSP.Structures.TextDocumentPositionParams)
      return LSP.Structures.Command_Optional;
   --  Create the needed command to add the missing with-clause/qualifier
   --  when accepting an invisible completion item.

   function Compute_Completion_Item
     (Handler                   : in out LSP.Ada_Handlers.Message_Handler;
      Context                   : LSP.Ada_Contexts.Context;
      Name                      : Libadalang.Analysis.Defining_Name;
      Label                     : VSS.Strings.Virtual_String;
      Command                   : LSP.Structures.Command_Optional;
      Use_Snippets              : Boolean;
      Compute_Doc_And_Details   : Boolean;
      Has_Label_Details_Support : Boolean;
      Named_Notation_Threshold  : Natural;
      Is_Dot_Call               : Boolean;
      Is_Visible                : Boolean;
      Pos                       : Integer;
      Weight                    : Ada_Completions.Completion_Item_Weight_Type)
       return LSP.Structures.CompletionItem;
   --  Compute a completion item.
   --  Node is the node from which the completion starts (e.g: 'A' in 'A.').
   --  BD is the basic declaration and Label is the defining name text
   --  that should be used to compute the completion item.
   --  When Use_Snippets is True, subprogram completion items are computed
   --  as snippets that list all the subprogram's formal parameters.
   --  Has_Label_Details_Support is set to True when the client supports
   --  label details. If supported, it is used to display the defining name's
   --  fully qualified name.
   --  Named_Notation_Threshold defines the number of parameters at which point
   --  named notation is used for subprogram completion snippets.
   --  Is_Dot_Call is used to know if we should omit the first parameter
   --  when computing subprogram snippets.
   --  Weight is used for sorting: items with an higher weight will be placed
   --  at the top.
   --  Completions_Count is the total number of completion items.

   function Compute_Completion_Item
     (Handler                   : in out LSP.Ada_Handlers.Message_Handler;
      Context                   : LSP.Ada_Contexts.Context;
      Name                      : Libadalang.Analysis.Defining_Name;
      Label                     : VSS.Strings.Virtual_String;
      Command                   : LSP.Structures.Command_Optional;
      Use_Snippets              : Boolean;
      Compute_Doc_And_Details   : Boolean;
      Has_Label_Details_Support : Boolean;
      Named_Notation_Threshold  : Natural;
      Is_Dot_Call               : Boolean;
      Is_Visible                : Boolean;
      Pos                       : Integer;
      Weight                    : Ada_Completions.Completion_Item_Weight_Type)
        return LSP.Structures.CompletionItem
   is
      package Weight_Formatters renames VSS.Strings.Formatters.Integers;

      Item           : LSP.Structures.CompletionItem;
      Subp_Spec_Node : Libadalang.Analysis.Base_Subp_Spec;

      Last_Weight : constant Ada_Completions.Completion_Item_Weight_Type :=
        Ada_Completions.Completion_Item_Weight_Type'Last;

      Unit_Full_Qual_Name : constant VSS.Strings.Virtual_String :=
        VSS.Strings.To_Virtual_String
          (Name.P_Enclosing_Compilation_Unit.P_Decl.P_Fully_Qualified_Name);
      --  The fully qualified name of the completion item symbol's unit.

      function Get_Sort_Text
        (Base_Label : VSS.Strings.Virtual_String)
         return VSS.Strings.Virtual_String;
      --  Return a suitable sortText according to the completion item's
      --  visibility and position in the completion list.

      -------------------
      -- Get_Sort_Text --
      -------------------

      function Get_Sort_Text
        (Base_Label : VSS.Strings.Virtual_String)
         return VSS.Strings.Virtual_String is
      begin
         return Sort_Text : VSS.Strings.Virtual_String do

            Sort_Text :=
              VSS.Strings.Templates.Format
                ("{:02}&{:05}{}",
                 Weight_Formatters.Image (Last_Weight - Weight),
                 Weight_Formatters.Image (Pos),
                 VSS.Strings.Formatters.Strings.Image (Base_Label));

            if not Is_Visible then
               Sort_Text.Prepend ('~');
            end if;
         end return;
      end Get_Sort_Text;

   begin
      Item.label := Label;
      Item.command := Command;

      Item.kind :=
        (True,
         To_Completion_Kind (LSP.Utils.Get_Decl_Kind (Name.P_Basic_Decl)));

      --  When the client supports it, show the fully qualified name
      --  via completion item label details.
      if Has_Label_Details_Support then
         declare

         begin
            Item.labelDetails :=
              (Is_Set => True,
               Value  => (description => Unit_Full_Qual_Name, others => <>));
         end;
      end if;

      if not Is_Visible then
         Item.insertText := Label;
         Item.label.Append (" (invisible)");
         Item.filterText := Label;
      end if;

      Item.sortText := Get_Sort_Text (Label);

      Set_Completion_Item_Documentation
        (Handler                 => Handler,
         Context                 => Context,
         Name                    => Name,
         Item                    => Item,
         Compute_Doc_And_Details => Compute_Doc_And_Details);

      --  Return immediately if we should not use snippets (e.g: completion for
      --  invisible symbols).
      if not Use_Snippets then
         return Item;
      end if;

      --  Check if we are dealing with a subprogram and return a completion
      --  snippet that lists all the formal parameters if it's the case.

      Subp_Spec_Node := Name.P_Basic_Decl.P_Subp_Spec_Or_Null;

      if Subp_Spec_Node.Is_Null then
         return Item;
      end if;

      declare
         Insert_Text : VSS.Strings.Virtual_String := Label;
         All_Params  : constant Libadalang.Analysis.Param_Spec_Array :=
           Subp_Spec_Node.P_Params;

         Params : constant Libadalang.Analysis.Param_Spec_Array :=
           (if Is_Dot_Call
            then All_Params (All_Params'First + 1 .. All_Params'Last)
            else All_Params);
         --  Remove the first formal parameter from the list when the dotted
         --  notation is used.

         Idx                : Positive := 1;
         Nb_Params          : Natural := 0;
         Use_Named_Notation : Boolean := False;
      begin

         --  Create a completion snippet if the subprogram expects some
         --  parameters.

         if Params'Length /= 0 then
            Item.insertTextFormat :=
              (Is_Set => True, Value => LSP.Enumerations.Snippet);

            Insert_Text.Append (" (");

            --  Compute number of params to know if named notation should be
            --  used.

            for Param of Params loop
               Nb_Params := Nb_Params + Param.F_Ids.Children_Count;
            end loop;

            Use_Named_Notation :=
              Named_Notation_Threshold > 0
              and then Nb_Params >= Named_Notation_Threshold;

            for Param of Params loop
               for Id of Param.F_Ids loop
                  declare
                     Mode      : constant Langkit_Support.Text.Text_Type :=
                       Param.F_Mode.Text;
                     Mode_Text : constant Langkit_Support.Text.Text_Type :=
                       (if Mode /= "" then Mode & " " else "");

                     Named_Template      :
                       constant VSS
                                  .Strings
                                  .Templates
                                  .Virtual_String_Template :=
                         "{} => ${{{}:{}{}}, ";
                     Positional_Template :
                       constant VSS
                                  .Strings
                                  .Templates
                                  .Virtual_String_Template :=
                         "${{{}:{} : {}{}}, ";
                     Text                : VSS.Strings.Virtual_String;

                  begin
                     if Use_Named_Notation then
                        Text :=
                          Named_Template.Format
                            (LSP.Formatters.Texts.Image (Id.Text),
                             VSS.Strings.Formatters.Integers.Image (Idx),
                             LSP.Formatters.Texts.Image (Mode_Text),
                             LSP.Formatters.Texts.Image
                               (Param.F_Type_Expr.Text));

                     else
                        Text :=
                          Positional_Template.Format
                            (VSS.Strings.Formatters.Integers.Image (Idx),
                             LSP.Formatters.Texts.Image (Id.Text),
                             LSP.Formatters.Texts.Image (Mode_Text),
                             LSP.Formatters.Texts.Image
                               (Param.F_Type_Expr.Text));
                     end if;

                     Insert_Text.Append (Text);
                     Idx := Idx + 1;
                  end;
               end loop;
            end loop;

            --  Remove the ", " substring that has been appended in the last
            --  loop iteration.

            declare
               First   :
                 constant VSS.Strings.Character_Iterators.Character_Iterator :=
                   Insert_Text.At_First_Character;
               Last    : VSS.Strings.Character_Iterators.Character_Iterator :=
                 Insert_Text.At_Last_Character;
               Success : Boolean
               with Unreferenced;

            begin
               Success := Last.Backward;
               Success := Last.Backward;

               Insert_Text := Insert_Text.Slice (First, Last);
               --  ??? May be replaced by "Head" like procedure when it will be
               --  implemented.
            end;

            --  Insert '$0' (i.e: the final tab stop) at the end.
            Insert_Text.Append (")$0");

            Item.insertText := Insert_Text;
         end if;
      end;

      return Item;
   end Compute_Completion_Item;

   function Create_Auto_Import_Command
     (Context : LSP.Ada_Contexts.Context;
      Node    : Libadalang.Analysis.Ada_Node;
      Name    : Libadalang.Analysis.Defining_Name;
      Where   : LSP.Structures.TextDocumentPositionParams)
      return LSP.Structures.Command_Optional
   is

      procedure Get_Missing_Unit_And_Qualifier
        (Missing_Unit_Name : out VSS.Strings.Virtual_String;
         Missing_Qualifier : out VSS.Strings.Virtual_String);
      --  Get the missing unit name and qualifier (if needed) for invisible
      --  completion items.

      ------------------------------------
      -- Get_Missing_unit_and_qualifier --
      ------------------------------------

      procedure Get_Missing_Unit_And_Qualifier
        (Missing_Unit_Name : out VSS.Strings.Virtual_String;
         Missing_Qualifier : out VSS.Strings.Virtual_String)
      is
         use Libadalang.Analysis;

         Unit_Full_Qual_Name : constant VSS.Strings.Virtual_String :=
           VSS.Strings.To_Virtual_String
             (Name
                .P_Enclosing_Compilation_Unit
                .P_Decl
                .P_Fully_Qualified_Name);
         --  The fully qualified name of the completion item symbol's unit.

         Dotted_Node : constant Ada_Node :=
           (if Node.Kind in Libadalang.Common.Ada_Dotted_Name_Range
            then Node
            else Node.Parent);

         Is_Dotted_Name : constant Boolean :=
           not Dotted_Node.Is_Null
           and then
             Dotted_Node.Kind in Libadalang.Common.Ada_Dotted_Name_Range;
         --  Check if we are completing a dotted name. We want to prepend the
         --  right qualifier only if it's not the case.

         Dotted_Node_Prefix : constant VSS.Strings.Virtual_String :=
           (if Is_Dotted_Name
            then
              VSS.Strings.To_Virtual_String
                (Dotted_Node.As_Dotted_Name.F_Prefix.Text)
            else VSS.Strings.Empty_Virtual_String);
         --  The prefix of the dotted name we are completing, or an empty
         --  string if we are not completing a dotted name.

         Missing_Unit_Root_Decl : constant Libadalang.Analysis.Basic_Decl :=
           Name.P_Enclosing_Compilation_Unit.P_Decl;
         --  The missing unit root declaration for this invisible symbol (e.g:
         --  the "Ada.Text_IO" package declaration for the
         --  "Ada.Text_IO.Put_Line" subprogram).

      begin
         Missing_Unit_Name := Unit_Full_Qual_Name;

         --  We are completing a dotted name but its prefix does not match
         --  with the completion item's defining name's unit: this means we
         --  are dealing with either renames (e.g: 'GNAT.Strings.Strings_Access'
         --  is a forward declaration of 'System.Strings.String_Access') or
         --  partially qualified dotted completion (e.g: the user typed
         --  'Child.' to reference a 'Parent.Child.' package).
         --  If the prefix corresponds to a known LAL unit (will be the case for
         --  renames), import the unit corresponding to the prefix instead.
         --  We don't want to this for partially qualified names: we still want
         --  to import the fully qualified unit (so import 'Parent.Child' even
         --  if the user just type 'Child.')
         if Is_Dotted_Name
           and then not Missing_Unit_Name.Starts_With (Dotted_Node_Prefix)
           and then
             not Get_From_Provider
                   (Context => Context.LAL_Context,
                    Name    => VSS.Strings.Conversions.To_Wide_Wide_String
                        (Dotted_Node_Prefix),
                    Kind    => Libadalang.Common.Unit_Specification)
                   .Root
                   .Is_Null
         then
            Missing_Unit_Name := Dotted_Node_Prefix;
         end if;

         --  We should not add any qualifier if the user accepted the
         --  completion item corresponding to the missing unit itself (e.g: if
         --  the user selects "Ada.Text_IO" in the completion window, we do not
         --  need to add any qualifier) or if he's completing a dotted name.
         Missing_Qualifier :=
           (if Is_Dotted_Name
              or else Name.P_Basic_Decl = Missing_Unit_Root_Decl
            then VSS.Strings.Empty_Virtual_String
            else Missing_Unit_Name);
      end Get_Missing_Unit_And_Qualifier;

      use LSP.Ada_Handlers.Refactor;

      Auto_Import_Command : Auto_Import.Command;
         --  The auto-import command.

      Missing_Unit_Name : VSS.Strings.Virtual_String;
      Missing_Qualifier : VSS.Strings.Virtual_String;
   begin
      Get_Missing_Unit_And_Qualifier (Missing_Unit_Name, Missing_Qualifier);

      Auto_Import_Command.Initialize
        (Context     => Context,
         Where       => Where,
         With_Clause => Missing_Unit_Name,
         Prefix      => Missing_Qualifier);

      return
        (Is_Set => True,
            Value  =>
              (title     => <>,
               command   =>
                 VSS.Strings.Conversions.To_Virtual_String
                   (Auto_Import.Command'External_Tag),
               arguments => Auto_Import_Command.Write_Command_Args));
   end Create_Auto_Import_Command;

   ------------------
   -- Is_End_Token --
   ------------------

   function Is_End_Token
     (Token : Libadalang.Common.Token_Reference) return Boolean
   is
      use Libadalang.Common;
   begin
      if Token = No_Token then
         return False;
      end if;

      declare
         End_Token : constant Libadalang.Common.Token_Data_Type :=
           Libadalang.Common.Data (Token);

         Token_Kind : constant Libadalang.Common.Token_Kind :=
           Libadalang.Common.Kind (End_Token);
      begin
         return Token_Kind = Libadalang.Common.Ada_End;
      end;
   end Is_End_Token;

   ------------------------
   -- Is_Full_Sloc_Equal --
   ------------------------

   function Is_Full_Sloc_Equal
     (Left, Right : Libadalang.Analysis.Defining_Name) return Boolean
   is
      use type Libadalang.Analysis.Analysis_Unit;
      use type Langkit_Support.Slocs.Source_Location_Range;
   begin
      return Left.Sloc_Range = Right.Sloc_Range
        and then Left.Unit = Right.Unit;
   end Is_Full_Sloc_Equal;

   ---------------------------------------
   -- Set_Completion_Item_Documentation --
   ---------------------------------------

   procedure Set_Completion_Item_Documentation
     (Handler                 : in out LSP.Ada_Handlers.Message_Handler;
      Context                 : LSP.Ada_Contexts.Context;
      Name                    : Libadalang.Analysis.Defining_Name;
      Item                    : in out LSP.Structures.CompletionItem;
      Compute_Doc_And_Details : Boolean) is
   begin
      --  Compute the 'documentation' and 'detail' fields immediately if
      --  requested (i.e: when the client does not support lazy computation
      --  for these fields or if we are dealing with predefined types).
      if Compute_Doc_And_Details or else LSP.Utils.Is_Synthetic (Name) then
         declare
            Qual_Text            : VSS.Strings.Virtual_String;
            Decl_Text            : VSS.Strings.Virtual_String;
            Loc_Text             : VSS.Strings.Virtual_String;
            Doc_Text             : VSS.Strings.Virtual_String;
            Aspects_Text         : VSS.Strings.Virtual_String;
            Fully_Qualified_Name : constant VSS.Strings.Virtual_String :=
              VSS.Strings.To_Virtual_String
                (Name.P_Fully_Qualified_Name);
         begin
            LSP.Ada_Documentation.Get_Tooltip_Text
              (Name               => Name,
               Origin             => Libadalang.Analysis.No_Ada_Node,
               Style              => Context.Get_Documentation_Style,
               Declaration_Text   => Decl_Text,
               Qualifier_Text     => Qual_Text,
               Location_Text      => Loc_Text,
               Documentation_Text => Doc_Text,
               Aspects_Text       => Aspects_Text);

            Item.detail :=
              Fully_Qualified_Name
              & VSS.Characters.Latin.Line_Feed
              & VSS.Characters.Latin.Line_Feed
              & Decl_Text;

            if not Doc_Text.Is_Empty then
               Loc_Text.Append (2 * VSS.Characters.Latin.Line_Feed);
               Loc_Text.Append (Doc_Text);
            end if;

            Item.documentation :=
              (Is_Set => True,
               Value  =>
                 LSP.Structures.Virtual_String_Or_MarkupContent'
                   (Is_Virtual_String => True, Virtual_String => Loc_Text));
         end;

      else
         --  Set node's location to the 'data' field of the completion item, so
         --  that we can retrieve it in the completionItem/resolve handler.
         LSP.Structures.LSPAny_Vectors.To_Any
           (LSP.Ada_Handlers.Locations.To_LSP_Location (Handler, Name),
            Item.data);
      end if;
   end Set_Completion_Item_Documentation;

   -----------------------
   -- Skip_Dotted_Names --
   -----------------------

   function Skip_Dotted_Names
     (Node : Libadalang.Analysis.Ada_Node)
      return Libadalang.Analysis.Ada_Node
   is
      use Libadalang.Common;
      Parent : Libadalang.Analysis.Ada_Node := Node;
   begin
      while not Parent.Is_Null
        and then Parent.Kind = Libadalang.Common.Ada_Dotted_Name
      loop
         Parent := Parent.Parent;
      end loop;

      return Parent;
   end Skip_Dotted_Names;

   -----------------------
   -- Write_Completions --
   -----------------------

   procedure Write_Completions
     (Handler                   : in out LSP.Ada_Handlers.Message_Handler;
      Context                   : LSP.Ada_Contexts.Context;
      Document                  : LSP.Ada_Documents.Document;
      Token                     : Libadalang.Common.Token_Reference;
      Node                      : Libadalang.Analysis.Ada_Node;
      Names                     : Completion_Maps.Map;
      Named_Notation_Threshold  : Natural;
      Compute_Doc_And_Details   : Boolean;
      Has_Label_Details_Support : Boolean;
      Result                    : in out LSP.Structures.CompletionItem_Vector)
   is
      use all type Libadalang.Common.Ada_Node_Kind_Type;

      Use_Auto_Import_Command : constant Boolean :=
        Handler.Get_Configuration.Insert_With_Clauses
        and then not Node.Is_Null
        and then
          not (for some Parent of Node.Parents =>
                 Parent.Kind = Ada_With_Clause);

      function Start_Sloc
        (Token : Libadalang.Common.Token_Reference)
         return Langkit_Support.Slocs.Source_Location
      is (Langkit_Support.Slocs.Start_Sloc
            (Libadalang.Common.Sloc_Range (Libadalang.Common.Data (Token))));

      function Auto_Import_Command
        (Name : Libadalang.Analysis.Defining_Name; Visible : Boolean)
         return LSP.Structures.Command_Optional
      is (if Visible or not Use_Auto_Import_Command
          then (Is_Set => False)
          else
            Create_Auto_Import_Command
              (Context,
               Node,
               Name,
               Where => Document.To_LSP_Position (Start_Sloc (Token))));
      --  If the corresponding setting is enabled, and if we are not completing
      --  within a with-clause, create a command to insert the missing
      --  with-clause/qualifier.

      package String_Sets is new
        Ada.Containers.Hashed_Sets
          (VSS.Strings.Virtual_String,
           VSS.Strings.Hash,
           VSS.Strings."=",
           VSS.Strings."=");

      Seen : String_Sets.Set;
      --  Set of found visible names in canonical form
   begin

      --  Write Result in two pases. Firstly append all visible names and
      --  populate Seen set. Then append invisible names not in Seen.

      for Visible in reverse Boolean loop
         --  Phase: True then False
         for Cursor in Names.Iterate loop
            declare
               Append    : Boolean := False;
               Info      : constant Name_Information := Names (Cursor);
               Name      : constant Libadalang.Analysis.Defining_Name :=
                 Completion_Maps.Key (Cursor);
               Selector  : constant Libadalang.Analysis.Name :=
                 Name.P_Relative_Name;
               Label     : VSS.Strings.Virtual_String;
               Canonical : VSS.Strings.Virtual_String;
            begin
               if Visible and Info.Is_Visible then
                  Label := VSS.Strings.To_Virtual_String (Selector.Text);
                  Canonical := LSP.Utils.Canonicalize (Label);
                  Seen.Include (Canonical);
                  Append := True;
               elsif not Visible and not Info.Is_Visible then
                  --  Append invisible name on if no such visible name found
                  Label := VSS.Strings.To_Virtual_String (Selector.Text);
                  Canonical := LSP.Utils.Canonicalize (Label);
                  Append := not Seen.Contains (Canonical);
               end if;

               if Append then
                  Result.Append
                    (Compute_Completion_Item
                       (Handler                   => Handler,
                        Context                   => Context,
                        Name                      => Name,
                        Label                     => Label,
                        Command                   =>
                          Auto_Import_Command (Name, Info.Is_Visible),
                        Use_Snippets              => Info.Use_Snippets,
                        Has_Label_Details_Support => Has_Label_Details_Support,
                        Compute_Doc_And_Details   => Compute_Doc_And_Details,
                        Named_Notation_Threshold  => Named_Notation_Threshold,
                        Is_Dot_Call               => Info.Is_Dot_Call,
                        Is_Visible                => Info.Is_Visible,
                        Pos                       => Info.Pos,
                        Weight                    => Info.Weight));
               end if;
            end;
         end loop;
      end loop;
   end Write_Completions;

   --------------------------
   -- Pretty_Print_Snippet --
   --------------------------

   procedure Pretty_Print_Snippet
     (Context  : LSP.Ada_Contexts.Context;
      Document : LSP.Ada_Documents.Document;
      Prefix   : VSS.Strings.Virtual_String;
      Offset   : Natural;
      Span     : LSP.Structures.A_Range;
      Rule     : Libadalang.Common.Grammar_Rule;
      Result   : in out LSP.Structures.CompletionItem)
   is
      use LSP.Ada_Contexts;

      Encoding : Encoding_Maps.Map;
      --  Map of Snippet fake name to snippet:
      --  {"foobar_1" : "$1", "foobar_2" : "${2: Integer}"}
      --  $0 will not be replaced

      function Encode_String (S : Virtual_String) return Virtual_String;
      --  Create pseudo code for the snippet

      function Decode_String (S : Virtual_String) return Virtual_String;
      --  Recreate the snippet via the pseudo code

      function Snippet_Placeholder (S : Virtual_String) return Virtual_String
      is ("foobar_" & S);
      --  Generate a fake entity for a snippet (S is the index of the snippet)
      --  The length of the placeholder will affect where the strings is cut
      --  to avoid exceeding line length => 8/9 characters seems to be
      --  reasonable (most of the time a placeholder is replacing the
      --  parameter type's name)

      function Find_Next_Placeholder
        (S     : Virtual_String;
         Start : VSS.Strings.Cursors.Iterators.Characters.
           Character_Iterator'Class)
         return VSS.Regular_Expressions.Regular_Expression_Match;
      --  Find the next placeholder starting from Start in S

      function Post_Pretty_Print (S : Virtual_String) return Virtual_String;
      --  Indent the block using the initial location and add back $0 (it has
      --  been removed to not generate invalid pseudo code)

      -------------------
      -- Encode_String --
      -------------------

      function Encode_String (S : Virtual_String) return Virtual_String
      is
         Res           : Virtual_String;
         Start_Encoded : VSS.Strings.Cursors.Markers.Character_Marker;
         Search_Index  : Boolean := False;
         Encoded_Index : Virtual_String;
         In_Snippet    : Boolean := False;

         procedure Add_Placeholder
           (Cursor : VSS.Strings.Cursors.Abstract_Cursor'Class);

         ---------------------
         -- Add_Placeholder --
         ---------------------

         procedure Add_Placeholder
           (Cursor : VSS.Strings.Cursors.Abstract_Cursor'Class)
         is
            Placeholder : constant Virtual_String :=
              Snippet_Placeholder (Encoded_Index);
         begin
            if not Encoded_Index.Is_Empty then
               Encoding.Insert
                 (Placeholder,
                  S.Slice (S.At_Character (Start_Encoded), Cursor));
               Append (Res, Placeholder);
               Encoded_Index.Clear;
            end if;
         end Add_Placeholder;

         I     : VSS.Strings.Cursors.Iterators.Characters.
           Character_Iterator := S.At_First_Character;
         Dummy : Boolean;
      begin
         while I.Has_Element loop
            case I.Element is
               when '$' =>
                  Search_Index := True;
                  Start_Encoded := I.Marker;
                  Encoded_Index.Clear;
               when '{' =>
                  In_Snippet := True;
               when '}' =>
                  Add_Placeholder (I);
                  In_Snippet := False;
                  Search_Index := False;
               when '0' .. '9' =>
                  if Search_Index then
                     Append (Encoded_Index, I.Element);
                  elsif not In_Snippet then
                     Append (Res, I.Element);
                  end if;
               when others =>
                  Search_Index := False;
                  if not In_Snippet then
                     Add_Placeholder (I);
                     Append (Res, I.Element);
                  end if;
            end case;
            Dummy := I.Forward;
         end loop;

         --  Do nothing for $0 which will be removed at the end

         return Res;
      end Encode_String;

      -------------------
      -- Decode_String --
      -------------------

      function Decode_String (S : Virtual_String) return Virtual_String
      is
         use type VSS.Characters.Virtual_Character;

         Res   : Virtual_String;
         U     : Virtual_String := S;
         Dummy : Boolean;
      begin
         --  Remove the extra whitespace at the start
         while U.At_First_Character.Element = VSS.Characters.Latin.Space loop
            U.Delete (U.At_First_Character, U.At_First_Character);
         end loop;

         declare
            Iterator : VSS.Strings.Cursors.Iterators.Characters.
              Character_Iterator := U.At_First_Character;
         begin
            loop
               declare
                  Match : constant VSS.Regular_Expressions.
                    Regular_Expression_Match :=
                      Find_Next_Placeholder (U, Iterator);
               begin
                  exit when not Match.Has_Match;
                  declare
                     Pos : VSS.Strings.Cursors.Iterators.Characters.
                       Character_Iterator := U.At_Character
                         (Match.First_Marker);
                  begin
                     Dummy := Pos.Backward;
                     Append (Res, U.Slice (Iterator, Pos));
                  end;
                  Append
                    (Res,
                     Encoding
                       (VSS.Transformers.Casing.To_Lowercase.Transform
                            (Match.Captured)));
                  Iterator.Set_At (Match.Last_Marker);
                  Dummy := Iterator.Forward;
               end;
            end loop;

            Append (Res, U.Slice (Iterator, U.At_Last_Character));
         end;

         return Res;
      end Decode_String;

      ---------------------------
      -- Find_Next_Placeholder --
      ---------------------------

      function Find_Next_Placeholder
        (S     : Virtual_String;
         Start : VSS.Strings.Cursors.Iterators.Characters.
           Character_Iterator'Class)
         return VSS.Regular_Expressions.Regular_Expression_Match
      is
         Placeholder_Regexp : constant VSS.Regular_Expressions.
           Regular_Expression := VSS.Regular_Expressions.To_Regular_Expression
             ("FooBar_[0-9]+",
              Options => [VSS.Regular_Expressions.Case_Insensitive => True,
                          others => False]);
      begin
         return Placeholder_Regexp.Match (S, Start);
      end Find_Next_Placeholder;

      -----------------------
      -- Post_Pretty_Print --
      -----------------------

      function Post_Pretty_Print (S : Virtual_String) return Virtual_String
      is
         Res : Virtual_String;
      begin
         --  Remove this condition when V725-006 is fixed
         if Ends_With (S, New_Line_Function) then
            declare
               C : VSS.Strings.Cursors.Iterators.Characters.
                 Character_Iterator := S.At_Last_Character;
               Dummy : Boolean;
            begin
               Dummy := C.Backward;
               Res := Slice (S, S.At_First_Character, C);
            end;
         else
            Res := S;
         end if;

         --  Add the offset for each lines except the first one which is
         --  already aligned with the cursor
         declare
            Lines : constant VSS.String_Vectors.Virtual_String_Vector :=
              Res.Split_Lines
                (Terminators     =>
                   (VSS.Strings.CR | VSS.Strings.CRLF | VSS.Strings.LF => True,
                    others => False),
                 Keep_Terminator => True);
            First : Boolean := True;
         begin
            Res := Empty_Virtual_String;
            for Line of Lines loop
               if First then
                  First := False;
                  Res.Append (Line);
               else
                  Res.Append
                    (VSS.Strings.Conversions.To_Virtual_String
                       (Ada.Strings.Fixed."*" (Offset, " "))
                     & Line);
               end if;
            end loop;
         end;

         --  Add back snippet terminator
         Append (Res, "$0");
         return Res;
      end Post_Pretty_Print;

      use type LSP.Enumerations.InsertTextFormat;
   begin
      if LSP.Ada_Configurations.Completion_Formatting
        and then not Result.insertText.Is_Empty
        and then Result.insertTextFormat.Is_Set
        and then Result.insertTextFormat.Value = LSP.Enumerations.Snippet
      then
         declare
            Output      : Unbounded_String;
            S           : Virtual_String;
            Tmp_Unit    : Libadalang.Analysis.Analysis_Unit;
            Tmp_Context : constant Libadalang.Analysis.Analysis_Context :=
              Libadalang.Analysis.Create_Context;
         begin
            declare
               Full : constant String :=
                 VSS.Strings.Conversions.To_UTF_8_String
                   (Prefix & Encode_String (Result.insertText));
               Unparsing_Diagnostics :
                 Langkit_Support.Diagnostics.Diagnostics_Vectors.Vector;
            begin
               Tmp_Unit :=
                 Libadalang.Analysis.Get_From_Buffer
                   (Context  => Tmp_Context,
                    Filename => "",
                    Buffer   => Full,
                    Rule     => Rule);
               --  Tmp_Unit above is a synthetic buffer with Filename => "",
               --  but we deliberately resolve the unparsing configuration
               --  using Document's base name: this honours any source-specific
               --  formatting overrides of the destination document (e.g.
               --  keyword casing) when formatting the snippet.
               Output := Gnatformat.Formatting.Format
                 (Unit           => Tmp_Unit,
                  Format_Options => Context.Get_Format_Options,
                  Configuration  =>
                    Context.Get_Unparsing_Configuration
                      (Format_Options  => Context.Get_Format_Options,
                       Source_Filename =>
                         Context.URI_To_File (Document.URI).Display_Base_Name,
                       Diagnostics     => Unparsing_Diagnostics));

               if not Unparsing_Diagnostics.Is_Empty then
                  Context.Tracer.Trace
                    ("Diagnostics found while loading the unparsing "
                     & "configuration for "
                     & Context.URI_To_File (Document.URI).Display_Base_Name
                     & ":");
                  for Diagnostic of Unparsing_Diagnostics loop
                     Context.Tracer.Trace
                       (Langkit_Support.Diagnostics.To_Pretty_String
                          (Diagnostic));
                  end loop;
               end if;
            exception
               when E : others =>
                  --  Failed to pretty print the snippet, keep the previous
                  --  value.
                  Context.Tracer.Trace_Exception (E);
                  return;
            end;

            S :=
              VSS.Strings.Conversions.To_Virtual_String (To_String (Output));

            if not S.Is_Empty then
               --  The text is already formatted, don't try to indent it
               Result.insertTextMode :=
                 (Is_Set => True,
                  Value  => LSP.Enumerations.asIs);

               --  Label is too verbose and can conflict with client filtering
               --  set filterText to the content of the span we are replacing.
               Result.filterText := Prefix;

               Result.textEdit :=
                 (Is_Set => True,
                  Value  =>
                    (Is_TextEdit => True,
                     TextEdit    =>
                       (a_range => Span,
                        newText => Post_Pretty_Print (Decode_String (S)))));
            end if;
         end;
      end if;
   end Pretty_Print_Snippet;

end LSP.Ada_Completions;
