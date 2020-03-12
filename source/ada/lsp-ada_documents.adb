------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2018-2019, AdaCore                     --
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

with Ada.Characters.Wide_Latin_1;
with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
with Ada.Strings.Wide_Wide_Unbounded;

with Langkit_Support.Slocs;
with Langkit_Support.Text;
with Libadalang.Common;
with Libadalang.Iterators;

with LSP.Ada_Contexts; use LSP.Ada_Contexts;
with LSP.Lal_Utils;

package body LSP.Ada_Documents is

   function To_LSP_String
     (Value : Wide_Wide_String) return LSP.Types.LSP_String;

   function Get_Decl_Kind
     (Node : Libadalang.Analysis.Basic_Decl)
      return LSP.Messages.SymbolKind;
   --  Return a LSP SymbolKind for the given Libadalang Basic_Decl

   function Compute_Completion_Detail
     (BD : Libadalang.Analysis.Basic_Decl) return LSP.Types.LSP_String;

   function Unit
     (Self    : Document;
      Context : LSP.Ada_Contexts.Context)
      return Libadalang.Analysis.Analysis_Unit;
   --  Return the analysis unit for Self in the given context

   function To_Completion_Kind
     (K : LSP.Messages.SymbolKind) return LSP.Messages.CompletionItemKind
   is
     (case K is
        when LSP.Messages.A_Function => LSP.Messages.A_Function,
        when LSP.Messages.Field      => LSP.Messages.Field,
        when LSP.Messages.Variable   => LSP.Messages.Variable,
        when LSP.Messages.A_Package  => LSP.Messages.Module,
        when LSP.Messages.Module     => LSP.Messages.Module,
        when LSP.Messages.Class      => LSP.Messages.Class,
        when LSP.Messages.Number     => LSP.Messages.Value,
        when LSP.Messages.Enum       => LSP.Messages.Enum,
        when LSP.Messages.String     => LSP.Messages.Value,
        when LSP.Messages.A_Constant => LSP.Messages.Value,
        when others                  => LSP.Messages.Reference);
   --  Convert a SymbolKind to a CompletionItemKind.
   --  TODO: It might be better to have a unified kind, and then convert to
   --  specific kind types, but for the moment this is good enough.

   procedure Recompute_Indexes (Self : in out Document);
   --  Recompute the line-to-offset indexes in Self

   -----------------------
   -- Recompute_Indexes --
   -----------------------

   procedure Recompute_Indexes (Self : in out Document) is
      use LSP.Types;
   begin
      Self.Line_To_Index.Clear;

      --  To avoid too many reallocations during the initial filling
      --  of the index vector, pre-allocate it. Give a generous
      --  pre-allocation assuming that there is a line break every
      --  20 characters on average (this file has one line break
      --  every 33 characters).
      Self.Line_To_Index.Reserve_Capacity
        (Ada.Containers.Count_Type (Length (Self.Text) / 20));

      --  The first line (index 0) starts at offset 1
      Self.Line_To_Index.Append (1);

      for Ind in 1 .. Length (Self.Text) loop
         if Element (Self.Text, Ind) = Ada.Characters.Wide_Latin_1.LF then
            --  The contents of Line_To_Index is the first character
            --  in each line, so index Ind + 1 for the start of line.
            Self.Line_To_Index.Append (Ind + 1);
         end if;
      end loop;
   end Recompute_Indexes;

   -------------------
   -- Apply_Changes --
   -------------------

   procedure Apply_Changes
     (Self    : aliased in out Document;
      Version : LSP.Types.Optional_Number;
      Vector  : LSP.Messages.TextDocumentContentChangeEvent_Vector)
   is
      File : constant String :=
        Types.To_UTF_8_String (URI_To_File (Self.URI));
      Dummy : Libadalang.Analysis.Analysis_Unit;
      use LSP.Types;
   begin
      Self.Trace.Trace ("Applying changes for document " & File);

      if Version.Is_Set then
         Self.Version := Version.Value;
      end if;

      for Change of Vector loop
         if Change.span.Is_Set then
            --  We're replacing a range

            declare
               Low_Index : constant Natural :=
                 Self.Line_To_Index.Element
                   (Natural (Change.span.Value.first.line))
                 + Natural (Change.span.Value.first.character);
               High_Index : constant Natural :=
                 Self.Line_To_Index.Element
                   (Natural (Change.span.Value.last.line))
                 + Natural (Change.span.Value.last.character);
               Chars_Delta : Integer := 0;

               Orig : constant Line_To_Index_Vectors.Vector :=
                 Self.Line_To_Index;
            begin
               --  Do the actual replacement.

               --  Note: do this rather than Self.Text.Slice, to avoid
               --  allocating a potentially big string on the stack in the
               --  parameter to Slice.
               Self.Text := Unbounded_Slice (Self.Text, 1, Low_Index - 1)
                 & Change.text
                 & Unbounded_Slice (Self.Text, High_Index,
                                    Length (Self.Text));

               --  Recompute the indexes
               Self.Line_To_Index.Delete
                 (Index => Natural (Change.span.Value.first.line) + 1,
                  Count => Ada.Containers.Count_Type
                    (Self.Line_To_Index.Last_Index
                     - Natural (Change.span.Value.first.line)));

               --  This recomputes the line indexes for the characters that
               --  we've just added.
               for Ind in
                 Low_Index .. Low_Index + Length (Change.text) - 1
               loop
                  if Element (Self.Text, Ind) =
                    Ada.Characters.Wide_Latin_1.LF
                  then
                     Self.Line_To_Index.Append (Ind + 1);
                  end if;
               end loop;

               --  The delta in characters is the number of characters added
               --  minus the number of characters removed
               Chars_Delta := Length (Change.text) - (High_Index - Low_Index);

               --  All the lines after the change are still there, at an
               --  index modified by Chars_Delta.
               for J in
                 Natural (Change.span.Value.last.line) + 1 .. Orig.Last_Index
               loop
                  Self.Line_To_Index.Append (Orig.Element (J) + Chars_Delta);
               end loop;
            end;
         else
            Self.Text := Change.text;

            --  We're setting the whole text: compute the indexes now.
            Self.Recompute_Indexes;
         end if;
      end loop;
      Self.Trace.Trace ("Done applying changes for document " & File);
   end Apply_Changes;

   ----------------
   -- Get_Errors --
   ----------------

   procedure Get_Errors
     (Self    : Document;
      Context : LSP.Ada_Contexts.Context;
      Errors  : out LSP.Messages.Diagnostic_Vector)
   is
      Item : LSP.Messages.Diagnostic;
      Nb_Diags : Natural := 0;

      Unit : constant Libadalang.Analysis.Analysis_Unit := Self.Unit (Context);
   begin
      Errors.Clear;

      if Unit.Has_Diagnostics then
         for Error of Unit.Diagnostics loop
            Item.span := LSP.Lal_Utils.To_Span (Error.Sloc_Range);

            Item.message := To_LSP_String
              (Ada.Strings.Wide_Wide_Unbounded.To_Wide_Wide_String
                 (Error.Message));

            --  Filter out diagnostics that simply report "Cannot parse <..>",
            --  as these are generally not useful to the end user.
            if not LSP.Types.Starts_With (Item.message, "Cannot parse <") then
               Errors.Append (Item);
               Nb_Diags := Nb_Diags + 1;
               exit when Nb_Diags >= MAX_NB_DIAGNOSTICS;
            end if;
         end loop;
      end if;
   end Get_Errors;

   --------------------------
   -- Get_Symbol_Hierarchy --
   --------------------------

   procedure Get_Symbol_Hierarchy
     (Self    : Document;
      Context : LSP.Ada_Contexts.Context;
      Result  : out LSP.Messages.Symbol_Vector)
   is
      procedure Walk
        (Node   : Libadalang.Analysis.Ada_Node;
         Cursor : LSP.Messages.DocumentSymbol_Trees.Cursor;
         Tree   : in out LSP.Messages.DocumentSymbol_Tree);
      --  Traverse Node and all its children recursively. Find any defining
      --  name and construct corresponding symbol node, then append it to
      --  the Tree under a position pointed by the Cursor.

      ----------
      -- Walk --
      ----------

      procedure Walk
        (Node   : Libadalang.Analysis.Ada_Node;
         Cursor : LSP.Messages.DocumentSymbol_Trees.Cursor;
         Tree   : in out LSP.Messages.DocumentSymbol_Tree)
      is
         Next : LSP.Messages.DocumentSymbol_Trees.Cursor := Cursor;
      begin
         if Node.Kind in Libadalang.Common.Ada_Basic_Decl then
            declare
               use type Libadalang.Analysis.Defining_Name;

               Decl : constant Libadalang.Analysis.Basic_Decl :=
                 Node.As_Basic_Decl;

               Names : constant Libadalang.Analysis.Defining_Name_Array :=
                 Decl.P_Defining_Names;

            begin
               for Name of Names loop

                  exit when Name = Libadalang.Analysis.No_Defining_Name;

                  declare
                     Item : constant LSP.Messages.DocumentSymbol :=
                       (name           => To_LSP_String (Name.Text),
                        detail         => (Is_Set => False),
                        kind           => Get_Decl_Kind (Decl),
                        deprecated     => (Is_Set => False),
                        span           => LSP.Lal_Utils.To_Span
                          (Node.Sloc_Range),
                        selectionRange => LSP.Lal_Utils.To_Span
                          (Node.Sloc_Range),
                        children       => True);
                  begin
                     Tree.Insert_Child
                       (Parent   => Cursor,
                        Before   => Messages.DocumentSymbol_Trees.No_Element,
                        New_Item => Item,
                        Position => Next);
                  end;
               end loop;
            end;
         end if;

         for Child of Node.Children loop
            if Child not in Libadalang.Analysis.No_Ada_Node then
               Walk (Child, Next, Tree);
            end if;
         end loop;
      end Walk;

      Root : constant Libadalang.Analysis.Ada_Node := Self.Unit (Context).Root;
   begin
      Result := (Is_Tree => True, others => <>);
      Walk (Root, Result.Tree.Root, Result.Tree);
   end Get_Symbol_Hierarchy;

   -----------------
   -- Get_Symbols --
   -----------------

   procedure Get_Symbols
     (Self    : Document;
      Context : LSP.Ada_Contexts.Context;
      Result  : out LSP.Messages.Symbol_Vector)
   is
      Element : Libadalang.Analysis.Ada_Node;
      Item    : LSP.Messages.SymbolInformation;

      Is_Defining_Name : constant Libadalang.Iterators.Ada_Node_Predicate :=
        Libadalang.Iterators.Kind_Is (Libadalang.Common.Ada_Defining_Name);
      --  This object will be deallocated by Cursor's finalization

      Cursor : Libadalang.Iterators.Traverse_Iterator'Class :=
        Libadalang.Iterators.Find
          (Self.Unit (Context).Root, Is_Defining_Name);

   begin
      Result := LSP.Messages.Symbol_Vector'
        (Is_Tree => False,
         Vector  => <>);

      while Cursor.Next (Element) loop
         Item.name := To_LSP_String (Element.Text);
         Item.kind := Get_Decl_Kind (Element.As_Defining_Name.P_Basic_Decl);
         Item.location :=
           (uri     => Self.URI,
            span    => LSP.Lal_Utils.To_Span (Element.Sloc_Range),
            alsKind => LSP.Messages.Empty_Set);

         Result.Vector.Append (Item);
      end loop;
   end Get_Symbols;

   -----------------
   -- Get_Node_At --
   -----------------

   function Get_Node_At
     (Self     : Document;
      Context  : LSP.Ada_Contexts.Context;
      Position : LSP.Messages.Position)
      return Libadalang.Analysis.Ada_Node
   is
      use Libadalang.Analysis;
      use Langkit_Support.Slocs;

      Unit : constant Libadalang.Analysis.Analysis_Unit :=
        Self.Unit (Context);
   begin
      if Unit.Root = No_Ada_Node then
         return No_Ada_Node;
      end if;

      return Unit.Root.Lookup
        ((Line   => Line_Number (Position.line) + 1,
          Column => Column_Number (Position.character) + 1));
   end Get_Node_At;

   ------------------------
   -- Get_Folding_Blocks --
   ------------------------

   procedure Get_Folding_Blocks
     (Self       : Document;
      Context    : LSP.Ada_Contexts.Context;
      Lines_Only : Boolean;
      Result     : out LSP.Messages.FoldingRange_Vector)
   is
      use Libadalang.Common;
      use Libadalang.Analysis;

      Location     : LSP.Messages.Location;
      foldingRange : LSP.Messages.FoldingRange;
      Have_With    : Boolean := False;

      function Parse (Node : Ada_Node'Class) return Visit_Status;
      --  Includes Node location to the result if the node has "proper" kind

      procedure Store_Span (Span : LSP.Messages.Span);
      --  Include Span to the result .

      -----------
      -- Parse --
      -----------

      function Parse (Node : Ada_Node'Class) return Visit_Status
      is

         procedure Store_With_Block;
         --  Store folding for with/use clauses as one folding block

         ----------------------
         -- Store_With_Block --
         ----------------------

         procedure Store_With_Block is
         begin
            if not Have_With then
               return;
            end if;

            if foldingRange.startLine /= foldingRange.endLine then
               Result.Append (foldingRange);
            end if;

            Have_With := False;
         end Store_With_Block;

         Result : Visit_Status := Into;
      begin
--        Cat_Namespace,
--        Cat_Constructor,
--        Cat_Destructor,
--        Cat_Structure,
--        Cat_Case_Inside_Record,
--        Cat_Union,
--        Cat_Custom

         case Node.Kind is
            when Ada_Package_Decl |
                 Ada_Generic_Formal_Package |
                 Ada_Package_Body |
                 --  Cat_Package

                 Ada_Type_Decl |
                 Ada_Record_Def |

                 Ada_Classwide_Type_Decl |
--        Cat_Class

                 Ada_Protected_Type_Decl |
--        Cat_Protected

                 Ada_Task_Type_Decl |
                 Ada_Single_Task_Type_Decl |
--        Cat_Task

                 Ada_Subp_Decl |
                 Ada_Subp_Body |
                 Ada_Generic_Formal_Subp_Decl |
                 Ada_Abstract_Subp_Decl |
                 Ada_Abstract_Formal_Subp_Decl |
                 Ada_Concrete_Formal_Subp_Decl |
                 Ada_Generic_Subp_Internal |
                 Ada_Null_Subp_Decl |
                 Ada_Subp_Renaming_Decl |
                 Ada_Subp_Body_Stub |
                 Ada_Generic_Subp_Decl |
                 Ada_Generic_Subp_Instantiation |
                 Ada_Generic_Subp_Renaming_Decl |
                 Ada_Subp_Kind_Function |
                 Ada_Subp_Kind_Procedure |
                 Ada_Access_To_Subp_Def |
--        Cat_Procedure
--        Cat_Function
--        Cat_Method

                 Ada_Case_Stmt |
--        Cat_Case_Statement

                 Ada_If_Stmt |
--        Cat_If_Statement

                 Ada_For_Loop_Stmt |
                 Ada_While_Loop_Stmt |
--        Cat_Loop_Statement

                 Ada_Begin_Block |
                 Ada_Decl_Block |
--        Cat_Declare_Block
--        Cat_Simple_Block

--                 Ada_Return_Stmt |
--                 Ada_Extended_Return_Stmt |
                 Ada_Extended_Return_Stmt_Object_Decl |
--        Cat_Return_Block

                 Ada_Select_Stmt |
--        Cat_Select_Statement

                 Ada_Entry_Body |
--        Cat_Entry

                 Ada_Exception_Handler |
--        Cat_Exception_Handler

                 Ada_Pragma_Node_List |
                 Ada_Pragma_Argument_Assoc |
                 Ada_Pragma_Node |
--        Cat_Pragma

                 Ada_Aspect_Spec =>
--        Cat_Aspect

               Store_With_Block;

               foldingRange.kind :=
                 (Is_Set => True, Value => LSP.Messages.Region);

               Location := LSP.Lal_Utils.Get_Node_Location (Ada_Node (Node));
               Store_Span (Location.span);

            when Ada_With_Clause |
                 Ada_Use_Package_Clause |
                 Ada_Use_Type_Clause =>

               Location := LSP.Lal_Utils.Get_Node_Location (Ada_Node (Node));

               if not Have_With then
                  Have_With := True;

                  foldingRange.kind :=
                    (Is_Set => True, Value => LSP.Messages.Imports);

                  foldingRange.startLine := Integer (Location.span.first.line);
               end if;

               foldingRange.endLine := Integer (Location.span.last.line);

               --  Do not step into with/use clause
               Result := Over;

            when others =>
               Store_With_Block;
         end case;

         return Result;
      end Parse;

      ----------------
      -- Store_Span --
      ----------------

      procedure Store_Span (Span : LSP.Messages.Span) is
         use type LSP.Types.Line_Number;
      begin
         if not Lines_Only
           or else Span.first.line /= Span.last.line
         then
            foldingRange.startLine := Integer (Span.first.line);
            foldingRange.endLine   := Integer (Span.last.line);

            if not Lines_Only then
               foldingRange.startCharacter :=
                 (Is_Set => True,
                  Value  => Integer (Span.first.character));

               foldingRange.startCharacter :=
                 (Is_Set => True,
                  Value  => Integer (Span.last.character));
            end if;

            Result.Append (foldingRange);
         end if;
      end Store_Span;

      Token : Token_Reference;
      Span  : LSP.Messages.Span;

   begin
      Traverse (Self.Unit (Context).Root, Parse'Access);

      --  Looking for comments
      foldingRange.kind := (Is_Set => False);
      Token             := First_Token (Self.Unit (Context));

      while Token /= No_Token loop
         case Kind (Data (Token)) is
            when Ada_Comment =>
               if not foldingRange.kind.Is_Set then
                  foldingRange.kind :=
                    (Is_Set => True, Value => LSP.Messages.Comment);
                  Span := LSP.Lal_Utils.Get_Token_Span (Token);
               else
                  Span.last := LSP.Lal_Utils.Get_Token_Span (Token).last;
               end if;

            when Ada_Whitespace =>
               null;

            when others =>
               if foldingRange.kind.Is_Set then
                  Store_Span (Span);
                  foldingRange.kind := (Is_Set => False);
               end if;
         end case;

         Token := Next (Token);
      end loop;
   end Get_Folding_Blocks;

   -------------------
   -- Get_Decl_Kind --
   -------------------

   function Get_Decl_Kind
     (Node : Libadalang.Analysis.Basic_Decl)
      return LSP.Messages.SymbolKind
   is
      use Libadalang.Common;

   begin
      case Node.Kind is
         when Ada_Generic_Formal_Subp_Decl |
              Ada_Abstract_Subp_Decl |
              Ada_Abstract_Formal_Subp_Decl |
              Ada_Concrete_Formal_Subp_Decl |
              Ada_Null_Subp_Decl |
              Ada_Subp_Decl |
              Ada_Subp_Renaming_Decl |
              Ada_Expr_Function |
              Ada_Subp_Body |
              Ada_Subp_Body_Stub |
              Ada_Entry_Body |
              Ada_Entry_Decl |
              Ada_Generic_Subp_Decl |
              Ada_Generic_Subp_Instantiation |
              Ada_Generic_Subp_Renaming_Decl =>
            return LSP.Messages.A_Function;

         when Ada_Component_Decl |
              Ada_Discriminant_Spec =>
            return LSP.Messages.Field;

         when Ada_Generic_Formal_Obj_Decl |
              Ada_Param_Spec |
              Ada_Exception_Handler |
              Ada_Object_Decl |
              Ada_Extended_Return_Stmt_Object_Decl |
              Ada_Single_Protected_Decl |
              Ada_Single_Task_Decl =>
            return LSP.Messages.Variable;

         when Ada_Generic_Formal_Package |
              Ada_Package_Decl |
              Ada_Generic_Package_Decl |
              Ada_Generic_Package_Instantiation |
              Ada_Generic_Package_Renaming_Decl |
              Ada_Package_Renaming_Decl =>
            return LSP.Messages.A_Package;

         when
              Ada_Package_Body_Stub |
              Ada_Protected_Body_Stub |
              Ada_Task_Body_Stub |
              Ada_Package_Body |
              Ada_Protected_Body |
              Ada_Task_Body =>
            return LSP.Messages.Module;

         when Ada_Generic_Formal_Type_Decl |
              Ada_Classwide_Type_Decl |
              Ada_Incomplete_Type_Decl |
              Ada_Incomplete_Tagged_Type_Decl |
              Ada_Protected_Type_Decl |
              Ada_Task_Type_Decl |
              Ada_Type_Decl |
              Ada_Subtype_Decl |
              Ada_Anonymous_Type_Decl |
              Ada_Synth_Anonymous_Type_Decl =>
            return LSP.Messages.Class;

         when Ada_Entry_Index_Spec |
              Ada_Number_Decl =>
            return LSP.Messages.Number;

         when Ada_Enum_Literal_Decl =>
            return LSP.Messages.Enum;

         when Ada_Exception_Decl =>
            return LSP.Messages.String;

         when Ada_For_Loop_Var_Decl |
              Ada_Label_Decl |
              Ada_Named_Stmt_Decl =>
            return LSP.Messages.A_Constant;

         when others
            => null;
      end case;

      return LSP.Messages.A_Function;
   end Get_Decl_Kind;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self : in out Document;
      URI  : LSP.Messages.DocumentUri;
      Text : LSP.Types.LSP_String)
   is
   begin
      Self.URI  := URI;
      Self.Version := 1;
      Self.Text := Text;
      Recompute_Indexes (Self);
   end Initialize;

   -------------------
   -- To_LSP_String --
   -------------------

   function To_LSP_String
     (Value : Wide_Wide_String) return LSP.Types.LSP_String is
   begin
      return LSP.Types.To_LSP_String
        (Ada.Strings.UTF_Encoding.Wide_Wide_Strings.Encode (Value));
   end To_LSP_String;

   --------------------
   -- Compute_Detail --
   --------------------

   function Compute_Completion_Detail
     (BD : Libadalang.Analysis.Basic_Decl) return LSP.Types.LSP_String
   is
      use Libadalang.Analysis;
      use Libadalang.Common;
      use LSP.Messages;
      use LSP.Types;

      Ret : LSP_String;
   begin
      case Get_Decl_Kind (BD) is
         when A_Function =>
            Append (Ret, "(subprogram) ");

         when Variable =>
            case BD.Kind is
               when Ada_Param_Spec =>
                  Append (Ret, "(param) ");
               when others =>
                  Append (Ret, "(var) ");
            end case;

            declare
               TE : constant Type_Expr := BD.As_Basic_Decl.P_Type_Expression;
            begin
               if not TE.Is_Null then
                  Append
                    (Ret,
                     To_LSP_String (TE.Text));
               end if;
            end;
         when LSP.Messages.Class =>

            Append (Ret, "(type) ");

         when LSP.Messages.A_Package =>
            Append (Ret, "(package) ");

         when others => null;
      end case;

      return Ret;
   end Compute_Completion_Detail;

   ------------------------
   -- Get_Completions_At --
   ------------------------

   procedure Get_Completions_At
     (Self     : Document;
      Context  : LSP.Ada_Contexts.Context;
      Position : LSP.Messages.Position;
      Result   : out LSP.Messages.CompletionList)
   is
      use Libadalang.Analysis;
      use Libadalang.Common;
      use LSP.Messages;
      use LSP.Types;

      Real_Pos : constant LSP.Messages.Position :=
        (Position.line,
         UTF_16_Index'Max (0, Position.character - 1));
      --  Compute the position we want for completion, which is one character
      --  before the cursor.

      Node       : Libadalang.Analysis.Ada_Node :=
        Self.Get_Node_At (Context, Real_Pos);
      --  Get the corresponding LAL node
   begin
      Context.Trace.Trace ("In Get_Completions_At");

      --  Get the outermost dotted name of which node is a prefix, so that when
      --  completing in a situation such as the following:
      --
      --      Ada.Tex|
      --             ^ Cursor here
      --
      --  we get the DottedName node rather than just the "Tex" BaseId. We want
      --  the DottedName rather than the Id so as to get the proper completions
      --  (all elements in the "Ada" namespace).

      while Node.Kind in Ada_Single_Tok_Node | Ada_Dotted_Name loop
         if Node.Parent.Kind = Ada_Dotted_Name
           and then Node.Parent.As_Dotted_Name.F_Suffix = Node
         then
            Node := Node.Parent;
         else
            exit;
         end if;
      end loop;

      Context.Trace.Trace
        ("Getting completions, Pos = ("
         & Real_Pos.line'Image & ", " & Real_Pos.character'Image & ") Node = "
         & Image (Node));

      declare
         Raw_Completions : constant Basic_Decl_Array :=
           Node.P_Complete;
      begin
         Context.Trace.Trace
           ("Number of raw completions : " & Raw_Completions'Length'Image);
         for BD of Raw_Completions loop
            if not BD.Is_Null then
               for DN of BD.P_Defining_Names loop
                  declare
                     R      : CompletionItem;
                     Prefix : constant Ada.Strings.UTF_Encoding.UTF_8_String :=
                              Langkit_Support.Text.To_UTF8 (Node.Text);
                  begin

                     --  If we are not completing a dotted name, filter the
                     --  raw completion results by the node's prefix.
                     if Node.Kind in Ada_Dotted_Name_Range
                       or else Starts_With
                         (To_LSP_String (DN.P_Relative_Name.Text),
                          Prefix => Prefix)
                     then
                        R.label := To_LSP_String (DN.P_Relative_Name.Text);
                        R.kind := (True, To_Completion_Kind
                                   (Get_Decl_Kind (BD)));
                        R.detail := (True, Compute_Completion_Detail (BD));
                        Result.items.Append (R);
                     end if;
                  end;
               end loop;
            end if;
         end loop;

         Context.Trace.Trace
           ("Number of filtered completions : " & Result.items.Length'Image);
      end;
   end Get_Completions_At;

   ----------
   -- Unit --
   ----------

   function Unit
     (Self    : Document;
      Context : LSP.Ada_Contexts.Context)
      return Libadalang.Analysis.Analysis_Unit
   is
      File : constant LSP.Types.LSP_String := URI_To_File (Self.URI);
   begin
      return Context.LAL_Context.Get_From_File
        (Filename => LSP.Types.To_UTF_8_String (File),
         Reparse  => False);
   end Unit;

   --------------------------
   -- Versioned_Identifier --
   --------------------------

   function Versioned_Identifier
     (Self : Document) return LSP.Messages.VersionedTextDocumentIdentifier is
   begin
      return (uri     => Self.URI,
              version => (Is_Set => True, Value => Self.Version));
   end Versioned_Identifier;

end LSP.Ada_Documents;
