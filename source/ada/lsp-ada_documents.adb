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

with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
with Ada.Strings.Wide_Wide_Unbounded;

with Langkit_Support.Slocs;
with Libadalang.Common;
with Libadalang.Iterators;

with LSP.Ada_Contexts; use LSP.Ada_Contexts;

package body LSP.Ada_Documents is

   function To_Span
     (Value : Langkit_Support.Slocs.Source_Location_Range)
      return LSP.Messages.Span;

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

   -------------------
   -- Apply_Changes --
   -------------------

   procedure Apply_Changes
     (Self    : aliased in out Document;
      Vector  : LSP.Messages.TextDocumentContentChangeEvent_Vector)
   is
      File : constant String :=
        Types.To_UTF_8_String (URI_To_File (Self.URI));
      Dummy : Libadalang.Analysis.Analysis_Unit;
   begin
      Self.Trace.Trace ("Applying changes for document " & File);
      for Change of reverse Vector loop
         --  If whole document then store it as the new text
         if Change.span.Is_Set then
            --  TODO: add support for changes given by span here. Until
            --  this is done, raise an error.
            raise Program_Error with "change ranges are not yet supported";
         else
            Self.Text := Change.text;
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
            Item.span := To_Span (Error.Sloc_Range);

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
                        span           => To_Span (Node.Sloc_Range),
                        selectionRange => To_Span (Node.Sloc_Range),
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
            span    => To_Span (Element.Sloc_Range),
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
      Self.Text := Text;
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

   -------------
   -- To_Span --
   -------------

   function To_Span
     (Value : Langkit_Support.Slocs.Source_Location_Range)
      return LSP.Messages.Span
   is
      use type LSP.Types.Line_Number;
      use type LSP.Types.UTF_16_Index;

      Result : constant LSP.Messages.Span :=
        (first =>
           (line      => LSP.Types.Line_Number (Value.Start_Line) - 1,
            character => LSP.Types.UTF_16_Index   --  FIXME (UTF16 index)!
              (Value.Start_Column) - 1),
         last =>
           (line => LSP.Types.Line_Number (Value.End_Line) - 1,
            character => LSP.Types.UTF_16_Index  --  FIXME (UTF16 index)!
              (Value.End_Column) - 1));
   begin
      return Result;
   end To_Span;

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
                     R : CompletionItem;
                  begin
                     R.label := To_LSP_String (DN.P_Relative_Name.Text);
                     R.kind := (True, To_Completion_Kind (Get_Decl_Kind (BD)));
                     R.detail := (True, Compute_Completion_Detail (BD));
                     Result.items.Append (R);
                  end;
               end loop;
            end if;
         end loop;
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

end LSP.Ada_Documents;
