------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                        Copyright (C) 2018, AdaCore                       --
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

with LSP.Types;

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

   not overriding procedure Apply_Changes
     (Self   : aliased in out Document;
      Vector : LSP.Messages.TextDocumentContentChangeEvent_Vector)
   is
      File : constant LSP.Types.LSP_String :=
        LSP.Types.Delete (Self.URI, 1, 7);  --  Delete file://
   begin
      for Change of reverse Vector loop
         --  If whole document then reparse it
         if not Change.span.Is_Set then
            Self.Unit := Self.LAL.Get_From_Buffer
              (Filename => LSP.Types.To_UTF_8_String (File),
               Charset  => "utf-8",
               Buffer   => LSP.Types.To_UTF_8_String (Change.text));
         end if;
      end loop;
   end Apply_Changes;

   -----------------------
   -- Get_Definition_At --
   -----------------------



   not overriding function Get_Definition_At
     (Self     : Document;
      Position : LSP.Messages.Position)
      return Libadalang.Analysis.Defining_Name
   is

      use Libadalang.Common;

      Node : constant Libadalang.Analysis.Ada_Node :=
        Self.Get_Node_At (Position);

   begin

      if not Node.Is_Null and Node.Kind in Ada_Name then
         declare
            Name : constant Libadalang.Analysis.Name := Node.As_Name;
         begin
            if Name.P_Is_Defining then
               --  If this name is part of a definition, return that exact
               --  definition.
               return Name.P_Enclosing_Defining_Name;
            else
               --  Else return the name of the definition which is referred by
               --  this name.
               return Name.P_Xref;
            end if;
         end;
      else
         return Libadalang.Analysis.No_Defining_Name;
      end if;

   end Get_Definition_At;

   ----------------
   -- Get_Errors --
   ----------------

   not overriding procedure Get_Errors
     (Self   : Document;
      Errors : out LSP.Messages.Diagnostic_Vector)
   is
      Item : LSP.Messages.Diagnostic;
   begin
      Errors.Clear;

      if Self.Unit.Has_Diagnostics then
         for Error of Self.Unit.Diagnostics loop
            Item.span := To_Span (Error.Sloc_Range);

            Item.message := To_LSP_String
              (Ada.Strings.Wide_Wide_Unbounded.To_Wide_Wide_String
                 (Error.Message));

            Errors.Append (Item);
         end loop;
      end if;
   end Get_Errors;

   -----------------
   -- Get_Symbols --
   -----------------

   not overriding procedure Get_Symbols
     (Self   : Document;
      Result : out LSP.Messages.SymbolInformation_Vector)
   is
      Element : Libadalang.Analysis.Ada_Node;
      Item    : LSP.Messages.SymbolInformation;

      Is_Defining_Name : constant Libadalang.Iterators.Ada_Node_Predicate :=
        Libadalang.Iterators.Kind_Is (Libadalang.Common.Ada_Defining_Name);
      --  This object will be deallocated by Cursor's finalization

      Cursor : Libadalang.Iterators.Traverse_Iterator'Class :=
        Libadalang.Iterators.Find (Self.Unit.Root, Is_Defining_Name);

   begin
      Result.Clear;

      while Cursor.Next (Element) loop
         Item.name := To_LSP_String (Element.Text);
         Item.kind := Get_Decl_Kind (Element.As_Defining_Name.P_Basic_Decl);
         Item.location :=
           (uri  => Self.URI,
            span => To_Span (Element.Sloc_Range));

         Result.Append (Item);
      end loop;
   end Get_Symbols;

   -----------------
   -- Get_Node_At --
   -----------------

   not overriding function Get_Node_At
     (Self     : Document;
      Position : LSP.Messages.Position)
      return Libadalang.Analysis.Ada_Node
   is
      use Libadalang.Analysis;
      use Langkit_Support.Slocs;
   begin
      if Self.Unit.Root = No_Ada_Node then
         return No_Ada_Node;
      end if;

      return Self.Unit.Root.Lookup
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
              Ada_Subtype_Decl |
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

   not overriding procedure Initialize
     (Self : in out Document;
      LAL  : Libadalang.Analysis.Analysis_Context;
      Item : LSP.Messages.TextDocumentItem)
   is
      File : constant LSP.Types.LSP_String :=
        LSP.Types.Delete (Item.uri, 1, 7);  --  Delete file://
   begin
      Self.Unit := LAL.Get_From_Buffer
        (Filename => LSP.Types.To_UTF_8_String (File),
         Charset  => "utf-8",
         Buffer   => LSP.Types.To_UTF_8_String (Item.text));
      Self.URI := Item.uri;
      Self.LAL := LAL;
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

   ------------------------
   -- Get_Completions_At --
   ------------------------

   not overriding procedure Get_Completions_At
     (Self     : Document;
      Position : LSP.Messages.Position;
      Result   : out LSP.Messages.CompletionList)
   is
      use LSP.Types;
      use Libadalang.Analysis;
      use LSP.Messages;

      Real_Pos : constant LSP.Messages.Position :=
        (Position.line,
         UTF_16_Index'Max (0, Position.character - 1));
      --  Compute the position we want for completion, which is one character
      --  before the cursor.

      Node       : constant Libadalang.Analysis.Ada_Node :=
        Self.Get_Node_At (Real_Pos);
      --  Get the corresponding LAL node

   begin
      Server_Trace.Trace
        ("Getting completions, Pos = ("
         & Real_Pos.line'Image & ", " & Real_Pos.character'Image & ") Node = "
         & Image (Node));

      declare
         Raw_Completions : constant Basic_Decl_Array := Node.P_Complete;
      begin
         Server_Trace.Trace
           ("Number of raw completions : " & Raw_Completions'Length'Image);
         for BD of Raw_Completions loop
            if not BD.Is_Null then
               for DN of BD.P_Defining_Names loop
                  declare
                     R : CompletionItem;
                  begin
                     R.label := To_LSP_String (DN.P_Relative_Name.Text);
                     R.kind := (True, To_Completion_Kind (Get_Decl_Kind (BD)));
                     Result.items.Append (R);
                  end;
               end loop;
            end if;
         end loop;
      end;
   end Get_Completions_At;

end LSP.Ada_Documents;
