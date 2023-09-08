------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2018-2023, AdaCore                     --
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

with Ada.Characters.Wide_Wide_Latin_1;
with Ada.Tags;
with Ada.Unchecked_Deallocation;

with GNAT.Strings;
with GNATCOLL.Traces;
with GNATCOLL.Utils;
with GNATCOLL.VFS;

with Langkit_Support.Symbols;
with Langkit_Support.Text;

with Laltools.Common;
with Pp.Scanner;
with Utils.Char_Vectors;

with Libadalang.Iterators;
with Libadalang.Sources;

with VSS.Characters;
with VSS.Strings.Character_Iterators;
with VSS.Strings.Conversions;
with VSS.Strings.Cursors;
with VSS.Strings.Formatters.Integers;
with VSS.Strings.Formatters.Strings;
with VSS.Strings.Line_Iterators;
with VSS.Strings.Templates;
with VSS.Unicode;

with LSP.Ada_Completions.Filters;
with LSP.Ada_Contexts;
with LSP.Ada_Documentation;
with LSP.Ada_Documents.LAL_Diagnostics;
with LSP.Ada_Id_Iterators;
with LSP.Enumerations;
with LSP.Predicates;
with LSP.Utils;
with LSP.Structures.LSPAny_Vectors;
with LSP.Ada_Handlers.Locations;

package body LSP.Ada_Documents is
   pragma Warnings (Off);

   package Utils renames Standard.Utils;

   Lal_PP_Output : constant GNATCOLL.Traces.Trace_Handle :=
     GNATCOLL.Traces.Create
       ("ALS.LAL_PP_OUTPUT_ON_FORMATTING", GNATCOLL.Traces.Off);
   --  Logging lalpp output if On

   procedure Recompute_Indexes (Self : in out Document'Class);
   --  Recompute the line-to-offset indexes in Self

   procedure Recompute_Markers
     (Self         : in out Document'Class;
      Low_Line     : Natural;
      Start_Marker : VSS.Strings.Markers.Character_Marker;
      End_Marker   : VSS.Strings.Markers.Character_Marker);
   --  Recompute line-to-marker index starting from Start_Marker till
   --  End_Marker and filling index table starting at Low_Line. End_Marker
   --  may be invalid marker, in this case indexing down to the end of the
   --  text.

   procedure Span_To_Markers
     (Self : Document'Class;
      Span : LSP.Structures.A_Range;
      From : out VSS.Strings.Markers.Character_Marker;
      To   : out VSS.Strings.Markers.Character_Marker);

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

   -------------------
   -- Apply_Changes --
   -------------------

   procedure Apply_Changes
     (Self    : aliased in out Document;
      Version : Integer;
      Vector  : LSP.Structures.TextDocumentContentChangeEvent_Vector)
   is
      Dummy : Libadalang.Analysis.Analysis_Unit;
   begin
      Self.Version := Version;

      for Change of Vector loop
         if Change.a_range.Is_Set then
            --  We're replacing a range

            declare
               Low_Line    : Natural := Change.a_range.Value.start.line;
               High_Line   : Natural := Change.a_range.Value.an_end.line;
               Delete_High : Natural := High_Line;
               Start_Index : Natural;

               First_Marker : VSS.Strings.Markers.Character_Marker;
               Last_Marker  : VSS.Strings.Markers.Character_Marker;
               Start_Marker : VSS.Strings.Markers.Character_Marker;
               End_Marker   : VSS.Strings.Markers.Character_Marker;

            begin
               --  Do text replacement

               Self.Span_To_Markers
                 (Change.a_range.Value, First_Marker, Last_Marker);
               Self.Text.Replace (First_Marker, Last_Marker, Change.text);

               --  Markers inside modified range of lines need to be
               --  recomputed, markers outside of this range has been
               --  recomputed by call to Replace.

               --  Use marker of the line before the first modified line as
               --  start marker for recompute because marker of the first
               --  modified line may be ether invalidated or moved by Replace,
               --  or start from first character of the new text when first
               --  line was modified.

               if Low_Line /= Self.Line_To_Marker.First_Index then
                  Low_Line     := Low_Line - 1;
                  Start_Index  := Low_Line;
                  Start_Marker := Self.Line_To_Marker (Low_Line);

               else
                  Start_Index  := Self.Line_To_Marker.First_Index;
                  Start_Marker := Self.Text.At_First_Character.Marker;
               end if;

               --  Use marker of the line after the last modified line as end
               --  marker for recompute because marker of the last modified
               --  line may be ether invalidated or moved and not point to the
               --  beginning of the line, or use invalid marker when last line
               --  was modified.

               if High_Line /= Self.Line_To_Marker.Last_Index then
                  Delete_High := High_Line;
                  High_Line := High_Line + 1;
                  End_Marker := Self.Line_To_Marker (High_Line);
               end if;

               if Low_Line = Self.Line_To_Marker.First_Index
                 and then High_Line = Self.Line_To_Marker.Last_Index
               then
                  Self.Recompute_Indexes;

               else
                  if Delete_High >= Low_Line then
                     Self.Line_To_Marker.Delete
                       (Low_Line,
                        Ada.Containers.Count_Type
                          (Delete_High - Low_Line + 1));
                  end if;

                  Self.Recompute_Markers
                    (Start_Index, Start_Marker, End_Marker);
               end if;
            end;

         else
            Self.Text := Change.text;

            --  We're setting the whole text: compute the indexes now.
            Self.Recompute_Indexes;
         end if;
      end loop;
   end Apply_Changes;

   -------------
   -- Cleanup --
   -------------

   procedure Cleanup (Self : in out Document) is
   begin
      for Source of Self.Diagnostic_Sources loop
         LSP.Diagnostic_Sources.Unchecked_Free (Source);
      end loop;
   end Cleanup;

   -----------------------------
   -- Compute_Completion_Item --
   -----------------------------

   function Compute_Completion_Item
     (Document                 : LSP.Ada_Documents.Document;
      Handler                  : in out LSP.Ada_Handlers.Message_Handler;
      Context                  : LSP.Ada_Contexts.Context;
      Sloc                     : Langkit_Support.Slocs.Source_Location;
      Node                     : Libadalang.Analysis.Ada_Node;
      BD                       : Libadalang.Analysis.Basic_Decl;
      Label                    : VSS.Strings.Virtual_String;
      Use_Snippets             : Boolean;
      Compute_Doc_And_Details  : Boolean;
      Named_Notation_Threshold : Natural;
      Is_Dot_Call              : Boolean;
      Is_Visible               : Boolean;
      Pos                      : Integer;
      Weight                   : Ada_Completions.Completion_Item_Weight_Type;
      Completions_Count        : Natural)
      return LSP.Structures.CompletionItem
   is

      package Weight_Formatters renames VSS.Strings.Formatters.Integers;

      Item           : LSP.Structures.CompletionItem;
      Subp_Spec_Node : Libadalang.Analysis.Base_Subp_Spec;
      Min_Width      : constant Natural := Completions_Count'Image'Length - 1;
      --  The -1 remove the whitespace added by 'Image

      Last_Weight : constant Ada_Completions.Completion_Item_Weight_Type :=
        Ada_Completions.Completion_Item_Weight_Type'Last;

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
         return VSS.Strings.Virtual_String
      is
         use VSS.Strings;
      begin
         return Sort_Text : VSS.Strings.Virtual_String do

            Sort_Text := VSS.Strings.Templates.Format
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
      Item.kind := (True, To_Completion_Kind (LSP.Utils.Get_Decl_Kind (BD)));

      if not Is_Visible then
         Item.insertText := Label;
         Item.label.Append (" (invisible)");
         Item.filterText := Label;
      end if;

      Item.sortText := Get_Sort_Text (Label);

      Set_Completion_Item_Documentation
        (Handler                 => Handler,
         Context                 => Context,
         BD                      => BD,
         Item                    => Item,
         Compute_Doc_And_Details => Compute_Doc_And_Details);

      --  Return immediately if we should not use snippets (e.g: completion for
      --  invisible symbols).
      if not Use_Snippets then
         return Item;
      end if;

      --  Check if we are dealing with a subprogram and return a completion
      --  snippet that lists all the formal parameters if it's the case.

      Subp_Spec_Node := BD.P_Subp_Spec_Or_Null;

      if Subp_Spec_Node.Is_Null then
         return Item;
      end if;

      declare
         Insert_Text : VSS.Strings.Virtual_String := Label;
         All_Params  : constant Libadalang.Analysis.Param_Spec_Array :=
           Subp_Spec_Node.P_Params;

         Params      : constant Libadalang.Analysis.Param_Spec_Array :=
           (if Is_Dot_Call then
               All_Params (All_Params'First + 1 .. All_Params'Last)
            else
               All_Params);
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
              (Is_Set => True,
               Value  => LSP.Enumerations.Snippet);

            Insert_Text.Append (" (");

            --  Compute number of params to know if named notation should be
            --  used.

            for Param of Params loop
               Nb_Params := Nb_Params + Param.F_Ids.Children_Count;
            end loop;

            Use_Named_Notation := Named_Notation_Threshold > 0
              and then Nb_Params >= Named_Notation_Threshold;

            for Param of Params loop
               for Id of Param.F_Ids loop
                  declare
                     Mode : constant Langkit_Support.Text.Text_Type :=
                       Param.F_Mode.Text;

                  begin
                     if Use_Named_Notation then
                        Insert_Text.Append
                          (VSS.Strings.To_Virtual_String (Id.Text));
                        Insert_Text.Append (" => ");
                        Insert_Text.Append ("${");
                        Insert_Text.Append
                          (VSS.Strings.Conversions.To_Virtual_String
                            (GNATCOLL.Utils.Image (Idx, Min_Width => 1)));
                        Insert_Text.Append (':');
                        Insert_Text.Append
                          (VSS.Strings.To_Virtual_String (Id.Text));
                        Insert_Text.Append (" : ");
                        Insert_Text.Append
                          ((if Mode /= ""
                             then VSS.Strings.To_Virtual_String (Mode & " ")
                             else ""));
                        Insert_Text.Append
                          (VSS.Strings.To_Virtual_String
                             (Param.F_Type_Expr.Text));
                        Insert_Text.Append ("}, ");

                     else
                        Insert_Text.Append ("${");
                        Insert_Text.Append
                          (VSS.Strings.Conversions.To_Virtual_String
                             (GNATCOLL.Utils.Image (Idx, Min_Width => 1)));
                        Insert_Text.Append (':');
                        Insert_Text.Append
                          (VSS.Strings.To_Virtual_String (Id.Text));
                        Insert_Text.Append (" : ");
                        Insert_Text.Append
                          ((if Mode /= ""
                             then VSS.Strings.To_Virtual_String (Mode & " ")
                             else ""));
                        Insert_Text.Append
                          (VSS.Strings.To_Virtual_String
                             (Param.F_Type_Expr.Text));
                        Insert_Text.Append ("}, ");
                     end if;

                     Idx := Idx + 1;
                  end;
               end loop;
            end loop;

            --  Remove the ", " substring that has been appended in the last
            --  loop iteration.

            declare
               First   : constant
                 VSS.Strings.Character_Iterators.Character_Iterator :=
                   Insert_Text.At_First_Character;
               Last    : VSS.Strings.Character_Iterators.Character_Iterator :=
                 Insert_Text.At_Last_Character;
               Success : Boolean with Unreferenced;

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

   ----------
   -- Diff --
   ----------

   procedure Diff
     (Self     : Document;
      New_Text : VSS.Strings.Virtual_String;
      Old_Span : LSP.Structures.A_Range := Empty_Range;
      New_Span : LSP.Structures.A_Range := Empty_Range;
      Edit     : out LSP.Structures.TextEdit_Vector)
   is
      use type LSP.Structures.A_Range;
      use type LSP.Structures.Position;

      Old_First_Line : Natural;
      New_First_Line : Natural;

      Old_Lines, New_Lines   : VSS.String_Vectors.Virtual_String_Vector;
      Old_Length, New_Length : Natural;

   begin
      Old_Lines :=
        Self.Text.Split_Lines
          (Terminators     => LSP_New_Line_Function_Set,
           Keep_Terminator => True);
      New_Lines :=
        New_Text.Split_Lines
          (Terminators     => LSP_New_Line_Function_Set,
           Keep_Terminator => True);

      if Old_Span = Empty_Range then
         Old_First_Line := 1;
         Old_Length     := Old_Lines.Length;

      else
         Old_First_Line := Natural (Old_Span.start.line + 1);
         Old_Length :=
           Natural (Old_Span.an_end.line - Old_Span.start.line + 1);
      end if;

      if New_Span = Empty_Range then
         New_First_Line := 1;
         New_Length     := New_Lines.Length;
      else
         New_First_Line := Natural (New_Span.start.line + 1);
         New_Length :=
           Natural (New_Span.an_end.line - New_Span.start.line + 1);
      end if;

      declare
         use type VSS.Strings.Virtual_String;

         type LCS_Array is array
           (Natural range 0 .. Old_Length,
            Natural range 0 .. New_Length) of Integer;
         type LCS_Array_Access is access all LCS_Array;

         procedure Free is
           new Ada.Unchecked_Deallocation (LCS_Array, LCS_Array_Access);

         LCS    : LCS_Array_Access := new LCS_Array;
         Match  : Integer;
         Delete : Integer;
         Insert : Integer;

         Old_Index : Natural := Old_Length;
         New_Index : Natural := New_Length;

         Old_Natural : Natural;
         --  needed to determine which line number in the old buffer is
         --  changed, deleted or before which new lines are inserted

         Changed_Block_Text : VSS.Strings.Virtual_String;
         Changed_Block_Span : LSP.Structures.A_Range := ((0, 0), (0, 0));

         procedure Prepare
           (Line : Natural;
            Text : VSS.Strings.Virtual_String);
         --  Store imformation for Text_Etid in New_String and Span

         procedure Add (From_Line : Natural);
         --  Add prepared New_String and Span into Text_Edit

         -------------
         -- Prepare --
         -------------

         procedure Prepare
           (Line : Natural;
            Text : VSS.Strings.Virtual_String) is
         begin
            if Changed_Block_Span.an_end = (0, 0) then
               --  it is the first portion of a changed block so store
               --  last position of the changes
               Changed_Block_Span.an_end := (Line, 0);
            end if;

            --  accumulating new text for the changed block
            Changed_Block_Text.Prepend (Text);
         end Prepare;

         ---------
         -- Add --
         ---------

         procedure Add (From_Line : Natural) is
         begin
            if Changed_Block_Span.an_end = (0, 0) then
               --  No information for Text_Edit
               return;
            end if;

            Changed_Block_Span.start :=
              (line      => From_Line,
               character => 0);

            Edit.Prepend
              (LSP.Structures.TextEdit'
                 (a_range => Changed_Block_Span,
                  newText => Changed_Block_Text));

            --  clearing
            Changed_Block_Text.Clear;
            Changed_Block_Span := ((0, 0), (0, 0));
         end Add;

      begin
         --  prepare LCS

         --  default values for line 0
         for Index in 0 .. Old_Length loop
            LCS (Index, 0) := -5 * Index;
         end loop;

         --  default values for the first column
         for Index in 0 .. New_Length loop
            LCS (0, Index) := -5 * Index;
         end loop;

         --  calculate LCS
         for Row in 1 .. Old_Length loop
            for Column in 1 .. New_Length loop
               Match := LCS (Row - 1, Column - 1) +
                 (if Old_Lines (Old_First_Line + Row - 1) =
                      New_Lines (New_First_Line + Column - 1)
                  then 10   --  +10 is the 'weight' for equal lines
                  else -1); --  and -1 for the different

               Delete := LCS (Row - 1, Column) - 5;
               Insert := LCS (Row, Column - 1) - 5;

               LCS (Row, Column) := Integer'Max (Match, Insert);
               LCS (Row, Column) := Integer'Max (LCS (Row, Column), Delete);
            end loop;
         end loop;

         --  iterate over LCS and create Text_Edit

         Old_Natural := Natural (Old_First_Line + Old_Length - 1);

         while Old_Index > 0
           and then New_Index > 0
         loop
            if LCS (Old_Index, New_Index) =
              LCS (Old_Index - 1, New_Index - 1) +
              (if Old_Lines (Old_First_Line + Old_Index - 1) =
                   New_Lines (New_First_Line + New_Index - 1)
               then 10
               else -1)
            then
               --  both has lines
               if New_Lines.Element (New_First_Line + New_Index - 1) =
                 Old_Lines.Element (Old_First_Line + Old_Index - 1)
               then
                  --  lines are equal, add Text_Edit after current line
                  --  if any is already prepared
                  Add (Old_Natural);
               else
                  --  lines are different, change old line by new one,
                  --  we deleted whole line so 'To' position will be
                  --  the beginning of the next line
                  Prepare
                    (Old_Natural,
                     New_Lines.Element (New_First_Line + New_Index - 1));
               end if;

               --  move lines cursor backward
               Old_Natural := Old_Natural - 1;

               New_Index := New_Index - 1;
               Old_Index := Old_Index - 1;

            elsif LCS (Old_Index, New_Index) =
              LCS (Old_Index - 1, New_Index) - 5
            then
               --  line has been deleted, move lines cursor backward
               Prepare (Old_Natural, VSS.Strings.Empty_Virtual_String);

               Old_Natural := Old_Natural - 1;
               Old_Index       := Old_Index - 1;

            elsif LCS (Old_Index, New_Index) =
              LCS (Old_Index, New_Index - 1) - 5
            then
               --  line has been inserted
               --  insert Text_Edit information with insertion after
               --  current line, do not move lines cursor because it is
               --  additional line not present in the old document
               Prepare
                 (Old_Natural,
                  New_Lines.Element (New_First_Line + New_Index - 1));

               New_Index := New_Index - 1;
            end if;
         end loop;

         while Old_Index > 0 loop
            --  deleted
            Prepare (Old_Natural, VSS.Strings.Empty_Virtual_String);

            Old_Natural := Old_Natural - 1;
            Old_Index       := Old_Index - 1;
         end loop;

         while New_Index > 0 loop
            --  inserted
            Prepare
              (Old_Natural,
               New_Lines.Element (New_First_Line + New_Index - 1));

            New_Index := New_Index - 1;
         end loop;

         Add (Old_Natural);
         Free (LCS);

         --  Handle the edge case where the last location of
         --  the edit is trying to affect a non existent line.
         --  The edits are ordered so we only need to check the last one.

         if not Edit.Is_Empty
            and then not Self.Line_To_Marker.Is_Empty
            and then Edit.Last_Element.a_range.an_end.line not in
              Self.Line_To_Marker.First_Index .. Self.Line_To_Marker.Last_Index
         then
            declare
               use type VSS.Unicode.UTF16_Code_Unit_Offset;

               Element   : LSP.Structures.TextEdit := Edit.Last_Element;
               Last_Line : constant VSS.Strings.Virtual_String :=
                 Old_Lines (Old_Lines.Length);
               Iterator  :
                 constant VSS.Strings.Character_Iterators.Character_Iterator :=
                   Last_Line.At_Last_Character;

            begin
               --  Replace the wrong location by the end of the buffer
               Element.a_range.an_end :=
                 (line      => Natural (Old_Lines.Length) - 1,
                  character => Natural (Iterator.Last_UTF16_Offset) + 1);
               Edit.Replace_Element (Edit.Last, Element);
            end;
         end if;

      exception
         when others =>
            Free (LCS);
            raise;
      end;
   end Diff;

   ------------------
   -- Diff_Symbols --
   ------------------

   procedure Diff_Symbols
     (Self     : Document;
      Span     : LSP.Structures.A_Range;
      New_Text : VSS.Strings.Virtual_String;
      Edit     : in out LSP.Structures.TextEdit_Vector)
   is
      use VSS.Strings;
      use VSS.Characters;

      Old_Text  : VSS.Strings.Virtual_String;
      Old_Lines : VSS.String_Vectors.Virtual_String_Vector;
      Old_Line  : VSS.Strings.Virtual_String;
      Old_Length, New_Length : Natural;

      First_Marker : VSS.Strings.Markers.Character_Marker;
      Last_Marker  : VSS.Strings.Markers.Character_Marker;

   begin
      Self.Span_To_Markers (Span, First_Marker, Last_Marker);

      Old_Text  := Self.Text.Slice (First_Marker, Last_Marker);
      Old_Lines := Old_Text.Split_Lines
        (Terminators     => LSP_New_Line_Function_Set,
         Keep_Terminator => True);
      Old_Line := Old_Lines.Element (Old_Lines.Length);

      Old_Length := Integer (Character_Length (Old_Text));
      New_Length := Integer (Character_Length (New_Text));

      declare
         type LCS_Array is array
           (Natural range 0 .. Old_Length,
            Natural range 0 .. New_Length) of Integer;
         type LCS_Array_Access is access all LCS_Array;

         procedure Free is
           new Ada.Unchecked_Deallocation (LCS_Array, LCS_Array_Access);

         LCS    : LCS_Array_Access := new LCS_Array;
         Match  : Integer;
         Delete : Integer;
         Insert : Integer;

         Old_Char : VSS.Strings.Character_Iterators.Character_Iterator :=
           Old_Text.At_First_Character;

         New_Char : VSS.Strings.Character_Iterators.Character_Iterator :=
           New_Text.At_First_Character;

         Dummy : Boolean;

         Old_Index, New_Index : Integer;

         Changed_Block_Text : VSS.Strings.Virtual_String;
         Changed_Block_Span : LSP.Structures.A_Range := ((0, 0), (0, 0));
         Span_Set           : Boolean := False;

         --  to calculate span
         Current_Natural : Natural :=
           (if Natural (Span.an_end.character) = 0
            then Span.an_end.line - 1
            else Span.an_end.line);
         --  we do not have a line at all when the range end is on the
         --  begin of a line, so set Current_Natural to the previous one
         Old_Lines_Number    : Natural := Old_Lines.Length;

         Cursor : VSS.Strings.Character_Iterators.Character_Iterator :=
           Old_Line.After_Last_Character;

         procedure Backward;
         --  Move old line Cursor backward, update Old_Line and
         --  Old_Lines_Number if needed

         function Get_Position
           (Insert : Boolean) return LSP.Structures.Position;
         --  get Position for a Span based on Cursor to prepare first/last
         --  position for changes

         procedure Prepare_Last_Span (Insert : Boolean);
         --  Store position based on Cursor to Changed_Block_Span.an_end if
         --  it is not stored yet

         procedure Prepare_Change
           (Insert : Boolean;
            Char   : VSS.Characters.Virtual_Character);
         --  Collect change information for Text_Edit in Changed_Block_Text
         --  and Changed_Block_Span

         procedure Add_Prepared_Change;
         --  Add prepared New_String and corresponding Span into Text_Edit

         --------------
         -- Backward --
         --------------

         procedure Backward is
         begin
            if not Cursor.Backward
              and then Old_Lines_Number > 1
            then
               Current_Natural := Current_Natural - 1;
               Old_Lines_Number    := Old_Lines_Number - 1;
               Old_Line            := Old_Lines.Element (Old_Lines_Number);
               Cursor.Set_At_Last (Old_Line);
            end if;

            Old_Index := Old_Index - 1;
            Dummy     := Old_Char.Backward;
         end Backward;

         ------------------
         -- Get_Position --
         ------------------

         function Get_Position
           (Insert : Boolean) return LSP.Structures.Position
         is
            --------------
            -- Backward --
            --------------

            function Backward return LSP.Structures.Position;

            function Backward return LSP.Structures.Position is
               C : VSS.Strings.Character_Iterators.Character_Iterator :=
                 Old_Line.At_Character (Cursor);
            begin
               --  "Cursor" is after the current character but we should
               --  insert before it
               if C.Backward then
                  return
                    (line      => Current_Natural,
                     character => Natural (C.First_UTF16_Offset));
               else
                  return
                    (line      => Current_Natural,
                     character => 0);
               end if;
            end Backward;

         begin
            if not Cursor.Has_Element then
               return
                 (line      => Current_Natural,
                  character => 0);

            elsif Insert then
               --  "Cursor" is after the current character but we should
               --  insert before it
               return Backward;

            else
               return
                 (line      => Current_Natural,
                  character => Natural (Cursor.First_UTF16_Offset));
            end if;
         end Get_Position;

         -----------------------
         -- Prepare_Last_Span --
         -----------------------

         procedure Prepare_Last_Span (Insert : Boolean) is
         begin
            if not Span_Set then
               --  it is the first portion of a changed block so store
               --  last position of the changes
               Span_Set := True;
               Changed_Block_Span.an_end := Get_Position (Insert);
            end if;
         end Prepare_Last_Span;

         --------------------
         -- Prepare_Change --
         --------------------

         procedure Prepare_Change
           (Insert : Boolean;
            Char   : VSS.Characters.Virtual_Character) is
         begin
            Prepare_Last_Span (Insert);
            --  accumulating new text for the changed block
            Changed_Block_Text.Prepend (Char);
         end Prepare_Change;

         -------------------------
         -- Add_Prepared_Change --
         -------------------------

         procedure Add_Prepared_Change is
         begin
            if not Span_Set then
               --  No information for Text_Edit
               return;
            end if;

            Changed_Block_Span.start := Get_Position (False);

            Edit.Prepend
              (LSP.Structures.TextEdit'
                 (a_range => Changed_Block_Span,
                  newText => Changed_Block_Text));

            --  clearing
            Changed_Block_Text.Clear;

            Changed_Block_Span := ((0, 0), (0, 0));
            Span_Set := False;
         end Add_Prepared_Change;

      begin
         --  prepare LCS

         --  default values for line 0
         for Index in 0 .. Old_Length loop
            LCS (Index, 0) := -5 * Index;
         end loop;

         --  default values for the first column
         for Index in 0 .. New_Length loop
            LCS (0, Index) := -5 * Index;
         end loop;

         --  calculate LCS
         for Row in 1 .. Old_Length loop
            New_Char.Set_At_First (New_Text);
            for Column in 1 .. New_Length loop
               Match := LCS (Row - 1, Column - 1) +
                 (if Old_Char.Element = New_Char.Element
                  then 10   --  +10 is the 'weight' for equal lines
                  else -1); --  and -1 for the different

               Delete := LCS (Row - 1, Column) - 5;
               Insert := LCS (Row, Column - 1) - 5;

               LCS (Row, Column) := Integer'Max (Match, Insert);
               LCS (Row, Column) := Integer'Max (LCS (Row, Column), Delete);

               Dummy := New_Char.Forward;
            end loop;
            Dummy := Old_Char.Forward;
         end loop;

         --  iterate over LCS and create Text_Edit

         Old_Char.Set_At_Last (Old_Text);
         New_Char.Set_At_Last (New_Text);
         Old_Index := Old_Length;
         New_Index := New_Length;

         while Old_Index > 0
           and then New_Index > 0
         loop
            if LCS (Old_Index, New_Index) =
              LCS (Old_Index - 1, New_Index - 1) +
              (if Old_Char.Element = New_Char.Element
               then 10
               else -1)
            then
               --  both has elements
               if Old_Char.Element = New_Char.Element then
                  --  elements are equal, add prepared Text_Edit
                  Add_Prepared_Change;
               else
                  --  elements are different, change old one by new
                  Prepare_Change (False, New_Char.Element);
               end if;

               --  move old element cursors backward
               Backward;

               New_Index := New_Index - 1;
               Dummy     := New_Char.Backward;

            elsif LCS (Old_Index, New_Index) =
              LCS (Old_Index - 1, New_Index) - 5
            then
               --  element has been deleted, move old cursor backward
               Prepare_Last_Span (False);
               Backward;

            elsif LCS (Old_Index, New_Index) =
              LCS (Old_Index, New_Index - 1) - 5
            then
               --  element has been inserted
               Prepare_Change (True, New_Char.Element);

               New_Index := New_Index - 1;
               Dummy     := New_Char.Backward;
            end if;
         end loop;

         while Old_Index > 0 loop
            --  deleted
            Prepare_Last_Span (False);
            Backward;
         end loop;

         while New_Index > 0 loop
            --  inserted
            Prepare_Change (True, New_Char.Element);

            New_Index := New_Index - 1;
            Dummy     := New_Char.Backward;
         end loop;

         Add_Prepared_Change;
         Free (LCS);

      exception
         when others =>
            Free (LCS);
            raise;
      end;
   end Diff_Symbols;

   -------------------------
   -- Find_All_References --
   -------------------------

   procedure Find_All_References
     (Self       : Document; Context : LSP.Ada_Contexts.Context;
      Definition : Libadalang.Analysis.Defining_Name;
      Callback   : not null access procedure
        (Base_Id : Libadalang.Analysis.Base_Id;
         Kind    : Libadalang.Common.Ref_Result_Kind; Cancel : in out Boolean))
   is
      Units : constant Libadalang.Analysis.Analysis_Unit_Array :=
        (1 =>  LSP.Ada_Documents.Unit (Self    => Self,
                                       Context => Context));
   begin
      LSP.Ada_Id_Iterators.Find_All_References (Definition, Units, Callback);
   exception
      when E : Libadalang.Common.Property_Error =>
         Self.Tracer.Trace_Exception (E, "in Find_All_References");
   end Find_All_References;

   ----------------
   -- Formatting --
   ----------------

   function Formatting
     (Self     : Document;
      Context  : LSP.Ada_Contexts.Context;
      Span     : LSP.Structures.A_Range;
      Cmd      : Pp.Command_Lines.Cmd_Line;
      Edit     : out LSP.Structures.TextEdit_Vector;
      Messages : out VSS.String_Vectors.Virtual_String_Vector) return Boolean
   is
      use type Libadalang.Slocs.Source_Location_Range;
      use type LSP.Structures.A_Range;

      Sloc        : constant Libadalang.Slocs.Source_Location_Range :=
        (if Span = LSP.Constants.Empty
         then Libadalang.Slocs.No_Source_Location_Range
         else Libadalang.Slocs.Make_Range
                (Self.Get_Source_Location (Span.start),
                 Self.Get_Source_Location (Span.an_end)));

      Input       : Utils.Char_Vectors.Char_Vector;
      Output      : Utils.Char_Vectors.Char_Vector;
      Out_Span    : LSP.Structures.A_Range;
      PP_Messages : Pp.Scanner.Source_Message_Vector;
      Out_Sloc    : Libadalang.Slocs.Source_Location_Range;
      S           : GNAT.Strings.String_Access;

   begin
      if Span /= LSP.Constants.Empty then
         --  Align Span to line bounds

         if Span.start.character /= 0 then
            return Self.Formatting
              (Context  => Context,
               Span     => ((Span.start.line, 0), Span.an_end),
               Cmd      => Cmd,
               Edit     => Edit,
               Messages => Messages);

         elsif Span.an_end.character /= 0 then
            return Self.Formatting
              (Context  => Context,
               Span     => (Span.start, (Span.an_end.line + 1, 0)),
               Cmd      => Cmd,
               Edit     => Edit,
               Messages => Messages);
         end if;
      end if;

      S := new String'(VSS.Strings.Conversions.To_UTF_8_String (Self.Text));
      Input.Append (S.all);
      GNAT.Strings.Free (S);

      LSP.Utils.Format_Vector
        (Cmd       => Cmd,
         Input     => Input,
         Node      => Self.Unit (Context).Root,
         In_Sloc   => Sloc,
         Output    => Output,
         Out_Sloc  => Out_Sloc,
         Messages  => PP_Messages);

      --  Properly format the messages received from gnatpp, using the
      --  the GNAT standard way for messages (i.e: <filename>:<sloc>: <msg>)

      if not PP_Messages.Is_Empty then
         declare
            File     : constant GNATCOLL.VFS.Virtual_File :=
              Context.URI_To_File (Self.URI);
            Template : constant VSS.Strings.Templates.Virtual_String_Template :=
              "{}:{}:{}: {}";

         begin
            for Error of PP_Messages loop
               Messages.Append
                 (Template.Format
                    (VSS.Strings.Formatters.Strings.Image
                         (VSS.Strings.Conversions.To_Virtual_String
                              (File.Display_Base_Name)),
                     VSS.Strings.Formatters.Integers.Image (Error.Sloc.Line),
                     VSS.Strings.Formatters.Integers.Image (Error.Sloc.Col),
                     VSS.Strings.Formatters.Strings.Image
                       (VSS.Strings.Conversions.To_Virtual_String
                            (String
                                 (Utils.Char_Vectors.Char_Vectors.To_Array
                                    (Error.Text))))));
            end loop;

            return False;
         end;
      end if;

      S := new String'(Output.To_Array);

      if Lal_PP_Output.Is_Active then
         Lal_PP_Output.Trace (S.all);
      end if;

      if Span = LSP.Constants.Empty then
         --  diff for the whole document

         Diff
           (Self,
            VSS.Strings.Conversions.To_Virtual_String (S.all),
            Edit => Edit);

      elsif Out_Sloc = Libadalang.Slocs.No_Source_Location_Range then
         --  Range formating fails. Do nothing, skip formating altogether

         null;

      else
         --  diff for a part of the document

         Out_Span := Self.To_LSP_Range (Out_Sloc);

         --  Use line diff if the range is too wide

         if Span.an_end.line - Span.start.line > 5 then
            Diff
              (Self,
               VSS.Strings.Conversions.To_Virtual_String (S.all),
               Span,
               Out_Span,
               Edit);

         else
            declare
               Formatted : constant VSS.Strings.Virtual_String :=
                 VSS.Strings.Conversions.To_Virtual_String (S.all);
               Slice     : VSS.Strings.Virtual_String;

            begin
               LSP.Utils.Span_To_Slice (Formatted, Out_Span, Slice);

               Diff_Symbols
                 (Self,
                  Span,
                  Slice,
                  Edit);
            end;
         end if;
      end if;

      GNAT.Strings.Free (S);

      return True;

   exception
      when E : others =>
         Lal_PP_Output.Trace (E);
         GNAT.Strings.Free (S);

         return False;
   end Formatting;

   --------------------
   -- Get_Any_Symbol --
   --------------------

   procedure Get_Any_Symbol
     (Self        : in out Document; Context : LSP.Ada_Contexts.Context;
      Pattern     : LSP.Search.Search_Pattern'Class;
      Limit       : Ada.Containers.Count_Type;
      Only_Public : Boolean;
      Canceled    : access function return Boolean;
      Result      : in out LSP.Ada_Completions.Completion_Maps.Map)
   is

      procedure Refresh_Symbol_Cache;
      --  Find intresting definings names in the document and put them
      --  into Self.Symbol_Cache

      procedure Insert
        (Item : Name_Information;
         Name : Libadalang.Analysis.Defining_Name);
      --  Populate Result with the name information if Result doesn't have
      --  the Name already

      function Get_Defining_Name
        (Loc : Langkit_Support.Slocs.Source_Location)
         return Libadalang.Analysis.Defining_Name;

      -----------------------
      -- Get_Defining_Name --
      -----------------------

      function Get_Defining_Name
        (Loc : Langkit_Support.Slocs.Source_Location)
         return Libadalang.Analysis.Defining_Name
      is
         Unit : constant Libadalang.Analysis.Analysis_Unit :=
             Self.Unit (Context);

         Name : constant Libadalang.Analysis.Name :=
           Laltools.Common.Get_Node_As_Name (Unit.Root.Lookup (Loc));
      begin
         return Laltools.Common.Get_Name_As_Defining (Name);
      end Get_Defining_Name;

      ------------
      -- Insert --
      ------------

      procedure Insert
        (Item : Name_Information;
         Name : Libadalang.Analysis.Defining_Name) is
      begin
         if not Result.Contains (Name) and then
           (not Only_Public or else Item.Is_Public)
         then
            Result.Insert
              (Name,
               (Is_Dot_Call  => False,
                Is_Visible   => False,
                Use_Snippets => False,
                Pos          => <>,
                Weight       => <>));
         end if;
      end Insert;

      --------------------------
      -- Refresh_Symbol_Cache --
      --------------------------

      procedure Refresh_Symbol_Cache is
         use Langkit_Support.Symbols;
         use Libadalang.Common;
         use Libadalang.Iterators;

         Node : Libadalang.Analysis.Ada_Node;

         Global_Visible : constant Libadalang.Iterators.Ada_Node_Predicate :=
           LSP.Predicates.Is_Global_Visible;

         Restricted_Kind : constant Libadalang.Iterators.Ada_Node_Predicate :=
           LSP.Predicates.Is_Restricted_Kind;

         --  Find all definings names excluding private parts and bodies
         It : Libadalang.Iterators.Traverse_Iterator'Class :=
           Libadalang.Iterators.Find
             (Self.Unit (Context).Root,
              Libadalang.Iterators.Kind_Is (Ada_Defining_Name)
                and not Restricted_Kind);

      begin
         while It.Next (Node) loop
            declare
               Token     : constant Token_Reference := Node.Token_End;
               Text      : constant Langkit_Support.Text.Text_Type :=
                 Libadalang.Common.Text (Token);
               Canonical : constant Symbolization_Result :=
                 Libadalang.Sources.Canonicalize (Text);
               Cursor    : Symbol_Maps.Cursor;
               Inserted  : Boolean;

            begin
               if Canonical.Success then
                  Self.Symbol_Cache.Insert
                    (VSS.Strings.To_Virtual_String (Canonical.Symbol),
                     Name_Vectors.Empty_Vector,
                     Cursor,
                     Inserted);

                  Self.Symbol_Cache (Cursor).Append
                    (Name_Information'
                       (Langkit_Support.Slocs.Start_Sloc (Node.Sloc_Range),
                        Global_Visible.Unchecked_Get.Evaluate (Node)));
               end if;
            end;
         end loop;
      end Refresh_Symbol_Cache;

      Cursor      : Symbol_Maps.Cursor;

      use type LSP.Search.Search_Kind;

      --  In "Celling" mode we scan only range of cache where a key prefix
      --  matches lowercased pattern as is.
      Use_Celling : constant Boolean :=
        not Pattern.Get_Negate
        and then ((Pattern.Get_Kind = LSP.Search.Full_Text
                   and then Pattern.Get_Whole_Word)
                  or else Pattern.Get_Kind = LSP.Search.Start_Word_Text);

   begin
      if Self.Refresh_Symbol_Cache then
         Refresh_Symbol_Cache;
         Self.Refresh_Symbol_Cache := False;
      end if;

      if Use_Celling then
         Cursor := Self.Symbol_Cache.Ceiling (Pattern.Get_Canonical_Pattern);
      else
         Cursor := Self.Symbol_Cache.First;
      end if;

      while Symbol_Maps.Has_Element (Cursor) loop

         if Use_Celling
           and then not Pattern.Match (Symbol_Maps.Key (Cursor))
         then
            --  We use "Celling mode" and key stops matching,
            --  Symbol_Cache is ordered so we will not find any
            --  matches more

            exit when Use_Celling or else Canceled.all;

         else

            for Item of Self.Symbol_Cache (Cursor) loop
               declare
                  Defining_Name : constant Libadalang.Analysis.Defining_Name :=
                    Get_Defining_Name (Item.Loc);
               begin
                  --  Match each element individually in case of sensitive
                  --  search or non-celling mode
                  if not Defining_Name.Is_Null
                    and then
                      ((Use_Celling
                        and then not Pattern.Get_Case_Sensitive)
                       or else Pattern.Match
                         (VSS.Strings.To_Virtual_String
                            (Defining_Name.As_Ada_Node.Text)))
                  then
                     Insert (Item, Defining_Name);
                  end if;

                  exit when Canceled.all;

               end;
            end loop;

         end if;

         Symbol_Maps.Next (Cursor);
      end loop;
   end Get_Any_Symbol;

   -------------------------
   -- Get_Completion_Node --
   -------------------------

   procedure Get_Completion_Node
     (Self     : Document;
      Context  : LSP.Ada_Contexts.Context;
      Position : LSP.Structures.Position;
      Sloc     : out Langkit_Support.Slocs.Source_Location;
      Token    : out Libadalang.Common.Token_Reference;
      Node     : out Libadalang.Analysis.Ada_Node)
   is
      use Libadalang.Common;

      function Completion_Token
        (Sloc  : Langkit_Support.Slocs.Source_Location)
         return Libadalang.Common.Token_Reference;
      --  Get token under completion for given cursor position.
      --  If cursor at the first symbol of a token return previous token:
      --  XXX___
      --     ^ cursor just after a token mean user is completion XXX token.

      ----------------------
      -- Completion_Token --
      ----------------------

      function Completion_Token
        (Sloc  : Langkit_Support.Slocs.Source_Location)
         return Libadalang.Common.Token_Reference
      is
         use type Langkit_Support.Slocs.Source_Location;

         Token : constant Libadalang.Common.Token_Reference :=
           Self.Get_Token_At (Context, Position);

         Prev  : constant Libadalang.Common.Token_Reference :=
           (if Token = Libadalang.Common.No_Token
            then Token
            else Libadalang.Common.Previous (Token));

      begin
         if Libadalang.Common.No_Token not in Token | Prev then
            declare
               Data  : constant Libadalang.Common.Token_Data_Type :=
                 Libadalang.Common.Data (Token);

               Start : constant Langkit_Support.Slocs.Source_Location :=
                 Langkit_Support.Slocs.Start_Sloc
                   (Libadalang.Common.Sloc_Range (Data));
            begin
               if Start = Sloc then
                  return Prev;
               end if;
            end;
         end if;

         return Token;
      end Completion_Token;
   begin
      Sloc := Self.Get_Source_Location (Position);
      Token := Completion_Token (Sloc);
      declare
         From : constant Langkit_Support.Slocs.Source_Location :=
           Langkit_Support.Slocs.Start_Sloc
             (Libadalang.Common.Sloc_Range
                (Libadalang.Common.Data (Token)));

         Root : constant Libadalang.Analysis.Ada_Node :=
           Self.Unit (Context).Root;
      begin
         Node := (if Root.Is_Null then Root else Root.Lookup (From));
      end;
   end Get_Completion_Node;

   ------------------------
   -- Get_Completions_At --
   ------------------------

   procedure Get_Completions_At
     (Self      : Document;
      Providers : LSP.Ada_Completions.Completion_Provider_List;
      Context   : LSP.Ada_Contexts.Context;
      Sloc      : Langkit_Support.Slocs.Source_Location;
      Token     : Libadalang.Common.Token_Reference;
      Node      : Libadalang.Analysis.Ada_Node;
      Names     : out Ada_Completions.Completion_Maps.Map;
      Result    : out LSP.Structures.CompletionList)
   is
      Parent : constant Libadalang.Analysis.Ada_Node :=
        (if Node.Is_Null then Node else Node.Parent);

      Filter : LSP.Ada_Completions.Filters.Filter;
   begin
      if not Parent.Is_Null
        and then (Parent.Kind not in
          Libadalang.Common.Ada_Dotted_Name | Libadalang.Common.Ada_End_Name
          and then Node.Kind in Libadalang.Common.Ada_String_Literal_Range)
      then
         --  Do nothing when inside a string
         return;
      end if;

      Self.Tracer.Trace
        ("Getting completions, Pos = ("
         & Sloc.Line'Image & ", " & Sloc.Column'Image & ") Node = "
         & Libadalang.Analysis.Image (Node));

      Filter.Initialize (Token, Node);

      for Provider of Providers loop
         begin
            Provider.Propose_Completion
              (Sloc   => Sloc,
               Token  => Token,
               Node   => Node,
               Filter => Filter,
               Names  => Names,
               Result => Result);

         exception
            when E : Libadalang.Common.Property_Error =>
               Self.Tracer.Trace_Exception
                 (E,
                  "LAL EXCEPTION occurred with following completion provider");
               Self.Tracer.Trace (Ada.Tags.Expanded_Name (Provider'Tag));
         end;
      end loop;

      Self.Tracer.Trace
        ("Number of filtered completions : " & Names.Length'Image);
   end Get_Completions_At;

   ----------------
   -- Get_Errors --
   ----------------

   procedure Get_Errors
     (Self    : in out Document;
      Context : LSP.Ada_Contexts.Context;
      Changed : out Boolean;
      Errors  : out LSP.Structures.Diagnostic_Vector;
      Force   : Boolean := False)
   is
   begin
      Errors.Clear;
      Changed := (for some Source of Self.Diagnostic_Sources =>
                    Source.Has_New_Diagnostic (Context));

      if Changed or else Force then
         for Source of Self.Diagnostic_Sources loop
            Source.Get_Diagnostic (Context, Errors);
         end loop;
      end if;
   end Get_Errors;

   ------------------------
   -- Get_Folding_Blocks --
   ------------------------

   procedure Get_Folding_Blocks
     (Self       : Document;
      Context    : LSP.Ada_Contexts.Context;
      Lines_Only : Boolean;
      Comments   : Boolean;
      Canceled   : access function return Boolean;
      Result     : out LSP.Structures.FoldingRange_Vector)
   is
      use Libadalang.Common;
      use Libadalang.Analysis;

      Location     : LSP.Structures.Location;
      foldingRange : LSP.Structures.FoldingRange;
      Have_With    : Boolean := False;

      function Parse (Node : Ada_Node'Class) return Visit_Status;
      --  Includes Node location to the result if the node has "proper" kind

      procedure Store_Span (Span : LSP.Structures.A_Range);
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
         if Canceled.all then
            return Stop;
         end if;

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
--        Cat_Package

                 Ada_Type_Decl |

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
                 (Is_Set => True, Value => LSP.Enumerations.Region);

               Location := Self.To_LSP_Location (Node.Sloc_Range);
               Store_Span (Location.a_range);

            when Ada_With_Clause |
                 Ada_Use_Package_Clause |
                 Ada_Use_Type_Clause =>

               Location := Self.To_LSP_Location (Node.Sloc_Range);

               if not Have_With then
                  Have_With := True;

                  foldingRange.kind :=
                    (Is_Set => True, Value => LSP.Enumerations.Imports);

                  foldingRange.startLine := Location.a_range.start.line;
               end if;

               foldingRange.endLine := Location.a_range.an_end.line;

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

      procedure Store_Span (Span : LSP.Structures.A_Range) is
      begin
         if not Lines_Only
           or else Span.start.line /= Span.an_end.line
         then
            foldingRange.startLine := Span.start.line;
            foldingRange.endLine   := Span.an_end.line;

            if not Lines_Only then
               foldingRange.startCharacter :=
                 (Is_Set => True,
                  Value  => Span.start.character);

               foldingRange.startCharacter :=
                 (Is_Set => True,
                  Value  => Span.an_end.character);
            end if;

            Result.Append (foldingRange);
         end if;
      end Store_Span;

      Token : Token_Reference;
      Span  : LSP.Structures.A_Range;

   begin
      Traverse (Self.Unit (Context).Root, Parse'Access);

      if not Comments then
         --  do not process comments
         return;
      end if;

      --  Looking for comments
      foldingRange.kind := (Is_Set => False);
      Token             := First_Token (Self.Unit (Context));

      while Token /= No_Token
        and then not Canceled.all
      loop
         case Kind (Data (Token)) is
            when Ada_Comment =>
               if not foldingRange.kind.Is_Set then
                  foldingRange.kind :=
                    (Is_Set => True, Value => LSP.Enumerations.Comment);
                  Span := Self.To_LSP_Range (Sloc_Range (Data (Token)));
               else
                  Span.an_end :=
                    Self.To_LSP_Range (Sloc_Range (Data (Token))).an_end;
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

   ---------------------------
   -- Get_Formatting_Region --
   ---------------------------

   function Get_Formatting_Region
     (Self     : Document;
      Context  : LSP.Ada_Contexts.Context;
      Position : LSP.Structures.Position)
      return Laltools.Partial_GNATPP.Formatting_Region_Type
   is (Laltools.Partial_GNATPP.Get_Formatting_Region
        (Unit        => Self.Unit (Context),
         Input_Range =>
           Langkit_Support.Slocs.Make_Range
             (Self.Get_Source_Location (Position),
              Self.Get_Source_Location (Position))));

   ---------------------
   -- Get_Indentation --
   ---------------------

   function Get_Indentation
     (Self    : Document;
      Context : LSP.Ada_Contexts.Context;
      Line    : Positive)
      return VSS.Strings.Character_Count
   is
     (VSS.Strings.Character_Count
        (Laltools.Partial_GNATPP.Estimate_Indentation
             (Self.Unit (Context),
              Self.Get_Source_Location ((Line, 1)).Line)));

   -----------------
   -- Get_Node_At --
   -----------------

   function Get_Node_At
     (Self     : Document;
      Context  : LSP.Ada_Contexts.Context;
      Position : LSP.Structures.Position) return Libadalang.Analysis.Ada_Node
   is
      Unit : constant Libadalang.Analysis.Analysis_Unit := Self.Unit (Context);
   begin
      return (if Unit.Root.Is_Null then Libadalang.Analysis.No_Ada_Node
              else Unit.Root.Lookup (Self.Get_Source_Location (Position)));
   end Get_Node_At;

   -------------------------
   -- Get_Source_Location --
   -------------------------

   function Get_Source_Location
     (Self : Document'Class; Position : LSP.Structures.Position)
      return Langkit_Support.Slocs.Source_Location
   is
      use type VSS.Unicode.UTF16_Code_Unit_Offset;
      use type VSS.Strings.Character_Index;

      Iterator : VSS.Strings.Character_Iterators.Character_Iterator :=
        Self.Text.At_Character (Self.Line_To_Marker (Position.line));

      Line_Offset : constant VSS.Unicode.UTF16_Code_Unit_Offset :=
        Iterator.First_UTF16_Offset;

      Line_First_Character : constant VSS.Strings.Character_Index :=
        Iterator.Character_Index;
   begin
      while Integer (Iterator.First_UTF16_Offset - Line_Offset)
               <= Position.character
        and then Iterator.Forward
      loop
         null;
      end loop;

      return ((Line   => Langkit_Support.Slocs.Line_Number (Position.line + 1),
               Column => Langkit_Support.Slocs.Column_Number
                 (Iterator.Character_Index - Line_First_Character)));
   end Get_Source_Location;

   --------------------------
   -- Get_Symbol_Hierarchy --
   --------------------------

   procedure Get_Symbol_Hierarchy
     (Self     :     Document; Context : LSP.Ada_Contexts.Context;
      Pattern  :     LSP.Search.Search_Pattern'Class;
      Canceled :     access function return Boolean;
      Result   : out LSP.Structures.DocumentSymbol_Vector)
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Get_Symbol_Hierarchy unimplemented");
      raise Program_Error with "Unimplemented procedure Get_Symbol_Hierarchy";
   end Get_Symbol_Hierarchy;

   -----------------
   -- Get_Symbols --
   -----------------

   procedure Get_Symbols
     (Self     :     Document; Context : LSP.Ada_Contexts.Context;
      Pattern  :     LSP.Search.Search_Pattern'Class;
      Canceled :     access function return Boolean;
      Result   : out LSP.Structures.DocumentSymbol_Vector)
   is
   begin
      pragma Compile_Time_Warning (Standard.True, "Get_Symbols unimplemented");
      raise Program_Error with "Unimplemented procedure Get_Symbols";
   end Get_Symbols;

   -----------------
   -- Get_Text_At --
   -----------------

   function Get_Text_At
     (Self    : Document; Start_Pos : LSP.Structures.Position;
      End_Pos : LSP.Structures.Position) return VSS.Strings.Virtual_String
   is
      First_Marker : VSS.Strings.Markers.Character_Marker;
      Last_Marker  : VSS.Strings.Markers.Character_Marker;

   begin
      Self.Span_To_Markers
        ((Start_Pos, End_Pos), First_Marker, Last_Marker);

      return Self.Text.Slice (First_Marker, Last_Marker);
   end Get_Text_At;

   ------------------
   -- Get_Token_At --
   ------------------

   function Get_Token_At
     (Self     : Document'Class; Context : LSP.Ada_Contexts.Context;
      Position : LSP.Structures.Position)
      return Libadalang.Common.Token_Reference
   is
     (Self.Unit (Context).Lookup_Token (Self.Get_Source_Location (Position)));

   ----------------
   -- Get_Tokens --
   ----------------

   function Get_Tokens
     (Self        : Document'Class; Context : LSP.Ada_Contexts.Context;
      Highlighter : LSP.Ada_Highlighters.Ada_Highlighter;
      Span        : LSP.Structures.A_Range := ((1, 1), (0, 0)))
      return LSP.Structures.Natural_Vector
   is
      (Highlighter.Get_Tokens (Self.Unit (Context), Context.Tracer.all, Span));

   -----------------
   -- Get_Word_At --
   -----------------

   function Get_Word_At
     (Self     : Document;
      Context  : LSP.Ada_Contexts.Context;
      Position : LSP.Structures.Position) return VSS.Strings.Virtual_String
   is
      use Langkit_Support.Slocs;
      use all type Libadalang.Common.Token_Kind;

      Result : VSS.Strings.Virtual_String;

      Unit : constant Libadalang.Analysis.Analysis_Unit :=
        Self.Unit (Context);

      Origin : constant Source_Location := Self.Get_Source_Location (Position);
      Where : constant Source_Location := (Origin.Line, Origin.Column - 1);
      --  Compute the position we want for completion, which is one character
      --  before the cursor.

      Token : constant Libadalang.Common.Token_Reference :=
        Unit.Lookup_Token (Where);

      Data : constant Libadalang.Common.Token_Data_Type :=
        Libadalang.Common.Data (Token);

      Kind : constant Libadalang.Common.Token_Kind :=
        Libadalang.Common.Kind (Data);

      Text : constant Langkit_Support.Text.Text_Type :=
        Libadalang.Common.Text (Token);

      Sloc : constant Source_Location_Range :=
        Libadalang.Common.Sloc_Range (Data);

      Span : constant Integer :=
        Natural (Where.Column) - Natural (Sloc.Start_Column);

   begin
      if Kind in Ada_Identifier .. Ada_Xor
        and then Compare (Sloc, Where) = Inside
      then
         Result := VSS.Strings.To_Virtual_String
           (Text (Text'First .. Text'First + Span));
      end if;

      return Result;
   end Get_Word_At;

   ---------------------
   -- Has_Diagnostics --
   ---------------------

   function Has_Diagnostics
     (Self : Document; Context : LSP.Ada_Contexts.Context) return Boolean
   is
      (Self.Unit (Context).Has_Diagnostics);

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self       : in out Document;
      URI        : LSP.Structures.DocumentUri;
      Text       : VSS.Strings.Virtual_String;
      Diagnostic : LSP.Diagnostic_Sources.Diagnostic_Source_Access)
   is
   begin
      Self.URI  := URI;
      Self.Version := 1;
      Self.Text := Text;
      Self.Refresh_Symbol_Cache := True;
      Self.Diagnostic_Sources (1) := new
        LSP.Ada_Documents.LAL_Diagnostics.Diagnostic_Source
          (Self'Unchecked_Access);
      Self.Diagnostic_Sources (2) := Diagnostic;
      Recompute_Indexes (Self);
   end Initialize;

   ---------------------
   -- Line_Terminator --
   ---------------------

   function Line_Terminator
     (Self : Document'Class) return VSS.Strings.Virtual_String
   is
      (if Self.Line_Terminator.Is_Empty then
         --  Document has no line terminator yet, return LF as most used
         --
         --  Should it be platform specific? CRLF for Windows, CR for Mac?

          VSS.Strings.To_Virtual_String
             ((1 => Ada.Characters.Wide_Wide_Latin_1.LF))

      else
         Self.Line_Terminator);

   ----------------------
   -- Range_Formatting --
   ----------------------

   function Range_Formatting
     (Self       : Document;
      Context    : LSP.Ada_Contexts.Context;
      Span       : LSP.Structures.A_Range;
      PP_Options : Pp.Command_Lines.Cmd_Line;
      Edit       : out LSP.Structures.TextEdit_Vector;
      Messages   : out VSS.String_Vectors.Virtual_String_Vector) return Boolean
   is
      use Libadalang.Analysis;
      use Langkit_Support.Slocs;
      use Laltools.Partial_GNATPP;
      use LSP.Structures;
      use Utils.Char_Vectors;
      use Utils.Char_Vectors.Char_Vectors;

      procedure Append_PP_Messages
        (PP_Messages : Pp.Scanner.Source_Message_Vector);
      --  Append any message of PP_Messages to Messages properly formatting
      --  them using the GNAT standard way for messages
      --  (i.e: <filename>:<sloc>: <msg>)

      ------------------------
      -- Append_PP_Messages --
      ------------------------

      procedure Append_PP_Messages
        (PP_Messages : Pp.Scanner.Source_Message_Vector) is
      begin
         for Message of PP_Messages loop
            declare
               Error : LSP.Structures.DocumentUri := Self.URI;
            begin
               Error.Append (":");
               Error.Append
                 (VSS.Strings.Conversions.To_Virtual_String
                    (Pp.Scanner.Sloc_Image (Message.Sloc)));
               Error.Append (": ");
               Error.Append
                 (VSS.Strings.Conversions.To_Virtual_String
                    (String (To_Array (Message.Text))));
               Messages.Append (Error);
            end;
         end loop;
      end Append_PP_Messages;

   begin
      Self.Tracer.Trace ("On Range_Formatting");

      Self.Tracer.Trace ("Format_Selection");
      declare
         Unit                    : constant Analysis_Unit :=
           Self.Unit (Context);
         Input_Selection_Range   : constant Source_Location_Range :=
           (if Span = Empty_Range then No_Source_Location_Range
            else Make_Range
                (Self.Get_Source_Location (Span.start),
                 Self.Get_Source_Location (Span.an_end)));
         Partial_Formatting_Edit :
           constant Laltools.Partial_GNATPP.Partial_Formatting_Edit :=
             Format_Selection (Unit, Input_Selection_Range, PP_Options);

      begin
         if not Partial_Formatting_Edit.Diagnostics.Is_Empty then
            Append_PP_Messages (Partial_Formatting_Edit.Diagnostics);
            Self.Tracer.Trace
              ("Non empty diagnostics from GNATPP - "
               & "not continuing with Range_Formatting");
            return False;
         end if;

         Self.Tracer.Trace ("Computing Range_Formatting Text_Edits");
         Edit.Clear;
         declare
            Edit_Span : constant LSP.Structures.A_Range :=
              Self.To_LSP_Range (Partial_Formatting_Edit.Edit.Location);
            Edit_Text : constant VSS.Strings.Virtual_String :=
              VSS.Strings.Conversions.To_Virtual_String
                (Partial_Formatting_Edit.Edit.Text);

         begin
            Edit.Append (TextEdit'(Edit_Span, Edit_Text));
         end;

         return True;
      end;

   exception
      when E : others =>
         Self.Tracer.Trace_Exception (E, "in Range_Formatting");
         return False;
   end Range_Formatting;

   -----------------------
   -- Recompute_Indexes --
   -----------------------

   procedure Recompute_Indexes (Self : in out Document'Class) is
      use type VSS.Strings.Character_Count;

   begin
      Self.Line_To_Marker.Clear;

      --  To avoid too many reallocations during the initial filling
      --  of the index vector, pre-allocate it. Give a generous
      --  pre-allocation assuming that there is a line break every
      --  20 characters on average (this file has one line break
      --  every 33 characters).
      Self.Line_To_Marker.Reserve_Capacity
        (Ada.Containers.Count_Type (Self.Text.Character_Length / 20));

      declare
         J                    : VSS.Strings.Line_Iterators.Line_Iterator :=
           Self.Text.At_First_Line
             (Terminators     => LSP_New_Line_Function_Set,
              Keep_Terminator => True);
         Last_Line_Terminated : Boolean := False;

      begin
         if J.Has_Element then
            --  Update Line_Terminator of the document
            Self.Line_Terminator := Self.Text.Slice
              (J.Terminator_First_Marker, J.Terminator_Last_Marker);

            loop
               Self.Line_To_Marker.Append (J.First_Marker);
               Last_Line_Terminated := J.Has_Line_Terminator;

               exit when not J.Forward;
            end loop;

         else
            Last_Line_Terminated := True;
            --  Force to add one line for an empty document.
         end if;

         --  Append marker at the end of the text when the last line has line
         --  terminator sequence or text is empty. It allows to avoid checks
         --  for corner cases.

         if Last_Line_Terminated then
            Self.Line_To_Marker.Append (J.First_Marker);
         end if;
      end;
   end Recompute_Indexes;

   -----------------------
   -- Recompute_Markers --
   -----------------------

   procedure Recompute_Markers
     (Self         : in out Document'Class;
      Low_Line     : Natural;
      Start_Marker : VSS.Strings.Markers.Character_Marker;
      End_Marker   : VSS.Strings.Markers.Character_Marker)
   is
      use type VSS.Strings.Character_Count;

      M    : VSS.Strings.Markers.Character_Marker;
      J    : VSS.Strings.Line_Iterators.Line_Iterator :=
        Self.Text.At_Line
          (Position        => Start_Marker,
           Terminators     => LSP_New_Line_Function_Set,
           Keep_Terminator => True);
      Line : Natural := Low_Line;

   begin
      if J.Has_Element then
         loop
            M := J.First_Marker;

            exit
              when End_Marker.Is_Valid
                and then M.Character_Index = End_Marker.Character_Index;

            Self.Line_To_Marker.Insert (Line, M);
            Line := Line + 1;

            exit when not J.Forward;
         end loop;

         if not End_Marker.Is_Valid then
            Self.Line_To_Marker.Append (J.First_Marker);
         end if;
      end if;
   end Recompute_Markers;

   ------------------------
   -- Reset_Symbol_Cache --
   ------------------------

   procedure Reset_Symbol_Cache (Self : in out Document'Class) is
   begin
      for Item of Self.Symbol_Cache loop
         --  We clear defining name vectors, but keep symbol map in hope, that
         --  we will reuse the same elements after reindexing in
         --  Refresh_Symbol_Cache call, so we avoid memory reallocation.
         Item.Clear;
      end loop;

      Self.Refresh_Symbol_Cache := True;
   end Reset_Symbol_Cache;

   ---------------------------------------
   -- Set_Completion_Item_Documentation --
   ---------------------------------------

   procedure Set_Completion_Item_Documentation
     (Handler                 : in out LSP.Ada_Handlers.Message_Handler;
      Context                 : LSP.Ada_Contexts.Context;
      BD                      : Libadalang.Analysis.Basic_Decl;
      Item                    : in out LSP.Structures.CompletionItem;
      Compute_Doc_And_Details : Boolean)
   is
   begin
      --  Compute the 'documentation' and 'detail' fields immediately if
      --  requested (i.e: when the client does not support lazy computation
      --  for these fields or if we are dealing with predefined types).
      if Compute_Doc_And_Details or else LSP.Utils.Is_Synthetic (BD) then
         declare
            Qual_Text    : VSS.Strings.Virtual_String;
            Decl_Text    : VSS.Strings.Virtual_String;
            Loc_Text     : VSS.Strings.Virtual_String;
            Doc_Text     : VSS.Strings.Virtual_String;
            Aspects_Text : VSS.Strings.Virtual_String;

         begin
            LSP.Ada_Documentation.Get_Tooltip_Text
              (BD                 => BD,
               Style              => Context.Get_Documentation_Style,
               Declaration_Text   => Decl_Text,
               Qualifier_Text     => Qual_Text,
               Location_Text      => Loc_Text,
               Documentation_Text => Doc_Text,
               Aspects_Text       => Aspects_Text);

            Item.detail := Decl_Text;

            if not Doc_Text.Is_Empty then
               Loc_Text.Append
                 (VSS.Strings.To_Virtual_String
                    ((1 .. 2 => Ada.Characters.Wide_Wide_Latin_1.LF)));

               Loc_Text.Append (Doc_Text);
            end if;

            Item.documentation :=
              (Is_Set => True,
               Value  => LSP.Structures.Virtual_String_Or_MarkupContent'
                 (Is_Virtual_String => True,
                  Virtual_String    => Loc_Text));
         end;

      else
         --  Set node's location to the 'data' field of the completion item, so
         --  that we can retrieve it in the completionItem/resolve handler.
         LSP.Structures.LSPAny_Vectors.To_Any
           (LSP.Ada_Handlers.Locations.To_LSP_Location (Handler, BD),
            Item.data);
      end if;
   end Set_Completion_Item_Documentation;

   ---------------------
   -- Span_To_Markers --
   ---------------------

   procedure Span_To_Markers
     (Self : Document'Class;
      Span : LSP.Structures.A_Range;
      From : out VSS.Strings.Markers.Character_Marker;
      To   : out VSS.Strings.Markers.Character_Marker)
   is
      use type VSS.Unicode.UTF16_Code_Unit_Offset;

      J1 : VSS.Strings.Character_Iterators.Character_Iterator :=
        Self.Text.At_Character (Self.Line_To_Marker (Span.start.line));
      U1 : constant VSS.Unicode.UTF16_Code_Unit_Offset :=
        J1.First_UTF16_Offset;

      J2 : VSS.Strings.Character_Iterators.Character_Iterator :=
        Self.Text.At_Character (Self.Line_To_Marker (Span.an_end.line));
      U2 : constant VSS.Unicode.UTF16_Code_Unit_Offset :=
        J2.First_UTF16_Offset;

      Dummy : Boolean;

   begin
      while Span.start.character /= Integer (J1.First_UTF16_Offset - U1)
        and then J1.Forward
      loop
         null;
      end loop;

      From := J1.Marker;

      while Span.an_end.character /= Integer (J2.First_UTF16_Offset - U2)
        and then J2.Forward
      loop
         null;
      end loop;

      Dummy := J2.Backward;
      To    := J2.Marker;
   end Span_To_Markers;

   ---------------------
   -- To_LSP_Location --
   ---------------------

   function To_LSP_Location
     (Self    : Document;
      Segment : Langkit_Support.Slocs.Source_Location_Range;
      Kinds   : LSP.Structures.AlsReferenceKind_Set := LSP.Constants.Empty)
      return LSP.Structures.Location
        is (uri     => Self.URI,
            a_range => Self.To_LSP_Range (Segment),
            alsKind => Kinds);

   ------------------
   -- To_LSP_Range --
   ------------------

   function To_LSP_Range
     (Self    : Document;
      Segment : Langkit_Support.Slocs.Source_Location_Range)
      return LSP.Structures.A_Range
   is

      Start_Line      : constant Natural := Natural (Segment.Start_Line) - 1;

      Start_Line_Text : constant VSS.Strings.Virtual_String :=
        (if Self.Line_To_Marker.Last_Index = Start_Line then
           Self.Text.Slice
             (Self.Line_To_Marker (Start_Line), Self.Text.After_Last_Character)
         else
           Self.Text.Slice
             (Self.Line_To_Marker (Start_Line),
              Self.Line_To_Marker (Start_Line + 1)));
      Start_Iterator  : VSS.Strings.Character_Iterators.Character_Iterator :=
        Start_Line_Text.At_First_Character;

      End_Line        : constant Natural := Natural (Segment.End_Line) - 1;
      End_Line_Text   : constant VSS.Strings.Virtual_String :=
        (if Self.Line_To_Marker.Last_Index = End_Line then
           Self.Text.Slice
             (Self.Line_To_Marker (End_Line), Self.Text.After_Last_Character)
         else
           Self.Text.Slice
             (Self.Line_To_Marker (End_Line),
              Self.Line_To_Marker (End_Line + 1)));
      End_Iterator   : VSS.Strings.Character_Iterators.Character_Iterator :=
        End_Line_Text.At_First_Character;
      Success        : Boolean with Unreferenced;

   begin
      --  Iterating forward through the line of the start position, initial
      --  iterator points to the first characters, thus "starts" from the
      --  second one.

      for J in 2 .. Segment.Start_Column loop
         Success := Start_Iterator.Forward;
      end loop;

      --  Iterating forward through the line of the end position. For the same
      --  reason "starts" from second character.

      for J in 2 .. Segment.End_Column loop
         Success := End_Iterator.Forward;
      end loop;

      return
        (start =>
           (line      => Start_Line,
            character => Natural (Start_Iterator.First_UTF16_Offset)),
         an_end =>
           (line      => End_Line,
            character => Natural (End_Iterator.Last_UTF16_Offset)));
   end To_LSP_Range;

   ----------
   -- Unit --
   ----------

   function Unit
     (Self : Document'Class; Context : LSP.Ada_Contexts.Context)
      return Libadalang.Analysis.Analysis_Unit
   is
      (Context.LAL_Context.Get_From_File
        (Filename => Context.URI_To_File (Self.URI).Display_Full_Name,
         Charset  => Context.Charset,
         Reparse  => False));

   --------------------------
   -- Versioned_Identifier --
   --------------------------

   function Versioned_Identifier
     (Self : Document) return LSP.Structures.VersionedTextDocumentIdentifier
       is (Self.URI, Self.Version);

end LSP.Ada_Documents;
