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

with Ada.Characters.Conversions;
with Ada.Characters.Latin_1;
with Ada.Characters.Wide_Latin_1;
with Ada.Strings.Unbounded;
with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
with Ada.Strings.Wide_Wide_Unbounded;
with Ada.Unchecked_Deallocation;

with GNAT.Strings;

with GNATCOLL.Utils;
with GNATCOLL.VFS;        use GNATCOLL.VFS;

with Langkit_Support.Slocs;
with Langkit_Support.Symbols;
with Langkit_Support.Text;
with Libadalang.Analysis; use Libadalang.Analysis;
with Libadalang.Sources;
with Libadalang.Doc_Utils;
with Libadalang.Iterators;

with VSS.String_Vectors;
with VSS.Strings.Iterators.Characters;
with VSS.Unicode;

with LSP.Ada_Contexts; use LSP.Ada_Contexts;
with LSP.Ada_Id_Iterators;
with LSP.Predefined_Completion;
with LSP.Common; use LSP.Common;
with LSP.Lal_Utils;
with LSP.Types.Utils;

with Pp.Actions;
with Pp.Scanner;
with Utils.Char_Vectors;

package body LSP.Ada_Documents is

   Lal_PP_Output : constant GNATCOLL.Traces.Trace_Handle :=
     GNATCOLL.Traces.Create ("ALS.LAL_PP_OUTPUT_ON_FORMATTING",
                             GNATCOLL.Traces.Off);
   --  Logging lalpp output if On

   function To_LSP_String
     (Value : Wide_Wide_String) return LSP.Types.LSP_String;

   function Get_Profile
     (Node        : Libadalang.Analysis.Basic_Decl;
      Is_Function : out Boolean)
      return LSP.Types.LSP_String;
   --  Return the profile of Node.

   function Is_Declaration
     (Node : Libadalang.Analysis.Basic_Decl) return Boolean;

   function Get_Visibility
     (Node : Libadalang.Analysis.Basic_Decl)
      return LSP.Messages.Als_Visibility;

   procedure Get_Aggregate_Completion
     (Node                     : Libadalang.Analysis.Aggregate;
      Context                  : LSP.Ada_Contexts.Context;
      Named_Notation_Threshold : Natural;
      Result                   : out LSP.Messages.CompletionItem_Vector);
   --  Return the completion list for the given aggregate node.
   --  The returned completion list may contain several items if the aggregate
   --  is used to assign a value to a variant record: in that case, a snippet
   --  per shape (i.e: a shape corresponds to one of the various forms that a
   --  discriminated variant record can take) will be proposed to the user.
   --  Named_Notation_Threshold defines the number of parameters/components at
   --  which point named notation is used for subprogram/aggregate completion
   --  snippets.

   procedure Get_Keywords_Completion
     (Node   : Libadalang.Analysis.Ada_Node;
      Prefix : Ada.Strings.UTF_Encoding.UTF_8_String;
      Result : out LSP.Messages.CompletionItem_Vector);
   --  Get completion for keywords, filtering them with the given Prefix.

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
      Version : LSP.Messages.Nullable_Number;
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

   -----------------
   -- Get_Text_At --
   -----------------

   function Get_Text_At
     (Self      : Document;
      Start_Pos : LSP.Messages.Position;
      End_Pos   : LSP.Messages.Position) return String
   is
      use LSP.Types;

      Start_Index  : constant Natural :=
        Self.Line_To_Index.Element
          (Natural (Start_Pos.line))
            + Natural (Start_Pos.character);
      End_Index    : constant Natural :=
        Self.Line_To_Index.Element
          (Natural (End_Pos.line))
            + Natural (End_Pos.character);
   begin
      return To_UTF_8_String
        (Unbounded_Slice (Self.Text, Start_Index, End_Index));
   end Get_Text_At;

   ----------
   -- Diff --
   ----------

   procedure Diff
     (Self     : Document;
      New_Text : LSP.Types.LSP_String;
      Old_Span : LSP.Messages.Span := LSP.Messages.Empty_Span;
      New_Span : LSP.Messages.Span := LSP.Messages.Empty_Span;
      Edit     : out LSP.Messages.TextEdit_Vector)
   is
      use LSP.Types;
      use LSP.Messages;

      LSP_New_Line_Function : constant VSS.Strings.Line_Terminator_Set :=
        (VSS.Strings.CR | VSS.Strings.CRLF | VSS.Strings.LF => True,
         others => False);
      --  LSP allows to use three kinds of line terminators: CR, CR+LF and LF.

      Old_First_Line : Natural;
      New_First_Line : Natural;

      Old_Lines, New_Lines   : VSS.String_Vectors.Virtual_String_Vector;
      Old_Length, New_Length : Natural;

   begin
      Old_Lines :=
        LSP.Types.To_Virtual_String (Self.Text).Split_Lines
          (LSP_New_Line_Function, True);
      New_Lines :=
        LSP.Types.To_Virtual_String (New_Text).Split_Lines
          (LSP_New_Line_Function, True);

      if Old_Span = Empty_Span then
         Old_First_Line := 1;
         Old_Length     := Old_Lines.Length;

      else
         Old_First_Line := Natural (Old_Span.first.line + 1);
         Old_Length := Natural (Old_Span.last.line - Old_Span.first.line + 1);
      end if;

      if New_Span = Empty_Span then
         New_First_Line := 1;
         New_Length     := New_Lines.Length;
      else
         New_First_Line := Natural (New_Span.first.line + 1);
         New_Length := Natural (New_Span.last.line - New_Span.first.line + 1);
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

         Old_Line_Number : Line_Number;
         --  needed to determine which line number in the old buffer is
         --  changed, deleted or before which new lines are inserted

         Changed_Block_Text : LSP_String;
         Changed_Block_Span : LSP.Messages.Span := ((0, 0), (0, 0));

         procedure Prepare
           (Line : Line_Number;
            Text : VSS.Strings.Virtual_String);
         --  Store imformation for Text_Etid in New_String and Span

         procedure Add (From_Line : Line_Number);
         --  Add prepared New_String and Span into Text_Edit

         -------------
         -- Prepare --
         -------------

         procedure Prepare
           (Line : Line_Number;
            Text : VSS.Strings.Virtual_String) is
         begin
            if Changed_Block_Span.last = (0, 0) then
               --  it is the first portion of a changed block so store
               --  last position of the changes
               Changed_Block_Span.last := (Line, 0);
            end if;

            --  accumulating new text for the changed block
            Changed_Block_Text :=
              LSP.Types.To_LSP_String (Text) & Changed_Block_Text;
         end Prepare;

         ---------
         -- Add --
         ---------

         procedure Add (From_Line : Line_Number) is
         begin
            if Changed_Block_Span.last = (0, 0) then
               --  No information for Text_Edit
               return;
            end if;

            Changed_Block_Span.first :=
              (line      => From_Line,
               character => 0);

            LSP.Messages.Prepend
              (Edit, LSP.Messages.TextEdit'
                 (span    => Changed_Block_Span,
                  newText => Changed_Block_Text));

            --  clearing
            Changed_Block_Text := Empty_LSP_String;
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

         Old_Line_Number := Line_Number (Old_First_Line + Old_Length - 1);

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
                  Add (Old_Line_Number);
               else
                  --  lines are different, change old line by new one,
                  --  we deleted whole line so 'To' position will be
                  --  the beginning of the next line
                  Prepare
                    (Old_Line_Number,
                     New_Lines.Element (New_First_Line + New_Index - 1));
               end if;

               --  move lines cursor backward
               Old_Line_Number := Old_Line_Number - 1;

               New_Index := New_Index - 1;
               Old_Index := Old_Index - 1;

            elsif LCS (Old_Index, New_Index) =
              LCS (Old_Index - 1, New_Index) - 5
            then
               --  line has been deleted, move lines cursor backward
               Prepare (Old_Line_Number, VSS.Strings.Empty_Virtual_String);

               Old_Line_Number := Old_Line_Number - 1;
               Old_Index       := Old_Index - 1;

            elsif LCS (Old_Index, New_Index) =
              LCS (Old_Index, New_Index - 1) - 5
            then
               --  line has been inserted
               --  insert Text_Edit information with insertion after
               --  current line, do not move lines cursor because it is
               --  additional line not present in the old document
               Prepare
                 (Old_Line_Number,
                  New_Lines.Element (New_First_Line + New_Index - 1));

               New_Index := New_Index - 1;
            end if;
         end loop;

         while Old_Index > 0 loop
            --  deleted
            Prepare (Old_Line_Number, VSS.Strings.Empty_Virtual_String);

            Old_Line_Number := Old_Line_Number - 1;
            Old_Index       := Old_Index - 1;
         end loop;

         while New_Index > 0 loop
            --  inserted
            Prepare
              (Old_Line_Number,
               New_Lines.Element (New_First_Line + New_Index - 1));

            New_Index := New_Index - 1;
         end loop;

         Add (Old_Line_Number);
         Free (LCS);

         --  Handle the edge case where the last location of
         --  the edit is trying to affect a non existent line.
         --  The edits are ordered so we only need to check the last one.
         if not (Integer (Edit.Last_Element.span.last.line)
                 in Self.Line_To_Index.First_Index ..
                   Self.Line_To_Index.Last_Index)
         then
            declare
               Element   : LSP.Messages.TextEdit := Edit.Last_Element;
               Last_Line : constant VSS.Strings.Virtual_String :=
                 Old_Lines (Old_Lines.Length);
               Iterator  :
                 VSS.Strings.Iterators.Characters.Character_Iterator :=
                   Last_Line.First_Character;

            begin
               --  Iterate to the end of the line to compute UTF16 offset of
               --  the last character

               while Iterator.Forward loop
                  null;
               end loop;

               --  Replace the wrong location by the end of the buffer
               Element.span.last :=
                 (line      => Line_Number (Old_Lines.Length - 1),
                  character => Iterator.UTF16_Offset);
               Edit.Replace_Element (Edit.Last, Element);
            end;
         end if;

      exception
         when others =>
            Free (LCS);
            raise;
      end;
   end Diff;

   ----------------
   -- Formatting --
   ----------------

   function Formatting
     (Self     : Document;
      Context  : LSP.Ada_Contexts.Context;
      Span     : LSP.Messages.Span;
      Cmd      : Pp.Command_Lines.Cmd_Line;
      Edit     : out LSP.Messages.TextEdit_Vector)
      return Boolean
   is
      use Utils.Char_Vectors;
      use Utils.Char_Vectors.Char_Vectors;
      use LSP.Types;
      use LSP.Messages;

      Input     : Char_Vector;
      Output    : Char_Vector;
      In_Range  : Char_Subrange;
      Out_Range : Char_Subrange;
      Out_Span  : LSP.Messages.Span;

      Messages  : Pp.Scanner.Source_Message_Vector;

      function Get_Range
        (Span : LSP.Messages.Span)
         return Utils.Char_Vectors.Char_Subrange;
      --  Convert Span to Char_Subrange

      function Get_Range
        (Index         : Natural;
         From_Index    : Natural := 1;
         From_Position : LSP.Messages.Position := (0, 0))
         return LSP.Messages.Position;
      --  Convert UTF-8 Index to Position in UTF-16 string
      --  From_Index and corresponding From_Position are used when we know
      --  some 'start' point to avoid scanning from the beginning of
      --  the buffer

      ---------------
      -- Get_Range --
      ---------------

      function Get_Range
        (Span : LSP.Messages.Span)
         return Utils.Char_Vectors.Char_Subrange
      is
         use type VSS.Unicode.UTF16_Code_Unit_Count;

         Line   : Line_Number  := 0;
         Char   : UTF_16_Index := 0;
         Result : Utils.Char_Vectors.Char_Subrange := (1, 1);
      begin
         if Span.first.line > 0 then
            --  iterating over symbols to find the beginning
            --  of the needed line
            for Index in 1 .. Natural (Length (Input)) loop
               Result.First := Index;

               if Input.Element (Index) = Ada.Characters.Latin_1.LF then
                  Line := Line + 1;

                  if Line = Span.first.line then
                     --  skip '\n'
                     Result.First := Result.First + 1;
                     exit;
                  end if;
               end if;
            end loop;
         end if;

         --  looking for a start symbol from the beginning of the line
         while Result.First <= Natural (Length (Input))
           and then Char < Span.first.character
         loop
            Char := Char + 1;

            Result.First := Result.First +
              LSP.Types.Utils.Lenght_Of_UTF8_Symbol
                (Input.Element (Result.First));
         end loop;

         if Line = Span.last.line then
            --  continue from the 'start' symbol
            Result.Last := Result.First;

         else
            --  set Char to 0 because we will start from the beginning
            --  of the new line
            Char := 0;

            --  continue iterating from the founded 'start' symbol e.g.
            --  from the 'start' line

            for Index in Result.First .. Natural (Length (Input)) loop
               Result.Last := Index;

               if Input.Element (Index) = Ada.Characters.Latin_1.LF then
                  Line := Line + 1;

                  if Line = Span.last.line then
                     --  skip '\n'
                     Result.Last := Result.Last + 1;
                     exit;
                  end if;
               end if;
            end loop;
         end if;

         --  looking for a last symbol
         while Result.Last <= Natural (Length (Input))
           and then Char < Span.last.character
         loop
            Result.Last := Result.Last +
              LSP.Types.Utils.Lenght_Of_UTF8_Symbol
                (Input.Element (Result.Last));

            Char := Char + 1;
         end loop;

         return Result;
      end Get_Range;

      ---------------
      -- Get_Range --
      ---------------

      function Get_Range
        (Index         : Natural;
         From_Index    : Natural := 1;
         From_Position : LSP.Messages.Position := (0, 0))
         return LSP.Messages.Position
      is
         use type VSS.Unicode.UTF16_Code_Unit_Count;

         Current : Natural := From_Index;
         Result  : LSP.Messages.Position := From_Position;
      begin
         while Current < Natural (Length (Output))
           and then Current < Index
         loop
            if Output.Element (Current) = Ada.Characters.Latin_1.LF then
               --  new line
               Result.line      := Result.line + 1;
               Result.character := 0;
               Current          := Current + 1;

            else
               --  goto to the beginning of the next UTF-8 character
               Current := Current + LSP.Types.Utils.Lenght_Of_UTF8_Symbol
                 (Output.Element (Current));

               --  one more symbol has been passed
               Result.character := Result.character + 1;
            end if;
         end loop;

         return Result;
      end Get_Range;

      S : GNAT.Strings.String_Access;
   begin
      S := new String'
        (Ada.Strings.Unbounded.To_String
           (LSP.Types.To_UTF_8_Unbounded_String (Self.Text)));
      Input.Append (S.all);
      GNAT.Strings.Free (S);

      if Span = LSP.Messages.Empty_Span then
         In_Range := Input.Full_Range;
      else
         In_Range := Get_Range (Span);
      end if;

      Pp.Actions.Format_Vector
        (Cmd       => Cmd,
         Input     => Input,
         Node      => Self.Unit (Context).Root,
         In_Range  => In_Range,
         Output    => Output,
         Out_Range => Out_Range,
         Messages  => Messages);

      if not Messages.Is_Empty then
         return False;
      end if;

      S := new String'(Output.To_Array);
      if Lal_PP_Output.Is_Active then
         Lal_PP_Output.Trace (S.all);
      end if;

      --  it seems that Format_Vector does not set Out_Range properly, so
      --  using full diff for now
      Out_Range.First := 1;

      if Span = LSP.Messages.Empty_Span
        or else Out_Range.First < In_Range.First
      then
         --  diff for the whole document
         Diff (Self, LSP.Types.To_LSP_String (S.all), Edit => Edit);

      else
         --  diff for a part of the document

         Out_Span.first := Span.first;
         Out_Span.last  := Get_Range
           (Out_Range.Last, In_Range.First, Span.first);

         Diff (Self, LSP.Types.To_LSP_String (S.all), Span, Out_Span, Edit);
      end if;

      GNAT.Strings.Free (S);
      return True;

   exception
      when others =>
         GNAT.Strings.Free (S);
         return False;
   end Formatting;

   ------------------------
   -- Get_Imported_Units --
   ------------------------

   procedure Get_Imported_Units
     (Self          : Document;
      Context       : LSP.Ada_Contexts.Context;
      Project_Path  : GNATCOLL.VFS.Virtual_File;
      Show_Implicit : Boolean;
      Result        : out LSP.Messages.ALS_Unit_Description_Vector)
   is
      use LSP.Messages;

      Unit     : constant Libadalang.Analysis.Analysis_Unit :=
        LSP.Ada_Documents.Unit (Self    => Self,
                                Context => Context);
      Root     : constant Ada_Node := Unit.Root;

      procedure Append_Units
        (Units : Libadalang.Analysis.Compilation_Unit_Array);

      ------------------
      -- Append_Units --
      ------------------

      procedure Append_Units
        (Units : Libadalang.Analysis.Compilation_Unit_Array) is
      begin
         for Unit of Units loop
            Result.Append
              (LSP.Messages.ALS_Unit_Description'
                 (uri        => LSP.Common.From_File
                      (GNATCOLL.VFS.Create (+Unit.Unit.Get_Filename)),
                  projectUri => LSP.Common.From_File (Project_Path)));
         end loop;
      end Append_Units;

   begin

      case Root.Kind is
         when Libadalang.Common.Ada_Compilation_Unit_Range =>
            declare
               Comp_Unit    : constant Compilation_Unit :=
                 As_Compilation_Unit (Root);
               Units        : constant Compilation_Unit_Array :=
                 (if Show_Implicit then
                     Comp_Unit.P_Unit_Dependencies
                  else
                     Comp_Unit.P_Withed_Units);
            begin
               Append_Units (Units);
            end;

         when Libadalang.Common.Ada_Compilation_Unit_List_Range =>
            declare
               Comp_Unit_List : constant Compilation_Unit_List :=
                 As_Compilation_Unit_List (Root);
            begin
               for Comp_Unit of Comp_Unit_List loop
                  Append_Units
                    (if Show_Implicit then
                        Comp_Unit.P_Withed_Units
                     else
                        Comp_Unit.P_Unit_Dependencies);
               end loop;
            end;
         when others =>
            null;
      end case;
   end Get_Imported_Units;

   -------------------------
   -- Get_Importing_Units --
   -------------------------

   procedure Get_Importing_Units
     (Self          : Document;
      Context       : LSP.Ada_Contexts.Context;
      Project_Path  : GNATCOLL.VFS.Virtual_File;
      Show_Implicit : Boolean;
      Result        : out LSP.Messages.ALS_Unit_Description_Vector)
   is

      Root : constant Ada_Node := Self.Unit (Context).Root;

      procedure Append_Units
        (Units : Libadalang.Analysis.Analysis_Unit_Array);

      ------------------
      -- Append_Units --
      ------------------

      procedure Append_Units
        (Units : Libadalang.Analysis.Analysis_Unit_Array) is
      begin
         for Unit of Units loop
            Result.Append
              (LSP.Messages.ALS_Unit_Description'
                 (uri        => LSP.Common.From_File
                      (GNATCOLL.VFS.Create (+Unit.Get_Filename)),
                  projectUri => LSP.Common.From_File (Project_Path)));
         end loop;
      end Append_Units;

   begin

      case Root.Kind is
         when Libadalang.Common.Ada_Compilation_Unit_Range =>
            declare
               Comp_Unit : constant Compilation_Unit :=
                 As_Compilation_Unit (Root);
               Importing_Units  : constant
                 Libadalang.Analysis.Analysis_Unit_Array :=
                   Comp_Unit.P_Filter_Is_Imported_By
                     (Units      => Context.Analysis_Units,
                      Transitive => Show_Implicit);
            begin
               Append_Units (Importing_Units);
            end;

         when Libadalang.Common.Ada_Compilation_Unit_List_Range =>
            declare
               Comp_Unit_List : constant Compilation_Unit_List :=
                 As_Compilation_Unit_List (Root);
            begin
               for Comp_Unit of Comp_Unit_List loop
                  Append_Units
                    (Comp_Unit.P_Filter_Is_Imported_By
                       (Units      => Context.Analysis_Units,
                        Transitive => Show_Implicit));
               end loop;
            end;
         when others =>
            null;
      end case;
   end Get_Importing_Units;

   -------------------------
   -- Find_All_References --
   -------------------------

   procedure Find_All_References
     (Self       : Document;
      Context    : LSP.Ada_Contexts.Context;
      Definition : Libadalang.Analysis.Defining_Name;
      Callback   : not null access procedure
        (Base_Id : Libadalang.Analysis.Base_Id;
         Kind    : Libadalang.Common.Ref_Result_Kind;
         Cancel  : in out Boolean))
   is
      Units : constant Libadalang.Analysis.Analysis_Unit_Array :=
        (1 =>  LSP.Ada_Documents.Unit (Self    => Self,
                                       Context => Context));
   begin
      LSP.Ada_Id_Iterators.Find_All_References (Definition, Units, Callback);
   exception
      when E : Libadalang.Common.Property_Error =>
         Log (Self.Trace, E, "in Find_All_References");
   end Find_All_References;

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

   ---------------------
   -- Has_Diagnostics --
   ---------------------

   function Has_Diagnostics
     (Self    : Document;
      Context : LSP.Ada_Contexts.Context)
      return Boolean is
   begin
      return Self.Unit (Context).Has_Diagnostics;
   end Has_Diagnostics;

   --------------------------
   -- Get_Symbol_Hierarchy --
   --------------------------

   procedure Get_Symbol_Hierarchy
     (Self    : Document;
      Context : LSP.Ada_Contexts.Context;
      Result  : out LSP.Messages.Symbol_Vector)
   is
      use LSP.Messages;

      procedure Walk
        (Node         : Libadalang.Analysis.Ada_Node;
         Cursor       : LSP.Messages.DocumentSymbol_Trees.Cursor;
         Nested_Level : Integer;
         Tree         : in out LSP.Messages.DocumentSymbol_Tree);
      --  Traverse Node and all its children recursively. Find any defining
      --  name and construct corresponding symbol node, then append it to
      --  the Tree under a position pointed by the Cursor.

      ----------
      -- Walk --
      ----------

      procedure Walk
        (Node         : Libadalang.Analysis.Ada_Node;
         Cursor       : LSP.Messages.DocumentSymbol_Trees.Cursor;
         Nested_Level : Integer;
         Tree         : in out LSP.Messages.DocumentSymbol_Tree)
      is
         Next             : LSP.Messages.DocumentSymbol_Trees.Cursor := Cursor;
         Dummy            : LSP.Messages.DocumentSymbol_Trees.Cursor;
         --  Dummy is used for node which can't be parents like pragma and with
         --  statements
         New_Nested_Level : Integer := Nested_Level;
      begin
         if Node = No_Ada_Node
           or else Node.Kind in Libadalang.Common.Ada_Expr
         then
            return;
         end if;

         if Node.Kind in Libadalang.Common.Ada_Basic_Decl then
            declare
               Decl : constant Libadalang.Analysis.Basic_Decl :=
                 Node.As_Basic_Decl;

               Kind : constant LSP.Messages.SymbolKind :=
                 LSP.Lal_Utils.Get_Decl_Kind
                   (Decl, Ignore_Local => Nested_Level > 1);

            begin
               if Kind /= LSP.Messages.A_Null then
                  New_Nested_Level := New_Nested_Level + 1;
                  declare
                     Names : constant Libadalang.Analysis.Defining_Name_Array
                       := Decl.P_Defining_Names;
                  begin

                     for Name of Names loop
                        exit when Name = Libadalang.Analysis.No_Defining_Name;

                        declare
                           Is_Function : Boolean;
                           Profile : constant LSP.Types.LSP_String :=
                             Get_Profile (Decl, Is_Function);
                           Item : constant LSP.Messages.DocumentSymbol :=
                             (name              => To_LSP_String (Name.Text),
                              detail            =>
                                (Is_Set => True, Value => Profile),
                              kind              => Kind,
                              deprecated        => (Is_Set => False),
                              span              => LSP.Lal_Utils.To_Span
                                (Node.Sloc_Range),
                              selectionRange    => LSP.Lal_Utils.To_Span
                                (Name.Sloc_Range),
                              alsIsDeclaration  =>
                                (Is_Set => True,
                                 Value  => Is_Declaration (Decl)),
                              alsIsAdaProcedure =>
                                (if Is_Function
                                 then (Is_Set => False)
                                 else (Is_Set => True, Value => True)),
                              alsVisibility     =>
                                (Is_Set => True,
                                 Value  => Get_Visibility (Decl)),
                              children          => True);
                        begin
                           Tree.Insert_Child
                             (Parent   => Cursor,
                              Before   =>
                                Messages.DocumentSymbol_Trees.No_Element,
                              New_Item => Item,
                              Position => Next);
                        end;
                     end loop;
                  end;
               end if;
            end;
         elsif Node.Kind in Libadalang.Common.Ada_With_Clause_Range then
            declare
               With_Node : constant Libadalang.Analysis.With_Clause :=
                 Node.As_With_Clause;
            begin
               for N of With_Node.F_Packages loop
                  declare
                     Item : constant LSP.Messages.DocumentSymbol :=
                       (name              => To_LSP_String (N.Text),
                        detail            => (Is_Set => False),
                        kind              => Namespace,
                        deprecated        => (Is_Set => False),
                        span              => LSP.Lal_Utils.To_Span
                          (Node.Sloc_Range),
                        selectionRange    => LSP.Lal_Utils.To_Span
                          (N.Sloc_Range),
                        alsIsDeclaration  => (Is_Set => False),
                        alsIsAdaProcedure => (Is_Set => False),
                        alsVisibility     => (Is_Set => False),
                        children          => False);
                  begin
                     Tree.Insert_Child
                       (Parent   => Next,
                        Before   =>
                          Messages.DocumentSymbol_Trees.No_Element,
                        New_Item => Item,
                        Position => Dummy);
                  end;
               end loop;
            end;
         elsif Nested_Level <=  1
           and then Node.Kind in Libadalang.Common.Ada_Pragma_Node
         then
            declare
               Pragma_Node : constant Libadalang.Analysis.Pragma_Node :=
                 Node.As_Pragma_Node;
               Id          : constant Libadalang.Analysis.Identifier  :=
                 Pragma_Node.F_Id;
               Item        : constant LSP.Messages.DocumentSymbol :=
                 (name              =>
                    To_LSP_String (Id.Text),
                  detail            =>
                    (Is_Set => True,
                     Value  =>
                       To_LSP_String ("(" & (Pragma_Node.F_Args.Text & ")"))),
                  kind              => Property,
                  deprecated        => (Is_Set => False),
                  span              => LSP.Lal_Utils.To_Span
                    (Node.Sloc_Range),
                  selectionRange    => LSP.Lal_Utils.To_Span
                    (Id.Sloc_Range),
                  alsIsDeclaration  => (Is_Set => False),
                  alsIsAdaProcedure => (Is_Set => False),
                  alsVisibility     => (Is_Set => False),
                  children          => False);
            begin
               Tree.Insert_Child
                 (Parent   => Next,
                  Before   =>
                    Messages.DocumentSymbol_Trees.No_Element,
                  New_Item => Item,
                  Position => Dummy);
            end;
         end if;

         for Child of Node.Children loop
            if Child not in Libadalang.Analysis.No_Ada_Node then
               Walk (Child, Next, New_Nested_Level, Tree);
            end if;
         end loop;
      end Walk;

      Root : constant Libadalang.Analysis.Ada_Node := Self.Unit (Context).Root;
   begin
      Result := (Is_Tree => True, others => <>);
      Walk (Root, Result.Tree.Root, 0, Result.Tree);
   end Get_Symbol_Hierarchy;

   -----------------
   -- Get_Symbols --
   -----------------

   procedure Get_Symbols
     (Self    : Document;
      Context : LSP.Ada_Contexts.Context;
      Result  : out LSP.Messages.Symbol_Vector)
   is
      use LSP.Messages;
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
         declare
            Kind : constant LSP.Messages.SymbolKind :=
              LSP.Lal_Utils.Get_Decl_Kind
                (Element.As_Defining_Name.P_Basic_Decl, Ignore_Local => True);
         begin
            if Kind /= LSP.Messages.A_Null then
               Item.name := To_LSP_String (Element.Text);
               Item.kind := Kind;
               Item.location :=
                 (uri     => Self.URI,
                  span    => LSP.Lal_Utils.To_Span (Element.Sloc_Range),
                  alsKind => LSP.Messages.Empty_Set);

               Result.Vector.Append (Item);
            end if;
         end;
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
      Comments   : Boolean;
      Result     : out LSP.Messages.FoldingRange_Vector)
   is
      use Libadalang.Common;

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
            use type LSP.Types.Line_Number;
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

                  foldingRange.startLine := Location.span.first.line;
               end if;

               foldingRange.endLine := Location.span.last.line;

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
            foldingRange.startLine := Span.first.line;
            foldingRange.endLine   := Span.last.line;

            if not Lines_Only then
               foldingRange.startCharacter :=
                 (Is_Set => True,
                  Value  => LSP.Types.LSP_Number (Span.first.character));

               foldingRange.startCharacter :=
                 (Is_Set => True,
                  Value  => LSP.Types.LSP_Number (Span.last.character));
            end if;

            Result.Append (foldingRange);
         end if;
      end Store_Span;

      Token : Token_Reference;
      Span  : LSP.Messages.Span;

   begin
      Traverse (Self.Unit (Context).Root, Parse'Access);

      if not Comments then
         --  do not process comments
         return;
      end if;

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

   -----------------
   -- Get_Profile --
   -----------------

   function Get_Profile
     (Node        : Libadalang.Analysis.Basic_Decl;
      Is_Function : out Boolean)
      return LSP.Types.LSP_String
   is
      use Ada.Strings.Wide_Wide_Unbounded;
      use Libadalang.Common;

      function To_Text (Node : Ada_Node'Class) return Wide_Wide_String;
      --  Retrieve the node text and format it

      function To_Profile
        (Node : Libadalang.Analysis.Subp_Spec'Class)
         return LSP.Types.LSP_String;

      -------------
      -- To_Text --
      -------------

      function To_Text (Node : Ada_Node'Class) return Wide_Wide_String
      is
         Node_Text : constant String :=
           Langkit_Support.Text.To_UTF8 (Node.Text);
         Result    : String  := Node_Text;
         Was_Space : Boolean := False;
         Cur       : Integer := Node_Text'First;
      begin
         for I in Node_Text'Range loop
            if Node_Text (I) = ' ' then
               --  Trim multiple whitespace to only keep one
               if not Was_Space then
                  Result (Cur) := Node_Text (I);
                  Cur := Cur + 1;
               end if;
               Was_Space := True;
               --  Remove the new line character
            elsif Node_Text (I) /= ASCII.LF then
               Was_Space := False;
               Result (Cur) := Node_Text (I);
               Cur := Cur + 1;
            end if;
         end loop;
         return Ada.Characters.Conversions.To_Wide_Wide_String
           (Result (Result'First .. Cur - 1));
      end To_Text;

      ----------------
      -- To_Profile --
      ----------------

      function To_Profile
        (Node : Libadalang.Analysis.Subp_Spec'Class)
         return LSP.Types.LSP_String
      is
         Result  : Unbounded_Wide_Wide_String;
         Params  : constant Param_Spec_Array := Node.P_Params;
         Returns : constant Type_Expr := Node.F_Subp_Returns;
      begin
         if Params'Length > 0 then
            Append (Result, "(");
         end if;

         for Param of Params loop
            declare
               Names : constant Defining_Name_List := Param.F_Ids;
               Init  : constant Expr := Param.F_Default_Expr;
               Item  : Unbounded_Wide_Wide_String;
            begin
               Append (Item, " :");

               case Param.F_Mode is
                  when Ada_Mode_Default | Ada_Mode_In =>
                     Append (Item, " in ");
                  when Ada_Mode_In_Out =>
                     Append (Item, " in out ");
                  when Ada_Mode_Out =>
                     Append (Item, " out ");
               end case;

               Append (Item, To_Text (Param.F_Type_Expr));

               if not Init.Is_Null then
                  Append (Item, " := ");
                  Append (Item, To_Text (Init));
               end if;

               for J in Names.First_Child_Index .. Names.Last_Child_Index loop
                  if Length (Result) /= 1 then
                     Append (Result, "; ");
                  end if;

                  Append (Result, To_Text (Names.Child (J)));
                  Append (Result, Item);
               end loop;

            end;
         end loop;

         if Params'Length > 0 then
            Append (Result, ")");
         end if;

         if not Returns.Is_Null then
            Is_Function := True;
            Append (Result, " return ");
            Append (Result, To_Text (Returns));
         end if;

         return To_LSP_String (To_Wide_Wide_String (Result));
      end To_Profile;
   begin
      Is_Function := False;
      case Node.Kind is
         when Ada_Classic_Subp_Decl =>
            return To_Profile (Node.As_Classic_Subp_Decl.F_Subp_Spec);
         when Ada_Base_Subp_Body    =>
            return To_Profile (Node.As_Base_Subp_Body.F_Subp_Spec);
         when Ada_Generic_Subp_Decl =>
            return To_Profile
              (Node.As_Generic_Subp_Decl.F_Subp_Decl.F_Subp_Spec);
         when others =>
            return LSP.Types.Empty_LSP_String;
      end case;
   end Get_Profile;

   --------------------
   -- Is_Declaration --
   --------------------

   function Is_Declaration
     (Node : Libadalang.Analysis.Basic_Decl) return Boolean
   is
      use Libadalang.Common;
   begin
      case Node.Kind is
         when Ada_Generic_Package_Decl |
              Ada_Generic_Package_Instantiation |
              Ada_Generic_Package_Renaming_Decl |
              Ada_Package_Decl |
              Ada_Package_Renaming_Decl |
              Ada_Abstract_Subp_Decl |
              Ada_Formal_Subp_Decl |
              Ada_Subp_Decl |
              Ada_Subp_Renaming_Decl |
              Ada_Generic_Subp_Instantiation |
              Ada_Generic_Subp_Renaming_Decl |
              Ada_Generic_Subp_Decl |
              Ada_Null_Subp_Decl |
              Ada_Expr_Function |
              Ada_Protected_Type_Decl |
              Ada_Single_Protected_Decl |
              Ada_Entry_Decl |
              Ada_Type_Decl |
              Ada_Single_Task_Decl |
              Ada_Task_Type_Decl =>
            return True;
         when others =>
            return False;
      end case;
   end Is_Declaration;

   --------------------
   -- Get_Visibility --
   --------------------

   function Get_Visibility
     (Node : Libadalang.Analysis.Basic_Decl)
      return LSP.Messages.Als_Visibility
   is
      use Libadalang.Common;
   begin
      for Parent of Node.Parents loop
         if Parent.Kind = Ada_Private_Part then
            return LSP.Messages.Als_Private;
         elsif Parent.Kind in Ada_Protected_Body | Ada_Protected_Def then
            return LSP.Messages.Als_Protected;
         end if;
      end loop;
      return LSP.Messages.Als_Public;
   end Get_Visibility;

   -----------------
   -- Get_Word_At --
   -----------------

   function Get_Word_At
     (Self     : Document;
      Context  : LSP.Ada_Contexts.Context;
      Position : LSP.Messages.Position)
      return LSP.Types.LSP_String
   is
      use Langkit_Support.Slocs;
      use all type Libadalang.Common.Token_Kind;

      Result : LSP.Types.LSP_String;

      Unit : constant Libadalang.Analysis.Analysis_Unit :=
        Self.Unit (Context);

      Where : constant Source_Location :=
        (Line   => Line_Number (Position.line) + 1,
         Column => Column_Number (Position.character));
      --  Compute the position we want for completion, which is one character
      --  before the cursor.

      Token : constant Libadalang.Common.Token_Reference :=
        Unit.Lookup_Token (Where);

      Data : constant Libadalang.Common.Token_Data_Type :=
        Libadalang.Common.Data (Token);

      Kind : constant Libadalang.Common.Token_Kind :=
        Libadalang.Common.Kind (Data);

      Text : constant Wide_Wide_String :=
        Libadalang.Common.Text (Token);

      Sloc : constant Source_Location_Range :=
        Libadalang.Common.Sloc_Range (Data);

      Span : constant Integer :=
        Natural (Where.Column) - Natural (Sloc.Start_Column);
   begin
      if Kind in Ada_Identifier .. Ada_Xor
        and then Compare (Sloc, Where) = Inside
      then
         Result := LSP.Types.To_LSP_String
           (Text (Text'First .. Text'First + Span));
      end if;

      return Result;
   end Get_Word_At;

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
      Self.Refresh_Symbol_Cache := True;
      Recompute_Indexes (Self);
   end Initialize;

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
      use Libadalang.Common;

      Result : LSP.Types.LSP_String;
   begin

      --  If the basic declaration is an enum literal, display the whole
      --  enumeration type declaration instead.
      if BD.Kind in Ada_Enum_Literal_Decl then
         Result := LSP.Common.Get_Hover_Text
           (As_Enum_Literal_Decl (BD).P_Enum_Type.As_Basic_Decl);
      else
         Result := LSP.Common.Get_Hover_Text (BD);
      end if;

      return Result;
   end Compute_Completion_Detail;

   -----------------------------
   -- Compute_Completion_Item --
   -----------------------------

   function Compute_Completion_Item
     (Context                  : LSP.Ada_Contexts.Context;
      BD                       : Libadalang.Analysis.Basic_Decl;
      DN                       : Libadalang.Analysis.Defining_Name;
      Use_Snippets         : Boolean;
      Named_Notation_Threshold : Natural;
      Is_Dot_Call              : Boolean;
      Is_Visible               : Boolean)
      return LSP.Messages.CompletionItem
   is
      use LSP.Messages;
      use LSP.Types;

      Item           : CompletionItem;
      Subp_Spec_Node : Base_Subp_Spec;
      Decl_Unit_File : Virtual_File;
      Doc_Text       : LSP_String;
      Loc_Text       : LSP_String;
   begin
      Item.label := To_LSP_String (DN.P_Relative_Name.Text);
      Item.kind := (True, To_Completion_Kind
                            (LSP.Lal_Utils.Get_Decl_Kind (BD)));
      Item.detail := (True, Compute_Completion_Detail (BD));

      if not Is_Visible then
         Item.sortText := (True, '~' & Item.label);
         Item.insertText := (True, Item.label);
         Item.label := Item.label & " (invisible)";
      end if;

      --  Property_Errors can occur when calling
      --  Get_Documentation on unsupported docstrings, so
      --  add an exception handler to catch them and recover.
      begin
         Doc_Text := To_LSP_String
           (Ada.Strings.UTF_Encoding.Wide_Wide_Strings.
              Encode
                (Libadalang.Doc_Utils.Get_Documentation
                     (BD).Doc.To_String));

         --  Append the declaration's location.
         --  In addition, append the project's name if we are dealing with an
         --  aggregate project.

         Decl_Unit_File := GNATCOLL.VFS.Create (+BD.Unit.Get_Filename);

         if Doc_Text /= Empty_LSP_String then
            Loc_Text := To_LSP_String (ASCII.LF & ASCII.LF);
         end if;

         Loc_Text := To_LSP_String
           ("at " & Decl_Unit_File.Display_Base_Name & " ("
            & GNATCOLL.Utils.Image
              (Integer (BD.Sloc_Range.Start_Line), Min_Width => 1)
            & ":"
            & GNATCOLL.Utils.Image
              (Integer (BD.Sloc_Range.Start_Column), Min_Width => 1)
            & ")") & Loc_Text;

         Item.documentation :=
           (Is_Set => True,
            Value  => String_Or_MarkupContent'
              (Is_String => True,
               String    => Loc_Text & Doc_Text));

      exception
         when E : Libadalang.Common.Property_Error =>
            LSP.Common.Log (Context.Trace, E);
            Item.documentation := (others => <>);
      end;

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
         Insert_Text : LSP_String := Item.label;
         All_Params  : constant Param_Spec_Array := Subp_Spec_Node.P_Params;

         Params      : constant Param_Spec_Array :=
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
            Item.insertTextFormat := Optional_InsertTextFormat'
              (Is_Set => True,
               Value  => Snippet);

            Insert_Text := Insert_Text & " (";

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
                     Mode : constant String :=
                       Langkit_Support.Text.To_UTF8 (Param.F_Mode.Text);
                  begin
                     if Use_Named_Notation then
                        Insert_Text := Insert_Text & To_LSP_String
                          (Langkit_Support.Text.To_UTF8 (Id.Text)
                           & " => "
                           & "${"
                           & GNATCOLL.Utils.Image (Idx, Min_Width => 1)
                           & ":"
                           &  Langkit_Support.Text.To_UTF8 (Id.Text)
                           & " : "
                           & (if Mode /= "" then
                                  Mode & " "
                             else
                                "")
                           & Langkit_Support.Text.To_UTF8
                             (Param.F_Type_Expr.Text)
                           & "}, ");
                     else
                        Insert_Text := Insert_Text & To_LSP_String
                          ("${"
                           & GNATCOLL.Utils.Image (Idx, Min_Width => 1)
                           & ":"
                           & Langkit_Support.Text.To_UTF8 (Id.Text)
                           & " : "
                           & (if Mode /= "" then
                                  Mode & " "
                             else
                                "")
                           & Langkit_Support.Text.To_UTF8
                             (Param.F_Type_Expr.Text)
                           & "}, ");
                     end if;

                     Idx := Idx + 1;
                  end;
               end loop;
            end loop;

            --  Remove the "}, " substring that has been appended in the last
            --  loop iteration.
            Insert_Text := Unbounded_Slice
              (Insert_Text,
               1,
               Length (Insert_Text) - 2);

            --  Insert '$0' (i.e: the final tab stop) at the end.
            Insert_Text := Insert_Text & ")$0";

            Item.insertText :=
              (Is_Set => True,
               Value  => Insert_Text);
         end if;
      end;

      return Item;
   end Compute_Completion_Item;

   -------------------------------
   -- Get_Any_Symbol_Completion --
   -------------------------------

   procedure Get_Any_Symbol_Completion
     (Self    : in out Document;
      Context : LSP.Ada_Contexts.Context;
      Prefix  : VSS.Strings.Virtual_String;
      Limit   : Ada.Containers.Count_Type;
      Result  : in out LSP.Ada_Completion_Sets.Completion_Maps.Map)
   is

      procedure Refresh_Symbol_Cache;

      --------------------------
      -- Refresh_Symbol_Cache --
      --------------------------

      procedure Refresh_Symbol_Cache is
         use Langkit_Support.Symbols;
         use Libadalang.Common;

         Node : Libadalang.Analysis.Ada_Node;

         It : Libadalang.Iterators.Traverse_Iterator'Class :=
           Libadalang.Iterators.Find
             (Self.Unit (Context).Root,
              Libadalang.Iterators.Kind_Is (Ada_Defining_Name));
      begin
         while It.Next (Node) loop
            declare
               Token : constant Token_Reference := Node.Token_End;

               Text : constant Wide_Wide_String :=
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
               end if;

               Self.Symbol_Cache (Cursor).Append (Node.As_Defining_Name);
            end;
         end loop;
      end Refresh_Symbol_Cache;

      Cursor : Symbol_Maps.Cursor;

   begin
      if Self.Refresh_Symbol_Cache then
         Refresh_Symbol_Cache;
         Self.Refresh_Symbol_Cache := False;
      end if;

      Cursor := Self.Symbol_Cache.Ceiling (Prefix);

      Each_Prefix :
      while Symbol_Maps.Has_Element (Cursor) loop
         declare
            Key : constant VSS.Strings.Virtual_String :=
              Symbol_Maps.Key (Cursor);

         begin
            exit Each_Prefix when not Key.Starts_With (Prefix);

            for Name of Self.Symbol_Cache (Cursor) loop
               if not Result.Contains (Name) then
                  Result.Insert
                    (Name,
                     (Is_Dot_Call  => False,
                      Is_Visible   => False,
                      Use_Snippets => False));
               end if;
            end loop;

            Symbol_Maps.Next (Cursor);
         end;
      end loop Each_Prefix;
   end Get_Any_Symbol_Completion;

   ------------------------------
   -- Get_Aggregate_Completion --
   ------------------------------

   procedure Get_Aggregate_Completion
     (Node                     : Libadalang.Analysis.Aggregate;
      Context                  : LSP.Ada_Contexts.Context;
      Named_Notation_Threshold : Natural;
      Result                   : out LSP.Messages.CompletionItem_Vector)
   is
      pragma Unreferenced (Context);

      use Libadalang.Common;
      use LSP.Messages;
      use LSP.Types;

      Aggr_Type          : constant Base_Type_Decl :=
        Node.P_Expression_Type.P_Canonical_Type;
      Use_Named_Notation : Boolean := False;

      function Get_Snippet_For_Component
        (Param              : Base_Formal_Param_Decl;
         Idx                : Natural;
         Use_Named_Notation : Boolean) return LSP_String;
      --  Return a snippet for the given component

      function Get_Snippet_For_Discriminant
        (Disc               : Discriminant_Values;
         Idx                : Natural;
         Use_Named_Notation : Boolean) return LSP_String;
      --  Return a snippet for the given discriminant

      function Get_Label_For_Shape
        (Discriminants : Discriminant_Values_Array) return LSP_String;
      --  Return a suitable label for the given shape (i.e: a shape corresponds
      --  to one of the various forms that a discriminated variant record can
      --  take).

      -------------------------------
      -- Get_Snippet_For_Component --
      -------------------------------

      function Get_Snippet_For_Component
        (Param              : Base_Formal_Param_Decl;
         Idx                : Natural;
         Use_Named_Notation : Boolean) return LSP_String
      is
         Snippet    : LSP_String;
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
               Snippet := Snippet & To_LSP_String
                 (Langkit_Support.Text.To_UTF8 (Id.Text)
                  & " => ");
            end if;

            Snippet := Snippet & To_LSP_String
              ("${"
               & GNATCOLL.Utils.Image (Idx, Min_Width => 1)
               & ":"
               & Langkit_Support.Text.To_UTF8 (Id.Text)
               & " : "
               & Langkit_Support.Text.To_UTF8
                 (Param_Type.Text)
               & "}, ");
         end loop;

         return Snippet;
      end Get_Snippet_For_Component;

      ----------------------------------
      -- Get_Snippet_For_Discriminant --
      ----------------------------------

      function Get_Snippet_For_Discriminant
        (Disc               : Discriminant_Values;
         Idx                : Natural;
         Use_Named_Notation : Boolean) return LSP_String
      is
         Snippet : LSP_String;
         Values  : constant Alternatives_List'Class :=
           Libadalang.Analysis.Values (Disc);
         Value_Node : Ada_Node;
      begin

         if Use_Named_Notation then
            if Values.Children_Count = 1 then
               Value_Node := Values.Child (Values.First_Child_Index);

               Snippet := To_LSP_String
                 (Langkit_Support.Text.To_UTF8 (Discriminant (Disc).Text))
                 & " => ";

               if Value_Node.Kind in Ada_Others_Designator_Range then
                  Snippet := Snippet & To_LSP_String
                    ("${"
                     & GNATCOLL.Utils.Image
                       (Idx, Min_Width => 1)
                     & ":"
                     & Langkit_Support.Text.To_UTF8 (Values.Text)
                     & "}, ");
               else
                  Snippet := Snippet & To_LSP_String
                    (Langkit_Support.Text.To_UTF8 (Values.Text)
                     & ", ");
               end if;
            else
               Snippet := To_LSP_String
                 (Langkit_Support.Text.To_UTF8 (Discriminant (Disc).Text)
                  & " => "
                  & "${"
                  & GNATCOLL.Utils.Image
                    (Idx, Min_Width => 1)
                  & ":"
                  & Langkit_Support.Text.To_UTF8 (Values.Text)
                  & "}, ");
            end if;
         else
            if Values.Children_Count = 1 then
               Value_Node := Values.Child (Values.First_Child_Index);

               if Value_Node.Kind in Ada_Others_Designator_Range then
                  Snippet := Snippet & To_LSP_String
                    ("${"
                     & GNATCOLL.Utils.Image
                       (Idx, Min_Width => 1)
                     & ":"
                     & Langkit_Support.Text.To_UTF8 (Values.Text)
                     & "}, ");
               else
                  Snippet := Snippet & To_LSP_String
                    (Langkit_Support.Text.To_UTF8 (Values.Text)
                     & ", ");
               end if;
            else
               Snippet := To_LSP_String
                 ("${"
                  & GNATCOLL.Utils.Image
                    (Idx, Min_Width => 1)
                  & ":"
                  & Langkit_Support.Text.To_UTF8 (Values.Text)
                  & "}, ");
            end if;
         end if;

         return Snippet;
      end Get_Snippet_For_Discriminant;

      -------------------------
      -- Get_Label_For_Shape --
      -------------------------

      function Get_Label_For_Shape
        (Discriminants : Discriminant_Values_Array) return LSP_String
      is
         Label  : LSP_String;
         Length : constant Integer := Discriminants'Length;
      begin
         if Length = 0 then
            return To_LSP_String
              ("Aggregate for "
               & Langkit_Support.Text.To_UTF8 (Aggr_Type.F_Name.Text));
         end if;

         Label := To_LSP_String (String'("Aggregate when "));

         for Idx in Discriminants'Range loop
            declare
               Disc_Values : constant Discriminant_Values :=
                 Discriminants (Idx);
            begin
               Label := Label & To_LSP_String
                 (Langkit_Support.Text.To_UTF8
                    (Discriminant (Disc_Values).Text))
                 & " => ";

               Label := Label & To_LSP_String
                 (Langkit_Support.Text.To_UTF8 (Values (Disc_Values).Text));

               if Idx < Discriminants'Length then
                  Label := Label & ", ";
               end if;
            end;
         end loop;

         return Label;
      end Get_Label_For_Shape;

   begin
      if Aggr_Type.Kind in Ada_Type_Decl_Range then
         declare
            Shapes        : constant Libadalang.Analysis.Shape_Array :=
              Aggr_Type.P_Shapes
                (Include_Discriminants => False,
                 Origin                => Node);
            Item          : CompletionItem;
            Idx           : Positive := 1;
            Nb_Components : Natural := 0;
            Insert_Text   : LSP_String;
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
                     Compute_Completion_Detail
                       (Aggr_Type.As_Basic_Decl));
                  Item.insertTextFormat :=
                    Optional_InsertTextFormat'
                      (Is_Set => True,
                       Value  => Snippet);
                  Insert_Text := Empty_LSP_String;

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
                     Insert_Text := LSP.Types.To_LSP_String
                       (Langkit_Support.Text.To_UTF8 (Base_Type.F_Name.Text))
                       & " with ";

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
                           Insert_Text := Insert_Text
                             & Get_Snippet_For_Component
                             (Param              => Disc,
                              Idx                => Idx,
                              Use_Named_Notation => Use_Named_Notation);

                           Idx := Idx + 1;
                        end loop;
                     end;
                  end if;

                  --  Compute the snippets for the record discriminants, if any
                  for Disc of Discriminants loop
                     Insert_Text := Insert_Text
                       & Get_Snippet_For_Discriminant
                       (Disc               => Disc,
                        Idx                => Idx,
                        Use_Named_Notation => Use_Named_Notation);
                     Idx := Idx + 1;
                  end loop;

                  --  Compute the snippets for the record components
                  for Comp of Components loop
                     Insert_Text := Insert_Text
                       & Get_Snippet_For_Component
                       (Param              => Comp,
                        Idx                => Idx,
                        Use_Named_Notation => Use_Named_Notation);

                     Idx := Idx + 1;
                  end loop;

                  if Idx > 1 then
                     --  Remove the "}, " substring that has been
                     --  appended in the last loop iteration.
                     Insert_Text := Unbounded_Slice
                       (Insert_Text,
                        1,
                        Length (Insert_Text) - 2);

                     --  Insert '$0' (i.e: the final tab stop) at the
                     --  end.
                     Insert_Text := Insert_Text & ")$0";
                     Item.insertText :=
                       (Is_Set => True,
                        Value  => Insert_Text);
                     Result.Append (Item);
                  end if;
               end;
            end loop;
         end;
      end if;
   end Get_Aggregate_Completion;

   -----------------------------
   -- Get_Keywords_Completion --
   -----------------------------

   procedure Get_Keywords_Completion
     (Node   : Libadalang.Analysis.Ada_Node;
      Prefix : Ada.Strings.UTF_Encoding.UTF_8_String;
      Result : out LSP.Messages.CompletionItem_Vector)
   is
      Keywords : constant Unbounded_Text_Type_Array := Node.P_Valid_Keywords;
      Item     : LSP.Messages.CompletionItem;
   begin
      for Keyword of Keywords loop
         declare
            Label : constant Langkit_Support.Text.Text_Type :=
              Langkit_Support.Text.To_Text (Keyword);
         begin
            if LSP.Types.Starts_With
              (Text           => To_LSP_String (Label),
               Prefix         => Prefix,
               Case_Sensitive => False)
            then
               Item.label := To_LSP_String (Label);
               Item.insertTextFormat := (True, LSP.Messages.PlainText);
               Item.insertText := (True, Item.label);
               Item.kind := (True, LSP.Messages.Keyword);
               Result.Append (Item);
            end if;
         end;
      end loop;
   end Get_Keywords_Completion;

   ------------------------
   -- Get_Completions_At --
   ------------------------

   procedure Get_Completions_At
     (Self                     : Document;
      Context                  : LSP.Ada_Contexts.Context;
      Position                 : LSP.Messages.Position;
      Named_Notation_Threshold : Natural;
      Snippets_Enabled      : Boolean;
      Should_Use_Names         : in out Boolean;
      Names                    : out Ada_Completion_Sets.Completion_Maps.Map;
      Result                   : out LSP.Messages.CompletionList)
   is
      use Libadalang.Common;
      use LSP.Types;
      use type VSS.Unicode.UTF16_Code_Unit_Count;

      Real_Pos : constant LSP.Messages.Position :=
        (Position.line,
         UTF_16_Index'Max (0, Position.character - 1));
      --  Compute the position we want for completion, which is one character
      --  before the cursor.

      Node       : Libadalang.Analysis.Ada_Node :=
        Self.Get_Node_At (Context, Real_Pos);
      --  Get the corresponding LAL node

      Parent   : Libadalang.Analysis.Ada_Node;
      --  The parent of the node to complete.

      Sibling  : Libadalang.Analysis.Ada_Node;
      --  The right sibling of the node to complete.

      In_End_Label : Boolean := False;
      --  Set to True if we are completing an end label
      --  (e.g: end <Subp_Name>);

      Prefix   : constant Ada.Strings.UTF_Encoding.UTF_8_String :=
        Langkit_Support.Text.To_UTF8 (Node.Text);

      Use_Snippets : Boolean := Snippets_Enabled;
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

      while not Node.Is_Null
        and then Node.Kind in Ada_Single_Tok_Node | Ada_Dotted_Name
      loop
         if Node.Parent.Kind = Ada_Dotted_Name
           and then Node.Parent.As_Dotted_Name.F_Suffix = Node
         then
            Node := Node.Parent;
         else
            exit;
         end if;
      end loop;

      --  Return immediately if we are dealing with a null node or if the
      --  node's parent is a Defining_Name, meaning that we are declaring a
      --  new symbol.

      if Node.Is_Null or else
        (not Node.Parent.Is_Null
                and then Node.Parent.Kind in Ada_Defining_Name_Range)
      then
         return;
      end if;

      Context.Trace.Trace
        ("Getting completions, Pos = ("
         & Real_Pos.line'Image & ", " & Real_Pos.character'Image & ") Node = "
         & Image (Node));

      Parent := Node.Parent;
      Sibling := Node.Next_Sibling;

      --  Check if we are completing an end label. If it's the case, we want
      --  to disable snippets since end labels don't expect any parameters.
      In_End_Label := not Parent.Is_Null
        and then Parent.Kind in Ada_End_Name_Range;

      --  Check if we are dealing with an aggregate node. If yes, handle it
      --  separately to propose snipppets to the user, allowing him to fill
      --  all the needed discriminants/components easily.

      if Node.Kind in Ada_Aggregate_Range then
         Get_Aggregate_Completion
           (Node                     => As_Aggregate (Node),
            Context                  => Context,
            Named_Notation_Threshold => Named_Notation_Threshold,
            Result                   => Result.items);
         return;
      end if;

      --  Get completion for aspects if we are within an aspect association
      --  node and return immediatly since we only expect aspects here.
      if not Parent.Is_Null and then Parent.Kind in Ada_Aspect_Assoc_Range then
         LSP.Predefined_Completion.Get_Aspects
           (Prefix => Prefix,
            Result => Result.items);
         return;
      end if;

      --  Get completion for pragmas if we are within an pragma node and return
      --  immediately, since we don't want to propose other items than pragmas
      --  when wthin a pragma node.
      if not Parent.Is_Null and then Parent.Kind in Ada_Pragma_Node_Range then
         LSP.Predefined_Completion.Get_Pragmas
           (Prefix => Prefix,
            Result => Result.items);
         return;
      end if;

      declare
         Previous_Char : constant String :=
           (if Position.character > 1 then
               Self.Get_Text_At
              (Start_Pos => LSP.Messages.Position'
                   (line      => Position.line,
                    character => Position.character - 2),
               End_Pos   => LSP.Messages.Position'
                 (line      => Position.line,
                  character => Position.character - 2))
            else
               "");
      begin
         --  Propose keyword completion if we are not within and end label
         --  and if there is no previous character of if it's a whitespace (we
         --  don't want to propose keywords after typing '(' to feed subprogram
         --  parameters for instance).

         if not In_End_Label
           and then (Previous_Char = ""
                     or else GNATCOLL.Utils.Is_Whitespace
                       (Previous_Char (Previous_Char'Last)))
         then
            Get_Keywords_Completion
              (Node   => Node,
               Prefix => Prefix,
               Result => Result.items);
         end if;
      end;

      --  Return without asing Libadalang for completion results we are dealing
      --  with a syntax error.
      if Node.Kind in Ada_Error_Decl_Range then
         return;
      end if;

      Should_Use_Names := True;  --  Let's use defining names for completion

      if In_End_Label
        or else not Sibling.Is_Null
        or else
          (not Parent.Is_Null and then Parent.Kind in Ada_Param_Assoc_Range)
      then
         --  Snippets should not be used in the following cases:
         --
         --   . The Use_Snippets parameter if set to False
         --
         --   . When the queried node is within an end label
         --
         --   . When the queried node has a sibling: this is to avoid proposing
         --     snippets when a list of parameters is already present on the
         --     right of the completion point for instance.
         --
         --   . When we are providing an actual parameter to a subprogram call

         Use_Snippets := False;
      end if;

      declare
         Raw_Completions     : constant Completion_Item_Array :=
           Node.P_Complete;
         BD                  : Basic_Decl;
      begin
         Context.Trace.Trace
           ("Number of LAL completions : " & Raw_Completions'Length'Image);

         for CI of Raw_Completions loop
            BD := Decl (CI).As_Basic_Decl;
            if not BD.Is_Null then
               for DN of BD.P_Defining_Names loop

                  --  If we are not completing a dotted name, filter the
                  --  raw completion results by the node's prefix.
                  if Node.Kind in Ada_Dotted_Name_Range
                    or else Starts_With
                      (To_LSP_String (DN.P_Relative_Name.Text),
                       Prefix         => Prefix,
                       Case_Sensitive => False)
                  then
                     Names.Include
                       (DN, (Is_Dot_Call (CI), True, Use_Snippets));
                  end if;
               end loop;
            end if;
         end loop;

         Context.Trace.Trace
           ("Number of filtered completions : " &
              Names.Length'Image);
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
