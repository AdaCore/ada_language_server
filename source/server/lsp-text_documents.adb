------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                        Copyright (C) 2023, AdaCore                       --
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

with Ada.Unchecked_Deallocation;

with VSS.Characters.Latin;
with VSS.Strings.Character_Iterators;
with VSS.Strings.Line_Iterators;
with VSS.String_Vectors;
with VSS.Unicode;

package body LSP.Text_Documents is

   procedure Range_To_Markers
     (Self : Text_Document'Class;
      Span : LSP.Structures.A_Range;
      From : out VSS.Strings.Markers.Character_Marker;
      To   : out VSS.Strings.Markers.Character_Marker);

   procedure Recompute_Indexes (Self : in out Text_Document'Class);
   --  Recompute the line-to-offset indexes in Self

   procedure Recompute_Markers
     (Self         : in out Text_Document'Class;
      Low_Line     : Natural;
      Start_Marker : VSS.Strings.Markers.Character_Marker;
      End_Marker   : VSS.Strings.Markers.Character_Marker);
   --  Recompute line-to-marker index starting from Start_Marker till
   --  End_Marker and filling index table starting at Low_Line. End_Marker
   --  may be invalid marker, in this case indexing down to the end of the
   --  text.

   -------------------
   -- Apply_Changes --
   -------------------

   procedure Apply_Changes
     (Self    : in out Text_Document'Class;
      Version : Integer;
      Vector  : LSP.Structures.TextDocumentContentChangeEvent_Vector) is
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

               Self.Range_To_Markers
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

               if Low_Line /= Self.Line_Marker.First_Index then
                  Low_Line     := Low_Line - 1;
                  Start_Index  := Low_Line;
                  Start_Marker := Self.Line_Marker (Low_Line);

               else
                  Start_Index  := Self.Line_Marker.First_Index;
                  Start_Marker := Self.Text.At_First_Character.Marker;
               end if;

               --  Use marker of the line after the last modified line as end
               --  marker for recompute because marker of the last modified
               --  line may be ether invalidated or moved and not point to the
               --  beginning of the line, or use invalid marker when last line
               --  was modified.

               if High_Line /= Self.Line_Marker.Last_Index then
                  Delete_High := High_Line;
                  High_Line   := High_Line + 1;
                  End_Marker  := Self.Line_Marker (High_Line);
               end if;

               if Low_Line = Self.Line_Marker.First_Index
                 and then High_Line = Self.Line_Marker.Last_Index
               then
                  Self.Recompute_Indexes;

               else
                  if Delete_High >= Low_Line then
                     Self.Line_Marker.Delete
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

   ------------------
   -- Constructors --
   ------------------

   package body Constructors is

      ----------------
      -- Initialize --
      ----------------

      procedure Initialize
        (Self : in out Text_Document'Class;
         URI  : LSP.Structures.DocumentUri;
         Text : VSS.Strings.Virtual_String) is
      begin
         Self.URI  := URI;
         Self.Text := Text;

         Self.Recompute_Indexes;
      end Initialize;

   end Constructors;

   ----------
   -- Diff --
   ----------

   procedure Diff
     (Self     : Text_Document'Class;
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
            and then not Self.Line_Marker.Is_Empty
            and then Edit.Last_Element.a_range.an_end.line not in
              Self.Line_Marker.First_Index .. Self.Line_Marker.Last_Index
         then
            declare
               Element   : LSP.Structures.TextEdit := Edit.Last_Element;
               Last_Line : constant VSS.Strings.Virtual_String :=
                 Old_Lines (Old_Lines.Length);
               Iterator  :
                 constant VSS.Strings.Character_Iterators.Character_Iterator :=
                   Last_Line.At_Last_Character;

            begin
               --  Replace the wrong location by the end of the buffer

               Element.a_range.an_end :=
                 (line      => Old_Lines.Length - 1,
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
     (Self     : Text_Document'Class;
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
      Self.Range_To_Markers (Span, First_Marker, Last_Marker);

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
           (if Span.an_end.character = 0
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

   ----------------
   -- Identifier --
   ----------------

   function Identifier
     (Self : Text_Document'Class)
      return LSP.Structures.OptionalVersionedTextDocumentIdentifier is
   begin
      return (uri     => Self.URI,
              version =>
                (Is_Null => False,
                 Value   => Self.Version));
   end Identifier;

   ---------------------
   -- Line_Terminator --
   ---------------------

   function Line_Terminator
     (Self : Text_Document'Class) return VSS.Strings.Virtual_String
   is
      use type VSS.Strings.Virtual_String;

   begin
      return
        (if Self.Line_Terminator.Is_Empty then
            --  Document has no line terminator yet, return LF as most used
            --
            --  Should it be platform specific? CRLF for Windows, CR for Mac?

            1 * VSS.Characters.Latin.Line_Feed

         else
            Self.Line_Terminator);
   end Line_Terminator;

   ----------------------
   -- Range_To_Markers --
   ----------------------

   procedure Range_To_Markers
     (Self : Text_Document'Class;
      Span : LSP.Structures.A_Range;
      From : out VSS.Strings.Markers.Character_Marker;
      To   : out VSS.Strings.Markers.Character_Marker)
   is
      use type VSS.Unicode.UTF16_Code_Unit_Offset;

      J1 : VSS.Strings.Character_Iterators.Character_Iterator :=
        Self.Text.At_Character (Self.Line_Marker (Span.start.line));
      U1 : constant VSS.Unicode.UTF16_Code_Unit_Offset :=
        J1.First_UTF16_Offset;

      J2 : VSS.Strings.Character_Iterators.Character_Iterator :=
        Self.Text.At_Character (Self.Line_Marker (Span.an_end.line));
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
   end Range_To_Markers;

   -----------------------
   -- Recompute_Indexes --
   -----------------------

   procedure Recompute_Indexes (Self : in out Text_Document'Class) is
      use type VSS.Strings.Character_Count;

   begin
      Self.Line_Marker.Clear;

      --  To avoid too many reallocations during the initial filling
      --  of the index vector, pre-allocate it. Give a generous
      --  pre-allocation assuming that there is a line break every
      --  20 characters on average (this file has one line break
      --  every 33 characters).
      Self.Line_Marker.Reserve_Capacity
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
               Self.Line_Marker.Append (J.First_Marker);
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
            Self.Line_Marker.Append (J.First_Marker);
         end if;
      end;
   end Recompute_Indexes;

   -----------------------
   -- Recompute_Markers --
   -----------------------

   procedure Recompute_Markers
     (Self         : in out Text_Document'Class;
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

            Self.Line_Marker.Insert (Line, M);
            Line := Line + 1;

            exit when not J.Forward;
         end loop;

         if not End_Marker.Is_Valid then
            Self.Line_Marker.Append (J.First_Marker);
         end if;
      end if;
   end Recompute_Markers;

   -----------
   -- Slice --
   -----------

   function Slice
     (Self    : Text_Document'Class;
      A_Range : LSP.Structures.A_Range) return VSS.Strings.Virtual_String
   is
      First_Marker : VSS.Strings.Markers.Character_Marker;
      Last_Marker  : VSS.Strings.Markers.Character_Marker;

   begin
      Self.Range_To_Markers (A_Range, First_Marker, Last_Marker);

      return Self.Text.Slice (First_Marker, Last_Marker);
   end Slice;

end LSP.Text_Documents;
