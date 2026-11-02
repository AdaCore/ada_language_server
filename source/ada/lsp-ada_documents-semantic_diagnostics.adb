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

with VSS.Strings.Line_Iterators;
with VSS.Unicode;

with LSP.Utils;

package body LSP.Ada_Documents.Semantic_Diagnostics is

   ---------------------
   -- Get_Diagnostics --
   ---------------------

   overriding
   procedure Get_Diagnostics
     (Self    : in out Diagnostic_Source;
      Context : LSP.Ada_Contexts.Context;
      Errors  : out LSP.Structures.Diagnostic_Vector)
   is
      pragma Unreferenced (Context);
   begin
      if Self.Is_Enabled then
         Errors.Append_Vector (Self.Cached_Errors);
         Self.Has_Unpublished_Results := False;
      else
         Self.Cached_Errors := [];
         Self.Has_Unpublished_Results := False;
      end if;
   end Get_Diagnostics;

   ------------------------
   -- Has_New_Diagnostic --
   ------------------------

   overriding
   function Has_New_Diagnostic
     (Self : in out Diagnostic_Source; Context : LSP.Ada_Contexts.Context)
      return Boolean
   is
      Is_Enabled : constant Boolean :=
        Self.Handler.Semantic_Diagnostics_Enabled;
   begin
      --  The 'semanticDiagnostics' option has just changed: always return
      --  True in this case (e.g: to clear any existing diagnostic when the
      --  option gets disabled)
      if Self.Enabled /= Is_Enabled then
         Self.Enabled := Is_Enabled;
         Self.Has_Unpublished_Results := True;
      end if;

      return Self.Has_Unpublished_Results;
   end Has_New_Diagnostic;

   ----------------
   -- Is_Enabled --
   ----------------

   overriding
   function Is_Enabled (Self : Diagnostic_Source) return Boolean is
   begin
      return Self.Handler.Semantic_Diagnostics_Enabled;
   end Is_Enabled;

   ------------------------
   -- Rebase_Diagnostics --
   ------------------------

   procedure Rebase_Diagnostics
     (Self    : in out Diagnostic_Source;
      Changes : LSP.Structures.TextDocumentContentChangeEvent_Vector) is
   begin
      --  Each change uses the coordinates left by the preceding change,
      --  just as in Text_Documents.Apply_Changes. Rebase before merging any
      --  partial results so their replacement boundary uses the same version.
      for Change of Changes loop
         if not Change.a_range.Is_Set then
            Self.Cached_Errors.Clear;

         elsif not Self.Cached_Errors.Is_Empty then
            declare
               R       : LSP.Structures.A_Range renames Change.a_range.Value;
               New_End : LSP.Structures.Position := R.start;
               Lines   : VSS.Strings.Line_Iterators.Line_Iterator :=
                 Change.text.At_First_Line
                   (LSP.Text_Documents.LSP_New_Line_Function_Set);
               Updated : LSP.Structures.Diagnostic_Vector;

               -----------
               -- Shift --
               -----------

               function Shift
                 (Position : LSP.Structures.Position)
                  return LSP.Structures.Position
               is (line      => New_End.line + (Position.line - R.an_end.line),
                   character =>
                     (if Position.line = R.an_end.line
                      then
                        New_End.character
                        + (Position.character - R.an_end.character)
                      else Position.character));
               --  Move a position after the edit. Only positions on the
               --  edit's last line need their column changed as well.
            begin
               --  Use the same line endings as the document. VSS counts
               --  UTF-16 units, so characters such as emoji take two columns.
               while Lines.Has_Element loop
                  if Lines.Has_Line_Terminator then
                     New_End := (New_End.line + 1, 0);
                  else
                     declare
                        use type VSS.Unicode.UTF16_Code_Unit_Offset;
                     begin
                        New_End.character := New_End.character + Natural
                          (Lines.Last_UTF16_Offset
                           - Lines.First_UTF16_Offset + 1);
                     end;
                  end if;
                  exit when not Lines.Forward;
               end loop;

               for Old of Self.Cached_Errors loop
                  --  Edits exclude their end position. Keep diagnostics
                  --  touching either edge; an insertion moves those that
                  --  start at the insertion point.
                  if not LSP.Utils.Before (Old.a_range.start, R.an_end) then
                     declare
                        Item : LSP.Structures.Diagnostic := Old;
                     begin
                        Item.a_range :=
                          (Shift (Old.a_range.start),
                           Shift (Old.a_range.an_end));
                        Updated.Append (Item);
                     end;
                  elsif not LSP.Utils.Before (R.start, Old.a_range.an_end) then
                     Updated.Append (Old);
                  end if;
                  --  An overlapping diagnostic must be recomputed.
               end loop;
               Self.Cached_Errors := Updated;
            end;
         end if;
      end loop;
   end Rebase_Diagnostics;

   -------------------------
   -- Update_Diagnostics --
   -------------------------

   procedure Update_Diagnostics
     (Self             : in out Diagnostic_Source;
      Errors           : LSP.Structures.Diagnostic_Vector;
      Range_To_Replace : LSP.Structures.A_Range_Optional := (Is_Set => False))
   is
   begin
      if Range_To_Replace.Is_Set then
         declare
            Updated : LSP.Structures.Diagnostic_Vector := Errors;
            R       : LSP.Structures.A_Range renames Range_To_Replace.Value;
         begin
            for Old of Self.Cached_Errors loop
               if not LSP.Utils.Overlaps (Old.a_range, R) then
                  Updated.Append (Old);
               end if;
            end loop;
            Self.Cached_Errors := Updated;
         end;
      else
         Self.Cached_Errors := Errors;
      end if;
      Self.Has_Unpublished_Results := True;
   end Update_Diagnostics;

end LSP.Ada_Documents.Semantic_Diagnostics;
