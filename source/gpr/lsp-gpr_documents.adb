------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                       Copyright (C) 2023, AdaCore                        --
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

with GNATCOLL.Traces;

with GPR2.Message;

with VSS.Characters.Latin;
with VSS.Strings.Character_Iterators;
with VSS.Strings.Conversions;
with VSS.Strings.Line_Iterators;
with VSS.Unicode;

package body LSP.GPR_Documents is

   Document_Changes_Trace : constant GNATCOLL.Traces.Trace_Handle :=
     GNATCOLL.Traces.Create ("ALS.DOCUMENT_CHANGES",
                             GNATCOLL.Traces.Off);
   --  Logging each document change

   LSP_New_Line_Function_Set : constant VSS.Strings.Line_Terminator_Set :=
     (VSS.Strings.CR | VSS.Strings.CRLF | VSS.Strings.LF => True,
      others => False);
   --  LSP allows to use three kinds of line terminators: CR, CR+LF and LF.

   procedure Span_To_Markers
     (Self : Document'Class;
      Span : LSP.Structures.A_Range;
      From : out VSS.Strings.Markers.Character_Marker;
      To   : out VSS.Strings.Markers.Character_Marker);

   procedure Recompute_Indexes (Self : in out Document);
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

   -------------------
   -- Apply_Changes --
   -------------------

   procedure Apply_Changes
     (Self    : aliased in out Document;
      Version : Integer;
      Vector  : LSP.Structures.TextDocumentContentChangeEvent_Vector)
   is
      URI : constant String :=
        VSS.Strings.Conversions.To_UTF_8_String (Self.URI);

   begin
      Document_Changes_Trace.Trace ("Applying changes for document " & URI);

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

      Document_Changes_Trace.Trace
        ("Done applying changes for document " & URI);
   end Apply_Changes;

   -------------
   -- Cleanup --
   -------------

   procedure Cleanup (Self : in out Document) is
   begin
      Self.Tree.Unload (True);
   end Cleanup;

   ----------------
   -- Get_Errors --
   ----------------

   procedure Get_Errors
     (Self      : in out Document;
      Root_File : GPR2.Path_Name.Object;
      Changed   : out Boolean;
      Errors    : out Message_Map) is
   begin
      Changed := Self.Errors_Changed;
      Self.Errors_Changed := False;

      Errors.Clear;

      --  Insert empty log for unopened files that had logs on previous
      --  publish diags call to let client clear problems

      for File of Self.Published_Files_With_Diags loop
         if not Self.File_Provider.Is_Openened_Document (File.Virtual_File)
         then
            Errors.Insert (File, GPR2.Log.Undefined);
         end if;
      end loop;

      --  Root_File should always have diagnostic published to clear diags on
      --  corrected file.

      if not Errors.Contains (Root_File) then
         Errors.Insert (Root_File, GPR2.Log.Undefined);
      end if;

      if Changed then

         --  for error & warning messages
         for C in Self.Tree.Log_Messages.Iterate (False) loop
            declare
               Message  : constant GPR2.Message.Object := C.Element;
               File     : constant GPR2.Path_Name.Object :=
                            (if Message.Sloc.Is_Defined and then
                                Message.Sloc.Has_Source_Reference
                             then GPR2.Path_Name.Create_File
                               (GPR2.Filename_Type (Message.Sloc.Filename))
                             else Root_File);
               --  message without location will be attached to root_file.

               C        : constant Message_Maps.Cursor :=
                             Errors.Find (File);
            begin
               if C.Has_Element then
                  declare
                     Log : GPR2.Log.Object := C.Element;
                     --  existing log to update
                  begin
                     Log.Append (Message);
                     Errors.Replace_Element (C, Log);
                  end;
               else
                  declare
                     Log : GPR2.Log.Object;
                     --  new log to be inserted
                  begin
                     Log.Append (Message);
                     Errors.Insert (File, Log);
                  end;
               end if;
            end;
         end loop;
      end if;
   end Get_Errors;

   -------------------------
   -- Get_Source_Location --
   -------------------------

   function Get_Source_Location
     (Self     : Document'Class;
      Position : LSP.Structures.Position)
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

      return
        ((Line   => Langkit_Support.Slocs.Line_Number (Position.line + 1),
          Column => Langkit_Support.Slocs.Column_Number
                      (Iterator.Character_Index - Line_First_Character)));
   end Get_Source_Location;

   -----------------
   -- Get_Text_At --
   -----------------

   function Get_Text_At
     (Self      : Document;
      Start_Pos : LSP.Structures.Position;
      End_Pos   : LSP.Structures.Position) return VSS.Strings.Virtual_String
   is
      First_Marker : VSS.Strings.Markers.Character_Marker;
      Last_Marker  : VSS.Strings.Markers.Character_Marker;

   begin
      Self.Span_To_Markers
        ((Start_Pos, End_Pos), First_Marker, Last_Marker);

      return Self.Text.Slice (First_Marker, Last_Marker);
   end Get_Text_At;

   -----------------
   -- Get_Word_At --
   -----------------

   function Get_Word_At
     (Self     : Document;
      Position : LSP.Structures.Position)
      return VSS.Strings.Virtual_String
   is
      Result : VSS.Strings.Virtual_String;

   begin
      return Result;
   end Get_Word_At;

   ---------------------
   -- Has_Diagnostics --
   ---------------------

   function Has_Diagnostics
     (Self    : Document)
      return Boolean is
   begin
      return Self.Has_Messages;
   end Has_Diagnostics;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self        : in out Document;
      URI         : LSP.Structures.DocumentUri;
      File        : GPR2.Path_Name.Object;
      Text        : VSS.Strings.Virtual_String;
      Provider    : LSP.GPR_Files.File_Provider_Access) is
   begin
      Self.URI           := URI;
      Self.File          := File;
      Self.Version       := 1;
      Self.Text          := Text;
      Self.File_Provider := Provider;

      Recompute_Indexes (Self);
   end Initialize;

   ---------------------
   -- Line_Terminator --
   ---------------------

   function Line_Terminator
     (Self : Document'Class) return VSS.Strings.Virtual_String
   is
      use type VSS.Strings.Virtual_String;

   begin
      if Self.Line_Terminator.Is_Empty then
         --  Document has no line terminator yet, return LF as most used
         --
         --  Should it be platform specific? CRLF for Windows, CR for Mac?

         return 1 * VSS.Characters.Latin.Line_Feed;

      else
         return Self.Line_Terminator;
      end if;
   end Line_Terminator;

   ----------
   -- Load --
   ----------

   procedure Load  (Self : in out Document) is

      procedure Update_Diagnostics;
      --  Update Self.Messages, Self.Errors_Changed, Self.Has_Diagnostics

      ------------------------
      -- Update_Diagnostics --
      ------------------------

      procedure Update_Diagnostics is
         New_Messages : constant GPR2.Log.Object :=
                          Self.Tree.Log_Messages.all;
      begin
         if New_Messages /= Self.Messages then
            Self.Messages := New_Messages;
            Self.Errors_Changed := True;

            Self.Has_Messages := False;
            for C in Self.Tree.Log_Messages.Iterate (False) loop
               Self.Has_Messages := True;
               return;
            end loop;
         end if;
      end Update_Diagnostics;

   begin
      Self.Errors_Changed := True;

      --  Unload it to clean log.
      Self.Tree.Unload;

      Self.Tree.Load_Autoconf
        (Filename          => Self.File,
         Context           => Self.Context,
         File_Reader       => Self.File_Provider.Get_File_Reader,
         Environment       => Self.Environment);

      Update_Diagnostics;

   exception
      when GPR2.Project_Error | GPR2.Processing_Error
         | GPR2.Attribute_Error =>

         Update_Diagnostics;

   end Load;

   -----------------------
   -- Recompute_Indexes --
   -----------------------

   procedure Recompute_Indexes (Self : in out Document) is
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

   -----------------------------
   -- Update_Files_With_Diags --
   -----------------------------

   procedure Update_Files_With_Diags
     (Self : in out Document'Class; Files : GPR2.Path_Name.Set.Object) is
   begin
      Self.Published_Files_With_Diags := Files;
   end Update_Files_With_Diags;

   --------------------------
   -- Versioned_Identifier --
   --------------------------

   function Versioned_Identifier
     (Self : Document) return LSP.Structures.VersionedTextDocumentIdentifier is
   begin
      return (uri     => Self.URI,
              version => Self.Version);
   end Versioned_Identifier;

end LSP.GPR_Documents;
