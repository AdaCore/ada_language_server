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

with LSP.Enumerations;
with VSS.Characters.Latin;
with VSS.Strings.Cursors.Iterators.Characters;

package body LSP.Ada_Handlers.Formatting is

   GNATformat_Exception_Found_Msg : constant VSS.Strings.Virtual_String :=
     "GNATformat: exception raised when parsing source code";
   --  Error message when an exception is raised in GNATformat.

   Fallback_Exception_Found_Msg : constant VSS.Strings.Virtual_String :=
     "Exception raised in the fallback indenter";
   --  Error message when an exception is raised in the fallback indenter

   Incorrect_Code_Msg : constant VSS.Strings.Virtual_String :=
     "GNATformat: Syntactically invalid code can't be formatted";
   --  Error message sent when trying to format invalid code.

   -------------------
   -- Reindent_Line --
   -------------------

   function Reindent_Line
     (Filename   : GNATCOLL.VFS.Virtual_File;
      Line       : VSS.Strings.Virtual_String;
      Options    : Gnatformat.Configuration.Format_Options_Type;
      Pos        : LSP.Structures.Position;
      New_Indent : Natural) return LSP.Structures.TextEdit
   is
      use type VSS.Characters.Virtual_Character;
      use VSS.Strings;

      --  Get the full line because we want to remove its existing blank
      --  characters.
      New_Prefix      : constant Virtual_String :=
        VSS.Strings."*" (Character_Count (New_Indent), ' ');
      Character_It    :
        VSS.Strings.Cursors.Iterators.Characters.Character_Iterator :=
          Line.Before_First_Character;
      First_Non_Blank : Natural := 0;
   begin
      --  Count the number of Blank characters at the start
      while Character_It.Forward loop
         if Character_It.Element = VSS.Characters.Latin.Space
           or else Character_It.Element
                   = VSS.Characters.Latin.Character_Tabulation
         then
            First_Non_Blank := First_Non_Blank + 1;
         else
            exit;
         end if;
      end loop;

      return
        LSP.Structures.TextEdit'
          (a_range => (Pos, (Pos.line, First_Non_Blank)),
           newText =>
             Handle_Tabs
               (Filename => Filename,
                Options  => Options,
                S        => New_Prefix));
   end Reindent_Line;

   ------------
   -- Format --
   ------------

   procedure Format
     (Context  : LSP.Ada_Contexts.Context;
      Document : not null LSP.Ada_Documents.Document_Access;
      Span     : LSP.Structures.A_Range;
      Options  : Gnatformat.Configuration.Format_Options_Type;
      Success  : out Boolean;
      Response : out LSP.Structures.TextEdit_Vector;
      Messages : out VSS.String_Vectors.Virtual_String_Vector;
      Error    : out LSP.Errors.ResponseError)
   is
      pragma Unreferenced (Messages);
      use type LSP.Structures.A_Range;
   begin
      if Document.Has_Diagnostics (Context) then
         Success := False;
         Error :=
           (code    =>
              LSP.Enumerations.ErrorCodes (LSP.Enumerations.RequestFailed),
            message => Incorrect_Code_Msg);

         return;
      end if;

      if Span /= LSP.Constants.Empty then
         Response := Document.Range_Format (Context, Span, Options);
      else
         Response := Document.Format (Context, Options);
      end if;
      Success := True;

   exception
      when E : others =>
         Context.Tracer.Trace_Exception (E, GNATformat_Exception_Found_Msg);
         Success := False;
         Error :=
           (code    =>
              LSP.Enumerations.ErrorCodes (LSP.Enumerations.RequestFailed),
            message => GNATformat_Exception_Found_Msg);
   end Format;

   ------------------
   -- Range_Format --
   ------------------

   procedure Range_Format
     (Context  : LSP.Ada_Contexts.Context;
      Document : not null LSP.Ada_Documents.Document_Access;
      Span     : LSP.Structures.A_Range;
      Options  : Gnatformat.Configuration.Format_Options_Type;
      Success  : out Boolean;
      Response : out LSP.Structures.TextEdit_Vector;
      Error    : out LSP.Errors.ResponseError) is
   begin
      if Document.Has_Diagnostics (Context) then
         Success := False;
         Error :=
           (code    =>
              LSP.Enumerations.ErrorCodes (LSP.Enumerations.RequestFailed),
            message => Incorrect_Code_Msg);

         return;
      end if;
      Response.Clear;
      Response.Append
        (Document.Range_Format (Context, Span, Options => Options));
      Success := True;

   exception
      when E : others =>
         Context.Tracer.Trace_Exception (E, GNATformat_Exception_Found_Msg);
         Success := False;
         Error :=
           (code    =>
              LSP.Enumerations.ErrorCodes (LSP.Enumerations.RequestFailed),
            message => GNATformat_Exception_Found_Msg);
   end Range_Format;

   ---------------------
   -- Get_Indentation --
   ---------------------

   function Get_Indentation
     (Filename : GNATCOLL.VFS.Virtual_File;
      Buffer   : VSS.Strings.Virtual_String;
      Span     : LSP.Structures.A_Range;
      Options  : Gnatformat.Configuration.Format_Options_Type)
      return LSP.Formatters.Fallback_Indenter.Indentation_Array
   is
      Indentation      : constant Positive :=
        Gnatformat.Configuration.Get_Indentation
          (Options, Filename.Display_Full_Name);
      Indentation_Cont : constant Positive :=
        Gnatformat.Configuration.Get_Indentation_Continuation
          (Options, Filename.Display_Full_Name);
   begin
      return
        LSP.Formatters.Fallback_Indenter.Get_Indentation
          (Buffer          => VSS.Strings.Conversions.To_UTF_8_String (Buffer),
           From            => Span.start.line + 1,
           To              => Span.an_end.line + 1,
           Indent_Level    => Indentation,
           Indent_Continue => Indentation_Cont);
   end Get_Indentation;

   ------------------
   -- Indent_Lines --
   ------------------

   procedure Indent_Lines
     (Tracer   : not null LSP.Tracers.Tracer_Access;
      Filename : GNATCOLL.VFS.Virtual_File;
      Document : LSP.Text_Documents.Text_Document'Class;
      Options  : Gnatformat.Configuration.Format_Options_Type;
      Span     : LSP.Structures.A_Range := LSP.Text_Documents.Empty_Range;
      Success  : out Boolean;
      Response : out LSP.Structures.TextEdit_Vector;
      Messages : out VSS.String_Vectors.Virtual_String_Vector;
      Error    : out LSP.Errors.ResponseError)
   is
      pragma Unreferenced (Messages);
      use LSP.Structures;
      use LSP.Text_Documents;
      use VSS.Strings;

      Actual_Span : constant A_Range :=
        (if Span = Empty_Range
         then ((0, 0), (Integer'Max (Document.Line_Count - 1, 0), 0))
         else Span);
      --  If no span is provided, indent the whole document.

      Buffer : constant VSS.Strings.Virtual_String :=
        (if Span = Empty_Range
         then Document.Text
         else
           Document.Slice
             (((0, 0),
               (Natural'Min (Document.Line_Count - 1,
                Actual_Span.an_end.line + 1),
                0))));
      --  Get the relevant buffer to indent.
      --  If no span is provided, get the whole document buffer.
      --  Otherwise get the buffer from the start of the document
      --  to the end of the given span.

      Indent_Lines :
        constant LSP.Formatters.Fallback_Indenter.Indentation_Array :=
          Get_Indentation
            (Filename => Filename,
             Buffer   => Buffer,
             Span     => Actual_Span,
             Options  => Options);
      --  Get the indentation levels for each line in the span.

   begin
      Tracer.Trace_Text (Incorrect_Code_Msg & ", using the fallback indenter");
      for Line in Indent_Lines'Range loop
         if Indent_Lines (Line) /= -1 then
            --  Generate a text edit to reindent the line.
            Response.Append
              (Reindent_Line
                 (Filename   => Filename,
                  Line       => Document.Get_Line (Line),
                  Options    => Options,
                  Pos        => (Line, 0),
                  New_Indent => Indent_Lines (Line)));
         end if;
      end loop;
      Success := True;
   exception
      when E : others =>
         Tracer.Trace_Exception (E, Fallback_Exception_Found_Msg);
         Success := False;
         Error :=
           (code    =>
              LSP.Enumerations.ErrorCodes (LSP.Enumerations.RequestFailed),
            message => Fallback_Exception_Found_Msg);
   end Indent_Lines;

   -----------------
   -- Handle_Tabs --
   -----------------

   function Handle_Tabs
     (Filename : GNATCOLL.VFS.Virtual_File;
      Options  : Gnatformat.Configuration.Format_Options_Type;
      S        : VSS.Strings.Virtual_String) return VSS.Strings.Virtual_String
   is
      use Gnatformat.Configuration;
      use type VSS.Characters.Virtual_Character;
      use VSS.Strings;

      Indentation  : constant Character_Count :=
        Character_Count
          (Get_Indentation (Options, Filename.Display_Full_Name));
      Use_Tabs     : constant Boolean :=
        Get_Indentation_Kind (Options, Filename.Display_Full_Name) = Tabs;
      Res          : VSS.Strings.Virtual_String;
      Character_It :
        VSS.Strings.Cursors.Iterators.Characters.Character_Iterator :=
          S.Before_First_Character;
   begin
      if Use_Tabs then
         declare
            Count_Space : Character_Count := 0;
         begin
            --  Convert spaces to tabs
            while Character_It.Forward loop
               if Character_It.Element = VSS.Characters.Latin.Space then
                  Count_Space := Count_Space + 1;
                  if Count_Space = Indentation then
                     Res.Append (VSS.Characters.Latin.Character_Tabulation);
                     Count_Space := 0;
                  end if;
               else
                  exit;
               end if;
            end loop;

            --  Add the remaining spaces
            if Count_Space > 0 then
               Res.Append (Count_Space * VSS.Characters.Latin.Space);
            end if;

            --  Add the rest of the line
            Res.Append (S.Slice (Character_It, S.At_Last_Character));
         end;
      else
         --  Convert tabs to spaces
         while Character_It.Forward loop
            if Character_It.Element = VSS.Characters.Latin.Character_Tabulation
            then
               Res.Append (Indentation * VSS.Characters.Latin.Space);
            else
               exit;
            end if;
         end loop;

         --  Add the rest of the line
         Res.Append (S.Slice (Character_It, S.At_Last_Character));
      end if;

      return Res;
   end Handle_Tabs;

   ----------------------------
   -- Get_Formatting_Options --
   ----------------------------

   function Get_Formatting_Options
     (Context     : LSP.Ada_Contexts.Context;
      LSP_Options : LSP.Structures.FormattingOptions)
      return Gnatformat.Configuration.Format_Options_Type
   is
      use Gnatformat;
      use Gnatformat.Configuration;

      function Get_LSP_Options return Format_Options_Type;

      ---------------------
      -- Get_LSP_Options --
      ---------------------

      function Get_LSP_Options return Format_Options_Type is
         Format_Options_Builder : Format_Options_Builder_Type :=
           Create_Format_Options_Builder;
      begin
         if LSP_Options.tabSize /= 0 then
            --  FormattingOptions is not optional, however in case a client
            --  forgot to set it try to be resilient.
            Format_Options_Builder.With_Indentation
              (LSP_Options.tabSize, Ada_Language);
         end if;
         Format_Options_Builder.With_Indentation_Kind
           ((if LSP_Options.insertSpaces then Spaces else Tabs), Ada_Language);

         if LSP_Options.gnatFormatMaxSize.Is_Set then
            Format_Options_Builder.With_Width
              (LSP_Options.gnatFormatMaxSize.Value, Ada_Language);
         end if;

         if LSP_Options.gnatFormatContinuationLineIndent.Is_Set then
            Format_Options_Builder.With_Indentation_Continuation
              (LSP_Options.gnatFormatContinuationLineIndent.Value,
               Ada_Language);
         end if;

         return Format_Options_Builder.Build;
      end Get_LSP_Options;

      Full_Format_Options : Format_Options_Type := Default_Format_Options;
      --  Start with the default options
   begin
      --  Overwrite is lazy and will not affect options which are not
      --  explicitly defined. By order of precedence use Default then overwrite
      --  it with the LSP options and finally overwrite them with the project
      --  options.
      Full_Format_Options.Overwrite (Get_LSP_Options);
      Full_Format_Options.Overwrite (Context.Get_Format_Options);
      return Full_Format_Options;
   end Get_Formatting_Options;

end LSP.Ada_Handlers.Formatting;
