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

--  This package provides implementation of the code formatting requests.

with Gnatformat.Configuration;
with Gnatformat.Edits;

with LSP.Ada_Contexts;
with LSP.Errors;
with LSP.Structures;
with LSP.Formatters.Fallback_Indenter;
with LSP.Text_Documents;
with LSP.Utils;

with VSS.Strings;

package LSP.Ada_Handlers.Formatting is

   procedure Format
     (Context  : LSP.Ada_Contexts.Context;
      Document : not null LSP.Ada_Documents.Document_Access;
      Options  : Gnatformat.Configuration.Format_Options_Type;
      Success  : out Boolean;
      Response : out LSP.Structures.TextEdit_Vector;
      Error    : out LSP.Errors.ResponseError);
   --  Format the text of the given document.

   procedure Range_Format
     (Context  : LSP.Ada_Contexts.Context;
      Document : not null LSP.Ada_Documents.Document_Access;
      Span     : LSP.Structures.A_Range;
      Options  : Gnatformat.Configuration.Format_Options_Type;
      Success  : out Boolean;
      Response : out LSP.Structures.TextEdit_Vector;
      Error    : out LSP.Errors.ResponseError)
        with Pre => not LSP.Utils.Is_Empty_Range (Span);
   --  Format the text of the given document in the given range (span).

   procedure Narrow_Range_Format
     (Unit    : Libadalang.Analysis.Analysis_Unit;
      Span    : Langkit_Support.Slocs.Source_Location_Range;
      Edit    : Gnatformat.Edits.Formatting_Edit_Type;
      Result  : out VSS.Strings.Virtual_String;
      Success : out Boolean);
   --  Auxiliary procedure for Range Formatting. The procedure takes the result
   --  of gnatformat as input and attempts to limit the formatting area to the
   --  specified range. Parameters
   --  * @param Unit - source code unit
   --  * @param Span - range to be formatted
   --  * @param Edit - gnatformat result covering the desired range
   --  * @param Result - calculated result
   --  * @param Success - processing status

   function Get_Indentation
     (Filename : GNATCOLL.VFS.Virtual_File;
      Buffer   : VSS.Strings.Virtual_String;
      Span     : LSP.Structures.A_Range;
      Options  : Gnatformat.Configuration.Format_Options_Type)
      return LSP.Formatters.Fallback_Indenter.Indentation_Array;
   --  Use the fallback indenter to get an array of indentation for each
   --  line in Span.
   --  Each line in the array is 1-based indexed (i.e., the first line is at
   --  index 1).
   --  Buffer is the content of the document referenced by Filename. Should
   --  contain the whole content of the document or a substring including
   --  at least the lines in Span.

   procedure Indent_Lines
     (Tracer   : not null LSP.Tracers.Tracer_Access;
      Filename : GNATCOLL.VFS.Virtual_File;
      Document : LSP.Text_Documents.Text_Document'Class;
      Options  : Gnatformat.Configuration.Format_Options_Type;
      Span     : LSP.Structures.A_Range := LSP.Text_Documents.Empty_Range;
      Success  : out Boolean;
      Response : out LSP.Structures.TextEdit_Vector;
      Error    : out LSP.Errors.ResponseError);
   --  Generate a TextEdit_Vector to reindent the lines in Span using the
   --  fallback indenter.
   --  If no Span is provided, the whole document is indented.
   --  Document is the document to indent.
   --  Tracer is used to log messages.
   --  Filename is the name of the file referenced by Document. Used to
   --  retrieve file-specific indentation options.
   --  Options are the formatting options to use.
   --  Success is set to True if indentation was successful.
   --  Response contains the generated TextEdit_Vector.
   --  Error is set if an error occurred.

   function Reindent_Line
     (Filename   : GNATCOLL.VFS.Virtual_File;
      Line       : VSS.Strings.Virtual_String;
      Options    : Gnatformat.Configuration.Format_Options_Type;
      Pos        : LSP.Structures.Position;
      New_Indent : Natural) return LSP.Structures.TextEdit;
   --  Reindent a line to the given new indentation.
   --  Line contains the actual line in the document.
   --  Options are the formatting options to handle tabs/spaces.
   --  Pos is the LSP position of the line in the document.
   --  New_Indent is the new indentation level for the line.

   function Handle_Tabs
     (Filename : GNATCOLL.VFS.Virtual_File;
      Options  : Gnatformat.Configuration.Format_Options_Type;
      S        : VSS.Strings.Virtual_String) return VSS.Strings.Virtual_String;
   --  Handle tabs and whitespaces convertion depending on the
   --  tabulation-related settings in Options.

   function Get_Formatting_Options
     (Context     : LSP.Ada_Contexts.Context;
      LSP_Options : LSP.Structures.FormattingOptions)
      return Gnatformat.Configuration.Format_Options_Type;
   --  Apply local options to the project options and return the complete
   --  set of options

end LSP.Ada_Handlers.Formatting;
