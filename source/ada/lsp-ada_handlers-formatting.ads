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

--  This package provides implementation of the code formatting requests.

with Gnatformat.Configuration;

with LSP.Ada_Contexts;
with LSP.Errors;
with LSP.Structures;
with LSP.Formatters.Fallback_Indenter;
with LSP.Text_Documents;

with VSS.Strings;
with VSS.String_Vectors;

package LSP.Ada_Handlers.Formatting is

   procedure Format
     (Context  : LSP.Ada_Contexts.Context;
      Document : not null LSP.Ada_Documents.Document_Access;
      Span     : LSP.Structures.A_Range;
      Options  : Gnatformat.Configuration.Format_Options_Type;
      Success  : out Boolean;
      Response : out LSP.Structures.TextEdit_Vector;
      Messages : out VSS.String_Vectors.Virtual_String_Vector;
      Error    : out LSP.Errors.ResponseError);
   --  Format the text of the given document in the given range (span).

   procedure Range_Format
     (Context  : LSP.Ada_Contexts.Context;
      Document : not null LSP.Ada_Documents.Document_Access;
      Span     : LSP.Structures.A_Range;
      Options  : Gnatformat.Configuration.Format_Options_Type;
      Success  : out Boolean;
      Response : out LSP.Structures.TextEdit_Vector;
      Error    : out LSP.Errors.ResponseError);
   --  Format the text of the given document in the given range (span).

   function Get_Indentation
     (Filename : GNATCOLL.VFS.Virtual_File;
      Buffer   : VSS.Strings.Virtual_String;
      Span     : LSP.Structures.A_Range;
      Options  : Gnatformat.Configuration.Format_Options_Type)
      return LSP.Formatters.Fallback_Indenter.Indentation_Array;
   --  Use the fallback indenter to get an array of indentation for each
   --  line in Span.
   --  Buffer is the content of the document referenced by Filename. Should
   --  contain the whole content of the document or a substring including
   --  at least the lines in Span.

   procedure Indent_Lines
     (Tracer   : not null LSP.Tracers.Tracer_Access;
      Filename : GNATCOLL.VFS.Virtual_File;
      Document : LSP.Text_Documents.Text_Document'Class;
      Span     : LSP.Structures.A_Range;
      Options  : Gnatformat.Configuration.Format_Options_Type;
      Success  : out Boolean;
      Response : out LSP.Structures.TextEdit_Vector;
      Messages : out VSS.String_Vectors.Virtual_String_Vector;
      Error    : out LSP.Errors.ResponseError);
   --  Generate a TextEdit_Vector to reindent the lines in Span using the
   --  fallback indenter.
   --  Document is the document to indent.
   --  Tracer is used to log messages.
   --  Filename is the name of the file referenced by Document. Used to
   --  retrieve file-specific indentation options.
   --  Options are the formatting options to use.
   --  Success is set to True if indentation was successful.
   --  Response contains the generated TextEdit_Vector.
   --  Messages contains any informational or warning messages.
   --  Error is set if an error occurred.

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
