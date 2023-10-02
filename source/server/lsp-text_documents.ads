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

--  This package provides an text document abstraction. It provides capability
--  to apply and generate text edits.

private with Ada.Containers.Vectors;

with VSS.Strings;
private with VSS.Strings.Markers;

with LSP.Structures;

package LSP.Text_Documents is

   LSP_New_Line_Function_Set : constant VSS.Strings.Line_Terminator_Set :=
     (VSS.Strings.CR | VSS.Strings.CRLF | VSS.Strings.LF => True,
      others => False);
   --  LSP allows to use three kinds of line terminators: CR, CR+LF and LF.

   Empty_Range : LSP.Structures.A_Range := ((1, 1), (0, 0));

   type Text_Position is record
      Line   : VSS.Strings.Line_Index;
      Column : VSS.Strings.Line_Index'Base;
   end record;

   type Text_Slice is record
      First : Text_Position;
      Last  : Text_Position;
   end record;

   type Text_Document is abstract tagged limited private;

   function URI (Self : Text_Document'Class) return LSP.Structures.DocumentUri;
   --  Return the URI associated with Self

   function Identifier
     (Self : Text_Document'Class)
      return LSP.Structures.OptionalVersionedTextDocumentIdentifier;

   function Text
     (Self : Text_Document'Class) return VSS.Strings.Virtual_String;

   function Slice
     (Self    : Text_Document'Class;
      A_Range : LSP.Structures.A_Range) return VSS.Strings.Virtual_String;
   --  Return the text in the specified range.

   function Line_Terminator
     (Self : Text_Document'Class) return VSS.Strings.Virtual_String;
   --  Return line terminator for the document

   procedure Apply_Changes
     (Self    : in out Text_Document'Class;
      Version : Integer;
      Vector  : LSP.Structures.TextDocumentContentChangeEvent_Vector);
   --  Modify document according to event vector provided by LSP client.

   procedure Diff
     (Self     : Text_Document'Class;
      New_Text : VSS.Strings.Virtual_String;
      Old_Span : LSP.Structures.A_Range := Empty_Range;
      New_Span : LSP.Structures.A_Range := Empty_Range;
      Edit     : out LSP.Structures.TextEdit_Vector);
   --  Create a diff between document Text and New_Text and return Text_Edit
   --  based on Needleman-Wunsch algorithm.
   --  Old_Span and New_Span are used when we need to compare certain
   --  old/new lines instead of whole buffers.

   procedure Diff_Symbols
     (Self     : Text_Document'Class;
      Span     : LSP.Structures.A_Range;
      New_Text : VSS.Strings.Virtual_String;
      Edit     : in out LSP.Structures.TextEdit_Vector);
   --  Create a diff between document Text inside Span and New_Chunk and
   --  return Text_Edit. Tests individual symbols instead of lines
   --  as above. Do not use it for large text slices because it
   --  creates an N^M map for symbols.

   package Constructors is

      procedure Initialize
        (Self : in out Text_Document'Class;
         URI  : LSP.Structures.DocumentUri;
         Text : VSS.Strings.Virtual_String);

   end Constructors;

private

   package Line_Marker_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => VSS.Strings.Markers.Character_Marker,
      "="          => VSS.Strings.Markers."=");

   type Text_Document is abstract tagged limited record
      URI             : LSP.Structures.DocumentUri;
      --  URI of the document

      Version         : Integer := 1;
      --  Document version

      Text            : VSS.Strings.Virtual_String;
      --  The text of the document

      Line_Terminator : VSS.Strings.Virtual_String;
      --  Line terminator for the text, if known, "" otherwise

      Line_Marker     : Line_Marker_Vectors.Vector;
      --  Within text, an array associating a line number (starting at 0) to
      --  the marker of the first character of that line in Text.
      --  This serves as cache to be able to modify text ranges in Text
      --  given in line/column coordinates without having to scan the whole
      --  text from the beginning.
   end record;

   function Text
     (Self : Text_Document'Class) return VSS.Strings.Virtual_String is
       (Self.Text);

   function URI
     (Self : Text_Document'Class) return LSP.Structures.DocumentUri is
       (Self.URI);

end LSP.Text_Documents;
