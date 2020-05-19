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
--
--  This package provides an Ada document abstraction.

with Ada.Containers.Vectors;

with LSP.Messages;
with LSP.Types;
with Libadalang.Analysis;
limited with LSP.Ada_Contexts;

with GNATCOLL.Traces;

package LSP.Ada_Documents is

   MAX_NB_DIAGNOSTICS : constant := 2;

   type Document (Trace : GNATCOLL.Traces.Trace_Handle) is
     tagged limited private;
   --  An Ada document (file).

   type Document_Access is access all LSP.Ada_Documents.Document
     with Storage_Size => 0;

   procedure Initialize
     (Self : in out Document;
      URI  : LSP.Messages.DocumentUri;
      Text : LSP.Types.LSP_String);
   --  Create a new document from a TextDocumentItem. Use LAL as libadalang
   --  context to parse text of the document.

   -----------------------
   -- Contents handling --
   -----------------------

   function URI (Self : Document) return LSP.Messages.DocumentUri;
   --  Return the URI associated with Self

   function Text (Self : Document) return LSP.Types.LSP_String;
   --  Return the text associated with Self

   procedure Apply_Changes
     (Self    : aliased in out Document;
      Version : LSP.Types.Optional_Number;
      Vector  : LSP.Messages.TextDocumentContentChangeEvent_Vector);
   --  Modify document according to event vector provided by LSP client.

   function Versioned_Identifier
     (Self : Document) return LSP.Messages.VersionedTextDocumentIdentifier;

   --------------
   -- Requests --
   --------------

   --  These requests are meaningful within a document/context pair

   procedure Get_Errors
     (Self    : Document;
      Context : LSP.Ada_Contexts.Context;
      Errors  : out LSP.Messages.Diagnostic_Vector);
   --  Get errors found during document parsing.

   procedure Get_Symbols
     (Self    : Document;
      Context : LSP.Ada_Contexts.Context;
      Result  : out LSP.Messages.Symbol_Vector);
   --  Populate Result with symbols from the document.

   procedure Get_Symbol_Hierarchy
     (Self    : Document;
      Context : LSP.Ada_Contexts.Context;
      Result  : out LSP.Messages.Symbol_Vector);
   --  Populate Result with a symbol hierarchy from the document.

   function Get_Node_At
     (Self     : Document;
      Context  : LSP.Ada_Contexts.Context;
      Position : LSP.Messages.Position)
      return Libadalang.Analysis.Ada_Node;
   --  Get Libadalang Node for given position in the document.

   procedure Get_Completions_At
     (Self             : Document;
      Context          : LSP.Ada_Contexts.Context;
      Position         : LSP.Messages.Position;
      Snippets_Enabled : Boolean;
      Result           : out LSP.Messages.CompletionList);
   --  Populate Result with completions for given position in the document.

   procedure Get_Folding_Blocks
     (Self       : Document;
      Context    : LSP.Ada_Contexts.Context;
      Lines_Only : Boolean;
      Result     : out LSP.Messages.FoldingRange_Vector);
   --  Populate Result with code folding blocks in the document. If Lines_Only
   --  is True does not return characters positions in lines.

   function Formatting
     (Self     : Document;
      Context  : LSP.Ada_Contexts.Context;
      Span     : LSP.Messages.Span;
      Options  : LSP.Messages.FormattingOptions;
      Edit     : out LSP.Messages.TextEdit_Vector)
      return Boolean;
   --  Format whole document, Return False on error

   -----------------------
   -- Document_Provider --
   -----------------------

   type Document_Provider is limited interface;
   type Document_Provider_Access is access all Document_Provider'Class;
   --  A Document_Provider is an object that contains documents and
   --  is able to retrieve a document from its given URI.

   function Get_Open_Document
     (Self  : access Document_Provider;
      URI   : LSP.Messages.DocumentUri;
      Force : Boolean := False)
      return Document_Access is abstract;
   --  Return the open document for the given URI.
   --  If the document is not opened, then if Force a new document
   --  will be created and must be freed by the user else null will be
   --  returned.

private

   package Line_To_Index_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => Positive,
      "="          => "=");
   use Line_To_Index_Vectors;

   type Document (Trace : GNATCOLL.Traces.Trace_Handle) is tagged limited
   record
      URI  : LSP.Messages.DocumentUri;

      Version : LSP.Types.LSP_Number := 1;
      --  Document version

      Text : LSP.Types.LSP_String;
      --  The text of the document

      Line_To_Index : Vector;
      --  Within text, an array associating a line number (starting at 0) to
      --  the offset of the first character of that line in Text.
      --  This serves as cache to be able to modify text ranges in Text
      --  given in line/column coordinates without having to scan the whole
      --  text from the beginning.
   end record;

   procedure Diff
     (Self     : Document;
      New_Text : LSP.Types.LSP_String;
      Old_Span : LSP.Messages.Span := LSP.Messages.Empty_Span;
      New_Span : LSP.Messages.Span := LSP.Messages.Empty_Span;
      Edit     : out LSP.Messages.TextEdit_Vector);
   --  Create a diff between document Text and New_Text and return Text_Edit
   --  based on Needleman-Wunsch algorithm
   --  Old_Span and New_Span are used when we need to compare certain
   --  old/new lines instead of whole buffers

   function URI (Self : Document) return LSP.Messages.DocumentUri is
     (Self.URI);
   function Text (Self : Document) return LSP.Types.LSP_String is (Self.Text);

end LSP.Ada_Documents;
