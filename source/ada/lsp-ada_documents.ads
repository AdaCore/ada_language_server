------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2018-2021, AdaCore                     --
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

with Ada.Containers.Ordered_Maps;
with Ada.Containers.Vectors;

with VSS.Strings;
private with VSS.Strings.Markers;

with LSP.Messages;
with LSP.Types;

with Libadalang.Analysis;

limited with LSP.Ada_Contexts;
with LSP.Ada_Completions;

with GNATCOLL.Traces;
with Libadalang.Common;

with Pp.Command_Lines;

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
      Text : VSS.Strings.Virtual_String);
   --  Create a new document from a TextDocumentItem. Use LAL as libadalang
   --  context to parse text of the document.

   -----------------------
   -- Contents handling --
   -----------------------

   function URI (Self : Document) return LSP.Messages.DocumentUri;
   --  Return the URI associated with Self

   function Text (Self : Document) return VSS.Strings.Virtual_String;
   --  Return the text associated with Self

   function Get_Text_At
     (Self      : Document;
      Start_Pos : LSP.Messages.Position;
      End_Pos   : LSP.Messages.Position) return VSS.Strings.Virtual_String;
   --  Return the text in the specified range.

   procedure Apply_Changes
     (Self    : aliased in out Document;
      Version : LSP.Types.LSP_Number;
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

   function Has_Diagnostics
     (Self    : Document;
      Context : LSP.Ada_Contexts.Context)
      return Boolean;
   --  Returns True when errors found during document parsing.

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

   function Get_Word_At
     (Self     : Document;
      Context  : LSP.Ada_Contexts.Context;
      Position : LSP.Messages.Position)
      return VSS.Strings.Virtual_String;
   --  Get an identifier at given position in the document or an empty string.

   procedure Get_Completions_At
     (Self                     : Document;
      Context                  : LSP.Ada_Contexts.Context;
      Position                 : LSP.Messages.Position;
      Named_Notation_Threshold : Natural;
      Snippets_Enabled         : Boolean;
      Should_Use_Names         : in out Boolean;
      Names                    : out Ada_Completions.Completion_Maps.Map;
      Result                   : out LSP.Messages.CompletionList);
   --  Populate Result with completions for given position in the document.
   --  Named_Notation_Threshold defines the number of components at which point
   --  named notation is used for aggregate completion snippets.
   --  If Use_Snippets is True, completion for subprograms and aggregates
   --  will be given in the form of snippets when it makes sense.
   --  In case when no defining names could be used for completion (for
   --  instange inside aggregates, pragmas, keywords, etc) set Should_Use_Names
   --  to False. Otherwise set it to True and populate Names instead of Result.

   procedure Get_Any_Symbol_Completion
     (Self    : in out Document;
      Context : LSP.Ada_Contexts.Context;
      Prefix  : VSS.Strings.Virtual_String;
      Limit   : Ada.Containers.Count_Type;
      Result  : in out LSP.Ada_Completions.Completion_Maps.Map);
   --  See Contests.Get_Any_Symbol_Completion

   procedure Get_Folding_Blocks
     (Self       : Document;
      Context    : LSP.Ada_Contexts.Context;
      Lines_Only : Boolean;
      Comments   : Boolean;
      Result     : out LSP.Messages.FoldingRange_Vector);
   --  Populate Result with code folding blocks in the document. If Lines_Only
   --  is True does not return characters positions in lines.

   function Formatting
     (Self     : Document;
      Context  : LSP.Ada_Contexts.Context;
      Span     : LSP.Messages.Span;
      Cmd      : Pp.Command_Lines.Cmd_Line;
      Edit     : out LSP.Messages.TextEdit_Vector)
      return Boolean;
   --  Format document or its part defined in Span

   procedure Get_Imported_Units
     (Self          : Document;
      Context       : LSP.Ada_Contexts.Context;
      Project_URI   : LSP.Types.LSP_URI;
      Show_Implicit : Boolean;
      Result        : out LSP.Messages.ALS_Unit_Description_Vector);
   --  Return all the units that import the document's unit.
   --  If Show_Implicit is True, units that import implicitly on the document's
   --  unit are also returned.

   procedure Get_Importing_Units
     (Self          : Document;
      Context       : LSP.Ada_Contexts.Context;
      Project_URI   : LSP.Types.LSP_URI;
      Show_Implicit : Boolean;
      Result        : out LSP.Messages.ALS_Unit_Description_Vector);
   --  Return the units that import the document's unit among the given list.
   --  If Show_Implicit is True, units that depend on the document's unit in
   --  an implicit way will also be returned.

   procedure Find_All_References
     (Self       : Document;
      Context    : LSP.Ada_Contexts.Context;
      Definition : Libadalang.Analysis.Defining_Name;
      Callback   : not null access procedure
        (Base_Id : Libadalang.Analysis.Base_Id;
         Kind    : Libadalang.Common.Ref_Result_Kind;
         Cancel  : in out Boolean));
   --  Finds all references to a given defining name in the document's unit and
   --  call Calbback on each found reference.

   procedure Reset_Symbol_Cache (Self : in out Document'Class);
   --  Clean cache for defining name symbols of the document.

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

   function Get_Open_Document_Version
     (Self  : access Document_Provider;
      URI   : LSP.Messages.DocumentUri)
      return LSP.Messages.OptionalVersionedTextDocumentIdentifier is abstract;
   --  Return the version of an open document for the given URI.
   --  If the document is not opened, then it returns a
   --  VersionedTextDocumentIdentifier with a null version.

   function Compute_Completion_Item
     (Context                  : LSP.Ada_Contexts.Context;
      BD                       : Libadalang.Analysis.Basic_Decl;
      DN                       : Libadalang.Analysis.Defining_Name;
      Use_Snippets             : Boolean;
      Named_Notation_Threshold : Natural;
      Is_Dot_Call              : Boolean;
      Is_Visible               : Boolean)
      return LSP.Messages.CompletionItem;
   --  Compute a completion item.
   --  Node is the node from which the completion starts (e.g: 'A' in 'A.').
   --  BD and DN are respectively the basic declaration and the defining name
   --  that should be used to compute the completion item.
   --  When Use_Snippets is True, subprogram completion items are computed
   --  as snippets that list all the subprogram's formal parameters.
   --  Named_Notation_Threshold defines the number of parameters at which point
   --  named notation is used for subprogram completion snippets.
   --  Is_Dot_Call is used to know if we should omit the first parameter
   --  when computing subprogram snippets.

private

   package Line_Marker_Vectors is new Ada.Containers.Vectors
     (Index_Type   => LSP.Types.Line_Number,
      Element_Type => VSS.Strings.Markers.Character_Marker,
      "="          => VSS.Strings.Markers."=");

   package Name_Vectors is new Ada.Containers.Vectors
     (Positive, Libadalang.Analysis.Defining_Name, Libadalang.Analysis."=");

   package Symbol_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type     => VSS.Strings.Virtual_String,
      Element_Type => Name_Vectors.Vector,
      "<"          => VSS.Strings."<",
      "="          => Name_Vectors."=");

   type Document (Trace : GNATCOLL.Traces.Trace_Handle) is tagged limited
   record
      URI  : LSP.Messages.DocumentUri;

      Version : LSP.Types.LSP_Number := 1;
      --  Document version

      Text : VSS.Strings.Virtual_String;
      --  The text of the document

      Line_To_Marker : Line_Marker_Vectors.Vector;
      --  Within text, an array associating a line number (starting at 0) to
      --  the marker of the first character of that line in Text.
      --  This serves as cache to be able to modify text ranges in Text
      --  given in line/column coordinates without having to scan the whole
      --  text from the beginning.

      Symbol_Cache : Symbol_Maps.Map;
      --  Cache of all defining name symbol of the document.
      Refresh_Symbol_Cache : Boolean := False;
      --  Symbol_Cache rebuild is required before.
   end record;

   procedure Diff
     (Self     : Document;
      New_Text : VSS.Strings.Virtual_String;
      Old_Span : LSP.Messages.Span := LSP.Messages.Empty_Span;
      New_Span : LSP.Messages.Span := LSP.Messages.Empty_Span;
      Edit     : out LSP.Messages.TextEdit_Vector);
   --  Create a diff between document Text and New_Text and return Text_Edit
   --  based on Needleman-Wunsch algorithm
   --  Old_Span and New_Span are used when we need to compare certain
   --  old/new lines instead of whole buffers

   function URI (Self : Document) return LSP.Messages.DocumentUri is
     (Self.URI);
   function Text (Self : Document) return VSS.Strings.Virtual_String is
     (Self.Text);

end LSP.Ada_Documents;
