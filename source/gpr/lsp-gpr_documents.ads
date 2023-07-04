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
--
--  This package provides an Gpr document abstraction.

with Ada.Containers.Hashed_Maps;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Vectors;

with Langkit_Support.Slocs;

with GNATCOLL.Traces;
with GNATCOLL.VFS;

with GPR2.Context;
with GPR2.Environment;
with GPR2.Log;
with GPR2.Path_Name;
with GPR2.Path_Name.Set;
with GPR2.Project.Tree;

with LSP.GPR_Files;
with LSP.Messages;
with LSP.Types;

with VSS.Strings;
private with VSS.Strings.Markers;

package LSP.GPR_Documents is

   type Document (Trace : GNATCOLL.Traces.Trace_Handle) is
     tagged limited private;
   --  A GPR document (file).

   type Document_Access is access all LSP.GPR_Documents.Document
     with Storage_Size => 0;

   use type GPR2.Log.Object;
   use GPR2.Path_Name;

   package Message_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => GPR2.Path_Name.Object,
      Element_Type    => GPR2.Log.Object,
      Hash            => GPR2.Path_Name.Hash,
      Equivalent_Keys => "=");

   subtype Message_Map is Message_Maps.Map;
   --  Used to split a tree's log into logs (one for each file with message)

   procedure Initialize
     (Self        : in out Document;
      URI         : LSP.Messages.DocumentUri;
      File        : GPR2.Path_Name.Object;
      Text        : VSS.Strings.Virtual_String;
      Provider    : LSP.GPR_Files.File_Provider_Access);
   --  Create a new document from a TextDocumentItem. Use Diagnostic as
   --  project status diagnostic source.

   procedure Load (Self : in out Document);
   --  Load associated GPR tree.

   procedure Cleanup (Self : in out Document);
   --  Free all the data associated to this document.

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
     (Self      : in out Document;
      Root_File : GPR2.Path_Name.Object;
      Changed   : out Boolean;
      Errors    : out Message_Map);
   --  Get errors found during document parsing.

   function Has_Diagnostics
     (Self : Document)
      return Boolean;
   --  Returns True when errors found during document parsing.

   function Get_Word_At
     (Self     : Document;
      Position : LSP.Messages.Position)
      return VSS.Strings.Virtual_String;
   --  Get an identifier at given position in the document or an empty string.

   -----------------------
   -- Document_Provider --
   -----------------------

   type Document_Provider is limited interface;
   type Document_Provider_Access is access all Document_Provider'Class;
   --  A Document_Provider is an object that contains documents and
   --  is able to retrieve a document from its given URI.

   function Is_Openened_Document
     (Self : access Document_Provider;
      File : GNATCOLL.VFS.Virtual_File) return Boolean is abstract;
   --  Return True if file currently opened in client

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

   function Get_Source_Location
     (Self     : Document'Class;
      Position : LSP.Messages.Position)
      return Langkit_Support.Slocs.Source_Location;
   --  Convert a Position to a Source_Location

   function Line_Terminator
     (Self : Document'Class) return VSS.Strings.Virtual_String;
   --  Return line terminator for the document

   procedure Update_Files_With_Diags
     (Self : in out Document'Class; Files : GPR2.Path_Name.Set.Object);

private

   package Line_Marker_Vectors is new Ada.Containers.Vectors
     (Index_Type   => LSP.Types.Line_Number,
      Element_Type => VSS.Strings.Markers.Character_Marker,
      "="          => VSS.Strings.Markers."=");

   type Name_Information is record
      Loc       : Langkit_Support.Slocs.Source_Location;
      Is_Public : Boolean;
   end record;

   package Name_Vectors is new Ada.Containers.Vectors
     (Positive, Name_Information);

   package Symbol_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type     => VSS.Strings.Virtual_String,
      Element_Type => Name_Vectors.Vector,
      "<"          => VSS.Strings."<",
      "="          => Name_Vectors."=");

   type Document
     (Trace : GNATCOLL.Traces.Trace_Handle) is tagged limited record

      URI  : LSP.Messages.DocumentUri;
      --  document's file URI

      File : GPR2.Path_Name.Object;
      --  document's file path

      Version : LSP.Types.LSP_Number := 1;
      --  Document version

      Text : VSS.Strings.Virtual_String;
      --  The text of the document

      Tree : GPR2.Project.Tree.Object;
      --  The loaded tree

      Context : GPR2.Context.Object;
      --  GPR scenario variables

      File_Provider : LSP.GPR_Files.File_Provider_Access;
      --  Reader used by GPR2 to access opened documents contents

      Environment : GPR2.Environment.Object;
      --  Environment used by GPR2 extending ALS process environment

      Messages : GPR2.Log.Object;
      --  Latest Tree's log

      Has_Messages : Boolean;
      --  True if Messages contains errors and/or warnings

      Errors_Changed : Boolean;
      --  True if Messages content was not yet published

      Line_To_Marker : Line_Marker_Vectors.Vector;
      --  Within text, an array associating a line number (starting at 0) to
      --  the marker of the first character of that line in Text.
      --  This serves as cache to be able to modify text ranges in Text
      --  given in line/column coordinates without having to scan the whole
      --  text from the beginning.

      Line_Terminator : VSS.Strings.Virtual_String;
      --  Line terminator for the text, if known, "" otherwise

      Published_Files_With_Diags : GPR2.Path_Name.Set.Object;
      --  Protocol requires publishing empty diags to clear diags on client.
      --  This set records files with diags previously published.

   end record;

   function URI (Self : Document) return LSP.Messages.DocumentUri is
     (Self.URI);
   function Text (Self : Document) return VSS.Strings.Virtual_String is
     (Self.Text);

end LSP.GPR_Documents;
