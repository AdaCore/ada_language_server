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
--  This package provides some Libadalang related utility subprograms.

with GNATCOLL.VFS;

with LSP.Ada_Contexts;
with LSP.Messages;
with LSP.Types;

with Laltools.Common;
with Laltools.Refactor;

with Libadalang.Analysis;  use Libadalang.Analysis;
with Libadalang.Common;

with Langkit_Support.Slocs;
with Langkit_Support.Text;

with Utils.Command_Lines;
with Utils.Char_Vectors;
with Pp.Scanner;
with Ada.Strings.Unbounded;

with VSS.Strings;
with LSP.Ada_Documents;

package LSP.Lal_Utils is

   procedure Append_Location
     (Result : in out LSP.Messages.Location_Or_Link_Vector;
      Node   : Libadalang.Analysis.Ada_Node'Class;
      Kind   : LSP.Messages.AlsReferenceKind_Set := LSP.Messages.Empty_Set);
   --  Append given Node location to the Result.
   --  Do nothing if the item inside of an synthetic file (like __standard).
   --  See description of Kind in Get_Node_Location comments.

   procedure Append_Location
     (Result : in out LSP.Messages.Location_Vector;
      Node   : Libadalang.Analysis.Ada_Node'Class;
      Kind   : LSP.Messages.AlsReferenceKind_Set := LSP.Messages.Empty_Set);
   --  The same for Location_Vector.

   procedure Append_Location
     (Result : in out LSP.Messages.DocumentHighlight_Vector;
      Node   : Libadalang.Analysis.Ada_Node'Class;
      Kind   : LSP.Messages.Optional_DocumentHighlightKind;
      File   : GNATCOLL.VFS.Virtual_File);
   --  The same for DocumentHighlight_Vector.
   --  File is used to filter out the location not in the Document.

   procedure Sort_And_Remove_Duplicates
     (Result : in out LSP.Messages.Location_Vector);
   --  Sort Result and remove duplicates from it.

   function Get_Location
     (Unit : Libadalang.Analysis.Analysis_Unit;
      Span : Langkit_Support.Slocs.Source_Location_Range;
      Kind : LSP.Messages.AlsReferenceKind_Set := LSP.Messages.Empty_Set)
      return LSP.Messages.Location;
   --  Return the location in a unit. Populate alsKind field of the result with
   --  given Kind.

   function Get_Node_Location
     (Node : Libadalang.Analysis.Ada_Node'Class;
      Kind : LSP.Messages.AlsReferenceKind_Set := LSP.Messages.Empty_Set)
      return LSP.Messages.Location;
   --  Return the location of the given node. Populate alsKind field of the
   --  result with given Kind.

   function Get_Token_Span
     (Token : Libadalang.Common.Token_Reference)
      return LSP.Messages.Span;
   --  Return the span of the given token.

   function To_Span
     (Value : Langkit_Support.Slocs.Source_Location_Range)
      return LSP.Messages.Span;
   --  Convert Source_Location_Range to Span

   function To_Span
     (Value : Langkit_Support.Slocs.Source_Location)
      return LSP.Messages.Span;
   --  Convert Source_Location to Span

   function To_TextEdit
     (E : Laltools.Refactor.Text_Edit)
      return LSP.Messages.TextEdit;
   --  Converts an Edit into a TextEdit

   function To_Workspace_Edit
     (EM                  : Laltools.Refactor.Text_Edit_Map;
      Versioned_Documents : Boolean := False;
      Document_Provider   : access LSP.Ada_Documents.Document_Provider'Class
      := null)
      return LSP.Messages.WorkspaceEdit
     with Pre => (if Versioned_Documents then Document_Provider /= null);
   --  Converts an Edit_Map into a WorkspaceEdit

   function To_Workspace_Edit
     (Edits               : Laltools.Refactor.Refactoring_Edits;
      Versioned_Documents : Boolean := False;
      Document_Provider   : access LSP.Ada_Documents.Document_Provider'Class
      := null;
      Rename              : Boolean := False)
      return LSP.Messages.WorkspaceEdit;
   --  Converts a Refactoring_Edits into a WorkspaceEdit. The Rename flag
   --  controls if files that are supposed to be deleted, are renamed instead.

   ---------------
   -- Called_By --
   ---------------

   function Find_Incoming_Calls
     (Context           : LSP.Ada_Contexts.Context;
      Definition        : Defining_Name;
      Imprecise_Results : out Boolean)
      return Laltools.Common.References_By_Subprogram.Map
     with Pre => Definition.P_Basic_Decl.P_Is_Subprogram;
   --  Return the list of all the calls made to the subprogram pointed at by
   --  the node given by Definition, organized by the subprograms in which
   --  these calls are listed, ordered by the name of these subprograms.
   --  Imprecise_Results is set to True if we don't know whether the results
   --  are precise.

   function Find_Outgoing_Calls
     (Context           : LSP.Ada_Contexts.Context;
      Definition        : Defining_Name;
      Imprecise_Results : out Boolean)
      return Laltools.Common.References_By_Subprogram.Map
     with Pre => Definition.P_Basic_Decl.P_Is_Subprogram;
   --  Return the list of all the calls made in the subprogram pointed at by
   --  the node given by Definition, organized by the subprograms in which
   --  these calls are listed, ordered by the name of these subprograms.
   --  Imprecise_Results is set to True if we don't know whether the results
   --  are precise.

   --  function Is_Task
   --    (Node      : Ada_Node'Class;
   --     Trace     : GNATCOLL.Traces.Trace_Handle;
   --     Imprecise : out Boolean) return Boolean;
   --  TODO: Reactivate these lines when libadalang supports
   --  P_Next_Part for tasks: T716-049

   function Canonicalize
     (Text : LSP.Types.LSP_String) return VSS.Strings.Virtual_String;
   --  Return a canonicalized value for Text. This performs case folding and
   --  brackets decoding.

   function Get_Decl_Kind
     (Node         : Libadalang.Analysis.Basic_Decl;
      Ignore_Local : Boolean := False)
      return LSP.Messages.SymbolKind;
   --  Return a LSP SymbolKind for the given Libadalang Basic_Decl
   --  When Ignore_Local it will return Is_Null for all local objects like
   --  variables.

   procedure Get_Call_Expr_Name
     (Node            : Libadalang.Analysis.Ada_Node'Class;
      Cursor_Line     : Langkit_Support.Slocs.Line_Number;
      Cursor_Column   : Langkit_Support.Slocs.Column_Number;
      Active_Position : out LSP.Types.LSP_Number;
      Designator      : out Libadalang.Analysis.Ada_Node;
      Name_Node       : out Libadalang.Analysis.Name);
   --  If Node is inside a Call_Expr returns the following:
   --  Active_Position: the index of the parameter in the Call_Expr
   --  Designator: the designator of the Active_Position
   --  Name_Node: the name of the Call_Expr
   --  Cursor_Line and Cursor_Column the position of the cursor when the
   --  request was triggered.

   procedure Get_Parameters
     (Node : Libadalang.Analysis.Basic_Decl;
      Parameters : in out LSP.Messages.ParameterInformation_Vector);
   --  Append all the parameters of Node inside Parameters

   function Get_Active_Parameter
     (Node       : Libadalang.Analysis.Basic_Decl;
      Designator : Libadalang.Analysis.Ada_Node;
      Position   : LSP.Types.LSP_Number)
      return LSP.Types.LSP_Number;
   --  Return the position of Designator in the parameters of Node else -1
   --  If Designator is null try check if Position is a valid parameter index

   function To_Call_Hierarchy_Item
     (Name : Libadalang.Analysis.Defining_Name)
      return LSP.Messages.CallHierarchyItem;
   --  Create CallHierarchyItem for the given subprogram

   procedure To_Call_Hierarchy_Result
     (Node  : Libadalang.Analysis.Defining_Name;
      Refs  : Laltools.Common.References_Sets.Set;
      Item  : out LSP.Messages.CallHierarchyItem;
      Spans : out LSP.Messages.Span_Vector;
      Kinds : out LSP.Messages.AlsReferenceKind_Vector);
   --  Convert the given Node and the given references to it to the
   --  corresponding CallHierarchyItem and its associated spans, which contains
   --  the references. This should be used for the callHierarchy requests.

   function To_Unbounded_Text_Type
     (Item : LSP.Types.LSP_String)
      return Langkit_Support.Text.Unbounded_Text_Type;
   --  Converts a string from LSP_String to Unbounded_Text_Type. The intent
   --  of this function is to convert names and small strings but not buffers.
   --  It's dangerous to return a string when the string might be giant,
   --  therefore, a buffer should not be passed to this function.

   function Node_Location_Image
     (Node : Libadalang.Analysis.Ada_Node'Class)
      return VSS.Strings.Virtual_String;
   --  Return "file.adb:line:col" as a string

   function Containing_Entity
     (Ref       : Ada_Node;
      Canonical : Boolean := True) return Defining_Name;
   --  Return the declaration of the subprogram or task that contains Ref.
   --  Return No_Defining_Name if this fails.
   --  If Canonical is True, the first part of the enclosing entity will be
   --  returned (i.e: if the enclosing entity is a subprogram body, the
   --  function will return the spec declaration node).

   function To_Unbounded_String
     (Input : Utils.Char_Vectors.Char_Vector)
       return Ada.Strings.Unbounded.Unbounded_String;
   --  Convert Input to unbounded string.

   procedure Format_Vector
     (Cmd      : Utils.Command_Lines.Command_Line;
      Input    : Utils.Char_Vectors.Char_Vector;
      Node     : Ada_Node;
      In_Sloc  : Langkit_Support.Slocs.Source_Location_Range;
      Output   : out Utils.Char_Vectors.Char_Vector;
      Out_Sloc : out Langkit_Support.Slocs.Source_Location_Range;
      Messages : out Pp.Scanner.Source_Message_Vector);
   --  A wrapper around Pp.Actions.Format_Vector that populates Out_Range,

   function Compute_Completion_Detail
     (BD : Libadalang.Analysis.Basic_Decl) return VSS.Strings.Virtual_String;

end LSP.Lal_Utils;
