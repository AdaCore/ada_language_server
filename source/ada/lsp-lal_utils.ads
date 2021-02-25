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
--  This package provides some Libadalang related utility subprograms.

with LSP.Ada_Contexts;
with LSP.Messages;
with LSP.Types;

with Laltools.Common;

with Libadalang.Analysis;  use Libadalang.Analysis;
with Libadalang.Common;

with Langkit_Support.Slocs;
with Langkit_Support.Text;

with Utils.Command_Lines;
with Utils.Char_Vectors;
with Pp.Scanner;
with Ada.Strings.Unbounded;

with VSS.Strings;

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
      Kind   : LSP.Messages.Optional_DocumentHighlightKind);
   --  The same for Location_Vector.

   procedure Sort_And_Remove_Duplicates
     (Result : in out LSP.Messages.Location_Vector);
   --  Sort Result and remove duplicates from it.

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

   ---------------
   -- Called_By --
   ---------------

   function Find_All_Calls
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

   function To_Call_Hierarchy_Item
     (Name : Libadalang.Analysis.Defining_Name)
      return LSP.Messages.CallHierarchyItem;
   --  Create CallHierarchyItem for the given subprogram

   function To_Unbounded_Text_Type
     (Item : LSP.Types.LSP_String)
      return Langkit_Support.Text.Unbounded_Text_Type;
   --  Converts a string from LSP_String to Unbounded_Text_Type. The intent
   --  of this function is to convert names and small strings but not buffers.
   --  It's dangerous to return a string when the string might be giant,
   --  therefore, a buffer should not be passed to this function.

   function Node_Location_Image
     (Node : Libadalang.Analysis.Ada_Node'Class) return LSP.Types.LSP_String;
   --  Return "file.adb:line:col" as a string

   function Containing_Entity (Ref : Ada_Node) return Defining_Name;
   --  Return the declaration of the subprogram or task that contains Ref.
   --  Return No_Defining_Name if this fails.

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

end LSP.Lal_Utils;
