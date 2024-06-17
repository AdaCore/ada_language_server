------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2023, AdaCore                          --
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

--  This package provides some utility subprograms.

with VSS.Strings.Conversions;

with GNATCOLL.VFS;
with GPR2.Path_Name;
with GPR2.Message;
with GPR2.Source_Reference;
with Libadalang.Analysis;
with Langkit_Support.Slocs;
with Pp.Scanner;
with Utils.Char_Vectors;
with Utils.Command_Lines;

with LSP.Enumerations;
with LSP.Structures;

package LSP.Utils is

   function Canonicalize
     (Text : VSS.Strings.Virtual_String) return VSS.Strings.Virtual_String;
   --  Return a canonicalized value for Text. This performs case folding and
   --  brackets decoding.

   function Node_Location_Image
     (Node : Libadalang.Analysis.Ada_Node'Class)
      return VSS.Strings.Virtual_String;
   --  Return "file.adb:line:col" as a string

   function Get_Decl_Kind
     (Node         : Libadalang.Analysis.Basic_Decl;
      Ignore_Local : Boolean := False)
      return LSP.Enumerations.SymbolKind;
   --  Return a LSP SymbolKind for the given Libadalang Basic_Decl
   --  When Ignore_Local it will return Is_Null for all local objects like
   --  variables.

   function To_Range
     (Value : Langkit_Support.Slocs.Source_Location_Range)
      return LSP.Structures.A_Range;
   --  Convert Source_Location_Range to A_Range
   --  XXX Please avoid use of this subprogram, it doesn't provide Document
   --  to convert LAL's Source_Location_Range to LSP's Range. Consider to
   --  use Document.To_LSP_Range instead, or add necessary wrapper.

   function Get_Location
     (Unit : Libadalang.Analysis.Analysis_Unit;
      Span : Langkit_Support.Slocs.Source_Location_Range)
      return LSP.Structures.Location;
   --  Return the location in a unit.
   --  XXX Please avoid use of this subprogram, it doesn't provide Document
   --  to convert LAL's Source_Location_Range to LSP's Range. Consider to
   --  use Document.To_LSP_Range instead, or add necessary wrapper.

   function Get_Node_Location
     (Node : Libadalang.Analysis.Ada_Node'Class)
      return LSP.Structures.Location;
   --  Return the location of the given node.
   --  XXX Please avoid use of this subprogram, it doesn't provide Document
   --  to convert LAL's Source_Location_Range to LSP's Range. Consider to
   --  use Document.To_LSP_Range instead, or add necessary wrapper.

   function Is_Synthetic
     (Node : Libadalang.Analysis.Ada_Node'Class) return Boolean;
   --  Check if Node is in a synthetic file (like "__standard").

   procedure Format_Vector
     (Cmd      : Standard.Utils.Command_Lines.Command_Line;
      Input    : Standard.Utils.Char_Vectors.Char_Vector;
      Node     : Libadalang.Analysis.Ada_Node;
      In_Sloc  : Langkit_Support.Slocs.Source_Location_Range;
      Output   : out Standard.Utils.Char_Vectors.Char_Vector;
      Out_Sloc : out Langkit_Support.Slocs.Source_Location_Range;
      Messages : out Pp.Scanner.Source_Message_Vector);
   --  A wrapper around Pp.Actions.Format_Vector that populates Out_Range,

   procedure Span_To_Slice
     (Text  : VSS.Strings.Virtual_String;
      Span  : LSP.Structures.A_Range;
      Slice : out VSS.Strings.Virtual_String);
   --  Return a slice of the Text in Span range

   function Image
     (Value : LSP.Structures.Integer_Or_Virtual_String)
      return VSS.Strings.Virtual_String;
   --  Return a string representation of the given value.

   function To_Range
     (Sloc : GPR2.Source_Reference.Object) return LSP.Structures.A_Range;
   --  Convert a GPR2 source location into a LSP range.

   function To_URI
     (Path : GPR2.Path_Name.Object) return LSP.Structures.DocumentUri;
   --  Convert a GPR2 file path into a LSP URI.

   function To_Optional_DiagnosticSeverity
     (Level : GPR2.Message.Level_Value)
      return LSP.Structures.DiagnosticSeverity_Optional;
   --  Convert a GPR2 message level into a LSP diagnostic severity.

   function To_LSP_Diagnostic
     (Message : GPR2.Message.Object) return LSP.Structures.Diagnostic;
   --  Convert a GPR2 message into a proper LSP diagnostic, with the right
   --  severity level and the location reported by GPR2.

   function To_Virtual_File
     (Value : VSS.Strings.Virtual_String) return GNATCOLL.VFS.Virtual_File is
     (GNATCOLL.VFS.Create_From_UTF8
        (VSS.Strings.Conversions.To_UTF_8_String (Value)));
   --  Cast Virtual_String to Virtual_File

   function To_Virtual_String
     (Value : GNATCOLL.VFS.Virtual_File) return VSS.Strings.Virtual_String is
     (VSS.Strings.Conversions.To_Virtual_String (Value.Display_Full_Name));
   --  Cast Virtual_File to Virtual_String

   function To_Virtual_String
     (Path : GPR2.Path_Name.Object) return VSS.Strings.Virtual_String is
     (VSS.Strings.Conversions.To_Virtual_String (String (Path.Value)));
   --  Cast GPR2.Path_Name.Object to Virtual_String

end LSP.Utils;
