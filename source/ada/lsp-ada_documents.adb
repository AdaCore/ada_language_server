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

package body LSP.Ada_Documents is
   pragma Warnings (Off);

   -------------------
   -- Apply_Changes --
   -------------------

   procedure Apply_Changes
     (Self   : aliased in out Document; Version : Integer;
      Vector :            LSP.Structures.TextDocumentContentChangeEvent_Vector)
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Apply_Changes unimplemented");
      raise Program_Error with "Unimplemented procedure Apply_Changes";
   end Apply_Changes;

   -------------
   -- Cleanup --
   -------------

   procedure Cleanup (Self : in out Document) is
   begin
      pragma Compile_Time_Warning (Standard.True, "Cleanup unimplemented");
      raise Program_Error with "Unimplemented procedure Cleanup";
   end Cleanup;

   -----------------------------
   -- Compute_Completion_Item --
   -----------------------------

   function Compute_Completion_Item
     (Document                : LSP.Ada_Documents.Document;
      Context                 : LSP.Ada_Contexts.Context;
      Sloc                    : Langkit_Support.Slocs.Source_Location;
      Node : Libadalang.Analysis.Ada_Node; BD : Libadalang.Analysis.Basic_Decl;
      Label : VSS.Strings.Virtual_String; Use_Snippets : Boolean;
      Compute_Doc_And_Details : Boolean; Named_Notation_Threshold : Natural;
      Is_Dot_Call             : Boolean; Is_Visible : Boolean; Pos : Integer;
      Weight                  : Ada_Completions.Completion_Item_Weight_Type;
      Completions_Count       : Natural) return LSP.Structures.CompletionItem
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Compute_Completion_Item unimplemented");
      return
        raise Program_Error
          with "Unimplemented function Compute_Completion_Item";
   end Compute_Completion_Item;

   ----------
   -- Diff --
   ----------

   procedure Diff
     (Self     :     Document; New_Text : VSS.Strings.Virtual_String;
      Old_Span :     LSP.Structures.A_Range := Empty_Range;
      New_Span :     LSP.Structures.A_Range := Empty_Range;
      Edit     : out LSP.Structures.TextEdit_Vector)
   is
   begin
      pragma Compile_Time_Warning (Standard.True, "Diff unimplemented");
      raise Program_Error with "Unimplemented procedure Diff";
   end Diff;

   ------------------
   -- Diff_Symbols --
   ------------------

   procedure Diff_Symbols
     (Self     :        Document; Span : LSP.Structures.A_Range;
      New_Text :        VSS.Strings.Virtual_String;
      Edit     : in out LSP.Structures.TextEdit_Vector)
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Diff_Symbols unimplemented");
      raise Program_Error with "Unimplemented procedure Diff_Symbols";
   end Diff_Symbols;

   -------------------------
   -- Find_All_References --
   -------------------------

   procedure Find_All_References
     (Self       : Document; Context : LSP.Ada_Contexts.Context;
      Definition : Libadalang.Analysis.Defining_Name;
      Callback   : not null access procedure
        (Base_Id : Libadalang.Analysis.Base_Id;
         Kind    : Libadalang.Common.Ref_Result_Kind; Cancel : in out Boolean))
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Find_All_References unimplemented");
      raise Program_Error with "Unimplemented procedure Find_All_References";
   end Find_All_References;

   ----------------
   -- Formatting --
   ----------------

   function Formatting
     (Self     :     Document; Context : LSP.Ada_Contexts.Context;
      Span     :     LSP.Structures.A_Range; Cmd : Pp.Command_Lines.Cmd_Line;
      Edit     : out LSP.Structures.TextEdit_Vector;
      Messages : out VSS.String_Vectors.Virtual_String_Vector) return Boolean
   is
   begin
      pragma Compile_Time_Warning (Standard.True, "Formatting unimplemented");
      return raise Program_Error with "Unimplemented function Formatting";
   end Formatting;

   --------------------
   -- Get_Any_Symbol --
   --------------------

   procedure Get_Any_Symbol
     (Self     : in out Document; Context : LSP.Ada_Contexts.Context;
      Pattern  :        LSP.Search.Search_Pattern'Class;
      Limit    :        Ada.Containers.Count_Type; Only_Public : Boolean;
      Canceled :        access function return Boolean;
      Result   : in out LSP.Ada_Completions.Completion_Maps.Map)
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Get_Any_Symbol unimplemented");
      raise Program_Error with "Unimplemented procedure Get_Any_Symbol";
   end Get_Any_Symbol;

   -------------------------
   -- Get_Completion_Node --
   -------------------------

   procedure Get_Completion_Node
     (Self     :     Document; Context : LSP.Ada_Contexts.Context;
      Position :     LSP.Structures.Position;
      Sloc     : out Langkit_Support.Slocs.Source_Location;
      Token    : out Libadalang.Common.Token_Reference;
      Node     : out Libadalang.Analysis.Ada_Node)
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Get_Completion_Node unimplemented");
      raise Program_Error with "Unimplemented procedure Get_Completion_Node";
   end Get_Completion_Node;

   ------------------------
   -- Get_Completions_At --
   ------------------------

   procedure Get_Completions_At
     (Self      :     Document;
      Providers :     LSP.Ada_Completions.Completion_Provider_List;
      Context   :     LSP.Ada_Contexts.Context;
      Sloc      :     Langkit_Support.Slocs.Source_Location;
      Token     :     Libadalang.Common.Token_Reference;
      Node      :     Libadalang.Analysis.Ada_Node;
      Names     : out Ada_Completions.Completion_Maps.Map;
      Result    : out LSP.Structures.CompletionList)
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Get_Completions_At unimplemented");
      raise Program_Error with "Unimplemented procedure Get_Completions_At";
   end Get_Completions_At;

   ----------------
   -- Get_Errors --
   ----------------

   procedure Get_Errors
     (Self    : in out Document; Context : LSP.Ada_Contexts.Context;
      Changed :    out Boolean; Errors : out LSP.Structures.Diagnostic_Vector;
      Force   :        Boolean := False)
   is
   begin
      pragma Compile_Time_Warning (Standard.True, "Get_Errors unimplemented");
      raise Program_Error with "Unimplemented procedure Get_Errors";
   end Get_Errors;

   ------------------------
   -- Get_Folding_Blocks --
   ------------------------

   procedure Get_Folding_Blocks
     (Self       :     Document; Context : LSP.Ada_Contexts.Context;
      Lines_Only :     Boolean; Comments : Boolean;
      Canceled   :     access function return Boolean;
      Result     : out LSP.Structures.FoldingRange_Vector)
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Get_Folding_Blocks unimplemented");
      raise Program_Error with "Unimplemented procedure Get_Folding_Blocks";
   end Get_Folding_Blocks;

   ---------------------------
   -- Get_Formatting_Region --
   ---------------------------

   function Get_Formatting_Region
     (Self     : Document; Context : LSP.Ada_Contexts.Context;
      Position : LSP.Structures.Position)
      return Laltools.Partial_GNATPP.Formatting_Region_Type
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Get_Formatting_Region unimplemented");
      return
        raise Program_Error
          with "Unimplemented function Get_Formatting_Region";
   end Get_Formatting_Region;

   ---------------------
   -- Get_Indentation --
   ---------------------

   function Get_Indentation
     (Self : Document; Context : LSP.Ada_Contexts.Context; Line : Positive)
      return Natural
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Get_Indentation unimplemented");
      return raise Program_Error with "Unimplemented function Get_Indentation";
   end Get_Indentation;

   -----------------
   -- Get_Node_At --
   -----------------

   function Get_Node_At
     (Self     : Document; Context : LSP.Ada_Contexts.Context;
      Position : LSP.Structures.Position) return Libadalang.Analysis.Ada_Node
   is
   begin
      pragma Compile_Time_Warning (Standard.True, "Get_Node_At unimplemented");
      return raise Program_Error with "Unimplemented function Get_Node_At";
   end Get_Node_At;

   -------------------------
   -- Get_Source_Location --
   -------------------------

   function Get_Source_Location
     (Self : Document'Class; Position : LSP.Structures.Position)
      return Langkit_Support.Slocs.Source_Location
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Get_Source_Location unimplemented");
      return
        raise Program_Error with "Unimplemented function Get_Source_Location";
   end Get_Source_Location;

   --------------------------
   -- Get_Symbol_Hierarchy --
   --------------------------

   procedure Get_Symbol_Hierarchy
     (Self     :     Document; Context : LSP.Ada_Contexts.Context;
      Pattern  :     LSP.Search.Search_Pattern'Class;
      Canceled :     access function return Boolean;
      Result   : out LSP.Structures.DocumentSymbol_Vector)
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Get_Symbol_Hierarchy unimplemented");
      raise Program_Error with "Unimplemented procedure Get_Symbol_Hierarchy";
   end Get_Symbol_Hierarchy;

   -----------------
   -- Get_Symbols --
   -----------------

   procedure Get_Symbols
     (Self     :     Document; Context : LSP.Ada_Contexts.Context;
      Pattern  :     LSP.Search.Search_Pattern'Class;
      Canceled :     access function return Boolean;
      Result   : out LSP.Structures.DocumentSymbol_Vector)
   is
   begin
      pragma Compile_Time_Warning (Standard.True, "Get_Symbols unimplemented");
      raise Program_Error with "Unimplemented procedure Get_Symbols";
   end Get_Symbols;

   -----------------
   -- Get_Text_At --
   -----------------

   function Get_Text_At
     (Self    : Document; Start_Pos : LSP.Structures.Position;
      End_Pos : LSP.Structures.Position) return VSS.Strings.Virtual_String
   is
   begin
      pragma Compile_Time_Warning (Standard.True, "Get_Text_At unimplemented");
      return raise Program_Error with "Unimplemented function Get_Text_At";
   end Get_Text_At;

   ------------------
   -- Get_Token_At --
   ------------------

   function Get_Token_At
     (Self     : Document'Class; Context : LSP.Ada_Contexts.Context;
      Position : LSP.Structures.Position)
      return Libadalang.Common.Token_Reference
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Get_Token_At unimplemented");
      return raise Program_Error with "Unimplemented function Get_Token_At";
   end Get_Token_At;

   ----------------
   -- Get_Tokens --
   ----------------

   function Get_Tokens
     (Self        : Document'Class; Context : LSP.Ada_Contexts.Context;
      Highlighter : LSP.Ada_Highlighters.Ada_Highlighter;
      Span        : LSP.Structures.A_Range := ((1, 1), (0, 0)))
      return LSP.Structures.Natural_Vector
   is
   begin
      pragma Compile_Time_Warning (Standard.True, "Get_Tokens unimplemented");
      return raise Program_Error with "Unimplemented function Get_Tokens";
   end Get_Tokens;

   -----------------
   -- Get_Word_At --
   -----------------

   function Get_Word_At
     (Self     : Document; Context : LSP.Ada_Contexts.Context;
      Position : LSP.Structures.Position) return VSS.Strings.Virtual_String
   is
   begin
      pragma Compile_Time_Warning (Standard.True, "Get_Word_At unimplemented");
      return raise Program_Error with "Unimplemented function Get_Word_At";
   end Get_Word_At;

   ---------------------
   -- Has_Diagnostics --
   ---------------------

   function Has_Diagnostics
     (Self : Document; Context : LSP.Ada_Contexts.Context) return Boolean
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Has_Diagnostics unimplemented");
      return raise Program_Error with "Unimplemented function Has_Diagnostics";
   end Has_Diagnostics;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self       : in out Document; URI : LSP.Structures.DocumentUri;
      Text       :        VSS.Strings.Virtual_String;
      Diagnostic :        LSP.Diagnostic_Sources.Diagnostic_Source_Access)
   is
   begin
      pragma Compile_Time_Warning (Standard.True, "Initialize unimplemented");
      raise Program_Error with "Unimplemented procedure Initialize";
   end Initialize;

   ---------------------
   -- Line_Terminator --
   ---------------------

   function Line_Terminator
     (Self : Document'Class) return VSS.Strings.Virtual_String
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Line_Terminator unimplemented");
      return raise Program_Error with "Unimplemented function Line_Terminator";
   end Line_Terminator;

   ----------------------
   -- Range_Formatting --
   ----------------------

   function Range_Formatting
     (Self     :     Document; Context : LSP.Ada_Contexts.Context;
      Span : LSP.Structures.A_Range; PP_Options : Pp.Command_Lines.Cmd_Line;
      Edit     : out LSP.Structures.TextEdit_Vector;
      Messages : out VSS.String_Vectors.Virtual_String_Vector) return Boolean
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Range_Formatting unimplemented");
      return
        raise Program_Error with "Unimplemented function Range_Formatting";
   end Range_Formatting;

   ------------------------
   -- Reset_Symbol_Cache --
   ------------------------

   procedure Reset_Symbol_Cache (Self : in out Document'Class) is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Reset_Symbol_Cache unimplemented");
      raise Program_Error with "Unimplemented procedure Reset_Symbol_Cache";
   end Reset_Symbol_Cache;

   ---------------------------------------
   -- Set_Completion_Item_Documentation --
   ---------------------------------------

   procedure Set_Completion_Item_Documentation
     (Context : LSP.Ada_Contexts.Context; BD : Libadalang.Analysis.Basic_Decl;
      Item                    : in out LSP.Structures.CompletionItem;
      Compute_Doc_And_Details :        Boolean)
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Set_Completion_Item_Documentation unimplemented");
      raise Program_Error
        with "Unimplemented procedure Set_Completion_Item_Documentation";
   end Set_Completion_Item_Documentation;

   ---------------------
   -- To_LSP_Location --
   ---------------------

   function To_LSP_Location
     (Self : Document; Segment : Langkit_Support.Slocs.Source_Location_Range)
      return LSP.Structures.Location
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "To_LSP_Location unimplemented");
      return raise Program_Error with "Unimplemented function To_LSP_Location";
   end To_LSP_Location;

   ------------------
   -- To_LSP_Range --
   ------------------

   function To_LSP_Range
     (Self : Document; Segment : Langkit_Support.Slocs.Source_Location_Range)
      return LSP.Structures.A_Range
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "To_LSP_Range unimplemented");
      return raise Program_Error with "Unimplemented function To_LSP_Range";
   end To_LSP_Range;

   ----------
   -- Unit --
   ----------

   function Unit
     (Self : Document'Class; Context : LSP.Ada_Contexts.Context)
      return Libadalang.Analysis.Analysis_Unit
   is
   begin
      pragma Compile_Time_Warning (Standard.True, "Unit unimplemented");
      return raise Program_Error with "Unimplemented function Unit";
   end Unit;

   --------------------------
   -- Versioned_Identifier --
   --------------------------

   function Versioned_Identifier
     (Self : Document) return LSP.Structures.VersionedTextDocumentIdentifier
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Versioned_Identifier unimplemented");
      return
        raise Program_Error with "Unimplemented function Versioned_Identifier";
   end Versioned_Identifier;

end LSP.Ada_Documents;
