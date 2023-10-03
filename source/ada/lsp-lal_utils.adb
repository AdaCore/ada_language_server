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

with Ada.Strings.Unbounded;
with Ada.Strings.Wide_Wide_Unbounded;
with System;

with GNATCOLL.Utils;

with VSS.Strings.Character_Iterators;
with VSS.Strings.Conversions;
with VSS.String_Vectors;
with VSS.Unicode;

with Langkit_Support;
with Langkit_Support.Symbols; use Langkit_Support.Symbols;
with Libadalang.Common;       use Libadalang.Common;
with Libadalang.Sources;

with Laltools.Call_Hierarchy;

with Libadalang.Lexer;
with Langkit_Support.Diagnostics;
pragma Warnings (Off, "redundant with clause");
with Langkit_Support.Symbols;  --  Fails with gnat ce 2020
pragma Warnings (On, "redundant with clause");
with Langkit_Support.Token_Data_Handlers;

with Pp.Actions;

with LSP.Common;
with LSP.Types;             use LSP.Types;

package body LSP.Lal_Utils is

   function To_Unbounded_String
     (Input : Utils.Char_Vectors.Char_Vector)
       return Ada.Strings.Unbounded.Unbounded_String;
   --  Convert Input to unbounded string.

   ---------------------
   -- Append_Location --
   ---------------------

   procedure Append_Location
     (Result : in out LSP.Messages.Location_Or_Link_Vector;
      Node   : Libadalang.Analysis.Ada_Node'Class;
      Kind   : LSP.Messages.AlsReferenceKind_Set := LSP.Messages.Empty_Set)
   is
   begin
      Append_Location (Result.Locations, Node, Kind);
   end Append_Location;

   ---------------------
   -- Append_Location --
   ---------------------

   procedure Append_Location
     (Result : in out LSP.Messages.Location_Vector;
      Node   : Libadalang.Analysis.Ada_Node'Class;
      Kind   : LSP.Messages.AlsReferenceKind_Set := LSP.Messages.Empty_Set)
   is
      Location : constant LSP.Messages.Location :=
        LSP.Lal_Utils.Get_Node_Location
          (Libadalang.Analysis.As_Ada_Node (Node), Kind);
   begin
      if not Is_Synthetic (Node) then
         Result.Append (Location);
      end if;
   end Append_Location;

   ---------------------
   -- Append_Location --
   ---------------------

   procedure Append_Location
     (Result   : in out LSP.Messages.DocumentHighlight_Vector;
      Document : not null access LSP.Ada_Documents.Document'Class;
      File     : GNATCOLL.VFS.Virtual_File;
      Node     : Libadalang.Analysis.Ada_Node'Class;
      Kind     : LSP.Messages.Optional_DocumentHighlightKind)
   is
      use type GNATCOLL.VFS.Virtual_File;

      Node_File : constant GNATCOLL.VFS.Virtual_File :=
        GNATCOLL.VFS.Create_From_UTF8 (Node.Unit.Get_Filename);

   begin
      if File = Node_File then
         Result.Append
           (LSP.Messages.DocumentHighlight'
              (span => Document.To_LSP_Range (Node.Sloc_Range),
               kind => Kind));
      end if;
   end Append_Location;

   ------------------------
   -- Get_Token_Location --
   ------------------------

   function Get_Token_Span
     (Token : Libadalang.Common.Token_Reference)
      return LSP.Messages.Span is
   begin
      return To_Span (Sloc_Range (Data (Token)));
   end Get_Token_Span;

   -------------
   -- To_Span --
   -------------

   function To_Span
     (Value : Langkit_Support.Slocs.Source_Location)
      return LSP.Messages.Span
   is
      use type VSS.Unicode.UTF16_Code_Unit_Count;

      Result : constant LSP.Messages.Span :=
        (first =>
           (line      => LSP.Types.Line_Number (Value.Line) - 1,
            character => LSP.Types.UTF_16_Index
              (Value.Column) - 1),
         last =>
           (line => LSP.Types.Line_Number (Value.Line) - 1,
            character => LSP.Types.UTF_16_Index
              (Value.Column) - 1));
   begin
      return Result;
   end To_Span;

   -----------------
   -- To_TextEdit --
   -----------------

   function To_TextEdit
     (E : LAL_Refactor.Text_Edit)
      return LSP.Messages.TextEdit
   is (LSP.Messages.TextEdit'
         (To_Span (E.Location),
          VSS.Strings.Conversions.To_Virtual_String (E.Text)));

   -----------------------
   -- To_Workspace_Edit --
   -----------------------

   function To_Workspace_Edit
     (EM                  : LAL_Refactor.Text_Edit_Map;
      Versioned_Documents : Boolean := False;
      Document_Provider   : access LSP.Ada_Documents.Document_Provider'Class
      := null)
      return LSP.Messages.WorkspaceEdit
   is
      File_URI : LSP.Messages.DocumentUri;

      Text_Edits : LSP.Messages.TextEdit_Vector;

      use LAL_Refactor.Text_Edit_Ordered_Maps;

      Edits_Cursor : Cursor := EM.First;

   begin
      return WE : LSP.Messages.WorkspaceEdit do
         while Has_Element (Edits_Cursor) loop
            Text_Edits.Clear;

            for Edit of Element (Edits_Cursor) loop
               Text_Edits.Append (To_TextEdit (Edit));
            end loop;

            File_URI := LSP.Types.File_To_URI (Key (Edits_Cursor));

            if Versioned_Documents then
               declare
                  Annotaded_Edits : LSP.Messages.AnnotatedTextEdit_Vector;
               begin
                  Annotaded_Edits.Reserve_Capacity (Text_Edits.Capacity);
                  for X of Text_Edits loop
                     Annotaded_Edits.Append
                       (LSP.Messages.AnnotatedTextEdit'
                          (X with annotationId => <>));
                  end loop;

                  WE.documentChanges.Append
                    (LSP.Messages.Document_Change'(
                     (Kind               => LSP.Messages.Text_Document_Edit,
                      Text_Document_Edit => LSP.Messages.TextDocumentEdit'
                        (textDocument => Document_Provider.
                                          Get_Open_Document_Version (File_URI),
                         edits        => Annotaded_Edits))));
               end;
            else
               WE.changes.Insert (File_URI, Text_Edits);
            end if;

            Next (Edits_Cursor);
         end loop;
      end return;
   end To_Workspace_Edit;

   -----------------------
   -- Containing_Entity --
   -----------------------

   function Containing_Entity
     (Ref       : Ada_Node;
      Canonical : Boolean := True) return Defining_Name
   is
      Parents : constant Ada_Node_Array := Ref.Parents;
   begin
      for Parent of Parents loop
         if Parent.Kind in Ada_Subp_Decl
                         | Ada_Subp_Body
                         | Ada_Task_Def
                         | Ada_Task_Body
                         | Ada_Package_Body
                         | Ada_Package_Decl
         then
            if Canonical then
               return Parent.As_Basic_Decl.P_Canonical_Part.P_Defining_Name;
            else
               return Parent.As_Basic_Decl.P_Defining_Name;
            end if;
         end if;
      end loop;

      return No_Defining_Name;
   end Containing_Entity;

   -----------------------
   -- To_Virtual_String --
   -----------------------

   function To_Virtual_String
     (Item : Langkit_Support.Text.Text_Type)
      return VSS.Strings.Virtual_String is
   begin
      return VSS.Strings.To_Virtual_String (Item);
   end To_Virtual_String;

   -----------------------
   -- To_Virtual_String --
   -----------------------

   function To_Virtual_String
     (Item : Langkit_Support.Text.Unbounded_Text_Type)
      return VSS.Strings.Virtual_String is
   begin
      return
        VSS.Strings.To_Virtual_String
          (Ada.Strings.Wide_Wide_Unbounded.To_Wide_Wide_String (Item));
   end To_Virtual_String;

   -------------
   -- Is_Task --
   -------------

   --  TODO: Reactivate these lines when libadalang supports
   --  P_Next_Part for tasks: T716-049

   --  function Is_Task
   --    (Node      : Ada_Node'Class;
   --     Trace     : GNATCOLL.Traces.Trace_Handle;
   --     Imprecise : out Boolean) return Boolean is
   --  begin
   --     return Ada_Node (N) /= No_Ada_Node
   --       and then N.Kind in Ada_Name
   --       and then (N.P_Basic_Decl.Kind = Ada_Task_Body or else
   --                 N.P_Basic_Decl.Kind = Ada_Single_Task_Type_Decl or else
   --                 N.P_Basic_Decl.Kind = Ada_Task_Type_Decl);
   --  end Is_Task;

   -------------------------
   -- To_Unbounded_String --
   -------------------------

   function To_Unbounded_String
     (Input : Utils.Char_Vectors.Char_Vector)
      return Ada.Strings.Unbounded.Unbounded_String is
   begin
      return Result : Ada.Strings.Unbounded.Unbounded_String do
         for Char of Input loop
            Ada.Strings.Unbounded.Append (Result, Char);
         end loop;
      end return;
   end To_Unbounded_String;

   -------------------
   -- Format_Vector --
   -------------------

   procedure Format_Vector
     (Cmd       : Utils.Command_Lines.Command_Line;
      Input     : Utils.Char_Vectors.Char_Vector;
      Node      : Ada_Node;
      In_Sloc   : Langkit_Support.Slocs.Source_Location_Range;
      Output    : out Utils.Char_Vectors.Char_Vector;
      Out_Sloc  : out Langkit_Support.Slocs.Source_Location_Range;
      Messages  : out Pp.Scanner.Source_Message_Vector)
   is
      use type Langkit_Support.Slocs.Source_Location_Range;

      procedure Tokenize_Output;
      --  Split Output document into tokens and store them into TDH

      procedure Synchronize_Tokens
        (In_Stop   : Token_Reference;
         Out_Stop  : out Langkit_Support.Token_Data_Handlers.Token_Index;
         In_Start  : Token_Reference;
         Out_Start : Langkit_Support.Token_Data_Handlers.Token_Index;
         Ok        : out Boolean);
      --  Find a token in Output document that corresponds to Is_Stop token in
      --  the Input document. Store token index into Out_Stop. To do this
      --  start scanning both token chains starting from In_Start (for Input)
      --  and Out_Start (for Output document). If no corresponding token found
      --  return Ok = False.

      function Lookup_Token
        (Sloc : Langkit_Support.Slocs.Source_Location) return Token_Reference;
      --  Like Node.Unit.Lookup_Token, but skip Trivia

      TDH     : Langkit_Support.Token_Data_Handlers.Token_Data_Handler;
      Diags   : Langkit_Support.Diagnostics.Diagnostics_Vectors.Vector;
      Symbols : Langkit_Support.Symbols.Symbol_Table :=
        Langkit_Support.Symbols.Create_Symbol_Table;

      ------------------
      -- Lookup_Token --
      ------------------

      function Lookup_Token
        (Sloc : Langkit_Support.Slocs.Source_Location) return Token_Reference
      is
         Result : Token_Reference := Node.Unit.Lookup_Token (Sloc);
      begin
         if Is_Trivia (Result) then
            Result := Previous (Result, Exclude_Trivia => True);
         end if;

         return Result;
      end Lookup_Token;

      ------------------------
      -- Synchronize_Tokens --
      ------------------------

      procedure Synchronize_Tokens
        (In_Stop   : Token_Reference;
         Out_Stop  : out Langkit_Support.Token_Data_Handlers.Token_Index;
         In_Start  : Token_Reference;
         Out_Start : Langkit_Support.Token_Data_Handlers.Token_Index;
         Ok        : out Boolean)
      is
         procedure Find_Next_Token
           (Kind  : Token_Kind;
            Index : in out Langkit_Support.Token_Data_Handlers.Token_Index;
            Ok    : out Boolean);
         --  Find nearest token of a given Kind in the Output document starting
         --  from Index. Set Ok to False in no such token found and don't
         --  update Index in this case.

         ---------------------
         -- Find_Next_Token --
         ---------------------

         procedure Find_Next_Token
           (Kind  : Token_Kind;
            Index : in out Langkit_Support.Token_Data_Handlers.Token_Index;
            Ok    : out Boolean)
         is
            use type Langkit_Support.Token_Data_Handlers.Token_Index;
            Max_Look_Ahead : constant := 4;  --  How far search for the token

            Next_Kind : Token_Kind;
         begin
            Ok := False;

            for J in Index + 1 .. Index + Max_Look_Ahead loop
               Next_Kind := Libadalang.Common.To_Token_Kind
                 (Langkit_Support.Token_Data_Handlers.Get_Token
                    (TDH, J).Kind);

               if Next_Kind = Kind then
                  Ok := True;
                  Index := J;
                  exit;
               end if;
            end loop;
         end Find_Next_Token;

         Input : Token_Reference;
      begin
         Input := In_Start;
         Out_Stop := Out_Start;
         Ok := True;  --  Now Out_Stop is synchronized with Input

         while Input /= In_Stop loop
            Input := Next (Input, Exclude_Trivia => True);
            Find_Next_Token (Kind (Data (Input)), Out_Stop, Ok);
         end loop;
      end Synchronize_Tokens;

      ---------------------
      -- Tokenize_Output --
      ---------------------

      procedure Tokenize_Output is
         Input : constant Libadalang.Lexer.Lexer_Input :=
           (Kind     => Libadalang.Common.Bytes_Buffer,
            Charset  => Ada.Strings.Unbounded.To_Unbounded_String ("utf-8"),
            Read_BOM => False,
            Bytes    => To_Unbounded_String (Output));

      begin
         Langkit_Support.Token_Data_Handlers.Initialize
           (TDH, Symbols, System.Null_Address);

         Libadalang.Lexer.Extract_Tokens
           (Input,
            TDH         => TDH,
            Diagnostics => Diags,
            With_Trivia => True);
      end Tokenize_Output;

      use type Langkit_Support.Slocs.Line_Number;

      From : Token_Reference;
      --  Nearest to range start token (in Input document)
      To   : Token_Reference;
      --  Nearest to range end token (in Input document)
      From_Index : Langkit_Support.Token_Data_Handlers.Token_Index;
      --  Corresponding From-token in Output document
      To_Index : Langkit_Support.Token_Data_Handlers.Token_Index;
      --  Corresponding To-token in Output document
      Ignore : Utils.Char_Vectors.Char_Subrange;
      Ok : Boolean;

   begin
      Pp.Actions.Format_Vector
        (Cmd, Input, Node, Output, Messages);

      if In_Sloc = Langkit_Support.Slocs.No_Source_Location_Range then
         --  Return full range of Output
         Out_Sloc := In_Sloc;
         Langkit_Support.Symbols.Destroy (Symbols);
         return;
      elsif Node.Unit.Token_Count = 0 then  --  Ignore a cornercase for now
         Out_Sloc := Langkit_Support.Slocs.No_Source_Location_Range;
         Langkit_Support.Symbols.Destroy (Symbols);
         return;
      end if;

      Tokenize_Output;  --  Fill TDH
      From := Lookup_Token (Langkit_Support.Slocs.Start_Sloc (In_Sloc));
      To := Lookup_Token (Langkit_Support.Slocs.End_Sloc (In_Sloc));

      Synchronize_Tokens
        (In_Stop   => From,
         Out_Stop  => From_Index,
         In_Start  => Node.Unit.First_Token,
         Out_Start => Langkit_Support.Token_Data_Handlers.First_Token_Index,
         Ok        => Ok);

      if Ok then
         Synchronize_Tokens
           (In_Stop   => To,
            Out_Stop  => To_Index,
            In_Start  => From,
            Out_Start => From_Index,
            Ok        => Ok);
      end if;

      if Ok then
         Out_Sloc.Start_Line :=
           Langkit_Support.Token_Data_Handlers.Sloc_Start
             (TDH, Langkit_Support.Token_Data_Handlers.Get_Token
                (TDH, From_Index)).Line
           + In_Sloc.Start_Line
           - Sloc_Range (Data (From)).Start_Line;

         Out_Sloc.End_Line :=
           Langkit_Support.Token_Data_Handlers.Sloc_End
             (TDH, Langkit_Support.Token_Data_Handlers.Get_Token
                (TDH, To_Index)).Line
           + In_Sloc.End_Line
           - Sloc_Range (Data (To)).End_Line;

         Out_Sloc.Start_Column := 1;
         Out_Sloc.End_Column := 1;
      end if;

      Langkit_Support.Token_Data_Handlers.Free (TDH);
      Langkit_Support.Symbols.Destroy (Symbols);
   end Format_Vector;

   ------------------
   -- Is_End_Token --
   ------------------

   function Is_End_Token (Token : Libadalang.Common.Token_Reference)
                             return Boolean
   is
      End_Token : constant Libadalang.Common.Token_Data_Type :=
        Libadalang.Common.Data (Token);

      Token_Kind : constant Libadalang.Common.Token_Kind :=
        Libadalang.Common.Kind (End_Token);
   begin
      return Token_Kind = Libadalang.Common.Ada_End;
   end Is_End_Token;

   -----------------------
   -- Skip_Dotted_Names --
   -----------------------

   function Skip_Dotted_Names (Node : Libadalang.Analysis.Ada_Node)
                                  return Libadalang.Analysis.Ada_Node
   is
      Parent : Libadalang.Analysis.Ada_Node := Node;
   begin
      while not Parent.Is_Null
        and then Parent.Kind = Libadalang.Common.Ada_Dotted_Name
      loop
         Parent := Parent.Parent;
      end loop;

      return Parent;
   end Skip_Dotted_Names;

   -------------------
   -- Span_To_Slice --
   -------------------

   procedure Span_To_Slice
     (Text  : VSS.Strings.Virtual_String;
      Span  : LSP.Messages.Span;
      Slice : out VSS.Strings.Virtual_String)
   is
      use type VSS.Unicode.UTF16_Code_Unit_Offset;
      Dummy : Boolean;
      Lines : VSS.String_Vectors.Virtual_String_Vector;
      Line  : VSS.Strings.Virtual_String;
      Num   : Natural := Natural (Span.first.line) + 1;
   begin
      Lines :=
        Text.Split_Lines
          (Terminators     => LSP.Common.LSP_New_Line_Function_Set,
           Keep_Terminator => True);
      Line := Lines.Element (Num);

      declare
         J1 : VSS.Strings.Character_Iterators.Character_Iterator :=
           Line.At_First_Character;
         U1 : constant VSS.Unicode.UTF16_Code_Unit_Offset :=
           J1.First_UTF16_Offset;
      begin
         while Span.first.character /= J1.First_UTF16_Offset - U1
           and then J1.Forward
         loop
            null;
         end loop;

         if Span.first.line /= Span.last.line then
            Slice.Append
              (Line.Slice (J1.Marker, Line.At_Last_Character.Marker));
         end if;

         loop
            Num := Num + 1;
            exit when Num > Natural (Span.last.line);
            Slice.Append (Lines.Element (Num));
         end loop;

         Line := Lines.Element (Natural (Span.last.line) + 1);
         declare
            J2 : VSS.Strings.Character_Iterators.Character_Iterator :=
              Line.At_First_Character;
            U2 : constant VSS.Unicode.UTF16_Code_Unit_Offset :=
              J2.First_UTF16_Offset;
         begin
            while Span.last.character /= J2.First_UTF16_Offset - U2
              and then J2.Forward
            loop
               null;
            end loop;
            Dummy := J2.Backward;

            if Span.first.line /= Span.last.line then
               Slice.Append
                 (Line.Slice (Line.At_First_Character.Marker, J2.Marker));
            else
               Slice.Append (Line.Slice (J1.Marker, J2.Marker));
            end if;
         end;
      end;
   end Span_To_Slice;

end LSP.Lal_Utils;
