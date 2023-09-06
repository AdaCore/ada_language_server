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

with Ada.Strings.Unbounded;
with System;

with GNATCOLL.VFS;

with Libadalang.Common;
with Libadalang.Lexer;
with Libadalang.Sources;
with Langkit_Support.Diagnostics;
with Langkit_Support.Symbols;
with Langkit_Support.Token_Data_Handlers;
with Pp.Actions;

with VSS.Strings.Character_Iterators;
with VSS.Strings.Formatters.Generic_Modulars;
with VSS.Strings.Formatters.Strings;
with VSS.Strings.Templates;
with VSS.String_Vectors;
with VSS.Unicode;
with Laltools.Common;

with LSP.Ada_Documents;
with LSP.Constants;
with URIs;

package body LSP.Utils is

   function To_Unbounded_String
     (Input : Standard.Utils.Char_Vectors.Char_Vector)
       return Ada.Strings.Unbounded.Unbounded_String;
   --  Convert Input to unbounded string.

   ------------------
   -- Canonicalize --
   ------------------

   function Canonicalize
     (Text : VSS.Strings.Virtual_String) return VSS.Strings.Virtual_String
   is
      use Langkit_Support.Symbols;

      UTF_32 : constant Wide_Wide_String :=
        VSS.Strings.Conversions.To_Wide_Wide_String (Text);
      Result : constant Symbolization_Result :=
        Libadalang.Sources.Canonicalize (UTF_32);

   begin
      if Result.Success then
         return VSS.Strings.To_Virtual_String (Result.Symbol);
      else
         return VSS.Strings.Empty_Virtual_String;
      end if;
   end Canonicalize;

   -------------------
   -- Format_Vector --
   -------------------

   procedure Format_Vector
     (Cmd       : Standard.Utils.Command_Lines.Command_Line;
      Input     : Standard.Utils.Char_Vectors.Char_Vector;
      Node      : Libadalang.Analysis.Ada_Node;
      In_Sloc   : Langkit_Support.Slocs.Source_Location_Range;
      Output    : out Standard.Utils.Char_Vectors.Char_Vector;
      Out_Sloc  : out Langkit_Support.Slocs.Source_Location_Range;
      Messages  : out Pp.Scanner.Source_Message_Vector)
   is
      use type Langkit_Support.Slocs.Source_Location_Range;

      procedure Tokenize_Output;
      --  Split Output document into tokens and store them into TDH

      procedure Synchronize_Tokens
        (In_Stop   : Libadalang.Common.Token_Reference;
         Out_Stop  : out Langkit_Support.Token_Data_Handlers.Token_Index;
         In_Start  : Libadalang.Common.Token_Reference;
         Out_Start : Langkit_Support.Token_Data_Handlers.Token_Index;
         Ok        : out Boolean);
      --  Find a token in Output document that corresponds to Is_Stop token in
      --  the Input document. Store token index into Out_Stop. To do this
      --  start scanning both token chains starting from In_Start (for Input)
      --  and Out_Start (for Output document). If no corresponding token found
      --  return Ok = False.

      function Lookup_Token
        (Sloc : Langkit_Support.Slocs.Source_Location)
         return Libadalang.Common.Token_Reference;
      --  Like Node.Unit.Lookup_Token, but skip Trivia

      TDH     : Langkit_Support.Token_Data_Handlers.Token_Data_Handler;
      Diags   : Langkit_Support.Diagnostics.Diagnostics_Vectors.Vector;
      Symbols : Langkit_Support.Symbols.Symbol_Table :=
        Langkit_Support.Symbols.Create_Symbol_Table;

      ------------------
      -- Lookup_Token --
      ------------------

      function Lookup_Token
        (Sloc : Langkit_Support.Slocs.Source_Location)
         return Libadalang.Common.Token_Reference
      is
         Result : Libadalang.Common.Token_Reference :=
           Node.Unit.Lookup_Token (Sloc);

      begin
         if Libadalang.Common.Is_Trivia (Result) then
            Result :=
              Libadalang.Common.Previous (Result, Exclude_Trivia => True);
         end if;

         return Result;
      end Lookup_Token;

      ------------------------
      -- Synchronize_Tokens --
      ------------------------

      procedure Synchronize_Tokens
        (In_Stop   : Libadalang.Common.Token_Reference;
         Out_Stop  : out Langkit_Support.Token_Data_Handlers.Token_Index;
         In_Start  : Libadalang.Common.Token_Reference;
         Out_Start : Langkit_Support.Token_Data_Handlers.Token_Index;
         Ok        : out Boolean)
      is
         use type Libadalang.Common.Token_Reference;

         procedure Find_Next_Token
           (Kind  : Libadalang.Common.Token_Kind;
            Index : in out Langkit_Support.Token_Data_Handlers.Token_Index;
            Ok    : out Boolean);
         --  Find nearest token of a given Kind in the Output document starting
         --  from Index. Set Ok to False in no such token found and don't
         --  update Index in this case.

         ---------------------
         -- Find_Next_Token --
         ---------------------

         procedure Find_Next_Token
           (Kind  : Libadalang.Common.Token_Kind;
            Index : in out Langkit_Support.Token_Data_Handlers.Token_Index;
            Ok    : out Boolean)
         is
            use type Langkit_Support.Token_Data_Handlers.Token_Index;
            use type Libadalang.Common.Token_Kind;

            Max_Look_Ahead : constant := 4;  --  How far search for the token
            Next_Kind      : Libadalang.Common.Token_Kind;

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

         Input : Libadalang.Common.Token_Reference;

      begin
         Input := In_Start;
         Out_Stop := Out_Start;
         Ok := True;  --  Now Out_Stop is synchronized with Input

         while Input /= In_Stop loop
            Input := Libadalang.Common.Next (Input, Exclude_Trivia => True);
            Find_Next_Token
              (Libadalang.Common.Kind (Libadalang.Common.Data (Input)),
               Out_Stop,
               Ok);
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

      From       : Libadalang.Common.Token_Reference;
      --  Nearest to range start token (in Input document)
      To         : Libadalang.Common.Token_Reference;
      --  Nearest to range end token (in Input document)
      From_Index : Langkit_Support.Token_Data_Handlers.Token_Index;
      --  Corresponding From-token in Output document
      To_Index   : Langkit_Support.Token_Data_Handlers.Token_Index;
      --  Corresponding To-token in Output document
      Ignore     : Standard.Utils.Char_Vectors.Char_Subrange;
      Ok         : Boolean;

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
           - Libadalang.Common.Sloc_Range
              (Libadalang.Common.Data (From)).Start_Line;

         Out_Sloc.End_Line :=
           Langkit_Support.Token_Data_Handlers.Sloc_End
             (TDH, Langkit_Support.Token_Data_Handlers.Get_Token
                (TDH, To_Index)).Line
           + In_Sloc.End_Line
           - Libadalang.Common.Sloc_Range
              (Libadalang.Common.Data (To)).End_Line;

         Out_Sloc.Start_Column := 1;
         Out_Sloc.End_Column := 1;
      end if;

      Langkit_Support.Token_Data_Handlers.Free (TDH);
      Langkit_Support.Symbols.Destroy (Symbols);
   end Format_Vector;

   -------------------
   -- Get_Decl_Kind --
   -------------------

   function Get_Decl_Kind
     (Node         : Libadalang.Analysis.Basic_Decl;
      Ignore_Local : Boolean := False)
        return LSP.Enumerations.SymbolKind
   is
      use Libadalang.Common;
   begin
      case Node.Kind is
         when Ada_Classic_Subp_Decl |
              Ada_Base_Subp_Body |
              Ada_Entry_Body_Range |
              Ada_Entry_Decl_Range |
              Ada_Generic_Subp_Decl_Range |
              Ada_Generic_Subp_Instantiation_Range |
              Ada_Generic_Subp_Renaming_Decl_Range |
              Ada_Subp_Body_Stub_Range  =>
            return LSP.Enumerations.A_Function;

         when Ada_Component_Decl |
              Ada_Discriminant_Spec =>
            return LSP.Enumerations.Field;

         when Ada_Generic_Formal_Obj_Decl |
              Ada_Param_Spec |
              Ada_Exception_Handler |
              Ada_Object_Decl |
              Ada_Extended_Return_Stmt_Object_Decl |
              Ada_Single_Protected_Decl |
              Ada_Single_Task_Decl =>
            return (if Ignore_Local
                    then LSP.Enumerations.A_Null
                    else
                      (if Laltools.Common.Is_Constant (Node)
                       then LSP.Enumerations.A_Constant
                       else LSP.Enumerations.Variable));

         when Ada_Base_Package_Decl |
              Ada_Generic_Formal_Package |
              --  Ignore: Ada_Generic_Package_Decl kind, this node always have
              --  an Ada_Generic_Package_Internal as a child and we will use it
              --  to create the CompletionItem/DocumentSymbol
              Ada_Generic_Package_Instantiation |
              Ada_Generic_Package_Renaming_Decl |
              Ada_Package_Renaming_Decl =>
            return LSP.Enumerations.A_Package;

         when Ada_Package_Body_Stub |
              Ada_Protected_Body_Stub |
              Ada_Task_Body_Stub |
              Ada_Package_Body |
              Ada_Protected_Body |
              Ada_Task_Body =>
            return LSP.Enumerations.Module;

         when Ada_Concrete_Type_Decl |
              Ada_Formal_Type_Decl =>
            return (if Laltools.Common.Is_Structure (Node)
                    then LSP.Enumerations.Struct
                    else LSP.Enumerations.Class);

         when Ada_Generic_Formal_Type_Decl |
              Ada_Classwide_Type_Decl |
              Ada_Incomplete_Type_Decl |
              Ada_Incomplete_Tagged_Type_Decl |
              Ada_Protected_Type_Decl |
              Ada_Task_Type_Decl |
              Ada_Subtype_Decl |
              Ada_Anonymous_Type_Decl |
              Ada_Synth_Anonymous_Type_Decl =>
            return LSP.Enumerations.Class;

         when Ada_Entry_Index_Spec |
              Ada_Number_Decl =>
            return LSP.Enumerations.Number;

         when Ada_Enum_Literal_Decl =>
            return (if Ignore_Local
                    then LSP.Enumerations.A_Null
                    else LSP.Enumerations.Enum);

         when Ada_Exception_Decl =>
            return LSP.Enumerations.String;

         when Ada_For_Loop_Var_Decl |
              Ada_Label_Decl |
              Ada_Named_Stmt_Decl =>
            return (if Ignore_Local
                    then LSP.Enumerations.A_Null
                    else LSP.Enumerations.A_Constant);

         when others =>
            null;
      end case;

      return LSP.Enumerations.A_Null;
   end Get_Decl_Kind;

   ------------------
   -- Get_Location --
   ------------------

   function Get_Location
     (Unit : Libadalang.Analysis.Analysis_Unit;
      Span : Langkit_Support.Slocs.Source_Location_Range)
      return LSP.Structures.Location is
   begin
      return
        (uri     =>
           (VSS.Strings.Conversions.To_Virtual_String
                (URIs.Conversions.From_File (Unit.Get_Filename))
            with null record),
         a_range => To_Range (Span),
         alsKind => LSP.Constants.Empty);
   end Get_Location;

   -----------------------
   -- Get_Node_Location --
   -----------------------

   function Get_Node_Location
     (Node : Libadalang.Analysis.Ada_Node'Class)
      return LSP.Structures.Location is
   begin
      return Get_Location (Node.Unit, Node.Sloc_Range);
   end Get_Node_Location;

   ------------------
   -- Is_Synthetic --
   ------------------

   function Is_Synthetic
     (Node : Libadalang.Analysis.Ada_Node'Class) return Boolean
   is
      use type Libadalang.Analysis.Analysis_Unit;

      Std : constant Libadalang.Analysis.Analysis_Unit := Node.Unit;

      Unit : constant Libadalang.Analysis.Analysis_Unit :=
        Node.P_Standard_Unit;
   begin

      return Std = Unit;
   end Is_Synthetic;

   ----------
   -- Hash --
   ----------

   function Hash
     (Value : LSP.Structures.A_Range) return Ada.Containers.Hash_Type
   is
      use type Ada.Containers.Hash_Type;
      Prime : constant := 271;
      From  : constant Ada.Containers.Hash_Type :=
        Prime * Ada.Containers.Hash_Type'Mod (Value.start.line)
        + Ada.Containers.Hash_Type'Mod (Value.start.character);
      To    : constant Ada.Containers.Hash_Type :=
        Prime * Ada.Containers.Hash_Type'Mod (Value.an_end.line)
        + Ada.Containers.Hash_Type'Mod (Value.an_end.character);
   begin
      return From + To;
   end Hash;

   -------------------------
   -- Node_Location_Image --
   -------------------------

   function Node_Location_Image
     (Node : Libadalang.Analysis.Ada_Node'Class)
      return VSS.Strings.Virtual_String
   is
      package Line_Number_Formatters is
        new VSS.Strings.Formatters.Generic_Modulars
          (Libadalang.Slocs.Line_Number);

      package Column_Number_Formatters is
        new VSS.Strings.Formatters.Generic_Modulars
          (Libadalang.Slocs.Column_Number);

      File     : constant GNATCOLL.VFS.Virtual_File :=
        GNATCOLL.VFS.Create_From_UTF8 (Node.Unit.Get_Filename);
      Template : constant VSS.Strings.Templates.Virtual_String_Template :=
        "at {} ({}:{})";

   begin
      return
        Template.Format
          (VSS.Strings.Formatters.Strings.Image
             (VSS.Strings.Conversions.To_Virtual_String
                (File.Display_Base_Name)),
           Line_Number_Formatters.Image (Node.Sloc_Range.Start_Line),
           Column_Number_Formatters.Image (Node.Sloc_Range.Start_Column));
   end Node_Location_Image;

   -------------------
   -- Span_To_Slice --
   -------------------

   procedure Span_To_Slice
     (Text  : VSS.Strings.Virtual_String;
      Span  : LSP.Structures.A_Range;
      Slice : out VSS.Strings.Virtual_String)
   is
      use type VSS.Unicode.UTF16_Code_Unit_Offset;

      Dummy : Boolean;
      Lines : VSS.String_Vectors.Virtual_String_Vector;
      Line  : VSS.Strings.Virtual_String;
      Num   : Natural := Span.start.line + 1;

   begin
      Lines :=
        Text.Split_Lines
          (Terminators     => LSP.Ada_Documents.LSP_New_Line_Function_Set,
           Keep_Terminator => True);
      Line := Lines (Num);

      declare
         J1 : VSS.Strings.Character_Iterators.Character_Iterator :=
           Line.At_First_Character;
         U1 : constant VSS.Unicode.UTF16_Code_Unit_Offset :=
           J1.First_UTF16_Offset;

      begin
         while VSS.Unicode.UTF16_Code_Unit_Offset (Span.start.character)
                 /= J1.First_UTF16_Offset - U1
           and then J1.Forward
         loop
            null;
         end loop;

         if Span.start.line /= Span.an_end.line then
            Slice.Append (Line.Slice (J1.Marker, Line.At_Last_Character));
         end if;

         loop
            Num := Num + 1;

            exit when Num > Span.an_end.line;

            Slice.Append (Lines (Num));
         end loop;

         Line := Lines (Span.an_end.line + 1);

         declare
            J2 : VSS.Strings.Character_Iterators.Character_Iterator :=
              Line.At_First_Character;
            U2 : constant VSS.Unicode.UTF16_Code_Unit_Offset :=
              J2.First_UTF16_Offset;
         begin
            while VSS.Unicode.UTF16_Code_Unit_Offset (Span.an_end.character)
                    /= J2.First_UTF16_Offset - U2
              and then J2.Forward
            loop
               null;
            end loop;

            Dummy := J2.Backward;

            if Span.start.line /= Span.an_end.line then
               Slice.Append (Line.Slice (Line.At_First_Character, J2));

            else
               Slice.Append (Line.Slice (J1, J2));
            end if;
         end;
      end;
   end Span_To_Slice;

   --------------
   -- To_Range --
   --------------

   function To_Range
     (Value : Langkit_Support.Slocs.Source_Location_Range)
      return LSP.Structures.A_Range
   is
      use type Langkit_Support.Slocs.Source_Location_Range;

      Result : constant LSP.Structures.A_Range :=
        (if Value = Langkit_Support.Slocs.No_Source_Location_Range then
           LSP.Constants.Empty
         else
           (start =>
                (line      => Natural (Value.Start_Line) - 1,
                 character => Natural --  FIXME (UTF16 index)!
                   (Value.Start_Column) - 1),
            an_end =>
              (line      => Natural (Value.End_Line) - 1,
               character => Natural --  FIXME (UTF16 index)!
                 (Value.End_Column) - 1)));
         --  XXX Code unit offset computation is incorrect here

   begin
      return Result;
   end To_Range;

   -------------------------
   -- To_Unbounded_String --
   -------------------------

   function To_Unbounded_String
     (Input : Standard.Utils.Char_Vectors.Char_Vector)
      return Ada.Strings.Unbounded.Unbounded_String is
   begin
      return Result : Ada.Strings.Unbounded.Unbounded_String do
         for Char of Input loop
            Ada.Strings.Unbounded.Append (Result, Char);
         end loop;
      end return;
   end To_Unbounded_String;

end LSP.Utils;
