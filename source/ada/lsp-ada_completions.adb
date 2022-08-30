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

with Ada.Characters.Latin_1;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Hashed_Sets;
with Ada.Strings;
with Ada.Strings.Hash;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GNATCOLL.Traces;

with GNAT.Regpat;           use GNAT.Regpat;
with GNAT.Strings;

with Langkit_Support;
with Libadalang.Analysis;   use Libadalang.Analysis;

with LSP.Ada_Contexts;
with LSP.Ada_Documents;
with LSP.Common;
with LSP.Lal_Utils;
with LSP.Messages;          use LSP.Messages;

with Pp.Actions;
with Pp.Command_Lines;
with Pp.Scanner;
with Utils.Char_Vectors;
with Utils.Command_Lines;
with Utils.Command_Lines.Common;
with VSS.Strings;
with VSS.Strings.Conversions;

package body LSP.Ada_Completions is

   Me_Formatting : constant GNATCOLL.Traces.Trace_Handle :=
     GNATCOLL.Traces.Create
       ("ALS.COMPLETION.FORMATTING", Default => GNATCOLL.Traces.Off);

   ------------------------
   -- Is_Full_Sloc_Equal --
   ------------------------

   function Is_Full_Sloc_Equal
     (Left, Right : Libadalang.Analysis.Defining_Name) return Boolean is
   begin
      return Libadalang.Analysis.Full_Sloc_Image
          (Left) = Libadalang.Analysis.Full_Sloc_Image (Right);
   end Is_Full_Sloc_Equal;

   -----------------------
   -- Write_Completions --
   -----------------------

   procedure Write_Completions
     (Context                  : LSP.Ada_Contexts.Context;
      Document                 : LSP.Ada_Documents.Document;
      Sloc                     : Langkit_Support.Slocs.Source_Location;
      Node                     : Libadalang.Analysis.Ada_Node;
      Names                    : Completion_Maps.Map;
      Named_Notation_Threshold : Natural;
      Compute_Doc_And_Details  : Boolean;
      Result                   : in out LSP.Messages.CompletionItem_Vector)
   is
      function Hash (Value : VSS.Strings.Virtual_String)
        return Ada.Containers.Hash_Type
          is (Ada.Containers.Hash_Type'Mod (Value.Hash));

      package String_Sets is new Ada.Containers.Hashed_Sets
        (VSS.Strings.Virtual_String, Hash, VSS.Strings."=", VSS.Strings."=");

      Seen   : String_Sets.Set;
      --  Set of found visible names in canonical form
      Length : constant Natural := Natural (Names.Length);
   begin

      --  Write Result in two pases. Firstly append all visible names and
      --  populate Seen set. Then append invisible names not in Seen.

      for Visible in reverse Boolean loop  --  Phase: True then False
         for Cursor in Names.Iterate loop
            declare
               Append    : Boolean := False;
               Info      : constant Name_Information := Names (Cursor);
               Name      : constant Libadalang.Analysis.Defining_Name :=
                 Completion_Maps.Key (Cursor);
               Selector  : constant Libadalang.Analysis.Single_Tok_Node :=
                 Name.P_Relative_Name;
               Label     : VSS.Strings.Virtual_String;
               Canonical : VSS.Strings.Virtual_String;
            begin
               if Visible and Info.Is_Visible then
                  Label := LSP.Lal_Utils.To_Virtual_String (Selector.Text);
                  Canonical := LSP.Lal_Utils.Canonicalize (Label);
                  Seen.Include (Canonical);
                  Append := True;
               elsif not Visible and not Info.Is_Visible then
                  --  Append invisible name on if no such visible name found
                  Label := LSP.Lal_Utils.To_Virtual_String (Selector.Text);
                  Canonical := LSP.Lal_Utils.Canonicalize (Label);
                  Append := not Seen.Contains (Canonical);
               end if;

               if Append then
                  Result.Append
                    (LSP.Ada_Documents.Compute_Completion_Item
                       (Document                 => Document,
                        Context                  => Context,
                        Sloc                     => Sloc,
                        Node                     => Node,
                        BD                       => Name.P_Basic_Decl,
                        Label                    => Label,
                        Use_Snippets             => Info.Use_Snippets,
                        Compute_Doc_And_Details  => Compute_Doc_And_Details,
                        Named_Notation_Threshold => Named_Notation_Threshold,
                        Is_Dot_Call              => Info.Is_Dot_Call,
                        Is_Visible               => Info.Is_Visible,
                        Pos                      => Info.Pos,
                        Weight                   => Info.Weight,
                        Completions_Count        => Length));
               end if;
            end;
         end loop;
      end loop;
   end Write_Completions;

   --------------------------
   -- Pretty_Print_Snippet --
   --------------------------

   procedure Pretty_Print_Snippet
     (Context : LSP.Ada_Contexts.Context;
      Prefix  : String;
      Offset  : Natural;
      Span    : LSP.Messages.Span;
      Rule    : Libadalang.Common.Grammar_Rule;
      Result  : in out LSP.Messages.CompletionItem)
   is

      package Encoding_Maps is new Ada.Containers.Indefinite_Hashed_Maps
        (Key_Type        => String,
         Element_Type    => String,
         Hash            => Ada.Strings.Hash,
         Equivalent_Keys => "=");

      Encoding : Encoding_Maps.Map;
      --  Map of Snippet fake name to snippet:
      --  {"Foo_1" : "$1", "Foo_2" : "${2: Integer}"}
      --  $0 will not be replaced

      procedure Set_PP_Switches
        (Cmd : in out Utils.Command_Lines.Command_Line);
      --  Force switches not enabled by default by GNATpp

      function Encode_String (S : String) return String;
      --  Create pseudo code for the snippet

      function Decode_String (S : String) return String;
      --  Recreate the snippet via the pseudo code

      function Snippet_Placeholder (S : String) return String
      is ("FooBar_" & S);
      --  Generate a fake entity for a snippet (S is the index of the snippet)
      --  The length of the placeholder will affect where the strings is cut
      --  to avoid exceeding line length => 8/9 characters seems to be
      --  reasonable (most of the time a placeholder is replacing the
      --  parameter type's name)

      function Find_Next_Placeholder
        (S     : String;
         Start : Integer)
         return Match_Location;
      --  Find the next placeholder starting from Start in S

      function Post_Pretty_Print (S : String) return String;
      --  Indent the block using the initial location and add back $0 (it has
      --  been removed to not generate invalid pseudo code)

      ---------------------
      -- Set_PP_Switches --
      ---------------------

      procedure Set_PP_Switches
        (Cmd : in out Utils.Command_Lines.Command_Line) is

      begin
         --  If not set by the user: align parameters and aggregates

         if not Pp.Command_Lines.Pp_Nat_Switches.Explicit
           (Cmd, Pp.Command_Lines.Call_Threshold)
         then
            Pp.Command_Lines.Pp_Nat_Switches.Set_Arg
              (Cmd, Pp.Command_Lines.Call_Threshold, 1);
         end if;

         if not Pp.Command_Lines.Pp_Nat_Switches.Explicit
           (Cmd, Pp.Command_Lines.Par_Threshold)
         then
            Pp.Command_Lines.Pp_Nat_Switches.Set_Arg
              (Cmd, Pp.Command_Lines.Par_Threshold, 1);
         end if;

         if not Pp.Command_Lines.Pp_Boolean_Switches.Explicit
           (Cmd, Pp.Command_Lines.Vertical_Named_Aggregates)
         then
            Pp.Command_Lines.Pp_Boolean_Switches.Set_Arg
              (Cmd, Pp.Command_Lines.Vertical_Named_Aggregates);
         end if;
      end Set_PP_Switches;

      -------------------
      -- Encode_String --
      -------------------

      function Encode_String (S : String) return String
      is
         Res           : Unbounded_String;
         Start_Encoded : Natural := 0;
         Search_Index  : Boolean := False;
         Encoded_Index : Unbounded_String;
         In_Snippet    : Boolean := False;

         procedure Add_Placeholder (Cursor : Natural);

         ---------------------
         -- Add_Placeholder --
         ---------------------

         procedure Add_Placeholder (Cursor : Natural) is
            Placeholder : constant String :=
              Snippet_Placeholder (To_String (Encoded_Index));
         begin
            if Encoded_Index /= Null_Unbounded_String then
               Encoding.Insert
                 (Placeholder, S (Start_Encoded .. Cursor));
               Append (Res, Placeholder);
               Encoded_Index := Null_Unbounded_String;
            end if;
         end Add_Placeholder;

      begin
         for I in S'Range loop
            case S (I) is
               when '$' =>
                  Search_Index := True;
                  Start_Encoded := I;
                  Encoded_Index := Null_Unbounded_String;
               when '{' =>
                  In_Snippet := True;
               when '}' =>
                  Add_Placeholder (I);
                  In_Snippet := False;
                  Search_Index := False;
               when '0' .. '9' =>
                  if Search_Index then
                     Append (Encoded_Index, S (I));
                  elsif not In_Snippet then
                     Append (Res, S (I));
                  end if;
               when others =>
                  Search_Index := False;
                  if not In_Snippet then
                     Add_Placeholder (I);
                     Append (Res, S (I));
                  end if;
            end case;
         end loop;

         --  Do nothing for $0 which will be removed at the end

         return To_String (Res);
      end Encode_String;

      -------------------
      -- Decode_String --
      -------------------

      function Decode_String (S : String) return String
      is
         Res : Unbounded_String;
         U   : Unbounded_String := To_Unbounded_String (S);
      begin
         --  Remove the extra whitespace at the start
         Trim (U, Ada.Strings.Left);

         declare
            Trimed_S  : constant String := To_String (U);
            Cur_Index : Natural := Trimed_S'First;
         begin
            loop
               declare
                  Match : constant Match_Location :=
                    Find_Next_Placeholder (Trimed_S, Cur_Index);
               begin
                  exit when Match = No_Match;
                  Append
                    (Res, Trimed_S (Cur_Index .. Match.First - 1));
                  Append
                    (Res, Encoding (Trimed_S (Match.First .. Match.Last)));
                  Cur_Index := Match.Last + 1;
               end;
            end loop;

            Append (Res, Trimed_S (Cur_Index .. Trimed_S'Last));
         end;

         return To_String (Res);
      end Decode_String;

      ---------------------------
      -- Find_Next_Placeholder --
      ---------------------------

      function Find_Next_Placeholder
        (S     : String;
         Start : Integer)
         return Match_Location
      is
         Placeholder_Regexp : constant Pattern_Matcher :=
           Compile ("FooBar_[0-9]+");
         Matched : Match_Array (0 .. 0);
      begin
         Match
           (Self       => Placeholder_Regexp,
            Data       => S,
            Matches    => Matched,
            Data_First => Start);
         return Matched (0);
      end Find_Next_Placeholder;

      -----------------------
      -- Post_Pretty_Print --
      -----------------------

      function Post_Pretty_Print (S : String) return String
      is
         Res : Unbounded_String;
      begin
         --  Remove this condition when V725-006 is fixed
         if S (S'Last) = Ada.Characters.Latin_1.LF then
            Append (Res, S (S'First .. S'Last - 1));
         else
            Append (Res, S);
         end if;

         --  Add back snippet terminator
         Append (Res, "$0");
         return To_String (Res);
      end Post_Pretty_Print;

   begin
      if Me_Formatting.Active
        and then Result.insertText.Is_Set
        and then Result.insertTextFormat.Is_Set
        and then Result.insertTextFormat.Value = Snippet
      then
         declare
            Input : Utils.Char_Vectors.Char_Vector;
            Output : Utils.Char_Vectors.Char_Vector;
            PP_Messages : Pp.Scanner.Source_Message_Vector;
            S : GNAT.Strings.String_Access;
            Tmp_Unit : Analysis_Unit;
            Cmd : Utils.Command_Lines.Command_Line := Context.Get_PP_Options;
            Tmp_Context : constant Analysis_Context :=
              Create_Context
                (Charset =>
                   Utils.Command_Lines.Common.Wide_Character_Encoding (Cmd));
         begin
            Set_PP_Switches (Cmd);

            S :=
              new String'
                (VSS.Strings.Conversions.To_UTF_8_String
                   (Result.insertText.Value));

            declare
               Full : constant String := Prefix & Encode_String (S.all);
            begin
               Input.Append (Full);
               Tmp_Unit :=
                 Get_From_Buffer
                   (Context  => Tmp_Context,
                    Filename => "",
                    Buffer   => Full,
                    Rule     => Rule);
               Pp.Actions.Set_Partial_Gnatpp_Offset (Offset);
               Pp.Actions.Format_Vector
                 (Cmd            => Cmd,
                  Input          => Input,
                  Node           => Root (Tmp_Unit),
                  Output         => Output,
                  Messages       => PP_Messages,
                  Partial_Gnatpp => True);
               Pp.Actions.Set_Partial_Gnatpp_Offset (0);
            exception
               when E : others =>
                  --  Failed to pretty print the snippet, keep the previous
                  --  value.
                  LSP.Common.Log (Context.Trace, E);
                  return;
            end;

            GNAT.Strings.Free (S);
            S := new String'(Output.To_Array);

            if S.all /= "" then
               --  The text is already formatted, don't try to indent it
               Result.insertTextMode :=
                 (Is_Set => True,
                  Value  => asIs);
               Result.textEdit :=
                 (Is_Set => True,
                  Value  =>
                    (Is_TextEdit => True,
                     TextEdit    =>
                       (span    => Span,
                        newText =>
                          VSS.Strings.Conversions.To_Virtual_String
                            (Post_Pretty_Print (Decode_String (S.all))))));
            end if;
            GNAT.Strings.Free (S);
         end;
      end if;
   end Pretty_Print_Snippet;

   ---------------------------
   -- Generic_Write_Symbols --
   ---------------------------

   procedure Generic_Write_Symbols
     (Names  : Completion_Maps.Map;
      Result : in out LSP.Messages.Symbol_Vector) is
   begin
      for Cursor in Names.Iterate loop
         declare
            Name : constant Libadalang.Analysis.Defining_Name :=
              Completion_Maps.Key (Cursor);
            Node : Libadalang.Analysis.Ada_Node := Name.As_Ada_Node;
         begin
            while not Node.Is_Null and then
              Node.Kind not in Libadalang.Common.Ada_Basic_Decl
            loop
               Node := Node.Parent;
            end loop;

            if not Node.Is_Null then
               Result.Vector.Append
                 (LSP.Messages.SymbolInformation'
                    (name     => LSP.Lal_Utils.To_Virtual_String (Name.Text),
                     kind     => LSP.Lal_Utils.Get_Decl_Kind
                                  (Node.As_Basic_Decl),
                     location => LSP.Lal_Utils.Get_Node_Location
                                  (Name.As_Ada_Node),
                     alsIsAdaProcedure => <>,
                     tags              => LSP.Messages.Empty,
                     deprecated        => <>,
                     containerName => <>));
            end if;

            exit when Has_Been_Canceled;
         end;
      end loop;
   end Generic_Write_Symbols;

end LSP.Ada_Completions;
