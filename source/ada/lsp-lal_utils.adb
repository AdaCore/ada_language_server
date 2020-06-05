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

with Ada.Wide_Wide_Characters.Handling;
with Ada.Strings.Wide_Wide_Fixed;

with LSP.Types;         use LSP.Types;

with Libadalang.Common; use Libadalang.Common;
with Libadalang.Lexer;
with Langkit_Support.Diagnostics;

with Langkit_Support;
with Pp.Actions;

package body LSP.Lal_Utils is

   function Containing_Entity (Ref : Ada_Node) return Defining_Name;
   --  Return the declaration of the subprogram or task that contains Ref.
   --  Return No_Defining_Name if this fails.

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
      function Is_Synthetic return Boolean;
      --  Check if Node is in a synthetic file (like "__standard").
      --  TODO: Replace this with LAL property as it will be available.

      ------------------
      -- Is_Synthetic --
      ------------------

      function Is_Synthetic return Boolean is
         Std  : constant String := "__standard";
         File : constant String := Node.Unit.Get_Filename;
      begin
         return File'Length >= Std'Length
           and then File (File'Last - Std'Length + 1 .. File'Last) = Std;
      end Is_Synthetic;

      Location : constant LSP.Messages.Location :=
        LSP.Lal_Utils.Get_Node_Location
          (Libadalang.Analysis.As_Ada_Node (Node), Kind);
   begin
      if not Is_Synthetic then
         Result.Append (Location);
      end if;
   end Append_Location;

   --------------------------------
   -- Sort_And_Remove_Duplicates --
   --------------------------------

   procedure Sort_And_Remove_Duplicates
     (Result : in out LSP.Messages.Location_Vector)
   is
      function URI_Inf (Left, Right : LSP.Types.LSP_String) return Boolean;
      --  Comparison function for URIs, return True if Left < Right

      function "<" (Left, Right : LSP.Messages.Location) return Boolean is
        (URI_Inf (Left.uri, Right.uri) or else
           (Left.uri = Right.uri
            and then (Left.span.first.line < Right.span.first.line
                      or else (Left.span.first.line = Right.span.first.line
                               and then Left.span.first.character <
                                 Right.span.first.character))));

      -------------
      -- URI_Inf --
      -------------

      function URI_Inf (Left, Right : LSP.Types.LSP_String) return Boolean is

         function URI_Dir (X : String) return String;
         --  Return the dir in X

         -------------
         -- URI_Dir --
         -------------

         function URI_Dir (X : String) return String is
         begin
            for J in reverse X'First .. X'Last loop
               --  In an URI the directory separator is '/'
               if X (J) = '/' then
                  return X (X'First .. J - 1);
               end if;
            end loop;
            return X;
         end URI_Dir;

         L : constant String := To_UTF_8_String (Left);
         R : constant String := To_UTF_8_String (Right);

         L_Dir : constant String := URI_Dir (L);
         R_Dir : constant String := URI_Dir (R);
         Ind   : Natural;
      begin
         --  We're being a bit clever when comparing two URIs:
         --    * for a same file, return ".ads" before ".adb"
         --    * return "pack.adb" before "pack-child.adb"

         if L_Dir'Length /= R_Dir'Length then
            --  The directories are different: compare them
            return L_Dir < R_Dir;
         else
            --  The directories have the same length: are they the same?
            for Ind in 1 .. L_Dir'Length - 1 loop
               if L_Dir (L_Dir'First + Ind) /= R_Dir (R_Dir'First + Ind) then
                  return L_Dir (L_Dir'First + Ind) < R_Dir (R_Dir'First + Ind);
               end if;
            end loop;

            --  The directories are the same: compare the filenames
            Ind := L_Dir'Length + 1;
            loop
               if L'First + Ind > L'Last then
                  if R'First + Ind > R'Last then
                     return False;
                  end if;
                  return True;
               end if;

               if R'First + Ind > R'Last then
                  return False;
               end if;

               if L (L'First + Ind) /= R (R'First + Ind) then
                  --  Return "pack.adb" before "pack-child.adb"
                  if L (L'First + Ind) = '-'
                    and then R (R'First + Ind) = '.'
                  then
                     return False;
                  elsif L (L'First + Ind) = '.'
                    and then R (R'First + Ind) = '-'
                  then
                     return True;

                     --   Return ".ads" before ".adb"
                  elsif L'First + Ind = L'Last
                    and then R'First + Ind = R'Last
                  then
                     if L (L'First + Ind) = 's'
                       and then R (R'First + Ind) = 'b'
                     then
                        return True;
                     elsif L (L'First + Ind) = 'b'
                       and then R (R'First + Ind) = 's'
                     then
                        return False;
                     else
                        return L (L'First + Ind) < R (R'First + Ind);
                     end if;

                     --  Normal comparison
                  else
                     return L (L'First + Ind) < R (R'First + Ind);
                  end if;
               end if;

               Ind := Ind + 1;
            end loop;
         end if;
      end URI_Inf;

      package Sort is new
        LSP.Messages.Location_Vectors.Element_Vectors.Generic_Sorting;

      R : LSP.Messages.Location_Vectors.Element_Vectors.Vector :=
        LSP.Messages.Location_Vectors.Element_Vectors.Vector (Result);
      New_Result : LSP.Messages.Location_Vector;
      use type LSP.Messages.Location;
   begin
      Sort.Sort (R);
      for Loc of R loop
         if New_Result.Is_Empty
           or else Loc /= New_Result.Last_Element
         then
            New_Result.Append (Loc);
         end if;
      end loop;
      Result := New_Result;
   end Sort_And_Remove_Duplicates;

   --------------
   -- Contains --
   --------------

   function Contains
     (Token   : Token_Reference;
      Pattern : Wide_Wide_String;
      As_Word : Boolean;
      Span    : out LSP.Messages.Span)
      return Boolean
   is
      use Langkit_Support.Text;
      use Langkit_Support.Slocs;

      T    : constant Text_Type := Ada.Wide_Wide_Characters.Handling.To_Lower
        (Text (Token));
      Idx  : constant Integer := Ada.Strings.Wide_Wide_Fixed.Index
        (T, Pattern);
      Last : Integer;

      function Is_Word_Delimiter (C : Wide_Wide_Character) return Boolean;

      -----------------------
      -- Is_Word_Delimiter --
      -----------------------

      function Is_Word_Delimiter (C : Wide_Wide_Character) return Boolean is
      begin
         return not Ada.Wide_Wide_Characters.Handling.Is_Alphanumeric (C)
           and then C /= '_';
      end Is_Word_Delimiter;

   begin
      if Idx < T'First then
         return False;
      end if;

      --  Treat the Pattern as a word
      if As_Word then
         if Idx > T'First
           and then not Is_Word_Delimiter (T (Idx - 1))
         then
            return False;
         end if;

         Last := Idx + Pattern'Length;
         if Last <= T'Last
           and then not Is_Word_Delimiter (T (Last))
         then
            return False;
         end if;
      end if;

      declare
         Sloc : constant Source_Location_Range := Sloc_Range (Data (Token));

         Line  : constant LSP.Types.Line_Number :=
           LSP.Types.Line_Number (Sloc.Start_Line) - 1;
         Start : constant UTF_16_Index := UTF_16_Index
           (Sloc.Start_Column - 1 + Column_Number (Idx - T'First));
         Last : constant UTF_16_Index := Start +
           UTF_16_Index (Pattern'Length);

      begin
         Span := (first => (Line, Start),
                  last  => (Line, Last));
         return True;
      end;
   end Contains;

   ----------------------
   -- Get_Node_As_Name --
   ----------------------

   function Get_Node_As_Name (Node : Ada_Node) return Name is
   begin

      if Node = No_Ada_Node or else Node.Kind not in Ada_Name then
         return No_Name;
      end if;

      return Node.As_Name;

   end Get_Node_As_Name;

   --------------------------
   -- Get_Name_As_Defining --
   --------------------------

   function Get_Name_As_Defining (Name_Node : Name) return Defining_Name is
   begin

      if Name_Node = No_Name or else not Name_Node.P_Is_Defining then
         return No_Defining_Name;
      end if;

      return Name_Node.P_Enclosing_Defining_Name;

   end Get_Name_As_Defining;

   -------------------
   -- Get_Last_Name --
   -------------------

   function Get_Last_Name (Name_Node : Name)
      return Langkit_Support.Text.Unbounded_Text_Type
   is
      Names : constant Unbounded_Text_Type_Array :=
        P_As_Symbol_Array (Name_Node);

   begin
      return Names (Names'Last);
   end Get_Last_Name;

   -----------------------
   -- Get_Node_Location --
   -----------------------

   function Get_Node_Location
     (Node : Libadalang.Analysis.Ada_Node;
      Kind : LSP.Messages.AlsReferenceKind_Set := LSP.Messages.Empty_Set)
      return LSP.Messages.Location
   is
      Start_Sloc_Range                                     :
      constant Langkit_Support.Slocs.Source_Location_Range :=
         Sloc_Range (Data (Node.Token_Start));
      End_Sloc_Range                                       :
      constant Langkit_Support.Slocs.Source_Location_Range :=
         Sloc_Range (Data (Node.Token_End));

      First_Position : constant LSP.Messages.Position :=
                         (Line_Number (Start_Sloc_Range.Start_Line) - 1,
                          UTF_16_Index (Start_Sloc_Range.Start_Column) - 1);
      Last_Position  : constant LSP.Messages.Position :=
                         (Line_Number (End_Sloc_Range.End_Line) - 1,
                          UTF_16_Index (End_Sloc_Range.End_Column) - 1);

      File : constant LSP.Types.LSP_String :=
        LSP.Types.To_LSP_String (Node.Unit.Get_Filename);

      Location : constant LSP.Messages.Location :=
        (uri     => LSP.Ada_Contexts.File_To_URI (File),
         span    => LSP.Messages.Span'(First_Position, Last_Position),
         alsKind => Kind);

   begin
      return Location;
   end Get_Node_Location;

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
     (Value : Langkit_Support.Slocs.Source_Location_Range)
      return LSP.Messages.Span
   is
      Result : constant LSP.Messages.Span :=
        (first =>
           (line      => LSP.Types.Line_Number (Value.Start_Line) - 1,
            character => LSP.Types.UTF_16_Index   --  FIXME (UTF16 index)!
              (Value.Start_Column) - 1),
         last =>
           (line => LSP.Types.Line_Number (Value.End_Line) - 1,
            character => LSP.Types.UTF_16_Index  --  FIXME (UTF16 index)!
              (Value.End_Column) - 1));
   begin
      return Result;
   end To_Span;

   ----------------------
   -- To_Base_Id_Array --
   ----------------------

   function To_Base_Id_Array
     (Basic_Decls : Basic_Decl_Array) return Base_Id_Array
   is
      Base_Ids : Base_Id_Array (Basic_Decls'Range);
      Idx      : Positive := Base_Ids'First;
   begin
      for Decl of Basic_Decls loop
         Base_Ids (Idx) := Decl.P_Defining_Name.F_Name.As_Base_Id;
         Idx := Idx + 1;
      end loop;

      return Base_Ids;
   end To_Base_Id_Array;

   ------------------
   -- Resolve_Name --
   ------------------

   function Resolve_Name
     (Name_Node : Name;
      Trace     : GNATCOLL.Traces.Trace_Handle;
      Imprecise : out Boolean) return Defining_Name
   is
      Result : Defining_Name;
   begin
      Imprecise := False;

      --  First try to resolve precisely
      begin
         if Name_Node.P_Is_Defining then
            Result := Name_Node.P_Enclosing_Defining_Name.P_Canonical_Part;
         else
            Result := Name_Node.P_Referenced_Defining_Name
              (Imprecise_Fallback => False).P_Canonical_Part;
         end if;
      exception
         when E : Property_Error =>
            Log (Trace, E);
            Result := No_Defining_Name;
      end;

      --  The result was found precisely: return it
      if Result /= No_Defining_Name then
         return Result;
      end if;

      --  If we reach this, it means we've failed to get a precise result.
      --  Try again with the imprecise fallback.
      if not Name_Node.P_Is_Defining then
         Result := Name_Node.P_Referenced_Defining_Name
           (Imprecise_Fallback => True).P_Canonical_Part;

         Imprecise := Result /= No_Defining_Name;
      end if;

      return Result;

   exception
      when E : Property_Error =>
         Log (Trace, E);
         return No_Defining_Name;
   end Resolve_Name;

   ---------------------
   -- Find_First_Part --
   ---------------------

   function Find_Canonical_Part
     (Definition : Defining_Name;
      Trace      : GNATCOLL.Traces.Trace_Handle) return Defining_Name
   is
      Canonical : Defining_Name;
   begin
      Canonical := Definition.P_Canonical_Part;

      if Canonical = Definition then
         return No_Defining_Name;
      else
         return Canonical;
      end if;

   exception
      when E :  Libadalang.Common.Property_Error =>
         Log (Trace, E);
         return No_Defining_Name;
   end Find_Canonical_Part;

   --------------------
   -- Find_Next_Part --
   --------------------

   function Find_Next_Part
     (Definition : Defining_Name;
      Trace      : GNATCOLL.Traces.Trace_Handle) return Defining_Name
   is
      Next : Defining_Name;
   begin
      Next := Definition.P_Next_Part;

      if Next = Definition then
         return No_Defining_Name;
      else
         return Next;
      end if;
   exception
      when E :  Libadalang.Common.Property_Error =>
         Log (Trace, E);
         return No_Defining_Name;
   end Find_Next_Part;

   ---------------------------------------------------
   -- Is_Definition_Without_Separate_Implementation --
   ---------------------------------------------------

   function Is_Definition_Without_Separate_Implementation
     (Definition : Defining_Name) return Boolean
   is
      Parents : constant Ada_Node_Array := Definition.Parents;
   begin
      return Parents'Length > 2 and then Parents (Parents'First + 2).Kind in
        Libadalang.Common.Ada_Abstract_Subp_Decl
      --  This is as abstract subprogram
        | Libadalang.Common.Ada_Null_Subp_Decl
      --  This is an "is null" procedure
        | Libadalang.Common.Ada_Expr_Function;
      --  This is an expression function
   end Is_Definition_Without_Separate_Implementation;

   ---------------------------------
   -- Find_Defining_Name_Manually --
   ---------------------------------

   function Find_Other_Part_Fallback
     (Definition : Defining_Name;
      Trace      : GNATCOLL.Traces.Trace_Handle) return Defining_Name
   is
      Qualified_Name : constant Langkit_Support.Text.Text_Type :=
        Definition.P_Basic_Decl.P_Fully_Qualified_Name;
      --  The name that we'll try to match

      Found : Defining_Name := No_Defining_Name;
      --  The result that has been found

      function Matches
        (Node : Ada_Node'Class) return Libadalang.Common.Visit_Status;
      --  Return True if the name of Node matches Qualified_Name

      -------------
      -- Matches --
      -------------

      function Matches
        (Node : Ada_Node'Class) return Libadalang.Common.Visit_Status
      is
         use type Langkit_Support.Slocs.Line_Number;
      begin
         if Node.Is_Null
           or else Node.Kind not in Libadalang.Common.Ada_Basic_Decl
         then
            return Libadalang.Common.Into;
         end if;

         --  Note: in this function, we are simply looking at the first
         --  result that matches.
         --  TODO: improve this by find all entities that match, and
         --  finding the best through a distance/scoring heuristics.

         if Node.As_Basic_Decl.P_Fully_Qualified_Name = Qualified_Name
           and then Node.Sloc_Range.Start_Line
             /= Definition.Sloc_Range.Start_Line
         then
            Found := Node.As_Basic_Decl.P_Defining_Name;
            return Libadalang.Common.Stop;
         end if;

         return Libadalang.Common.Into;
      end Matches;

      Parent_Node : Ada_Node;
      Parent_Spec : Defining_Name;
      Parent_Body : Defining_Name;
   begin
      --  The heuristics implemented is the following: we're looking at the
      --  spec and body of the enclosing entity, to find an entity that
      --  could correspond to Definition.
      --
      --  For instance, if Definition points to a function Foo that is defined
      --  in a package P, we're going to look in the spec and body of P for
      --  any items named Foo, excluding Definition itself.

      --  Eliminate some cases. The subprogram does not have an other part if
      --  it is an expression function, or an abstract subprogram declaration,
      --  or a null procedure.

      if Is_Definition_Without_Separate_Implementation (Definition) then
         return No_Defining_Name;
      end if;

      --  First obtain the spec.
      --  Note: we could refine the number of calls to P_Semantic_Parent.
      --  Two calls to P_Semantic_Parents are needed in the case of a
      --  subprogram: the first jumps to the SubpDecl, the second to the
      --  PackageDecl.

      Parent_Node := Definition.P_Semantic_Parent.P_Semantic_Parent;

      if Parent_Node.Is_Null
        or else Parent_Node.Kind not in Ada_Basic_Decl
      then
         return No_Defining_Name;
      end if;

      Parent_Spec := Parent_Node.As_Basic_Decl.
          P_Canonical_Part.P_Defining_Name;

      --  Traverse the spec. The visiting function assigns the matching
      --  result, if any, to Found.
      Parent_Spec.Parent.Traverse (Matches'Unrestricted_Access);

      --  If we didn't find a result when traversing the spec, traverse the
      --  body of the containing entity.
      if Found = No_Defining_Name then
         Parent_Body := Find_Next_Part (Parent_Spec, Trace);
         if Parent_Body = No_Defining_Name then
            Parent_Body := Find_Next_Part (Parent_Spec, Trace);
         end if;
         if Parent_Body /= No_Defining_Name then
            Parent_Body.Parent.Traverse (Matches'Unrestricted_Access);
         end if;
      end if;

      return Found;

   exception
      when E : Property_Error =>
         Log (Trace, E);
         return No_Defining_Name;
   end Find_Other_Part_Fallback;

   -----------------------
   -- Containing_Entity --
   -----------------------

   function Containing_Entity (Ref : Ada_Node) return Defining_Name is
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
            return Parent.As_Basic_Decl.P_Canonical_Part.P_Defining_Name;
         end if;
      end loop;

      return No_Defining_Name;
   end Containing_Entity;

   --------------------
   -- Find_All_Calls --
   --------------------

   function Find_All_Calls
     (Context           : LSP.Ada_Contexts.Context;
      Definition        : Defining_Name;
      Imprecise_Results : out Boolean)
      return References_By_Subprogram.Map
   is
      use References_By_Subprogram;
      use References_List;
      Result     : Map;
      Containing : Defining_Name;

      --  Obtain all the references
      Refs      : constant Base_Id_Array := Context.Find_All_Calls
        (Definition, Imprecise_Results);
   begin
      --  Go through all references to Name, organising them by containing
      --  subprogram.

      for Ref of Refs loop
         --  We have a reference, and this a call: find the containing
         --  subprogram or task
         Containing := Containing_Entity (Ref.As_Ada_Node);

         if Containing /= No_Defining_Name then
            if Result.Contains (Containing) then
               declare
                  L : List := Result.Element (Containing);
               begin
                  L.Append (Ref);
                  Result.Replace (Containing, L);
               end;
            else
               declare
                  L : List;
               begin
                  L.Append (Ref);
                  Result.Insert (Containing, L);
               end;
            end if;
         end if;
      end loop;

      --  TODO: sort?
      return Result;
   end Find_All_Calls;

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
         Out_Stop  : out Libadalang.Common.Token_Data_Handlers.Token_Index;
         In_Start  : Token_Reference;
         Out_Start : Libadalang.Common.Token_Data_Handlers.Token_Index;
         Ok        : out Boolean);
      --  Find a token in Output document that corresponds to Is_Stop token in
      --  the Input document. Store token index into Out_Stop. To do this
      --  start scanning both token chains starting from In_Start (for Input)
      --  and Out_Start (for Output document). If no corresponding token found
      --  return Ok = False.

      function Lookup_Token
        (Sloc : Langkit_Support.Slocs.Source_Location) return Token_Reference;
      --  Like Node.Unit.Lookup_Token, but skip Trivia

      TDH     : Libadalang.Common.Token_Data_Handlers.Token_Data_Handler;
      Diags   : Langkit_Support.Diagnostics.Diagnostics_Vectors.Vector;
      Symbols : Libadalang.Common.Symbols.Symbol_Table :=
        Libadalang.Common.Symbols.Create_Symbol_Table;

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
         Out_Stop  : out Libadalang.Common.Token_Data_Handlers.Token_Index;
         In_Start  : Token_Reference;
         Out_Start : Libadalang.Common.Token_Data_Handlers.Token_Index;
         Ok        : out Boolean)
      is
         procedure Find_Next_Token
           (Kind  : Token_Kind;
            Index : in out Libadalang.Common.Token_Data_Handlers.Token_Index;
            Ok    : out Boolean);
         --  Find nearest token of a given Kind in the Output document starting
         --  from Index. Set Ok to False in no such token found and don't
         --  update Index in this case.

         ---------------------
         -- Find_Next_Token --
         ---------------------

         procedure Find_Next_Token
           (Kind  : Token_Kind;
            Index : in out Libadalang.Common.Token_Data_Handlers.Token_Index;
            Ok    : out Boolean)
         is
            use type Libadalang.Common.Token_Data_Handlers.Token_Index;
            Max_Look_Ahead : constant := 4;  --  How far search for the token

            Next_Kind : Token_Kind;
         begin
            Ok := False;

            for J in Index + 1 .. Index + Max_Look_Ahead loop
               Next_Kind := Libadalang.Common.To_Token_Kind
                 (Libadalang.Common.Token_Data_Handlers.Get_Token
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
         Libadalang.Common.Token_Data_Handlers.Initialize (TDH, Symbols);
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
      From_Index : Libadalang.Common.Token_Data_Handlers.Token_Index;
      --  Corresponding From-token in Output document
      To_Index : Libadalang.Common.Token_Data_Handlers.Token_Index;
      --  Corresponding To-token in Output document
      Ignore : Utils.Char_Vectors.Char_Subrange;
      Ok : Boolean;
   begin
      --  it seems that Format_Vector does not use In_/Out_Range properly, so
      --  using full text for now
      Pp.Actions.Format_Vector
        (Cmd, Input, Node, Input.Full_Range, Output, Ignore, Messages);

      if In_Sloc = Langkit_Support.Slocs.No_Source_Location_Range then
         --  Return full range of Output
         Out_Sloc := In_Sloc;
         return;
      elsif Node.Unit.Token_Count = 0 then  --  Ignore a cornercase for now
         Out_Sloc := Langkit_Support.Slocs.No_Source_Location_Range;
         return;
      end if;

      Tokenize_Output;  --  Fill TDH
      From := Lookup_Token (Langkit_Support.Slocs.Start_Sloc (In_Sloc));
      To := Lookup_Token (Langkit_Support.Slocs.End_Sloc (In_Sloc));

      Synchronize_Tokens
        (In_Stop   => From,
         Out_Stop  => From_Index,
         In_Start  => Node.Unit.First_Token,
         Out_Start => Libadalang.Common.Token_Data_Handlers.First_Token_Index,
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
         Out_Sloc.Start_Line := Libadalang.Common.Token_Data_Handlers.Get_Token
           (TDH, From_Index).Sloc_Range.Start_Line
           + In_Sloc.Start_Line
           - Sloc_Range (Data (From)).Start_Line;

         Out_Sloc.End_Line := Libadalang.Common.Token_Data_Handlers.Get_Token
           (TDH, To_Index).Sloc_Range.End_Line
           + In_Sloc.End_Line
           - Sloc_Range (Data (To)).End_Line;

         Out_Sloc.Start_Column := 1;
         Out_Sloc.End_Column := 1;
      end if;

      Libadalang.Common.Token_Data_Handlers.Free (TDH);
      Libadalang.Common.Symbols.Destroy (Symbols);
   end Format_Vector;

end LSP.Lal_Utils;
