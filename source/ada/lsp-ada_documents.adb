------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2018-2026, AdaCore                     --
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

with Ada.Tags;

with GNATCOLL.VFS;
with Gnatformat.Edits;
with Gnatformat.Formatting;
with Gnatformat.Identifier_Casing;

with Langkit_Support.Diagnostics;
with Langkit_Support.Symbols;
with Langkit_Support.Text;
with Langkit_Support.Token_Data_Handlers;

with Laltools.Common;

with Libadalang.Iterators;
with Libadalang.Sources;

with VSS.Strings.Conversions;

with LSP.Ada_Completions.Filters;
with LSP.Ada_Contexts;
with LSP.Ada_Documents.LAL_Diagnostics;
with LSP.Ada_Documents.Semantic_Diagnostics;
with LSP.Ada_Documents.Source_Info_Diagnostics;
with LSP.Ada_Handlers.Formatting;
with LSP.Ada_Id_Iterators;
with LSP.Enumerations;
with LSP.Predicates;

package body LSP.Ada_Documents is
   pragma Warnings (Off);

   -------------
   -- Cleanup --
   -------------

   procedure Cleanup (Self : in out Document) is
   begin
      for Source of Self.Diagnostic_Sources loop
         LSP.Diagnostic_Sources.Unchecked_Free (Source);
      end loop;
   end Cleanup;

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
      Units : constant Libadalang.Analysis.Analysis_Unit_Array :=
        (1 =>  LSP.Ada_Documents.Unit (Self    => Self,
                                       Context => Context));
   begin
      LSP.Ada_Id_Iterators.Find_All_References (Definition, Units, Callback);
   exception
      when E : Libadalang.Common.Property_Error =>
         Self.Tracer.Trace_Exception (E, "in Find_All_References");
   end Find_All_References;

   ------------
   -- Format --
   ------------

   procedure Format
     (Self    : Document;
      Context : LSP.Ada_Contexts.Context;
      Options : Gnatformat.Configuration.Format_Options_Type;
      Result  : out LSP.Structures.TextEdit_Vector)
   is
      function Format
        (Unparsing_Diagnostics :
           out Langkit_Support.Diagnostics.Diagnostics_Vectors.Vector)
         return VSS.Strings.Virtual_String;
      --  Helper to  format Self with formatting options based on Context

      ------------
      -- Format --
      ------------

      function Format
        (Unparsing_Diagnostics :
           out Langkit_Support.Diagnostics.Diagnostics_Vectors.Vector)
         return VSS.Strings.Virtual_String
      is
         Unit            : constant Libadalang.Analysis.Analysis_Unit :=
           Self.Unit (Context);
         Source_Filename : constant String :=
           Context.URI_To_File (Self.URI).Display_Base_Name;

      begin
         return
           VSS.Strings.Conversions.To_Virtual_String
             (Gnatformat.Formatting.Format
                ((case Options.Get_Identifier_Casing (Source_Filename) is
                    when Gnatformat.Configuration.Keep       => Unit,
                    when Gnatformat.Configuration.Definition =>
                      Gnatformat.Identifier_Casing.Normalized_Unit (Unit)),
                 Format_Options => Options,
                 Configuration  =>
                   Context.Get_Unparsing_Configuration
                     (Format_Options  => Options,
                      Source_Filename => Source_Filename,
                      Diagnostics     => Unparsing_Diagnostics)));
      end Format;

      Unparsing_Diagnostics :
        Langkit_Support.Diagnostics.Diagnostics_Vectors.Vector;
      Formatted_Document    : constant VSS.Strings.Virtual_String :=
        Format (Unparsing_Diagnostics);

   begin
      if not Unparsing_Diagnostics.Is_Empty then
         Self.Tracer.Trace
           ("Diagnostics found while loading the unparsing "
            & "configuration for "
            & Context.URI_To_File (Self.URI).Display_Base_Name
            & ":");
         for Diagnostic of Unparsing_Diagnostics loop
            Self.Tracer.Trace
              (Langkit_Support.Diagnostics.To_Pretty_String (Diagnostic));
         end loop;
      end if;

      Self.Diff_C
        (New_Text => Formatted_Document,
         Span     => LSP.Text_Documents.Empty_Range,
         Edit     => Result);
   end Format;

   --------------------
   -- Get_Any_Symbol --
   --------------------

   procedure Get_Any_Symbol
     (Self        : in out Document; Context : LSP.Ada_Contexts.Context;
      Pattern     : LSP.Search.Search_Pattern'Class;
      Limit       : Ada.Containers.Count_Type;
      Only_Public : Boolean;
      Canceled    : access function return Boolean;
      Result      : in out LSP.Ada_Completions.Completion_Maps.Map)
   is

      procedure Refresh_Symbol_Cache;
      --  Find intresting definings names in the document and put them
      --  into Self.Symbol_Cache

      procedure Insert
        (Item : Name_Information;
         Name : Libadalang.Analysis.Defining_Name);
      --  Populate Result with the name information if Result doesn't have
      --  the Name already

      function Get_Defining_Name
        (Loc : Langkit_Support.Slocs.Source_Location)
         return Libadalang.Analysis.Defining_Name;

      -----------------------
      -- Get_Defining_Name --
      -----------------------

      function Get_Defining_Name
        (Loc : Langkit_Support.Slocs.Source_Location)
         return Libadalang.Analysis.Defining_Name
      is
         Unit : constant Libadalang.Analysis.Analysis_Unit :=
             Self.Unit (Context);

         Name : constant Libadalang.Analysis.Name :=
           Laltools.Common.Get_Node_As_Name (Unit.Root.Lookup (Loc));
      begin
         return Laltools.Common.Get_Name_As_Defining (Name);
      end Get_Defining_Name;

      ------------
      -- Insert --
      ------------

      procedure Insert
        (Item : Name_Information;
         Name : Libadalang.Analysis.Defining_Name) is
      begin
         if not Result.Contains (Name) and then
           (not Only_Public or else Item.Is_Public)
         then
            Result.Insert
              (Name,
               (Is_Dot_Call  => False,
                Is_Visible   => False,
                Use_Snippets => False,
                Pos          => <>,
                Weight       => <>));
         end if;
      end Insert;

      --------------------------
      -- Refresh_Symbol_Cache --
      --------------------------

      procedure Refresh_Symbol_Cache is
         use Langkit_Support.Symbols;
         use Libadalang.Common;
         use Libadalang.Iterators;

         Node : Libadalang.Analysis.Ada_Node;

         Global_Visible : constant Libadalang.Iterators.Ada_Node_Predicate :=
           LSP.Predicates.Is_Global_Visible;

         Restricted_Kind : constant Libadalang.Iterators.Ada_Node_Predicate :=
           LSP.Predicates.Is_Restricted_Kind;

         --  Find all definings names excluding private parts and bodies
         It : Libadalang.Iterators.Traverse_Iterator'Class :=
           Libadalang.Iterators.Find
             (Self.Unit (Context).Root,
              Libadalang.Iterators.Kind_Is (Ada_Defining_Name)
                and not Restricted_Kind);

      begin
         while It.Next (Node) loop
            declare
               Token     : constant Token_Reference := Node.Token_End;
               Text      : constant Langkit_Support.Text.Text_Type :=
                 Libadalang.Common.Text (Token);
               Canonical : constant Symbolization_Result :=
                 Libadalang.Sources.Canonicalize (Text);
               Cursor    : Symbol_Maps.Cursor;
               Inserted  : Boolean;

            begin
               if Canonical.Success then
                  Self.Symbol_Cache.Insert
                    (VSS.Strings.To_Virtual_String (Canonical.Symbol),
                     Name_Vectors.Empty_Vector,
                     Cursor,
                     Inserted);

                  Self.Symbol_Cache (Cursor).Append
                    (Name_Information'
                       (Langkit_Support.Slocs.Start_Sloc (Node.Sloc_Range),
                        Global_Visible.Unchecked_Get.Evaluate (Node)));
               end if;
            end;
         end loop;
      end Refresh_Symbol_Cache;

      Cursor      : Symbol_Maps.Cursor;

      use type LSP.Search.Search_Kind;

      --  In "Celling" mode we scan only range of cache where a key prefix
      --  matches lowercased pattern as is.
      Use_Celling : constant Boolean :=
        not Pattern.Get_Negate
        and then ((Pattern.Get_Kind = LSP.Enumerations.Full_Text
                   and then Pattern.Get_Whole_Word)
                  or else Pattern.Get_Kind = LSP.Enumerations.Start_Word_Text);

   begin
      if Self.Refresh_Symbol_Cache then
         Refresh_Symbol_Cache;
         Self.Refresh_Symbol_Cache := False;
      end if;

      if Use_Celling then
         Cursor := Self.Symbol_Cache.Ceiling (Pattern.Get_Canonical_Pattern);
      else
         Cursor := Self.Symbol_Cache.First;
      end if;

      while Symbol_Maps.Has_Element (Cursor) loop

         if Use_Celling
           and then not Pattern.Match (Symbol_Maps.Key (Cursor))
         then
            --  We use "Celling mode" and key stops matching,
            --  Symbol_Cache is ordered so we will not find any
            --  matches more

            exit when Use_Celling or else Canceled.all;

         else

            for Item of Self.Symbol_Cache (Cursor) loop
               declare
                  Defining_Name : constant Libadalang.Analysis.Defining_Name :=
                    Get_Defining_Name (Item.Loc);
               begin
                  --  Match each element individually in case of sensitive
                  --  search or non-celling mode
                  if not Defining_Name.Is_Null
                    and then
                      ((Use_Celling
                        and then not Pattern.Get_Case_Sensitive)
                       or else Pattern.Match
                         (VSS.Strings.To_Virtual_String
                            (Defining_Name.As_Ada_Node.Text)))
                  then
                     Insert (Item, Defining_Name);
                  end if;

                  exit when Canceled.all;

               end;
            end loop;

         end if;

         Symbol_Maps.Next (Cursor);
      end loop;
   end Get_Any_Symbol;

   -------------------------
   -- Get_Completion_Node --
   -------------------------

   procedure Get_Completion_Node
     (Self     : Document;
      Context  : LSP.Ada_Contexts.Context;
      Position : LSP.Structures.Position;
      Sloc     : out Langkit_Support.Slocs.Source_Location;
      Token    : out Libadalang.Common.Token_Reference;
      Node     : out Libadalang.Analysis.Ada_Node)
   is
      use Libadalang.Common;

      function Completion_Token
        (Sloc  : Langkit_Support.Slocs.Source_Location)
         return Libadalang.Common.Token_Reference;
      --  Get token under completion for given cursor position.
      --  If cursor at the first symbol of a token return previous token:
      --  XXX___
      --     ^ cursor just after a token mean user is completion XXX token.

      ----------------------
      -- Completion_Token --
      ----------------------

      function Completion_Token
        (Sloc  : Langkit_Support.Slocs.Source_Location)
         return Libadalang.Common.Token_Reference
      is
         use type Langkit_Support.Slocs.Source_Location;

         Token : constant Libadalang.Common.Token_Reference :=
           Self.Get_Token_At (Context, Position);

         Prev  : constant Libadalang.Common.Token_Reference :=
           (if Token = Libadalang.Common.No_Token
            then Token
            else Libadalang.Common.Previous (Token));

      begin
         if Libadalang.Common.No_Token not in Token | Prev then
            declare
               Data  : constant Libadalang.Common.Token_Data_Type :=
                 Libadalang.Common.Data (Token);

               Start : constant Langkit_Support.Slocs.Source_Location :=
                 Langkit_Support.Slocs.Start_Sloc
                   (Libadalang.Common.Sloc_Range (Data));
            begin
               if Start = Sloc then
                  return Prev;
               end if;
            end;
         end if;

         return Token;
      end Completion_Token;
   begin
      Sloc  := Self.To_Source_Location (Position);
      Token := Completion_Token (Sloc);
      declare
         From : constant Langkit_Support.Slocs.Source_Location :=
           Langkit_Support.Slocs.Start_Sloc
             (Libadalang.Common.Sloc_Range (Libadalang.Common.Data (Token)));
         Root : constant Libadalang.Analysis.Ada_Node :=
           Self.Unit (Context).Root;
      begin
         Node := (if Root.Is_Null then Root else Root.Lookup (From));
      end;
   end Get_Completion_Node;

   ------------------------
   -- Get_Completions_At --
   ------------------------

   procedure Get_Completions_At
     (Self      : Document;
      Providers : LSP.Ada_Completions.Completion_Provider_List;
      Context   : LSP.Ada_Contexts.Context;
      Sloc      : Langkit_Support.Slocs.Source_Location;
      Token     : Libadalang.Common.Token_Reference;
      Node      : Libadalang.Analysis.Ada_Node;
      Names     : out Ada_Completions.Completion_Maps.Map;
      Result    : out LSP.Structures.CompletionList)
   is
      procedure Append (Name_Map : LSP.Ada_Completions.Completion_Maps.Map);
      procedure Append (List : LSP.Structures.CompletionItem_Vector);

      ------------
      -- Append --
      ------------

      procedure Append (Name_Map : LSP.Ada_Completions.Completion_Maps.Map) is
      begin
         for Cursor in Name_Map.Iterate loop
            declare
               Name : Libadalang.Analysis.Defining_Name :=
                 LSP.Ada_Completions.Completion_Maps.Key (Cursor);
            begin
               if not Names.Contains (Name) then
                  Names.Include
                    (Name,
                     LSP.Ada_Completions.Completion_Maps.Element (Cursor));
               end if;
            end;
         end loop;
      end Append;

      ------------
      -- Append --
      ------------

      procedure Append (List : LSP.Structures.CompletionItem_Vector) is
      begin
         for Item of List loop
            Result.items.Append (Item);
         end loop;
      end Append;

      Parent : constant Libadalang.Analysis.Ada_Node :=
        (if Node.Is_Null then Node else Node.Parent);

      Filter : LSP.Ada_Completions.Filters.Filter;
   begin
      if not Parent.Is_Null
        and then (Parent.Kind not in
          Libadalang.Common.Ada_Dotted_Name | Libadalang.Common.Ada_End_Name
          and then Node.Kind in Libadalang.Common.Ada_String_Literal_Range)
      then
         --  Do nothing when inside a string
         return;
      end if;

      Self.Tracer.Trace
        ("Getting completions, Pos = ("
         & Sloc.Line'Image & ", " & Sloc.Column'Image & ") Node = "
         & Libadalang.Analysis.Image (Node));

      Filter.Initialize (Token, Node);

      for Provider of Providers loop
         declare
            use all type LSP.Ada_Completions.Completion_Result_Kind;
            Next : LSP.Ada_Completions.Completion_Result;
         begin
            Provider.Propose_Completion
              (Sloc   => Sloc,
               Token  => Token,
               Node   => Node,
               Filter => Filter,
               Result => Next);

            case Next.Kind is
               when Name_Map =>
                  if Names.Is_Empty then
                     Names.Move (Next.Name_Map);
                  else
                     Append (Next.Name_Map);
                  end if;

               when Completion_List =>
                  if Result.items.Is_Empty then
                     Result.items.Move (Next.Completion_List);
                  else
                     Append (Next.Completion_List);
                  end if;
            end case;

         exception
            when E : Libadalang.Common.Property_Error =>
               Self.Tracer.Trace_Exception
                 (E,
                  "LAL EXCEPTION occurred with following completion provider");
               Self.Tracer.Trace (Ada.Tags.Expanded_Name (Provider'Tag));
         end;
      end loop;

      Self.Tracer.Trace
        ("Number of filtered completions : " & Names.Length'Image);
   end Get_Completions_At;

   ----------------
   -- Get_Errors --
   ----------------

   procedure Get_Errors
     (Self    : in out Document;
      Context : LSP.Ada_Contexts.Context;
      Changed : out Boolean;
      Errors  : out LSP.Structures.Diagnostic_Vector;
      Force   : Boolean := False)
   is
   begin
      Errors.Clear;
      Changed := (for some Source of Self.Diagnostic_Sources =>
                    Source.Has_New_Diagnostic (Context));

      if Changed or else Force then
         for Source of Self.Diagnostic_Sources loop
            Source.Get_Diagnostics (Context, Errors);
         end loop;
      end if;
   end Get_Errors;

   ---------------------------
   -- Get_Formatting_Region --
   ---------------------------

   function Get_Formatting_Region
     (Self     : Document;
      Context  : LSP.Ada_Contexts.Context;
      Position : LSP.Structures.Position)
      return Laltools.Partial_GNATPP.Formatting_Region_Type
   is (Laltools.Partial_GNATPP.Get_Formatting_Region
        (Unit        => Self.Unit (Context),
         Input_Range =>
            Self.To_Source_Location_Range ((Position, Position))));

   -----------------
   -- Get_Node_At --
   -----------------

   function Get_Node_At
     (Self     : Document;
      Context  : LSP.Ada_Contexts.Context;
      Position : LSP.Structures.Position) return Libadalang.Analysis.Ada_Node
   is
      use Langkit_Support.Slocs;

      Unit         : constant Libadalang.Analysis.Analysis_Unit :=
        Self.Unit (Context);

      function Get_Sloc return Langkit_Support.Slocs.Source_Location;
      --  Return the source location corresponding to Position.
      --  This function handles properly the case where the position is
      --  at the end of an indentifier token and at the start of the next
      --  token (e.g: 'An_Identifier^;' where '^' is the cursor's position):
      --  in that case we want to retrieve the node corresponding to
      --  the previous identifier token.

      --------------
      -- Get_Sloc --
      --------------

      function Get_Sloc return Langkit_Support.Slocs.Source_Location is
         use type Langkit_Support.Slocs.Source_Location;
         use Libadalang.Common;

         Initial_Sloc : constant Langkit_Support.Slocs.Source_Location :=
           Self.To_Source_Location (Position);
         Token        : constant Libadalang.Common.Token_Reference :=
           Self.Get_Token_At (Context, Position);
         Prev_Token : constant Libadalang.Common.Token_Reference :=
           (if Token /= No_Token
            then Libadalang.Common.Previous (Token)
            else No_Token);
      begin
         --  No previous token, return the initial SLOC
         if Prev_Token = No_Token then
            return Initial_Sloc;
         end if;

         declare
            Token_Data       : constant Libadalang.Common.Token_Data_Type :=
              Libadalang.Common.Data (Token);
            Token_Range      :
              constant Libadalang.Slocs.Source_Location_Range :=
                Token_Data.Sloc_Range;
            Prev_Token_Data  : constant Libadalang.Common.Token_Data_Type :=
              Libadalang.Common.Data (Prev_Token);
            Prev_Token_Range :
              constant Libadalang.Slocs.Source_Location_Range :=
                Prev_Token_Data.Sloc_Range;
         begin
            --  The current token is an identifier and its start SLOC and
            --  the previous one's end SLOC are equal: consider that the query
            --  is being done on the previous identifier token.
            if Prev_Token_Data.Kind = Ada_Identifier
              and then Initial_Sloc = Token_Range.Start_Sloc
              and then Initial_Sloc = Prev_Token_Range.End_Sloc
            then
               return Prev_Token_Range.Start_Sloc;
            else
               return Initial_Sloc;
            end if;
         end;
      end Get_Sloc;

   begin
      return (if Unit.Root.Is_Null then Libadalang.Analysis.No_Ada_Node
              else Unit.Root.Lookup (Get_Sloc));
   end Get_Node_At;

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

   ------------------
   -- Get_Token_At --
   ------------------

   function Get_Token_At
     (Self     : Document'Class; Context : LSP.Ada_Contexts.Context;
      Position : LSP.Structures.Position)
      return Libadalang.Common.Token_Reference
   is
     (Self.Unit (Context).Lookup_Token (Self.To_Source_Location (Position)));

   -----------------
   -- Get_Word_At --
   -----------------

   function Get_Word_At
     (Self     : Document;
      Context  : LSP.Ada_Contexts.Context;
      Position : LSP.Structures.Position) return VSS.Strings.Virtual_String
   is
      use Langkit_Support.Slocs;
      use all type Libadalang.Common.Token_Kind;

      Result : VSS.Strings.Virtual_String;

      Unit : constant Libadalang.Analysis.Analysis_Unit :=
        Self.Unit (Context);

      Origin : constant Source_Location := Self.To_Source_Location (Position);
      Where : constant Source_Location := (Origin.Line, Origin.Column - 1);
      --  Compute the position we want for completion, which is one character
      --  before the cursor.

      Token : constant Libadalang.Common.Token_Reference :=
        Unit.Lookup_Token (Where);

      Data : constant Libadalang.Common.Token_Data_Type :=
        Libadalang.Common.Data (Token);

      Kind : constant Libadalang.Common.Token_Kind :=
        Libadalang.Common.Kind (Data);

      Text : constant Langkit_Support.Text.Text_Type :=
        Libadalang.Common.Text (Token);

      Sloc : constant Source_Location_Range :=
        Libadalang.Common.Sloc_Range (Data);

      Span : constant Integer :=
        Natural (Where.Column) - Natural (Sloc.Start_Column);

   begin
      if Kind in Ada_Identifier .. Ada_Xor
        and then Compare (Sloc, Where) = Inside
      then
         Result := VSS.Strings.To_Virtual_String
           (Text (Text'First .. Text'First + Span));
      end if;

      return Result;
   end Get_Word_At;

   --------------
   -- Get_Text --
   --------------

   function Get_Text
     (Self : Document;
      From : LSP.Structures.Position;
      To   : LSP.Structures.Position)
      return VSS.Strings.Virtual_String
   is
      Span : constant LSP.Structures.A_Range := (start => From, an_end => To);
   begin
      return Self.Slice (Span);
   end Get_Text;

   ---------------------
   -- Has_Diagnostics --
   ---------------------

   function Has_Diagnostics
     (Self : Document; Context : LSP.Ada_Contexts.Context) return Boolean
   is
      (Self.Unit (Context).Has_Diagnostics);

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self    : in out Document;
      Handler : LSP.Ada_Handlers.Message_Handler'Class;
      URI     : LSP.Structures.DocumentUri;
      Text    : VSS.Strings.Virtual_String;
      Version : Integer) is
   begin
      LSP.Text_Documents.Constructors.Initialize (Self, URI, Text, Version);

      Self.Refresh_Symbol_Cache := True;

      Self.Diagnostic_Sources :=
        (Syntax        => new LSP.Ada_Documents.LAL_Diagnostics.Diagnostic_Source
               (Handler => Handler'Unrestricted_Access,
                Document => Self'Unrestricted_Access),
         Source_Info   => new LSP.Ada_Documents.Source_Info_Diagnostics.Diagnostic_Source
               (Handler => Handler'Unrestricted_Access,
                Document => Self'Unrestricted_Access),
         Semantic      => new LSP.Ada_Documents.Semantic_Diagnostics.Diagnostic_Source
               (Handler => Handler'Unrestricted_Access,
                Document => Self'Unrestricted_Access));
   end Initialize;

   ------------------
   -- Range_Format --
   ------------------

   procedure Range_Format
     (Self    : Document;
      Context : LSP.Ada_Contexts.Context;
      Span    : LSP.Structures.A_Range;
      Options : Gnatformat.Configuration.Format_Options_Type;
      Result  : out LSP.Structures.TextEdit_Vector)
   is
      use type LSP.Structures.A_Range;
      use type Langkit_Support.Slocs.Line_Number;
      use type Langkit_Support.Slocs.Column_Number;
      use type Langkit_Support.Slocs.Source_Location;
      use type Langkit_Support.Slocs.Source_Location_Range;
      use type Libadalang.Common.Token_Reference;
      use type Langkit_Support.Token_Data_Handlers.Token_Index;

      use Gnatformat.Configuration;

      function To_GNATformat_Range
        (Unit : Libadalang.Analysis.Analysis_Unit;
         Span : LSP.Structures.A_Range)
           return Langkit_Support.Slocs.Source_Location_Range;
      --  Convert range selection to one accepted by gnatformat.
      --  It looks like gnatformat has its own vision on selection range:
      --  column = 0 has special meaning and End_Sloc is not excluded
      --  from the range.

      -------------------------
      -- To_GNATformat_Range --
      -------------------------

      function To_GNATformat_Range
        (Unit : Libadalang.Analysis.Analysis_Unit;
         Span : LSP.Structures.A_Range)
           return Langkit_Support.Slocs.Source_Location_Range
      is
         use type Langkit_Support.Slocs.Line_Number;
         use type Langkit_Support.Slocs.Column_Number;

         Result : Langkit_Support.Slocs.Source_Location_Range :=
           Self.To_Source_Location_Range (Span);
      begin
         if Result.End_Column <= 1 then
            --  Increment end_line because gnatformat includes it in the range
            --  while LAL and LSP exclude.
            Result.End_Line := Langkit_Support.Slocs.Line_Number'Max
              (Result.Start_Line, Result.End_Line - 1);
         end if;

         Result.Start_Column := 1;  --  Start_Column is ignored by gnatformat
         Result.End_Column :=
           Unit.Get_Line (Positive (Result.End_Line))'Length - 1;
         --  Expanded to end of line

         return Result;
      end To_GNATformat_Range;

      Source_Filename : constant String :=
        Context.URI_To_File (Self.URI).Display_Base_Name;

      Unit : constant Libadalang.Analysis.Analysis_Unit :=
        (case Options.Get_Identifier_Casing (Source_Filename) is
           when Gnatformat.Configuration.Keep       => Self.Unit (Context),
           when Gnatformat.Configuration.Definition =>
             Gnatformat.Identifier_Casing.Normalized_Unit
               (Self.Unit (Context)));

      Format_Range : constant Langkit_Support.Slocs.Source_Location_Range :=
        To_GNATformat_Range (Unit, Range_Format.Span);
      --  Origin span rounded to lines bounds (end position is included)

      Excluded : constant Langkit_Support.Slocs.Source_Location_Range :=
        (Start_Line   => Format_Range.Start_Line,
         Start_Column => 1,
         End_Line     => Format_Range.End_Line + 1,
         End_Column   => 1);
      --  Selection range (end position is excluded)

      Text : VSS.Strings.Virtual_String;

      Unparsing_Diagnostics :
        Langkit_Support.Diagnostics.Diagnostics_Vectors.Vector;

      Range_Formatted_Document :
        constant Gnatformat.Edits.Formatting_Edit_Type :=
          Gnatformat.Formatting.Range_Format
            (Unit            => Unit,
             Selection_Range => Format_Range,
             Format_Options  => Options,
             Configuration   =>
               Context.Get_Unparsing_Configuration
                 (Format_Options  => Options,
                  Source_Filename => Source_Filename,
                  Diagnostics     => Unparsing_Diagnostics));

      Ok : Boolean;
   begin
      if not Unparsing_Diagnostics.Is_Empty then
         Self.Tracer.Trace
           ("Diagnostics found while loading the unparsing configuration for "
            & Source_Filename
            & ":");
         for Diagnostic of Unparsing_Diagnostics loop
            Self.Tracer.Trace
              (Langkit_Support.Diagnostics.To_Pretty_String (Diagnostic));
         end loop;
      end if;

      LSP.Ada_Handlers.Formatting.Narrow_Range_Format
        (Unit, Excluded, Range_Formatted_Document, Text, Ok);

      if Ok then
         Self.Diff_C
           (New_Text => Text,
            Span     => Self.To_A_Range (Format_Range),
            Edit     => Result);
         return;
      end if;

      Self.Diff_C
        (New_Text =>
           VSS.Strings.Conversions.To_Virtual_String
             (Range_Formatted_Document.Text_Edit.Text),
         Span     =>
           Self.To_A_Range (Range_Formatted_Document.Text_Edit.Location),
         Edit     => Result);
   end Range_Format;

   ------------------------
   -- Reset_Symbol_Cache --
   ------------------------

   procedure Reset_Symbol_Cache (Self : in out Document'Class) is
   begin
      for Item of Self.Symbol_Cache loop
         --  We clear defining name vectors, but keep symbol map in hope, that
         --  we will reuse the same elements after reindexing in
         --  Refresh_Symbol_Cache call, so we avoid memory reallocation.
         Item.Clear;
      end loop;

      Self.Refresh_Symbol_Cache := True;
   end Reset_Symbol_Cache;

   --------------------------------
   -- Semantic_Diagnostic_Source --
   --------------------------------

   function Semantic_Diagnostic_Source
     (Self : in out Document)
      return LSP.Diagnostic_Sources.Diagnostic_Source_Access
   is
   begin
      return Self.Diagnostic_Sources (Semantic);
   end Semantic_Diagnostic_Source;

   ---------------------
   -- To_LSP_Location --
   ---------------------

   function To_LSP_Location
     (Self    : Document;
      Segment : Langkit_Support.Slocs.Source_Location_Range;
      Kinds   : LSP.Structures.AlsReferenceKind_Set := LSP.Constants.Empty;
      Hidden  : Boolean := False)
      return LSP.Structures.Location
        is (uri     => Self.URI,
            a_range => Self.To_A_Range (Segment),
            alsKind => Kinds,
            hidden  =>
              (if Hidden
               then (Is_Set => True, Value => True)
               else (Is_Set => False)));

   ----------
   -- Unit --
   ----------

   function Unit
     (Self : Document'Class; Context : LSP.Ada_Contexts.Context)
      return Libadalang.Analysis.Analysis_Unit
   is
      (Context.LAL_Context.Get_From_File
        (Filename => Context.URI_To_File (Self.URI).Display_Full_Name,
         Charset  => Context.Charset,
         Reparse  => False));

end LSP.Ada_Documents;
