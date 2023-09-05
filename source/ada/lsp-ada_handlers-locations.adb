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

with VSS.Characters;
with VSS.String_Vectors;
with VSS.Strings.Character_Iterators;
with VSS.Strings.Conversions;

with LSP.Utils;

with URIs;

package body LSP.Ada_Handlers.Locations is

   function To_LSP_Range
     (Unit : Libadalang.Analysis.Analysis_Unit;
      Sloc : Langkit_Support.Slocs.Source_Location_Range)
        return LSP.Structures.A_Range;

   ---------------------
   -- Append_Location --
   ---------------------

   procedure Append_Location
     (Self   : in out Message_Handler;
      Result : in out LSP.Structures.Location_Vector;
      Filter : in out LSP.Ada_Handlers.Locations.File_Span_Sets.Set;
      Node   : Libadalang.Analysis.Ada_Node'Class;
      Kinds  : LSP.Structures.AlsReferenceKind_Set := LSP.Constants.Empty) is
   begin
      if not Node.Is_Synthetic then
         declare
            Value : constant LSP.Structures.Location :=
              To_LSP_Location (Self, Node, Kinds);
         begin
            if not Filter.Contains (Value) then
               Result.Append (Value);
               Filter.Insert (Value);
            end if;
         end;
      end if;
   end Append_Location;

   ---------------------
   -- Append_Location --
   ---------------------

   procedure Append_Location
     (Result   : in out LSP.Structures.DocumentHighlight_Vector;
      Document : not null access LSP.Ada_Documents.Document'Class;
      File     : GNATCOLL.VFS.Virtual_File;
      Node     : Libadalang.Analysis.Ada_Node'Class;
      Kind     : LSP.Structures.DocumentHighlightKind_Optional)
   is
      use type GNATCOLL.VFS.Virtual_File;

      Node_File : constant GNATCOLL.VFS.Virtual_File :=
        GNATCOLL.VFS.Create_From_UTF8 (Node.Unit.Get_Filename);

   begin
      if File = Node_File then
         Result.Append
           (LSP.Structures.DocumentHighlight'
              (a_range => Document.To_LSP_Range (Node.Sloc_Range),
               kind    => Kind));
      end if;
   end Append_Location;

   -----------------
   -- Get_Node_At --
   -----------------

   function Get_Node_At
     (Self     : in out Message_Handler'Class;
      Context  : LSP.Ada_Contexts.Context;
      Value    : LSP.Structures.TextDocumentPositionParams'Class)
      return Libadalang.Analysis.Ada_Node
   is
      use type LSP.Ada_Documents.Document_Access;

      Document : constant LSP.Ada_Documents.Document_Access :=
        Self.Get_Open_Document (Value.textDocument.uri);

   begin
      if Document /= null then
         return Document.Get_Node_At (Context, Value.position);
      end if;

      declare
         File : constant GNATCOLL.VFS.Virtual_File :=
           Self.To_File (Value.textDocument.uri);

         Unit : constant Libadalang.Analysis.Analysis_Unit :=
           Context.Get_AU (File);

         Sloc : Langkit_Support.Slocs.Source_Location :=
           (Line   => Langkit_Support.Slocs.Line_Number
              (Value.position.line + 1),
            Column => <>);

         Line : constant VSS.Strings.Virtual_String :=
           VSS.Strings.To_Virtual_String
             (Unit.Get_Line (Value.position.line + 1));

         Cursor : VSS.Strings.Character_Iterators.Character_Iterator :=
           Line.Before_First_Character;
      begin
         while Cursor.Forward and then
           Natural (Cursor.First_UTF16_Offset) < Value.position.character
         loop
            null;  -- Skip characters on the left of the `position.character`
         end loop;

         Sloc.Column := Langkit_Support.Slocs.Column_Number
           (Cursor.Character_Index);

         return Unit.Root.Lookup (Sloc);
      end;
   end Get_Node_At;

   ----------
   -- Hash --
   ----------

   function Hash
     (Value : LSP.Structures.Location) return Ada.Containers.Hash_Type is
      use type Ada.Containers.Hash_Type;
   begin
      return Value.uri.Get_Hash + LSP.Utils.Hash (Value.a_range);
   end Hash;

   ----------
   -- Sort --
   ----------

   procedure Sort (Result : in out LSP.Structures.Location_Vector) is

      function Less
        (Left, Right : LSP.Structures.DocumentUri) return Boolean;
      --  Comparison function for URIs, return True if Left < Right

      function "<" (Left, Right : LSP.Structures.Location) return Boolean is
        (Less (Left.uri, Right.uri) or else
           (LSP.Structures."=" (Left.uri, Right.uri)
              and then
                (Left.a_range.start.line < Right.a_range.start.line
                 or else
                   (Left.a_range.start.line = Right.a_range.start.line
                    and then Left.a_range.start.character <
                      Right.a_range.start.character))));

      ----------
      -- Less --
      ----------

      function Less
        (Left, Right : LSP.Structures.DocumentUri) return Boolean
      is
         use type VSS.Strings.Virtual_String;
         use type VSS.Characters.Virtual_Character;

         function Last_Component
           (X : LSP.Structures.DocumentUri) return VSS.Strings.Virtual_String;
         --  Return the last component of the URI

         function Last_Component
           (X : LSP.Structures.DocumentUri) return VSS.Strings.Virtual_String
         is
            List : constant VSS.String_Vectors.Virtual_String_Vector :=
              X.Split ('/');
         begin
            return List (List.Length);
         end Last_Component;

         L_File : constant VSS.Strings.Virtual_String := Last_Component (Left);

         R_File : constant VSS.Strings.Virtual_String :=
           Last_Component (Right);

         L_Cursor : VSS.Strings.Character_Iterators.Character_Iterator :=
           L_File.Before_First_Character;

         R_Cursor : VSS.Strings.Character_Iterators.Character_Iterator :=
           R_File.Before_First_Character;

         L : VSS.Characters.Virtual_Character'Base;
         R : VSS.Characters.Virtual_Character'Base;

         L_Spec : Boolean := False;
         --  Prev L is `s` and R is `b`
         R_Spec : Boolean := False;
         --  Prev L is `b` and R is `s`

      begin
         if L_File = R_File then
            --  for the same last component, compare full pathes
            return LSP.Structures."<" (Left, Right);
         end if;

         --  Compare the filenames
         while L_Cursor.Forward (L) and R_Cursor.Forward (R) loop

            if L_Spec or R_Spec then
               --  Prev L /= R in non-endigng position
               return R_Spec;
            elsif L = R then
               null;  --  Skip equal characters
            elsif L = '-' and R = '.' then
               --  Return "pack.adb" before "pack-child.adb"
               return False;
            elsif L = '.' and R = '-' then
               --  Other side
               return True;
            elsif L = 's' and R = 'b' then
               L_Spec := True;  --  Special case to check at the end
            elsif L = 'b' and R = 's' then
               R_Spec := True;  --  Special case to check at the end
            else
               return L < R;
            end if;
         end loop;

         if not L_Cursor.Has_Element and not R_Cursor.Has_Element then
            --   Return ".ads" before ".adb"
            return L_Spec;
         else
            return R_Cursor.Has_Element;
         end if;
      end Less;

      package Sorting is new
        LSP.Structures.Location_Vectors.Generic_Sorting;

   begin
      Sorting.Sort (LSP.Structures.Location_Vectors.Vector (Result));
   end Sort;

   --------------------
   -- Start_Position --
   --------------------

   function Start_Position
     (Token : Libadalang.Common.Token_Reference)
      return LSP.Structures.Position
   is
      Location : constant Libadalang.Slocs.Source_Location :=
        Libadalang.Slocs.Start_Sloc
          (Libadalang.Common.Sloc_Range (Libadalang.Common.Data (Token)));
      Line     : constant VSS.Strings.Virtual_String :=
        VSS.Strings.To_Virtual_String
          (Libadalang.Analysis.Unit
             (Token).Get_Line (Positive (Location.Line)));

      Cursor   : VSS.Strings.Character_Iterators.Character_Iterator :=
        Line.Before_First_Character;

   begin
      return Result : LSP.Structures.Position :=
               (line => Positive (Location.Line) - 1, character => 0)
      do
         for J in 1 .. Location.Column loop
            exit when not Cursor.Forward;
         end loop;

         Result.character := Natural (Cursor.First_UTF16_Offset);
      end return;
   end Start_Position;

   ---------------------
   -- To_LSP_Location --
   ---------------------

   function To_LSP_Location
     (Self    : in out Message_Handler'Class;
      Context : LSP.Ada_Contexts.Context;
      File    : String;
      Sloc    : Langkit_Support.Slocs.Source_Location_Range;
      Kinds   : LSP.Structures.AlsReferenceKind_Set := LSP.Constants.Empty)
      return LSP.Structures.Location
   is
      use type LSP.Ada_Documents.Document_Access;

      URI : constant LSP.Structures.DocumentUri :=
        (VSS.Strings.Conversions.To_Virtual_String
           (URIs.Conversions.From_File (File))
         with null record);

      Doc : constant LSP.Ada_Documents.Document_Access :=
        Self.Get_Open_Document (URI);

   begin
      if Doc /= null then
         return Doc.To_LSP_Location (Sloc, Kinds);
      else
         return
           (uri     => URI,
            a_range => To_LSP_Range
              (Context.Get_AU (GNATCOLL.VFS.Create_From_UTF8 (File)), Sloc),
            alsKind => Kinds);
      end if;
   end To_LSP_Location;

   ---------------------
   -- To_LSP_Location --
   ---------------------

   function To_LSP_Location
     (Self : in out Message_Handler'Class;
      Node : Libadalang.Analysis.Ada_Node'Class;
      Kind : LSP.Structures.AlsReferenceKind_Set := LSP.Constants.Empty)
        return LSP.Structures.Location
   is
      use type LSP.Ada_Documents.Document_Access;

      URI : constant LSP.Structures.DocumentUri :=
        (VSS.Strings.Conversions.To_Virtual_String
           (URIs.Conversions.From_File (Node.Unit.Get_Filename))
         with null record);

      Sloc : constant Langkit_Support.Slocs.Source_Location_Range :=
        Node.Sloc_Range;

      Doc : constant LSP.Ada_Documents.Document_Access :=
        Self.Get_Open_Document (URI);

   begin
      if Doc /= null then
         return Doc.To_LSP_Location (Sloc, Kind);

      else
         return
           (uri => URI,
            a_range => To_LSP_Range (Node.Unit, Sloc),
            alsKind => Kind);

      end if;
   end To_LSP_Location;

   ------------------
   -- To_LSP_Range --
   ------------------

   function To_LSP_Range
     (Unit : Libadalang.Analysis.Analysis_Unit;
      Sloc : Langkit_Support.Slocs.Source_Location_Range)
        return LSP.Structures.A_Range
   is
      Result : LSP.Structures.A_Range :=
        (start  => (line      => Positive (Sloc.Start_Line) - 1,
                    character => 0),
         an_end => (line      => Positive (Sloc.End_Line) - 1,
                          character => 0));
   begin
      declare
         use type Langkit_Support.Slocs.Column_Number;

         Line   : constant VSS.Strings.Virtual_String :=
           VSS.Strings.To_Virtual_String
             (Unit.Get_Line (Positive (Sloc.Start_Line)));

         Cursor : VSS.Strings.Character_Iterators.Character_Iterator :=
           Line.Before_First_Character;

      begin
         for J in 1 .. Sloc.Start_Column loop
            exit when not Cursor.Forward;
         end loop;

         Result.start.character := Natural (Cursor.First_UTF16_Offset);

         if Result.start.line = Result.an_end.line then
            for J in Sloc.Start_Column .. Sloc.End_Column - 1 loop
               exit when not Cursor.Forward;
            end loop;

            Result.an_end.character :=
              Natural (Cursor.First_UTF16_Offset);

            return Result;
         end if;
      end;

      declare
         Line : constant VSS.Strings.Virtual_String :=
           VSS.Strings.To_Virtual_String
             (Unit.Get_Line (Positive (Sloc.End_Line)));

         Cursor : VSS.Strings.Character_Iterators.Character_Iterator :=
           Line.Before_First_Character;

      begin
         for J in 1 .. Sloc.End_Column loop
            exit when not Cursor.Forward;
         end loop;

         Result.an_end.character := Natural (Cursor.First_UTF16_Offset);

         return Result;
      end;
   end To_LSP_Range;

end LSP.Ada_Handlers.Locations;
