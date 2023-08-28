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

with Langkit_Support.Slocs;

with VSS.Strings.Conversions;
with VSS.Strings.Character_Iterators;

with URIs;

package body LSP.Ada_Handlers.Locations is

   ---------------------
   -- Append_Location --
   ---------------------

   procedure Append_Location
     (Self   : in out Message_Handler;
      Result : in out LSP.Structures.Location_Vector;
      Node   : Libadalang.Analysis.Ada_Node'Class;
      Ignore : AlsReferenceKind_Array := Empty) is
   begin
      if not Node.Is_Synthetic then
         Result.Append (To_LSP_Location (Self, Node));
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
     (Self : in out Message_Handler'Class;
      Node : Libadalang.Analysis.Ada_Node'Class)
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

      Result : LSP.Structures.Location;
   begin
      if Doc /= null then
         return Doc.To_LSP_Location (Sloc);

      else
         Result :=
           (uri     => URI,
            a_range =>
              (start  => (line      => Positive (Sloc.Start_Line) - 1,
                          character => 0),
               an_end => (line      => Positive (Sloc.End_Line) - 1,
                          character => 0)));
      end if;

      declare
         use type Langkit_Support.Slocs.Column_Number;

         Line   : constant VSS.Strings.Virtual_String :=
           VSS.Strings.To_Virtual_String
             (Node.Unit.Get_Line (Positive (Sloc.Start_Line)));

         Cursor : VSS.Strings.Character_Iterators.Character_Iterator :=
           Line.Before_First_Character;

      begin
         for J in 1 .. Sloc.Start_Column loop
            exit when not Cursor.Forward;
         end loop;

         Result.a_range.start.character := Natural (Cursor.First_UTF16_Offset);

         if Result.a_range.start.line = Result.a_range.an_end.line then
            for J in Sloc.Start_Column .. Sloc.End_Column - 1 loop
               exit when not Cursor.Forward;
            end loop;

            Result.a_range.an_end.character :=
              Natural (Cursor.First_UTF16_Offset);

            return Result;
         end if;
      end;

      declare
         Line : constant VSS.Strings.Virtual_String :=
           VSS.Strings.To_Virtual_String
             (Node.Unit.Get_Line (Positive (Sloc.End_Line)));

         Cursor : VSS.Strings.Character_Iterators.Character_Iterator :=
           Line.Before_First_Character;

      begin
         for J in 1 .. Sloc.End_Column loop
            exit when not Cursor.Forward;
         end loop;

         Result.a_range.an_end.character := Natural (Cursor.First_UTF16_Offset);

         return Result;
      end;
   end To_LSP_Location;

end LSP.Ada_Handlers.Locations;
