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
         for J in 1 .. Sloc.Start_Column loop
            exit when not Cursor.Forward;
         end loop;

         Result.a_range.an_end.character := Natural (Cursor.First_UTF16_Offset);

         return Result;
      end;
   end To_LSP_Location;

end LSP.Ada_Handlers.Locations;
