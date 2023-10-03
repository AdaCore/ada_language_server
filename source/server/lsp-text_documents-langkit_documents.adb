------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                        Copyright (C) 2023, AdaCore                       --
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

with VSS.Strings.Character_Iterators;
with VSS.Unicode;

package body LSP.Text_Documents.Langkit_Documents is

   function To_LSP_Line
     (Line : Langkit_Support.Slocs.Line_Number) return Natural
        is (Natural (Line) - 1);

   function Line
     (Self  : Langkit_Text_Document'Class;
      Index : Langkit_Support.Slocs.Line_Number)
      return VSS.Strings.Virtual_String;

   ----------
   -- Line --
   ----------

   function Line
     (Self  : Langkit_Text_Document'Class;
      Index : Langkit_Support.Slocs.Line_Number)
      return VSS.Strings.Virtual_String
   is
      Line : constant Natural := To_LSP_Line (Index);

   begin
      if Self.Line_Marker.Last_Index = Line then
         return
           Self.Text.Slice
             (Self.Line_Marker (Line), Self.Text.After_Last_Character);

      else
         return
           Self.Text.Slice
             (Self.Line_Marker (Line), Self.Line_Marker (Line + 1));
      end if;
   end Line;

   ----------------
   -- To_A_Range --
   ----------------

   function To_A_Range
     (Self    : Langkit_Text_Document'Class;
      A_Range : Langkit_Support.Slocs.Source_Location_Range)
      return LSP.Structures.A_Range
   is
      Start_Line      : constant Natural := To_LSP_Line (A_Range.Start_Line);
      Start_Line_Text : constant VSS.Strings.Virtual_String :=
        Self.Line (A_Range.Start_Line);
      Start_Iterator  : VSS.Strings.Character_Iterators.Character_Iterator :=
        Start_Line_Text.At_First_Character;

      End_Line        : constant Natural := Natural (A_Range.End_Line) - 1;
      End_Line_Text   : constant VSS.Strings.Virtual_String :=
        Self.Line (A_Range.End_Line);
      End_Iterator    : VSS.Strings.Character_Iterators.Character_Iterator :=
        End_Line_Text.At_First_Character;

      Success         : Boolean with Unreferenced;

   begin
      --  Iterating forward through the line of the start position, initial
      --  iterator points to the first characters, thus "starts" from the
      --  second one.

      for J in 2 .. A_Range.Start_Column loop
         Success := Start_Iterator.Forward;
      end loop;

      --  Iterating forward through the line of the end position. For the same
      --  reason "starts" from second character.

      for J in 2 .. A_Range.End_Column loop
         Success := End_Iterator.Forward;
      end loop;

      return
        (start =>
           (line      => Start_Line,
            character => Natural (Start_Iterator.First_UTF16_Offset)),
         an_end =>
           (line      => End_Line,
            character => Natural (End_Iterator.Last_UTF16_Offset)));
   end To_A_Range;

   ------------------------
   -- To_Source_Location --
   ------------------------

   function To_Source_Location
     (Self     : Langkit_Text_Document'Class;
      Position : LSP.Structures.Position)
      return Langkit_Support.Slocs.Source_Location
   is
      use type VSS.Strings.Character_Index;
      use type VSS.Unicode.UTF16_Code_Unit_Offset;

      Iterator    : VSS.Strings.Character_Iterators.Character_Iterator :=
        Self.Text.At_Character (Self.Line_Marker (Position.line));

      Line_Offset          : constant VSS.Unicode.UTF16_Code_Unit_Offset :=
        Iterator.First_UTF16_Offset;
      Line_First_Character : constant VSS.Strings.Character_Index :=
        Iterator.Character_Index;

   begin
      while Integer (Iterator.First_UTF16_Offset - Line_Offset)
               <= Position.character
        and then Iterator.Forward
      loop
         null;
      end loop;

      return ((Line   => Langkit_Support.Slocs.Line_Number (Position.line + 1),
               Column => Langkit_Support.Slocs.Column_Number
                 (Iterator.Character_Index - Line_First_Character)));
   end To_Source_Location;

   ------------------------------
   -- To_Source_Location_Range --
   ------------------------------

   function To_Source_Location_Range
     (Self    : Langkit_Text_Document'Class;
      A_Range : LSP.Structures.A_Range)
      return Langkit_Support.Slocs.Source_Location_Range is
   begin
      return
        Langkit_Support.Slocs.Make_Range
          (Self.To_Source_Location (A_Range.start),
           Self.To_Source_Location (A_Range.an_end));
   end To_Source_Location_Range;

end LSP.Text_Documents.Langkit_Documents;
