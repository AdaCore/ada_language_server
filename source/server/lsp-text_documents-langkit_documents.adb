------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                      Copyright (C) 2023-2024, AdaCore                    --
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

   function To_LSP_Column
     (Line_Text : VSS.Strings.Virtual_String;
      Column    : Langkit_Support.Slocs.Column_Number;
      At_Start  : Boolean := True) return Natural;

   use type Langkit_Support.Slocs.Line_Number;

   function To_LSP_Line
     (Line : Langkit_Support.Slocs.Line_Number) return Natural
   is (if Line /= 0
       then Natural (Line) - 1
       else 0);

   function Line
     (Self  : Langkit_Text_Document'Class;
      Index : Langkit_Support.Slocs.Line_Number)
      return VSS.Strings.Virtual_String;

   function To_Source_Column
     (Iterator : in out VSS.Strings.Character_Iterators.Character_Iterator;
      Column   : Natural)
      return Langkit_Support.Slocs.Column_Number;

   function To_Source_Column
     (Line_Text : VSS.Strings.Virtual_String;
      Column    : Natural)
      return Langkit_Support.Slocs.Column_Number;

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
   is (start =>
         (line      => To_LSP_Line (A_Range.Start_Line),
          character => To_LSP_Column
            (Self.Line (A_Range.Start_Line), A_Range.Start_Column)),
       an_end =>
         (line      => To_LSP_Line (A_Range.End_Line),
          character => To_LSP_Column
            (Self.Line (A_Range.End_Line), A_Range.End_Column, False)));

   function To_A_Range
     (Start_Line_Text : VSS.Strings.Virtual_String;
      End_Line_Text   : VSS.Strings.Virtual_String;
      A_Range         : Langkit_Support.Slocs.Source_Location_Range)
      return LSP.Structures.A_Range
   is (start =>
         (line      => To_LSP_Line (A_Range.Start_Line),
          character => To_LSP_Column
            (Start_Line_Text, A_Range.Start_Column)),
       an_end =>
         (line      => To_LSP_Line (A_Range.End_Line),
          character => To_LSP_Column
            (End_Line_Text, A_Range.End_Column, False)));

   ---------------------
   -- To_LSP_Position --
   ---------------------

   function To_LSP_Position
     (Self    : Langkit_Text_Document'Class;
      Sloc : Langkit_Support.Slocs.Source_Location)
      return LSP.Structures.Position
   is
     (line => To_LSP_Line (Sloc.Line),
      character => To_LSP_Column
        (Self.Line (Sloc.Line), Sloc.Column, False));

   -------------------
   -- To_LSP_Column --
   -------------------

   function To_LSP_Column
     (Line_Text : VSS.Strings.Virtual_String;
      Column    : Langkit_Support.Slocs.Column_Number;
      At_Start  : Boolean := True) return Natural
   is
      Iterator : VSS.Strings.Character_Iterators.Character_Iterator :=
                   Line_Text.At_First_Character;
      Success  : Boolean with Unreferenced;
   begin
      --  Iterating forward through the line, initial
      --  iterator points to the first characters, thus "starts" from the
      --  second one.

      for J in 2 .. Column loop
         Success := Iterator.Forward;
      end loop;

      if At_Start then
         return Natural (Iterator.First_UTF16_Offset);
      else
         return Natural (Iterator.Last_UTF16_Offset);
      end if;
   end To_LSP_Column;

   ----------------------
   -- To_Source_Column --
   ----------------------

   function To_Source_Column
     (Iterator : in out VSS.Strings.Character_Iterators.Character_Iterator;
      Column   : Natural)
   return Langkit_Support.Slocs.Column_Number is
      use type VSS.Strings.Character_Index;
      use type VSS.Unicode.UTF16_Code_Unit_Offset;

      Line_Offset          : constant VSS.Unicode.UTF16_Code_Unit_Offset :=
                               Iterator.First_UTF16_Offset;
      Line_First_Character : constant VSS.Strings.Character_Index :=
                               Iterator.Character_Index;
   begin
      while Integer (Iterator.First_UTF16_Offset - Line_Offset) <= Column
        and then Iterator.Forward
      loop
         null;
      end loop;

      return Langkit_Support.Slocs.Column_Number
        (Iterator.Character_Index - Line_First_Character);
   end To_Source_Column;

   ----------------------
   -- To_Source_Column --
   ----------------------

   function To_Source_Column
     (Line_Text : VSS.Strings.Virtual_String;
      Column    : Natural)
   return Langkit_Support.Slocs.Column_Number is
      Iterator : VSS.Strings.Character_Iterators.Character_Iterator :=
                   VSS.Strings.At_First_Character (Line_Text);
   begin
      return To_Source_Column (Iterator, Column);
   end To_Source_Column;

   ------------------------
   -- To_Source_Location --
   ------------------------

   function To_Source_Location
     (Self     : Langkit_Text_Document'Class;
      Position : LSP.Structures.Position)
      return Langkit_Support.Slocs.Source_Location
   is
      Iterator    : VSS.Strings.Character_Iterators.Character_Iterator :=
        Self.Text.At_Character (Self.Line_Marker (Position.line));
   begin
      return ((Line   => To_Source_Line (Position.line),
               Column => To_Source_Column (Iterator, Position.character)));
   end To_Source_Location;

   function To_Source_Location
     (Line_Text : VSS.Strings.Virtual_String;
      Position : LSP.Structures.Position)
      return Langkit_Support.Slocs.Source_Location is
      ((Line   => To_Source_Line (Position.line),
        Column => To_Source_Column (Line_Text, Position.character)));

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
