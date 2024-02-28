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

--  This package provides an text document that use Langkit's conventions of
--  indexing characters, positions, and slices (ranges).
--
--  The indexing conventions in Langkit align with VSS's
--  Line_Index/Character_Index when tabulation expansion in Langkit is turned
--  off (with a tabulation size set to 1). In a similar manner, the indices of
--  ranges (spans, slices) directly correspond to the characters themselves.
--
--  LSP employs a distinct set of conventions, and it's essential to avoid
--  converting VSS/Langkit indexes and ranges directly through type conversion.

with Langkit_Support.Slocs;

package LSP.Text_Documents.Langkit_Documents is

   type Langkit_Text_Document is abstract new Text_Document with private;

   function To_Source_Line
     (Line : Natural) return Langkit_Support.Slocs.Line_Number is
     (Langkit_Support.Slocs.Line_Number (Line + 1));
   --  Convert LSP line to Langkit line number. For 'Line_Text' initialization
   --  used during To_Source_Location(Line_Text, Position) calls.

   function To_Source_Location
     (Self     : Langkit_Text_Document'Class;
      Position : LSP.Structures.Position)
      return Langkit_Support.Slocs.Source_Location;
   --  Convert a LSP's Position to a Langkit's Source_Location

   function To_Source_Location
     (Line_Text : VSS.Strings.Virtual_String;
      Position : LSP.Structures.Position)
      return Langkit_Support.Slocs.Source_Location;
   --  Convert a LSP's Position to a Langkit's Source_Location when
   --  Langkit_Text_Document not available

   function To_Source_Location_Range
     (Self    : Langkit_Text_Document'Class;
      A_Range : LSP.Structures.A_Range)
      return Langkit_Support.Slocs.Source_Location_Range;
   --  Convert a LSP's A_Range to a Langkit's Source_Location_Range

   function To_A_Range
     (Self    : Langkit_Text_Document'Class;
      A_Range : Langkit_Support.Slocs.Source_Location_Range)
      return LSP.Structures.A_Range;
   --  Convert LAL's Source_Location_Range to LSP's A_Range

   function To_A_Range
     (Start_Line_Text : VSS.Strings.Virtual_String;
      End_Line_Text   : VSS.Strings.Virtual_String;
      A_Range         : Langkit_Support.Slocs.Source_Location_Range)
      return LSP.Structures.A_Range;
   --  Convert Langkit Source_Location_Range to LSP's A_Range

private

   type Langkit_Text_Document is abstract new Text_Document with null record;

end LSP.Text_Documents.Langkit_Documents;
