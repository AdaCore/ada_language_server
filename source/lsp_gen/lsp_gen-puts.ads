------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2022-2023, AdaCore                     --
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

with VSS.Strings;
with VSS.String_Vectors;

with LSP_Gen.Entities;

package LSP_Gen.Puts is

   procedure Put (Text : VSS.Strings.Virtual_String);
   --  Write Text to stdout

   procedure Put (Number : Integer);

   procedure Put_Line (Text : VSS.Strings.Virtual_String);
   --  Write Text and new line to stdout

   procedure Put_Lines
     (List   : VSS.String_Vectors.Virtual_String_Vector;
      Prefix : VSS.Strings.Virtual_String := VSS.Strings.Empty_Virtual_String);
   --  Write several lines, put the Prefix at the beginning of each line

   procedure New_Line;
   --  Write a line terminator to stdout

   procedure Put_Id (Id : VSS.Strings.Virtual_String);
   --  Write Id as an Ada identifier, use some prefix if Id is an Ada reserved
   --  keyword.

   procedure Put_Type (X : LSP_Gen.Entities.AType);
   --  Write a type name

end LSP_Gen.Puts;
