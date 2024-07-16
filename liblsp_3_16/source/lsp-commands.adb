------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2020-2021, AdaCore                     --
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

with VSS.Strings.Conversions;

package body LSP.Commands is

   Command_Name_List : VSS.String_Vectors.Virtual_String_Vector;

   --------------
   -- Register --
   --------------

   procedure Register (Value : Ada.Tags.Tag) is
      Name : constant VSS.Strings.Virtual_String :=
        VSS.Strings.Conversions.To_Virtual_String
          (Ada.Tags.External_Tag (Value));

   begin
      Command_Name_List.Append (Name);
   end Register;

   ------------------
   -- All_Commands --
   ------------------

   function All_Commands return VSS.String_Vectors.Virtual_String_Vector is
   begin
      return Command_Name_List;
   end All_Commands;

end LSP.Commands;
