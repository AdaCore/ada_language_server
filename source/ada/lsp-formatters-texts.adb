------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2023, AdaCore                          --
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

package body LSP.Formatters.Texts is

   -----------
   -- Image --
   -----------

   function Image (Item : Langkit_Support.Text.Text_Type) return Formatter is
   begin
      return
        (VSS.Strings.Formatters.Strings.Image
           (VSS.Strings.To_Virtual_String (Item)) with null record);
   end Image;

   -----------
   -- Image --
   -----------

   function Image
     (Name : VSS.Strings.Virtual_String;
      Item : Langkit_Support.Text.Text_Type) return Formatter is
   begin
      return
        (VSS.Strings.Formatters.Strings.Image
           (Name, VSS.Strings.To_Virtual_String (Item)) with null record);
   end Image;

end LSP.Formatters.Texts;
