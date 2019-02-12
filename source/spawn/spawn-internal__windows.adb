------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2018-2019, AdaCore                     --
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

with Ada.Strings.UTF_Encoding.Wide_Strings;
with Ada.Wide_Characters.Unicode;

package body Spawn.Internal is

   package body Environments is

      ---------
      -- "=" --
      ---------

      function "=" (Left, Right : UTF_8_String) return Boolean is
      begin
         return To_Key (Left) = To_Key (Right);
      end "=";

      ---------
      -- "<" --
      ---------

      function "<" (Left, Right : UTF_8_String) return Boolean is
      begin
         return To_Key (Left) < To_Key (Right);
      end "<";

      ------------
      -- To_Key --
      ------------

      function To_Key (Text : UTF_8_String) return Wide_String is
         Value : Wide_String :=
           Ada.Strings.UTF_Encoding.Wide_Strings.Decode (Text);
      begin
         for Char of Value loop
            Char := Ada.Wide_Characters.Unicode.To_Upper_Case (Char);
         end loop;

         return Value;
      end To_Key;

   end Environments;

end Spawn.Internal;
