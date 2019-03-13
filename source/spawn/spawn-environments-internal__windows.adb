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
with Ada.Strings.Wide_Unbounded;

pragma Warnings (Off);
with System.Win32;
pragma Warnings (On);

with Spawn.Internal;

package body Spawn.Environments.Internal is

   ---------
   -- Raw --
   ---------

   function Raw
     (Self : Process_Environment'Class) return Interfaces.C.wchar_array
   is
      Sum : Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;
   begin
      for J in Self.Map.Iterate loop
         Ada.Strings.Wide_Unbounded.Append
           (Sum,
            Spawn.Internal.Environments.To_Key (UTF_8_String_Maps.Key (J)));

         Ada.Strings.Wide_Unbounded.Append (Sum, "=");

         Ada.Strings.Wide_Unbounded.Append
           (Sum,
            Ada.Strings.UTF_Encoding.Wide_Strings.Decode
              (UTF_8_String_Maps.Element (J)));

         Ada.Strings.Wide_Unbounded.Append (Sum, Wide_Character'Val (0));
      end loop;

      Ada.Strings.Wide_Unbounded.Append (Sum, Wide_Character'Val (0));

      return Interfaces.C.To_C
        (Ada.Strings.Wide_Unbounded.To_Wide_String (Sum));
   end Raw;

end Spawn.Environments.Internal;
