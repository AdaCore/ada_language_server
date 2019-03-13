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

with Ada.Strings.Fixed;
with Interfaces.C.Strings;

with Spawn.Posix;

separate (Spawn.Environments)
procedure Initialize_Default
  (Default : out Spawn.Environments.Process_Environment)
is
   use type Interfaces.C.Strings.chars_ptr;
begin
   for J in Spawn.Posix.environ'Range loop
      declare
         Item : constant Interfaces.C.Strings.chars_ptr :=
           Spawn.Posix.environ (J);

         Text : constant UTF_8_String :=
           (if Item = Interfaces.C.Strings.Null_Ptr then ""
            else Interfaces.C.Strings.Value (Item));

         Separator : constant Natural :=
           Ada.Strings.Fixed.Index (Text, "=");
      begin
         exit when Separator = 0;

         Default.Insert
           (Text (Text'First .. Separator - 1),
            Text (Separator + 1 .. Text'Last));
      end;
   end loop;
end Initialize_Default;
