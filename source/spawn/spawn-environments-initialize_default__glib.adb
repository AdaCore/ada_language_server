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
with Interfaces.C;

with GNAT.Strings;

with Glib.Spawn;

separate (Spawn.Environments)
procedure Initialize_Default
  (Default : out Spawn.Environments.Process_Environment)
is
   List : GNAT.Strings.String_List := Glib.Spawn.Get_Environ;
begin
   for Text of List loop
      declare
         Separator : constant Natural :=
           Ada.Strings.Fixed.Index (Text.all, "=");
      begin
         exit when Separator = 0;

         Default.Insert
           (Text (Text'First .. Separator - 1),
            Text (Separator + 1 .. Text'Last));

         GNAT.Strings.Free (Text);
      end;
   end loop;
end Initialize_Default;
