------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                        Copyright (C) 2018, AdaCore                       --
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

with Gtkada.Bindings;
with Glib.Spawns;

package body Spawn.Environments.Internal is

   ------------------------
   -- Initialize_Default --
   ------------------------

   procedure Initialize_Default
     (Default : out Spawn.Environments.Process_Environment)
   is
      List : GNAT.Strings.String_List :=
        Gtkada.Bindings.To_String_List_And_Free (Glib.Spawns.g_get_environ);
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

   ---------
   -- Raw --
   ---------

   function Raw
     (Self : Process_Environment'Class)
      return Gtkada.Types.Chars_Ptr_Array
   is
      use type Interfaces.C.size_t;

      Index : Interfaces.C.size_t := 1;
   begin
      return Result : Gtkada.Types.Chars_Ptr_Array
        (1 .. Interfaces.C.size_t (Self.Map.Length) + 1)
      do
         for J in Self.Map.Iterate loop
            Result (Index) := Gtkada.Types.New_String
              (UTF_8_String_Maps.Key (J) & "=" &
                 UTF_8_String_Maps.Element (J));
            Index := Index + 1;
         end loop;

         Result (Index) := Gtkada.Types.Null_Ptr;
      end return;
   end Raw;

end Spawn.Environments.Internal;
