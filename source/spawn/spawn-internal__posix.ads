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

with Ada.Finalization;
with Interfaces.C;

private package Spawn.Internal is

   package Environments is

      function "=" (Left, Right : UTF_8_String) return Boolean;
      function "<" (Left, Right : UTF_8_String) return Boolean;

   end Environments;

   type Pipe_Kinds is (Stdin, Stdout, Stderr, Launch);

   type Pipe_Array is array (Pipe_Kinds) of Interfaces.C.int;
   --  File descriptors array

   type Index_Array is array (Pipe_Kinds) of Natural;
   --  Index in poll for each descriptors array

   type Process is new Ada.Finalization.Limited_Controlled with record
      pid   : Interfaces.C.int := 0;
      pipe  : Pipe_Array := (others => 0);
      Index : Index_Array := (others => 0);
   end record;

end Spawn.Internal;
