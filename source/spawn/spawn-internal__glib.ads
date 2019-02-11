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

with Glib.IOChannel;
with Glib.Main;
with Glib.Spawn;

private package Spawn.Internal is

   package Environments is

      function "=" (Left, Right : UTF_8_String) return Boolean;
      function "<" (Left, Right : UTF_8_String) return Boolean;

   end Environments;

   type Pipe_Kinds is (Stdin, Stdout, Stderr);

   type Pipe_Record is record
      FD      : aliased Glib.Gint;
      Channel : Glib.IOChannel.Giochannel;
      Event   : Glib.Main.G_Source_Id;
      --  Pipe is watched if Event /= No_Source_Id
      Watch   : Boolean;
      --  If Read_Standard_*, Write_Standard_Input is called from
      --  Standard_*_Available callback, then set Watch=True to continue
      --  watching of file descriptor.
   end record;

   type Pipe_Array is array (Pipe_Kinds) of Pipe_Record;
   --  File descriptors array

   type Process;

   type Process_Reference is record
      Self : access Process'Class;
   end record;
   --  A wrapper to pass process pointer to C binding functions

   type Process is new Ada.Finalization.Limited_Controlled with record
      Reference : aliased Process_Reference;
      Event     : Glib.Main.G_Source_Id := 0;
      pid       : aliased Glib.Spawn.GPid := 0;
      pipe      : Pipe_Array :=
        (others => (0, null, Glib.Main.No_Source_Id, False));
   end record;

end Spawn.Internal;
