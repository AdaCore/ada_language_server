------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2018-2020, AdaCore                     --
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
--
--  This is an OSX implementation. For some reason, we are unable to catch
--  SIGCHLD signal until it's enabled for the environment task.
--

with Ada.Interrupts.Names;
with Interfaces.C;

pragma Warnings (Off);
with System.OS_Interface;
pragma Warnings (Off);

separate (Spawn.Processes.Monitor)
procedure Initialize is
   Ignore : Interfaces.C.int;
   Value  : aliased System.OS_Interface.struct_sigaction :=
     (sa_flags    => 0,
      others      => <>);
begin
   --  Reset sigaction to call a null procedure
   Ignore := System.OS_Interface.sigemptyset
     (Value.sa_mask'Unrestricted_Access);
   --  Set dummy procedure as the handler
   Value.sa_handler := Dummy'Address;

   --  Assign the custom handler to SIGCHLD signal
   Ignore := System.OS_Interface.sigaction
     (System.OS_Interface.Signal (Ada.Interrupts.Names.SIGCHLD),
      Value'Unchecked_Access,
      null);
end Initialize;
