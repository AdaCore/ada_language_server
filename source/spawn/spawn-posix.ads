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

with Ada.Streams;
with Interfaces.C.Strings;

package Spawn.Posix is

   function close (fd : Interfaces.C.int) return Interfaces.C.int
     with Import, Convention => C, External_Name => "close";

   function read
     (fd    : Interfaces.C.int;
      buf   : out Ada.Streams.Stream_Element_Array;
      count : Interfaces.C.size_t)
        return Interfaces.C.size_t
          with Import, Convention => C, External_Name => "read";

   function write
     (fd    : Interfaces.C.int;
      buf   : Ada.Streams.Stream_Element_Array;
      count : Interfaces.C.size_t)
        return Interfaces.C.size_t
          with Import, Convention => C, External_Name => "write";

   type Pipe_Ends is (Read_End, Write_End);

   type Fd_Pair is array (Pipe_Ends) of Interfaces.C.int
     with Convention => C;

   function pipe2 (pipefd : out Fd_Pair; flags : Interfaces.C.int)
     return Interfaces.C.int
        with Import, Convention => C, External_Name => "pipe2";

   O_CLOEXEC  : constant Interfaces.C.int
     with Import, Convention => C, External_Name => "SPAWN_O_CLOEXEC";
   O_NONBLOCK : constant Interfaces.C.int
     with Import, Convention => C, External_Name => "SPAWN_O_NONBLOCK";
   POLLIN     : constant Interfaces.C.unsigned_short
     with Import, Convention => C, External_Name => "SPAWN_POLLIN";
   POLLOUT    : constant Interfaces.C.unsigned_short
     with Import, Convention => C, External_Name => "SPAWN_POLLOUT";
   --
   function fork  return Interfaces.C.int
     with Import, Convention => C, External_Name => "fork";

   function dup2
     (oldfd : Interfaces.C.int;
      newfd : Interfaces.C.int)
        return Interfaces.C.int
          with Import, Convention => C, External_Name => "dup2";

   function chdir (path : Interfaces.C.Strings.chars_ptr)
     return Interfaces.C.int
       with Import, Convention => C, External_Name => "chdir";

   type chars_ptr_array is array (Natural range <>) of
     aliased Interfaces.C.Strings.chars_ptr;

   function execve
     (file : Interfaces.C.Strings.chars_ptr;
      argv : chars_ptr_array;
      anvp : chars_ptr_array)
     return Interfaces.C.int
        with Import, Convention => C, External_Name => "execve";

   type pollfd is record
      fd      : Interfaces.C.int;
      events  : Interfaces.C.unsigned_short;
      revents : Interfaces.C.unsigned_short;
   end record with Convention => C;

   type pollfd_array is array (Positive range <>) of pollfd;

   function poll
     (fds     : in out pollfd_array;
      nfds    : Interfaces.C.unsigned_long;
      timeout : Interfaces.C.int) return Interfaces.C.int
        with Import, Convention => C, External_Name => "poll";

   function waitpid
     (pid     : Interfaces.C.int;
      wstatus : access Interfaces.C.unsigned;
      options : Interfaces.C.int) return Interfaces.C.int
        with Import, Convention => C, External_Name => "waitpid";

   WNOHANG : constant Interfaces.C.int
     with Import, Convention => C, External_Name => "SPAWN_WNOHANG";

   function fcntl
     (fd    : Interfaces.C.int;
      cmd   : Interfaces.C.int;
      flags : Interfaces.C.int;
      dummy : Interfaces.C.C_float := 0.0)
        return Interfaces.C.int
          with Import, Convention => C, External_Name => "fcntl";
   --  An extra float argument is used to make this binding compatible
   --  with amd64 ABI for C functions with ellipsis (...).

   F_SETFL : constant Interfaces.C.int
     with Import, Convention => C, External_Name => "SPAWN_F_SETFL";

   subtype constrained_chars_ptr_array is
     Interfaces.C.Strings.chars_ptr_array (1 .. Interfaces.C.size_t'Last);

   environ : constrained_chars_ptr_array
     with Import, Convention => C, External_Name => "environ";

   --  Errno values
   EINTR  : constant Interfaces.C.int
     with Import, Convention => C, External_Name => "SPAWN_EINTR";
   EAGAIN : constant Interfaces.C.int
     with Import, Convention => C, External_Name => "SPAWN_EAGAIN";

end Spawn.Posix;
