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

with Ada.Streams;
with Interfaces.C.Strings;

package Spawn.Posix is

   function close (fd : Interfaces.C.int) return Interfaces.C.int
     with Import, Convention => C, Link_Name => "close";

   function read
     (fd    : Interfaces.C.int;
      buf   : out Ada.Streams.Stream_Element_Array;
      count : Interfaces.C.size_t)
        return Interfaces.C.size_t
          with Import, Convention => C, Link_Name => "read";

   function write
     (fd    : Interfaces.C.int;
      buf   : Ada.Streams.Stream_Element_Array;
      count : Interfaces.C.size_t)
        return Interfaces.C.size_t
          with Import, Convention => C, Link_Name => "write";

   type Pipe_Ends is (Read_End, Write_End);

   type Fd_Pair is array (Pipe_Ends) of Interfaces.C.int
     with Convention => C;

   function pipe2 (pipefd : out Fd_Pair; flags : Interfaces.C.int)
     return Interfaces.C.int
        with Import, Convention => C, Link_Name => "pipe2";

   O_CLOEXEC  : constant := 16#80000#;
   O_NONBLOCK : constant := 16#800#;
   POLLIN     : constant := 16#0001#;
   POLLOUT    : constant := 16#0004#;
   POLLERR    : constant := 16#0008#;
   POLLHUP    : constant := 16#0010#;
   POLLNVAL   : constant := 16#0020#;

   function fork  return Interfaces.C.int
     with Import, Convention => C, Link_Name => "fork";

   function dup2
     (oldfd : Interfaces.C.int;
      newfd : Interfaces.C.int)
        return Interfaces.C.int
          with Import, Convention => C, Link_Name => "dup2";

   function chdir (path : Interfaces.C.Strings.chars_ptr)
     return Interfaces.C.int
       with Import, Convention => C, Link_Name => "chdir";

   type chars_ptr_array is array (Natural range <>) of
     aliased Interfaces.C.Strings.chars_ptr;

   function execve
     (file : Interfaces.C.Strings.chars_ptr;
      argv : chars_ptr_array;
      anvp : chars_ptr_array)
     return Interfaces.C.int
        with Import, Convention => C, Link_Name => "execve";

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
        with Import, Convention => C, Link_Name => "poll";

   function waitpid
     (pid     : Interfaces.C.int;
      wstatus : access Interfaces.C.unsigned;
      options : Interfaces.C.int) return Interfaces.C.int
        with Import, Convention => C, Link_Name => "waitpid";

   WNOHANG : constant := 1;

   subtype constrained_chars_ptr_array is
     Interfaces.C.Strings.chars_ptr_array (1 .. Interfaces.C.size_t'Last);

   environ : constrained_chars_ptr_array
     with Import, Convention => C, Link_Name => "environ";

end Spawn.Posix;
