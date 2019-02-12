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
with Ada.Streams;

with Spawn.Windows_API;
pragma Warnings (Off);
with System.Win32;
pragma Warnings (On);

package Spawn.Internal is
   pragma Preelaborate;

   package Environments is
      function To_Key (Text : UTF_8_String) return Wide_String;

      function "=" (Left, Right : UTF_8_String) return Boolean;
      function "<" (Left, Right : UTF_8_String) return Boolean;
   end Environments;

   type Process;

   type Pipe_Kinds is (Stdin, Stdout, Stderr);
   subtype Stream_Element_Buffer is
     Ada.Streams.Stream_Element_Array (1 .. 512);

   type Context is limited record
      lpOverlapped : Windows_API.OVERLAPPED;
      Process      : access Spawn.Internal.Process'Class;
      Kind         : Pipe_Kinds;
      Handle       : Windows_API.HANDLE := System.Win32.INVALID_HANDLE_VALUE;
      Buffer       : Stream_Element_Buffer;
      Last         : Ada.Streams.Stream_Element_Count := 0 with Atomic;
      --  Last could be > Buffer'Last for Stdin that means 'send notification'
   end record;

   type Pipe_Array is array (Pipe_Kinds) of aliased Context;
   --  Context for each pipe kind

   type Process is new Ada.Finalization.Limited_Controlled with record
      pid   : aliased Windows_API.PROCESS_INFORMATION;
      pipe  : Pipe_Array;
      Index : Natural := 0;
   end record;

end Spawn.Internal;
