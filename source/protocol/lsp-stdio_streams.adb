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

with Interfaces.C;
with Ada.IO_Exceptions;

package body LSP.Stdio_Streams is

   package C renames Interfaces.C;

   procedure Initialize;
   --  Do OS dependent lowlevel initialization, if required.

   procedure Initialize is separate;

   ----------
   -- Read --
   ----------

   overriding procedure Read
     (Stream : in out Stdio_Stream;
      Item   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset)
   is
      pragma Unreferenced (Stream);
      use type Ada.Streams.Stream_Element_Offset;

      function read
        (fildes : C.int;
         buf    : out Ada.Streams.Stream_Element_Array;
         nbyte  : C.size_t) return C.size_t
           with Import => True,
                Convention => C,
                External_Name => "read";
      Done    : C.size_t;
   begin
      Done := read (0, Item, Item'Length);
      Last := Item'First + Ada.Streams.Stream_Element_Offset (Done) - 1;

      if Last < Item'First then
         raise Ada.IO_Exceptions.End_Error with "end of file";
      end if;
   end Read;

   -----------
   -- Write --
   -----------

   overriding procedure Write
     (Stream : in out Stdio_Stream;
      Item   : Ada.Streams.Stream_Element_Array)
   is
      function write
        (fildes : C.int;
         buf    : Ada.Streams.Stream_Element_Array;
         nbyte  : C.size_t) return C.size_t
           with Import => True,
                Convention => C,
                External_Name => "write";
      pragma Unreferenced (Stream);

      Ignore : C.size_t := write (1, Item, Item'Length);
   begin
      null;
   end Write;

begin
   Initialize;
end LSP.Stdio_Streams;
