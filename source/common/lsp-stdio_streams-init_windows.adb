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
--
--  Windows dependent initialization. Turn stdio into binary mode to avoid
--  CR/LF convertions

separate (LSP.Stdio_Streams)
procedure Initialize is
   procedure setmode (df, mode : Interfaces.C.int)
     with Import,
          Convention => C,
          External_Name => "_setmode";

   O_BINARY : constant := 16#8000#;
begin
   --  Process stdin/stdout in binary mode to be compatible with linux
   setmode (0, O_BINARY);
   setmode (1, O_BINARY);
end Initialize;
