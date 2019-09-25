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

with Ada.Exceptions;           use Ada.Exceptions;
with GNAT.Traceback.Symbolic;  use GNAT.Traceback.Symbolic;

package body LSP.Common is

   ---------
   -- Log --
   ---------

   procedure Log
     (Trace   : GNATCOLL.Traces.Trace_Handle;
      E       : Ada.Exceptions.Exception_Occurrence;
      Message : String := "") is
   begin
      if Message /= "" then
         Trace.Trace (Message);
      end if;

      Trace.Trace (Exception_Name (E) & ": " & Exception_Message (E)
                   & ASCII.LF & Symbolic_Traceback (E));
   end Log;

end LSP.Common;
