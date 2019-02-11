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

private package Spawn.Processes.Monitor is

   type Command_Kind is
     (Start, Close_Pipe, Watch_Pipe);

   type Command (Kind : Command_Kind := Start) is record
      Process : access Spawn.Processes.Process'Class;
      case Kind is
         when Start =>
            null;
         when Close_Pipe | Watch_Pipe =>
            Pipe : Standard_Pipe;
      end case;
   end record;

   procedure Enqueue (Value : Command);

   procedure Loop_Cycle (Timeout : Integer);
   --  Timeout in milliseconds. Dont wait if zero. Wait forever if < 0

end Spawn.Processes.Monitor;
