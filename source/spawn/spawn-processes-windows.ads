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

with Spawn.Windows_API;

private
package Spawn.Processes.Windows is

   procedure Do_Start_Process
     (Self     : aliased in out Process'Class;
      On_Start : access procedure);

   procedure Do_Close_Pipe
     (Self : in out Process'Class;
      Kind : Pipe_Kinds);

   procedure Do_Write
     (Self       : in out Process'Class;
      Data       : Ada.Streams.Stream_Element_Array;
      Last       : out Ada.Streams.Stream_Element_Offset;
      On_No_Data : access procedure);

   procedure Do_Read
     (Self       : in out Process'Class;
      Data       : out Ada.Streams.Stream_Element_Array;
      Last       : out Ada.Streams.Stream_Element_Offset;
      Kind       : Pipe_Kinds;
      On_No_Data : access procedure);

   procedure On_Process_Died (Self : in out Process'Class);

   procedure IO_Callback
     (dwErrorCode               : Windows_API.DWORD;
      dwNumberOfBytesTransfered : Windows_API.DWORD;
      lpOverlapped              : access Internal.Context;
      Kind                      : Standard_Pipe);
   --  Implementation shared between Standard_[Output/Error]_Callback

end Spawn.Processes.Windows;
