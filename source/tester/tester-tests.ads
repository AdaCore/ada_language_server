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

with GNATCOLL.JSON;

with Spawn.Processes;

package Tester.Tests is

   type Test is tagged limited private;

   not overriding procedure Run
     (Self     : in out Test;
      Commands : GNATCOLL.JSON.JSON_Array);

private

   type Listener (Test : access Tester.Tests.Test) is limited
     new Spawn.Processes.Process_Listener with null record;

   overriding procedure Error_Occurred
    (Self          : in out Listener;
     Process_Error : Integer);

   type Test is tagged limited record
      Server   : Spawn.Processes.Process;
      Listener : aliased Tester.Tests.Listener (Test'Unchecked_Access);
      Index    : Positive := 1;
   end record;

   not overriding procedure Execute_Command
     (Self    : in out Test;
      Command : GNATCOLL.JSON.JSON_Value);

   not overriding procedure Do_Abort (Self : Test);

   not overriding procedure Do_Fail (Self : Test; Message : String);
   --  Mark tes as failed with given message

end Tester.Tests;
