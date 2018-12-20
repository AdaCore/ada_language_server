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

with Ada.Strings.Unbounded;

with GNATCOLL.JSON;

with LSP.Raw_Clients;

package Tester.Tests is

   type Test is tagged limited private;

   not overriding procedure Run
     (Self     : in out Test;
      Commands : GNATCOLL.JSON.JSON_Array);

private

   type Test is new LSP.Raw_Clients.Raw_Client with record
      Index     : Positive := 1;
      Waits     : GNATCOLL.JSON.JSON_Array;
      --  Array of JSON object to wait
   end record;

   overriding procedure On_Error
     (Self  : in out Test;
      Error : String);

   overriding procedure On_Raw_Message
     (Self : in out Test;
      Data : Ada.Strings.Unbounded.Unbounded_String);

   not overriding procedure Execute_Command
     (Self    : in out Test;
      Command : GNATCOLL.JSON.JSON_Value);

   not overriding procedure Do_Abort (Self : Test);

   not overriding procedure Do_Fail (Self : Test; Message : String);
   --  Mark tes as failed with given message

end Tester.Tests;
