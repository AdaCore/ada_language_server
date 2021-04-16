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

with GNATCOLL.JSON;

with Spawn.Environments;

package Tester.Macros is

   procedure Expand
     (Test : in out GNATCOLL.JSON.JSON_Value;
      Env  : Spawn.Environments.Process_Environment;
      Path : String);
   --  Expand macros in given JSON test. The Path is test's path.
   --
   --  Currently only one macro is supported:
   --  * ${NAME} - expands with environment variable NAME from Env
   --
   --  * $URI{x} - rewrite as "file:///path", treat x as relative to test
   --  directory (Path)

end Tester.Macros;
