------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                        Copyright (C) 2023, AdaCore                       --
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

--  Alire integration routines

with VSS.Strings;
with VSS.String_Vectors;

private
package LSP.Ada_Handlers.Alire is

   procedure Run_Alire
     (Root        : String;
      Has_Alire   : out Boolean;
      Error       : out VSS.Strings.Virtual_String;
      Project     : out VSS.Strings.Virtual_String;
      Search_Path : out VSS.String_Vectors.Virtual_String_Vector;
      Scenario    : in out Scenario_Variable_List);
   --  if Root directory contains `alire.toml` file, then run
   --  `alr printenv` and fetch the first project from `alire.toml`.

   procedure Run_Alire
     (Root        : String;
      Has_Alire   : out Boolean;
      Error       : out VSS.Strings.Virtual_String;
      Search_Path : out VSS.String_Vectors.Virtual_String_Vector;
      Scenario    : in out Scenario_Variable_List);
   --  THe same, but when without fetching the project file.

end LSP.Ada_Handlers.Alire;
