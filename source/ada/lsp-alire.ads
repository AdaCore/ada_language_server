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

with GPR2.Environment;

with LSP.Ada_Client_Capabilities;

with VSS.Strings;

private package LSP.Alire is

   function Is_Alire_Crate
     (Client : LSP.Ada_Client_Capabilities.Client_Capability) return Boolean;
   --  True if 'alire.toml' exists at 'Client' root

   function Should_Setup_Alire_Env
     (Client : LSP.Ada_Client_Capabilities.Client_Capability) return Boolean;
   --  True if 'alire.toml' exists at 'Client' root, and if there isn't an
   --  environment variable ALIRE = "True". The latter variable indicates that
   --  we are in a context where the Alire environment has already been
   --  set up.

   procedure Conservative_Alire_Sync
     (Root : String; Error : out VSS.Strings.Virtual_String);
   --  Perform a conservative Alire sync action. The goal is to ensure a state
   --  where the crate's dependencies have been deployed, and the config/
   --  directory has been generated, such that the pre-requisites for loading
   --  the crate's GPR project are satisfied.
   --
   --  Currently that action is `alr --non-interactive build --stop-after=generation`.
   --
   --  If the workspace is a fresh crate checkout where Alire has never been
   --  called before, this action will deploy dependencies and generate the
   --  config/ directory which are both needed for loading the project.
   --
   --  Otherwise if the workspace is not a fresh checkout, the action will not
   --  alter existing dependencies to avoid taking drastic actions without User
   --  confirmation.

   procedure Determine_Alire_Project
     (Root    : String;
      Error   : out VSS.Strings.Virtual_String;
      Project : out VSS.Strings.Virtual_String);
   --  if Root directory contains `alire.toml` file, then run
   --  `alr show` and determine the project from the output.

   procedure Setup_Alire_Env
     (Root        : String;
      Error       : out VSS.Strings.Virtual_String;
      Environment : in out GPR2.Environment.Object);
   --  Run `alr printenv` and set up the obtained environment variables

end LSP.Alire;
