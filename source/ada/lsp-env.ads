------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2024, AdaCore                          --
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

with GNATCOLL.VFS;
with VSS.Strings;
with VSS.Application;

--  This package exposes the values of environment values used by the ALS, e.g.
--  location of global user configuration, location of log files...

package LSP.Env is
   use type GNATCOLL.VFS.Virtual_File;

   Testing : constant Boolean :=
      VSS.Application.System_Environment.Contains ("ALS_TESTING");
   --  Constant set to True when ALS is running within a testsuite.

   Home : constant VSS.Strings.Virtual_String :=
     VSS.Application.System_Environment.Value ("HOME");
   --  Value of the HOME environment variable

   function Home_Dir return GNATCOLL.VFS.Virtual_File;
   --  Directory pointed by the HOME environment variable

   ALS_Home : constant VSS.Strings.Virtual_String :=
     VSS.Application.System_Environment.Value ("ALS_HOME");
   --  The value of the ALS_HOME environment variable.
   --
   --  The ALS_HOME environment controls the location where ALS produces logs.
   --  If specified, $ALS_HOME/.als is used, otherwise $HOME/.als is used.

   GPR_Path : constant VSS.Strings.Virtual_String :=
     VSS.Application.System_Environment.Value ("GPR_PROJECT_PATH");
   --  The value of the GPR_PROJECT_PATH environment variable.

   Path : constant VSS.Strings.Virtual_String :=
     VSS.Application.System_Environment.Value ("PATH");
   --  The value of the PATH environment variable.

   function ALS_Log_Dir return GNATCOLL.VFS.Virtual_File;
   --  The location where ALS produces logs. If ALS_HOME is specified,
   --  $ALS_HOME/.als is used, otherwise $HOME/.als is used.
   --
   --  In an exotic case where HOME is not defined, use the current directory

   function XDG_CONFIG_HOME return VSS.Strings.Virtual_String;
   --  The XDG_CONFIG_HOME environment variable, defaulting to $HOME/.config if
   --  unspecified.
   --
   --  See XDG Base Directory Specification at
   --  https://specifications.freedesktop.org/basedir-spec/latest

   function ALS_User_Config_Dir return GNATCOLL.VFS.Virtual_File;
   --  The $XDG_CONFIG_HOME/als directory where ALS reads the global
   --  configuration file.

   function ALS_User_Config_File return GNATCOLL.VFS.Virtual_File;
   --  The $XDG_CONFIG_HOME/als/config.json file where ALS reads the global
   --  configuration.

   function ALS_Workspace_Config_File return GNATCOLL.VFS.Virtual_File;
   --  The file .als.json in the current directory. This is the workspace
   --  configuration file.

end LSP.Env;
