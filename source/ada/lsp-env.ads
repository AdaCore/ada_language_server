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
with LSP.Utils;
with VSS.Strings;
with VSS.Application;

--  This package exposes the values of environment values used by the ALS, e.g.
--  location of global user configuration, location of log files...

package LSP.Env is
   use type GNATCOLL.VFS.Virtual_File;

   Home : constant VSS.Strings.Virtual_String :=
     VSS.Application.System_Environment.Value ("HOME");

   Home_Dir : constant GNATCOLL.VFS.Virtual_File :=
     LSP.Utils.To_Virtual_File (Home);

   ALS_Home : constant VSS.Strings.Virtual_String :=
     VSS.Application.System_Environment.Value ("ALS_HOME");
   --  The ALS_HOME environment controls the location where ALS produces logs.
   --  If specified, $ALS_HOME/.als is used, otherwise $HOME/.als is used.

   GPR_Path : constant VSS.Strings.Virtual_String :=
     VSS.Application.System_Environment.Value ("GPR_PROJECT_PATH");

   Path : constant VSS.Strings.Virtual_String :=
     VSS.Application.System_Environment.Value ("PATH");

   ALS_Log_Dir : constant GNATCOLL.VFS.Virtual_File :=
     ((if ALS_Home.Is_Empty then Home_Dir
       else LSP.Utils.To_Virtual_File (ALS_Home))
      / ".als");
   --  The location where ALS produces logs. If ALS_HOME is specified,
   --  $ALS_HOME/.als is used, otherwise $HOME/.als is used.

   XDG_CONFIG_HOME : constant VSS.Strings.Virtual_String :=
     VSS.Application.System_Environment.Value
       ("XDG_CONFIG_HOME", LSP.Utils.To_Virtual_String (Home_Dir / ".config"));
   --  The XDG_CONFIG_HOME environment variable, defaulting to $HOME/.config if
   --  unspecified.
   --
   --  See XDG Base Directory Specification at
   --  https://specifications.freedesktop.org/basedir-spec/latest

   ALS_User_Config_Dir : constant GNATCOLL.VFS.Virtual_File :=
     LSP.Utils.To_Virtual_File (XDG_CONFIG_HOME) / "als";

   ALS_User_Config_File : constant GNATCOLL.VFS.Virtual_File :=
     ALS_User_Config_Dir / "config.json";

   ALS_Workspace_Config_File : constant GNATCOLL.VFS.Virtual_File :=
     GNATCOLL.VFS.Create_From_Base (".als.json");

end LSP.Env;
