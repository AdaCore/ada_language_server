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

with LSP.Utils;
with VSS.Standard_Paths;

package body LSP.Env is

   --------------
   -- Home_Dir --
   --------------

   function Home_Dir return GNATCOLL.VFS.Virtual_File is
      Home_Path : constant VSS.Strings.Virtual_String :=
        VSS.Standard_Paths.Writable_Location
          (VSS.Standard_Paths.Home_Location);
   begin
      if Home_Path.Is_Empty then
         raise Program_Error
           with "Unable to determine the user home location, ALS cannot proceed safely.";
      else
         declare
            Result : constant GNATCOLL.VFS.Virtual_File :=
              LSP.Utils.To_Virtual_File (Home_Path);
         begin
            if Result.Is_Directory then
               return Result;
            else
               raise Program_Error
                 with "The user home location is not a directory: " &
                 Result.Display_Full_Name;
            end if;
         end;
      end if;

   end Home_Dir;

   -----------------
   -- ALS_Log_Dir --
   -----------------

   function ALS_Log_Dir return GNATCOLL.VFS.Virtual_File is
     ((if not ALS_Home.Is_Empty then LSP.Utils.To_Virtual_File (ALS_Home)
       else
         (if Home_Dir.Is_Directory then Home_Dir
          else GNATCOLL.VFS.Create ("."))) /
      ".als");

   ---------------------
   -- XDG_CONFIG_HOME --
   ---------------------

   function XDG_CONFIG_HOME return GNATCOLL.VFS.Virtual_File is
      Xdg_Config_Home_Value : constant VSS.Strings.Virtual_String :=
        VSS.Application.System_Environment.Value ("XDG_CONFIG_HOME");
   begin
      if not Xdg_Config_Home_Value.Is_Empty then
         return LSP.Utils.To_Virtual_File (Xdg_Config_Home_Value);
      else
         return Home_Dir / ".config";
      end if;
   end XDG_CONFIG_HOME;

   -------------------------
   -- ALS_User_Config_Dir --
   -------------------------

   function ALS_User_Config_Dir return GNATCOLL.VFS.Virtual_File is
     (XDG_CONFIG_HOME / "als");

   --------------------------
   -- ALS_User_Config_File --
   --------------------------

   function ALS_User_Config_File return GNATCOLL.VFS.Virtual_File is
     (ALS_User_Config_Dir / "config.json");

   -------------------------------
   -- ALS_Workspace_Config_File --
   -------------------------------

   function ALS_Workspace_Config_File return GNATCOLL.VFS.Virtual_File is
     (GNATCOLL.VFS.Create_From_Base (".als.json"));

end LSP.Env;
