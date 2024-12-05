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

package body LSP.Env is

   --------------
   -- Home_Dir --
   --------------

   function Home_Dir return GNATCOLL.VFS.Virtual_File is
   begin
      if Home.Is_Empty then
         raise Program_Error
           with
             "The HOME environment variable is empty, ALS cannot proceed safely.";
      end if;

      return LSP.Utils.To_Virtual_File (Home);
   end Home_Dir;

   -----------------
   -- ALS_Log_Dir --
   -----------------

   function ALS_Log_Dir return GNATCOLL.VFS.Virtual_File
   is ((if not ALS_Home.Is_Empty then LSP.Utils.To_Virtual_File (ALS_Home)
        else
          (if Home_Dir.Is_Directory then Home_Dir
           else GNATCOLL.VFS.Create (".")))
       / ".als");

   ---------------------
   -- XDG_CONFIG_HOME --
   ---------------------

   function XDG_CONFIG_HOME return VSS.Strings.Virtual_String
   is (VSS.Application.System_Environment.Value
         ("XDG_CONFIG_HOME",
          LSP.Utils.To_Virtual_String (Home_Dir / ".config")));

   -------------------------
   -- ALS_User_Config_Dir --
   -------------------------

   function ALS_User_Config_Dir return GNATCOLL.VFS.Virtual_File
   is (LSP.Utils.To_Virtual_File (XDG_CONFIG_HOME) / "als");

   --------------------------
   -- ALS_User_Config_File --
   --------------------------

   function ALS_User_Config_File return GNATCOLL.VFS.Virtual_File
   is (ALS_User_Config_Dir / "config.json");

   -------------------------------
   -- ALS_Workspace_Config_File --
   -------------------------------

   function ALS_Workspace_Config_File return GNATCOLL.VFS.Virtual_File
   is (GNATCOLL.VFS.Create_From_Base (".als.json"));

end LSP.Env;
