------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                        Copyright (C) 2020, AdaCore                       --
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

with GNAT.Strings;

with GNATCOLL.VFS;

package body LSP.Ada_Project_Environments is

   ------------------
   -- Spawn_Gnatls --
   ------------------

   overriding procedure Spawn_Gnatls
     (Self         : LSP_Project_Environment;
      Fd           : out GNAT.Expect.Process_Descriptor_Access;
      Gnatls_Args  : GNAT.OS_Lib.Argument_List_Access;
      Errors       : GNATCOLL.Projects.Error_Report)
   is
      use type GNATCOLL.Projects.Error_Report;
      use type GNATCOLL.VFS.Filesystem_String;
      use type GNATCOLL.VFS.Virtual_File;

      Gnatls      : constant String := Gnatls_Args (Gnatls_Args'First).all;
      Gnatls_Path : GNATCOLL.VFS.Virtual_File :=
        GNATCOLL.VFS.Locate_On_Path (+Gnatls);

   begin
      if Gnatls_Path = GNATCOLL.VFS.No_File and Gnatls = "gnatls" then
         --  Try to fallback to "codepeer-gnatls" when target is not defined
         --  and native toolchain is not available.

         GNAT.Strings.Free (Gnatls_Args (Gnatls_Args'First));
         Gnatls_Args (Gnatls_Args'First) := new String'("codepeer-gnatls");

         Gnatls_Path :=
           GNATCOLL.VFS.Locate_On_Path (+Gnatls_Args (Gnatls_Args'First).all);

         if Gnatls_Path = GNATCOLL.VFS.No_File then
            --  "codepeer-gnatls" is not available too, rerport error for
            --  initially passed "gnatls"

            if Errors /= null then
               Errors ("Could not locate exec " & Gnatls);
            end if;

            return;
         end if;
      end if;

      GNATCOLL.Projects.Project_Environment (Self).Spawn_Gnatls
        (Fd, Gnatls_Args, Errors);
   end Spawn_Gnatls;

end LSP.Ada_Project_Environments;
