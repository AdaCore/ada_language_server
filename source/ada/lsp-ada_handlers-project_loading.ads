------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2018-2023, AdaCore                     --
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

with VSS.Strings;
with GPR2.Environment;

with LSP.Ada_Configurations;

private

package LSP.Ada_Handlers.Project_Loading is

   procedure Load_Project
     (Self         : in out Message_Handler'Class;
      Project_Path : VSS.Strings.Virtual_String;
      Scenario     : LSP.Ada_Configurations.Variable_List;
      Environment  : GPR2.Environment.Object;
      Charset      : VSS.Strings.Virtual_String;
      Status       : Load_Project_Status);
   --  Attempt to load the given project file, with the scenario provided.
   --  This unloads all currently loaded project contexts. This factorizes code
   --  between Load_Project_With_Alire and Ensure_Project_Loaded.

   procedure Ensure_Project_Loaded (Self : in out Message_Handler'Class);

   procedure Reload_Project (Self : in out Message_Handler'CLass);

   procedure Release_Contexts_And_Project_Info
     (Self : in out Message_Handler'Class);
   --  Release the memory associated to project information in Self

   procedure Reload_Implicit_Project_Dirs
     (Self : in out Message_Handler'Class);
   --  Reload as project source dirs the directories in
   --  Self.Project_Dirs_Loaded.

end LSP.Ada_Handlers.Project_Loading;
