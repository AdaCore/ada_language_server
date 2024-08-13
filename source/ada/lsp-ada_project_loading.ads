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
with GPR2.Log;
with LSP.Structures;

package LSP.Ada_Project_Loading is

   type Project_Status_Type is private;
   --  Description of the project after trying to load it.

   No_Project_Status : constant Project_Status_Type;

   type Project_Types is
     (Not_Set,
      Configured_Project,
      Single_Project_Found,
      Alire_Project,
      Implicit_Project);
   --  Variants for type of project loaded into the handler:
   --
   --  @value Not_Set: initial value when starting the server
   --
   --  @value Configured_Project: project provided by ada.projectFile
   --
   --  @value Single_Project_Found: no project in ada.projectFile, but
   --  just one project in Root dir
   --
   --  @value Alire_Project: toml file found so use Alire project, it has
   --  priority over the configured project.
   --
   --  @value Implicit_Project: use the implicit project as a last chance

   type Project_Status is
     (Valid_Project,
      Warning_In_Project,
      No_Project,
      Multiple_Projects,
      Invalid_Project,
      Project_Not_Found);
   --  Variants for state of the project loaded into the handler:
   --
   --  @value Valid_Project: the project is valid
   --
   --  @value Warning_In_Project: warnings when loading the project
   --
   --  @value No_Project: no project found in Root dir
   --
   --  @value Multiple_Projects_Found: no project in ada.projectFile and
   --  several projects in Root dir
   --
   --  @value Invalid_Project: errors when loading the project
   --
   --  @value Project_Not_Found: the configured project was not found.

   procedure Set_Load_Status
     (Project : in out Project_Status_Type;
      Status  : Project_Status);
   --  Set the status of the project

   procedure Set_Project_Type
     (Project      : in out Project_Status_Type;
      Project_Type : Project_Types);
   --  Set the type of the project.

   procedure Set_Has_Runtime
     (Project      : in out Project_Status_Type;
      Has_Runtime  : Boolean);
   --  Should be called when the runtime for Project is found

   procedure Set_GPR2_Messages
     (Project       : in out Project_Status_Type;
      GPR2_Messages : GPR2.Log.Object);
   --  Set the messages related to GPR2 project loading

   procedure Set_Project_File
     (Project      : in out Project_Status_Type;
      Project_File : GNATCOLL.VFS.Virtual_File);
   --  Set the file we are trying to load

   function Is_Implicit_Fallback
     (Project : Project_Status_Type) return Boolean;
   --  Return True if the implicit project has been loaded

   function Is_Project_Loaded
     (Project : Project_Status_Type) return Boolean;
   --  Return True if the project was loaded

   function Has_New_Diagnostics
     (Old_Project : Project_Status_Type;
      New_Project : Project_Status_Type)
      return Boolean;
   --  Return True when the New_Project has a different status or different
   --  messages compare to Old_Project

   procedure Project_Status_Code_Actions
     (Result      : in out LSP.Structures.Command_Or_CodeAction_Vector;
      Project     : Project_Status_Type;
      Diagnostics : LSP.Structures.Diagnostic_Vector;
      Default_URI : LSP.Structures.DocumentUri);
   --  Add code actions related to Project in Result

   function Get_Diagnostics
     (Project : Project_Status_Type)
      return LSP.Structures.Diagnostic_Vector;
   --  Compute and return the diagnostics of the project

private

   type Project_Status_Type is record
      Project_Type      : Project_Types := Not_Set;
      --  The type of loaded project

      Status            : Project_Status := Valid_Project;
      --  Status of the loaded project

      Project_File      : GNATCOLL.VFS.Virtual_File := GNATCOLL.VFS.No_File;
      --  The project file we have attempted to load, successfully or not.

      Has_Runtime       : Boolean := False;
      --  Were we able to find the runtime for the project

      GPR2_Messages     : GPR2.Log.Object := GPR2.Log.Undefined;
      --  The warning/error messages emitted by GPR2 while loading the project.
   end record;

   No_Project_Status : constant Project_Status_Type :=
     Project_Status_Type'
       (Project_Type  => Not_Set,
        Status        => Valid_Project,
        Project_File  => GNATCOLL.VFS.No_File,
        Has_Runtime   => False,
        GPR2_Messages => <>);

end LSP.Ada_Project_Loading;
