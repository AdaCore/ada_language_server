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

with GNATCOLL.VFS;
with LSP.Ada_Project_Loading;
with LSP.Diagnostic_Sources;

package LSP.Ada_Handlers.Project_Diagnostics is

   Project_Diagnostics_Source_ID : constant VSS.Strings.Virtual_String :=
     "ada.project";
   type Diagnostic_Source
     (Handler : not null access LSP.Ada_Handlers.Message_Handler'Class)
   is limited new LSP.Diagnostic_Sources.Workspace_Diagnostic_Source with private;

   overriding procedure Get_Diagnostics
     (Self          : in out Diagnostic_Source;
      Diagnostics   : out LSP.Structures.Diagnostic_Vector;
      Target_File   : out GNATCOLL.VFS.Virtual_File);
   --  Fill diagnostics for given document.
   --  Target_File is the file where diagnostics should be published
   --  (e.g: project file, workspace's root directory).

   overriding function Has_New_Diagnostic
     (Self    : in out Diagnostic_Source) return Boolean;

private

   type Diagnostic_Source
     (Handler : not null access LSP.Ada_Handlers.Message_Handler'Class)
   is limited new LSP.Diagnostic_Sources.Workspace_Diagnostic_Source with record
      Last_Status : LSP.Ada_Project_Loading.Project_Status_Type;
   end record;

end LSP.Ada_Handlers.Project_Diagnostics;
