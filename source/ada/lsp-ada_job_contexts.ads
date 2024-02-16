------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                        Copyright (C) 2024, AdaCore                       --
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
--
--  This package provides interface for data shared between jobs. This data
--  includes open documents, non-aggregate project trees, settings, etc

with GNATCOLL.VFS;

with LSP.Ada_Configurations;
with LSP.Ada_Documents;
with LSP.Structures;
with LSP.Ada_Context_Sets;

package LSP.Ada_Job_Contexts is

   type Ada_Job_Context is limited interface;

   function To_File
     (Self : Ada_Job_Context;
      URI  : LSP.Structures.DocumentUri)
        return GNATCOLL.VFS.Virtual_File is abstract;

   function Get_Configuration (Self : Ada_Job_Context)
     return access constant LSP.Ada_Configurations.Configuration'Class
       is abstract;

   procedure Set_Configuration
      (Self  : in out Ada_Job_Context;
       Value : LSP.Ada_Configurations.Configuration'Class) is abstract;

   procedure Increment_Project_Timestamp (Self : in out Ada_Job_Context)
     is abstract;

   function Project_Tree_Is_Defined
     (Self : Ada_Job_Context) return Boolean is abstract;

   procedure Reload_Project (Self : in out Ada_Job_Context) is abstract;

   function Get_Open_Document
     (Self : in out Ada_Job_Context;
      URI  : LSP.Structures.DocumentUri)
        return LSP.Ada_Documents.Document_Access is abstract;

   procedure Publish_Diagnostics
     (Self              : in out Ada_Job_Context;
      Document          : not null LSP.Ada_Documents.Document_Access;
      Other_Diagnostics : LSP.Structures.Diagnostic_Vector :=
        LSP.Structures.Empty;
      Force             : Boolean := False) is abstract;

   function Contexts_For_File
     (Self : Ada_Job_Context;
      File : GNATCOLL.VFS.Virtual_File)
      return LSP.Ada_Context_Sets.Context_Lists.List is abstract;

end LSP.Ada_Job_Contexts;
