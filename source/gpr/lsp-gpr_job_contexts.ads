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

with Ada.Exceptions;

with VSS.Strings;

with LSP.Ada_Configurations;
with LSP.GPR_Documents;
with LSP.GPR_Files;
with LSP.Structures;

package LSP.GPR_Job_Contexts is

   type GPR_Job_Context is limited interface
     and LSP.GPR_Files.File_Provider;

   function Get_Open_Document
     (Self : access GPR_Job_Context;
      URI  : LSP.Structures.DocumentUri)
        return LSP.GPR_Documents.Document_Access is abstract;

   procedure Publish_Diagnostics
     (Self     : in out GPR_Job_Context;
      Document : not null LSP.GPR_Documents.Document_Access) is abstract;

   procedure Trace_Exception
     (Self    : GPR_Job_Context;
      Error   : Ada.Exceptions.Exception_Occurrence;
      Message : VSS.Strings.Virtual_String :=
        VSS.Strings.Empty_Virtual_String) is abstract;

   function Get_Configuration
     (Self : GPR_Job_Context)
      return LSP.Ada_Configurations.Configuration is abstract;

   procedure Set_Configuration
     (Self  : in out GPR_Job_Context;
      Value : LSP.Ada_Configurations.Configuration) is abstract;

end LSP.GPR_Job_Contexts;
