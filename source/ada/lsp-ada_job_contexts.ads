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

with GNATCOLL.VFS;

with Libadalang.Analysis;

with VSS.Strings;

with LSP.Ada_Configurations;
with LSP.Ada_Context_Sets;
with LSP.Ada_Contexts;
with LSP.Ada_Documents;
with LSP.Constants;
with LSP.Locations;
with LSP.Structures;

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

   function Imprecise_Resolve_Name
     (Self     : in out Ada_Job_Context;
      Context  : LSP.Ada_Contexts.Context;
      Position : LSP.Structures.TextDocumentPositionParams'Class)
        return Libadalang.Analysis.Defining_Name is abstract;

   procedure Append_Location
     (Self   : in out Ada_Job_Context;
      Result : in out LSP.Structures.Location_Vector;
      Filter : in out LSP.Locations.File_Span_Sets.Set;
      Node   : Libadalang.Analysis.Ada_Node'Class;
      Kinds  : LSP.Structures.AlsReferenceKind_Set := LSP.Constants.Empty)
        is abstract;
   --  Append given Node location to the Result.
   --  Do nothing if the item inside of an synthetic file (like __standard).

   procedure Trace_Exception
     (Self    : Ada_Job_Context;
      Error   : Ada.Exceptions.Exception_Occurrence;
      Message : VSS.Strings.Virtual_String :=
        VSS.Strings.Empty_Virtual_String) is abstract;

end LSP.Ada_Job_Contexts;
