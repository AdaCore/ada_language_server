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

with GNATCOLL.Traces;
with GNATCOLL.VFS;

with Libadalang.Analysis;
with Libadalang.Common;

with Laltools.Common;
with GPR2.Build.Source.Sets;

with LSP.Enumerations;
with VSS.String_Vectors;
with VSS.Strings;

with LSP.Ada_Client_Capabilities;
with LSP.Ada_Configurations;
with LSP.Ada_Context_Sets;
with LSP.Ada_Contexts;
with LSP.Ada_Documents;
with LSP.Ada_Highlighters;
with LSP.Constants;
with LSP.Locations;
with LSP.Structures;

package LSP.Ada_Job_Contexts is

   type Ada_Job_Context is limited interface;

   function To_File
     (Self : Ada_Job_Context;
      URI  : LSP.Structures.DocumentUri)
        return GNATCOLL.VFS.Virtual_File is abstract;

   function Client (Self : Ada_Job_Context) return
     access constant LSP.Ada_Client_Capabilities.Client_Capability'Class
       is abstract;

   function Get_Base_Configuration (Self : Ada_Job_Context)
     return access constant LSP.Ada_Configurations.Configuration'Class
       is abstract;
   --  The base configuration loaded at process startup from configuration
   --  files.

   function Get_Configuration (Self : Ada_Job_Context)
     return access constant LSP.Ada_Configurations.Configuration'Class
       is abstract;
   --  The current applicable configuration.

   procedure Set_Configuration
      (Self  : in out Ada_Job_Context;
       Value : LSP.Ada_Configurations.Configuration'Class) is abstract;

   procedure Increment_Project_Timestamp (Self : in out Ada_Job_Context)
     is abstract;

   procedure Send_Messages
     (Self     : Ada_Job_Context;
      Show     : Boolean;
      Messages : VSS.String_Vectors.Virtual_String_Vector;
      Severity : LSP.Enumerations.MessageType;
      File     : GNATCOLL.VFS.Virtual_File) is abstract;
   --  Send Messages of Severity to the client using LogMessages.
   --  If Show is True then also send showMessages.

   function Project_Tree_Is_Defined
     (Self : Ada_Job_Context) return Boolean is abstract;

   function Project_Tree_Is_Aggregate
     (Self : Ada_Job_Context) return Boolean is abstract;

   function Get_Runtime_Sources
     (Self : Ada_Job_Context) return GPR2.Build.Source.Sets.Object is abstract;

   procedure Reload_Project (Self : in out Ada_Job_Context) is abstract;

   procedure Refresh_Diagnostics (Self : in out Ada_Job_Context) is abstract;

   function Get_Open_Document
     (Self : in out Ada_Job_Context;
      URI  : LSP.Structures.DocumentUri)
        return LSP.Ada_Documents.Document_Access is abstract;

   function Get_Highlighter
     (Self : in out Ada_Job_Context)
      return access constant LSP.Ada_Highlighters.Ada_Highlighter is abstract;

   procedure Publish_Diagnostics
     (Self              : in out Ada_Job_Context;
      Document          : not null LSP.Ada_Documents.Document_Access;
      Other_Diagnostics : LSP.Structures.Diagnostic_Vector :=
        LSP.Structures.Empty;
      Force             : Boolean := False) is abstract;
   --  Publish document diagnostics

   function Contexts_For_File
     (Self : Ada_Job_Context;
      File : GNATCOLL.VFS.Virtual_File)
      return LSP.Ada_Context_Sets.Context_Lists.List is abstract;
   --  Return the list of contexts containing File as a source

   function Get_Best_Context
     (Self : Ada_Job_Context;
      URI  : LSP.Structures.DocumentUri)
      return LSP.Ada_Context_Sets.Context_Access is abstract;
   --  Return the first context containing URI as a source or the first
   --  context is the list if no such context exist

   function Contexts_For_Position
     (Self : in out Ada_Job_Context;
      Pos  : LSP.Structures.TextDocumentPositionParams'Class)
      return LSP.Ada_Context_Sets.Context_Lists.List is abstract;
   --  Return all the contexts recognizing and sharing the symbol at Pos

   function Get_Node_At
     (Self     : in out Ada_Job_Context;
      Context  : LSP.Ada_Contexts.Context;
      Value    : LSP.Structures.TextDocumentPositionParams'Class)
      return Libadalang.Analysis.Ada_Node is abstract;

   function Imprecise_Resolve_Name
     (Self      : in out Ada_Job_Context;
      Name_Node : Libadalang.Analysis.Name)
      return Libadalang.Analysis.Defining_Name is abstract;
   --  Return the definition node (canonical part) of the given name.
   --  If an error happened then return No_Defining_Name.

   function Imprecise_Resolve_Name
     (Self     : in out Ada_Job_Context'Class;
      Context  : LSP.Ada_Contexts.Context;
      Position : LSP.Structures.TextDocumentPositionParams'Class)
        return Libadalang.Analysis.Defining_Name is
          (Self.Imprecise_Resolve_Name
            (Laltools.Common.Get_Node_As_Name
               (Self.Get_Node_At (Context, Position))));

   function To_LSP_Location
     (Self   : in out Ada_Job_Context;
      Node   : Libadalang.Analysis.Ada_Node'Class)
      return LSP.Structures.Location is abstract;

   function To_LSP_Range
     (Self   : in out Ada_Job_Context;
      Node   : Libadalang.Analysis.Ada_Node'Class)
      return LSP.Structures.A_Range is abstract;

   function To_LSP_Range
     (Self  : in out Ada_Job_Context;
      Unit  : Libadalang.Analysis.Analysis_Unit;
      Token : Libadalang.Common.Token_Reference)
      return LSP.Structures.A_Range is abstract;

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

   function Get_Trace_Handle (Self : Ada_Job_Context)
     return GNATCOLL.Traces.Trace_Handle is abstract;

end LSP.Ada_Job_Contexts;
