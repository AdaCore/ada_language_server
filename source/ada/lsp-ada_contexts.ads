------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2018-2019, AdaCore                     --
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
--  This package provides a context of Ada Language server.

with Ada.Containers.Hashed_Sets;
with Ada.Strings.Unbounded;

with GNATCOLL.Projects;
with GNATCOLL.Traces;
with GNATCOLL.VFS;

with Libadalang.Analysis;
with LSP.Messages;

with LSP.Ada_Documents;
with LSP.Types;

package LSP.Ada_Contexts is

   type Context (Trace : GNATCOLL.Traces.Trace_Handle) is
     tagged limited private;
   --  A context contains a non-aggregate project tree and its associated
   --  libadalang context.

   procedure Initialize (Self : in out Context);
   --  Initialize the context

   function Has_Project (Self : Context) return Boolean;
   --  Check if context has a project

   procedure Load_Project
     (Self     : in out Context;
      Tree     : not null GNATCOLL.Projects.Project_Tree_Access;
      Root     : GNATCOLL.Projects.Project_Type;
      Charset  : String);
   --  Use the given project tree, and root project within this project
   --  tree, as project for this context. Root must be a non-aggregate
   --  projec tree representing the root of a hierarchy inside Tree.

   procedure Reload (Self : in out Context);
   --  Reload the current context. This will invalidate and destroy any
   --  Libadalang related data, and recreate it from scratch.

   procedure Free (Self : in out Context);
   --  Release the memory associated to Self. You should not use the
   --  context after calling this.

   function URI_To_File
     (URI  : LSP.Types.LSP_String) return LSP.Types.LSP_String;
   --  Turn URI into path by stripping schema from it

   function File_To_URI
     (File  : LSP.Types.LSP_String) return LSP.Types.LSP_String;
   --  Convert file name to URI

   function Get_Node_At
     (Self     : Context;
      Document : LSP.Ada_Documents.Document_Access;
      Position : LSP.Messages.TextDocumentPositionParams'Class)
      return Libadalang.Analysis.Ada_Node;
   --  Return the node at the given location.
   --  If Document is not null, get the location from the document, otherwise
   --  get it from the file.

   function Find_All_References
     (Self              : Context;
      Definition        : Libadalang.Analysis.Defining_Name;
      Imprecise_Results : out Boolean)
      return Libadalang.Analysis.Base_Id_Array;
   --  Finds all references to a given defining name in all units of the
   --  context.
   --  Imprecise_Results is set to True if we don't know whether the results
   --  are precise.

   function Find_All_Overrides
     (Self              : Context;
      Decl              : Libadalang.Analysis.Basic_Decl;
      Imprecise_Results : out Boolean)
      return Libadalang.Analysis.Basic_Decl_Array;
   --  Finds all overriding subprograms of the given basic declaration.
   --  This is used to propose all the implementations of a given subprogram
   --  when textDocument/definition requests happen on dispatching calls.
   --  Imprecise_Results is set to True if we don't know whether the results
   --  are precise.

   function Find_All_Calls
     (Self              : Context;
      Definition        : Libadalang.Analysis.Defining_Name;
      Imprecise_Results : out Boolean)
      return Libadalang.Analysis.Base_Id_Array;
   --  Return all the enclosing entities that call Definition in all sources
   --  known to this project.

   function Is_Part_Of_Project
     (Self : Context;
      File : GNATCOLL.VFS.Virtual_File) return Boolean;
   --  Check if given file belongs to the project loaded in the Context

   function List_Files (Self : Context) return GNATCOLL.VFS.File_Array_Access;
   --  Return the list of files known to this context. The result is
   --  guaranteed not to be null. Caller must not free the result.

   procedure Index_File (Self : Context; File : GNATCOLL.VFS.Virtual_File);
   --  Index the given file. This translates to refreshing the Libadalang
   --  Analysis_Unit associated to it.

   procedure Index_Document
     (Self : Context; Document : LSP.Ada_Documents.Document);
   --  Index/reindex the given document in this context

   procedure Append_Declarations
     (Self      : Context;
      Document  : LSP.Ada_Documents.Document_Access;
      Position  : LSP.Messages.TextDocumentPositionParams;
      Result    : in out LSP.Messages.Location_Vector;
      Imprecise : in out Boolean);
   --  Find corresponding declarations for a name at given Position and append
   --  their locations to Result.
   --  Document is the document from which the request originates; it can
   --  be null if no document is open for this location.
   --
   --  Here we follow C terminology, where 'Declaration' equals to
   --  Ada subprogram specification while 'Definition' equals to
   --  Ada subprogram body (completion).

   function LAL_Context
     (Self : Context) return Libadalang.Analysis.Analysis_Context;
   --  Return the LAL context corresponding to Self

private

   use type GNATCOLL.Projects.Project_Tree_Access;

   type Project_Status is
     (User_Provided_Project,  --  Server uses user provides project
      Default_Project,        --  No project provided or found, use default
      Found_Unique_Project);  --  No project provided, but server found one

   package File_Sets is new Ada.Containers.Hashed_Sets
     (Element_Type        => GNATCOLL.VFS.Virtual_File,
      Hash                => GNATCOLL.VFS.Full_Name_Hash,
      Equivalent_Elements => GNATCOLL.VFS."=",
      "="                 => GNATCOLL.VFS."=");

   type Context (Trace : GNATCOLL.Traces.Trace_Handle) is tagged limited record
      Unit_Provider  : Libadalang.Analysis.Unit_Provider_Reference;
      LAL_Context    : Libadalang.Analysis.Analysis_Context;
      Charset        : Ada.Strings.Unbounded.Unbounded_String;

      Source_Files   : GNATCOLL.VFS.File_Array_Access :=
        new GNATCOLL.VFS.File_Array'(1 .. 0 => <>);
      --  Cache for the list of Ada source files in the loaded project tree.
      --  This should never be null.

      Project_Tree   : GNATCOLL.Projects.Project_Tree_Access;
      --  The currently loaded project tree, or null for contexts that
      --  don't have a project.
   end record;

   procedure Update_Source_Files (Self : in out Context);
   --  Update Self.Source_Files value

   function Has_Project (Self : Context) return Boolean
   is
     (Self.Project_Tree /= null);

   function LAL_Context
     (Self : Context) return Libadalang.Analysis.Analysis_Context is
     (Self.LAL_Context);

end LSP.Ada_Contexts;
