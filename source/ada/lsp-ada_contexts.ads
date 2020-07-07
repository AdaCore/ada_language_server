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

with Ada.Containers.Ordered_Sets;

with Ada.Strings.Unbounded;

with GNATCOLL.Projects;
with GNATCOLL.Traces;
with GNATCOLL.VFS;

with Libadalang.Analysis;

with Utils.Command_Lines;
with Pp.Command_Lines;

with LSP.Common; use LSP.Common;
with LSP.Messages;
with LSP.Ada_Documents;
with LSP.Types;

package LSP.Ada_Contexts is

   type Context (Trace : GNATCOLL.Traces.Trace_Handle) is
     tagged limited private;
   --  A context contains a non-aggregate project tree and its associated
   --  libadalang context.

   package File_Sets is new Ada.Containers.Ordered_Sets
     (Element_Type        => GNATCOLL.VFS.Virtual_File,
      "<"                 => GNATCOLL.VFS."<",
      "="                 => GNATCOLL.VFS."=");

   procedure Initialize (Self : in out Context);
   --  Initialize the context

   procedure Load_Project
     (Self     : in out Context;
      Tree     : not null GNATCOLL.Projects.Project_Tree_Access;
      Root     : GNATCOLL.Projects.Project_Type;
      Charset  : String);
   --  Use the given project tree, and root project within this project
   --  tree, as project for this context. Root must be a non-aggregate
   --  project tree representing the root of a hierarchy inside Tree.

   function Id (Self : Context) return LSP.Types.LSP_String;
   --  Return unique identifier of the context.

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

   procedure Format
     (Self     : in out Context;
      Document : LSP.Ada_Documents.Document_Access;
      Span     : LSP.Messages.Span;
      Options  : LSP.Messages.FormattingOptions;
      Edit     : out LSP.Messages.TextEdit_Vector;
      Success  : out Boolean);

   function Find_All_References
     (Self              : Context;
      Definition        : Libadalang.Analysis.Defining_Name;
      Imprecise_Results : out Boolean)
      return Base_Id_Array;
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
   --  Returns an empty array if Decl is null.

   function Find_All_Base_Declarations
     (Self              : Context;
      Decl              : Libadalang.Analysis.Basic_Decl;
      Imprecise_Results : out Boolean)
      return Libadalang.Analysis.Basic_Decl_Array;
   --  Given a subprogram declaration in Decl, find all the base subprograms
   --  that it inherits, not including self.
   --  Imprecise_Results is set to True if we don't know whether the results
   --  are precise.
   --  Returns an empty array if Decl is null.

   function Find_All_Calls
     (Self              : Context;
      Definition        : Libadalang.Analysis.Defining_Name;
      Imprecise_Results : out Boolean)
      return Base_Id_Array;
   --  Return all the enclosing entities that call Definition in all sources
   --  known to this project.

   function Get_References_For_Renaming
     (Self              : Context;
      Definition        : Libadalang.Analysis.Defining_Name;
      Imprecise_Results : out Boolean)
      return Base_Id_Array;
   --  Get all the references to a given defining name in all units for
   --  renaming purposes: for instance, when called on a tagged type primitive
   --  definition, references to the base subprograms it inherits and to the
   --  overriding ones are also returned.

   function Is_Part_Of_Project
     (Self : Context;
      File : GNATCOLL.VFS.Virtual_File) return Boolean;
   --  Check if given file belongs to the project loaded in the Context

   function List_Files (Self : Context) return File_Sets.Set;
   --  Return the list of files known to this context.

   function Analysis_Units
     (Self : Context) return Libadalang.Analysis.Analysis_Unit_Array;
   --  Return the analysis units for all Ada sources known to this context

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
      Result    : in out LSP.Messages.Location_Or_Link_Vector;
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

   type Project_Status is
     (User_Provided_Project,  --  Server uses user provides project
      Default_Project,        --  No project provided or found, use default
      Found_Unique_Project);  --  No project provided, but server found one

   type Context (Trace : GNATCOLL.Traces.Trace_Handle) is tagged limited record
      Id             : LSP.Types.LSP_String;
      Unit_Provider  : Libadalang.Analysis.Unit_Provider_Reference;
      LAL_Context    : Libadalang.Analysis.Analysis_Context;
      Charset        : Ada.Strings.Unbounded.Unbounded_String;

      Source_Files   : File_Sets.Set;
      --  Cache for the list of Ada source files in the loaded project tree.

      PP_Options : Utils.Command_Lines.Command_Line
                    (Pp.Command_Lines.Descriptor'Access);
      --  Object to keep gnatpp options
   end record;

   function LAL_Context
     (Self : Context) return Libadalang.Analysis.Analysis_Context is
     (Self.LAL_Context);

   function List_Files (Self : Context) return File_Sets.Set
   is (Self.Source_Files);

end LSP.Ada_Contexts;
