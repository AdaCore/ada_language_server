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
with Ada.Containers.Hashed_Maps;
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
   --  Context includes set of edited documents, Libadalang context, project
   --  tree and others data.

   procedure Initialize (Self : in out Context);
   --  Initialize the context

   function Has_Project (Self : Context) return Boolean;
   --  Check if context has a project

   procedure Load_Project
     (Self     : in out Context;
      Tree     : not null GNATCOLL.Projects.Project_Tree_Access;
      Root     : not null GNATCOLL.Projects.Project_Type_Access;
      Charset  : String);
   --  Use the given project tree, and root project within this project
   --  tree, as project for this context. Root must be a non-aggregate
   --  projec tree representing the root of a hierarchy inside Tree.

   procedure Reload (Self : in out Context);
   --  Reload the current context. This will invalidate and destroy any
   --  Libadalang related data, and recreate it from scratch.

   procedure Unload (Self : in out Context);
   --  Release the memory associated to this context. Do not use the
   --  context after this.

   function Load_Document
     (Self : aliased in out Context;
      Item : LSP.Messages.TextDocumentItem)
      return LSP.Ada_Documents.Document_Access;
   --  Load new document with text provided by LSP client. Document is managed
   --  by Context, user shouldn't free it or store it. Document will be
   --  destroyed on Unload_Document call.

   procedure Unload_Document
     (Self : in out Context;
      Item : LSP.Messages.TextDocumentIdentifier);
   --  Remove document from set of openned documents and destroy it.

   function Has_Document
     (Self : Context;
      URI  : LSP.Messages.DocumentUri) return Boolean;
   --  Return whether there is a document being managed by this context

   function Get_Document
     (Self : Context;
      URI  : LSP.Messages.DocumentUri)
      return LSP.Ada_Documents.Document_Access
     with Pre => Has_Document (Self, URI);
   --  Retrive document identified by given URI. User shouldn't free it or
   --  store it.

   function URI_To_File
     (URI  : LSP.Types.LSP_String) return LSP.Types.LSP_String;
   --  Turn URI into path by stripping schema from it

   function File_To_URI
     (File  : LSP.Types.LSP_String) return LSP.Types.LSP_String;
   --  Convert file name to URI

   function Get_Node_At
     (Self     : Context;
      Position : LSP.Messages.TextDocumentPositionParams'Class)
      return Libadalang.Analysis.Ada_Node;
   --  Return the node at the given location. This is like LSP.Ada_Documents,
   --  but it works even when given document hasn't opened yet.

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

private
   use type Types.LSP_String;
   use type GNATCOLL.Projects.Project_Tree_Access;

   type Internal_Document_Access is access all LSP.Ada_Documents.Document;

   package Document_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => LSP.Messages.DocumentUri,
      Element_Type    => Internal_Document_Access,
      Hash            => LSP.Types.Hash,
      Equivalent_Keys => LSP.Types."=");

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

      Documents      : Document_Maps.Map;
   end record;

   procedure Update_Source_Files (Self : in out Context);
   --  Update Self.Source_Files value

   function Has_Project (Self : Context) return Boolean
   is
     (Self.Project_Tree /= null);

end LSP.Ada_Contexts;
