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

private with GNATCOLL.Projects;
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

   procedure Initialize
     (Self  : in out Context;
      Root  : LSP.Types.LSP_String);
   --  Reset context. Set Root directory as LSP client provides it.
   --  Use Trace to log errors.

   function Is_Initialized (Self : Context) return Boolean;
   --  Check if context has been initialized

   function Has_Project (Self : Context) return Boolean;
   --  Check if context has a project

   procedure Load_Project
     (Self     : in out Context;
      File     : LSP.Types.LSP_String;
      Scenario : LSP.Types.LSP_Any;
      Charset  : String;
      Errors   : out LSP.Messages.ShowMessageParams);
   --  Load given project File using given Scenario variables.
   --  In case of errors create and load default project.
   --  Set the charset as well.
   --  Return warnings and errors in Errors parameter.

   procedure Set_Diagnostics_Enabled
     (Self    : in out Context;
      Enabled : Boolean);
   --  Enable/Disable diagnostics; by default, diagnostics are enabled

   function Get_Diagnostics_Enabled (Self : Context) return Boolean;
   --  Return whether diagnostics are enabled

   procedure Reload (Self : in out Context);
   --  Reload the current context. This will invalidate and destroy any
   --  Libadalang related data, and recreate it from scratch.

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
     (Self : Context;
      URI  : LSP.Types.LSP_String) return LSP.Types.LSP_String;
   --  Turn URI into path by stripping schema from it

   function File_To_URI
     (Self : Context;
      File  : LSP.Types.LSP_String) return LSP.Types.LSP_String;
   --  Convert file name to URI

   function Get_Node_At
     (Self     : Context;
      Position : LSP.Messages.TextDocumentPositionParams'Class)
      return Libadalang.Analysis.Ada_Node;
   --  Return the node at the given location. This is like LSP.Ada_Documents,
   --  but it works even when given document hasn't opened yet.

   function Find_All_References
     (Self       : Context;
      Definition : Libadalang.Analysis.Defining_Name)
        return Libadalang.Analysis.Base_Id_Array;
   --  Finds all references to a given defining name in all units of the
   --  context.

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

      Project_Tree   : GNATCOLL.Projects.Project_Tree_Access;
      Root           : LSP.Types.LSP_String;
      Charset        : Ada.Strings.Unbounded.Unbounded_String;

      Source_Files   : GNATCOLL.VFS.File_Array_Access;
      --  Cache for the list of Ada source files in the loaded project tree

      Documents      : Document_Maps.Map;

      Diagnostics_Enabled : Boolean := True;
      --  Whether to publish diagnostics
   end record;

   function Is_Part_Of_Project
     (Self : Context;
      File : GNATCOLL.VFS.Virtual_File) return Boolean;
   --  Check if given file belongs to the project loaded in the Context

   procedure Update_Source_Files (Self : in out Context);
   --  Update Self.Source_Files value

   procedure Find_Project_File
     (Self      : in out Context;
      File      : LSP.Types.LSP_String;
      Error     : out LSP.Types.LSP_String;
      Project   : out GNATCOLL.VFS.Virtual_File;
      Status    : out Project_Status);
   --  Find GPR file. Fill Error is specified project doesn't exist

   function Is_Initialized (Self : Context) return Boolean
   is
     (Self.Root /= Types.Empty_LSP_String);

   function Has_Project (Self : Context) return Boolean
   is
     (Self.Project_Tree /= null);

end LSP.Ada_Contexts;
