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

with Ada.Containers.Hashed_Maps;

private with GNATCOLL.Projects;
with GNATCOLL.VFS;

private with Libadalang.Analysis;
with LSP.Messages;

with LSP.Ada_Documents;
with LSP.Types;

package LSP.Ada_Contexts is

   type Context is tagged limited private;
   --  Context includes set of edited documents, Libadalang context, project
   --  tree and others data.

   procedure Initialize
     (Self : in out Context;
      Root : LSP.Types.LSP_String);
   --  Reset context. Set Root directory as LSP client provides it.

   function Is_Initialized (Self : Context) return Boolean;
   --  Check if context has been initialized

   function Has_Project (Self : Context) return Boolean;
   --  Check if context has a project

   procedure Load_Project
     (Self     : in out Context;
      File     : LSP.Types.LSP_String;
      Scenario : LSP.Types.LSP_Any;
      Errors   : out LSP.Messages.ShowMessageParams);
   --  Load given project File using given Scenario variables.
   --  In case of errors create and load default project.
   --  Return warnings and errors in Errors parameter.

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

   function Get_Document
     (Self : Context;
      URI  : LSP.Messages.DocumentUri)
      return LSP.Ada_Documents.Document_Access;
   --  Retrive document identified by given URI. User shouldn't free it or
   --  store it.

   function Get_Ada_Source_Files
     (Self : Context) return GNATCOLL.VFS.File_Array_Access;
   --  Return the list of Ada source files in the loaded project tree.
   --  Callers must free the result using GNATCOLL.VFS.Unchecked_Free.

   function URI_To_File
     (Self : Context;
      URI  : LSP.Types.LSP_String) return LSP.Types.LSP_String;
   --  Turn URI into path by stripping schema from it

   function File_To_URI
     (Self : Context;
      File  : LSP.Types.LSP_String) return LSP.Types.LSP_String;
   --  Convert file name to URI

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

   type Context is tagged limited record
      Unit_Provider  : Libadalang.Analysis.Unit_Provider_Reference;
      LAL_Context    : Libadalang.Analysis.Analysis_Context;

      Project_Tree   : GNATCOLL.Projects.Project_Tree_Access;
      Root           : LSP.Types.LSP_String;

      Documents      : Document_Maps.Map;
   end record;

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
