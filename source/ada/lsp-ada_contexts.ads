------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                        Copyright (C) 2018, AdaCore                       --
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

with Ada.Containers.Hashed_Maps;

private with GNATCOLL.Projects;
with GNATCOLL.VFS;

private with Libadalang.Analysis;
with LSP.Messages;

with LSP.Ada_Documents;
with LSP.Types;

package LSP.Ada_Contexts is

   type Context is tagged limited private;

   procedure Initialize
     (Self : in out Context;
      Root : LSP.Types.LSP_String);

   function Is_Initialized (Self : Context) return Boolean;
   function Has_Project (Self : Context) return Boolean;

   procedure Load_Project
     (Self     : in out Context;
      File     : LSP.Types.LSP_String;
      Scenario : LSP.Types.LSP_Any);

   procedure Reload (Self : in out Context);
   --  Reload the current context. This will invalidate and destroy any
   --  Libadalang related data, and recreate it from scratch.

   procedure Load_Document
     (Self : aliased in out Context;
      Item : LSP.Messages.TextDocumentItem);

   procedure Unload_Document
     (Self : in out Context;
      Item : LSP.Messages.TextDocumentIdentifier);

   function Get_Document
     (Self : Context;
      URI  : LSP.Messages.DocumentUri)
      return LSP.Ada_Documents.Document_Access;

   function Get_Source_Files
     (Self : Context) return GNATCOLL.VFS.File_Array_Access;

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

   package Document_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => LSP.Messages.DocumentUri,
      Element_Type    => LSP.Ada_Documents.Document_Access,
      Hash            => LSP.Types.Hash,
      Equivalent_Keys => LSP.Types."=",
      "="             => LSP.Ada_Documents."=");

   type Context is tagged limited record
      Unit_Provider  : Libadalang.Analysis.Unit_Provider_Reference;
      LAL_Context    : Libadalang.Analysis.Analysis_Context;

      Project_Tree   : GNATCOLL.Projects.Project_Tree_Access;
      Root           : LSP.Types.LSP_String;

      Documents      : Document_Maps.Map;
   end record;

   function Find_Project_File
     (Self : in out Context;
      File : LSP.Types.LSP_String) return GNATCOLL.VFS.Virtual_File;
   --  Find GPR file

   function Is_Initialized (Self : Context) return Boolean
   is
     (Self.Root /= Types.Empty_LSP_String);

   function Has_Project (Self : Context) return Boolean
   is
     (Self.Project_Tree /= null);

end LSP.Ada_Contexts;
