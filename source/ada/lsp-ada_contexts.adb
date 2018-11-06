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

with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;

package body LSP.Ada_Contexts is

   ------------------
   -- Get_Document --
   ------------------

   not overriding function Get_Document
     (Self : Context;
      URI  : LSP.Messages.DocumentUri)
        return LSP.Ada_Documents.Document_Access is
   begin
      return Self.Documents (URI);
   end Get_Document;

   --------------
   -- Get_Unit --
   --------------

   overriding function Get_Unit
     (Self    : Unit_Provider;
      Context : Libadalang.Analysis.Analysis_Context'Class;
      Name    : Wide_Wide_String;
      Kind    : Libadalang.Common.Analysis_Unit_Kind;
      Charset : String := "";
      Reparse : Boolean := False)
      return Libadalang.Analysis.Analysis_Unit'Class
   is
      File : constant String := Self.Get_Unit_Filename (Name, Kind);
   begin
      return Libadalang.Analysis.Get_From_File
        (Context  => Context,
         Filename => File,
         Charset  => Charset,
         Reparse  => Reparse);
   end Get_Unit;

   -----------------------
   -- Get_Unit_Filename --
   -----------------------

   overriding function Get_Unit_Filename
     (Self : Unit_Provider;
      Name : Wide_Wide_String;
      Kind : Libadalang.Common.Analysis_Unit_Kind)
      return String
   is
      Map : constant array (Libadalang.Common.Analysis_Unit_Kind) of
        GNATCOLL.Projects.Unit_Parts :=
          (Libadalang.Common.Unit_Specification =>
             GNATCOLL.Projects.Unit_Spec,
           Libadalang.Common.Unit_Body =>
             GNATCOLL.Projects.Unit_Body);

      Unit_Name : constant String :=
        Ada.Strings.UTF_Encoding.Wide_Wide_Strings.Encode (Name);

      File : constant GNATCOLL.VFS.Filesystem_String :=
        Self.Context.Project_Tree.Root_Project.File_From_Unit
          (Unit_Name => Unit_Name,
           Part      => Map (Kind),
           Language  => "Ada");
   begin
      if File'Length = 0 then
         return "";
      end if;

      return String
        (GNATCOLL.VFS.Filesystem_String'
           (Self.Context.Project_Tree.Create (File).Full_Name));
   end Get_Unit_Filename;

   ----------------
   -- Initialize --
   ----------------

   not overriding procedure Initialize
     (Self : in out Context;
      Root : LSP.Types.LSP_String)
   is
      Dir  : constant GNATCOLL.VFS.Virtual_File :=
        GNATCOLL.VFS.Create
          (GNATCOLL.VFS.Filesystem_String
             (LSP.Types.To_UTF_8_String (Root)));
   begin
      Self.Root := Root;
      GNATCOLL.Projects.Initialize (Self.Project_Env);
      Self.Project_Tree.Load
        (GNATCOLL.VFS.Create_From_Dir (Dir, "gnat/lsp.gpr"),
         Self.Project_Env);

      Self.LAL_Context := Libadalang.Analysis.Create_Context
        (Unit_Provider => Libadalang.Analysis.Create_Unit_Provider_Reference
          (Self.Unit_Provider),
         With_Trivia   => True,
         Charset       => "utf-8");
   end Initialize;

   -------------------
   -- Load_Document --
   -------------------

   not overriding procedure Load_Document
     (Self : in out Context;
      Item : LSP.Messages.TextDocumentItem)
   is
      Object : constant LSP.Ada_Documents.Document_Access :=
        new LSP.Ada_Documents.Document;
   begin
      Object.Initialize (Self.LAL_Context, Item);
      Self.Documents.Insert (Item.uri, Object);
   end Load_Document;

   ---------------------
   -- Unload_Document --
   ---------------------

   not overriding procedure Unload_Document
     (Self : in out Context;
      Item : LSP.Messages.TextDocumentIdentifier)
   is
   begin
      Self.Documents.Delete (Item.uri);
   end Unload_Document;

   ----------------------
   -- Get_Source_Files --
   ----------------------

   not overriding function Get_Source_Files
     (Self : Context) return GNATCOLL.VFS.File_Array_Access is
     (Self.Project_Tree.Root_Project.Source_Files);

end LSP.Ada_Contexts;
