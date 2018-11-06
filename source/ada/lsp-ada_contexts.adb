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

with Ada.Directories;
with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
with Ada.Text_IO;

with GNATCOLL.JSON;

package body LSP.Ada_Contexts is

   -----------------------
   -- Find_Project_File --
   -----------------------

   not overriding function Find_Project_File
     (Self : in out Context;
      File : LSP.Types.LSP_String) return GNATCOLL.VFS.Virtual_File
   is
      procedure Search_GPR_File
        (Root   : Ada.Strings.UTF_Encoding.UTF_8_String;
         Result : out GNATCOLL.VFS.Virtual_File);
      --  Look for suitable GPR file under given directory

      procedure Create_Default
        (Root   : Ada.Strings.UTF_Encoding.UTF_8_String;
         Result : out GNATCOLL.VFS.Virtual_File);
      --  Create default.gpr project file

      --------------------
      -- Create_Default --
      --------------------

      procedure Create_Default
        (Root   : Ada.Strings.UTF_Encoding.UTF_8_String;
         Result : out GNATCOLL.VFS.Virtual_File)
      is
         Output : Ada.Text_IO.File_Type;
         Name   : constant String :=
           Ada.Directories.Compose (Root, "default", "gpr");
      begin
         Ada.Text_IO.Create (Output, Ada.Text_IO.Out_File, Name);
         Ada.Text_IO.Put_Line (Output, "project Default is");
         Ada.Text_IO.Put_Line (Output, "   for Source_Dirs use (""./**"");");
         Ada.Text_IO.Put_Line (Output, "   package Compiler is");
         Ada.Text_IO.Put_Line (Output, "      for Switches (""ada"") use (");
         Ada.Text_IO.Put_Line (Output, "        ""-gnatW8"",");
         Ada.Text_IO.Put_Line (Output, "        ""-gnatwa"",");
         Ada.Text_IO.Put_Line (Output, "        ""-gnaty""");
         Ada.Text_IO.Put_Line (Output, "      );");
         Ada.Text_IO.Put_Line (Output, "   end Compiler;");
         Ada.Text_IO.Put_Line (Output, "end Default;");
         Ada.Text_IO.Close (Output);

         Result := GNATCOLL.VFS.Create
           (GNATCOLL.VFS.Filesystem_String (Ada.Directories.Full_Name (Name)));
      end Create_Default;

      ---------------------
      -- Search_GPR_File --
      ---------------------

      procedure Search_GPR_File
        (Root   : Ada.Strings.UTF_Encoding.UTF_8_String;
         Result : out GNATCOLL.VFS.Virtual_File)
      is
         procedure On_File (Item : Ada.Directories.Directory_Entry_Type);
         procedure On_Dir (Item : Ada.Directories.Directory_Entry_Type);

         ------------
         -- On_Dir --
         ------------

         procedure On_Dir (Item : Ada.Directories.Directory_Entry_Type) is
         begin
            if Ada.Directories.Simple_Name (Item) not in "." | ".." then
               Search_GPR_File (Ada.Directories.Full_Name (Item), Result);
            end if;
         end On_Dir;

         -------------
         -- On_File --
         -------------

         procedure On_File (Item : Ada.Directories.Directory_Entry_Type) is
         begin
            Result := GNATCOLL.VFS.Create
              (GNATCOLL.VFS.Filesystem_String
                 (Ada.Directories.Full_Name (Item)));
         end On_File;

         Files_Only : constant Ada.Directories.Filter_Type :=
           (Ada.Directories.Ordinary_File => True, others => False);
         Dirs_Only : constant Ada.Directories.Filter_Type :=
           (Ada.Directories.Directory => True, others => False);
      begin
         Ada.Directories.Search (Root, "*.gpr", Files_Only, On_File'Access);
         Ada.Directories.Search (Root, "", Dirs_Only, On_Dir'Access);
      end Search_GPR_File;

      Root : constant Ada.Strings.UTF_Encoding.UTF_8_String :=
        LSP.Types.To_UTF_8_String (Self.Root);

      Name : constant Ada.Strings.UTF_Encoding.UTF_8_String :=
        LSP.Types.To_UTF_8_String (File);

      Result : GNATCOLL.VFS.Virtual_File;
   begin
      if Name /= "" then
         Result := GNATCOLL.VFS.Create_From_Base
           (Base_Dir  => GNATCOLL.VFS.Filesystem_String (Root),
            Base_Name => GNATCOLL.VFS.Filesystem_String (Name));

         if Result.Is_Regular_File then
            return Result;
         end if;
      end if;

      Search_GPR_File (Root, Result);

      if Result.Is_Regular_File then
         return Result;
      end if;

      Create_Default (Root, Result);
      return Result;
   end Find_Project_File;

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
      Root : LSP.Types.LSP_String) is
   begin
      Self.Root := Root;
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

   ------------------
   -- Load_Project --
   ------------------

   not overriding procedure Load_Project
     (Self     : in out Context;
      File     : LSP.Types.LSP_String;
      Scenario : LSP.Types.LSP_Any)
   is
      procedure Add_Variable (Name : String; Value : GNATCOLL.JSON.JSON_Value);

      ------------------
      -- Add_Variable --
      ------------------

      procedure Add_Variable
        (Name : String; Value : GNATCOLL.JSON.JSON_Value)
      is
         use type GNATCOLL.JSON.JSON_Value_Type;
      begin
         if Value.Kind = GNATCOLL.JSON.JSON_String_Type then
            Self.Project_Env.Change_Environment (Name, Value.Get);
         end if;
      end Add_Variable;

      GPR : constant GNATCOLL.VFS.Virtual_File :=
        Self.Find_Project_File (File);
   begin
      GNATCOLL.Projects.Free (Self.Project_Env);
      GNATCOLL.Projects.Initialize (Self.Project_Env);

      if not Scenario.Is_Empty then
         Scenario.Map_JSON_Object (Add_Variable'Access);
      end if;

      Self.Project_Tree.Load (GPR, Self.Project_Env);

      Self.LAL_Context := Libadalang.Analysis.Create_Context
        (Unit_Provider => Libadalang.Analysis.Create_Unit_Provider_Reference
          (Self.Unit_Provider),
         With_Trivia   => True,
         Charset       => "utf-8");
   end Load_Project;

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
