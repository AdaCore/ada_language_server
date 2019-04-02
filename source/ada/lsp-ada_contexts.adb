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

with Ada.Directories;
with Ada.Strings.UTF_Encoding;
with Ada.Text_IO;

with GNATCOLL.JSON;
with GNAT.OS_Lib;

with URIs;

with Libadalang.Project_Provider;

package body LSP.Ada_Contexts is

   -----------------
   -- File_To_URI --
   -----------------

   function File_To_URI
     (Self : Context;
      File : LSP.Types.LSP_String) return LSP.Types.LSP_String
   is
      pragma Unreferenced (Self);

      Result : constant URIs.URI_String :=
        URIs.Conversions.From_File (LSP.Types.To_UTF_8_String (File));
   begin
      return LSP.Types.To_LSP_String (Result);
   end File_To_URI;

   -----------------------
   -- Find_Project_File --
   -----------------------

   procedure Find_Project_File
     (Self      : in out Context;
      File      : LSP.Types.LSP_String;
      Project   : out GNATCOLL.VFS.Virtual_File;
      Status    : out Project_Status)
   is

      procedure Search_GPR_File
        (Root   : Ada.Strings.UTF_Encoding.UTF_8_String;
         Result : out GNATCOLL.VFS.Virtual_File;
         Status : in out Project_Status);
      --  Look for suitable GPR file under given directory

      Root : constant Ada.Strings.UTF_Encoding.UTF_8_String :=
        LSP.Types.To_UTF_8_String (Self.Root);

      ---------------------
      -- Search_GPR_File --
      ---------------------

      procedure Search_GPR_File
        (Root   : Ada.Strings.UTF_Encoding.UTF_8_String;
         Result : out GNATCOLL.VFS.Virtual_File;
         Status : in out Project_Status)
      is
         procedure On_File (Item : Ada.Directories.Directory_Entry_Type);
         procedure On_Dir (Item : Ada.Directories.Directory_Entry_Type);

         ------------
         -- On_Dir --
         ------------

         procedure On_Dir (Item : Ada.Directories.Directory_Entry_Type) is
         begin
            if Ada.Directories.Simple_Name (Item) not in "." | ".." then
               Search_GPR_File
                 (Ada.Directories.Full_Name (Item), Result, Status);
            end if;
         end On_Dir;

         -------------
         -- On_File --
         -------------

         procedure On_File (Item : Ada.Directories.Directory_Entry_Type) is
         begin
            case Status is
               when Default_Project =>
                  Status := Found_Unique_Project;
               when Found_Unique_Project =>
                  Status := Found_Non_Unique_Project;
               when others =>
                  null;
            end case;

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

      Name : constant Ada.Strings.UTF_Encoding.UTF_8_String :=
        LSP.Types.To_UTF_8_String (File);

   begin
      --  If Name was provided, search for the corresponding project
      if Name /= "" then

         --  First, search the project file relatively to the root
         Project := GNATCOLL.VFS.Create_From_Base
           (Base_Dir  => GNATCOLL.VFS.Filesystem_String (Root),
            Base_Name => GNATCOLL.VFS.Filesystem_String (Name));

         if Project.Is_Regular_File then
            Status := User_Provided_Project;
            return;
         end if;
      end if;

      --  If not found, perform a comprehensive search everywhere below
      --  root.
      Status := Default_Project;

      --  This call changes Status if project found
      Search_GPR_File (Root, Project, Status);
   end Find_Project_File;

   ------------------
   -- Get_Document --
   ------------------

   function Get_Document
     (Self : Context;
      URI  : LSP.Messages.DocumentUri)
        return LSP.Ada_Documents.Document_Access is
   begin
      return Self.Documents (URI);
   end Get_Document;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self : in out Context;
      Root : LSP.Types.LSP_String) is
   begin
      Self.Root := Root;
   end Initialize;

   -------------------
   -- Load_Document --
   -------------------

   procedure Load_Document
     (Self : aliased in out Context;
      Item : LSP.Messages.TextDocumentItem)
   is
      Object : constant LSP.Ada_Documents.Document_Access :=
        new LSP.Ada_Documents.Document (Self'Unchecked_Access);
   begin
      Object.Initialize (Self.LAL_Context, Item);
      Self.Documents.Insert (Item.uri, Object);
   end Load_Document;

   ------------------
   -- Load_Project --
   ------------------

   procedure Load_Project
     (Self     : in out Context;
      File     : LSP.Types.LSP_String;
      Scenario : LSP.Types.LSP_Any;
      Errors   : out LSP.Messages.ShowMessageParams)
   is
      use GNATCOLL.VFS;

      procedure Add_Variable (Name : String; Value : GNATCOLL.JSON.JSON_Value);
      procedure On_Error (Text : String);
      function Create_Default return GNATCOLL.VFS.Virtual_File;
      --  Create default project file in temp directory

      Project_Env   : GNATCOLL.Projects.Project_Environment_Access;
      Error_Text    : LSP.Types.LSP_String_Vector;

      ------------------
      -- Add_Variable --
      ------------------

      procedure Add_Variable
        (Name : String; Value : GNATCOLL.JSON.JSON_Value)
      is
         use type GNATCOLL.JSON.JSON_Value_Type;
      begin
         if Value.Kind = GNATCOLL.JSON.JSON_String_Type then
            Project_Env.Change_Environment (Name, Value.Get);
         end if;
      end Add_Variable;

      --------------------
      -- Create_Default --
      --------------------

      function Create_Default return GNATCOLL.VFS.Virtual_File
      is
         use GNAT.OS_Lib;

         Output : Ada.Text_IO.File_Type;
         Pid_Image : constant String :=
           Pid_To_Integer (Current_Process_Id)'Image;

         Prj_Name  : constant String :=
           "ALS_default_"
           & Pid_Image (Pid_Image'First + 1 .. Pid_Image'Last);

         Name   : constant String :=
           Ada.Directories.Compose
             (+GNATCOLL.VFS.Get_Tmp_Directory.Full_Name, Prj_Name, "gpr");
         --  Create a project file in the temp directory

         Root : constant Ada.Strings.UTF_Encoding.UTF_8_String :=
           LSP.Types.To_UTF_8_String (Self.Root);

      begin

         Ada.Text_IO.Create (Output, Ada.Text_IO.Out_File, Name);
         Ada.Text_IO.Put_Line (Output, "project " & Prj_Name & " is");
         Ada.Text_IO.Put_Line
           (Output, "   for Source_Dirs use (""" & Root & "/**"");");
         Ada.Text_IO.Put_Line (Output, "   package Compiler is");
         Ada.Text_IO.Put_Line (Output, "      for Switches (""ada"") use (");
         Ada.Text_IO.Put_Line (Output, "        ""-gnatW8"",");
         Ada.Text_IO.Put_Line (Output, "        ""-gnatwa"",");
         Ada.Text_IO.Put_Line (Output, "        ""-gnaty""");
         Ada.Text_IO.Put_Line (Output, "      );");
         Ada.Text_IO.Put_Line (Output, "   end Compiler;");
         Ada.Text_IO.Put_Line (Output, "end " & Prj_Name & ";");
         Ada.Text_IO.Close (Output);

         return GNATCOLL.VFS.Create
           (GNATCOLL.VFS.Filesystem_String (Ada.Directories.Full_Name (Name)));
      end Create_Default;

      --------------
      -- On_Error --
      --------------

      procedure On_Error (Text : String) is
      begin
         LSP.Types.Append (Error_Text, LSP.Types.To_LSP_String (Text));
      end On_Error;

      GPR    : GNATCOLL.VFS.Virtual_File;
      Status : Project_Status;

   begin
      --  Here, we overwrite previous content of Self.Project_Tree without
      --  freeing. That's OK because the Unit provider owns it and will free
      --  the old project tree when we renew the provider.

      Self.Project_Tree := new GNATCOLL.Projects.Project_Tree;

      GNATCOLL.Projects.Initialize (Project_Env);

      if not Scenario.Is_Empty then
         Scenario.Map_JSON_Object (Add_Variable'Access);
      end if;

      Self.Find_Project_File (File, GPR, Status);

      if Status /= Default_Project then
         begin
            --  We have existing file, try to load it

            Self.Project_Tree.Load
              (GPR, Project_Env, Errors => On_Error'Unrestricted_Access);
            Errors.the_type := LSP.Messages.Warning;

         exception
            when E : GNATCOLL.Projects.Invalid_Project =>

               Server_Trace.Trace (E);
               Status := Default_Project;
               Errors.the_type := LSP.Messages.Error;

               LSP.Types.Append
                 (Errors.message,
                  LSP.Types.To_LSP_String
                    ("Unable to load project file: " &
                     (+GPR.Full_Name.all)));
         end;

         --  Populate Errors if there is any error/warning
         for Line of Error_Text loop
            LSP.Types.Append (Errors.message, Line);
         end loop;
      end if;

      if Status = Default_Project then

         --  At this stage, either a project file name was provided but not
         --  found, the project could not be loaded, either it wasn't provided
         --  at all. In any case, create a default project.

         GPR := Create_Default;

         Server_Trace.Trace ("Using default project " & (+GPR.Full_Name.all));

         Self.Project_Tree.Load
           (GPR, Project_Env, Errors => On_Error'Unrestricted_Access);

      end if;

      Self.Unit_Provider :=
        Libadalang.Project_Provider.Create_Project_Unit_Provider_Reference
          (Self.Project_Tree, Project_Env);

      Self.LAL_Context := Libadalang.Analysis.Create_Context
        (Unit_Provider => Self.Unit_Provider,
         With_Trivia   => True,
         Charset       => "utf-8");
   end Load_Project;

   ------------
   -- Reload --
   ------------

   procedure Reload (Self : in out Context) is
   begin
      Self.LAL_Context := Libadalang.Analysis.Create_Context
        (Unit_Provider => Self.Unit_Provider,
         With_Trivia   => True,
         Charset       => "utf-8");
   end Reload;

   ---------------------
   -- Unload_Document --
   ---------------------

   procedure Unload_Document
     (Self : in out Context;
      Item : LSP.Messages.TextDocumentIdentifier)
   is
   begin
      Self.Documents.Delete (Item.uri);
   end Unload_Document;

   ----------------------
   -- Get_Source_Files --
   ----------------------

   function Get_Source_Files
     (Self : Context) return GNATCOLL.VFS.File_Array_Access is
     (Self.Project_Tree.Root_Project.Source_Files);

   -----------------
   -- URI_To_File --
   -----------------

   function URI_To_File
     (Self : Context;
      URI  : LSP.Types.LSP_String) return LSP.Types.LSP_String
   is
      pragma Unreferenced (Self);

      Result : constant String := URIs.Conversions.To_File
        (LSP.Types.To_UTF_8_String (URI));
   begin
      return LSP.Types.To_LSP_String (Result);
   end URI_To_File;

end LSP.Ada_Contexts;
