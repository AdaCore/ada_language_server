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
with Ada.Characters.Handling;  use Ada.Characters.Handling;
with Ada.Strings.UTF_Encoding;

with GNATCOLL.JSON;
with GNATCOLL.Projects; use GNATCOLL.Projects;
with GNATCOLL.VFS;      use GNATCOLL.VFS;

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
      Error     : out LSP.Types.LSP_String;
      Project   : out GNATCOLL.VFS.Virtual_File;
      Status    : out Project_Status)
   is

      procedure Search_GPR_File
        (Root     : Ada.Strings.UTF_Encoding.UTF_8_String;
         Result   : out GNATCOLL.VFS.Virtual_File;
         Multiple : in out Boolean;
         Status   : in out Project_Status);
      --  Look for suitable GPR file under given directory. Set Multiple if
      --  several project files have been found

      Root : constant Ada.Strings.UTF_Encoding.UTF_8_String :=
        LSP.Types.To_UTF_8_String (Self.Root);

      ---------------------
      -- Search_GPR_File --
      ---------------------

      procedure Search_GPR_File
        (Root     : Ada.Strings.UTF_Encoding.UTF_8_String;
         Result   : out GNATCOLL.VFS.Virtual_File;
         Multiple : in out Boolean;
         Status   : in out Project_Status)
      is
         procedure On_File (Item : Ada.Directories.Directory_Entry_Type);

         -------------
         -- On_File --
         -------------

         procedure On_File (Item : Ada.Directories.Directory_Entry_Type) is
         begin
            case Status is
               when Default_Project =>
                  Status := Found_Unique_Project;
               when Found_Unique_Project =>
                  Multiple := True;
               when User_Provided_Project =>
                  raise Program_Error;  --  This should never happen
            end case;

            Result := GNATCOLL.VFS.Create
              (GNATCOLL.VFS.Filesystem_String
                 (Ada.Directories.Full_Name (Item)));
         end On_File;

         Files_Only : constant Ada.Directories.Filter_Type :=
           (Ada.Directories.Ordinary_File => True, others => False);
      begin
         Ada.Directories.Search (Root, "*.gpr", Files_Only, On_File'Access);
      end Search_GPR_File;

      Name : constant Ada.Strings.UTF_Encoding.UTF_8_String :=
        LSP.Types.To_UTF_8_String (File);

      Found_Non_Unique_Project : Boolean := False;

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
         else
            LSP.Types.Append (Error, "Specified project doesn't exist: ");
            LSP.Types.Append (Error, File);
         end if;
      end if;

      --  If not found, perform a search in the root directory
      Status := Default_Project;

      --  This call changes Status if project found
      Search_GPR_File (Root, Project, Found_Non_Unique_Project, Status);

      if Found_Non_Unique_Project then
         LSP.Types.Append
           (Error, "Please specify project file in ada.projectFile setting.");
         --  Fallback to default project
         Status := Default_Project;
      end if;
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
      procedure Add_Variable (Name : String; Value : GNATCOLL.JSON.JSON_Value);
      procedure On_Error (Text : String);

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

      --------------
      -- On_Error --
      --------------

      procedure On_Error (Text : String) is
      begin
         LSP.Types.Append (Error_Text, LSP.Types.To_LSP_String (Text));
      end On_Error;

      GPR    : GNATCOLL.VFS.Virtual_File;
      Status : Project_Status;
      Root   : constant Ada.Strings.UTF_Encoding.UTF_8_String :=
        LSP.Types.To_UTF_8_String (Self.Root);

   begin
      --  Here, we overwrite previous content of Self.Project_Tree without
      --  freeing. That's OK because the Unit provider owns it and will free
      --  the old project tree when we renew the provider.

      Errors.the_type := LSP.Messages.Warning;
      Self.Project_Tree := new GNATCOLL.Projects.Project_Tree;

      GNATCOLL.Projects.Initialize (Project_Env);

      if not Scenario.Is_Empty then
         Scenario.Map_JSON_Object (Add_Variable'Access);
      end if;

      Self.Find_Project_File (File, Errors.message, GPR, Status);

      if Status /= Default_Project then
         begin
            --  We have existing file, try to load it

            Self.Project_Tree.Load
              (GPR, Project_Env, Errors => On_Error'Unrestricted_Access);

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
         --  at all. In any case, use a default project.

         Server_Trace.Trace ("Using default project in " & Root);

         Ada.Directories.Set_Directory (Root);
         Self.Project_Tree.Load_Implicit_Project (Project_Env);

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

   --------------------------
   -- Get_Ada_Source_Files --
   --------------------------

   function Get_Ada_Source_Files
     (Self : Context) return GNATCOLL.VFS.File_Array_Access
   is
      All_Sources     : File_Array_Access :=
        Self.Project_Tree.Root_Project.Source_Files (Recursive => True);
      All_Ada_Sources : File_Array (1 .. All_Sources'Length);
      Free_Index      : Natural := All_Ada_Sources'First;
      Set             : File_Info_Set;
   begin
      --  Iterate through all sources, returning only those that have Ada as
      --  language.
      for J in All_Sources'Range loop
         Set := Self.Project_Tree.Info_Set (All_Sources (J));
         if not Set.Is_Empty then
            --  The file can be listed in several projects with different
            --  Info_Sets, in the case of aggregate project. However, assume
            --  that the language is the same in all projects, so look only
            --  at the first entry in the set.
            declare
               Info : constant File_Info'Class :=
                 File_Info'Class (Set.First_Element);
            begin
               if To_Lower (Info.Language) = "ada" then
                  All_Ada_Sources (Free_Index) := All_Sources (J);
                  Free_Index := Free_Index + 1;
               end if;
            end;
         end if;
      end loop;

      Unchecked_Free (All_Sources);
      return new File_Array'(All_Ada_Sources (1 .. Free_Index - 1));
   end Get_Ada_Source_Files;

   -----------------
   -- URI_To_File --
   -----------------

   function URI_To_File
     (Self : Context;
      URI  : LSP.Types.LSP_String) return LSP.Types.LSP_String
   is
      pragma Unreferenced (Self);

      To     : constant URIs.URI_String := LSP.Types.To_UTF_8_String (URI);
      Result : constant String := URIs.Conversions.To_File (To);
   begin
      return LSP.Types.To_LSP_String (Result);
   end URI_To_File;

end LSP.Ada_Contexts;
