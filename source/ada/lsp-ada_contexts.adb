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
with Ada.Unchecked_Deallocation;

with GNATCOLL.JSON;
with GNATCOLL.Projects; use GNATCOLL.Projects;
with GNATCOLL.VFS;      use GNATCOLL.VFS;

with URIs;
with LSP.Ada_Unit_Providers;

with Langkit_Support.Slocs;

package body LSP.Ada_Contexts is

   function Get_Charset (Self : Context'Class) return String;
   --  Return the charset with which the context was initialized

   procedure Free is new Ada.Unchecked_Deallocation
     (LSP.Ada_Documents.Document,
      Internal_Document_Access);

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

   -------------------------
   -- Find_All_References --
   -------------------------

   function Find_All_References
     (Self       : Context;
      Definition : Libadalang.Analysis.Defining_Name)
        return Libadalang.Analysis.Base_Id_Array
   is
      Source_Units : Libadalang.Analysis.Analysis_Unit_Array
        (Self.Source_Files'Range);
   begin
      for N in Self.Source_Files'Range loop
         Source_Units (N) := Self.LAL_Context.Get_From_File
           (Self.Source_Files (N).Display_Full_Name,
            Charset => Self.Get_Charset);
      end loop;

      return Definition.P_Find_All_References (Source_Units);
   end Find_All_References;

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
   -- Has_Document --
   ------------------

   function Has_Document
     (Self : Context;
      URI  : LSP.Messages.DocumentUri) return Boolean is
   begin
      return Self.Documents.Contains (URI);
   end Has_Document;

   ------------------
   -- Get_Document --
   ------------------

   function Get_Document
     (Self : Context;
      URI  : LSP.Messages.DocumentUri)
        return LSP.Ada_Documents.Document_Access
   is
      Object : constant Internal_Document_Access := Self.Documents (URI);
   begin
      return LSP.Ada_Documents.Document_Access (Object);
   end Get_Document;

   -----------------
   -- Get_Charset --
   -----------------

   function Get_Charset (Self : Context'Class) return String is
   begin
      return Ada.Strings.Unbounded.To_String (Self.Charset);
   end Get_Charset;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self  : in out Context;
      Root  : LSP.Types.LSP_String) is
   begin
      Self.Root := Root;
      Self.Source_Files := new File_Array'(1 .. 0 => <>);
   end Initialize;

   ------------------------
   -- Is_Part_Of_Project --
   ------------------------

   function Is_Part_Of_Project
     (Self : Context;
      File : Virtual_File) return Boolean
   is
      Set   : constant File_Info_Set := Self.Project_Tree.Info_Set (File);
      First : constant File_Info'Class := File_Info'Class (Set.First_Element);
   begin
      return First.Project /= No_Project;
   end Is_Part_Of_Project;

   -------------------
   -- Load_Document --
   -------------------

   function Load_Document
     (Self : aliased in out Context;
      Item : LSP.Messages.TextDocumentItem)
      return LSP.Ada_Documents.Document_Access
   is
      Object : constant Internal_Document_Access :=
        new LSP.Ada_Documents.Document (Self'Unchecked_Access);
   begin
      Object.Initialize (Self.LAL_Context, Item);
      Self.Documents.Insert (Item.uri, Object);

      declare
         Name : constant LSP.Types.LSP_String := Self.URI_To_File (Item.uri);
         File : constant Virtual_File := Create
           (Filesystem_String (LSP.Types.To_UTF_8_String (Name)),
            Normalize => True);
      begin
         if not Self.Is_Part_Of_Project (File) then
            --  File that we are loading doesn't belong to the project, so
            --  firstly recompute project view just in case this is a new file
            --  in the project. Then update list of source files.
            Self.Project_Tree.Recompute_View;
            Self.Update_Source_Files;
         end if;
      end;

      return LSP.Ada_Documents.Document_Access (Object);
   end Load_Document;

   ------------------
   -- Load_Project --
   ------------------

   procedure Load_Project
     (Self     : in out Context;
      File     : LSP.Types.LSP_String;
      Scenario : LSP.Types.LSP_Any;
      Charset  : String;
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
      Self.Charset := Ada.Strings.Unbounded.To_Unbounded_String (Charset);

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

               Self.Trace.Trace (E);
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

         Self.Trace.Trace ("Using default project in " & Root);

         Ada.Directories.Set_Directory (Root);
         Self.Project_Tree.Load_Implicit_Project (Project_Env);

      end if;

      declare
         Provider : LSP.Ada_Unit_Providers.Unit_Provider (Self.Project_Tree);
      begin
         Self.Unit_Provider.Set (Provider);
      end;

      Self.Reload;

      Self.Update_Source_Files;
   end Load_Project;

   ------------
   -- Reload --
   ------------

   procedure Reload (Self : in out Context) is
   begin
      Self.LAL_Context := Libadalang.Analysis.Create_Context
        (Unit_Provider => Self.Unit_Provider,
         With_Trivia   => True,
         Charset       => Self.Get_Charset);

      --  After re-creation of the LAL context all Analysis_Units become
      --  outdated, let's refresh all of them
      for Document of Self.Documents loop
         Document.Reload (Self.LAL_Context);
      end loop;
   end Reload;

   ---------------------
   -- Unload_Document --
   ---------------------

   procedure Unload_Document
     (Self : in out Context;
      Item : LSP.Messages.TextDocumentIdentifier)
   is
      Document : Internal_Document_Access :=
        Self.Documents.Element (Item.uri);
   begin
      Self.Documents.Delete (Item.uri);
      Free (Document);
   end Unload_Document;

   -------------------------
   -- Update_Source_Files --
   -------------------------

   procedure Update_Source_Files (Self : in out Context) is
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
      Unchecked_Free (Self.Source_Files);

      Self.Source_Files :=
        new File_Array'(All_Ada_Sources (1 .. Free_Index - 1));
   end Update_Source_Files;

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

   -----------------------------
   -- Set_Diagnostics_Enabled --
   -----------------------------

   procedure Set_Diagnostics_Enabled
     (Self    : in out Context;
      Enabled : Boolean) is
   begin
      Self.Diagnostics_Enabled := Enabled;
   end Set_Diagnostics_Enabled;

   -----------------------------
   -- Get_Diagnostics_Enabled --
   -----------------------------

   function Get_Diagnostics_Enabled (Self : Context) return Boolean is
   begin
      return Self.Diagnostics_Enabled;
   end Get_Diagnostics_Enabled;

   -----------------
   -- Get_Node_At --
   -----------------

   function Get_Node_At
     (Self     : Context;
      Position : LSP.Messages.TextDocumentPositionParams'Class)
      return Libadalang.Analysis.Ada_Node
   is
      use type Libadalang.Analysis.Ada_Node;
      use type Langkit_Support.Slocs.Line_Number;
      use type Langkit_Support.Slocs.Column_Number;

      Unit : Libadalang.Analysis.Analysis_Unit;

      URI : constant LSP.Messages.DocumentUri := Position.textDocument.uri;

   begin
      if Self.Has_Document (URI) then
         return Self.Get_Document (URI).Get_Node_At (Position.position);
      else
         Unit := Self.LAL_Context.Get_From_File
           (LSP.Types.To_UTF_8_String (Self.URI_To_File (URI)),
            Charset => Self.Get_Charset);

         if Unit.Root = Libadalang.Analysis.No_Ada_Node then
            return Libadalang.Analysis.No_Ada_Node;
         end if;

         return Unit.Root.Lookup
           ((Line   => Langkit_Support.Slocs.Line_Number
                         (Position.position.line) + 1,
             Column => Langkit_Support.Slocs.Column_Number
                         (Position.position.character) + 1));
      end if;
   end Get_Node_At;

end LSP.Ada_Contexts;
