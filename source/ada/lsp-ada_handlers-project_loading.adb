------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2018-2024, AdaCore                     --
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

with GNATCOLL.Traces;
with GNATCOLL.VFS;

with GPR2.Environment;
with GPR2.Options;
with GPR2.Containers;
with GPR2.Context;
with GPR2.Log;
with GPR2.Path_Name;
with GPR2.Project.Registry.Attribute;
with GPR2.Project.Tree.View_Builder;
with GPR2.Project.View;

pragma Warnings (Off, "unit ""GPR2.Build.Source.Sets"" is not referenced");
with GPR2.Build.Source.Sets;

with Libadalang.Preprocessing;
with LSP.Ada_Contexts;
with LSP.Ada_Context_Sets;
with LSP.Ada_Documents; use LSP.Ada_Documents;
with LSP.Alire;
with LSP.Ada_Handlers.File_Readers;
with LSP.Ada_Indexing;
with LSP.Ada_Project_Loading;
with LSP.Utils;

with URIs;

with VSS.Strings;
with VSS.Strings.Conversions;

package body LSP.Ada_Handlers.Project_Loading is

   Runtime_Indexing : constant GNATCOLL.Traces.Trace_Handle :=
     GNATCOLL.Traces.Create ("ALS.RUNTIME_INDEXING",
                             GNATCOLL.Traces.On);
   --  Trace to enable/disable runtime indexing. Useful for the testsuite.

   procedure Load_Project_With_Alire
     (Self         : in out Message_Handler'Class;
      Project_File : VSS.Strings.Virtual_String := "";
      Context      : GPR2.Context.Object;
      Charset      : VSS.Strings.Virtual_String);
   --  Core procedure to find project, search path, scenario and load the
   --  project.
   --
   --  @param Self                 The message handler itself
   --  @param Project_File         GPR, if set by the user in settings
   --  @param Scenario_Variables   Scenario as set by the user in settings
   --  @param Charset              Charset, if set by the user in settings
   --
   --  Load a project with a help of alire. If there is `alire.toml` in the
   --  root directory and `alr` in the `PATH`, then use Alire to setup project
   --  search path, extra scenario variables (and a project file name if
   --  Project_File is empty). If Alire reports error then show it to the
   --  user and fallback to an implicit project.
   --
   --  If Alire succeed or no alire/crate then load project if provided.

   procedure Load_Project
     (Self         : in out Message_Handler'Class;
      Project_Path : VSS.Strings.Virtual_String;
      Context      : GPR2.Context.Object;
      Environment  : GPR2.Environment.Object;
      Charset      : VSS.Strings.Virtual_String;
      Project_Type : LSP.Ada_Project_Loading.Project_Types);
   --  Attempt to load the given project file, with the scenario provided.
   --  This unloads all currently loaded project contexts. This factorizes code
   --  between Load_Project_With_Alire and Ensure_Project_Loaded.

   procedure Load_Implicit_Project
     (Self   : in out Message_Handler'Class;
      Status : LSP.Ada_Project_Loading.Project_Status);
   --  Load the implicit project

   procedure Update_Project_Predefined_Sources
     (Self : in out Message_Handler'Class);
   --  Fill Self.Project_Predefined_Sources with loaded project tree runtime

   procedure Mark_Source_Files_For_Indexing
     (Self : in out Message_Handler'Class);
   --  Mark all sources in all projects for indexing. This factorizes code
   --  between Load_Project and Load_Implicit_Project.

   ---------------------------
   -- Ensure_Project_Loaded --
   ---------------------------

   procedure Ensure_Project_Loaded (Self : in out Message_Handler'Class) is
      GPRs_Found   : Natural := 0;
      Project_File : VSS.Strings.Virtual_String;

   begin
      if not Self.Contexts.Is_Empty then
         --  Rely on the fact that there is at least one context initialized
         --  as a guarantee that the initialization has been done.
         return;
      end if;

      Self.Tracer.Trace ("Looking for a project... Root:");
      Self.Tracer.Trace_Text (Self.Client.Root);

      Load_Project_With_Alire
        (Self         => Self,
         Project_File => VSS.Strings.Empty_Virtual_String,
         Context      => Self.Configuration.Context,
         Charset      => Self.Configuration.Charset);

      if not Self.Contexts.Is_Empty then
         --  Some project was found by alire and loaded. We are done!
         return;
      elsif
        not LSP.Ada_Project_Loading.Is_Project_Loaded (Self.Project_Status)
      then
         --  Create an empty context and don't load the implicit project
         declare
            use LSP.Ada_Context_Sets;
            use LSP.Ada_Contexts;
            C      : constant Context_Access := new Context (Self.Tracer);
            Reader : LSP.Ada_Handlers.File_Readers.LSP_File_Reader
              (Self'Unchecked_Access);
         begin
            C.Initialize
              (File_Reader         => Reader,
               Follow_Symlinks     => Self.Configuration.Follow_Symlinks,
               Style               => Self.Configuration.Documentation_Style,
               As_Fallback_Context => True);
            Self.Contexts.Prepend (C);
            --  Errors should be handled by the user
            return;
         end;
      end if;

      --  We don't have alire/crate.
      --  We're going to look for a project in Root: list all the files
      --  in this directory, looking for .gpr files.

      if not Self.Client.Root.Is_Empty then
         declare
            Files : GNATCOLL.VFS.File_Array_Access :=
              Self.Client.Root_Directory.Read_Dir (GNATCOLL.VFS.Files_Only);
         begin
            for X of Files.all loop
               if X.Has_Suffix (".gpr") then
                  GPRs_Found := GPRs_Found + 1;
                  exit when GPRs_Found > 1;
                  Project_File := LSP.Utils.To_Virtual_String (X);
               end if;
            end loop;

            GNATCOLL.VFS.Unchecked_Free (Files);
         end;
      end if;

      --  What we do depends on the number of .gpr files found:

      if GPRs_Found = 0 then
         --  We have found zero .gpr files: load the implicit project

         LSP.Ada_Project_Loading.Set_Project_Type
           (Self.Project_Status, LSP.Ada_Project_Loading.Implicit_Project);

         Load_Implicit_Project
           (Self, LSP.Ada_Project_Loading.No_Project);
      elsif GPRs_Found = 1 then
         --  We have found exactly one .gpr file: let's load it.
         Self.Tracer.Trace ("Loading:");
         Self.Tracer.Trace_Text (Project_File);

         Load_Project
           (Self         => Self, Project_Path => Project_File,
            Context      => Self.Configuration.Context,
            Environment  => GPR2.Environment.Process_Environment,
            Charset      => "iso-8859-1",
            Project_Type => LSP.Ada_Project_Loading.Single_Project_Found);
      else
         Load_Implicit_Project
           (Self, LSP.Ada_Project_Loading.Multiple_Projects);
      end if;
   end Ensure_Project_Loaded;

   ---------------------------
   -- Load_Implicit_Project --
   ---------------------------

   procedure Load_Implicit_Project
     (Self   : in out Message_Handler'Class;
      Status : LSP.Ada_Project_Loading.Project_Status)
   is
      use LSP.Ada_Context_Sets;
      use LSP.Ada_Contexts;

      C : constant Context_Access := new Context (Self.Tracer);

      Reader : LSP.Ada_Handlers.File_Readers.LSP_File_Reader
        (Self'Unchecked_Access);
   begin
      Self.Tracer.Trace ("Loading the implicit project");

      LSP.Ada_Project_Loading.Set_Project_Type
        (Self.Project_Status, LSP.Ada_Project_Loading.Implicit_Project);
      LSP.Ada_Project_Loading.Set_Load_Status (Self.Project_Status, Status);
      Release_Contexts_And_Project_Info (Self);

      C.Initialize
        (File_Reader         => Reader,
         Follow_Symlinks     => Self.Configuration.Follow_Symlinks,
         Style               => Self.Configuration.Documentation_Style,
         As_Fallback_Context => True);

      --  Note: we would call Load_Implicit_Project here, but this has
      --  two problems:
      --    - there is a bug under Windows where the files returned by
      --      Source_Files have an extraneous directory separator
      --    - the implicit project relies on the current working
      --      of the ALS, which imposes a restriction on clients, and
      --      is an extra pitfall for developers of this server
      --
      --  Instead, use Load_Empty_Project and set the source dir and
      --  language manually: this does not have these inconvenients.

      --  When there is no .gpr, create a project which loads the
      --  root directory in the workspace.

      if not Self.Client.Root.Is_Empty then
         Self.Project_Dirs_Loaded.Include (Self.Client.Root_Directory);
      end if;

      Reload_Implicit_Project_Dirs (Self);
      C.Load_Project (Self.Project_Tree,
                      Self.Project_Tree.Root_Project,
                      "iso-8859-1");

      Update_Project_Predefined_Sources (Self);

      Self.Contexts.Prepend (C);

      --  Reindex the files from disk in the background after a project reload
      Mark_Source_Files_For_Indexing (Self);
   end Load_Implicit_Project;

   ------------------
   -- Load_Project --
   ------------------

   procedure Load_Project
     (Self         : in out Message_Handler'Class;
      Project_Path : VSS.Strings.Virtual_String;
      Context      : GPR2.Context.Object;
      Environment  : GPR2.Environment.Object;
      Charset      : VSS.Strings.Virtual_String;
      Project_Type : LSP.Ada_Project_Loading.Project_Types)
   is
      Project_File        : GNATCOLL.VFS.Virtual_File :=
        LSP.Utils.To_Virtual_File (Project_Path);

      procedure Create_Context_For_Non_Aggregate
        (View : GPR2.Project.View.Object);
      --  Create a new context for the given project view.

      --------------------------------------
      -- Create_Context_For_Non_Aggregate --
      --------------------------------------

      procedure Create_Context_For_Non_Aggregate
        (View : GPR2.Project.View.Object)
      is
         use LSP.Ada_Context_Sets;
         use LSP.Ada_Contexts;

         C                   : constant Context_Access :=
           new LSP.Ada_Contexts.Context (Self.Tracer);

         Reader : LSP.Ada_Handlers.File_Readers.LSP_File_Reader
           (Self'Unchecked_Access);

         Default_Config : Libadalang.Preprocessing.File_Config;
         File_Configs   : Libadalang.Preprocessing.File_Config_Maps.Map;

         procedure Set_Line_Mode
           (Config : in out Libadalang.Preprocessing.File_Config);
         --  Used to force the preprocessing line mode to Blank_Lines, which
         --  is needed to preserve the number of lines after preprocessing a
         --  source file, otherwise LSP requests based on SLOCs will fail.

         -------------------
         -- Set_Line_Mode --
         -------------------

         procedure Set_Line_Mode
           (Config : in out Libadalang.Preprocessing.File_Config) is
         begin
            if Config.Enabled then
               Config.Line_Mode := Libadalang.Preprocessing.Blank_Lines;
            end if;
         end Set_Line_Mode;

      begin
         --  Extract the preprocessing options from the context's project
         --  and create the file reader which will preprocess the files
         --  accordingly.

         Libadalang.Preprocessing.Extract_Preprocessor_Data_From_Project
           (Tree           => Self.Project_Tree,
            Project        => View,
            Default_Config => Default_Config,
            File_Configs   => File_Configs);

         Libadalang.Preprocessing.Iterate
           (Default_Config => Default_Config,
            File_Configs   => File_Configs,
            Process        => Set_Line_Mode'Access);

         Reader.Preprocessing_Data :=
           Libadalang.Preprocessing.Create_Preprocessor_Data
             (Default_Config, File_Configs);

         C.Initialize
           (Reader,
            Style           => Self.Configuration.Documentation_Style,
            Follow_Symlinks => Self.Configuration.Follow_Symlinks);

         C.Load_Project
           (Tree    => Self.Project_Tree,
            Root    => View,
            Charset => VSS.Strings.Conversions.To_UTF_8_String (Charset));

         Self.Tracer.Trace ("Prepend Context Id: "
                            & VSS.Strings.Conversions.To_UTF_8_String (C.Id));
         Self.Contexts.Prepend (C);
      end Create_Context_For_Non_Aggregate;

   begin
      LSP.Ada_Project_Loading.Set_Project_Type
        (Self.Project_Status, Project_Type);

      --  The projectFile may be either an absolute path or a
      --  relative path; if so, we're assuming it's relative
      --  to Self.Root.

      if not Project_File.Is_Absolute_Path
        and then not Self.Client.Root.Is_Empty
      then
         Project_File := GNATCOLL.VFS.Join (Self.Client.Root_Directory,
                                            Project_File);
      end if;

      if not Project_File.Is_Regular_File then
         Self.Tracer.Trace
           ("The project set in the configuration doesn't exist: "
            & Project_File.Display_Full_Name);
         LSP.Ada_Project_Loading.Set_Project_File
           (Self.Project_Status, Project_File);
         LSP.Ada_Project_Loading.Set_Load_Status
           (Self.Project_Status,
            LSP.Ada_Project_Loading.Project_Not_Found);
         return;
      end if;

      --  Unload the project tree and the project environment
      Release_Contexts_And_Project_Info (Self);

      declare
         Opts       : GPR2.Options.Object;
         Update_Log : GPR2.Log.Object;
         Success    : Boolean;
      begin
         --  Do not print any gpr messages on the standard output
         GPR2.Project.Tree.Verbosity := GPR2.Project.Tree.Quiet;

         --  Load the project
         Opts.Add_Switch (GPR2.Options.P, Project_File.Display_Full_Name);

         Success := Self.Project_Tree.Load
            (Opts,
             With_Runtime     => True,
             Absent_Dir_Error => GPR2.No_Error,
             Environment      => Environment);

         if Success then
            Success := Self.Project_Tree.Set_Context (Context);
         end if;

         if not Success then
            LSP.Ada_Project_Loading.Set_Load_Status
              (Self.Project_Status, LSP.Ada_Project_Loading.Invalid_Project);
         end if;

         Self.Project_Tree.Update_Sources (Update_Log);
         if Update_Log.Has_Error then
            LSP.Ada_Project_Loading.Set_Load_Status
              (Self.Project_Status, LSP.Ada_Project_Loading.Invalid_Project);
         end if;

         --  Retrieve the GPR2 error/warning messages right after loading the
         --  project.

         --  Merge all messages coming from Update_Sources with
         --  all messages coming from Load...
         for C in Self.Project_Tree.Log_Messages.Iterate loop
            Update_Log.Append (C.Element);
         end loop;

         --  Retrieve the GPR2 error/warning messages right after loading the
         --  project.
         LSP.Ada_Project_Loading.Set_GPR2_Messages
           (Self.Project_Status, Update_Log);

      exception
         when E : others =>
            Self.Tracer.Trace_Exception (E);
            LSP.Ada_Project_Loading.Set_Load_Status
              (Self.Project_Status, LSP.Ada_Project_Loading.Invalid_Project);
      end;

      LSP.Ada_Project_Loading.Set_Project_File
        (Self.Project_Status, Project_File);
      Self.Tracer.Trace ("GPR2 Log Messages:");
      for Msg of Self.Project_Tree.Log_Messages.all loop
         declare
            Location : constant String :=
              Msg.Sloc.Format (Full_Path_Name => True);
            Message : constant String := Msg.Message;
         begin
            Self.Tracer.Trace (Location & " " & Message);
         end;
      end loop;

      if not LSP.Ada_Project_Loading.Is_Project_Loaded (Self.Project_Status)
      then
         --  The project was invalid: fallback on loading the implicit project.
         Load_Implicit_Project
           (Self, LSP.Ada_Project_Loading.Invalid_Project);

      else
         --  No exception during Load_Autoconf, check if we have runtime
         if Self.Project_Tree.Has_Runtime_Project then
            LSP.Ada_Project_Loading.Set_Has_Runtime
              (Self.Project_Status, True);
         end if;

         Update_Project_Predefined_Sources (Self);

         if Self.Project_Tree.Root_Project.Kind in GPR2.Aggregate_Kind then
            for View of Self.Project_Tree.Root_Project.Aggregated loop
               Create_Context_For_Non_Aggregate (View);
            end loop;
         else
            Create_Context_For_Non_Aggregate
              (Self.Project_Tree.Root_Project);
         end if;
      end if;

      --  Reindex all open documents immediately after project reload, so
      --  that navigation from editors is accurate.
      for Document of Self.Open_Documents loop
         for Context of Self.Contexts_For_URI (Document.URI) loop
            Context.Index_Document (Document.all);
         end loop;

         Self.Publish_Diagnostics
           (LSP.Ada_Documents.Document_Access (Document));
      end loop;

      --  We have successfully loaded a real project: monitor the filesystem
      --  for any changes on the sources of the project
      Self.File_Monitor.Monitor_Directories
        (Self.Contexts.All_Source_Directories);

      --  Reindex the files from disk in the background after a project reload
      Mark_Source_Files_For_Indexing (Self);
   end Load_Project;

   -----------------------------
   -- Load_Project_With_Alire --
   -----------------------------

   procedure Load_Project_With_Alire
     (Self         : in out Message_Handler'Class;
      Project_File : VSS.Strings.Virtual_String := "";
      Context      : GPR2.Context.Object;
      Charset      : VSS.Strings.Virtual_String)
   is

      Has_Alire    : Boolean;
      Errors       : VSS.Strings.Virtual_String;
      Project      : VSS.Strings.Virtual_String := Project_File;
      UTF_8        : constant VSS.Strings.Virtual_String := "utf-8";

      Environment  : GPR2.Environment.Object :=
        GPR2.Environment.Process_Environment;

   begin
      if LSP.Alire.Alire_Active (Self.Client) then

         Self.Tracer.Trace ("Check alire:");

         if Project.Is_Empty then

            LSP.Alire.Determine_Alire_Project
              (Root        => Self.Client.Root_Directory.Display_Full_Name,
               Has_Alire   => Has_Alire,
               Error       => Errors,
               Project     => Project);
         end if;

         LSP.Alire.Setup_Alire_Env
           (Root        => Self.Client.Root_Directory.Display_Full_Name,
            Has_Alire   => Has_Alire,
            Error       => Errors,
            Environment => Environment);

         if Has_Alire then
            LSP.Ada_Project_Loading.Set_Project_Type
              (Self.Project_Status, LSP.Ada_Project_Loading.Alire_Project);

            if not Errors.Is_Empty then

               --  Something wrong with alire. Report error and don't load the
               --  project.
               Self.Tracer.Trace_Text (Errors);

               LSP.Ada_Project_Loading.Set_Load_Status
                 (Self.Project_Status,
                  LSP.Ada_Project_Loading.Invalid_Project);
               return;
            else

               --  No errors means the project has been found
               pragma Assert (not Project.Is_Empty);

               Self.Tracer.Trace ("Project:");
               Self.Tracer.Trace_Text (Project);

               Load_Project
                 (Self         => Self,
                  Project_Path => Project,
                  Context      => Context,
                  Environment  => Environment,
                  Charset      =>
                    (if Charset.Is_Empty then UTF_8 else Charset),
                  Project_Type => LSP.Ada_Project_Loading.Alire_Project);
               --  Alire projects tend to use utf-8

               return;
            end if;
         else
            Self.Tracer.Trace ("No alr in the PATH.");
         end if;
      end if;

      --  There is no alire.toml or no alr, but we know the project, load it
      if not Project.Is_Empty then
         Load_Project
           (Self         => Self,
            Project_Path => Project,
            Context      => Context,
            Environment  => Environment,
            Charset      => Charset,
            Project_Type => LSP.Ada_Project_Loading.Configured_Project);
      end if;
   end Load_Project_With_Alire;

   ------------------------------------
   -- Mark_Source_Files_For_Indexing --
   ------------------------------------

   procedure Mark_Source_Files_For_Indexing
     (Self : in out Message_Handler'Class)
   is
      Files : LSP.Ada_Indexing.File_Sets.Set;

   begin
      --  Mark all the project's source files
      for C of Self.Contexts.Each_Context loop
         for F in C.List_Files loop
            Files.Include (LSP.Ada_File_Sets.File_Sets.Element (F));
         end loop;
      end loop;

      if Runtime_Indexing.Is_Active then
         --  Mark all the predefined sources too (runtime)
         for F in Self.Project_Predefined_Sources.Iterate loop
            declare
               File : GNATCOLL.VFS.Virtual_File renames
                 LSP.Ada_File_Sets.File_Sets.Element (F);
            begin
               for Context of Self.Contexts.Each_Context loop
                  Files.Include (File);
               end loop;
            end;
         end loop;
      end if;

      LSP.Ada_Indexing.Schedule_Indexing
        (Self.Server,
         Self'Unchecked_Access,
         Self.Configuration,
         Self.Project_Stamp,
         Files);
   end Mark_Source_Files_For_Indexing;

   ---------------------------------------
   -- Release_Contexts_And_Project_Info --
   ---------------------------------------

   procedure Release_Contexts_And_Project_Info
     (Self : in out Message_Handler'Class) is
   begin
      Self.Contexts.Cleanup;

      Self.Project_Tree.Unload;
      Self.Project_Predefined_Sources.Clear;
      Self.Project_Dirs_Loaded.Clear;

      Self.Project_Stamp := @ + 1;
   end Release_Contexts_And_Project_Info;

   ----------------------------------
   -- Reload_Implicit_Project_Dirs --
   ----------------------------------

   procedure Reload_Implicit_Project_Dirs
     (Self : in out Message_Handler'Class)
   is
      Project : GPR2.Project.Tree.View_Builder.Object :=
                  GPR2.Project.Tree.View_Builder.Create
                    (Project_Dir => GPR2.Path_Name.Create_Directory ("."),
                     Name        => "default");
      Values  : GPR2.Containers.Value_List;
      Opts    : GPR2.Options.Object;
      Success : Boolean;
   begin
      Self.Project_Tree.Unload;

      --  Load all the dirs

      for Dir of Self.Project_Dirs_Loaded loop
            Values.Append (Dir.Display_Full_Name);
      end loop;

      Project.Set_Attribute
        (GPR2.Project.Registry.Attribute.Source_Dirs,
        Values);

      --  First we load the fallback project
      Success := Self.Project_Tree.Load_Virtual_View
         (Project,
          Opts,
          With_Runtime     => True,
          Absent_Dir_Error => GPR2.No_Error);

      if not Success then
         for C in Self.Project_Tree.Log_Messages.Iterate loop
            Self.Tracer.Trace (C.Element.Format);
         end loop;
         LSP.Ada_Project_Loading.Set_Load_Status
           (Self.Project_Status, LSP.Ada_Project_Loading.Invalid_Project);
      end if;

      Self.Project_Tree.Update_Sources;

   exception
      when E : others =>
         Self.Tracer.Trace_Exception (E, "Reload_Implicit_Project_Dirs");
         LSP.Ada_Project_Loading.Set_Load_Status
           (Self.Project_Status, LSP.Ada_Project_Loading.Invalid_Project);
   end Reload_Implicit_Project_Dirs;

   --------------------
   -- Reload_Project --
   --------------------

   procedure Reload_Project (Self : in out Message_Handler'CLass) is
      Project_File : VSS.Strings.Virtual_String :=
        Self.Configuration.Project_File;
   begin
      if Project_File.Starts_With ("file://") then
         Project_File := VSS.Strings.Conversions.To_Virtual_String
           (URIs.Conversions.To_File
              (VSS.Strings.Conversions.To_UTF_8_String (Project_File), True));
      end if;

      if Project_File.Is_Empty then
         Release_Contexts_And_Project_Info (Self);
         Ensure_Project_Loaded (Self);
      else
         Load_Project_With_Alire
           (Self,
            Project_File,
            Self.Configuration.Context,
            Self.Configuration.Charset);
      end if;
   end Reload_Project;

   ---------------------------------------
   -- Update_Project_Predefined_Sources --
   ---------------------------------------

   procedure Update_Project_Predefined_Sources
     (Self : in out Message_Handler'Class)
   is
      use GPR2;
      use GPR2.Build.Source.Sets;
   begin
      Self.Project_Predefined_Sources.Clear;

      if Self.Project_Tree.Is_Defined
        and then Self.Project_Tree.Has_Runtime_Project
      then
         for Source of Self.Project_Tree.Runtime_Project.Sources loop
            if Source.Language = GPR2.Ada_Language then
               Self.Project_Predefined_Sources.Include
                 (Source.Path_Name.Virtual_File);
            end if;
         end loop;
      end if;
   end Update_Project_Predefined_Sources;

end LSP.Ada_Handlers.Project_Loading;
