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

with GPR2;
with GPR2.Environment;
with GPR2.Options;
with GPR2.Containers;
with GPR2.Context;
with GPR2.Log;
with GPR2.Message;
with GPR2.Path_Name;
with GPR2.Project.Registry.Attribute;
with GPR2.Project.Tree.View_Builder;
with GPR2.Project.View;
with GPR2.Reporter;

pragma Warnings (Off, "unit ""GPR2.Build.Source.Sets"" is not referenced");
with GPR2.Build.Source.Sets;

with Libadalang.Preprocessing;
with Libadalang.Project_Provider;
with LSP.Ada_Contexts;
with LSP.Ada_Context_Sets;
with LSP.Ada_Documents; use LSP.Ada_Documents;
with LSP.Alire;
with LSP.Ada_Handlers.File_Readers;
with LSP.Ada_Indexing;
with LSP.Ada_Project_Loading;
with LSP.GNATCOLL_Tracers;
with LSP.Utils;

with URIs;

with VSS.Characters;
with VSS.Characters.Latin;
with VSS.Strings;
with VSS.Strings.Conversions;

package body LSP.Ada_Handlers.Project_Loading is

   Tracer : constant LSP.GNATCOLL_Tracers.Tracer :=
     LSP.GNATCOLL_Tracers.Create ("ALS.PROJECT", GNATCOLL.Traces.On);

   Runtime_Indexing : constant GNATCOLL.Traces.Trace_Handle :=
     GNATCOLL.Traces.Create ("ALS.RUNTIME_INDEXING",
                             GNATCOLL.Traces.On);
   --  Trace to enable/disable runtime indexing. Useful for the testsuite.

   type GPR2_Reporter is new GPR2.Reporter.Object with record
      Log : GPR2.Log.Object;
   end record;

   overriding procedure Internal_Report
     (Self : in out GPR2_Reporter;
      Msg  : GPR2.Message.Object);

   overriding function Verbosity
     (Self : GPR2_Reporter) return GPR2.Reporter.Verbosity_Level
   is (GPR2.Reporter.Regular);

   LF : VSS.Characters.Virtual_Character renames VSS.Characters.Latin.Line_Feed;

   procedure Load_Project
     (Self            : in out Message_Handler'Class;
      Project_Path    : VSS.Strings.Virtual_String;
      GPR_Config_Path : VSS.Strings.Virtual_String;
      Context         : GPR2.Context.Object;
      Environment     : GPR2.Environment.Object;
      Charset         : VSS.Strings.Virtual_String);
   --  Attempt to load the given project file, with the scenario provided.
   --  This unloads all currently loaded project contexts. This factorizes code
   --  between Load_Project_With_Alire and Ensure_Project_Loaded.

   procedure Load_Implicit_Project
     (Self   : in out Message_Handler'Class;
      Status : LSP.Ada_Project_Loading.Project_Status);
   --  Load the implicit project

   procedure Release_Contexts_And_Project_Info
     (Self : in out Message_Handler'Class);
   --  Release the memory associated to project information in Self

   procedure Reload_Implicit_Project_Dirs
     (Self : in out Message_Handler'Class);
   --  Reload as project source dirs the directories in
   --  Self.Project_Dirs_Loaded.

   procedure Update_Project_Predefined_Sources
     (Self : in out Message_Handler'Class);
   --  Fill Self.Project_Predefined_Sources with loaded project tree runtime

   procedure Enqueue_Indexing_Job
     (Self : in out Message_Handler'Class);
   --  Enqueue the indexing job, which indexes all the project's sources.
   --  This also indexes immediately any already opened document, creating
   --  the handler's fallback context before for that purpose.

   ---------------------------
   -- Ensure_Project_Loaded --
   ---------------------------

   procedure Ensure_Project_Loaded (Self : in out Message_Handler'Class) is
      use type VSS.Strings.Virtual_String;

      GPRs_Found   : Natural := 0;
      Project_File : VSS.Strings.Virtual_String :=
        Self.Configuration.Project_File;
      GPR_Configuration_File : VSS.Strings.Virtual_String :=
        Self.Configuration.GPR_Configuration_File;
      Is_Alire_Crate : constant Boolean := Alire.Is_Alire_Crate (Self.Client);
      Has_Alire : Boolean;
      Alire_Errors : VSS.Strings.Virtual_String;

   begin
      if not Self.Contexts.Is_Empty then
         --  Rely on the fact that there is at least one context initialized
         --  as a guarantee that the initialization has been done.
         return;
      end if;

      --  First consider ada.projectFile
      if not Project_File.Is_Empty then
         Tracer.Trace_Text ("Found ada.projectFile = " & Project_File);

         if Project_File.Starts_With ("file://") then
            Project_File :=
              VSS.Strings.Conversions.To_Virtual_String
                (URIs.Conversions.To_File
                   (VSS.Strings.Conversions.To_UTF_8_String (Project_File),
                    True));
         end if;

         --  Report how we found the project
         Self.Project_Status.Set_Project_Type
           (LSP.Ada_Project_Loading.Configured_Project);

      elsif Is_Alire_Crate then
         Tracer.Trace ("Workspace is an Alire crate");
         Tracer.Trace ("Determining project from 'alr show' output");

         LSP.Alire.Determine_Alire_Project
           (Root      => Self.Client.Root_Directory.Display_Full_Name,
            Has_Alire => Has_Alire,
            Error     => Alire_Errors,
            Project   => Project_File);

         if not Has_Alire then
            Tracer.Trace
              ("'alr' is not available on PATH, cannot determine project from Alire."
               & " Falling back to other methods for finding a project file.");
            --  TODO consider reporting this situation to the client as a
            --  diagnostic instead of just a log trace
         elsif not Alire_Errors.Is_Empty then
            Tracer.Trace_Text ("Encountered errors: " & Alire_Errors);
         else
            --  Report how we found the project
            Self.Project_Status.Set_Project_Type
              (LSP.Ada_Project_Loading.Alire_Project);
         end if;
      end if;

      --  If still haven't found a project, try to find a unique project at the root
      if Project_File.Is_Empty then
         Tracer.Trace ("Looking for a unique project at the root");
         declare
            Files : GNATCOLL.VFS.File_Array_Access :=
              Self.Client.Root_Directory.Read_Dir (GNATCOLL.VFS.Files_Only);
            Found : GNATCOLL.VFS.Virtual_File;
         begin
            for X of Files.all loop
               if X.Has_Suffix (".gpr") then
                  GPRs_Found := GPRs_Found + 1;
                  exit when GPRs_Found > 1;
                  Found := X;
               end if;
            end loop;

            GNATCOLL.VFS.Unchecked_Free (Files);

            if GPRs_Found = 1 then
               Project_File := LSP.Utils.To_Virtual_String (Found);

               --  Report how we found the project
               Self.Project_Status.Set_Project_Type
                 (LSP.Ada_Project_Loading.Single_Project_Found);

               Tracer.Trace_Text ("Found unique project: " & Project_File);
            else
               Tracer.Trace
                 ("Found " & GPRs_Found'Image & " projects at the root");
            end if;
         end;
      end if;

      --  At this stage we tried everything to find a project file. Now let's try to load.
      if not Project_File.Is_Empty then
         declare
            Environment : GPR2.Environment.Object :=
              GPR2.Environment.Process_Environment;

            Charset : constant VSS.Strings.Virtual_String :=
              (if not Self.Configuration.Charset.Is_Empty
               then Self.Configuration.Charset
               elsif Is_Alire_Crate
               then
                 VSS.Strings.Virtual_String'
                   ("utf-8")  --  Alire projects tend to prefer utf-8
               else
                 "iso-8859-1");

            Errors : VSS.Strings.Virtual_String;
         begin
            if Is_Alire_Crate then
               if LSP.Alire.Should_Setup_Alire_Env (Self.Client) then
                  Tracer.Trace ("Setting environment from 'alr printenv'");

                  LSP.Alire.Setup_Alire_Env
                    (Root        =>
                       Self.Client.Root_Directory.Display_Full_Name,
                     Has_Alire   => Has_Alire,
                     Error       => Errors,
                     Environment => Environment);

                  if not Errors.Is_Empty then
                     Tracer.Trace_Text
                       ("Encountered errors with Alire:" & LF & Errors);
                  end if;
               else
                  Tracer.Trace
                    ("Alire environment is already set up. Not calling 'alr printenv'.");
               end if;
            end if;

            if GPR_Configuration_File.Starts_With ("file://") then
               GPR_Configuration_File :=
                 VSS.Strings.Conversions.To_Virtual_String
                   (URIs.Conversions.To_File
                      (VSS.Strings.Conversions.To_UTF_8_String
                         (GPR_Configuration_File), True));
            end if;

            Load_Project
              (Self            => Self,
               Project_Path    => Project_File,
               GPR_Config_Path => GPR_Configuration_File,
               Context         => Self.Configuration.Context,
               Environment     => Environment,
               Charset         => Charset);
         end;
      else
         --  We didn't find a project file. Let's load an implicit project. We
         --  reach this point either because there are no GPR projects at the
         --  root, or there are more than one.
         pragma Assert (GPRs_Found = 0 or GPRs_Found > 1);

         Load_Implicit_Project
           (Self,
            (if GPRs_Found = 0 then LSP.Ada_Project_Loading.No_Project
             elsif GPRs_Found > 1
             then LSP.Ada_Project_Loading.Multiple_Projects
             else LSP.Ada_Project_Loading.Project_Not_Found));
      end if;

      --  By this point we must have a context: either a project was
      --  successfully loaded, or project loading failed and we created a dummy
      --  context to avoid retrying.
      pragma Assert (not Self.Contexts.Is_Empty);

      --  Publish workspace diagnostics every time a project might have
      --  been loaded/reloaded.
      Self.Publish_Diagnostics (Force => True);
   end Ensure_Project_Loaded;

   ---------------------
   -- Internal_Report --
   ---------------------

   overriding procedure Internal_Report
     (Self : in out GPR2_Reporter;
      Msg  : GPR2.Message.Object) is
   begin
      Self.Log.Append (Msg);
   end Internal_Report;

   ---------------------------
   -- Load_Implicit_Project --
   ---------------------------

   procedure Load_Implicit_Project
     (Self   : in out Message_Handler'Class;
      Status : LSP.Ada_Project_Loading.Project_Status)
   is
      use LSP.Ada_Context_Sets;
      use LSP.Ada_Contexts;
      use type GNATCOLL.VFS.Virtual_File;

      C : constant Context_Access := new Context (Self.Tracer);

      Reader : LSP.Ada_Handlers.File_Readers.LSP_File_Reader
        (Self'Unchecked_Access);
      Root   : GNATCOLL.VFS.Virtual_File;
   begin
      Tracer.Trace
        ("Loading the implicit project because " & Status'Image);

      LSP.Ada_Project_Loading.Set_Project_Type
        (Self.Project_Status, LSP.Ada_Project_Loading.Implicit_Project);
      LSP.Ada_Project_Loading.Set_Load_Status (Self.Project_Status, Status);
      Release_Contexts_And_Project_Info (Self);

      C.Initialize
        (File_Reader     => Reader,
         Follow_Symlinks => Self.Configuration.Follow_Symlinks,
         Style           => Self.Configuration.Documentation_Style);

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

      Root := Self.Client.Root_Directory;
      if Root /= GNATCOLL.VFS.No_File then
         Self.Project_Dirs_Loaded.Include (Root);
      end if;

      Reload_Implicit_Project_Dirs (Self);

      --  Create a basic GPR2_Provider_And_Projects containing only the
      --  implicit project and load it.
      declare
         Provider : Libadalang.Project_Provider.GPR2_Provider_And_Projects :=
           (Provider =>
              Libadalang.Project_Provider.Create_Project_Unit_Provider
                (Tree    => Self.Project_Tree,
                 Project => Self.Project_Tree.Root_Project),
            Projects => <>);
      begin
         Provider.Projects.Append (Self.Project_Tree.Root_Project);
         C.Load_Project
           (Provider => Provider,
            Tree     => Self.Project_Tree,
            Charset  => "iso-8859-1");
      end;

      Update_Project_Predefined_Sources (Self);

      Self.Contexts.Prepend (C);

      --  Reindex the files from disk in the background after a project reload
      Enqueue_Indexing_Job (Self);
   end Load_Implicit_Project;

   ------------------
   -- Load_Project --
   ------------------

   procedure Load_Project
     (Self            : in out Message_Handler'Class;
      Project_Path    : VSS.Strings.Virtual_String;
      GPR_Config_Path : VSS.Strings.Virtual_String;
      Context         : GPR2.Context.Object;
      Environment     : GPR2.Environment.Object;
      Charset         : VSS.Strings.Virtual_String)
   is
      use Libadalang.Project_Provider;
      use type VSS.Strings.Virtual_String;
      use type GNATCOLL.VFS.Virtual_File;

      Project_File    : GNATCOLL.VFS.Virtual_File :=
        LSP.Utils.To_Virtual_File (Project_Path);

      GPR_Config_File : GNATCOLL.VFS.Virtual_File;

      Root            : GNATCOLL.VFS.Virtual_File;

      procedure Create_Context_For_Non_Aggregate
        (View     : GPR2.Project.View.Object;
         Provider : GPR2_Provider_And_Projects);
      --  Create a new context for the given project view.
      --  It will use the same LAL provider (Provider.Provider) for the all the
      --  subprojects (Provider.Projects)

      procedure Log_GPR2_Diagnostics;
      --  Log the GPR2 messages

      --------------------------------------
      -- Create_Context_For_Non_Aggregate --
      --------------------------------------

      procedure Create_Context_For_Non_Aggregate
        (View     : GPR2.Project.View.Object;
         Provider : GPR2_Provider_And_Projects)
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
           (Provider => Provider,
            Tree     => Self.Project_Tree,
            Charset  => VSS.Strings.Conversions.To_UTF_8_String (Charset));

         Tracer.Trace ("Prepend Context Id: "
                       & VSS.Strings.Conversions.To_UTF_8_String (C.Id));
         Self.Contexts.Prepend (C);
      end Create_Context_For_Non_Aggregate;

      --------------------------
      -- Log_GPR2_Diagnostics --
      --------------------------

      procedure Log_GPR2_Diagnostics is
         Log : constant GPR2.Log.Object'Class :=
            GPR2_Reporter (Self.Project_Tree.Reporter.Element.all).Log;
      begin
         Tracer.Increase_Indent;
         if Log.Is_Empty then
            Tracer.Trace ("No GPR2 messages");
         else
            for Msg of Log loop
               declare
                  Message : constant String := Msg.Format (Full_Path_Name => True);
               begin
                  Tracer.Trace (Message);
               end;
            end loop;
         end if;
         Tracer.Decrease_Indent;
      end Log_GPR2_Diagnostics;

   begin
      Tracer.Trace_Text ("Loading project: " & Project_Path);

      --  The projectFile may be either an absolute path or a
      --  relative path; if so, we're assuming it's relative
      --  to Self.Root.

      Root := Self.Client.Root_Directory;

      if not Project_File.Is_Absolute_Path
        and then Root /= GNATCOLL.VFS.No_File
      then
         Project_File := GNATCOLL.VFS.Join (Root, Project_File);
      end if;

      if not GPR_Config_Path.Is_Empty then
         GPR_Config_File := LSP.Utils.To_Virtual_File (GPR_Config_Path);

         if not GPR_Config_File.Is_Absolute_Path
           and then Root /= GNATCOLL.VFS.No_File
         then
            GPR_Config_File := GNATCOLL.VFS.Join (Root, GPR_Config_File);
         end if;
      end if;

      --  Unload the project tree and the project environment
      Release_Contexts_And_Project_Info (Self);

      Self.Project_Status.Set_Project_File (Project_File);

      if not Project_File.Is_Regular_File then
         Tracer.Trace
           ("The project set in the configuration doesn't exist: "
            & Project_File.Display_Full_Name);
         Self.Project_Status.Set_Load_Status
           (LSP.Ada_Project_Loading.Project_Not_Found);

         --  Index source files and already opened files.
         Enqueue_Indexing_Job (Self);
         return;
      end if;

      --  Set Valid Status for now, it can be overwritten in case of errors
      Self.Project_Status.Set_Load_Status (LSP.Ada_Project_Loading.Valid_Project);

      declare
         Opts     : GPR2.Options.Object;
         Success  : Boolean;

         Reporter : GPR2_Reporter;
         --  This reporter object is passed to the GPR2 Load function, but it
         --  does not get populated. To obtain the messages, one has to access
         --  Self.Project_Tree.Reporter.
      begin
         --  Load the project
         Opts.Add_Switch (GPR2.Options.P, Project_File.Display_Full_Name);

         if GPR_Config_File /= GNATCOLL.VFS.No_File then
            Opts.Add_Switch
              (GPR2.Options.Config, GPR_Config_File.Display_Full_Name);
         end if;

         Opts.Add_Context (Context);

         Tracer.Trace ("Loading project with GPR2");

         Success := Self.Project_Tree.Load
           (Opts,
            Reporter         => Reporter,
            With_Runtime     => True,
            Absent_Dir_Error => GPR2.No_Error,
            Environment      => Environment);

         Tracer.Trace ("GPR2 messages after load:");
         Log_GPR2_Diagnostics;

         if not Success then
            Self.Project_Status.Set_Load_Status (LSP.Ada_Project_Loading.Invalid_Project);
         end if;

         if Success then
            Tracer.Trace ("Updating project sources");
            Self.Project_Tree.Update_Sources;
            Tracer.Trace ("GPR2 messages after updating sources:");
            Log_GPR2_Diagnostics;
         end if;

         LSP.Ada_Project_Loading.Set_GPR2_Messages
           (Self.Project_Status,
            GPR2_Reporter (Self.Project_Tree.Reporter.Element.all).Log);

      exception
         when E : others =>
            Tracer.Trace_Exception (E);
            Self.Project_Status.Set_Load_Status (LSP.Ada_Project_Loading.Invalid_Project);
      end;

      if Self.Project_Status.Is_Project_Loaded then
         --  No exception during Load_Autoconf, check if we have the runtime
         declare
            Root : constant GPR2.Project.View.Object :=
              Self.Project_Tree.Root_Project;
         begin
            --  Only check runtime issues for Ada
            Self.Project_Status.Set_Missing_Ada_Runtime (
               Value   =>
                 (not Root.Is_Defined
                  or else Root.Language_Ids.Contains (GPR2.Ada_Language))
               and then not Self.Project_Tree.Has_Runtime_Project);
         end;

         Update_Project_Predefined_Sources (Self);

         if Self.Project_Tree.Root_Project.Kind in GPR2.Aggregate_Kind then
            --  For aggregated root project, use LAL to create sets of
            --  aggregated projects and sub-projects which can coexist in
            --  the same LAL provider to reduce the memory footprint.
            declare
               Providers : GPR2_Provider_And_Projects_Array_Access :=
                 Create_Project_Unit_Providers (Self.Project_Tree);
            begin
               for Provider of Providers.all loop
                  Create_Context_For_Non_Aggregate
                    (View     => Self.Project_Tree.Root_Project,
                     Provider => Provider);
               end loop;
               Free (Providers);
            end;
         else
            declare
               Provider : GPR2_Provider_And_Projects :=
                 (Provider => Create_Project_Unit_Provider
                    (Tree    => Self.Project_Tree,
                     Project => Self.Project_Tree.Root_Project),
                  Projects => <>);
            begin
               Provider.Projects.Append (Self.Project_Tree.Root_Project);
               Create_Context_For_Non_Aggregate
                 (View     => Self.Project_Tree.Root_Project,
                  Provider => Provider);
            end;
         end if;
      end if;

      Tracer.Trace ("Project status after loading: " & Self.Project_Status'Image);

      --  We have successfully loaded a real project: monitor the filesystem
      --  for any changes on the sources of the project
      Self.File_Monitor.Monitor_Directories
        (Self.Contexts.All_Source_Directories);

      --  Index source files and already opened files.
      Enqueue_Indexing_Job (Self);
   end Load_Project;

   --------------------------
   -- Enqueue_Indexing_Job --
   --------------------------

   procedure Enqueue_Indexing_Job
     (Self : in out Message_Handler'Class)
   is
      procedure Create_Fallback_Context (Self : in out Message_Handler'Class);
      --  Create a fallback context for the given handler's contexts' set.

      -----------------------------
      -- Create_Fallback_Context --
      -----------------------------

      procedure Create_Fallback_Context (Self : in out Message_Handler'Class)
      is
         use LSP.Ada_Context_Sets;
         use LSP.Ada_Contexts;
      begin
         declare
            C      : constant Context_Access := new Context (Self.Tracer);
            Reader :
              LSP.Ada_Handlers.File_Readers.LSP_File_Reader
                (Self'Unchecked_Access);
         begin
            Self.Tracer.Trace_Text ("Creating fallback context");

            C.Initialize
              (File_Reader         => Reader,
               Follow_Symlinks     => Self.Configuration.Follow_Symlinks,
               Style               => Self.Configuration.Documentation_Style,
               As_Fallback_Context => True);
            Self.Contexts.Prepend (C);
         end;
      end Create_Fallback_Context;

      Files : LSP.Ada_Indexing.File_Sets.Set;
   begin
      --  Create a fallback context before indexing. This allows to
      --  have a context to index already opened non-source files, if any.
      Create_Fallback_Context (Self);

      for Document of Self.Open_Documents loop
         --  Reindex all open documents immediately after project reload,
         --  so that navigation from editors is accurate.
         for Context of Self.Contexts_For_URI (Document.URI) loop
            Context.Index_Document (Document.all);
         end loop;

         --  Refresh the diagnostics
         Self.Publish_Diagnostics
           (LSP.Ada_Documents.Document_Access (Document));
      end loop;

      --  Mark all the project's source files
      for C of Self.Contexts.Each_Context loop
         for F in C.List_Files loop
            Files.Include (LSP.Ada_File_Sets.File_Sets.Element (F));
         end loop;
      end loop;

      --  Avoid indexing the runtime in case of aggregate projects:
      --  runtime files are present in all contexts, indexing them
      --  in all contexts would cost too much memory
      if Runtime_Indexing.Is_Active
        and then Self.Project_Tree.Is_Defined
        and then Self.Project_Tree.Root_Project.Kind not in GPR2.Aggregate_Kind
      then
         --  Mark all the predefined sources too (runtime)
         for F in Self.Project_Predefined_Sources.Iterate loop
            Files.Include (LSP.Ada_File_Sets.File_Sets.Element (F));
         end loop;
      end if;

      --  Schedule the indexing job
      LSP.Ada_Indexing.Schedule_Indexing
        (Server        => Self.Server,
         Handler       => Self'Unchecked_Access,
         Configuration => Self.Configuration,
         Project_Stamp => Self.Project_Stamp,
         Files         => Files);
   end Enqueue_Indexing_Job;

   ---------------------------------------
   -- Release_Contexts_And_Project_Info --
   ---------------------------------------

   procedure Release_Contexts_And_Project_Info
     (Self : in out Message_Handler'Class) is
   begin
      Self.Contexts.Cleanup;

      Self.Project_Tree.Unload;
      Self.Project_Predefined_Sources.Clear;

      --  Intentionally do not clear Project_Dirs_Loaded: these
      --  are the directories that we load on-the-fly when are
      --  working without a project: keep all of them in memory.
      --  Self.Project_Dirs_Loaded.Clear;

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
      Release_Contexts_And_Project_Info (Self);
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
            Tracer.Trace (C.Element.Format);
         end loop;
         LSP.Ada_Project_Loading.Set_Load_Status
           (Self.Project_Status, LSP.Ada_Project_Loading.Invalid_Project);
      end if;

      Self.Project_Tree.Update_Sources;

   exception
      when E : others =>
         Tracer.Trace_Exception (E, "Reload_Implicit_Project_Dirs");
         LSP.Ada_Project_Loading.Set_Load_Status
           (Self.Project_Status, LSP.Ada_Project_Loading.Invalid_Project);
   end Reload_Implicit_Project_Dirs;

   --------------------
   -- Reload_Project --
   --------------------

   procedure Reload_Project (Self : in out Message_Handler'CLass) is
   begin
      Tracer.Trace ("Reload_Project was called. Releasing current project context.");
      Release_Contexts_And_Project_Info (Self);
      Self.Project_Status := LSP.Ada_Project_Loading.No_Project_Status;
      Ensure_Project_Loaded (Self);
   end Reload_Project;

   -----------------------------
   -- Reload_Implicit_Project --
   -----------------------------

   procedure Reload_Implicit_Project
     (Self : in out Message_Handler'Class) is
   begin
      Load_Implicit_Project (Self, Self.Project_Status.Get_Load_Status);
   end Reload_Implicit_Project;

   ---------------------------------------
   -- Update_Project_Predefined_Sources --
   ---------------------------------------

   procedure Update_Project_Predefined_Sources
     (Self : in out Message_Handler'Class)
   is
      use GPR2;

      procedure For_All_Part_Action
        (Kind     : Unit_Kind;
         View     : GPR2.Project.View.Object;
         Path     : Path_Name.Object;
         Index    : Unit_Index;
         Sep_Name : Optional_Name_Type);

      -------------------------
      -- For_All_Part_Action --
      -------------------------

      procedure For_All_Part_Action
        (Kind     : Unit_Kind;
         View     : GPR2.Project.View.Object;
         Path     : Path_Name.Object;
         Index    : Unit_Index;
         Sep_Name : Optional_Name_Type)
      is
         pragma Unreferenced (Kind);
         pragma Unreferenced (View);
         pragma Unreferenced (Index);
         pragma Unreferenced (Sep_Name);
      begin
         Self.Project_Predefined_Sources.Include (Path.Virtual_File);
      end For_All_Part_Action;
   begin
      Self.Project_Predefined_Sources.Clear;

      if Self.Project_Tree.Is_Defined
        and then Self.Project_Tree.Has_Runtime_Project
      then
         --  Note that the following loop differs rather subtly from iterating
         --  over the units in the runtime view: user projects are allowed to
         --  override units from the runtime, and when they do the overridden
         --  units should be ignored. We would incorrectly consider them if we
         --  just iterated over the units of the runtime view.
         for P of Self.Project_Tree.Namespace_Root_Projects loop
            for Unit of P.Units (With_Externally_Built => True) loop
               if Unit.Owning_View.Is_Runtime then
                  Unit.For_All_Part (For_All_Part_Action'Access);
               end if;
            end loop;
         end loop;
      end if;
   end Update_Project_Predefined_Sources;

end LSP.Ada_Handlers.Project_Loading;
