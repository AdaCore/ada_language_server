------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2018-2023, AdaCore                     --
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

with GPR2.Context;
with GPR2.Path_Name;
with GPR2.Project.View;
with GPR2.Containers;
with GPR2.Message;
with GPR2.Project.Registry.Attribute;
with GPR2.Project.Source.Set;
with GPR2.Project.Tree.View_Builder;

with Libadalang.Preprocessing;

with VSS.Characters.Latin;
with VSS.Strings.Conversions;
with VSS.String_Vectors;

with Spawn.Environments;

with LSP.Ada_Contexts;
with LSP.Ada_Context_Sets;
with LSP.Ada_Handlers.Alire;
with LSP.Ada_Handlers.File_Readers;
with LSP.Ada_Indexing;
with LSP.Enumerations;
with LSP.Structures;

with URIs;

package body LSP.Ada_Handlers.Project_Loading is

   Runtime_Indexing : constant GNATCOLL.Traces.Trace_Handle :=
     GNATCOLL.Traces.Create ("ALS.RUNTIME_INDEXING",
                             GNATCOLL.Traces.On);
   --  Trace to enable/disable runtime indexing. Useful for the testsuite.

   procedure Load_Project_With_Alire
     (Self               : in out Message_Handler'Class;
      Project_File       : VSS.Strings.Virtual_String := "";
      Scenario_Variables : LSP.Ada_Configurations.Variable_List;
      Charset            : VSS.Strings.Virtual_String);
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

   procedure Load_Implicit_Project
     (Self   : in out Message_Handler'Class;
      Status : Implicit_Project_Loaded);
   --  Load the implicit project

   procedure Update_Project_Predefined_Sources
     (Self : in out Message_Handler'Class);
   --  Fill Self.Project_Predefined_Sources with loaded project tree runtime

   procedure Mark_Source_Files_For_Indexing
     (Self : in out Message_Handler'Class);
   --  Mark all sources in all projects for indexing. This factorizes code
   --  between Load_Project and Load_Implicit_Project.

   function To_Virtual_String
     (Value : GNATCOLL.VFS.Virtual_File) return VSS.Strings.Virtual_String is
       (VSS.Strings.Conversions.To_Virtual_String (Value.Display_Full_Name));
   --  Cast Virtual_File to Virtual_String

   function Root
     (Self : Message_Handler'Class) return GNATCOLL.VFS.Virtual_File;
   --  Return the root directory of the client workspace

   type Environment is record
      Context    : GPR2.Context.Object := GPR2.Context.Empty;
      Build_Path : GPR2.Path_Name.Object := GPR2.Path_Name.Undefined;
   end record;

   ---------------------------
   -- Ensure_Project_Loaded --
   ---------------------------

   procedure Ensure_Project_Loaded (Self : in out Message_Handler'Class) is
      use type VSS.Strings.Virtual_String;

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
        (Self                => Self,
         Project_File        => VSS.Strings.Empty_Virtual_String,
         Scenario_Variables  => Self.Configuration.Scenario_Variables,
         Charset             => Self.Configuration.Charset);

      if not Self.Contexts.Is_Empty then
         --  Some project was found by alire and loaded. We are done!
         return;
      end if;

      --  We don't have alire/crate.
      --  We're going to look for a project in Root: list all the files
      --  in this directory, looking for .gpr files.

      if not Self.Client.Root.Is_Empty then
         declare
            Files : GNATCOLL.VFS.File_Array_Access :=
              Root (Self).Read_Dir (GNATCOLL.VFS.Files_Only);
         begin
            for X of Files.all loop
               if X.Has_Suffix (".gpr") then
                  GPRs_Found := GPRs_Found + 1;
                  exit when GPRs_Found > 1;
                  Project_File := To_Virtual_String (X);
               end if;
            end loop;

            GNATCOLL.VFS.Unchecked_Free (Files);
         end;
      end if;

      --  What we do depends on the number of .gpr files found:

      if GPRs_Found = 0 then
         --  We have found zero .gpr files: load the implicit project

         Load_Implicit_Project (Self, No_Project_Found);
      elsif GPRs_Found = 1 then
         --  We have found exactly one .gpr file: let's load it.
         Self.Tracer.Trace ("Loading:");
         Self.Tracer.Trace_Text (Project_File);

         Load_Project
           (Self,
            Project_File,
            Self.Configuration.Scenario_Variables,
            GPR2.Environment.Process_Environment,
            "iso-8859-1",
            Single_Project_Found);
      else
         --  We have found more than one project: warn the user!

         Self.Sender.On_ShowMessage_Notification
           ((a_type  => LSP.Enumerations.Error,
             message =>
               "More than one .gpr found."
                  & VSS.Characters.Latin.Line_Feed
                  & "Note: you can configure a project "
                  & " through the ada.projectFile setting."));

         Load_Implicit_Project (Self, Multiple_Projects_Found);
      end if;
   end Ensure_Project_Loaded;

   ---------------------------
   -- Load_Implicit_Project --
   ---------------------------

   procedure Load_Implicit_Project
     (Self   : in out Message_Handler'Class;
      Status : Implicit_Project_Loaded)
   is
      use LSP.Ada_Context_Sets;
      use LSP.Ada_Contexts;

      C : constant Context_Access := new Context (Self.Tracer);

      Reader : LSP.Ada_Handlers.File_Readers.LSP_File_Reader
        (Self'Unchecked_Access);
   begin
      Self.Tracer.Trace ("Loading the implicit project");

      Self.Project_Status := Status;
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
         Self.Project_Dirs_Loaded.Include (Root (Self));
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
      Project_File : VSS.Strings.Virtual_String;
      Scenario     : LSP.Ada_Configurations.Variable_List;
      Environment  : GPR2.Environment.Object;
      Charset      : VSS.Strings.Virtual_String;
      Status       : Load_Project_Status)
   is
      use type GNATCOLL.VFS.Virtual_File;

      Message  : LSP.Structures.ShowMessageParams;
      Errors   : VSS.String_Vectors.Virtual_String_Vector;
      Warnings : VSS.String_Vectors.Virtual_String_Vector;

      procedure Create_Context_For_Non_Aggregate
        (View : GPR2.Project.View.Object);

      procedure Append_Errors;

      function To_Virtual_File (Value : VSS.Strings.Virtual_String)
         return GNATCOLL.VFS.Virtual_File is
           (GNATCOLL.VFS.Create_From_UTF8
             (VSS.Strings.Conversions.To_UTF_8_String (Value)));
      --  Cast Virtual_String to Virtual_File

      -------------------
      -- Append_Errors --
      -------------------

      procedure Append_Errors is
      begin
         for Message of Self.Project_Tree.Log_Messages.all loop
            case Message.Level is
               when GPR2.Message.Error =>
                  Errors.Append
                    (VSS.Strings.Conversions.To_Virtual_String
                       (Message.Format));
               when GPR2.Message.Warning =>
                  Warnings.Append
                    (VSS.Strings.Conversions.To_Virtual_String
                       (Message.Format));
               when others =>
                  null;
            end case;
         end loop;
      end Append_Errors;

      --------------------------------------
      -- Create_Context_For_Non_Aggregate --
      --------------------------------------

      procedure Create_Context_For_Non_Aggregate
        (View : GPR2.Project.View.Object)
      is
         use LSP.Ada_Context_Sets;
         use LSP.Ada_Contexts;

         C : constant Context_Access := new Context (Self.Tracer);

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

         Self.Contexts.Prepend (C);
      end Create_Context_For_Non_Aggregate;

      GPR                 : GNATCOLL.VFS.Virtual_File :=
        To_Virtual_File (Project_File);

      Project_Environment : Project_Loading.Environment;

      Relocate_Build_Tree : constant GNATCOLL.VFS.Virtual_File :=
        To_Virtual_File (Self.Configuration.Relocate_Build_Tree);

      Root_Dir            : constant GNATCOLL.VFS.Virtual_File :=
        To_Virtual_File (Self.Configuration.Relocate_Root);

   begin
      --  The projectFile may be either an absolute path or a
      --  relative path; if so, we're assuming it's relative
      --  to Self.Root.

      if not GPR.Is_Absolute_Path and then not Self.Client.Root.Is_Empty then
         GPR := GNATCOLL.VFS.Join (Root (Self), GPR);
      end if;

      --  Unload the project tree and the project environment
      Release_Contexts_And_Project_Info (Self);

      --  Now load the new project
      Self.Project_Status := Status;

      if not Self.Configuration.Relocate_Build_Tree.Is_Empty then
         Project_Environment.Build_Path :=
           GPR2.Path_Name.Create (Relocate_Build_Tree);

         if not Self.Configuration.Relocate_Root.Is_Empty
           and then GPR /= GNATCOLL.VFS.No_File
         then
            if not Root_Dir.Is_Absolute_Path then
               Project_Environment.Build_Path :=
                 GPR2.Path_Name.Create_Directory
                   (GPR2.Path_Name.Create (GPR).Relative_Path
                     (GPR2.Path_Name.Create (Root_Dir)).Name,
                      GPR2.Filename_Type
                       (Project_Environment.Build_Path.Value));
            end if;
         end if;
      end if;

      --  Update scenario variables with user provided values
      for J in 1 .. Scenario.Names.Length loop
         Project_Environment.Context.Insert
           (GPR2.Optional_Name_Type
              (VSS.Strings.Conversions.To_UTF_8_String (Scenario.Names (J))),
            VSS.Strings.Conversions.To_UTF_8_String (Scenario.Values (J)));
      end loop;

      begin
         Self.Project_Tree.Load_Autoconf
           (Filename    => GPR2.Path_Name.Create (GPR),
            Context     => Project_Environment.Context,
            Build_Path  => Project_Environment.Build_Path,
            Environment => Environment);

         Self.Project_Tree.Update_Sources (With_Runtime => True);

      exception
         when E : GPR2.Project_Error
                | GPR2.Processing_Error
                | GPR2.Attribute_Error =>

            Self.Tracer.Trace_Exception (E);

            Self.Project_Status := Invalid_Project_Configured;
      end;

      --  Keep errors and warnings
      Append_Errors;

      if Self.Project_Status /= Status
        or else not Self.Project_Tree.Is_Defined
      then
         --  The project was invalid: fallback on loading the implicit project.
         Errors.Prepend
           (VSS.Strings.Conversions.To_Virtual_String
              ("Unable to load project file: " & GPR.Display_Full_Name));

         Load_Implicit_Project (Self, Invalid_Project_Configured);

      else
         --  No exception during Load_Autoconf, check if we have runtime
         if not Self.Project_Tree.Has_Runtime_Project then
            Self.Project_Status := No_Runtime_Found;
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

      --  Report the warnings, if any
      if not Warnings.Is_Empty then
         Message.message := Warnings.Join_Lines (VSS.Strings.LF);
         Message.a_type := LSP.Enumerations.Warning;
         Self.Sender.On_ShowMessage_Notification (Message);
      end if;

      --  Report the errors, if any
      if not Errors.Is_Empty then
         Message.message := Errors.Join_Lines (VSS.Strings.LF);
         Message.a_type := LSP.Enumerations.Error;
         Self.Sender.On_ShowMessage_Notification (Message);
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
     (Self               : in out Message_Handler'Class;
      Project_File       : VSS.Strings.Virtual_String := "";
      Scenario_Variables : LSP.Ada_Configurations.Variable_List;
      Charset            : VSS.Strings.Virtual_String)
   is

      Has_Alire   : Boolean;
      Status      : Load_Project_Status;
      Errors      : VSS.Strings.Virtual_String;
      Project     : VSS.Strings.Virtual_String := Project_File;
      UTF_8       : constant VSS.Strings.Virtual_String := "utf-8";

      Environment : GPR2.Environment.Object :=
        GPR2.Environment.Process_Environment;

      Alire_TOML  : constant GNATCOLL.VFS.Virtual_File :=
        (if Self.Client.Root.Is_Empty then GNATCOLL.VFS.No_File
         else Root (Self).Create_From_Dir ("alire.toml"));

   begin
      if Alire_TOML.Is_Regular_File
        and Spawn.Environments.System_Environment.Value ("ALIRE") /= "True"
      then

         Self.Tracer.Trace ("Check alire:");

         if Project.Is_Empty then

            LSP.Ada_Handlers.Alire.Run_Alire
              (Root        => Root (Self).Display_Full_Name,
               Has_Alire   => Has_Alire,
               Error       => Errors,
               Project     => Project,
               Environment => Environment);

            Status := Alire_Project;
         else

            LSP.Ada_Handlers.Alire.Run_Alire
              (Root        => Root (Self).Display_Full_Name,
               Has_Alire   => Has_Alire,
               Error       => Errors,
               Environment => Environment);

            Status := Valid_Project_Configured;
         end if;

         if Has_Alire and then not Errors.Is_Empty then

            --  Something wrong with alire. Report error. Don't load the
            --  project. Fallback to implicit project.

            declare
               Error : LSP.Structures.ShowMessageParams;
            begin
               Error.a_type := LSP.Enumerations.Error;
               Error.message := Errors;
               Self.Sender.On_ShowMessage_Notification (Error);
               Self.Tracer.Trace_Text (Errors);

               Load_Implicit_Project (Self, Invalid_Project_Configured);

               return;
            end;
         elsif Has_Alire then

            --  No errors means the project has been found
            pragma Assert (not Project.Is_Empty);

            Self.Tracer.Trace ("Project:");
            Self.Tracer.Trace_Text (Project);

            Load_Project
              (Self         => Self,
               Project_File => Project,
               Scenario     => Scenario_Variables,
               Environment  => Environment,
               Charset      => (if Charset.Is_Empty then UTF_8 else Charset),
               Status       => Status);
            --  Alire projects tend to use utf-8

            return;
         else
            Self.Tracer.Trace ("No alr in the PATH.");
         end if;
      end if;

      --  There is no alire.toml or no alr, but we know the project, load it
      if not Project.Is_Empty then

         Load_Project
           (Self         => Self,
            Project_File => Project,
            Scenario     => Scenario_Variables,
            Environment  => Environment,
            Charset      => Charset,
            Status       => Valid_Project_Configured);
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
      --  Self.Project_Environment := Empty_Environment;
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
   begin
      for Dir of Self.Project_Dirs_Loaded loop
         Values.Append (Dir.Display_Full_Name);
      end loop;

      Project.Set_Attribute
        (GPR2.Project.Registry.Attribute.Source_Dirs, Values);

      --  Load_Autoconf is assuming loading unloaded tree.

      Self.Project_Tree.Unload;

      GPR2.Project.Tree.View_Builder.Load_Autoconf
        (Self    => Self.Project_Tree,
         Project => Project,
         Context => GPR2.Context.Empty);

      Self.Project_Tree.Update_Sources (With_Runtime => True);

   exception
      when E : others =>
         Self.Tracer.Trace_Exception (E, "Reload_Implicit_Project_Dirs");
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
            Self.Configuration.Scenario_Variables,
            Self.Configuration.Charset);
      end if;
   end Reload_Project;

   ----------
   -- Root --
   ----------

   function Root
     (Self : Message_Handler'Class) return GNATCOLL.VFS.Virtual_File
   is
      Value : constant VSS.Strings.Virtual_String := Self.Client.Root;
      Root  : constant String :=
        VSS.Strings.Conversions.To_UTF_8_String (Value);
   begin
      return GNATCOLL.VFS.Create_From_UTF8
        (if Value.Starts_With ("file://")
         then URIs.Conversions.To_File (Root, True)
         else Root);
   end Root;

   ---------------------------------------
   -- Update_Project_Predefined_Sources --
   ---------------------------------------

   procedure Update_Project_Predefined_Sources
     (Self : in out Message_Handler'Class)
   is
      use GPR2;
      use GPR2.Project.Source.Set;
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
