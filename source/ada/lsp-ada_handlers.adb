------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2018-2022, AdaCore                     --
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

with Ada.Calendar; use Ada.Calendar;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Characters.Latin_1;
with Ada.Characters.Wide_Wide_Latin_1;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Exceptions;
with Ada.Strings.Wide_Wide_Unbounded;
with Ada.Strings.UTF_Encoding;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;

with GNAT.OS_Lib; use GNAT.OS_Lib;
with GNAT.Strings;
with GNATCOLL.JSON;
with GNATCOLL.Utils;             use GNATCOLL.Utils;

with VSS.Characters.Latin;
with VSS.Strings.Character_Iterators;
with VSS.Strings.Conversions;
with VSS.Unicode;

with LSP.Ada_Documents;        use LSP.Ada_Documents;
with LSP.Search;               use LSP.Search;
with LSP.Ada_Contexts;         use LSP.Ada_Contexts;
with LSP.Ada_Completions;
with LSP.Ada_Completions.Aspects;
with LSP.Ada_Completions.Attributes;
with LSP.Ada_Completions.End_Names;
with LSP.Ada_Completions.Keywords;
with LSP.Ada_Completions.Names;
with LSP.Ada_Completions.Parameters;
with LSP.Ada_Completions.Pragmas;
with LSP.Ada_Handlers.Invisibles;
with LSP.Ada_Handlers.Named_Parameters_Commands;
with LSP.Ada_Handlers.Refactor_Change_Parameter_Mode;
with LSP.Ada_Handlers.Refactor_Change_Parameters_Type;
with LSP.Ada_Handlers.Refactor_Change_Parameters_Default_Value;
with LSP.Ada_Handlers.Refactor_Add_Parameter;
with LSP.Ada_Handlers.Refactor_Introduce_Parameter;
with LSP.Ada_Handlers.Refactor_Extract_Subprogram;
with LSP.Ada_Handlers.Refactor_Imports_Commands;
with LSP.Ada_Handlers.Refactor_Move_Parameter;
with LSP.Ada_Handlers.Refactor_Remove_Parameter;
with LSP.Ada_Handlers.Refactor_Suppress_Seperate;
with LSP.Ada_Handlers.Refactor_Pull_Up_Declaration;
with LSP.Ada_Handlers.Project_Diagnostics;
with LSP.Ada_Project_Environments;
with LSP.Client_Side_File_Monitors;
with LSP.Commands;
with LSP.Common;       use LSP.Common;
with LSP.Ada_Handlers.File_Readers;
with LSP.Diagnostic_Sources;
with LSP.Errors;
with LSP.Lal_Utils;    use LSP.Lal_Utils;
with LSP.Messages.Client_Requests;
with LSP.Messages.Server_Notifications;
with LSP.Servers.FS_Watch;
with LSP.Types;        use LSP.Types;
with LSP.Generic_Cancel_Check;

with Langkit_Support.Slocs;
with Langkit_Support.Text;

with Laltools.Common;
with Laltools.Refactor_Imports;
with Laltools.Refactor.Subprogram_Signature;
with Laltools.Refactor.Safe_Rename;
with Laltools.Refactor.Suppress_Separate;
with Laltools.Refactor.Extract_Subprogram;
with Laltools.Refactor.Introduce_Parameter;
with Laltools.Refactor.Pull_Up_Declaration;
with Laltools.Refactor.Subprogram_Signature.Change_Parameters_Type;
with Laltools.Refactor.Subprogram_Signature.Change_Parameters_Default_Value;
with Laltools.Refactor.Subprogram_Signature.Remove_Parameter;

with Libadalang.Analysis;
with Libadalang.Common;    use Libadalang.Common;
with Libadalang.Doc_Utils;
with Libadalang.Helpers;
with Libadalang.Preprocessing;

with GNATdoc.Comments.Helpers;

with URIs;

package body LSP.Ada_Handlers is

   type Cancel_Countdown is mod 128;
   --  Counter to restrict frequency of Request.Canceled checks

   Allow_Incremental_Text_Changes : constant GNATCOLL.Traces.Trace_Handle :=
     GNATCOLL.Traces.Create ("ALS.ALLOW_INCREMENTAL_TEXT_CHANGES",
                             GNATCOLL.Traces.On);
   --  Trace to activate the support for incremental text changes.

   Notifications_For_Imprecise : constant GNATCOLL.Traces.Trace_Handle :=
     GNATCOLL.Traces.Create ("ALS.NOTIFICATIONS_FOR_IMPRECISE_NAVIGATION",
                             GNATCOLL.Traces.Off);

   Runtime_Indexing : constant GNATCOLL.Traces.Trace_Handle :=
     GNATCOLL.Traces.Create ("ALS.RUNTIME_INDEXING",
                             GNATCOLL.Traces.On);
   --  Trace to enable/disable runtime indexing. Useful for the testsuite.

   Partial_GNATpp : constant GNATCOLL.Traces.Trace_Handle :=
     GNATCOLL.Traces.Create ("ALS.PARTIAL_GNATPP",
                             GNATCOLL.Traces.On);
   --  Use partial formatting mode of gnatpp if On. Otherwise, use diff
   --  algorithm.

   Is_Parent : constant LSP.Messages.AlsReferenceKind_Set :=
     (Is_Server_Side => True,
      As_Flags => [LSP.Messages.Parent => True, others => False]);
   Is_Child : constant LSP.Messages.AlsReferenceKind_Set :=
     (Is_Server_Side => True,
      As_Flags => [LSP.Messages.Child => True, others => False]);
   --  Convenient constants

   Line_Feed : constant Wide_Wide_Character :=
     Ada.Characters.Wide_Wide_Latin_1.LF;
   --  Backspace : constant Character := Ada.Characters.Latin_1.BS;

   procedure Log_Imprecise_Xref_Message
     (Self     : access Message_Handler;
      URI      : LSP.Messages.DocumentUri;
      Position : LSP.Messages.Position);
   --  Log a message to record that we have made an imprecise navigation

   procedure Log_Unexpected_Null_Document
     (Self     : access Message_Handler;
      Where    : String);
   --  Log a message saying we unexpectedly couldn't find an open document

   procedure Imprecise_Resolve_Name
     (Self       : access Message_Handler;
      In_Context : Context_Access;
      Position   : LSP.Messages.TextDocumentPositionParams'Class;
      Definition : out Libadalang.Analysis.Defining_Name);
   --  If node at given Position is a name, then resolve it.
   --  Send a message in case of a possible imprecise result.
   --  See description of Msg_Type in Send_Imprecise_Xref_Message comments.

   procedure Show_Message
     (Self : access Message_Handler;
      Text : VSS.Strings.Virtual_String;
      Mode : LSP.Messages.MessageType := LSP.Messages.Error);
   --  Convenience function to send a message to the user.

   procedure Show_Imprecise_Reference_Warning
     (Self      : access Message_Handler;
      Operation : String);
   --  Convenience function to send the warning that the navigation is
   --  imprecise.

   function Get_Unique_Progress_Token
     (Self      : access Message_Handler;
      Operation : String := "") return LSP_Number_Or_String;
   --  Return an unique token for indicating progress

   procedure Index_Files (Self : access Message_Handler);
   --  Index all loaded files in each context. Emit progresormation.

   procedure Release_Contexts_And_Project_Info (Self : access Message_Handler);
   --  Release the memory associated to project information in Self

   function Contexts_For_File
     (Self : access Message_Handler;
      File : Virtual_File)
      return LSP.Ada_Context_Sets.Context_Lists.List;
   function Contexts_For_URI
     (Self : access Message_Handler;
      URI  : LSP.Messages.DocumentUri)
      return LSP.Ada_Context_Sets.Context_Lists.List;
   --  Return a list of contexts that are suitable for the given File/URI:
   --  a list of all contexts where the file is known to be part of the
   --  project tree, or is a runtime file for this project. If the file
   --  is not known to any project, return an empty list.
   --  The result should not be freed.

   procedure Reload_Implicit_Project_Dirs (Self : access Message_Handler);
   --  Reload as project source dirs the directories in
   --  Self.Project_Dirs_Loaded.

   procedure Publish_Diagnostics
     (Self     : access Message_Handler'Class;
      Document : not null LSP.Ada_Documents.Document_Access);
   --  Publish diagnostic messages for given document if needed

   function Is_Ada_Source
     (Self : access Message_Handler;
      File : GNATCOLL.VFS.Virtual_File)
            return Boolean
   is (Is_Ada_File (Self.Project_Tree, File));
   --  Checks if File is an Ada source of Self's project. This is needed
   --  to filter non Ada sources on notifications like
   --  DidCreate/Rename/DeleteFiles and DidChangeWatchedFiles since it's not
   --  possible to filter them in the FileOperationRegistrationOptions.

   function Compute_File_Operations_Server_Capabilities
     (Self : access Message_Handler)
      return LSP.Messages.Optional_FileOperationsServerCapabilities;
   --  Computes FileOperationsServerCapabilities based on the client's
   --  capabilities. If the client does have any, then this function returns
   --  an unset object.

   function Compute_File_Operation_Registration_Options
     (Self : access Message_Handler)
      return LSP.Messages.FileOperationRegistrationOptions;
   --  Computes FileOperationRegistrationOptions based on the project held by
   --  Self. These registration options will include any file that is in a
   --  source folder of Self's project. We can't filter non Ada sources here,
   --  only on the DidCreate/Rename/DeleteFiles and DidChangeWatchedFiles
   --  notifications.

   function Format
     (Self     : in out LSP.Ada_Contexts.Context;
      Document : LSP.Ada_Documents.Document_Access;
      Span     : LSP.Messages.Span;
      Options  : LSP.Messages.FormattingOptions;
      Handler  : access Message_Handler)
      return LSP.Messages.Server_Responses.Formatting_Response;
   --  Format the text of the given document in the given range (span).

   function Range_Format
     (Self     : in out LSP.Ada_Contexts.Context;
      Document : LSP.Ada_Documents.Document_Access;
      Span     : LSP.Messages.Span;
      Options  : LSP.Messages.FormattingOptions)
      return LSP.Messages.Server_Responses.Formatting_Response;
   --  Format the text of the given document in the given range (span).

   type File_Span is record
      File : GNATCOLL.VFS.Virtual_File;
      Span : LSP.Messages.Span;
   end record;
   --  A text range in a file

   function Hash (Value : File_Span) return Ada.Containers.Hash_Type;

   package File_Span_Sets is new Ada.Containers.Hashed_Sets
     (Element_Type        => File_Span,
      Hash                => Hash,
      Equivalent_Elements => "=");

   ---------------------
   -- Project loading --
   ---------------------

   --  The heuristics that is used for loading a project is the following:
   --
   --     * if a project (and optionally a scenario) was specified by
   --       the user via the workspace/didChangeConfiguration request,
   --       attempt to use this. If this fails to load, report an error
   --       but do not attempt to load another project.
   --     => This case is handled by a call to Load_Project in
   --        On_DidChangeConfiguration_Notification.
   --
   --     * if no project was specified by the user, then look in the Root
   --       directory, mimicking the behavior of gprbuild:
   --           * if there are zero .gpr files in this directory, load the
   --             implicit project
   --           * if there is exactly one .gpr file in this directory, load
   --             it, returning an error if this failed
   --           * if there are more than one .gpr files in this directory,
   --             display an error
   --      => These cases are handled by Ensure_Project_Loaded
   --
   --  At any point where requests are made, Self.Contexts should
   --  contain one or more contexts, each one containing a non-aggregate
   --  project hierarchy.
   --
   --  The attempt to load a project should be done in reaction to
   --  On_DidChangeConfiguration_Notification. However, the IDEs that
   --  are not configured specifically for this language server might
   --  not pass a .gpr file to didChangeConfiguration: for these IDEs,
   --  we fallback to loading the project the first time an editor is
   --  open or a request on non-openned file.

   procedure Ensure_Project_Loaded (Self : access Message_Handler);

   procedure Ensure_Project_Loaded
     (Self : access Message_Handler;
      URI  : LSP.Types.LSP_URI);
   --  This function makes sure that the contexts in Self are properly
   --  initialized and a project is loaded. If they are not initialized,
   --  initialize them. Use URI to find a custom root directory if provided.

   procedure Load_Implicit_Project
     (Self   : access Message_Handler;
      Status : Implicit_Project_Loaded);
   --  Load the implicit project

   procedure Load_Project
     (Self                : access Message_Handler;
      GPR                 : Virtual_File;
      Scenario            : LSP.Types.LSP_Any;
      Charset             : String;
      Status              : Load_Project_Status;
      Relocate_Build_Tree : Virtual_File := No_File;
      Root_Dir            : Virtual_File := No_File);
   --  Attempt to load the given project file, with the scenario provided.
   --  This unloads all currently loaded project contexts.

   procedure Mark_Source_Files_For_Indexing (Self : access Message_Handler);
   --  Mark all sources in all projects for indexing. This factorizes code
   --  between Load_Project and Load_Implicit_Project.

   function URI_To_File
     (Self : Message_Handler'Class;
      URI  : VSS.Strings.Virtual_String) return VSS.Strings.Virtual_String;
   --  Turn URI into path

   -----------------------
   -- Contexts_For_File --
   -----------------------

   function Contexts_For_File
     (Self : access Message_Handler;
      File : Virtual_File)
      return LSP.Ada_Context_Sets.Context_Lists.List
   is
      function Is_A_Source (Self : LSP.Ada_Contexts.Context) return Boolean is
        (Self.Is_Part_Of_Project (File));
      --  Return True if File is a source of the project held by Context

   begin
      --  If the file does not exist on disk, assume this is a file
      --  being created and, as a special convenience in this case,
      --  assume it could belong to any project.
      if not File.Is_Regular_File
      --  If the file is a runtime file for the loaded project environment,
      --  all projects can see it.
        or else Self.Project_Predefined_Sources.Contains (File)
      then
         return Self.Contexts.Each_Context;
      end if;

      --  List contexts where File is a source of the project hierarchy
      return Self.Contexts.Each_Context (Is_A_Source'Unrestricted_Access);
   end Contexts_For_File;

   ----------------------
   -- Contexts_For_URI --
   ----------------------

   function Contexts_For_URI
     (Self : access Message_Handler;
      URI  : LSP.Messages.DocumentUri)
      return LSP.Ada_Context_Sets.Context_Lists.List
   is
      File : constant Virtual_File := Self.To_File (URI);
   begin
      return Self.Contexts_For_File (File);
   end Contexts_For_URI;

   -----------------------
   -- Get_Open_Document --
   -----------------------

   overriding function Get_Open_Document
     (Self  : access Message_Handler;
      URI   : LSP.Messages.DocumentUri;
      Force : Boolean := False)
      return LSP.Ada_Documents.Document_Access
   is
      File : constant GNATCOLL.VFS.Virtual_File := Self.To_File (URI);
   begin
      Self.Ensure_Project_Loaded;

      if Self.Open_Documents.Contains (File) then
         return LSP.Ada_Documents.Document_Access
           (Self.Open_Documents.Element (File));
      elsif Force then
         declare
            Document : constant Internal_Document_Access :=
              new LSP.Ada_Documents.Document (Self.Trace);
         begin
            Document.Initialize (URI, VSS.Strings.Empty_Virtual_String, null);
            return LSP.Ada_Documents.Document_Access (Document);
         end;
      else
         return null;
      end if;
   end Get_Open_Document;

   -------------------------------
   -- Get_Open_Document_Version --
   -------------------------------

   overriding
   function Get_Open_Document_Version
     (Self  : access Message_Handler;
      URI   : LSP.Messages.DocumentUri)
      return LSP.Messages.OptionalVersionedTextDocumentIdentifier
   is
      Target_Text_Document : constant LSP.Ada_Documents.Document_Access :=
        Self.Get_Open_Document (URI);

   begin
      --  If the target textDocument hasn't been opened in the editor
      --  then ALS hasn't received an open notification before. Therefore
      --  Target_Text_Document will be null.
      --  In that case, its VersionedTextDocumentIdentifier.version will
      --  be null.

      if Target_Text_Document = null then
         return (URI, LSP.Messages.Nullable_Number'(Is_Set => False));

      else
         return
           (uri     => Target_Text_Document.Versioned_Identifier.uri,
            version =>
              (True, Target_Text_Document.Versioned_Identifier.version));
      end if;
   end Get_Open_Document_Version;

   ----------------------------------
   -- Log_Unexpected_Null_Document --
   ----------------------------------

   procedure Log_Unexpected_Null_Document
     (Self     : access Message_Handler;
      Where    : String) is
   begin
      Self.Trace.Trace ("Unexpected null document in " & Where);
   end Log_Unexpected_Null_Document;

   --------------------------------
   -- Log_Imprecise_Xref_Message --
   --------------------------------

   procedure Log_Imprecise_Xref_Message
     (Self     : access Message_Handler;
      URI      : LSP.Messages.DocumentUri;
      Position : LSP.Messages.Position)
   is
      File : constant GNATCOLL.VFS.Virtual_File := Self.To_File (URI);
   begin
      Self.Trace.Trace
        ("Imprecise fallback used to compute cross-references on entity at "
         & File.Display_Base_Name
         & ":" & Integer'Image (Integer (Position.line) + 1)
         & ":" & Integer'Image (Integer (Position.character) + 1));
   end Log_Imprecise_Xref_Message;

   ----------------------------
   -- Imprecise_Resolve_Name --
   ----------------------------

   procedure Imprecise_Resolve_Name
     (Self       : access Message_Handler;
      In_Context : Context_Access;
      Position   : LSP.Messages.TextDocumentPositionParams'Class;
      Definition : out Libadalang.Analysis.Defining_Name)
   is
      use type Libadalang.Analysis.Name;

      Name_Node : constant Libadalang.Analysis.Name :=
        Laltools.Common.Get_Node_As_Name
          (In_Context.Get_Node_At
             (Get_Open_Document (Self, Position.textDocument.uri),
              Position,
              Project_Only => False));

      Imprecise : Boolean;
   begin
      if Name_Node = Libadalang.Analysis.No_Name then
         return;
      end if;

      Definition := Laltools.Common.Resolve_Name
        (Name_Node,
         Self.Trace,
         Imprecise => Imprecise);

      --  If we used the imprecise fallback to get to the definition, log it
      if Imprecise then
         Self.Log_Imprecise_Xref_Message
           (URI      => Position.textDocument.uri,
            Position => Position.position);
      end if;
   end Imprecise_Resolve_Name;

   ---------------------------------------
   -- Release_Contexts_And_Project_Info --
   ---------------------------------------

   procedure Release_Contexts_And_Project_Info
     (Self : access Message_Handler)
   is
      use GNATCOLL.Projects;
   begin
      Self.Contexts.Cleanup;

      if Self.Project_Tree /= null then
         Self.Project_Tree.Unload;
         Free (Self.Project_Tree);
      end if;
      if Self.Project_Environment /= null then
         Free (Self.Project_Environment);
      end if;
      Self.Project_Predefined_Sources.Clear;
      Self.Project_Dirs_Loaded.Clear;

      --  Clear indexing data
      Self.Files_To_Index.Clear;
      Self.Total_Files_To_Index := 1;
      Self.Total_Files_Indexed := 0;
   end Release_Contexts_And_Project_Info;

   --------------------------
   -- Stop_File_Monitoring --
   --------------------------

   procedure Stop_File_Monitoring (Self : access Message_Handler) is
   begin
      if Self.File_Monitor.Assigned then
         Self.File_Monitor.Stop_Monitoring_Directories;
      end if;
   end Stop_File_Monitoring;

   -------------
   -- Cleanup --
   -------------

   procedure Cleanup (Self : access Message_Handler)
   is
   begin
      --  Cleanup documents
      for Document of Self.Open_Documents loop
         Free (Document);
      end loop;
      Self.Open_Documents.Clear;

      --  Cleanup contexts, project and environment
      Self.Release_Contexts_And_Project_Info;

      --  Free the file monitor
      LSP.File_Monitors.Unchecked_Free (Self.File_Monitor);
   end Cleanup;

   ----------------
   -- Clean_Logs --
   ----------------

   procedure Clean_Logs (Self : access Message_Handler; Dir : Virtual_File) is
      Files : File_Array_Access := Read_Dir (Dir, Files_Only);
      Dummy : Boolean;
      Cpt   : Integer := 0;
   begin
      Sort (Files.all);
      --  Browse the log files in reverse timestamp order
      for F of reverse Files.all loop
         --  Filter out files like traces.cfg
         if GNATCOLL.Utils.Ends_With (+F.Base_Name, ".log") then
            Cpt := Cpt + 1;
            --  Delete the old logs
            if Cpt > Self.Log_Threshold then
               Delete (F, Dummy);
            end if;
         end if;
      end loop;
      Unchecked_Free (Files);
   end Clean_Logs;

   -----------------------
   -- Exit_Notification --
   -----------------------

   overriding procedure On_Exit_Notification (Self : access Message_Handler) is
   begin
      Self.Server.Stop;
   end On_Exit_Notification;

   ----------------------------------
   -- Reload_Implicit_Project_Dirs --
   ----------------------------------

   procedure Reload_Implicit_Project_Dirs (Self : access Message_Handler) is
      Attr  : GNAT.Strings.String_List
        (1 .. Natural (Self.Project_Dirs_Loaded.Length));
      Index : Natural := 1;
      use GNATCOLL.Projects;
   begin
      for Dir of Self.Project_Dirs_Loaded loop
         Attr (Index) := new String'(Dir.Display_Full_Name);
         Index := Index + 1;
      end loop;

      Set_Attribute
        (Self.Project_Tree.Root_Project,
         Source_Dirs_Attribute,
         Attr);
      Self.Project_Tree.Recompute_View;

      for J in Attr'Range loop
         GNAT.Strings.Free (Attr (J));
      end loop;
   end Reload_Implicit_Project_Dirs;

   ---------------------------
   -- Load_Implicit_Project --
   ---------------------------

   procedure Load_Implicit_Project
     (Self   : access Message_Handler;
      Status : Implicit_Project_Loaded)
   is
      C    : constant Context_Access := new Context (Self.Trace);
      Attr : GNAT.Strings.String_List (1 .. 1);
      use GNATCOLL.Projects;

      Reader : LSP.Ada_Handlers.File_Readers.LSP_Reader_Interface (Self);
   begin
      Self.Trace.Trace ("Loading the implicit project");

      Self.Project_Status := Status;
      Self.Release_Contexts_And_Project_Info;
      Self.Project_Environment :=
        new LSP.Ada_Project_Environments.LSP_Project_Environment;
      Initialize (Self.Project_Environment);
      Self.Project_Environment.Set_Trusted_Mode (not Self.Follow_Symlinks);
      Self.Project_Tree := new Project_Tree;

      C.Initialize (Reader, Self.Follow_Symlinks,
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

      Load_Empty_Project
        (Self.Project_Tree.all, Self.Project_Environment);
      Attr := [1 => new String'("Ada")];
      Set_Attribute
        (Self.Project_Tree.Root_Project, Languages_Attribute, Attr);
      GNAT.Strings.Free (Attr (1));

      --  When there is no .gpr, create a project which loads the
      --  root directory in the workspace.

      Self.Project_Dirs_Loaded.Include (Self.Root);
      Self.Reload_Implicit_Project_Dirs;
      C.Load_Project (Self.Project_Tree,
                      Self.Project_Tree.Root_Project,
                      "iso-8859-1");

      for File of Self.Project_Environment.Predefined_Source_Files loop
         Self.Project_Predefined_Sources.Include (File);
      end loop;

      Self.Contexts.Prepend (C);

      --  Reindex the files from disk in the background after a project reload
      Self.Mark_Source_Files_For_Indexing;
   end Load_Implicit_Project;

   ---------------------------
   -- Ensure_Project_Loaded --
   ---------------------------

   procedure Ensure_Project_Loaded
     (Self : access Message_Handler;
      URI  : LSP.Types.LSP_URI)
   is
   begin
      if not Self.Contexts.Is_Empty then
         --  Rely on the fact that there is at least one context initialized
         --  as a guarantee that the initialization has been done.
         return;
      end if;

      Self.Root := Self.To_File (URI).Dir;
      Self.Ensure_Project_Loaded;
   end Ensure_Project_Loaded;

   ---------------------------
   -- Ensure_Project_Loaded --
   ---------------------------

   procedure Ensure_Project_Loaded (Self : access Message_Handler) is
      GPRs_Found : Natural := 0;
      Files      : File_Array_Access;
      GPR        : Virtual_File;
   begin
      if not Self.Contexts.Is_Empty then
         --  Rely on the fact that there is at least one context initialized
         --  as a guarantee that the initialization has been done.
         return;
      end if;

      Self.Trace.Trace ("Project loading ...");
      Self.Trace.Trace ("Root : " & Self.Root.Display_Full_Name);

      --  We're going to look for a project in Root: list all the files
      --  in this directory, looking for .gpr files.

      Files := Self.Root.Read_Dir (Files_Only);
      if Files /= null then
         for X of Files.all loop
            if Ends_With (+X.Base_Name, ".gpr") then
               GPRs_Found := GPRs_Found + 1;
               exit when GPRs_Found > 1;
               GPR := X;
            end if;
         end loop;
         Unchecked_Free (Files);
      end if;

      --  What we do depends on the number of .gpr files found:

      if GPRs_Found = 0 then
         --  We have found zero .gpr files: load the implicit project

         Self.Load_Implicit_Project (No_Project_Found);
      elsif GPRs_Found = 1 then
         --  We have not found exactly one .gpr file: load the default
         --  project.
         Self.Trace.Trace ("Loading " & GPR.Display_Base_Name);
         Self.Load_Project (GPR, No_Any, "iso-8859-1", Single_Project_Found);
      else
         --  We have found more than one project: warn the user!

         Self.Show_Message
           (VSS.Strings.To_Virtual_String
              ("More than one .gpr found." & Line_Feed &
                 "Note: you can configure a project " &
                 " through the ada.projectFile setting."));
         Self.Load_Implicit_Project (Multiple_Projects_Found);
      end if;
   end Ensure_Project_Loaded;

   -------------------------------------------------
   -- Compute_File_Operations_Server_Capabilities --
   -------------------------------------------------

   function Compute_File_Operations_Server_Capabilities
     (Self : access Message_Handler)
      return LSP.Messages.Optional_FileOperationsServerCapabilities
   is
      use LSP.Messages;
      Client_Capabilities :
        LSP.Messages.Optional_FileOperationsClientCapabilities
          renames Self.Client.capabilities.workspace.fileOperations;
   begin
      if Client_Capabilities.Is_Set
        and then not Self.Contexts.Each_Context.Is_Empty
      then
         declare
            Registration_Options :
              constant Optional_FileOperationRegistrationOptions :=
                (Is_Set => True,
                 Value  => Self.Compute_File_Operation_Registration_Options);
         begin
            return Server_Capabilities :
                     Optional_FileOperationsServerCapabilities (Is_Set => True)
            do
               if Client_Capabilities.Value.didCreate = True then
                  Server_Capabilities.Value.didCreate := Registration_Options;
               end if;
               if Client_Capabilities.Value.willCreate = True then
                  Server_Capabilities.Value.willCreate := Registration_Options;
               end if;
               if Client_Capabilities.Value.didRename = True then
                  Server_Capabilities.Value.didRename := Registration_Options;
               end if;
               if Client_Capabilities.Value.willRename = True then
                  Server_Capabilities.Value.willRename := Registration_Options;
               end if;
               if Client_Capabilities.Value.didDelete = True then
                  Server_Capabilities.Value.didDelete := Registration_Options;
               end if;
               if Client_Capabilities.Value.willDelete = True then
                  Server_Capabilities.Value.willDelete := Registration_Options;
               end if;
            end return;
         end;
      else
         return Optional_FileOperationsServerCapabilities'(Is_Set => False);
      end if;
   end Compute_File_Operations_Server_Capabilities;

   -------------------------------------------------
   -- Compute_File_Operation_Registration_Options --
   -------------------------------------------------

   function Compute_File_Operation_Registration_Options
     (Self : access Message_Handler)
      return LSP.Messages.FileOperationRegistrationOptions
   is
      use LSP.Messages;

      File_Operation_Filters : LSP.Messages.FileOperationFilter_Vector;

   begin
      for Context of Self.Contexts.Each_Context loop
         for Source_Dir of Context.List_Source_Directories loop
            declare
               Dir_Full_Name : constant GNATCOLL.VFS.Filesystem_String :=
                 GNATCOLL.VFS."/" (Source_Dir, "*").Full_Name;
               Scheme        : constant VSS.Strings.Virtual_String := "file";
               Sources_Glob  : constant VSS.Strings.Virtual_String :=
                 VSS.Strings.Conversions.To_Virtual_String (+Dir_Full_Name);

               File_Operation_Filter :
                 constant LSP.Messages.FileOperationFilter :=
                   (scheme => (Is_Set => True, Value => Scheme),
                    pattern => (glob => Sources_Glob, others => <>));
            begin
               File_Operation_Filters.Append (File_Operation_Filter);
            end;
         end loop;
      end loop;

      return
        FileOperationRegistrationOptions'(filters => File_Operation_Filters);
   end Compute_File_Operation_Registration_Options;

   ------------
   -- Format --
   ------------

   function Format
     (Self     : in out LSP.Ada_Contexts.Context;
      Document : LSP.Ada_Documents.Document_Access;
      Span     : LSP.Messages.Span;
      Options  : LSP.Messages.FormattingOptions;
      Handler  : access Message_Handler)
      return LSP.Messages.Server_Responses.Formatting_Response
   is
      Response : LSP.Messages.Server_Responses.Formatting_Response
        (Is_Error => False);
      Success  : Boolean;
      Messages : VSS.String_Vectors.Virtual_String_Vector;
   begin
      Self.Format
        (Document => Document,
         Span     => Span,
         Options  => Options,
         Edit     => Response.result,
         Success  => Success,
         Messages => Messages);

      if not Success then
         declare
            use VSS.Strings;

            Response  : LSP.Messages.Server_Responses.Formatting_Response
              (Is_Error => True);
            Error_Msg : VSS.Strings.Virtual_String;
         begin
            --  Display error messages from gnatpp, if any
            for Msg of Messages loop
               Error_Msg := Error_Msg & Msg;
            end loop;

            Response.error :=
              (True,
               (code    => LSP.Errors.InternalError,
                message => Error_Msg,
                data    => <>));
            return Response;
         end;
      else
         --  If the formntting succeeded, still display messages in the client
         --  if any.
         for Msg of Messages loop
            Show_Message
              (Self => Handler,
               Text => Msg,
               Mode => LSP.Messages.Info);
         end loop;
      end if;

      return Response;
   end Format;

   ------------------
   -- Range_Format --
   ------------------

   function Range_Format
     (Self     : in out LSP.Ada_Contexts.Context;
      Document : LSP.Ada_Documents.Document_Access;
      Span     : LSP.Messages.Span;
      Options  : LSP.Messages.FormattingOptions)
      return LSP.Messages.Server_Responses.Formatting_Response
   is
      Response : LSP.Messages.Server_Responses.Formatting_Response
        (Is_Error => False);
      Success  : Boolean;
      Messages : VSS.String_Vectors.Virtual_String_Vector;

   begin
      Self.Range_Format
        (Document => Document,
         Span     => Span,
         Options  => Options,
         Edit     => Response.result,
         Success  => Success,
         Messages => Messages);

      if not Success then
         declare
            use VSS.Strings;

            Response  : LSP.Messages.Server_Responses.Formatting_Response
              (Is_Error => True);
            Error_Msg : VSS.Strings.Virtual_String;
         begin
            --  Display error messages from gnatpp, if any
            for Msg of Messages loop
               Error_Msg := Error_Msg & Msg;
            end loop;

            Response.error :=
              (True,
               (code    => LSP.Errors.InternalError,
                message => Error_Msg,
                data    => <>));
            return Response;
         end;
      end if;

      return Response;
   end Range_Format;

   ------------------------
   -- Initialize_Request --
   ------------------------

   overriding function On_Initialize_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Initialize_Request)
      return LSP.Messages.Server_Responses.Initialize_Response
   is
      use LSP.Messages;

      Value            : LSP.Messages.InitializeParams renames Request.params;
      Code_Action      : LSP.Messages.Optional_CodeActionClientCapabilities
        renames Value.capabilities.textDocument.codeAction;
      Has_Rename       : LSP.Messages.Optional_RenameClientCapabilities
        renames Value.capabilities.textDocument.rename;
      Semantic_Tokens  : LSP.Messages.Optional_SemanticTokensClientCapabilities
        renames Value.capabilities.textDocument.semanticTokens;
      Experimental_Client_Capabilities : LSP.Types.Optional_LSP_Any renames
        Value.capabilities.experimental;
      Response         : LSP.Messages.Server_Responses.Initialize_Response
        (Is_Error => False);
      Root             : VSS.Strings.Virtual_String;
      codeActionKinds  : LSP.Messages.CodeActionKindSet;
      Backspace_String : VSS.Strings.Virtual_String;
      Retrigger_Vector : VSS.String_Vectors.Virtual_String_Vector;

   begin
      Backspace_String.Append (VSS.Characters.Latin.Backspace);
      Retrigger_Vector.Append (Backspace_String);

      Response.result.capabilities.declarationProvider :=
        (Is_Set => True,
         Value => (Is_Boolean => True, Bool => True));
      Response.result.capabilities.definitionProvider :=
        (Is_Set => True,
         Value  => (workDoneProgress => LSP.Types.None));
      Response.result.capabilities.typeDefinitionProvider :=
        (Is_Set => True,
         Value => (Is_Boolean => True, Bool => True));
      Response.result.capabilities.implementationProvider :=
        (Is_Set => True,
         Value => (Is_Boolean => True, Bool => True));
      Response.result.capabilities.referencesProvider :=
        (Is_Set => True,
         Value  => (workDoneProgress => LSP.Types.None));
      Response.result.capabilities.documentFormattingProvider :=
        (Is_Set => True,
         Value  => (workDoneProgress => LSP.Types.None));
      if Partial_GNATpp.Is_Active then
         Response.result.capabilities.documentRangeFormattingProvider :=
           (Is_Set => True,
            Value  => (workDoneProgress => LSP.Types.None));
      end if;
      Response.result.capabilities.callHierarchyProvider :=
        (Is_Set => True,
         Value  => (Is_Boolean => False, Options => <>));
      Response.result.capabilities.documentHighlightProvider :=
        (Is_Set => True,
         Value => (workDoneProgress => LSP.Types.None));

      Response.result.capabilities.workspaceSymbolProvider :=
        (Is_Set => True,
         Value  => (workDoneProgress => LSP.Types.None));
      Response.result.capabilities.documentSymbolProvider :=
        (Is_Set => True,
         Value  => (workDoneProgress => LSP.Types.None, label => <>));
      Response.result.capabilities.renameProvider :=
        (Is_Set => True,
         Value  => (prepareProvider  =>
                     (if Has_Rename.Is_Set
                      and then Has_Rename.Value.prepareSupport = True
                        then LSP.Types.True else LSP.Types.None),
                    workDoneProgress => LSP.Types.None));
      Response.result.capabilities.textDocumentSync :=
        (Is_Set => True, Is_Number => True,
         Value  =>
           (if Allow_Incremental_Text_Changes.Active then
               LSP.Messages.Incremental
            else
               LSP.Messages.Full));
      Response.result.capabilities.signatureHelpProvider :=
        (True,
         (triggerCharacters   => (True, [",", "("]),
          retriggerCharacters => (True, Retrigger_Vector),
          workDoneProgress    => LSP.Types.None));
      Response.result.capabilities.completionProvider :=
        (True,
         (resolveProvider     => LSP.Types.True,
          triggerCharacters   => (True, [".", ",", "'", "("]),
          allCommitCharacters => (Is_Set => False),
          workDoneProgress    => LSP.Types.None));
      Response.result.capabilities.hoverProvider :=
        (Is_Set => True,
         Value  => (workDoneProgress => LSP.Types.None));
      Response.result.capabilities.executeCommandProvider :=
        (Is_Set => True,
         Value  => (commands => LSP.Commands.All_Commands,
                    workDoneProgress  => LSP.Types.None));

      if Code_Action.Is_Set and then
        Code_Action.Value.codeActionLiteralSupport.Is_Set
      then
         LSP.Messages.Include (codeActionKinds, LSP.Messages.QuickFix);
         LSP.Messages.Include (codeActionKinds, LSP.Messages.RefactorRewrite);

         Response.result.capabilities.codeActionProvider :=
           (Is_Set => True,
            Value  =>
              (codeActionKinds  => (True, codeActionKinds),
               workDoneProgress => LSP.Types.None,
               resolveProvider  => LSP.Types.None));
      else
         Response.result.capabilities.codeActionProvider :=
           (Is_Set => True, Value => <>);
      end if;

      Response.result.capabilities.alsShowDepsProvider := True;

      Response.result.capabilities.alsReferenceKinds :=
        (Is_Set => True,
         Value  => (Is_Server_Side => True, As_Flags => [others => True]));

      Response.result.capabilities.foldingRangeProvider :=
        (Is_Set => True,
         Value => (Is_Boolean => True, Bool => True));

      Self.Resource_Operations :=
        Value.capabilities.workspace.workspaceEdit.resourceOperations;

      Response.result.capabilities.alsCheckSyntaxProvider := True;

      --  Client capability to support versioned document changes in
      --  `WorkspaceEdit`s.
      Self.Versioned_Documents :=
        Value.capabilities.workspace.workspaceEdit.documentChanges = True;

      if Value.capabilities.textDocument.documentSymbol.Is_Set
        and then Value.capabilities.textDocument.documentSymbol.Value
          .hierarchicalDocumentSymbolSupport = True
      then
         Self.Get_Symbols := LSP.Ada_Documents.Get_Symbol_Hierarchy'Access;
      else
         Self.Get_Symbols := LSP.Ada_Documents.Get_Symbols'Access;
      end if;

      if Value.capabilities.textDocument.foldingRange.Is_Set
        and then Value.capabilities.textDocument.foldingRange.Value.
          lineFoldingOnly = True
      then
         --  Client capability to fold only entire lines
         Self.Line_Folding_Only := True;
      end if;

      if Value.capabilities.textDocument.completion.completionItem.Is_Set
        and then Value.capabilities.textDocument.completion.
          completionItem.Value.snippetSupport = True
      then
         --  Client capability to support snippets for completion
         Self.Completion_Snippets_Enabled := True;
      end if;

      if Value.capabilities.textDocument.completion.completionItem.Is_Set
        and then Value.capabilities.textDocument.completion.
          completionItem.Value.resolveSupport.Is_Set
      then
         Self.Completion_Resolve_Properties :=
           Value.capabilities.textDocument.completion.
             completionItem.Value.resolveSupport.Value.properties;
      end if;

      if Value.capabilities.workspace.didChangeWatchedFiles.Is_Set
        and then Value.capabilities.workspace.didChangeWatchedFiles.Value
           .dynamicRegistration = True
      then
         Self.File_Monitor := new LSP.Client_Side_File_Monitors.File_Monitor
           (Self.Server);
      end if;

      if Semantic_Tokens.Is_Set then
         declare
            Legend : LSP.Messages.SemanticTokensLegend;
         begin
            Self.Highlighter.Initialize (Semantic_Tokens.Value, Legend);

            Response.result.capabilities.semanticTokensProvider :=
              (True,
               (legend => Legend,
                full   => (True, (diff => <>)),
                span   => LSP.Types.True,
                others => <>));
         end;
      end if;

      if Value.rootUri.Is_Set
        and then not Value.rootUri.Value.Is_Empty
      then
         Root := Self.URI_To_File (Value.rootUri.Value);
      elsif Value.rootPath.Is_Set and then Value.rootPath.Value.Is_Set then
         --  URI isn't provided, rollback to deprecated rootPath
         Root := Value.rootPath.Value.Value;
      end if;

      --  Some clients - notably VS Code as of version 33, when opening a file
      --  rather than a workspace - don't provide a root at all. In that case
      --  use the current directory as root.

      if Root.Is_Empty then
         Root := ".";
      end if;

      Self.Root :=
        Create_From_UTF8 (VSS.Strings.Conversions.To_UTF_8_String (Root));
      Self.Client := Value;

      --  Log the context root
      Self.Trace.Trace
        ("Context root: " & VSS.Strings.Conversions.To_UTF_8_String (Root));

      --  Client/ServerCapabilities.workspace.fileOperations capabilities

      declare
         File_Operation_Capabilities :
           constant Optional_FileOperationsServerCapabilities :=
             Self.Compute_File_Operations_Server_Capabilities;

      begin
         if File_Operation_Capabilities.Is_Set then
            Response.result.capabilities.workspace :=
              (Is_Set => True,
               Value =>
                 (workspaceFolders => (Is_Set => False),
                  fileOperations   => File_Operation_Capabilities));
         end if;
      end;

      Response.result.serverInfo := LSP.Messages.Optional_ProgramInfo'
        (True,
         (log_filename =>
              (True, VSS.Strings.Conversions.To_Virtual_String
                 (Runtime_Indexing.Get_Stream_File.Display_Full_Name)),
          others       => <>));

      --  Experimental Client Capabilities
      Self.Experimental_Client_Capabilities :=
        Parse (Experimental_Client_Capabilities);

      return Response;
   end On_Initialize_Request;

   -------------------------
   -- On_Shutdown_Request --
   -------------------------

   overriding function On_Shutdown_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Shutdown_Request)
      return LSP.Messages.Server_Responses.Shutdown_Response
   is
      pragma Unreferenced (Request);
   begin
      --  Suspend files/runtime indexing after shutdown requst
      Self.Indexing_Enabled := False;

      return Response : LSP.Messages.Server_Responses.Shutdown_Response
        (Is_Error => False);
   end On_Shutdown_Request;

   ---------------------------
   -- On_CodeAction_Request --
   ---------------------------

   overriding function On_CodeAction_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.CodeAction_Request)
      return LSP.Messages.Server_Responses.CodeAction_Response
   is
      Params   : LSP.Messages.CodeActionParams renames Request.params;

      procedure Analyse_In_Context
        (Context  : Context_Access;
         Document : LSP.Ada_Documents.Document_Access;
         Result   : out LSP.Messages.CodeAction_Vector;
         Found    : in out Boolean);
      --  Perform refactoring ananlysis given Document in the Context.
      --  Return Found = True if some refactoring is possible. Populate
      --  Result with Code_Actions in this case.

      function Has_Assoc_Without_Designator
        (Node : Libadalang.Analysis.Basic_Assoc_List) return Boolean;
      --  Check if Node is Basic_Assoc_List that contains at least one
      --  ParamAssoc without a designator.

      procedure Analyse_Node
        (Context : Context_Access;
         Node    : Libadalang.Analysis.Ada_Node;
         Result  : out LSP.Messages.CodeAction_Vector;
         Found   : in out Boolean);
      --  Look for a possible refactoring in given Node.
      --  Return Found = True if some refactoring is possible. Populate
      --  Result with Code_Actions in this case. Return Done = True if futher
      --  analysis has no sense.

      procedure Append_Project_Status_Code_Actions
        (Result : in out LSP.Messages.CodeAction_Vector);
      --  Append project status code action if needed

      ----------------------------------
      -- Has_Assoc_Without_Designator --
      ----------------------------------

      function Has_Assoc_Without_Designator
        (Node : Libadalang.Analysis.Basic_Assoc_List) return Boolean
      is
         Found : Boolean := False;

         function Process_Type_Expr
           (TE : Libadalang.Analysis.Type_Expr)
            return Boolean;
         --  Returns True if TE is associated to an access of a subprogram

         -----------------------
         -- Process_Type_Expr --
         -----------------------

         function Process_Type_Expr
           (TE : Libadalang.Analysis.Type_Expr)
            return Boolean
         is
            TD : Libadalang.Analysis.Base_Type_Decl;
            --  If TE is not an anonymous type then we'll need to know its
            --  declaration.

         begin
            case TE.Kind is
               when Ada_Subtype_Indication_Range =>
                  TD := TE.As_Subtype_Indication.P_Designated_Type_Decl;

                  if TD.Is_Null
                    or else not (TD.Kind in Ada_Type_Decl)
                  then
                     return False;
                  end if;

                  case TD.As_Type_Decl.F_Type_Def.Kind is
                     when Ada_Access_To_Subp_Def_Range =>
                        --  Confirmation that TD is an access to a subprogram

                        return True;

                     when Ada_Array_Type_Def_Range =>
                        --  If TD is an array type, then it might be an array
                        --  of accesses to subprograms. Therefore, recursively
                        --  call Process_Type_Expr to check the type of the
                        --  components of the array.

                        return Process_Type_Expr
                          (TD.As_Type_Decl.F_Type_Def.As_Array_Type_Def.
                             F_Component_Type.F_Type_Expr);

                     when others =>
                        return False;
                  end case;

               when Ada_Anonymous_Type_Range =>
                  return TE.As_Anonymous_Type.F_Type_Decl.F_Type_Def.Kind in
                    Ada_Access_To_Subp_Def_Range;

               when others =>
                  return False;

            end case;
         end Process_Type_Expr;

      begin
         for J of Node loop
            if J.Kind in Libadalang.Common.Ada_Param_Assoc and then
              J.As_Param_Assoc.F_Designator.Is_Null
            then
               Found := True;
               exit;
            end if;
         end loop;

         if not Found then
            return False;
         end if;

         declare
            Expr : constant Libadalang.Analysis.Ada_Node := Node.Parent;
            Name : Libadalang.Analysis.Name;
            Decl : Libadalang.Analysis.Basic_Decl;
         begin
            case Expr.Kind is
               when Libadalang.Common.Ada_Call_Expr =>
                  Name := Expr.As_Call_Expr.F_Name;
               when others =>
                  return False;
            end case;

            Decl := Name.P_Referenced_Decl;

            if Decl.Is_Null then
               return False;
            end if;

            --  For Ada_Param_Spec, Ada_Component_Decl or Object_Decl nodes,
            --  check the type definition of Decl. Named parameters can be
            --  added if Decl's type is a (possibly anonymous) access to a
            --  subprogram.

            case Decl.Kind is
               when Libadalang.Common.Ada_Base_Subp_Body
                  | Libadalang.Common.Ada_Basic_Subp_Decl =>
                  return True;

               when Libadalang.Common.Ada_Param_Spec_Range =>
                  return Process_Type_Expr (Decl.As_Param_Spec.F_Type_Expr);

               when Libadalang.Common.Ada_Component_Decl_Range =>
                  return Process_Type_Expr
                    (Decl.As_Component_Decl.F_Component_Def.F_Type_Expr);

               when  Libadalang.Common.Ada_Object_Decl_Range =>
                  --  This can either be an object which type is an access
                  --  to a subprogram or an array of accesses to
                  --  subprograms.
                  return Process_Type_Expr (Decl.As_Object_Decl.F_Type_Expr);

               when others =>
                  return False;
            end case;
         end;
      end Has_Assoc_Without_Designator;

      ------------------
      -- Analyse_Node --
      ------------------

      procedure Analyse_Node
        (Context : Context_Access;
         Node    : Libadalang.Analysis.Ada_Node;
         Result  : out LSP.Messages.CodeAction_Vector;
         Found   : in out Boolean)
      is
         procedure Change_Parameters_Type_Code_Action;
         --  Checks if the Change Parameters Type refactoring tool is avaiable,
         --  and if so, appends a Code Action with its Command.

         procedure Change_Parameters_Default_Value_Code_Action;
         --  Checks if the Change Parameters Default Value refactoring tool is
         --  avaiable, and if so, appends a Code Action with its Command.

         procedure Extract_Subprogram_Code_Action;
         --  Checks if the Extract Subprogram refactoring tool is available,
         --  and if so, appends a Code Action with its Command.

         procedure Introduce_Parameter_Code_Action;
         --  Checks if the Introduce Parameter refactoring tool is available,
         --  and if so, appends a Code Action with its Command.

         procedure Import_Package_Code_Action;
         --  Checks if the Import Package code assist is available,
         --  and if so, appends a Code Aciton with its Command.

         procedure Named_Parameters_Code_Action;
         --  Checks if the Named Parameters refactoring is available, and if
         --  so, appends a Code Action with its Command.

         procedure Pull_Up_Declaration_Code_Action;
         --  Checks if the Pull Up Declaration refactoring tool is available,
         --  and if so, appends a Code Action with its Command.

         ----------------------------------------
         -- Change_Parameters_Type_Code_Action --
         ----------------------------------------

         procedure Change_Parameters_Type_Code_Action is
            use Langkit_Support.Slocs;
            use Laltools.Refactor.Subprogram_Signature.Change_Parameters_Type;
            use LSP.Ada_Handlers.Refactor_Change_Parameters_Type;

            Span : constant Source_Location_Range :=
              (Langkit_Support.Slocs.Line_Number (Params.span.first.line) + 1,
               Langkit_Support.Slocs.Line_Number (Params.span.last.line) + 1,
               Column_Number (Params.span.first.character) + 1,
               Column_Number (Params.span.last.character) + 1);

            Syntax_Rules : Laltools.Common.Grammar_Rule_Vector;

            Change_Parameters_Type_Command : Command;

         begin
            if Is_Change_Parameters_Type_Available
                 (Unit                             => Node.Unit,
                  Parameters_Source_Location_Range => Span,
                  New_Parameter_Syntax_Rules       => Syntax_Rules)
            then
               Change_Parameters_Type_Command.Append_Code_Action
                 (Context                     => Context,
                  Commands_Vector             => Result,
                  Where                       =>
                    (uri     => Params.textDocument.uri,
                     span    => Params.span,
                     alsKind => LSP.Messages.Empty_Set),
                  Syntax_Rules                => Syntax_Rules);

               Found := True;
            end if;
         end Change_Parameters_Type_Code_Action;

         -------------------------------------------------
         -- Change_Parameters_Default_Value_Code_Action --
         -------------------------------------------------

         procedure Change_Parameters_Default_Value_Code_Action is
            use Langkit_Support.Slocs;
            use Laltools.Refactor.Subprogram_Signature.
                  Change_Parameters_Default_Value;
            use LSP.Ada_Handlers.Refactor_Change_Parameters_Default_Value;

            Span : constant Source_Location_Range :=
              (Langkit_Support.Slocs.Line_Number (Params.span.first.line) + 1,
               Langkit_Support.Slocs.Line_Number (Params.span.last.line) + 1,
               Column_Number (Params.span.first.character) + 1,
               Column_Number (Params.span.last.character) + 1);

            Change_Parameters_Default_Value_Command : Command;

         begin
            if Is_Change_Parameters_Default_Value_Available
                 (Unit                             => Node.Unit,
                  Parameters_Source_Location_Range => Span)
            then
               Change_Parameters_Default_Value_Command.Append_Code_Action
                 (Context                     => Context,
                  Commands_Vector             => Result,
                  Where                       =>
                    (uri     => Params.textDocument.uri,
                     span    => Params.span,
                     alsKind => LSP.Messages.Empty_Set));

               Found := True;
            end if;
         end Change_Parameters_Default_Value_Code_Action;

         ------------------------------------
         -- Extract_Subprogram_Code_Action --
         ------------------------------------

         procedure Extract_Subprogram_Code_Action is
            use LSP.Ada_Handlers.Refactor_Extract_Subprogram;
            use Langkit_Support.Slocs;
            use Laltools.Refactor.Extract_Subprogram;
            use type LSP.Messages.Position;

            Single_Location : constant Boolean :=
              Params.span.first = Params.span.last;

            Section_To_Extract_SLOC : constant Source_Location_Range :=
              (Langkit_Support.Slocs.Line_Number (Params.span.first.line) + 1,
               Langkit_Support.Slocs.Line_Number (Params.span.last.line) + 1,
               Column_Number (Params.span.first.character) + 1,
               Column_Number (Params.span.last.character) + 1);

            Available_Subprogram_Kinds : Available_Subprogram_Kinds_Type;

            Extract_Subprogram_Command : Command;

         begin
            if not Single_Location then
               if Is_Extract_Subprogram_Available
                 (Node.Unit,
                  Section_To_Extract_SLOC,
                  Available_Subprogram_Kinds)
               then
                  if Available_Subprogram_Kinds (Ada_Subp_Kind_Procedure) then
                     Extract_Subprogram_Command.Append_Code_Action
                       (Context                    => Context,
                        Commands_Vector            => Result,
                        Where                      =>
                          (Params.textDocument.uri,
                           Params.span,
                           LSP.Messages.Empty_Set),
                        Subprogram_Kind            =>
                          Ada_Subp_Kind_Procedure);
                  end if;

                  if Available_Subprogram_Kinds (Ada_Subp_Kind_Function) then
                     Extract_Subprogram_Command.Append_Code_Action
                       (Context                    => Context,
                        Commands_Vector            => Result,
                        Where                      =>
                          (Params.textDocument.uri,
                           Params.span,
                           LSP.Messages.Empty_Set),
                        Subprogram_Kind            =>
                          Ada_Subp_Kind_Function);
                  end if;

                  Found := True;
               end if;
            end if;
         end Extract_Subprogram_Code_Action;

         -------------------------------------
         -- Introduce_Parameter_Code_Action --
         -------------------------------------

         procedure Introduce_Parameter_Code_Action is
            use Langkit_Support.Slocs;
            use Laltools.Refactor.Introduce_Parameter;
            use LSP.Ada_Handlers.Refactor_Introduce_Parameter;

            Span : constant Source_Location_Range :=
              (Langkit_Support.Slocs.Line_Number (Params.span.first.line) + 1,
               Langkit_Support.Slocs.Line_Number (Params.span.last.line) + 1,
               Column_Number (Params.span.first.character) + 1,
               Column_Number (Params.span.last.character) + 1);

            Introduce_Parameter_Command : Command;

         begin
            if Is_Introduce_Parameter_Available
                 (Unit       => Node.Unit,
                  SLOC_Range => Span)
            then
               Introduce_Parameter_Command.Append_Code_Action
                 (Context         => Context,
                  Commands_Vector => Result,
                  Where           =>
                    (uri     => Params.textDocument.uri,
                     span    => Params.span,
                     alsKind => LSP.Messages.Empty_Set));

               Found := True;
            end if;
         end Introduce_Parameter_Code_Action;

         --------------------------------
         -- Import_Package_Code_Action --
         --------------------------------

         procedure Import_Package_Code_Action is
            use Libadalang.Analysis;
            use Laltools.Refactor_Imports;
            use LSP.Messages;

            Single_Location : constant Boolean :=
              Params.span.first = Params.span.last;

            Units_Vector : Libadalang.Helpers.Unit_Vectors.Vector;
            Units_Array  : constant Analysis_Unit_Array :=
              Context.Analysis_Units;

            Import_Suggestions : Import_Suggestions_Vector.Vector;

            function Is_Import_Suggestions_Available
              (This_Node : Ada_Node'Class)
               return Boolean;
            --  Checks if This_Node is a suitable node to get import
            --  suggestions. A suitable node must be an identifier, non
            --  defining and if it resolves, it must be to a declaration not
            --  declared in the standard package.
            --  This function also prepares Units_Vector with the right units
            --  where suggestions should be searched for.

            -------------------------------------
            -- Is_Import_Suggestions_Available --
            -------------------------------------

            function Is_Import_Suggestions_Available
              (This_Node : Ada_Node'Class)
               return Boolean
            is
               Aux_Node              : Ada_Node :=
                 (if This_Node.Is_Null then No_Ada_Node
                  else This_Node.As_Ada_Node);
               Referenced_Definition : Defining_Name := No_Defining_Name;

            begin
               --  Only get suggestions for Identifiers or Dotted_Names
               if Aux_Node.Is_Null
                 or else Aux_Node.Kind not in
                   Ada_Identifier_Range | Ada_Dotted_Name_Range
               then
                  return False;
               end if;

               --  Get the full Dotted_Name if applicable
               while not Aux_Node.Is_Null
                 and then not Aux_Node.Parent.Is_Null
                 and then Aux_Node.Parent.Kind in Ada_Dotted_Name_Range
               loop
                  Aux_Node := Aux_Node.Parent;
               end loop;

               --  Defining names do not need prefixes
               if Aux_Node.Is_Null or else Aux_Node.As_Name.P_Is_Defining then
                  return False;
               end if;

               Referenced_Definition :=
                 Aux_Node.As_Name.P_Referenced_Defining_Name;

               --  Declarations in the standard package do not need prefixes
               if not Referenced_Definition.Is_Null then
                  if Referenced_Definition.Unit = Node.P_Standard_Unit then
                     return False;
                  end if;
               end if;

               if Referenced_Definition.Is_Null then
                  --  The name could not be resolved so a full search needs to
                  --  be done.

                  for U of Units_Array loop
                     Units_Vector.Append (U);
                  end loop;

                  --  Add runtime analysis units for this context
                  --  ??? If possible, this should be cached.

                  for F in Self.Project_Predefined_Sources.Iterate loop
                     declare
                        VF : GNATCOLL.VFS.Virtual_File renames
                          LSP.Ada_File_Sets.File_Sets.Element (F);
                     begin
                        Units_Vector.Append
                          (Context.LAL_Context.Get_From_File
                             (VF.Display_Full_Name,
                              --  ??? What is the charset for predefined
                              --  files?
                              ""));
                     end;
                  end loop;

               else
                  --  Libadalang sometimes can resolve names that are not
                  --  withed.
                  --  For instance, with Ada.Text_IO, resolve
                  --  Ada.Text_IO.Put_Line, remove the Ada.Text_IO and then
                  --  resolve again Ada.Text_IO.Put_Line. Even though
                  --  Ada.Text_IO is no longer withed, Libadalang is still
                  --  able to resolve Put_Line.
                  --  For such cases, include only Referenced_Definition's
                  --  Analysis_Units and the tool will suggest the prefixes
                  --  (there can be more than one, for instance, when there
                  --  are nested packages.
                  Units_Vector.Append (Referenced_Definition.Unit);
               end if;

               return True;
            exception
               when others => return False;
            end Is_Import_Suggestions_Available;

         begin
            if not Single_Location
              or else not Is_Import_Suggestions_Available (Node)
            then
               return;
            end if;

            --  Get suggestions for all reachable declarations.
            --  Each suggestion contains a with clause and a
            --  prefix.

            Import_Suggestions :=
              Get_Import_Suggestions (Node, Units_Vector);

            --  Create a new codeAction command for each suggestion

            for Suggestion of Import_Suggestions loop
               declare
                  Command : LSP.Ada_Handlers.
                    Refactor_Imports_Commands.Command;
               begin
                  Command.Append_Suggestion
                    (Context         => Context,
                     Where           =>
                       LSP.Lal_Utils.Get_Node_Location (Node),
                     Commands_Vector => Result,
                     Suggestion      => Suggestion);
               end;
            end loop;

            if not Import_Suggestions.Is_Empty then
               Found := True;
            end if;
         end Import_Package_Code_Action;

         ----------------------------------
         -- Named_Parameters_Code_Action --
         ----------------------------------

         procedure Named_Parameters_Code_Action is
            Aux_Node : Libadalang.Analysis.Ada_Node := Node;
            Done     : Boolean := False;
            --  We propose only one choice of Named_Parameters refactoring per
            --  request. So, if a user clicks on `1` in `A (B (1))` we propose
            --  the refactoring for B (1), but not for A (...) call. We
            --  consider this as better user experience.
            --
            --  This boolean filter to detect such refactoring duplication.

            procedure Append_Command (Node : Libadalang.Analysis.Ada_Node);
            --  Contruct a command and append it to Result

            --------------------
            -- Append_Command --
            --------------------

            procedure Append_Command (Node : Libadalang.Analysis.Ada_Node) is
               Command : LSP.Ada_Handlers.Named_Parameters_Commands.Command;
               Pointer : LSP.Commands.Command_Pointer;
               Item    : LSP.Messages.CodeAction;
               Where   : constant LSP.Messages.Location :=
                 LSP.Lal_Utils.Get_Node_Location (Node);

            begin
               Command.Initialize
                 (Context => Context.all,
                  Where   => ((uri => Where.uri), Where.span.first));

               Pointer.Set (Command);

               Item :=
                 (title       => "Name parameters in the call",
                  kind        => (Is_Set => True,
                                  Value  => LSP.Messages.RefactorRewrite),
                  diagnostics => (Is_Set => False),
                  disabled    => (Is_Set => False),
                  edit        => (Is_Set => False),
                  isPreferred => (Is_Set => False),
                  command     => (Is_Set => True,
                                  Value  =>
                                    (Is_Unknown => False,
                                     title      => <>,
                                     Custom     => Pointer)));

               Result.Append (Item);

               Done := True;
               Found := True;
            end Append_Command;

         begin
            while not Done and then not Aux_Node.Is_Null loop
               case Aux_Node.Kind is
                  when Libadalang.Common.Ada_Stmt
                     | Libadalang.Common.Ada_Basic_Decl =>

                     Done := True;

                  when Libadalang.Common.Ada_Basic_Assoc_List =>
                     if Has_Assoc_Without_Designator
                          (Aux_Node.As_Basic_Assoc_List)
                     then
                        Append_Command (Aux_Node);
                     end if;

                  when Libadalang.Common.Ada_Call_Expr =>
                     declare
                        List : constant Libadalang.Analysis.Ada_Node :=
                          Aux_Node.As_Call_Expr.F_Suffix;

                     begin
                        if not List.Is_Null
                          and then List.Kind in
                                     Libadalang.Common.Ada_Basic_Assoc_List
                          and then Has_Assoc_Without_Designator
                                     (List.As_Basic_Assoc_List)
                        then
                           Append_Command (List);
                        end if;
                     end;
                  when others =>
                     null;
               end case;

               Aux_Node := Aux_Node.Parent;
            end loop;
         end Named_Parameters_Code_Action;

         -------------------------------------
         -- Pull_Up_Declaration_Code_Action --
         -------------------------------------

         procedure Pull_Up_Declaration_Code_Action is
            use Langkit_Support.Slocs;
            use Libadalang.Analysis;
            use Laltools.Refactor.Pull_Up_Declaration;
            use LSP.Ada_Handlers.Refactor_Pull_Up_Declaration;
            use LSP.Messages;

            --  This code action is not available when a range of text is
            --  selected.

            Single_Location : constant Boolean :=
              Params.span.first = Params.span.last;
            Location        : constant Source_Location :=
              (Langkit_Support.Slocs.Line_Number (Params.span.first.line) + 1,
               Column_Number (Params.span.first.character) + 1);

            Pull_Up_Declaration_Command :
              LSP.Ada_Handlers.Refactor_Pull_Up_Declaration.Command;

         begin
            if Single_Location
              and then Is_Pull_Up_Declaration_Available (Node.Unit, Location)
            then
               Pull_Up_Declaration_Command.Append_Code_Action
                 (Context                     => Context,
                  Commands_Vector             => Result,
                  Where                       =>
                    (uri     => Params.textDocument.uri,
                     span    => Params.span,
                     alsKind => Empty_Set));

               Found := True;
            end if;
         end Pull_Up_Declaration_Code_Action;

      begin
         Named_Parameters_Code_Action;

         Import_Package_Code_Action;

         --  Refactoring Code Actions

         --  Extract Subprogram
         Extract_Subprogram_Code_Action;

         --  Pull Up Declaration
         Pull_Up_Declaration_Code_Action;

         --  These refactorings are only available for clients that can
         --  provide user inputs:
         --  - Add Parameter
         --  - Change Parameters Type
         --  - Change Parameters Default Value

         --  Add Parameter
         if Self.Experimental_Client_Capabilities.
              Advanced_Refactorings (Add_Parameter)
         then
            declare
               use LSP.Ada_Handlers.Refactor_Add_Parameter;
               use Libadalang.Analysis;
               use Laltools.Refactor.Subprogram_Signature;
               use Langkit_Support.Slocs;
               use type LSP.Messages.Position;

               --  This code action is not available when a range of text is
               --  selected.

               Single_Location             : constant Boolean :=
                 Params.span.first = Params.span.last;
               Location                    : constant Source_Location :=
                 (if Single_Location then
                    (Langkit_Support.Slocs.Line_Number
                         (Params.span.first.line) + 1,
                     Column_Number (Params.span.first.character) + 1)
                  else
                     No_Source_Location);

               Requires_Full_Specification : Boolean;

               Add_Parameter_Commad : Command;

            begin
               if Single_Location
                 and then Is_Add_Parameter_Available
                   (Node.Unit,
                    Location,
                    Requires_Full_Specification)
               then
                  Add_Parameter_Commad.Append_Code_Action
                    (Context                     => Context,
                     Commands_Vector             => Result,
                     Where                       =>
                       (Params.textDocument.uri,
                        Params.span,
                        LSP.Messages.Empty_Set),
                     Requires_Full_Specification =>
                       Requires_Full_Specification);

                  Found := True;
               end if;
            end;
         end if;

         --  Change Parameters Type
         if Self.Experimental_Client_Capabilities.
              Advanced_Refactorings (Change_Parameters_Type)
         then
            Change_Parameters_Type_Code_Action;
         end if;

         --  Change Parameters Default Value
         if Self.Experimental_Client_Capabilities.
              Advanced_Refactorings (Change_Parameters_Default_Value)
         then
            Change_Parameters_Default_Value_Code_Action;
         end if;

         --  Remove Parameter
         declare
            use LSP.Ada_Handlers.Refactor_Remove_Parameter;
            use Libadalang.Analysis;
            use Laltools.Refactor.Subprogram_Signature;
            use Laltools.Refactor.Subprogram_Signature.Remove_Parameter;

            Target_Subp              : Basic_Decl := No_Basic_Decl;
            Parameter_Indices_Range  : Parameter_Indices_Range_Type;
            Remove_Parameter_Command : Command;

         begin
            if Is_Remove_Parameter_Available
              (Node, Target_Subp, Parameter_Indices_Range)
            then
               Remove_Parameter_Command.Append_Code_Action
                 (Context            => Context,
                  Commands_Vector    => Result,
                  Target_Subp        => Target_Subp,
                  Parameters_Indices => Parameter_Indices_Range);

               Found := True;
            end if;
         end;

         --  Move Parameter
         declare
            use LSP.Ada_Handlers.Refactor_Move_Parameter;
            use Libadalang.Analysis;
            use Laltools.Refactor.Subprogram_Signature;

            Target_Subp            : Basic_Decl := No_Basic_Decl;
            Parameter_Index        : Positive;
            Move_Directions        : Move_Direction_Availability_Type;
            Move_Parameter_Command : Command;

         begin
            if Is_Move_Parameter_Available
              (Node, Target_Subp, Parameter_Index, Move_Directions)
            then
               for Direction in Move_Direction_Type loop
                  if Move_Directions (Direction) then
                     Move_Parameter_Command.Append_Code_Action
                       (Context          => Context,
                        Commands_Vector  => Result,
                        Target_Subp      => Target_Subp,
                        Parameter_Index  => Parameter_Index,
                        Move_Direction   => Direction);
                  end if;
               end loop;

               Found := True;
            end if;
         end;

         --  Change Parameter Mode
         declare
            use LSP.Ada_Handlers.Refactor_Change_Parameter_Mode;
            use Libadalang.Analysis;
            use Laltools.Refactor.Subprogram_Signature;

            Target_Subp                   : Basic_Decl := No_Basic_Decl;
            Target_Parameters_Indices     : Parameter_Indices_Range_Type;
            Mode_Alternatives             : Mode_Alternatives_Type;
            Change_Parameter_Mode_Command : Command;

         begin
            if Is_Change_Mode_Available
              (Node, Target_Subp, Target_Parameters_Indices, Mode_Alternatives)
            then
               for Alternative of Mode_Alternatives loop
                  Change_Parameter_Mode_Command.Append_Code_Action
                    (Context            => Context,
                     Commands_Vector    => Result,
                     Target_Subp        => Target_Subp,
                     Parameters_Indices => Target_Parameters_Indices,
                     New_Mode           => Alternative);
               end loop;

               Found := True;
            end if;
         end;

         --  Introduce Parameter
         Introduce_Parameter_Code_Action;

         --  Suppress Subprogram
         declare
            use LSP.Ada_Handlers.Refactor_Suppress_Seperate;
            use Libadalang.Analysis;
            use Laltools.Refactor.Suppress_Separate;

            Target_Separate           : Basic_Decl := No_Basic_Decl;
            Suppress_Separate_Command : Command;
         begin
            if Is_Suppress_Separate_Available (Node, Target_Separate) then
               Suppress_Separate_Command.Append_Code_Action
                 (Context         => Context,
                  Commands_Vector => Result,
                  Target_Separate => Target_Separate);

               Found := True;
            end if;
         end;
      end Analyse_Node;

      ------------------------
      -- Analyse_In_Context --
      ------------------------

      procedure Analyse_In_Context
        (Context  : Context_Access;
         Document : LSP.Ada_Documents.Document_Access;
         Result   : out LSP.Messages.CodeAction_Vector;
         Found    : in out Boolean)
      is
         Node : constant Libadalang.Analysis.Ada_Node :=
           Document.Get_Node_At (Context.all, Params.span.first);
      begin
         if Node.Is_Null then
            Found := False;
            return;
         end if;

         Analyse_Node (Context, Node, Result, Found);
      end Analyse_In_Context;

      ----------------------------------------
      -- Append_Project_Status_Code_Actions --
      ----------------------------------------

      procedure Append_Project_Status_Code_Actions
        (Result : in out LSP.Messages.CodeAction_Vector)
      is
         use type VSS.Strings.Virtual_String;

         Diagnostics : LSP.Messages.Diagnostic_Vector;

      begin
         for Item of Params.context.diagnostics loop
            if Item.source.Is_Set and then Item.source.Value = "project" then
               Diagnostics.Append (Item);
            end if;
         end loop;

         case Self.Project_Status is
            when Valid_Project_Configured =>
               null;
            when Single_Project_Found | Multiple_Projects_Found =>
               declare
                  Item    : LSP.Messages.CodeAction;
                  Command : LSP.Messages.Command (Is_Unknown => True);
                  Arg     : constant LSP.Types.LSP_Any :=
                    LSP.Types.Create ("ada.projectFile");
               begin
                  Command.title := "Open settings for ada.projectFile";
                  Command.command := "workbench.action.openSettings";
                  Command.arguments := (Is_Set => True, Value => <>);
                  Command.arguments.Value.Append (Arg);

                  Item :=
                    (title       => Command.title,
                     kind        => (True, LSP.Messages.QuickFix),
                     diagnostics => (True, Diagnostics),
                     disabled    => (Is_Set => False),
                     edit        => (Is_Set => False),
                     isPreferred => LSP.Types.True,
                     command     => (True, Command));

                  Result.Append (Item);
               end;
            when No_Project_Found =>
               declare
                  Title  : constant VSS.Strings.Virtual_String :=
                    "Create a default project file (default.gpr)";
                  URI    : constant LSP.Messages.DocumentUri :=
                    LSP.Types.File_To_URI
                      (Self.Root.Join ("default.gpr").Display_Full_Name);
                  Create : constant LSP.Messages.Document_Change :=
                    (LSP.Messages.Create_File,
                     (kind => LSP.Messages.create,
                      uri  => URI,
                      others => <>));
                  Text   : constant LSP.Messages.AnnotatedTextEdit :=
                    ((span => ((0, 0), (0, 0)),
                      newText => "project Default is end Default;",
                      others => <>));
                  Insert : constant LSP.Messages.Document_Change :=
                    (LSP.Messages.Text_Document_Edit,
                     (textDocument => (uri => URI, others => <>),
                      edits        => LSP.Messages.To_Vector (Text, 1)));
                  Item   : LSP.Messages.CodeAction;
                  Edit   : LSP.Messages.WorkspaceEdit;
               begin
                  Edit.documentChanges.Append (Create);
                  Edit.documentChanges.Append (Insert);
                  Item :=
                    (title       => Title,
                     kind        => (True, LSP.Messages.QuickFix),
                     diagnostics => (True, Diagnostics),
                     disabled    => (Is_Set => False),
                     edit        => (True, Edit),
                     isPreferred => LSP.Types.True,
                     command     => (Is_Set => False));

                  Result.Append (Item);
               end;
            when Invalid_Project_Configured =>
               null;
         end case;
      end Append_Project_Status_Code_Actions;

      use type LSP.Messages.Position;

      Document : constant LSP.Ada_Documents.Document_Access :=
        Get_Open_Document (Self, Params.textDocument.uri);

      Response : LSP.Messages.Server_Responses.CodeAction_Response
        (Is_Error => False);

      Found : Boolean := False;
   begin
      if Document = null then
         Self.Log_Unexpected_Null_Document ("On_CodeAction_Request");
         return Response;
      end if;

      --  Find any context where we can do some refactoring
      for C of Self.Contexts_For_URI (Params.textDocument.uri) loop
         Analyse_In_Context (C, Document, Response.result, Found);

         exit when Request.Canceled or else Found;
      end loop;

      if Params.span.first = (0, 0) then
         Append_Project_Status_Code_Actions (Response.result);
      end if;

      return Response;
   end On_CodeAction_Request;

   --------------------------------
   -- On_Execute_Command_Request --
   --------------------------------

   overriding function On_Execute_Command_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Execute_Command_Request)
      return LSP.Messages.Server_Responses.ExecuteCommand_Response
   is
      Error    : LSP.Errors.Optional_ResponseError;
      Params   : LSP.Messages.ExecuteCommandParams renames
        Request.params;
      Response : LSP.Messages.Server_Responses.ExecuteCommand_Response
        (Is_Error => True);
   begin
      if Params.Is_Unknown or else Params.Custom.Is_Null then
         Response.error :=
           (True,
            (code => LSP.Errors.InternalError,
             message => "Not implemented",
             data    => <>));
         return Response;
      end if;

      Params.Custom.Unchecked_Get.Execute
        (Handler => Self,
         Client  => Self.Server,
         Error   => Error);

      if Error.Is_Set then
         Response.error := Error;
         return Response;
      end if;

      --  No particular response in case of success.
      return (Is_Error => False,
              error    => (Is_Set => False),
              others   => <>);
   end On_Execute_Command_Request;

   ----------------------------
   -- On_Declaration_Request --
   ----------------------------

   overriding function On_Declaration_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Declaration_Request)
      return LSP.Messages.Server_Responses.Location_Link_Response
   is

      Value   : LSP.Messages.DeclarationParams renames
        Request.params;
      Response   : LSP.Messages.Server_Responses.Location_Link_Response
        (Is_Error => False);
      Imprecise  : Boolean := False;

      Display_Method_Ancestry_Policy                                  :
      LSP.Messages.AlsDisplayMethodAncestryOnNavigationPolicy :=
        Self.Display_Method_Ancestry_Policy;

      Document : constant LSP.Ada_Documents.Document_Access :=
        Get_Open_Document (Self, Value.textDocument.uri);

   begin
      --  Override the displayMethodAncestryOnNavigation global configuration
      --  flag if there is on embedded in the request.
      if Value.alsDisplayMethodAncestryOnNavigation.Is_Set then
         Display_Method_Ancestry_Policy :=
           Value.alsDisplayMethodAncestryOnNavigation.Value;
      end if;

      for C of Self.Contexts_For_URI (Value.textDocument.uri) loop
         C.Append_Declarations
           (Document,
            LSP.Messages.TextDocumentPositionParams (Value),
            Display_Method_Ancestry_Policy,
            Response.result,
            Imprecise);

         exit when Request.Canceled;
      end loop;

      if Imprecise then
         Self.Show_Imprecise_Reference_Warning ("declaration");
      end if;

      Sort_And_Remove_Duplicates (Response.result.Locations);
      return Response;
   end On_Declaration_Request;

   -------------------------------
   -- On_Implementation_Request --
   -------------------------------

   overriding function On_Implementation_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Implementation_Request)
      return LSP.Messages.Server_Responses.Location_Link_Response
   is
      use Libadalang.Analysis;
      use LSP.Messages;

      Value   : LSP.Messages.ImplementationParams renames
        Request.params;
      Response   : LSP.Messages.Server_Responses.Location_Link_Response
        (Is_Error => False);
      Imprecise  : Boolean := False;

      Display_Method_Ancestry_Policy                                  :
      LSP.Messages.AlsDisplayMethodAncestryOnNavigationPolicy :=
        Self.Display_Method_Ancestry_Policy;

      Document : constant LSP.Ada_Documents.Document_Access :=
        Get_Open_Document (Self, Value.textDocument.uri);

      procedure Resolve_In_Context (C : Context_Access);
      --  Utility function to gather results on one context

      ------------------------
      -- Resolve_In_Context --
      ------------------------

      procedure Resolve_In_Context (C : Context_Access) is
         Name_Node      : constant Name := Laltools.Common.Get_Node_As_Name
           (C.Get_Node_At (Document, Value));

         procedure Update_Response
           (Bodies : Laltools.Common.Bodies_List.List;
            Kind   : LSP.Messages.AlsReferenceKind_Set);
         --  Utility function to update response with the bodies

         ---------------------
         -- Update_Response --
         ---------------------

         procedure Update_Response
           (Bodies : Laltools.Common.Bodies_List.List;
            Kind   : LSP.Messages.AlsReferenceKind_Set)
         is
         begin
            for E of Bodies loop
               Append_Location
                 (Response.result,
                  E,
                  Kind);
            end loop;
         end Update_Response;

         Definition         : Defining_Name;
         This_Imprecise     : Boolean;
         Find_All_Imprecise : Boolean;
         Decl               : Basic_Decl;

      begin
         if Name_Node = No_Name then
            return;
         end if;

         --  Find the definition
         Definition := Laltools.Common.Resolve_Name
           (Name_Node, Self.Trace, This_Imprecise);
         Imprecise := Imprecise or This_Imprecise;

         --  If we didn't find a definition, give up for this context
         if Definition = No_Defining_Name then
            return;
         end if;

         --  First list the bodies of this definition
         Update_Response
           (Laltools.Common.List_Bodies_Of
              (Definition, Self.Trace, Imprecise),
            LSP.Messages.Empty_Set);

         --  Then list the bodies of the parent implementations
         Decl := Definition.P_Basic_Decl;

         --  Display overriding/overridden subprograms depending on the
         --  displayMethodAncestryOnNavigation flag.
         if Display_Method_Ancestry_Policy in Definition_Only | Always
           or else
             (Display_Method_Ancestry_Policy = Usage_And_Abstract_Only
                     and then Decl.Kind in Ada_Abstract_Subp_Decl_Range)
         then
            for Subp of C.Find_All_Base_Declarations (Decl, Find_All_Imprecise)
            loop
               Update_Response
                 (Laltools.Common.List_Bodies_Of
                    (Subp.P_Defining_Name, Self.Trace, This_Imprecise),
                  Is_Parent);
               Imprecise := Imprecise or This_Imprecise;
            end loop;
            Imprecise := Imprecise or Find_All_Imprecise;

            --  And finally the bodies of child implementations
            for Subp of C.Find_All_Overrides (Decl, Find_All_Imprecise) loop
               Update_Response
                 (Laltools.Common.List_Bodies_Of
                    (Subp.P_Defining_Name, Self.Trace, This_Imprecise),
                  Is_Child);
               Imprecise := Imprecise or This_Imprecise;
            end loop;
            Imprecise := Imprecise or Find_All_Imprecise;
         end if;
      end Resolve_In_Context;

   begin
      --  Override the displayMethodAncestryOnNavigation global configuration
      --  flag if there is on embedded in the request.
      if Value.alsDisplayMethodAncestryOnNavigation.Is_Set then
         Display_Method_Ancestry_Policy :=
           Value.alsDisplayMethodAncestryOnNavigation.Value;
      end if;

      for C of Self.Contexts_For_URI (Value.textDocument.uri) loop
         Resolve_In_Context (C);

         exit when Request.Canceled;
      end loop;

      if Imprecise then
         Self.Show_Imprecise_Reference_Warning ("implementation");
      end if;

      Sort_And_Remove_Duplicates (Response.result.Locations);
      return Response;
   end On_Implementation_Request;

   ---------------------------
   -- On_Definition_Request --
   ---------------------------

   overriding function On_Definition_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Definition_Request)
      return LSP.Messages.Server_Responses.Location_Link_Response
   is
      use Libadalang.Analysis;
      use LSP.Messages;

      Value      : LSP.Messages.DefinitionParams renames
        Request.params;
      Response   : LSP.Messages.Server_Responses.Location_Link_Response
        (Is_Error => False);
      Imprecise  : Boolean := False;

      Display_Method_Ancestry_Policy                          :
      LSP.Messages.AlsDisplayMethodAncestryOnNavigationPolicy :=
        Self.Display_Method_Ancestry_Policy;

      Document : constant LSP.Ada_Documents.Document_Access :=
        Get_Open_Document (Self, Value.textDocument.uri);

      procedure Resolve_In_Context (C : Context_Access);
      --  Utility function, appends to Resonse.result all results of the
      --  definition requests found in context C.

      ------------------------
      -- Resolve_In_Context --
      ------------------------

      procedure Resolve_In_Context (C : Context_Access) is
         Name_Node               : constant Name :=
                                     Laltools.Common.Get_Node_As_Name
                                       (C.Get_Node_At (Document, Value));
         Definition              : Defining_Name;
         Other_Part              : Defining_Name;
         Manual_Fallback         : Defining_Name;
         Definition_Node         : Basic_Decl := No_Basic_Decl;
         Decl_For_Find_Overrides : Basic_Decl := No_Basic_Decl;
         Entry_Decl_Node         : Entry_Decl := No_Entry_Decl;
      begin
         if Name_Node = No_Name then
            return;
         end if;

         --  Check if we are on some defining name
         Definition := Laltools.Common.Get_Name_As_Defining (Name_Node);

         if Definition = No_Defining_Name then
            Self.Imprecise_Resolve_Name (C, Value, Definition);

            if Definition /= No_Defining_Name then
               Append_Location (Response.result, Definition);

               if Display_Method_Ancestry_Policy
                  in Usage_And_Abstract_Only | Always
               then
                  Decl_For_Find_Overrides := Definition.P_Basic_Decl;
               end if;
            end if;
         else  --  If we are on a defining_name already
            Other_Part := Laltools.Common.Find_Next_Part
              (Definition, Self.Trace);

            Definition_Node := Definition.P_Basic_Decl;

            --  Search for overriding subprograms only if we are on an
            --  abstract subprogram.
            if Display_Method_Ancestry_Policy = Never
              or else
                (Display_Method_Ancestry_Policy = Usage_And_Abstract_Only
                        and then Definition_Node.Kind
                        not in Ada_Abstract_Subp_Decl_Range)
            then
               Decl_For_Find_Overrides := No_Basic_Decl;
            else
               Decl_For_Find_Overrides := Definition_Node;
            end if;

            --  Search for accept statements only if we are on an entry
            if Definition_Node.Kind in Ada_Entry_Decl_Range then
               Entry_Decl_Node := Definition_Node.As_Entry_Decl;
            elsif Definition_Node.Kind in
              Ada_Single_Task_Type_Decl_Range | Ada_Protected_Type_Decl_Range
            then
               --  These node types are not handled by Find_Next_Part
               --  (LAL design limitations)
               declare
                  Other_Part_For_Decl : constant Basic_Decl :=
                    Laltools.Common.Find_Next_Part_For_Decl
                      (Definition_Node, Self.Trace);
               begin
                  if Other_Part_For_Decl /= No_Basic_Decl then
                     Other_Part := Other_Part_For_Decl.P_Defining_Name;
                  end if;
               end;
            end if;

            if Other_Part = No_Defining_Name then
               --  No next part is found. Check first defining name
               Other_Part := Laltools.Common.Find_Canonical_Part
                 (Definition, Self.Trace);
            end if;

            if Other_Part /= No_Defining_Name then
               Append_Location (Response.result, Other_Part);
            else
               --  We were on a defining name, but did not manage to find
               --  an answer using Find_Next_Part / Find_Canonical_Part.
               --  Use the manual fallback to attempt to find a good enough
               --  result.
               Manual_Fallback := Laltools.Common.Find_Other_Part_Fallback
                 (Definition, Self.Trace);

               if Manual_Fallback /= No_Defining_Name then
                  --  We have found a result using the imprecise heuristics.
                  --  We'll warn the user and send the result.
                  Imprecise := True;
                  Append_Location (Response.result, Manual_Fallback);
               end if;
            end if;
         end if;

         if Decl_For_Find_Overrides /= Libadalang.Analysis.No_Basic_Decl then
            declare
               Imprecise_Over       : Boolean;
               Imprecise_Base       : Boolean;
               Overriding_Subps     : constant Basic_Decl_Array :=
                 C.Find_All_Overrides
                   (Decl_For_Find_Overrides,
                    Imprecise_Results => Imprecise_Over);
               Base_Subps           : constant Basic_Decl_Array :=
                 C.Find_All_Base_Declarations
                   (Decl_For_Find_Overrides,
                    Imprecise_Results => Imprecise_Base);
            begin
               for Subp of Base_Subps loop
                  Append_Location
                    (Response.result, Subp.P_Defining_Name, Is_Parent);
               end loop;
               for Subp of Overriding_Subps loop
                  Append_Location
                    (Response.result, Subp.P_Defining_Name, Is_Child);
               end loop;
               Imprecise := Imprecise or Imprecise_Over or Imprecise_Base;
            end;
         end if;

         if Entry_Decl_Node /= Libadalang.Analysis.No_Entry_Decl then
            for Accept_Node of Entry_Decl_Node.P_Accept_Stmts loop
               Append_Location (Response.result, Accept_Node.F_Name);
            end loop;
         end if;
      end Resolve_In_Context;

   begin
      --  Override the displayMethodAncestryOnNavigation global configuration
      --  flag if there is on embedded in the request.
      if Value.alsDisplayMethodAncestryOnNavigation.Is_Set then
         Display_Method_Ancestry_Policy :=
           Value.alsDisplayMethodAncestryOnNavigation.Value;
      end if;

      for C of Self.Contexts_For_URI (Value.textDocument.uri) loop
         Resolve_In_Context (C);

         exit when Request.Canceled;
      end loop;

      if Imprecise then
         Self.Show_Imprecise_Reference_Warning ("definition");
      end if;

      Sort_And_Remove_Duplicates (Response.result.Locations);
      return Response;
   end On_Definition_Request;

   --------------------------------
   -- On_Type_Definition_Request --
   --------------------------------

   overriding function On_Type_Definition_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Type_Definition_Request)
      return LSP.Messages.Server_Responses.Location_Link_Response
   is
      use Libadalang.Analysis;

      Position   : LSP.Messages.TextDocumentPositionParams renames
        Request.params;
      Response   : LSP.Messages.Server_Responses.Location_Link_Response
        (Is_Error => False);
      Imprecise  : Boolean := False;

      Document : constant LSP.Ada_Documents.Document_Access :=
        Get_Open_Document (Self, Position.textDocument.uri);

      procedure Resolve_In_Context (C : Context_Access);
      --  Utility function to gather results on one context

      ------------------------
      -- Resolve_In_Context --
      ------------------------

      procedure Resolve_In_Context (C : Context_Access) is
         Name_Node      : constant Name := Laltools.Common.Get_Node_As_Name
             (C.Get_Node_At (Document, Position));
         Definition     : Defining_Name;
         Type_Decl : Base_Type_Decl;
      begin
         if Name_Node = No_Name then
            return;
         end if;

         if Name_Node.P_Is_Defining then
            --  Special case if Name_Node is defining, for instance on the X in
            --      X : My_Type;
            declare
               Def_Name : constant Defining_Name :=
                 Name_Node.P_Enclosing_Defining_Name;
               Type_Expr : constant Libadalang.Analysis.Type_Expr :=
                 Def_Name.P_Basic_Decl.P_Type_Expression;
            begin
               if not Type_Expr.Is_Null then
                  Definition := Laltools.Common.Resolve_Name
                    (Type_Expr.P_Type_Name, Self.Trace, Imprecise);
               end if;
            end;
         else
            --  Name_Node is not defining. In this case we can rely on
            --  P_Expression_Type.
            Type_Decl := Name_Node.P_Expression_Type;

            --  P_Expression_Type returns the entire expression: narrow the
            --  result down to the type declaration. Here we assume that the
            --  first defining name in this expression is the name of the type.
            if Type_Decl /= No_Type_Decl then
               Definition := Type_Decl.P_Defining_Name;
            end if;
         end if;

         if Definition /= No_Defining_Name then
            Append_Location (Response.result, Definition);
         end if;
      end Resolve_In_Context;

   begin
      for C of Self.Contexts_For_URI (Position.textDocument.uri) loop
         Resolve_In_Context (C);

         exit when Request.Canceled;
      end loop;

      return Response;
   end On_Type_Definition_Request;

   -------------------------------------------
   -- On_DidChangeTextDocument_Notification --
   -------------------------------------------

   overriding procedure On_DidChangeTextDocument_Notification
     (Self  : access Message_Handler;
      Value : LSP.Messages.DidChangeTextDocumentParams)
   is
      function Skip_Did_Change return Boolean;
      --  Check if the following message in the queue is didChange for
      --  the same document

      ---------------------
      -- Skip_Did_Change --
      ---------------------

      function Skip_Did_Change return Boolean is
         use type LSP.Servers.Message_Access;

         subtype DidChangeTextDocument_Notification is LSP.Messages
           .Server_Notifications.DidChangeTextDocument_Notification;

         Next : constant LSP.Servers.Message_Access :=
           Self.Server.Look_Ahead_Message;
      begin
         if Next = null
           or else Next.all not in
             DidChangeTextDocument_Notification'Class
         then
            return False;
         end if;

         declare
            Object : DidChangeTextDocument_Notification'Class renames
              DidChangeTextDocument_Notification'Class (Next.all);
            Object_File : constant GNATCOLL.VFS.Virtual_File :=
              Self.To_File (Object.params.textDocument.uri);
            Value_File : constant GNATCOLL.VFS.Virtual_File :=
              Self.To_File (Value.textDocument.uri);
         begin
            if Object_File /= Value_File then
               return False;
            end if;
         end;

         return True;
      end Skip_Did_Change;

      Document : constant LSP.Ada_Documents.Document_Access :=
        Get_Open_Document (Self, Value.textDocument.uri);
   begin
      if Document = null then
         Self.Log_Unexpected_Null_Document
           ("On_DidChangeTextDocument_Notification");
      end if;

      if Allow_Incremental_Text_Changes.Active then
         --  If we are applying incremental changes, we can't skip the
         --  call to Apply_Changes, since this would break synchronization.
         Document.Apply_Changes
           (Value.textDocument.version,
            Value.contentChanges);

         --  However, we should skip the Indexing part if the next change in
         --  the queue will re-change the text document.
         if Skip_Did_Change then
            return;
         end if;
      else
         --  If we are not applying incremental changes, we can skip
         --  Apply_Changes: the next change will contain the full text.
         if Skip_Did_Change then
            return;
         end if;
         Document.Apply_Changes
           (Value.textDocument.version,
            Value.contentChanges);
      end if;

      --  Reindex the document in each of the contexts where it is relevant

      for Context of Self.Contexts_For_URI (Value.textDocument.uri) loop
         Context.Index_Document (Document.all);
      end loop;

      --  Emit diagnostics
      Self.Publish_Diagnostics (Document);
   end On_DidChangeTextDocument_Notification;

   ------------------------------------------
   -- On_DidCloseTextDocument_Notification --
   ------------------------------------------

   overriding procedure On_DidCloseTextDocument_Notification
     (Self  : access Message_Handler;
      Value : LSP.Messages.DidCloseTextDocumentParams)
   is
      URI      : LSP.Messages.DocumentUri renames Value.textDocument.uri;
      File     : constant GNATCOLL.VFS.Virtual_File := Self.To_File (URI);
      Diag     : LSP.Messages.PublishDiagnosticsParams;
      Document : Internal_Document_Access;
   begin
      if Self.Open_Documents.Contains (File) then
         Document := Self.Open_Documents.Element (File);

         --  Remove the URI from the set of open documents now: this way,
         --  the call to Flush_Document below will not attempt to reindex
         --  from an open document, but from the file on disk.
         Self.Open_Documents.Delete (File);

         for Context of Self.Contexts_For_URI (URI) loop
            Context.Flush_Document (File);
         end loop;

         Free (Document);

      else
         --  We have received a didCloseTextDocument but the document was
         --  not open: this is not supposed to happen, log it.

         Self.Trace.Trace
           ("received a didCloseTextDocument for non-open document with uri: "
            & To_UTF_8_String (URI));
      end if;

      --  Clean diagnostics up on closing document
      if Self.Diagnostics_Enabled then
         Diag.uri := URI;
         Self.Server.On_Publish_Diagnostics (Diag);
      end if;
   end On_DidCloseTextDocument_Notification;

   -----------------------------------------
   -- On_DidOpenTextDocument_Notification --
   -----------------------------------------

   overriding procedure On_DidOpenTextDocument_Notification
     (Self  : access Message_Handler;
      Value : LSP.Messages.DidOpenTextDocumentParams)
   is
      URI    : LSP.Messages.DocumentUri renames Value.textDocument.uri;
      File   : constant GNATCOLL.VFS.Virtual_File := Self.To_File (URI);
      Object : constant Internal_Document_Access :=
        new LSP.Ada_Documents.Document (Self.Trace);
      Diag   : constant LSP.Diagnostic_Sources.Diagnostic_Source_Access :=
        new LSP.Ada_Handlers.Project_Diagnostics.Diagnostic_Source (Self);
   begin
      Self.Trace.Trace ("In Text_Document_Did_Open");
      Self.Trace.Trace ("Uri : " & To_UTF_8_String (URI));

      --  Some clients don't properly call initialize, or don't pass the
      --  project to didChangeConfiguration: fallback here on loading a
      --  project in this directory, if needed.
      Self.Ensure_Project_Loaded (URI);

      --  We have received a document: add it to the documents container
      Object.Initialize (URI, Value.textDocument.text, Diag);
      Self.Open_Documents.Include (File, Object);

      --  Handle the case where we're loading the implicit project: do
      --  we need to add the directory in which the document is open?

      if Self.Project_Status in Implicit_Project_Loaded then
         declare
            Dir : constant Virtual_File := Self.To_File (URI).Dir;
         begin
            if not Self.Project_Dirs_Loaded.Contains (Dir) then
               --  We do need to add this directory
               Self.Project_Dirs_Loaded.Insert (Dir);
               Self.Reload_Implicit_Project_Dirs;
            end if;
         end;
      end if;

      --  Index the document in all the contexts where it is relevant
      for Context of Self.Contexts_For_URI (URI) loop
         Context.Index_Document (Object.all);
      end loop;

      --  Emit diagnostics
      Self.Publish_Diagnostics (LSP.Ada_Documents.Document_Access (Object));

      Self.Trace.Trace ("Finished Text_Document_Did_Open");
   end On_DidOpenTextDocument_Notification;

   ------------------------------
   -- On_Folding_Range_Request --
   ------------------------------

   overriding function On_Folding_Range_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Folding_Range_Request)
      return LSP.Messages.Server_Responses.FoldingRange_Response
   is
      Value : LSP.Messages.FoldingRangeParams renames
        Request.params;

      Context  : constant Context_Access :=
        Self.Contexts.Get_Best_Context (Value.textDocument.uri);
      Document : constant LSP.Ada_Documents.Document_Access :=
        Get_Open_Document (Self, Value.textDocument.uri);
      Result   : LSP.Messages.FoldingRange_Vector;

      package Canceled is new LSP.Generic_Cancel_Check (Request'Access, 127);

   begin
      if Document /= null then
         Document.Get_Folding_Blocks
           (Context.all,
            Self.Line_Folding_Only,
            Self.Options.Folding.Comments,
            Canceled.Has_Been_Canceled'Access,
            Result);

         return Response : LSP.Messages.Server_Responses.FoldingRange_Response
           (Is_Error => False)
         do
            if not Canceled.Has_Been_Canceled then
               Response.result := Result;
            end if;
         end return;

      else
         return Response : LSP.Messages.Server_Responses.FoldingRange_Response
           (Is_Error => True)
         do
            Response.error :=
              (True,
               (code => LSP.Errors.InternalError,
                message => "Document is not opened",
                data => <>));
         end return;
      end if;
   end On_Folding_Range_Request;

   --------------------------------
   -- On_Selection_Range_Request --
   --------------------------------

   overriding function On_Selection_Range_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Selection_Range_Request)
      return LSP.Messages.Server_Responses.SelectionRange_Response
   is
      pragma Unreferenced (Self, Request);
      Response : LSP.Messages.Server_Responses.SelectionRange_Response
        (Is_Error => True);
   begin
      Response.error :=
        (True,
         (code => LSP.Errors.InternalError,
          message => "Not implemented",
          data => <>));
      return Response;
   end On_Selection_Range_Request;

   --------------------------
   -- On_Highlight_Request --
   --------------------------

   overriding function On_Highlight_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Highlight_Request)
      return LSP.Messages.Server_Responses.Highlight_Response
   is
      use Libadalang.Analysis;
      use LSP.Messages;

      Value      : LSP.Messages.TextDocumentPositionParams renames
        Request.params;
      Context    : constant Context_Access :=
        Self.Contexts.Get_Best_Context (Value.textDocument.uri);
      Document   : constant LSP.Ada_Documents.Document_Access :=
        Get_Open_Document (Self, Value.textDocument.uri);
      File : constant GNATCOLL.VFS.Virtual_File :=
        Self.To_File (Value.textDocument.uri);
      Response   : LSP.Messages.Server_Responses.Highlight_Response
        (Is_Error => False);
      Definition : Defining_Name;

      procedure Callback
        (Node   : Libadalang.Analysis.Base_Id;
         Kind   : Libadalang.Common.Ref_Result_Kind;
         Cancel : in out Boolean);
      --  Called on each found reference. Used to append the reference to the
      --  final result.

      function Get_Highlight_Kind
        (Node : Ada_Node) return LSP.Messages.Optional_DocumentHighlightKind;
      --  Fetch highlight kind for given node

      ------------------------
      -- Get_Highlight_Kind --
      ------------------------

      function Get_Highlight_Kind
        (Node : Ada_Node) return LSP.Messages.Optional_DocumentHighlightKind
      is
         Id : constant Name := Laltools.Common.Get_Node_As_Name (Node);
      begin
         if Id.P_Is_Write_Reference then
            return LSP.Messages.Optional_DocumentHighlightKind'
              (Is_Set => True, Value => Write);
         else
            return LSP.Messages.Optional_DocumentHighlightKind'
              (Is_Set => True, Value  => Read);
         end if;
      end Get_Highlight_Kind;

      --------------
      -- Callback --
      --------------

      procedure Callback
        (Node   : Libadalang.Analysis.Base_Id;
         Kind   : Libadalang.Common.Ref_Result_Kind;
         Cancel : in out Boolean)
      is
         pragma Unreferenced (Kind);
         pragma Unreferenced (Cancel);

      begin
         if not Laltools.Common.Is_End_Label (Node.As_Ada_Node) then
            Append_Location
              (Result   => Response.result,
               Document => Document,
               File     => File,
               Node     => Node,
               Kind     => Get_Highlight_Kind (Node.As_Ada_Node));
         end if;

      end Callback;

   begin
      if Document /= null then
         Self.Imprecise_Resolve_Name (Context, Value, Definition);

         if Definition = No_Defining_Name or else Request.Canceled then
            return Response;
         end if;

         --  Find all references will return all the references except the
         --  declaration ...
         Document.Find_All_References
           (Context    => Context.all,
            Definition => Definition,
            Callback   => Callback'Access);

         --  ... add it manually
         Append_Location
           (Result   => Response.result,
            Document => Document,
            File     => File,
            Node     => Definition,
            Kind     => Get_Highlight_Kind (Definition.As_Ada_Node));
      end if;
      return Response;
   end On_Highlight_Request;

   ----------------------
   -- On_Hover_Request --
   ----------------------

   overriding function On_Hover_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Hover_Request)
      return LSP.Messages.Server_Responses.Hover_Response
   is
      use Libadalang.Analysis;

      Value    : LSP.Messages.TextDocumentPositionParams renames
        Request.params;
      Response : LSP.Messages.Server_Responses.Hover_Response
        (Is_Error => False);

      Defining_Name_Node : Defining_Name;
      Decl               : Basic_Decl;
      Decl_Lines         : VSS.String_Vectors.Virtual_String_Vector;
      Decl_Text          : VSS.Strings.Virtual_String;
      Comments_Lines     : VSS.String_Vectors.Virtual_String_Vector;
      Comments_Text      : VSS.Strings.Virtual_String;
      Location_Text      : VSS.Strings.Virtual_String;

      C : constant Context_Access :=
        Self.Contexts.Get_Best_Context (Value.textDocument.uri);
      --  For the Hover request, we're only interested in the "best"
      --  response value, not in the list of values for all contexts

      Options : constant
        GNATdoc.Comments.Options.Extractor_Options :=
          (Style    => Self.Options.Documentation.Style,
           Fallback => True);

   begin
      Self.Imprecise_Resolve_Name (C, Value, Defining_Name_Node);

      if Defining_Name_Node = No_Defining_Name then
         return Response;
      end if;

      --  Get the associated basic declaration
      Decl := Defining_Name_Node.P_Basic_Decl;

      if Decl = No_Basic_Decl or else Request.Canceled then
         return Response;
      end if;

      --  Extract documentation with GNATdoc when supported.

      GNATdoc.Comments.Helpers.Get_Plain_Text_Documentation
        (Defining_Name_Node, Options, Decl_Lines, Comments_Lines);

      Decl_Text := Decl_Lines.Join_Lines (VSS.Strings.LF, False);
      Comments_Text := Comments_Lines.Join_Lines (VSS.Strings.LF, False);

      --  Obtain documentation when GNATdoc support is missing.

      if Comments_Text.Is_Empty then
         Comments_Text :=
           VSS.Strings.To_Virtual_String
             (Libadalang.Doc_Utils.Get_Documentation (Decl).Doc.To_String);
      end if;

      if Decl_Text.Is_Empty
        or else not Decl.P_Subp_Spec_Or_Null.Is_Null
      then
         --  For subprograms additional information is added, use old code to
         --  obtain it yet.

         Decl_Text := Get_Hover_Text (Decl, Decl_Lines);
      end if;

      if Decl_Text.Is_Empty then
         return Response;
      end if;

      --  Append the whole declaration text to the response
      Response.result := (Is_Set => True, others => <>);

      Response.result.Value.contents.Vector.Append
        (LSP.Messages.MarkedString'
           (Is_String => False,
            value     => Decl_Text,
            language  => "ada"));

      --  Append the declaration's location.
      --  In addition, append the project's name if we are dealing with an
      --  aggregate project.

      Location_Text := LSP.Lal_Utils.Node_Location_Image (Decl);

      if Self.Project_Tree.Root_Project.Is_Aggregate_Project then
         Location_Text.Append (" in project ");
         Location_Text.Append (C.Id);
      end if;

      Response.result.Value.contents.Vector.Append
        (LSP.Messages.MarkedString'
           (Is_String => True,
            value     => Location_Text));

      --  Append the comments associated with the basic declaration
      --  if any.

      if not Comments_Text.Is_Empty then
         Response.result.Value.contents.Vector.Append
           (LSP.Messages.MarkedString'
              (Is_String => False,
               language  => "plaintext",
               value     => Comments_Text));
      end if;

      return Response;
   end On_Hover_Request;

   ---------------------------
   -- On_References_Request --
   ---------------------------

   overriding function On_References_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.References_Request)
      return LSP.Messages.Server_Responses.Location_Response
   is
      use Libadalang.Analysis;

      Value      : LSP.Messages.ReferenceParams renames Request.params;
      Response   : LSP.Messages.Server_Responses.Location_Response
        (Is_Error => False);
      Imprecise  : Boolean := False;

      Additional_Kinds : LSP.Messages.AlsReferenceKind_Array :=
        [others => False];

      procedure Process_Context (C : Context_Access);
      --  Process the references found in one context and append
      --  them to Response.results.

      function Get_Reference_Kind
        (Node               : Ada_Node;
         Is_Overriding_Decl : Boolean := False)
         return LSP.Messages.AlsReferenceKind_Set;
      --  Fetch reference kind for given node.

      ------------------------
      -- Get_Reference_Kind --
      ------------------------

      function Get_Reference_Kind
        (Node               : Ada_Node;
         Is_Overriding_Decl : Boolean := False)
         return LSP.Messages.AlsReferenceKind_Set
      is
         use LSP.Messages;

         Id     : constant Name := Laltools.Common.Get_Node_As_Name (Node);
         Result : LSP.Messages.AlsReferenceKind_Set := LSP.Messages.Empty_Set;
      begin
         begin
            Result.As_Flags (LSP.Messages.Write) := Id.P_Is_Write_Reference;
         exception
            when E : Libadalang.Common.Property_Error =>
               Log (Self.Trace, E);
         end;

         begin
            Result.As_Flags (LSP.Messages.Access_Ref) :=
              Laltools.Common.Is_Access_Ref (Id.As_Ada_Node);
         exception
            when E : Libadalang.Common.Property_Error =>
               Log (Self.Trace, E);
         end;

         begin
            Result.As_Flags (LSP.Messages.Static_Call) := Id.P_Is_Static_Call;
         exception
            when E : Libadalang.Common.Property_Error =>
               Log (Self.Trace, E);
         end;

         begin
            Result.As_Flags (LSP.Messages.Dispatching_Call) :=
              Id.P_Is_Dispatching_Call;
         exception
            when E : Libadalang.Common.Property_Error =>
               Log (Self.Trace, E);
         end;

         begin
            Result.As_Flags (LSP.Messages.Child) :=
              Laltools.Common.Is_Type_Derivation (Id.As_Ada_Node);
         exception
            when E : Libadalang.Common.Property_Error =>
               Log (Self.Trace, E);
         end;

         Result.As_Flags (LSP.Messages.Overriding_Decl) := Is_Overriding_Decl;

         --  If the result has not any set flags at this point, flag it as a
         --  simple reference.
         if Result.As_Flags = AlsReferenceKind_Array'(others => False) then
            Result.As_Flags (LSP.Messages.Simple) := True;
         end if;

         --  Apply additional kinds
         Result.As_Flags := Result.As_Flags or Additional_Kinds;

         return Result;
      end Get_Reference_Kind;

      ---------------------
      -- Process_Context --
      ---------------------

      procedure Process_Context (C : Context_Access) is
         procedure Callback
           (Node   : Libadalang.Analysis.Base_Id;
            Kind   : Libadalang.Common.Ref_Result_Kind;
            Cancel : in out Boolean);

         Count : Cancel_Countdown := 0;

         procedure Callback
           (Node   : Libadalang.Analysis.Base_Id;
            Kind   : Libadalang.Common.Ref_Result_Kind;
            Cancel : in out Boolean) is
         begin
            Imprecise := Imprecise or Kind = Libadalang.Common.Imprecise;

            if not Laltools.Common.Is_End_Label (Node.As_Ada_Node) then
               Count := Count - 1;

               Append_Location
                 (Response.result,
                  Node,
                  Get_Reference_Kind (Node.As_Ada_Node));
            end if;

            Cancel := Count = 0 and then Request.Canceled;
         end Callback;

         Definition : Defining_Name;

      begin

         Self.Imprecise_Resolve_Name (C, Value, Definition);

         if Definition = No_Defining_Name or else Request.Canceled then
            return;
         end if;

         --  Set additional "reference" kind for enumeration literal
         declare
            Decl : constant Basic_Decl := P_Basic_Decl (Definition);
         begin
            if Decl /= No_Basic_Decl
              and then Kind (Decl) = Ada_Enum_Literal_Decl
            then
               Additional_Kinds (LSP.Messages.Simple) := True;
            end if;

            --  Find all the references
            C.Find_All_References (Definition, Callback'Access);

            --  Find all the overriding declarations, if any
            for Subp of C.Find_All_Overrides (Decl, Imprecise) loop
               Append_Location
                 (Response.result,
                  Subp.P_Defining_Name,
                  Get_Reference_Kind
                    (Definition.As_Ada_Node,
                     Is_Overriding_Decl => True));
            end loop;

            if Value.context.includeDeclaration then
               Append_Location
                 (Response.result,
                  Definition,
                  Get_Reference_Kind (Definition.As_Ada_Node));
            end if;
         end;
      end Process_Context;

   begin
      for C of Self.Contexts_For_URI (Value.textDocument.uri) loop
         Process_Context (C);

         exit when Request.Canceled;
      end loop;

      if Imprecise then
         Self.Show_Imprecise_Reference_Warning ("references");
      end if;

      Sort_And_Remove_Duplicates (Response.result);
      return Response;
   end On_References_Request;

   --------------------------------
   -- On_ALS_Source_Dirs_Request --
   --------------------------------

   overriding function On_ALS_Source_Dirs_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.ALS_Source_Dirs_Request)
      return LSP.Messages.Server_Responses.ALS_SourceDirs_Response
   is
      Response : LSP.Messages.Server_Responses.ALS_SourceDirs_Response
        (Is_Error => False);
      Unit_Desc : LSP.Messages.ALS_Source_Dir_Description;
      Source_Dirs : constant GNATCOLL.VFS.File_Array :=
        Self.Contexts.All_Source_Directories
          (Include_Externally_Built => True);
   begin
      for Dir of Source_Dirs loop
         Unit_Desc :=
           (name => VSS.Strings.Conversions.To_Virtual_String
              (+Dir.Base_Dir_Name),
            uri  =>
              File_To_URI (Dir.Display_Full_Name));
         Response.result.Append (Unit_Desc);
      end loop;

      Self.Trace.Trace
        ("Response.result.length: " & Response.result.Length'Img);

      return Response;
   end On_ALS_Source_Dirs_Request;

   --------------------------------------
   -- On_ALS_Show_Dependencies_Request --
   --------------------------------------

   overriding function On_ALS_Show_Dependencies_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.ALS_Show_Dependencies_Request)
      return LSP.Messages.Server_Responses.ALS_ShowDependencies_Response
   is
      use LSP.Messages;

      Params   : LSP.Messages.ALS_ShowDependenciesParams renames
        Request.params;
      Response : LSP.Messages.Server_Responses.ALS_ShowDependencies_Response
        (Is_Error => False);
      Document : constant LSP.Ada_Documents.Document_Access :=
        Get_Open_Document (Self, Params.textDocument.uri, Force => False);
      Context  : constant Context_Access :=
        Self.Contexts.Get_Best_Context (Params.textDocument.uri);

   begin
      if Document = null then
         Self.Log_Unexpected_Null_Document
           ("On_ALS_Show_Dependencies_Request");
         return Response;
      end if;

      case Params.kind is
         when LSP.Messages.Show_Imported =>
            Document.Get_Imported_Units
              (Context       => Context.all,
               Project_URI   => Self.From_File (Self.Root),
               Show_Implicit => Params.showImplicit,
               Result        => Response.result);

         when LSP.Messages.Show_Importing => null;
            declare
               Contexts : constant LSP.Ada_Context_Sets.Context_Lists.List :=
                 Self.Contexts_For_URI (Params.textDocument.uri);
            begin
               for Context of Contexts loop
                  Document.Get_Importing_Units
                    (Context       => Context.all,
                     Project_URI   => Self.From_File (Self.Root),
                     Show_Implicit => Params.showImplicit,
                     Result        => Response.result);
               end loop;
            end;
      end case;

      return Response;
   end On_ALS_Show_Dependencies_Request;

   --------------------------
   -- On_ALS_Debug_Request --
   --------------------------

   overriding function On_ALS_Debug_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.ALS_Debug_Request)
      return LSP.Messages.Server_Responses.ALS_Debug_Response is
   begin
      case Request.params.Kind is
         when LSP.Messages.Suspend_Execution =>
            declare
               Limit : constant LSP_Number := Request.params.inputQueueLength;
            begin
               while Self.Server.Input_Queue_Length < Integer (Limit) loop
                  delay 0.1;
               end loop;
            end;
      end case;

      return Response : LSP.Messages.Server_Responses.ALS_Debug_Response
        (Is_Error => False);
   end On_ALS_Debug_Request;

   -------------------------------
   -- On_Signature_Help_Request --
   -------------------------------

   overriding function On_Signature_Help_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Signature_Help_Request)
      return LSP.Messages.Server_Responses.SignatureHelp_Response
   is
      use Libadalang.Analysis;
      use Langkit_Support.Slocs;
      use LSP.Messages;
      use type VSS.Strings.Virtual_String;

      Value   : LSP.Messages.SignatureHelpParams renames
        Request.params;
      Response : LSP.Messages.Server_Responses.SignatureHelp_Response
        (Is_Error => False);

      C : constant Context_Access :=
        Self.Contexts.Get_Best_Context (Value.textDocument.uri);

      Document : constant LSP.Ada_Documents.Document_Access :=
        Get_Open_Document (Self, Value.textDocument.uri);

      Node : Libadalang.Analysis.Ada_Node;
      Sloc : Langkit_Support.Slocs.Source_Location;

      Name_Node        : Libadalang.Analysis.Name;
      Prev_Designators : Laltools.Common.Node_Vectors.Vector;
      Designator       : Libadalang.Analysis.Ada_Node;
      Active_Position  : LSP.Types.LSP_Number;
      Active_Signature : LSP.Types.LSP_Number := 0;

      procedure Add_Signature (Decl_Node : Libadalang.Analysis.Basic_Decl);
      --  Add the signature of Decl_Node if it matches the current context.

      procedure Filter_Signature
        (Signature_Info : SignatureInformation;
         Designator     : Libadalang.Analysis.Ada_Node;
         Position       : LSP.Types.LSP_Number;
         Is_Active      : Boolean);
      --  Filter the signatures returned by the client using the current
      --  current Position and designator.
      --  Set Active_Signature to 0 when Is_Active and filtered out.

      function Is_Signature_Active
        (Parameters      : ParameterInformation_Vector;
         Sig_Label       : VSS.Strings.Virtual_String;
         Position        : LSP.Types.LSP_Number;
         Designator      : Libadalang.Analysis.Ada_Node;
         Active_Position : out LSP.Types.LSP_Number)
         return Boolean;
      --  Return True if Parameters is valid for the current Position and
      --  Designator.
      --  Active_Position will point to the active parameter inside Parameters.

      -------------------
      -- Add_Signature --
      -------------------

      procedure Add_Signature (Decl_Node : Libadalang.Analysis.Basic_Decl)
      is
         Param_Index : constant LSP.Types.LSP_Number :=
           Get_Active_Parameter
             (Node             => Decl_Node,
              Designator       => Designator,
              Prev_Designators => Prev_Designators,
              Position         => Active_Position);
      begin
         if Param_Index = -1 then
            return;
         end if;

         declare
            Signature : LSP.Messages.SignatureInformation :=
              (label          => Get_Hover_Text (Decl_Node),
               documentation  =>
                 (Is_Set => True,
                  Value  =>
                    (Is_String => True,
                     String    =>
                       VSS.Strings.To_Virtual_String
                         (Libadalang.Doc_Utils.Get_Documentation
                              (Decl_Node).Doc.To_String))),
               activeParameter =>
                 (Is_Set => True,
                  Value  => Param_Index),
               others          => <>
              );
         begin
            Get_Parameters (Decl_Node, Signature.parameters);
            Response.result.signatures.Append (Signature);
         end;
      end Add_Signature;

      ----------------------
      -- Filter_Signature --
      ----------------------

      procedure Filter_Signature
        (Signature_Info : SignatureInformation;
         Designator     : Libadalang.Analysis.Ada_Node;
         Position       : LSP.Types.LSP_Number;
         Is_Active      : Boolean)
      is
         Active_Position : LSP.Types.LSP_Number;
      begin
         if Is_Signature_Active
           (Parameters      => Signature_Info.parameters,
            Sig_Label       => Signature_Info.label,
            Position        => Position,
            Designator      => Designator,
            Active_Position => Active_Position)
         then
            declare
               New_Signature : SignatureInformation := Signature_Info;
            begin
               --  Reuse Signature_Info after updating the active_position
               New_Signature.activeParameter := (Is_Set => True,
                                                 Value  => Active_Position);
               Response.result.signatures.Append (New_Signature);
            end;
         elsif Is_Active then
            Active_Signature := 0;
         end if;
      end Filter_Signature;

      -------------------------
      -- Is_Signature_Active --
      -------------------------

      function Is_Signature_Active
        (Parameters      : ParameterInformation_Vector;
         Sig_Label       : VSS.Strings.Virtual_String;
         Position        : LSP.Types.LSP_Number;
         Designator      : Libadalang.Analysis.Ada_Node;
         Active_Position : out LSP.Types.LSP_Number)
         return Boolean is
      begin
         Active_Position := 0;
         if Designator = No_Ada_Node then
            --  Check if Position is valid in Parameters (Note: Position starts
            --  at 0)
            Active_Position := Position;
            return Position < LSP_Number (Parameters.Length);
         else
            declare
               Name : constant VSS.Strings.Virtual_String :=
                 LSP.Lal_Utils.To_Virtual_String (Designator.Text);

            begin
               for Param of Parameters loop
                  declare
                     use type VSS.Unicode.UTF16_Code_Unit_Offset;

                     First   :
                       VSS.Strings.Character_Iterators.Character_Iterator
                         := Sig_Label.At_First_Character;
                     Last    :
                       VSS.Strings.Character_Iterators.Character_Iterator
                         := Sig_Label.At_First_Character;
                     Success : Boolean with Unreferenced;

                  begin
                     --  Convert to lower case?
                     if Param.label.Is_String then
                        if Param.label.String = Name then
                           return True;
                        end if;

                     else
                        while First.First_UTF16_Offset < Param.label.From
                          and then First.Forward
                        loop
                           null;
                        end loop;

                        --  'till' is exclusive offset, thus lookup for it
                        --  location and move backward to point to last
                        --  character of the slice

                        while Last.First_UTF16_Offset < Param.label.Till
                          and then Last.Forward
                        loop
                           null;
                        end loop;

                        Success := Last.Backward;

                        if Sig_Label.Slice (First, Last) = Name then
                           return True;
                        end if;
                     end if;

                     Active_Position := Active_Position + 1;
                  end;
               end loop;
            end;
         end if;

         return False;
      end Is_Signature_Active;

   begin
      Response.result := (others => <>);

      Node := C.Get_Node_At (Document, Value, Previous => True);
      Sloc := Document.Get_Source_Location (Value.position);

      --  Check if we are inside a function call and get the caller name
      Get_Call_Expr_Name
        (Node             => Node,
         Cursor           => Sloc,
         Active_Position  => Active_Position,
         Designator       => Designator,
         Prev_Designators => Prev_Designators,
         Name_Node        => Name_Node);

      if Name_Node = Libadalang.Analysis.No_Name then
         --  Try again with the current position
         Node := C.Get_Node_At (Document, Value, Previous => False);
         Get_Call_Expr_Name
           (Node             => Node,
            Cursor           => Sloc,
            Active_Position  => Active_Position,
            Designator       => Designator,
            Prev_Designators => Prev_Designators,
            Name_Node        => Name_Node);

         if Name_Node = Libadalang.Analysis.No_Name then
            return Response;
         end if;
      end if;

      --  Is this a type cast?
      if Name_Node.P_Name_Designated_Type /= No_Ada_Node
        --  Does the cast make sense?
        and then Active_Position = 0
        --  Do we have the previous signatures?
        and then Value.context.Is_Set
        and then Value.context.Value.activeSignatureHelp.Is_Set
      then
         --  At this point, the user is writing a typecast in a previous
         --  signature => keep showing the previous signatures.
         Response.result := Value.context.Value.activeSignatureHelp.Value;
         return Response;
      end if;

      if Value.context.Is_Set
        and then Value.context.Value.isRetrigger
        and then Value.context.Value.activeSignatureHelp.Is_Set
        and then
          (Value.context.Value.triggerKind /= TriggerCharacter
           or else
           --  Adding a ',' will not add new results only filter the previous
             (Value.context.Value.triggerCharacter.Is_Set
              and then Value.context.Value.triggerCharacter.Value = ","))
      then
         --  At this point, we are filtering the previous signatures:
         --  * Don't recompute the list of signature
         --  * Keep the previous activeSignature if not filtered out

         declare
            Prev_Res : SignatureHelp renames
              Value.context.Value.activeSignatureHelp.Value;
            --  Use index to find the activeSignature
            Index    : LSP.Types.LSP_Number := 0;
         begin
            if Prev_Res.activeSignature.Is_Set then
               Active_Signature := Prev_Res.activeSignature.Value;
            else
               Active_Signature := 0;
            end if;

            for Signature_Info of Prev_Res.signatures loop
               Filter_Signature
                 (Signature_Info => Signature_Info,
                  Designator     => Designator,
                  Position       => Active_Position,
                  Is_Active      => Index = Active_Signature);
               Index := Index + 1;
            end loop;
         end;
      else

         for N of C.Find_All_Env_Elements (Name_Node) loop
            if N.Kind in Ada_Subp_Decl_Range
              | Ada_Null_Subp_Decl_Range
                | Ada_Expr_Function_Range
            then
               Add_Signature (N.As_Basic_Decl);
            end if;
         end loop;
      end if;

      --  Set the active values to default
      Response.result.activeSignature := (Is_Set => True,
                                          Value  => Active_Signature);
      --  activeParameter will be ignored because it is properly set in
      --  the signatures.
      Response.result.activeParameter := (Is_Set => True,
                                          Value  => 0);

      return Response;
   end On_Signature_Help_Request;

   -----------------------------------
   -- On_Color_Presentation_Request --
   -----------------------------------

   overriding function On_Color_Presentation_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Color_Presentation_Request)
      return LSP.Messages.Server_Responses.ColorPresentation_Response
   is
      pragma Unreferenced (Self, Request);
      Response : LSP.Messages.Server_Responses.ColorPresentation_Response
        (Is_Error => True);
   begin
      Response.error :=
        (True,
         (code => LSP.Errors.InternalError,
          message => "Not implemented",
          data => <>));
      return Response;
   end On_Color_Presentation_Request;

   -------------------------------
   -- On_Document_Color_Request --
   -------------------------------

   overriding function On_Document_Color_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Document_Color_Request)
      return LSP.Messages.Server_Responses.DocumentColor_Response
   is
      pragma Unreferenced (Self, Request);
      Response : LSP.Messages.Server_Responses.DocumentColor_Response
        (Is_Error => True);
   begin
      Response.error :=
        (True,
         (code => LSP.Errors.InternalError,
          message => "Not implemented",
          data => <>));
      return Response;
   end On_Document_Color_Request;

   -------------------------------
   -- On_Document_Links_Request --
   -------------------------------

   overriding function On_Document_Links_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Document_Links_Request)
      return LSP.Messages.Server_Responses.Links_Response
   is
      pragma Unreferenced (Self, Request);
      Response : LSP.Messages.Server_Responses.Links_Response
        (Is_Error => True);
   begin
      Response.error :=
        (True,
         (code => LSP.Errors.InternalError,
          message => "Not implemented",
          data => <>));
      return Response;
   end On_Document_Links_Request;

   -------------------------------------
   -- On_Document_Tokens_Full_Request --
   -------------------------------------

   overriding function On_Document_Tokens_Full_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Document_Tokens_Full_Request)
      return LSP.Messages.Server_Responses.SemanticTokens_Response
   is
      Value    : LSP.Messages.SemanticTokensParams renames Request.params;
      Document : constant LSP.Ada_Documents.Document_Access :=
        Get_Open_Document (Self, Value.textDocument.uri, Force => False);

      Context  : constant Context_Access :=
        Self.Contexts.Get_Best_Context (Value.textDocument.uri);

      Response : LSP.Messages.Server_Responses.SemanticTokens_Response
        (Is_Error => False);

      Result   : LSP.Messages.uinteger_Vector;
   begin
      if Document = null then
         Self.Log_Unexpected_Null_Document
           ("On_Document_Tokens_Full_Request");
         return Response;
      end if;

      Result := Document.Get_Tokens (Context.all, Self.Highlighter);
      Response.result.data.Move (Result);

      return Response;
   end On_Document_Tokens_Full_Request;

   --------------------------------------
   -- On_Document_Tokens_Range_Request --
   --------------------------------------

   overriding function On_Document_Tokens_Range_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Document_Tokens_Range_Request)
      return LSP.Messages.Server_Responses.SemanticTokens_Response
   is
      Value    : LSP.Messages.SemanticTokensRangeParams renames Request.params;
      Document : constant LSP.Ada_Documents.Document_Access :=
        Get_Open_Document (Self, Value.textDocument.uri, Force => False);

      Context  : constant Context_Access :=
        Self.Contexts.Get_Best_Context (Value.textDocument.uri);

      Response : LSP.Messages.Server_Responses.SemanticTokens_Response
        (Is_Error => False);

      Result   : LSP.Messages.uinteger_Vector;
   begin
      if Document = null then
         Self.Log_Unexpected_Null_Document
           ("On_Document_Tokens_Range_Request");
         return Response;
      end if;

      Result := Document.Get_Tokens
        (Context.all, Self.Highlighter, Value.span);
      Response.result.data.Move (Result);

      return Response;
   end On_Document_Tokens_Range_Request;

   ---------------------------------
   -- On_Document_Symbols_Request --
   ---------------------------------

   overriding function On_Document_Symbols_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Document_Symbols_Request)
      return LSP.Messages.Server_Responses.Symbol_Response
   is
      --  The list of symbols for one document shouldn't depend
      --  on the project: we can just choose the best context for this.
      Value    : LSP.Messages.DocumentSymbolParams renames Request.params;
      Document : constant LSP.Ada_Documents.Document_Access :=
        Get_Open_Document (Self, Value.textDocument.uri, Force => False);
      Context  : constant Context_Access :=
        Self.Contexts.Get_Best_Context (Value.textDocument.uri);
      Result   : LSP.Messages.Server_Responses.Symbol_Response :=
        (Is_Error => False,
         result   => <>,
         error    => (Is_Set => False),
         others   => <>);

      Pattern : constant Search_Pattern'Class := Build
        (Pattern        => Value.query,
         Case_Sensitive => Value.case_sensitive = LSP.Types.True,
         Whole_Word     => Value.whole_word = LSP.Types.True,
         Negate         => Value.negate = LSP.Types.True,
         Kind           =>
           (if Value.kind.Is_Set
            then Value.kind.Value
            else LSP.Messages.Start_Word_Text));

      package Canceled is new LSP.Generic_Cancel_Check (Request'Access, 127);

   begin
      if Document = null then
         declare
            Document : LSP.Ada_Documents.Document_Access :=
              Get_Open_Document (Self, Value.textDocument.uri, Force => True);
         begin
            Self.Get_Symbols
              (Document.all, Context.all, Pattern,
               Canceled.Has_Been_Canceled'Access, Result.result);

            Free (Internal_Document_Access (Document));
         end;
      else
         Self.Get_Symbols
           (Document.all, Context.all, Pattern,
            Canceled.Has_Been_Canceled'Access, Result.result);
      end if;
      return Result;
   end On_Document_Symbols_Request;

   -----------------------
   -- On_Rename_Request --
   -----------------------

   overriding function On_Rename_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Rename_Request)
      return LSP.Messages.Server_Responses.Rename_Response
   is
      use Libadalang.Analysis;

      Value     : LSP.Messages.RenameParams renames Request.params;
      Position  : constant LSP.Messages.TextDocumentPositionParams :=
        (Value.textDocument, Value.position);
      Response  : LSP.Messages.Server_Responses.Rename_Response
        (Is_Error => False);
      --  If a rename problem is found when Process_Context is called,
      --  then Edits.Diagnotics will not be empty. This Response will be
      --  discarded and a new response with an error meessage is returned
      --  instead.
      --  If no problems are found, then this Response will contain all the
      --  references to be renamed and is returned by this function.
      Document  : constant LSP.Ada_Documents.Document_Access :=
        Get_Open_Document (Self, Value.textDocument.uri);

      Safe_Renamer : Laltools.Refactor.Safe_Rename.Safe_Renamer;
      Algorithm    : constant Laltools.Refactor.Safe_Rename.
        Problem_Finder_Algorithm_Kind :=
          Laltools.Refactor.Safe_Rename.Analyse_AST;

      Context_Edits : Laltools.Refactor.Refactoring_Edits;
      --  Edits found for a particular context
      All_Edits     : Laltools.Refactor.Refactoring_Edits;
      --  When iterating over all contexts (and therefore all projects), it's
      --  possible to encounter the same Text_Edit more than once, so this
      --  stores all the unique edits

      procedure Process_Context (C : Context_Access);
      --  Process the rename request for the given context, and add the
      --  edits to `All_Edits`.

      ---------------------
      -- Process_Context --
      ---------------------

      procedure Process_Context (C : Context_Access) is
         use GNATCOLL.Projects;
         use Laltools.Refactor.Safe_Rename;

         Node       : constant Ada_Node := C.Get_Node_At (Document, Position);
         Name_Node  : constant Libadalang.Analysis.Name :=
           Laltools.Common.Get_Node_As_Name (Node);
         Definition : constant Defining_Name :=
           Laltools.Common.Resolve_Name_Precisely (Name_Node);

         function Attribute_Value_Provider_Callback
           (Attribute : GNATCOLL.Projects.Attribute_Pkg_String;
            Index : String := "";
            Default : String := "";
            Use_Extended : Boolean := False)
            return String
          is (C.Project_Attribute_Value
                (Attribute, Index, Default, Use_Extended));

         Attribute_Value_Provider : constant Attribute_Value_Provider_Access :=
           Attribute_Value_Provider_Callback'Unrestricted_Access;

         function Analysis_Units return Analysis_Unit_Array is
           (C.Analysis_Units);
         --  Callback needed to provide the analysis units to the safe rename
         --  tool.

         procedure Process_Comments (Node : Ada_Node);
         --  Iterate over all comments and include them in the response when
         --  they contain a renamed word.

         procedure Process_File_Renames;
         --  Merges Context_Edits.File_Renames into All_Edits.File_Renames

         procedure Process_References;
         --  Merges Context_Edits.Text_Edits into All_Edits.Text_Edits and for
         --  each Text_Edit (which represents a reference) processes its
         --  references in comments.

         -----------------------
         --  Process_Comments --
         -----------------------

         procedure Process_Comments (Node : Ada_Node)
         is
            use Laltools.Refactor;

            File_Name : constant File_Name_Type :=
              Node.Unit.Get_Filename;
            Token     : Token_Reference := First_Token (Node.Unit);
            Name      : constant Wide_Wide_String :=
              Ada.Strings.Wide_Wide_Unbounded.To_Wide_Wide_String
                (Laltools.Common.Get_Last_Name (Name_Node));
            Text_Edit : Laltools.Refactor.Text_Edit;
            Span      : Langkit_Support.Slocs.Source_Location_Range;
            Current   : Token_Reference;
            Diff      : Integer;

            function Process_Box return Boolean;
            --  Check whether Current is box header/footer and modify it.
            --  Return False when the searching cycle should be stopped.

            -----------------
            -- Process_Box --
            -----------------

            function Process_Box return Boolean is
               use Langkit_Support.Text;
               use Langkit_Support.Slocs;

            begin
               if Current = No_Token then
                  return False;
               end if;

               case Kind (Data (Current)) is
                  when Ada_Whitespace =>
                     return True;

                  when Ada_Comment =>
                     declare
                        Value : constant Text_Type := Text (Current);
                     begin
                        for Idx in Value'Range loop
                           if Value (Idx) /= '-' then
                              return False;
                           end if;
                        end loop;

                        if Diff > 0 then
                           --  Increase '-', Diff is positive
                           declare
                              Sloc : Source_Location_Range :=
                                Sloc_Range (Data (Current));

                           begin
                              Sloc.Start_Column := Sloc.End_Column;

                              Laltools.Refactor.Safe_Insert
                                (All_Edits.Text_Edits,
                                 File_Name,
                                 Laltools.Refactor.Text_Edit'
                                   (Sloc,
                                    Ada.Strings.Unbounded."*" (Diff, '-')));
                           end;

                        else
                           --  Decrease '-', Diff is negative
                           declare
                              Sloc : Source_Location_Range :=
                                Sloc_Range (Data (Current));

                           begin
                              Sloc.Start_Column :=
                                Sloc.End_Column - Column_Number (abs Diff);

                              Laltools.Refactor.Safe_Insert
                                (All_Edits.Text_Edits,
                                 File_Name,
                                 Laltools.Refactor.Text_Edit'
                                   (Sloc, Null_Unbounded_String));
                           end;
                        end if;

                        return False;
                     end;

                  when others =>
                     return False;
               end case;
            end Process_Box;

         begin
            Diff := Natural (Value.newName.Character_Length) - Name'Length;

            while Token /= No_Token loop
               declare
                  This_Span : Langkit_Support.Slocs.Source_Location_Range;
               begin
                  if Kind (Data (Token)) = Ada_Comment
                    and then Laltools.Common.Contains
                      (Token, Name, True, This_Span)
                  then
                     Text_Edit.Location := This_Span;
                     Text_Edit.Text :=
                       VSS.Strings.Conversions.To_Unbounded_UTF_8_String
                         (Value.newName);

                     if Diff /= 0
                       and then Laltools.Common.Contains
                         (Token, "-- " & Name & " --", False, Span)
                     then
                        --  Can be a comment box
                        Current := Previous (Token);
                        loop
                           --  Looking for the box header
                           exit when not Process_Box;
                           Current := Previous (Current);
                        end loop;

                        --  Include corrected comment itself
                        Laltools.Refactor.Safe_Insert
                          (All_Edits.Text_Edits,
                           Node.Unit.Get_Filename,
                           Text_Edit);

                        Current := Next (Token);
                        loop
                           --  Looking for the box footer
                           exit when not Process_Box;
                           Current := Next (Current);
                        end loop;
                     else
                        Laltools.Refactor.Safe_Insert
                          (All_Edits.Text_Edits,
                           Node.Unit.Get_Filename,
                           Text_Edit);
                     end if;
                  end if;
               end;

               Token := Next (Token);
            end loop;
         end Process_Comments;

         --------------------------
         -- Process_File_Renames --
         --------------------------

         procedure Process_File_Renames is
         begin
            All_Edits.File_Renames.Union (Context_Edits.File_Renames);
         end Process_File_Renames;

         ------------------------
         -- Process_References --
         ------------------------

         procedure Process_References
         is
            use Laltools.Refactor;

            Text_Edits_Cursor : Text_Edit_Ordered_Maps.Cursor :=
              Context_Edits.Text_Edits.First;

            Unit : Analysis_Unit; -- Reference Unit
            Node : Ada_Node; -- Reference Node

         begin
            Text_Edits_Cursor := Context_Edits.Text_Edits.First;

            while Text_Edit_Ordered_Maps.Has_Element (Text_Edits_Cursor) loop
               for Text_Edit of
                 Text_Edit_Ordered_Maps.Element (Text_Edits_Cursor)
               loop
                  --  Check if we've already seen this reference from another
                  --  context.

                  if not Contains
                    (All_Edits.Text_Edits,
                     Text_Edit_Ordered_Maps.Key (Text_Edits_Cursor),
                     Text_Edit)
                  then
                     --  First time we see this reference, so add it All_Edits
                     --  and process comments.

                     Safe_Insert
                       (All_Edits.Text_Edits,
                        Text_Edit_Ordered_Maps.Key (Text_Edits_Cursor),
                        Text_Edit);

                     Unit := C.Get_AU
                       (GNATCOLL.VFS.Create_From_UTF8
                          (String (Text_Edit_Ordered_Maps.Key
                           (Text_Edits_Cursor))));
                     Node := Unit.Root.Lookup
                       ((Text_Edit.Location.Start_Line,
                         Text_Edit.Location.Start_Column));

                     if Self.Options.Refactoring.Renaming.In_Comments then
                        Process_Comments (Node);
                     end if;
                  end if;
               end loop;

               Text_Edits_Cursor :=
                 Text_Edit_Ordered_Maps.Next (Text_Edits_Cursor);
            end loop;
         end Process_References;

      begin
         if Definition.Is_Null then
            return;
         end if;

         Safe_Renamer := Laltools.Refactor.Safe_Rename.Create_Safe_Renamer
           (Definition               => Definition,
            New_Name                 =>
              Libadalang.Text.To_Unbounded_Text
                (VSS.Strings.Conversions.To_Wide_Wide_String (Value.newName)),
            Algorithm                => Algorithm,
            Attribute_Value_Provider => Attribute_Value_Provider);

         Context_Edits := Safe_Renamer.Refactor (Analysis_Units'Access);

         --  If problems were found, do not continue processing references

         if not Context_Edits.Diagnostics.Is_Empty then
            return;
         end if;

         Process_References;
         Process_File_Renames;
      end Process_Context;

   begin
      for C of Self.Contexts_For_URI (Value.textDocument.uri) loop
         Process_Context (C);

         --  If problems were found, send an error message and do not proceed
         --  with the renames.

         if not Context_Edits.Diagnostics.Is_Empty then
            return Response : LSP.Messages.Server_Responses.Rename_Response
              (Is_Error => True)
            do
               declare
                  Error_Message : VSS.String_Vectors.Virtual_String_Vector;

               begin
                  for Problem of Context_Edits.Diagnostics loop
                     Error_Message.Append
                       (VSS.Strings.Conversions.To_Virtual_String
                          (Problem.Info));
                  end loop;

                  Response.error :=
                    (True,
                     (code    => LSP.Errors.InvalidRequest,
                      message => Error_Message.Join_Lines (VSS.Strings.LF),
                      data    => Empty));
               end;
            end return;
         end if;

         exit when Request.Canceled;
      end loop;

      --  All contexts were processed, and no rename problems were found

      Response.result := To_Workspace_Edit
        (All_Edits, Self.Resource_Operations, Self.Versioned_Documents, Self);

      return Response;

   exception
      when E : others =>
         return Response : LSP.Messages.Server_Responses.Rename_Response
           (Is_Error => True)
         do
            Response.error :=
              (True,
               (code    => LSP.Errors.InternalError,
                message => VSS.Strings.Conversions.To_Virtual_String
                  (Ada.Exceptions.Exception_Information (E)),
                data    => <>));
         end return;
   end On_Rename_Request;

   --------------------------------------------
   -- On_DidChangeConfiguration_Notification --
   --------------------------------------------

   overriding procedure On_DidChangeConfiguration_Notification
     (Self  : access Message_Handler;
      Value : LSP.Messages.DidChangeConfigurationParams)
   is
      use type GNATCOLL.JSON.JSON_Value_Type;

      relocateBuildTree                 : constant String :=
        "relocateBuildTree";
      rootDir                           : constant String :=
        "rootDir";
      projectFile                       : constant String :=
        "projectFile";
      scenarioVariables                 : constant String :=
        "scenarioVariables";
      defaultCharset                    : constant String :=
        "defaultCharset";
      enableDiagnostics                 : constant String :=
        "enableDiagnostics";
      enableIndexing                    : constant String :=
        "enableIndexing";
      renameInComments                  : constant String :=
        "renameInComments";
      namedNotationThreshold            : constant String :=
        "namedNotationThreshold";
      foldComments                      : constant String :=
        "foldComments";
      displayMethodAncestryOnNavigation : constant String :=
        "displayMethodAncestryOnNavigation";
      followSymlinks                    : constant String :=
        "followSymlinks";
      documentationStyle                : constant String :=
        "documentationStyle";
      useCompletionSnippets             : constant String :=
        "useCompletionSnippets";
      logThreshold                      : constant String :=
        "logThreshold";

      Ada       : constant LSP.Types.LSP_Any := Value.settings.Get ("ada");
      File      : VSS.Strings.Virtual_String;
      Charset   : Unbounded_String;
      Variables : LSP.Types.LSP_Any;
      Relocate  : Virtual_File := No_File;
      Root      : Virtual_File := No_File;

      --  Is client capable of dynamically registering file operations?
      Dynamically_Register_File_Operations : constant Boolean :=
        Self.Client.capabilities.workspace.fileOperations.Is_Set
        and then Self.Client.capabilities.workspace.fileOperations.
                   Value.dynamicRegistration.Is_Set = True;

   begin
      if Ada.Kind = GNATCOLL.JSON.JSON_Object_Type then
         if Ada.Has_Field (relocateBuildTree) then
            Relocate := Create_From_UTF8 (Get (Get (Ada, relocateBuildTree)));
         end if;

         if Ada.Has_Field (rootDir) then
            Root := Create_From_UTF8 (Get (Get (Ada, rootDir)));
         end if;

         if Ada.Has_Field (projectFile) then
            File :=
              VSS.Strings.Conversions.To_Virtual_String
                (String'(Get (Get (Ada, projectFile))));

            --  Drop uri scheme if present
            if File.Starts_With ("file:") then
               File := Self.URI_To_File (File);
            end if;
         end if;

         if Ada.Has_Field (scenarioVariables) and then
           Ada.Get (scenarioVariables).Kind  = GNATCOLL.JSON.JSON_Object_Type
         then
            Variables := Ada.Get (scenarioVariables);
         end if;

         if Ada.Has_Field (defaultCharset) then
            Charset := Ada.Get (defaultCharset);
         end if;

         --  It looks like the protocol does not allow clients to say whether
         --  or not they want diagnostics as part of
         --  InitializeParams.capabilities.textDocument. So we support
         --  deactivating of diagnostics via a setting here.
         if Ada.Has_Field (enableDiagnostics) then
            Self.Diagnostics_Enabled := Ada.Get (enableDiagnostics);
         end if;

         --  Similarly to diagnostics, we support selectively activating
         --  indexing in the parameters to this request.
         if Ada.Has_Field (enableIndexing) then
            Self.Indexing_Enabled := Ada.Get (enableIndexing);
         end if;

         --  Retrieve the different textDocument/rename options if specified

         if Ada.Has_Field (renameInComments) then
            Self.Options.Refactoring.Renaming.In_Comments :=
              Ada.Get (renameInComments);
         end if;

         if Ada.Has_Field (foldComments) then
            Self.Options.Folding.Comments := Ada.Get (foldComments);
         end if;

         --  Retrieve the number of parameters / components at which point
         --  named notation is used for subprogram/aggregate completion
         --  snippets.

         if Ada.Has_Field (namedNotationThreshold) then
            Self.Named_Notation_Threshold := Ada.Get (namedNotationThreshold);
         end if;

         if Ada.Has_Field (logThreshold) then
            Self.Log_Threshold := Ada.Get (logThreshold);
         end if;

         --  Check the 'useCompletionSnippets' flag to see if we should use
         --  snippets in completion (if the client supports it).
         if not Self.Completion_Snippets_Enabled then
            Self.Use_Completion_Snippets := False;
         elsif Ada.Has_Field (useCompletionSnippets) then
            Self.Use_Completion_Snippets := Ada.Get (useCompletionSnippets);
         end if;

         --  Retrieve the policy for displaying type hierarchy on navigation
         --  requests.
         if Ada.Has_Field (displayMethodAncestryOnNavigation) then
            Self.Display_Method_Ancestry_Policy :=
              LSP.Messages.AlsDisplayMethodAncestryOnNavigationPolicy'Value
                (Ada.Get (displayMethodAncestryOnNavigation));
         end if;

         --  Retrieve the follow symlinks policy.

         if Ada.Has_Field (followSymlinks) then
            Self.Follow_Symlinks := Ada.Get (followSymlinks);
         end if;

         if Ada.Has_Field (documentationStyle) then
            begin
               Self.Options.Documentation.Style :=
                 GNATdoc.Comments.Options.Documentation_Style'Value
                   (Ada.Get (documentationStyle));

            exception
               when Constraint_Error =>
                  Self.Options.Documentation.Style :=
                    GNATdoc.Comments.Options.GNAT;
            end;
         end if;
      end if;

      if not File.Is_Empty then
         --  The projectFile may be either an absolute path or a
         --  relative path; if so, we're assuming it's relative
         --  to Self.Root.
         declare
            Project_File : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String (File);
            GPR          : Virtual_File;

         begin
            if Is_Absolute_Path (Project_File) then
               GPR := Create_From_UTF8 (Project_File);
            else
               GPR := Join (Self.Root, Create_From_UTF8 (Project_File));
            end if;

            Self.Load_Project
              (GPR,
               Variables,
               To_String (Charset),
               Valid_Project_Configured,
               Relocate,
               Root);
         end;
      end if;

      Self.Ensure_Project_Loaded;

      --  Register rangeFormatting provider is the client supports
      --  dynamic registration for it (and we haven't done it before).
      if not Self.Range_Formatting_Enabled
        and then Self.Client.capabilities.textDocument.rangeFormatting.Is_Set
        and then Self.Client.capabilities.textDocument.rangeFormatting.Value
          .dynamicRegistration = True
      then
         declare
            Request : LSP.Messages.Client_Requests.RegisterCapability_Request;
            Registration : LSP.Messages.Registration;
            Selector     : LSP.Messages.DocumentSelector;
            Filter       : constant LSP.Messages.DocumentFilter :=
              (language => (True, "ada"),
               others   => <>);
         begin
            Selector.Append (Filter);
            Registration.method := "textDocument/rangeFormatting";
            Registration.registerOptions :=
              (LSP.Types.Text_Document_Registration_Option,
               (documentSelector => Selector));
            Registration.id := "rf";
            Request.params.registrations.Append (Registration);
            Self.Server.On_RegisterCapability_Request (Request);
            Self.Range_Formatting_Enabled := True;
         end;
      end if;

      --  Dynamically register file operations if supported by the client
      if Dynamically_Register_File_Operations
        and then not Self.Contexts.Each_Context.Is_Empty
      then
         declare
            Request : LSP.Messages.Client_Requests.RegisterCapability_Request;
            Registration : LSP.Messages.Registration;
            Registration_Options :
              constant LSP.Messages.FileOperationRegistrationOptions :=
                Self.Compute_File_Operation_Registration_Options;
            File_Operations_Client_Capabilities :
              constant LSP.Messages.FileOperationsClientCapabilities :=
                Self.Client.capabilities.workspace.fileOperations.Value;

         begin
            Registration.registerOptions :=
              (LSP.Types.File_Operation_Registration_Option,
               FileOperation => Registration_Options);

            if File_Operations_Client_Capabilities.willCreate = True then
               Registration.id := "Will_Create";
               Registration.method := "workspace/willCreateFiles";
               Request.params.registrations.Append (Registration);
            end if;

            if File_Operations_Client_Capabilities.didCreate = True then
               Registration.id := "Did_Create";
               Registration.method := "workspace/didCreateFiles";
               Request.params.registrations.Append (Registration);
            end if;

            if File_Operations_Client_Capabilities.willRename = True then
               Registration.id := "Will_Rename";
               Registration.method := "workspace/willRenameFiles";
               Request.params.registrations.Append (Registration);
            end if;

            if File_Operations_Client_Capabilities.didRename = True then
               Registration.id := "Did_Rename";
               Registration.method := "workspace/didRenameFiles";
               Request.params.registrations.Append (Registration);
            end if;

            if File_Operations_Client_Capabilities.willDelete = True then
               Registration.id := "Will_Delete";
               Registration.method := "workspace/willDeleteFiles";
               Request.params.registrations.Append (Registration);
            end if;

            if File_Operations_Client_Capabilities.didDelete = True then
               Registration.id := "Did_Delete";
               Registration.method := "workspace/didDeleteFiles";
               Request.params.registrations.Append (Registration);
            end if;

            Self.Server.On_RegisterCapability_Request (Request);
         end;
      end if;
   end On_DidChangeConfiguration_Notification;

   -------------------------------------------
   -- On_DidChangeWatchedFiles_Notification --
   -------------------------------------------

   overriding procedure On_DidChangeWatchedFiles_Notification
     (Self  : access Message_Handler;
      Value : LSP.Messages.DidChangeWatchedFilesParams)
   is
      URI  : LSP.Messages.DocumentUri;
      File : GNATCOLL.VFS.Virtual_File;

      procedure Process_Created_File;
      --  Processes a created file

      procedure Process_Deleted_File;
      --  Processes a deleted file

      procedure Process_Changed_File;
      --  Processes a changed file

      --------------------------
      -- Process_Created_File --
      --------------------------

      procedure Process_Created_File
      is
         use VSS.Strings.Conversions;

         Contexts : constant LSP.Ada_Context_Sets.Context_Lists.List :=
           Self.Contexts_For_File (File);

         function Has_Dir
           (Context : LSP.Ada_Contexts.Context)
            return Boolean
         is (Context.List_Source_Directories.Contains (File.Dir));
         --  Return True if File is in a source directory of the project held
         --  by Context.

      begin
         --  If the file was created by the client, then the DidCreateFiles
         --  notification might have been received from it. In that case,
         --  Contexts wont be empty, and all we need to do is check if
         --  there's an open document. If there is, it takes precedence over
         --  the filesystem.
         --  If Contexts is empty, then we need to check if is a new source
         --  that needs to be added. For instance, a source that was moved
         --  to the the project source directories.

         if Contexts.Is_Empty then
            for Context of Self.Contexts.Each_Context
              (Has_Dir'Unrestricted_Access)
            loop
               Context.Include_File (File);
               Context.Index_File (File);

               Self.Trace.Trace
                 ("Included " & File.Display_Base_Name
                  & " in context " & To_UTF_8_String (Context.Id));
            end loop;

         else
            if Self.Get_Open_Document (URI) = null then
               for Context of Contexts loop
                  Context.Index_File (File);
               end loop;
            end if;
         end if;
      end Process_Created_File;

      ---------------------------
      -- Process_Deleted_Files --
      ---------------------------

      procedure Process_Deleted_File is
      begin
         if Self.Get_Open_Document (URI) = null then
            --  If there is no document, remove from the sources list
            --  and reindex the file for each context where it is
            --  relevant.
            File := Self.To_File (URI);

            for C of Self.Contexts_For_File (File) loop
               C.Exclude_File (File);
               C.Index_File (File);
            end loop;
         end if;
      end Process_Deleted_File;

      --------------------------
      -- Process_Changed_File --
      --------------------------

      procedure Process_Changed_File is
      begin
         if Self.Get_Open_Document (URI) = null then
            --  If there is no document, reindex the file for each
            --  context where it is relevant.
            File := Self.To_File (URI);

            for C of Self.Contexts_For_File (File) loop
               C.Index_File (File);
            end loop;
         end if;
      end Process_Changed_File;

   begin
      --  Look through each change, filtering non Ada source files
      for Change of Value.changes loop
         URI := Change.uri;
         File := Self.To_File (URI);
         if Self.Is_Ada_Source (File) then
            case Change.a_type is
               when LSP.Messages.Created =>
                  Process_Created_File;
               when LSP.Messages.Deleted =>
                  Process_Deleted_File;
               when LSP.Messages.Changed =>
                  Process_Changed_File;
            end case;
         end if;
      end loop;
   end On_DidChangeWatchedFiles_Notification;

   ------------------------------------
   -- Mark_Source_Files_For_Indexing --
   ------------------------------------

   procedure Mark_Source_Files_For_Indexing (Self : access Message_Handler) is
   begin
      Self.Files_To_Index.Clear;

      --  Mark all the project's source files
      for C of Self.Contexts.Each_Context loop
         for F in C.List_Files loop
            Self.Files_To_Index.Include
              (LSP.Ada_File_Sets.File_Sets.Element (F));
         end loop;
      end loop;

      if Runtime_Indexing.Is_Active then
         --  Mark all the predefined sources too (runtime)
         for F in Self.Project_Predefined_Sources.Iterate loop
            declare
               File : GNATCOLL.VFS.Virtual_File renames
                 LSP.Ada_File_Sets.File_Sets.Element (F);
            begin
               for Context of Self.Contexts_For_File (File) loop
                  Self.Files_To_Index.Include (File);
               end loop;
            end;
         end loop;
      end if;

      Self.Total_Files_Indexed := 0;
      Self.Total_Files_To_Index := Positive'Max
        (1, Natural (Self.Files_To_Index.Length));
   end Mark_Source_Files_For_Indexing;

   ------------------
   -- Load_Project --
   ------------------

   procedure Load_Project
     (Self                : access Message_Handler;
      GPR                 : Virtual_File;
      Scenario            : LSP.Types.LSP_Any;
      Charset             : String;
      Status              : Load_Project_Status;
      Relocate_Build_Tree : Virtual_File := No_File;
      Root_Dir            : Virtual_File := No_File)
   is
      use GNATCOLL.Projects;
      Errors     : LSP.Messages.ShowMessageParams;
      Error_Text : VSS.String_Vectors.Virtual_String_Vector;

      procedure Create_Context_For_Non_Aggregate (P : Project_Type);
      procedure Add_Variable (Name : String; Value : GNATCOLL.JSON.JSON_Value);
      procedure On_Error (Text : String);

      ------------------
      -- Add_Variable --
      ------------------

      procedure Add_Variable
        (Name : String; Value : GNATCOLL.JSON.JSON_Value)
      is
         use type GNATCOLL.JSON.JSON_Value_Type;
      begin
         if Value.Kind = GNATCOLL.JSON.JSON_String_Type then
            Self.Project_Environment.Change_Environment (Name, Value.Get);
         end if;
      end Add_Variable;

      --------------
      -- On_Error --
      --------------

      procedure On_Error (Text : String) is
      begin
         Error_Text.Append (VSS.Strings.Conversions.To_Virtual_String (Text));
      end On_Error;

      --------------------------------------
      -- Create_Context_For_Non_Aggregate --
      --------------------------------------

      procedure Create_Context_For_Non_Aggregate (P : Project_Type) is
         C : constant Context_Access := new Context (Self.Trace);
         Reader : LSP.Ada_Handlers.File_Readers.LSP_Reader_Interface (Self);

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
           (Tree           => Self.Project_Tree.all,
            Project        => P,
            Default_Config => Default_Config,
            File_Configs   => File_Configs);

         Libadalang.Preprocessing.Iterate
           (Default_Config => Default_Config,
            File_Configs   => File_Configs,
            Process        => Set_Line_Mode'Access);

         Reader.Preprocessing_Data :=
           Libadalang.Preprocessing.Create_Preprocessor_Data
             (Default_Config, File_Configs);

         C.Initialize (Reader, Self.Follow_Symlinks);
         C.Load_Project (Tree    => Self.Project_Tree,
                         Root    => P,
                         Charset => Charset);
         Self.Contexts.Prepend (C);
      end Create_Context_For_Non_Aggregate;

   begin
      --  Unload the project tree and the project environment
      Self.Release_Contexts_And_Project_Info;

      --  We're loading an actual project
      Self.Project_Status := Status;

      --  Now load the new project
      Errors.a_type := LSP.Messages.Warning;
      Self.Project_Environment :=
        new LSP.Ada_Project_Environments.LSP_Project_Environment;
      Initialize (Self.Project_Environment);
      Self.Project_Environment.Set_Trusted_Mode (not Self.Follow_Symlinks);

      if Relocate_Build_Tree /= No_File then
         Self.Project_Environment.Set_Build_Tree_Dir
           (Relocate_Build_Tree.Full_Name);
      end if;
      if Root_Dir /= No_File then
         Self.Project_Environment.Set_Root_Dir
           (Root_Dir.Full_Name);
      end if;
      if not Scenario.Is_Empty then
         Scenario.Map_JSON_Object (Add_Variable'Access);
      end if;

      begin
         Self.Project_Tree := new Project_Tree;
         Self.Project_Tree.Load
           (GPR,
            Self.Project_Environment,
            Report_Missing_Dirs => False,
            Errors              => On_Error'Unrestricted_Access);
         for File of Self.Project_Environment.Predefined_Source_Files loop
            if Self.Is_Ada_Source (File) then
               Self.Project_Predefined_Sources.Include (File);
            end if;
         end loop;
         if Self.Project_Tree.Root_Project.Is_Aggregate_Project then
            declare
               Aggregated : Project_Array_Access :=
                 Self.Project_Tree.Root_Project.Aggregated_Projects;
            begin
               for X of Aggregated.all loop
                  Create_Context_For_Non_Aggregate (X);
               end loop;
               Unchecked_Free (Aggregated);
            end;
         else
            Create_Context_For_Non_Aggregate (Self.Project_Tree.Root_Project);
         end if;
      exception
         when E : Invalid_Project =>
            Self.Release_Contexts_And_Project_Info;

            Self.Trace.Trace (E);
            Errors.a_type := LSP.Messages.Error;

            Errors.message.Append
              (VSS.Strings.Conversions.To_Virtual_String
                 ("Unable to load project file: " &
                    String (GPR.Full_Name.all) & Ada.Characters.Latin_1.LF));

            --  The project was invalid: fallback on loading the implicit
            --  project.
            Self.Load_Implicit_Project (Invalid_Project_Configured);
      end;

      --  Report the errors, if any
      if not Error_Text.Is_Empty then
         for Line of Error_Text loop
            Errors.message.Append (Line);
         end loop;
         Self.Server.On_Show_Message (Errors);
      end if;

      --  Reindex all open documents immediately after project reload, so
      --  that navigation from editors is accurate.
      for Document of Self.Open_Documents loop
         for Context of Self.Contexts_For_URI (Document.URI) loop
            Context.Index_Document (Document.all);
         end loop;

         Self.Publish_Diagnostics (Document_Access (Document));
      end loop;

      if not Self.File_Monitor.Assigned then
         Self.File_Monitor :=
           new LSP.Servers.FS_Watch.FS_Watch_Monitor (Self.Server);
      end if;

      --  We have successfully loaded a real project: monitor the filesystem
      --  for any changes on the sources of the project
      Self.File_Monitor.Monitor_Directories
        (Self.Contexts.All_Source_Directories);

      --  Reindex the files from disk in the background after a project reload
      Self.Mark_Source_Files_For_Indexing;
   end Load_Project;

   -------------------------------
   -- Get_Unique_Progress_Token --
   -------------------------------

   function Get_Unique_Progress_Token
     (Self      : access Message_Handler;
      Operation : String := "") return LSP_Number_Or_String
   is

      Pid : constant String :=
        GNATCOLL.Utils.Image (Pid_To_Integer (Current_Process_Id), 1);
   begin
      Self.Token_Id := Self.Token_Id + 1;
      --  Generate an identifier that has little risk of collision with
      --  other language servers, or other occurrences of this server.
      --  (There is still a very small risk of collision with PID recyclings,
      --  but the consequences are acceptable.)
      return
        (Is_Number => False,
         String    => VSS.Strings.Conversions.To_Virtual_String
           ("ada_ls-"
            & Pid & "-" & Operation & "-"
            & GNATCOLL.Utils.Image (Self.Token_Id, 1)));
   end Get_Unique_Progress_Token;

   -----------------
   -- Index_Files --
   -----------------

   procedure Index_Files (Self : access Message_Handler) is

      procedure Emit_Progress_Begin;
      procedure Emit_Progress_Report (Files_Indexed, Total_Files : Natural);
      procedure Emit_Progress_End;
      --  Emit a message to inform that the indexing has begun / is in
      --  progress / has finished.

      Progress_Report_Sent : Time := Clock;

      -------------------------
      -- Emit_Progress_Begin --
      -------------------------

      procedure Emit_Progress_Begin is
         P : LSP.Messages.Progress_Params (LSP.Messages.Progress_Begin);

         Create_Progress : constant LSP.Messages.Client_Requests
           .WorkDoneProgressCreate_Request :=
             (params => (token => Self.Indexing_Token), others => <>);
      begin
         Self.Server.On_WorkDoneProgress_Create_Request
           (Create_Progress);
         --  FIXME: wait response before sending progress notifications.
         --  Currenctly, we just send a `window/workDoneProgress/create`
         --  request and immediately after this start sending notifications.
         --  We could do better, send request, wait for client response and
         --  start progress-report sending only after response.
         P.Begin_Param.token := Self.Indexing_Token;
         P.Begin_Param.value.title := "Indexing";
         P.Begin_Param.value.percentage := (Is_Set => True, Value => 0);
         Self.Server.On_Progress (P);
      end Emit_Progress_Begin;

      --------------------------
      -- Emit_Progress_Report --
      --------------------------

      procedure Emit_Progress_Report (Files_Indexed, Total_Files : Natural) is
         P : LSP.Messages.Progress_Params (LSP.Messages.Progress_Report);

         function Image (N : Natural) return Wide_Wide_String;
         function Image (N : Natural) return Wide_Wide_String is
            S : constant Wide_Wide_String := Natural'Wide_Wide_Image (N);
         begin
            return S (S'First + 1 .. S'Last);
         end Image;

         Current : constant Time := Clock;
      begin
         if Current - Progress_Report_Sent < 0.5 then
            --  Send only 2 notifications per second
            return;
         end if;
         Progress_Report_Sent := Current;

         P.Report_Param.token := Self.Indexing_Token;
         P.Report_Param.value.percentage :=
           (Is_Set => True, Value => LSP_Number
              ((Files_Indexed * 100) / Total_Files));
         P.Report_Param.value.message :=
           (Is_Set => True,
            Value  => VSS.Strings.To_Virtual_String
              (Image (Files_Indexed) & "/" & Image (Total_Files) & " files"));
         Self.Server.On_Progress (P);
      end Emit_Progress_Report;

      -----------------------
      -- Emit_Progress_End --
      -----------------------

      procedure Emit_Progress_End is
         P : LSP.Messages.Progress_Params (LSP.Messages.Progress_End);
      begin
         P.End_Param.token := Self.Indexing_Token;
         Self.Server.On_Progress (P);
      end Emit_Progress_End;

   begin
      --  Prevent work if the indexing has been explicitly disabled or
      --  if we have other messages to process.
      if not Self.Indexing_Enabled or Self.Server.Has_Pending_Work then
         return;
      end if;

      if Self.Indexing_Token = Empty_Token then
         Self.Indexing_Token := Self.Get_Unique_Progress_Token ("indexing");
         Emit_Progress_Begin;
      end if;

      while not Self.Files_To_Index.Is_Empty loop
         declare
            Cursor : File_Sets.Cursor := Self.Files_To_Index.First;
            File   : constant GNATCOLL.VFS.Virtual_File :=
              File_Sets.Element (Cursor);
         begin
            Self.Files_To_Index.Delete (Cursor);
            Self.Total_Files_Indexed := Self.Total_Files_Indexed + 1;

            if not Self.Open_Documents.Contains (File) then
               Emit_Progress_Report
                 (Self.Total_Files_Indexed, Self.Total_Files_To_Index);

               for Context of Self.Contexts_For_File (File) loop
                  --  Set Reparse to False to avoid issues with LAL envs
                  --  for now (see T226-048 for more info).
                  Context.Index_File (File, Reparse => False);
               end loop;

               --  Check whether another request is pending. If so, pause
               --  the indexing; it will be resumed later as part of
               --  After_Request.
               if not Self.Files_To_Index.Is_Empty
                 and then Self.Server.Has_Pending_Work
               then
                  return;
               end if;
            end if;
         end;
      end loop;

      Emit_Progress_End;
      Self.Indexing_Token := Empty_Token;
   end Index_Files;

   ------------------------------------------
   -- On_Workspace_Execute_Command_Request --
   ------------------------------------------

   overriding function On_Workspace_Execute_Command_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Workspace_Execute_Command_Request)
      return LSP.Messages.Server_Responses.ExecuteCommand_Response
   is
      Error    : LSP.Errors.Optional_ResponseError;
      Params   : LSP.Messages.ExecuteCommandParams renames
        Request.params;
      Response : LSP.Messages.Server_Responses.ExecuteCommand_Response
        (Is_Error => True);
   begin
      if Params.Is_Unknown or else Params.Custom.Is_Null then
         Response.error :=
           (True,
            (code => LSP.Errors.InternalError,
             message => "Not implemented",
             data    => <>));
         return Response;
      end if;

      Params.Custom.Unchecked_Get.Execute
        (Handler => Self,
         Client  => Self.Server,
         Error   => Error);

      if Error.Is_Set then
         Response.error := Error;
         return Response;
      end if;

      --  No particular response in case of success.
      return (Is_Error => False,
              error    => (Is_Set => False),
              others   => <>);
   end On_Workspace_Execute_Command_Request;

   --------------------------------------------
   -- On_Workspace_Will_Create_Files_Request --
   --------------------------------------------

   overriding function On_Workspace_Will_Create_Files_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.
                  Workspace_Will_Create_Files_Request)
      return LSP.Messages.Server_Responses.WillCreateFiles_Response
   is
      Response : LSP.Messages.Server_Responses.WillCreateFiles_Response
        (Is_Error => False);
   begin
      Self.Trace.Trace
        ("Message_Handler On_Workspace_Will_Create_Files_Request");
      return Response;
   end On_Workspace_Will_Create_Files_Request;

   ------------------------------------
   -- On_DidCreateFiles_Notification --
   ------------------------------------

   overriding procedure On_DidCreateFiles_Notification
     (Self  : access Message_Handler;
      Value : LSP.Messages.CreateFilesParams) is
   begin
      Self.Trace.Trace
        ("Message_Handler On_DidCreateFiles_Notification");

      --  New sources were created on this project, so recompute its view

      Self.Project_Tree.Recompute_View;

      --  For each created file of Value.files:
      --  - find the contexts that contains its directory
      --  - add it to those contexts
      --  - index it on those contexts

      for File of Value.files loop
         declare
            use VSS.Strings.Conversions;

            Created_File : constant Virtual_File :=
              Self.To_File (To_LSP_URI (File.uri));

            function Has_Dir
              (Context : LSP.Ada_Contexts.Context)
                  return Boolean
            is (Context.List_Source_Directories.Contains
                (Created_File.Dir));
            --  Return True if Old_File is a source of the project held by
            --  Context.

         begin
            if Self.Is_Ada_Source (Created_File) then
               for Context of Self.Contexts.Each_Context
                 (Has_Dir'Unrestricted_Access)
               loop
                  Context.Include_File (Created_File);
                  Context.Index_File (Created_File);

                  Self.Trace.Trace
                    ("Included " & Created_File.Display_Base_Name
                     & " in context " & To_UTF_8_String (Context.Id));
               end loop;
            end if;
         end;
      end loop;

      Self.Trace.Trace
        ("Finished Message_Handler On_DidCreateFiles_Notification");
   end On_DidCreateFiles_Notification;

   --------------------------------------------
   -- On_Workspace_Will_Rename_Files_Request --
   --------------------------------------------

   overriding function On_Workspace_Will_Rename_Files_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.
                  Workspace_Will_Rename_Files_Request)
      return LSP.Messages.Server_Responses.WillRenameFiles_Response
   is
      Response : LSP.Messages.Server_Responses.WillRenameFiles_Response
        (Is_Error => False);
   begin
      Self.Trace.Trace
        ("Message_Handler On_Workspace_Will_Rename_Files_Request");
      return Response;
   end On_Workspace_Will_Rename_Files_Request;

   ------------------------------------
   -- On_DidRenameFiles_Notification --
   ------------------------------------

   overriding procedure On_DidRenameFiles_Notification
     (Self  : access Message_Handler;
      Value : LSP.Messages.RenameFilesParams)
   is
      use LSP.Ada_Context_Sets;

      package URI_Contexts_Maps is new
        Ada.Containers.Indefinite_Hashed_Maps
          (Key_Type        => LSP_URI,
           Element_Type    => Context_Lists.List,
           Hash            => Hash,
           Equivalent_Keys => Equal,
           "="             => Context_Lists."=");

      subtype URI_Contexts_Map is URI_Contexts_Maps.Map;

      URIs_Contexts : URI_Contexts_Map;

   begin
      Self.Trace.Trace
        ("Message_Handler On_DidRenameFiles_Notification");

      --  Some project sources were renamed, so recompute its view

      Self.Project_Tree.Recompute_View;

      --  For each oldUri of Value.files:
      --  - map it to a list of context that contains it
      --  - remove it from those contexts
      --  - re-index it on those contexts so that an empty unit is reparsed

      for File_Rename of Value.files loop
         declare
            use VSS.Strings.Conversions;

            Old_File : constant Virtual_File :=
              Self.To_File (To_LSP_URI (File_Rename.oldUri));

            function Has_File
              (Context : LSP.Ada_Contexts.Context)
               return Boolean
            is (Context.Is_Part_Of_Project (Old_File));
            --  Return True if Old_File is a source of the project held by
            --  Context.

            URI_Contexts : Context_Lists.List;

         begin
            if Self.Is_Ada_Source (Old_File) then
               for Context of Self.Contexts.Each_Context
                 (Has_File'Unrestricted_Access)
               loop
                  URI_Contexts.Append (Context);
                  Context.Exclude_File (Old_File);
                  Context.Index_File (Old_File);

                  Self.Trace.Trace
                    ("Excluded " & Old_File.Display_Full_Name
                     & " from context " & To_UTF_8_String (Context.Id));
               end loop;

               URIs_Contexts.Insert
                 (To_LSP_URI (File_Rename.oldUri), URI_Contexts);
            end if;
         end;
      end loop;

      --  For each (oldUri, newUri) tuple:
      --  - add newUri to all contexts that contained oldUri
      --  - index the newUri (using the appriate method depending if
      --    (there's an open document of not)

      for File_Rename of Value.files loop
         declare
            use VSS.Strings.Conversions;

            New_File : constant GNATCOLL.VFS.Virtual_File :=
              Self.To_File (LSP.Types.To_LSP_URI (File_Rename.newUri));
            Document : constant LSP.Ada_Documents.Document_Access :=
              Get_Open_Document
                (Self,
                 LSP.Messages.DocumentUri
                   (LSP.Types.To_LSP_URI (File_Rename.newUri)));
            Is_Document_Open : constant Boolean := Document /= null;

         begin
            if Self.Is_Ada_Source (New_File) then
               for Context of
                 URIs_Contexts.Constant_Reference
                   (To_LSP_URI (File_Rename.oldUri))
               loop
                  Context.Include_File (New_File);
                  if Is_Document_Open then
                     Context.Index_Document (Document.all);
                  else
                     Context.Index_File (New_File);
                  end if;
                  Self.Trace.Trace
                    ("Included " & New_File.Display_Base_Name & " in context "
                     & To_UTF_8_String (Context.Id));
               end loop;
            end if;
         end;
      end loop;

      Self.Trace.Trace
        ("Finished Message_Handler On_DidRenameFiles_Notification");
   end On_DidRenameFiles_Notification;

   --------------------------------------------
   -- On_Workspace_Will_Delete_Files_Request --
   --------------------------------------------

   overriding function On_Workspace_Will_Delete_Files_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.
                  Workspace_Will_Delete_Files_Request)
      return LSP.Messages.Server_Responses.WillDeleteFiles_Response
   is
      Response : LSP.Messages.Server_Responses.WillDeleteFiles_Response
        (Is_Error => False);
   begin
      Self.Trace.Trace
        ("Message_Handler On_Workspace_Will_Delete_Files_Request");
      return Response;
   end On_Workspace_Will_Delete_Files_Request;

   ------------------------------------
   -- On_DidDeleteFiles_Notification --
   ------------------------------------

   overriding procedure On_DidDeleteFiles_Notification
     (Self  : access Message_Handler;
      Value : LSP.Messages.DeleteFilesParams) is
   begin
      Self.Trace.Trace
        ("Message_Handler On_DidDeleteFiles_Notification");

      --  Some project sources were deleted, so recompute its view

      Self.Project_Tree.Recompute_View;

      --  For each delete file of Value.files:
      --  - find the contexts that contains it
      --  - remove it from those contexts
      --  - re-index it on those contexts so that an empty unit is reparsed

      for File of Value.files loop
         declare
            Deleted_File : constant Virtual_File :=
              Self.To_File (To_LSP_URI (File.uri));

            function Has_File
              (Context : LSP.Ada_Contexts.Context)
               return Boolean
            is (Context.Is_Part_Of_Project (Deleted_File));
            --  Return True if Old_File is a source of the project held by
            --  Context.

         begin
            if Self.Is_Ada_Source (Deleted_File) then
               for Context of Self.Contexts.Each_Context
                 (Has_File'Unrestricted_Access)
               loop
                  Context.Exclude_File (Deleted_File);
                  Context.Index_File (Deleted_File);

                  Self.Trace.Trace
                    ("Excluded " & Deleted_File.Display_Base_Name
                     & " from context "
                     & VSS.Strings.Conversions.To_UTF_8_String (Context.Id));
               end loop;
            end if;
         end;
      end loop;

      Self.Trace.Trace
        ("Finished Message_Handler On_DidDeleteFiles_Notification");
   end On_DidDeleteFiles_Notification;

   ----------------------------------
   -- On_Workspace_Symbols_Request --
   ----------------------------------

   overriding function On_Workspace_Symbols_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Workspace_Symbols_Request)
      return LSP.Messages.Server_Responses.Symbol_Response
   is
      use type LSP.Messages.Search_Kind;
      use type VSS.Strings.Character_Count;
      use type Ada.Containers.Count_Type;

      procedure On_Inaccessible_Name
        (File : GNATCOLL.VFS.Virtual_File;
         Name : Libadalang.Analysis.Defining_Name;
         Stop : in out Boolean);

      Names : LSP.Ada_Completions.Completion_Maps.Map;

      package Canceled is new LSP.Generic_Cancel_Check (Request'Access, 127);

      procedure Write_Symbols is
        new LSP.Ada_Completions.Generic_Write_Symbols
          (Canceled.Has_Been_Canceled);

      --------------------------
      -- On_Inaccessible_Name --
      --------------------------

      procedure On_Inaccessible_Name
        (File : GNATCOLL.VFS.Virtual_File;
         Name : Libadalang.Analysis.Defining_Name;
         Stop : in out Boolean) is
      begin
         --  Skip all names in open documents, because they could have
         --  stale references. Then skip already provided results.
         if not Self.Open_Documents.Contains (File)
           and then not Names.Contains (Name)
         then
            Names.Insert
              (Name,
               (Is_Dot_Call  => False,
                Is_Visible   => False,
                Use_Snippets => False,
                Pos          => <>,
                Weight       => <>));
         end if;

         Stop := Canceled.Has_Been_Canceled;
      end On_Inaccessible_Name;

      Pattern : constant Search_Pattern'Class := Build
        (Pattern        => Request.params.query,
         Case_Sensitive => Request.params.case_sensitive = LSP.Types.True,
         Whole_Word     => Request.params.whole_word = LSP.Types.True,
         Negate         => Request.params.negate = LSP.Types.True,
         Kind           =>
           (if Request.params.kind.Is_Set
            then Request.params.kind.Value
            else LSP.Messages.Start_Word_Text));

      Response : LSP.Messages.Server_Responses.Symbol_Response
        (Is_Error => False);

      Partial_Response_Sended : Boolean := False;

      -- Send_Partial_Response --

      procedure Send_Partial_Response;
      procedure Send_Partial_Response
      is
         P : LSP.Messages.Progress_SymbolInformation_Vector;
         V : LSP.Messages.Symbol_Vector;
      begin
         if Canceled.Has_Been_Canceled then
            return;
         end if;

         Write_Symbols (Names, V);
         Names.Clear;

         P.token := Request.params.partialResultToken.Value;
         P.value := V.Vector;

         Self.Server.On_Progress_SymbolInformation_Vector (P);

         Partial_Response_Sended := True;
      end Send_Partial_Response;

   begin
      if Pattern.Get_Kind /= LSP.Messages.Start_Word_Text
        and then Pattern.Get_Canonical_Pattern.Character_Length < 2
      then
         --  Do not process too small pattern because
         --  this produces a huge response that is useless
         --  and costs a while.

         return Response;
      end if;

      for Context of Self.Contexts.Each_Context loop
         Context.Get_Any_Symbol
           (Pattern     => Pattern,
            Only_Public => False,
            Callback    => On_Inaccessible_Name'Access);

         if Canceled.Has_Been_Canceled then
            return Response;

         elsif Request.params.partialResultToken.Is_Set
           and then Names.Length > 100
         then
            Send_Partial_Response;
         end if;
      end loop;

      for Doc of Self.Open_Documents loop
         declare
            Context : constant Context_Access :=
              Self.Contexts.Get_Best_Context (Doc.URI);
         begin
            Doc.Get_Any_Symbol
              (Context.all,
               Pattern,
               Ada.Containers.Count_Type'Last,
               False,
               Canceled.Has_Been_Canceled'Access,
               Names);
         end;

         if Canceled.Has_Been_Canceled then
            return Response;

         elsif Request.params.partialResultToken.Is_Set
           and then Names.Length > 100
         then
            Send_Partial_Response;
         end if;
      end loop;

      if Partial_Response_Sended then
         Send_Partial_Response;
      else
         Write_Symbols (Names, Response.result);
      end if;

      return Response;
   end On_Workspace_Symbols_Request;

   ---------------------------
   -- On_Completion_Request --
   ---------------------------

   overriding function On_Completion_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Completion_Request)
      return LSP.Messages.Server_Responses.Completion_Response
   is
      --  We're completing only based on one context, ie one project
      --  tree: this seems reasonable. One further refinement could
      --  be to return only results that are available for all
      --  project contexts.

      Value    : LSP.Messages.TextDocumentPositionParams renames
        Request.params;

      Context  : constant Context_Access :=
        Self.Contexts.Get_Best_Context (Value.textDocument.uri);

      Document : constant LSP.Ada_Documents.Document_Access :=
        Get_Open_Document (Self, Value.textDocument.uri);

      Names     : LSP.Ada_Completions.Completion_Maps.Map;

      --  If lazy computation for the 'detail' and 'documentation' fields is
      --  supported by the client, set the Compute_Doc_And_Details flag to
      --  False.
      Compute_Doc_And_Details : constant Boolean :=
        not
          (Self.Completion_Resolve_Properties.Contains
             (VSS.Strings.Conversions.To_Virtual_String ("detail"))
           and then
             Self.Completion_Resolve_Properties.Contains
                (VSS.Strings.Conversions.To_Virtual_String ("documentation")));

      P1 : aliased LSP.Ada_Completions.Aspects.Aspect_Completion_Provider;
      P2 : aliased LSP.Ada_Completions.Pragmas.Pragma_Completion_Provider;
      P3 : aliased LSP.Ada_Completions.Keywords.Keyword_Completion_Provider;
      P4 : aliased
        LSP.Ada_Completions.Attributes.Attributes_Completion_Provider;

      P5 : aliased LSP.Ada_Completions.Names.Name_Completion_Provider
        (Self.Use_Completion_Snippets);
      P6 : aliased LSP.Ada_Handlers.Invisibles.Invisible_Completion_Provider
        (Self, Context);
      P7 : aliased
        LSP.Ada_Completions.Parameters.Parameter_Completion_Provider
          (Context                  => Context,
           Document                 => Document,
           Compute_Doc_And_Details  => Compute_Doc_And_Details,
           Named_Notation_Threshold => Self.Named_Notation_Threshold);
      P8 : aliased LSP.Ada_Completions.End_Names.End_Name_Completion_Provider;

      Providers : constant LSP.Ada_Completions.Completion_Provider_List :=
        [P1'Unchecked_Access,
         P2'Unchecked_Access,
         P3'Unchecked_Access,
         P4'Unchecked_Access,
         P5'Unchecked_Access,
         P6'Unchecked_Access,
         P7'Unchecked_Access,
         P8'Unchecked_Access];

      Response : LSP.Messages.Server_Responses.Completion_Response
        (Is_Error => False);

      Sloc  : Langkit_Support.Slocs.Source_Location;
      Token : Libadalang.Common.Token_Reference;
      Node  : Libadalang.Analysis.Ada_Node;
   begin
      Document.Get_Completion_Node
        (Context  => Context.all,
         Position => Value.position,
         Sloc     => Sloc,
         Token    => Token,
         Node     => Node);

      Document.Get_Completions_At
        (Context   => Context.all,
         Providers => Providers,
         Sloc      => Sloc,
         Token     => Token,
         Node      => Node,
         Names     => Names,
         Result    => Response.result);

      LSP.Ada_Completions.Write_Completions
        (Context                  => Context.all,
         Document                 => Document.all,
         Sloc                     => Sloc,
         Node                     => Node,
         Names                    => Names,
         Named_Notation_Threshold => Self.Named_Notation_Threshold,
         Compute_Doc_And_Details  => Compute_Doc_And_Details,
         Result                   => Response.result.items);

      return Response;
   end On_Completion_Request;

   --------------------------------------
   -- On_CompletionItemResolve_Request --
   --------------------------------------

   overriding function On_CompletionItemResolve_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.CompletionItemResolve_Request)
      return LSP.Messages.Server_Responses.CompletionItemResolve_Response
   is
      Item      : LSP.Messages.CompletionItem :=
        Request.params;
      Response  : LSP.Messages.Server_Responses.CompletionItemResolve_Response
        (Is_Error => False);
      C         : Context_Access;
      Node      : Libadalang.Analysis.Ada_Node;
   begin
      --  Return immediately if we don't have data that allows us to compute
      --  additional information for the given item.
      --  This is the case when all the completion item's fields have already
      --  been computed.
      if not Item.data.Is_Set then
         Response.result := Item;
         return Response;
      end if;

      C := Self.Contexts.Get_Best_Context (Item.data.Value.uri);
      Node := Get_Node_At
        (Self         => C.all,
         Document     => null,
         Project_Only => False,
         Position     => LSP.Messages.TextDocumentPositionParams'
           (textDocument => (uri => Item.data.Value.uri),
            position     => Item.data.Value.span.first));

      --  Retrieve the Basic_Decl from the completion item's SLOC
      while not Node.Is_Null
        and then Node.Kind not in Libadalang.Common.Ada_Basic_Decl
      loop
         Node := Node.Parent;
      end loop;

      --  Compute the completion item's details
      if not Node.Is_Null then
         declare
            BD : constant Libadalang.Analysis.Basic_Decl :=
              Node.As_Basic_Decl;
         begin
            Item.detail := (True, Compute_Completion_Detail (BD));

            --  Property_Errors can occur when calling
            --  Get_Documentation on unsupported docstrings, so
            --  add an exception handler to catch them and recover.

            Item.documentation :=
              (Is_Set => True,
               Value  => LSP.Messages.String_Or_MarkupContent'
                 (Is_String => True,
                  String    => LSP.Lal_Utils.Compute_Completion_Doc (BD)));

         exception
            when E : Libadalang.Common.Property_Error =>
               LSP.Common.Log (C.Trace, E);
               Item.documentation := (others => <>);
         end;

         Response.result := Item;
      end if;

      return Response;
   end On_CompletionItemResolve_Request;

   ---------------------------
   -- On_Formatting_Request --
   ---------------------------

   overriding function On_Formatting_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Formatting_Request)
      return LSP.Messages.Server_Responses.Formatting_Response
   is
      Context  : constant Context_Access :=
        Self.Contexts.Get_Best_Context (Request.params.textDocument.uri);

      Document : constant LSP.Ada_Documents.Document_Access :=
        Get_Open_Document (Self, Request.params.textDocument.uri);
   begin
      if Document.Has_Diagnostics (Context.all) then
         return Response : LSP.Messages.Server_Responses.Formatting_Response
           (Is_Error => True)
         do
            Response.error :=
              (True,
               (code    => LSP.Errors.InternalError,
                message => "Incorrect code can't be formatted",
                data    => <>));
         end return;
      end if;

      declare
         use LSP.Messages;

         Response : constant Server_Responses.Formatting_Response :=
           Format
             (Self     => Context.all,
              Document => Document,
              Span     => LSP.Messages.Empty_Span,
              Options  => Request.params.options,
              Handler  => Self);
      begin
         return Response;
      end;
   end On_Formatting_Request;

   ---------------------------------------
   -- On_Prepare_Call_Hierarchy_Request --
   ---------------------------------------

   overriding function On_Prepare_Call_Hierarchy_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Prepare_Call_Hierarchy_Request)
      return LSP.Messages.Server_Responses.PrepareCallHierarchy_Response
   is
      Value     : LSP.Messages.CallHierarchyPrepareParams renames
        Request.params;

      Response  : LSP.Messages.Server_Responses.PrepareCallHierarchy_Response
        (Is_Error => False);

      Imprecise : Boolean := False;

      C         : constant Context_Access :=
        Self.Contexts.Get_Best_Context (Value.textDocument.uri);
      --  For the PrepareCallHierarchy request, we're only interested in the
      --  "best" response value, not in the list of values for all contexts

      Node      : constant Libadalang.Analysis.Name :=
        Laltools.Common.Get_Node_As_Name
          (C.Get_Node_At
            (Get_Open_Document (Self, Value.textDocument.uri), Value));

      Name      : constant Libadalang.Analysis.Defining_Name :=
        Laltools.Common.Resolve_Name
          (Node,
           Self.Trace,
           Imprecise);

      Decl      : Libadalang.Analysis.Basic_Decl;
      Next_Part : Libadalang.Analysis.Basic_Decl;
   begin
      if Name.Is_Null then
         return Response;
      end if;

      Decl := Name.P_Basic_Decl;

      if Decl.Is_Null or else not Decl.P_Is_Subprogram then
         return Response;
      end if;

      Next_Part :=
        Laltools.Common.Find_Next_Part_For_Decl (Decl, Self.Trace);

      if Next_Part.Is_Null then
         Next_Part := Decl;
      end if;

      Response.result.Append
        (To_Call_Hierarchy_Item (Next_Part.P_Defining_Name));

      if Imprecise then
         Self.Show_Imprecise_Reference_Warning ("prepareCallHierarchy");
      end if;

      return Response;
   end On_Prepare_Call_Hierarchy_Request;

   -------------------------------
   -- On_Prepare_Rename_Request --
   -------------------------------

   overriding function On_Prepare_Rename_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Prepare_Rename_Request)
      return LSP.Messages.Server_Responses.Prepare_Rename_Response
   is
      Value    : LSP.Messages.TextDocumentPositionParams renames
        Request.params;

      Response : LSP.Messages.Server_Responses.Prepare_Rename_Response
        (Is_Error => False);

      Document : constant LSP.Ada_Documents.Document_Access :=
        Get_Open_Document (Self, Value.textDocument.uri);

      Context : constant Context_Access :=
        Self.Contexts.Get_Best_Context (Value.textDocument.uri);
      --  For the prepareRename request, we're only interested in the "best"
      --  context to check that we are able to rename the name.

      Name_Node : constant Libadalang.Analysis.Name :=
        Laltools.Common.Get_Node_As_Name
          (Context.Get_Node_At (Document, Value));

      Imprecise : Boolean := False;

      Defining_Name : constant Libadalang.Analysis.Defining_Name :=
        Laltools.Common.Resolve_Name (Name_Node, Self.Trace, Imprecise);

   begin
      if not Name_Node.Is_Null
        and then not Defining_Name.Is_Null
        and then not Imprecise
      then
         --  Success only if the node is a name and can be resolved precisely
         Response.result :=
           (Is_Set => True,
            Value  => LSP.Lal_Utils.To_Span (Name_Node.Sloc_Range));

      end if;

      return Response;
   end On_Prepare_Rename_Request;

   -------------------------------
   -- On_Incoming_Calls_Request --
   -------------------------------

   overriding function On_Incoming_Calls_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Incoming_Calls_Request)
      return LSP.Messages.Server_Responses.IncomingCalls_Response
   is
      use Libadalang.Analysis;

      procedure Process_Context (C : Context_Access);
      --  Process the subprogram found in one context and append corresponding
      --  calls to Response.results.

      Item : LSP.Messages.CallHierarchyItem renames
        Request.params.item;
      Response   : LSP.Messages.Server_Responses.IncomingCalls_Response
        (Is_Error => False);
      Imprecise  : Boolean := False;

      Position : constant LSP.Messages.TextDocumentPositionParams :=
        (textDocument => (uri => Item.uri),
         position     => Item.selectionRange.first);

      Filter : File_Span_Sets.Set;

      procedure Add_Incoming_Call
        (Filter : in out File_Span_Sets.Set;
         Call   : LSP.Messages.CallHierarchyIncomingCall);
      --  Add an incoming call in results. Use Filter to prevent having
      --  duplicates

      -----------------------
      -- Add_Incoming_Call --
      -----------------------

      procedure Add_Incoming_Call
        (Filter : in out File_Span_Sets.Set;
         Call : LSP.Messages.CallHierarchyIncomingCall)
      is
         Span : constant File_Span :=
           (Self.To_File (Call.from.uri), Call.from.span);
      begin
         if not Filter.Contains (Span) then
            Response.result.Append (Call);
            Filter.Insert (Span);
         end if;
      end Add_Incoming_Call;

      ---------------------
      -- Process_Context --
      ---------------------

      procedure Process_Context (C : Context_Access) is
         Definition : Defining_Name;
      begin
         Self.Imprecise_Resolve_Name (C, Position, Definition);

         --  Attempt to resolve the name, return no results if we can't or if
         --  the name does not resolve to a callable object, like a subprogram
         --  or an entry.

         if Definition = No_Defining_Name
           or else not Definition.P_Basic_Decl.P_Is_Subprogram
           or else Request.Canceled
         then
            return;
         end if;

         declare
            use Laltools.Common;

            This_Imprecise : Boolean;
            Incoming_Calls : constant References_By_Subprogram.Map :=
              LSP.Lal_Utils.Find_Incoming_Calls
                (C.all, Definition, This_Imprecise);
            C              : References_By_Subprogram.Cursor :=
              Incoming_Calls.First;
         begin
            Imprecise := Imprecise or This_Imprecise;

            --  Iterate through all the results, converting them to protocol
            --  objects.
            while References_By_Subprogram.Has_Element (C) loop
               declare
                  Node     : constant Defining_Name :=
                    References_By_Subprogram.Key (C);
                  Refs     : constant References_Sets.Set :=
                    References_By_Subprogram.Element (C);
                  New_Call : LSP.Messages.CallHierarchyIncomingCall;
               begin
                  To_Call_Hierarchy_Result
                    (Node  => Node,
                     Refs  => Refs,
                     Item  => New_Call.from,
                     Spans => New_Call.fromRanges,
                     Kinds => New_Call.kinds);

                  Add_Incoming_Call (Filter, New_Call);
                  References_By_Subprogram.Next (C);
               end;
            end loop;
         end;
      end Process_Context;

   begin
      --  Find the references in all contexts
      for C of Self.Contexts_For_URI (Item.uri) loop
         Process_Context (C);

         exit when Request.Canceled;
      end loop;

      if Imprecise then
         Self.Show_Imprecise_Reference_Warning ("incomingCalls");
      end if;

      return Response;
   end On_Incoming_Calls_Request;

   -------------------------------
   -- On_Outgoing_Calls_Request --
   -------------------------------

   overriding function On_Outgoing_Calls_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Outgoing_Calls_Request)
      return LSP.Messages.Server_Responses.OutgoingCalls_Response
   is
      use Libadalang.Analysis;

      Item : LSP.Messages.CallHierarchyItem renames
        Request.params.item;
      Response   : LSP.Messages.Server_Responses.OutgoingCalls_Response
        (Is_Error => False);
      Imprecise  : Boolean := False;

      Position : constant LSP.Messages.TextDocumentPositionParams :=
           (textDocument => (uri => Item.uri),
            position     => Item.selectionRange.first);

      Filter : File_Span_Sets.Set;

      procedure Add_Outgoing_Call
        (Filter : in out File_Span_Sets.Set;
         Call   : LSP.Messages.CallHierarchyOutgoingCall);
      --  Add a subprogram in results. Use Filter to prevent having duplicates

      procedure Process_Context (C : Context_Access);
      --  Process the calls found in one context and append
      --  them to Response.results.

      -----------------------
      -- Add_Outgoing_Call --
      -----------------------

      procedure Add_Outgoing_Call
        (Filter : in out File_Span_Sets.Set;
         Call   : LSP.Messages.CallHierarchyOutgoingCall)
      is
         Span : constant File_Span :=
           (Self.To_File (Call.to.uri), Call.to.span);
      begin
         if not Filter.Contains (Span) then
            Response.result.Append (Call);
            Filter.Insert (Span);
         end if;
      end Add_Outgoing_Call;

      ---------------------
      -- Process_Context --
      ---------------------

      procedure Process_Context (C : Context_Access) is
         Definition : Defining_Name;
      begin
         Self.Imprecise_Resolve_Name (C, Position, Definition);

         --  Attempt to resolve the name, return no results if we can't or if
         --  the name does not resolve to a callable object, like a subprogram
         --  or an entry.

         if Definition = No_Defining_Name
           or else not Definition.P_Basic_Decl.P_Is_Subprogram
           or else Request.Canceled
         then
            return;
         end if;

         declare
            use Laltools.Common;

            This_Imprecise : Boolean;
            Outgoing_Calls : constant References_By_Subprogram.Map :=
              LSP.Lal_Utils.Find_Outgoing_Calls
                (C.all, Definition, This_Imprecise);
            C              : References_By_Subprogram.Cursor :=
              Outgoing_Calls.First;
         begin
            Imprecise := Imprecise or This_Imprecise;

            --  Iterate through all the results, converting them to protocol
            --  objects.
            while References_By_Subprogram.Has_Element (C) loop
               declare
                  Node     : constant Defining_Name :=
                    References_By_Subprogram.Key (C);
                  Refs     : constant References_Sets.Set :=
                    References_By_Subprogram.Element (C);
                  New_Call : LSP.Messages.CallHierarchyOutgoingCall;
               begin
                  To_Call_Hierarchy_Result
                    (Node  => Node,
                     Refs  => Refs,
                     Item  => New_Call.to,
                     Spans => New_Call.fromRanges,
                     Kinds => New_Call.kinds);

                  Add_Outgoing_Call (Filter, New_Call);
                  References_By_Subprogram.Next (C);
               end;
            end loop;
         end;
      end Process_Context;
   begin
      --  Find the references in all contexts
      for C of Self.Contexts_For_URI (Item.uri) loop
         Process_Context (C);

         exit when Request.Canceled;
      end loop;

      if Imprecise then
         Self.Show_Imprecise_Reference_Warning ("outgoingCalls");
      end if;

      return Response;
   end On_Outgoing_Calls_Request;

   ---------------------------------
   -- On_Range_Formatting_Request --
   ---------------------------------

   overriding function On_Range_Formatting_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Range_Formatting_Request)
      return LSP.Messages.Server_Responses.Range_Formatting_Response
   is
      Context  : constant Context_Access :=
        Self.Contexts.Get_Best_Context (Request.params.textDocument.uri);

      Document : constant LSP.Ada_Documents.Document_Access :=
        Get_Open_Document (Self, Request.params.textDocument.uri);

   begin
      if Document.Has_Diagnostics (Context.all) then
         return Response : LSP.Messages.Server_Responses.
           Range_Formatting_Response (Is_Error => True)
         do
            Response.error :=
              (True,
               (code    => LSP.Errors.InternalError,
                message => "Syntactically incorrect code can't be formatted",
                data    => <>));
         end return;
      end if;

      declare
         use LSP.Messages;

         Result   : constant Server_Responses.Formatting_Response :=
           (if Partial_GNATpp.Is_Active then
              Range_Format
                (Self     => Context.all,
                 Document => Document,
                 Span     => Request.params.span,
                 Options  => Request.params.options)
            else
              Format
                (Self     => Context.all,
                 Document => Document,
                 Span     => Request.params.span,
                 Options  => Request.params.options,
                 Handler  => Self));
         Response : Server_Responses.Range_Formatting_Response
           (Is_Error => Result.Is_Error);
      begin
         if not Result.Is_Error then
            Response.result := Result.result;
         else
            Response.error := Result.error;
         end if;

         return Response;
      end;
   end On_Range_Formatting_Request;

   ------------------
   -- Handle_Error --
   ------------------

   overriding procedure Handle_Error
     (Self : access Message_Handler) is
   begin
      --  Reload the contexts in case of unexpected errors.
      Self.Contexts.Reload_All_Contexts;
   end Handle_Error;

   function Hash (Value : File_Span) return Ada.Containers.Hash_Type is
      use type Ada.Containers.Hash_Type;
      Prime : constant := 271;
      Name  : constant Ada.Containers.Hash_Type := Value.File.Full_Name_Hash;
      From  : constant Ada.Containers.Hash_Type :=
        Prime * Ada.Containers.Hash_Type'Mod (Value.Span.first.line)
        + Ada.Containers.Hash_Type'Mod (Value.Span.first.character);
      To    : constant Ada.Containers.Hash_Type :=
        Prime * Ada.Containers.Hash_Type'Mod (Value.Span.last.line)
        + Ada.Containers.Hash_Type'Mod (Value.Span.last.character);
   begin
      return Name + From + To;
   end Hash;

   -------------------------
   -- Publish_Diagnostics --
   -------------------------

   procedure Publish_Diagnostics
     (Self     : access Message_Handler'Class;
      Document : not null LSP.Ada_Documents.Document_Access)
   is
      Ok   : Boolean;
      Diag : LSP.Messages.PublishDiagnosticsParams;
   begin
      if Self.Diagnostics_Enabled then
         Document.Get_Errors
           (Self.Contexts.Get_Best_Context (Document.URI).all,
            Ok,
            Diag.diagnostics);

         if Ok then
            Diag.uri := Document.URI;
            Self.Server.On_Publish_Diagnostics (Diag);
         end if;
      end if;
   end Publish_Diagnostics;

   ----------
   -- Free --
   ----------

   procedure Free (Self : in out Internal_Document_Access) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (LSP.Ada_Documents.Document, Internal_Document_Access);
   begin
      Self.Cleanup;
      Unchecked_Free (Self);
   end Free;

   ------------------
   -- Show_Message --
   ------------------

   procedure Show_Message
     (Self : access Message_Handler;
      Text : VSS.Strings.Virtual_String;
      Mode : LSP.Messages.MessageType := LSP.Messages.Error) is
   begin
      Self.Server.On_Show_Message ((Mode, Text));
   end Show_Message;

   --------------------------------------
   -- Show_Imprecise_Reference_Warning --
   --------------------------------------

   procedure Show_Imprecise_Reference_Warning
     (Self      : access Message_Handler;
      Operation : String) is
   begin
      if Notifications_For_Imprecise.Is_Active then
         Self.Server.On_Show_Message
           ((LSP.Messages.Warning,
            VSS.Strings.Conversions.To_Virtual_String
              ("The result of '" & Operation & "' is approximate.")));
      end if;
   end Show_Imprecise_Reference_Warning;

   -----------------
   -- Before_Work --
   -----------------

   overriding procedure Before_Work
     (Self    : access Message_Handler;
      Message : LSP.Messages.Message'Class) is null;

   ----------------
   -- After_Work --
   ----------------

   overriding procedure After_Work
     (Self    : access Message_Handler;
      Message : LSP.Messages.Message'Class)
   is
      pragma Unreferenced (Message);
   begin
     --  We have finished processing a request or notification:
     --  if it happens that indexing is required, do it now.
      if not Self.Files_To_Index.Is_Empty then
         Self.Index_Files;
      end if;
   end After_Work;

   ---------------
   -- From_File --
   ---------------

   function From_File
     (Self : Message_Handler'Class;
      File : Virtual_File) return LSP.Messages.DocumentUri is
        (LSP.Types.To_LSP_URI
           (VSS.Strings.Conversions.To_Virtual_String
                (URIs.Conversions.From_File (File.Display_Full_Name))));

   -------------
   -- To_File --
   -------------

   function To_File
     (Self : Message_Handler'Class;
      URI  : LSP.Types.LSP_URI) return GNATCOLL.VFS.Virtual_File
   is
      To     : constant URIs.URI_String := LSP.Types.To_UTF_8_String (URI);
      Result : constant String := URIs.Conversions.To_File
        (To, Normalize => Self.Follow_Symlinks);
   begin
      return GNATCOLL.VFS.Create_From_UTF8 (Result);
   end To_File;

   -----------------
   -- URI_To_File --
   -----------------

   function URI_To_File
     (Self : Message_Handler'Class;
      URI  : VSS.Strings.Virtual_String) return VSS.Strings.Virtual_String
   is
      To     : constant URIs.URI_String :=
        VSS.Strings.Conversions.To_UTF_8_String (URI);
      Result : constant String := URIs.Conversions.To_File
        (To, Normalize => Self.Follow_Symlinks);

   begin
      return VSS.Strings.Conversions.To_Virtual_String (Result);
   end URI_To_File;

   ---------------------------------
   -- On_ALS_Check_Syntax_Request --
   ---------------------------------

   overriding function On_ALS_Check_Syntax_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.ALS_Check_Syntax_Request)
      return LSP.Messages.Server_Responses.ALS_Check_Syntax_Response
   is
      use Laltools.Common;
      use LSP.Messages.Server_Responses;
      use VSS.Strings;

      function "+"
        (Item : Virtual_String'Class)
         return Ada.Strings.UTF_Encoding.UTF_8_String
         renames VSS.Strings.Conversions.To_UTF_8_String;

      function "+"
        (Item : Virtual_String'Class)
         return Unbounded_String
         renames VSS.Strings.Conversions.To_Unbounded_UTF_8_String;

      Invalid_Rule_Error_Message : constant Virtual_String :=
        "Error parsing the grammar rules for the syntax check";

      Input : constant Unbounded_String := +Request.params.Input;
      Rules : Grammar_Rule_Vector;

      Valid : Boolean := False;

   begin
      if Request.params.Rules.Length = 0 then
         --  We need at least one rule in order to validate the input

         raise Constraint_Error;
      end if;

      for Rule_Image of Request.params.Rules  loop
         --  A Constraint_Error can be raised here is an invalid rule is
         --  received in the request parameters.
         Rules.Append (Grammar_Rule'Value (+Rule_Image));
      end loop;

      --  The input cannot be empty and only needs to be valid against one of
      --  the rules.

      if Input /= "" then
         Valid := Validate_Syntax (Input, Rules);
      end if;

      if Valid then
         return Response : ALS_Check_Syntax_Response (Is_Error => False) do
            Response.result := (Is_Set => False);
         end return;

      else
         return Response : ALS_Check_Syntax_Response (Is_Error => False) do
            Response.result := (Is_Set => True, Value => "Invalid Syntax");
         end return;
      end if;

   exception
      when Constraint_Error =>
         return Response : ALS_Check_Syntax_Response (Is_Error => True) do
            Response.error :=
              (Is_Set => True,
               Value  => (code    => LSP.Errors.InvalidRequest,
                          message => Invalid_Rule_Error_Message,
                          data    => <>));
         end return;
   end On_ALS_Check_Syntax_Request;

   -----------
   -- Parse --
   -----------

   function Parse
     (Value : LSP.Types.Optional_LSP_Any)
      return Experimental_Client_Capabilities is
   begin
      return Result : Experimental_Client_Capabilities :=
               (Advanced_Refactorings => [others => False])
      do
         if Value.Is_Set then
            if Value.Value.
              Has_Field ("advanced_refactorings")
              and then Value.Value.Get ("advanced_refactorings").Kind in
                         GNATCOLL.JSON.JSON_Array_Type
            then
               declare
                  Advanced_Refactorings : constant GNATCOLL.JSON.JSON_Array :=
                    Value.Value.Get ("advanced_refactorings");
                  Advanced_Refactoring  :
                    LSP.Ada_Handlers.Advanced_Refactorings;
               begin
                  for Refactoring of Advanced_Refactorings loop
                     if Refactoring.Kind in GNATCOLL.JSON.JSON_String_Type then
                        begin
                           Advanced_Refactoring :=
                             LSP.Ada_Handlers.Advanced_Refactorings'Value
                               (GNATCOLL.JSON.Get (Refactoring));
                           Result.Advanced_Refactorings
                             (Advanced_Refactoring) := True;
                        exception
                           when others =>
                              null;
                        end;
                     end if;
                  end loop;
               end;
            end if;
         end if;
      end return;
   end Parse;

end LSP.Ada_Handlers;
