------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2018-2021, AdaCore                     --
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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Characters.Latin_1;
with Ada.Strings.Wide_Wide_Unbounded;
with Ada.Strings.UTF_Encoding;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;

with GNAT.OS_Lib; use GNAT.OS_Lib;
with GNAT.Strings;
with GNATCOLL.JSON;
with GNATCOLL.Utils;             use GNATCOLL.Utils;

with VSS.String_Vectors;
with VSS.Strings.Conversions;
with VSS.Unicode;

with LSP.Ada_Documents; use LSP.Ada_Documents;
with LSP.Ada_Completions;
with LSP.Ada_Contexts;  use LSP.Ada_Contexts;
with LSP.Ada_Handlers.Named_Parameters_Commands;
with LSP.Ada_Handlers.Refactor_Imports_Commands;
with LSP.Ada_Handlers.Refactor_Remove_Parameter;
with LSP.Ada_Handlers.Refactor_Move_Parameter;
with LSP.Ada_Handlers.Refactor_Change_Parameter_Mode;
with LSP.Ada_Handlers.Refactor_Suppress_Seperate;
with LSP.Ada_Project_Environments;
with LSP.Client_Side_File_Monitors;
with LSP.Commands;
with LSP.Common;       use LSP.Common;
with LSP.Ada_Handlers.File_Readers;
with LSP.Errors;
with LSP.Lal_Utils;    use LSP.Lal_Utils;
with LSP.Messages.Client_Requests;
with LSP.Messages.Server_Notifications;
with LSP.Servers.FS_Watch;
with LSP.Types;        use LSP.Types;

with Langkit_Support.Slocs;
with Langkit_Support.Text;

with Laltools.Common;
with Laltools.Refactor_Imports;
with Laltools.Refactor.Subprogram_Signature;
with Laltools.Refactor.Safe_Rename;
with Laltools.Refactor.Suppress_Separate;

with Libadalang.Analysis;
with Libadalang.Common;    use Libadalang.Common;
with Libadalang.Doc_Utils;
with Libadalang.Helpers;

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
   --  Whether to send notifications to the client for "imprecise"
   --  navigation operations.

   Is_Parent : constant LSP.Messages.AlsReferenceKind_Set :=
     (Is_Server_Side => True,
      As_Flags => (LSP.Messages.Parent => True, others => False));
   Is_Child : constant LSP.Messages.AlsReferenceKind_Set :=
     (Is_Server_Side => True,
      As_Flags => (LSP.Messages.Child => True, others => False));
   --  Convenient constants

   Line_Feed : constant Character := Ada.Characters.Latin_1.LF;

   function "+" (Text : Ada.Strings.UTF_Encoding.UTF_8_String)
     return LSP.Types.LSP_String renames
       LSP.Types.To_LSP_String;

   procedure Send_Imprecise_Xref_Message
     (Self     : access Message_Handler;
      URI      : LSP.Messages.DocumentUri;
      Position : LSP.Messages.Position;
      Msg_Type : LSP.Messages.MessageType);
   --  Send a message of the given Msg_Type to the LSP client to warn the user
   --  of a possible imprecise result while computing xrefs on the given
   --  node.

   procedure Imprecise_Resolve_Name
     (Self       : access Message_Handler;
      In_Context : Context_Access;
      Position   : LSP.Messages.TextDocumentPositionParams'Class;
      Definition : out Libadalang.Analysis.Defining_Name;
      Msg_Type   : LSP.Messages.MessageType := LSP.Messages.Log);
   --  If node at given Position is a name, then resolve it.
   --  Send a message in case of a possible imprecise result.
   --  See description of Msg_Type in Send_Imprecise_Xref_Message comments.

   procedure Show_Message
     (Self : access Message_Handler;
      Text : String;
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

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (LSP.Ada_Documents.Document, Internal_Document_Access);

   procedure Release_Project_Info (Self : access Message_Handler);
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

   procedure Load_Implicit_Project (Self : access Message_Handler);
   --  Load the implicit project

   procedure Load_Project
     (Self                : access Message_Handler;
      GPR                 : Virtual_File;
      Scenario            : LSP.Types.LSP_Any;
      Charset             : String;
      Relocate_Build_Tree : Virtual_File := No_File;
      Root_Dir            : Virtual_File := No_File);
   --  Attempt to load the given project file, with the scenario provided.
   --  This unloads all currently loaded project contexts.

   procedure Mark_Source_Files_For_Indexing (Self : access Message_Handler);
   --  Mark all sources in all projects for indexing. This factorizes code
   --  between Load_Project and Load_Implicit_Project.

   function URI_To_File
     (Self : Message_Handler'Class;
      URI  : LSP.Types.LSP_String) return LSP.Types.LSP_String;
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
            Document.Initialize (URI, VSS.Strings.Empty_Virtual_String);
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
      use type LSP.Ada_Documents.Document_Access;

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

   ---------------------------------
   -- Send_Imprecise_Xref_Message --
   ---------------------------------

   procedure Send_Imprecise_Xref_Message
     (Self     : access Message_Handler;
      URI      : LSP.Messages.DocumentUri;
      Position : LSP.Messages.Position;
      Msg_Type : LSP.Messages.MessageType)
   is
      File : constant GNATCOLL.VFS.Virtual_File := Self.To_File (URI);
   begin
      Self.Server.On_Show_Message
        ((Msg_Type,
         "Imprecise fallback used to compute cross-references on entity at:"
         & To_LSP_String
           (Line_Feed & "   " & File.Display_Base_Name)
         & To_LSP_String
           (Line_Feed & "   line:" & Position.line'Img)
         & To_LSP_String
           (Line_Feed & "   column:" & Position.character'Img)));
   end Send_Imprecise_Xref_Message;

   ----------------------------
   -- Imprecise_Resolve_Name --
   ----------------------------

   procedure Imprecise_Resolve_Name
     (Self       : access Message_Handler;
      In_Context : Context_Access;
      Position   : LSP.Messages.TextDocumentPositionParams'Class;
      Definition : out Libadalang.Analysis.Defining_Name;
      Msg_Type   : LSP.Messages.MessageType := LSP.Messages.Log)
   is
      use type Libadalang.Analysis.Name;

      Name_Node : constant Libadalang.Analysis.Name :=
        Laltools.Common.Get_Node_As_Name
          (In_Context.Get_Node_At
             (Get_Open_Document (Self, Position.textDocument.uri),
              Position));

      Imprecise  : Boolean;
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
         Self.Send_Imprecise_Xref_Message
           (URI      => Position.textDocument.uri,
            Position => Position.position,
            Msg_Type => Msg_Type);
      end if;
   end Imprecise_Resolve_Name;

   --------------------------
   -- Release_Project_Info --
   --------------------------

   procedure Release_Project_Info (Self : access Message_Handler) is
      use GNATCOLL.Projects;
   begin
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
   end Release_Project_Info;

   -------------
   -- Cleanup --
   -------------

   procedure Cleanup (Self : access Message_Handler) is
   begin
      if Self.File_Monitor.Assigned then
         Self.File_Monitor.Stop_Monitoring_Directories;
      end if;

      --  Cleanup documents
      for Document of Self.Open_Documents loop
         Unchecked_Free (Document);
      end loop;
      Self.Open_Documents.Clear;

      --  Cleanup contexts
      Self.Contexts.Cleanup;

      --  Cleanup project and environment
      Self.Release_Project_Info;
   end Cleanup;

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

   procedure Load_Implicit_Project (Self : access Message_Handler) is
      C    : constant Context_Access := new Context (Self.Trace);
      Attr : GNAT.Strings.String_List (1 .. 1);
      use GNATCOLL.Projects;

      Reader : LSP.Ada_Handlers.File_Readers.LSP_Reader_Interface (Self);
   begin
      --  Unload all the contexts
      Self.Contexts.Cleanup;

      Self.Trace.Trace ("Loading the implicit project");

      Self.Implicit_Project_Loaded := True;
      Self.Release_Project_Info;
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
      Attr := (1 => new String'("Ada"));
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
      Self.Trace.Trace ("Root : " & To_UTF_8_String
                        (+Self.Root.Display_Full_Name));

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

         Self.Load_Implicit_Project;
      elsif GPRs_Found = 1 then
         --  We have not found exactly one .gpr file: load the default
         --  project.
         Self.Trace.Trace ("Loading " & GPR.Display_Base_Name);
         Self.Load_Project (GPR, No_Any, "iso-8859-1");
      else
         --  We have found more than one project: warn the user!

         Self.Show_Message
           ("More than one .gpr found." & Line_Feed &
              "Note: you can configure a project " &
              " through the ada.projectFile setting.");
      end if;
   end Ensure_Project_Loaded;

   ------------------------
   -- Initialize_Request --
   ------------------------

   overriding function On_Initialize_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Initialize_Request)
      return LSP.Messages.Server_Responses.Initialize_Response
   is
      use all type LSP.Types.Optional_Boolean;
      Value    : LSP.Messages.InitializeParams renames Request.params;
      Code_Action : LSP.Messages.Optional_CodeActionClientCapabilities renames
        Value.capabilities.textDocument.codeAction;
      Has_Rename : LSP.Messages.Optional_RenameClientCapabilities renames
        Value.capabilities.textDocument.rename;
      Response : LSP.Messages.Server_Responses.Initialize_Response
        (Is_Error => False);
      Root     : LSP.Types.LSP_String;
   begin
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
      Response.result.capabilities.callHierarchyProvider :=
        (Is_Set => True,
         Value  => (Is_Boolean => False, Options => <>));
      Response.result.capabilities.documentHighlightProvider :=
        (Is_Set => True,
         Value => (workDoneProgress => LSP.Types.None));

      --  lalpp does not support range formatting for now
      --  do not set the option
      --
      --  Response.result.capabilities.documentRangeFormattingProvider :=
      --    (Is_Set => True,
      --     Value  => (workDoneProgress => LSP.Types.None));

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
         (triggerCharacters   => (True, Empty_Vector & (+",") & (+"(")),
          retriggerCharacters => (True, Empty_Vector & (+" ")),
          workDoneProgress    => LSP.Types.None));
      Response.result.capabilities.completionProvider :=
        (True,
         (resolveProvider     => LSP.Types.False,
          triggerCharacters   => (True, Empty_Vector & (+".") & (+"(")),
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
         Response.result.capabilities.codeActionProvider :=
           (Is_Set => True,
            Value  =>
              (codeActionKinds =>
                   (Is_Set => True,
                    Value  => LSP.Messages.To_Set
                      (From => LSP.Messages.RefactorRewrite,
                       To   => LSP.Messages.RefactorRewrite)),
               workDoneProgress => LSP.Types.None,
               resolveProvider  => LSP.Types.None));
      else
         Response.result.capabilities.codeActionProvider :=
           (Is_Set => True, Value => <>);
      end if;

      Response.result.capabilities.alsShowDepsProvider := True;

      Response.result.capabilities.alsReferenceKinds :=
        (Is_Set => True,
         Value  => (Is_Server_Side => True, As_Flags => (others => True)));

      Response.result.capabilities.foldingRangeProvider :=
        (Is_Set => True,
         Value => (Is_Boolean => True, Bool => True));

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

      if Value.capabilities.workspace.didChangeWatchedFiles
           .dynamicRegistration = True
      then
         Self.File_Monitor := new LSP.Client_Side_File_Monitors.File_Monitor
           (Self.Server);
      end if;

      if Value.rootUri.Is_Set
        and then not LSP.Types.Is_Empty (Value.rootUri.Value)
      then
         Root := Self.URI_To_File (Value.rootUri.Value);
      elsif Value.rootPath.Is_Set and then Value.rootPath.Value.Is_Set then
         --  URI isn't provided, rollback to deprecated rootPath
         Root := Value.rootPath.Value.Value;
      end if;

      --  Some clients - notably VS Code as of version 33, when opening a file
      --  rather than a workspace - don't provide a root at all. In that case
      --  use the current directory as root.

      if LSP.Types.Is_Empty (Root) then
         Root := +".";
      end if;

      Self.Root := Create_From_UTF8 (To_UTF_8_String (Root));
      Self.Client := Value;

      --  Log the context root
      Self.Trace.Trace ("Context root: " & To_UTF_8_String (Root));

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
      pragma Unreferenced (Self, Request);
   begin
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
         Found   : in out Boolean;
         Done    : in out Boolean);
      --  Look for a possible refactoring in given Node.
      --  Return Found = True if some refactoring is possible. Populate
      --  Result with Code_Actions in this case. Return Done = True if futher
      --  analysis has no sense.

      Found_Named_Parameters : Boolean := False;
      --  We propose only one choice of Named_Parameters refactoring per
      --  request. So, if a user clicks on `1` in `A (B (1))` we propose the
      --  refactoring for B (1), but not for A (...) call. We consider this
      --  as better user experience.
      --
      --  This boolean filter to detect such refactoring duplication.

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
                    or else not (TD.Kind in Ada_Type_Decl_Range)
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
         Found   : in out Boolean;
         Done    : in out Boolean)
      is
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
            if Found_Named_Parameters then
               return;
            end if;

            Command.Initialize
              (Context => Context.all,
               Where   => ((uri => Where.uri), Where.span.first));

            Pointer.Set (Command);

            Item :=
              (title       => +"Name parameters in the call",
               kind        => (Is_Set => True,
                               Value  => LSP.Messages.RefactorRewrite),
               diagnostics => (Is_Set => False),
               disabled    => (Is_Set => False),
               edit        => (Is_Set => False),
               isPreferred => (Is_Set => False),
               command     => (Is_Set => True,
                               Value  =>
                                 (Is_Unknown => False,
                                  title      => +"",
                                  Custom     => Pointer)));

            Result.Append (Item);
            Found := True;
            Found_Named_Parameters := True;
         end Append_Command;

         Kind : constant Libadalang.Common.Ada_Node_Kind_Type := Node.Kind;

      begin
         case Kind is
            when Libadalang.Common.Ada_Stmt
               | Libadalang.Common.Ada_Basic_Decl =>

               Done := True;

            when Libadalang.Common.Ada_Basic_Assoc_List =>
               if Has_Assoc_Without_Designator (Node.As_Basic_Assoc_List) then
                  Append_Command (Node);
               end if;

            when Libadalang.Common.Ada_Call_Expr =>
               declare
                  List : constant Libadalang.Analysis.Ada_Node :=
                    Node.As_Call_Expr.F_Suffix;
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

            when Libadalang.Common.Ada_Identifier =>
               declare
                  Name      : constant Libadalang.Analysis.Name
                    := Laltools.Common.Get_Node_As_Name (Node);
                  Imprecise : Boolean;
                  use type Libadalang.Analysis.Name;
                  use type Libadalang.Analysis.Defining_Name;
               begin
                  --  Only suggest with clause / prefix for unresolved nodes

                  if Name /= Libadalang.Analysis.No_Name and then
                    Laltools.Common.Resolve_Name
                      (Name, Context.Trace, Imprecise)
                    = Libadalang.Analysis.No_Defining_Name
                  then
                     declare
                        Units_Vector : Libadalang.Helpers.Unit_Vectors.Vector;
                        Units_Array  : constant
                          Libadalang.Analysis.Analysis_Unit_Array
                            := Context.Analysis_Units;
                        Import_Suggestions : Laltools.Refactor_Imports.
                          Import_Suggestions_Vector.Vector;
                     begin

                        for U of Units_Array loop
                           Units_Vector.Append (U);
                        end loop;

                        --  Add runtime analysis units for this context

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

                        --  Get suggestions for all reachable declarations.
                        --  Each suggestion contains a with clause and a
                        --  prefix.

                        Import_Suggestions := Laltools.Refactor_Imports.
                          Get_Import_Suggestions
                            (Node.As_Identifier, Units_Vector);

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
                     end;
                  end if;
               end;

            when others =>
               null;

         end case;

         --  Refactoring Code Actions

         --  Remove Parameter
         declare
            use LSP.Ada_Handlers.Refactor_Remove_Parameter;
            use Libadalang.Analysis;
            use Laltools.Common;
            use Laltools.Refactor.Subprogram_Signature;

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
               Done := True;
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
               Done := True;
            end if;
         end;

         --  Change Parameter Mode
         declare
            use LSP.Ada_Handlers.Refactor_Change_Parameter_Mode;
            use Libadalang.Analysis;
            use Laltools.Common;
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
               Done := True;
            end if;
         end;

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
               Done := True;
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
         Done : Boolean := False;  --  True when futher analysis has no sense
         Node : Libadalang.Analysis.Ada_Node :=
           Document.Get_Node_At (Context.all, Params.span.first);
      begin
         while not Done and then not Node.Is_Null loop
            Analyse_Node (Context, Node, Result, Found, Done);
            Node := Node.Parent;
         end loop;
      end Analyse_In_Context;

      Document : constant LSP.Ada_Documents.Document_Access :=
        Get_Open_Document (Self, Params.textDocument.uri);

      Response : LSP.Messages.Server_Responses.CodeAction_Response
        (Is_Error => False);

      Found : Boolean := False;
   begin
      if Document = null then
         return Response;
      end if;

      --  Find any context where we can do some refactoring
      for C of Self.Contexts_For_URI (Params.textDocument.uri) loop
         Analyse_In_Context (C, Document, Response.result, Found);

         exit when Request.Canceled or else Found;
      end loop;

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
              Error    => (Is_Set => False),
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
         Decl_For_Find_Overrides : Basic_Decl := No_Basic_Decl;
      begin
         if Name_Node = No_Name then
            return;
         end if;

         --  Check if we are on some defining name
         Definition := Laltools.Common.Get_Name_As_Defining (Name_Node);

         if Definition = No_Defining_Name then
            Self.Imprecise_Resolve_Name
              (C, Value, Definition, LSP.Messages.Info);

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

            Decl_For_Find_Overrides := Definition.P_Basic_Decl;

            --  Search for overriding subprograms only if we are on an
            --  abstract subprogram.
            if Display_Method_Ancestry_Policy = Never
              or else
                (Display_Method_Ancestry_Policy = Usage_And_Abstract_Only
                        and then Decl_For_Find_Overrides.Kind
                        not in Ada_Abstract_Subp_Decl_Range)
            then
               Decl_For_Find_Overrides := No_Basic_Decl;
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
      Diag     : LSP.Messages.PublishDiagnosticsParams;
      Diags_Already_Published : Boolean := False;
   begin
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

         --  Emit diagnostics - do this for only one context
         if Self.Diagnostics_Enabled
           and then not Diags_Already_Published
         then
            Document.Get_Errors (Context.all, Diag.diagnostics);
            Diag.uri := Value.textDocument.uri;
            Self.Server.On_Publish_Diagnostics (Diag);
            Diags_Already_Published := True;
         end if;
      end loop;
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

         Unchecked_Free (Document);

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
   begin
      Self.Trace.Trace ("In Text_Document_Did_Open");
      Self.Trace.Trace ("Uri : " & To_UTF_8_String (URI));

      --  Some clients don't properly call initialize, or don't pass the
      --  project to didChangeConfiguration: fallback here on loading a
      --  project in this directory, if needed.
      Self.Ensure_Project_Loaded (URI);

      --  We have received a document: add it to the documents container
      Object.Initialize
        (URI, LSP.Types.To_Virtual_String (Value.textDocument.text));
      Self.Open_Documents.Insert (File, Object);

      --  Handle the case where we're loading the implicit project: do
      --  we need to add the directory in which the document is open?

      if Self.Implicit_Project_Loaded then
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
      declare
         Diag : LSP.Messages.PublishDiagnosticsParams;
         Diags_Already_Published : Boolean := False;
      begin
         for Context of Self.Contexts_For_URI (URI) loop
            Context.Index_Document (Object.all);

            if Self.Diagnostics_Enabled
              and then not Diags_Already_Published
            then
               Object.Get_Errors (Context.all, Diag.diagnostics);
               Diag.uri := URI;
               Self.Server.On_Publish_Diagnostics (Diag);

               --  Publish diagnostics only for one context,
               --  to avoid emitting too much noise.
               Diags_Already_Published := True;
            end if;
         end loop;
      end;

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

   begin
      if Document /= null then
         Document.Get_Folding_Blocks
           (Context.all,
            Self.Line_Folding_Only,
            Self.Options.Folding.Comments,
            Result);

         return Response : LSP.Messages.Server_Responses.FoldingRange_Response
           (Is_Error => False)
         do
            Response.result := Result;
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
      Imprecise  : Boolean := False;
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
         pragma Unreferenced (Cancel);
      begin
         Imprecise := Imprecise or Kind = Libadalang.Common.Imprecise;

         if not Laltools.Common.Is_End_Label (Node.As_Ada_Node) then
            Append_Location
              (Result => Response.Result,
               Node   => Node,
               Kind   => Get_Highlight_Kind (Node.As_Ada_Node),
               File   => File);
         end if;

      end Callback;
   begin

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
        (Result => Response.result,
         Node   => Definition,
         Kind   => Get_Highlight_Kind (Definition.As_Ada_Node),
         File   => File);

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
      Decl_Text          : VSS.Strings.Virtual_String;
      Comments_Text      : VSS.Strings.Virtual_String;
      Location_Text      : VSS.Strings.Virtual_String;

      C : constant Context_Access :=
        Self.Contexts.Get_Best_Context (Value.textDocument.uri);
      --  For the Hover request, we're only interested in the "best"
      --  response value, not in the list of values for all contexts

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

      --  If the basic declaration is an enum literal, display the whole
      --  enumeration type declaration instead.
      if Decl.Kind in Ada_Enum_Literal_Decl then
         Decl := As_Enum_Literal_Decl (Decl).P_Enum_Type.As_Basic_Decl;
         Decl_Text := Get_Hover_Text (Decl);
      else
         Decl_Text := Get_Hover_Text (Decl);
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
            language  => +"ada"));

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

      Comments_Text :=
        VSS.Strings.To_Virtual_String
           (Libadalang.Doc_Utils.Get_Documentation (Decl).Doc.To_String);

      if not Comments_Text.Is_Empty then
         Response.result.Value.contents.Vector.Append
           (LSP.Messages.MarkedString'
              (Is_String => True,
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
        (others => False);

      procedure Process_Context (C : Context_Access);
      --  Process the references found in one context and append
      --  them to Response.results.

      function Get_Reference_Kind
        (Node : Ada_Node) return LSP.Messages.AlsReferenceKind_Set;
      --  Fetch reference kind for given node

      ------------------------
      -- Get_Reference_Kind --
      ------------------------

      function Get_Reference_Kind
        (Node : Ada_Node) return LSP.Messages.AlsReferenceKind_Set
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
         end;

         C.Find_All_References (Definition, Callback'Access);

         if Value.context.includeDeclaration then
            Append_Location
              (Response.result,
               Definition,
               Get_Reference_Kind (Definition.As_Ada_Node));
         end if;
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

   ----------------------------------
   -- On_ALS_Show_Dependencies_Request --
   ----------------------------------

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

      Value   : LSP.Messages.SignatureHelpParams renames
        Request.params;
      Response : LSP.Messages.Server_Responses.SignatureHelp_Response
        (Is_Error => False);

      C : constant Context_Access :=
        Self.Contexts.Get_Best_Context (Value.textDocument.uri);

      Node : constant Libadalang.Analysis.Ada_Node :=
        C.Get_Node_At
          (Get_Open_Document (Self, Value.textDocument.uri),
           Value);
      Name_Node       : Libadalang.Analysis.Name;
      Designator      : Libadalang.Analysis.Ada_Node;
      Active_Position : LSP.Types.LSP_Number;

      procedure Add_Signature (Decl_Node : Libadalang.Analysis.Basic_Decl);

      -------------------
      -- Add_Signature --
      -------------------

      procedure Add_Signature (Decl_Node : Libadalang.Analysis.Basic_Decl)
      is
         Param_Index : constant LSP.Types.LSP_Number :=
           Get_Active_Parameter (Decl_Node, Designator, Active_Position);
      begin
         if Param_Index = -1 then
            return;
         end if;

         declare
            Signature : LSP.Messages.SignatureInformation :=
              (label          =>
                 LSP.Types.To_LSP_String (Get_Hover_Text (Decl_Node)),
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
                  Value  => Param_Index
                 ),
               others          => <>
              );
         begin
            Get_Parameters (Decl_Node, Signature.parameters);
            Response.result.signatures.Append (Signature);
         end;
      end Add_Signature;

   begin
      Response.result := (others => <>);

      --  Check if we are inside a function call and get the caller name
      Get_Call_Expr_Name
        (Node            => Node,
         Cursor_Line     =>
           Langkit_Support.Slocs.Line_Number (Value.position.line + 1),
         Cursor_Column   =>
           Langkit_Support.Slocs.Column_Number (Value.position.character) + 1,
         Active_Position => Active_Position,
         Designator      => Designator,
         Name_Node       => Name_Node);

      if Name_Node = Libadalang.Analysis.No_Name then
         return Response;
      end if;

      for N of C.Find_All_Env_Elements (Name_Node) loop
         if N.Kind in Ada_Subp_Decl_Range then
            Add_Signature (N.As_Basic_Decl);
         end if;
      end loop;

      --  Set the active values to default
      Response.result.activeSignature := (Is_Set => True,
                                          Value  => 0);
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
   begin
      if Document = null then
         declare
            Document : LSP.Ada_Documents.Document_Access :=
              Get_Open_Document (Self, Value.textDocument.uri, Force => True);
         begin
            Self.Get_Symbols (Document.all, Context.all, Result.result);
            Unchecked_Free (Internal_Document_Access (Document));
         end;
      else
         Self.Get_Symbols (Document.all, Context.all, Result.result);
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
      Response  : LSP.Messages.Server_Responses.Rename_Response
        (Is_Error => False);
      --  If a rename problem is found when Process_Context is called,
      --  then Refs.Problems will not be empty. This Response will be discarded
      --  and a new response with an error meessage is returned instead.
      --  If no problems are found, then this Response will contain all the
      --  references to be renamed and is returned by this function.

      Document  : constant LSP.Ada_Documents.Document_Access :=
                   Get_Open_Document (Self, Value.textDocument.uri);

      Refs      : Laltools.Refactor.Safe_Rename.Renamable_References;

      procedure Process_Context (C : Context_Access);
      --  Process the rename request for the given context, and add the
      --  results to Response.

      ---------------------
      -- Process_Context --
      ---------------------

      procedure Process_Context (C : Context_Access) is
         Position   : constant LSP.Messages.TextDocumentPositionParams :=
           (Value.textDocument, Value.position);

         Node      : Ada_Node := C.Get_Node_At (Document, Position);
         Name_Node : Name := Laltools.Common.Get_Node_As_Name
           (C.Get_Node_At (Document, Position));

         Empty     : LSP.Messages.TextEdit_Vector;

         procedure Process_Comments
           (Node : Ada_Node;
            Uri  : LSP.Messages.DocumentUri);
         --  Iterate over all comments and include them in the response when
         --  they contain a renamed word.

         procedure Process_Reference
           (Unit       : Libadalang.Analysis.Analysis_Unit;
            Sloc_Range : Langkit_Support.Slocs.Source_Location_Range);
         --  Add a reference to Response if it has not been added yet

         -----------------------
         -- Process_Reference --
         -----------------------

         procedure Process_Reference
           (Unit       : Libadalang.Analysis.Analysis_Unit;
            Sloc_Range : Langkit_Support.Slocs.Source_Location_Range)
         is
            Location : constant LSP.Messages.Location :=
              LSP.Lal_Utils.Get_Location (Unit, Sloc_Range);
            Item     : constant LSP.Messages.TextEdit :=
              (span    => Location.span,
               newText => Value.newName);
         begin
            if not Response.result.changes.Contains (Location.uri) then
               --  We haven't touched this document yet, create an empty
               --  change list
               Response.result.changes.Insert (Location.uri, Empty);

               --  Process comments if it is needed

               if Self.Options.Refactoring.Renaming.In_Comments then
                  Process_Comments
                    (Unit.Root.Lookup
                       ((Sloc_Range.Start_Line, Sloc_Range.Start_Column)),
                     Location.uri);
               end if;
            end if;

            --  When iterating over all contexts (and therefore all
            --  projects), it's possible to encounter the same
            --  definitions more than once, so verify that the result
            --  is not already recorded before adding it.

            if not Response.result.changes
              (Location.uri).Contains (Item)
            then
               Response.result.changes (Location.uri).Append (Item);
            end if;
         end Process_Reference;

         -----------------------
         --  Process_Comments --
         -----------------------

         procedure Process_Comments
           (Node : Ada_Node;
            Uri  : LSP.Messages.DocumentUri)
         is
            Token     : Token_Reference := First_Token (Node.Unit);
            Name      : constant Wide_Wide_String :=
              Ada.Strings.Wide_Wide_Unbounded.To_Wide_Wide_String
                (Laltools.Common.Get_Last_Name (Name_Node));
            Text_Edit : LSP.Messages.TextEdit;
            Span      : Langkit_Support.Slocs.Source_Location_Range;
            Current   : Token_Reference;
            Diff      : Integer;

            Box_Line : constant Wide_String (1 .. 80) := (others => '-');

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
                              Sloc : constant Source_Location_Range :=
                                Sloc_Range (Data (Current));

                              Line  : constant LSP.Types.Line_Number :=
                                LSP.Types.Line_Number (Sloc.Start_Line) - 1;
                              Start : constant UTF_16_Index := UTF_16_Index
                                (Sloc.End_Column - 1);
                           begin
                              Response.result.changes (Uri).Append
                                (LSP.Messages.TextEdit'
                                   (span =>
                                        (first => (Line, Start),
                                         last  => (Line, Start)),
                                    newText =>
                                      LSP.Types.To_Unbounded_Wide_String
                                        (Box_Line (1 .. Diff))));
                           end;

                        else
                           --  Decrease '-', Diff is negative
                           declare
                              use type VSS.Unicode.UTF16_Code_Unit_Count;

                              Sloc : constant Source_Location_Range :=
                                Sloc_Range (Data (Current));

                              Line  : constant LSP.Types.Line_Number :=
                                LSP.Types.Line_Number (Sloc.Start_Line) - 1;
                              Last : constant UTF_16_Index := UTF_16_Index
                                (Sloc.End_Column - 1);
                           begin
                              Response.result.changes (Uri).Append
                                (LSP.Messages.TextEdit'
                                   (span =>
                                        (first =>
                                             (Line, Last - UTF_16_Index
                                                (abs Diff)),
                                         last  =>
                                           (Line, Last)),
                                    newText =>
                                      LSP.Types.To_Unbounded_Wide_String
                                        ("")));
                           end;
                        end if;

                        return False;
                     end;

                  when others =>
                     return False;
               end case;
            end Process_Box;

         begin
            Diff := Length (Value.newName) - Name'Length;

            while Token /= No_Token loop
               declare
                  This_Span : Langkit_Support.Slocs.Source_Location_Range;
               begin
                  if Kind (Data (Token)) = Ada_Comment
                    and then Laltools.Common.Contains
                      (Token, Name, True, This_Span)
                  then
                     Text_Edit.span := To_Span (This_Span);
                     Text_Edit.newText := Value.newName;

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
                        Response.result.changes (Uri).Append (Text_Edit);

                        Current := Next (Token);
                        loop
                           --  Looking for the box footer
                           exit when not Process_Box;
                           Current := Next (Current);
                        end loop;
                     else
                        Response.result.changes (Uri).Append (Text_Edit);
                     end if;
                  end if;
               end;

               Token := Next (Token);
            end loop;
         end Process_Comments;

         use Laltools.Refactor.Safe_Rename;

         Unit_Cursor : Unit_Slocs_Maps.Cursor;
         Sloc_Cursor : Slocs_Maps.Cursor;

         use Unit_Slocs_Maps;
         use Slocs_Maps;
      begin

         Refs := Find_All_Renamable_References
           (Node           => Node,
            New_Name       => To_Unbounded_Text_Type (Value.newName),
            Units          => C.Analysis_Units,
            Algorithm_Kind => Analyse_AST);

         --  Call to Find_All_Renamable_References above reparses all analysis
         --  units, therefore, Node and Name_Node need to be recomputed.

         Node := C.Get_Node_At (Document, Position);
         Name_Node := Laltools.Common.Get_Node_As_Name
           (C.Get_Node_At (Document, Position));

         --  If problems were found, do not continue processing references.
         if not Refs.Problems.Is_Empty then
            return;
         end if;

         Unit_Cursor := Refs.References.First;
         while Has_Element (Unit_Cursor) loop
            Sloc_Cursor :=
              Refs.References.Constant_Reference (Unit_Cursor).First;
            while Has_Element (Sloc_Cursor) loop
               for Sloc of
                 Refs.References.Constant_Reference (Unit_Cursor).
                 Constant_Reference (Sloc_Cursor)
               loop
                  Process_Reference (Key (Unit_Cursor), Sloc);
               end loop;
               Next (Sloc_Cursor);
            end loop;
            Next (Unit_Cursor);
         end loop;
      end Process_Context;

   begin
      for C of Self.Contexts_For_URI (Value.textDocument.uri) loop
         Process_Context (C);

         --  If problems were found, send an error message and do not proceed
         --  with the renames.

         if not Refs.Problems.Is_Empty then
            return Response : LSP.Messages.Server_Responses.Rename_Response
              (Is_Error => True)
            do
               declare
                  Error_Message : VSS.Strings.Virtual_String;

               begin
                  for Problem of Refs.Problems loop
                     Error_Message.Append
                       (VSS.Strings.Conversions.To_Virtual_String
                          (Problem.Info & ASCII.LF));
                  end loop;

                  Response.error :=
                    (True,
                     (code    => LSP.Errors.InvalidRequest,
                      message => Error_Message,
                      data    => Empty));
               end;
            end return;
         end if;

         exit when Request.Canceled;
      end loop;

      return Response;
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

      Ada       : constant LSP.Types.LSP_Any := Value.settings.Get ("ada");
      File      : LSP.Types.LSP_String;
      Charset   : Unbounded_String;
      Variables : LSP.Types.LSP_Any;
      Relocate  : Virtual_File := No_File;
      Root      : Virtual_File := No_File;
   begin
      if Ada.Kind = GNATCOLL.JSON.JSON_Object_Type then
         if Ada.Has_Field (relocateBuildTree) then
            Relocate := Create_From_UTF8
              (To_UTF_8_String (+Ada.Get (relocateBuildTree).Get));
         end if;

         if Ada.Has_Field (rootDir) then
            Root := Create_From_UTF8
              (To_UTF_8_String (+Ada.Get (rootDir).Get));
         end if;

         if Ada.Has_Field (projectFile) then
            File := +Ada.Get (projectFile).Get;

            --  Drop uri scheme if present
            if LSP.Types.Starts_With (File, "file:") then
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
      end if;

      if File /= Empty_LSP_String then
         --  The projectFile may be either an absolute path or a
         --  relative path; if so, we're assuming it's relative
         --  to Self.Root.
         declare
            Project_File : constant String := To_UTF_8_String (File);
            GPR : Virtual_File;
         begin
            if Is_Absolute_Path (Project_File) then
               GPR := Create_From_UTF8 (Project_File);
            else
               GPR := Join (Self.Root, Create_From_UTF8 (Project_File));
            end if;

            Self.Load_Project
              (GPR, Variables, To_String (Charset), Relocate, Root);
         end;
      end if;

      --  Register rangeFormatting provider is the client supports
      --  dynamic registration for it (and we haven't done it before).
      if not Self.Range_Formatting_Enabled
        and then Self.Client.capabilities.textDocument.rangeFormatting
                   .dynamicRegistration = True
      then
         declare
            Request : LSP.Messages.Client_Requests.RegisterCapability_Request;
            Registration : LSP.Messages.Registration;
            Selector     : LSP.Messages.DocumentSelector;
            Filter       : constant LSP.Messages.DocumentFilter :=
              (language => (True, +"ada"),
               others   => <>);
         begin
            Selector.Append (Filter);
            Registration.method := +"textDocument/rangeFormatting";
            Registration.registerOptions :=
              (LSP.Types.Text_Document_Registration_Option,
               (documentSelector => Selector));
            Registration.id := +"rf";
            Request.params.registrations.Append (Registration);
            Self.Server.On_RegisterCapability_Request (Request);
            Self.Range_Formatting_Enabled := True;
         end;
      end if;

      Self.Ensure_Project_Loaded;
   end On_DidChangeConfiguration_Notification;

   -------------------------------------------
   -- On_DidChangeWatchedFiles_Notification --
   -------------------------------------------

   overriding procedure On_DidChangeWatchedFiles_Notification
     (Self  : access Message_Handler;
      Value : LSP.Messages.DidChangeWatchedFilesParams)
   is
      Document : LSP.Ada_Documents.Document_Access;
      File     : GNATCOLL.VFS.Virtual_File;
   begin
      --  Look through each changes
      for Change of Value.changes loop
         --  A watched file has changed on disk. If there is an open
         --  document for this file, nothing to do: the document takes
         --  precedence over the filesystem.

         Document := Self.Get_Open_Document (Change.uri);

         if Document = null then
            --  If there is no document, reindex the file for each
            --  context where it is relevant.
            File := Self.To_File (Change.uri);

            for C of Self.Contexts_For_File (File) loop
               C.Index_File (File);
            end loop;
         end if;
      end loop;
   end On_DidChangeWatchedFiles_Notification;

   ------------------------------------
   -- Mark_Source_Files_For_Indexing --
   ------------------------------------

   procedure Mark_Source_Files_For_Indexing (Self : access Message_Handler) is
   begin
      Self.Files_To_Index.Clear;
      for C of Self.Contexts.Each_Context loop
         for F in C.List_Files loop
            Self.Files_To_Index.Include
              (LSP.Ada_File_Sets.File_Sets.Element (F));
         end loop;
      end loop;
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
      begin
         C.Initialize (Reader, Self.Follow_Symlinks);
         C.Load_Project (Tree    => Self.Project_Tree,
                         Root    => P,
                         Charset => Charset);
         Self.Contexts.Prepend (C);
      end Create_Context_For_Non_Aggregate;

   begin
      --  Unload all the contexts
      Self.Contexts.Cleanup;

      --  Unload the project tree and the project environment
      Self.Release_Project_Info;

      --  We're loading an actual project
      Self.Implicit_Project_Loaded := False;

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
            Errors => On_Error'Unrestricted_Access);
         for File of Self.Project_Environment.Predefined_Source_Files loop
            Self.Project_Predefined_Sources.Include (File);
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
            Self.Release_Project_Info;

            Self.Trace.Trace (E);
            Errors.a_type := LSP.Messages.Error;

            LSP.Types.Append
              (Errors.message,
               LSP.Types.To_LSP_String
                 ("Unable to load project file: " &
                  (+GPR.Full_Name.all) & Line_Feed));

            --  The project was invalid: fallback on loading the implicit
            --  project.
            Self.Load_Implicit_Project;
      end;

      --  Report the errors, if any
      if not Error_Text.Is_Empty then
         for Line of Error_Text loop
            LSP.Types.Append (Errors.message, LSP.Types.To_LSP_String (Line));
         end loop;
         Self.Server.On_Show_Message (Errors);
      end if;

      --  TODO: doc
      for F in Self.Project_Predefined_Sources.Iterate loop
         declare
            File : GNATCOLL.VFS.Virtual_File renames
              LSP.Ada_File_Sets.File_Sets.Element (F);
         begin
            for Context of Self.Contexts_For_File (File) loop
               Context.Index_File
                 (File    => File,
                  Reparse => True,
                  PLE     => True);
            end loop;
         end;
      end loop;

      --  Reindex all open documents immediately after project reload, so
      --  that navigation from editors is accurate.
      for Document of Self.Open_Documents loop
         for Context of Self.Contexts_For_URI (Document.URI) loop
            Context.Index_Document (Document.all);
         end loop;
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
      procedure Emit_Progress_Report (Percent : Natural);
      procedure Emit_Progress_End;
      --  Emit a message to inform that the indexing has begun / is in
      --  progress / has finished.

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
         P.Begin_Param.value.title := +"Indexing";
         P.Begin_Param.value.percentage := (Is_Set => True, Value => 0);
         Self.Server.On_Progress (P);
      end Emit_Progress_Begin;

      --------------------------
      -- Emit_Progress_Report --
      --------------------------

      procedure Emit_Progress_Report (Percent : Natural) is
         P : LSP.Messages.Progress_Params (LSP.Messages.Progress_Report);
      begin
         P.Report_Param.token := Self.Indexing_Token;
         P.Report_Param.value.percentage :=
           (Is_Set => True, Value => LSP_Number (Percent));
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

      Last_Percent    : Natural := 0;
      Current_Percent : Natural := 0;
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
               Current_Percent := (Self.Total_Files_Indexed * 100)
                 / Self.Total_Files_To_Index;
               --  If the value of the indexing increased by at least one
               --  percent, emit one progress report.
               if Current_Percent > Last_Percent then
                  Emit_Progress_Report (Current_Percent);
                  Last_Percent := Current_Percent;
               end if;

               for Context of Self.Contexts_For_File (File) loop
                  --  Set Reparse to False to avoid issues with LAL envs
                  --  for now (see T226-048 for more info).
                  Context.Index_File (File, Reparse => False);
               end loop;

               --  Check whether another request is pending. If so, pause
               --  the indexing; it will be resumed later as part of
               --  After_Request.
               if Self.Server.Has_Pending_Work then
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
              Error    => (Is_Set => False),
              others   => <>);
   end On_Workspace_Execute_Command_Request;

   ----------------------------------
   -- On_Workspace_Symbols_Request --
   ----------------------------------

   overriding function On_Workspace_Symbols_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Workspace_Symbols_Request)
      return LSP.Messages.Server_Responses.Symbol_Response
   is
      procedure On_Inaccessible_Name
        (File : GNATCOLL.VFS.Virtual_File;
         Name : Libadalang.Analysis.Defining_Name;
         Stop : in out Boolean);

      function Has_Been_Canceled return Boolean;

      procedure Write_Symbols is
        new LSP.Ada_Completions.Write_Symbols (Has_Been_Canceled);

      Count : Cancel_Countdown := 0;
      Names : LSP.Ada_Completions.Completion_Maps.Map;

      -----------------------
      -- Has_Been_Canceled --
      -----------------------

      function Has_Been_Canceled return Boolean is
      begin
         Count := Count - 1;
         return Count = 0  and then Request.Canceled;
      end Has_Been_Canceled;

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
                Use_Snippets => False));

            Stop := Has_Been_Canceled;
         end if;
      end On_Inaccessible_Name;

      Query : constant VSS.Strings.Virtual_String :=
        Canonicalize (LSP.Types.To_Virtual_String (Request.params.query));
      Response : LSP.Messages.Server_Responses.Symbol_Response
        (Is_Error => False);

   begin
      for Context of Self.Contexts.Each_Context loop
         Context.Get_Any_Symbol_Completion
           (Prefix   => Query,
            Callback => On_Inaccessible_Name'Access);

         exit when Request.Canceled;
      end loop;

      for Doc of Self.Open_Documents loop
         declare
            Context : constant Context_Access :=
              Self.Contexts.Get_Best_Context (Doc.URI);
         begin
            Doc.Get_Any_Symbol_Completion
              (Context.all, Query, Ada.Containers.Count_Type'Last, Names);
         end;
      end loop;

      Write_Symbols (Names, Response.result);

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
      use type Ada.Containers.Count_Type;

      --  We're completing only based on one context, ie one project
      --  tree: this seems reasonable. One further refinement could
      --  be to return only results that are available for all
      --  project contexts.

      procedure On_Inaccessible_Name
        (File : GNATCOLL.VFS.Virtual_File;
         Name : Libadalang.Analysis.Defining_Name;
         Stop : in out Boolean) with Unreferenced;

      Value    : LSP.Messages.TextDocumentPositionParams renames
        Request.params;

      Context  : constant Context_Access :=
        Self.Contexts.Get_Best_Context (Value.textDocument.uri);

      Snippets_Enabled : constant Boolean := Self.Completion_Snippets_Enabled;

      Names     : LSP.Ada_Completions.Completion_Maps.Map;
      Use_Names : Boolean := False;

      Limit : constant := 10;

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
                Use_Snippets => False));

            Stop := Names.Length >= Limit;
         end if;
      end On_Inaccessible_Name;

      Document : constant LSP.Ada_Documents.Document_Access :=
        Get_Open_Document (Self, Value.textDocument.uri);
      Response : LSP.Messages.Server_Responses.Completion_Response
        (Is_Error => False);
   begin

      Document.Get_Completions_At
        (Context                  => Context.all,
         Position                 => Value.position,
         Named_Notation_Threshold => Self.Named_Notation_Threshold,
         Snippets_Enabled         => Snippets_Enabled,
         Should_Use_Names         => Use_Names,
         Names                    => Names,
         Result                   => Response.result);

      --  We are not expecting a defining name: it means that we are completing
      --  a pragma, an aspect or an attribute. In this case, we don't want to
      --  search for invisible symbols since it's costly so return immediately.
      if not Use_Names then
         return Response;
      end if;

      --  declare
      --     Word : constant VSS.Strings.Virtual_String := Document.Get_Word_At
      --       (Context.all, Value.position);
      --
      --     Canonical_Prefix : constant VSS.Strings.Virtual_String :=
      --       Canonicalize (Word);
      --
      --  begin
      --     if not Word.Is_Empty then
      --        Context.Get_Any_Symbol_Completion
      --          (Prefix   => Canonical_Prefix,
      --           Callback => On_Inaccessible_Name'Access);
      --
      --        for Doc of Self.Open_Documents loop
      --           Doc.Get_Any_Symbol_Completion
      --             (Context.all, Canonical_Prefix, Limit, Names);
      --        end loop;
      --     end if;
      --  end;

      LSP.Ada_Completions.Write_Completions
        (Context                  => Context.all,
         Names                    => Names,
         Named_Notation_Threshold => Self.Named_Notation_Threshold,
         Result                   => Response.result.items);

      Response.result.isIncomplete := Names.Length >= Limit;

      return Response;
   end On_Completion_Request;

   ---------------------------
   -- On_Formatting_Request --
   ---------------------------

   overriding function On_Formatting_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Formatting_Request)
      return LSP.Messages.Server_Responses.Formatting_Response
   is
      Success  : Boolean;
      Context  : constant Context_Access :=
        Self.Contexts.Get_Best_Context (Request.params.textDocument.uri);

      Document : constant LSP.Ada_Documents.Document_Access :=
        Get_Open_Document (Self, Request.params.textDocument.uri);

      Response : LSP.Messages.Server_Responses.Formatting_Response
        (Is_Error => False);
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

      Context.Format
        (Document,
         LSP.Messages.Empty_Span,
         Request.params.options,
         Response.result,
         Success);

      if not Success then
         return Response : LSP.Messages.Server_Responses.Formatting_Response
           (Is_Error => True)
         do
            Response.error :=
              (True,
               (code    => LSP.Errors.InternalError,
                message => "Internal error",
                data    => <>));
         end return;
      end if;

      return Response;
   end On_Formatting_Request;

   ---------------------------------------
   -- On_Prepare_Call_Hierarchy_Request --
   ---------------------------------------

   overriding function On_Prepare_Call_Hierarchy_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Prepare_Call_Hierarchy_Request)
      return LSP.Messages.Server_Responses.PrepareCallHierarchy_Response
   is
      Value : LSP.Messages.CallHierarchyPrepareParams renames
        Request.params;

      Response : LSP.Messages.Server_Responses.PrepareCallHierarchy_Response
        (Is_Error => False);

      Imprecise : Boolean := False;

      C : constant Context_Access :=
        Self.Contexts.Get_Best_Context (Value.textDocument.uri);
      --  For the PrepareCallHierarchy request, we're only interested in the
      --  "best" response value, not in the list of values for all contexts

      Node : constant Libadalang.Analysis.Name :=
        Laltools.Common.Get_Node_As_Name
          (C.Get_Node_At
            (Get_Open_Document (Self, Value.textDocument.uri), Value));

      Name : constant Libadalang.Analysis.Defining_Name :=
        Laltools.Common.Resolve_Name
          (Node,
           Self.Trace,
           Imprecise);

   begin
      if Name.Is_Null then
         return Response;
      end if;

      Response.result.Append (To_Call_Hierarchy_Item (Name));

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
      Success  : Boolean;
      Context  : constant Context_Access :=
        Self.Contexts.Get_Best_Context (Request.params.textDocument.uri);

      Document : constant LSP.Ada_Documents.Document_Access :=
        Get_Open_Document (Self, Request.params.textDocument.uri);

      Response : LSP.Messages.Server_Responses.Range_Formatting_Response
        (Is_Error => False);
   begin
      if Document.Has_Diagnostics (Context.all) then
         return Response : LSP.Messages.Server_Responses.
           Range_Formatting_Response (Is_Error => True)
         do
            Response.error :=
              (True,
               (code    => LSP.Errors.InternalError,
                message => "Incorrect code can't be formatted",
                data    => <>));
         end return;
      end if;

      Context.Format
        (Document,
         Request.params.span,
         Request.params.options,
         Response.result,
         Success);

      if not Success then
         return Response : LSP.Messages.Server_Responses.
           Range_Formatting_Response (Is_Error => True)
         do
            Response.error :=
              (True,
               (code    => LSP.Errors.InternalError,
                message => "Internal error",
                data    => <>));
         end return;
      end if;

      return Response;
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

   ------------------
   -- Show_Message --
   ------------------

   procedure Show_Message
     (Self : access Message_Handler;
      Text : String;
      Mode : LSP.Messages.MessageType := LSP.Messages.Error) is
   begin
      Self.Server.On_Show_Message ((Mode, +Text));
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
            +("The result of '" & Operation & "' is approximate.")));
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

   function From_File
     (Self : Message_Handler'Class;
      File : Virtual_File) return LSP.Messages.DocumentUri is
        (LSP.Types.To_LSP_String
          (URIs.Conversions.From_File (File.Display_Full_Name)));

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
      URI  : LSP.Types.LSP_String) return LSP.Types.LSP_String
   is
      To     : constant URIs.URI_String := LSP.Types.To_UTF_8_String (URI);
      Result : constant String := URIs.Conversions.To_File
        (To, Normalize => Self.Follow_Symlinks);
   begin
      return LSP.Types.To_LSP_String (Result);
   end URI_To_File;

   -----------------
   -- File_To_URI --
   -----------------

   function File_To_URI
     (Self : Message_Handler'Class;
      File : LSP.Types.LSP_String) return LSP.Types.LSP_String
   is
      pragma Unreferenced (Self);
      Result : constant URIs.URI_String :=
        URIs.Conversions.From_File (LSP.Types.To_UTF_8_String (File));
   begin
      return LSP.Types.To_LSP_String (Result);
   end File_To_URI;

end LSP.Ada_Handlers;
