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
--
--  This package provides requests and notifications handler for Ada
--  language.

with Ada.Containers.Hashed_Maps;
with Ada.Containers.Hashed_Sets;
with Ada.Exceptions;

with GNATCOLL.Traces;
with GNATCOLL.VFS;

with GPR2.Build.Source.Sets;
with GPR2.Project.Tree;

with Libadalang.Analysis;
with Libadalang.Common;

with LSP.Enumerations;
with VSS.String_Vectors;
with VSS.Strings.Conversions;

with LSP.Ada_Client_Capabilities;
with LSP.Ada_Configurations;
with LSP.Ada_Context_Sets;
with LSP.Ada_Contexts;
with LSP.Ada_Documents;
with LSP.Ada_File_Sets;
with LSP.Ada_Highlighters;
with LSP.Ada_Job_Contexts;
with LSP.Ada_Project_Loading;
with LSP.Client_Message_Receivers;
with LSP.Constants;
with LSP.Diagnostic_Sources; use LSP.Diagnostic_Sources;
with LSP.File_Monitors;
with LSP.Locations;
with LSP.Server_Message_Visitors;
with LSP.Server_Notification_Receivers;
with LSP.Server_Notifications;
with LSP.Server_Request_Receivers;
with LSP.Server_Requests;
with LSP.Servers;
with LSP.Structures;
with LSP.Tracers;
with LSP.Unimplemented_Handlers;
with URIs;

private with LAL_Refactor;

package LSP.Ada_Handlers is

   type Message_Handler
     (Server : not null LSP.Servers.Server_Access;
      --  Please avoid to use this discriminant!
      Sender : not null access LSP.Client_Message_Receivers
        .Client_Message_Receiver'Class;
      Tracer : not null LSP.Tracers.Tracer_Access) is limited
   new LSP.Server_Message_Visitors.Server_Message_Visitor
     and LSP.Server_Request_Receivers.Server_Request_Receiver
     and LSP.Server_Notification_Receivers.Server_Notification_Receiver
     and LSP.Ada_Job_Contexts.Ada_Job_Context
   with private;

   procedure Initialize
     (Self                     : in out Message_Handler;
      Incremental_Text_Changes : Boolean;
      CLI_Config_File          : GNATCOLL.VFS.Virtual_File :=
        GNATCOLL.VFS.No_File);
   --  Initialize the message handler and configure it.
   --
   --  Incremental_Text_Changes - activate the support for incremental text
   --  changes.
   --
   --  CLI_Config_File - custom configuration file

   procedure Load_Config_Files
     (Self : in out Message_Handler;
      CLI_Config_File : GNATCOLL.VFS.Virtual_File);
   --  Read configuration files in the following order:
   --
   --  1. $XDG_CONFIG_HOME/als/config.json, if it exists
   --  2. .als.json in the current directory, if it exists
   --  3. The given CLI_Config_File, if it exists

   function Is_Shutdown (Self : Message_Handler'Class) return Boolean;
   --  Return True when shutdown has been requested.

   function Allocate_Progress_Token
     (Self      : in out Message_Handler'Class;
      Operation : VSS.Strings.Virtual_String)
      return LSP.Structures.ProgressToken;
   --  Return an unique token for indicating progress

   -----------------------------
   --  Open Document Manager  --
   -----------------------------

   overriding function Get_Open_Document
     (Self : in out Message_Handler;
      URI  : LSP.Structures.DocumentUri)
      return LSP.Ada_Documents.Document_Access;
   --  Return the open document for the given URI.
   --  If the document is not opened, then null will be returned.

   function Is_Open_Document
     (Self : Message_Handler;
      File : GNATCOLL.VFS.Virtual_File) return Boolean;
   --  Return True when given document is open.

   -----------------------
   --  Project Manager  --
   -----------------------

   type Project_Stamp is private;

   function Get_Project_Stamp
     (Self : Message_Handler'Class) return Project_Stamp;
   --  Return stamp of the state of the project. Stamp is changed each time
   --  project is (re)loaded.

   function Get_Open_Document_Version
     (Self : in out Message_Handler;
      URI  : LSP.Structures.DocumentUri)
      return LSP.Structures.OptionalVersionedTextDocumentIdentifier;
   --  Return the version of an open document for the given URI.
   --  If the document is not opened, then it returns a
   --  OptionalVersionedTextDocumentIdentifier with a null version.

private
   type Project_Stamp is mod 2**32;

   type Internal_Document_Access is access all LSP.Ada_Documents.Document;

   procedure Free (Self : in out Internal_Document_Access);
   --  Free all the data for the given document.

   --  Container for documents indexed by URI
   package Document_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => GNATCOLL.VFS.Virtual_File,
      Element_Type    => Internal_Document_Access,
      Hash            => GNATCOLL.VFS.Full_Name_Hash,
      Equivalent_Keys => GNATCOLL.VFS."=");

   --  Container for the predefined source files
   package File_Sets is new Ada.Containers.Hashed_Sets
     (Element_Type        => GNATCOLL.VFS.Virtual_File,
      Hash                => GNATCOLL.VFS.Full_Name_Hash,
      Equivalent_Elements => GNATCOLL.VFS."=",
      "="                 => GNATCOLL.VFS."=");

   type Has_Been_Canceled_Function is access function return Boolean;

   type Message_Handler
     (Server : not null LSP.Servers.Server_Access;
      --  Please avoid to use this discriminant!
      Sender : not null access LSP.Client_Message_Receivers
        .Client_Message_Receiver'Class;
      Tracer : not null LSP.Tracers.Tracer_Access) is limited
   new LSP.Unimplemented_Handlers.Unimplemented_Handler
     and LSP.Server_Message_Visitors.Server_Message_Visitor
     and LSP.Server_Notification_Receivers.Server_Notification_Receiver
     and LSP.Ada_Job_Contexts.Ada_Job_Context
   with record
      Client : aliased LSP.Ada_Client_Capabilities.Client_Capability;

      Base_Configuration : aliased LSP.Ada_Configurations.Configuration;
      --  This is the initial configuration loaded at process startup from
      --  configuration files (global, local .als.json, CLI --config). Later
      --  on the configuration may be overwritten by the 'initialize' request
      --  or 'onDidChangeConfiguration' notifications so saving this initial
      --  configuration allows to revert settings back when a null value is
      --  received in an `onDidChangeConfiguration` notification.

      Configuration : aliased LSP.Ada_Configurations.Configuration;
      --  The current configuration in use.

      Base_Configuration_Received : Boolean := False;
      --  Set to true once we receive the user's base configuration, either
      --  through config files, the 'initialize' request or the first
      --  'didChangeConfiguration' notification.

      Contexts : LSP.Ada_Context_Sets.Context_Set;
      --  There is one context in this list per loaded project.
      --  There should always be at least one "project" context - if no .gpr
      --  is known to the server, this context should map to the implicit
      --  project.

      Workspace_Diagnostic_Sources :
        Workspace_Diagnostic_Source_Vectors.Vector;
      --  Workspace diagnostic sources.

      Workspace_Diagnostic_Files : LSP.Ada_File_Sets.File_Sets.Set;
      --  Files for which workspace diagnostics have been published.
      --  Used to clear any existing workspace diagnostic before querying
      --  new ones.

      Highlighter    : aliased LSP.Ada_Highlighters.Ada_Highlighter;
      --  Semantic token highlighter for Ada

      Incremental_Text_Changes : Boolean;
      --  the support for incremental text changes is active

      Shutdown         : Boolean := False;
      --  Server is in the shutdown state.

      Open_Documents : Document_Maps.Map;
      --  The documents that are currently open

      Token_Id       : Integer := 0;
      --  An ever-increasing number used to generate unique progress tokens

      File_Monitor    : LSP.File_Monitors.File_Monitor_Access;
      --  Filesystem monitoring

      ----------------------
      -- Project handling --
      ----------------------

      Project_Tree : GPR2.Project.Tree.Object;
      --  The currently loaded project tree

      --  Project_Environment : Environment;
      --  The project environment for the currently loaded project

      Project_Predefined_Sources : LSP.Ada_File_Sets.Indexed_File_Set;
      --  A cache for the predefined sources in the loaded project (typically,
      --  runtime files).

      Project_Status : LSP.Ada_Project_Loading.Project_Status_Type :=
        LSP.Ada_Project_Loading.No_Project_Status;
      --  Indicates whether a project has been successfully loaded and
      --  how. Stores GPR2 error/warning messages emitted while loading
      --  (or attempting to load) the project.

      Project_Dirs_Loaded : File_Sets.Set;
      --  The directories to load in the "implicit project"

      Project_Stamp       : LSP.Ada_Handlers.Project_Stamp := 0;
      --  Stamp of the current project.

      Is_Canceled : Has_Been_Canceled_Function;
      --  Is request has been canceled
   end record;

   overriding procedure On_Server_Request
     (Self  : in out Message_Handler;
      Value : LSP.Server_Requests.Server_Request'Class);

   overriding procedure On_AlsCheckSyntax_Request
     (Self  : in out Message_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.AlsCheckSyntaxParams);

   overriding procedure On_DocumentHighlight_Request
     (Self  : in out Message_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.DocumentHighlightParams);

   overriding procedure On_Exits_Notification (Self : in out Message_Handler);

   overriding procedure On_Shutdown_Request
     (Self : in out Message_Handler;
      Id   : LSP.Structures.Integer_Or_Virtual_String);

   overriding procedure On_Server_Notification
     (Self  : in out Message_Handler;
      Value : LSP.Server_Notifications.Server_Notification'Class);

   overriding procedure On_IncomingCalls_Request
     (Self  : in out Message_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.CallHierarchyIncomingCallsParams);

   overriding procedure On_Implementation_Request
     (Self  : in out Message_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.ImplementationParams);

   overriding procedure On_Initialize_Request
     (Self  : in out Message_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.InitializeParams);

   overriding procedure On_OutgoingCalls_Request
     (Self  : in out Message_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.CallHierarchyOutgoingCallsParams);

   overriding procedure On_PrepareCallHierarchy_Request
     (Self  : in out Message_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.CallHierarchyPrepareParams);

   overriding procedure On_PrepareRename_Request
     (Self  : in out Message_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.PrepareRenameParams);

   overriding procedure On_Rename_Request
     (Self  : in out Message_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.RenameParams);

   overriding procedure On_Formatting_Request
     (Self  : in out Message_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.DocumentFormattingParams);

   overriding procedure On_RangeFormatting_Request
     (Self  : in out Message_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.DocumentRangeFormattingParams);

   overriding procedure On_Initialized_Notification
     (Self  : in out Message_Handler;
      Value : LSP.Structures.InitializedParams);

   overriding procedure On_DidChangeWatchedFiles_Notification
     (Self  : in out Message_Handler;
      Value : LSP.Structures.DidChangeWatchedFilesParams);

   overriding procedure On_DidOpen_Notification
     (Self  : in out Message_Handler;
      Value : LSP.Structures.DidOpenTextDocumentParams);

   overriding procedure On_DidClose_Notification
     (Self  : in out Message_Handler;
      Value : LSP.Structures.DidCloseTextDocumentParams);

   overriding procedure On_DidCreateFiles_Notification
     (Self  : in out Message_Handler;
      Value : LSP.Structures.CreateFilesParams);

   overriding procedure On_DidDeleteFiles_Notification
     (Self  : in out Message_Handler;
      Value : LSP.Structures.DeleteFilesParams);

   overriding procedure On_DidRenameFiles_Notification
     (Self  : in out Message_Handler;
      Value : LSP.Structures.RenameFilesParams);

   overriding procedure On_DidChangeWorkspaceFolders_Notification
     (Self  : in out Message_Handler;
      Value : LSP.Structures.DidChangeWorkspaceFoldersParams);

   overriding procedure On_Completion_Request
     (Self  : in out Message_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.CompletionParams);

   overriding procedure On_OnTypeFormatting_Request
     (Self  : in out Message_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.DocumentOnTypeFormattingParams);

   overriding procedure On_SignatureHelp_Request
     (Self  : in out Message_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.SignatureHelpParams);

   overriding procedure On_TypeDefinition_Request
     (Self  : in out Message_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.TypeDefinitionParams);

   overriding procedure On_CodeAction_Request
     (Self  : in out Message_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.CodeActionParams);

   overriding procedure On_Completion_Resolve_Request
     (Self  : in out Message_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.CompletionItem);

   overriding procedure On_Symbol_Request
     (Self  : in out Message_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.WorkspaceSymbolParams);

   overriding procedure Publish_Diagnostics
     (Self              : in out Message_Handler;
      Document          : not null LSP.Ada_Documents.Document_Access;
      Other_Diagnostics : LSP.Structures.Diagnostic_Vector :=
        LSP.Structures.Empty;
      Force             : Boolean := False);
   --  Publish diagnostic messages for given document if needed.
   --  Other_Diagnostics can be used to specify punctual diagnostics not coming
   --  from sources that analyze files when being opened or modified.
   --  When Force is True, the diagnostics will always be sent, regardless if
   --  they have changed or not.

   procedure Publish_Diagnostics
     (Self : in out Message_Handler; Force : Boolean := False);
   --  Publish workspace diagnostic messages.
   --  When Force is True, the diagnostics will always be sent, regardless if
   --  they have changed or not.
   --  Currently published workspace diagnostics are always cleared
   --  before querying new workspace diagnostics.

   overriding function To_File
     (Self : Message_Handler;
      URI  : LSP.Structures.DocumentUri) return GNATCOLL.VFS.Virtual_File
   is
     (GNATCOLL.VFS.Create_From_UTF8
        (URIs.Conversions.To_File
           (VSS.Strings.Conversions.To_UTF_8_String (URI),
            Normalize => Self.Configuration.Follow_Symlinks)));

   overriding function Contexts_For_File
     (Self : Message_Handler;
      File : GNATCOLL.VFS.Virtual_File)
      return LSP.Ada_Context_Sets.Context_Lists.List;

   overriding function Contexts_For_Position
     (Self : in out Message_Handler;
      Pos  : LSP.Structures.TextDocumentPositionParams'Class)
      return LSP.Ada_Context_Sets.Context_Lists.List;

   function To_URI
     (Ignore : Message_Handler'Class;
      File   : String) return LSP.Structures.DocumentUri
   is
     (VSS.Strings.Conversions.To_Virtual_String
        (URIs.Conversions.From_File (File)) with null record);

   function To_Workspace_Edit
     (Self   : in out Message_Handler'Class;
      Edits  : LAL_Refactor.Refactoring_Edits;
      Rename : Boolean := False)
      return LSP.Structures.WorkspaceEdit;
   --  Converts a Refactoring_Edits into a WorkspaceEdit. The Rename flag
   --  controls if files that are supposed to be deleted, are renamed instead.

   function Contexts_For_URI
     (Self : Message_Handler'Class;
      URI  : LSP.Structures.DocumentUri)
      return LSP.Ada_Context_Sets.Context_Lists.List
   is
      (Self.Contexts_For_File (Self.To_File (URI)));
   --  Return a list of contexts that are suitable for the given File/URI:
   --  a list of all contexts where the file is known to be part of the
   --  project tree, or is a runtime file for this project. If the file
   --  is not known to any project, return an empty list.

   ------------------
   --  Job_Context --
   ------------------

   overriding function Client (Self : Message_Handler) return
     access constant LSP.Ada_Client_Capabilities.Client_Capability'Class
       is (Self.Client'Unchecked_Access);

   overriding function Get_Base_Configuration (Self : Message_Handler)
     return access constant LSP.Ada_Configurations.Configuration'Class
       is (Self.Base_Configuration'Unchecked_Access);

   overriding function Get_Configuration (Self : Message_Handler)
     return access constant LSP.Ada_Configurations.Configuration'Class is
       (Self.Configuration'Unchecked_Access);

   overriding procedure Set_Configuration
      (Self  : in out Message_Handler;
       Value : LSP.Ada_Configurations.Configuration'Class);

   overriding procedure Increment_Project_Timestamp
     (Self : in out Message_Handler);

   overriding procedure Send_Messages
     (Self     : Message_Handler;
      Show     : Boolean;
      Messages : VSS.String_Vectors.Virtual_String_Vector;
      Severity : LSP.Enumerations.MessageType;
      File     : GNATCOLL.VFS.Virtual_File);

   overriding function Project_Tree_Is_Defined (Self : Message_Handler)
     return Boolean is (Self.Project_Tree.Is_Defined);

   overriding function Project_Tree_Is_Aggregate (Self : Message_Handler)
     return Boolean is
     (Self.Project_Tree_Is_Defined
      and then Self.Project_Tree.Root_Project.Is_Defined
      and then Self.Project_Tree.Root_Project.Kind in GPR2.Aggregate_Kind);

   overriding function Get_Runtime_Sources (Self : Message_Handler)
     return GPR2.Build.Source.Sets.Object is
     (if Self.Project_Tree_Is_Defined
      then Self.Project_Tree.Runtime_Project.Sources
      else GPR2.Build.Source.Sets.Empty_Set);

   overriding procedure Reload_Project (Self : in out Message_Handler);

   overriding function Get_Best_Context
     (Self : Message_Handler;
      URI  : LSP.Structures.DocumentUri)
      return LSP.Ada_Context_Sets.Context_Access is
     (Self.Contexts.Get_Best_Context (URI));

   overriding function Get_Node_At
     (Self     : in out Message_Handler;
      Context  : LSP.Ada_Contexts.Context;
      Value    : LSP.Structures.TextDocumentPositionParams'Class)
      return Libadalang.Analysis.Ada_Node;

   overriding function Imprecise_Resolve_Name
     (Self      : in out Message_Handler;
      Name_Node : Libadalang.Analysis.Name)
      return Libadalang.Analysis.Defining_Name;

   overriding function To_LSP_Location
     (Self : in out Message_Handler;
      Node : Libadalang.Analysis.Ada_Node'Class)
      return LSP.Structures.Location;

   overriding function To_LSP_Range
     (Self  : in out Message_Handler;
      Unit  : Libadalang.Analysis.Analysis_Unit;
      Token : Libadalang.Common.Token_Reference)
      return LSP.Structures.A_Range;

   overriding function To_LSP_Range
     (Self  : in out Message_Handler;
      Node   : Libadalang.Analysis.Ada_Node'Class)
      return LSP.Structures.A_Range;

   overriding procedure Append_Location
     (Self   : in out Message_Handler;
      Result : in out LSP.Structures.Location_Vector;
      Filter : in out LSP.Locations.File_Span_Sets.Set;
      Node   : Libadalang.Analysis.Ada_Node'Class;
      Kinds  : LSP.Structures.AlsReferenceKind_Set := LSP.Constants.Empty);

   overriding procedure Trace_Exception
     (Self    : Message_Handler;
      Error   : Ada.Exceptions.Exception_Occurrence;
      Message : VSS.Strings.Virtual_String :=
        VSS.Strings.Empty_Virtual_String);

   overriding function Get_Trace_Handle (Self : Message_Handler)
     return GNATCOLL.Traces.Trace_Handle;

   overriding function Get_Highlighter
     (Self : in out Message_Handler)
      return access constant LSP.Ada_Highlighters.Ada_Highlighter is
       (Self.Highlighter'Unchecked_Access);

end LSP.Ada_Handlers;
