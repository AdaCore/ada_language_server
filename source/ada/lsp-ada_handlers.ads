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
--
--  This package provides requests and notifications handler for Ada
--  language.

with Ada.Containers.Hashed_Maps;
with Ada.Containers.Hashed_Sets;
with VSS.String_Vectors;

with GNATCOLL.VFS;    use GNATCOLL.VFS;
with GNATCOLL.Projects;
with GNATCOLL.Traces;

private with GNATdoc.Comments.Options;

with LSP.Ada_Contexts;
with LSP.Ada_Context_Sets;
with LSP.Ada_Documents;
with LSP.Ada_File_Sets;
with LSP.Ada_Highlighters;

with LSP.File_Monitors;
with LSP.Messages.Server_Requests;
with LSP.Messages.Server_Responses;
with LSP.Search;
with LSP.Server_Backends;
with LSP.Server_Request_Handlers;
with LSP.Server_Notification_Receivers;
with LSP.Servers;
with LSP.Types;
with LSP.Messages;

package LSP.Ada_Handlers is

   type Message_Handler
     (Server  : access LSP.Servers.Server;
      Trace   : GNATCOLL.Traces.Trace_Handle) is
   limited new LSP.Server_Request_Handlers.Server_Request_Handler
     and LSP.Server_Notification_Receivers.Server_Notification_Receiver
     and LSP.Server_Backends.Server_Backend
     and LSP.Ada_Documents.Document_Provider
   with private;
   --  A handler of LSP notifications and requests from Ada language

   overriding procedure Handle_Error (Self : access Message_Handler);
   --  This procedure will be called when an unexpected error is raised in the
   --  request processing loop.

   procedure Stop_File_Monitoring (Self : access Message_Handler);

   procedure Cleanup (Self : access Message_Handler);
   --  Free memory referenced by Self

   procedure Clean_Logs (Self : access Message_Handler; Dir : Virtual_File);
   --  Remove the oldest logs in Dir

   subtype Context_Access is LSP.Ada_Context_Sets.Context_Access;

   function From_File
     (Self : Message_Handler'Class;
      File : Virtual_File) return LSP.Messages.DocumentUri;
   --  Turn Virtual_File to URI

   function To_File
     (Self : Message_Handler'Class;
      URI  : LSP.Types.LSP_URI) return GNATCOLL.VFS.Virtual_File;
   --  Turn URI into Virtual_File

private

   --  Options for refactoring/renaming
   type Renaming_Options is record
      In_Comments : Boolean := False;
   end record;

   --  Options for refactoring
   type Refactoring_Options is record
      Renaming : Renaming_Options;
   end record;

   -- Options for folding --
   type Folding_Options is record
      Comments : Boolean := True;
   end record;

   --  Options for documentation
   type Documentation_Options is record
      Style : GNATdoc.Comments.Options.Documentation_Style :=
        GNATdoc.Comments.Options.GNAT;
   end record;

   -- Options holder --
   type Options_Holder is record
      Refactoring   : Refactoring_Options;
      --  Configuration options for refactoring

      Folding       : Folding_Options;
      --  folding options

      Documentation : Documentation_Options;
      --  Configuration options for documentation
   end record;

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

   type Get_Symbol_Access is access procedure
     (Self     : LSP.Ada_Documents.Document;
      Context  : LSP.Ada_Contexts.Context;
      Pattern  : LSP.Search.Search_Pattern'Class;
      Canceled : access function return Boolean;
      Result   : out LSP.Messages.Symbol_Vector);
   --  textDocument/documentSymbol handler

   Empty_Token : LSP.Types.LSP_Number_Or_String :=
     (Is_Number => False, String => <>);

   type Load_Project_Status is
     (Valid_Project_Configured,
      Single_Project_Found,
      No_Project_Found,
      Multiple_Projects_Found,
      Invalid_Project_Configured);
   --  Variants for state of the project loaded into the handler:
   --
   --  @value Valid_Project_Configured didChangeConfiguration provided a valid
   --  project
   --
   --  @value Single_Project_Found no project in didChangeConfiguration, but
   --  just one project in Root dir
   --
   --  @value No_Project_Found no project in didChangeConfiguration and no
   --  project in Root dir
   --
   --  @value Multiple_Projects_Found no project in didChangeConfiguration and
   --  several projects in Root dir
   --
   --  @value Invalid_Project_Configured didChangeConfiguration provided a
   --  valid project

   subtype Implicit_Project_Loaded is Load_Project_Status range
     No_Project_Found .. Invalid_Project_Configured;
   --  Project status when an implicit project loaded

   type Advanced_Refactorings is
     (Add_Parameter, Change_Parameters_Type, Change_Parameters_Default_Value);
   --  Enum with the advanced refactorings that clients might support

   type Advanced_Refactorings_Capabilities is
     array (Advanced_Refactorings) of Boolean;
   --  Array that determines what advanced refactorings clients support

   type Experimental_Client_Capabilities is record
      Advanced_Refactorings : Advanced_Refactorings_Capabilities;
   end record;
   --  Experimental client capabilities that are extensions of the ones defined
   --  in the LSP

   function Parse
     (Value : LSP.Types.Optional_LSP_Any)
      return Experimental_Client_Capabilities;
   --  Parses an LSP.Types.Optional_LSP_Any and creates an
   --  Experimental_Client_Capabilities object.

   type Message_Handler
     (Server  : access LSP.Servers.Server;
      Trace   : GNATCOLL.Traces.Trace_Handle)
   is limited new LSP.Server_Request_Handlers.Server_Request_Handler
     and LSP.Server_Notification_Receivers.Server_Notification_Receiver
     and LSP.Server_Backends.Server_Backend
     and LSP.Ada_Documents.Document_Provider
   with record
      Contexts : LSP.Ada_Context_Sets.Context_Set;
      --  There is one context in this list per loaded project.
      --  There should always be at least one "project" context - if no .gpr
      --  is known to the server, this context should map to the implicit
      --  project.

      Client : LSP.Messages.InitializeParams;
      --  Client settings got during initialization request

      Root : Virtual_File;
      --  The directory passed under rootURI/rootPath during the initialize
      --  request.

      Diagnostics_Enabled : Boolean := True;
      --  Whether to publish diagnostics

      Token_Id : Integer := 0;
      --  An ever-increasing number used to generate unique progress tokens

      Indexing_Enabled  : Boolean := True;
      --  Whether to index sources in the background. This should be True
      --  for normal use, and can be disabled for debug or testing purposes.

      Indexing_Token  : LSP.Types.LSP_Number_Or_String := Empty_Token;
      --  The token of the current indexing progress sequence

      Files_To_Index : File_Sets.Set;
      --  Contains any files that need indexing.
      --
      --  Indexing of sources is performed in the background as soon as needed
      --  (typically after a project load), and pre-indexes the Ada source
      --  files, so that subsequent request are fast.
      --  The way the "backgrounding" works is the following:
      --
      --      * each request which should trigger indexing (for instance
      --        project load) adds files to Files_To_Index
      --
      --      * the procedure Index_Files takes care of the indexing; it's also
      --        looking at the queue after each indexing to see if there
      --        are requests pending. If a request is pending, it stops
      --        indexing.
      --
      --      * whenever the server has finished processing a notification
      --        or a requests, it looks at whether Files_To_Index contains
      --        files; if it does, it runs Index_Files

      Total_Files_Indexed  : Natural := 0;
      Total_Files_To_Index : Positive := 1;
      --  These two fields are used to produce a progress bar for the indexing
      --  operations. Total_Files_To_Index starts at 1 so that the progress
      --  bar starts at 0%.

      Options : Options_Holder;

      Open_Documents : Document_Maps.Map;
      --  The documents that are currently open

      Get_Symbols : Get_Symbol_Access;
      --  textDocument/documentSymbol handler. Actual value depends on
      --  client's capabilities.

      Resource_Operations : LSP.Messages.Optional_ResourceOperationKindSet;
      --  Client capabilities to support resource operations in
      --  `WorkspaceEdit`s.

      Versioned_Documents : Boolean := False;
      --  Client capabilities to support versioned document changes in
      --  `WorkspaceEdit`s.

      Line_Folding_Only : Boolean := False;
      --  Client capabilities, folding only per lines

      Completion_Snippets_Enabled : Boolean := False;
      --  True if the client supports completion snippets

      Use_Completion_Snippets : Boolean := True;
      --  True if we should use snippets for completion (e.g:
      --  subprogram calls).

      Completion_Resolve_Properties : VSS.String_Vectors.Virtual_String_Vector;
      --  The list of CompletionItem properties that can be resolved
      --  lazily (i.e: when the item is selected on client-side) via
      --  the completionItem/resolve request.

      Range_Formatting_Enabled : Boolean := False;
      --  True if the handler has registered rangeFormatting provider

      Named_Notation_Threshold : Natural := 3;
      --  Defines the number of parameters/components at which point named
      --  notation is used for subprogram/aggregate completion snippets.

      Display_Method_Ancestry_Policy :
         LSP.Messages.AlsDisplayMethodAncestryOnNavigationPolicy :=
           LSP.Messages.Usage_And_Abstract_Only;
      --  Defines the policy regarding the listing of
      --  overriding/overridden subprograms for navigation requests
      --  such as textDocument/definition, textDocument/declaration
      --  and textDocument/implementation.

      Follow_Symlinks : Boolean := True;
      --  False if the client disables symlink following. In this case
      --  URIs from client should match file names reported by LAL and
      --  GNATCOLL.Project.

      Highlighter    : LSP.Ada_Highlighters.Ada_Highlighter;
      --  Semantic token highlighter for Ada

      ----------------------
      -- Project handling --
      ----------------------

      Project_Tree : GNATCOLL.Projects.Project_Tree_Access;
      --  The currently loaded project tree

      Project_Environment : GNATCOLL.Projects.Project_Environment_Access;
      --  The project environment for the currently loaded project

      Project_Predefined_Sources : LSP.Ada_File_Sets.Indexed_File_Set;
      --  A cache for the predefined sources in the loaded project (typically,
      --  runtime files).

      Project_Status : Load_Project_Status := No_Project_Found;
      --  Status of loading the project

      --  Scenario : LSP.Types.LSP_Any;
      --  Last used scenario variables

      --  Charset : Ada.Strings.Unbounded.Unbounded_String;
      --  Character set from didChangeConfiguration

      Project_Dirs_Loaded : File_Sets.Set;
      --  The directories to load in the "implicit project"

      File_Monitor    : LSP.File_Monitors.File_Monitor_Access;
      --  Filesystem monitoring

      Log_Threshold : Natural := 10;
      --  Maximum number of logs (should be > to the number of servers run
      --  simultaneously)

      ---------------------------------------
      --  Experimental Client Capabilities --
      ---------------------------------------

      Experimental_Client_Capabilities :
        LSP.Ada_Handlers.Experimental_Client_Capabilities :=
          (Advanced_Refactorings => (others => False));
   end record;

   overriding procedure Before_Work
     (Self    : access Message_Handler;
      Message : LSP.Messages.Message'Class);

   overriding procedure After_Work
     (Self    : access Message_Handler;
      Message : LSP.Messages.Message'Class);

   overriding function On_Initialize_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Initialize_Request)
      return LSP.Messages.Server_Responses.Initialize_Response;

   overriding function On_Shutdown_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Shutdown_Request)
      return LSP.Messages.Server_Responses.Shutdown_Response;

   overriding function On_CodeAction_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.CodeAction_Request)
      return LSP.Messages.Server_Responses.CodeAction_Response;

   overriding function On_Completion_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Completion_Request)
      return LSP.Messages.Server_Responses.Completion_Response;

   overriding function On_CompletionItemResolve_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.CompletionItemResolve_Request)
      return LSP.Messages.Server_Responses.CompletionItemResolve_Response;

   overriding function On_Definition_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Definition_Request)
      return LSP.Messages.Server_Responses.Location_Link_Response;

   overriding function On_Declaration_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Declaration_Request)
      return LSP.Messages.Server_Responses.Location_Link_Response;

   overriding function On_Implementation_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Implementation_Request)
      return LSP.Messages.Server_Responses.Location_Link_Response;

   overriding function On_Type_Definition_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Type_Definition_Request)
      return LSP.Messages.Server_Responses.Location_Link_Response;

   overriding function On_Highlight_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Highlight_Request)
      return LSP.Messages.Server_Responses.Highlight_Response;

   overriding function On_Hover_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Hover_Request)
      return LSP.Messages.Server_Responses.Hover_Response;

   overriding function On_References_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.References_Request)
      return LSP.Messages.Server_Responses.Location_Response;

   overriding function On_Signature_Help_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Signature_Help_Request)
      return LSP.Messages.Server_Responses.SignatureHelp_Response;

   overriding function On_Document_Links_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Document_Links_Request)
      return LSP.Messages.Server_Responses.Links_Response;

   overriding function On_Document_Tokens_Full_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Document_Tokens_Full_Request)
      return LSP.Messages.Server_Responses.SemanticTokens_Response;

   overriding function On_Document_Tokens_Range_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Document_Tokens_Range_Request)
      return LSP.Messages.Server_Responses.SemanticTokens_Response;

   overriding function On_Document_Symbols_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Document_Symbols_Request)
      return LSP.Messages.Server_Responses.Symbol_Response;

   overriding function On_Folding_Range_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Folding_Range_Request)
      return LSP.Messages.Server_Responses.FoldingRange_Response;

   overriding function On_Prepare_Call_Hierarchy_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Prepare_Call_Hierarchy_Request)
      return LSP.Messages.Server_Responses.PrepareCallHierarchy_Response;

   overriding function On_Incoming_Calls_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Incoming_Calls_Request)
      return LSP.Messages.Server_Responses.IncomingCalls_Response;

   overriding function On_Outgoing_Calls_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Outgoing_Calls_Request)
      return LSP.Messages.Server_Responses.OutgoingCalls_Response;

   overriding function On_Selection_Range_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Selection_Range_Request)
      return LSP.Messages.Server_Responses.SelectionRange_Response;

   overriding function On_Rename_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Rename_Request)
      return LSP.Messages.Server_Responses.Rename_Response;

   overriding function On_Prepare_Rename_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Prepare_Rename_Request)
      return LSP.Messages.Server_Responses.Prepare_Rename_Response;

   overriding function On_Execute_Command_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Execute_Command_Request)
      return LSP.Messages.Server_Responses.ExecuteCommand_Response;

   overriding function On_Workspace_Symbols_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Workspace_Symbols_Request)
      return LSP.Messages.Server_Responses.Symbol_Response;

   overriding function On_Workspace_Execute_Command_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Workspace_Execute_Command_Request)
      return LSP.Messages.Server_Responses.ExecuteCommand_Response;

   overriding function On_Workspace_Will_Create_Files_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.
                  Workspace_Will_Create_Files_Request)
      return LSP.Messages.Server_Responses.WillCreateFiles_Response;

   overriding procedure On_DidCreateFiles_Notification
     (Self  : access Message_Handler;
      Value : LSP.Messages.CreateFilesParams);

   overriding function On_Workspace_Will_Rename_Files_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.
                  Workspace_Will_Rename_Files_Request)
      return LSP.Messages.Server_Responses.WillRenameFiles_Response;

   overriding procedure On_DidRenameFiles_Notification
     (Self  : access Message_Handler;
      Value : LSP.Messages.RenameFilesParams);

   overriding function On_Workspace_Will_Delete_Files_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.
                  Workspace_Will_Delete_Files_Request)
      return LSP.Messages.Server_Responses.WillDeleteFiles_Response;

   overriding procedure On_DidDeleteFiles_Notification
     (Self  : access Message_Handler;
      Value : LSP.Messages.DeleteFilesParams);

   overriding function On_Color_Presentation_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Color_Presentation_Request)
      return LSP.Messages.Server_Responses.ColorPresentation_Response;

   overriding function On_Document_Color_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Document_Color_Request)
      return LSP.Messages.Server_Responses.DocumentColor_Response;

   overriding function On_ALS_Show_Dependencies_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.ALS_Show_Dependencies_Request)
      return LSP.Messages.Server_Responses.ALS_ShowDependencies_Response;

   overriding function On_ALS_Source_Dirs_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.ALS_Source_Dirs_Request)
      return LSP.Messages.Server_Responses.ALS_SourceDirs_Response;

   overriding function On_ALS_Debug_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.ALS_Debug_Request)
      return LSP.Messages.Server_Responses.ALS_Debug_Response;

   overriding function On_Formatting_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Formatting_Request)
      return LSP.Messages.Server_Responses.Formatting_Response;

   overriding function On_Range_Formatting_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Range_Formatting_Request)
      return LSP.Messages.Server_Responses.Range_Formatting_Response;

   overriding procedure On_Initialized_Notification
     (Self  : access Message_Handler) is null;

   overriding procedure On_Exit_Notification
     (Self  : access Message_Handler);

   overriding procedure On_DidChangeTextDocument_Notification
     (Self  : access Message_Handler;
      Value : LSP.Messages.DidChangeTextDocumentParams);

   overriding procedure On_DidCloseTextDocument_Notification
     (Self  : access Message_Handler;
      Value : LSP.Messages.DidCloseTextDocumentParams);

   overriding procedure On_DidOpenTextDocument_Notification
     (Self  : access Message_Handler;
      Value : LSP.Messages.DidOpenTextDocumentParams);

   overriding procedure On_DidSaveTextDocument_Notification
     (Self  : access Message_Handler;
      Value : LSP.Messages.DidSaveTextDocumentParams) is null;

   overriding procedure On_DidChangeConfiguration_Notification
     (Self  : access Message_Handler;
      Value : LSP.Messages.DidChangeConfigurationParams);

   overriding procedure On_DidChangeWorkspaceFolders_Notification
     (Self  : access Message_Handler;
      Value : LSP.Messages.DidChangeWorkspaceFoldersParams) is null;

   overriding procedure On_DidChangeWatchedFiles_Notification
     (Self  : access Message_Handler;
      Value : LSP.Messages.DidChangeWatchedFilesParams);

   overriding procedure On_Cancel_Notification
     (Self  : access Message_Handler;
      Value : LSP.Messages.CancelParams) is null;
   --  This is intentionally null procedure, because cancel is implemented by
   --  LSP server itself.

   overriding function Get_Open_Document
     (Self  : access Message_Handler;
      URI   : LSP.Messages.DocumentUri;
      Force : Boolean := False)
      return LSP.Ada_Documents.Document_Access;
   --  Return the open document for the given URI.
   --  If the document is not opened, then if Force a new document
   --  will be created and must be freed by the user else null will be
   --  returned.

   overriding function Get_Open_Document_Version
     (Self  : access Message_Handler;
      URI   : LSP.Messages.DocumentUri)
      return LSP.Messages.OptionalVersionedTextDocumentIdentifier;
   --  Return the version of an open document for the given URI.
   --  If the document is not opened, then it returns a
   --  VersionedTextDocumentIdentifier with a null version.

   overriding function On_ALS_Check_Syntax_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.ALS_Check_Syntax_Request)
      return LSP.Messages.Server_Responses.ALS_Check_Syntax_Response;

end LSP.Ada_Handlers;
