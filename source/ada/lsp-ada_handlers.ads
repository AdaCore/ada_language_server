------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2018-2020, AdaCore                     --
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

with GNATCOLL.VFS;    use GNATCOLL.VFS;
with GNATCOLL.Projects;
with GNATCOLL.Traces;

with LSP.Ada_Contexts;
with LSP.Ada_Context_Sets;
with LSP.Ada_Documents;
with LSP.Ada_File_Sets;

with LSP.Messages.Server_Requests;
with LSP.Messages.Server_Responses;
with LSP.Server_Backends;
with LSP.Server_Request_Handlers;
with LSP.Server_Notification_Receivers;
with LSP.Servers;
with LSP.Types;

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

   procedure Cleanup (Self : access Message_Handler);
   --  Free memory referenced by Self

   subtype Context_Access is LSP.Ada_Context_Sets.Context_Access;

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

   -- Options holder --
   type Options_Holder is record
      Refactoring : Refactoring_Options;
      --  Configuration options for refactoring

      Folding : Folding_Options;
      --  folding options
   end record;

   type Internal_Document_Access is access all LSP.Ada_Documents.Document;

   --  Container for documents indexed by URI
   package Document_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => LSP.Messages.DocumentUri,
      Element_Type    => Internal_Document_Access,
      Hash            => LSP.Types.Hash,
      Equivalent_Keys => LSP.Types."=");

   --  Container for the predefined source files
   package File_Sets is new Ada.Containers.Hashed_Sets
     (Element_Type        => GNATCOLL.VFS.Virtual_File,
      Hash                => GNATCOLL.VFS.Full_Name_Hash,
      Equivalent_Elements => GNATCOLL.VFS."=",
      "="                 => GNATCOLL.VFS."=");

   type Get_Symbol_Access is access procedure
     (Self     : LSP.Ada_Documents.Document;
      Context  : LSP.Ada_Contexts.Context;
      Result   : out LSP.Messages.Symbol_Vector);
   --  textDocument/documentSymbol handler

   type Message_Handler
     (Server  : access LSP.Servers.Server;
      Trace   : GNATCOLL.Traces.Trace_Handle)
   is limited new LSP.Server_Request_Handlers.Server_Request_Handler
     and LSP.Server_Notification_Receivers.Server_Notification_Receiver
     and LSP.Server_Backends.Server_Backend
     and LSP.Ada_Documents.Document_Provider with
   record
      Contexts : LSP.Ada_Context_Sets.Context_Set;
      --  There is one context in this list per loaded project.
      --  There should always be at least one "project" context - if no .gpr
      --  is known to the server, this context should map to the implicit
      --  project.

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

      Indexing_Required : Boolean := False;
      --  Set to True if an indexing operation had been paused in order to
      --  process requests.
      --  Indexing of sources is performed in the background as soon as needed
      --  (typically after a project load), and pre-indexes the Ada source
      --  files, so that subsequent request are fast.
      --  The way the "backgrounding" works is the following:
      --
      --      * each request which should trigger indexing (for instance
      --        project load) sets Indexing_Required to True
      --
      --      * the procedure Index_Files takes care of the indexing; it's also
      --        looking at the queue after each indexing to see if there
      --        are requests pending. If a request is pending, it stops
      --        indexing, and leaves Indexing_Required to True
      --
      --      * whenever the server has finished processing a notification
      --        or a requests, it looks at whether Indexing_Required is True,
      --        if it is, it runs Indexing_Required
      --
      --      * Indexing_Required is set to False when Index_Files has
      --        completed

      Options : Options_Holder;

      Open_Documents : Document_Maps.Map;
      --  The documents that are currently open

      Get_Symbols : Get_Symbol_Access;
      --  textDocument/documentSymbol handler. Actual value depends on
      --  client's capabilities.

      Line_Folding_Only : Boolean := False;
      --  Client capabilities, folding only per lines

      Completion_Snippets_Enabled : Boolean := False;
      --  True if the client supports completion snippets

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

      Implicit_Project_Loaded : Boolean := False;
      --  Whether we are loading the implicit project

      Project_Dirs_Loaded : File_Sets.Set;
      --  The directories to load in the "implicit project"
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

   overriding function On_Color_Presentation_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Color_Presentation_Request)
      return LSP.Messages.Server_Responses.ColorPresentation_Response;

   overriding function On_Document_Color_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Document_Color_Request)
      return LSP.Messages.Server_Responses.DocumentColor_Response;

   overriding function On_ALS_Called_By_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.ALS_Called_By_Request)
      return LSP.Messages.Server_Responses.ALS_Called_By_Response;

   overriding function On_ALS_Calls_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.ALS_Calls_Request)
      return LSP.Messages.Server_Responses.ALS_Calls_Response;

   overriding function On_ALS_Show_Dependencies_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.ALS_Show_Dependencies_Request)
      return LSP.Messages.Server_Responses.ALS_ShowDependencies_Response;

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

end LSP.Ada_Handlers;
