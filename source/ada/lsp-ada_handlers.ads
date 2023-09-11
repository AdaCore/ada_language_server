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

with GNATCOLL.VFS;

with GPR2.Project.Tree;

with VSS.Strings.Conversions;

with LSP.Ada_Client_Capabilities;
with LSP.Ada_Configurations;
with LSP.Ada_Context_Sets;
with LSP.Ada_Documents;
with LSP.Ada_File_Sets;
with LSP.Ada_Highlighters;
with LSP.Client_Message_Receivers;
with LSP.Server_Message_Visitors;
with LSP.Server_Notification_Receivers;
with LSP.Server_Notifications;
with LSP.Server_Request_Receivers;
with LSP.Server_Requests;
limited with LSP.Servers;
with LSP.Structures;
with LSP.Tracers;
with LSP.Unimplemented_Handlers;

with URIs;

private with LAL_Refactor;

package LSP.Ada_Handlers is

   type Message_Handler
     (Server : not null access LSP.Servers.Server'Class;
      --  Please avoid to use this discriminant!
      Sender : not null access LSP.Client_Message_Receivers
        .Client_Message_Receiver'Class;
      Tracer : not null LSP.Tracers.Tracer_Access) is limited
   new LSP.Server_Message_Visitors.Server_Message_Visitor
     and LSP.Server_Request_Receivers.Server_Request_Receiver
     and LSP.Server_Notification_Receivers.Server_Notification_Receiver
   with private;

   procedure Initialize
     (Self : in out Message_Handler'Class;
      Incremental_Text_Changes : Boolean);
   --  Initialize the message handler and configure it.
   --
   --  Incremental_Text_Changes - activate the support for incremental text
   --  changes.

   function Contexts_For_File
     (Self : access Message_Handler;
      File : GNATCOLL.VFS.Virtual_File)
      return LSP.Ada_Context_Sets.Context_Lists.List;

   function Is_Shutdown (Self : Message_Handler'Class) return Boolean;
   --  Return True when shutdown has been requested.

   function Allocate_Progress_Token
     (Self      : in out Message_Handler'Class;
      Operation : VSS.Strings.Virtual_String)
      return LSP.Structures.ProgressToken;
   --  Return an unique token for indicating progress

   ----------------------------
   --  Open Document Manger  --
   ----------------------------

   function Get_Open_Document
     (Self  : in out Message_Handler;
      URI   : LSP.Structures.DocumentUri;
      Force : Boolean := False)
      return LSP.Ada_Documents.Document_Access;
   --  Return the open document for the given URI.
   --  If the document is not opened, then if Force a new document
   --  will be created and must be freed by the user else null will be
   --  returned.

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

   type Load_Project_Status is
     (Valid_Project_Configured,
      Single_Project_Found,
      Alire_Project,
      No_Runtime_Found,
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
   --  @value Alire_Project no project in didChangeConfiguration, but Alire
   --  knows what project to use
   --
   --  @value No_Runtime_Found project loaded, but no Ada runtime library was
   --  found
   --
   --  @value No_Project_Found no project in didChangeConfiguration and no
   --  project in Root dir
   --
   --  @value Multiple_Projects_Found no project in didChangeConfiguration and
   --  several projects in Root dir
   --
   --  @value Invalid_Project_Configured didChangeConfiguration provided a
   --  valid project

   type Project_Stamp is mod 2**32;

   subtype Implicit_Project_Loaded is Load_Project_Status range
     No_Project_Found .. Invalid_Project_Configured;
   --  Project status when an implicit project loaded

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
     (Server : not null access LSP.Servers.Server'Class;
      --  Please avoid to use this discriminant!
      Sender : not null access LSP.Client_Message_Receivers
        .Client_Message_Receiver'Class;
      Tracer : not null LSP.Tracers.Tracer_Access) is limited
   new LSP.Unimplemented_Handlers.Unimplemented_Handler
     and LSP.Server_Message_Visitors.Server_Message_Visitor
     and LSP.Server_Notification_Receivers.Server_Notification_Receiver
   with record
      Client : LSP.Ada_Client_Capabilities.Client_Capability;
      Configuration : LSP.Ada_Configurations.Configuration;

      Contexts : LSP.Ada_Context_Sets.Context_Set;
      --  There is one context in this list per loaded project.
      --  There should always be at least one "project" context - if no .gpr
      --  is known to the server, this context should map to the implicit
      --  project.

      Highlighter    : LSP.Ada_Highlighters.Ada_Highlighter;
      --  Semantic token highlighter for Ada

      Incremental_Text_Changes : Boolean;
      --  the support for incremental text changes is active

      Shutdown         : Boolean := False;
      --  Server is in the shutdown state.

      Open_Documents : Document_Maps.Map;
      --  The documents that are currently open

      Token_Id       : Integer := 0;
      --  An ever-increasing number used to generate unique progress tokens

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

      Project_Status : Load_Project_Status := No_Project_Found;
      --  Status of loading the project

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

   overriding procedure On_Declaration_Request
     (Self  : in out Message_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.DeclarationParams);

   overriding procedure On_Definition_Request
     (Self  : in out Message_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.DefinitionParams);

   overriding procedure On_DocumentHighlight_Request
     (Self  : in out Message_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.DocumentHighlightParams);

   overriding procedure On_DocumentSymbol_Request
     (Self  : in out Message_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.DocumentSymbolParams);

   overriding procedure On_Exits_Notification (Self : in out Message_Handler);

   overriding procedure On_Hover_Request
     (Self  : in out Message_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.HoverParams);

   overriding procedure On_References_Request
     (Self  : in out Message_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.ReferenceParams);

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

   overriding procedure On_FoldingRange_Request
     (Self  : in out Message_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.FoldingRangeParams);

   overriding procedure On_Formatting_Request
     (Self  : in out Message_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.DocumentFormattingParams);

   overriding procedure On_Tokens_Full_Request
     (Self  : in out Message_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.SemanticTokensParams);

   overriding procedure On_RangeFormatting_Request
     (Self  : in out Message_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.DocumentRangeFormattingParams);

   overriding procedure On_DidChangeConfiguration_Notification
     (Self  : in out Message_Handler;
      Value : LSP.Structures.DidChangeConfigurationParams);

   overriding procedure On_DidOpen_Notification
     (Self  : in out Message_Handler;
      Value : LSP.Structures.DidOpenTextDocumentParams);

   overriding procedure On_DidChange_Notification
     (Self  : in out Message_Handler;
      Value : LSP.Structures.DidChangeTextDocumentParams);

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

   overriding procedure On_Tokens_Range_Request
     (Self  : in out Message_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.SemanticTokensRangeParams);

   overriding procedure On_TypeDefinition_Request
     (Self  : in out Message_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.TypeDefinitionParams);

   overriding procedure On_CodeAction_Request
     (Self  : in out Message_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.CodeActionParams);

   overriding procedure On_ExecuteCommand_Request
     (Self  : in out Message_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.ExecuteCommandParams);

   overriding procedure On_Completion_Resolve_Request
     (Self  : in out Message_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.CompletionItem);

   overriding procedure On_Symbol_Request
     (Self  : in out Message_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.WorkspaceSymbolParams);

   procedure Publish_Diagnostics
     (Self              : in out Message_Handler'Class;
      Document          : not null LSP.Ada_Documents.Document_Access;
      Other_Diagnostics : LSP.Structures.Diagnostic_Vector :=
        LSP.Structures.Empty;
      Force             : Boolean := False);
   --  Publish diagnostic messages for given document if needed.
   --  Other_Diagnostics can be used to specify punctual diagnostics not coming
   --  from sources that analyze files when being opened or modified.
   --  When Force is True, the diagnostics will always be sent, not matter if
   --  they have changed or not.

   function To_File
     (Self : Message_Handler'Class;
      URI  : LSP.Structures.DocumentUri) return GNATCOLL.VFS.Virtual_File
   is
     (GNATCOLL.VFS.Create_From_UTF8
        (URIs.Conversions.To_File
           (VSS.Strings.Conversions.To_UTF_8_String (URI),
            Normalize => Self.Configuration.Follow_Symlinks)));

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

end LSP.Ada_Handlers;
