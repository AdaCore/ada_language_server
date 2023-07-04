------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2022-2023, AdaCore                     --
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
--  This package provides requests and notifications handler for GPR language.

with Ada.Containers.Hashed_Maps;

with GNATCOLL.Traces;
with GNATCOLL.VFS;

with GPR2.File_Readers;
with GPR2.Path_Name;

with LSP.GPR_Documents;
with LSP.GPR_Files;
with LSP.Messages.Server_Requests;
with LSP.Messages.Server_Responses;
with LSP.Server_Request_Handlers;
with LSP.Server_Notification_Receivers;
with LSP.Servers;
with LSP.Types;

package LSP.GPR_Handlers is

   type Message_Handler
     (Server  : access LSP.Servers.Server;
      Trace   : GNATCOLL.Traces.Trace_Handle) is
   limited new LSP.Server_Request_Handlers.Server_Request_Handler
     and LSP.Server_Notification_Receivers.Server_Notification_Receiver
     and LSP.GPR_Documents.Document_Provider
     and LSP.GPR_Files.File_Provider
   with private;
   --  A handler of LSP notifications and requests from GPR language
   --  A LSP notifications/requests handler for GPR language

   function From_File
     (Self : Message_Handler'Class;
      File : GNATCOLL.VFS.Virtual_File) return LSP.Messages.DocumentUri;
   --  Turn Virtual_File to URI

   function To_File
     (Self : Message_Handler'Class;
      URI  : LSP.Types.LSP_URI) return GNATCOLL.VFS.Virtual_File;
   --  Turn URI into Virtual_File

   function To_File
     (URI             : LSP.Types.LSP_URI;
      Follow_Symlinks : Boolean) return GPR2.Path_Name.Object;
   --  Turn URI into GPR2 path object.

private

   type Internal_Document_Access is access all LSP.GPR_Documents.Document;

   procedure Free (Self : in out Internal_Document_Access);
   --  Free all the data for the given document.

   --  Container for documents indexed by URI (diagnostics request)
   package Document_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => GNATCOLL.VFS.Virtual_File,
      Element_Type    => Internal_Document_Access,
      Hash            => GNATCOLL.VFS.Full_Name_Hash,
      Equivalent_Keys => GNATCOLL.VFS."=");

   type Internal_File_Access is access all LSP.GPR_Files.File;

   --  Container for documents indexed by URI (others request)
   package Files_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => GPR2.Path_Name.Object,
      Element_Type    => Internal_File_Access,
      Hash            => GPR2.Path_Name.Hash,
      Equivalent_Keys => GPR2.Path_Name."=");

   type Get_Symbol_Access is access procedure
     (Provider : LSP.GPR_Files.File_Provider_Access;
      Request  : LSP.Messages.Server_Requests.Document_Symbols_Request;
      Result   : out LSP.Messages.Symbol_Vector);
   --  textDocument/documentSymbol handler

   type Message_Handler
     (Server  : access LSP.Servers.Server;
      Trace   : GNATCOLL.Traces.Trace_Handle) is
   limited new LSP.Server_Request_Handlers.Server_Request_Handler
     and LSP.Server_Notification_Receivers.Server_Notification_Receiver
     and LSP.GPR_Documents.Document_Provider
     and LSP.GPR_Files.File_Provider
   with record
      Client_Settings : LSP.Messages.InitializeParams;
      --  Client information from the initialization request

      Open_Documents : Document_Maps.Map;
      --  The documents that are currently open

      Parsed_Files : Files_Maps.Map;
      --  open document & related files (imported, extended, aggregated)

      Get_Symbols : Get_Symbol_Access;
      --  textDocument/documentSymbol handler. Actual value depends on
      --  client's capabilities.

      Follow_Symlinks : Boolean := True;
      --  False if the client disables symlink following. In this case
      --  URIs from client should match file names reported by GPR2.Path_Name

      Diagnostics_Enabled : Boolean := True;
      --  Whether to publish diagnostics

      File_Reader : GPR2.File_Readers.File_Reader_Reference;
   end record;

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
      Value : LSP.Messages.CreateFilesParams) is null;

   overriding function On_Workspace_Will_Rename_Files_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.
                  Workspace_Will_Rename_Files_Request)
      return LSP.Messages.Server_Responses.WillRenameFiles_Response;

   overriding procedure On_DidRenameFiles_Notification
     (Self  : access Message_Handler;
      Value : LSP.Messages.RenameFilesParams) is null;

   overriding function On_Workspace_Will_Delete_Files_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.
                  Workspace_Will_Delete_Files_Request)
      return LSP.Messages.Server_Responses.WillDeleteFiles_Response;

   overriding procedure On_DidDeleteFiles_Notification
     (Self  : access Message_Handler;
      Value : LSP.Messages.DeleteFilesParams) is null;

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
     (Self  : access Message_Handler) is null;

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
      Value : LSP.Messages.DidChangeConfigurationParams) is null;

   overriding procedure On_DidChangeWorkspaceFolders_Notification
     (Self  : access Message_Handler;
      Value : LSP.Messages.DidChangeWorkspaceFoldersParams) is null;

   overriding procedure On_DidChangeWatchedFiles_Notification
     (Self  : access Message_Handler;
      Value : LSP.Messages.DidChangeWatchedFilesParams) is null;

   overriding procedure On_Cancel_Notification
     (Self  : access Message_Handler;
      Value : LSP.Messages.CancelParams) is null;
   --  This is intentionally null procedure, because cancel is implemented by
   --  LSP server itself.

   overriding procedure On_SetTrace_Notification
     (Self  : access Message_Handler;
      Value : LSP.Messages.SetTraceParams) is null;

   overriding function On_ALS_Check_Syntax_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.ALS_Check_Syntax_Request)
      return LSP.Messages.Server_Responses.ALS_Check_Syntax_Response;

   overriding function On_GLS_Mains_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.GLS_Mains_Request)
      return LSP.Messages.Server_Responses.GLS_Mains_Response;

   overriding function On_GLS_Executables_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.GLS_Executables_Request)
      return LSP.Messages.Server_Responses.GLS_Executables_Response;

   -----------------------------------------
   -- LSP.GPR_Documents.Document_Provider --
   -----------------------------------------

   overriding function Get_Open_Document
     (Self  : access Message_Handler;
      URI   : LSP.Messages.DocumentUri;
      Force : Boolean := False)
      return LSP.GPR_Documents.Document_Access;
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

   ---------------------------------
   -- LSP.GPR_Files.File_Provider --
   ---------------------------------

   overriding function Get_Parsed_File
     (Self  : access Message_Handler;
      Path  : GPR2.Path_Name.Object)
      return LSP.GPR_Files.File_Access;
   --  Return the parsed file for the given URI.
   --  If the file is not yet parsed, then it is parsed.

   overriding function Follow_Symlinks
     (Self : access Message_Handler) return Boolean is
     (Self.Follow_Symlinks);
   --  Return False if the client disables symlink following.

   overriding function Is_Openened_Document
     (Self : access Message_Handler;
      File  : GNATCOLL.VFS.Virtual_File) return Boolean is
     (Self.Open_Documents.Contains (File));

   overriding function Get_File_Reader
     (Self : access Message_Handler)
      return GPR2.File_Readers.File_Reader_Reference is (Self.File_Reader);

end LSP.GPR_Handlers;
