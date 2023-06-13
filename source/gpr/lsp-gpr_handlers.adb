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

with Ada.Unchecked_Deallocation;

with GPR2.Log;
with GPR2.Message;
with GPR2.Path_Name.Set;
with GPR2.Source_Reference;

with LSP.Errors;
with LSP.GPR_File_Readers;
with LSP.GPR_Files.Symbols;
with LSP.Messages.Server_Notifications;

with URIs;

with VSS.Strings.Conversions; use VSS.Strings.Conversions;
with VSS.Unicode;

package body LSP.GPR_Handlers is

   Allow_Incremental_Text_Changes : constant GNATCOLL.Traces.Trace_Handle :=
     GNATCOLL.Traces.Create ("ALS.ALLOW_INCREMENTAL_TEXT_CHANGES",
                             GNATCOLL.Traces.On);
   --  Trace to activate the support for incremental text changes.

   procedure Publish_Diagnostics
     (Self     : access Message_Handler'Class;
      Document : not null LSP.GPR_Documents.Document_Access);
   --  Publish diagnostic messages for given document if needed

   procedure Log_Unexpected_Null_Document
     (Self     : access Message_Handler;
      Where    : String);
   --  Log a message saying we unexpectedly couldn't find an open document

   function To_Optional_DiagnosticSeverity
     (Level : GPR2.Message.Level_Value)
      return LSP.Messages.Optional_DiagnosticSeverity;

   function To_Span
     (Sloc : GPR2.Source_Reference.Object) return LSP.Messages.Span;

   ----------
   -- Free --
   ----------

   procedure Free (Self : in out Internal_Document_Access) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (LSP.GPR_Documents.Document, Internal_Document_Access);
   begin
      Self.Cleanup;
      Unchecked_Free (Self);
   end Free;

   ---------------
   -- From_File --
   ---------------

   function From_File
     (Self : Message_Handler'Class;
      File : GNATCOLL.VFS.Virtual_File) return LSP.Messages.DocumentUri is
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

   -------------
   -- To_File --
   -------------

   function To_File
     (URI             : LSP.Types.LSP_URI;
      Follow_Symlinks : Boolean) return GPR2.Path_Name.Object
   is
      To     : constant URIs.URI_String := LSP.Types.To_UTF_8_String (URI);
      Result : constant String := URIs.Conversions.To_File
        (To, Normalize => Follow_Symlinks);
   begin
      return GPR2.Path_Name.Create_File (GPR2.Filename_Type (Result));
   end To_File;

   -----------------------
   -- Get_Open_Document --
   -----------------------

   overriding function Get_Open_Document
     (Self  : access Message_Handler;
      URI   : LSP.Messages.DocumentUri;
      Force : Boolean := False)
      return LSP.GPR_Documents.Document_Access
   is
      File : constant GNATCOLL.VFS.Virtual_File := Self.To_File (URI);
   begin

      if Self.Open_Documents.Contains (File) then
         return LSP.GPR_Documents.Document_Access
           (Self.Open_Documents.Element (File));
      elsif Force then
         declare
            Document : constant Internal_Document_Access :=
              new LSP.GPR_Documents.Document (Self.Trace);
         begin
            Document.Initialize (URI,
                                 GPR2.Path_Name.Create (File),
                                 VSS.Strings.Empty_Virtual_String,
                                 Self);
            return LSP.GPR_Documents.Document_Access (Document);
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
      Target_Text_Document : constant LSP.GPR_Documents.Document_Access :=
        Self.Get_Open_Document (URI);

      use type LSP.GPR_Documents.Document_Access;

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

   ---------------------
   -- Get_Parsed_File --
   ---------------------

   overriding function Get_Parsed_File
     (Self  : access Message_Handler;
      Path  : GPR2.Path_Name.Object)
      return LSP.GPR_Files.File_Access is
      C : constant LSP.GPR_Handlers.Files_Maps.Cursor :=
        Self.Parsed_Files.Find (Path);
   begin
      if LSP.GPR_Handlers.Files_Maps.Has_Element (C) then
         return LSP.GPR_Files.File_Access
           (LSP.GPR_Handlers.Files_Maps.Element (C));
      else
         declare
            Parsed_File : constant Internal_File_Access :=
                            new LSP.GPR_Files.File (Self.Trace);
         begin
            Parsed_File.Initialize
              (Path          => Path,
               File_Provider => Self);

            Self.Parsed_Files.Insert (Path, Parsed_File);
            return LSP.GPR_Files.File_Access (Parsed_File);
         end;
      end if;
   end Get_Parsed_File;

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
   -- On_GLS_Executables_Request --
   --------------------------------

   overriding function On_GLS_Executables_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.GLS_Executables_Request)
      return LSP.Messages.Server_Responses.GLS_Executables_Response is
   begin
      return raise Program_Error with "Unimplemented request";
   end On_GLS_Executables_Request;

   --------------------------
   -- On_GLS_Mains_Request --
   --------------------------

   overriding function On_GLS_Mains_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.GLS_Mains_Request)
      return LSP.Messages.Server_Responses.GLS_Mains_Response is
   begin
      return raise Program_Error with "Unimplemented request";
   end On_GLS_Mains_Request;

   ---------------------------------
   -- On_ALS_Check_Syntax_Request --
   ---------------------------------

   overriding function On_ALS_Check_Syntax_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.ALS_Check_Syntax_Request)
      return LSP.Messages.Server_Responses.ALS_Check_Syntax_Response is
   begin
      return raise Program_Error with "Unimplemented request";
   end On_ALS_Check_Syntax_Request;

   --------------------------
   -- On_ALS_Debug_Request --
   --------------------------

   overriding function On_ALS_Debug_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.ALS_Debug_Request)
      return LSP.Messages.Server_Responses.ALS_Debug_Response is
   begin
      return raise Program_Error with "Unimplemented request";
   end On_ALS_Debug_Request;

   --------------------------------------
   -- On_ALS_Show_Dependencies_Request --
   --------------------------------------

   overriding function On_ALS_Show_Dependencies_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.ALS_Show_Dependencies_Request)
      return LSP.Messages.Server_Responses.ALS_ShowDependencies_Response is
   begin
      return raise Program_Error with "Unimplemented request";
   end On_ALS_Show_Dependencies_Request;

   --------------------------------
   -- On_ALS_Source_Dirs_Request --
   --------------------------------

   overriding function On_ALS_Source_Dirs_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.ALS_Source_Dirs_Request)
      return LSP.Messages.Server_Responses.ALS_SourceDirs_Response is
   begin
      return raise Program_Error with "Unimplemented request";
   end On_ALS_Source_Dirs_Request;

   ---------------------------
   -- On_CodeAction_Request --
   ---------------------------

   overriding function On_CodeAction_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.CodeAction_Request)
      return LSP.Messages.Server_Responses.CodeAction_Response is
   begin
      return raise Program_Error with "Unimplemented request";
   end On_CodeAction_Request;

   -----------------------------------
   -- On_Color_Presentation_Request --
   -----------------------------------

   overriding function On_Color_Presentation_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Color_Presentation_Request)
      return LSP.Messages.Server_Responses.ColorPresentation_Response is
   begin
      return raise Program_Error with "Unimplemented request";
   end On_Color_Presentation_Request;

   ---------------------------
   -- On_Completion_Request --
   ---------------------------

   overriding function On_Completion_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Completion_Request)
      return LSP.Messages.Server_Responses.Completion_Response is
   begin
      return raise Program_Error with "Unimplemented request";
   end On_Completion_Request;

   --------------------------------------
   -- On_CompletionItemResolve_Request --
   --------------------------------------

   overriding function On_CompletionItemResolve_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.CompletionItemResolve_Request)
      return LSP.Messages.Server_Responses.CompletionItemResolve_Response is
   begin
      return raise Program_Error with "Unimplemented request";
   end On_CompletionItemResolve_Request;

   ----------------------------
   -- On_Declaration_Request --
   ----------------------------

   overriding function On_Declaration_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Declaration_Request)
      return LSP.Messages.Server_Responses.Location_Link_Response is
   begin
      return raise Program_Error with "Unimplemented request";
   end On_Declaration_Request;

   ---------------------------
   -- On_Definition_Request --
   ---------------------------

   overriding function On_Definition_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Definition_Request)
      return LSP.Messages.Server_Responses.Location_Link_Response is
   begin
      return raise Program_Error with "Unimplemented request";
   end On_Definition_Request;

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

            use type GNATCOLL.VFS.Virtual_File;
         begin
            if Object_File /= Value_File then
               return False;
            end if;
         end;

         return True;
      end Skip_Did_Change;

      Document : constant LSP.GPR_Documents.Document_Access :=
        Get_Open_Document (Self, Value.textDocument.uri);

      use type LSP.GPR_Documents.Document_Access;
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

      --  Load gpr tree & prepare diagnostics
      Document.Load;

      --  Build GPR file for LSP needs.
      LSP.GPR_Files.Parse_Modified_Document
        (File_Provider => Self,
         Path          =>
           To_File (Value.textDocument.uri, Self.Follow_Symlinks));

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

         Free (Document);

         --  Build GPR file for LSP needs using disk content.
         LSP.GPR_Files.Parse_Modified_Document
           (File_Provider => Self,
            Path          =>
              To_File (Value.textDocument.uri, Self.Follow_Symlinks));

      else
         --  We have received a didCloseTextDocument but the document was
         --  not open: this is not supposed to happen, log it.

         Self.Trace.Trace
           ("received a didCloseTextDocument for non-open document with uri: "
            & LSP.Types.To_UTF_8_String (URI));
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
        new LSP.GPR_Documents.Document (Self.Trace);
   begin
      Self.Trace.Trace ("In Text_Document_Did_Open");
      Self.Trace.Trace ("Uri : " & LSP.Types.To_UTF_8_String (URI));

      --  We have received a document: add it to the documents container
      Object.Initialize (URI,
                         GPR2.Path_Name.Create (Self.To_File (URI)),
                         Value.textDocument.text,
                         Self);

      Self.Open_Documents.Include (File, Object);

      --  Load gpr tree & prepare diagnostics
      Object.Load;

      --  Build GPR file for LSP needs.
      LSP.GPR_Files.Parse_Modified_Document
        (File_Provider => Self,
         Path          =>
           To_File (Value.textDocument.uri, Self.Follow_Symlinks));

      --  Emit diagnostics
      Self.Publish_Diagnostics (LSP.GPR_Documents.Document_Access (Object));

      Self.Trace.Trace ("Finished Text_Document_Did_Open");
   end On_DidOpenTextDocument_Notification;

   -------------------------------
   -- On_Document_Color_Request --
   -------------------------------

   overriding function On_Document_Color_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Document_Color_Request)
      return LSP.Messages.Server_Responses.DocumentColor_Response is
   begin
      return raise Program_Error with "Unimplemented request";
   end On_Document_Color_Request;

   -------------------------------
   -- On_Document_Links_Request --
   -------------------------------

   overriding function On_Document_Links_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Document_Links_Request)
      return LSP.Messages.Server_Responses.Links_Response is
   begin
      return raise Program_Error with "Unimplemented request";
   end On_Document_Links_Request;

   -------------------------------------
   -- On_Document_Tokens_Full_Request --
   -------------------------------------

   overriding function On_Document_Tokens_Full_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Document_Tokens_Full_Request)
      return LSP.Messages.Server_Responses.SemanticTokens_Response is
   begin
      return raise Program_Error with "Unimplemented request";
   end On_Document_Tokens_Full_Request;

   --------------------------------------
   -- On_Document_Tokens_Range_Request --
   --------------------------------------

   overriding function On_Document_Tokens_Range_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Document_Tokens_Range_Request)
      return LSP.Messages.Server_Responses.SemanticTokens_Response
   is
      pragma Unreferenced (Self, Request);

      Response : LSP.Messages.Server_Responses.SemanticTokens_Response
        (Is_Error => True);
   begin
      Response.error :=
        (True,
         (code => LSP.Errors.InternalError,
          message => "Not implemented",
          data => <>));
      return Response;
   end On_Document_Tokens_Range_Request;

   ---------------------------------
   -- On_Document_Symbols_Request --
   ---------------------------------

   overriding function On_Document_Symbols_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Document_Symbols_Request)
      return LSP.Messages.Server_Responses.Symbol_Response is

      Result   : LSP.Messages.Server_Responses.Symbol_Response :=
        (Is_Error => False,
         result   => <>,
         error    => (Is_Set => False),
         others   => <>);

   begin
      Self.Get_Symbols (Self, Request, Result.result);
      return Result;
   end On_Document_Symbols_Request;

   --------------------------------
   -- On_Execute_Command_Request --
   --------------------------------

   overriding function On_Execute_Command_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Execute_Command_Request)
      return LSP.Messages.Server_Responses.ExecuteCommand_Response is
   begin
      return raise Program_Error with "Unimplemented request";
   end On_Execute_Command_Request;

   ------------------------------
   -- On_Folding_Range_Request --
   ------------------------------

   overriding function On_Folding_Range_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Folding_Range_Request)
      return LSP.Messages.Server_Responses.FoldingRange_Response is
   begin
      return raise Program_Error with "Unimplemented request";
   end On_Folding_Range_Request;

   ---------------------------
   -- On_Formatting_Request --
   ---------------------------

   overriding function On_Formatting_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Formatting_Request)
      return LSP.Messages.Server_Responses.Formatting_Response is
   begin
      return raise Program_Error with "Unimplemented request";
   end On_Formatting_Request;

   --------------------------
   -- On_Highlight_Request --
   --------------------------

   overriding function On_Highlight_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Highlight_Request)
      return LSP.Messages.Server_Responses.Highlight_Response is
   begin
      return raise Program_Error with "Unimplemented request";
   end On_Highlight_Request;

   ----------------------
   -- On_Hover_Request --
   ----------------------

   overriding function On_Hover_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Hover_Request)
      return LSP.Messages.Server_Responses.Hover_Response is
   begin
      return raise Program_Error with "Unimplemented request";
   end On_Hover_Request;

   -------------------------------
   -- On_Implementation_Request --
   -------------------------------

   overriding function On_Implementation_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Implementation_Request)
      return LSP.Messages.Server_Responses.Location_Link_Response is
   begin
      return raise Program_Error with "Unimplemented request";
   end On_Implementation_Request;

   -------------------------------
   -- On_Incoming_Calls_Request --
   -------------------------------

   overriding function On_Incoming_Calls_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Incoming_Calls_Request)
      return LSP.Messages.Server_Responses.IncomingCalls_Response is
   begin
      return raise Program_Error with "Unimplemented request";
   end On_Incoming_Calls_Request;

   ---------------------------
   -- On_Initialize_Request --
   ---------------------------

   overriding function On_Initialize_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Initialize_Request)
      return LSP.Messages.Server_Responses.Initialize_Response
   is
      use all type LSP.Types.Optional_Boolean;

      Value : LSP.Messages.InitializeParams renames Request.params;

      Response : LSP.Messages.Server_Responses.Initialize_Response
        (Is_Error => False);
   begin
      Self.Client_Settings := Value;

      Self.File_Reader := LSP.GPR_File_Readers.Create (Self);

      Response.result.capabilities.textDocumentSync :=
        (Is_Set    => True,
         Is_Number => False,
         Options   =>
           (openClose => True,
            change    => (True, LSP.Messages.Full),
            others    => <>));

      Response.result.capabilities.documentSymbolProvider :=
        (Is_Set => True,
         Value  => (workDoneProgress => LSP.Types.None, label => <>));

      if Value.capabilities.textDocument.documentSymbol.Is_Set
        and then Value.capabilities.textDocument.documentSymbol.Value
          .hierarchicalDocumentSymbolSupport = True
      then
         Self.Get_Symbols :=
           LSP.GPR_Files.Symbols.Get_Symbols_Hierarchy'Access;
      else
         Self.Get_Symbols :=
           LSP.GPR_Files.Symbols.Get_Symbols'Access;
      end if;

      return Response;
   end On_Initialize_Request;

   -------------------------------
   -- On_Outgoing_Calls_Request --
   -------------------------------

   overriding function On_Outgoing_Calls_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Outgoing_Calls_Request)
      return LSP.Messages.Server_Responses.OutgoingCalls_Response is
   begin
      return raise Program_Error with "Unimplemented request";
   end On_Outgoing_Calls_Request;

   ---------------------------------------
   -- On_Prepare_Call_Hierarchy_Request --
   ---------------------------------------

   overriding function On_Prepare_Call_Hierarchy_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Prepare_Call_Hierarchy_Request)
      return LSP.Messages.Server_Responses.PrepareCallHierarchy_Response is
   begin
      return raise Program_Error with "Unimplemented request";
   end On_Prepare_Call_Hierarchy_Request;

   -------------------------------
   -- On_Prepare_Rename_Request --
   -------------------------------

   overriding function On_Prepare_Rename_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Prepare_Rename_Request)
      return LSP.Messages.Server_Responses.Prepare_Rename_Response is
   begin
      return raise Program_Error with "Unimplemented request";
   end On_Prepare_Rename_Request;

   ---------------------------------
   -- On_Range_Formatting_Request --
   ---------------------------------

   overriding function On_Range_Formatting_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Range_Formatting_Request)
      return LSP.Messages.Server_Responses.Range_Formatting_Response is
   begin
      return raise Program_Error with "Unimplemented request";
   end On_Range_Formatting_Request;

   -----------------------------------
   -- On_On_Type_Formatting_Request --
   -----------------------------------

   overriding function On_On_Type_Formatting_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.On_Type_Formatting_Request)
      return LSP.Messages.Server_Responses.On_Type_Formatting_Response is
   begin
      return raise Program_Error with "Unimplemented request";
   end On_On_Type_Formatting_Request;

   ---------------------------
   -- On_References_Request --
   ---------------------------

   overriding function On_References_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.References_Request)
      return LSP.Messages.Server_Responses.Location_Response is
   begin
      return raise Program_Error with "Unimplemented request";
   end On_References_Request;

   -----------------------
   -- On_Rename_Request --
   -----------------------

   overriding function On_Rename_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Rename_Request)
      return LSP.Messages.Server_Responses.Rename_Response is
   begin
      return raise Program_Error with "Unimplemented request";
   end On_Rename_Request;

   --------------------------------
   -- On_Selection_Range_Request --
   --------------------------------

   overriding function On_Selection_Range_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Selection_Range_Request)
      return LSP.Messages.Server_Responses.SelectionRange_Response is
   begin
      return raise Program_Error with "Unimplemented request";
   end On_Selection_Range_Request;

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
      return Response : LSP.Messages.Server_Responses.Shutdown_Response
        (Is_Error => False);
   end On_Shutdown_Request;

   -------------------------------
   -- On_Signature_Help_Request --
   -------------------------------

   overriding function On_Signature_Help_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Signature_Help_Request)
      return LSP.Messages.Server_Responses.SignatureHelp_Response is
   begin
      return raise Program_Error with "Unimplemented request";
   end On_Signature_Help_Request;

   --------------------------------
   -- On_Type_Definition_Request --
   --------------------------------

   overriding function On_Type_Definition_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Type_Definition_Request)
      return LSP.Messages.Server_Responses.Location_Link_Response is
   begin
      return raise Program_Error with "Unimplemented request";
   end On_Type_Definition_Request;

   ------------------------------------------
   -- On_Workspace_Execute_Command_Request --
   ------------------------------------------

   overriding function On_Workspace_Execute_Command_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Workspace_Execute_Command_Request)
      return LSP.Messages.Server_Responses.ExecuteCommand_Response is
   begin
      return raise Program_Error with "Unimplemented request";
   end On_Workspace_Execute_Command_Request;

   ----------------------------------
   -- On_Workspace_Symbols_Request --
   ----------------------------------

   overriding function On_Workspace_Symbols_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Workspace_Symbols_Request)
      return LSP.Messages.Server_Responses.Symbol_Response is
   begin
      return raise Program_Error with "Unimplemented request";
   end On_Workspace_Symbols_Request;

   --------------------------------------------
   -- On_Workspace_Will_Create_Files_Request --
   --------------------------------------------

   overriding function On_Workspace_Will_Create_Files_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests
        .Workspace_Will_Create_Files_Request)
      return LSP.Messages.Server_Responses.WillCreateFiles_Response is
   begin
      return raise Program_Error with "Unimplemented request";
   end On_Workspace_Will_Create_Files_Request;

   --------------------------------------------
   -- On_Workspace_Will_Delete_Files_Request --
   --------------------------------------------

   overriding function On_Workspace_Will_Delete_Files_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests
        .Workspace_Will_Delete_Files_Request)
      return LSP.Messages.Server_Responses.WillDeleteFiles_Response is
   begin
      return raise Program_Error with "Unimplemented request";
   end On_Workspace_Will_Delete_Files_Request;

   --------------------------------------------
   -- On_Workspace_Will_Rename_Files_Request --
   --------------------------------------------

   overriding function On_Workspace_Will_Rename_Files_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests
        .Workspace_Will_Rename_Files_Request)
      return LSP.Messages.Server_Responses.WillRenameFiles_Response is
   begin
      return raise Program_Error with "Unimplemented request";
   end On_Workspace_Will_Rename_Files_Request;

   -------------------------
   -- Publish_Diagnostics --
   -------------------------

   procedure Publish_Diagnostics
     (Self     : access Message_Handler'Class;
      Document : not null LSP.GPR_Documents.Document_Access)
   is
      Changed          : Boolean;
      Msg_Map          : LSP.GPR_Documents.Message_Map;

      Files_With_Diags : GPR2.Path_Name.Set.Object;
      --  Used to update Document.Files_With_Diags

   begin
      if Self.Diagnostics_Enabled then
         Document.Get_Errors
           (Root_File => GPR2.Path_Name.Create (Self.To_File (Document.URI)),
            Changed   => Changed,
            Errors    => Msg_Map);

         if Changed then
            for C in Msg_Map.Iterate loop
               declare
                  File : constant GPR2.Path_Name.Object :=
                            LSP.GPR_Documents.Message_Maps.Key (C);
                  Log  : constant GPR2.Log.Object :=
                            LSP.GPR_Documents.Message_Maps.Element (C);
                  Diag : LSP.Messages.PublishDiagnosticsParams;

                  Diag_Empty : Boolean := True;

               begin
                  if not File.Is_Defined then
                     Diag.uri := Document.URI;
                  else
                     Diag.uri := LSP.Types.File_To_URI (File.Value);
                  end if;
                  for C in Log.Iterate loop
                     declare
                        Message    : constant GPR2.Message.Object := C.Element;
                        Diagnostic : LSP.Messages.Diagnostic;
                     begin
                        Diagnostic.span := To_Span (Message.Sloc);
                        Diagnostic.severity :=
                          To_Optional_DiagnosticSeverity (Message.Level);
                        Diagnostic.message :=
                          VSS.Strings.Conversions.To_Virtual_String
                            (Message.Message);
                        Diag.diagnostics.Append (Diagnostic);
                        Diag_Empty := False;
                     end;
                  end loop;
                  if not Diag_Empty then
                     Files_With_Diags.Append (File);
                  end if;
                  Self.Server.On_Publish_Diagnostics (Diag);
               end;
            end loop;
            Document.Update_Files_With_Diags (Files_With_Diags);
         end if;
      end if;
   end Publish_Diagnostics;

   ------------------------------------
   -- To_Optional_DiagnosticSeverity --
   ------------------------------------

   function To_Optional_DiagnosticSeverity
     (Level : GPR2.Message.Level_Value)
      return LSP.Messages.Optional_DiagnosticSeverity
   is
      use GPR2.Message;
   begin
      case Level is
         when Information =>
            return (True, LSP.Messages.Information);
         when Warning =>
            return (True, LSP.Messages.Warning);
         when Error =>
            return (True, LSP.Messages.Error);
         when Lint =>
            return (True, LSP.Messages.Hint);
      end case;
   end To_Optional_DiagnosticSeverity;

   -------------
   -- To_Span --
   -------------

   function To_Span
     (Sloc : GPR2.Source_Reference.Object) return LSP.Messages.Span
   is
      use type VSS.Unicode.UTF16_Code_Unit_Count;
      use LSP.Types;

      Result : constant LSP.Messages.Span :=
        (if Sloc.Is_Defined and then Sloc.Has_Source_Reference then
           (first =>
                (line      => LSP.Types.Line_Number (Sloc.Line) - 1,
                 character => LSP.Types.UTF_16_Index   --  FIXME (UTF16 index)!
                   (Sloc.Column) - 1),
            last =>
              (line => LSP.Types.Line_Number (Sloc.Line) - 1,
               character => LSP.Types.UTF_16_Index  --  FIXME (UTF16 index)!
                 (Sloc.Column) - 1))
         else LSP.Messages.Empty_Span);
   begin
      return Result;
   end To_Span;

end LSP.GPR_Handlers;
