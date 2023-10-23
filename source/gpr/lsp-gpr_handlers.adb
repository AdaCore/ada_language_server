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

with Ada.Exceptions;
with Ada.Unchecked_Deallocation;

with GNATCOLL.Traces;

with GPR2.Log;
with GPR2.Message;
with GPR2.Path_Name.Set;
with GPR2.Source_Reference;

with LSP.Constants;
with LSP.Enumerations;
with LSP.Generic_Cancel_Check;
with LSP.GPR_Documentation;
with LSP.GPR_File_Readers;
with LSP.GPR_Files.Symbols;
with LSP.Servers;
with LSP.Server_Notifications.DidChange;
with URIs;

with VSS.Strings.Conversions;

package body LSP.GPR_Handlers is

   Allow_Incremental_Text_Changes : constant GNATCOLL.Traces.Trace_Handle :=
     GNATCOLL.Traces.Create ("ALS.ALLOW_INCREMENTAL_TEXT_CHANGES",
                             GNATCOLL.Traces.On);
   --  Trace to activate the support for incremental text changes.

   function To_File
     (Self : Message_Handler'Class;
      Item : LSP.Structures.DocumentUri) return GNATCOLL.VFS.Virtual_File;
   --  Turn URI into Virtual_File

   function To_File
     (Self : Message_Handler'Class;
      Item : LSP.Structures.DocumentUri) return GPR2.Path_Name.Object;
   --  Turn URI into GPR2 path object.

   function To_URI
     (Self : Message_Handler'Class;
      Item : GPR2.Path_Name.Object) return LSP.Structures.DocumentUri;
   --  Turn GPR2 path object into URI.

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
      return LSP.Structures.DiagnosticSeverity_Optional;

   function To_Range
     (Sloc : GPR2.Source_Reference.Object) return LSP.Structures.A_Range;

   procedure Free (Self : in out Internal_Document_Access);
   --  Free all the data for the given document.

   ----------
   -- Free --
   ----------

   procedure Free (Self : in out Internal_Document_Access) is

      procedure Unchecked_Free is
        new Ada.Unchecked_Deallocation
              (LSP.GPR_Documents.Document, Internal_Document_Access);

   begin
      Self.Cleanup;
      Unchecked_Free (Self);
   end Free;

   -----------------------
   -- Get_Open_Document --
   -----------------------

   overriding function Get_Open_Document
     (Self  : access Message_Handler;
      URI   : LSP.Structures.DocumentUri;
      Force : Boolean := False) return LSP.GPR_Documents.Document_Access
   is
      File : constant GNATCOLL.VFS.Virtual_File := Self.To_File (URI);

   begin
      if Self.Open_Documents.Contains (File) then
         return
           LSP.GPR_Documents.Document_Access
             (Self.Open_Documents.Element (File));

      elsif Force then
         declare
            Document : constant Internal_Document_Access :=
              new LSP.GPR_Documents.Document (Self.Tracer);
         begin
            Document.Initialize
              (URI,
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

   overriding function Get_Open_Document_Version
     (Self  : access Message_Handler;
      URI   : LSP.Structures.DocumentUri)
      return LSP.Structures.OptionalVersionedTextDocumentIdentifier
   is
      use type LSP.GPR_Documents.Document_Access;

      Target_Text_Document : constant LSP.GPR_Documents.Document_Access :=
        Self.Get_Open_Document (URI);

   begin
      --  If the target textDocument hasn't been opened in the editor
      --  then ALS hasn't received an open notification before. Therefore
      --  Target_Text_Document will be null.
      --  In that case, its VersionedTextDocumentIdentifier.version will
      --  be null.

      if Target_Text_Document = null then
         return (URI, (Is_Null => True));

      else
         return Target_Text_Document.Identifier;
      end if;
   end Get_Open_Document_Version;

   ---------------------
   -- Get_Parsed_File --
   ---------------------

   overriding function Get_Parsed_File
     (Self  : access Message_Handler;
      Path  : GPR2.Path_Name.Object)
      return LSP.GPR_Files.File_Access
   is
      C : constant LSP.GPR_Handlers.Files_Maps.Cursor :=
        Self.Parsed_Files.Find (Path);

   begin
      if LSP.GPR_Handlers.Files_Maps.Has_Element (C) then
         return LSP.GPR_Files.File_Access
           (LSP.GPR_Handlers.Files_Maps.Element (C));

      else
         declare
            Parsed_File : constant Internal_File_Access :=
              new LSP.GPR_Files.File (Self.Tracer);

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
      Self.Tracer.Trace ("Unexpected null document in " & Where);
   end Log_Unexpected_Null_Document;

   -------------------------------
   -- On_DidChange_Notification --
   -------------------------------

   overriding procedure On_DidChange_Notification
     (Self  : in out Message_Handler;
      Value : LSP.Structures.DidChangeTextDocumentParams)
   is
      use type GNATCOLL.VFS.Virtual_File;
      use type LSP.GPR_Documents.Document_Access;

      function Skip_Did_Change return Boolean;
      --  Check if the following message in the queue is didChange for
      --  the same document

      ---------------------
      -- Skip_Did_Change --
      ---------------------

      function Skip_Did_Change return Boolean is
         use type LSP.Servers.Server_Message_Access;

         subtype DidChange_Notification is
           LSP.Server_Notifications.DidChange.Notification;

         Next : constant LSP.Servers.Server_Message_Access :=
           LSP.Servers.Server'Class (Self.Sender.all).Look_Ahead_Message;

      begin
         if Next = null
           or else Next.all not in DidChange_Notification'Class
         then
            return False;
         end if;

         declare
            Object      : DidChange_Notification'Class renames
              DidChange_Notification'Class (Next.all);
            Object_File : constant GNATCOLL.VFS.Virtual_File :=
              Self.To_File (Object.Params.textDocument.uri);
            Value_File  : constant GNATCOLL.VFS.Virtual_File :=
              Self.To_File (Value.textDocument.uri);

         begin
            return Object_File = Value_File;
         end;
      end Skip_Did_Change;

      Document : constant LSP.GPR_Documents.Document_Access :=
        Self.Get_Open_Document (Value.textDocument.uri);

   begin
      if Document = null then
         Self.Log_Unexpected_Null_Document
           ("On_DidChangeTextDocument_Notification");
      end if;

      if Allow_Incremental_Text_Changes.Active then
         --  If we are applying incremental changes, we can't skip the
         --  call to Apply_Changes, since this would break synchronization.

         Document.Apply_Changes
           (Value.textDocument.version, Value.contentChanges);

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
           (Value.textDocument.version, Value.contentChanges);
      end if;

      --  Load gpr tree & prepare diagnostics

      Document.Load;

      --  Build GPR file for LSP needs.

      LSP.GPR_Files.Parse_Modified_Document
        (File_Provider => Self'Unchecked_Access,
         Path          => Self.To_File (Value.textDocument.uri));

      --  Emit diagnostics

      Self.Publish_Diagnostics (Document);
   end On_DidChange_Notification;

   ------------------------------
   -- On_DidClose_Notification --
   ------------------------------

   overriding procedure On_DidClose_Notification
     (Self  : in out Message_Handler;
      Value : LSP.Structures.DidCloseTextDocumentParams)
   is
      URI      : constant LSP.Structures.DocumentUri := Value.textDocument.uri;
      File     : constant GNATCOLL.VFS.Virtual_File  := Self.To_File (URI);
      Diag     : LSP.Structures.PublishDiagnosticsParams;
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
           (File_Provider => Self'Unchecked_Access,
            Path          => Self.To_File (Value.textDocument.uri));

      else
         --  We have received a didCloseTextDocument but the document was
         --  not open: this is not supposed to happen, log it.

         Self.Tracer.Trace
           ("received a didCloseTextDocument for non-open document with uri: "
            & VSS.Strings.Conversions.To_UTF_8_String (URI));
      end if;

      --  Clean diagnostics up on closing document

      if Self.Diagnostics_Enabled then
         Diag.uri := URI;
         Self.Sender.On_PublishDiagnostics_Notification (Diag);
      end if;
   end On_DidClose_Notification;

   -----------------------------
   -- On_DidOpen_Notification --
   -----------------------------

   overriding procedure On_DidOpen_Notification
     (Self  : in out Message_Handler;
      Value : LSP.Structures.DidOpenTextDocumentParams)
   is
      URI    : constant LSP.Structures.DocumentUri := Value.textDocument.uri;
      File   : constant GNATCOLL.VFS.Virtual_File  := Self.To_File (URI);
      Object : constant Internal_Document_Access   :=
        new LSP.GPR_Documents.Document (Self.Tracer);

   begin
      Self.Tracer.Trace ("In Text_Document_Did_Open");
      Self.Tracer.Trace
        ("Uri : " & VSS.Strings.Conversions.To_UTF_8_String (URI));

      --  We have received a document: add it to the documents container
      Object.Initialize
        (URI,
         GPR2.Path_Name.Create (Self.To_File (URI)),
         Value.textDocument.text,
         Self'Unchecked_Access);

      Self.Open_Documents.Include (File, Object);

      --  Load gpr tree & prepare diagnostics

      Object.Load;

      --  Build GPR file for LSP needs.

      LSP.GPR_Files.Parse_Modified_Document
        (File_Provider => Self'Unchecked_Access,
         Path          => Self.To_File (Value.textDocument.uri));

      --  Emit diagnostics

      Self.Publish_Diagnostics (LSP.GPR_Documents.Document_Access (Object));

      Self.Tracer.Trace ("Finished Text_Document_Did_Open");
   end On_DidOpen_Notification;

   -------------------------------
   -- On_DocumentSymbol_Request --
   -------------------------------

   overriding procedure On_DocumentSymbol_Request
     (Self  : in out Message_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.DocumentSymbolParams)
   is
      Response : LSP.Structures.DocumentSymbol_Result;

   begin
      if Self.Hierarchical_Symbols then
         Response := (Kind => LSP.Structures.Variant_2, others => <>);
         LSP.GPR_Files.Symbols.Get_Symbols_Hierarchy
           (Self'Unchecked_Access,
            Value.textDocument.uri,
            Self.To_File (Value.textDocument.uri),
            Response.Variant_2);

      else
         LSP.GPR_Files.Symbols.Get_Symbols
           (Self'Unchecked_Access,
            Value.textDocument.uri,
            Self.To_File (Value.textDocument.uri),
            Response.Variant_1);
      end if;

      Self.Sender.On_DocumentSymbol_Response (Id, Response);
   end On_DocumentSymbol_Request;

   ----------------------
   -- On_Hover_Request --
   ----------------------

   overriding procedure On_Hover_Request
     (Self  : in out Message_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.HoverParams)
   is

      Response : LSP.Structures.Hover_Or_Null;

      procedure Compute_Response;

      ----------------------
      -- Compute_Response --
      ----------------------

      procedure Compute_Response is
         File         : constant LSP.GPR_Files.File_Access :=
                           LSP.GPR_Files.Parse
                             (File_Provider => Self'Unchecked_Access,
                              Path          => Self.To_File
                                                 (Value.textDocument.uri));
         Tooltip_Text : VSS.Strings.Virtual_String;
      begin

         LSP.GPR_Documentation.Get_Tooltip_Text
           (Self         => File.all,
            Position     => Value.position,
            Tooltip_Text => Tooltip_Text);

         if Tooltip_Text.Is_Empty then
            return;
         end if;

         Response := (Is_Null => False, others => <>);
         Response.Value.contents := (Is_MarkupContent => False, others => <>);

         --  Append the package/attribute description

         Response.Value.contents.MarkedString_Vector.Append
           (LSP.Structures.MarkedString'
              (Is_Virtual_String => False,
               language          => "plaintext",
               value             => Tooltip_Text));

      end Compute_Response;

   begin
      Compute_Response;
      Self.Sender.On_Hover_Response (Id, Response);
   end On_Hover_Request;

   ---------------------------
   -- On_Initialize_Request --
   ---------------------------

   overriding procedure On_Initialize_Request
     (Self  : in out Message_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.InitializeParams)
   is
      Response : LSP.Structures.InitializeResult;

   begin
      Self.File_Reader := LSP.GPR_File_Readers.Create (Self'Unchecked_Access);

      Response.capabilities.textDocumentSync :=
        (Is_Set => True,
         Value  => (Is_TextDocumentSyncOptions => True,
                    TextDocumentSyncOptions =>
                      (openClose => (Is_Set => True, Value => True),
                       change    =>
                         (Is_Set => True, Value => LSP.Enumerations.Full),
                       others    => <>)));

      Response.capabilities.documentSymbolProvider :=
        (Is_Set => True,
         Value  =>
           (Is_Boolean => False,
            DocumentSymbolOptions =>
              (workDoneProgress => (Is_Set => False), label => <>)));

      Self.Hierarchical_Symbols :=
        Value.capabilities.textDocument.Is_Set
        and then Value.capabilities.textDocument.Value.documentSymbol.Is_Set
        and then Value.capabilities.textDocument.Value.documentSymbol.Value
          .hierarchicalDocumentSymbolSupport.Is_Set
        and then Value.capabilities.textDocument.Value.documentSymbol.Value
          .hierarchicalDocumentSymbolSupport.Value;

      Self.Sender.On_Initialize_Response (Id, Response);
   end On_Initialize_Request;

   ----------------------------
   -- On_Server_Notification --
   ----------------------------

   overriding procedure On_Server_Notification
     (Self  : in out Message_Handler;
      Value : LSP.Server_Notifications.Server_Notification'Class) is
   begin
      Value.Visit_Server_Receiver (Self);

   exception
      when E : others =>
         Self.Tracer.Trace_Exception (E, "On_Server_Notification");
   end On_Server_Notification;

   -----------------------
   -- On_Server_Request --
   -----------------------

   overriding procedure On_Server_Request
     (Self  : in out Message_Handler;
      Value : LSP.Server_Requests.Server_Request'Class)
   is
      package Canceled is new LSP.Generic_Cancel_Check (Value'Access, 127);

   begin
      if Value.Canceled then
         Self.Sender.On_Error_Response
           (Value.Id,
            (code    => LSP.Constants.RequestCancelled,
             message => "Request was canceled"));

         return;
      end if;

      Self.Implemented := True;
      Self.Is_Canceled := Canceled.Has_Been_Canceled'Unrestricted_Access;

      Value.Visit_Server_Receiver (Self);

      if not Self.Implemented then
         Self.Sender.On_Error_Response
           (Value.Id,
            (code    => LSP.Enumerations.MethodNotFound,
             message => "Not implemented"));
      end if;

   exception
      when E : others =>
         declare
            Message : constant VSS.Strings.Virtual_String :=
              VSS.Strings.Conversions.To_Virtual_String
                ("Exception: " &
                   Ada.Exceptions.Exception_Name (E) & " (" &
                     Ada.Exceptions.Exception_Message (E) & ")");

         begin
            Self.Tracer.Trace_Exception (E, "On_Server_Request");

            Self.Sender.On_Error_Response
              (Value.Id,
               (code    => LSP.Enumerations.InternalError,
                message => Message));
         end;
   end On_Server_Request;

   -------------------------
   -- On_Shutdown_Request --
   -------------------------

   overriding procedure On_Shutdown_Request
     (Self : in out Message_Handler;
      Id   : LSP.Structures.Integer_Or_Virtual_String)
   is
      Response : LSP.Structures.Null_Record;

   begin
      Self.Sender.On_Shutdown_Response (Id, Response);
   end On_Shutdown_Request;

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
                  File  : constant GPR2.Path_Name.Object :=
                    LSP.GPR_Documents.Message_Maps.Key (C);
                  Log   : constant GPR2.Log.Object :=
                    LSP.GPR_Documents.Message_Maps.Element (C);
                  Diag  : LSP.Structures.PublishDiagnosticsParams :=
                    (uri    =>
                       (if File.Is_Defined
                          then Self.To_URI (File) else Document.URI),
                     others => <>);

               begin
                  for C in Log.Iterate loop
                     declare
                        Message    : constant GPR2.Message.Object := C.Element;
                        Diagnostic : LSP.Structures.Diagnostic;

                     begin
                        Diagnostic.a_range := To_Range (Message.Sloc);
                        Diagnostic.severity :=
                          To_Optional_DiagnosticSeverity (Message.Level);
                        Diagnostic.message :=
                          VSS.Strings.Conversions.To_Virtual_String
                            (Message.Message);
                        Diag.diagnostics.Append (Diagnostic);
                     end;
                  end loop;

                  if not Diag.diagnostics.Is_Empty then
                     Files_With_Diags.Append (File);
                  end if;

                  Self.Sender.On_PublishDiagnostics_Notification (Diag);
               end;
            end loop;

            Document.Update_Files_With_Diags (Files_With_Diags);
         end if;
      end if;
   end Publish_Diagnostics;

   -------------
   -- To_File --
   -------------

   function To_File
     (Self : Message_Handler'Class;
      Item : LSP.Structures.DocumentUri) return GNATCOLL.VFS.Virtual_File
   is
     (GNATCOLL.VFS.Create_From_UTF8
        (URIs.Conversions.To_File
           (URI       => VSS.Strings.Conversions.To_UTF_8_String (Item),
            Normalize => Self.Follow_Symlinks)));

   -------------
   -- To_File --
   -------------

   function To_File
     (Self : Message_Handler'Class;
      Item : LSP.Structures.DocumentUri) return GPR2.Path_Name.Object
   is
     (GPR2.Path_Name.Create_File
        (GPR2.Filename_Type
             (URIs.Conversions.To_File
                  (URI       => VSS.Strings.Conversions.To_UTF_8_String (Item),
                   Normalize => Self.Follow_Symlinks))));

   ------------------------------------
   -- To_Optional_DiagnosticSeverity --
   ------------------------------------

   function To_Optional_DiagnosticSeverity
     (Level : GPR2.Message.Level_Value)
      return LSP.Structures.DiagnosticSeverity_Optional
   is
      use all type GPR2.Message.Level_Value;

   begin
      case Level is
         when Information =>
            return (True, LSP.Enumerations.Information);
         when Warning =>
            return (True, LSP.Enumerations.Warning);
         when Error =>
            return (True, LSP.Enumerations.Error);
         when Lint =>
            return (True, LSP.Enumerations.Hint);
      end case;
   end To_Optional_DiagnosticSeverity;

   --------------
   -- To_Range --
   --------------

   function To_Range
     (Sloc : GPR2.Source_Reference.Object) return LSP.Structures.A_Range
   is
      Result : constant LSP.Structures.A_Range :=
        (if Sloc.Is_Defined and then Sloc.Has_Source_Reference then
           (start  =>
              (line      => Sloc.Line - 1,
               character => Sloc.Column - 1),
               --  FIXME (UTF16 index)!
            an_end =>
              (line      => Sloc.Line - 1,
               character => Sloc.Column - 1))
               --  FIXME (UTF16 index)!
         else LSP.Constants.Empty);

   begin
      return Result;
   end To_Range;

   ------------
   -- To_URI --
   ------------

   function To_URI
     (Self : Message_Handler'Class;
      Item : GPR2.Path_Name.Object) return LSP.Structures.DocumentUri
   is
     (VSS.Strings.Conversions.To_Virtual_String
        (URIs.Conversions.From_File (Item.Value)) with null record);

end LSP.GPR_Handlers;
