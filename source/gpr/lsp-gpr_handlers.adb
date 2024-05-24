------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2022-2024, AdaCore                     --
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

with Langkit_Support.Slocs;

with LSP.GPR_Completions;
with LSP.Constants;
with LSP.Enumerations;
with LSP.Generic_Cancel_Check;
with LSP.GPR_Documentation;
with LSP.GPR_File_Readers;
with LSP.GPR_Files.References;
with LSP.GPR_Files.Symbols;
with LSP.Text_Documents.Langkit_Documents;
with LSP.Utils;

with Gpr_Parser.Common;

package body LSP.GPR_Handlers is

   function To_Range (File_Provider : LSP.GPR_Files.File_Provider_Access;
                      Reference : Gpr_Parser.Common.Token_Reference)
                         return LSP.Structures.A_Range;

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
      URI   : LSP.Structures.DocumentUri)
        return LSP.GPR_Documents.Document_Access
   is
      File : constant GNATCOLL.VFS.Virtual_File := Self.To_File (URI);

   begin
      if Self.Open_Documents.Contains (File) then
         return
           LSP.GPR_Documents.Document_Access
             (Self.Open_Documents.Element (File));
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

      begin
         Object.Load (Self.Get_Configuration);
      exception
         when E : others =>
            Self.Tracer.Trace_Exception (E, "On_DidOpen_Notification");
      end;

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
         Declaration_Text          : VSS.Strings.Virtual_String;
         Documentation_Text : VSS.Strings.Virtual_String;
         Location_Text      : VSS.Strings.Virtual_String;
      begin

         LSP.GPR_Documentation.Get_Tooltip_Text
           (Self               => File,
            URI                => Value.textDocument.uri,
            Document_Provider  => Self'Unchecked_Access,
            Position           => Value.position,
            Style              => Self.Configuration.Documentation_Style,
            Declaration_Text   => Declaration_Text,
            Documentation_Text => Documentation_Text,
            Location_Text      => Location_Text);

         if Declaration_Text.Is_Empty
           and then Documentation_Text.Is_Empty
           and then Location_Text.Is_Empty
         then
            return;
         end if;

         Response := (Is_Null => False, others => <>);
         Response.Value.contents := (Is_MarkupContent => False, others => <>);

         --  Append the whole declaration text to the response

         if not Declaration_Text.Is_Empty then
            Response.Value.contents.MarkedString_Vector.Append
              (LSP.Structures.MarkedString'
                 (Is_Virtual_String => False,
                  value             => Declaration_Text,
                  language          => "gpr"));
         end if;

         --  Append the location text to the response

         if not Location_Text.Is_Empty then
            Response.Value.contents.MarkedString_Vector.Append
              (LSP.Structures.MarkedString'
                 (Is_Virtual_String => True,
                  Virtual_String    => Location_Text));
         end if;

         --  Append the comments associated with the basic declaration if any.

         if not Documentation_Text.Is_Empty then
            Response.Value.contents.MarkedString_Vector.Append
              (LSP.Structures.MarkedString'
                 (Is_Virtual_String => False,
                  language          => "plaintext",
                  value             => Documentation_Text));
         end if;

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
      Response     : LSP.Structures.InitializeResult;
      Capabilities : LSP.Structures.ServerCapabilities;

   begin
      Self.File_Reader := LSP.GPR_File_Readers.Create (Self'Unchecked_Access);

      Self.Client.Initialize (Value);

      Capabilities.hoverProvider := LSP.Constants.True;
      Capabilities.definitionProvider := LSP.Constants.True;
      Capabilities.declarationProvider := LSP.Constants.True;
      Capabilities.completionProvider :=
        (Is_Set => True,
         Value  => (triggerCharacters => [" "],
                    resolveProvider   => LSP.Constants.True,
                    others            => <>));

      Response.capabilities := Capabilities;

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

   ---------------------------
   -- On_Completion_Request --
   ---------------------------

   overriding procedure On_Completion_Request
     (Self  : in out Message_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.CompletionParams)
   is
      --  If lazy computation for the 'detail' and 'documentation' fields is
      --  supported by the client, set the Compute_Doc_And_Details flag to
      --  False.
      Compute_Doc_And_Details : constant Boolean :=
        not Self.Client.Resolve_Lazily;

      Response : LSP.Structures.Completion_Result
        (Kind => LSP.Structures.Variant_2);
   begin
      Response.Variant_2.isIncomplete := False;

      LSP.GPR_Completions.Fill_Completion_Response
        (File_Provider           => Self'Unchecked_Access,
         Value                   => Value,
         Compute_Doc_And_Details => Compute_Doc_And_Details,
         Response                => Response);

      Self.Sender.On_Completion_Response (Id, Response);
   end On_Completion_Request;

   -----------------------------------
   -- On_Completion_Resolve_Request --
   -----------------------------------

   overriding procedure On_Completion_Resolve_Request
     (Self  : in out Message_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.CompletionItem)
   is
      Response : LSP.Structures.CompletionItem := Value;
   begin
      --  Return immediately if we don't have data that allows us to compute
      --  additional information for the given item.
      --  This is the case when all the completion item's fields have already
      --  been computed.
      if Value.data.Is_Empty then
         Self.Sender.On_Completion_Resolve_Response (Id, Value);
         return;
      end if;

      LSP.GPR_Completions.Fill_Completion_Resolve_Response
        (Response => Response);

      Self.Sender.On_Completion_Resolve_Response (Id, Response);
   end On_Completion_Resolve_Request;

   ----------------------------
   -- On_Declaration_Request --
   ----------------------------

   overriding procedure On_Declaration_Request
     (Self  : in out Message_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.DeclarationParams)
   is
      procedure Fill_Declaration;
      --  Utility function, appends to Vector the definition if any.

      Response : LSP.Structures.Declaration_Result  (LSP.Structures.Variant_1);

      ----------------------
      -- Fill_Declaration --
      ----------------------

      procedure Fill_Declaration is
         File         : constant LSP.GPR_Files.File_Access :=
                          LSP.GPR_Files.Parse
                            (File_Provider => Self'Unchecked_Access,
                             Path          => Self.To_File
                               (Value.textDocument.uri));

         Reference : constant Gpr_Parser.Common.Token_Reference :=
                       LSP.GPR_Files.References.Token_Reference
                         (File, Value.position);

         Location : LSP.Structures.Location;

         use type Gpr_Parser.Common.Token_Reference;

      begin
         if Reference /= Gpr_Parser.Common.No_Token then
            Location.uri :=
              LSP.GPR_File_Readers.To_URI (Reference.Origin_Filename);
            Location.a_range := To_Range (Self'Unchecked_Access, Reference);
            Response.Variant_1.Append (Location);
         end if;

      end Fill_Declaration;

   begin
      Fill_Declaration;

      Self.Sender.On_Declaration_Response (Id, Response);
   end On_Declaration_Request;

   ---------------------------
   -- On_Definition_Request --
   ---------------------------

   overriding procedure On_Definition_Request
     (Self  : in out Message_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.DefinitionParams)
   is

      procedure Fill_Definition;
      --  Utility function, appends to Vector the definition if any.

      Response   : LSP.Structures.Definition_Result (LSP.Structures.Variant_1);

      ---------------------
      -- Fill_Definition --
      ---------------------

      procedure Fill_Definition is
         File         : constant LSP.GPR_Files.File_Access :=
                          LSP.GPR_Files.Parse
                            (File_Provider => Self'Unchecked_Access,
                             Path          => Self.To_File
                               (Value.textDocument.uri));

         Reference : constant Gpr_Parser.Common.Token_Reference :=
                       LSP.GPR_Files.References.Token_Reference
                         (File, Value.position);

         Location : LSP.Structures.Location;

         use type Gpr_Parser.Common.Token_Reference;

      begin
         if Reference /= Gpr_Parser.Common.No_Token then
            Location.uri :=
              LSP.GPR_File_Readers.To_URI (Reference.Origin_Filename);
            Location.a_range := To_Range (Self'Unchecked_Access, Reference);
            Response.Variant_1.Append (Location);
         end if;

      end Fill_Definition;

   begin
      Fill_Definition;

      Self.Sender.On_Definition_Response (Id, Response);
   end On_Definition_Request;

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

   --------------------------------------------
   -- On_DidChangeConfiguration_Notification --
   --------------------------------------------

   overriding procedure On_DidChangeConfiguration_Notification
     (Self  : in out Message_Handler;
      Value : LSP.Structures.DidChangeConfigurationParams) is
      Reload : Boolean;
      pragma Warnings (Off, Reload);
   begin
      Self.Configuration.Read_JSON (Value.settings, Reload);

      for Document of Self.Open_Documents loop
         begin
            --  reload gpr tree
            Document.Load (Self.Configuration);

         exception
            when E : others =>
               Self.Tracer.Trace_Exception
                 (E,
                  VSS.Strings.Conversions.To_Virtual_String
                    ("On_DidChangeConfiguration_Notification for " &
                       Document.File.Value));

         end;
      end loop;
   end On_DidChangeConfiguration_Notification;

   -------------------------
   -- Publish_Diagnostics --
   -------------------------

   overriding procedure Publish_Diagnostics
     (Self     : in out Message_Handler;
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
                        Diagnostic : constant LSP.Structures.Diagnostic :=
                          LSP.Utils.To_LSP_Diagnostic (Message);
                     begin
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

   -----------------------
   -- Set_Configuration --
   -----------------------

   overriding procedure Set_Configuration
     (Self  : in out Message_Handler;
      Value : LSP.Ada_Configurations.Configuration)
   is
   begin
      Self.Configuration := Value;
   end Set_Configuration;

   --------------
   -- To_Range --
   --------------

   function To_Range
     (File_Provider : LSP.GPR_Files.File_Provider_Access;
      Reference     : Gpr_Parser.Common.Token_Reference)
      return LSP.Structures.A_Range
   is
      use type Gpr_Parser.Common.Token_Reference;
   begin
      if Reference /= Gpr_Parser.Common.No_Token then
         declare
            package LK_Documents renames LSP.Text_Documents.Langkit_Documents;

            Referenced_File : constant LSP.GPR_Files.File_Access :=
                                LSP.GPR_Files.Parse
                                  (File_Provider => File_Provider,
                                   Path          => GPR2.Path_Name.Create_File
                                     (GPR2.Filename_Type
                                        (Reference.Origin_Filename)));
            Sloc_Range : constant Gpr_Parser.Slocs.Source_Location_Range :=
                           Reference.Data.Sloc_Range;
         begin
            return LK_Documents.To_A_Range
              (Start_Line_Text => Referenced_File.Get_Line
                 (Sloc_Range.Start_Line),
               End_Line_Text   => Referenced_File.Get_Line
                 (Sloc_Range.End_Line),
               A_Range         =>
                 (Start_Line   => Langkit_Support.Slocs.Line_Number
                      (Sloc_Range.Start_Line),
                  End_Line     => Langkit_Support.Slocs.Line_Number
                    (Sloc_Range.End_Line),
                  Start_Column => Langkit_Support.Slocs.Column_Number
                    (Sloc_Range.Start_Column),
                  End_Column   => Langkit_Support.Slocs.Column_Number
                    (Sloc_Range.End_Column)));
         end;

      else
         return LSP.Constants.Empty;
      end if;
   end To_Range;

   ---------------------
   -- Trace_Exception --
   ---------------------

   overriding procedure Trace_Exception
     (Self    : Message_Handler;
      Error   : Ada.Exceptions.Exception_Occurrence;
      Message : VSS.Strings.Virtual_String :=
        VSS.Strings.Empty_Virtual_String) is
   begin
      Self.Tracer.Trace_Exception (Error, Message);
   end Trace_Exception;

end LSP.GPR_Handlers;
