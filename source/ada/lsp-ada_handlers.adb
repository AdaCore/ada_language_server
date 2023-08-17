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

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Exceptions;
with Ada.Unchecked_Deallocation;

with VSS.Strings.Conversions;

with URIs;

with LSP.Ada_Contexts;
with LSP.Ada_Handlers.Project_Diagnostics;
with LSP.Ada_Handlers.Project_Loading;
with LSP.Diagnostic_Sources;
with LSP.Enumerations;
with LSP.Generic_Cancel_Check;
with LSP.Server_Notifications.DidChange;
with LSP.Servers;

package body LSP.Ada_Handlers is

   pragma Style_Checks ("o");  --  check subprogram bodies in alphabetical ordr

   function Contexts_For_URI
     (Self : access Message_Handler;
      URI  : LSP.Structures.DocumentUri)
      return LSP.Ada_Context_Sets.Context_Lists.List;
   --  Return a list of contexts that are suitable for the given File/URI:
   --  a list of all contexts where the file is known to be part of the
   --  project tree, or is a runtime file for this project. If the file
   --  is not known to any project, return an empty list.

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

   procedure Clean_Diagnostics
     (Self     : in out Message_Handler'Class;
      Document : not null LSP.Ada_Documents.Document_Access);
   --  Clean diagnostics up for the document

   function To_DocumentUri (X : VSS.Strings.Virtual_String)
     return LSP.Structures.DocumentUri is (X with null record);

   function To_DocumentUri
     (X : LSP.Structures.URI)
      return LSP.Structures.DocumentUri is
     (VSS.Strings.Virtual_String (X) with null record);

   EmptyDocumentUri : constant LSP.Structures.DocumentUri :=
     To_DocumentUri (VSS.Strings.Empty_Virtual_String);

   procedure Log_Method_In
     (Self : in out Message_Handler;
      Name : String;
      URI  : LSP.Structures.DocumentUri := EmptyDocumentUri);

   procedure Log_Method_Out
     (Self : in out Message_Handler;
      Name : String);
   --  Save method in/out in a log file

   function To_File
     (Self : Message_Handler'Class;
      URI  : LSP.Structures.DocumentUri) return GNATCOLL.VFS.Virtual_File
   is
     (GNATCOLL.VFS.Create_From_UTF8
        (URIs.Conversions.To_File
           (VSS.Strings.Conversions.To_UTF_8_String (URI),
            Normalize => Self.Configuration.Follow_Symlinks)));

   -----------------------
   -- Clean_Diagnostics --
   -----------------------

   procedure Clean_Diagnostics
     (Self     : in out Message_Handler'Class;
      Document : not null LSP.Ada_Documents.Document_Access)
   is
      Diag : LSP.Structures.PublishDiagnosticsParams;
   begin
      if Self.Configuration.Diagnostics_Enabled then
         Diag.uri := Document.URI;
         Self.Sender.On_PublishDiagnostics_Notification (Diag);
      end if;
   end Clean_Diagnostics;

   ----------------------
   -- Contexts_For_URI --
   ----------------------

   function Contexts_For_URI
     (Self : access Message_Handler;
      URI  : LSP.Structures.DocumentUri)
      return LSP.Ada_Context_Sets.Context_Lists.List
   is
      function Is_A_Source (Self : LSP.Ada_Contexts.Context) return Boolean is
        (Self.Is_Part_Of_Project (URI));
      --  Return True if URI is a source of the project held by Context

      File : constant GNATCOLL.VFS.Virtual_File := Self.To_File (URI);
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
   end Contexts_For_URI;

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

   -----------------------
   -- Get_Open_Document --
   -----------------------

   function Get_Open_Document
     (Self  : in out Message_Handler;
      URI   : LSP.Structures.DocumentUri;
      Force : Boolean := False)
      return LSP.Ada_Documents.Document_Access
   is
      File : constant GNATCOLL.VFS.Virtual_File := Self.To_File (URI);
   begin
      Project_Loading.Ensure_Project_Loaded (Self);

      if Self.Open_Documents.Contains (File) then
         return LSP.Ada_Documents.Document_Access
           (Self.Open_Documents.Element (File));
      elsif Force then
         declare
            Document : constant Internal_Document_Access :=
              new LSP.Ada_Documents.Document (Self.Tracer);
         begin
            Document.Initialize (URI, VSS.Strings.Empty_Virtual_String, null);
            return LSP.Ada_Documents.Document_Access (Document);
         end;
      else
         return null;
      end if;
   end Get_Open_Document;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self : in out Message_Handler'Class;
      Incremental_Text_Changes : Boolean) is
   begin
      Self.Incremental_Text_Changes := Incremental_Text_Changes;
   end Initialize;

   -------------------
   -- Log_Method_In --
   -------------------

   procedure Log_Method_In
     (Self : in out Message_Handler;
      Name : String;
      URI  : LSP.Structures.DocumentUri := EmptyDocumentUri) is
   begin
      if not URI.Is_Empty then
         Self.Tracer.Trace ("In Message_Handler " & Name & " URI:");
         Self.Tracer.Trace_Text (URI);
      else
         Self.Tracer.Trace ("In Message_Handler " & Name);
      end if;
   end Log_Method_In;

   --------------------
   -- Log_Method_Out --
   --------------------

   procedure Log_Method_Out
     (Self : in out Message_Handler;
      Name : String) is
   begin
      Self.Tracer.Trace ("Out Message_Handler " & Name);
   end Log_Method_Out;

   -------------------------------
   -- On_DidChange_Notification --
   -------------------------------

   overriding procedure On_DidChange_Notification
     (Self  : in out Message_Handler;
      Value : LSP.Structures.DidChangeTextDocumentParams)
   is
      use type LSP.Ada_Documents.Document_Access;

      function Skip_Did_Change return Boolean;
      --  Check if the following message in the queue is didChange for
      --  the same document

      URI      : LSP.Structures.DocumentUri renames Value.textDocument.uri;
      Document : constant LSP.Ada_Documents.Document_Access :=
        Get_Open_Document (Self, URI);

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
            use GNATCOLL.VFS;
            Object      : DidChange_Notification'Class renames
              DidChange_Notification'Class (Next.all);
            Object_File : constant Virtual_File := Self.To_File
              (Object.Params.textDocument.uri);
            Value_File  : constant Virtual_File := Self.To_File (URI);
         begin
            if Object_File /= Value_File then
               return False;
            end if;
         end;

         return True;
      end Skip_Did_Change;

   begin
      if Document = null then
         Self.Tracer.Trace
           ("Unexpected null document in On_DidChange_Notification");
         return;
      end if;

      if Self.Incremental_Text_Changes then
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

      for Context of Self.Contexts_For_URI (URI) loop
         Context.Index_Document (Document.all);
      end loop;

      --  Emit diagnostics
      Self.Publish_Diagnostics (Document);
   end On_DidChange_Notification;

   --------------------------------------------
   -- On_DidChangeConfiguration_Notification --
   --------------------------------------------

   overriding procedure On_DidChangeConfiguration_Notification
     (Self  : in out Message_Handler;
      Value : LSP.Structures.DidChangeConfigurationParams)
   is
      Reload : Boolean;
   begin
      Self.Configuration.Read_JSON (Value.settings, Reload);

      --  Always reload project if Project_Tree isn't ready
      Reload := Reload or not Self.Project_Tree.Is_Defined;

      if Reload then
         LSP.Ada_Handlers.Project_Loading.Reload_Project (Self);
      end if;
   end On_DidChangeConfiguration_Notification;

   -----------------------------------------------
   -- On_DidChangeWorkspaceFolders_Notification --
   -----------------------------------------------

   overriding procedure On_DidChangeWorkspaceFolders_Notification
     (Self  : in out Message_Handler;
      Value : LSP.Structures.DidChangeWorkspaceFoldersParams)
   is
      use type LSP.Ada_Documents.Document_Access;

      URI  : LSP.Structures.DocumentUri;
      File : GNATCOLL.VFS.Virtual_File;

      procedure Process_Created_File;
      --  Processes a created file

      procedure Process_Deleted_File;
      --  Processes a deleted file

      --------------------------
      -- Process_Created_File --
      --------------------------

      procedure Process_Created_File
      is
         use VSS.Strings.Conversions;

         Contexts : constant LSP.Ada_Context_Sets.Context_Lists.List :=
           Self.Contexts_For_URI (URI);

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

               Self.Tracer.Trace
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
            for C of Self.Contexts_For_URI (URI) loop
               C.Exclude_File (File);
               C.Index_File (File);
            end loop;
         end if;
      end Process_Deleted_File;

   begin
      --  Look through each change, filtering non Ada source files
      for Change of Value.event.added loop
         URI  := To_DocumentUri (Change.uri);
         File := Self.To_File (URI);
         Process_Created_File;
      end loop;

      for Change of Value.event.removed loop
         URI  := To_DocumentUri (Change.uri);
         File := Self.To_File (URI);
         Process_Deleted_File;
      end loop;
   end On_DidChangeWorkspaceFolders_Notification;

   ------------------------------
   -- On_DidClose_Notification --
   ------------------------------

   overriding procedure On_DidClose_Notification
     (Self  : in out Message_Handler;
      Value : LSP.Structures.DidCloseTextDocumentParams)
   is
      URI      : LSP.Structures.DocumentUri renames Value.textDocument.uri;
      File     : constant GNATCOLL.VFS.Virtual_File := Self.To_File (URI);
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

         Self.Tracer.Trace
           ("received a On_DidClose_Notification for non-open document "
            & "with uri: ");
         Self.Tracer.Trace_Text (URI);
      end if;

      --  Clean diagnostics up on closing document
      Self.Clean_Diagnostics
        (LSP.Ada_Documents.Document_Access (Document));
   end On_DidClose_Notification;

   ------------------------------------
   -- On_DidCreateFiles_Notification --
   ------------------------------------

   overriding procedure On_DidCreateFiles_Notification
     (Self  : in out Message_Handler;
      Value : LSP.Structures.CreateFilesParams) is
   begin
      Self.Log_Method_In ("On_DidCreateFiles_Notification");

      --  New sources were created on this project, so recompute its view

      Self.Project_Tree.Update_Sources (With_Runtime => True);

      --  For each created file of Value.files:
      --  - find the contexts that contains its directory
      --  - add it to those contexts
      --  - index it on those contexts

      for File of Value.files loop
         declare
            use VSS.Strings.Conversions;

            Created_File : constant GNATCOLL.VFS.Virtual_File :=
              Self.To_File (To_DocumentUri (File.uri));

            function Has_Dir
              (Context : LSP.Ada_Contexts.Context)
                  return Boolean
            is (Context.List_Source_Directories.Contains
                (Created_File.Dir));
            --  Return True if Old_File is a source of the project held by
            --  Context.

         begin
            for Context of Self.Contexts.Each_Context
              (Has_Dir'Unrestricted_Access)
            loop
               Context.Include_File (Created_File);
               Context.Index_File (Created_File);

               Self.Tracer.Trace
                 ("Included " & Created_File.Display_Base_Name
                  & " in context " & To_UTF_8_String (Context.Id));
            end loop;
         end;
      end loop;

      Self.Log_Method_Out ("On_DidCreateFiles_Notification");
   end On_DidCreateFiles_Notification;

   ------------------------------------
   -- On_DidDeleteFiles_Notification --
   ------------------------------------

   overriding procedure On_DidDeleteFiles_Notification
     (Self  : in out Message_Handler;
      Value : LSP.Structures.DeleteFilesParams) is
   begin
      Self.Log_Method_In ("On_DidDeleteFiles_Notification");

      --  Some project sources were deleted, so recompute its view

      Self.Project_Tree.Update_Sources (With_Runtime => True);

      --  For each delete file of Value.files:
      --  - find the contexts that contains it
      --  - remove it from those contexts
      --  - re-index it on those contexts so that an empty unit is reparsed

      for File of Value.files loop
         declare
            Deleted_File : constant GNATCOLL.VFS.Virtual_File :=
              Self.To_File (To_DocumentUri (File.uri));

            function Has_File
              (Context : LSP.Ada_Contexts.Context)
               return Boolean
            is (Context.Is_Part_Of_Project (To_DocumentUri (File.uri)));
            --  Return True if Old_File is a source of the project held by
            --  Context.

         begin
            for Context of Self.Contexts.Each_Context
              (Has_File'Unrestricted_Access)
            loop
               Context.Exclude_File (Deleted_File);
               Context.Index_File (Deleted_File);

               Self.Tracer.Trace
                 ("Excluded " & Deleted_File.Display_Base_Name
                  & " from context "
                  & VSS.Strings.Conversions.To_UTF_8_String (Context.Id));
            end loop;
         end;
      end loop;

      Self.Log_Method_Out ("On_DidDeleteFiles_Notification");
   end On_DidDeleteFiles_Notification;

   -----------------------------
   -- On_DidOpen_Notification --
   -----------------------------

   overriding procedure On_DidOpen_Notification
     (Self  : in out Message_Handler;
      Value : LSP.Structures.DidOpenTextDocumentParams)
   is
      URI    : LSP.Structures.DocumentUri renames Value.textDocument.uri;
      File   : constant GNATCOLL.VFS.Virtual_File := Self.To_File (URI);
      Object : constant Internal_Document_Access :=
        new LSP.Ada_Documents.Document (Self.Tracer);
      Diag   : constant LSP.Diagnostic_Sources.Diagnostic_Source_Access :=
        new LSP.Ada_Handlers.Project_Diagnostics.Diagnostic_Source
          (Self'Unchecked_Access);
   begin
      Self.Log_Method_In ("Text_Document_Did_Open", URI);

      --  Some clients don't properly call initialize, or don't pass the
      --  project to didChangeConfiguration: fallback here on loading a
      --  project in this directory, if needed.
      Self.Client.Set_Root_If_Empty
        (VSS.Strings.Conversions.To_Virtual_String
           (File.Dir.Display_Full_Name));

      Project_Loading.Ensure_Project_Loaded (Self);

      --  We have received a document: add it to the documents container
      Object.Initialize (URI, Value.textDocument.text, Diag);
      Self.Open_Documents.Include (File, Object);

      --  Handle the case where we're loading the implicit project: do
      --  we need to add the directory in which the document is open?

      if Self.Project_Status in Implicit_Project_Loaded then
         declare
            Dir : constant GNATCOLL.VFS.Virtual_File := Self.To_File (URI).Dir;
         begin
            if not Self.Project_Dirs_Loaded.Contains (Dir) then
               --  We do need to add this directory
               Self.Project_Dirs_Loaded.Insert (Dir);
               Project_Loading.Reload_Implicit_Project_Dirs (Self);
            end if;
         end;
      end if;

      --  Index the document in all the contexts where it is relevant
      for Context of Self.Contexts_For_URI (URI) loop
         Context.Index_Document (Object.all);
      end loop;

      --  Emit diagnostics
      Self.Publish_Diagnostics (LSP.Ada_Documents.Document_Access (Object));

      Self.Log_Method_Out ("Text_Document_Did_Open");
   end On_DidOpen_Notification;

   ------------------------------------
   -- On_DidRenameFiles_Notification --
   ------------------------------------

   overriding procedure On_DidRenameFiles_Notification
     (Self  : in out Message_Handler;
      Value : LSP.Structures.RenameFilesParams)
   is
      use LSP.Ada_Context_Sets;

      package URI_Contexts_Maps is new
        Ada.Containers.Indefinite_Hashed_Maps
          (Key_Type        => LSP.Structures.DocumentUri,
           Element_Type    => Context_Lists.List,
           Hash            => LSP.Structures.Get_Hash,
           Equivalent_Keys => LSP.Structures."=",
           "="             => Context_Lists."=");

      subtype URI_Contexts_Map is URI_Contexts_Maps.Map;

      URIs_Contexts : URI_Contexts_Map;

   begin
      Self.Log_Method_In ("On_DidRenameFiles_Notification");

      --  Some project sources were renamed, so recompute its view

      Self.Project_Tree.Update_Sources (With_Runtime => True);

      --  For each oldUri of Value.files:
      --  - map it to a list of context that contains it
      --  - remove it from those contexts
      --  - re-index it on those contexts so that an empty unit is reparsed

      for File_Rename of Value.files loop
         declare
            use VSS.Strings.Conversions;

            Old_File : constant GNATCOLL.VFS.Virtual_File :=
              Self.To_File (To_DocumentUri (File_Rename.oldUri));

            function Has_File
              (Context : LSP.Ada_Contexts.Context)
               return Boolean
            is (Context.Is_Part_Of_Project
                (To_DocumentUri (File_Rename.oldUri)));
            --  Return True if Old_File is a source of the project held by
            --  Context.

            URI_Contexts : Context_Lists.List;

         begin
            for Context of Self.Contexts.Each_Context
              (Has_File'Unrestricted_Access)
            loop
               URI_Contexts.Append (Context);
               Context.Exclude_File (Old_File);
               Context.Index_File (Old_File);

               Self.Tracer.Trace
                 ("Excluded " & Old_File.Display_Full_Name
                  & " from context " & To_UTF_8_String (Context.Id));
            end loop;

            URIs_Contexts.Insert
              (To_DocumentUri (File_Rename.oldUri), URI_Contexts);
         end;
      end loop;

      --  For each (oldUri, newUri) tuple:
      --  - add newUri to all contexts that contained oldUri
      --  - index the newUri (using the appriate method depending if
      --    (there's an open document of not)

      for File_Rename of Value.files loop
         declare
            use VSS.Strings.Conversions;
            use type LSP.Ada_Documents.Document_Access;

            New_File : constant GNATCOLL.VFS.Virtual_File :=
              Self.To_File (To_DocumentUri (File_Rename.newUri));
            Document : constant LSP.Ada_Documents.Document_Access :=
              Get_Open_Document (Self, To_DocumentUri (File_Rename.newUri));
            Is_Document_Open : constant Boolean := Document /= null;

         begin
            for Context of URIs_Contexts.Constant_Reference
              (To_DocumentUri (File_Rename.oldUri))
            loop
               Context.Include_File (New_File);
               if Is_Document_Open then
                  Context.Index_Document (Document.all);
               else
                  Context.Index_File (New_File);
               end if;
               Self.Tracer.Trace
                 ("Included " & New_File.Display_Base_Name & " in context "
                  & To_UTF_8_String (Context.Id));
            end loop;
         end;
      end loop;

      Self.Log_Method_Out ("On_DidRenameFiles_Notification");
   end On_DidRenameFiles_Notification;

   -----------------------------
   -- On_FoldingRange_Request --
   -----------------------------

   overriding procedure On_FoldingRange_Request
     (Self  : in out Message_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.FoldingRangeParams)
   is
      use type LSP.Ada_Documents.Document_Access;

      Context  : constant LSP.Ada_Context_Sets.Context_Access :=
        Self.Contexts.Get_Best_Context (Value.textDocument.uri);
      Document : constant LSP.Ada_Documents.Document_Access :=
        Get_Open_Document (Self, Value.textDocument.uri);
      Response : LSP.Structures.FoldingRange_Vector_Or_Null;

   begin
      if Document /= null then
         Document.Get_Folding_Blocks
           (Context.all,
            Self.Client.Line_Folding_Only,
            Self.Configuration.Folding_Comments,
            Self.Is_Canceled,
            Response);

         if Self.Is_Canceled.all then
            Response.Clear;
         end if;
         Self.Sender.On_FoldingRange_Response (Id, Response);

      else
         Self.Sender.On_Error_Response
           (Id, (code => LSP.Enumerations.InternalError,
                 message => "Document is not opened"));
      end if;
   end On_FoldingRange_Request;

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
      Self.Client.Initialize (Value);

      Response.capabilities := Self.Client.To_Server_Capabilities
        (Self.Incremental_Text_Changes);

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
      Result : LSP.Structures.Null_Record;

   begin
      --  Suspend files/runtime indexing after shutdown requst
      Self.Indexing_Enabled := False;

      Self.Sender.On_Shutdown_Response (Id, Result);
   end On_Shutdown_Request;

   -------------------------
   -- Publish_Diagnostics --
   -------------------------

   procedure Publish_Diagnostics
     (Self              : in out Message_Handler'Class;
      Document          : not null LSP.Ada_Documents.Document_Access;
      Other_Diagnostics : LSP.Structures.Diagnostic_Vector :=
        LSP.Structures.Empty;
      Force             : Boolean := False)
   is
      Changed : Boolean;
      Diag    : LSP.Structures.PublishDiagnosticsParams;
   begin
      if Self.Configuration.Diagnostics_Enabled then
         Document.Get_Errors
           (Context => Self.Contexts.Get_Best_Context (Document.URI).all,
            Changed => Changed,
            Errors  => Diag.diagnostics,
            Force   => Force);

         Diag.diagnostics.Append_Vector (Other_Diagnostics);

         if Changed or else not Other_Diagnostics.Is_Empty then
            Diag.uri := Document.URI;
            Self.Sender.On_PublishDiagnostics_Notification (Diag);
         end if;
      end if;
   end Publish_Diagnostics;

end LSP.Ada_Handlers;
