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

with Ada.Exceptions;
with Ada.Unchecked_Deallocation;

with VSS.Strings.Conversions;

with URIs;

with LSP.Ada_Contexts;
with LSP.Ada_Handlers.Project_Diagnostics;
with LSP.Ada_Handlers.Project_Loading;
with LSP.Diagnostic_Sources;
with LSP.Enumerations;

package body LSP.Ada_Handlers is

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

   function To_File
     (Self : Message_Handler'Class;
      URI  : LSP.Structures.DocumentUri) return GNATCOLL.VFS.Virtual_File
   is
     (GNATCOLL.VFS.Create_From_UTF8
        (URIs.Conversions.To_File
           (VSS.Strings.Conversions.To_UTF_8_String (URI),
            Normalize => Self.Configuration.Follow_Symlinks)));

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
     (Self  : access Message_Handler;
      URI   : LSP.Structures.DocumentUri;
      Force : Boolean := False)
      return LSP.Ada_Documents.Document_Access
   is
      File : constant GNATCOLL.VFS.Virtual_File := Self.To_File (URI);
   begin
      Project_Loading.Ensure_Project_Loaded (Self.all);

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

   --------------------------------------------
   -- On_DidChangeConfiguration_Notification --
   --------------------------------------------

   overriding procedure On_DidChangeConfiguration_Notification
     (Self  : in out Message_Handler;
      Value : LSP.Structures.DidChangeConfigurationParams)
   is
      Reload : Boolean := False;
   begin
      Self.Configuration.Read_JSON (Value.settings, Reload);

      if Reload then
         LSP.Ada_Handlers.Project_Loading.Reload_Project (Self);
      end if;
   end On_DidChangeConfiguration_Notification;

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
      Self.Tracer.Trace ("In Text_Document_Did_Open. Uri :");
      Self.Tracer.Trace_Text (URI);

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

      Self.Tracer.Trace ("Finished Text_Document_Did_Open");
   end On_DidOpen_Notification;

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
        (Incremental_Text_Changes => Self.Incremental_Text_Changes);

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
      Value : LSP.Server_Requests.Server_Request'Class) is
   begin
      Self.Implemented := True;

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
