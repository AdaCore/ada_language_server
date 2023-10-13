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

--  This package provides requests and notifications handler for GPR language.

private with Ada.Containers.Hashed_Maps;

private with GNATCOLL.VFS;

private with GPR2.File_Readers;
private with GPR2.Path_Name;

with LSP.Client_Message_Receivers;
with LSP.GPR_Client_Capabilities;
with LSP.GPR_Documents;
with LSP.GPR_Files;
with LSP.Server_Message_Visitors;
private with LSP.Server_Requests;
with LSP.Server_Request_Receivers;
private with LSP.Server_Notifications;
with LSP.Server_Notification_Receivers;
private with LSP.Structures;
with LSP.Tracers;
with LSP.Unimplemented_Handlers;

with URIs;

with VSS.Strings.Conversions;

package LSP.GPR_Handlers is

   type Message_Handler
      (Sender : not null access LSP.Client_Message_Receivers
         .Client_Message_Receiver'Class;
       Tracer : not null LSP.Tracers.Tracer_Access) is
   limited new LSP.Server_Message_Visitors.Server_Message_Visitor
     and LSP.Server_Request_Receivers.Server_Request_Receiver
     and LSP.Server_Notification_Receivers.Server_Notification_Receiver
     and LSP.GPR_Documents.Document_Provider
     and LSP.GPR_Files.File_Provider
   with private;
   --  A handler of LSP notifications and requests from GPR language
   --  A LSP notifications/requests handler for GPR language

private

   type Internal_Document_Access is access all LSP.GPR_Documents.Document;

   package Document_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => GNATCOLL.VFS.Virtual_File,
      Element_Type    => Internal_Document_Access,
      Hash            => GNATCOLL.VFS.Full_Name_Hash,
      Equivalent_Keys => GNATCOLL.VFS."=");
   --  Container for documents indexed by URI (diagnostics request)

   type Internal_File_Access is access all LSP.GPR_Files.File;

   package Files_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => GPR2.Path_Name.Object,
      Element_Type    => Internal_File_Access,
      Hash            => GPR2.Path_Name.Hash,
      Equivalent_Keys => GPR2.Path_Name."=");
   --  Container for documents indexed by URI (others request)

   type Has_Been_Canceled_Function is access function return Boolean;

   type Message_Handler
      (Sender : not null access LSP.Client_Message_Receivers
         .Client_Message_Receiver'Class;
       Tracer : not null LSP.Tracers.Tracer_Access) is
   limited new LSP.Unimplemented_Handlers.Unimplemented_Handler
     and LSP.Server_Message_Visitors.Server_Message_Visitor
     and LSP.Server_Request_Receivers.Server_Request_Receiver
     and LSP.Server_Notification_Receivers.Server_Notification_Receiver
     and LSP.GPR_Documents.Document_Provider
     and LSP.GPR_Files.File_Provider
   with record
      Client               : LSP.GPR_Client_Capabilities.Client_Capability;

      Open_Documents       : Document_Maps.Map;
      --  The documents that are currently open

      Parsed_Files         : Files_Maps.Map;
      --  open document & related files (imported, extended, aggregated)

      Hierarchical_Symbols : Boolean := False;
      --  textDocument/documentSymbol handler. Actual value depends on
      --  client's capabilities.

      Follow_Symlinks      : Boolean := True;
      --  False if the client disables symlink following. In this case
      --  URIs from client should match file names reported by GPR2.Path_Name

      Diagnostics_Enabled  : Boolean := True;
      --  Whether to publish diagnostics

      File_Reader          : GPR2.File_Readers.File_Reader_Reference;

      Is_Canceled          : Has_Been_Canceled_Function;
      --  Is request has been canceled
   end record;

   overriding procedure On_Server_Notification
     (Self  : in out Message_Handler;
      Value : LSP.Server_Notifications.Server_Notification'Class);

   overriding procedure On_Server_Request
     (Self  : in out Message_Handler;
      Value : LSP.Server_Requests.Server_Request'Class);

   overriding procedure On_Initialize_Request
     (Self  : in out Message_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.InitializeParams);

   overriding procedure On_Shutdown_Request
     (Self : in out Message_Handler;
      Id   : LSP.Structures.Integer_Or_Virtual_String);

   overriding procedure On_DocumentSymbol_Request
     (Self  : in out Message_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.DocumentSymbolParams);

   overriding procedure On_DidChange_Notification
     (Self  : in out Message_Handler;
      Value : LSP.Structures.DidChangeTextDocumentParams);

   overriding procedure On_DidClose_Notification
     (Self  : in out Message_Handler;
      Value : LSP.Structures.DidCloseTextDocumentParams);

   overriding procedure On_DidOpen_Notification
     (Self  : in out Message_Handler;
      Value : LSP.Structures.DidOpenTextDocumentParams);

   overriding procedure On_Hover_Request
     (Self  : in out Message_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.HoverParams);

   overriding procedure On_Completion_Request
     (Self  : in out Message_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.CompletionParams);

   overriding procedure On_Completion_Resolve_Request
     (Self  : in out Message_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.CompletionItem);

   -----------------------------------------
   -- LSP.GPR_Documents.Document_Provider --
   -----------------------------------------

   overriding function Get_Open_Document
     (Self  : access Message_Handler;
      URI   : LSP.Structures.DocumentUri;
      Force : Boolean := False)
      return LSP.GPR_Documents.Document_Access;
   --  Return the open document for the given URI.
   --  If the document is not opened, then if Force a new document
   --  will be created and must be freed by the user else null will be
   --  returned.

   overriding function Get_Open_Document_Version
     (Self  : access Message_Handler;
      URI   : LSP.Structures.DocumentUri)
      return LSP.Structures.OptionalVersionedTextDocumentIdentifier;
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

   overriding function To_File
     (Self : access Message_Handler;
      Item : LSP.Structures.DocumentUri) return GNATCOLL.VFS.Virtual_File
   is
     (GNATCOLL.VFS.Create_From_UTF8
        (URIs.Conversions.To_File
             (URI       => VSS.Strings.Conversions.To_UTF_8_String (Item),
              Normalize => Self.Follow_Symlinks)));

   overriding function To_File
     (Self : access Message_Handler;
      Item : LSP.Structures.DocumentUri) return GPR2.Path_Name.Object
   is
     (GPR2.Path_Name.Create_File
        (GPR2.Filename_Type
             (URIs.Conversions.To_File
                  (URI       => VSS.Strings.Conversions.To_UTF_8_String (Item),
                   Normalize => Self.Follow_Symlinks))));

   overriding function To_URI
     (Self : access Message_Handler;
      Item : GPR2.Path_Name.Object) return LSP.Structures.DocumentUri
   is
     (VSS.Strings.Conversions.To_Virtual_String
        (URIs.Conversions.From_File (Item.Value)) with null record);

end LSP.GPR_Handlers;
