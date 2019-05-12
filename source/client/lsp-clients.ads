------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2018-2019, AdaCore                     --
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

with Ada.Containers.Hashed_Maps;
with Ada.Streams;
with Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;
with Ada.Strings.UTF_Encoding;

with LSP.Messages;
with LSP.Messages.Notifications;
with LSP.Raw_Clients;
with LSP.Types;

limited with LSP.Clients.Request_Handlers;
limited with LSP.Clients.Response_Handlers;
limited with LSP.Client_Notifications;

package LSP.Clients is

   type Client is new LSP.Raw_Clients.Raw_Client
     and LSP.Messages.Notifications.Server_Notification_Handler
   with private;
   --  Client object to send/recieve request and notification to/from
   --  the LSP server

   procedure Initialize (Self : in out Client'Class);
   --  Initialize Client to correct state

   overriding procedure On_Initialized_Notification (Self : access Client);

   overriding procedure On_Exit_Notification (Self : access Client);

   overriding procedure On_DidChangeConfiguration_Notification
     (Self  : access Client;
      Value : LSP.Messages.DidChangeConfigurationParams);

   overriding procedure On_ShowMessage_Notification
     (Self  : access Client;
      Value : LSP.Messages.ShowMessageParams) is null;

   overriding procedure On_LogMessage_Notification
     (Self  : access Client;
      Value : LSP.Messages.LogMessageParams) is null;

   overriding procedure On_PublishDiagnostics_Notification
     (Self  : access Client;
      Value : LSP.Messages.PublishDiagnosticsParams) is null;

   overriding procedure On_DidOpenTextDocument_Notification
     (Self  : access Client;
      Value : LSP.Messages.DidOpenTextDocumentParams);

   overriding procedure On_DidChangeTextDocument_Notification
     (Self  : access Client;
      Value : LSP.Messages.DidChangeTextDocumentParams);

   overriding procedure On_DidSaveTextDocument_Notification
     (Self  : access Client;
      Value : LSP.Messages.DidSaveTextDocumentParams);

   overriding procedure On_DidCloseTextDocument_Notification
     (Self  : access Client;
      Value : LSP.Messages.DidCloseTextDocumentParams);

   procedure Set_Response_Handler
     (Self  : in out Client'Class;
      Value : access LSP.Clients.Response_Handlers.Response_Handler'Class);
   --  Set response handler

   procedure Set_Request_Handler
     (Self  : in out Client'Class;
      Value : access LSP.Clients.Request_Handlers.Request_Handler'Class);
   --  Set request handler

   procedure Set_Notification_Handler
     (Self  : in out Client'Class;
      Value : access Client_Notifications.Client_Notification_Handler'Class);
   --  Set notification handler

   --  Routines to send request to the LSP server

   procedure Initialize_Request
     (Self     : in out Client'Class;
      Request  : out LSP.Types.LSP_Number;
      Value    : LSP.Messages.InitializeParams);

   procedure Shutdown_Request
     (Self     : in out Client'Class;
      Request  : out LSP.Types.LSP_Number);

   procedure Text_Document_Code_Action_Request
     (Self     : in out Client'Class;
      Request  : out LSP.Types.LSP_Number;
      Value    : LSP.Messages.CodeActionParams);

   procedure Text_Document_Completion_Request
     (Self     : in out Client'Class;
      Request  : out LSP.Types.LSP_Number;
      Value    : LSP.Messages.TextDocumentPositionParams);

   procedure Text_Document_Definition_Request
     (Self     : in out Client'Class;
      Request  : out LSP.Types.LSP_Number;
      Value    : LSP.Messages.TextDocumentPositionParams);

   procedure Text_Document_Hover_Request
     (Self     : in out Client'Class;
      Request  : out LSP.Types.LSP_Number;
      Value    : LSP.Messages.TextDocumentPositionParams);

   procedure Text_Document_Highlight_Request
     (Self     : in out Client'Class;
      Request  : out LSP.Types.LSP_Number;
      Value    : LSP.Messages.TextDocumentPositionParams);

   procedure Text_Document_References_Request
     (Self     : in out Client'Class;
      Request  : out LSP.Types.LSP_Number;
      Value    : LSP.Messages.ReferenceParams);

   procedure Text_Document_Signature_Help_Request
     (Self     : in out Client'Class;
      Request  : out LSP.Types.LSP_Number;
      Value    : LSP.Messages.TextDocumentPositionParams);

   procedure Text_Document_Symbol_Request
     (Self     : in out Client'Class;
      Request  : out LSP.Types.LSP_Number;
      Value    : LSP.Messages.DocumentSymbolParams);

   procedure Workspace_Execute_Command_Request
     (Self     : in out Client'Class;
      Request  : out LSP.Types.LSP_Number;
      Value    : LSP.Messages.ExecuteCommandParams);

   procedure Workspace_Symbol_Request
     (Self     : in out Client'Class;
      Request  : out LSP.Types.LSP_Number;
      Value    : LSP.Messages.WorkspaceSymbolParams);

   --  Send response to the LSP server

   procedure Workspace_Apply_Edit
     (Self    : in out Client'Class;
      Request : LSP.Types.LSP_Number_Or_String;
      Applied : Boolean);

   function Allocate_Request_Id
     (Self : in out Client'Class) return LSP.Types.LSP_Number_Or_String;
   --  Allocates request id.

private

   type Response_Decoder is access procedure
     (Stream  : access Ada.Streams.Root_Stream_Type'Class;
      Request : LSP.Types.LSP_Number;
      Handler : access LSP.Clients.Response_Handlers.Response_Handler'Class);

   function Hash (Value : LSP.Types.LSP_Number)
      return Ada.Containers.Hash_Type is
        (Ada.Containers.Hash_Type'Val (Value));

   package Request_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => LSP.Types.LSP_Number,
      Element_Type    => Response_Decoder,
      Hash            => Hash,
      Equivalent_Keys => "=");

   type Notification_Decoder is access procedure
     (Stream  : access Ada.Streams.Root_Stream_Type'Class;
      Handler : access Client_Notifications.Client_Notification_Handler'Class);

   package Notification_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Ada.Strings.Unbounded.Unbounded_String,
      Element_Type    => Notification_Decoder,
      Hash            => Ada.Strings.Unbounded.Hash,
      Equivalent_Keys => Ada.Strings.Unbounded."=");

   type Client is new LSP.Raw_Clients.Raw_Client
     and LSP.Messages.Notifications.Server_Notification_Handler
   with record
      Request_Id       : LSP.Types.LSP_Number := 0;  --  Id of prev request
      Request_Map      : Request_Maps.Map;  --  issued requests
      Notif_Decoders   : Notification_Maps.Map;  --  notification decoders
      Response_Handler : access
        LSP.Clients.Response_Handlers.Response_Handler'Class;
      Request_Handler  : access
        LSP.Clients.Request_Handlers.Request_Handler'Class;
      Notification     : access
        LSP.Client_Notifications.Client_Notification_Handler'Class;
   end record;

   overriding procedure On_Raw_Message
     (Self : in out Client;
      Data : Ada.Strings.Unbounded.Unbounded_String);

   procedure Send_Notification
     (Self   : in out Client'Class;
      Method : Ada.Strings.UTF_Encoding.UTF_8_String;
      Value  : in out LSP.Messages.NotificationMessage'Class);

   procedure Send_Request
     (Self    : in out Client'Class;
      Request : out LSP.Types.LSP_Number;
      Method  : Ada.Strings.UTF_Encoding.UTF_8_String;
      Decoder : Response_Decoder;
      Value   : in out LSP.Messages.RequestMessage'Class);

   procedure Send_Response
     (Self    : in out Client'Class;
      Request : LSP.Types.LSP_Number_Or_String;
      Value   : in out LSP.Messages.ResponseMessage'Class);

end LSP.Clients;
