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

with GNATCOLL.JSON;

with LSP.Client_Notifications;
with LSP.Clients.Request_Handlers;
with LSP.Clients.Response_Handlers;
with LSP.JSON_Streams;
with LSP.Messages.Requests; use LSP.Messages.Requests;
with LSP.Messages.Notifications; use LSP.Messages.Notifications;

package body LSP.Clients is

   function "+" (Text : Ada.Strings.UTF_Encoding.UTF_8_String)
      return LSP.Types.LSP_String renames
       LSP.Types.To_LSP_String;

   package Decoders is

      --  Responses

      procedure Initialize_Response
        (Stream  : access Ada.Streams.Root_Stream_Type'Class;
         Request : LSP.Types.LSP_Number;
         Handler : access
           LSP.Clients.Response_Handlers.Response_Handler'Class);

      procedure Shutdown_Response
        (Stream  : access Ada.Streams.Root_Stream_Type'Class;
         Request : LSP.Types.LSP_Number;
         Handler : access
           LSP.Clients.Response_Handlers.Response_Handler'Class);

      procedure Text_Document_Code_Action_Response
        (Stream  : access Ada.Streams.Root_Stream_Type'Class;
         Request : LSP.Types.LSP_Number;
         Handler : access
           LSP.Clients.Response_Handlers.Response_Handler'Class);

      procedure Text_Document_Completion_Response
        (Stream  : access Ada.Streams.Root_Stream_Type'Class;
         Request : LSP.Types.LSP_Number;
         Handler : access
           LSP.Clients.Response_Handlers.Response_Handler'Class);

      procedure Text_Document_Definition_Response
        (Stream  : access Ada.Streams.Root_Stream_Type'Class;
         Request : LSP.Types.LSP_Number;
         Handler : access
           LSP.Clients.Response_Handlers.Response_Handler'Class);

      procedure Text_Document_Hover_Response
        (Stream  : access Ada.Streams.Root_Stream_Type'Class;
         Request : LSP.Types.LSP_Number;
         Handler : access
           LSP.Clients.Response_Handlers.Response_Handler'Class);

      procedure Text_Document_Highlight_Response
        (Stream  : access Ada.Streams.Root_Stream_Type'Class;
         Request : LSP.Types.LSP_Number;
         Handler : access
           LSP.Clients.Response_Handlers.Response_Handler'Class);

      procedure Text_Document_References_Response
        (Stream  : access Ada.Streams.Root_Stream_Type'Class;
         Request : LSP.Types.LSP_Number;
         Handler : access
           LSP.Clients.Response_Handlers.Response_Handler'Class);

      procedure Text_Document_Signature_Help_Response
        (Stream  : access Ada.Streams.Root_Stream_Type'Class;
         Request : LSP.Types.LSP_Number;
         Handler : access
           LSP.Clients.Response_Handlers.Response_Handler'Class);

      procedure Text_Document_Symbol_Response
        (Stream  : access Ada.Streams.Root_Stream_Type'Class;
         Request : LSP.Types.LSP_Number;
         Handler : access
           LSP.Clients.Response_Handlers.Response_Handler'Class);

      procedure Workspace_Execute_Command_Response
        (Stream  : access Ada.Streams.Root_Stream_Type'Class;
         Request : LSP.Types.LSP_Number;
         Handler : access
           LSP.Clients.Response_Handlers.Response_Handler'Class);

      procedure Workspace_Symbol_Response
        (Stream  : access Ada.Streams.Root_Stream_Type'Class;
         Request : LSP.Types.LSP_Number;
         Handler : access
           LSP.Clients.Response_Handlers.Response_Handler'Class);

      --  Notifications

      procedure Show_Message
        (Stream  : access Ada.Streams.Root_Stream_Type'Class;
         Handler : access
           LSP.Client_Notifications.Client_Notification_Handler'Class);

      procedure Log_Message
        (Stream  : access Ada.Streams.Root_Stream_Type'Class;
         Handler : access
           LSP.Client_Notifications.Client_Notification_Handler'Class);

      procedure Publish_Diagnostics
        (Stream  : access Ada.Streams.Root_Stream_Type'Class;
         Handler : access
           LSP.Client_Notifications.Client_Notification_Handler'Class);

   end Decoders;

   -------------------------
   -- Allocate_Request_Id --
   -------------------------

   function Allocate_Request_Id
     (Self : in out Client'Class) return LSP.Types.LSP_Number_Or_String is
   begin
      Self.Request_Id := Self.Request_Id + 1;

      return (True, Self.Request_Id);
   end Allocate_Request_Id;

   package body Decoders is

      -------------------------
      -- Initialize_Response --
      -------------------------

      procedure Initialize_Response
        (Stream  : access Ada.Streams.Root_Stream_Type'Class;
         Request : LSP.Types.LSP_Number;
         Handler : access LSP.Clients.Response_Handlers.Response_Handler'Class)
      is
         Response : constant LSP.Messages.Initialize_Response :=
           LSP.Messages.Initialize_Response'Input (Stream);
      begin
         Handler.Initialize_Response (Request, Response);
      end Initialize_Response;

      -----------------------
      -- Shutdown_Response --
      -----------------------

      procedure Shutdown_Response
        (Stream  : access Ada.Streams.Root_Stream_Type'Class;
         Request : LSP.Types.LSP_Number;
         Handler : access LSP.Clients.Response_Handlers.Response_Handler'Class)
      is
         Response : constant LSP.Messages.ResponseMessage :=
           LSP.Messages.Input_ResponseMessage (Stream);
      begin
         Handler.Shutdown_Response (Request, Response);
      end Shutdown_Response;

      ----------------------------------------
      -- Text_Document_Code_Action_Response --
      ----------------------------------------

      procedure Text_Document_Code_Action_Response
        (Stream  : access Ada.Streams.Root_Stream_Type'Class;
         Request : LSP.Types.LSP_Number;
         Handler : access LSP.Clients.Response_Handlers.Response_Handler'Class)
      is
         Response : constant LSP.Messages.CodeAction_Response :=
           LSP.Messages.CodeAction_Response'Input (Stream);
      begin
         Handler.Text_Document_Code_Action_Response (Request, Response);
      end Text_Document_Code_Action_Response;

      ---------------------------------------
      -- Text_Document_Completion_Response --
      ---------------------------------------

      procedure Text_Document_Completion_Response
        (Stream  : access Ada.Streams.Root_Stream_Type'Class;
         Request : LSP.Types.LSP_Number;
         Handler : access LSP.Clients.Response_Handlers.Response_Handler'Class)
      is
         Response : constant LSP.Messages.Completion_Response :=
           LSP.Messages.Completion_Response'Input (Stream);
      begin
         Handler.Text_Document_Completion_Response (Request, Response);
      end Text_Document_Completion_Response;

      ---------------------------------------
      -- Text_Document_Definition_Response --
      ---------------------------------------

      procedure Text_Document_Definition_Response
        (Stream  : access Ada.Streams.Root_Stream_Type'Class;
         Request : LSP.Types.LSP_Number;
         Handler : access LSP.Clients.Response_Handlers.Response_Handler'Class)
      is
         Response : constant LSP.Messages.Location_Response :=
           LSP.Messages.Location_Response'Input (Stream);
      begin
         Handler.Text_Document_Definition_Response (Request, Response);
      end Text_Document_Definition_Response;

      ----------------------------------
      -- Text_Document_Hover_Response --
      ----------------------------------

      procedure Text_Document_Hover_Response
        (Stream  : access Ada.Streams.Root_Stream_Type'Class;
         Request : LSP.Types.LSP_Number;
         Handler : access LSP.Clients.Response_Handlers.Response_Handler'Class)
      is
         Response : constant LSP.Messages.Hover_Response :=
           LSP.Messages.Hover_Response'Input (Stream);
      begin
         Handler.Text_Document_Hover_Response (Request, Response);
      end Text_Document_Hover_Response;

      --------------------------------------
      -- Text_Document_Highlight_Response --
      --------------------------------------

      procedure Text_Document_Highlight_Response
        (Stream  : access Ada.Streams.Root_Stream_Type'Class;
         Request : LSP.Types.LSP_Number;
         Handler : access
           LSP.Clients.Response_Handlers.Response_Handler'Class)
      is
         Response : constant LSP.Messages.Highlight_Response :=
           LSP.Messages.Highlight_Response'Input (Stream);
      begin
         Handler.Text_Document_Highlight_Response (Request, Response);
      end Text_Document_Highlight_Response;

      ---------------------------------------
      -- Text_Document_References_Response --
      ---------------------------------------

      procedure Text_Document_References_Response
        (Stream  : access Ada.Streams.Root_Stream_Type'Class;
         Request : LSP.Types.LSP_Number;
         Handler : access
           LSP.Clients.Response_Handlers.Response_Handler'Class)
      is
         Response : constant LSP.Messages.Location_Response :=
           LSP.Messages.Location_Response'Input (Stream);
      begin
         Handler.Text_Document_References_Response (Request, Response);
      end Text_Document_References_Response;

      -------------------------------------------
      -- Text_Document_Signature_Help_Response --
      -------------------------------------------

      procedure Text_Document_Signature_Help_Response
        (Stream  : access Ada.Streams.Root_Stream_Type'Class;
         Request : LSP.Types.LSP_Number;
         Handler : access
           LSP.Clients.Response_Handlers.Response_Handler'Class)
      is
         Response : constant LSP.Messages.SignatureHelp_Response :=
           LSP.Messages.SignatureHelp_Response'Input (Stream);
      begin
         Handler.Text_Document_Signature_Help_Response (Request, Response);
      end Text_Document_Signature_Help_Response;

      -----------------------------------
      -- Text_Document_Symbol_Response --
      -----------------------------------

      procedure Text_Document_Symbol_Response
        (Stream  : access Ada.Streams.Root_Stream_Type'Class;
         Request : LSP.Types.LSP_Number;
         Handler : access
           LSP.Clients.Response_Handlers.Response_Handler'Class)
      is
         Response : constant LSP.Messages.Symbol_Response :=
           LSP.Messages.Symbol_Response'Input (Stream);
      begin
         Handler.Text_Document_Symbol_Response (Request, Response);
      end Text_Document_Symbol_Response;

      ----------------------------------------
      -- Workspace_Execute_Command_Response --
      ----------------------------------------

      procedure Workspace_Execute_Command_Response
        (Stream  : access Ada.Streams.Root_Stream_Type'Class;
         Request : LSP.Types.LSP_Number;
         Handler : access
           LSP.Clients.Response_Handlers.Response_Handler'Class)
      is
         Response : constant LSP.Messages.ExecuteCommand_Response :=
           LSP.Messages.ExecuteCommand_Response'Input (Stream);
      begin
         Handler.Workspace_Execute_Command_Response (Request, Response);
      end Workspace_Execute_Command_Response;

      -------------------------------
      -- Workspace_Symbol_Response --
      -------------------------------

      procedure Workspace_Symbol_Response
        (Stream  : access Ada.Streams.Root_Stream_Type'Class;
         Request : LSP.Types.LSP_Number;
         Handler : access
           LSP.Clients.Response_Handlers.Response_Handler'Class)
      is
         Response : constant LSP.Messages.Symbol_Response :=
           LSP.Messages.Symbol_Response'Input (Stream);
      begin
         Handler.Workspace_Symbol_Response (Request, Response);
      end Workspace_Symbol_Response;

      -----------------
      -- Log_Message --
      -----------------

      procedure Log_Message
        (Stream  : access Ada.Streams.Root_Stream_Type'Class;
         Handler : access
           LSP.Client_Notifications.Client_Notification_Handler'Class)
      is
         Message : LogMessage_Notification;
      begin
         LogMessage_Notification'Read (Stream, Message);
         Handler.Log_Message (Message.params);
      end Log_Message;

      -------------------------
      -- Publish_Diagnostics --
      -------------------------

      procedure Publish_Diagnostics
        (Stream  : access Ada.Streams.Root_Stream_Type'Class;
         Handler : access
           LSP.Client_Notifications.Client_Notification_Handler'Class)
      is
         Message : PublishDiagnostics_Notification;
      begin
         PublishDiagnostics_Notification'Read (Stream, Message);
         Handler.Publish_Diagnostics (Message.params);
      end Publish_Diagnostics;

      ------------------
      -- Show_Message --
      ------------------

      procedure Show_Message
        (Stream  : access Ada.Streams.Root_Stream_Type'Class;
         Handler : access
           LSP.Client_Notifications.Client_Notification_Handler'Class)
      is
         Message : ShowMessage_Notification;
      begin
         ShowMessage_Notification'Read (Stream, Message);
         Handler.Show_Message (Message.params);
      end Show_Message;

   end Decoders;

   -----------------------
   -- Exit_Notification --
   -----------------------

   procedure Exit_Notification (Self : access Client) is
      Message : LSP.Messages.Notifications.Exit_Notification;
   begin
      Self.Send_Notification ("exit", Message);
   end Exit_Notification;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self : in out Client'Class) is
      function "-"
        (Text : String) return Ada.Strings.Unbounded.Unbounded_String
          renames Ada.Strings.Unbounded.To_Unbounded_String;
   begin
      Self.Notif_Decoders.Insert
        (-"window/showMessage",
         Decoders.Show_Message'Access);
      Self.Notif_Decoders.Insert
        (-"window/logMessage",
         Decoders.Log_Message'Access);
      --  "telemetry/event",
      Self.Notif_Decoders.Insert
        (-"textDocument/publishDiagnostics",
         Decoders.Publish_Diagnostics'Access);
   end Initialize;

   ------------------------
   -- Initialize_Request --
   ------------------------

   procedure Initialize_Request
     (Self    : in out Client'Class;
      Request : out LSP.Types.LSP_Number;
      Value   : LSP.Messages.InitializeParams)
   is
      Message : LSP.Messages.Requests.Initialize_Request :=
        (params => Value, others => <>);
   begin
      Self.Send_Request
        (Request, "initialize", Decoders.Initialize_Response'Access, Message);
   end Initialize_Request;

   --------------------
   -- On_Raw_Message --
   --------------------

   overriding procedure On_Raw_Message
     (Self : in out Client;
      Data : Ada.Strings.Unbounded.Unbounded_String)
   is
      function Get_Id
        (JSON   : GNATCOLL.JSON.JSON_Array)
           return LSP.Types.LSP_Number_Or_String;

      ------------
      -- Get_Id --
      ------------

      function Get_Id
        (JSON   : GNATCOLL.JSON.JSON_Array)
          return LSP.Types.LSP_Number_Or_String
      is
         Stream : aliased LSP.JSON_Streams.JSON_Stream;
         Result : LSP.Types.LSP_Number_Or_String;
      begin
         Stream.Set_JSON_Document (JSON);
         Stream.Start_Object;
         LSP.Types.Read_Number_Or_String (Stream, +"id", Result);

         return Result;
      end Get_Id;

      Value  : constant GNATCOLL.JSON.JSON_Value := GNATCOLL.JSON.Read (Data);
      JSON   : GNATCOLL.JSON.JSON_Array;
      Stream : aliased LSP.JSON_Streams.JSON_Stream;
      Id     : LSP.Types.LSP_Number_Or_String;
   begin
      GNATCOLL.JSON.Append (JSON, Value);
      Stream.Set_JSON_Document (JSON);

      if Value.Has_Field ("id") then
         Id := Get_Id (JSON);

         if Value.Has_Field ("method") then
            --   Request from server;
            if Value.Get ("method") = "workspace/applyEdit" then
               declare
                  Params : LSP.Messages.ApplyWorkspaceEditParams;
               begin
                  Stream.Start_Object;
                  Stream.Key ("params");
                  LSP.Messages.ApplyWorkspaceEditParams'Read
                    (Stream'Access, Params);

                  Self.Request_Handler.Workspace_Apply_Edit
                    (Request => Id,
                     Params  => Params);
               end;
            else
               declare
                  Error : LSP.Messages.ResponseMessage :=
                    (Is_Error => True,
                     error =>
                       (True,
                        (code => LSP.Messages.MethodNotFound,
                         message =>
                           +("Unknown method:" & Value.Get ("method")),
                         data => <>)),
                     others => <>);
               begin
                  Self.Send_Response (Id, Error);
               end;
            end if;
         else
            --  Response from server

            if Id.Is_Number and then Self.Request_Map.Contains (Id.Number) then
               Self.Request_Map (Id.Number).all
                 (Stream'Access, Id.Number, Self.Response_Handler);
            else
               raise Constraint_Error with "Unknown request id";
            end if;
         end if;

      elsif Value.Has_Field ("method") then
         --  Notification from server

         declare
            Position : constant Notification_Maps.Cursor :=
                         Self.Notif_Decoders.Find (Value.Get ("method"));
         begin
            if Notification_Maps.Has_Element (Position) then
               Notification_Maps.Element (Position).all
                 (Stream'Access, Self.Notification);
            end if;
         end;
      end if;
   end On_Raw_Message;

   -----------------------
   -- Send_Notification --
   -----------------------

   procedure Send_Notification
     (Self   : in out Client'Class;
      Method : Ada.Strings.UTF_Encoding.UTF_8_String;
      Value  : in out LSP.Messages.NotificationMessage'Class)
   is
      JS : aliased LSP.JSON_Streams.JSON_Stream;
      JSON : GNATCOLL.JSON.JSON_Value;
   begin
      Value.jsonrpc := +"2.0";
      Value.method := +Method;
      LSP.Messages.NotificationMessage'Class'Write (JS'Access, Value);
      JSON := GNATCOLL.JSON.Get (JS.Get_JSON_Document, 1);
      Self.Send_Message (JSON.Write);
   end Send_Notification;

   ------------------
   -- Send_Request --
   ------------------

   procedure Send_Request
     (Self    : in out Client'Class;
      Request : out LSP.Types.LSP_Number;
      Method  : Ada.Strings.UTF_Encoding.UTF_8_String;
      Decoder : Response_Decoder;
      Value   : in out LSP.Messages.RequestMessage'Class)
   is
      JS : aliased LSP.JSON_Streams.JSON_Stream;
      JSON : GNATCOLL.JSON.JSON_Value;
   begin
      Request := Self.Allocate_Request_Id.Number;
      Self.Request_Map.Insert (Request, Decoder);

      Value.jsonrpc := +"2.0";
      Value.method := +Method;
      Value.id := (True, Request);
      LSP.Messages.RequestMessage'Class'Write (JS'Access, Value);
      JSON := GNATCOLL.JSON.Get (JS.Get_JSON_Document, 1);
      Self.Send_Message (JSON.Write);
   end Send_Request;

   ------------------------------
   -- Set_Notification_Handler --
   ------------------------------

   procedure Set_Notification_Handler
     (Self  : in out Client'Class;
      Value : access Client_Notifications.Client_Notification_Handler'Class) is
   begin
      Self.Notification := Value;
   end Set_Notification_Handler;

   -------------------------
   -- Set_Request_Handler --
   -------------------------

   procedure Set_Request_Handler
     (Self  : in out Client'Class;
      Value : access LSP.Clients.Request_Handlers.Request_Handler'Class) is
   begin
      Self.Request_Handler := Value;
   end Set_Request_Handler;

   --------------------------
   -- Set_Response_Handler --
   --------------------------

   procedure Set_Response_Handler
     (Self  : in out Client'Class;
      Value : access LSP.Clients.Response_Handlers.Response_Handler'Class) is
   begin
      Self.Response_Handler := Value;
   end Set_Response_Handler;

   ----------------------
   -- Shutdown_Request --
   ----------------------

   procedure Shutdown_Request
     (Self    : in out Client'Class;
      Request : out LSP.Types.LSP_Number)
   is
      Message : LSP.Messages.Requests.Shutdown_Request;
   begin
      Self.Send_Request
        (Request,
         "shutdown",
         Decoders.Shutdown_Response'Access,
         Message);
   end Shutdown_Request;

   -------------------
   -- Send_Response --
   -------------------

   procedure Send_Response
     (Self    : in out Client'Class;
      Request : LSP.Types.LSP_Number_Or_String;
      Value   : in out LSP.Messages.ResponseMessage'Class)
   is
      JS : aliased LSP.JSON_Streams.JSON_Stream;
      JSON : GNATCOLL.JSON.JSON_Value;
   begin
      Value.jsonrpc := +"2.0";
      Value.id := Request;
      LSP.Messages.ResponseMessage'Class'Write (JS'Access, Value);
      JSON := GNATCOLL.JSON.Get (JS.Get_JSON_Document, 1);
      Self.Send_Message (JSON.Write);
   end Send_Response;

   ---------------------------------------
   -- Text_Document_Code_Action_Request --
   ---------------------------------------

   procedure Text_Document_Code_Action_Request
     (Self    : in out Client'Class;
      Request : out LSP.Types.LSP_Number;
      Value   : LSP.Messages.CodeActionParams)
   is
      Message : CodeAction_Request := (params => Value, others => <>);
   begin
      Self.Send_Request
        (Request,
         "textDocument/codeAction",
         Decoders.Text_Document_Code_Action_Response'Access,
         Message);
   end Text_Document_Code_Action_Request;

   --------------------------------------
   -- Text_Document_Completion_Request --
   --------------------------------------

   procedure Text_Document_Completion_Request
     (Self    : in out Client'Class;
      Request : out LSP.Types.LSP_Number;
      Value   : LSP.Messages.TextDocumentPositionParams)
   is
      Message : Completion_Request := (params => Value, others => <>);
   begin
      Self.Send_Request
        (Request,
         "textDocument/completion",
         Decoders.Text_Document_Completion_Response'Access,
         Message);
   end Text_Document_Completion_Request;

   --------------------------------------
   -- Text_Document_Definition_Request --
   --------------------------------------

   procedure Text_Document_Definition_Request
     (Self    : in out Client'Class;
      Request : out LSP.Types.LSP_Number;
      Value   : LSP.Messages.TextDocumentPositionParams)
   is
      Message : Definition_Request := (params => Value, others => <>);
   begin
      Self.Send_Request
        (Request,
         "textDocument/definition",
         Decoders.Text_Document_Definition_Response'Access,
         Message);
   end Text_Document_Definition_Request;

   ------------------------------
   -- Text_Document_Did_Change --
   ------------------------------

   procedure Text_Document_Did_Change
     (Self  : access Client;
      Value : LSP.Messages.DidChangeTextDocumentParams)
   is
      Message : DidChangeTextDocument_Notification :=
        (params => Value, others => <>);
   begin
      Self.Send_Notification ("textDocument/didChange", Message);
   end Text_Document_Did_Change;

   -----------------------------
   -- Text_Document_Did_Close --
   -----------------------------

   procedure Text_Document_Did_Close
     (Self  : access Client;
      Value : LSP.Messages.DidCloseTextDocumentParams)
   is
      Message : DidCloseTextDocument_Notification :=
        (params => Value, others => <>);
   begin
      Self.Send_Notification ("textDocument/didClose", Message);
   end Text_Document_Did_Close;

   ----------------------------
   -- Text_Document_Did_Open --
   ----------------------------

   procedure Text_Document_Did_Open
     (Self  : access Client;
      Value : LSP.Messages.DidOpenTextDocumentParams)
   is
      Message : DidOpenTextDocument_Notification :=
        (params => Value, others => <>);
   begin
      Self.Send_Notification ("textDocument/didOpen", Message);
   end Text_Document_Did_Open;

   ----------------------------
   -- Text_Document_Did_Save --
   ----------------------------

   procedure Text_Document_Did_Save
     (Self  : access Client;
      Value : LSP.Messages.DidSaveTextDocumentParams)
   is
      Message : DidSaveTextDocument_Notification :=
        (params => Value, others => <>);
   begin
      Self.Send_Notification ("textDocument/didSave", Message);
   end Text_Document_Did_Save;

   -------------------------------------
   -- Text_Document_Highlight_Request --
   -------------------------------------

   procedure Text_Document_Highlight_Request
     (Self    : in out Client'Class;
      Request : out LSP.Types.LSP_Number;
      Value   : LSP.Messages.TextDocumentPositionParams)
   is
      Message : Highlight_Request := (params => Value, others => <>);
   begin
      Self.Send_Request
        (Request,
         "textDocument/documentHighlight",
         Decoders.Text_Document_Highlight_Response'Access,
         Message);
   end Text_Document_Highlight_Request;

   ---------------------------------
   -- Text_Document_Hover_Request --
   ---------------------------------

   procedure Text_Document_Hover_Request
     (Self    : in out Client'Class;
      Request : out LSP.Types.LSP_Number;
      Value   : LSP.Messages.TextDocumentPositionParams)
   is
      Message : Hover_Request := (params => Value, others => <>);
   begin
      Self.Send_Request
        (Request,
         "textDocument/hover",
         Decoders.Text_Document_Hover_Response'Access,
         Message);
   end Text_Document_Hover_Request;

   --------------------------------------
   -- Text_Document_References_Request --
   --------------------------------------

   procedure Text_Document_References_Request
     (Self    : in out Client'Class;
      Request : out LSP.Types.LSP_Number;
      Value   : LSP.Messages.ReferenceParams)
   is
      Message : References_Request := (params => Value, others => <>);
   begin
      Self.Send_Request
        (Request,
         "textDocument/references",
         Decoders.Text_Document_References_Response'Access,
         Message);
   end Text_Document_References_Request;

   ------------------------------------------
   -- Text_Document_Signature_Help_Request --
   ------------------------------------------

   procedure Text_Document_Signature_Help_Request
     (Self    : in out Client'Class;
      Request : out LSP.Types.LSP_Number;
      Value   : LSP.Messages.TextDocumentPositionParams)
   is
      Message : Signature_Help_Request := (params => Value, others => <>);
   begin
      Self.Send_Request
        (Request,
         "textDocument/signatureHelp",
         Decoders.Text_Document_Signature_Help_Response'Access,
         Message);
   end Text_Document_Signature_Help_Request;

   ----------------------------------
   -- Text_Document_Symbol_Request --
   ----------------------------------

   procedure Text_Document_Symbol_Request
     (Self    : in out Client'Class;
      Request : out LSP.Types.LSP_Number;
      Value   : LSP.Messages.DocumentSymbolParams)
   is
      Message : Document_Symbols_Request := (params => Value, others => <>);
   begin
      Self.Send_Request
        (Request, "textDocument/documentSymbol",
         Decoders.Text_Document_Symbol_Response'Access,
         Message);
   end Text_Document_Symbol_Request;

   --------------------------
   -- Workspace_Apply_Edit --
   --------------------------

   procedure Workspace_Apply_Edit
     (Self    : in out Client'Class;
      Request : LSP.Types.LSP_Number_Or_String;
      Applied : Boolean)
   is
      Message : LSP.Messages.ApplyWorkspaceEdit_Response :=
        (Is_Error => False,
         result => (applied => Applied),
         error => (Is_Set => False),
         others => <>);
   begin
      Self.Send_Response (Request, Message);
   end Workspace_Apply_Edit;

   -----------------
   -- Initialized --
   -----------------

   procedure Initialized (Self : access Client) is
      Message : Initialized_Notification := (others => <>);
   begin
      Self.Send_Notification ("initialized", Message);
   end Initialized;

   ----------------------------------------
   -- Workspace_Did_Change_Configuration --
   ----------------------------------------

   procedure Workspace_Did_Change_Configuration
     (Self  : access Client;
      Value :  LSP.Messages.DidChangeConfigurationParams)
   is
      Message : DidChangeConfiguration_Notification :=
        (params => Value, others => <>);
   begin
      Self.Send_Notification ("workspace/didChangeConfiguration", Message);
   end Workspace_Did_Change_Configuration;

   ---------------------------------------
   -- Workspace_Execute_Command_Request --
   ---------------------------------------

   procedure Workspace_Execute_Command_Request
     (Self    : in out Client'Class;
      Request : out LSP.Types.LSP_Number;
      Value   : LSP.Messages.ExecuteCommandParams)
   is
      Message : Execute_Command_Request := (params => Value, others => <>);
   begin
      Self.Send_Request
        (Request,
         "workspace/executeCommand",
         Decoders.Workspace_Execute_Command_Response'Access,
         Message);
   end Workspace_Execute_Command_Request;

   ------------------------------
   -- Workspace_Symbol_Request --
   ------------------------------

   procedure Workspace_Symbol_Request
     (Self    : in out Client'Class;
      Request : out LSP.Types.LSP_Number;
      Value   : LSP.Messages.WorkspaceSymbolParams)
   is
      Message : Workspace_Symbols_Request := (params => Value, others => <>);
   begin
      Self.Send_Request
        (Request,
         "workspace/symbol",
         Decoders.Workspace_Symbol_Response'Access,
         Message);
   end Workspace_Symbol_Request;

   -------------------------
   -- Handle_Notification --
   -------------------------

   overriding procedure Handle_Notification
     (Self         : access Client;
      Notification : LSP.Messages.NotificationMessage'Class) is
   begin
      if Notification in Initialized_Notification'Class then
         Self.Initialized;
      elsif Notification in
        LSP.Messages.Notifications.Exit_Notification'Class
      then
         Self.Exit_Notification;
      elsif Notification in DidChangeConfiguration_Notification'Class then
         Self.Workspace_Did_Change_Configuration
           (DidChangeConfiguration_Notification (Notification).params);
      elsif Notification in DidOpenTextDocument_Notification'Class then
         Self.Text_Document_Did_Open
           (DidOpenTextDocument_Notification (Notification).params);
      elsif Notification in DidChangeTextDocument_Notification'Class then
         Self.Text_Document_Did_Change
           (DidChangeTextDocument_Notification (Notification).params);
      elsif Notification in DidSaveTextDocument_Notification'Class then
         Self.Text_Document_Did_Save
           (DidSaveTextDocument_Notification (Notification).params);
      elsif Notification in DidCloseTextDocument_Notification'Class then
         Self.Text_Document_Did_Close
           (DidCloseTextDocument_Notification (Notification).params);
      end if;
   end Handle_Notification;

end LSP.Clients;
