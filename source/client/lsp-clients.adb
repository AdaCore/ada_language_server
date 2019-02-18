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

   package body Decoders is

      procedure Initialize_Response
        (Stream  : access Ada.Streams.Root_Stream_Type'Class;
         Request : LSP.Types.LSP_Number;
         Handler : access LSP.Clients.Response_Handlers.Response_Handler'Class)
      is
         Response : LSP.Messages.Initialize_Response;
      begin
         LSP.Messages.Initialize_Response'Read (Stream, Response);
         Handler.Initialize_Response (Request, Response);
      end Initialize_Response;

      -----------------
      -- Log_Message --
      -----------------

      procedure Log_Message
        (Stream  : access Ada.Streams.Root_Stream_Type'Class;
         Handler : access
           LSP.Client_Notifications.Client_Notification_Handler'Class)
      is
         Message : LSP.Messages.LogMessage_Notification;
      begin
         LSP.Messages.LogMessage_Notification'Read (Stream, Message);
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
         Message : LSP.Messages.PublishDiagnostics_Notification;
      begin
         LSP.Messages.PublishDiagnostics_Notification'Read (Stream, Message);
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
         Message : LSP.Messages.ShowMessage_Notification;
      begin
         LSP.Messages.ShowMessage_Notification'Read (Stream, Message);
         Handler.Show_Message (Message.params);
      end Show_Message;

   end Decoders;

   -----------------------
   -- Exit_Notification --
   -----------------------

   overriding procedure Exit_Notification (Self : access Client) is
      Message : LSP.Messages.Exit_Notification;
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
      Message : LSP.Messages.Initialize_Request :=
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
                  Stream.Key (+"params");
                  LSP.Messages.ApplyWorkspaceEditParams'Read
                    (Stream'Access, Params);

                  Self.Request_Handler.Workspace_Apply_Edit
                    (Request => Id,
                     Params  => Params);
               end;
            else
               declare
                  Error : LSP.Messages.ResponseMessage :=
                    (error =>
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
         Self.Notif_Decoders (Value.Get ("method")).all
           (Stream'Access, Self.Notification);
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
      Self.Request_Id := Self.Request_Id + 1;
      Request := Self.Request_Id;
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
      Message : LSP.Messages.Shutdown_Request;
   begin
      Self.Send_Request (Request, "shutdown", null, Message);
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
      Message : LSP.Messages.CodeAction_Request :=
        (params => Value, others => <>);
   begin
      Self.Send_Request (Request, "textDocument/codeAction", null, Message);
   end Text_Document_Code_Action_Request;

   --------------------------------------
   -- Text_Document_Completion_Request --
   --------------------------------------

   procedure Text_Document_Completion_Request
     (Self    : in out Client'Class;
      Request : out LSP.Types.LSP_Number;
      Value   : LSP.Messages.TextDocumentPositionParams)
   is
      Message : LSP.Messages.Completion_Request :=
        (params => Value, others => <>);
   begin
      Self.Send_Request (Request, "textDocument/completion", null, Message);
   end Text_Document_Completion_Request;

   --------------------------------------
   -- Text_Document_Definition_Request --
   --------------------------------------

   procedure Text_Document_Definition_Request
     (Self    : in out Client'Class;
      Request : out LSP.Types.LSP_Number;
      Value   : LSP.Messages.TextDocumentPositionParams)
   is
      Message : LSP.Messages.Definition_Request :=
        (params => Value, others => <>);
   begin
      Self.Send_Request (Request, "textDocument/definition", null, Message);
   end Text_Document_Definition_Request;

   ------------------------------
   -- Text_Document_Did_Change --
   ------------------------------

   overriding procedure Text_Document_Did_Change
     (Self  : access Client;
      Value : LSP.Messages.DidChangeTextDocumentParams)
   is
      Message : LSP.Messages.DidChangeTextDocument_Notification :=
        (params => Value, others => <>);
   begin
      Self.Send_Notification ("textDocument/didChange", Message);
   end Text_Document_Did_Change;

   -----------------------------
   -- Text_Document_Did_Close --
   -----------------------------

   overriding procedure Text_Document_Did_Close
     (Self  : access Client;
      Value : LSP.Messages.DidCloseTextDocumentParams)
   is
      Message : LSP.Messages.DidCloseTextDocument_Notification :=
        (params => Value, others => <>);
   begin
      Self.Send_Notification ("textDocument/didClose", Message);
   end Text_Document_Did_Close;

   ----------------------------
   -- Text_Document_Did_Open --
   ----------------------------

   overriding procedure Text_Document_Did_Open
     (Self  : access Client;
      Value : LSP.Messages.DidOpenTextDocumentParams)
   is
      Message : LSP.Messages.DidOpenTextDocument_Notification :=
        (params => Value, others => <>);
   begin
      Self.Send_Notification ("textDocument/didOpen", Message);
   end Text_Document_Did_Open;

   ----------------------------
   -- Text_Document_Did_Save --
   ----------------------------

   overriding procedure Text_Document_Did_Save
     (Self  : access Client;
      Value : LSP.Messages.DidSaveTextDocumentParams)
   is
      Message : LSP.Messages.DidSaveTextDocument_Notification :=
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
      Message : LSP.Messages.Highlight_Request :=
        (params => Value, others => <>);
   begin
      Self.Send_Request
        (Request, "textDocument/documentHighlight", null, Message);
   end Text_Document_Highlight_Request;

   ---------------------------------
   -- Text_Document_Hover_Request --
   ---------------------------------

   procedure Text_Document_Hover_Request
     (Self    : in out Client'Class;
      Request : out LSP.Types.LSP_Number;
      Value   : LSP.Messages.TextDocumentPositionParams)
   is
      Message : LSP.Messages.Hover_Request :=
        (params => Value, others => <>);
   begin
      Self.Send_Request (Request, "textDocument/hover", null, Message);
   end Text_Document_Hover_Request;

   --------------------------------------
   -- Text_Document_References_Request --
   --------------------------------------

   procedure Text_Document_References_Request
     (Self    : in out Client'Class;
      Request : out LSP.Types.LSP_Number;
      Value   : LSP.Messages.ReferenceParams)
   is
      Message : LSP.Messages.References_Request :=
        (params => Value, others => <>);
   begin
      Self.Send_Request (Request, "textDocument/references", null, Message);
   end Text_Document_References_Request;

   ------------------------------------------
   -- Text_Document_Signature_Help_Request --
   ------------------------------------------

   procedure Text_Document_Signature_Help_Request
     (Self    : in out Client'Class;
      Request : out LSP.Types.LSP_Number;
      Value   : LSP.Messages.TextDocumentPositionParams)
   is
      Message : LSP.Messages.Signature_Help_Request :=
        (params => Value, others => <>);
   begin
      Self.Send_Request (Request, "textDocument/signatureHelp", null, Message);
   end Text_Document_Signature_Help_Request;

   ----------------------------------
   -- Text_Document_Symbol_Request --
   ----------------------------------

   procedure Text_Document_Symbol_Request
     (Self    : in out Client'Class;
      Request : out LSP.Types.LSP_Number;
      Value   : LSP.Messages.DocumentSymbolParams)
   is
      Message : LSP.Messages.Document_Symbols_Request :=
        (params => Value, others => <>);
   begin
      Self.Send_Request
        (Request, "textDocument/documentSymbol", null, Message);
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
        (result => (applied => Applied),
         others => <>);
   begin
      Self.Send_Response (Request, Message);
   end Workspace_Apply_Edit;

   -----------------
   -- Initialized --
   -----------------

   overriding procedure Initialized (Self : access Client) is
      Message : LSP.Messages.Initialized_Notification :=
                  (others => <>);
   begin
      Self.Send_Notification ("initialized", Message);
   end Initialized;

   ----------------------------------------
   -- Workspace_Did_Change_Configuration --
   ----------------------------------------

   overriding procedure Workspace_Did_Change_Configuration
     (Self  : access Client;
      Value :  LSP.Messages.DidChangeConfigurationParams)
   is
      Message : LSP.Messages.DidChangeConfiguration_Notification :=
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
      Message : LSP.Messages.Execute_Command_Request :=
        (params => Value, others => <>);
   begin
      Self.Send_Request (Request, "workspace/executeCommand", null, Message);
   end Workspace_Execute_Command_Request;

   ------------------------------
   -- Workspace_Symbol_Request --
   ------------------------------

   procedure Workspace_Symbol_Request
     (Self    : in out Client'Class;
      Request : out LSP.Types.LSP_Number;
      Value   : LSP.Messages.WorkspaceSymbolParams)
   is
      Message : LSP.Messages.Workspace_Symbols_Request :=
        (params => Value, others => <>);
   begin
      Self.Send_Request (Request, "workspace/symbol", null, Message);
   end Workspace_Symbol_Request;

end LSP.Clients;
