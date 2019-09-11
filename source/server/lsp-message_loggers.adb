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

with Ada.Containers;
with LSP.Types;

with Ada.Strings.UTF_Encoding.Wide_Strings;
with GNATCOLL.JSON;

package body LSP.Message_Loggers is

   function "+" (Text : LSP.Types.LSP_String) return String
     renames LSP.Types.To_UTF_8_String;

   function Image (Value : LSP.Messages.RequestMessage'Class) return String;
   function Image (Value : LSP.Messages.ResponseMessage'Class) return String;
   function Image (Value : LSP.Messages.Position) return String;
   function Image (Value : LSP.Messages.Span) return String;

   function Image
     (Value : LSP.Messages.TextDocumentPositionParams'Class) return String;

   -----------
   -- Image --
   -----------

   function Image (Value : LSP.Messages.Position) return String is
      Line : constant String := LSP.Types.Line_Number'Image (Value.line);
      Col  : constant String := LSP.Types.UTF_16_Index'Image (Value.character);
   begin
      return ':' & Line (2 .. Line'Last) & ':' & Col (2 .. Col'Last);
   end Image;

   -----------
   -- Image --
   -----------

   function Image (Value : LSP.Messages.RequestMessage'Class) return String is
      Prefix : constant String := "Request ";
   begin
      if Value.id.Is_Number then
         return Prefix & LSP.Types.LSP_Number'Image (Value.id.Number) & ' ';
      else
         return Prefix & (+Value.id.String) & ' ';
      end if;
   end Image;

   -----------
   -- Image --
   -----------

   function Image (Value : LSP.Messages.ResponseMessage'Class) return String is
      Prefix : constant String := "Response ";
   begin
      if Value.id.Is_Number then
         return Prefix & LSP.Types.LSP_Number'Image (Value.id.Number) & ' ';
      else
         return Prefix & (+Value.id.String) & ' ';
      end if;
   end Image;

   -----------
   -- Image --
   -----------

   function Image (Value : LSP.Messages.Span) return String is
   begin
      return '[' & Image (Value.first) & ".." & Image (Value.last) & ']';
   end Image;

   -----------
   -- Image --
   -----------

   function Image
     (Value : LSP.Messages.TextDocumentPositionParams'Class) return String is
   begin
      return (+Value.textDocument.uri) & Image (Value.position);
   end Image;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self  : in out Message_Logger;
      Trace : not null GNATCOLL.Traces.Trace_Handle) is
   begin
      Self.Trace := Trace;
   end Initialize;

   ------------------------------
   -- On_ALS_Called_By_Request --
   ------------------------------

   overriding procedure On_ALS_Called_By_Request
     (Self  : access Message_Logger;
      Value : LSP.Messages.Server_Requests.ALS_Called_By_Request)
   is
   begin
      Self.Trace.Trace
        ("ALS_Called_By_Request: "
         & Image (Value)
         & Image (Value.params));
   end On_ALS_Called_By_Request;

   -------------------------------
   -- On_ALS_Called_By_Response --
   -------------------------------

   overriding procedure On_ALS_Called_By_Response
     (Self   : in out Message_Logger;
      Value  : LSP.Messages.Server_Responses.ALS_Called_By_Response) is
   begin
      if Value.Is_Error then
         Self.Trace.Trace
           ("ALS_Called_By_Response: "
            & Image (Value)
            & " Error");
         return;
      end if;

      Self.Trace.Trace
        ("ALS_Called_By_Response: "
         & Image (Value)
         & Ada.Containers.Count_Type'Image (Value.result.Length));
   end On_ALS_Called_By_Response;

   ------------------------------------
   -- On_ApplyWorkspaceEdit_Response --
   ------------------------------------

   overriding procedure On_ApplyWorkspaceEdit_Response
     (Self   : in out Message_Logger;
      Value  : LSP.Messages.Client_Responses.ApplyWorkspaceEdit_Response)
   is
   begin
      if Value.Is_Error then
         Self.Trace.Trace
           ("ApplyWorkspaceEdit_Response: "
            & Image (Value)
            & " Error");
         return;
      end if;

      Self.Trace.Trace
        ("ApplyWorkspaceEdit_Response: "
         & Image (Value)
         & Boolean'Image (Value.result.applied));
   end On_ApplyWorkspaceEdit_Response;

   ----------------------------
   -- On_Cancel_Notification --
   ----------------------------

   overriding procedure On_Cancel_Notification
     (Self  : access Message_Logger;
      Value : LSP.Messages.CancelParams)
   is
      Id : constant String :=
        (if Value.id.Is_Number then Value.id.Number'Image
         else '"' & (+Value.id.String) & '"');
   begin
      Self.Trace.Trace ("On_Cancel_Notification: " & Id);
   end On_Cancel_Notification;

   ----------------------------
   -- On_Client_Notification --
   ----------------------------

   overriding procedure On_Client_Notification
     (Self   : access Message_Logger;
      Value  : LSP.Messages.Client_Notifications.Client_Notification'Class) is
   begin
      Value.Visit (Self);
   end On_Client_Notification;

   -----------------------
   -- On_Client_Request --
   -----------------------

   overriding procedure On_Client_Request
     (Self   : access Message_Logger;
      Value  : LSP.Messages.Client_Requests.Client_Request'Class) is
   begin
      Value.Visit (Self);
   end On_Client_Request;

   ------------------------
   -- On_Client_Response --
   ------------------------

   overriding procedure On_Client_Response
     (Self   : access Message_Logger;
      Value  : LSP.Messages.Client_Responses.Client_Response'Class) is
   begin
      Value.Visit (Self);
   end On_Client_Response;

   ---------------------------
   -- On_CodeAction_Request --
   ---------------------------

   overriding procedure On_CodeAction_Request
     (Self  : access Message_Logger;
      Value : LSP.Messages.Server_Requests.CodeAction_Request)
   is
   begin
      Self.Trace.Trace
        ("CodeAction_Request: "
         & Image (Value)
         & (+Value.params.textDocument.uri)
         & Image (Value.params.span));
   end On_CodeAction_Request;

   ----------------------------
   -- On_CodeAction_Response --
   ----------------------------

   overriding procedure On_CodeAction_Response
     (Self   : in out Message_Logger;
      Value  : LSP.Messages.Server_Responses.CodeAction_Response) is
   begin
      if Value.Is_Error then
         Self.Trace.Trace
           ("CodeAction_Response: "
            & Image (Value)
            & " Error");
         return;
      end if;

      Self.Trace.Trace
        ("CodeAction_Response: "
         & Image (Value)
         & Ada.Containers.Count_Type'Image (Value.result.Length));
   end On_CodeAction_Response;

   ---------------------------
   -- On_Completion_Request --
   ---------------------------

   overriding procedure On_Completion_Request
     (Self  : access Message_Logger;
      Value : LSP.Messages.Server_Requests.Completion_Request)
   is
   begin
      Self.Trace.Trace
        ("Completion_Request: "
         & Image (Value)
         & Image (Value.params));
   end On_Completion_Request;

   ----------------------------
   -- On_Completion_Response --
   ----------------------------

   overriding procedure On_Completion_Response
     (Self   : in out Message_Logger;
      Value  : LSP.Messages.Server_Responses.Completion_Response) is
   begin
      if Value.Is_Error then
         Self.Trace.Trace
           ("Completion_Response: "
            & Image (Value)
            & " Error");
         return;
      end if;

      Self.Trace.Trace
        ("Completion_Response: "
         & Image (Value)
         & Ada.Containers.Count_Type'Image (Value.result.items.Length));
   end On_Completion_Response;

   ---------------------------
   -- On_Definition_Request --
   ---------------------------

   overriding procedure On_Definition_Request
     (Self  : access Message_Logger;
      Value : LSP.Messages.Server_Requests.Definition_Request) is
   begin
      Self.Trace.Trace
        ("Definition_Request: "
         & Image (Value)
         & Image (Value.params));
   end On_Definition_Request;

   --------------------------------------------
   -- On_DidChangeConfiguration_Notification --
   --------------------------------------------

   overriding procedure On_DidChangeConfiguration_Notification
     (Self  : access Message_Logger;
      Value : LSP.Messages.DidChangeConfigurationParams)
   is
      use all type LSP.Types.LSP_String;

      procedure Each (Name : String; Value : GNATCOLL.JSON.JSON_Value);
      --  Append Value image into Image variable

      Image : LSP.Types.LSP_String;

      ----------
      -- Each --
      ----------

      procedure Each (Name : String; Value : GNATCOLL.JSON.JSON_Value) is
         Field : constant Wide_String :=
           Ada.Strings.UTF_Encoding.Wide_Strings.Decode (Name);
      begin
         Append (Image, Field);
         Append (Image, "=");

         case Value.Kind is
            when GNATCOLL.JSON.JSON_String_Type =>
               Append
                 (Image,
                  Ada.Strings.UTF_Encoding.Wide_Strings.Decode (Value.Get));
            when GNATCOLL.JSON.JSON_Object_Type =>
               Append (Image, "(");
               Value.Map_JSON_Object (Each'Access);
               Append (Image, ")");
            when others =>
               Append
                 (Image, "..."
                  & GNATCOLL.JSON.JSON_Value_Type'Wide_Image (Value.Kind));
         end case;
      end Each;

   begin
      Value.settings.Map_JSON_Object (Each'Access);

      Self.Trace.Trace
        ("DidChangeConfiguration_Notification: "
         & (+Image));
   end On_DidChangeConfiguration_Notification;

   -------------------------------------------
   -- On_DidChangeTextDocument_Notification --
   -------------------------------------------

   overriding procedure On_DidChangeTextDocument_Notification
     (Self  : access Message_Logger;
      Value : LSP.Messages.DidChangeTextDocumentParams) is
   begin
      Self.Trace.Trace
        ("DidChangeTextDocument_Notification: "
         & (+Value.textDocument.uri));
   end On_DidChangeTextDocument_Notification;

   ------------------------------------------
   -- On_DidCloseTextDocument_Notification --
   ------------------------------------------

   overriding procedure On_DidCloseTextDocument_Notification
     (Self  : access Message_Logger;
      Value : LSP.Messages.DidCloseTextDocumentParams)
   is
   begin
      Self.Trace.Trace
        ("DidChangeTextDocument_Notification: "
         & (+Value.textDocument.uri));
   end On_DidCloseTextDocument_Notification;

   -----------------------------------------
   -- On_DidOpenTextDocument_Notification --
   -----------------------------------------

   overriding procedure On_DidOpenTextDocument_Notification
     (Self  : access Message_Logger;
      Value : LSP.Messages.DidOpenTextDocumentParams)
   is
   begin
      Self.Trace.Trace
        ("On_DidOpenTextDocument_Notification: "
         & (+Value.textDocument.uri));
   end On_DidOpenTextDocument_Notification;

   -----------------------------------------
   -- On_DidSaveTextDocument_Notification --
   -----------------------------------------

   overriding procedure On_DidSaveTextDocument_Notification
     (Self  : access Message_Logger;
      Value : LSP.Messages.DidSaveTextDocumentParams)
   is
   begin
      Self.Trace.Trace
        ("On_DidSaveTextDocument_Notification: "
         & (+Value.textDocument.uri));
   end On_DidSaveTextDocument_Notification;

   ---------------------------------
   -- On_Document_Symbols_Request --
   ---------------------------------

   overriding procedure On_Document_Symbols_Request
     (Self  : access Message_Logger;
      Value : LSP.Messages.Server_Requests.Document_Symbols_Request)
   is
   begin
      Self.Trace.Trace
        ("Document_Symbols_Request: "
         & Image (Value)
         & (+Value.params.textDocument.uri));
   end On_Document_Symbols_Request;

   --------------------------------
   -- On_Execute_Command_Request --
   --------------------------------

   overriding procedure On_Execute_Command_Request
     (Self  : access Message_Logger;
      Value : LSP.Messages.Server_Requests.Execute_Command_Request) is
   begin
      Self.Trace.Trace
        ("Execute_Command_Request: "
         & Image (Value)
         & (+Value.params.command));
   end On_Execute_Command_Request;

   --------------------------------
   -- On_ExecuteCommand_Response --
   --------------------------------

   overriding procedure On_ExecuteCommand_Response
     (Self   : in out Message_Logger;
      Value  : LSP.Messages.Server_Responses.ExecuteCommand_Response) is
   begin
      if Value.Is_Error then
         Self.Trace.Trace
           ("ExecuteCommand_Response: "
            & Image (Value)
            & " Error");
         return;
      end if;

      Self.Trace.Trace
        ("ExecuteCommand_Response: "
         & Image (Value));
   end On_ExecuteCommand_Response;

   --------------------------
   -- On_Exit_Notification --
   --------------------------

   overriding procedure On_Exit_Notification (Self : access Message_Logger) is
   begin
      Self.Trace.Trace ("Exit_Notification: ");
   end On_Exit_Notification;

   --------------------------
   -- On_Highlight_Request --
   --------------------------

   overriding procedure On_Highlight_Request
     (Self  : access Message_Logger;
      Value : LSP.Messages.Server_Requests.Highlight_Request) is
   begin
      Self.Trace.Trace
        ("Highlight_Request: "
         & Image (Value)
         & Image (Value.params));
   end On_Highlight_Request;

   ---------------------------
   -- On_Highlight_Response --
   ---------------------------

   overriding procedure On_Highlight_Response
     (Self   : in out Message_Logger;
      Value  : LSP.Messages.Server_Responses.Highlight_Response) is
   begin
      if Value.Is_Error then
         Self.Trace.Trace
           ("Highlight_Response: "
            & Image (Value)
            & " Error");
         return;
      end if;

      Self.Trace.Trace
        ("Highlight_Response: "
         & Image (Value)
         & Ada.Containers.Count_Type'Image (Value.result.Length));
   end On_Highlight_Response;

   ----------------------
   -- On_Hover_Request --
   ----------------------

   overriding procedure On_Hover_Request
     (Self  : access Message_Logger;
      Value : LSP.Messages.Server_Requests.Hover_Request) is
   begin
      Self.Trace.Trace
        ("Hover_Request: "
         & Image (Value)
         & Image (Value.params));
   end On_Hover_Request;

   -----------------------
   -- On_Hover_Response --
   -----------------------

   overriding procedure On_Hover_Response
     (Self   : in out Message_Logger;
      Value  : LSP.Messages.Server_Responses.Hover_Response) is
   begin
      if Value.Is_Error then
         Self.Trace.Trace
           ("Hover_Response: "
            & Image (Value)
            & " Error");
         return;
      end if;

      Self.Trace.Trace
        ("Hover_Response: "
         & Image (Value)
         & Ada.Containers.Count_Type'Image (Value.result.contents.Length));
   end On_Hover_Response;

   ---------------------------
   -- On_Initialize_Request --
   ---------------------------

   overriding procedure On_Initialize_Request
     (Self  : access Message_Logger;
      Value : LSP.Messages.Server_Requests.Initialize_Request)
   is
   begin
      Self.Trace.Trace
        ("Initialize_Request: "
         & Image (Value)
         & (+Value.params.rootPath));
   end On_Initialize_Request;

   ----------------------------
   -- On_Initialize_Response --
   ----------------------------

   overriding procedure On_Initialize_Response
     (Self   : in out Message_Logger;
      Value  : LSP.Messages.Server_Responses.Initialize_Response)
   is
   begin
      if Value.Is_Error then
         Self.Trace.Trace
           ("Initialize_Response: "
            & Image (Value)
            & " Error");
         return;
      end if;

      Self.Trace.Trace
        ("Initialize_Response: "
         & Image (Value));
   end On_Initialize_Response;

   ---------------------------------
   -- On_Initialized_Notification --
   ---------------------------------

   overriding procedure On_Initialized_Notification
     (Self : access Message_Logger) is
   begin
      Self.Trace.Trace ("Exit_Notification: ");
   end On_Initialized_Notification;

   --------------------------
   -- On_Location_Response --
   --------------------------

   overriding procedure On_Location_Response
     (Self   : in out Message_Logger;
      Value  : LSP.Messages.Server_Responses.Location_Response) is
   begin
      if Value.Is_Error then
         Self.Trace.Trace
           ("Location_Response: "
            & Image (Value)
            & " Error");
         return;
      end if;

      Self.Trace.Trace
        ("Location_Response: "
         & Image (Value)
         & Ada.Containers.Count_Type'Image (Value.result.Length));
   end On_Location_Response;

   --------------------
   -- On_Log_Message --
   --------------------

   overriding procedure On_Log_Message
     (Self   : access Message_Logger;
      Params : LSP.Messages.LogMessageParams) is
   begin
      Self.Trace.Trace
        ("Log_Message: "
         & (+Params.message));
   end On_Log_Message;

   ----------------------------
   -- On_Publish_Diagnostics --
   ----------------------------

   overriding procedure On_Publish_Diagnostics
     (Self   : access Message_Logger;
      Params : LSP.Messages.PublishDiagnosticsParams)
   is
   begin
      Self.Trace.Trace
        ("Publish_Diagnostics: "
         & (+Params.uri)
         & Ada.Containers.Count_Type'Image (Params.diagnostics.Length));
   end On_Publish_Diagnostics;

   ---------------------------
   -- On_References_Request --
   ---------------------------

   overriding procedure On_References_Request
     (Self  : access Message_Logger;
      Value : LSP.Messages.Server_Requests.References_Request) is
   begin
      Self.Trace.Trace
        ("References_Request: "
         & Image (Value)
         & Image (Value.params));
   end On_References_Request;

   -----------------------
   -- On_Rename_Request --
   -----------------------

   overriding procedure On_Rename_Request
     (Self  : access Message_Logger;
      Value : LSP.Messages.Server_Requests.Rename_Request)
   is
      Pos : constant LSP.Messages.TextDocumentPositionParams :=
        (Value.params.textDocument, Value.params.position);
   begin
      Self.Trace.Trace
        ("Rename_Request: "
         & Image (Value)
         & Image (Pos)
         & " to " & (+Value.params.newName));
   end On_Rename_Request;

   ------------------------
   -- On_Rename_Response --
   ------------------------

   overriding procedure On_Rename_Response
     (Self   : in out Message_Logger;
      Value  : LSP.Messages.Server_Responses.Rename_Response) is
   begin
      if Value.Is_Error then
         Self.Trace.Trace
           ("Rename_Response: "
            & Image (Value)
            & " Error");
         return;
      end if;

      Self.Trace.Trace
        ("Rename_Response: "
         & Image (Value));
   end On_Rename_Response;

   ----------------------------
   -- On_Server_Notification --
   ----------------------------

   overriding procedure On_Server_Notification
     (Self   : access Message_Logger;
      Value  : LSP.Messages.Server_Notifications.Server_Notification'Class)
   is
   begin
      Value.Visit (Self);
   end On_Server_Notification;

   -----------------------
   -- On_Server_Request --
   -----------------------

   overriding procedure On_Server_Request
     (Self   : access Message_Logger;
      Value  : LSP.Messages.Server_Requests.Server_Request'Class) is
   begin
      Value.Visit (Self);
   end On_Server_Request;

   ------------------------
   -- On_Server_Response --
   ------------------------

   overriding procedure On_Server_Response
     (Self   : access Message_Logger;
      Value  : LSP.Messages.Server_Responses.Server_Response'Class) is
   begin
      Value.Visit (Self);
   end On_Server_Response;

   ---------------------
   -- On_Show_Message --
   ---------------------

   overriding procedure On_Show_Message
     (Self : access Message_Logger;
      Params : LSP.Messages.ShowMessageParams) is
   begin
      Self.Trace.Trace
        ("Show_Message: "
         & (+Params.message));
   end On_Show_Message;

   ----------------------------
   -- On_ShowMessage_Request --
   ----------------------------

   overriding procedure On_ShowMessage_Request
     (Self   : access Message_Logger;
      Value  : LSP.Messages.Client_Requests.ShowMessage_Request) is
   begin
      Self.Trace.Trace
        ("ShowMessage_Request: "
         & Image (Value)
         & (+Value.params.message));
   end On_ShowMessage_Request;

   -----------------------------
   -- On_ShowMessage_Response --
   -----------------------------

   overriding procedure On_ShowMessage_Response
     (Self   : in out Message_Logger;
      Value  : LSP.Messages.Client_Responses.ShowMessage_Response) is
   begin
      if Value.Is_Error then
         Self.Trace.Trace
           ("ShowMessage_Response: "
            & Image (Value)
            & " Error");
         return;
      end if;

      Self.Trace.Trace
        ("ShowMessage_Response: "
         & Image (Value));
   end On_ShowMessage_Response;

   -------------------------
   -- On_Shutdown_Request --
   -------------------------

   overriding procedure On_Shutdown_Request
     (Self  : access Message_Logger;
      Value : LSP.Messages.Server_Requests.Shutdown_Request) is
   begin
      Self.Trace.Trace
        ("ShowMessage_Request: "
         & Image (Value));
   end On_Shutdown_Request;

   --------------------------
   -- On_Shutdown_Response --
   --------------------------

   overriding procedure On_Shutdown_Response
     (Self   : in out Message_Logger;
      Value  : LSP.Messages.Server_Responses.Shutdown_Response) is
   begin
      if Value.Is_Error then
         Self.Trace.Trace
           ("Shutdown_Response: "
            & Image (Value)
            & " Error");
         return;
      end if;

      Self.Trace.Trace
        ("Shutdown_Response: "
         & Image (Value));
   end On_Shutdown_Response;

   -------------------------------
   -- On_Signature_Help_Request --
   -------------------------------

   overriding procedure On_Signature_Help_Request
     (Self  : access Message_Logger;
      Value : LSP.Messages.Server_Requests.Signature_Help_Request) is
   begin
      Self.Trace.Trace
        ("Signature_Request: "
         & Image (Value)
         & Image (Value.params));
   end On_Signature_Help_Request;

   -------------------------------
   -- On_SignatureHelp_Response --
   -------------------------------

   overriding procedure On_SignatureHelp_Response
     (Self   : in out Message_Logger;
      Value  : LSP.Messages.Server_Responses.SignatureHelp_Response)
   is
   begin
      if Value.Is_Error then
         Self.Trace.Trace
           ("SignatureHelp_Response: "
            & Image (Value)
            & " Error");
         return;
      end if;

      Self.Trace.Trace
        ("SignatureHelp_Response: "
         & Image (Value)
         & Ada.Containers.Count_Type'Image (Value.result.signatures.Length));
   end On_SignatureHelp_Response;

   ------------------------
   -- On_Symbol_Response --
   ------------------------

   overriding procedure On_Symbol_Response
     (Self   : in out Message_Logger;
      Value  : LSP.Messages.Server_Responses.Symbol_Response) is
   begin
      if Value.Is_Error then
         Self.Trace.Trace
           ("Symbol_Response: "
            & Image (Value)
            & " Error");
         return;
      end if;

      Self.Trace.Trace
        ("Symbol_Response: "
         & Image (Value)
         & Ada.Containers.Count_Type'Image (Value.result.Length));
   end On_Symbol_Response;

   --------------------------------
   -- On_Type_Definition_Request --
   --------------------------------

   overriding procedure On_Type_Definition_Request
     (Self  : access Message_Logger;
      Value : LSP.Messages.Server_Requests.Type_Definition_Request)
   is
   begin
      Self.Trace.Trace
        ("Type_Definition_Request: "
         & Image (Value)
         & Image (Value.params));
   end On_Type_Definition_Request;

   -------------------------------------
   -- On_Workspace_Apply_Edit_Request --
   -------------------------------------

   overriding procedure On_Workspace_Apply_Edit_Request
     (Self   : access Message_Logger;
      Value  : LSP.Messages.Client_Requests.Workspace_Apply_Edit_Request)
   is
   begin
      Self.Trace.Trace
        ("Workspace_Apply_Edit_Request: "
         & Image (Value));
   end On_Workspace_Apply_Edit_Request;

   ------------------------------------------
   -- On_Workspace_Execute_Command_Request --
   ------------------------------------------

   overriding procedure On_Workspace_Execute_Command_Request
     (Self  : access Message_Logger;
      Value : LSP.Messages.Server_Requests.Workspace_Execute_Command_Request)
   is
   begin
      Self.Trace.Trace
        ("Workspace_Execute_Command_Request: "
         & Image (Value)
         & (+Value.params.command));
   end On_Workspace_Execute_Command_Request;

   ----------------------------------
   -- On_Workspace_Symbols_Request --
   ----------------------------------

   overriding procedure On_Workspace_Symbols_Request
     (Self  : access Message_Logger;
      Value : LSP.Messages.Server_Requests.Workspace_Symbols_Request)
   is
   begin
      Self.Trace.Trace
        ("Workspace_Symbols_Request: "
         & Image (Value)
         & (+Value.params.query));
   end On_Workspace_Symbols_Request;

end LSP.Message_Loggers;
