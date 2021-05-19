------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2018-2021, AdaCore                     --
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
with LSP.Messages; use LSP.Messages;

with Ada.Strings.UTF_Encoding.Wide_Strings;
with GNATCOLL.JSON;

package body LSP.Message_Loggers is

   function "+" (Text : LSP.Types.LSP_String) return String
     renames LSP.Types.To_UTF_8_String;
   function "+" (Text : LSP.Types.LSP_URI) return String
     renames LSP.Types.To_UTF_8_String1;

   function Image (Value : LSP.Types.LSP_Number_Or_String) return String;
   function Image (Value : LSP.Types.Optional_Boolean) return String;

   function Image (Value : LSP.Messages.RequestMessage'Class) return String;
   function Image (Value : LSP.Messages.ResponseMessage'Class) return String;
   function Image (Value : LSP.Messages.Position) return String;
   function Image (Value : LSP.Messages.Span) return String;
   function Image (Value : LSP.Messages.TextDocumentIdentifier) return String;
   function Image (Value : LSP.Messages.FormattingOptions) return String;
   function Image (Value : LSP.Messages.TextEdit_Vector) return String;

   function Image
     (Value : LSP.Messages.DocumentRangeFormattingParams) return String;
   function Image
     (Value : LSP.Messages.DocumentFormattingParams) return String;
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

   function Image (Value : LSP.Types.LSP_Number_Or_String) return String is
   begin
      if Value.Is_Number then
         return LSP.Types.LSP_Number'Image (Value.Number);
      else
         return +Value.String;
      end if;
   end Image;

   -----------
   -- Image --
   -----------

   function Image (Value : LSP.Messages.RequestMessage'Class) return String is
      Prefix : constant String := "Request ";
   begin
      return Prefix & Image (Value.id) & ' ';
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

   -----------
   -- Image --
   -----------

   function Image
     (Value : LSP.Messages.TextDocumentIdentifier) return String is
   begin
      return (+Value.uri);
   end Image;

   -----------
   -- Image --
   -----------

   function Image (Value : LSP.Types.Optional_Boolean) return String is
   begin
      if Value.Is_Set then
         return "(Value =>" & Boolean'Image (Value.Value) & ")";
      else
         return "(Is_Set => False)";
      end if;
   end Image;

   -----------
   -- Image --
   -----------

   function Image (Value : LSP.Messages.FormattingOptions) return String is
   begin
      return '(' & LSP.Types.LSP_Number'Image (Value.tabSize) & ","
        & Boolean'Image (Value.insertSpaces) & ","
        & Image (Value.trimTrailingWhitespace) & ","
        & Image (Value.insertFinalNewline) & ","
        & Image (Value.trimFinalNewlines) & ')';
   end Image;

   -----------
   -- Image --
   -----------

   function Image
     (Value : LSP.Messages.DocumentFormattingParams) return String is
   begin
      return Image (Value.textDocument) & Image (Value.options);
   end Image;

   -----------
   -- Image --
   -----------

   function Image
     (Value : LSP.Messages.DocumentRangeFormattingParams) return String is
   begin
      return Image (Value.textDocument) & Image (Value.span)
        & Image (Value.options);
   end Image;

   -----------
   -- Image --
   -----------

   function Image (Value : LSP.Messages.TextEdit_Vector) return String is
   begin
      return Ada.Containers.Count_Type'Image (Value.Length);
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

   --------------------------
   -- On_ALS_Calls_Request --
   --------------------------

   overriding procedure On_ALS_Calls_Request
     (Self  : access Message_Logger;
      Value : LSP.Messages.Server_Requests.ALS_Calls_Request)
   is
   begin
      Self.Trace.Trace
        ("ALS_Calls_Request: "
         & Image (Value)
         & Image (Value.params));
   end On_ALS_Calls_Request;

   --------------------------------------
   -- On_ALS_Show_Dependencies_Request --
   --------------------------------------

   overriding procedure On_ALS_Show_Dependencies_Request
     (Self   : access Message_Logger;
      Value  : LSP.Messages.Server_Requests.ALS_Show_Dependencies_Request) is
   begin
      Self.Trace.Trace
        ("ALS_Show_Dependencies_Request: "
         & Image (Value)
         & Image (Value.params.textDocument));
   end On_ALS_Show_Dependencies_Request;

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

   ---------------------------
   -- On_ALS_Calls_Response --
   ---------------------------

   overriding procedure On_ALS_Calls_Response
     (Self   : in out Message_Logger;
      Value  : LSP.Messages.Server_Responses.ALS_Calls_Response) is
   begin
      if Value.Is_Error then
         Self.Trace.Trace
           ("ALS_Calls_Response: "
            & Image (Value)
            & " Error");
         return;
      end if;

      Self.Trace.Trace
        ("ALS_Calls_Response: "
         & Image (Value)
         & Ada.Containers.Count_Type'Image (Value.result.Length));
   end On_ALS_Calls_Response;

   --------------------------------------
   -- On_ALS_ShowDependencies_Response --
   --------------------------------------

   overriding procedure On_ALS_ShowDependencies_Response
     (Self   : in out Message_Logger;
      Value  : LSP.Messages.Server_Responses.ALS_ShowDependencies_Response) is
   begin
      if Value.Is_Error then
         Self.Trace.Trace
           ("ALS_ShowDependencies_Response: "
            & Image (Value)
            & " Error");
         return;
      end if;

      Self.Trace.Trace
        ("ALS_ShowDependencies_Response: "
         & Image (Value)
         & Ada.Containers.Count_Type'Image (Value.result.Length));
   end On_ALS_ShowDependencies_Response;

   --------------------------
   -- On_ALS_Debug_Request --
   --------------------------

   overriding procedure On_ALS_Debug_Request
     (Self  : access Message_Logger;
      Value : LSP.Messages.Server_Requests.ALS_Debug_Request) is
   begin
      Self.Trace.Trace ("On_ALS_Debug_Request: " & Image (Value));
   end On_ALS_Debug_Request;

   ---------------------------
   -- On_ALS_Debug_Response --
   ---------------------------

   overriding procedure On_ALS_Debug_Response
     (Self   : in out Message_Logger;
      Value  : LSP.Messages.Server_Responses.ALS_Debug_Response) is
   begin
      if Value.Is_Error then
         Self.Trace.Trace
           ("ALS_Debug_Response: "
            & Image (Value)
            & " Error");
         return;
      end if;

      Self.Trace.Trace ("ALS_Debug_Response: " & Image (Value));
   end On_ALS_Debug_Response;

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
      Value : LSP.Messages.CancelParams) is
   begin
      Self.Trace.Trace
        ("On_Cancel_Notification: " & LSP.Types.To_UTF_8_String (Value.id));
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

   ----------------------------------------
   -- On_Workspace_Configuration_Request --
   ----------------------------------------

   overriding procedure On_Workspace_Configuration_Request
     (Self  : access Message_Logger;
      Value : LSP.Messages.Client_Requests.Workspace_Configuration_Request)
   is
   begin
      Self.Trace.Trace
        ("Workspace_Configuration_Request: "
         & Image (Value)
         & Ada.Containers.Count_Type'Image (Value.params.items.Length));
   end On_Workspace_Configuration_Request;

   -------------------------------
   -- On_Configuration_Response --
   -------------------------------

   overriding procedure On_Configuration_Response
     (Self  : in out Message_Logger;
      Value : LSP.Messages.Client_Responses.Configuration_Response) is
   begin
      if Value.Is_Error then
         Self.Trace.Trace
           ("Configuration_Response: "
            & Image (Value)
            & " Error");
         return;
      end if;

      Self.Trace.Trace
        ("Configuration_Response: "
         & Image (Value)
         & Ada.Containers.Count_Type'Image (Value.result.Length));
   end On_Configuration_Response;

   ----------------------------
   -- On_Declaration_Request --
   ----------------------------

   overriding procedure On_Declaration_Request
     (Self  : access Message_Logger;
      Value : LSP.Messages.Server_Requests.Declaration_Request) is
   begin
      Self.Trace.Trace
        ("Declaration_Request: "
         & Image (Value)
         & Image (Value.params));
   end On_Declaration_Request;

   -------------------------------
   -- On_Implementation_Request --
   -------------------------------

   overriding procedure On_Implementation_Request
     (Self  : access Message_Logger;
      Value : LSP.Messages.Server_Requests.Implementation_Request) is
   begin
      Self.Trace.Trace
        ("Implementation_Request: "
         & Image (Value)
         & Image (Value.params));
   end On_Implementation_Request;

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

   -----------------------------------------------
   -- On_DidChangeWorkspaceFolders_Notification --
   -----------------------------------------------

   overriding procedure On_DidChangeWorkspaceFolders_Notification
     (Self  : access Message_Logger;
      Value : LSP.Messages.DidChangeWorkspaceFoldersParams) is
   begin
      Self.Trace.Trace
        ("DidChangeWorkspaceFolders_Notification: added"
         & (Value.event.added.Length'Img)
         & " removed"
         & (Value.event.removed.Length'Img));
   end On_DidChangeWorkspaceFolders_Notification;

   -------------------------------------------
   -- On_DidChangeWatchedFiles_Notification --
   -------------------------------------------

   overriding procedure On_DidChangeWatchedFiles_Notification
     (Self  : access Message_Logger;
      Value : LSP.Messages.DidChangeWatchedFilesParams)
   is
      use LSP.Types;
      Result : LSP.Types.LSP_String;
   begin
      for Change of Value.changes loop
         Append (Result, " " & LSP.Types.To_Wide_String (Change.uri) & ": " &
                   Change.a_type'Wide_Image & ";");
      end loop;

      Self.Trace.Trace ("DidChangeWatchedFiles_Notification:"
                        & ASCII.LF & To_UTF_8_String (Result));
   end On_DidChangeWatchedFiles_Notification;

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
        ("DidCloseTextDocument_Notification: "
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

   -----------------------------------
   -- On_Color_Presentation_Request --
   -----------------------------------

   overriding procedure On_Color_Presentation_Request
     (Self  : access Message_Logger;
      Value : LSP.Messages.Server_Requests.Color_Presentation_Request) is
   begin
      Self.Trace.Trace
        ("Color_Presentation_Request: "
         & Image (Value)
         & (+Value.params.textDocument.uri));
   end On_Color_Presentation_Request;

   -------------------------------
   -- On_Document_Color_Request --
   -------------------------------

   overriding procedure On_Document_Color_Request
     (Self  : access Message_Logger;
      Value : LSP.Messages.Server_Requests.Document_Color_Request) is
   begin
      Self.Trace.Trace
        ("Document_Color_Request: "
         & Image (Value)
         & (+Value.params.textDocument.uri));
   end On_Document_Color_Request;

   -------------------------------
   -- On_Document_Links_Request --
   -------------------------------

   overriding procedure On_Document_Links_Request
     (Self  : access Message_Logger;
      Value : LSP.Messages.Server_Requests.Document_Links_Request) is
   begin
      Self.Trace.Trace
        ("Document_Links_Request: "
         & Image (Value)
         & (+Value.params.textDocument.uri));
   end On_Document_Links_Request;

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

   ------------------------------
   -- On_Folding_Range_Request --
   ------------------------------

   overriding procedure On_Folding_Range_Request
     (Self  : access Message_Logger;
      Value : LSP.Messages.Server_Requests.Folding_Range_Request) is
   begin
      Self.Trace.Trace
        ("Folding_Range_Request: "
         & Image (Value)
         & (+Value.params.textDocument.uri));
   end On_Folding_Range_Request;

   ------------------------------
   -- On_FoldingRange_Response --
   ------------------------------

   overriding procedure On_FoldingRange_Response
     (Self  : in out Message_Logger;
      Value : LSP.Messages.Server_Responses.FoldingRange_Response) is
   begin
      if Value.Is_Error then
         Self.Trace.Trace
           ("FoldingRange_Response: "
            & Image (Value)
            & " Error");
         return;
      end if;

      Self.Trace.Trace
        ("FoldingRange_Response: "
         & Image (Value)
         & Ada.Containers.Count_Type'Image (Value.result.Length));
   end On_FoldingRange_Response;

   ---------------------------------------
   -- On_Prepare_Call_Hierarchy_Request --
   ---------------------------------------

   overriding procedure On_Prepare_Call_Hierarchy_Request
     (Self  : access Message_Logger;
      Value : LSP.Messages.Server_Requests.Prepare_Call_Hierarchy_Request) is
   begin
      Self.Trace.Trace
        ("Prepare_Call_Hierarchy_Request: "
         & Image (Value)
         & (+Value.params.textDocument.uri));
   end On_Prepare_Call_Hierarchy_Request;

   ----------------------------------------
   -- On_Prepare_Call_Hierarchy_Response --
   ----------------------------------------

   overriding procedure On_Prepare_Call_Hierarchy_Response
     (Self  : in out Message_Logger;
      Value : LSP.Messages.Server_Responses.PrepareCallHierarchy_Response) is
   begin
      if Value.Is_Error then
         Self.Trace.Trace
           ("PrepareCallHierarchy_Response: "
            & Image (Value)
            & " Error");
         return;
      end if;

      Self.Trace.Trace
        ("PrepareCallHierarchy_Response: "
         & Image (Value)
         & Ada.Containers.Count_Type'Image (Value.result.Length));
   end On_Prepare_Call_Hierarchy_Response;

   -------------------------------
   -- On_Incoming_Calls_Request --
   -------------------------------

   overriding procedure On_Incoming_Calls_Request
     (Self  : access Message_Logger;
      Value : LSP.Messages.Server_Requests.Incoming_Calls_Request) is
   begin
      Self.Trace.Trace
        ("Incoming_Calls_Request: "
         & Image (Value)
         & (+Value.params.item.uri)
         & " : "
         & (+Value.params.item.name));
   end On_Incoming_Calls_Request;

   --------------------------------
   -- On_Incoming_Calls_Response --
   --------------------------------

   overriding procedure On_Incoming_Calls_Response
     (Self  : in out Message_Logger;
      Value : LSP.Messages.Server_Responses.IncomingCalls_Response) is
   begin
      if Value.Is_Error then
         Self.Trace.Trace
           ("IncomingCalls_Response: "
            & Image (Value)
            & " Error");
         return;
      end if;

      Self.Trace.Trace
        ("IncomingCalls_Response: "
         & Image (Value)
         & Ada.Containers.Count_Type'Image (Value.result.Length));
   end On_Incoming_Calls_Response;

   -------------------------------
   -- On_Outgoing_Calls_Request --
   -------------------------------

   overriding procedure On_Outgoing_Calls_Request
     (Self  : access Message_Logger;
      Value : LSP.Messages.Server_Requests.Outgoing_Calls_Request) is
   begin
      Self.Trace.Trace
        ("Outgoing_Calls_Request: "
         & Image (Value)
         & (+Value.params.item.uri)
         & " : "
         & (+Value.params.item.name));
   end On_Outgoing_Calls_Request;

   --------------------------------
   -- On_Outgoing_Calls_Response --
   --------------------------------

   overriding procedure On_Outgoing_Calls_Response
     (Self  : in out Message_Logger;
      Value : LSP.Messages.Server_Responses.OutgoingCalls_Response) is
   begin
      if Value.Is_Error then
         Self.Trace.Trace
           ("OutgoingCalls_Response: "
            & Image (Value)
            & " Error");
         return;
      end if;

      Self.Trace.Trace
        ("OutgoingCalls_Response: "
         & Image (Value)
         & Ada.Containers.Count_Type'Image (Value.result.Length));
   end On_Outgoing_Calls_Response;

   --------------------------------
   -- On_Selection_Range_Request --
   --------------------------------

   overriding procedure On_Selection_Range_Request
     (Self  : access Message_Logger;
      Value : LSP.Messages.Server_Requests.Selection_Range_Request) is
   begin
      Self.Trace.Trace
        ("Selection_Range_Request: "
         & Image (Value)
         & (+Value.params.textDocument.uri));
   end On_Selection_Range_Request;

   --------------------------------
   -- On_SelectionRange_Response --
   --------------------------------

   overriding procedure On_SelectionRange_Response
     (Self  : in out Message_Logger;
      Value : LSP.Messages.Server_Responses.SelectionRange_Response) is
   begin
      if Value.Is_Error then
         Self.Trace.Trace
           ("SelectionRange_Response: "
            & Image (Value)
            & " Error");
         return;
      end if;

      Self.Trace.Trace
        ("SelectionRange_Response: "
         & Image (Value)
         & Ada.Containers.Count_Type'Image (Value.result.Length));
   end On_SelectionRange_Response;

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

      if not Value.result.Is_Set then
         Self.Trace.Trace ("Hover_Response: null");
      elsif Value.result.Value.contents.Is_MarkupContent then
         Self.Trace.Trace ("Hover_Response: " & Image (Value));
      else
         Self.Trace.Trace
           ("Hover_Response: "
            & Image (Value)
            & Ada.Containers.Count_Type'Image
              (Value.result.Value.contents.Vector.Length));
      end if;
   end On_Hover_Response;

   ---------------------------
   -- On_Initialize_Request --
   ---------------------------

   overriding procedure On_Initialize_Request
     (Self  : access Message_Logger;
      Value : LSP.Messages.Server_Requests.Initialize_Request)
   is
   begin
      if Value.params.rootPath.Is_Set
        and then Value.params.rootPath.Value.Is_Set
      then
         Self.Trace.Trace
           ("Initialize_Request: "
            & Image (Value)
            & (+Value.params.rootPath.Value.Value));
      elsif Value.params.rootPath.Is_Set then
         Self.Trace.Trace
           ("Initialize_Request: "
            & Image (Value)
            & " null");
      else
         Self.Trace.Trace ("Initialize_Request: " & Image (Value));
      end if;
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

   -------------------------------
   -- On_Location_Link_Response --
   -------------------------------

   overriding procedure On_Location_Link_Response
     (Self   : in out Message_Logger;
      Value  : LSP.Messages.Server_Responses.Location_Link_Response) is
   begin
      if Value.Is_Error then
         Self.Trace.Trace
           ("Location_Response: "
            & Image (Value)
            & " Error");
         return;
      end if;

      case Value.result.Kind is
         when Empty_Vector_Kind =>
            Self.Trace.Trace
              ("Location_Response: "
               & Image (Value)
               & "empty");
         when Location_Vector_Kind =>
            Self.Trace.Trace
              ("Location_Response: "
               & Image (Value)
               & " Locations"
               & Ada.Containers.Count_Type'Image
                   (Value.result.Locations.Length));
         when LocationLink_Vector_Kind =>
            Self.Trace.Trace
              ("Location_Response: "
               & Image (Value)
               & " LocationLinks"
               & Ada.Containers.Count_Type'Image
                 (Value.result.LocationLinks.Length));
      end case;
   end On_Location_Link_Response;

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

   -----------------------
   -- On_Progress_Begin --
   -----------------------

   overriding procedure On_Progress
     (Self   : access Message_Logger;
      Params : LSP.Messages.Progress_Params) is
   begin
      case Params.Kind is
         when Progress_Begin =>
            Self.Trace.Trace ("Progress_Begin: "
                              & (LSP.Types.To_UTF_8_String
                                (Params.Begin_Param.token)));
         when Progress_Report =>
            Self.Trace.Trace ("Progress_Report: "
                              & (LSP.Types.To_UTF_8_String
                                (Params.Report_Param.token)));
         when Progress_End =>
            Self.Trace.Trace ("Progress_End: "
                              & (LSP.Types.To_UTF_8_String
                                (Params.End_Param.token)));
      end case;
   end On_Progress;

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

   -------------------------------
   -- On_Prepare_Rename_Request --
   -------------------------------

   overriding procedure On_Prepare_Rename_Request
     (Self  : access Message_Logger;
      Value : LSP.Messages.Server_Requests.Prepare_Rename_Request) is
   begin
      Self.Trace.Trace
        ("Prepare_Rename_Request: "
         & Image (Value)
         & Image (Value.params));
   end On_Prepare_Rename_Request;

   --------------------------------
   -- On_Prepare_Rename_Response --
   --------------------------------

   overriding procedure On_Prepare_Rename_Response
     (Self  : in out Message_Logger;
      Value : LSP.Messages.Server_Responses.Prepare_Rename_Response) is
   begin
      if Value.Is_Error then
         Self.Trace.Trace
           ("Prepare_Rename_Response: "
            & Image (Value)
            & " Error");
      elsif Value.result.Is_Set then
         Self.Trace.Trace
           ("Rename_Rename_Response: "
            & Image (Value)
            & Image (Value.result.Value));
      else
         Self.Trace.Trace
           ("Rename_Rename_Response: "
            & Image (Value)
            & " null");
      end if;
   end On_Prepare_Rename_Response;

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

   -----------------------------------
   -- On_RegisterCapability_Request --
   -----------------------------------

   overriding procedure On_RegisterCapability_Request
     (Self  : access Message_Logger;
      Value : LSP.Messages.Client_Requests.RegisterCapability_Request) is
   begin
      Self.Trace.Trace
        ("RegisterCapability_Request: "
         & Image (Value)
         & Ada.Containers.Count_Type'Image
            (Value.params.registrations.Length));
   end On_RegisterCapability_Request;

   ------------------------------------
   -- On_RegisterCapability_Response --
   ------------------------------------

   overriding procedure On_RegisterCapability_Response
     (Self  : in out Message_Logger;
      Value : LSP.Messages.Client_Responses.RegisterCapability_Response) is
   begin
      if Value.Is_Error then
         Self.Trace.Trace
           ("RegisterCapability_Response: "
            & Image (Value)
            & " Error");
         return;
      end if;

      Self.Trace.Trace
        ("RegisterCapability_Response: "
         & Image (Value));
   end On_RegisterCapability_Response;

   -------------------------------------
   -- On_UnregisterCapability_Request --
   -------------------------------------

   overriding procedure On_UnregisterCapability_Request
     (Self  : access Message_Logger;
      Value : LSP.Messages.Client_Requests.UnregisterCapability_Request) is
   begin
      Self.Trace.Trace
        ("UnregisterCapability_Request: "
         & Image (Value)
         & Ada.Containers.Count_Type'Image
            (Value.params.unregisterations.Length));
   end On_UnregisterCapability_Request;

   --------------------------------------
   -- On_UnregisterCapability_Response --
   --------------------------------------

   overriding procedure On_UnregisterCapability_Response
     (Self  : in out Message_Logger;
      Value : LSP.Messages.Client_Responses.UnregisterCapability_Response) is
   begin
      if Value.Is_Error then
         Self.Trace.Trace
           ("UnregisterCapability_Response: "
            & Image (Value)
            & " Error");
         return;
      end if;

      Self.Trace.Trace
        ("UnregisterCapability_Response: "
         & Image (Value));
   end On_UnregisterCapability_Response;

   ----------------------------------
   -- On_Workspace_Folders_Request --
   ----------------------------------

   overriding procedure On_Workspace_Folders_Request
     (Self  : access Message_Logger;
      Value : LSP.Messages.Client_Requests.Workspace_Folders_Request) is
   begin
      Self.Trace.Trace
        ("Workspace_Folders_Request: "
         & Image (Value));
   end On_Workspace_Folders_Request;

   overriding procedure On_WorkspaceFolders_Response
     (Self  : in out Message_Logger;
      Value : LSP.Messages.Client_Responses.WorkspaceFolders_Response) is
   begin
      if Value.Is_Error then
         Self.Trace.Trace
           ("WorkspaceFolders_Response: "
            & Image (Value)
            & " Error");
         return;
      end if;

      Self.Trace.Trace
        ("WorkspaceFolders_Response: "
         & Image (Value));
   end On_WorkspaceFolders_Response;

   -----------------------------
   -- On_ShowDocument_Request --
   -----------------------------

   overriding procedure On_ShowDocument_Request
     (Self   : access Message_Logger;
      Value  : LSP.Messages.Client_Requests.ShowDocument_Request) is
   begin
      Self.Trace.Trace
        ("ShowDocument_Request: "
         & Image (Value)
         & (+Value.params.uri));
   end On_ShowDocument_Request;

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
        ("Shutdown_Request: "
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

   -----------------------
   -- On_Links_Response --
   -----------------------

   overriding procedure On_Links_Response
     (Self  : in out Message_Logger;
      Value : LSP.Messages.Server_Responses.Links_Response) is
   begin
      if Value.Is_Error then
         Self.Trace.Trace
           ("Links_Response: "
            & Image (Value)
            & " Error");
         return;
      end if;

      Self.Trace.Trace
        ("Links_Response: "
         & Image (Value)
         & Ada.Containers.Count_Type'Image (Value.result.Length));
   end On_Links_Response;

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
         & Image (Value) & " Is_Tree="
         & Boolean'Image (Value.result.Is_Tree) & " "
         & (if not Value.result.Is_Tree then
              Ada.Containers.Count_Type'Image
                 (Value.result.Vector.Length)
           else
              Ada.Containers.Count_Type'Image
                 (Value.result.Tree.Node_Count)));
   end On_Symbol_Response;

   -----------------------------------
   -- On_ColorPresentation_Response --
   -----------------------------------

   overriding procedure On_ColorPresentation_Response
     (Self  : in out Message_Logger;
      Value : LSP.Messages.Server_Responses.ColorPresentation_Response) is
   begin
      if Value.Is_Error then
         Self.Trace.Trace
           ("ColorPresentation: "
            & Image (Value)
            & " Error");
         return;
      end if;

      Self.Trace.Trace
        ("ColorPresentation: "
         & Image (Value)
         & Ada.Containers.Count_Type'Image (Value.result.Length));
   end On_ColorPresentation_Response;

   -------------------------------
   -- On_DocumentColor_Response --
   -------------------------------

   overriding procedure On_DocumentColor_Response
     (Self  : in out Message_Logger;
      Value : LSP.Messages.Server_Responses.DocumentColor_Response) is
   begin
      if Value.Is_Error then
         Self.Trace.Trace
           ("DocumentColor: "
            & Image (Value)
            & " Error");
         return;
      end if;

      Self.Trace.Trace
        ("DocumentColor: "
         & Image (Value)
         & Ada.Containers.Count_Type'Image (Value.result.Length));
   end On_DocumentColor_Response;

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

   ----------------------------------------
   -- On_WorkDoneProgress_Create_Request --
   ----------------------------------------

   overriding procedure On_WorkDoneProgress_Create_Request
     (Self  : access Message_Logger;
      Value : LSP.Messages.Client_Requests.WorkDoneProgressCreate_Request)
   is
   begin
      Self.Trace.Trace
        ("WorkDoneProgress_Create_Request: "
         & Image (Value)
         & Image (Value.params.token));
   end On_WorkDoneProgress_Create_Request;

   ----------------------------------------
   -- On_WorkDoneProgressCreate_Response --
   ----------------------------------------

   overriding procedure On_WorkDoneProgressCreate_Response
     (Self  : in out Message_Logger;
      Value : LSP.Messages.Client_Responses.WorkDoneProgressCreate_Response) is
   begin
      if Value.Is_Error then
         Self.Trace.Trace
           ("WorkDoneProgressCreate_Response: "
            & Image (Value)
            & " Error");
         return;
      end if;

      Self.Trace.Trace
        ("WorkDoneProgressCreate_Response: "
         & Image (Value));
   end On_WorkDoneProgressCreate_Response;

   ---------------------------
   -- On_Formatting_Request --
   ---------------------------

   overriding procedure On_Formatting_Request
     (Self  : access Message_Logger;
      Value : LSP.Messages.Server_Requests.Formatting_Request) is
   begin
      Self.Trace.Trace
        ("Formatting_Request: "
         & Image (Value)
         & Image (Value.params));
   end On_Formatting_Request;

   ---------------------------------
   -- On_Range_Formatting_Request --
   ---------------------------------

   overriding procedure On_Range_Formatting_Request
     (Self  : access Message_Logger;
      Value : LSP.Messages.Server_Requests.Range_Formatting_Request)
   is
   begin
      Self.Trace.Trace
        ("Range_Formatting_Request: "
         & Image (Value)
         & Image (Value.params));
   end On_Range_Formatting_Request;

   ----------------------------
   -- On_Formatting_Response --
   ----------------------------

   overriding procedure On_Formatting_Response
     (Self     : in out Message_Logger;
      Response : LSP.Messages.Server_Responses.Formatting_Response) is
   begin
      if Response.Is_Error then
         Self.Trace.Trace
           ("Formatting_Response: " & Image (Response) & " Error");
         return;
      end if;

      Self.Trace.Trace
        ("Formatting_Response: "
         & Image (Response)
         & Image (Response.result));
   end On_Formatting_Response;

   ----------------------------------
   -- On_Range_Formatting_Response --
   ----------------------------------

   overriding procedure On_Range_Formatting_Response
     (Self     : in out Message_Logger;
      Response : LSP.Messages.Server_Responses.Range_Formatting_Response) is
   begin
      if Response.Is_Error then
         Self.Trace.Trace
           ("Range_Formatting_Response: " & Image (Response) & " Error");
         return;
      end if;

      Self.Trace.Trace
        ("Range_Formatting_Response: "
         & Image (Response) & Image (Response.result));
   end On_Range_Formatting_Response;

end LSP.Message_Loggers;
