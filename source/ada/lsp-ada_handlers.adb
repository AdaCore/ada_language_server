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

with Ada.Strings.UTF_Encoding;
with Ada.Directories;

with GNATCOLL.JSON;

with LSP.Types; use LSP.Types;

with LSP.Ada_Documents;
with LSP.Lal_Utils;

with Langkit_Support.Slocs;

with Libadalang.Analysis;
with Libadalang.Common;

with GNATCOLL.VFS;
with GNATCOLL.Traces;

package body LSP.Ada_Handlers is

   function "+" (Text : Ada.Strings.UTF_Encoding.UTF_8_String)
                 return LSP.Types.LSP_String renames
     LSP.Types.To_LSP_String;

   -----------------------
   -- Exit_Notification --
   -----------------------

   overriding procedure Exit_Notification (Self : access Message_Handler) is
   begin
      Self.Server.Stop;
   end Exit_Notification;

   ------------------------
   -- Initialize_Request --
   ------------------------

   overriding function Initialize_Request
     (Self  : access Message_Handler;
      Value : LSP.Messages.InitializeParams)
      return LSP.Messages.Initialize_Response
   is
      Response : LSP.Messages.Initialize_Response (Is_Error => False);
      Root     : LSP.Types.LSP_String;
   begin
      Response.result.capabilities.definitionProvider := True;
      Response.result.capabilities.referencesProvider := True;
      Response.result.capabilities.documentSymbolProvider := True;
      Response.result.capabilities.textDocumentSync :=
        (Is_Set => True, Is_Number => True, Value => LSP.Messages.Full);
      Response.result.capabilities.completionProvider :=
        (True,
         (resolveProvider => (True, False),
          triggerCharacters => Empty_Vector & To_LSP_String (".")));

      if not LSP.Types.Is_Empty (Value.rootUri) then
         Root := Self.Context.URI_To_File (Value.rootUri);
      else
         --  URI isn't provided, rollback to deprecated rootPath
         Root := Value.rootPath;
      end if;

      Self.Context.Initialize (Root);

      return Response;
   end Initialize_Request;

   ----------------------
   -- Shutdown_Request --
   ----------------------

   overriding function Shutdown_Request
     (Self  : access Message_Handler)
      return LSP.Messages.ResponseMessage
   is
      pragma Unreferenced (Self);
   begin
      return Response : LSP.Messages.ResponseMessage (Is_Error => False);
   end Shutdown_Request;

   ---------------------------------------
   -- Text_Document_Code_Action_Request --
   ---------------------------------------

   overriding function Text_Document_Code_Action_Request
     (Self  : access Message_Handler;
      Value : LSP.Messages.CodeActionParams)
      return LSP.Messages.CodeAction_Response
   is
      pragma Unreferenced (Self, Value);
      Response : LSP.Messages.CodeAction_Response (Is_Error => True);
   begin
      Response.error :=
        (True,
         (code => LSP.Messages.InternalError,
          message => To_LSP_String ("Not implemented"),
          data => <>));
      return Response;
   end Text_Document_Code_Action_Request;

   --------------------------------------
   -- Text_Document_Definition_Request --
   --------------------------------------

   overriding function Text_Document_Definition_Request
     (Self  : access Message_Handler;
      Value : LSP.Messages.TextDocumentPositionParams)
      return LSP.Messages.Location_Response
   is

      Document   : constant LSP.Ada_Documents.Document_Access :=
        Self.Context.Get_Document (Value.textDocument.uri);

      use Libadalang.Analysis;

      Name_Node : constant Name := LSP.Lal_Utils.Get_Node_As_Name
        (Document.Get_Node_At (Value.position));

      Definition : Defining_Name;
      Response   : LSP.Messages.Location_Response (Is_Error => False);

   begin

      if Name_Node = No_Name then
         return Response;
      end if;

      Definition := LSP.Lal_Utils.Resolve_Name (Name_Node);

      if Definition = No_Defining_Name then
         return Response;
      end if;

      declare

         use Libadalang.Common;

         Start_Sloc_Range :
         constant Langkit_Support.Slocs.Source_Location_Range :=
           Sloc_Range (Data (Definition.Token_Start));
         End_Sloc_Range   :
         constant Langkit_Support.Slocs.Source_Location_Range :=
           Sloc_Range (Data (Definition.Token_End));

         First_Position : constant LSP.Messages.Position :=
           (Line_Number (Start_Sloc_Range.Start_Line) - 1,
            UTF_16_Index (Start_Sloc_Range.Start_Column) - 1);
         Last_Position  : constant LSP.Messages.Position :=
           (Line_Number (End_Sloc_Range.End_Line) - 1,
            UTF_16_Index (End_Sloc_Range.End_Column) - 1);

         Location : constant LSP.Messages.Location :=
           (uri  => Self.Context.File_To_URI (+Definition.Unit.Get_Filename),
            span => LSP.Messages.Span'(First_Position, Last_Position));

      begin
         Response.result.Append (Location);
         return Response;
      end;

   end Text_Document_Definition_Request;

   ------------------------------
   -- Text_Document_Did_Change --
   ------------------------------

   overriding procedure Text_Document_Did_Change
     (Self  : access Message_Handler;
      Value : LSP.Messages.DidChangeTextDocumentParams)
   is
      Document : constant LSP.Ada_Documents.Document_Access :=
        Self.Context.Get_Document (Value.textDocument.uri);
      Note     : LSP.Messages.PublishDiagnostics_Notification;
   begin
      Document.Apply_Changes (Value.contentChanges);
      Document.Get_Errors (Note.params.diagnostics);

      Note.method := +"textDocument/publishDiagnostics";
      Note.params.uri := Value.textDocument.uri;
      Self.Server.Send_Notification (Note);
   end Text_Document_Did_Change;

   -----------------------------
   -- Text_Document_Did_Close --
   -----------------------------

   overriding procedure Text_Document_Did_Close
     (Self  : access Message_Handler;
      Value : LSP.Messages.DidCloseTextDocumentParams)
   is
   begin
      Self.Context.Unload_Document (Value.textDocument);
   end Text_Document_Did_Close;

   ----------------------------
   -- Text_Document_Did_Open --
   ----------------------------

   overriding procedure Text_Document_Did_Open
     (Self  : access Message_Handler;
      Value : LSP.Messages.DidOpenTextDocumentParams)
   is
   begin

      GNATCOLL.Traces.Trace (Server_Trace, "In Text_Document_Did_Open");
      GNATCOLL.Traces.Trace
        (Server_Trace, "Uri : " & To_UTF_8_String (Value.textDocument.uri));

      --  Some clients don't properly call initialize, in which case we want to
      --  call it anyway at the first open file request.

      if not Self.Context.Is_Initialized then
         GNATCOLL.Traces.Trace
           (Server_Trace, "No project loaded, creating default one ...");

         declare
            Root : LSP.Types.LSP_String :=
              Self.Context.URI_To_File (Value.textDocument.uri);
         begin
            Root := To_LSP_String
              (Ada.Directories.Containing_Directory (To_UTF_8_String (Root)));

            GNATCOLL.Traces.Trace
              (Server_Trace, "Root : " & To_UTF_8_String (Root));

            Self.Context.Initialize (Root);

         end;
      end if;

      if not Self.Context.Has_Project then
         Self.Context.Load_Project (Empty_LSP_String, GNATCOLL.JSON.JSON_Null);
      end if;

      Self.Context.Load_Document (Value.textDocument);
   end Text_Document_Did_Open;

   -------------------------------------
   -- Text_Document_Highlight_Request --
   -------------------------------------

   overriding function Text_Document_Highlight_Request
     (Self  : access Message_Handler;
      Value : LSP.Messages.TextDocumentPositionParams)
      return LSP.Messages.Highlight_Response
   is
      pragma Unreferenced (Self, Value);
      Response : LSP.Messages.Highlight_Response (Is_Error => True);
   begin
      Response.error :=
        (True,
         (code => LSP.Messages.InternalError,
          message => To_LSP_String ("Not implemented"),
          data => <>));
      return Response;
   end Text_Document_Highlight_Request;

   ---------------------------------
   -- Text_Document_Hover_Request --
   ---------------------------------

   overriding function Text_Document_Hover_Request
     (Self  : access Message_Handler;
      Value : LSP.Messages.TextDocumentPositionParams)
      return LSP.Messages.Hover_Response
   is
      pragma Unreferenced (Self, Value);
      Response : LSP.Messages.Hover_Response (Is_Error => True);
   begin
      Response.error :=
        (True,
         (code => LSP.Messages.InternalError,
          message => To_LSP_String ("Not implemented"),
          data => <>));
      return Response;
   end Text_Document_Hover_Request;

   --------------------------------------
   -- Text_Document_References_Request --
   --------------------------------------

   overriding function Text_Document_References_Request
     (Self  : access Message_Handler;
      Value : LSP.Messages.ReferenceParams)
      return LSP.Messages.Location_Response
   is
      use Libadalang.Analysis;

      function Find_All_References
        (Definition         : Defining_Name;
         Sources            : GNATCOLL.VFS.File_Array_Access;
         Include_Definition : Boolean := False)
      return Ada_Node_Array;
      --  Helper function, finds all references of a given defining name in a
      --  given list of units.

      function Find_All_References
        (Definition         : Defining_Name;
         Sources            : GNATCOLL.VFS.File_Array_Access;
         Include_Definition : Boolean := False)
      return Ada_Node_Array
      is
         Context : constant Analysis_Context := Definition.Unit.Context;
         Source_Units : Analysis_Unit_Array (Sources'Range);
      begin
         for N in Sources'Range loop
            Source_Units (N) := Context.Get_From_File
              (Sources (N).Display_Full_Name);
         end loop;

         declare
            References : constant Ada_Node_Array :=
              Definition.P_Find_All_References (Source_Units);
         begin
            if Include_Definition then
               return References & (1 => Definition.As_Ada_Node);
            else
               return References;
            end if;
         end;
      end Find_All_References;

      Document   : constant LSP.Ada_Documents.Document_Access :=
        Self.Context.Get_Document (Value.textDocument.uri);

      Name_Node : constant Name := LSP.Lal_Utils.Get_Node_As_Name
        (Document.Get_Node_At (Value.position));

      Definition : Defining_Name;
      Response   : LSP.Messages.Location_Response (Is_Error => False);

   begin

      if Name_Node = No_Name then
         return Response;
      end if;

      Definition := LSP.Lal_Utils.Resolve_Name (Name_Node);

      if Definition = No_Defining_Name then
         return Response;
      end if;

      declare
         References : constant Ada_Node_Array := Find_All_References
             (Definition         => Definition,
              Sources            => Self.Context.Get_Source_Files,
              Include_Definition => Value.context.includeDeclaration);
      begin
         for Node of References loop
            declare
               use Libadalang.Common;

               Start_Sloc_Range :
               constant Langkit_Support.Slocs.Source_Location_Range :=
                 Sloc_Range (Data (Node.Token_Start));
               End_Sloc_Range   :
               constant Langkit_Support.Slocs.Source_Location_Range :=
                 Sloc_Range (Data (Node.Token_End));

               First_Position : constant LSP.Messages.Position :=
                 (Line_Number (Start_Sloc_Range.Start_Line) - 1,
                  UTF_16_Index (Start_Sloc_Range.Start_Column) - 1);
               Last_Position  : constant LSP.Messages.Position :=
                 (Line_Number (End_Sloc_Range.End_Line) - 1,
                  UTF_16_Index (End_Sloc_Range.End_Column) - 1);

               Location : constant LSP.Messages.Location :=
                 (uri  => Self.Context.File_To_URI (+Node.Unit.Get_Filename),
                  span => LSP.Messages.Span'(First_Position, Last_Position));
            begin
               Response.result.Append (Location);
            end;
         end loop;

         return Response;
      end;

   end Text_Document_References_Request;

   ------------------------------------------
   -- Text_Document_Signature_Help_Request --
   ------------------------------------------

   overriding function Text_Document_Signature_Help_Request
     (Self  : access Message_Handler;
      Value : LSP.Messages.TextDocumentPositionParams)
      return LSP.Messages.SignatureHelp_Response
   is
      pragma Unreferenced (Self, Value);
      Response : LSP.Messages.SignatureHelp_Response (Is_Error => True);
   begin
      Response.error :=
        (True,
         (code => LSP.Messages.InternalError,
          message => To_LSP_String ("Not implemented"),
          data => <>));
      return Response;
   end Text_Document_Signature_Help_Request;

   ----------------------------------
   -- Text_Document_Symbol_Request --
   ----------------------------------

   overriding function Text_Document_Symbol_Request
     (Self  : access Message_Handler;
      Value : LSP.Messages.DocumentSymbolParams)
      return LSP.Messages.Symbol_Response
   is
      Document : constant LSP.Ada_Documents.Document_Access :=
        Self.Context.Get_Document (Value.textDocument.uri);
      Response : LSP.Messages.Symbol_Response (Is_Error => False);
   begin
      Document.Get_Symbols (Response.result);
      return Response;
   end Text_Document_Symbol_Request;

   ----------------------------------------
   -- Workspace_Did_Change_Configuration --
   ----------------------------------------

   overriding procedure Workspace_Did_Change_Configuration
     (Self     : access Message_Handler;
      Value    : LSP.Messages.DidChangeConfigurationParams)
   is
      use type GNATCOLL.JSON.JSON_Value_Type;

      projectFile       : constant String := "projectFile";
      scenarioVariables : constant String := "scenarioVariables";

      Ada       : constant LSP.Types.LSP_Any := Value.settings.Get ("ada");
      File      : LSP.Types.LSP_String;
      Variables : LSP.Types.LSP_Any;
   begin
      if Ada.Kind = GNATCOLL.JSON.JSON_Object_Type then
         if Ada.Has_Field (projectFile) then
            File := +Ada.Get (projectFile).Get;

            --  Drop uri scheme if present
            if LSP.Types.Starts_With (File, "file:") then
               File := Self.Context.URI_To_File (File);
            end if;
         end if;

         if Ada.Has_Field (scenarioVariables) and then
           Ada.Get (scenarioVariables).Kind  = GNATCOLL.JSON.JSON_Object_Type
         then
            Variables := Ada.Get (scenarioVariables);
         end if;
      end if;

      Self.Context.Load_Project (File, Variables);
   end Workspace_Did_Change_Configuration;

   ---------------------------------------
   -- Workspace_Execute_Command_Request --
   ---------------------------------------

   overriding function Workspace_Execute_Command_Request
     (Self  : access Message_Handler;
      Value : LSP.Messages.ExecuteCommandParams)
      return LSP.Messages.ExecuteCommand_Response
   is
      pragma Unreferenced (Self, Value);
      Response : LSP.Messages.ExecuteCommand_Response (Is_Error => True);
   begin
      Response.error :=
        (True,
         (code => LSP.Messages.InternalError,
          message => To_LSP_String ("Not implemented"),
          data => <>));
      return Response;
   end Workspace_Execute_Command_Request;

   ------------------------------
   -- Workspace_Symbol_Request --
   ------------------------------

   overriding function Workspace_Symbol_Request
     (Self  : access Message_Handler;
      Value : LSP.Messages.WorkspaceSymbolParams)
      return LSP.Messages.Symbol_Response
   is
      pragma Unreferenced (Self, Value);
      Response : LSP.Messages.Symbol_Response (Is_Error => True);
   begin
      Response.error :=
        (True,
         (code => LSP.Messages.InternalError,
          message => To_LSP_String ("Not implemented"),
          data => <>));
      return Response;
   end Workspace_Symbol_Request;

   --------------------------------------
   -- Text_Document_Completion_Request --
   --------------------------------------

   overriding function Text_Document_Completion_Request
     (Self  : access Message_Handler;
      Value : LSP.Messages.TextDocumentPositionParams)
      return LSP.Messages.Completion_Response
   is
      Document : constant LSP.Ada_Documents.Document_Access :=
        Self.Context.Get_Document (Value.textDocument.uri);
      Response : LSP.Messages.Completion_Response (Is_Error => False);
   begin
      Document.Get_Completions_At (Value.position, Response.result);
      return Response;
   end Text_Document_Completion_Request;

end LSP.Ada_Handlers;
