------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                       Copyright (C) 2022, AdaCore                        --
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

with LSP.Errors;
with LSP.Types;

package body LSP.GPR_Handlers is

   ---------------------------------
   -- On_ALS_Check_Syntax_Request --
   ---------------------------------

   overriding function On_ALS_Check_Syntax_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.ALS_Check_Syntax_Request)
      return LSP.Messages.Server_Responses.ALS_Check_Syntax_Response is
   begin
      return raise Program_Error with "Unimplemented request";
   end On_ALS_Check_Syntax_Request;

   --------------------------
   -- On_ALS_Debug_Request --
   --------------------------

   overriding function On_ALS_Debug_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.ALS_Debug_Request)
      return LSP.Messages.Server_Responses.ALS_Debug_Response is
   begin
      return raise Program_Error with "Unimplemented request";
   end On_ALS_Debug_Request;

   --------------------------------------
   -- On_ALS_Show_Dependencies_Request --
   --------------------------------------

   overriding function On_ALS_Show_Dependencies_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.ALS_Show_Dependencies_Request)
      return LSP.Messages.Server_Responses.ALS_ShowDependencies_Response is
   begin
      return raise Program_Error with "Unimplemented request";
   end On_ALS_Show_Dependencies_Request;

   --------------------------------
   -- On_ALS_Source_Dirs_Request --
   --------------------------------

   overriding function On_ALS_Source_Dirs_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.ALS_Source_Dirs_Request)
      return LSP.Messages.Server_Responses.ALS_SourceDirs_Response is
   begin
      return raise Program_Error with "Unimplemented request";
   end On_ALS_Source_Dirs_Request;

   ---------------------------
   -- On_CodeAction_Request --
   ---------------------------

   overriding function On_CodeAction_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.CodeAction_Request)
      return LSP.Messages.Server_Responses.CodeAction_Response is
   begin
      return raise Program_Error with "Unimplemented request";
   end On_CodeAction_Request;

   -----------------------------------
   -- On_Color_Presentation_Request --
   -----------------------------------

   overriding function On_Color_Presentation_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Color_Presentation_Request)
      return LSP.Messages.Server_Responses.ColorPresentation_Response is
   begin
      return raise Program_Error with "Unimplemented request";
   end On_Color_Presentation_Request;

   ---------------------------
   -- On_Completion_Request --
   ---------------------------

   overriding function On_Completion_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Completion_Request)
      return LSP.Messages.Server_Responses.Completion_Response is
   begin
      return raise Program_Error with "Unimplemented request";
   end On_Completion_Request;

   --------------------------------------
   -- On_CompletionItemResolve_Request --
   --------------------------------------

   overriding function On_CompletionItemResolve_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.CompletionItemResolve_Request)
      return LSP.Messages.Server_Responses.CompletionItemResolve_Response is
   begin
      return raise Program_Error with "Unimplemented request";
   end On_CompletionItemResolve_Request;

   ----------------------------
   -- On_Declaration_Request --
   ----------------------------

   overriding function On_Declaration_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Declaration_Request)
      return LSP.Messages.Server_Responses.Location_Link_Response is
   begin
      return raise Program_Error with "Unimplemented request";
   end On_Declaration_Request;

   ---------------------------
   -- On_Definition_Request --
   ---------------------------

   overriding function On_Definition_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Definition_Request)
      return LSP.Messages.Server_Responses.Location_Link_Response is
   begin
      return raise Program_Error with "Unimplemented request";
   end On_Definition_Request;

   -------------------------------
   -- On_Document_Color_Request --
   -------------------------------

   overriding function On_Document_Color_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Document_Color_Request)
      return LSP.Messages.Server_Responses.DocumentColor_Response is
   begin
      return raise Program_Error with "Unimplemented request";
   end On_Document_Color_Request;

   -------------------------------
   -- On_Document_Links_Request --
   -------------------------------

   overriding function On_Document_Links_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Document_Links_Request)
      return LSP.Messages.Server_Responses.Links_Response is
   begin
      return raise Program_Error with "Unimplemented request";
   end On_Document_Links_Request;

   -------------------------------------
   -- On_Document_Tokens_Full_Request --
   -------------------------------------

   overriding function On_Document_Tokens_Full_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Document_Tokens_Full_Request)
      return LSP.Messages.Server_Responses.SemanticTokens_Response is
   begin
      return raise Program_Error with "Unimplemented request";
   end On_Document_Tokens_Full_Request;

   --------------------------------------
   -- On_Document_Tokens_Range_Request --
   --------------------------------------

   overriding function On_Document_Tokens_Range_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Document_Tokens_Range_Request)
      return LSP.Messages.Server_Responses.SemanticTokens_Response
   is
      pragma Unreferenced (Self, Request);

      Response : LSP.Messages.Server_Responses.SemanticTokens_Response
        (Is_Error => True);
   begin
      Response.error :=
        (True,
         (code => LSP.Errors.InternalError,
          message => "Not implemented",
          data => <>));
      return Response;
   end On_Document_Tokens_Range_Request;

   ---------------------------------
   -- On_Document_Symbols_Request --
   ---------------------------------

   overriding function On_Document_Symbols_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Document_Symbols_Request)
      return LSP.Messages.Server_Responses.Symbol_Response is
   begin
      return raise Program_Error with "Unimplemented request";
   end On_Document_Symbols_Request;

   --------------------------------
   -- On_Execute_Command_Request --
   --------------------------------

   overriding function On_Execute_Command_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Execute_Command_Request)
      return LSP.Messages.Server_Responses.ExecuteCommand_Response is
   begin
      return raise Program_Error with "Unimplemented request";
   end On_Execute_Command_Request;

   ------------------------------
   -- On_Folding_Range_Request --
   ------------------------------

   overriding function On_Folding_Range_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Folding_Range_Request)
      return LSP.Messages.Server_Responses.FoldingRange_Response is
   begin
      return raise Program_Error with "Unimplemented request";
   end On_Folding_Range_Request;

   ---------------------------
   -- On_Formatting_Request --
   ---------------------------

   overriding function On_Formatting_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Formatting_Request)
      return LSP.Messages.Server_Responses.Formatting_Response is
   begin
      return raise Program_Error with "Unimplemented request";
   end On_Formatting_Request;

   --------------------------
   -- On_Highlight_Request --
   --------------------------

   overriding function On_Highlight_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Highlight_Request)
      return LSP.Messages.Server_Responses.Highlight_Response is
   begin
      return raise Program_Error with "Unimplemented request";
   end On_Highlight_Request;

   ----------------------
   -- On_Hover_Request --
   ----------------------

   overriding function On_Hover_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Hover_Request)
      return LSP.Messages.Server_Responses.Hover_Response is
   begin
      return raise Program_Error with "Unimplemented request";
   end On_Hover_Request;

   -------------------------------
   -- On_Implementation_Request --
   -------------------------------

   overriding function On_Implementation_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Implementation_Request)
      return LSP.Messages.Server_Responses.Location_Link_Response is
   begin
      return raise Program_Error with "Unimplemented request";
   end On_Implementation_Request;

   -------------------------------
   -- On_Incoming_Calls_Request --
   -------------------------------

   overriding function On_Incoming_Calls_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Incoming_Calls_Request)
      return LSP.Messages.Server_Responses.IncomingCalls_Response is
   begin
      return raise Program_Error with "Unimplemented request";
   end On_Incoming_Calls_Request;

   ---------------------------
   -- On_Initialize_Request --
   ---------------------------

   overriding function On_Initialize_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Initialize_Request)
      return LSP.Messages.Server_Responses.Initialize_Response
   is
      use all type LSP.Types.Optional_Boolean;

      Value : LSP.Messages.InitializeParams renames Request.params;

      Response : LSP.Messages.Server_Responses.Initialize_Response
        (Is_Error => False);
   begin
      Self.Client_Settings := Value;

      Response.result.capabilities.textDocumentSync :=
        (Is_Set    => True,
         Is_Number => False,
         Options   =>
           (openClose => True,
            change    => (True, LSP.Messages.Full),
            others    => <>));

      return Response;
   end On_Initialize_Request;

   -------------------------------
   -- On_Outgoing_Calls_Request --
   -------------------------------

   overriding function On_Outgoing_Calls_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Outgoing_Calls_Request)
      return LSP.Messages.Server_Responses.OutgoingCalls_Response is
   begin
      return raise Program_Error with "Unimplemented request";
   end On_Outgoing_Calls_Request;

   ---------------------------------------
   -- On_Prepare_Call_Hierarchy_Request --
   ---------------------------------------

   overriding function On_Prepare_Call_Hierarchy_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Prepare_Call_Hierarchy_Request)
      return LSP.Messages.Server_Responses.PrepareCallHierarchy_Response is
   begin
      return raise Program_Error with "Unimplemented request";
   end On_Prepare_Call_Hierarchy_Request;

   -------------------------------
   -- On_Prepare_Rename_Request --
   -------------------------------

   overriding function On_Prepare_Rename_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Prepare_Rename_Request)
      return LSP.Messages.Server_Responses.Prepare_Rename_Response is
   begin
      return raise Program_Error with "Unimplemented request";
   end On_Prepare_Rename_Request;

   ---------------------------------
   -- On_Range_Formatting_Request --
   ---------------------------------

   overriding function On_Range_Formatting_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Range_Formatting_Request)
      return LSP.Messages.Server_Responses.Range_Formatting_Response is
   begin
      return raise Program_Error with "Unimplemented request";
   end On_Range_Formatting_Request;

   ---------------------------
   -- On_References_Request --
   ---------------------------

   overriding function On_References_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.References_Request)
      return LSP.Messages.Server_Responses.Location_Response is
   begin
      return raise Program_Error with "Unimplemented request";
   end On_References_Request;

   -----------------------
   -- On_Rename_Request --
   -----------------------

   overriding function On_Rename_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Rename_Request)
      return LSP.Messages.Server_Responses.Rename_Response is
   begin
      return raise Program_Error with "Unimplemented request";
   end On_Rename_Request;

   --------------------------------
   -- On_Selection_Range_Request --
   --------------------------------

   overriding function On_Selection_Range_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Selection_Range_Request)
      return LSP.Messages.Server_Responses.SelectionRange_Response is
   begin
      return raise Program_Error with "Unimplemented request";
   end On_Selection_Range_Request;

   -------------------------
   -- On_Shutdown_Request --
   -------------------------

   overriding function On_Shutdown_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Shutdown_Request)
      return LSP.Messages.Server_Responses.Shutdown_Response
   is
      pragma Unreferenced (Request);
   begin
      return Response : LSP.Messages.Server_Responses.Shutdown_Response
        (Is_Error => False);
   end On_Shutdown_Request;

   -------------------------------
   -- On_Signature_Help_Request --
   -------------------------------

   overriding function On_Signature_Help_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Signature_Help_Request)
      return LSP.Messages.Server_Responses.SignatureHelp_Response is
   begin
      return raise Program_Error with "Unimplemented request";
   end On_Signature_Help_Request;

   --------------------------------
   -- On_Type_Definition_Request --
   --------------------------------

   overriding function On_Type_Definition_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Type_Definition_Request)
      return LSP.Messages.Server_Responses.Location_Link_Response is
   begin
      return raise Program_Error with "Unimplemented request";
   end On_Type_Definition_Request;

   ------------------------------------------
   -- On_Workspace_Execute_Command_Request --
   ------------------------------------------

   overriding function On_Workspace_Execute_Command_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Workspace_Execute_Command_Request)
      return LSP.Messages.Server_Responses.ExecuteCommand_Response is
   begin
      return raise Program_Error with "Unimplemented request";
   end On_Workspace_Execute_Command_Request;

   ----------------------------------
   -- On_Workspace_Symbols_Request --
   ----------------------------------

   overriding function On_Workspace_Symbols_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests.Workspace_Symbols_Request)
      return LSP.Messages.Server_Responses.Symbol_Response is
   begin
      return raise Program_Error with "Unimplemented request";
   end On_Workspace_Symbols_Request;

   --------------------------------------------
   -- On_Workspace_Will_Create_Files_Request --
   --------------------------------------------

   overriding function On_Workspace_Will_Create_Files_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests
        .Workspace_Will_Create_Files_Request)
      return LSP.Messages.Server_Responses.WillCreateFiles_Response is
   begin
      return raise Program_Error with "Unimplemented request";
   end On_Workspace_Will_Create_Files_Request;

   --------------------------------------------
   -- On_Workspace_Will_Delete_Files_Request --
   --------------------------------------------

   overriding function On_Workspace_Will_Delete_Files_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests
        .Workspace_Will_Delete_Files_Request)
      return LSP.Messages.Server_Responses.WillDeleteFiles_Response is
   begin
      return raise Program_Error with "Unimplemented request";
   end On_Workspace_Will_Delete_Files_Request;

   --------------------------------------------
   -- On_Workspace_Will_Rename_Files_Request --
   --------------------------------------------

   overriding function On_Workspace_Will_Rename_Files_Request
     (Self    : access Message_Handler;
      Request : LSP.Messages.Server_Requests
        .Workspace_Will_Rename_Files_Request)
      return LSP.Messages.Server_Responses.WillRenameFiles_Response is
   begin
      return raise Program_Error with "Unimplemented request";
   end On_Workspace_Will_Rename_Files_Request;

end LSP.GPR_Handlers;
