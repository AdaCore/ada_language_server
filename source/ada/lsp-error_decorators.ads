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
--
--  This package provides requests handler as a wrapper around another requests
--  handler. The purpose of the wrapper is to handle in one place all leaks of
--  Property_Error exception from Libadalang.
with GNATCOLL.Traces;

with LSP.Messages.Server_Requests;
with LSP.Messages.Server_Responses;
with LSP.Servers;
with LSP.Server_Request_Handlers;

package LSP.Error_Decorators is

   type Error_Decorator
     (Trace   : GNATCOLL.Traces.Trace_Handle;
      Handler : not null
        LSP.Server_Request_Handlers.Server_Request_Handler_Access;
     On_Error : not null LSP.Servers.Uncaught_Exception_Handler) is new
        LSP.Server_Request_Handlers.Server_Request_Handler with null record;
   --  The decorator redirect all request to corresponding function of Handler.
   --  If the Property_Error exception is raised then the wrapper log in in the
   --  Trace and return default response. In case of any other exception
   --  On_Error is called and then exception is reraised.

   overriding function On_Initialize_Request
     (Self    : access Error_Decorator;
      Request : LSP.Messages.Server_Requests.Initialize_Request)
      return LSP.Messages.Server_Responses.Initialize_Response;

   overriding function On_Shutdown_Request
     (Self    : access Error_Decorator;
      Request : LSP.Messages.Server_Requests.Shutdown_Request)
      return LSP.Messages.Server_Responses.Shutdown_Response;

   overriding function On_CodeAction_Request
     (Self    : access Error_Decorator;
      Request : LSP.Messages.Server_Requests.CodeAction_Request)
      return LSP.Messages.Server_Responses.CodeAction_Response;

   overriding function On_Completion_Request
     (Self    : access Error_Decorator;
      Request : LSP.Messages.Server_Requests.Completion_Request)
      return LSP.Messages.Server_Responses.Completion_Response;

   overriding function On_CompletionItemResolve_Request
     (Self    : access Error_Decorator;
      Request : LSP.Messages.Server_Requests.CompletionItemResolve_Request)
      return LSP.Messages.Server_Responses.CompletionItemResolve_Response;

   overriding function On_Definition_Request
     (Self    : access Error_Decorator;
      Request : LSP.Messages.Server_Requests.Definition_Request)
      return LSP.Messages.Server_Responses.Location_Link_Response;

   overriding function On_Declaration_Request
     (Self    : access Error_Decorator;
      Request : LSP.Messages.Server_Requests.Declaration_Request)
      return LSP.Messages.Server_Responses.Location_Link_Response;

   overriding function On_Implementation_Request
     (Self    : access Error_Decorator;
      Request : LSP.Messages.Server_Requests.Implementation_Request)
      return LSP.Messages.Server_Responses.Location_Link_Response;

   overriding function On_Type_Definition_Request
     (Self    : access Error_Decorator;
      Request : LSP.Messages.Server_Requests.Type_Definition_Request)
      return LSP.Messages.Server_Responses.Location_Link_Response;

   overriding function On_Highlight_Request
     (Self    : access Error_Decorator;
      Request : LSP.Messages.Server_Requests.Highlight_Request)
      return LSP.Messages.Server_Responses.Highlight_Response;

   overriding function On_Hover_Request
     (Self    : access Error_Decorator;
      Request : LSP.Messages.Server_Requests.Hover_Request)
      return LSP.Messages.Server_Responses.Hover_Response;

   overriding function On_References_Request
     (Self    : access Error_Decorator;
      Request : LSP.Messages.Server_Requests.References_Request)
      return LSP.Messages.Server_Responses.Location_Response;

   overriding function On_Signature_Help_Request
     (Self    : access Error_Decorator;
      Request : LSP.Messages.Server_Requests.Signature_Help_Request)
      return LSP.Messages.Server_Responses.SignatureHelp_Response;

   overriding function On_Color_Presentation_Request
     (Self    : access Error_Decorator;
      Request : LSP.Messages.Server_Requests.Color_Presentation_Request)
      return LSP.Messages.Server_Responses.ColorPresentation_Response;

   overriding function On_Document_Color_Request
     (Self    : access Error_Decorator;
      Request : LSP.Messages.Server_Requests.Document_Color_Request)
      return LSP.Messages.Server_Responses.DocumentColor_Response;

   overriding function On_Document_Links_Request
     (Self    : access Error_Decorator;
      Request : LSP.Messages.Server_Requests.Document_Links_Request)
      return LSP.Messages.Server_Responses.Links_Response;

   overriding function On_Document_Symbols_Request
     (Self    : access Error_Decorator;
      Request : LSP.Messages.Server_Requests.Document_Symbols_Request)
      return LSP.Messages.Server_Responses.Symbol_Response;

   overriding function On_Rename_Request
     (Self    : access Error_Decorator;
      Request : LSP.Messages.Server_Requests.Rename_Request)
      return LSP.Messages.Server_Responses.Rename_Response;

   overriding function On_Prepare_Rename_Request
     (Self    : access Error_Decorator;
      Request : LSP.Messages.Server_Requests.Prepare_Rename_Request)
      return LSP.Messages.Server_Responses.Prepare_Rename_Response;

   overriding function On_Execute_Command_Request
     (Self    : access Error_Decorator;
      Request : LSP.Messages.Server_Requests.Execute_Command_Request)
      return LSP.Messages.Server_Responses.ExecuteCommand_Response;

   overriding function On_Folding_Range_Request
     (Self    : access Error_Decorator;
      Request : LSP.Messages.Server_Requests.Folding_Range_Request)
      return LSP.Messages.Server_Responses.FoldingRange_Response;

   overriding function On_Selection_Range_Request
     (Self    : access Error_Decorator;
      Request : LSP.Messages.Server_Requests.Selection_Range_Request)
      return LSP.Messages.Server_Responses.SelectionRange_Response;

   overriding function On_Document_Tokens_Full_Request
     (Self    : access Error_Decorator;
      Request : LSP.Messages.Server_Requests.Document_Tokens_Full_Request)
      return LSP.Messages.Server_Responses.SemanticTokens_Response;

   overriding function On_Document_Tokens_Range_Request
     (Self    : access Error_Decorator;
      Request : LSP.Messages.Server_Requests.Document_Tokens_Range_Request)
      return LSP.Messages.Server_Responses.SemanticTokens_Response;

   overriding function On_Prepare_Call_Hierarchy_Request
     (Self    : access Error_Decorator;
      Request : LSP.Messages.Server_Requests.Prepare_Call_Hierarchy_Request)
      return LSP.Messages.Server_Responses.PrepareCallHierarchy_Response;

   overriding function On_Incoming_Calls_Request
     (Self    : access Error_Decorator;
      Request : LSP.Messages.Server_Requests.Incoming_Calls_Request)
      return LSP.Messages.Server_Responses.IncomingCalls_Response;

   overriding function On_Outgoing_Calls_Request
     (Self    : access Error_Decorator;
      Request : LSP.Messages.Server_Requests.Outgoing_Calls_Request)
      return LSP.Messages.Server_Responses.OutgoingCalls_Response;

   overriding function On_Workspace_Symbols_Request
     (Self    : access Error_Decorator;
      Request : LSP.Messages.Server_Requests.Workspace_Symbols_Request)
      return LSP.Messages.Server_Responses.Symbol_Response;

   overriding function On_Workspace_Execute_Command_Request
     (Self    : access Error_Decorator;
      Request : LSP.Messages.Server_Requests.Workspace_Execute_Command_Request)
      return LSP.Messages.Server_Responses.ExecuteCommand_Response;

   overriding function On_Workspace_Will_Create_Files_Request
     (Self    : access Error_Decorator;
      Request : LSP.Messages.Server_Requests.
                  Workspace_Will_Create_Files_Request)
      return LSP.Messages.Server_Responses.WillCreateFiles_Response;

   overriding function On_Workspace_Will_Rename_Files_Request
     (Self    : access Error_Decorator;
      Request : LSP.Messages.Server_Requests.
                  Workspace_Will_Rename_Files_Request)
      return LSP.Messages.Server_Responses.WillRenameFiles_Response;

   overriding function On_Workspace_Will_Delete_Files_Request
     (Self    : access Error_Decorator;
      Request : LSP.Messages.Server_Requests.
                  Workspace_Will_Delete_Files_Request)
      return LSP.Messages.Server_Responses.WillDeleteFiles_Response;

   overriding function On_ALS_Show_Dependencies_Request
     (Self    : access Error_Decorator;
      Request : LSP.Messages.Server_Requests.ALS_Show_Dependencies_Request)
      return LSP.Messages.Server_Responses.ALS_ShowDependencies_Response;

   overriding function On_ALS_Source_Dirs_Request
     (Self    : access Error_Decorator;
      Request : LSP.Messages.Server_Requests.ALS_Source_Dirs_Request)
      return LSP.Messages.Server_Responses.ALS_SourceDirs_Response;

   overriding function On_ALS_Debug_Request
     (Self    : access Error_Decorator;
      Request : LSP.Messages.Server_Requests.ALS_Debug_Request)
      return LSP.Messages.Server_Responses.ALS_Debug_Response;

   overriding function On_Formatting_Request
     (Self    : access Error_Decorator;
      Request : LSP.Messages.Server_Requests.Formatting_Request)
      return LSP.Messages.Server_Responses.Formatting_Response;

   overriding function On_Range_Formatting_Request
     (Self    : access Error_Decorator;
      Request : LSP.Messages.Server_Requests.Range_Formatting_Request)
      return LSP.Messages.Server_Responses.Range_Formatting_Response;

   overriding function On_ALS_Check_Syntax_Request
     (Self    : access Error_Decorator;
      Request : LSP.Messages.Server_Requests.ALS_Check_Syntax_Request)
      return LSP.Messages.Server_Responses.ALS_Check_Syntax_Response;

end LSP.Error_Decorators;
