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

with Ada.Assertions;
with Libadalang.Common;

package body LSP.Error_Decorators is

   generic
      type Request is private;

      type Response (Is_Error : Boolean) is private;

      type Handler is limited interface
        and LSP.Server_Request_Handlers.Server_Request_Handler;

      with function On_Request
        (Self  : not null access Handler;
         Value : Request) return Response is abstract;

   function Generic_Request
     (Self  : not null access Error_Decorator;
      Value : Request) return Response;

   ---------------------
   -- Generic_Request --
   ---------------------

   function Generic_Request
     (Self  : not null access Error_Decorator;
      Value : Request) return Response is
   begin
      return On_Request (Handler'Class (Self.Handler.all)'Access, Value);
   exception
      when E :
         --  Libadalang / Langkit might raise these when working on
         --  invalid Ada Code
         Libadalang.Common.Property_Error
         |  Libadalang.Common.Precondition_Failure

         --  Some versions of Libadalang_Tools raise Assertion_Error
         --  on invalid Ada Code
         | Ada.Assertions.Assertion_Error
         =>

         --  For these exceptions, add traces in the log...
         Self.Trace.Trace
           ("Uncaught exception probably linked to invalid Ada code");
         Self.Trace.Trace (E);

         --  ... and send an empty response to the request: we do not
         --  want the exception to reach the user.
         pragma Warnings (Off, "is read but never assigned");
         return Result : Response (Is_Error => False);
         pragma Warnings (On, "is read but never assigned");

      when E : others =>
         --  The exceptions above are expected to happen in the normal flow
         --  of events in LAL. However, for any other error than a
         --  property error, we want to reload the context.
         Self.On_Error (E);
         raise;
   end Generic_Request;

   ---------------------------
   -- On_Initialize_Request --
   ---------------------------

   overriding function On_Initialize_Request
     (Self    : access Error_Decorator;
      Request : LSP.Messages.Server_Requests.Initialize_Request)
        return LSP.Messages.Server_Responses.Initialize_Response is
   begin
      --  We don't expect Property_Error here and can't provide any good
      --  response by default, so just forward the request here.
      return Self.Handler.On_Initialize_Request (Request);
   end On_Initialize_Request;

   -------------------------
   -- On_Shutdown_Request --
   -------------------------

   function Shutdown_Request is new Generic_Request
     (Request    => LSP.Messages.Server_Requests.Shutdown_Request,
      Response   => LSP.Messages.Server_Responses.Shutdown_Response,
      Handler    => LSP.Server_Request_Handlers.Server_Request_Handler,
      On_Request => LSP.Server_Request_Handlers.On_Shutdown_Request);

   overriding function On_Shutdown_Request
     (Self    : access Error_Decorator;
      Request : LSP.Messages.Server_Requests.Shutdown_Request)
      return LSP.Messages.Server_Responses.Shutdown_Response
        renames Shutdown_Request;

   ---------------------------
   -- On_CodeAction_Request --
   ---------------------------

   function CodeAction_Request is new Generic_Request
     (Request    => LSP.Messages.Server_Requests.CodeAction_Request,
      Response   => LSP.Messages.Server_Responses.CodeAction_Response,
      Handler    => LSP.Server_Request_Handlers.Server_Request_Handler,
      On_Request => LSP.Server_Request_Handlers.On_CodeAction_Request);

   overriding function On_CodeAction_Request
     (Self    : access Error_Decorator;
      Request : LSP.Messages.Server_Requests.CodeAction_Request)
      return LSP.Messages.Server_Responses.CodeAction_Response
        renames CodeAction_Request;

   ---------------------------
   -- On_Completion_Request --
   ---------------------------

   function Completion_Request is new Generic_Request
     (Request    => LSP.Messages.Server_Requests.Completion_Request,
      Response   => LSP.Messages.Server_Responses.Completion_Response,
      Handler    => LSP.Server_Request_Handlers.Server_Request_Handler,
      On_Request => LSP.Server_Request_Handlers.On_Completion_Request);

   overriding function On_Completion_Request
     (Self    : access Error_Decorator;
      Request : LSP.Messages.Server_Requests.Completion_Request)
      return LSP.Messages.Server_Responses.Completion_Response
      renames Completion_Request;

   -----------------------------------
   -- CompletionItemResolve_Request --
   -----------------------------------

   function CompletionItemResolve_Request is new Generic_Request
     (Request    =>
         LSP.Messages.Server_Requests.CompletionItemResolve_Request,
      Response   =>
         LSP.Messages.Server_Responses.CompletionItemResolve_Response,
      Handler    =>
         LSP.Server_Request_Handlers.Server_Request_Handler,
      On_Request =>
         LSP.Server_Request_Handlers.On_CompletionItemResolve_Request);

   overriding function On_CompletionItemResolve_Request
     (Self    : access Error_Decorator;
      Request : LSP.Messages.Server_Requests.CompletionItemResolve_Request)
      return LSP.Messages.Server_Responses.CompletionItemResolve_Response
        renames CompletionItemResolve_Request;

   ---------------------------
   -- On_Declaration_Request --
   ---------------------------

   function Declaration_Request is new Generic_Request
     (Request    => LSP.Messages.Server_Requests.Declaration_Request,
      Response   => LSP.Messages.Server_Responses.Location_Link_Response,
      Handler    => LSP.Server_Request_Handlers.Server_Request_Handler,
      On_Request => LSP.Server_Request_Handlers.On_Declaration_Request);

   overriding function On_Declaration_Request
     (Self    : access Error_Decorator;
      Request : LSP.Messages.Server_Requests.Declaration_Request)
      return LSP.Messages.Server_Responses.Location_Link_Response
        renames Declaration_Request;

   -------------------------------
   -- On_Implementation_Request --
   -------------------------------

   function Implementation_Request is new Generic_Request
     (Request    => LSP.Messages.Server_Requests.Implementation_Request,
      Response   => LSP.Messages.Server_Responses.Location_Link_Response,
      Handler    => LSP.Server_Request_Handlers.Server_Request_Handler,
      On_Request => LSP.Server_Request_Handlers.On_Implementation_Request);

   overriding function On_Implementation_Request
     (Self    : access Error_Decorator;
      Request : LSP.Messages.Server_Requests.Implementation_Request)
      return LSP.Messages.Server_Responses.Location_Link_Response
        renames Implementation_Request;

   ---------------------------
   -- On_Definition_Request --
   ---------------------------

   function Definition_Request is new Generic_Request
     (Request    => LSP.Messages.Server_Requests.Definition_Request,
      Response   => LSP.Messages.Server_Responses.Location_Link_Response,
      Handler    => LSP.Server_Request_Handlers.Server_Request_Handler,
      On_Request => LSP.Server_Request_Handlers.On_Definition_Request);

   overriding function On_Definition_Request
     (Self    : access Error_Decorator;
      Request : LSP.Messages.Server_Requests.Definition_Request)
      return LSP.Messages.Server_Responses.Location_Link_Response
        renames Definition_Request;

   --------------------------------
   -- On_Type_Definition_Request --
   --------------------------------

   function Type_Definition_Request is new Generic_Request
     (Request    => LSP.Messages.Server_Requests.Type_Definition_Request,
      Response   => LSP.Messages.Server_Responses.Location_Link_Response,
      Handler    => LSP.Server_Request_Handlers.Server_Request_Handler,
      On_Request => LSP.Server_Request_Handlers.On_Type_Definition_Request);

   overriding function On_Type_Definition_Request
     (Self    : access Error_Decorator;
      Request : LSP.Messages.Server_Requests.Type_Definition_Request)
      return LSP.Messages.Server_Responses.Location_Link_Response
        renames Type_Definition_Request;

   ------------------------------
   -- On_Folding_Range_Request --
   ------------------------------

   function Folding_Range_Request is new Generic_Request
     (Request    => LSP.Messages.Server_Requests.Folding_Range_Request,
      Response   => LSP.Messages.Server_Responses.FoldingRange_Response,
      Handler    => LSP.Server_Request_Handlers.Server_Request_Handler,
      On_Request => LSP.Server_Request_Handlers.On_Folding_Range_Request);

   overriding function On_Folding_Range_Request
     (Self    : access Error_Decorator;
      Request : LSP.Messages.Server_Requests.Folding_Range_Request)
      return LSP.Messages.Server_Responses.FoldingRange_Response
        renames Folding_Range_Request;

   --------------------------------
   -- On_Selection_Range_Request --
   --------------------------------

   function Selection_Range_Request is new Generic_Request
     (Request    => LSP.Messages.Server_Requests.Selection_Range_Request,
      Response   => LSP.Messages.Server_Responses.SelectionRange_Response,
      Handler    => LSP.Server_Request_Handlers.Server_Request_Handler,
      On_Request => LSP.Server_Request_Handlers.On_Selection_Range_Request);

   overriding function On_Selection_Range_Request
     (Self    : access Error_Decorator;
      Request : LSP.Messages.Server_Requests.Selection_Range_Request)
      return LSP.Messages.Server_Responses.SelectionRange_Response
        renames Selection_Range_Request;

   -------------------------------------
   -- On_Document_Tokens_Full_Request --
   -------------------------------------

   function Document_Tokens_Full_Request is new Generic_Request
     (Request    => LSP.Messages.Server_Requests.Document_Tokens_Full_Request,
      Response   => LSP.Messages.Server_Responses.SemanticTokens_Response,
      Handler    => LSP.Server_Request_Handlers.Server_Request_Handler,
      On_Request =>
         LSP.Server_Request_Handlers.On_Document_Tokens_Full_Request);

   overriding function On_Document_Tokens_Full_Request
     (Self    : access Error_Decorator;
      Request : LSP.Messages.Server_Requests.Document_Tokens_Full_Request)
      return LSP.Messages.Server_Responses.SemanticTokens_Response
        renames Document_Tokens_Full_Request;

   --------------------------------------
   -- On_Document_Tokens_Range_Request --
   --------------------------------------

   function Document_Tokens_Range_Request is new Generic_Request
     (Request    => LSP.Messages.Server_Requests.Document_Tokens_Range_Request,
      Response   => LSP.Messages.Server_Responses.SemanticTokens_Response,
      Handler    => LSP.Server_Request_Handlers.Server_Request_Handler,
      On_Request =>
         LSP.Server_Request_Handlers.On_Document_Tokens_Range_Request);

   overriding function On_Document_Tokens_Range_Request
     (Self    : access Error_Decorator;
      Request : LSP.Messages.Server_Requests.Document_Tokens_Range_Request)
      return LSP.Messages.Server_Responses.SemanticTokens_Response
        renames Document_Tokens_Range_Request;

   --------------------------
   -- On_Highlight_Request --
   --------------------------

   function Highlight_Request is new Generic_Request
     (Request    => LSP.Messages.Server_Requests.Highlight_Request,
      Response   => LSP.Messages.Server_Responses.Highlight_Response,
      Handler    => LSP.Server_Request_Handlers.Server_Request_Handler,
      On_Request => LSP.Server_Request_Handlers.On_Highlight_Request);

   overriding function On_Highlight_Request
     (Self    : access Error_Decorator;
      Request : LSP.Messages.Server_Requests.Highlight_Request)
      return LSP.Messages.Server_Responses.Highlight_Response
        renames Highlight_Request;

   ----------------------
   -- On_Hover_Request --
   ----------------------

   function Hover_Request is new Generic_Request
     (Request    => LSP.Messages.Server_Requests.Hover_Request,
      Response   => LSP.Messages.Server_Responses.Hover_Response,
      Handler    => LSP.Server_Request_Handlers.Server_Request_Handler,
      On_Request => LSP.Server_Request_Handlers.On_Hover_Request);

   overriding function On_Hover_Request
     (Self    : access Error_Decorator;
      Request : LSP.Messages.Server_Requests.Hover_Request)
      return LSP.Messages.Server_Responses.Hover_Response
        renames Hover_Request;

   ---------------------------
   -- On_References_Request --
   ---------------------------

   function References_Request is new Generic_Request
     (Request    => LSP.Messages.Server_Requests.References_Request,
      Response   => LSP.Messages.Server_Responses.Location_Response,
      Handler    => LSP.Server_Request_Handlers.Server_Request_Handler,
      On_Request => LSP.Server_Request_Handlers.On_References_Request);

   overriding function On_References_Request
     (Self    : access Error_Decorator;
      Request : LSP.Messages.Server_Requests.References_Request)
      return LSP.Messages.Server_Responses.Location_Response
        renames References_Request;

   -------------------------------
   -- On_Signature_Help_Request --
   -------------------------------

   function Signature_Help_Request is new Generic_Request
     (Request    => LSP.Messages.Server_Requests.Signature_Help_Request,
      Response   => LSP.Messages.Server_Responses.SignatureHelp_Response,
      Handler    => LSP.Server_Request_Handlers.Server_Request_Handler,
      On_Request => LSP.Server_Request_Handlers.On_Signature_Help_Request);

   overriding function On_Signature_Help_Request
     (Self    : access Error_Decorator;
      Request : LSP.Messages.Server_Requests.Signature_Help_Request)
      return LSP.Messages.Server_Responses.SignatureHelp_Response
        renames Signature_Help_Request;

   -----------------------------------
   -- On_Color_Presentation_Request --
   -----------------------------------

   function Color_Presentation_Request is new Generic_Request
     (Request    => LSP.Messages.Server_Requests.Color_Presentation_Request,
      Response   => LSP.Messages.Server_Responses.ColorPresentation_Response,
      Handler    => LSP.Server_Request_Handlers.Server_Request_Handler,
      On_Request => LSP.Server_Request_Handlers.On_Color_Presentation_Request);

   overriding function On_Color_Presentation_Request
     (Self    : access Error_Decorator;
      Request : LSP.Messages.Server_Requests.Color_Presentation_Request)
      return LSP.Messages.Server_Responses.ColorPresentation_Response
        renames Color_Presentation_Request;

   -------------------------------
   -- On_Document_Color_Request --
   -------------------------------

   function Document_Color_Request is new Generic_Request
     (Request    => LSP.Messages.Server_Requests.Document_Color_Request,
      Response   => LSP.Messages.Server_Responses.DocumentColor_Response,
      Handler    => LSP.Server_Request_Handlers.Server_Request_Handler,
      On_Request => LSP.Server_Request_Handlers.On_Document_Color_Request);

   overriding function On_Document_Color_Request
     (Self    : access Error_Decorator;
      Request : LSP.Messages.Server_Requests.Document_Color_Request)
      return LSP.Messages.Server_Responses.DocumentColor_Response
        renames Document_Color_Request;

   -------------------------------
   -- On_Document_Links_Request --
   -------------------------------

   function Document_Links_Request is new Generic_Request
     (Request    => LSP.Messages.Server_Requests.Document_Links_Request,
      Response   => LSP.Messages.Server_Responses.Links_Response,
      Handler    => LSP.Server_Request_Handlers.Server_Request_Handler,
      On_Request => LSP.Server_Request_Handlers.On_Document_Links_Request);

   overriding function On_Document_Links_Request
     (Self    : access Error_Decorator;
      Request : LSP.Messages.Server_Requests.Document_Links_Request)
      return LSP.Messages.Server_Responses.Links_Response
        renames Document_Links_Request;

   ---------------------------------
   -- On_Document_Symbols_Request --
   ---------------------------------

   function Document_Symbols_Request is new Generic_Request
     (Request    => LSP.Messages.Server_Requests.Document_Symbols_Request,
      Response   => LSP.Messages.Server_Responses.Symbol_Response,
      Handler    => LSP.Server_Request_Handlers.Server_Request_Handler,
      On_Request => LSP.Server_Request_Handlers.On_Document_Symbols_Request);

   overriding function On_Document_Symbols_Request
     (Self    : access Error_Decorator;
      Request : LSP.Messages.Server_Requests.Document_Symbols_Request)
      return LSP.Messages.Server_Responses.Symbol_Response
        renames Document_Symbols_Request;

   ---------------------------------------
   -- On_Prepare_Call_Hierarchy_Request --
   ---------------------------------------

   function Prepare_Call_Hierarchy_Request is new Generic_Request
     (Request => LSP.Messages.Server_Requests.Prepare_Call_Hierarchy_Request,
      Response => LSP.Messages.Server_Responses.PrepareCallHierarchy_Response,
      Handler    => LSP.Server_Request_Handlers.Server_Request_Handler,
      On_Request => Server_Request_Handlers.On_Prepare_Call_Hierarchy_Request);

   overriding function On_Prepare_Call_Hierarchy_Request
     (Self    : access Error_Decorator;
      Request : LSP.Messages.Server_Requests.Prepare_Call_Hierarchy_Request)
      return LSP.Messages.Server_Responses.PrepareCallHierarchy_Response
        renames Prepare_Call_Hierarchy_Request;

   -------------------------------
   -- On_Prepare_Rename_Request --
   -------------------------------

   function Prepare_Rename_Request is new Generic_Request
     (Request    => LSP.Messages.Server_Requests.Prepare_Rename_Request,
      Response   => LSP.Messages.Server_Responses.Prepare_Rename_Response,
      Handler    => LSP.Server_Request_Handlers.Server_Request_Handler,
      On_Request => Server_Request_Handlers.On_Prepare_Rename_Request);

   overriding function On_Prepare_Rename_Request
     (Self    : access Error_Decorator;
      Request : LSP.Messages.Server_Requests.Prepare_Rename_Request)
      return LSP.Messages.Server_Responses.Prepare_Rename_Response
        renames Prepare_Rename_Request;

   -------------------------------
   -- On_Incoming_Calls_Request --
   -------------------------------

   function Incoming_Calls_Request is new Generic_Request
     (Request => LSP.Messages.Server_Requests.Incoming_Calls_Request,
      Response => LSP.Messages.Server_Responses.IncomingCalls_Response,
      Handler    => LSP.Server_Request_Handlers.Server_Request_Handler,
      On_Request => Server_Request_Handlers.On_Incoming_Calls_Request);

   overriding function On_Incoming_Calls_Request
     (Self    : access Error_Decorator;
      Request : LSP.Messages.Server_Requests.Incoming_Calls_Request)
      return LSP.Messages.Server_Responses.IncomingCalls_Response
        renames Incoming_Calls_Request;

   -------------------------------
   -- On_Outgoing_Calls_Request --
   -------------------------------

   function Outgoing_Calls_Request is new Generic_Request
     (Request => LSP.Messages.Server_Requests.Outgoing_Calls_Request,
      Response => LSP.Messages.Server_Responses.OutgoingCalls_Response,
      Handler    => LSP.Server_Request_Handlers.Server_Request_Handler,
      On_Request => Server_Request_Handlers.On_Outgoing_Calls_Request);

   overriding function On_Outgoing_Calls_Request
     (Self    : access Error_Decorator;
      Request : LSP.Messages.Server_Requests.Outgoing_Calls_Request)
      return LSP.Messages.Server_Responses.OutgoingCalls_Response
        renames Outgoing_Calls_Request;

   -----------------------
   -- On_Rename_Request --
   -----------------------

   function Rename_Request is new Generic_Request
     (Request    => LSP.Messages.Server_Requests.Rename_Request,
      Response   => LSP.Messages.Server_Responses.Rename_Response,
      Handler    => LSP.Server_Request_Handlers.Server_Request_Handler,
      On_Request => LSP.Server_Request_Handlers.On_Rename_Request);

   overriding function On_Rename_Request
     (Self    : access Error_Decorator;
      Request : LSP.Messages.Server_Requests.Rename_Request)
      return LSP.Messages.Server_Responses.Rename_Response
        renames Rename_Request;

   --------------------------------
   -- On_Execute_Command_Request --
   --------------------------------

   function Execute_Command_Request is new Generic_Request
     (Request    => LSP.Messages.Server_Requests.Execute_Command_Request,
      Response   => LSP.Messages.Server_Responses.ExecuteCommand_Response,
      Handler    => LSP.Server_Request_Handlers.Server_Request_Handler,
      On_Request => LSP.Server_Request_Handlers.On_Execute_Command_Request);

   overriding function On_Execute_Command_Request
     (Self    : access Error_Decorator;
      Request : LSP.Messages.Server_Requests.Execute_Command_Request)
      return LSP.Messages.Server_Responses.ExecuteCommand_Response
        renames Execute_Command_Request;

   ----------------------------------
   -- On_Workspace_Symbols_Request --
   ----------------------------------

   function Workspace_Symbols_Request is new Generic_Request
     (Request    => LSP.Messages.Server_Requests.Workspace_Symbols_Request,
      Response   => LSP.Messages.Server_Responses.Symbol_Response,
      Handler    => LSP.Server_Request_Handlers.Server_Request_Handler,
      On_Request => LSP.Server_Request_Handlers.On_Workspace_Symbols_Request);

   overriding function On_Workspace_Symbols_Request
     (Self    : access Error_Decorator;
      Request : LSP.Messages.Server_Requests.Workspace_Symbols_Request)
      return LSP.Messages.Server_Responses.Symbol_Response
        renames Workspace_Symbols_Request;

   ------------------------------------------
   -- On_Workspace_Execute_Command_Request --
   ------------------------------------------

   function Workspace_Execute_Command_Request is new Generic_Request
     (Request    => LSP.Messages.Server_Requests
                      .Workspace_Execute_Command_Request,
      Response   => LSP.Messages.Server_Responses.ExecuteCommand_Response,
      Handler    => LSP.Server_Request_Handlers.Server_Request_Handler,
      On_Request => LSP.Server_Request_Handlers
                      .On_Workspace_Execute_Command_Request);

   overriding function On_Workspace_Execute_Command_Request
     (Self    : access Error_Decorator;
      Request : LSP.Messages.Server_Requests.Workspace_Execute_Command_Request)
      return LSP.Messages.Server_Responses.ExecuteCommand_Response
        renames Workspace_Execute_Command_Request;

   --------------------------------------------
   -- On_Workspace_Will_Create_Files_Request --
   --------------------------------------------

   function Workspace_Will_Create_Files_Request
     is new Generic_Request
       (Request    => LSP.Messages.Server_Requests
                        .Workspace_Will_Create_Files_Request,
        Response   => LSP.Messages.Server_Responses.WillCreateFiles_Response,
        Handler    => LSP.Server_Request_Handlers.Server_Request_Handler,
        On_Request => LSP.Server_Request_Handlers
                        .On_Workspace_Will_Create_Files_Request);

   overriding function On_Workspace_Will_Create_Files_Request
     (Self    : access Error_Decorator;
      Request : LSP.Messages.Server_Requests.
                  Workspace_Will_Create_Files_Request)
      return LSP.Messages.Server_Responses.WillCreateFiles_Response
        renames Workspace_Will_Create_Files_Request;

   --------------------------------------------
   -- On_Workspace_Will_Rename_Files_Request --
   --------------------------------------------

   function Workspace_Will_Rename_Files_Request
     is new Generic_Request
       (Request    => LSP.Messages.Server_Requests
                        .Workspace_Will_Rename_Files_Request,
        Response   => LSP.Messages.Server_Responses.WillRenameFiles_Response,
        Handler    => LSP.Server_Request_Handlers.Server_Request_Handler,
        On_Request => LSP.Server_Request_Handlers
                        .On_Workspace_Will_Rename_Files_Request);

   overriding function On_Workspace_Will_Rename_Files_Request
     (Self    : access Error_Decorator;
      Request : LSP.Messages.Server_Requests.
                  Workspace_Will_Rename_Files_Request)
      return LSP.Messages.Server_Responses.WillRenameFiles_Response
        renames Workspace_Will_Rename_Files_Request;

   --------------------------------------------
   -- On_Workspace_Will_Delete_Files_Request --
   --------------------------------------------

   function Workspace_Will_Delete_Files_Request
     is new Generic_Request
       (Request    => LSP.Messages.Server_Requests
                        .Workspace_Will_Delete_Files_Request,
        Response   => LSP.Messages.Server_Responses.WillDeleteFiles_Response,
        Handler    => LSP.Server_Request_Handlers.Server_Request_Handler,
        On_Request => LSP.Server_Request_Handlers
                        .On_Workspace_Will_Delete_Files_Request);

   overriding function On_Workspace_Will_Delete_Files_Request
     (Self    : access Error_Decorator;
      Request : LSP.Messages.Server_Requests.
                  Workspace_Will_Delete_Files_Request)
      return LSP.Messages.Server_Responses.WillDeleteFiles_Response
        renames Workspace_Will_Delete_Files_Request;

   --------------------------------------
   -- On_ALS_Show_Dependencies_Request --
   --------------------------------------

   function ALS_Show_Dependencies_Request is new Generic_Request
     (Request    =>
         LSP.Messages.Server_Requests.ALS_Show_Dependencies_Request,
      Response   =>
         LSP.Messages.Server_Responses.ALS_ShowDependencies_Response,
      Handler    =>
         LSP.Server_Request_Handlers.Server_Request_Handler,
      On_Request =>
         LSP.Server_Request_Handlers.On_ALS_Show_Dependencies_Request);

   overriding function On_ALS_Show_Dependencies_Request
     (Self    : access Error_Decorator;
      Request : LSP.Messages.Server_Requests.ALS_Show_Dependencies_Request)
      return LSP.Messages.Server_Responses.ALS_ShowDependencies_Response
      renames ALS_Show_Dependencies_Request;

   -----------------------------
   -- ALS_Source_Dirs_Request --
   -----------------------------

   function ALS_Source_Dirs_Request is new Generic_Request
     (Request    =>
         LSP.Messages.Server_Requests.ALS_Source_Dirs_Request,
      Response   =>
         LSP.Messages.Server_Responses.ALS_SourceDirs_Response,
      Handler    =>
         LSP.Server_Request_Handlers.Server_Request_Handler,
      On_Request =>
         LSP.Server_Request_Handlers.On_ALS_Source_Dirs_Request);

   overriding function On_ALS_Source_Dirs_Request
     (Self    : access Error_Decorator;
      Request : LSP.Messages.Server_Requests.ALS_Source_Dirs_Request)
      return LSP.Messages.Server_Responses.ALS_SourceDirs_Response
      renames ALS_Source_Dirs_Request;

   --------------------------
   -- On_ALS_Debug_Request --
   --------------------------

   overriding function On_ALS_Debug_Request
     (Self    : access Error_Decorator;
      Request : LSP.Messages.Server_Requests.ALS_Debug_Request)
      return LSP.Messages.Server_Responses.ALS_Debug_Response
   is
   begin
      --  We don't expect Property_Error here
      return Self.Handler.On_ALS_Debug_Request (Request);
   end On_ALS_Debug_Request;

   ---------------------------
   -- On_Formatting_Request --
   ---------------------------

   function Formatting_Request is new Generic_Request
     (Request    => LSP.Messages.Server_Requests.Formatting_Request,
      Response   => LSP.Messages.Server_Responses.Formatting_Response,
      Handler    => LSP.Server_Request_Handlers.Server_Request_Handler,
      On_Request => LSP.Server_Request_Handlers.On_Formatting_Request);

   overriding function On_Formatting_Request
     (Self    : access Error_Decorator;
      Request : LSP.Messages.Server_Requests.Formatting_Request)
      return LSP.Messages.Server_Responses.Formatting_Response
        renames Formatting_Request;

   ---------------------------------
   -- On_Range_Formatting_Request --
   ---------------------------------

   function Range_Formatting_Request is new Generic_Request
     (Request    => LSP.Messages.Server_Requests.Range_Formatting_Request,
      Response   => LSP.Messages.Server_Responses.Range_Formatting_Response,
      Handler    => LSP.Server_Request_Handlers.Server_Request_Handler,
      On_Request => LSP.Server_Request_Handlers.On_Range_Formatting_Request);

   overriding function On_Range_Formatting_Request
     (Self    : access Error_Decorator;
      Request : LSP.Messages.Server_Requests.Range_Formatting_Request)
      return LSP.Messages.Server_Responses.Range_Formatting_Response
        renames Range_Formatting_Request;

   ---------------------------------
   -- On_ALS_Check_Syntax_Request --
   ---------------------------------

   function ALS_Check_Syntax_Request is new Generic_Request
     (Request    => LSP.Messages.Server_Requests.ALS_Check_Syntax_Request,
      Response   => LSP.Messages.Server_Responses.ALS_Check_Syntax_Response,
      Handler    => LSP.Server_Request_Handlers.Server_Request_Handler,
      On_Request => LSP.Server_Request_Handlers.On_ALS_Check_Syntax_Request);

   overriding function On_ALS_Check_Syntax_Request
     (Self    : access Error_Decorator;
      Request : LSP.Messages.Server_Requests.ALS_Check_Syntax_Request)
      return LSP.Messages.Server_Responses.ALS_Check_Syntax_Response
        renames ALS_Check_Syntax_Request;

end LSP.Error_Decorators;
