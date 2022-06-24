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

with LSP.Generic_Responses;
with LSP.Server_Response_Senders; use LSP.Server_Response_Senders;

package LSP.Messages.Server_Responses is

   type Server_Response is abstract new ResponseMessage with null record;

   procedure Visit
     (Self    : Server_Response;
      Handler : access Server_Response_Sender'Class) is abstract;

   package Initialize_Responses is new LSP.Generic_Responses
     (ResponseMessage => Server_Response,
      T               => InitializeResult);

   type Initialize_Response is new Initialize_Responses.Response with
     null record;

   overriding procedure Visit
     (Self    : Initialize_Response;
      Handler : access Server_Response_Sender'Class);

   package Completion_Responses is new LSP.Generic_Responses
     (ResponseMessage => Server_Response,
      T               => CompletionList);

   type Completion_Response is new Completion_Responses.Response with
     null record;

   overriding procedure Visit
     (Self    : Completion_Response;
      Handler : access Server_Response_Sender'Class);

   package CompletionItemResolve_Responses is new LSP.Generic_Responses
     (ResponseMessage => Server_Response,
      T               => CompletionItem);

   type CompletionItemResolve_Response is
     new CompletionItemResolve_Responses.Response with null record;

   overriding procedure Visit
     (Self    : CompletionItemResolve_Response;
      Handler : access Server_Response_Sender'Class);

   package Hover_Responses is new LSP.Generic_Responses
     (ResponseMessage => Server_Response,
      T               => Optional_Hover);

   type Hover_Response is new Hover_Responses.Response with null record;

   overriding procedure Visit
     (Self    : Hover_Response;
      Handler : access Server_Response_Sender'Class);

   package SignatureHelp_Responses is new LSP.Generic_Responses
     (ResponseMessage => Server_Response,
      T               => SignatureHelp);

   type SignatureHelp_Response is new SignatureHelp_Responses.Response with
     null record;

   overriding procedure Visit
     (Self    : SignatureHelp_Response;
      Handler : access Server_Response_Sender'Class);

   package Highlight_Responses is new LSP.Generic_Responses
     (ResponseMessage => Server_Response,
      T               => DocumentHighlight_Vector);

   type Highlight_Response is new Highlight_Responses.Response with
     null record;

   overriding procedure Visit
     (Self    : Highlight_Response;
      Handler : access Server_Response_Sender'Class);

   package Links_Responses is new LSP.Generic_Responses
     (ResponseMessage => Server_Response,
      T               => DocumentLink_Vector);

   type Links_Response is new Links_Responses.Response with null record;

   overriding procedure Visit
     (Self    : Links_Response;
      Handler : access Server_Response_Sender'Class);

   package Symbol_Responses is new LSP.Generic_Responses
     (ResponseMessage => Server_Response,
      T               => Symbol_Vector);

   type Symbol_Response is new Symbol_Responses.Response with null record;

   overriding procedure Visit
     (Self    : Symbol_Response;
      Handler : access Server_Response_Sender'Class);

   package Rename_Responses is new LSP.Generic_Responses
     (ResponseMessage => Server_Response,
      T               => WorkspaceEdit);

   type Rename_Response is new Rename_Responses.Response with null record;

   overriding procedure Visit
     (Self    : Rename_Response;
      Handler : access Server_Response_Sender'Class);

   package Prepare_Rename_Responses is new LSP.Generic_Responses
     (ResponseMessage => Server_Response,
      T               => Optional_Span_Or_Null);

   type Prepare_Rename_Response is new Prepare_Rename_Responses.Response
     with null record;

   overriding procedure Visit
     (Self    : Prepare_Rename_Response;
      Handler : access Server_Response_Sender'Class);

   package CodeAction_Responses is new LSP.Generic_Responses
     (ResponseMessage => Server_Response,
      T               => CodeAction_Vector);

   type CodeAction_Response is new CodeAction_Responses.Response with
     null record;

   overriding procedure Visit
     (Self    : CodeAction_Response;
      Handler : access Server_Response_Sender'Class);

   package Location_Responses is new LSP.Generic_Responses
     (ResponseMessage => Server_Response,
      T               => Location_Vector);

   type Location_Response is new Location_Responses.Response with null record;

   overriding procedure Visit
     (Self    : Location_Response;
      Handler : access Server_Response_Sender'Class);

   package Location_Link_Responses is new LSP.Generic_Responses
     (ResponseMessage => Server_Response,
      T               => Location_Or_Link_Vector);

   type Location_Link_Response is
     new Location_Link_Responses.Response with null record;

   overriding procedure Visit
     (Self    : Location_Link_Response;
      Handler : access Server_Response_Sender'Class);

   package DocumentColor_Responses is new LSP.Generic_Responses
     (ResponseMessage => Server_Response,
      T               => ColorInformation_Vector);

   type DocumentColor_Response is
     new DocumentColor_Responses.Response with null record;

   overriding procedure Visit
     (Self    : DocumentColor_Response;
      Handler : access Server_Response_Sender'Class);

   package ColorPresentation_Responses is new LSP.Generic_Responses
     (ResponseMessage => Server_Response,
      T               => ColorPresentation_Vector);

   type ColorPresentation_Response is
     new ColorPresentation_Responses.Response with null record;

   overriding procedure Visit
     (Self    : ColorPresentation_Response;
      Handler : access Server_Response_Sender'Class);

   package FoldingRange_Responses is new LSP.Generic_Responses
     (ResponseMessage => Server_Response,
      T               => FoldingRange_Vector);

   type FoldingRange_Response is
     new FoldingRange_Responses.Response with null record;

   overriding procedure Visit
     (Self    : FoldingRange_Response;
      Handler : access Server_Response_Sender'Class);

   package SelectionRange_Responses is new LSP.Generic_Responses
     (ResponseMessage => Server_Response,
      T               => SelectionRange_Vector);

   type SelectionRange_Response is
     new SelectionRange_Responses.Response with null record;

   overriding procedure Visit
     (Self    : SelectionRange_Response;
      Handler : access Server_Response_Sender'Class);

   package PrepareCallHierarchy_Responses is new LSP.Generic_Responses
     (ResponseMessage => Server_Response,
      T               => CallHierarchyItem_Vector);

   type PrepareCallHierarchy_Response is
     new PrepareCallHierarchy_Responses.Response with null record;

   overriding procedure Visit
     (Self    : PrepareCallHierarchy_Response;
      Handler : access Server_Response_Sender'Class);

   package IncomingCalls_Responses is new LSP.Generic_Responses
     (ResponseMessage => Server_Response,
      T               => CallHierarchyIncomingCall_Vector);

   type IncomingCalls_Response is
     new IncomingCalls_Responses.Response with null record;

   overriding procedure Visit
     (Self    : IncomingCalls_Response;
      Handler : access Server_Response_Sender'Class);

   package OutgoingCalls_Responses is new LSP.Generic_Responses
     (ResponseMessage => Server_Response,
      T               => CallHierarchyOutgoingCall_Vector);

   type OutgoingCalls_Response is
     new OutgoingCalls_Responses.Response with null record;

   overriding procedure Visit
     (Self    : OutgoingCalls_Response;
      Handler : access Server_Response_Sender'Class);

   package ALS_ShowDependencies_Responses is new LSP.Generic_Responses
     (ResponseMessage => Server_Response,
      T               => ALS_Unit_Description_Vector);

   type ALS_ShowDependencies_Response is
     new ALS_ShowDependencies_Responses.Response with null record;

   overriding procedure Visit
     (Self    : ALS_ShowDependencies_Response;
      Handler : access Server_Response_Sender'Class);

   package ALS_SourceDirs_Responses is new LSP.Generic_Responses
     (ResponseMessage => Server_Response,
      T               => ALS_Source_Dir_Description_Vector);

   type ALS_SourceDirs_Response is
     new ALS_SourceDirs_Responses.Response with null record;

   overriding procedure Visit
     (Self    : ALS_SourceDirs_Response;
      Handler : access Server_Response_Sender'Class);

   type ExecuteCommand_Response is new Server_Response with null record;

   overriding procedure Visit
     (Self    : ExecuteCommand_Response;
      Handler : access Server_Response_Sender'Class);

   package WillCreateFiles_Responses is new LSP.Generic_Responses
     (ResponseMessage => Server_Response,
      T               => Optional_WorkspaceEdit_Or_Null);

   type WillCreateFiles_Response is
     new WillCreateFiles_Responses.Response with null record;

   overriding procedure Visit
     (Self    : WillCreateFiles_Response;
      Handler : access Server_Response_Sender'Class);

   package WillRenameFiles_Responses is new LSP.Generic_Responses
     (ResponseMessage => Server_Response,
      T               => Optional_WorkspaceEdit_Or_Null);

   type WillRenameFiles_Response is
     new WillRenameFiles_Responses.Response with null record;

   overriding procedure Visit
     (Self    : WillRenameFiles_Response;
      Handler : access Server_Response_Sender'Class);

   package WillDeleteFiles_Responses is new LSP.Generic_Responses
     (ResponseMessage => Server_Response,
      T               => Optional_WorkspaceEdit_Or_Null);

   type WillDeleteFiles_Response is
     new WillDeleteFiles_Responses.Response with null record;

   overriding procedure Visit
     (Self    : WillDeleteFiles_Response;
      Handler : access Server_Response_Sender'Class);

   type Shutdown_Response is new Server_Response with null record;

   overriding procedure Visit
     (Self    : Shutdown_Response;
      Handler : access Server_Response_Sender'Class);

   type ALS_Debug_Response is new Server_Response with null record;

   overriding procedure Visit
     (Self    : ALS_Debug_Response;
      Handler : access Server_Response_Sender'Class);

   package Formatting_Responses is new LSP.Generic_Responses
     (ResponseMessage => Server_Response,
      T               => TextEdit_Vector);

   type Formatting_Response is
     new Formatting_Responses.Response with null record;

   overriding procedure Visit
     (Self    : Formatting_Response;
      Handler : access Server_Response_Sender'Class);

   package Range_Formatting_Responses is new LSP.Generic_Responses
     (ResponseMessage => Server_Response,
      T               => TextEdit_Vector);

   type Range_Formatting_Response is
     new Range_Formatting_Responses.Response with null record;

   overriding procedure Visit
     (Self    : Range_Formatting_Response;
      Handler : access Server_Response_Sender'Class);

   package SemanticTokens_Responses is new LSP.Generic_Responses
     (ResponseMessage => Server_Response,
      T               => SemanticTokens);

   type SemanticTokens_Response is
     new SemanticTokens_Responses.Response with null record;

   overriding procedure Visit
     (Self    : SemanticTokens_Response;
      Handler : access Server_Response_Sender'Class);

   package ALS_Check_Syntax_Responses is new LSP.Generic_Responses
     (ResponseMessage => Server_Response,
      T               => ALS_Check_Syntax_Result);

   type ALS_Check_Syntax_Response is
     new ALS_Check_Syntax_Responses.Response with null record;

   overriding procedure Visit
     (Self    : ALS_Check_Syntax_Response;
      Handler : access Server_Response_Sender'Class);

end LSP.Messages.Server_Responses;
