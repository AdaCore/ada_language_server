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
--  This package provides (server side) requests handler abstraction.
--  Requests are processed in a synchronous way.

with LSP.Messages;

package LSP.Message_Handlers is

   type Request_Handler is limited interface;
   type Request_Handler_Access is access all Request_Handler'Class;

   function Handle_Request
     (Self    : access Request_Handler;
      Request : LSP.Messages.RequestMessage'Class)
      return LSP.Messages.ResponseMessage'Class is abstract;
   --  Process the given request and return the response. Request is assumed
   --  to contain a RequestMessage.

   function Initialize_Request
     (Self  : access Request_Handler;
      Value : LSP.Messages.InitializeParams)
      return LSP.Messages.Initialize_Response is abstract;
   --  Process 'initialize' request

   function Shutdown_Request
     (Self  : access Request_Handler)
      return LSP.Messages.ResponseMessage is abstract;
   --  Process 'shutdown' request

   function Text_Document_Code_Action_Request
     (Self  : access Request_Handler;
      Value : LSP.Messages.CodeActionParams)
      return LSP.Messages.CodeAction_Response is abstract;
   --  Process 'textDocument/codeAction' request

   function Text_Document_Completion_Request
     (Self  : access Request_Handler;
      Value : LSP.Messages.TextDocumentPositionParams)
      return LSP.Messages.Completion_Response is abstract;
   --  Process 'textDocument/completion' request

   function Text_Document_Definition_Request
     (Self  : access Request_Handler;
      Value : LSP.Messages.TextDocumentPositionParams)
      return LSP.Messages.Location_Response is abstract;
   --  Process 'textDocument/typeDefinition' request

   function Text_Document_Hover_Request
     (Self  : access Request_Handler;
      Value : LSP.Messages.TextDocumentPositionParams)
      return LSP.Messages.Hover_Response is abstract;
   --  Process 'textDocument/hover' request

   function Text_Document_Highlight_Request
     (Self  : access Request_Handler;
      Value : LSP.Messages.TextDocumentPositionParams)
      return LSP.Messages.Highlight_Response is abstract;
   --  Process 'textDocument/documentHighlight' request

   function Text_Document_References_Request
     (Self  : access Request_Handler;
      Value : LSP.Messages.ReferenceParams)
      return LSP.Messages.Location_Response is abstract;
   --  Process 'textDocument/references' request

   function Text_Document_Signature_Help_Request
     (Self  : access Request_Handler;
      Value : LSP.Messages.TextDocumentPositionParams)
      return LSP.Messages.SignatureHelp_Response is abstract;
   --  Process 'textDocument/signatureHelp' request

   function Text_Document_Symbol_Request
     (Self  : access Request_Handler;
      Value : LSP.Messages.DocumentSymbolParams)
      return LSP.Messages.Symbol_Response is abstract;
   --  Process 'textDocument/documentSymbol' request

   function Workspace_Execute_Command_Request
     (Self  : access Request_Handler;
      Value : LSP.Messages.ExecuteCommandParams)
      return LSP.Messages.ExecuteCommand_Response is abstract;
   --  Process 'workspace/executeCommand' request

   function Workspace_Symbol_Request
     (Self  : access Request_Handler;
      Value : LSP.Messages.WorkspaceSymbolParams)
      return LSP.Messages.Symbol_Response is abstract;
   --  Process 'workspace/symbol' request

end LSP.Message_Handlers;
