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

with LSP.Messages;

package LSP.Message_Handlers is
--   pragma Preelaborate;

   type Request_Handler is limited interface;
   type Request_Handler_Access is access all Request_Handler'Class;

   procedure Initialize_Request
    (Self     : access Request_Handler;
     Value    : LSP.Messages.InitializeParams;
     Response : in out LSP.Messages.Initialize_Response) is null;

   procedure Shutdown_Request
    (Self     : access Request_Handler;
     Response : in out LSP.Messages.ResponseMessage) is null;

   procedure Text_Document_Code_Action_Request
    (Self     : access Request_Handler;
     Value    : LSP.Messages.CodeActionParams;
     Response : in out LSP.Messages.CodeAction_Response) is null;

   procedure Text_Document_Completion_Request
    (Self     : access Request_Handler;
     Value    : LSP.Messages.TextDocumentPositionParams;
     Response : in out LSP.Messages.Completion_Response) is null;

   procedure Text_Document_Definition_Request
    (Self     : access Request_Handler;
     Value    : LSP.Messages.TextDocumentPositionParams;
     Response : in out LSP.Messages.Location_Response) is null;

   procedure Text_Document_Hover_Request
    (Self     : access Request_Handler;
     Value    : LSP.Messages.TextDocumentPositionParams;
     Response : in out LSP.Messages.Hover_Response) is null;

   procedure Text_Document_Highlight_Request
    (Self     : access Request_Handler;
     Value    : LSP.Messages.TextDocumentPositionParams;
     Response : in out LSP.Messages.Highlight_Response) is null;

   procedure Text_Document_References_Request
    (Self     : access Request_Handler;
     Value    : LSP.Messages.ReferenceParams;
     Response : in out LSP.Messages.Location_Response) is null;

   procedure Text_Document_Signature_Help_Request
    (Self     : access Request_Handler;
     Value    : LSP.Messages.TextDocumentPositionParams;
     Response : in out LSP.Messages.SignatureHelp_Response) is null;

   procedure Text_Document_Symbol_Request
    (Self     : access Request_Handler;
     Value    : LSP.Messages.DocumentSymbolParams;
     Response : in out LSP.Messages.Symbol_Response) is null;

   procedure Workspace_Execute_Command_Request
    (Self     : access Request_Handler;
     Value    : LSP.Messages.ExecuteCommandParams;
     Response : in out LSP.Messages.ExecuteCommand_Response) is null;

   procedure Workspace_Symbol_Request
    (Self     : access Request_Handler;
     Value    : LSP.Messages.WorkspaceSymbolParams;
     Response : in out LSP.Messages.Symbol_Response) is null;

end LSP.Message_Handlers;
