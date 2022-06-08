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
--  Interface to process responses sent to the client.

limited with LSP.Messages.Server_Responses;

package LSP.Server_Response_Senders is

   type Server_Response_Sender is limited interface;

   procedure On_Initialize_Response
     (Self     : in out Server_Response_Sender;
      Response : LSP.Messages.Server_Responses.Initialize_Response)
       is abstract;

   procedure On_Completion_Response
     (Self     : in out Server_Response_Sender;
      Response : LSP.Messages.Server_Responses.Completion_Response)
       is abstract;

   procedure On_CompletionItemResolve_Response
     (Self     : in out Server_Response_Sender;
      Response : LSP.Messages.Server_Responses.CompletionItemResolve_Response)
       is abstract;

   procedure On_Hover_Response
     (Self     : in out Server_Response_Sender;
      Response : LSP.Messages.Server_Responses.Hover_Response) is abstract;

   procedure On_SignatureHelp_Response
     (Self     : in out Server_Response_Sender;
      Response : LSP.Messages.Server_Responses.SignatureHelp_Response)
       is abstract;

   procedure On_FoldingRange_Response
     (Self     : in out Server_Response_Sender;
      Response : LSP.Messages.Server_Responses.FoldingRange_Response)
       is abstract;

   procedure On_Highlight_Response
     (Self     : in out Server_Response_Sender;
      Response : LSP.Messages.Server_Responses.Highlight_Response) is abstract;

   procedure On_Symbol_Response
     (Self     : in out Server_Response_Sender;
      Response : LSP.Messages.Server_Responses.Symbol_Response) is abstract;

   procedure On_Links_Response
     (Self     : in out Server_Response_Sender;
      Response : LSP.Messages.Server_Responses.Links_Response) is abstract;

   procedure On_Rename_Response
     (Self     : in out Server_Response_Sender;
      Response : LSP.Messages.Server_Responses.Rename_Response) is abstract;

   procedure On_Prepare_Rename_Response
     (Self     : in out Server_Response_Sender;
      Response : LSP.Messages.Server_Responses.Prepare_Rename_Response)
       is abstract;

   procedure On_CodeAction_Response
     (Self     : in out Server_Response_Sender;
      Response : LSP.Messages.Server_Responses.CodeAction_Response)
       is abstract;

   procedure On_Location_Response
     (Self     : in out Server_Response_Sender;
      Response : LSP.Messages.Server_Responses.Location_Response) is abstract;

   procedure On_Location_Link_Response
     (Self     : in out Server_Response_Sender;
      Response : LSP.Messages.Server_Responses.Location_Link_Response)
       is abstract;

   procedure On_ALS_ShowDependencies_Response
     (Self     : in out Server_Response_Sender;
      Response : LSP.Messages.Server_Responses.ALS_ShowDependencies_Response)
   is abstract;

   procedure On_ALS_SourceDirs_Response
     (Self     : in out Server_Response_Sender;
      Response : LSP.Messages.Server_Responses.ALS_SourceDirs_Response)
   is abstract;

   procedure On_ExecuteCommand_Response
     (Self     : in out Server_Response_Sender;
      Response : LSP.Messages.Server_Responses.ExecuteCommand_Response)
        is abstract;

   procedure On_WillCreateFiles_Response
     (Self     : in out Server_Response_Sender;
      Response : LSP.Messages.Server_Responses.WillCreateFiles_Response)
   is abstract;

   procedure On_WillRenameFiles_Response
     (Self     : in out Server_Response_Sender;
      Response : LSP.Messages.Server_Responses.WillRenameFiles_Response)
   is abstract;

   procedure On_WillDeleteFiles_Response
     (Self     : in out Server_Response_Sender;
      Response : LSP.Messages.Server_Responses.WillDeleteFiles_Response)
   is abstract;

   procedure On_DocumentColor_Response
     (Self     : in out Server_Response_Sender;
      Response : LSP.Messages.Server_Responses.DocumentColor_Response)
        is abstract;

   procedure On_ColorPresentation_Response
     (Self     : in out Server_Response_Sender;
      Response : LSP.Messages.Server_Responses.ColorPresentation_Response)
        is abstract;

   procedure On_SelectionRange_Response
     (Self     : in out Server_Response_Sender;
      Response : LSP.Messages.Server_Responses.SelectionRange_Response)
       is abstract;

   procedure On_Prepare_Call_Hierarchy_Response
     (Self     : in out Server_Response_Sender;
      Response : LSP.Messages.Server_Responses.PrepareCallHierarchy_Response)
       is abstract;

   procedure On_Incoming_Calls_Response
     (Self     : in out Server_Response_Sender;
      Response : LSP.Messages.Server_Responses.IncomingCalls_Response)
       is abstract;

   procedure On_Outgoing_Calls_Response
     (Self     : in out Server_Response_Sender;
      Response : LSP.Messages.Server_Responses.OutgoingCalls_Response)
       is abstract;

   procedure On_Formatting_Response
     (Self     : in out Server_Response_Sender;
      Response : LSP.Messages.Server_Responses.Formatting_Response)
       is abstract;

   procedure On_Range_Formatting_Response
     (Self     : in out Server_Response_Sender;
      Response : LSP.Messages.Server_Responses.Range_Formatting_Response)
       is abstract;

   procedure On_SemanticTokens_Response
     (Self     : in out Server_Response_Sender;
      Response : LSP.Messages.Server_Responses.SemanticTokens_Response)
       is abstract;

   procedure On_Shutdown_Response
     (Self     : in out Server_Response_Sender;
      Response : LSP.Messages.Server_Responses.Shutdown_Response) is abstract;

   procedure On_ALS_Debug_Response
     (Self     : in out Server_Response_Sender;
      Response : LSP.Messages.Server_Responses.ALS_Debug_Response) is abstract;

   procedure On_ALS_Check_Syntax_Response
     (Self     : in out Server_Response_Sender;
      Response : LSP.Messages.Server_Responses.ALS_Check_Syntax_Response)
       is abstract;

end LSP.Server_Response_Senders;
