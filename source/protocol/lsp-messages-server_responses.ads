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

package LSP.Messages.Server_Responses is

   package Initialize_Responses is new LSP.Generic_Responses
     (ResponseMessage => ResponseMessage,
      T               => InitializeResult);

   type Initialize_Response is new Initialize_Responses.Response with
     null record;

   package Completion_Responses is new LSP.Generic_Responses
     (ResponseMessage => ResponseMessage,
      T               => CompletionList);

   type Completion_Response is new Completion_Responses.Response with
     null record;

   package Hover_Responses is new LSP.Generic_Responses
     (ResponseMessage => ResponseMessage,
      T               => Hover);

   type Hover_Response is new Hover_Responses.Response with null record;

   package SignatureHelp_Responses is new LSP.Generic_Responses
     (ResponseMessage => ResponseMessage,
      T               => SignatureHelp);

   type SignatureHelp_Response is new SignatureHelp_Responses.Response with
     null record;

   package Highlight_Responses is new LSP.Generic_Responses
     (ResponseMessage => ResponseMessage,
      T               => DocumentHighlight_Vector);

   type Highlight_Response is new Highlight_Responses.Response with
     null record;

   package Symbol_Responses is new LSP.Generic_Responses
     (ResponseMessage => ResponseMessage,
      T               => SymbolInformation_Vector);

   type Symbol_Response is new Symbol_Responses.Response with null record;

   package Rename_Responses is new LSP.Generic_Responses
     (ResponseMessage => ResponseMessage,
      T               => WorkspaceEdit);

   type Rename_Response is new Rename_Responses.Response with null record;

   package CodeAction_Responses is new LSP.Generic_Responses
     (ResponseMessage => ResponseMessage,
      T               => Command_Vector);

   type CodeAction_Response is new CodeAction_Responses.Response with
     null record;

   package Location_Responses is new LSP.Generic_Responses
     (ResponseMessage => ResponseMessage,
      T               => Location_Vector);

   type Location_Response is new Location_Responses.Response with null record;

   package ALS_Called_By_Responses is new LSP.Generic_Responses
     (ResponseMessage => ResponseMessage,
      T               => ALS_Subprogram_And_References_Vector);

   type ALS_Called_By_Response is new ALS_Called_By_Responses.Response with
     null record;

   type ExecuteCommand_Response is new ResponseMessage with null record;

   type Shutdown_Response is new ResponseMessage with null record;

end LSP.Messages.Server_Responses;
