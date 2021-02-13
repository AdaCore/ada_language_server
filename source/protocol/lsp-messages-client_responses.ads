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
--  Types for responses sent to the server.

with LSP.Generic_Responses;
with LSP.Client_Response_Senders; use LSP.Client_Response_Senders;

package LSP.Messages.Client_Responses is

   type Client_Response is abstract new ResponseMessage with null record;

   procedure Visit
     (Self    : Client_Response;
      Handler : access Client_Response_Sender'Class) is abstract;

   package ApplyWorkspaceEdit_Responses is new LSP.Generic_Responses
     (Client_Response,
      ApplyWorkspaceEditResult);

   type ApplyWorkspaceEdit_Response is
     new ApplyWorkspaceEdit_Responses.Response with null record;

   overriding procedure Visit
     (Self    : ApplyWorkspaceEdit_Response;
      Handler : access Client_Response_Sender'Class);

   package Configuration_Responses is new LSP.Generic_Responses
     (Client_Response,
      Any_Vector);

   type Configuration_Response is
     new Configuration_Responses.Response with null record;

   overriding procedure Visit
     (Self    : Configuration_Response;
      Handler : access Client_Response_Sender'Class);

   package WorkspaceFolders_Responses is new LSP.Generic_Responses
     (Client_Response,
      Optional_WorkspaceFolder_Vector);

   type WorkspaceFolders_Response is
     new WorkspaceFolders_Responses.Response with null record;

   overriding procedure Visit
     (Self    : WorkspaceFolders_Response;
      Handler : access Client_Response_Sender'Class);

   type ShowMessage_Response is new Client_Response with null record;

   overriding procedure Visit
     (Self    : ShowMessage_Response;
      Handler : access Client_Response_Sender'Class);

   type WorkDoneProgressCreate_Response is new Client_Response
     with null record;

   overriding procedure Visit
     (Self    : WorkDoneProgressCreate_Response;
      Handler : access Client_Response_Sender'Class);

   type RegisterCapability_Response is new Client_Response with null record;

   overriding procedure Visit
     (Self    : RegisterCapability_Response;
      Handler : access Client_Response_Sender'Class);

   type UnregisterCapability_Response is new Client_Response with null record;

   overriding procedure Visit
     (Self    : UnregisterCapability_Response;
      Handler : access Client_Response_Sender'Class);

end LSP.Messages.Client_Responses;
