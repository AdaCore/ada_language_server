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

with LSP.Generic_Requests;
with LSP.Client_Request_Receivers; use LSP.Client_Request_Receivers;

package LSP.Messages.Client_Requests is

   type Client_Request is abstract new LSP.Messages.RequestMessage
     with null record;

   procedure Visit
     (Self    : Client_Request;
      Reciver : access Client_Request_Receiver'Class) is abstract;

   package ShowMessage_Requests is
     new LSP.Generic_Requests (Client_Request, ShowMessageParams);

   type ShowMessage_Request is
     new ShowMessage_Requests.Request with null record;

   overriding procedure Visit
     (Self    : ShowMessage_Request;
      Reciver : access Client_Request_Receiver'Class);

   package Workspace_Apply_Edit_Requests is
     new LSP.Generic_Requests (Client_Request, ApplyWorkspaceEditParams);

   type Workspace_Apply_Edit_Request is
     new Workspace_Apply_Edit_Requests.Request with null record;

   overriding procedure Visit
     (Self    : Workspace_Apply_Edit_Request;
      Reciver : access Client_Request_Receiver'Class);

end LSP.Messages.Client_Requests;
