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

package body LSP.Messages.Client_Responses is

   -----------
   -- Visit --
   -----------

   overriding procedure Visit
     (Self    : ApplyWorkspaceEdit_Response;
      Handler : access Client_Response_Sender'Class) is
   begin
      Handler.On_ApplyWorkspaceEdit_Response (Self);
   end Visit;

   -----------
   -- Visit --
   -----------

   overriding procedure Visit
     (Self    : Configuration_Response;
      Handler : access Client_Response_Sender'Class) is
   begin
      Handler.On_Configuration_Response (Self);
   end Visit;

   -----------
   -- Visit --
   -----------

   overriding procedure Visit
     (Self    : ShowMessage_Response;
      Handler : access Client_Response_Sender'Class) is
   begin
      Handler.On_ShowMessage_Response (Self);
   end Visit;

   -----------
   -- Visit --
   -----------

   overriding procedure Visit
     (Self    : WorkspaceFolders_Response;
      Handler : access Client_Response_Sender'Class) is
   begin
      Handler.On_WorkspaceFolders_Response (Self);
   end Visit;

   -----------
   -- Visit --
   -----------

   overriding procedure Visit
     (Self    : WorkDoneProgressCreate_Response;
      Handler : access Client_Response_Sender'Class) is
   begin
      Handler.On_WorkDoneProgressCreate_Response (Self);
   end Visit;

   -----------
   -- Visit --
   -----------

   overriding procedure Visit
     (Self    : RegisterCapability_Response;
      Handler : access Client_Response_Sender'Class) is
   begin
      Handler.On_RegisterCapability_Response (Self);
   end Visit;

   -----------
   -- Visit --
   -----------

   overriding procedure Visit
     (Self    : UnregisterCapability_Response;
      Handler : access Client_Response_Sender'Class) is
   begin
      Handler.On_UnregisterCapability_Response (Self);
   end Visit;

end LSP.Messages.Client_Responses;
