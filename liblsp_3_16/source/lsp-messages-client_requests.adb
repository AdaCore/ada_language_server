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

package body LSP.Messages.Client_Requests is

   -----------
   -- Visit --
   -----------

   overriding procedure Visit
     (Self    : ShowDocument_Request;
      Reciver : access Client_Request_Receiver'Class) is
   begin
      Reciver.On_ShowDocument_Request (Self);
   end Visit;

   -----------
   -- Visit --
   -----------

   overriding procedure Visit
     (Self    : ShowMessage_Request;
      Reciver : access Client_Request_Receiver'Class) is
   begin
      Reciver.On_ShowMessage_Request (Self);
   end Visit;

   -----------
   -- Visit --
   -----------

   overriding procedure Visit
     (Self    : Workspace_Apply_Edit_Request;
      Reciver : access Client_Request_Receiver'Class) is
   begin
      Reciver.On_Workspace_Apply_Edit_Request (Self);
   end Visit;

   -----------
   -- Visit --
   -----------

   overriding procedure Visit
     (Self    : WorkDoneProgressCreate_Request;
      Reciver : access Client_Request_Receiver'Class) is
   begin
      Reciver.On_WorkDoneProgress_Create_Request (Self);
   end Visit;

   -----------
   -- Visit --
   -----------

   overriding procedure Visit
     (Self    : RegisterCapability_Request;
      Reciver : access Client_Request_Receiver'Class) is
   begin
      Reciver.On_RegisterCapability_Request (Self);
   end Visit;

   -----------
   -- Visit --
   -----------

   overriding procedure Visit
     (Self    : Workspace_Configuration_Request;
      Reciver : access Client_Request_Receiver'Class) is
   begin
      Reciver.On_Workspace_Configuration_Request (Self);
   end Visit;

   -----------
   -- Visit --
   -----------

   overriding procedure Visit
     (Self    : Workspace_Folders_Request;
      Reciver : access Client_Request_Receiver'Class) is
   begin
      Reciver.On_Workspace_Folders_Request (Self);
   end Visit;

   -----------
   -- Visit --
   -----------

   overriding procedure Visit
     (Self    : UnregisterCapability_Request;
      Reciver : access Client_Request_Receiver'Class) is
   begin
      Reciver.On_UnregisterCapability_Request (Self);
   end Visit;

end LSP.Messages.Client_Requests;
