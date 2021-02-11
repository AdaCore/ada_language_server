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

with LSP.Types;

package LSP.Clients.Request_Handlers is

   type Request_Handler is limited interface;

   procedure Workspace_Apply_Edit
     (Self    : not null access Request_Handler;
      Request : LSP.Types.LSP_Number_Or_String;
      Params  : LSP.Messages.ApplyWorkspaceEditParams) is null;
   --  The workspace/applyEdit request

   procedure Workspace_Folders
     (Self    : not null access Request_Handler;
      Request : LSP.Types.LSP_Number_Or_String) is null;
   --  The workspace/workspaceFolders request

   procedure Workspace_Configuration
     (Self    : not null access Request_Handler;
      Request : LSP.Types.LSP_Number_Or_String;
      Params  : LSP.Messages.ConfigurationParams) is null;
   --  The workspace/configuration request

   procedure Window_Show_Message
     (Self    : not null access Request_Handler;
      Request : LSP.Types.LSP_Number_Or_String;
      Params  : LSP.Messages.ShowMessageParams) is null;
   --  The window/showMessage request

   procedure Window_Work_Done_Progress_Create
     (Self    : not null access Request_Handler;
      Request : LSP.Types.LSP_Number_Or_String;
      Params  : LSP.Messages.WorkDoneProgressCreateParams) is null;
   --  The window/workDoneProgress/create request

   procedure Client_Register_Capability
     (Self    : not null access Request_Handler;
      Request : LSP.Types.LSP_Number_Or_String;
      Params  : LSP.Messages.RegistrationParams) is null;
   --  The client/registerCapability request

   procedure Client_Unregister_Capability
     (Self    : not null access Request_Handler;
      Request : LSP.Types.LSP_Number_Or_String;
      Params  : LSP.Messages.UnregistrationParams) is null;
   --  The client/unregisterCapability request

end LSP.Clients.Request_Handlers;
