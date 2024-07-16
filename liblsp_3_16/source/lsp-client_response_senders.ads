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
--
--  Interface to process responses sent to the server.

limited with LSP.Messages.Client_Responses;

package LSP.Client_Response_Senders is

   type Client_Response_Sender is limited interface;
   --  Handler of responses on LSP client side

   procedure On_ApplyWorkspaceEdit_Response
     (Self     : in out Client_Response_Sender;
      Response : LSP.Messages.Client_Responses.ApplyWorkspaceEdit_Response)
   is abstract;

   procedure On_Configuration_Response
     (Self     : in out Client_Response_Sender;
      Response : LSP.Messages.Client_Responses.Configuration_Response)
   is abstract;

   procedure On_ShowMessage_Response
     (Self     : in out Client_Response_Sender;
      Response : LSP.Messages.Client_Responses.ShowMessage_Response)
   is abstract;

   procedure On_WorkspaceFolders_Response
     (Self     : in out Client_Response_Sender;
      Response : LSP.Messages.Client_Responses.WorkspaceFolders_Response)
   is abstract;

   procedure On_WorkDoneProgressCreate_Response
     (Self     : in out Client_Response_Sender;
      Response : LSP.Messages.Client_Responses.WorkDoneProgressCreate_Response)
   is abstract;

   procedure On_RegisterCapability_Response
     (Self     : in out Client_Response_Sender;
      Response : LSP.Messages.Client_Responses.RegisterCapability_Response)
   is abstract;

   procedure On_UnregisterCapability_Response
     (Self     : in out Client_Response_Sender;
      Response : LSP.Messages.Client_Responses.UnregisterCapability_Response)
   is abstract;

end LSP.Client_Response_Senders;
