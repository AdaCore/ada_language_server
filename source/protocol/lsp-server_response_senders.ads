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

   procedure On_Hover_Response
     (Self     : in out Server_Response_Sender;
      Response : LSP.Messages.Server_Responses.Hover_Response) is abstract;

   procedure On_SignatureHelp_Response
     (Self     : in out Server_Response_Sender;
      Response : LSP.Messages.Server_Responses.SignatureHelp_Response)
       is abstract;

   procedure On_Highlight_Response
     (Self     : in out Server_Response_Sender;
      Response : LSP.Messages.Server_Responses.Highlight_Response) is abstract;

   procedure On_Symbol_Response
     (Self     : in out Server_Response_Sender;
      Response : LSP.Messages.Server_Responses.Symbol_Response) is abstract;

   procedure On_Rename_Response
     (Self     : in out Server_Response_Sender;
      Response : LSP.Messages.Server_Responses.Rename_Response) is abstract;

   procedure On_CodeAction_Response
     (Self     : in out Server_Response_Sender;
      Response : LSP.Messages.Server_Responses.CodeAction_Response)
       is abstract;

   procedure On_Location_Response
     (Self     : in out Server_Response_Sender;
      Response : LSP.Messages.Server_Responses.Location_Response) is abstract;

   procedure On_ALS_Called_By_Response
     (Self     : in out Server_Response_Sender;
      Response : LSP.Messages.Server_Responses.ALS_Called_By_Response)
       is abstract;

   procedure On_ExecuteCommand_Response
     (Self     : in out Server_Response_Sender;
      Response : LSP.Messages.Server_Responses.ExecuteCommand_Response)
        is abstract;

   procedure On_Shutdown_Response
     (Self     : in out Server_Response_Sender;
      Response : LSP.Messages.Server_Responses.Shutdown_Response) is abstract;

end LSP.Server_Response_Senders;
