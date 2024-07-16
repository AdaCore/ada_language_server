------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2019, AdaCore                          --
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
--  Interface to process notifications sent to the client.

with LSP.Types;
with LSP.Messages;

package LSP.Client_Notification_Receivers is

   type Progress_Value_Kind is (ProgressParams, SymbolInformation);

   type Client_Notification_Receiver is limited interface;
   --  Receiver of notification on LSP client side

   procedure On_Show_Message
     (Self   : access Client_Notification_Receiver;
      Params : LSP.Messages.ShowMessageParams) is abstract;
   --  Process window/showMessage notification

   procedure On_Log_Message
     (Self    : access Client_Notification_Receiver;
      Params : LSP.Messages.LogMessageParams) is abstract;
   --  Process window/logMessage notification

   procedure On_Publish_Diagnostics
     (Self   : access Client_Notification_Receiver;
      Params : LSP.Messages.PublishDiagnosticsParams) is abstract;
   --  Process textDocument/publishDiagnostics notification

   function Get_Progress_Type
     (Self  : access Client_Notification_Receiver;
      Token : LSP.Types.LSP_Number_Or_String)
      return Progress_Value_Kind is abstract;

   procedure On_Progress
     (Self   : access Client_Notification_Receiver;
      Params : LSP.Messages.Progress_Params) is abstract;
   --  Process a $/progress notification

   procedure On_Progress_SymbolInformation_Vector
     (Self   : access Client_Notification_Receiver;
      Params : LSP.Messages.Progress_SymbolInformation_Vector) is abstract;
   --  Process a $/progress notification that contains SymbolInformation_Vector

end LSP.Client_Notification_Receivers;
