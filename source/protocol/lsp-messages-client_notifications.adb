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

package body LSP.Messages.Client_Notifications is

   -----------
   -- Visit --
   -----------

   overriding procedure Visit
     (Self    : LogMessage_Notification;
      Reciver : access Client_Notification_Receiver'Class)
   is
   begin
      Reciver.On_Log_Message (Self.params);
   end Visit;

   -----------
   -- Visit --
   -----------

   overriding procedure Visit
     (Self    : ShowMessage_Notification;
      Reciver : access Client_Notification_Receiver'Class)
   is
   begin
      Reciver.On_Show_Message (Self.params);
   end Visit;

   -----------
   -- Visit --
   -----------

   overriding procedure Visit
     (Self    : PublishDiagnostics_Notification;
      Reciver : access Client_Notification_Receiver'Class)
   is
   begin
      Reciver.On_Publish_Diagnostics (Self.params);
   end Visit;

   -----------
   -- Visit --
   -----------

   overriding procedure Visit
     (Self     : Progress_Notification;
      Receiver : access Client_Notification_Receiver'Class)
   is
   begin
      Receiver.On_Progress (Self.params);
   end Visit;

   -----------
   -- Visit --
   -----------

   overriding procedure Visit
     (Self     : SymbolInformation_Vectors_Notification;
      Receiver : access Client_Notification_Receiver'Class) is
   begin
      Receiver.On_Progress_SymbolInformation_Vector (Self.params);
   end Visit;

end LSP.Messages.Client_Notifications;
