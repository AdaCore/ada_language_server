------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2018-2020, AdaCore                     --
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
--  Types for notifications sent to the client.

with LSP.Generic_Notifications;
with LSP.Client_Notification_Receivers;
use LSP.Client_Notification_Receivers;

package LSP.Messages.Client_Notifications is

   type Client_Notification is abstract new LSP.Messages.NotificationMessage
     with null record;

   procedure Visit
     (Self    : Client_Notification;
      Reciver : access Client_Notification_Receiver'Class) is abstract;

   package LogMessages is new LSP.Generic_Notifications
     (Client_Notification,
      LSP.Messages.LogMessageParams,
      Client_Notification_Receiver'Class);

   type LogMessage_Notification is
     new LogMessages.Notification with null record;

   overriding procedure Visit
     (Self    : LogMessage_Notification;
      Reciver : access Client_Notification_Receiver'Class);

   package ShowMessages is new LSP.Generic_Notifications
     (Client_Notification,
      LSP.Messages.ShowMessageParams,
      Client_Notification_Receiver'Class);

   type ShowMessage_Notification is
     new ShowMessages.Notification with null record;

   overriding procedure Visit
     (Self    : ShowMessage_Notification;
      Reciver : access Client_Notification_Receiver'Class);

   package PublishDiagnostics is new LSP.Generic_Notifications
     (Client_Notification,
      LSP.Messages.PublishDiagnosticsParams,
      Client_Notification_Receiver'Class);

   type PublishDiagnostics_Notification is
     new PublishDiagnostics.Notification with null record;

   overriding procedure Visit
     (Self    : PublishDiagnostics_Notification;
      Reciver : access Client_Notification_Receiver'Class);

   package Progress_Params is new LSP.Generic_Notifications
     (Client_Notification,
      LSP.Messages.Progress_Params,
      Client_Notification_Receiver'Class);

   type Progress_Notification is
     new Progress_Params.Notification with null record;

   overriding procedure Visit
     (Self     : Progress_Notification;
      Receiver : access Client_Notification_Receiver'Class);

   package SymbolInformation_Vectors is new LSP.Generic_Notifications
     (Client_Notification,
      LSP.Messages.Progress_SymbolInformation_Vector,
      Client_Notification_Receiver'Class);

   type SymbolInformation_Vectors_Notification is
     new SymbolInformation_Vectors.Notification with null record;

   overriding procedure Visit
     (Self     : SymbolInformation_Vectors_Notification;
      Receiver : access Client_Notification_Receiver'Class);

end LSP.Messages.Client_Notifications;
