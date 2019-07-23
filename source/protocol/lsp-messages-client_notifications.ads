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

with LSP.Generic_Notifications;

package LSP.Messages.Client_Notifications is

   package LogMessages is
     new LSP.Generic_Notifications (LSP.Messages.LogMessageParams);

   type LogMessage_Notification is
     new LogMessages.Notification with null record;

   package ShowMessages is
     new LSP.Generic_Notifications (LSP.Messages.ShowMessageParams);

   type ShowMessage_Notification is
     new ShowMessages.Notification with null record;

   package PublishDiagnostics is
     new LSP.Generic_Notifications (LSP.Messages.PublishDiagnosticsParams);

   type PublishDiagnostics_Notification is
     new PublishDiagnostics.Notification with null record;

end LSP.Messages.Client_Notifications;
