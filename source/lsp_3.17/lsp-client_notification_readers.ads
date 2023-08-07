--
--  Copyright (C) 2022-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with VSS.JSON.Pull_Readers;
with VSS.Strings;
with LSP.Client_Notifications;

package LSP.Client_Notification_Readers is

   function Read_Notification
     (Input  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Method : VSS.Strings.Virtual_String)
      return LSP.Client_Notifications.Client_Notification'Class;

   procedure Initialize;

end LSP.Client_Notification_Readers;
