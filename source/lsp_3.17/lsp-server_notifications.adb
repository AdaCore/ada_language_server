--
--  Copyright (C) 2022-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

package body LSP.Server_Notifications is

   overriding procedure Visit_Server_Message_Visitor
     (Self    : Server_Notification;
      Visitor : in out LSP.Server_Message_Visitors
      .Server_Message_Visitor'Class) is
   begin
      Visitor.On_Server_Notification (Self);
   end Visit_Server_Message_Visitor;

end LSP.Server_Notifications;
