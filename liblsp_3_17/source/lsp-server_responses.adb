--
--  Copyright (C) 2022-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

package body LSP.Server_Responses is

   overriding procedure Visit_Server_Message_Visitor
     (Self    : Server_Response;
      Visitor : in out LSP.Server_Message_Visitors
                   .Server_Message_Visitor'Class) is
   begin
      Visitor.On_Server_Response (Self);
   end Visit_Server_Message_Visitor;

   overriding procedure Visit_Server_Receiver
     (Self    : Server_Error_Response;
      Visitor : in out LSP.Server_Response_Receivers
        .Server_Response_Receiver'Class) is
   begin
      Visitor.On_Error_Response (Self.Id, Self.Error);
   end Visit_Server_Receiver;

end LSP.Server_Responses;
