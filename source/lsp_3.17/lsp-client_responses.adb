--
--  Copyright (C) 2022-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

package body LSP.Client_Responses is

   ----------------------------------
   -- Visit_Client_Message_Visitor --
   ----------------------------------

   overriding procedure Visit_Client_Message_Visitor
     (Self     : Client_Response;
      Visitor : in out LSP.Client_Message_Visitors
                   .Client_Message_Visitor'Class) is
   begin
      Visitor.On_Client_Response (Self);
   end Visit_Client_Message_Visitor;

   ---------------------------
   -- Visit_Client_Receiver --
   ---------------------------

   overriding procedure Visit_Client_Receiver
     (Self    : Client_Error_Response;
      Visitor : in out LSP.Client_Response_Receivers
      .Client_Response_Receiver'Class) is
   begin
      Visitor.On_Error_Response (Self.Id, Self.Error);
   end Visit_Client_Receiver;

end LSP.Client_Responses;
