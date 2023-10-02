--
--  Copyright (C) 2022-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

package body LSP.Client_Requests is

   ----------------------------------
   -- Visit_Client_Message_Visitor --
   ----------------------------------

   overriding procedure Visit_Client_Message_Visitor
     (Self    : Client_Request;
      Visitor : in out LSP.Client_Message_Visitors
                   .Client_Message_Visitor'Class) is
   begin
      Visitor.On_Client_Request (Self);
   end Visit_Client_Message_Visitor;

end LSP.Client_Requests;
