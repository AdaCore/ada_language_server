--
--  Copyright (C) 2022-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

package body LSP.Progress_Reports is

   ----------------------------------
   -- Visit_Client_Message_Visitor --
   ----------------------------------

   overriding procedure Visit_Client_Message_Visitor
     (Self    : Progress_Report;
      Visitor : in out LSP.Client_Message_Visitors.Client_Message_Visitor'
        Class) is
   begin
      Visitor.On_Progress_Report (Self);
   end Visit_Client_Message_Visitor;

end LSP.Progress_Reports;
