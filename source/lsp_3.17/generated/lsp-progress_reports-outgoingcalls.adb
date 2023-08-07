--
--  Copyright (C) <YEAR>, <COPYRIGHT HOLDER>
--
--  SPDX-License-Identifier: MIT
--

package body LSP.Progress_Reports.OutgoingCalls is

   overriding procedure Visit_Receiver
     (Self  : Partial_Result;
      Value : in out LSP.Progress_Report_Receivers.Progress_Report_Receiver'
        Class) is
   begin
      Value.On_OutgoingCalls_Partial_Result (Self.Token, Self.Params);
   end Visit_Receiver;

end LSP.Progress_Reports.OutgoingCalls;
