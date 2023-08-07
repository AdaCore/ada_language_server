--
--  Copyright (C) <YEAR>, <COPYRIGHT HOLDER>
--
--  SPDX-License-Identifier: MIT
--

package body LSP.Progress_Reports.ProgressBegin is

   overriding procedure Visit_Receiver
     (Self  : Work_Done;
      Value : in out LSP.Progress_Report_Receivers.Progress_Report_Receiver'
        Class) is
   begin
      Value.On_ProgressBegin_Work_Done (Self.Token, Self.Params);
   end Visit_Receiver;

end LSP.Progress_Reports.ProgressBegin;
