--
--  Copyright (C) <YEAR>, <COPYRIGHT HOLDER>
--
--  SPDX-License-Identifier: MIT
--
--  DON'T EDIT THIS FILE! It was generated from metaModel.json.
--

package body LSP.Progress_Reports.ProgressReport is

   overriding procedure Visit_Receiver
     (Self  : Work_Done;
      Value : in out LSP.Progress_Report_Receivers.Progress_Report_Receiver'
        Class) is
   begin
      Value.On_ProgressReport_Work_Done (Self.Token, Self.Params);
   end Visit_Receiver;

end LSP.Progress_Reports.ProgressReport;
