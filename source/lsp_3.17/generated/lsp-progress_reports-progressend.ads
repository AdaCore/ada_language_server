--
--  Copyright (C) <YEAR>, <COPYRIGHT HOLDER>
--
--  SPDX-License-Identifier: MIT
--
--  DON'T EDIT THIS FILE! It was generated from metaModel.json.
--

with LSP.Structures;

package LSP.Progress_Reports.ProgressEnd is
   pragma Preelaborate;

   type Work_Done is new LSP.Progress_Reports.Progress_Report with record
      Params : LSP.Structures.WorkDoneProgressEnd;
   end record;

   overriding procedure Visit_Receiver
     (Self  : Work_Done;
      Value : in out LSP.Progress_Report_Receivers.Progress_Report_Receiver'
        Class);

end LSP.Progress_Reports.ProgressEnd;
