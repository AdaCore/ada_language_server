--
--  Copyright (C) <YEAR>, <COPYRIGHT HOLDER>
--
--  SPDX-License-Identifier: MIT
--
--  DON'T EDIT THIS FILE! It was generated from metaModel.json.
--

with LSP.Structures;

package LSP.Progress_Reports.Definition is
   pragma Preelaborate;

   type Partial_Result is new LSP.Progress_Reports.Progress_Report with record
      Params : LSP.Structures.Definition_Progress_Report;
   end record;

   overriding procedure Visit_Receiver
     (Self  : Partial_Result;
      Value : in out LSP.Progress_Report_Receivers.Progress_Report_Receiver'
        Class);

end LSP.Progress_Reports.Definition;
