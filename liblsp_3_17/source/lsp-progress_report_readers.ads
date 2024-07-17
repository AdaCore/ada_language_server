--
--  Copyright (C) 2022-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with VSS.JSON.Pull_Readers;
with VSS.Strings;

with LSP.Progress_Reports;

package LSP.Progress_Report_Readers is

   procedure Initialize;

   function Read_Progress_Report
     (Input  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Method : VSS.Strings.Virtual_String)
      return LSP.Progress_Reports.Progress_Report'Class;

end LSP.Progress_Report_Readers;
