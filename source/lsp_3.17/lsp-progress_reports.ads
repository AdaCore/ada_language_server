--
--  Copyright (C) 2022-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with LSP.Client_Message_Visitors;
with LSP.Client_Messages;
with LSP.Progress_Report_Receivers;
with LSP.Structures;

package LSP.Progress_Reports is
   pragma Preelaborate;

   type Progress_Report is abstract limited
     new LSP.Client_Messages.Client_Message with record
      Token : LSP.Structures.ProgressToken;
   end record;

   procedure Visit_Receiver
     (Self  : Progress_Report;
      Value : in out LSP.Progress_Report_Receivers
        .Progress_Report_Receiver'Class) is abstract;

   overriding procedure Visit_Client_Message_Visitor
     (Self    : Progress_Report;
      Visitor : in out LSP.Client_Message_Visitors
                         .Client_Message_Visitor'Class);

end LSP.Progress_Reports;
