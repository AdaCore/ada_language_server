--
--  Copyright (C) <YEAR>, <COPYRIGHT HOLDER>
--
--  SPDX-License-Identifier: MIT
--

with LSP.Output_Tools;
with LSP.Outputs;

package body LSP.Base_Notification_Writers is

   overriding procedure On_CancelRequest_Notification
     (Self  : in out Base_Notification_Writer;
      Value : LSP.Structures.CancelParams) is
   begin
      LSP.Output_Tools.Write_Start_Notification
        (Self.Output.all, "$/cancelRequest");
      Self.Output.Key_Name ("params");
      LSP.Outputs.Write_CancelParams (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_CancelRequest_Notification;

   overriding procedure On_Progress_Notification
     (Self  : in out Base_Notification_Writer;
      Value : LSP.Structures.ProgressParams) is
   begin
      LSP.Output_Tools.Write_Start_Notification
        (Self.Output.all, "$/progress");
      Self.Output.Key_Name ("params");
      LSP.Outputs.Write_ProgressParams (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_Progress_Notification;

end LSP.Base_Notification_Writers;
