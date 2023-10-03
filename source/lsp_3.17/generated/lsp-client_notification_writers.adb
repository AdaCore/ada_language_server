--
--  Copyright (C) <YEAR>, <COPYRIGHT HOLDER>
--
--  SPDX-License-Identifier: MIT
--
--  DON'T EDIT THIS FILE! It was generated from metaModel.json.
--

with VSS.JSON.Content_Handlers;
with LSP.Output_Tools;
with LSP.Outputs;

package body LSP.Client_Notification_Writers is

   overriding procedure On_LogTrace_Notification
     (Self  : in out Client_Notification_Writer;
      Value : LSP.Structures.LogTraceParams) is
   begin
      LSP.Output_Tools.Write_Start_Notification
        (Self.Output.all, "$/logTrace");
      Self.Output.Key_Name ("params");
      LSP.Outputs.Write_LogTraceParams (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_LogTrace_Notification;

   overriding procedure On_Event_Notification
     (Self  : in out Client_Notification_Writer;
      Value : LSP.Structures.LSPAny) is
   begin
      LSP.Output_Tools.Write_Start_Notification
        (Self.Output.all, "telemetry/event");
      Self.Output.Key_Name ("params");
      LSP.Output_Tools.Write_LSPAny (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_Event_Notification;

   overriding procedure On_PublishDiagnostics_Notification
     (Self  : in out Client_Notification_Writer;
      Value : LSP.Structures.PublishDiagnosticsParams) is
   begin
      LSP.Output_Tools.Write_Start_Notification
        (Self.Output.all, "textDocument/publishDiagnostics");
      Self.Output.Key_Name ("params");
      LSP.Outputs.Write_PublishDiagnosticsParams (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_PublishDiagnostics_Notification;

   overriding procedure On_LogMessage_Notification
     (Self  : in out Client_Notification_Writer;
      Value : LSP.Structures.LogMessageParams) is
   begin
      LSP.Output_Tools.Write_Start_Notification
        (Self.Output.all, "window/logMessage");
      Self.Output.Key_Name ("params");
      LSP.Outputs.Write_LogMessageParams (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_LogMessage_Notification;

   overriding procedure On_ShowMessage_Notification
     (Self  : in out Client_Notification_Writer;
      Value : LSP.Structures.ShowMessageParams) is
   begin
      LSP.Output_Tools.Write_Start_Notification
        (Self.Output.all, "window/showMessage");
      Self.Output.Key_Name ("params");
      LSP.Outputs.Write_ShowMessageParams (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_ShowMessage_Notification;

end LSP.Client_Notification_Writers;
