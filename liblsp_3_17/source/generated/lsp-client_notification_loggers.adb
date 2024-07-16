--
--  Copyright (C) <YEAR>, <COPYRIGHT HOLDER>
--
--  SPDX-License-Identifier: MIT
--
--  DON'T EDIT THIS FILE! It was generated from metaModel.json.
--

with VSS.Strings;

package body LSP.Client_Notification_Loggers is

   overriding procedure On_LogTrace_Notification
     (Self  : in out Client_Notification_Logger;
      Value : LSP.Structures.LogTraceParams) is
      Ok : Boolean := True;
   begin
      Self.Output.Put ("'$/logTrace'", Ok);
      Self.Output.Put (" Params : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_LogTrace_Notification;

   overriding procedure On_Event_Notification
     (Self  : in out Client_Notification_Logger;
      Value : LSP.Structures.LSPAny) is
      Ok : Boolean := True;
   begin
      Self.Output.Put ("'telemetry/event'", Ok);
      Self.Output.Put (" Params : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_Event_Notification;

   overriding procedure On_PublishDiagnostics_Notification
     (Self  : in out Client_Notification_Logger;
      Value : LSP.Structures.PublishDiagnosticsParams) is
      Ok : Boolean := True;
   begin
      Self.Output.Put ("'textDocument/publishDiagnostics'", Ok);
      Self.Output.Put (" Params : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_PublishDiagnostics_Notification;

   overriding procedure On_LogMessage_Notification
     (Self  : in out Client_Notification_Logger;
      Value : LSP.Structures.LogMessageParams) is
      Ok : Boolean := True;
   begin
      Self.Output.Put ("'window/logMessage'", Ok);
      Self.Output.Put (" Params : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_LogMessage_Notification;

   overriding procedure On_ShowMessage_Notification
     (Self  : in out Client_Notification_Logger;
      Value : LSP.Structures.ShowMessageParams) is
      Ok : Boolean := True;
   begin
      Self.Output.Put ("'window/showMessage'", Ok);
      Self.Output.Put (" Params : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_ShowMessage_Notification;

end LSP.Client_Notification_Loggers;
