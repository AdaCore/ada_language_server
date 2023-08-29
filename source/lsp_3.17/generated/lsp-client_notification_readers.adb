--
--  Copyright (C) <YEAR>, <COPYRIGHT HOLDER>
--
--  SPDX-License-Identifier: MIT
--
--  DON'T EDIT THIS FILE! It was generated from metaModel.json.
--

with Minimal_Perfect_Hash;
with LSP.Inputs;
with LSP.Input_Tools;
with LSP.Structures;

with LSP.Client_Notifications.CancelRequest;
with LSP.Client_Notifications.LogTrace;
with LSP.Client_Notifications.Progress;
with LSP.Client_Notifications.Event;
with LSP.Client_Notifications.PublishDiagnostics;
with LSP.Client_Notifications.LogMessage;
with LSP.Client_Notifications.ShowMessage;

package body LSP.Client_Notification_Readers is

   package Method_Map is new Minimal_Perfect_Hash
     (["$/cancelRequest",
      "$/logTrace",
      "$/progress",
      "telemetry/event",
      "textDocument/publishDiagnostics",
      "window/logMessage",
      "window/showMessage"]);

   procedure Initialize is
   begin
      Method_Map.Initialize;
   end Initialize;

   procedure Read_CancelRequest is new LSP.Input_Tools.Read_Notification
     (LSP.Structures.CancelParams, "$/cancelRequest",
      LSP.Inputs.Read_CancelParams);

   procedure Read_LogTrace is new LSP.Input_Tools.Read_Notification
     (LSP.Structures.LogTraceParams, "$/logTrace",
      LSP.Inputs.Read_LogTraceParams);

   procedure Read_Progress is new LSP.Input_Tools.Read_Notification
     (LSP.Structures.ProgressParams, "$/progress",
      LSP.Inputs.Read_ProgressParams);

   procedure Read_Event is new LSP.Input_Tools.Read_Notification
     (LSP.Structures.LSPAny, "telemetry/event", LSP.Input_Tools.Read_LSPAny);

   procedure Read_PublishDiagnostics is new LSP.Input_Tools.Read_Notification
     (LSP.Structures.PublishDiagnosticsParams,
      "textDocument/publishDiagnostics",
      LSP.Inputs.Read_PublishDiagnosticsParams);

   procedure Read_LogMessage is new LSP.Input_Tools.Read_Notification
     (LSP.Structures.LogMessageParams, "window/logMessage",
      LSP.Inputs.Read_LogMessageParams);

   procedure Read_ShowMessage is new LSP.Input_Tools.Read_Notification
     (LSP.Structures.ShowMessageParams, "window/showMessage",
      LSP.Inputs.Read_ShowMessageParams);

   function Read_Notification
     (Input  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Method : VSS.Strings.Virtual_String)
      return LSP.Client_Notifications.Client_Notification'Class is
      Index : constant Natural := Method_Map.Get_Index (Method);
   begin
      case Index is
         when 1 =>  --  $/cancelRequest
            return
              Result : LSP.Client_Notifications.CancelRequest.Notification do
               Read_CancelRequest (Input, Result.Params);
            end return;

         when 2 =>  --  $/logTrace
            return Result : LSP.Client_Notifications.LogTrace.Notification do
               Read_LogTrace (Input, Result.Params);
            end return;

         when 3 =>  --  $/progress
            return Result : LSP.Client_Notifications.Progress.Notification do
               Read_Progress (Input, Result.Params);
            end return;

         when 4 =>  --  telemetry/event
            return Result : LSP.Client_Notifications.Event.Notification do
               Read_Event (Input, Result.Params);
            end return;

         when 5 =>  --  textDocument/publishDiagnostics
            return
              Result : LSP.Client_Notifications.PublishDiagnostics.Notification
            do
               Read_PublishDiagnostics (Input, Result.Params);
            end return;

         when 6 =>  --  window/logMessage
            return Result : LSP.Client_Notifications.LogMessage.Notification do
               Read_LogMessage (Input, Result.Params);
            end return;

         when 7 =>  --  window/showMessage
            return
              Result : LSP.Client_Notifications.ShowMessage.Notification do
               Read_ShowMessage (Input, Result.Params);
            end return;

         when others =>
            return raise Program_Error with "Unknown method";
      end case;
   end Read_Notification;
end LSP.Client_Notification_Readers;
