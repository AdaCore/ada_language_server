--
--  Copyright (C) <YEAR>, <COPYRIGHT HOLDER>
--
--  SPDX-License-Identifier: MIT
--

with Minimal_Perfect_Hash;
with LSP.Inputs;
with LSP.Input_Tools;
with LSP.Structures;

with LSP.Client_Requests.RegisterCapability;
with LSP.Client_Requests.UnregisterCapability;
with LSP.Client_Requests.ShowDocument;
with LSP.Client_Requests.ShowMessageRequest;
with LSP.Client_Requests.Progress_Create;
with LSP.Client_Requests.ApplyEdit;
with LSP.Client_Requests.Code_Lens_Refresh;
with LSP.Client_Requests.Configuration;
with LSP.Client_Requests.Diagnostic_Refresh;
with LSP.Client_Requests.Inlay_Refresh;
with LSP.Client_Requests.Inline_Refresh;
with LSP.Client_Requests.Tokens_Refresh;
with LSP.Client_Requests.WorkspaceFolders;

package body LSP.Client_Request_Readers is

   package Method_Map is new Minimal_Perfect_Hash
     (["client/registerCapability",
       "client/unregisterCapability",
       "window/showDocument",
       "window/showMessageRequest",
       "window/workDoneProgress/create",
       "workspace/applyEdit",
       "workspace/codeLens/refresh",
       "workspace/configuration",
       "workspace/diagnostic/refresh",
       "workspace/inlayHint/refresh",
       "workspace/inlineValue/refresh",
       "workspace/semanticTokens/refresh",
       "workspace/workspaceFolders"]);

   procedure Initialize is
   begin
      Method_Map.Initialize;
   end Initialize;

   procedure Read_RegisterCapability is new LSP.Input_Tools.Read_Request
     (LSP.Structures.RegistrationParams, "client/registerCapability",
      LSP.Inputs.Read_RegistrationParams);

   procedure Read_UnregisterCapability is new LSP.Input_Tools.Read_Request
     (LSP.Structures.UnregistrationParams, "client/unregisterCapability",
      LSP.Inputs.Read_UnregistrationParams);

   procedure Read_ShowDocument is new LSP.Input_Tools.Read_Request
     (LSP.Structures.ShowDocumentParams, "window/showDocument",
      LSP.Inputs.Read_ShowDocumentParams);

   procedure Read_ShowMessageRequest is new LSP.Input_Tools.Read_Request
     (LSP.Structures.ShowMessageRequestParams, "window/showMessageRequest",
      LSP.Inputs.Read_ShowMessageRequestParams);

   procedure Read_Progress_Create is new LSP.Input_Tools.Read_Request
     (LSP.Structures.WorkDoneProgressCreateParams,
      "window/workDoneProgress/create",
      LSP.Inputs.Read_WorkDoneProgressCreateParams);

   procedure Read_ApplyEdit is new LSP.Input_Tools.Read_Request
     (LSP.Structures.ApplyWorkspaceEditParams, "workspace/applyEdit",
      LSP.Inputs.Read_ApplyWorkspaceEditParams);

   procedure Read_Code_Lens_Refresh
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Id      : in out LSP.Structures.Integer_Or_Virtual_String);

   procedure Read_Code_Lens_Refresh
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Id      : in out LSP.Structures.Integer_Or_Virtual_String) is
   begin
      LSP.Input_Tools.Read_Null_Request
        (Handler, "workspace/codeLens/refresh", Id);
   end Read_Code_Lens_Refresh;

   procedure Read_Configuration is new LSP.Input_Tools.Read_Request
     (LSP.Structures.ConfigurationParams, "workspace/configuration",
      LSP.Inputs.Read_ConfigurationParams);

   procedure Read_Diagnostic_Refresh
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Id      : in out LSP.Structures.Integer_Or_Virtual_String);

   procedure Read_Diagnostic_Refresh
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Id      : in out LSP.Structures.Integer_Or_Virtual_String) is
   begin
      LSP.Input_Tools.Read_Null_Request
        (Handler, "workspace/diagnostic/refresh", Id);
   end Read_Diagnostic_Refresh;

   procedure Read_Inlay_Refresh
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Id      : in out LSP.Structures.Integer_Or_Virtual_String);

   procedure Read_Inlay_Refresh
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Id      : in out LSP.Structures.Integer_Or_Virtual_String) is
   begin
      LSP.Input_Tools.Read_Null_Request
        (Handler, "workspace/inlayHint/refresh", Id);
   end Read_Inlay_Refresh;

   procedure Read_Inline_Refresh
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Id      : in out LSP.Structures.Integer_Or_Virtual_String);

   procedure Read_Inline_Refresh
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Id      : in out LSP.Structures.Integer_Or_Virtual_String) is
   begin
      LSP.Input_Tools.Read_Null_Request
        (Handler, "workspace/inlineValue/refresh", Id);
   end Read_Inline_Refresh;

   procedure Read_Tokens_Refresh
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Id      : in out LSP.Structures.Integer_Or_Virtual_String);

   procedure Read_Tokens_Refresh
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Id      : in out LSP.Structures.Integer_Or_Virtual_String) is
   begin
      LSP.Input_Tools.Read_Null_Request
        (Handler, "workspace/semanticTokens/refresh", Id);
   end Read_Tokens_Refresh;

   procedure Read_WorkspaceFolders
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Id      : in out LSP.Structures.Integer_Or_Virtual_String);

   procedure Read_WorkspaceFolders
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Id      : in out LSP.Structures.Integer_Or_Virtual_String) is
   begin
      LSP.Input_Tools.Read_Null_Request
        (Handler, "workspace/workspaceFolders", Id);
   end Read_WorkspaceFolders;

   function Read_Request
     (Input  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Method : VSS.Strings.Virtual_String)
      return LSP.Client_Requests.Client_Request'Class is
      Index : constant Natural := Method_Map.Get_Index (Method);
   begin
      case Index is
         when 1 =>  --  client/registerCapability
            return Result : LSP.Client_Requests.RegisterCapability.Request do
               Read_RegisterCapability (Input, Result.Id, Result.Params);
            end return;

         when 2 =>  --  client/unregisterCapability
            return Result : LSP.Client_Requests.UnregisterCapability.Request do
               Read_UnregisterCapability (Input, Result.Id, Result.Params);
            end return;

         when 3 =>  --  window/showDocument
            return Result : LSP.Client_Requests.ShowDocument.Request do
               Read_ShowDocument (Input, Result.Id, Result.Params);
            end return;

         when 4 =>  --  window/showMessageRequest
            return Result : LSP.Client_Requests.ShowMessageRequest.Request do
               Read_ShowMessageRequest (Input, Result.Id, Result.Params);
            end return;

         when 5 =>  --  window/workDoneProgress/create
            return Result : LSP.Client_Requests.Progress_Create.Request do
               Read_Progress_Create (Input, Result.Id, Result.Params);
            end return;

         when 6 =>  --  workspace/applyEdit
            return Result : LSP.Client_Requests.ApplyEdit.Request do
               Read_ApplyEdit (Input, Result.Id, Result.Params);
            end return;

         when 7 =>  --  workspace/codeLens/refresh
            return Result : LSP.Client_Requests.Code_Lens_Refresh.Request do
               Read_Code_Lens_Refresh (Input, Result.Id);
            end return;

         when 8 =>  --  workspace/configuration
            return Result : LSP.Client_Requests.Configuration.Request do
               Read_Configuration (Input, Result.Id, Result.Params);
            end return;

         when 9 =>  --  workspace/diagnostic/refresh
            return Result : LSP.Client_Requests.Diagnostic_Refresh.Request do
               Read_Diagnostic_Refresh (Input, Result.Id);
            end return;

         when 10 =>  --  workspace/inlayHint/refresh
            return Result : LSP.Client_Requests.Inlay_Refresh.Request do
               Read_Inlay_Refresh (Input, Result.Id);
            end return;

         when 11 =>  --  workspace/inlineValue/refresh
            return Result : LSP.Client_Requests.Inline_Refresh.Request do
               Read_Inline_Refresh (Input, Result.Id);
            end return;

         when 12 =>  --  workspace/semanticTokens/refresh
            return Result : LSP.Client_Requests.Tokens_Refresh.Request do
               Read_Tokens_Refresh (Input, Result.Id);
            end return;

         when 13 =>  --  workspace/workspaceFolders
            return Result : LSP.Client_Requests.WorkspaceFolders.Request do
               Read_WorkspaceFolders (Input, Result.Id);
            end return;

         when others =>
            return raise Program_Error with "Unknown method";
      end case;
   end Read_Request;
end LSP.Client_Request_Readers;
