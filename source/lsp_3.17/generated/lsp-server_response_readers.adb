--
--  Copyright (C) <YEAR>, <COPYRIGHT HOLDER>
--
--  SPDX-License-Identifier: MIT
--

with Minimal_Perfect_Hash;
with LSP.Inputs;
with LSP.Input_Tools;
with LSP.Structures;

with LSP.Server_Responses.RegisterCapability;
with LSP.Server_Responses.UnregisterCapability;
with LSP.Server_Responses.ShowDocument;
with LSP.Server_Responses.ShowMessageRequest;
with LSP.Server_Responses.Progress_Create;
with LSP.Server_Responses.ApplyEdit;
with LSP.Server_Responses.Code_Lens_Refresh;
with LSP.Server_Responses.Configuration;
with LSP.Server_Responses.Diagnostic_Refresh;
with LSP.Server_Responses.Inlay_Refresh;
with LSP.Server_Responses.Inline_Refresh;
with LSP.Server_Responses.Tokens_Refresh;
with LSP.Server_Responses.WorkspaceFolders;

package body LSP.Server_Response_Readers is

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

   procedure Read_RegisterCapability is new LSP.Input_Tools.Read_Response
     (LSP.Structures.Null_Record, LSP.Inputs.Read_Null_Record);

   procedure Read_UnregisterCapability is new LSP.Input_Tools.Read_Response
     (LSP.Structures.Null_Record, LSP.Inputs.Read_Null_Record);

   procedure Read_ShowDocument is new LSP.Input_Tools.Read_Response
     (LSP.Structures.ShowDocumentResult, LSP.Inputs.Read_ShowDocumentResult);

   procedure Read_ShowMessageRequest is new LSP.Input_Tools.Read_Response
     (LSP.Structures.MessageActionItem_Or_Null,
      LSP.Inputs.Read_MessageActionItem_Or_Null);

   procedure Read_Progress_Create is new LSP.Input_Tools.Read_Response
     (LSP.Structures.Null_Record, LSP.Inputs.Read_Null_Record);

   procedure Read_ApplyEdit is new LSP.Input_Tools.Read_Response
     (LSP.Structures.ApplyWorkspaceEditResult,
      LSP.Inputs.Read_ApplyWorkspaceEditResult);

   procedure Read_Code_Lens_Refresh is new LSP.Input_Tools.Read_Response
     (LSP.Structures.Null_Record, LSP.Inputs.Read_Null_Record);

   procedure Read_Configuration is new LSP.Input_Tools.Read_Response
     (LSP.Structures.LSPAny_Vector, LSP.Inputs.Read_LSPAny_Vector);

   procedure Read_Diagnostic_Refresh is new LSP.Input_Tools.Read_Response
     (LSP.Structures.Null_Record, LSP.Inputs.Read_Null_Record);

   procedure Read_Inlay_Refresh is new LSP.Input_Tools.Read_Response
     (LSP.Structures.Null_Record, LSP.Inputs.Read_Null_Record);

   procedure Read_Inline_Refresh is new LSP.Input_Tools.Read_Response
     (LSP.Structures.Null_Record, LSP.Inputs.Read_Null_Record);

   procedure Read_Tokens_Refresh is new LSP.Input_Tools.Read_Response
     (LSP.Structures.Null_Record, LSP.Inputs.Read_Null_Record);

   procedure Read_WorkspaceFolders is new LSP.Input_Tools.Read_Response
     (LSP.Structures.WorkspaceFolder_Vector_Or_Null,
      LSP.Inputs.Read_WorkspaceFolder_Vector_Or_Null);

   function Read_Response
     (Input  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Method : VSS.Strings.Virtual_String)
      return LSP.Server_Responses.Server_Response'Class is
      Index : constant Natural := Method_Map.Get_Index (Method);
   begin
      case Index is
         when 1 =>  --  client/registerCapability
            return Result : LSP.Server_Responses.RegisterCapability.Response do
               Read_RegisterCapability (Input, Result.Id, Result.Result);
            end return;

         when 2 =>  --  client/unregisterCapability
            return
              Result : LSP.Server_Responses.UnregisterCapability.Response do
               Read_UnregisterCapability (Input, Result.Id, Result.Result);
            end return;

         when 3 =>  --  window/showDocument
            return Result : LSP.Server_Responses.ShowDocument.Response do
               Read_ShowDocument (Input, Result.Id, Result.Result);
            end return;

         when 4 =>  --  window/showMessageRequest
            return Result : LSP.Server_Responses.ShowMessageRequest.Response do
               Read_ShowMessageRequest (Input, Result.Id, Result.Result);
            end return;

         when 5 =>  --  window/workDoneProgress/create
            return Result : LSP.Server_Responses.Progress_Create.Response do
               Read_Progress_Create (Input, Result.Id, Result.Result);
            end return;

         when 6 =>  --  workspace/applyEdit
            return Result : LSP.Server_Responses.ApplyEdit.Response do
               Read_ApplyEdit (Input, Result.Id, Result.Result);
            end return;

         when 7 =>  --  workspace/codeLens/refresh
            return Result : LSP.Server_Responses.Code_Lens_Refresh.Response do
               Read_Code_Lens_Refresh (Input, Result.Id, Result.Result);
            end return;

         when 8 =>  --  workspace/configuration
            return Result : LSP.Server_Responses.Configuration.Response do
               Read_Configuration (Input, Result.Id, Result.Result);
            end return;

         when 9 =>  --  workspace/diagnostic/refresh
            return Result : LSP.Server_Responses.Diagnostic_Refresh.Response do
               Read_Diagnostic_Refresh (Input, Result.Id, Result.Result);
            end return;

         when 10 =>  --  workspace/inlayHint/refresh
            return Result : LSP.Server_Responses.Inlay_Refresh.Response do
               Read_Inlay_Refresh (Input, Result.Id, Result.Result);
            end return;

         when 11 =>  --  workspace/inlineValue/refresh
            return Result : LSP.Server_Responses.Inline_Refresh.Response do
               Read_Inline_Refresh (Input, Result.Id, Result.Result);
            end return;

         when 12 =>  --  workspace/semanticTokens/refresh
            return Result : LSP.Server_Responses.Tokens_Refresh.Response do
               Read_Tokens_Refresh (Input, Result.Id, Result.Result);
            end return;

         when 13 =>  --  workspace/workspaceFolders
            return Result : LSP.Server_Responses.WorkspaceFolders.Response do
               Read_WorkspaceFolders (Input, Result.Id, Result.Result);
            end return;

         when others =>
            return raise Program_Error with "Unknown method";
      end case;
   end Read_Response;
end LSP.Server_Response_Readers;
