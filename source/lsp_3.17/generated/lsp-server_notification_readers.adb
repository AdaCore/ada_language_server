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

with LSP.Server_Notifications.CancelRequest;
with LSP.Server_Notifications.Progress;
with LSP.Server_Notifications.SetTrace;
with LSP.Server_Notifications.Exits;
with LSP.Server_Notifications.Initialized;
with LSP.Server_Notifications.DidChangeNotebook;
with LSP.Server_Notifications.DidCloseNotebook;
with LSP.Server_Notifications.DidOpenNotebook;
with LSP.Server_Notifications.DidSaveNotebook;
with LSP.Server_Notifications.DidChange;
with LSP.Server_Notifications.DidClose;
with LSP.Server_Notifications.DidOpen;
with LSP.Server_Notifications.DidSave;
with LSP.Server_Notifications.WillSave;
with LSP.Server_Notifications.Cancel;
with LSP.Server_Notifications.DidChangeConfiguration;
with LSP.Server_Notifications.DidChangeWatchedFiles;
with LSP.Server_Notifications.DidChangeWorkspaceFolders;
with LSP.Server_Notifications.DidCreateFiles;
with LSP.Server_Notifications.DidDeleteFiles;
with LSP.Server_Notifications.DidRenameFiles;

package body LSP.Server_Notification_Readers is

   package Method_Map is new Minimal_Perfect_Hash
     (["$/cancelRequest",
      "$/progress",
      "$/setTrace",
      "exit",
      "initialized",
      "notebookDocument/didChange",
      "notebookDocument/didClose",
      "notebookDocument/didOpen",
      "notebookDocument/didSave",
      "textDocument/didChange",
      "textDocument/didClose",
      "textDocument/didOpen",
      "textDocument/didSave",
      "textDocument/willSave",
      "window/workDoneProgress/cancel",
      "workspace/didChangeConfiguration",
      "workspace/didChangeWatchedFiles",
      "workspace/didChangeWorkspaceFolders",
      "workspace/didCreateFiles",
      "workspace/didDeleteFiles",
      "workspace/didRenameFiles"]);

   procedure Initialize is
   begin
      Method_Map.Initialize;
   end Initialize;

   procedure Read_CancelRequest is new LSP.Input_Tools.Read_Notification
     (LSP.Structures.CancelParams, "$/cancelRequest",
      LSP.Inputs.Read_CancelParams);

   procedure Read_Progress is new LSP.Input_Tools.Read_Notification
     (LSP.Structures.ProgressParams, "$/progress",
      LSP.Inputs.Read_ProgressParams);

   procedure Read_SetTrace is new LSP.Input_Tools.Read_Notification
     (LSP.Structures.SetTraceParams, "$/setTrace",
      LSP.Inputs.Read_SetTraceParams);

   procedure Read_Exits
     (Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Method  : VSS.Strings.Virtual_String := "exit") is null;

   procedure Read_Initialized is new LSP.Input_Tools.Read_Notification
     (LSP.Structures.InitializedParams, "initialized",
      LSP.Inputs.Read_InitializedParams);

   procedure Read_DidChangeNotebook is new LSP.Input_Tools.Read_Notification
     (LSP.Structures.DidChangeNotebookDocumentParams,
      "notebookDocument/didChange",
      LSP.Inputs.Read_DidChangeNotebookDocumentParams);

   procedure Read_DidCloseNotebook is new LSP.Input_Tools.Read_Notification
     (LSP.Structures.DidCloseNotebookDocumentParams,
      "notebookDocument/didClose",
      LSP.Inputs.Read_DidCloseNotebookDocumentParams);

   procedure Read_DidOpenNotebook is new LSP.Input_Tools.Read_Notification
     (LSP.Structures.DidOpenNotebookDocumentParams, "notebookDocument/didOpen",
      LSP.Inputs.Read_DidOpenNotebookDocumentParams);

   procedure Read_DidSaveNotebook is new LSP.Input_Tools.Read_Notification
     (LSP.Structures.DidSaveNotebookDocumentParams, "notebookDocument/didSave",
      LSP.Inputs.Read_DidSaveNotebookDocumentParams);

   procedure Read_DidChange is new LSP.Input_Tools.Read_Notification
     (LSP.Structures.DidChangeTextDocumentParams, "textDocument/didChange",
      LSP.Inputs.Read_DidChangeTextDocumentParams);

   procedure Read_DidClose is new LSP.Input_Tools.Read_Notification
     (LSP.Structures.DidCloseTextDocumentParams, "textDocument/didClose",
      LSP.Inputs.Read_DidCloseTextDocumentParams);

   procedure Read_DidOpen is new LSP.Input_Tools.Read_Notification
     (LSP.Structures.DidOpenTextDocumentParams, "textDocument/didOpen",
      LSP.Inputs.Read_DidOpenTextDocumentParams);

   procedure Read_DidSave is new LSP.Input_Tools.Read_Notification
     (LSP.Structures.DidSaveTextDocumentParams, "textDocument/didSave",
      LSP.Inputs.Read_DidSaveTextDocumentParams);

   procedure Read_WillSave is new LSP.Input_Tools.Read_Notification
     (LSP.Structures.WillSaveTextDocumentParams, "textDocument/willSave",
      LSP.Inputs.Read_WillSaveTextDocumentParams);

   procedure Read_Cancel is new LSP.Input_Tools.Read_Notification
     (LSP.Structures.WorkDoneProgressCancelParams,
      "window/workDoneProgress/cancel",
      LSP.Inputs.Read_WorkDoneProgressCancelParams);

   procedure Read_DidChangeConfiguration is new LSP.Input_Tools
     .Read_Notification
     (LSP.Structures.DidChangeConfigurationParams,
      "workspace/didChangeConfiguration",
      LSP.Inputs.Read_DidChangeConfigurationParams);

   procedure Read_DidChangeWatchedFiles is new LSP.Input_Tools
     .Read_Notification
     (LSP.Structures.DidChangeWatchedFilesParams,
      "workspace/didChangeWatchedFiles",
      LSP.Inputs.Read_DidChangeWatchedFilesParams);

   procedure Read_DidChangeWorkspaceFolders is new LSP.Input_Tools
     .Read_Notification
     (LSP.Structures.DidChangeWorkspaceFoldersParams,
      "workspace/didChangeWorkspaceFolders",
      LSP.Inputs.Read_DidChangeWorkspaceFoldersParams);

   procedure Read_DidCreateFiles is new LSP.Input_Tools.Read_Notification
     (LSP.Structures.CreateFilesParams, "workspace/didCreateFiles",
      LSP.Inputs.Read_CreateFilesParams);

   procedure Read_DidDeleteFiles is new LSP.Input_Tools.Read_Notification
     (LSP.Structures.DeleteFilesParams, "workspace/didDeleteFiles",
      LSP.Inputs.Read_DeleteFilesParams);

   procedure Read_DidRenameFiles is new LSP.Input_Tools.Read_Notification
     (LSP.Structures.RenameFilesParams, "workspace/didRenameFiles",
      LSP.Inputs.Read_RenameFilesParams);

   function Read_Notification
     (Input  : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Method : VSS.Strings.Virtual_String)
      return LSP.Server_Notifications.Server_Notification'Class is
      Index : constant Natural := Method_Map.Get_Index (Method);
   begin
      case Index is
         when 1 =>  --  $/cancelRequest
            return
              Result : LSP.Server_Notifications.CancelRequest.Notification do
               Read_CancelRequest (Input, Result.Params);
            end return;

         when 2 =>  --  $/progress
            return Result : LSP.Server_Notifications.Progress.Notification do
               Read_Progress (Input, Result.Params);
            end return;

         when 3 =>  --  $/setTrace
            return Result : LSP.Server_Notifications.SetTrace.Notification do
               Read_SetTrace (Input, Result.Params);
            end return;

         when 4 =>  --  exit
            return Result : LSP.Server_Notifications.Exits.Notification do
               Read_Exits (Input);
            end return;

         when 5 =>  --  initialized
            return
              Result : LSP.Server_Notifications.Initialized.Notification do
               Read_Initialized (Input, Result.Params);
            end return;

         when 6 =>  --  notebookDocument/didChange
            return
              Result : LSP.Server_Notifications.DidChangeNotebook.Notification
            do
               Read_DidChangeNotebook (Input, Result.Params);
            end return;

         when 7 =>  --  notebookDocument/didClose
            return
              Result : LSP.Server_Notifications.DidCloseNotebook.Notification
            do
               Read_DidCloseNotebook (Input, Result.Params);
            end return;

         when 8 =>  --  notebookDocument/didOpen
            return
              Result : LSP.Server_Notifications.DidOpenNotebook.Notification do
               Read_DidOpenNotebook (Input, Result.Params);
            end return;

         when 9 =>  --  notebookDocument/didSave
            return
              Result : LSP.Server_Notifications.DidSaveNotebook.Notification do
               Read_DidSaveNotebook (Input, Result.Params);
            end return;

         when 10 =>  --  textDocument/didChange
            return Result : LSP.Server_Notifications.DidChange.Notification do
               Read_DidChange (Input, Result.Params);
            end return;

         when 11 =>  --  textDocument/didClose
            return Result : LSP.Server_Notifications.DidClose.Notification do
               Read_DidClose (Input, Result.Params);
            end return;

         when 12 =>  --  textDocument/didOpen
            return Result : LSP.Server_Notifications.DidOpen.Notification do
               Read_DidOpen (Input, Result.Params);
            end return;

         when 13 =>  --  textDocument/didSave
            return Result : LSP.Server_Notifications.DidSave.Notification do
               Read_DidSave (Input, Result.Params);
            end return;

         when 14 =>  --  textDocument/willSave
            return Result : LSP.Server_Notifications.WillSave.Notification do
               Read_WillSave (Input, Result.Params);
            end return;

         when 15 =>  --  window/workDoneProgress/cancel
            return Result : LSP.Server_Notifications.Cancel.Notification do
               Read_Cancel (Input, Result.Params);
            end return;

         when 16 =>  --  workspace/didChangeConfiguration
            return
              Result : LSP.Server_Notifications.DidChangeConfiguration
                .Notification
            do
               Read_DidChangeConfiguration (Input, Result.Params);
            end return;

         when 17 =>  --  workspace/didChangeWatchedFiles
            return
              Result : LSP.Server_Notifications.DidChangeWatchedFiles
                .Notification
            do
               Read_DidChangeWatchedFiles (Input, Result.Params);
            end return;

         when 18 =>  --  workspace/didChangeWorkspaceFolders
            return
              Result : LSP.Server_Notifications.DidChangeWorkspaceFolders
                .Notification
            do
               Read_DidChangeWorkspaceFolders (Input, Result.Params);
            end return;

         when 19 =>  --  workspace/didCreateFiles
            return
              Result : LSP.Server_Notifications.DidCreateFiles.Notification do
               Read_DidCreateFiles (Input, Result.Params);
            end return;

         when 20 =>  --  workspace/didDeleteFiles
            return
              Result : LSP.Server_Notifications.DidDeleteFiles.Notification do
               Read_DidDeleteFiles (Input, Result.Params);
            end return;

         when 21 =>  --  workspace/didRenameFiles
            return
              Result : LSP.Server_Notifications.DidRenameFiles.Notification do
               Read_DidRenameFiles (Input, Result.Params);
            end return;

         when others =>
            return raise Program_Error with "Unknown method";
      end case;
   end Read_Notification;
end LSP.Server_Notification_Readers;
