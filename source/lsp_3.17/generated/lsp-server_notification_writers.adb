--
--  Copyright (C) <YEAR>, <COPYRIGHT HOLDER>
--
--  SPDX-License-Identifier: MIT
--

with VSS.JSON.Content_Handlers;
with LSP.Output_Tools;
with LSP.Outputs;

package body LSP.Server_Notification_Writers is

   overriding procedure On_SetTrace_Notification
     (Self  : in out Server_Notification_Writer;
      Value : LSP.Structures.SetTraceParams) is
   begin
      LSP.Output_Tools.Write_Start_Notification
        (Self.Output.all, "$/setTrace");
      Self.Output.Key_Name ("params");
      LSP.Outputs.Write_SetTraceParams (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_SetTrace_Notification;

   overriding procedure On_Exits_Notification
     (Self : in out Server_Notification_Writer) is
   begin
      LSP.Output_Tools.Write_Start_Notification (Self.Output.all, "exit");
      Self.Output.End_Object;
   end On_Exits_Notification;

   overriding procedure On_Initialized_Notification
     (Self  : in out Server_Notification_Writer;
      Value : LSP.Structures.InitializedParams) is
   begin
      LSP.Output_Tools.Write_Start_Notification
        (Self.Output.all, "initialized");
      Self.Output.Key_Name ("params");
      LSP.Outputs.Write_InitializedParams (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_Initialized_Notification;

   overriding procedure On_DidChangeNotebook_Notification
     (Self  : in out Server_Notification_Writer;
      Value : LSP.Structures.DidChangeNotebookDocumentParams) is
   begin
      LSP.Output_Tools.Write_Start_Notification
        (Self.Output.all, "notebookDocument/didChange");
      Self.Output.Key_Name ("params");
      LSP.Outputs.Write_DidChangeNotebookDocumentParams
        (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_DidChangeNotebook_Notification;

   overriding procedure On_DidCloseNotebook_Notification
     (Self  : in out Server_Notification_Writer;
      Value : LSP.Structures.DidCloseNotebookDocumentParams) is
   begin
      LSP.Output_Tools.Write_Start_Notification
        (Self.Output.all, "notebookDocument/didClose");
      Self.Output.Key_Name ("params");
      LSP.Outputs.Write_DidCloseNotebookDocumentParams
        (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_DidCloseNotebook_Notification;

   overriding procedure On_DidOpenNotebook_Notification
     (Self  : in out Server_Notification_Writer;
      Value : LSP.Structures.DidOpenNotebookDocumentParams) is
   begin
      LSP.Output_Tools.Write_Start_Notification
        (Self.Output.all, "notebookDocument/didOpen");
      Self.Output.Key_Name ("params");
      LSP.Outputs.Write_DidOpenNotebookDocumentParams (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_DidOpenNotebook_Notification;

   overriding procedure On_DidSaveNotebook_Notification
     (Self  : in out Server_Notification_Writer;
      Value : LSP.Structures.DidSaveNotebookDocumentParams) is
   begin
      LSP.Output_Tools.Write_Start_Notification
        (Self.Output.all, "notebookDocument/didSave");
      Self.Output.Key_Name ("params");
      LSP.Outputs.Write_DidSaveNotebookDocumentParams (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_DidSaveNotebook_Notification;

   overriding procedure On_DidChange_Notification
     (Self  : in out Server_Notification_Writer;
      Value : LSP.Structures.DidChangeTextDocumentParams) is
   begin
      LSP.Output_Tools.Write_Start_Notification
        (Self.Output.all, "textDocument/didChange");
      Self.Output.Key_Name ("params");
      LSP.Outputs.Write_DidChangeTextDocumentParams (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_DidChange_Notification;

   overriding procedure On_DidClose_Notification
     (Self  : in out Server_Notification_Writer;
      Value : LSP.Structures.DidCloseTextDocumentParams) is
   begin
      LSP.Output_Tools.Write_Start_Notification
        (Self.Output.all, "textDocument/didClose");
      Self.Output.Key_Name ("params");
      LSP.Outputs.Write_DidCloseTextDocumentParams (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_DidClose_Notification;

   overriding procedure On_DidOpen_Notification
     (Self  : in out Server_Notification_Writer;
      Value : LSP.Structures.DidOpenTextDocumentParams) is
   begin
      LSP.Output_Tools.Write_Start_Notification
        (Self.Output.all, "textDocument/didOpen");
      Self.Output.Key_Name ("params");
      LSP.Outputs.Write_DidOpenTextDocumentParams (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_DidOpen_Notification;

   overriding procedure On_DidSave_Notification
     (Self  : in out Server_Notification_Writer;
      Value : LSP.Structures.DidSaveTextDocumentParams) is
   begin
      LSP.Output_Tools.Write_Start_Notification
        (Self.Output.all, "textDocument/didSave");
      Self.Output.Key_Name ("params");
      LSP.Outputs.Write_DidSaveTextDocumentParams (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_DidSave_Notification;

   overriding procedure On_WillSave_Notification
     (Self  : in out Server_Notification_Writer;
      Value : LSP.Structures.WillSaveTextDocumentParams) is
   begin
      LSP.Output_Tools.Write_Start_Notification
        (Self.Output.all, "textDocument/willSave");
      Self.Output.Key_Name ("params");
      LSP.Outputs.Write_WillSaveTextDocumentParams (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_WillSave_Notification;

   overriding procedure On_Cancel_Notification
     (Self  : in out Server_Notification_Writer;
      Value : LSP.Structures.WorkDoneProgressCancelParams) is
   begin
      LSP.Output_Tools.Write_Start_Notification
        (Self.Output.all, "window/workDoneProgress/cancel");
      Self.Output.Key_Name ("params");
      LSP.Outputs.Write_WorkDoneProgressCancelParams (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_Cancel_Notification;

   overriding procedure On_DidChangeConfiguration_Notification
     (Self  : in out Server_Notification_Writer;
      Value : LSP.Structures.DidChangeConfigurationParams) is
   begin
      LSP.Output_Tools.Write_Start_Notification
        (Self.Output.all, "workspace/didChangeConfiguration");
      Self.Output.Key_Name ("params");
      LSP.Outputs.Write_DidChangeConfigurationParams (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_DidChangeConfiguration_Notification;

   overriding procedure On_DidChangeWatchedFiles_Notification
     (Self  : in out Server_Notification_Writer;
      Value : LSP.Structures.DidChangeWatchedFilesParams) is
   begin
      LSP.Output_Tools.Write_Start_Notification
        (Self.Output.all, "workspace/didChangeWatchedFiles");
      Self.Output.Key_Name ("params");
      LSP.Outputs.Write_DidChangeWatchedFilesParams (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_DidChangeWatchedFiles_Notification;

   overriding procedure On_DidChangeWorkspaceFolders_Notification
     (Self  : in out Server_Notification_Writer;
      Value : LSP.Structures.DidChangeWorkspaceFoldersParams) is
   begin
      LSP.Output_Tools.Write_Start_Notification
        (Self.Output.all, "workspace/didChangeWorkspaceFolders");
      Self.Output.Key_Name ("params");
      LSP.Outputs.Write_DidChangeWorkspaceFoldersParams
        (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_DidChangeWorkspaceFolders_Notification;

   overriding procedure On_DidCreateFiles_Notification
     (Self  : in out Server_Notification_Writer;
      Value : LSP.Structures.CreateFilesParams) is
   begin
      LSP.Output_Tools.Write_Start_Notification
        (Self.Output.all, "workspace/didCreateFiles");
      Self.Output.Key_Name ("params");
      LSP.Outputs.Write_CreateFilesParams (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_DidCreateFiles_Notification;

   overriding procedure On_DidDeleteFiles_Notification
     (Self  : in out Server_Notification_Writer;
      Value : LSP.Structures.DeleteFilesParams) is
   begin
      LSP.Output_Tools.Write_Start_Notification
        (Self.Output.all, "workspace/didDeleteFiles");
      Self.Output.Key_Name ("params");
      LSP.Outputs.Write_DeleteFilesParams (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_DidDeleteFiles_Notification;

   overriding procedure On_DidRenameFiles_Notification
     (Self  : in out Server_Notification_Writer;
      Value : LSP.Structures.RenameFilesParams) is
   begin
      LSP.Output_Tools.Write_Start_Notification
        (Self.Output.all, "workspace/didRenameFiles");
      Self.Output.Key_Name ("params");
      LSP.Outputs.Write_RenameFilesParams (Self.Output.all, Value);
      Self.Output.End_Object;
   end On_DidRenameFiles_Notification;

end LSP.Server_Notification_Writers;
