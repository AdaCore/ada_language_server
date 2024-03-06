--
--  Copyright (C) <YEAR>, <COPYRIGHT HOLDER>
--
--  SPDX-License-Identifier: MIT
--
--  DON'T EDIT THIS FILE! It was generated from metaModel.json.
--

with VSS.Strings;

package body LSP.Server_Notification_Loggers is

   overriding procedure On_SetTrace_Notification
     (Self  : in out Server_Notification_Logger;
      Value : LSP.Structures.SetTraceParams) is
      Ok : Boolean := True;
   begin
      Self.Output.Put ("'$/setTrace'", Ok);
      Self.Output.Put (" Params : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_SetTrace_Notification;

   overriding procedure On_Exits_Notification
     (Self : in out Server_Notification_Logger) is
      Ok : Boolean := True;
   begin
      Self.Output.Put ("'exit'", Ok);
      Self.Output.New_Line (Ok);
   end On_Exits_Notification;

   overriding procedure On_Initialized_Notification
     (Self  : in out Server_Notification_Logger;
      Value : LSP.Structures.InitializedParams) is
      Ok : Boolean := True;
   begin
      Self.Output.Put ("'initialized'", Ok);
      Self.Output.Put (" Params : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_Initialized_Notification;

   overriding procedure On_DidChangeNotebook_Notification
     (Self  : in out Server_Notification_Logger;
      Value : LSP.Structures.DidChangeNotebookDocumentParams) is
      Ok : Boolean := True;
   begin
      Self.Output.Put ("'notebookDocument/didChange'", Ok);
      Self.Output.Put (" Params : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_DidChangeNotebook_Notification;

   overriding procedure On_DidCloseNotebook_Notification
     (Self  : in out Server_Notification_Logger;
      Value : LSP.Structures.DidCloseNotebookDocumentParams) is
      Ok : Boolean := True;
   begin
      Self.Output.Put ("'notebookDocument/didClose'", Ok);
      Self.Output.Put (" Params : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_DidCloseNotebook_Notification;

   overriding procedure On_DidOpenNotebook_Notification
     (Self  : in out Server_Notification_Logger;
      Value : LSP.Structures.DidOpenNotebookDocumentParams) is
      Ok : Boolean := True;
   begin
      Self.Output.Put ("'notebookDocument/didOpen'", Ok);
      Self.Output.Put (" Params : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_DidOpenNotebook_Notification;

   overriding procedure On_DidSaveNotebook_Notification
     (Self  : in out Server_Notification_Logger;
      Value : LSP.Structures.DidSaveNotebookDocumentParams) is
      Ok : Boolean := True;
   begin
      Self.Output.Put ("'notebookDocument/didSave'", Ok);
      Self.Output.Put (" Params : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_DidSaveNotebook_Notification;

   overriding procedure On_DidChange_Notification
     (Self  : in out Server_Notification_Logger;
      Value : LSP.Structures.DidChangeTextDocumentParams) is
      Ok : Boolean := True;
   begin
      Self.Output.Put ("'textDocument/didChange'", Ok);
      Self.Output.Put (" Params : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_DidChange_Notification;

   overriding procedure On_DidClose_Notification
     (Self  : in out Server_Notification_Logger;
      Value : LSP.Structures.DidCloseTextDocumentParams) is
      Ok : Boolean := True;
   begin
      Self.Output.Put ("'textDocument/didClose'", Ok);
      Self.Output.Put (" Params : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_DidClose_Notification;

   overriding procedure On_DidOpen_Notification
     (Self  : in out Server_Notification_Logger;
      Value : LSP.Structures.DidOpenTextDocumentParams) is
      Ok : Boolean := True;
   begin
      Self.Output.Put ("'textDocument/didOpen'", Ok);
      Self.Output.Put (" Params : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_DidOpen_Notification;

   overriding procedure On_DidSave_Notification
     (Self  : in out Server_Notification_Logger;
      Value : LSP.Structures.DidSaveTextDocumentParams) is
      Ok : Boolean := True;
   begin
      Self.Output.Put ("'textDocument/didSave'", Ok);
      Self.Output.Put (" Params : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_DidSave_Notification;

   overriding procedure On_WillSave_Notification
     (Self  : in out Server_Notification_Logger;
      Value : LSP.Structures.WillSaveTextDocumentParams) is
      Ok : Boolean := True;
   begin
      Self.Output.Put ("'textDocument/willSave'", Ok);
      Self.Output.Put (" Params : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_WillSave_Notification;

   overriding procedure On_Cancel_Notification
     (Self  : in out Server_Notification_Logger;
      Value : LSP.Structures.WorkDoneProgressCancelParams) is
      Ok : Boolean := True;
   begin
      Self.Output.Put ("'window/workDoneProgress/cancel'", Ok);
      Self.Output.Put (" Params : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_Cancel_Notification;

   overriding procedure On_DidChangeConfiguration_Notification
     (Self  : in out Server_Notification_Logger;
      Value : LSP.Structures.DidChangeConfigurationParams) is
      Ok : Boolean := True;
   begin
      Self.Output.Put ("'workspace/didChangeConfiguration'", Ok);
      Self.Output.Put (" Params : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_DidChangeConfiguration_Notification;

   overriding procedure On_DidChangeWatchedFiles_Notification
     (Self  : in out Server_Notification_Logger;
      Value : LSP.Structures.DidChangeWatchedFilesParams) is
      Ok : Boolean := True;
   begin
      Self.Output.Put ("'workspace/didChangeWatchedFiles'", Ok);
      Self.Output.Put (" Params : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_DidChangeWatchedFiles_Notification;

   overriding procedure On_DidChangeWorkspaceFolders_Notification
     (Self  : in out Server_Notification_Logger;
      Value : LSP.Structures.DidChangeWorkspaceFoldersParams) is
      Ok : Boolean := True;
   begin
      Self.Output.Put ("'workspace/didChangeWorkspaceFolders'", Ok);
      Self.Output.Put (" Params : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_DidChangeWorkspaceFolders_Notification;

   overriding procedure On_DidCreateFiles_Notification
     (Self  : in out Server_Notification_Logger;
      Value : LSP.Structures.CreateFilesParams) is
      Ok : Boolean := True;
   begin
      Self.Output.Put ("'workspace/didCreateFiles'", Ok);
      Self.Output.Put (" Params : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_DidCreateFiles_Notification;

   overriding procedure On_DidDeleteFiles_Notification
     (Self  : in out Server_Notification_Logger;
      Value : LSP.Structures.DeleteFilesParams) is
      Ok : Boolean := True;
   begin
      Self.Output.Put ("'workspace/didDeleteFiles'", Ok);
      Self.Output.Put (" Params : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_DidDeleteFiles_Notification;

   overriding procedure On_DidRenameFiles_Notification
     (Self  : in out Server_Notification_Logger;
      Value : LSP.Structures.RenameFilesParams) is
      Ok : Boolean := True;
   begin
      Self.Output.Put ("'workspace/didRenameFiles'", Ok);
      Self.Output.Put (" Params : ", Ok);
      Self.Output.Put
        (VSS.Strings.To_Virtual_String (Value'Wide_Wide_Image), Ok);
      Self.Output.New_Line (Ok);
   end On_DidRenameFiles_Notification;

end LSP.Server_Notification_Loggers;
