--
--  Copyright (C) <YEAR>, <COPYRIGHT HOLDER>
--
--  SPDX-License-Identifier: MIT
--

with LSP.Base_Notification_Receivers;
with LSP.Structures;

package LSP.Server_Notification_Receivers is
   pragma Preelaborate;

   type Server_Notification_Receiver is
     limited interface and
       LSP.Base_Notification_Receivers.Base_Notification_Receiver;

   procedure On_SetTrace_Notification
     (Self  : in out Server_Notification_Receiver;
      Value : LSP.Structures.SetTraceParams) is null;

   procedure On_Exits_Notification
     (Self : in out Server_Notification_Receiver) is null;
   --  The exit event is sent from the client to the server to ask the server
   --  to exit its process.

   procedure On_Initialized_Notification
     (Self  : in out Server_Notification_Receiver;
      Value : LSP.Structures.InitializedParams) is null;
   --  The initialized notification is sent from the client to the server
   --  after the client is fully initialized and the server is allowed to
   --  send requests from the server to the client.

   procedure On_DidChangeNotebook_Notification
     (Self  : in out Server_Notification_Receiver;
      Value : LSP.Structures.DidChangeNotebookDocumentParams) is null;

   procedure On_DidCloseNotebook_Notification
     (Self  : in out Server_Notification_Receiver;
      Value : LSP.Structures.DidCloseNotebookDocumentParams) is null;
   --  A notification sent when a notebook closes.
   --
   --  @since 3.17.0

   procedure On_DidOpenNotebook_Notification
     (Self  : in out Server_Notification_Receiver;
      Value : LSP.Structures.DidOpenNotebookDocumentParams) is null;
   --  A notification sent when a notebook opens.
   --
   --  @since 3.17.0

   procedure On_DidSaveNotebook_Notification
     (Self  : in out Server_Notification_Receiver;
      Value : LSP.Structures.DidSaveNotebookDocumentParams) is null;
   --  A notification sent when a notebook document is saved.
   --
   --  @since 3.17.0

   procedure On_DidChange_Notification
     (Self  : in out Server_Notification_Receiver;
      Value : LSP.Structures.DidChangeTextDocumentParams) is null;
   --  The document change notification is sent from the client to the server
   --  to signal changes to a text document.

   procedure On_DidClose_Notification
     (Self  : in out Server_Notification_Receiver;
      Value : LSP.Structures.DidCloseTextDocumentParams) is null;
   --  The document close notification is sent from the client to the server
   --  when the document got closed in the client. The document's truth now
   --  exists where the document's uri points to (e.g. if the document's uri is
   --  a file uri the truth now exists on disk). As with the open notification
   --  the close notification is about managing the document's content.
   --  Receiving a close notification doesn't mean that the document was open
   --  in an editor before. A close notification requires a previous open
   --  notification to be sent.

   procedure On_DidOpen_Notification
     (Self  : in out Server_Notification_Receiver;
      Value : LSP.Structures.DidOpenTextDocumentParams) is null;
   --  The document open notification is sent from the client to the server to
   --  signal newly opened text documents. The document's truth is now managed
   --  by the client and the server must not try to read the document's truth
   --  using the document's uri. Open in this sense means it is managed by the
   --  client. It doesn't necessarily mean that its content is presented in an
   --  editor. An open notification must not be sent more than once without a
   --  corresponding close notification send before. This means open and close
   --  notification must be balanced and the max open count is one.

   procedure On_DidSave_Notification
     (Self  : in out Server_Notification_Receiver;
      Value : LSP.Structures.DidSaveTextDocumentParams) is null;
   --  The document save notification is sent from the client to the server
   --  when the document got saved in the client.

   procedure On_WillSave_Notification
     (Self  : in out Server_Notification_Receiver;
      Value : LSP.Structures.WillSaveTextDocumentParams) is null;
   --  A document will save notification is sent from the client to the server
   --  before the document is actually saved.

   procedure On_Cancel_Notification
     (Self  : in out Server_Notification_Receiver;
      Value : LSP.Structures.WorkDoneProgressCancelParams) is null;
   --  The `window/workDoneProgress/cancel` notification is sent from the
   --  client to the server to cancel a progress initiated on the server side.

   procedure On_DidChangeConfiguration_Notification
     (Self  : in out Server_Notification_Receiver;
      Value : LSP.Structures.DidChangeConfigurationParams) is null;
   --  The configuration change notification is sent from the client to the
   --  server when the client's configuration has changed. The notification
   --  contains the changed configuration as defined by the language client.

   procedure On_DidChangeWatchedFiles_Notification
     (Self  : in out Server_Notification_Receiver;
      Value : LSP.Structures.DidChangeWatchedFilesParams) is null;
   --  The watched files notification is sent from the client to the server
   --  when the client detects changes to file watched by the language client.

   procedure On_DidChangeWorkspaceFolders_Notification
     (Self  : in out Server_Notification_Receiver;
      Value : LSP.Structures.DidChangeWorkspaceFoldersParams) is null;
   --  The `workspace/didChangeWorkspaceFolders` notification is sent from the
   --  client to the server when the workspace folder configuration changes.

   procedure On_DidCreateFiles_Notification
     (Self  : in out Server_Notification_Receiver;
      Value : LSP.Structures.CreateFilesParams) is null;
   --  The did create files notification is sent from the client to the server
   --  when files were created from within the client.
   --
   --  @since 3.16.0

   procedure On_DidDeleteFiles_Notification
     (Self  : in out Server_Notification_Receiver;
      Value : LSP.Structures.DeleteFilesParams) is null;
   --  The will delete files request is sent from the client to the server
   --  before files are actually deleted as long as the deletion is triggered
   --  from within the client.
   --
   --  @since 3.16.0

   procedure On_DidRenameFiles_Notification
     (Self  : in out Server_Notification_Receiver;
      Value : LSP.Structures.RenameFilesParams) is null;
   --  The did rename files notification is sent from the client to the server
   --  when files were renamed from within the client.
   --
   --  @since 3.16.0

end LSP.Server_Notification_Receivers;
