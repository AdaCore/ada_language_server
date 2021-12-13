--  Automatically generated, do not edit.

limited with LSP.Messages;

package LSP.Server_Notification_Receivers is

   type Server_Notification_Receiver is limited interface;
   type Server_Notification_Receiver_Access is
     access all Server_Notification_Receiver'Class;
   --  A type which represents a handler which supports reacting
   --  to Notifications. Clients implementing this interface should override
   --  the *_Notification methods, and clients making use of this interface
   --  should simply call corresponding method when they want to dispatch
   --  a Notification to the handler.

   procedure On_Initialized_Notification
     (Self  : access Server_Notification_Receiver) is abstract;

   procedure On_Exit_Notification
     (Self  : access Server_Notification_Receiver) is abstract;

   procedure On_DidChangeConfiguration_Notification
     (Self  : access Server_Notification_Receiver;
      Value : LSP.Messages.DidChangeConfigurationParams) is abstract;

   procedure On_DidChangeWorkspaceFolders_Notification
     (Self  : access Server_Notification_Receiver;
      Value : LSP.Messages.DidChangeWorkspaceFoldersParams) is abstract;

   procedure On_DidChangeWatchedFiles_Notification
     (Self  : access Server_Notification_Receiver;
      Value : LSP.Messages.DidChangeWatchedFilesParams) is abstract;

   procedure On_DidCreateFiles_Notification
     (Self  : access Server_Notification_Receiver;
      Value : LSP.Messages.CreateFilesParams) is abstract;

   procedure On_DidRenameFiles_Notification
     (Self  : access Server_Notification_Receiver;
      Value : LSP.Messages.RenameFilesParams) is abstract;

   procedure On_DidDeleteFiles_Notification
     (Self  : access Server_Notification_Receiver;
      Value : LSP.Messages.DeleteFilesParams) is abstract;

   procedure On_Cancel_Notification
     (Self  : access Server_Notification_Receiver;
      Value : LSP.Messages.CancelParams) is abstract;

   procedure On_DidOpenTextDocument_Notification
     (Self  : access Server_Notification_Receiver;
      Value : LSP.Messages.DidOpenTextDocumentParams) is abstract;

   procedure On_DidChangeTextDocument_Notification
     (Self  : access Server_Notification_Receiver;
      Value : LSP.Messages.DidChangeTextDocumentParams) is abstract;

   procedure On_DidSaveTextDocument_Notification
     (Self  : access Server_Notification_Receiver;
      Value : LSP.Messages.DidSaveTextDocumentParams) is abstract;

   procedure On_DidCloseTextDocument_Notification
     (Self  : access Server_Notification_Receiver;
      Value : LSP.Messages.DidCloseTextDocumentParams) is abstract;

end LSP.Server_Notification_Receivers;
