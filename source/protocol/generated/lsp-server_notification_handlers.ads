--  Automatically generated, do not edit.

with LSP.Messages;

package LSP.Server_Notification_Handlers is

   type Server_Notification_Handler is limited interface;
   type Server_Notification_Handler_Access is
     access all Server_Notification_Handler'Class;
   --  A type which represents a handler which supports reacting
   --  to Notifications. Clients implementing this interface should override
   --  the *_Notification methods, and clients making use of this interface
   --  should simply call Handle_Notification when they want to dispatch
   --  a Notification to the handler.

   procedure On_Initialized_Notification
     (Self  : access Server_Notification_Handler) is abstract;

   procedure On_Exit_Notification
     (Self  : access Server_Notification_Handler) is abstract;

   procedure On_DidChangeConfiguration_Notification
     (Self  : access Server_Notification_Handler;
      Value : LSP.Messages.DidChangeConfigurationParams) is abstract;

   procedure On_DidOpenTextDocument_Notification
     (Self  : access Server_Notification_Handler;
      Value : LSP.Messages.DidOpenTextDocumentParams) is abstract;

   procedure On_DidChangeTextDocument_Notification
     (Self  : access Server_Notification_Handler;
      Value : LSP.Messages.DidChangeTextDocumentParams) is abstract;

   procedure On_DidSaveTextDocument_Notification
     (Self  : access Server_Notification_Handler;
      Value : LSP.Messages.DidSaveTextDocumentParams) is abstract;

   procedure On_DidCloseTextDocument_Notification
     (Self  : access Server_Notification_Handler;
      Value : LSP.Messages.DidCloseTextDocumentParams) is abstract;

end LSP.Server_Notification_Handlers;
