pragma Ada_2012;
package body LSP.Fuzz_Decorators is

   ---------------------------------
   -- On_Initialized_Notification --
   ---------------------------------

   overriding procedure On_Initialized_Notification
     (Self : access Fuzz_Notification_Decorator)
   is
   begin
      Self.Handler.On_Initialized_Notification;
   end On_Initialized_Notification;

   --------------------------
   -- On_Exit_Notification --
   --------------------------

   overriding procedure On_Exit_Notification
     (Self : access Fuzz_Notification_Decorator)
   is
   begin
      Self.Handler.On_Exit_Notification;
   end On_Exit_Notification;

   --------------------------------------------
   -- On_DidChangeConfiguration_Notification --
   --------------------------------------------

   overriding procedure On_DidChangeConfiguration_Notification
     (Self  : access Fuzz_Notification_Decorator;
      Value : LSP.Messages.DidChangeConfigurationParams)
   is
   begin
      Self.Handler.On_DidChangeConfiguration_Notification (Value);
   end On_DidChangeConfiguration_Notification;

   -----------------------------------------------
   -- On_DidChangeWorkspaceFolders_Notification --
   -----------------------------------------------

   overriding procedure On_DidChangeWorkspaceFolders_Notification
     (Self  : access Fuzz_Notification_Decorator;
      Value : LSP.Messages.DidChangeWorkspaceFoldersParams)
   is
   begin
      Self.Handler.On_DidChangeWorkspaceFolders_Notification (Value);
   end On_DidChangeWorkspaceFolders_Notification;

   ----------------------------
   -- On_Cancel_Notification --
   ----------------------------

   overriding procedure On_Cancel_Notification
     (Self  : access Fuzz_Notification_Decorator;
      Value : LSP.Messages.CancelParams)
   is
   begin
      Self.Handler.On_Cancel_Notification (Value);
   end On_Cancel_Notification;

   -----------------------------------------
   -- On_DidOpenTextDocument_Notification --
   -----------------------------------------

   overriding procedure On_DidOpenTextDocument_Notification
     (Self  : access Fuzz_Notification_Decorator;
      Value : LSP.Messages.DidOpenTextDocumentParams)
   is
   begin
      Self.Handler.On_DidOpenTextDocument_Notification (Value);
   end On_DidOpenTextDocument_Notification;

   -------------------------------------------
   -- On_DidChangeTextDocument_Notification --
   -------------------------------------------

   overriding procedure On_DidChangeTextDocument_Notification
     (Self  : access Fuzz_Notification_Decorator;
      Value : LSP.Messages.DidChangeTextDocumentParams)
   is
   begin
      Self.Handler.On_DidChangeTextDocument_Notification (Value);
   end On_DidChangeTextDocument_Notification;

   -----------------------------------------
   -- On_DidSaveTextDocument_Notification --
   -----------------------------------------

   overriding procedure On_DidSaveTextDocument_Notification
     (Self  : access Fuzz_Notification_Decorator;
      Value : LSP.Messages.DidSaveTextDocumentParams)
   is
   begin
      Self.Handler.On_DidSaveTextDocument_Notification (Value);
   end On_DidSaveTextDocument_Notification;

   ------------------------------------------
   -- On_DidCloseTextDocument_Notification --
   ------------------------------------------

   overriding procedure On_DidCloseTextDocument_Notification
     (Self  : access Fuzz_Notification_Decorator;
      Value : LSP.Messages.DidCloseTextDocumentParams)
   is
   begin
      Self.Handler.On_DidCloseTextDocument_Notification (Value);
   end On_DidCloseTextDocument_Notification;

end LSP.Fuzz_Decorators;
