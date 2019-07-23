--  Automatically generated, do not edit.

with Ada.Streams;
with GNATCOLL.JSON; use GNATCOLL.JSON;

package LSP.Messages.Notifications is

   type Server_Notification_Handler is limited interface;
   type Server_Notification_Handler_Access is
     access all Server_Notification_Handler'Class;
   --  A type which represents a handler which supports reacting
   --  to Notifications. Clients implementing this interface should override
   --  the *_Notification methods, and clients making use of this interface
   --  should simply call Handle_Notification when they want to dispatch
   --  a Notification to the handler.

   type Initialized_Notification is new NotificationMessage with null record;

   type Exit_Notification is new NotificationMessage with null record;

   type DidChangeConfiguration_Notification is new NotificationMessage with
   record
      params : DidChangeConfigurationParams;
   end record;

   type DidOpenTextDocument_Notification is new NotificationMessage with
   record
      params : DidOpenTextDocumentParams;
   end record;

   type DidChangeTextDocument_Notification is new NotificationMessage with
   record
      params : DidChangeTextDocumentParams;
   end record;

   type DidSaveTextDocument_Notification is new NotificationMessage with
   record
      params : DidSaveTextDocumentParams;
   end record;

   type DidCloseTextDocument_Notification is new NotificationMessage with
   record
      params : DidCloseTextDocumentParams;
   end record;

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

private

   procedure Read
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Initialized_Notification);
   procedure Write
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Initialized_Notification);
   for Initialized_Notification'Read use Read;
   for Initialized_Notification'Write use Write;
   procedure Read
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Exit_Notification);
   procedure Write
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Exit_Notification);
   for Exit_Notification'Read use Read;
   for Exit_Notification'Write use Write;
   procedure Read
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DidChangeConfiguration_Notification);
   procedure Write
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DidChangeConfiguration_Notification);
   for DidChangeConfiguration_Notification'Read use Read;
   for DidChangeConfiguration_Notification'Write use Write;
   procedure Read
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DidOpenTextDocument_Notification);
   procedure Write
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DidOpenTextDocument_Notification);
   for DidOpenTextDocument_Notification'Read use Read;
   for DidOpenTextDocument_Notification'Write use Write;
   procedure Read
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DidChangeTextDocument_Notification);
   procedure Write
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DidChangeTextDocument_Notification);
   for DidChangeTextDocument_Notification'Read use Read;
   for DidChangeTextDocument_Notification'Write use Write;
   procedure Read
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DidSaveTextDocument_Notification);
   procedure Write
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DidSaveTextDocument_Notification);
   for DidSaveTextDocument_Notification'Read use Read;
   for DidSaveTextDocument_Notification'Write use Write;
   procedure Read
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DidCloseTextDocument_Notification);
   procedure Write
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DidCloseTextDocument_Notification);
   for DidCloseTextDocument_Notification'Read use Read;
   for DidCloseTextDocument_Notification'Write use Write;
end LSP.Messages.Notifications;
