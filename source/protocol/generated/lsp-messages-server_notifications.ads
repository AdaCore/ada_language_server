--  Automatically generated, do not edit.

with Ada.Streams;
with GNATCOLL.JSON; use GNATCOLL.JSON;

package LSP.Messages.Server_Notifications is

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
end LSP.Messages.Server_Notifications;
