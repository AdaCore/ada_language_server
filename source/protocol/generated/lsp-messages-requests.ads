--  Automatically generated, do not edit.

with Ada.Streams;
with GNATCOLL.JSON; use GNATCOLL.JSON;

package LSP.Messages.Requests is

   function Decode_Request (Document : JSON_Value) return RequestMessage'Class;
   --  Decode the request present in the given string.
   --  Document is an already decoded JSON representation of S, passed
   --  here for performance reasons.

   type Initialize_Request is new RequestMessage with record
      params : InitializeParams;
   end record;
   procedure Write
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Initialize_Request);
   for Initialize_Request'Write use Write;

   type Shutdown_Request is new RequestMessage with null record;
   procedure Write
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Shutdown_Request);
   for Shutdown_Request'Write use Write;

   type CodeAction_Request is new RequestMessage with record
      params : CodeActionParams;
   end record;
   procedure Write
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CodeAction_Request);
   for CodeAction_Request'Write use Write;

   type Completion_Request is new RequestMessage with record
      params : TextDocumentPositionParams;
   end record;
   procedure Write
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Completion_Request);
   for Completion_Request'Write use Write;

   type Definition_Request is new RequestMessage with record
      params : TextDocumentPositionParams;
   end record;
   procedure Write
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Definition_Request);
   for Definition_Request'Write use Write;

   type Highlight_Request is new RequestMessage with record
      params : TextDocumentPositionParams;
   end record;
   procedure Write
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Highlight_Request);
   for Highlight_Request'Write use Write;

   type Hover_Request is new RequestMessage with record
      params : TextDocumentPositionParams;
   end record;
   procedure Write
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Hover_Request);
   for Hover_Request'Write use Write;

   type References_Request is new RequestMessage with record
      params : ReferenceParams;
   end record;
   procedure Write
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : References_Request);
   for References_Request'Write use Write;

   type Signature_Help_Request is new RequestMessage with record
      params : TextDocumentPositionParams;
   end record;
   procedure Write
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Signature_Help_Request);
   for Signature_Help_Request'Write use Write;

   type Document_Symbols_Request is new RequestMessage with record
      params : DocumentSymbolParams;
   end record;
   procedure Write
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Document_Symbols_Request);
   for Document_Symbols_Request'Write use Write;

   type Execute_Command_Request is new RequestMessage with record
      params : ExecuteCommandParams;
   end record;
   procedure Write
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Execute_Command_Request);
   for Execute_Command_Request'Write use Write;

   type Workspace_Symbols_Request is new RequestMessage with record
      params : WorkspaceSymbolParams;
   end record;
   procedure Write
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Workspace_Symbols_Request);
   for Workspace_Symbols_Request'Write use Write;

   type Workspace_Execute_Command_Request is new RequestMessage with record
      params : ExecuteCommandParams;
   end record;
   procedure Write
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Workspace_Execute_Command_Request);
   for Workspace_Execute_Command_Request'Write use Write;

end LSP.Messages.Requests;
