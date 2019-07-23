--  Automatically generated, do not edit.

with Ada.Streams;
with GNATCOLL.JSON; use GNATCOLL.JSON;

package LSP.Messages.Requests is

   type Initialize_Request is new RequestMessage with
   record
      params : InitializeParams;
   end record;

   type Shutdown_Request is new RequestMessage with null record;

   type CodeAction_Request is new RequestMessage with
   record
      params : CodeActionParams;
   end record;

   type Completion_Request is new RequestMessage with
   record
      params : TextDocumentPositionParams;
   end record;

   type Definition_Request is new RequestMessage with
   record
      params : TextDocumentPositionParams;
   end record;

   type Type_Definition_Request is new RequestMessage with
   record
      params : TextDocumentPositionParams;
   end record;

   type Highlight_Request is new RequestMessage with
   record
      params : TextDocumentPositionParams;
   end record;

   type Hover_Request is new RequestMessage with
   record
      params : TextDocumentPositionParams;
   end record;

   type References_Request is new RequestMessage with
   record
      params : ReferenceParams;
   end record;

   type Signature_Help_Request is new RequestMessage with
   record
      params : TextDocumentPositionParams;
   end record;

   type Document_Symbols_Request is new RequestMessage with
   record
      params : DocumentSymbolParams;
   end record;

   type Rename_Request is new RequestMessage with
   record
      params : RenameParams;
   end record;

   type Execute_Command_Request is new RequestMessage with
   record
      params : ExecuteCommandParams;
   end record;

   type Workspace_Symbols_Request is new RequestMessage with
   record
      params : WorkspaceSymbolParams;
   end record;

   type Workspace_Execute_Command_Request is new RequestMessage with
   record
      params : ExecuteCommandParams;
   end record;

   type ALS_Called_By_Request is new RequestMessage with
   record
      params : TextDocumentPositionParams;
   end record;

private

   procedure Read
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Initialize_Request);
   procedure Write
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Initialize_Request);
   for Initialize_Request'Read use Read;
   for Initialize_Request'Write use Write;
   procedure Read
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Shutdown_Request);
   procedure Write
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Shutdown_Request);
   for Shutdown_Request'Read use Read;
   for Shutdown_Request'Write use Write;
   procedure Read
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CodeAction_Request);
   procedure Write
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CodeAction_Request);
   for CodeAction_Request'Read use Read;
   for CodeAction_Request'Write use Write;
   procedure Read
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Completion_Request);
   procedure Write
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Completion_Request);
   for Completion_Request'Read use Read;
   for Completion_Request'Write use Write;
   procedure Read
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Definition_Request);
   procedure Write
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Definition_Request);
   for Definition_Request'Read use Read;
   for Definition_Request'Write use Write;
   procedure Read
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Type_Definition_Request);
   procedure Write
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Type_Definition_Request);
   for Type_Definition_Request'Read use Read;
   for Type_Definition_Request'Write use Write;
   procedure Read
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Highlight_Request);
   procedure Write
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Highlight_Request);
   for Highlight_Request'Read use Read;
   for Highlight_Request'Write use Write;
   procedure Read
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Hover_Request);
   procedure Write
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Hover_Request);
   for Hover_Request'Read use Read;
   for Hover_Request'Write use Write;
   procedure Read
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out References_Request);
   procedure Write
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : References_Request);
   for References_Request'Read use Read;
   for References_Request'Write use Write;
   procedure Read
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Signature_Help_Request);
   procedure Write
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Signature_Help_Request);
   for Signature_Help_Request'Read use Read;
   for Signature_Help_Request'Write use Write;
   procedure Read
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Document_Symbols_Request);
   procedure Write
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Document_Symbols_Request);
   for Document_Symbols_Request'Read use Read;
   for Document_Symbols_Request'Write use Write;
   procedure Read
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Rename_Request);
   procedure Write
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Rename_Request);
   for Rename_Request'Read use Read;
   for Rename_Request'Write use Write;
   procedure Read
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Execute_Command_Request);
   procedure Write
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Execute_Command_Request);
   for Execute_Command_Request'Read use Read;
   for Execute_Command_Request'Write use Write;
   procedure Read
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Workspace_Symbols_Request);
   procedure Write
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Workspace_Symbols_Request);
   for Workspace_Symbols_Request'Read use Read;
   for Workspace_Symbols_Request'Write use Write;
   procedure Read
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Workspace_Execute_Command_Request);
   procedure Write
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Workspace_Execute_Command_Request);
   for Workspace_Execute_Command_Request'Read use Read;
   for Workspace_Execute_Command_Request'Write use Write;
   procedure Read
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ALS_Called_By_Request);
   procedure Write
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ALS_Called_By_Request);
   for ALS_Called_By_Request'Read use Read;
   for ALS_Called_By_Request'Write use Write;
end LSP.Messages.Requests;
