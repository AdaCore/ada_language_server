--  Automatically generated, do not edit.

with Ada.Streams;
with GNATCOLL.JSON; use GNATCOLL.JSON;

package LSP.Messages.Requests is

   function Decode_Request
     (Document : JSON_Value) return RequestMessage'Class;
   --  Decode the request present in the input document. Document is a JSON
   --  representation of the protocol string.

   type Server_Request_Handler is limited interface;
   type Server_Request_Handler_Access is
     access all Server_Request_Handler'Class;
   --  A type which represents a handler which supports reacting
   --  to Requests. Clients implementing this interface should override
   --  the *_Request methods, and clients making use of this interface
   --  should simply call Handle_Request when they want to dispatch
   --  a Request to the handler.

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

   function On_Initialize_Request
     (Self  : access Server_Request_Handler;
      Value : LSP.Messages.InitializeParams)
      return LSP.Messages.Initialize_Response is abstract;

   function On_Shutdown_Request
     (Self : access Server_Request_Handler)
      return LSP.Messages.ResponseMessage is abstract;

   function On_CodeAction_Request
     (Self  : access Server_Request_Handler;
      Value : LSP.Messages.CodeActionParams)
      return LSP.Messages.CodeAction_Response is abstract;

   function On_Completion_Request
     (Self  : access Server_Request_Handler;
      Value : LSP.Messages.TextDocumentPositionParams)
      return LSP.Messages.Completion_Response is abstract;

   function On_Definition_Request
     (Self  : access Server_Request_Handler;
      Value : LSP.Messages.TextDocumentPositionParams)
      return LSP.Messages.Location_Response is abstract;

   function On_Type_Definition_Request
     (Self  : access Server_Request_Handler;
      Value : LSP.Messages.TextDocumentPositionParams)
      return LSP.Messages.Location_Response is abstract;

   function On_Highlight_Request
     (Self  : access Server_Request_Handler;
      Value : LSP.Messages.TextDocumentPositionParams)
      return LSP.Messages.Highlight_Response is abstract;

   function On_Hover_Request
     (Self  : access Server_Request_Handler;
      Value : LSP.Messages.TextDocumentPositionParams)
      return LSP.Messages.Hover_Response is abstract;

   function On_References_Request
     (Self  : access Server_Request_Handler;
      Value : LSP.Messages.ReferenceParams)
      return LSP.Messages.Location_Response is abstract;

   function On_Signature_Help_Request
     (Self  : access Server_Request_Handler;
      Value : LSP.Messages.TextDocumentPositionParams)
      return LSP.Messages.SignatureHelp_Response is abstract;

   function On_Document_Symbols_Request
     (Self  : access Server_Request_Handler;
      Value : LSP.Messages.DocumentSymbolParams)
      return LSP.Messages.Symbol_Response is abstract;

   function On_Rename_Request
     (Self  : access Server_Request_Handler;
      Value : LSP.Messages.RenameParams)
      return LSP.Messages.Rename_Response is abstract;

   function On_Execute_Command_Request
     (Self  : access Server_Request_Handler;
      Value : LSP.Messages.ExecuteCommandParams)
      return LSP.Messages.ExecuteCommand_Response is abstract;

   function On_Workspace_Symbols_Request
     (Self  : access Server_Request_Handler;
      Value : LSP.Messages.WorkspaceSymbolParams)
      return LSP.Messages.Symbol_Response is abstract;

   function On_Workspace_Execute_Command_Request
     (Self  : access Server_Request_Handler;
      Value : LSP.Messages.ExecuteCommandParams)
      return LSP.Messages.ExecuteCommand_Response is abstract;

   function On_ALS_Called_By_Request
     (Self  : access Server_Request_Handler;
      Value : LSP.Messages.TextDocumentPositionParams)
      return LSP.Messages.ALS_Called_By_Response is abstract;

   procedure Handle_Error
     (Self  : access Server_Request_Handler) is null;
   --  This procedure will be called when an unexpected error is raised in the
   --  request processing loop.
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
