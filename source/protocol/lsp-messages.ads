------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                        Copyright (C) 2018, AdaCore                       --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

with Ada.Containers.Hashed_Maps;
with Ada.Containers.Vectors;
with Ada.Streams;

with LSP.Generic_Optional;
with LSP.Types; use LSP.Types;

package LSP.Messages is
--   pragma Preelaborate;
   pragma Style_Checks ("M125-bcht");

   --```typescript
   --interface Message {
   --	jsonrpc: string;
   --}
   --```
   type Message is abstract tagged record
      jsonrpc: LSP_String;
   end record;

   --```typescript
   --interface RequestMessage extends Message {
   --
   --	/**
   --	 * The request id.
   --	 */
   --	id: number | string;
   --
   --	/**
   --	 * The method to be invoked.
   --	 */
   --	method: string;
   --
   --	/**
   --	 * The method's params.
   --	 */
   --	params?: any
   --}
   --```
   type RequestMessage is new Message with record
      id: LSP_Number_Or_String;
      method: LSP_String;
--        params: LSP_Any;
   end record;

   --```typescript
   --interface ResponseMessage extends Message {
   --	/**
   --	 * The request id.
   --	 */
   --	id: number | string | null;
   --
   --	/**
   --	 * The result of a request. This can be omitted in
   --	 * the case of an error.
   --	 */
   --	result?: any;
   --
   --	/**
   --	 * The error object in case a request fails.
   --	 */
   --	error?: ResponseError<any>;
   --}
   --
   --interface ResponseError<D> {
   --	/**
   --	 * A number indicating the error type that occurred.
   --	 */
   --	code: number;
   --
   --	/**
   --	 * A string providing a short description of the error.
   --	 */
   --	message: string;
   --
   --	/**
   --	 * A Primitive or Structured value that contains additional
   --	 * information about the error. Can be omitted.
   --	 */
   --	data?: D;
   --}
   --
   --export namespace ErrorCodes {
   --	// Defined by JSON RPC
   --	export const ParseError: number = -32700;
   --	export const InvalidRequest: number = -32600;
   --	export const MethodNotFound: number = -32601;
   --	export const InvalidParams: number = -32602;
   --	export const InternalError: number = -32603;
   --	export const serverErrorStart: number = -32099;
   --	export const serverErrorEnd: number = -32000;
   --	export const ServerNotInitialized: number = -32002;
   --	export const UnknownErrorCode: number = -32001;
   --
   --	// Defined by the protocol.
   --	export const RequestCancelled: number = -32800;
   --}
   --```
   type ErrorCodes is
     (ParseError,
      InvalidRequest,
      MethodNotFound,
      InvalidParams,
      InternalError,
      serverErrorStart,
      serverErrorEnd,
      ServerNotInitialized,
      UnknownErrorCode,
      RequestCancelled);

   type ResponseError is record
      code: ErrorCodes;
      message: LSP_String;
      data: LSP_Any;
   end record;

   not overriding procedure Read_ResponseError
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ResponseError);
   for ResponseError'Read use Read_ResponseError;

   not overriding procedure Write_ResponseError
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ResponseError);
   for ResponseError'Write use Write_ResponseError;

   package Optional_ResponseErrors is new LSP.Generic_Optional (ResponseError);
   type Optional_ResponseError is new Optional_ResponseErrors.Optional_Type;

   type ResponseMessage is new Message with record
      id: LSP_Number_Or_String;  --  or null?
--        result: LSP_Any;
      error: Optional_ResponseError;
   end record;

   not overriding procedure Write_ResponseMessage
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ResponseMessage);

   for ResponseMessage'Write use Write_ResponseMessage;

   --```typescript
   --interface NotificationMessage extends Message {
   --	/**
   --	 * The method to be invoked.
   --	 */
   --	method: string;
   --
   --	/**
   --	 * The notification's params.
   --	 */
   --	params?: any
   --}
   --```
   type NotificationMessage is new Message with record
      method: LSP_String;
--      params: LSP_Any;
   end record;

   --```typescript
   --interface CancelParams {
   --	/**
   --	 * The request id to cancel.
   --	 */
   --	id: number | string;
   --}
   --```
   type CancelParams is record
      id: LSP_Number_Or_String;
   end record;

   --```typescript
   --type DocumentUri = string;
   --```
   subtype DocumentUri is LSP.Types.LSP_String;

   --```typescript
   --export const EOL: string[] = ['\n', '\r\n', '\r'];
   --```

   --  This is intentionally empty. Nothing to declare for EOL

   --```typescript
   --interface Position {
   --	/**
   --	 * Line position in a document (zero-based).
   --	 */
   --	line: number;
   --
   --	/**
   --	 * Character offset on a line in a document (zero-based).
   --	 */
   --	character: number;
   --}
   --```
   type Position is record
      line: Line_Number;
      character: UTF_16_Index;
   end record;

   not overriding procedure Read_Position
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Position);
   for Position'Read use Read_Position;

   not overriding procedure Write_Position
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Position);
   for Position'Write use Write_Position;

   --```typescript
   --interface Range {
   --	/**
   --	 * The range's start position.
   --	 */
   --	start: Position;
   --
   --	/**
   --	 * The range's end position.
   --	 */
   --	end: Position;
   --}
   --```
   type Span is record
      first: Position;
      last: Position;  --  end: is reserved work
   end record;

   not overriding procedure Read_Span
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Span);
   for Span'Read use Read_Span;

   not overriding procedure Write_Span
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Span);
   for Span'Write use Write_Span;

   package Optional_Spans is new LSP.Generic_Optional (Span);
   type Optional_Span is new Optional_Spans.Optional_Type;

   --```typescript
   --interface Location {
   --	uri: DocumentUri;
   --	range: Range;
   --}
   --```
   type Location is record
      uri: DocumentUri;
      span: LSP.Messages.Span;  --  range: is reserved word
   end record;

   not overriding procedure Write_Location
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Location);
   for Location'Write use Write_Location;

   package Location_Vectors is new Ada.Containers.Vectors
     (Positive, Location);

   --+1
   --```typescript
   --namespace DiagnosticSeverity {
   --	/**
   --	 * Reports an error.
   --	 */
   --	export const Error = 1;
   --	/**
   --	 * Reports a warning.
   --	 */
   --	export const Warning = 2;
   --	/**
   --	 * Reports an information.
   --	 */
   --	export const Information = 3;
   --	/**
   --	 * Reports a hint.
   --	 */
   --	export const Hint = 4;
   --}
   --```
   type DiagnosticSeverity is (Error, Warning, Information, Hint);

   not overriding procedure Write_DiagnosticSeverity
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DiagnosticSeverity);
   for DiagnosticSeverity'Write use Write_DiagnosticSeverity;

   package Optional_DiagnosticSeveritys is new LSP.Generic_Optional (DiagnosticSeverity);
   type Optional_DiagnosticSeverity is new Optional_DiagnosticSeveritys.Optional_Type;

   --```typescript
   --interface Diagnostic {
   --	/**
   --	 * The range at which the message applies.
   --	 */
   --	range: Range;
   --
   --	/**
   --	 * The diagnostic's severity. Can be omitted. If omitted it is up to the
   --	 * client to interpret diagnostics as error, warning, info or hint.
   --	 */
   --	severity?: number;
   --
   --	/**
   --	 * The diagnostic's code. Can be omitted.
   --	 */
   --	code?: number | string;
   --
   --	/**
   --	 * A human-readable string describing the source of this
   --	 * diagnostic, e.g. 'typescript' or 'super lint'.
   --	 */
   --	source?: string;
   --
   --	/**
   --	 * The diagnostic's message.
   --	 */
   --	message: string;
   --}
   --```
   type Diagnostic is record
      span: LSP.Messages.Span;
      severity: Optional_DiagnosticSeverity;
      code: LSP_Number_Or_String;
      source: Optional_String;
      message: LSP_String;
   end record;

   not overriding procedure Read_Diagnostic
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Diagnostic);
   for Diagnostic'Read use Read_Diagnostic;

   not overriding procedure Write_Diagnostic
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Diagnostic);
   for Diagnostic'Write use Write_Diagnostic;

   package Diagnostic_Vectors is new Ada.Containers.Vectors
     (Positive, Diagnostic);

   type Diagnostic_Vector is new Diagnostic_Vectors.Vector with null record;

   --```typescript
   --interface Command {
   --	/**
   --	 * Title of the command, like `save`.
   --	 */
   --	title: string;
   --	/**
   --	 * The identifier of the actual command handler.
   --	 */
   --	command: string;
   --	/**
   --	 * Arguments that the command handler should be
   --	 * invoked with.
   --	 */
   --	arguments?: any[];
   --}
   --```
   type Command is record
      title: LSP_String;
      command: LSP_String;
      arguments: LSP_Any;
   end record;

   not overriding procedure Write_Command
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Command);
   for Command'Write use Write_Command;

   package Command_Vectors is new Ada.Containers.Vectors
     (Positive, Command);

   type Command_Vector is new Command_Vectors.Vector with null record;

   --```typescript
   --interface TextEdit {
   --	/**
   --	 * The range of the text document to be manipulated. To insert
   --	 * text into a document create a range where start === end.
   --	 */
   --	range: Range;
   --
   --	/**
   --	 * The string to be inserted. For delete operations use an
   --	 * empty string.
   --	 */
   --	newText: string;
   --}
   --```
   type TextEdit is record
      span: LSP.Messages.Span;
      newText: LSP_String;
   end record;

   not overriding procedure Read_TextEdit
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out TextEdit);
   for TextEdit'Read use Read_TextEdit;

   not overriding procedure Write_TextEdit
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : TextEdit);
   for TextEdit'Write use Write_TextEdit;

   package Optional_TextEdits is new LSP.Generic_Optional (TextEdit);
   type Optional_TextEdit is new Optional_TextEdits.Optional_Type;

   package TextEdit_Vectors is new Ada.Containers.Vectors (Positive, TextEdit);
   type TextEdit_Vector is new TextEdit_Vectors.Vector with null record;

   not overriding procedure Read_TextEdit_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out TextEdit_Vector);
   for TextEdit_Vector'Read use Read_TextEdit_Vector;

   not overriding procedure Write_TextEdit_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : TextEdit_Vector);
   for TextEdit_Vector'Write use Write_TextEdit_Vector;

   --+N
   --```typescript
   --interface TextDocumentIdentifier {
   --	/**
   --	 * The text document's URI.
   --	 */
   --	uri: DocumentUri;
   --}
   --```
   type TextDocumentIdentifier is tagged record
      uri: DocumentUri;
   end record;

   not overriding procedure Read_TextDocumentIdentifier
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out TextDocumentIdentifier);
   for TextDocumentIdentifier'Read use Read_TextDocumentIdentifier;

   --+N+2
   --```typescript
   --interface VersionedTextDocumentIdentifier extends TextDocumentIdentifier {
   --	/**
   --	 * The version number of this document.
   --	 */
   --	version: number;
   --}
   --```
   type VersionedTextDocumentIdentifier is new TextDocumentIdentifier with record
      version: Version_Id;
   end record;

   not overriding procedure Read_VersionedTextDocumentIdentifier
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out VersionedTextDocumentIdentifier);
   for VersionedTextDocumentIdentifier'Read use
     Read_VersionedTextDocumentIdentifier;

   not overriding procedure Write_VersionedTextDocumentIdentifier
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : VersionedTextDocumentIdentifier);
   for VersionedTextDocumentIdentifier'Write use
     Write_VersionedTextDocumentIdentifier;

   --```typescript
   --export interface TextDocumentEdit {
   --	/**
   --	 * The text document to change.
   --	 */
   --	textDocument: VersionedTextDocumentIdentifier;
   --
   --	/**
   --	 * The edits to be applied.
   --	 */
   --	edits: TextEdit[];
   --}
   --```
   type TextDocumentEdit is record
      textDocument: VersionedTextDocumentIdentifier;
      edits: TextEdit_Vector;
   end record;

   not overriding procedure Read_TextDocumentEdit
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out TextDocumentEdit);
   for TextDocumentEdit'Read use Read_TextDocumentEdit;

   not overriding procedure Write_TextDocumentEdit
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : TextDocumentEdit);
   for TextDocumentEdit'Write use Write_TextDocumentEdit;

   package TextDocumentEdit_Vectors is
     new Ada.Containers.Vectors (Positive, TextDocumentEdit);

   package TextDocumentEdit_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => LSP.Types.LSP_String,
      Element_Type    => TextEdit_Vector,
      Hash            => LSP.Types.Hash,
      Equivalent_Keys => LSP.Types."=");

   --```typescript
   --export interface WorkspaceEdit {
   --	/**
   --	 * Holds changes to existing resources.
   --	 */
   --	changes?: { [uri: string]: TextEdit[]; };
   --
   --	/**
   --	 * An array of `TextDocumentEdit`s to express changes to n different text documents
   --	 * where each text document edit addresses a specific version of a text document. 
   --	 * Whether a client supports versioned document edits is expressed via 
   --	 * `WorkspaceClientCapabilities.workspaceEdit.documentChanges`.
   --	 */
   --	documentChanges?: TextDocumentEdit[];
   --}
   --```
   type WorkspaceEdit is record
      changes: TextDocumentEdit_Maps.Map;
      documentChanges: TextDocumentEdit_Vectors.Vector;
   end record;

   --```typescript
   --interface TextDocumentItem {
   --	/**
   --	 * The text document's URI.
   --	 */
   --	uri: DocumentUri;
   --
   --	/**
   --	 * The text document's language identifier.
   --	 */
   --	languageId: string;
   --
   --	/**
   --	 * The version number of this document (it will increase after each
   --	 * change, including undo/redo).
   --	 */
   --	version: number;
   --
   --	/**
   --	 * The content of the opened text document.
   --	 */
   --	text: string;
   --}
   --```
   type TextDocumentItem is record
      uri: DocumentUri;
      languageId: LSP_String;
      version: Version_Id;
      text: LSP_String;
   end record;

   --```typescript
   --interface TextDocumentPositionParams {
   --	/**
   --	 * The text document.
   --	 */
   --	textDocument: TextDocumentIdentifier;
   --
   --	/**
   --	 * The position inside the text document.
   --	 */
   --	position: Position;
   --}
   --```
   type TextDocumentPositionParams is tagged record
      textDocument: TextDocumentIdentifier;
      position: LSP.Messages.Position;
   end record;

   not overriding procedure Read_TextDocumentPositionParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out TextDocumentPositionParams);
   for TextDocumentPositionParams'Read use Read_TextDocumentPositionParams;

   --```typescript
   --{ language: 'typescript', scheme: 'file' }
   --{ language: 'json', pattern: '**/package.json' }
   --```
   --  This is just example of filter. Nothing to do

   --```typescript
   --export interface DocumentFilter {
   --	/**
   --	 * A language id, like `typescript`.
   --	 */
   --	language?: string;
   --
   --	/**
   --	 * A Uri [scheme](#Uri.scheme), like `file` or `untitled`.
   --	 */
   --	scheme?: string;
   --
   --	/**
   --	 * A glob pattern, like `*.{ts,js}`.
   --	 */
   --	pattern?: string;
   --}
   --```
   type DocumentFilter is record
      language: LSP.Types.Optional_String;
      scheme: LSP.Types.Optional_String;
      pattern: LSP.Types.Optional_String;
   end record;

   package DocumentFilter_Vectors is new Ada.Containers.Vectors
     (Positive, DocumentFilter);
   --```typescript
   --export type DocumentSelector = DocumentFilter[];
   --```
   type DocumentSelector is new DocumentFilter_Vectors.Vector with null record;

   type dynamicRegistration is new Optional_Boolean;
   type documentChanges is new Optional_Boolean;
   --+M
   --```typescript
   --/**
   -- * Workspace specific client capabilities.
   -- */
   --export interface WorkspaceClientCapabilities {
   --	/**
   --	 * The client supports applying batch edits to the workspace by supporting
   --	 * the request 'workspace/applyEdit'
   --	 */
   --	applyEdit?: boolean;
   --
   --	/**
   --	 * Capabilities specific to `WorkspaceEdit`s
   --	 */
   --	workspaceEdit?: {
   --		/**
   --		 * The client supports versioned document changes in `WorkspaceEdit`s
   --		 */
   --		documentChanges?: boolean;
   --	};
   --
   --	/**
   --	 * Capabilities specific to the `workspace/didChangeConfiguration` notification.
   --	 */
   --	didChangeConfiguration?: {
   --		/**
   --		 * Did change configuration notification supports dynamic registration.
   --		 */
   --		dynamicRegistration?: boolean;
   --	};
   --
   --	/**
   --	 * Capabilities specific to the `workspace/didChangeWatchedFiles` notification.
   --	 */
   --	didChangeWatchedFiles?: {
   --		/**
   --		 * Did change watched files notification supports dynamic registration.
   --		 */
   --		dynamicRegistration?: boolean;
   --	};
   --
   --	/**
   --	 * Capabilities specific to the `workspace/symbol` request.
   --	 */
   --	symbol?: {
   --		/**
   --		 * Symbol request supports dynamic registration.
   --		 */
   --		dynamicRegistration?: boolean;
   --	};
   --
   --	/**
   --	 * Capabilities specific to the `workspace/executeCommand` request.
   --	 */
   --	executeCommand?: {
   --		/**
   --		 * Execute command supports dynamic registration.
   --		 */
   --		dynamicRegistration?: boolean;
   --	};
   --}
   --```
   
   type WorkspaceClientCapabilities is record
      applyEdit: Optional_Boolean;
      workspaceEdit: documentChanges;
      didChangeConfiguration: dynamicRegistration;
      didChangeWatchedFiles: dynamicRegistration;
      symbol: dynamicRegistration;
      executeCommand: dynamicRegistration;
   end record;

   --```typescript
   --/**
   -- * Text document specific client capabilities.
   -- */
   --export interface TextDocumentClientCapabilities {
   --
   --	synchronization?: {
   --		/**
   --		 * Whether text document synchronization supports dynamic registration.
   --		 */
   --		dynamicRegistration?: boolean;
   --
   --		/**
   --		 * The client supports sending will save notifications.
   --		 */
   --		willSave?: boolean;
   --
   --		/**
   --		 * The client supports sending a will save request and
   --		 * waits for a response providing text edits which will
   --		 * be applied to the document before it is saved.
   --		 */
   --		willSaveWaitUntil?: boolean;
   --
   --		/**
   --		 * The client supports did save notifications.
   --		 */
   --		didSave?: boolean;
   --	}
   --
   --	/**
   --	 * Capabilities specific to the `textDocument/completion`
   --	 */
   --	completion?: {
   --		/**
   --		 * Whether completion supports dynamic registration.
   --		 */
   --		dynamicRegistration?: boolean;
   --
   --		/**
   --		 * The client supports the following `CompletionItem` specific
   --		 * capabilities.
   --		 */
   --		completionItem?: {
   --			/**
   --			 * Client supports snippets as insert text.
   --			 *
   --			 * A snippet can define tab stops and placeholders with `$1`, `$2`
   --			 * and `${3:foo}`. `$0` defines the final tab stop, it defaults to
   --			 * the end of the snippet. Placeholders with equal identifiers are linked,
   --			 * that is typing in one will update others too.
   --			 */
   --			snippetSupport?: boolean;
   --		}
   --	};
   --
   --	/**
   --	 * Capabilities specific to the `textDocument/hover`
   --	 */
   --	hover?: {
   --		/**
   --		 * Whether hover supports dynamic registration.
   --		 */
   --		dynamicRegistration?: boolean;
   --	};
   --
   --	/**
   --	 * Capabilities specific to the `textDocument/signatureHelp`
   --	 */
   --	signatureHelp?: {
   --		/**
   --		 * Whether signature help supports dynamic registration.
   --		 */
   --		dynamicRegistration?: boolean;
   --	};
   --
   --	/**
   --	 * Capabilities specific to the `textDocument/references`
   --	 */
   --	references?: {
   --		/**
   --		 * Whether references supports dynamic registration.
   --		 */
   --		dynamicRegistration?: boolean;
   --	};
   --
   --	/**
   --	 * Capabilities specific to the `textDocument/documentHighlight`
   --	 */
   --	documentHighlight?: {
   --		/**
   --		 * Whether document highlight supports dynamic registration.
   --		 */
   --		dynamicRegistration?: boolean;
   --	};
   --
   --	/**
   --	 * Capabilities specific to the `textDocument/documentSymbol`
   --	 */
   --	documentSymbol?: {
   --		/**
   --		 * Whether document symbol supports dynamic registration.
   --		 */
   --		dynamicRegistration?: boolean;
   --	};
   --
   --	/**
   --	 * Capabilities specific to the `textDocument/formatting`
   --	 */
   --	formatting?: {
   --		/**
   --		 * Whether formatting supports dynamic registration.
   --		 */
   --		dynamicRegistration?: boolean;
   --	};
   --
   --	/**
   --	 * Capabilities specific to the `textDocument/rangeFormatting`
   --	 */
   --	rangeFormatting?: {
   --		/**
   --		 * Whether range formatting supports dynamic registration.
   --		 */
   --		dynamicRegistration?: boolean;
   --	};
   --
   --	/**
   --	 * Capabilities specific to the `textDocument/onTypeFormatting`
   --	 */
   --	onTypeFormatting?: {
   --		/**
   --		 * Whether on type formatting supports dynamic registration.
   --		 */
   --		dynamicRegistration?: boolean;
   --	};
   --
   --	/**
   --	 * Capabilities specific to the `textDocument/definition`
   --	 */
   --	definition?: {
   --		/**
   --		 * Whether definition supports dynamic registration.
   --		 */
   --		dynamicRegistration?: boolean;
   --	};
   --
   --	/**
   --	 * Capabilities specific to the `textDocument/codeAction`
   --	 */
   --	codeAction?: {
   --		/**
   --		 * Whether code action supports dynamic registration.
   --		 */
   --		dynamicRegistration?: boolean;
   --	};
   --
   --	/**
   --	 * Capabilities specific to the `textDocument/codeLens`
   --	 */
   --	codeLens?: {
   --		/**
   --		 * Whether code lens supports dynamic registration.
   --		 */
   --		dynamicRegistration?: boolean;
   --	};
   --
   --	/**
   --	 * Capabilities specific to the `textDocument/documentLink`
   --	 */
   --	documentLink?: {
   --		/**
   --		 * Whether document link supports dynamic registration.
   --		 */
   --		dynamicRegistration?: boolean;
   --	};
   --
   --	/**
   --	 * Capabilities specific to the `textDocument/rename`
   --	 */
   --	rename?: {
   --		/**
   --		 * Whether rename supports dynamic registration.
   --		 */
   --		dynamicRegistration?: boolean;
   --	};
   --}
   --```
   type synchronization is record
      dynamicRegistration : Optional_Boolean;
      willSave : Optional_Boolean;
      willSaveWaitUntil : Optional_Boolean;
      didSave : Optional_Boolean;
   end record;

   type completion is record
      dynamicRegistration : Optional_Boolean;
      snippetSupport : Optional_Boolean;
   end record;

   type TextDocumentClientCapabilities is record
      synchronization: LSP.Messages.synchronization;
      completion: LSP.Messages.completion;
      hover: dynamicRegistration;
      signatureHelp: dynamicRegistration;
      references: dynamicRegistration;
      documentHighlight: dynamicRegistration;
      documentSymbol: dynamicRegistration;
      formatting: dynamicRegistration;
      rangeFormatting: dynamicRegistration;
      onTypeFormatting: dynamicRegistration;
      definition: dynamicRegistration;
      codeAction: dynamicRegistration;
      codeLens: dynamicRegistration;
      documentLink: dynamicRegistration;
      rename: dynamicRegistration;
   end record;

   --```typescript
   --interface ClientCapabilities {
   --	/**
   --	 * Workspace specific client capabilities.
   --	 */
   --	workspace?: WorkspaceClientCapabilities;
   --
   --	/**
   --	 * Text document specific client capabilities.
   --	 */
   --	textDocument?: TextDocumentClientCapabilities;
   --
   --	/**
   --	 * Experimental client capabilities.
   --	 */
   --	experimental?: any;
   --}
   --```
   type ClientCapabilities is record
      workspace: WorkspaceClientCapabilities;
      textDocument: TextDocumentClientCapabilities;
      --  experimental?: any;
   end record;

   --```typescript
   --interface InitializeParams {
   --	/**
   --	 * The process Id of the parent process that started
   --	 * the server. Is null if the process has not been started by another process.
   --	 * If the parent process is not alive then the server should exit (see exit notification) its process.
   --	 */
   --	processId: number | null;
   --
   --	/**
   --	 * The rootPath of the workspace. Is null
   --	 * if no folder is open.
   --	 *
   --	 * @deprecated in favour of rootUri.
   --	 */
   --	rootPath?: string | null;
   --
   --	/**
   --	 * The rootUri of the workspace. Is null if no
   --	 * folder is open. If both `rootPath` and `rootUri` are set
   --	 * `rootUri` wins.
   --	 */
   --	rootUri: DocumentUri | null;
   --
   --	/**
   --	 * User provided initialization options.
   --	 */
   --	initializationOptions?: any;
   --
   --	/**
   --	 * The capabilities provided by the client (editor or tool)
   --	 */
   --	capabilities: ClientCapabilities;
   --
   --	/**
   --	 * The initial trace setting. If omitted trace is disabled ('off').
   --	 */
   --	trace?: 'off' | 'messages' | 'verbose';
   --}
   --```
   type InitializeParams is record
      processId: Optional_Number;
      rootPath: LSP_String;
      rootUri: DocumentUri;  --  or null???
      --  initializationOptions?: any;
      capabilities: ClientCapabilities;
      trace: Trace_Kinds;
   end record;

   --+K
   --```typescript
   --/**
   -- * Defines how the host (editor) should sync document changes to the language server.
   -- */
   --export namespace TextDocumentSyncKind {
   --	/**
   --	 * Documents should not be synced at all.
   --	 */
   --	export const None = 0;
   --
   --	/**
   --	 * Documents are synced by always sending the full content
   --	 * of the document.
   --	 */
   --	export const Full = 1;
   --
   --	/**
   --	 * Documents are synced by sending the full content on open.
   --	 * After that only incremental updates to the document are
   --	 * send.
   --	 */
   --	export const Incremental = 2;
   --}
   --
   --/**
   -- * Completion options.
   -- */
   --export interface CompletionOptions {
   --	/**
   --	 * The server provides support to resolve additional
   --	 * information for a completion item.
   --	 */
   --	resolveProvider?: boolean;
   --
   --	/**
   --	 * The characters that trigger completion automatically.
   --	 */
   --	triggerCharacters?: string[];
   --}
   --/**
   -- * Signature help options.
   -- */
   --export interface SignatureHelpOptions {
   --	/**
   --	 * The characters that trigger signature help
   --	 * automatically.
   --	 */
   --	triggerCharacters?: string[];
   --}
   --
   --/**
   -- * Code Lens options.
   -- */
   --export interface CodeLensOptions {
   --	/**
   --	 * Code lens has a resolve provider as well.
   --	 */
   --	resolveProvider?: boolean;
   --}
   --
   --/**
   -- * Format document on type options
   -- */
   --export interface DocumentOnTypeFormattingOptions {
   --	/**
   --	 * A character on which formatting should be triggered, like `}`.
   --	 */
   --	firstTriggerCharacter: string;
   --
   --	/**
   --	 * More trigger characters.
   --	 */
   --	moreTriggerCharacter?: string[];
   --}
   --
   --/**
   -- * Document link options
   -- */
   --export interface DocumentLinkOptions {
   --	/**
   --	 * Document links have a resolve provider as well.
   --	 */
   --	resolveProvider?: boolean;
   --}
   --
   --/**
   -- * Execute command options.
   -- */
   --export interface ExecuteCommandOptions {
   --	/**
   --	 * The commands to be executed on the server
   --	 */
   --	commands: string[]
   --}
   --
   --/**
   -- * Save options.
   -- */
   --export interface SaveOptions {
   --	/**
   --	 * The client is supposed to include the content on save.
   --	 */
   --	includeText?: boolean;
   --}
   --
   --export interface TextDocumentSyncOptions {
   --	/**
   --	 * Open and close notifications are sent to the server.
   --	 */
   --	openClose?: boolean;
   --	/**
   --	 * Change notificatins are sent to the server. See TextDocumentSyncKind.None, TextDocumentSyncKind.Full
   --	 * and TextDocumentSyncKindIncremental.
   --	 */
   --	change?: number;
   --	/**
   --	 * Will save notifications are sent to the server.
   --	 */
   --	willSave?: boolean;
   --	/**
   --	 * Will save wait until requests are sent to the server.
   --	 */
   --	willSaveWaitUntil?: boolean;
   --	/**
   --	 * Save notifications are sent to the server.
   --	 */
   --	save?: SaveOptions;
   --}
   --
   --interface ServerCapabilities {
   --	/**
   --	 * Defines how text documents are synced. Is either a detailed structure defining each notification or
   --	 * for backwards compatibility the TextDocumentSyncKind number.
   --	 */
   --	textDocumentSync?: TextDocumentSyncOptions | number;
   --	/**
   --	 * The server provides hover support.
   --	 */
   --	hoverProvider?: boolean;
   --	/**
   --	 * The server provides completion support.
   --	 */
   --	completionProvider?: CompletionOptions;
   --	/**
   --	 * The server provides signature help support.
   --	 */
   --	signatureHelpProvider?: SignatureHelpOptions;
   --	/**
   --	 * The server provides goto definition support.
   --	 */
   --	definitionProvider?: boolean;
   --	/**
   --	 * The server provides find references support.
   --	 */
   --	referencesProvider?: boolean;
   --	/**
   --	 * The server provides document highlight support.
   --	 */
   --	documentHighlightProvider?: boolean;
   --	/**
   --	 * The server provides document symbol support.
   --	 */
   --	documentSymbolProvider?: boolean;
   --	/**
   --	 * The server provides workspace symbol support.
   --	 */
   --	workspaceSymbolProvider?: boolean;
   --	/**
   --	 * The server provides code actions.
   --	 */
   --	codeActionProvider?: boolean;
   --	/**
   --	 * The server provides code lens.
   --	 */
   --	codeLensProvider?: CodeLensOptions;
   --	/**
   --	 * The server provides document formatting.
   --	 */
   --	documentFormattingProvider?: boolean;
   --	/**
   --	 * The server provides document range formatting.
   --	 */
   --	documentRangeFormattingProvider?: boolean;
   --	/**
   --	 * The server provides document formatting on typing.
   --	 */
   --	documentOnTypeFormattingProvider?: DocumentOnTypeFormattingOptions;
   --	/**
   --	 * The server provides rename support.
   --	 */
   --	renameProvider?: boolean;
   --	/**
   --	 * The server provides document link support.
   --	 */
   --	documentLinkProvider?: DocumentLinkOptions;
   --	/**
   --	 * The server provides execute command support.
   --	 */
   --	executeCommandProvider?: ExecuteCommandOptions;
   --	/**
   --	 * Experimental server capabilities.
   --	 */
   --	experimental?: any;
   --}
   --```
   type TextDocumentSyncKind is (None, Full, Incremental);

   not overriding procedure Read_TextDocumentSyncKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out TextDocumentSyncKind);
   for TextDocumentSyncKind'Read use Read_TextDocumentSyncKind;

   not overriding procedure Write_TextDocumentSyncKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : TextDocumentSyncKind);
   for TextDocumentSyncKind'Write use Write_TextDocumentSyncKind;

   package Optional_TextDocumentSyncKinds is new LSP.Generic_Optional (TextDocumentSyncKind);
   type Optional_TextDocumentSyncKind is new Optional_TextDocumentSyncKinds.Optional_Type;

   type TextDocumentSyncOptions is record
      openClose: Optional_Boolean;
      change: Optional_TextDocumentSyncKind;
      willSave: Optional_Boolean;
      willSaveWaitUntil: Optional_Boolean;
      save: Optional_Boolean;
   end record;

   type Optional_TextDocumentSyncOptions
     (Is_Set    : Boolean := False;
      Is_Number : Boolean := False) is
      record
         case Is_Set is
            when True =>
               case Is_Number is
                  when True =>
                     Value : TextDocumentSyncKind;
                  when False =>
                     Options : TextDocumentSyncOptions;
               end case;
            when False => null;
         end case;
   end record;

   type CompletionOptions is record
      resolveProvider: LSP.Types.Optional_Boolean;
      triggerCharacters: LSP.Types.LSP_String_Vector;
   end record;

   not overriding procedure Read_CompletionOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CompletionOptions);
   for CompletionOptions'Read use Read_CompletionOptions;

   not overriding procedure Write_CompletionOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CompletionOptions);
   for CompletionOptions'Write use Write_CompletionOptions;

   package Optional_CompletionOptionss is
     new LSP.Generic_Optional (CompletionOptions);

   type Optional_CompletionOptions is
     new Optional_CompletionOptionss.Optional_Type;

   type SignatureHelpOptions is record
      triggerCharacters: LSP.Types.LSP_String_Vector;
   end record;

   not overriding procedure Read_SignatureHelpOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out SignatureHelpOptions);

   for SignatureHelpOptions'Read use Read_SignatureHelpOptions;

   not overriding procedure Write_SignatureHelpOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SignatureHelpOptions);

   for SignatureHelpOptions'Write use Write_SignatureHelpOptions;

   package Optional_SignatureHelpOptionss is
     new LSP.Generic_Optional (SignatureHelpOptions);

   type Optional_SignatureHelpOptions is
     new Optional_SignatureHelpOptionss.Optional_Type;

   type CodeLensOptions is record
      resolveProvider: LSP.Types.Optional_Boolean;
   end record;

   not overriding procedure Write_CodeLensOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CodeLensOptions);

   for CodeLensOptions'Write use Write_CodeLensOptions;

   package Optional_CodeLensOptionss is
     new LSP.Generic_Optional (CodeLensOptions);

   type Optional_CodeLensOptions is
     new Optional_CodeLensOptionss.Optional_Type;

   type DocumentOnTypeFormattingOptions is record
      firstTriggerCharacter: LSP.Types.LSP_String;
      moreTriggerCharacter: LSP.Types.LSP_String_Vector;
   end record;

   not overriding procedure Read_DocumentOnTypeFormattingOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DocumentOnTypeFormattingOptions);

   for DocumentOnTypeFormattingOptions'Read use Read_DocumentOnTypeFormattingOptions;

   not overriding procedure Write_DocumentOnTypeFormattingOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DocumentOnTypeFormattingOptions);

   for DocumentOnTypeFormattingOptions'Write use Write_DocumentOnTypeFormattingOptions;

   package Optional_DocumentOnTypeFormattingOptionss is
     new LSP.Generic_Optional (DocumentOnTypeFormattingOptions);

   type Optional_DocumentOnTypeFormattingOptions is
     new Optional_DocumentOnTypeFormattingOptionss.Optional_Type;

   type DocumentLinkOptions is record
      resolveProvider: LSP.Types.Optional_Boolean;
   end record;

   type ExecuteCommandOptions is record
      commands: LSP.Types.LSP_String_Vector;
   end record;

   type ServerCapabilities is record
      textDocumentSync: Optional_TextDocumentSyncOptions;
      hoverProvider: Optional_Boolean;
      completionProvider: Optional_CompletionOptions;
      signatureHelpProvider: Optional_SignatureHelpOptions;
      definitionProvider: Optional_Boolean;
      referencesProvider: Optional_Boolean;
      documentHighlightProvider: Optional_Boolean;
      documentSymbolProvider: Optional_Boolean;
      workspaceSymbolProvider: Optional_Boolean;
      codeActionProvider: Optional_Boolean;
      codeLensProvider: Optional_CodeLensOptions;
      documentFormattingProvider: Optional_Boolean;
      documentRangeFormattingProvider: Optional_Boolean;
      documentOnTypeFormattingProvider: Optional_DocumentOnTypeFormattingOptions;
      renameProvider: Optional_Boolean;
      documentLinkProvider: DocumentLinkOptions;
      executeCommandProvider: ExecuteCommandOptions;
   --	experimental?: any;
   end record;

   --```typescript
   --interface InitializeResult {
   --	/**
   --	 * The capabilities the language server provides.
   --	 */
   --	capabilities: ServerCapabilities;
   --}
   --```
   type InitializeResult is record
      capabilities: ServerCapabilities;
   end record;

   type Initialize_Response is new ResponseMessage with record
      result: InitializeResult;
   end record;

   type Initialize_Request is new RequestMessage with record
      params: InitializeParams;
   end record;

   --```typescript
   --/**
   -- * Known error codes for an `InitializeError`;
   -- */
   --export namespace InitializeError {
   --	/**
   --	 * If the protocol version provided by the client can't be handled by the server.
   --	 * @deprecated This initialize error got replaced by client capabilities. There is
   --	 * no version handshake in version 3.0x
   --	 */
   --	export const unknownProtocolVersion: number = 1;
   --}
   --```
   unknownProtocolVersion: constant := 1;

   --```typescript
   --interface InitializeError {
   --	/**
   --	 * Indicates whether the client execute the following retry logic:
   --	 * (1) show the message provided by the ResponseError to the user
   --	 * (2) user selects retry or cancel
   --	 * (3) if user selected retry the initialize method is sent again.
   --	 */
   --	retry: boolean;
   --}
   --```
   type InitializeError is record
      retry: Boolean;
   end record;

   --+J
   --```typescript
   --export namespace MessageType {
   --	/**
   --	 * An error message.
   --	 */
   --	export const Error = 1;
   --	/**
   --	 * A warning message.
   --	 */
   --	export const Warning = 2;
   --	/**
   --	 * An information message.
   --	 */
   --	export const Info = 3;
   --	/**
   --	 * A log message.
   --	 */
   --	export const Log = 4;
   --}
   --```
   type MessageType is (Error, Warning, Info, Log);

   --```typescript
   --interface ShowMessageParams {
   --	/**
   --	 * The message type. See {@link MessageType}.
   --	 */
   --	type: number;
   --
   --	/**
   --	 * The actual message.
   --	 */
   --	message: string;
   --}
   --```
   type ShowMessageParams is record
      the_type: MessageType;  --  type: is reserver word
      message: LSP_String;
   end record;

   --```typescript
   --interface ShowMessageRequestParams {
   --	/**
   --	 * The message type. See {@link MessageType}
   --	 */
   --	type: number;
   --
   --	/**
   --	 * The actual message
   --	 */
   --	message: string;
   --
   --	/**
   --	 * The message action items to present.
   --	 */
   --	actions?: MessageActionItem[];
   --}
   --```
   type ShowMessageRequestParams is record
      the_type: MessageType;  --  type: is reserver word
      message: LSP_String;
      actions: MessageActionItem_Vector;
   end record;

   --```typescript
   --interface MessageActionItem {
   --	/**
   --	 * A short title like 'Retry', 'Open Log' etc.
   --	 */
   --	title: string;
   --}
   --```
   --  Lets use League.Strings.Universal_String for MessageActionItem

   --```typescript
   --interface LogMessageParams {
   --	/**
   --	 * The message type. See {@link MessageType}
   --	 */
   --	type: number;
   --
   --	/**
   --	 * The actual message
   --	 */
   --	message: string;
   --}
   --```
   type LogMessageParams is record
      the_type: MessageType;  --  type: is reserver word
      message: LSP_String;
   end record;

   --```typescript
   --export interface TextDocumentRegistrationOptions {
   --	/**
   --	 * A document selector to identify the scope of the registration. If set to null
   --	 * the document selector provided on the client side will be used.
   --	 */
   --	documentSelector: DocumentSelector | null;
   --}
   --```
   type TextDocumentRegistrationOptions is tagged record
      documentSelector:  LSP.Messages.DocumentSelector;
   end record;

   --```typescript
   --/**
   -- * Descibe options to be used when registered for text document change events.
   -- */
   --export interface TextDocumentChangeRegistrationOptions extends TextDocumentRegistrationOptions {
   --	/**
   --	 * How documents are synced to the server. See TextDocumentSyncKind.Full
   --	 * and TextDocumentSyncKindIncremental.
   --	 */
   --	syncKind: number;
   --}
   --```
   type TextDocumentChangeRegistrationOptions is
     new TextDocumentRegistrationOptions with
   record
      syncKind: TextDocumentSyncKind;
   end record;

   --```typescript
   --export interface TextDocumentSaveRegistrationOptions extends TextDocumentRegistrationOptions {
   --	/**
   --	 * The client is supposed to include the content on save.
   --	 */
   --	includeText?: boolean;
   --}
   --```
   type TextDocumentSaveRegistrationOptions is
     new TextDocumentRegistrationOptions with record
      includeText: Optional_Boolean;
   end record;

   --```typescript
   --export interface CompletionRegistrationOptions extends TextDocumentRegistrationOptions {
   --	/**
   --	 * The characters that trigger completion automatically.
   --	 */
   --	triggerCharacters?: string[];
   --
   --	/**
   --	 * The server provides support to resolve additional
   --	 * information for a completion item.
   --	 */
   --	resolveProvider?: boolean;
   --}
   --```
   type CompletionRegistrationOptions is new TextDocumentRegistrationOptions with record
      triggerCharacters: LSP_String_Vector;
      resolveProvider: Optional_Boolean;
   end record;

   --```typescript
   --export interface SignatureHelpRegistrationOptions extends TextDocumentRegistrationOptions {
   --	/**
   --	 * The characters that trigger signature help
   --	 * automatically.
   --	 */
   --	triggerCharacters?: string[];
   --}
   --```
   type SignatureHelpRegistrationOptions is new TextDocumentRegistrationOptions with record
      triggerCharacters: LSP_String_Vector;
   end record;

   --```typescript
   --export interface CodeLensRegistrationOptions extends TextDocumentRegistrationOptions {
   --	/**
   --	 * Code lens has a resolve provider as well.
   --	 */
   --	resolveProvider?: boolean;
   --}
   --```
   type CodeLensRegistrationOptions is new TextDocumentRegistrationOptions with record
      resolveProvider: Optional_Boolean;
   end record;

   --```typescript
   --export interface DocumentLinkRegistrationOptions extends TextDocumentRegistrationOptions {
   --	/**
   --	 * Document links have a resolve provider as well.
   --	 */
   --	resolveProvider?: boolean;
   --}
   --```
   type DocumentLinkRegistrationOptions is new TextDocumentRegistrationOptions with record
      resolveProvider: Optional_Boolean;
   end record;

   --```typescript
   --export interface DocumentOnTypeFormattingRegistrationOptions extends TextDocumentRegistrationOptions {
   --	/**
   --	 * A character on which formatting should be triggered, like `}`.
   --	 */
   --	firstTriggerCharacter: string;
   --	/**
   --	 * More trigger characters.
   --	 */
   --	moreTriggerCharacter?: string[]
   --}
   --```
   type DocumentOnTypeFormattingRegistrationOptions is new TextDocumentRegistrationOptions with record
      firstTriggerCharacter: LSP_String;
      moreTriggerCharacter: LSP_String_Vector;
   end record;

   --```typescript
   --/**
   -- * Execute command registration options.
   -- */
   --export interface ExecuteCommandRegistrationOptions {
   --	/**
   --	 * The commands to be executed on the server
   --	 */
   --	commands: string[]
   --}
   --```
   type ExecuteCommandRegistrationOptions is record
      commands: LSP_String_Vector;
   end record;

   type Registration_Option (Kind : Registration_Option_Kinds := Absent) is record
      case Kind is
         when Absent =>
            null;
         when Text_Document_Registration_Option =>
            Text_Document : TextDocumentRegistrationOptions;
         when Text_Document_Change_Registration_Option =>
            Text_Document_Change : TextDocumentChangeRegistrationOptions;
         when Text_Document_Save_Registration_Option =>
            Text_Document_Save : TextDocumentSaveRegistrationOptions;
         when Completion_Registration_Option =>
            Completion : CompletionRegistrationOptions;
         when Signature_Help_Registration_Option =>
            SignatureHelp : SignatureHelpRegistrationOptions;
         when Code_Lens_Registration_Option =>
            CodeLens : CodeLensRegistrationOptions;
         when Document_Link_Registration_Option =>
            DocumentLink : DocumentLinkRegistrationOptions;
         when Document_On_Type_Formatting_Registration_Option =>
            DocumentOnTypeFormatting : DocumentOnTypeFormattingRegistrationOptions;
         when Execute_Command_Registration_Option =>
            ExecuteCommand : ExecuteCommandRegistrationOptions;
      end case;
   end record;

   --```typescript
   --/**
   -- * General parameters to register for a capability.
   -- */
   --export interface Registration {
   --	/**
   --	 * The id used to register the request. The id can be used to deregister
   --	 * the request again.
   --	 */
   --	id: string;
   --
   --	/**
   --	 * The method / capability to register for.
   --	 */
   --	method: string;
   --
   --	/**
   --	 * Options necessary for the registration.
   --	 */
   --	registerOptions?: any;
   --}
   --
   --export interface RegistrationParams {
   --	registrations: Registration[];
   --}
   --```
   type Registration is record
      id: LSP_String;
      method: LSP_String;
      registerOptions: Registration_Option;
   end record;

   type Registration_Array is array (Positive range <>) of Registration;

   type RegistrationParams (Length : Natural) is record
      registrations: Registration_Array (1 .. Length);
   end record;

   --```typescript
   --/**
   -- * General parameters to unregister a capability.
   -- */
   --export interface Unregistration {
   --	/**
   --	 * The id used to unregister the request or notification. Usually an id
   --	 * provided during the register request.
   --	 */
   --	id: string;
   --
   --	/**
   --	 * The method / capability to unregister for.
   --	 */
   --	method: string;
   --}
   --
   --export interface UnregistrationParams {
   --	unregisterations: Unregistration[];
   --}
   --```
   type Unregistration is record
      id: LSP_String;
      method: LSP_String;
   end record;

   package Unregistration_Vectors is new Ada.Containers.Vectors
     (Positive, Unregistration);

   type UnregistrationParams is
     new Unregistration_Vectors.Vector with null record;

   --```typescript
   --interface DidChangeConfigurationParams {
   --	/**
   --	 * The actual changed settings
   --	 */
   --	settings: any;
   --}
   --```
   type DidChangeConfigurationParams is record
      settings: LSP.Types.LSP_Any;
   end record;

   --```typescript
   --interface DidOpenTextDocumentParams {
   --	/**
   --	 * The document that was opened.
   --	 */
   --	textDocument: TextDocumentItem;
   --}
   --```
   type DidOpenTextDocumentParams is record
      textDocument: TextDocumentItem;
   end record;

   --```typescript
   --interface DidChangeTextDocumentParams {
   --	/**
   --	 * The document that did change. The version number points
   --	 * to the version after all provided content changes have
   --	 * been applied.
   --	 */
   --	textDocument: VersionedTextDocumentIdentifier;
   --
   --	/**
   --	 * The actual content changes. The content changes descibe single state changes
   --	 * to the document. So if there are two content changes c1 and c2 for a document 
   --	 * in state S10 then c1 move the document to S11 and c2 to S12.
   --	 */
   --	contentChanges: TextDocumentContentChangeEvent[];
   --}
   --
   --/**
   -- * An event describing a change to a text document. If range and rangeLength are omitted
   -- * the new text is considered to be the full content of the document.
   -- */
   --interface TextDocumentContentChangeEvent {
   --	/**
   --	 * The range of the document that changed.
   --	 */
   --	range?: Range;
   --
   --	/**
   --	 * The length of the range that got replaced.
   --	 */
   --	rangeLength?: number;
   --
   --	/**
   --	 * The new text of the range/document.
   --	 */
   --	text: string;
   --}
   --```
   type TextDocumentContentChangeEvent is record
      span: Optional_Span;
      rangeLength: LSP.Types.Optional_Number;
      text: LSP_String;
   end record;

   not overriding procedure Read_TextDocumentContentChangeEvent
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out TextDocumentContentChangeEvent);
   for TextDocumentContentChangeEvent'Read use
     Read_TextDocumentContentChangeEvent;

   package TextDocumentContentChangeEvent_Vectors is new Ada.Containers.Vectors
     (Positive, TextDocumentContentChangeEvent);

   type TextDocumentContentChangeEvent_Vector is
     new TextDocumentContentChangeEvent_Vectors.Vector with null record;

   not overriding procedure Read_TextDocumentContentChangeEvent_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out TextDocumentContentChangeEvent_Vector);
   for TextDocumentContentChangeEvent_Vector'Read use
     Read_TextDocumentContentChangeEvent_Vector;

   type DidChangeTextDocumentParams is record
      textDocument: VersionedTextDocumentIdentifier;
      contentChanges: TextDocumentContentChangeEvent_Vector;
   end record;

   --```typescript
   --/**
   -- * The parameters send in a will save text document notification.
   -- */
   --export interface WillSaveTextDocumentParams {
   --	/**
   --	 * The document that will be saved.
   --	 */
   --	textDocument: TextDocumentIdentifier;
   --
   --	/**
   --	 * The 'TextDocumentSaveReason'.
   --	 */
   --	reason: number;
   --}
   --
   --/**
   -- * Represents reasons why a text document is saved.
   -- */
   --export namespace TextDocumentSaveReason {
   --
   --	/**
   --	 * Manually triggered, e.g. by the user pressing save, by starting debugging,
   --	 * or by an API call.
   --	 */
   --	export const Manual = 1;
   --
   --	/**
   --	 * Automatic after a delay.
   --	 */
   --	export const AfterDelay = 2;
   --
   --	/**
   --	 * When the editor lost focus.
   --	 */
   --	export const FocusOut = 3;
   --}
   --```
   type TextDocumentSaveReason is (Manual, AfterDelay, FocusOut);

   type WillSaveTextDocumentParams is record
      textDocument: TextDocumentIdentifier;
      reason: TextDocumentSaveReason;
   end record;

   --```typescript
   --interface DidSaveTextDocumentParams {
   --	/**
   --	 * The document that was saved.
   --	 */
   --	textDocument: TextDocumentIdentifier;
   --
   --	/**
   --	 * Optional the content when saved. Depends on the includeText value
   --	 * when the save notifcation was requested.
   --	 */
   --	text?: string;
   --}
   --```
   type DidSaveTextDocumentParams is record
      textDocument: TextDocumentIdentifier;
      text: Optional_String;
   end record;

   --```typescript
   --interface DidCloseTextDocumentParams {
   --	/**
   --	 * The document that was closed.
   --	 */
   --	textDocument: TextDocumentIdentifier;
   --}
   --```
   type DidCloseTextDocumentParams is record
      textDocument: TextDocumentIdentifier;
   end record;

   --```typescript
   --/**
   -- * An event describing a file change.
   -- */
   --interface FileEvent {
   --	/**
   --	 * The file's URI.
   --	 */
   --	uri: DocumentUri;
   --	/**
   --	 * The change type.
   --	 */
   --	type: number;
   --}
   --
   --/**
   -- * The file event type.
   -- */
   --export namespace FileChangeType {
   --	/**
   --	 * The file got created.
   --	 */
   --	export const Created = 1;
   --	/**
   --	 * The file got changed.
   --	 */
   --	export const Changed = 2;
   --	/**
   --	 * The file got deleted.
   --	 */
   --	export const Deleted = 3;
   --}
   --```
   type FileChangeType is (Created, Changed, Deleted);
   type FileEvent is record
      uri: DocumentUri;
      the_type : FileChangeType;  -- type: is reserver word
   end record;

   package FileEvent_Vectors is new Ada.Containers.Vectors (Positive, FileEvent);

   --```typescript
   --interface DidChangeWatchedFilesParams {
   --	/**
   --	 * The actual file events.
   --	 */
   --	changes: FileEvent[];
   --}
   --```
   type DidChangeWatchedFilesParams is record
      changes: FileEvent_Vectors.Vector;
   end record;

   --```typescript
   --interface PublishDiagnosticsParams {
   --	/**
   --	 * The URI for which diagnostic information is reported.
   --	 */
   --	uri: DocumentUri;
   --
   --	/**
   --	 * An array of diagnostic information items.
   --	 */
   --	diagnostics: Diagnostic[];
   --}
   --```
   type PublishDiagnosticsParams is record
      uri: DocumentUri;
      diagnostics: Diagnostic_Vector;
   end record;

   --```typescript
   --/**
   -- * Represents a collection of [completion items](#CompletionItem) to be presented
   -- * in the editor.
   -- */
   --interface CompletionList {
   --	/**
   --	 * This list it not complete. Further typing should result in recomputing
   --	 * this list.
   --	 */
   --	isIncomplete: boolean;
   --	/**
   --	 * The completion items.
   --	 */
   --	items: CompletionItem[];
   --}
   --
   --/**
   -- * Defines whether the insert text in a completion item should be interpreted as
   -- * plain text or a snippet.
   -- */
   --namespace InsertTextFormat {
   --	/**
   --	 * The primary text to be inserted is treated as a plain string.
   --	 */
   --	export const PlainText = 1;
   --
   --	/**
   --	 * The primary text to be inserted is treated as a snippet.
   --	 *
   --	 * A snippet can define tab stops and placeholders with `$1`, `$2`
   --	 * and `${3:foo}`. `$0` defines the final tab stop, it defaults to
   --	 * the end of the snippet. Placeholders with equal identifiers are linked,
   --	 * that is typing in one will update others too.
   --	 *
   --	 * See also: https://github.com/Microsoft/vscode/blob/master/src/vs/editor/contrib/snippet/common/snippet.md
   --	 */
   --	export const Snippet = 2;
   --}
   --
   --type InsertTextFormat = 1 | 2;
   --
   --interface CompletionItem {
   --	/**
   --	 * The label of this completion item. By default
   --	 * also the text that is inserted when selecting
   --	 * this completion.
   --	 */
   --	label: string;
   --	/**
   --	 * The kind of this completion item. Based of the kind
   --	 * an icon is chosen by the editor.
   --	 */
   --	kind?: number;
   --	/**
   --	 * A human-readable string with additional information
   --	 * about this item, like type or symbol information.
   --	 */
   --	detail?: string;
   --	/**
   --	 * A human-readable string that represents a doc-comment.
   --	 */
   --	documentation?: string;
   --	/**
   --	 * A string that shoud be used when comparing this item
   --	 * with other items. When `falsy` the label is used.
   --	 */
   --	sortText?: string;
   --	/**
   --	 * A string that should be used when filtering a set of
   --	 * completion items. When `falsy` the label is used.
   --	 */
   --	filterText?: string;
   --	/**
   --	 * A string that should be inserted a document when selecting
   --	 * this completion. When `falsy` the label is used.
   --	 */
   --	insertText?: string;
   --	/**
   --	 * The format of the insert text. The format applies to both the `insertText` property
   --	 * and the `newText` property of a provided `textEdit`.
   --	 */
   --	insertTextFormat?: InsertTextFormat;
   --	/**
   --	 * An edit which is applied to a document when selecting this completion. When an edit is provided the value of
   --	 * `insertText` is ignored.
   --	 *
   --	 * *Note:* The range of the edit must be a single line range and it must contain the position at which completion
   --	 * has been requested.
   --	 */
   --	textEdit?: TextEdit;
   --	/**
   --	 * An optional array of additional text edits that are applied when
   --	 * selecting this completion. Edits must not overlap with the main edit
   --	 * nor with themselves.
   --	 */
   --	additionalTextEdits?: TextEdit[];
   --	/**
   --	 * An optional set of characters that when pressed while this completion is active will accept it first and
   --	 * then type that character. *Note* that all commit characters should have `length=1` and that superfluous
   --	 * characters will be ignored.
   --	 */
   --	commitCharacters?: string[];
   --	/**
   --	 * An optional command that is executed *after* inserting this completion. *Note* that
   --	 * additional modifications to the current document should be described with the
   --	 * additionalTextEdits-property.
   --	 */
   --	command?: Command;
   --	/**
   --	 * An data entry field that is preserved on a completion item between
   --	 * a completion and a completion resolve request.
   --	 */
   --	data?: any
   --}
   --
   --/**
   -- * The kind of a completion entry.
   -- */
   --namespace CompletionItemKind {
   --	export const Text = 1;
   --	export const Method = 2;
   --	export const Function = 3;
   --	export const Constructor = 4;
   --	export const Field = 5;
   --	export const Variable = 6;
   --	export const Class = 7;
   --	export const Interface = 8;
   --	export const Module = 9;
   --	export const Property = 10;
   --	export const Unit = 11;
   --	export const Value = 12;
   --	export const Enum = 13;
   --	export const Keyword = 14;
   --	export const Snippet = 15;
   --	export const Color = 16;
   --	export const File = 17;
   --	export const Reference = 18;
   --}
   --```
   type InsertTextFormat is (PlainText, Snippet);

   not overriding procedure Write_InsertTextFormat
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : InsertTextFormat);
   for InsertTextFormat'Write use Write_InsertTextFormat;

   package Optional_InsertTextFormats is new LSP.Generic_Optional (InsertTextFormat);
   type Optional_InsertTextFormat is new Optional_InsertTextFormats.Optional_Type;

   type CompletionItemKind is (
      Text,
      Method,
      A_Function,
      Constructor,
      Field,
      Variable,
      Class,
      An_Interface,
      Module,
      Property,
      Unit,
      Value,
      Enum,
      Keyword,
      Snippet,
      Color,
      File,
      Reference);

   not overriding procedure Write_CompletionItemKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CompletionItemKind);
   for CompletionItemKind'Write use Write_CompletionItemKind;

   package Optional_CompletionItemKinds is new LSP.Generic_Optional (CompletionItemKind);
   type Optional_CompletionItemKind is new Optional_CompletionItemKinds.Optional_Type;

   type CompletionItem is record
      label: LSP_String;
      kind: Optional_CompletionItemKind;
      detail: Optional_String;
      documentation: Optional_String;
      sortText: Optional_String;
      filterText: Optional_String;
      insertText: Optional_String;
      insertTextFormat: Optional_InsertTextFormat;
      textEdit: Optional_TextEdit;
      additionalTextEdits: TextEdit_Vector;
      commitCharacters: LSP_String_Vector;
      command: LSP.Messages.Command;  --  Optional ???
   --	data?: any
   end record;

   not overriding procedure Write_CompletionItem
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CompletionItem);
   for CompletionItem'Write use Write_CompletionItem;

   package CompletionItem_Vectors is new Ada.Containers.Vectors
     (Positive, CompletionItem);

   type CompletionList is record
      isIncomplete: Boolean := False;
      items: CompletionItem_Vectors.Vector;
   end record;

   type Completion_Response is new ResponseMessage with record
      result: CompletionList;
   end record;

   --```typescript
   --/**
   -- * MarkedString can be used to render human readable text. It is either a markdown string
   -- * or a code-block that provides a language and a code snippet. The language identifier
   -- * is sematically equal to the optional language identifier in fenced code blocks in GitHub
   -- * issues. See https://help.github.com/articles/creating-and-highlighting-code-blocks/#syntax-highlighting
   -- *
   -- * The pair of a language and a value is an equivalent to markdown:
   -- * ```${language}
   -- * ${value}
   -- * ```
   -- *
   -- * Note that markdown strings will be sanitized - that means html will be escaped.
   -- */
   --type MarkedString = string | { language: string; value: string };
   --```
   type MarkedString (Is_String : Boolean := True) is record
      value : LSP_String;

      case Is_String is
         when True =>
            null;
         when False =>
            language : LSP_String;
      end case;
   end record;

   not overriding procedure Write_MarkedString
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : MarkedString);
   for MarkedString'Write use Write_MarkedString;

   package MarkedString_Vectors is new Ada.Containers.Vectors
     (Positive, MarkedString);

   --```typescript
   --/**
   -- * The result of a hover request.
   -- */
   --interface Hover {
   --	/**
   --	 * The hover's content
   --	 */
   --	contents: MarkedString | MarkedString[];
   --
   --	/**
   --	 * An optional range is a range inside a text document
   --	 * that is used to visualize a hover, e.g. by changing the background color.
   --	 */
   --	range?: Range;
   --}
   --```
   type Hover is record
      contents: MarkedString_Vectors.Vector;
      Span: Optional_Span;
   end record;

   type Hover_Response is new ResponseMessage with record
      result: Hover;
   end record;

   --```typescript
   --/**
   -- * Signature help represents the signature of something
   -- * callable. There can be multiple signature but only one
   -- * active and only one active parameter.
   -- */
   --interface SignatureHelp {
   --	/**
   --	 * One or more signatures.
   --	 */
   --	signatures: SignatureInformation[];
   --
   --	/**
   --	 * The active signature. If omitted or the value lies outside the
   --	 * range of `signatures` the value defaults to zero or is ignored if
   --	 * `signatures.length === 0`. Whenever possible implementors should 
   --	 * make an active decision about the active signature and shouldn't 
   --	 * rely on a default value.
   --	 * In future version of the protocol this property might become
   --	 * mandantory to better express this.
   --	 */
   --	activeSignature?: number;
   --
   --	/**
   --	 * The active parameter of the active signature. If omitted or the value
   --	 * lies outside the range of `signatures[activeSignature].parameters` 
   --	 * defaults to 0 if the active signature has parameters. If 
   --	 * the active signature has no parameters it is ignored. 
   --	 * In future version of the protocol this property might become
   --	 * mandantory to better express the active parameter if the
   --	 * active signature does have any.
   --	 */
   --	activeParameter?: number;
   --}
   --
   --/**
   -- * Represents the signature of something callable. A signature
   -- * can have a label, like a function-name, a doc-comment, and
   -- * a set of parameters.
   -- */
   --interface SignatureInformation {
   --	/**
   --	 * The label of this signature. Will be shown in
   --	 * the UI.
   --	 */
   --	label: string;
   --
   --	/**
   --	 * The human-readable doc-comment of this signature. Will be shown
   --	 * in the UI but can be omitted.
   --	 */
   --	documentation?: string;
   --
   --	/**
   --	 * The parameters of this signature.
   --	 */
   --	parameters?: ParameterInformation[];
   --}
   --
   --/**
   -- * Represents a parameter of a callable-signature. A parameter can
   -- * have a label and a doc-comment.
   -- */
   --interface ParameterInformation {
   --	/**
   --	 * The label of this parameter. Will be shown in
   --	 * the UI.
   --	 */
   --	label: string;
   --
   --	/**
   --	 * The human-readable doc-comment of this parameter. Will be shown
   --	 * in the UI but can be omitted.
   --	 */
   --	documentation?: string;
   --}
   --```
   type ParameterInformation is record
      label: LSP_String;
      documentation: Optional_String;
   end record;

   not overriding procedure Write_ParameterInformation
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ParameterInformation);
   for ParameterInformation'Write use Write_ParameterInformation;

   package ParameterInformation_Vectors is new Ada.Containers.Vectors
     (Positive, ParameterInformation);

   type SignatureInformation is record
      label: LSP_String;
      documentation: Optional_String;
      parameters: ParameterInformation_Vectors.Vector;
   end record;

   not overriding procedure Write_SignatureInformation
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SignatureInformation);
   for SignatureInformation'Write use Write_SignatureInformation;

   package SignatureInformation_Vectors is new Ada.Containers.Vectors
     (Positive, SignatureInformation);

   type SignatureHelp is record
	signatures: SignatureInformation_Vectors.Vector;
	activeSignature: Optional_Number;
	activeParameter: Optional_Number;
   end record;

   type SignatureHelp_Response is new ResponseMessage with record
      result: SignatureHelp;
   end record;

   --```typescript
   --interface ReferenceContext {
   --	/**
   --	 * Include the declaration of the current symbol.
   --	 */
   --	includeDeclaration: boolean;
   --}
   --```
   type ReferenceContext is record
      includeDeclaration: Boolean;
   end record;

   --```typescript
   --interface ReferenceParams extends TextDocumentPositionParams {
   --	context: ReferenceContext
   --}
   --```
   type ReferenceParams is new TextDocumentPositionParams with record
      context: ReferenceContext;
   end record;

   --```typescript
   --/**
   -- * A document highlight is a range inside a text document which deserves
   -- * special attention. Usually a document highlight is visualized by changing
   -- * the background color of its range.
   -- *
   -- */
   --interface DocumentHighlight {
   --	/**
   --	 * The range this highlight applies to.
   --	 */
   --	range: Range;
   --
   --	/**
   --	 * The highlight kind, default is DocumentHighlightKind.Text.
   --	 */
   --	kind?: number;
   --}
   --
   --/**
   -- * A document highlight kind.
   -- */
   --export namespace DocumentHighlightKind {
   --	/**
   --	 * A textual occurrence.
   --	 */
   --	export const Text = 1;
   --
   --	/**
   --	 * Read-access of a symbol, like reading a variable.
   --	 */
   --	export const Read = 2;
   --
   --	/**
   --	 * Write-access of a symbol, like writing to a variable.
   --	 */
   --	export const Write = 3;
   --}
   --```
   type DocumentHighlightKind is (Unspecified, Text, Read, Write);

   type DocumentHighlight is record
      span: LSP.Messages.Span;
      kind: DocumentHighlightKind;
   end record;

   not overriding procedure Write_DocumentHighlight
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DocumentHighlight);
   for DocumentHighlight'Write use Write_DocumentHighlight;

   package DocumentHighlight_Vectors is new Ada.Containers.Vectors
     (Positive, DocumentHighlight);

   type Highlight_Response is new ResponseMessage with record
      result: DocumentHighlight_Vectors.Vector;
   end record;

   --```typescript
   --interface DocumentSymbolParams {
   --	/**
   --	 * The text document.
   --	 */
   --	textDocument: TextDocumentIdentifier;
   --}
   --```
   type DocumentSymbolParams is record
      textDocument: TextDocumentIdentifier;
   end record;

   --```typescript
   --/**
   -- * Represents information about programming constructs like variables, classes,
   -- * interfaces etc.
   -- */
   --interface SymbolInformation {
   --	/**
   --	 * The name of this symbol.
   --	 */
   --	name: string;
   --
   --	/**
   --	 * The kind of this symbol.
   --	 */
   --	kind: number;
   --
   --	/**
   --	 * The location of this symbol. The location's range is used by a tool
   --	 * to reveal the location in the editor. If the symbol is selected in the
   --	 * tool the range's start information is used to position the cursor. So
   --	 * the range usually spwans more then the actual symbol's name and does
   --	 * normally include thinks like visibility modifiers.
   --	 *
   --	 * The range doesn't have to denote a node range in the sense of a abstract
   --	 * syntax tree. It can therefore not be used to re-construct a hierarchy of
   --	 * the symbols.
   --	 */
   --	location: Location;
   --
   --	/**
   --	 * The name of the symbol containing this symbol. This information is for
   --	 * user interface purposes (e.g. to render a qaulifier in the user interface
   --	 * if necessary). It can't be used to re-infer a hierarchy for the document
   --	 * symbols.
   --	 */
   --	containerName?: string;
   --}
   --
   --/**
   -- * A symbol kind.
   -- */
   --export namespace SymbolKind {
   --	export const File = 1;
   --	export const Module = 2;
   --	export const Namespace = 3;
   --	export const Package = 4;
   --	export const Class = 5;
   --	export const Method = 6;
   --	export const Property = 7;
   --	export const Field = 8;
   --	export const Constructor = 9;
   --	export const Enum = 10;
   --	export const Interface = 11;
   --	export const Function = 12;
   --	export const Variable = 13;
   --	export const Constant = 14;
   --	export const String = 15;
   --	export const Number = 16;
   --	export const Boolean = 17;
   --	export const Array = 18;
   --}
   --```
   type SymbolKind is
     (File,
      Module,
      Namespace,
      A_Package,
      Class,
      Method,
      Property,
      Field,
      Constructor,
      Enum,
      An_Interface,
      A_Function,
      Variable,
      A_Constant,
      String,
      Number,
      A_Boolean,
      An_Array);

   type SymbolInformation is record
      name: LSP_String;
      kind: SymbolKind;
      location: LSP.Messages.Location;
      containerName: Optional_String;
   end record;

   not overriding procedure Write_SymbolInformation
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SymbolInformation);
   for SymbolInformation'Write use Write_SymbolInformation;

   package SymbolInformation_Vectors is new Ada.Containers.Vectors
     (Positive, SymbolInformation);

   type SymbolInformation_Vector is
     new SymbolInformation_Vectors.Vector with null record;

   type Symbol_Response is new ResponseMessage with record
      result: SymbolInformation_Vector;
   end record;

   --```typescript
   --/**
   -- * The parameters of a Workspace Symbol Request.
   -- */
   --interface WorkspaceSymbolParams {
   --	/**
   --	 * A non-empty query string
   --	 */
   --	query: string;
   --}
   --```
   type WorkspaceSymbolParams is record
      query: LSP_String;
   end record;

   --```typescript
   --/**
   -- * Params for the CodeActionRequest
   -- */
   --interface CodeActionParams {
   --	/**
   --	 * The document in which the command was invoked.
   --	 */
   --	textDocument: TextDocumentIdentifier;
   --
   --	/**
   --	 * The range for which the command was invoked.
   --	 */
   --	range: Range;
   --
   --	/**
   --	 * Context carrying additional information.
   --	 */
   --	context: CodeActionContext;
   --}
   --
   --/**
   -- * Contains additional diagnostic information about the context in which
   -- * a code action is run.
   -- */
   --interface CodeActionContext {
   --	/**
   --	 * An array of diagnostics.
   --	 */
   --	diagnostics: Diagnostic[];
   --}
   --```
   type CodeActionContext is record
      diagnostics: Diagnostic_Vector;
   end record;

   type CodeActionParams is record
      textDocument: TextDocumentIdentifier;
      span: LSP.Messages.Span;
      context: CodeActionContext;
   end record;

   type CodeAction_Response is new ResponseMessage with record
      result: Command_Vector;
   end record;

   --```typescript
   --interface CodeLensParams {
   --	/**
   --	 * The document to request code lens for.
   --	 */
   --	textDocument: TextDocumentIdentifier;
   --}
   --```
   type CodeLensParams is record
      textDocument: TextDocumentIdentifier;
   end record;

   --```typescript
   --/**
   -- * A code lens represents a command that should be shown along with
   -- * source text, like the number of references, a way to run tests, etc.
   -- *
   -- * A code lens is _unresolved_ when no command is associated to it. For performance
   -- * reasons the creation of a code lens and resolving should be done in two stages.
   -- */
   --interface CodeLens {
   --	/**
   --	 * The range in which this code lens is valid. Should only span a single line.
   --	 */
   --	range: Range;
   --
   --	/**
   --	 * The command this code lens represents.
   --	 */
   --	command?: Command;
   --
   --	/**
   --	 * A data entry field that is preserved on a code lens item between
   --	 * a code lens and a code lens resolve request.
   --	 */
   --	data?: any
   --}
   --```
   type CodeLens is record
      span: LSP.Messages.Span;
      command: LSP.Messages.Command;  --  Optional ???
      --  data?: any
   end record;

   --```typescript
   --interface DocumentLinkParams {
   --	/**
   --	 * The document to provide document links for.
   --	 */
   --	textDocument: TextDocumentIdentifier;
   --}
   --```
   type DocumentLinkParams is record
      textDocument: TextDocumentIdentifier;
   end record;

   --```typescript
   --/**
   -- * A document link is a range in a text document that links to an internal or external resource, like another
   -- * text document or a web site.
   -- */
   --interface DocumentLink {
   --	/**
   --	 * The range this link applies to.
   --	 */
   --	range: Range;
   --	/**
   --	 * The uri this link points to. If missing a resolve request is sent later.
   --	 */
   --	target?: DocumentUri;
   --}
   --```
   type DocumentLink is record
      span: LSP.Messages.Span;
      target: DocumentUri;  --  Optional ???
   end record;

   --```typescript
   --interface DocumentFormattingParams {
   --	/**
   --	 * The document to format.
   --	 */
   --	textDocument: TextDocumentIdentifier;
   --
   --	/**
   --	 * The format options.
   --	 */
   --	options: FormattingOptions;
   --}
   --
   --/**
   -- * Value-object describing what options formatting should use.
   -- */
   --interface FormattingOptions {
   --	/**
   --	 * Size of a tab in spaces.
   --	 */
   --	tabSize: number;
   --
   --	/**
   --	 * Prefer spaces over tabs.
   --	 */
   --	insertSpaces: boolean;
   --
   --	/**
   --	 * Signature for further properties.
   --	 */
   --	[key: string]: boolean | number | string;
   --}
   --```
   type FormattingOptions is record
      tabSize: LSP_Number;
      insertSpaces: Boolean;
      --  [key: string]: boolean | number | string; ???
   end record;

   type DocumentFormattingParams is record
      textDocument: TextDocumentIdentifier;
      options: FormattingOptions;
   end record;

   --```typescript
   --interface DocumentRangeFormattingParams {
   --	/**
   --	 * The document to format.
   --	 */
   --	textDocument: TextDocumentIdentifier;
   --
   --	/**
   --	 * The range to format
   --	 */
   --	range: Range;
   --
   --	/**
   --	 * The format options
   --	 */
   --	options: FormattingOptions;
   --}
   --```
   type DocumentRangeFormattingParams is record
      textDocument: TextDocumentIdentifier;
      span: LSP.Messages.Span;
      options: FormattingOptions;
   end record;

   --```typescript
   --interface DocumentOnTypeFormattingParams {
   --	/**
   --	 * The document to format.
   --	 */
   --	textDocument: TextDocumentIdentifier;
   --
   --	/**
   --	 * The position at which this request was sent.
   --	 */
   --	position: Position;
   --
   --	/**
   --	 * The character that has been typed.
   --	 */
   --	ch: string;
   --
   --	/**
   --	 * The format options.
   --	 */
   --	options: FormattingOptions;
   --}
   --```
   type DocumentOnTypeFormattingParams is record
      textDocument: TextDocumentIdentifier;
      position: LSP.Messages.Position;
      ch: LSP_String;
      options: FormattingOptions;
   end record;

   --```typescript
   --interface RenameParams {
   --	/**
   --	 * The document to format.
   --	 */
   --	textDocument: TextDocumentIdentifier;
   --
   --	/**
   --	 * The position at which this request was sent.
   --	 */
   --	position: Position;
   --
   --	/**
   --	 * The new name of the symbol. If the given name is not valid the
   --	 * request must return a [ResponseError](#ResponseError) with an
   --	 * appropriate message set.
   --	 */
   --	newName: string;
   --}
   --```
   type RenameParams is record
      textDocument: TextDocumentIdentifier;
      position: LSP.Messages.Position;
      newName: LSP_String;
   end record;

   --```typescript
   --export interface ExecuteCommandParams {
   --
   --	/**
   --	 * The identifier of the actual command handler.
   --	 */
   --	command: string;
   --	/**
   --	 * Arguments that the command should be invoked with.
   --	 */
   --	arguments?: any[];
   --}
   --```
   type ExecuteCommandParams is record
      command: LSP_String;
      arguments: LSP_Any;
   end record;

   type ExecuteCommand_Response is new ResponseMessage with null record;

   --```typescript
   --export interface ApplyWorkspaceEditParams {
   --	/**
   --	 * The edits to apply.
   --	 */
   --	edit: WorkspaceEdit;
   --}
   --```
   type ApplyWorkspaceEditParams is record
      edit: WorkspaceEdit;
   end record;

   --```typescript
   --export interface ApplyWorkspaceEditResponse {
   --	/**
   --	 * Indicates whether the edit was applied or not.
   --	 */
   --	applied: boolean;
   --}
   --```
   type ApplyWorkspaceEditResponse is record
      applied: Boolean;
   end record;

   type PublishDiagnostics_Notification is new NotificationMessage with record
      params : PublishDiagnosticsParams;
   end record;

   type ApplyWorkspaceEdit_Request is new RequestMessage with record
      params : ApplyWorkspaceEditParams;
   end record;

   type Location_Response is new ResponseMessage with record
      result : Location_Vectors.Vector;
   end record;

private

   not overriding procedure Read_ClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ClientCapabilities);

   not overriding procedure Read_CodeActionContext
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CodeActionContext);

   not overriding procedure Read_CodeActionParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CodeActionParams);

   not overriding procedure Read_completion
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out completion);

   not overriding procedure Read_Diagnostic_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Diagnostic_Vector);

   not overriding procedure Read_DidChangeConfigurationParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DidChangeConfigurationParams);

   not overriding procedure Read_DidChangeTextDocumentParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DidChangeTextDocumentParams);

   not overriding procedure Read_DidCloseTextDocumentParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DidCloseTextDocumentParams);

   not overriding procedure Read_DidOpenTextDocumentParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DidOpenTextDocumentParams);

   not overriding procedure Read_DidSaveTextDocumentParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DidSaveTextDocumentParams);

   not overriding procedure Read_DocumentSymbolParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DocumentSymbolParams);

   not overriding procedure Read_documentChanges
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out documentChanges);

   not overriding procedure Read_DocumentLinkOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DocumentLinkOptions);

   not overriding procedure Read_dynamicRegistration
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out dynamicRegistration);

   not overriding procedure Read_ExecuteCommandOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ExecuteCommandOptions);

   not overriding procedure Read_ExecuteCommandParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ExecuteCommandParams);

   not overriding procedure Read_Initialize_Response
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Initialize_Response);

   not overriding procedure Read_InitializeParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out InitializeParams);

   not overriding procedure Read_InitializeResult
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out InitializeResult);

   not overriding procedure Read_Optional_TextDocumentSyncOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Optional_TextDocumentSyncOptions);

   not overriding procedure Read_ReferenceContext
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ReferenceContext);

   not overriding procedure Read_ReferenceParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ReferenceParams);

   not overriding procedure Read_ServerCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ServerCapabilities);

   not overriding procedure Read_synchronization
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out synchronization);

   not overriding procedure Read_TextDocumentClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out TextDocumentClientCapabilities);

   not overriding procedure Read_TextDocumentItem
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out TextDocumentItem);

   not overriding procedure Read_TextDocumentSyncOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out TextDocumentSyncOptions);

   not overriding procedure Read_WorkspaceClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out WorkspaceClientCapabilities);

   not overriding procedure Read_WorkspaceSymbolParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out WorkspaceSymbolParams);

   not overriding procedure Write_ApplyWorkspaceEdit_Request
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ApplyWorkspaceEdit_Request);

   not overriding procedure Write_ApplyWorkspaceEditParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ApplyWorkspaceEditParams);

   not overriding procedure Write_ClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ClientCapabilities);

   not overriding procedure Write_CodeAction_Response
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CodeAction_Response);

   not overriding procedure Write_Command_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Command_Vector);

   not overriding procedure Write_completion
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : completion);

   not overriding procedure Write_Completion_Response
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Completion_Response);

   not overriding procedure Write_CompletionList
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CompletionList);

   not overriding procedure Write_Diagnostic_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Diagnostic_Vector);

   not overriding procedure Write_documentChanges
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : documentChanges);

   not overriding procedure Write_DocumentLinkOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DocumentLinkOptions);

   not overriding procedure Write_dynamicRegistration
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : dynamicRegistration);

   not overriding procedure Write_ExecuteCommand_Response
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ExecuteCommand_Response);

   not overriding procedure Write_ExecuteCommandOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ExecuteCommandOptions);

   not overriding procedure Write_Highlight_Response
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Highlight_Response);

   not overriding procedure Write_Hover
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Hover);

   not overriding procedure Write_Hover_Response
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Hover_Response);

   not overriding procedure Write_Initialize_Request
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Initialize_Request);

   not overriding procedure Write_Initialize_Response
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Initialize_Response);

   not overriding procedure Write_InitializeParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : InitializeParams);

   not overriding procedure Write_InitializeResult
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : InitializeResult);

   not overriding procedure Write_Location_Response
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Location_Response);

   not overriding procedure Write_Optional_TextDocumentSyncOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Optional_TextDocumentSyncOptions);

   not overriding procedure Write_PublishDiagnostics_Notification
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : PublishDiagnostics_Notification);

   not overriding procedure Write_PublishDiagnosticsParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : PublishDiagnosticsParams);

   not overriding procedure Write_ServerCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ServerCapabilities);

   not overriding procedure Write_SignatureHelp
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SignatureHelp);

   not overriding procedure Write_SignatureHelp_Response
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SignatureHelp_Response);

   not overriding procedure Write_Symbol_Response
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Symbol_Response);

   not overriding procedure Write_SymbolInformation_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SymbolInformation_Vector);

   not overriding procedure Write_synchronization
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : synchronization);

   not overriding procedure Write_TextDocumentClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : TextDocumentClientCapabilities);

   not overriding procedure Write_TextDocumentSyncOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : TextDocumentSyncOptions);

   not overriding procedure Write_WorkspaceClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : WorkspaceClientCapabilities);

   not overriding procedure Write_WorkspaceEdit
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : WorkspaceEdit);

   for ApplyWorkspaceEdit_Request'Write use Write_ApplyWorkspaceEdit_Request;
   for ApplyWorkspaceEditParams'Write use Write_ApplyWorkspaceEditParams;
   for ClientCapabilities'Write use Write_ClientCapabilities;
   for CodeAction_Response'Write use Write_CodeAction_Response;
   for Command_Vector'Write use Write_Command_Vector;
   for completion'Write use Write_completion;
   for Completion_Response'Write use Write_Completion_Response;
   for CompletionList'Write use Write_CompletionList;
   for Diagnostic_Vector'Write use Write_Diagnostic_Vector;
   for documentChanges'Write use Write_documentChanges;
   for dynamicRegistration'Write use Write_dynamicRegistration;
   for DocumentLinkOptions'Write use Write_DocumentLinkOptions;
   for ExecuteCommand_Response'Write use Write_ExecuteCommand_Response;
   for ExecuteCommandOptions'Write use Write_ExecuteCommandOptions;
   for Highlight_Response'Write use Write_Highlight_Response;
   for Hover'Write use Write_Hover;
   for Hover_Response'Write use Write_Hover_Response;
   for Initialize_Request'Write use Write_Initialize_Request;
   for Initialize_Response'Write use Write_Initialize_Response;
   for InitializeParams'Write use Write_InitializeParams;
   for InitializeResult'Write use Write_InitializeResult;
   for Location_Response'Write use Write_Location_Response;
   for Optional_TextDocumentSyncOptions'Write use Write_Optional_TextDocumentSyncOptions;
   for PublishDiagnostics_Notification'Write use Write_PublishDiagnostics_Notification;
   for PublishDiagnosticsParams'Write use Write_PublishDiagnosticsParams;
   for ServerCapabilities'Write use Write_ServerCapabilities;
   for SignatureHelp'Write use Write_SignatureHelp;
   for SignatureHelp_Response'Write use Write_SignatureHelp_Response;
   for Symbol_Response'Write use Write_Symbol_Response;
   for SymbolInformation_Vector'Write use Write_SymbolInformation_Vector;
   for synchronization'Write use Write_synchronization;
   for TextDocumentClientCapabilities'Write use Write_TextDocumentClientCapabilities;
   for TextDocumentItem'Read use Read_TextDocumentItem;
   for TextDocumentSyncOptions'Write use Write_TextDocumentSyncOptions;
   for WorkspaceClientCapabilities'Write use Write_WorkspaceClientCapabilities;
   for WorkspaceEdit'Write use Write_WorkspaceEdit;

   for ClientCapabilities'Read use Read_ClientCapabilities;
   for CodeActionContext'Read use Read_CodeActionContext;
   for CodeActionParams'Read use Read_CodeActionParams;
   for completion'Read use Read_completion;
   for Diagnostic_Vector'Read use Read_Diagnostic_Vector;
   for DidChangeConfigurationParams'Read use Read_DidChangeConfigurationParams;
   for DidChangeTextDocumentParams'Read use Read_DidChangeTextDocumentParams;
   for DidCloseTextDocumentParams'Read use Read_DidCloseTextDocumentParams;
   for DidOpenTextDocumentParams'Read use Read_DidOpenTextDocumentParams;
   for DidSaveTextDocumentParams'Read use Read_DidSaveTextDocumentParams;
   for DocumentSymbolParams'Read use Read_DocumentSymbolParams;
   for documentChanges'Read use Read_documentChanges;
   for DocumentLinkOptions'Read use Read_DocumentLinkOptions;
   for dynamicRegistration'Read use Read_dynamicRegistration;
   for ExecuteCommandOptions'Read use Read_ExecuteCommandOptions;
   for ExecuteCommandParams'Read use Read_ExecuteCommandParams;
   for Initialize_Response'Read use Read_Initialize_Response;
   for InitializeParams'Read use Read_InitializeParams;
   for InitializeResult'Read use Read_InitializeResult;
   for Optional_TextDocumentSyncOptions'Read use Read_Optional_TextDocumentSyncOptions;
   for ReferenceContext'Read use Read_ReferenceContext;
   for ReferenceParams'Read use Read_ReferenceParams;
   for ServerCapabilities'Read use Read_ServerCapabilities;
   for synchronization'Read use Read_synchronization;
   for TextDocumentClientCapabilities'Read use Read_TextDocumentClientCapabilities;
   for TextDocumentSyncOptions'Read use Read_TextDocumentSyncOptions;
   for WorkspaceClientCapabilities'Read use Read_WorkspaceClientCapabilities;
   for WorkspaceSymbolParams'Read use Read_WorkspaceSymbolParams;

end LSP.Messages;
