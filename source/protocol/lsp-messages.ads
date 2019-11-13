------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2018-2019, AdaCore                     --
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
--
--  This package provides LSP messages types, request parameters and results
--  types, corresponding encodein/decoding procedures to/from JSON stream.
--
--  We keep original LSP specification in the comments as TypeScript snippet.
--  Some of snippets are out of order, because of forward declaration
--  requirements in Ada.
--
--  See LSP specification for more protocol details.
--

with Ada.Containers.Hashed_Maps;
with Ada.Containers.Multiway_Trees;
with Ada.Streams;

with LSP.Generic_Optional;
with LSP.Generic_Sets;
with LSP.Generic_Vectors;
with LSP.Types; use LSP.Types;

package LSP.Messages is

   pragma Style_Checks ("M175-bcht");
   --  Disable style checks, because some TypeScript snippets are too wide.

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
   --	params?: Array<any> | object;
   --}
   --```
   type RequestMessage is new Message with record
      id: LSP_Number_Or_String;
      method: LSP_String;
   end record;

   procedure Read_RequestMessage
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out RequestMessage);
   for RequestMessage'Read use Read_RequestMessage;

   procedure Write_RequestMessage
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : RequestMessage);
   for RequestMessage'Write use Write_RequestMessage;

   --```typescript
   --interface ResponseMessage extends Message {
   --	/**
   --	 * The request id.
   --	 */
   --	id: number | string | null;
   --
   --	/**
   --	 * The result of a request. This member is REQUIRED on success.
   --	 * This member MUST NOT exist if there was an error invoking the method.
   --	 */
   --	result?: string | number | boolean | object | null;
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
   --	export const ContentModified: number = -32801;
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
      RequestCancelled,
      ContentModified);

   type ResponseError is record
      code: ErrorCodes;
      message: LSP_String;
      data: LSP_Any;
   end record;

   procedure Read_ResponseError
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ResponseError);
   for ResponseError'Read use Read_ResponseError;

   procedure Write_ResponseError
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ResponseError);
   for ResponseError'Write use Write_ResponseError;

   package Optional_ResponseErrors is new LSP.Generic_Optional (ResponseError);
   type Optional_ResponseError is new Optional_ResponseErrors.Optional_Type;

   type ResponseMessage (Is_Error : Boolean) is new Message with record
      id: LSP_Number_Or_String;  --  or null?
      error: Optional_ResponseError (Is_Error);
   end record;

   procedure Read_ResponseMessage
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ResponseMessage);
   for ResponseMessage'Read use Read_ResponseMessage;

   procedure Write_ResponseMessage
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ResponseMessage);

   for ResponseMessage'Write use Write_ResponseMessage;

   procedure Read_Response_Prefix
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.ResponseMessage'Class);

   procedure Write_Response_Prefix
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ResponseMessage'Class);

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
   --	params?: Array<any> | object;
   --}
   --```
   type NotificationMessage is new Message with record
      method: LSP_String;
   end record;

   procedure Read_NotificationMessage
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out NotificationMessage);
   for NotificationMessage'Read use Read_NotificationMessage;

   procedure Write_NotificationMessage
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : NotificationMessage);
   for NotificationMessage'Write use Write_NotificationMessage;

   procedure Read_Notification_Prefix
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.NotificationMessage'Class);

   procedure Write_Notification_Prefix
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.NotificationMessage'Class);

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

   procedure Read_CancelParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CancelParams);
   for CancelParams'Read use Read_CancelParams;

   procedure Write_CancelParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CancelParams);
   for CancelParams'Write use Write_CancelParams;

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
   --	 * Character offset on a line in a document (zero-based). Assuming that the line is
   --	 * represented as a string, the `character` value represents the gap between the
   --	 * `character` and `character + 1`.
   --	 *
   --	 * If the character value is greater than the line length it defaults back to the
   --	 * line length.
   --	 */
   --	character: number;
   --}
   --```
   type Position is record
      line: Line_Number;
      character: UTF_16_Index;
   end record;

   procedure Read_Position
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Position);
   for Position'Read use Read_Position;

   procedure Write_Position
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

   procedure Read_Span
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Span);
   for Span'Read use Read_Span;

   procedure Write_Span
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Span);
   for Span'Write use Write_Span;

   package Optional_Spans is new LSP.Generic_Optional (Span);
   type Optional_Span is new Optional_Spans.Optional_Type;

   type CodeActionKind is
     (Empty,
      QuickFix,
      Refactor, RefactorExtract, RefactorInline, RefactorRewrite,
      Source, SourceOrganizeImports);

   procedure Read_CodeActionKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CodeActionKind);

   procedure Write_CodeActionKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CodeActionKind);

   for CodeActionKind'Read use Read_CodeActionKind;
   for CodeActionKind'Write use Write_CodeActionKind;

   package CodeActionKindSets is
     new LSP.Generic_Sets (CodeActionKind);
   type CodeActionKindSet is new CodeActionKindSets.Set;

   package Optional_CodeActionKindSets is
     new LSP.Generic_Optional (CodeActionKindSet);
   type Optional_CodeActionKindSet is
     new Optional_CodeActionKindSets.Optional_Type;

   --  reference_kinds ALS extension:
   --
   --  export type AlsReferenceKind = 'w' | 'c' | 'd';
   --
   --  export namespace AlsReferenceKind {
   --     export const Write            : AlsReferenceKind = 'w';
   --     export const Static_Call      : AlsReferenceKind = 'c';
   --     export const Dispatching_Call : AlsReferenceKind = 'd';
   --  }

   type AlsReferenceKind is (Write, Static_Call, Dispatching_Call);
   type AlsReferenceKind_Array is array (AlsReferenceKind) of Boolean;
   type AlsReferenceKind_Set (Is_Server_Side : Boolean := True) is record
      case Is_Server_Side is
         when True =>
            As_Flags   : AlsReferenceKind_Array := (others => False);

         when False =>
            As_Strings : LSP.Types.LSP_String_Vector;
      end case;
   end record;
   function Empty_Set return AlsReferenceKind_Set is
      (Is_Server_Side => True, As_Flags => (others => False));

   procedure Read_AlsReferenceKind_Set
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out AlsReferenceKind_Set);
   procedure Write_AlsReferenceKind_Set
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : AlsReferenceKind_Set);
   for AlsReferenceKind_Set'Read use Read_AlsReferenceKind_Set;
   for AlsReferenceKind_Set'Write use Write_AlsReferenceKind_Set;

   package Optional_AlsReferenceKind_Sets is
     new LSP.Generic_Optional (AlsReferenceKind_Set);
   type Optional_AlsReferenceKind_Set is
     new Optional_AlsReferenceKind_Sets.Optional_Type;

   --```typescript
   --interface Location {
   --	uri: DocumentUri;
   --	range: Range;
   --   AlsKind?: AlsReferenceKind[];
   --}
   --```
   --
   --  This type has different representation for server and client sides,
   --  and it is controlled by Is_Server_Side discriminant.

   type Location is record
      uri     : DocumentUri;
      span    : LSP.Messages.Span;  --  range: is reserved word
      alsKind : AlsReferenceKind_Set := Empty_Set;
   end record;

   procedure Read_Location
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Location);
   procedure Write_Location
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Location);
   for Location'Read use Read_Location;
   for Location'Write use Write_Location;

   package Location_Vectors is new LSP.Generic_Vectors (Location);

   type Location_Vector is new Location_Vectors.Vector with null record;

   --```typescript
   --interface LocationLink {
   --
   --	/**
   --	 * Span of the origin of this link.
   --	 *
   --	 * Used as the underlined span for mouse interaction. Defaults to the word range at
   --	 * the mouse position.
   --	 */
   --	originSelectionRange?: Range;
   --
   --	/**
   --	 * The target resource identifier of this link.
   --	 */
   --	targetUri: DocumentUri;
   --
   --	/**
   --	 * The full target range of this link. If the target for example is a symbol then target range is the
   --	 * range enclosing this symbol not including leading/trailing whitespace but everything else
   --	 * like comments. This information is typically used to highlight the range in the editor.
   --	 */
   --	targetRange: Range;
   --
   --	/**
   --	 * The range that should be selected and revealed when this link is being followed, e.g the name of a function.
   --	 * Must be contained by the the `targetRange`. See also `DocumentSymbol#range`
   --	 */
   --	targetSelectionRange: Range;
   --}
   --```
   type LocationLink is record
      originSelectionRange : Optional_Span;
      targetUri            : LSP_String;
      targetRange          : Span;
      targetSelectionRange : Span;
      alsKind              : AlsReferenceKind_Set := Empty_Set;
   end record;

   procedure Read_LocationLink
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LocationLink);
   procedure Write_LocationLink
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LocationLink);
   for LocationLink'Read use Read_LocationLink;
   for LocationLink'Write use Write_LocationLink;

   package LocationLink_Vectors is new LSP.Generic_Vectors (LocationLink);

   type LocationLink_Vector is new LocationLink_Vectors.Vector with null record;

   type Location_Or_Link_Kind is
     (Empty_Vector_Kind,
      Location_Vector_Kind,
      LocationLink_Vector_Kind);

   type Location_Or_Link_Vector
     (Kind : Location_Or_Link_Kind := Empty_Vector_Kind) is
   record
      case Kind is
         when Empty_Vector_Kind =>
            null;
         when Location_Vector_Kind =>
            Locations : Location_Vector;
         when LocationLink_Vector_Kind =>
            LocationLinks : LocationLink_Vector;
      end case;
   end record;

   procedure Read_Location_Or_Link_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Location_Or_Link_Vector);
   procedure Write_Location_Or_Link_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Location_Or_Link_Vector);
   for Location_Or_Link_Vector'Read use Read_Location_Or_Link_Vector;
   for Location_Or_Link_Vector'Write use Write_Location_Or_Link_Vector;

   --
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

   procedure Read_DiagnosticSeverity
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DiagnosticSeverity);
   procedure Write_DiagnosticSeverity
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DiagnosticSeverity);
   for DiagnosticSeverity'Read use Read_DiagnosticSeverity;
   for DiagnosticSeverity'Write use Write_DiagnosticSeverity;

   package Optional_DiagnosticSeveritys is new LSP.Generic_Optional (DiagnosticSeverity);
   type Optional_DiagnosticSeverity is new Optional_DiagnosticSeveritys.Optional_Type;

   --```typescript
   --/**
   -- * Represents a related message and source code location for a diagnostic. This should be
   -- * used to point to code locations that cause or related to a diagnostics, e.g when duplicating
   -- * a symbol in a scope.
   -- */
   --export interface DiagnosticRelatedInformation {
   --	/**
   --	 * The location of this related diagnostic information.
   --	 */
   --	location: Location;
   --
   --	/**
   --	 * The message of this related diagnostic information.
   --	 */
   --	message: string;
   --}
   --```
   type DiagnosticRelatedInformation is record
      location: LSP.Messages.Location;
      message: LSP_String;
   end record;

   procedure Read_DiagnosticRelatedInformation
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DiagnosticRelatedInformation);
   for DiagnosticRelatedInformation'Read use Read_DiagnosticRelatedInformation;

   procedure Write_DiagnosticRelatedInformation
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DiagnosticRelatedInformation);
   for DiagnosticRelatedInformation'Write
     use Write_DiagnosticRelatedInformation;

   package DiagnosticRelatedInformation_Vectors is new LSP.Generic_Vectors
     (DiagnosticRelatedInformation);

   type DiagnosticRelatedInformation_Vector is
     new DiagnosticRelatedInformation_Vectors.Vector with null record;

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
   --	 * The diagnostic's code, which might appear in the user interface.
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
   --
   --	/**
   --	 * An array of related diagnostic information, e.g. when symbol-names within
   --	 * a scope collide all definitions can be marked via this property.
   --	 */
   --	relatedInformation?: DiagnosticRelatedInformation[];
   --}
   --```
   type Diagnostic is record
      span: LSP.Messages.Span;
      severity: Optional_DiagnosticSeverity;
      code: LSP_Number_Or_String;
      source: Optional_String;
      message: LSP_String;
      relatedInformation: DiagnosticRelatedInformation_Vector;
   end record;

   procedure Read_Diagnostic
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Diagnostic);
   for Diagnostic'Read use Read_Diagnostic;

   procedure Write_Diagnostic
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Diagnostic);
   for Diagnostic'Write use Write_Diagnostic;

   package Diagnostic_Vectors is new LSP.Generic_Vectors (Diagnostic);

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

   procedure Read_Command
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Command);
   procedure Write_Command
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Command);
   for Command'Read use Read_Command;
   for Command'Write use Write_Command;

   package Command_Vectors is new LSP.Generic_Vectors (Command);

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

   procedure Read_TextEdit
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out TextEdit);
   for TextEdit'Read use Read_TextEdit;

   procedure Write_TextEdit
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : TextEdit);
   for TextEdit'Write use Write_TextEdit;

   package Optional_TextEdits is new LSP.Generic_Optional (TextEdit);
   type Optional_TextEdit is new Optional_TextEdits.Optional_Type;

   package TextEdit_Vectors is new LSP.Generic_Vectors (TextEdit);
   type TextEdit_Vector is new TextEdit_Vectors.Vector with null record;

   --
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

   procedure Read_TextDocumentIdentifier
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out TextDocumentIdentifier);
   procedure Write_TextDocumentIdentifier
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : TextDocumentIdentifier);
   for TextDocumentIdentifier'Read use Read_TextDocumentIdentifier;
   for TextDocumentIdentifier'Write use Write_TextDocumentIdentifier;

   --
   --```typescript
   --interface VersionedTextDocumentIdentifier extends TextDocumentIdentifier {
   --	/**
   --	 * The version number of this document. If a versioned text document identifier
   --	 * is sent from the server to the client and the file is not open in the editor
   --	 * (the server has not received an open notification before) the server can send
   --	 * `null` to indicate that the version is known and the content on disk is the
   --	 * truth (as speced with document content ownership).
   --	 *
   --	 * The version number of a document will increase after each change, including
   --	 * undo/redo. The number doesn't need to be consecutive.
   --	 */
   --	version: number | null;
   --}
   --```
   type VersionedTextDocumentIdentifier is new TextDocumentIdentifier with record
      version: Optional_Number;
   end record;

   procedure Read_VersionedTextDocumentIdentifier
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out VersionedTextDocumentIdentifier);
   for VersionedTextDocumentIdentifier'Read use
     Read_VersionedTextDocumentIdentifier;

   procedure Write_VersionedTextDocumentIdentifier
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

   procedure Read_TextDocumentEdit
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out TextDocumentEdit);
   for TextDocumentEdit'Read use Read_TextDocumentEdit;

   procedure Write_TextDocumentEdit
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : TextDocumentEdit);
   for TextDocumentEdit'Write use Write_TextDocumentEdit;

   package TextDocumentEdit_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => DocumentUri,
      Element_Type    => TextEdit_Vector,
      Hash            => LSP.Types.Hash,
      Equivalent_Keys => LSP.Types."=");

   --```typescript
   --/**
   -- * Options to create a file.
   -- */
   --export interface CreateFileOptions {
   --	/**
   --	 * Overwrite existing file. Overwrite wins over `ignoreIfExists`
   --	 */
   --	overwrite?: boolean;
   --	/**
   --	 * Ignore if exists.
   --	 */
   --	ignoreIfExists?: boolean;
   --}
   --
   --/**
   -- * Create file operation
   -- */
   --export interface CreateFile {
   --	/**
   --	 * A create
   --	 */
   --	kind: 'create';
   --	/**
   --	 * The resource to create.
   --	 */
   --	uri: DocumentUri;
   --	/**
   --	 * Additional options
   --	 */
   --	options?: CreateFileOptions;
   --}
   --
   --/**
   -- * Rename file options
   -- */
   --export interface RenameFileOptions {
   --	/**
   --	 * Overwrite target if existing. Overwrite wins over `ignoreIfExists`
   --	 */
   --	overwrite?: boolean;
   --	/**
   --	 * Ignores if target exists.
   --	 */
   --	ignoreIfExists?: boolean;
   --}
   --
   --/**
   -- * Rename file operation
   -- */
   --export interface RenameFile {
   --	/**
   --	 * A rename
   --	 */
   --	kind: 'rename';
   --	/**
   --	 * The old (existing) location.
   --	 */
   --	oldUri: DocumentUri;
   --	/**
   --	 * The new location.
   --	 */
   --	newUri: DocumentUri;
   --	/**
   --	 * Rename options.
   --	 */
   --	options?: RenameFileOptions;
   --}
   --
   --/**
   -- * Delete file options
   -- */
   --export interface DeleteFileOptions {
   --	/**
   --	 * Delete the content recursively if a folder is denoted.
   --	 */
   --	recursive?: boolean;
   --	/**
   --	 * Ignore the operation if the file doesn't exist.
   --	 */
   --	ignoreIfNotExists?: boolean;
   --}
   --
   --/**
   -- * Delete file operation
   -- */
   --export interface DeleteFile {
   --	/**
   --	 * A delete
   --	 */
   --	kind: 'delete';
   --	/**
   --	 * The file to delete.
   --	 */
   --	uri: DocumentUri;
   --	/**
   --	 * Delete options.
   --	 */
   --	options?: DeleteFileOptions;
   --}
   --```

   type CreateFileOptions is record
      overwrite      : Optional_Boolean;
      ignoreIfExists : Optional_Boolean;
   end record;

   type CreateFile is record
      uri     : DocumentUri;
      options : CreateFileOptions;
   end record;

   type RenameFileOptions is record
      overwrite      : Optional_Boolean;
      ignoreIfExists : Optional_Boolean;
   end record;

   type RenameFile is record
      oldUri  : DocumentUri;
      newUri  : DocumentUri;
      options : RenameFileOptions;
   end record;

   type DeleteFileOptions is record
      recursive         : Optional_Boolean;
      ignoreIfNotExists : Optional_Boolean;
   end record;

   type DeleteFile is record
      uri     : DocumentUri;
      options : DeleteFileOptions;
   end record;

   type Document_Change_Kind is
     (Text_Document_Edit, Create_File, Rename_File, Delete_File);

   type Document_Change
     (Kind : Document_Change_Kind := Text_Document_Edit) is record
      case Kind is
         when Text_Document_Edit =>
            Text_Document_Edit : TextDocumentEdit;
         when Create_File =>
            Create_File : CreateFile;
         when Rename_File =>
            Rename_File : RenameFile;
         when Delete_File =>
            Delete_File : DeleteFile;
      end case;
   end record;

   procedure Read_Document_Change
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Document_Change);
   for Document_Change'Read use Read_Document_Change;

   procedure Write_Document_Change
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Document_Change);
   for Document_Change'Write use Write_Document_Change;

   package Document_Change_Vectors is
     new LSP.Generic_Vectors (Document_Change);

   type Document_Change_Vector is
     new Document_Change_Vectors.Vector with null record;

   --```typescript
   --export interface WorkspaceEdit {
   --	/**
   --	 * Holds changes to existing resources.
   --	 */
   --	changes?: { [uri: DocumentUri]: TextEdit[]; };
   --
   --	/**
   --	 * Depending on the client capability `workspace.workspaceEdit.resourceOperations` document changes
   --	 * are either an array of `TextDocumentEdit`s to express changes to n different text documents
   --	 * where each text document edit addresses a specific version of a text document. Or it can contain
   --	 * above `TextDocumentEdit`s mixed with create, rename and delete file / folder operations.
   --	 *
   --	 * Whether a client supports versioned document edits is expressed via
   --	 * `workspace.workspaceEdit.documentChanges` client capability.
   --	 *
   --	 * If a client neither supports `documentChanges` nor `workspace.workspaceEdit.resourceOperations` then
   --	 * only plain `TextEdit`s using the `changes` property are supported.
   --	 */
   --	documentChanges?: (TextDocumentEdit[] | (TextDocumentEdit | CreateFile | RenameFile | DeleteFile)[]);
   --}
   --```
   type WorkspaceEdit is record
      changes: TextDocumentEdit_Maps.Map;
      documentChanges: Document_Change_Vector;
   end record;

   procedure Read_WorkspaceEdit
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out WorkspaceEdit);
   for WorkspaceEdit'Read use Read_WorkspaceEdit;

   procedure Write_WorkspaceEdit
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : WorkspaceEdit);
   for WorkspaceEdit'Write use Write_WorkspaceEdit;

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

   procedure Read_TextDocumentItem
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out TextDocumentItem);

   procedure Write_TextDocumentItem
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : TextDocumentItem);

   for TextDocumentItem'Read use Read_TextDocumentItem;
   for TextDocumentItem'Write use Write_TextDocumentItem;

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

   procedure Read_TextDocumentPositionParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out TextDocumentPositionParams);
   procedure Write_TextDocumentPositionParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : TextDocumentPositionParams);
   for TextDocumentPositionParams'Read use Read_TextDocumentPositionParams;
   for TextDocumentPositionParams'Write use Write_TextDocumentPositionParams;

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
   --	 *
   --	 * Glob patterns can have the following syntax:
   --	 * - `*` to match one or more characters in a path segment
   --	 * - `?` to match on one character in a path segment
   --	 * - `**` to match any number of path segments, including none
   --	 * - `{}` to group conditions (e.g. `**​/*.{ts,js}` matches all TypeScript and JavaScript files)
   --	 * - `[]` to declare a range of characters to match in a path segment (e.g., `example.[0-9]` to match on `example.0`, `example.1`, …)
   --	 * - `[!...]` to negate a range of characters to match in a path segment (e.g., `example.[!0-9]` to match on `example.a`, `example.b`, but not `example.0`)
   --	 */
   --	pattern?: string;
   --}
   --```
   type DocumentFilter is record
      language: LSP.Types.Optional_String;
      scheme: LSP.Types.Optional_String;
      pattern: LSP.Types.Optional_String;
   end record;

   package DocumentFilter_Vectors is new LSP.Generic_Vectors (DocumentFilter);
   --```typescript
   --export type DocumentSelector = DocumentFilter[];
   --```
   type DocumentSelector is new DocumentFilter_Vectors.Vector with null record;

   type dynamicRegistration is new Optional_Boolean;

   procedure Read_dynamicRegistration
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out dynamicRegistration);

   procedure Write_dynamicRegistration
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : dynamicRegistration);

   for dynamicRegistration'Read use Read_dynamicRegistration;
   for dynamicRegistration'Write use Write_dynamicRegistration;

   --
   --```typescript
   --
   --/**
   -- * The kind of resource operations supported by the client.
   -- */
   --export type ResourceOperationKind = 'create' | 'rename' | 'delete';
   --
   --export namespace ResourceOperationKind {
   --
   --	/**
   --	 * Supports creating new files and folders.
   --	 */
   --	export const Create: ResourceOperationKind = 'create';
   --
   --	/**
   --	 * Supports renaming existing files and folders.
   --	 */
   --	export const Rename: ResourceOperationKind = 'rename';
   --
   --	/**
   --	 * Supports deleting existing files and folders.
   --	 */
   --	export const Delete: ResourceOperationKind = 'delete';
   --}
   --
   --export type FailureHandlingKind = 'abort' | 'transactional' | 'undo' | 'textOnlyTransactional';
   --
   --export namespace FailureHandlingKind {
   --
   --	/**
   --	 * Applying the workspace change is simply aborted if one of the changes provided
   --	 * fails. All operations executed before the failing operation stay executed.
   --	 */
   --	export const Abort: FailureHandlingKind = 'abort';
   --
   --	/**
   --	 * All operations are executed transactionally. That means they either all
   --	 * succeed or no changes at all are applied to the workspace.
   --	 */
   --	export const Transactional: FailureHandlingKind = 'transactional';
   --
   --
   --	/**
   --	 * If the workspace edit contains only textual file changes they are executed transactionally.
   --	 * If resource changes (create, rename or delete file) are part of the change the failure
   --	 * handling strategy is abort.
   --	 */
   --	export const TextOnlyTransactional: FailureHandlingKind = 'textOnlyTransactional';
   --
   --	/**
   --	 * The client tries to undo the operations already executed. But there is no
   --	 * guarantee that this succeeds.
   --	 */
   --	export const Undo: FailureHandlingKind = 'undo';
   --}
   --
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
   --
   --		/**
   --		 * The resource operations the client supports. Clients should at least
   --		 * support 'create', 'rename' and 'delete' files and folders.
   --		 */
   --		resourceOperations?: ResourceOperationKind[];
   --
   --		/**
   --		 * The failure handling strategy of a client if applying the workspace edit
   --		 * fails.
   --		 */
   --		failureHandling?: FailureHandlingKind;
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
   --		 * Did change watched files notification supports dynamic registration. Please note
   --		 * that the current protocol doesn't support static configuration for file changes
   --		 * from the server side.
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
   --
   --		/**
   --		 * Specific capabilities for the `SymbolKind` in the `workspace/symbol` request.
   --		 */
   --		symbolKind?: {
   --			/**
   --			 * The symbol kind values the client supports. When this
   --			 * property exists the client also guarantees that it will
   --			 * handle values outside its set gracefully and falls back
   --			 * to a default value when unknown.
   --			 *
   --			 * If this property is not present the client only supports
   --			 * the symbol kinds from `File` to `Array` as defined in
   --			 * the initial version of the protocol.
   --			 */
   --			valueSet?: SymbolKind[];
   --		}
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
   --
   --	/**
   --	 * The client has support for workspace folders.
   --	 *
   --	 * Since 3.6.0
   --	 */
   --	workspaceFolders?: boolean;
   --
   --	/**
   --	 * The client supports `workspace/configuration` requests.
   --	 *
   --	 * Since 3.6.0
   --	 */
   --	configuration?: boolean;
   --}
   --```

   type ResourceOperationKind is (create, rename, delete);

   procedure Read_ResourceOperationKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ResourceOperationKind);

   procedure Write_ResourceOperationKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ResourceOperationKind);

   for ResourceOperationKind'Read use Read_ResourceOperationKind;
   for ResourceOperationKind'Write use Write_ResourceOperationKind;

   package ResourceOperationKindSets is
     new LSP.Generic_Sets (ResourceOperationKind);

   type ResourceOperationKindSet is new ResourceOperationKindSets.Set;

   package Optional_ResourceOperationKindSets is
     new LSP.Generic_Optional (ResourceOperationKindSet);

   type Optional_ResourceOperationKindSet is
     new Optional_ResourceOperationKindSets.Optional_Type;

   type FailureHandlingKind is
     (abortApplying,  --  'abort' is reserver word in Ada, so change it
      transactional, undo, textOnlyTransactional);

   package Optional_FailureHandlingKinds is
     new LSP.Generic_Optional (FailureHandlingKind);

   type Optional_FailureHandlingKind is
     new Optional_FailureHandlingKinds.Optional_Type;

   type documentChanges is record
      documentChanges : Optional_Boolean;
      resourceOperations : Optional_ResourceOperationKindSet;
      failureHandling : Optional_FailureHandlingKind;
   end record;

   procedure Read_documentChanges
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out documentChanges);

   procedure Write_documentChanges
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : documentChanges);

   for documentChanges'Read use Read_documentChanges;
   for documentChanges'Write use Write_documentChanges;

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
      An_Array,
      Object,
      Key,
      A_Null,
      EnumMember,
      Struct,
      Event,
      Operator,
      TypeParameter);

   procedure Read_SymbolKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out SymbolKind);

   procedure Write_SymbolKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SymbolKind);

   for SymbolKind'Read use Read_SymbolKind;
   for SymbolKind'Write use Write_SymbolKind;

   package SymbolKindSets is new LSP.Generic_Sets (SymbolKind);

   type SymbolKindSet is new SymbolKindSets.Set;

   Default_SymbolKindSet : constant SymbolKindSet :=
     To_Set (From => File, To => An_Array);

   package Optional_SymbolKindSets is
     new LSP.Generic_Optional (SymbolKindSet);

   type Optional_SymbolKindSet is
     new Optional_SymbolKindSets.Optional_Type;

   type Workspace_Symbol_Capability is record
      dynamicRegistration: Optional_Boolean;
      symbolKind: Optional_SymbolKindSet;
   end record;

   procedure Read_Workspace_Symbol_Capability
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Workspace_Symbol_Capability);

   procedure Write_Workspace_Symbol_Capability
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Workspace_Symbol_Capability);

   for Workspace_Symbol_Capability'Read use Read_Workspace_Symbol_Capability;
   for Workspace_Symbol_Capability'Write use Write_Workspace_Symbol_Capability;

   package Optional_Workspace_Symbol_Capabilities is
     new LSP.Generic_Optional (Workspace_Symbol_Capability);

   type Optional_Workspace_Symbol_Capability is
     new Optional_Workspace_Symbol_Capabilities.Optional_Type;

   type WorkspaceClientCapabilities is record
      applyEdit: Optional_Boolean;
      workspaceEdit: documentChanges;
      didChangeConfiguration: dynamicRegistration;
      didChangeWatchedFiles: dynamicRegistration;
      symbol: Optional_Workspace_Symbol_Capability;
      executeCommand: dynamicRegistration;
      workspaceFolders: Optional_Boolean;
      configuration: Optional_Boolean;
   end record;

   procedure Read_WorkspaceClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out WorkspaceClientCapabilities);

   procedure Write_WorkspaceClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : WorkspaceClientCapabilities);

   for WorkspaceClientCapabilities'Read use Read_WorkspaceClientCapabilities;
   for WorkspaceClientCapabilities'Write use Write_WorkspaceClientCapabilities;

   --```typescript
   --/**
   -- * Describes the content type that a client supports in various
   -- * result literals like `Hover`, `ParameterInfo` or `CompletionItem`.
   -- *
   -- * Please note that `MarkupKinds` must not start with a `$`. This kinds
   -- * are reserved for internal usage.
   -- */
   --export namespace MarkupKind {
   --	/**
   --	 * Plain text is supported as a content format
   --	 */
   --	export const PlainText: 'plaintext' = 'plaintext';
   --
   --	/**
   --	 * Markdown is supported as a content format
   --	 */
   --	export const Markdown: 'markdown' = 'markdown';
   --}
   --export type MarkupKind = 'plaintext' | 'markdown';
   --
   --/**
   -- * A `MarkupContent` literal represents a string value which content is interpreted base on its
   -- * kind flag. Currently the protocol supports `plaintext` and `markdown` as markup kinds.
   -- *
   -- * If the kind is `markdown` then the value can contain fenced code blocks like in GitHub issues.
   -- * See https://help.github.com/articles/creating-and-highlighting-code-blocks/#syntax-highlighting
   -- *
   -- * Here is an example how such a string can be constructed using JavaScript / TypeScript:
   -- * ```typescript
   -- * let markdown: MarkdownContent = {
   -- *  kind: MarkupKind.Markdown,
   -- *	value: [
   -- *		'# Header',
   -- *		'Some text',
   -- *		'```typescript',
   -- *		'someCode();',
   -- *		'```'
   -- *	].join('\n')
   -- * };
   -- * ```
   -- *
   -- * *Please Note* that clients might sanitize the return markdown. A client could decide to
   -- * remove HTML from the markdown to avoid script execution.
   -- */
   --export interface MarkupContent {
   --	/**
   --	 * The type of the Markup
   --	 */
   --	kind: MarkupKind;
   --
   --	/**
   --	 * The content itself
   --	 */
   --	value: string;
   --}
   --```

   type MarkupKind is (plaintext, markdown);

   procedure Read_MarkupKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out MarkupKind);

   procedure Write_MarkupKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : MarkupKind);
   for MarkupKind'Read use Read_MarkupKind;
   for MarkupKind'Write use Write_MarkupKind;

   package MarkupKind_Vectors is new LSP.Generic_Vectors (MarkupKind);
   type MarkupKind_Vector is new MarkupKind_Vectors.Vector with null record;

   package Optional_MarkupKind_Vectors is
     new LSP.Generic_Optional (MarkupKind_Vector);

   type Optional_MarkupKind_Vector is
     new Optional_MarkupKind_Vectors.Optional_Type;

   type MarkupContent is record
      kind  : MarkupKind;
      value : LSP_String;
   end record;

   procedure Read_MarkupContent
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out MarkupContent);

   procedure Write_MarkupContent
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : MarkupContent);
   for MarkupContent'Read use Read_MarkupContent;
   for MarkupContent'Write use Write_MarkupContent;

   type String_Or_MarkupContent (Is_String : Boolean := False) is record
      case Is_String is
         when True =>
            String : LSP_String;
         when False =>
            Content : MarkupContent;
      end case;
   end record;

   procedure Read_String_Or_MarkupContent
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out String_Or_MarkupContent);
   procedure Write_String_Or_MarkupContent
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : String_Or_MarkupContent);
   for String_Or_MarkupContent'Read use Read_String_Or_MarkupContent;
   for String_Or_MarkupContent'Write use Write_String_Or_MarkupContent;

   package Optional_String_Or_MarkupContent_Package is
     new LSP.Generic_Optional (String_Or_MarkupContent);

   type Optional_String_Or_MarkupContent is
     new Optional_String_Or_MarkupContent_Package.Optional_Type;

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
   --			 * The client supports snippets as insert text.
   --			 *
   --			 * A snippet can define tab stops and placeholders with `$1`, `$2`
   --			 * and `${3:foo}`. `$0` defines the final tab stop, it defaults to
   --			 * the end of the snippet. Placeholders with equal identifiers are linked,
   --			 * that is typing in one will update others too.
   --			 */
   --			snippetSupport?: boolean;
   --
   --			/**
   --			 * The client supports commit characters on a completion item.
   --			 */
   --			commitCharactersSupport?: boolean
   --
   --			/**
   --			 * The client supports the following content formats for the documentation
   --			 * property. The order describes the preferred format of the client.
   --			 */
   --			documentationFormat?: MarkupKind[];
   --
   --			/**
   --			 * The client supports the deprecated property on a completion item.
   --			 */
   --			deprecatedSupport?: boolean;
   --
   --			/**
   --			 * The client supports the preselect property on a completion item.
   --			 */
   --			preselectSupport?: boolean;
   --		}
   --
   --		completionItemKind?: {
   --			/**
   --			 * The completion item kind values the client supports. When this
   --			 * property exists the client also guarantees that it will
   --			 * handle values outside its set gracefully and falls back
   --			 * to a default value when unknown.
   --			 *
   --			 * If this property is not present the client only supports
   --			 * the completion items kinds from `Text` to `Reference` as defined in
   --			 * the initial version of the protocol.
   --			 */
   --			valueSet?: CompletionItemKind[];
   --		},
   --
   --		/**
   --		 * The client supports to send additional context information for a
   --		 * `textDocument/completion` request.
   --		 */
   --		contextSupport?: boolean;
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
   --
   --		/**
   --		 * The client supports the follow content formats for the content
   --		 * property. The order describes the preferred format of the client.
   --		 */
   --		contentFormat?: MarkupKind[];
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
   --
   --		/**
   --		 * The client supports the following `SignatureInformation`
   --		 * specific properties.
   --		 */
   --		signatureInformation?: {
   --			/**
   --			 * The client supports the follow content formats for the documentation
   --			 * property. The order describes the preferred format of the client.
   --			 */
   --			documentationFormat?: MarkupKind[];
   --
   --			/**
   --			 * Client capabilities specific to parameter information.
   --			 */
   --			parameterInformation?: {
   --				/**
   --				 * The client supports processing label offsets instead of a
   --				 * simple label string.
   --				 *
   --				 * Since 3.14.0
   --				 */
   --				labelOffsetSupport?: boolean;
   --			}
   --		};
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
   --
   --		/**
   --		 * Specific capabilities for the `SymbolKind`.
   --		 */
   --		symbolKind?: {
   --			/**
   --			 * The symbol kind values the client supports. When this
   --			 * property exists the client also guarantees that it will
   --			 * handle values outside its set gracefully and falls back
   --			 * to a default value when unknown.
   --			 *
   --			 * If this property is not present the client only supports
   --			 * the symbol kinds from `File` to `Array` as defined in
   --			 * the initial version of the protocol.
   --			 */
   --			valueSet?: SymbolKind[];
   --		}
   --
   --		/**
   --		 * The client supports hierarchical document symbols.
   --		 */
   --		hierarchicalDocumentSymbolSupport?: boolean;
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
   --		* Capabilities specific to the `textDocument/declaration`
   --		*/
   --	declaration?: {
   --		/**
   --		 * Whether declaration supports dynamic registration. If this is set to `true`
   --		 * the client supports the new `(TextDocumentRegistrationOptions & StaticRegistrationOptions)`
   --		 * return value for the corresponding server capability as well.
   --		 */
   --		dynamicRegistration?: boolean;
   --
   --		/**
   --		 * The client supports additional metadata in the form of declaration links.
   --		 *
   --		 * Since 3.14.0
   --		 */
   --		linkSupport?: boolean;
   --	};
   --
   --	/**
   --	 * Capabilities specific to the `textDocument/definition`.
   --	 *
   --	 * Since 3.14.0
   --	 */
   --	definition?: {
   --		/**
   --		 * Whether definition supports dynamic registration.
   --		 */
   --		dynamicRegistration?: boolean;
   --
   --		/**
   --		 * The client supports additional metadata in the form of definition links.
   --		 */
   --		linkSupport?: boolean;
   --	};
   --
   --	/**
   --	 * Capabilities specific to the `textDocument/typeDefinition`
   --	 *
   --	 * Since 3.6.0
   --	 */
   --	typeDefinition?: {
   --		/**
   --		 * Whether typeDefinition supports dynamic registration. If this is set to `true`
   --		 * the client supports the new `(TextDocumentRegistrationOptions & StaticRegistrationOptions)`
   --		 * return value for the corresponding server capability as well.
   --		 */
   --		dynamicRegistration?: boolean;
   --
   --		/**
   --		 * The client supports additional metadata in the form of definition links.
   --		 *
   --		 * Since 3.14.0
   --		 */
   --		linkSupport?: boolean;
   --	};
   --
   --	/**
   --	 * Capabilities specific to the `textDocument/implementation`.
   --	 *
   --	 * Since 3.6.0
   --	 */
   --	implementation?: {
   --		/**
   --		 * Whether implementation supports dynamic registration. If this is set to `true`
   --		 * the client supports the new `(TextDocumentRegistrationOptions & StaticRegistrationOptions)`
   --		 * return value for the corresponding server capability as well.
   --		 */
   --		dynamicRegistration?: boolean;
   --
   --		/**
   --		 * The client supports additional metadata in the form of definition links.
   --		 *
   --		 * Since 3.14.0
   --		 */
   --		linkSupport?: boolean;
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
   --		/**
   --		 * The client support code action literals as a valid
   --		 * response of the `textDocument/codeAction` request.
   --		 *
   --		 * Since 3.8.0
   --		 */
   --		codeActionLiteralSupport?: {
   --			/**
   --			 * The code action kind is support with the following value
   --			 * set.
   --			 */
   --			codeActionKind: {
   --
   --				/**
   --				 * The code action kind values the client supports. When this
   --				 * property exists the client also guarantees that it will
   --				 * handle values outside its set gracefully and falls back
   --				 * to a default value when unknown.
   --				 */
   --				valueSet: CodeActionKind[];
   --			};
   --		};
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
   --	 * Capabilities specific to the `textDocument/documentColor` and the
   --	 * `textDocument/colorPresentation` request.
   --	 *
   --	 * Since 3.6.0
   --	 */
   --	colorProvider?: {
   --		/**
   --		 * Whether colorProvider supports dynamic registration. If this is set to `true`
   --		 * the client supports the new `(ColorProviderOptions & TextDocumentRegistrationOptions & StaticRegistrationOptions)`
   --		 * return value for the corresponding server capability as well.
   --		 */
   --		dynamicRegistration?: boolean;
   --	}
   --
   --	/**
   --	 * Capabilities specific to the `textDocument/rename`
   --	 */
   --	rename?: {
   --		/**
   --		 * Whether rename supports dynamic registration.
   --		 */
   --		dynamicRegistration?: boolean;
   --		/**
   --		 * The client supports testing for validity of rename operations
   --		 * before execution.
   --		 */
   --		prepareSupport?: boolean;
   --	};
   --
   --	/**
   --	 * Capabilities specific to `textDocument/publishDiagnostics`.
   --	 */
   --	publishDiagnostics?: {
   --		/**
   --		 * Whether the clients accepts diagnostics with related information.
   --		 */
   --		relatedInformation?: boolean;
   --	};
   --	/**
   --	 * Capabilities specific to `textDocument/foldingRange` requests.
   --	 *
   --	 * Since 3.10.0
   --	 */
   --	foldingRange?: {
   --		/**
   --		 * Whether implementation supports dynamic registration for folding range providers. If this is set to `true`
   --		 * the client supports the new `(FoldingRangeProviderOptions & TextDocumentRegistrationOptions & StaticRegistrationOptions)`
   --		 * return value for the corresponding server capability as well.
   --		 */
   --		dynamicRegistration?: boolean;
   --		/**
   --		 * The maximum number of folding ranges that the client prefers to receive per document. The value serves as a
   --		 * hint, servers are free to follow the limit.
   --		 */
   --		rangeLimit?: number;
   --		/**
   --		 * If set, the client signals that it only supports folding complete lines. If set, client will
   --		 * ignore specified `startCharacter` and `endCharacter` properties in a FoldingRange.
   --		 */
   --		lineFoldingOnly?: boolean;
   --	};
   --}
   --```
   type synchronization is record
      dynamicRegistration : Optional_Boolean;
      willSave : Optional_Boolean;
      willSaveWaitUntil : Optional_Boolean;
      didSave : Optional_Boolean;
   end record;

   procedure Read_synchronization
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out synchronization);

   procedure Write_synchronization
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : synchronization);

   for synchronization'Read use Read_synchronization;
   for synchronization'Write use Write_synchronization;

   type completionItemCapability is record
      snippetSupport : Optional_Boolean;
      commitCharactersSupport : Optional_Boolean;
      documentationFormat : MarkupKind_Vector;
      deprecatedSupport : Optional_Boolean;
      preselectSupport : Optional_Boolean;
   end record;

   procedure Read_completionItemCapability
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out completionItemCapability);

   procedure Write_completionItemCapability
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : completionItemCapability);

   for completionItemCapability'Read use Read_completionItemCapability;
   for completionItemCapability'Write use Write_completionItemCapability;

   package Optional_completionItemCapabilities is
     new LSP.Generic_Optional (completionItemCapability);

   type Optional_completionItemCapability is
     new Optional_completionItemCapabilities.Optional_Type;

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
      Reference,
      Folder,
      EnumMember,
      A_Constant,
      Struct,
      Event,
      Operator,
      TypeParameter);

   procedure Read_CompletionItemKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CompletionItemKind);
   procedure Write_CompletionItemKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CompletionItemKind);
   for CompletionItemKind'Read use Read_CompletionItemKind;
   for CompletionItemKind'Write use Write_CompletionItemKind;

   package CompletionItemKindSets is new LSP.Generic_Sets (CompletionItemKind);

   type CompletionItemKindSet is new CompletionItemKindSets.Set;

   Default_CompletionItemKindSet : constant CompletionItemKindSet :=
     To_Set (From => Text, To => Reference);

   package Optional_CompletionItemKindSets is
     new LSP.Generic_Optional (CompletionItemKindSet);

   type Optional_CompletionItemKindSet is
     new Optional_CompletionItemKindSets.Optional_Type;

   type completion is record
      dynamicRegistration : Optional_Boolean;
      completionItem : Optional_completionItemCapability;
      completionItemKind : Optional_CompletionItemKindSet;
      contextSupport : Optional_Boolean;
   end record;

   procedure Read_completion
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out completion);

   procedure Write_completion
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : completion);

   for completion'Read use Read_completion;
   for completion'Write use Write_completion;

   type Hover_Capability is record
      dynamicRegistration: Optional_Boolean;
      contentFormat: Optional_MarkupKind_Vector;
   end record;

   procedure Read_Hover_Capability
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Hover_Capability);

   procedure Write_Hover_Capability
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Hover_Capability);

   for Hover_Capability'Read use Read_Hover_Capability;
   for Hover_Capability'Write use Write_Hover_Capability;

   package Optional_Hover_Capabilities is
     new LSP.Generic_Optional (Hover_Capability);

   type Optional_Hover_Capability is
     new Optional_Hover_Capabilities.Optional_Type;

   type parameterInformation_Capability is record
      labelOffsetSupport: Optional_Boolean;
   end record;

   procedure Read_parameterInformation_Capability
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out parameterInformation_Capability);

   procedure Write_parameterInformation_Capability
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : parameterInformation_Capability);

   for parameterInformation_Capability'Read
     use Read_parameterInformation_Capability;
   for parameterInformation_Capability'Write
     use Write_parameterInformation_Capability;

   package Optional_parameterInformation_Capabilities is
     new LSP.Generic_Optional (parameterInformation_Capability);

   type Optional_parameterInformation_Capability is
     new Optional_parameterInformation_Capabilities.Optional_Type;

   type signatureInformation_Capability is record
      documentationFormat: Optional_MarkupKind_Vector;
      parameterInformation: Optional_parameterInformation_Capability;
   end record;

   procedure Read_signatureInformation_Capability
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out signatureInformation_Capability);

   procedure Write_signatureInformation_Capability
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : signatureInformation_Capability);

   for signatureInformation_Capability'Read
     use Read_signatureInformation_Capability;
   for signatureInformation_Capability'Write
     use Write_signatureInformation_Capability;

   package Optional_signatureInformation_Capabilities is
     new LSP.Generic_Optional (signatureInformation_Capability);

   type Optional_signatureInformation_Capability is
     new Optional_signatureInformation_Capabilities.Optional_Type;

   type signatureHelp_Capability is record
      dynamicRegistration: Optional_Boolean;
      signatureInformation: Optional_signatureInformation_Capability;
   end record;

   procedure Read_signatureHelp_Capability
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out signatureHelp_Capability);

   procedure Write_signatureHelp_Capability
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : signatureHelp_Capability);

   for signatureHelp_Capability'Read use Read_signatureHelp_Capability;
   for signatureHelp_Capability'Write use Write_signatureHelp_Capability;

   package Optional_signatureHelp_Capabilities is
     new LSP.Generic_Optional (signatureHelp_Capability);

   type Optional_signatureHelp_Capability is
     new Optional_signatureHelp_Capabilities.Optional_Type;

   type Document_Symbol_Capability is record
      dynamicRegistration: Optional_Boolean;
      symbolKind: Optional_SymbolKindSet;
      hierarchicalDocumentSymbolSupport: Optional_Boolean;
   end record;

   procedure Read_Document_Symbol_Capability
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Document_Symbol_Capability);

   procedure Write_Document_Symbol_Capability
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Document_Symbol_Capability);

   for Document_Symbol_Capability'Read use Read_Document_Symbol_Capability;
   for Document_Symbol_Capability'Write use Write_Document_Symbol_Capability;

   package Optional_Document_Symbol_Capabilities is
     new LSP.Generic_Optional (Document_Symbol_Capability);

   type Optional_Document_Symbol_Capability is
     new Optional_Document_Symbol_Capabilities.Optional_Type;

   type declaration_Capability is record
      dynamicRegistration: Optional_Boolean;
      linkSupport: Optional_Boolean;
   end record;

   procedure Read_declaration_Capability
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out declaration_Capability);

   procedure Write_declaration_Capability
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : declaration_Capability);

   for declaration_Capability'Read use Read_declaration_Capability;
   for declaration_Capability'Write use Write_declaration_Capability;

   package Optional_declaration_Capabilities is
     new LSP.Generic_Optional (declaration_Capability);

   type Optional_declaration_Capability is
     new Optional_declaration_Capabilities.Optional_Type;

   subtype Optional_definition_Capability is Optional_declaration_Capability;
   subtype Optional_typeDefinition_Capability is
     Optional_declaration_Capability;
   subtype Optional_implementation_Capability is
     Optional_declaration_Capability;

   type codeActionLiteralSupport_Capability is record
      codeActionKind: CodeActionKindSet;
   end record;

   procedure Read_codeActionLiteralSupport_Capability
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out codeActionLiteralSupport_Capability);

   procedure Write_codeActionLiteralSupport_Capability
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : codeActionLiteralSupport_Capability);

   for codeActionLiteralSupport_Capability'Read use Read_codeActionLiteralSupport_Capability;
   for codeActionLiteralSupport_Capability'Write use Write_codeActionLiteralSupport_Capability;

   package Optional_codeActionLiteralSupport_Capabilities is
     new LSP.Generic_Optional (codeActionLiteralSupport_Capability);

   type Optional_codeActionLiteralSupport_Capability is
     new Optional_codeActionLiteralSupport_Capabilities.Optional_Type;

   type codeAction_Capability is record
      dynamicRegistration: Optional_Boolean;
      codeActionLiteralSupport: Optional_codeActionLiteralSupport_Capability;
   end record;

   procedure Read_codeAction_Capability
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out codeAction_Capability);

   procedure Write_codeAction_Capability
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : codeAction_Capability);

   for codeAction_Capability'Read use Read_codeAction_Capability;
   for codeAction_Capability'Write use Write_codeAction_Capability;

   package Optional_codeAction_Capabilities is
     new LSP.Generic_Optional (codeAction_Capability);

   type Optional_codeAction_Capability is
     new Optional_codeAction_Capabilities.Optional_Type;

   type rename_Capability is record
      dynamicRegistration: Optional_Boolean;
      prepareSupport: Optional_Boolean;
   end record;

   procedure Read_rename_Capability
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out rename_Capability);

   procedure Write_rename_Capability
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : rename_Capability);

   for rename_Capability'Read use Read_rename_Capability;
   for rename_Capability'Write use Write_rename_Capability;

   package Optional_rename_Capabilities is
     new LSP.Generic_Optional (rename_Capability);

   type Optional_rename_Capability is
     new Optional_rename_Capabilities.Optional_Type;

   type publishDiagnostics_Capability is record
      relatedInformation: Optional_Boolean;
   end record;

   procedure Read_publishDiagnostics_Capability
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out publishDiagnostics_Capability);

   procedure Write_publishDiagnostics_Capability
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : publishDiagnostics_Capability);

   for publishDiagnostics_Capability'Read use Read_publishDiagnostics_Capability;
   for publishDiagnostics_Capability'Write use Write_publishDiagnostics_Capability;

   package Optional_publishDiagnostics_Capabilities is
     new LSP.Generic_Optional (publishDiagnostics_Capability);

   type Optional_publishDiagnostics_Capability is
     new Optional_publishDiagnostics_Capabilities.Optional_Type;

   type foldingRange_Capability is record
      dynamicRegistration: Optional_Boolean;
      rangeLimit: Optional_Number;
      lineFoldingOnly: Optional_Boolean;
   end record;

   procedure Read_foldingRange_Capability
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out foldingRange_Capability);

   procedure Write_foldingRange_Capability
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : foldingRange_Capability);

   for foldingRange_Capability'Read use Read_foldingRange_Capability;
   for foldingRange_Capability'Write use Write_foldingRange_Capability;

   package Optional_foldingRange_Capabilities is
     new LSP.Generic_Optional (foldingRange_Capability);

   type Optional_foldingRange_Capability is
     new Optional_foldingRange_Capabilities.Optional_Type;

   type TextDocumentClientCapabilities is record
      synchronization    : LSP.Messages.synchronization;
      completion         : LSP.Messages.completion;
      hover              : Optional_Hover_Capability;
      signatureHelp      : Optional_signatureHelp_Capability;
      references         : dynamicRegistration;
      documentHighlight  : dynamicRegistration;
      documentSymbol     : Optional_Document_Symbol_Capability;
      formatting         : dynamicRegistration;
      rangeFormatting    : dynamicRegistration;
      onTypeFormatting   : dynamicRegistration;
      declaration        : Optional_declaration_Capability;
      definition         : Optional_definition_Capability;
      typeDefinition     : Optional_typeDefinition_Capability;
      implementation     : Optional_implementation_Capability;
      codeAction         : Optional_codeAction_Capability;
      codeLens           : dynamicRegistration;
      documentLink       : dynamicRegistration;
      colorProvider      : dynamicRegistration;
      rename             : Optional_rename_Capability;
      publishDiagnostics : Optional_publishDiagnostics_Capability;
      foldingRange       : Optional_foldingRange_Capability;
   end record;

   procedure Read_TextDocumentClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out TextDocumentClientCapabilities);

   procedure Write_TextDocumentClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : TextDocumentClientCapabilities);

   for TextDocumentClientCapabilities'Read use Read_TextDocumentClientCapabilities;
   for TextDocumentClientCapabilities'Write use Write_TextDocumentClientCapabilities;

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

   procedure Read_ClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ClientCapabilities);

   procedure Write_ClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ClientCapabilities);

   for ClientCapabilities'Read use Read_ClientCapabilities;
   for ClientCapabilities'Write use Write_ClientCapabilities;

   --```typescript
   --export interface WorkspaceFolder {
   --	/**
   --	 * The associated URI for this workspace folder.
   --	 */
   --	uri: DocumentUri;
   --
   --	/**
   --	 * The name of the workspace folder. Used to refer to this
   --	 * workspace folder in the user interface.
   --	 */
   --	name: string;
   --}
   --```
   type WorkspaceFolder is record
      uri: DocumentUri;
      name: LSP_String;
   end record;

   procedure Read_WorkspaceFolder
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out WorkspaceFolder);

   procedure Write_WorkspaceFolder
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : WorkspaceFolder);

   for WorkspaceFolder'Read use Read_WorkspaceFolder;
   for WorkspaceFolder'Write use Write_WorkspaceFolder;

   package WorkspaceFolder_Vectors is new LSP.Generic_Vectors (WorkspaceFolder);
   type WorkspaceFolder_Vector is new WorkspaceFolder_Vectors.Vector with null record;

   package Optional_WorkspaceFolder_Vectors is new LSP.Generic_Optional (WorkspaceFolder_Vector);
   type Optional_WorkspaceFolder_Vector is new Optional_WorkspaceFolder_Vectors.Optional_Type;

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
   --
   --	/**
   --	 * The workspace folders configured in the client when the server starts.
   --	 * This property is only available if the client supports workspace folders.
   --	 * It can be `null` if the client supports workspace folders but none are
   --	 * configured.
   --	 *
   --	 * Since 3.6.0
   --	 */
   --	workspaceFolders?: WorkspaceFolder[] | null;
   --}
   --```
   type InitializeParams is record
      processId: Optional_Number;
      rootPath: LSP_String;
      rootUri: DocumentUri;  --  or null???
      --  initializationOptions?: any;
      capabilities: ClientCapabilities;
      trace: Trace_Kinds;
      workspaceFolders: Optional_WorkspaceFolder_Vector;
   end record;

   procedure Read_InitializeParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out InitializeParams);

   procedure Write_InitializeParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : InitializeParams);

   for InitializeParams'Read use Read_InitializeParams;
   for InitializeParams'Write use Write_InitializeParams;

   --
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
   -- * Code Action options.
   -- */
   --export interface CodeActionOptions {
   --	/**
   --	 * CodeActionKinds that this server may return.
   --	 *
   --	 * The list of kinds may be generic, such as `CodeActionKind.Refactor`, or the server
   --	 * may list out every specific kind they provide.
   --	 */
   --	codeActionKinds?: CodeActionKind[];
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
   -- * Format document on type options.
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
   -- * Rename options
   -- */
   --export interface RenameOptions {
   --	/**
   --	 * Renames should be checked and tested before being executed.
   --	 */
   --	prepareProvider?: boolean;
   --}
   --
   --/**
   -- * Document link options.
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
   --/**
   -- * Color provider options.
   -- */
   --export interface ColorProviderOptions {
   --}
   --
   --/**
   -- * Folding range provider options.
   -- */
   --export interface FoldingRangeProviderOptions {
   --}
   --
   --export interface TextDocumentSyncOptions {
   --	/**
   --	 * Open and close notifications are sent to the server. If omitted open close notification should not
   --	 * be sent.
   --	 */
   --	openClose?: boolean;
   --	/**
   --	 * Change notifications are sent to the server. See TextDocumentSyncKind.None, TextDocumentSyncKind.Full
   --	 * and TextDocumentSyncKind.Incremental. If omitted it defaults to TextDocumentSyncKind.None.
   --	 */
   --	change?: number;
   --	/**
   --	 * If present will save notifications are sent to the server. If omitted the notification should not be
   --	 * sent.
   --	 */
   --	willSave?: boolean;
   --	/**
   --	 * If present will save wait until requests are sent to the server. If omitted the request should not be
   --	 * sent.
   --	 */
   --	willSaveWaitUntil?: boolean;
   --	/**
   --	 * If present save notifications are sent to the server. If omitted the notification should not be
   --	 * sent.
   --	 */
   --	save?: SaveOptions;
   --}
   --
   --/**
   -- * Static registration options to be returned in the initialize request.
   -- */
   --interface StaticRegistrationOptions {
   --	/**
   --	 * The id used to register the request. The id can be used to deregister
   --	 * the request again. See also Registration#id.
   --	 */
   --	id?: string;
   --}
   --
   --interface ServerCapabilities {
   --	/**
   --	 * Defines how text documents are synced. Is either a detailed structure defining each notification or
   --	 * for backwards compatibility the TextDocumentSyncKind number. If omitted it defaults to `TextDocumentSyncKind.None`.
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
   --	 * The server provides Goto Type Definition support.
   --	 *
   --	 * Since 3.6.0
   --	 */
   --	typeDefinitionProvider?: boolean | (TextDocumentRegistrationOptions & StaticRegistrationOptions);
   --	/**
   --	 * The server provides Goto Implementation support.
   --	 *
   --	 * Since 3.6.0
   --	 */
   --	implementationProvider?: boolean | (TextDocumentRegistrationOptions & StaticRegistrationOptions);
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
   --	 * The server provides code actions. The `CodeActionOptions` return type is only
   --	 * valid if the client signals code action literal support via the property
   --	 * `textDocument.codeAction.codeActionLiteralSupport`.
   --	 */
   --	codeActionProvider?: boolean | CodeActionOptions;
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
   --	 * The server provides rename support. RenameOptions may only be
   --	 * specified if the client states that it supports
   --	 * `prepareSupport` in its initial `initialize` request.
   --	 */
   --	renameProvider?: boolean | RenameOptions;
   --	/**
   --	 * The server provides document link support.
   --	 */
   --	documentLinkProvider?: DocumentLinkOptions;
   --	/**
   --	 * The server provides color provider support.
   --	 *
   --	 * Since 3.6.0
   --	 */
   --	colorProvider?: boolean | ColorProviderOptions | (ColorProviderOptions & TextDocumentRegistrationOptions & StaticRegistrationOptions);
   --	/**
   --	 * The server provides folding provider support.
   --	 *
   --	 * Since 3.10.0
   --	 */
   --	foldingRangeProvider?: boolean | FoldingRangeProviderOptions | (FoldingRangeProviderOptions & TextDocumentRegistrationOptions & StaticRegistrationOptions);
   --	/**
   --	 * The server provides go to declaration support.
   --	 *
   --	 * Since 3.14.0
   --	 */
   --	declarationProvider?: boolean | (TextDocumentRegistrationOptions & StaticRegistrationOptions);
   --	/**
   --	 * The server provides execute command support.
   --	 */
   --	executeCommandProvider?: ExecuteCommandOptions;
   --	/**
   --	 * Workspace specific server capabilities
   --	 */
   --	workspace?: {
   --		/**
   --		 * The server supports workspace folder.
   --		 *
   --		 * Since 3.6.0
   --		 */
   --		workspaceFolders?: {
   --			/**
   --			* The server has support for workspace folders
   --			*/
   --			supported?: boolean;
   --			/**
   --			* Whether the server wants to receive workspace folder
   --			* change notifications.
   --			*
   --			* If a strings is provided the string is treated as a ID
   --			* under which the notification is registered on the client
   --			* side. The ID can be used to unregister for these events
   --			* using the `client/unregisterCapability` request.
   --			*/
   --			changeNotifications?: string | boolean;
   --		}
   --	}
   --	/**
   --	 * Experimental server capabilities.
   --	 */
   --	experimental?: any;
   --}
   --```
   type TextDocumentSyncKind is (None, Full, Incremental);

   procedure Read_TextDocumentSyncKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out TextDocumentSyncKind);
   for TextDocumentSyncKind'Read use Read_TextDocumentSyncKind;

   procedure Write_TextDocumentSyncKind
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

   procedure Read_TextDocumentSyncOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out TextDocumentSyncOptions);
   procedure Write_TextDocumentSyncOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : TextDocumentSyncOptions);
   for TextDocumentSyncOptions'Read use Read_TextDocumentSyncOptions;
   for TextDocumentSyncOptions'Write use Write_TextDocumentSyncOptions;

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

   procedure Read_Optional_TextDocumentSyncOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Optional_TextDocumentSyncOptions);
   procedure Write_Optional_TextDocumentSyncOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Optional_TextDocumentSyncOptions);
   for Optional_TextDocumentSyncOptions'Read use Read_Optional_TextDocumentSyncOptions;
   for Optional_TextDocumentSyncOptions'Write use Write_Optional_TextDocumentSyncOptions;

   type CompletionOptions is record
      resolveProvider: LSP.Types.Optional_Boolean;
      triggerCharacters: LSP.Types.LSP_String_Vector;
   end record;

   procedure Read_CompletionOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CompletionOptions);
   for CompletionOptions'Read use Read_CompletionOptions;

   procedure Write_CompletionOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CompletionOptions);
   for CompletionOptions'Write use Write_CompletionOptions;

   package Optional_Completion_Package is
     new LSP.Generic_Optional (CompletionOptions);

   type Optional_CompletionOptions is
     new Optional_Completion_Package.Optional_Type;

   type SignatureHelpOptions is record
      triggerCharacters: LSP.Types.LSP_String_Vector;
   end record;

   procedure Read_SignatureHelpOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out SignatureHelpOptions);

   for SignatureHelpOptions'Read use Read_SignatureHelpOptions;

   procedure Write_SignatureHelpOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SignatureHelpOptions);

   for SignatureHelpOptions'Write use Write_SignatureHelpOptions;

   package Optional_SignatureHelp_Package is
     new LSP.Generic_Optional (SignatureHelpOptions);

   type Optional_SignatureHelpOptions is
     new Optional_SignatureHelp_Package.Optional_Type;

   --  Here we define Ada type StaticRegistrationOptions that corresponds to
   --  (TextDocumentRegistrationOptions & StaticRegistrationOptions)
   --  typescript type, because it is always used with
   --  TextDocumentRegistrationOptions and we don't have '&' operator (or
   --  something similar) for Ada types.
   type StaticRegistrationOptions is record
      id: Optional_String;
      documentSelector:  LSP.Messages.DocumentSelector;
   end record;

   procedure Read_StaticRegistrationOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out StaticRegistrationOptions);

   procedure Write_StaticRegistrationOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : StaticRegistrationOptions);

   for StaticRegistrationOptions'Read use Read_StaticRegistrationOptions;
   for StaticRegistrationOptions'Write use Write_StaticRegistrationOptions;

   package Optional_StaticRegistration_Package is
     new LSP.Generic_Optional (StaticRegistrationOptions);

   type Optional_StaticRegistrationOptions is
     new Optional_StaticRegistration_Package.Optional_Type;

   --  Ada type Provider_Options correspond to this typescript type:
   --  boolean | (TextDocumentRegistrationOptions & StaticRegistrationOptions) | {}
   type Provider_Options (Is_Boolean : Boolean := False) is record
      case Is_Boolean is
         when True =>
            Bool : Boolean;
         when False =>
            Options : Optional_StaticRegistrationOptions;
      end case;
   end record;

   procedure Read_Provider_Options
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Provider_Options);

   procedure Write_Provider_Options
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Provider_Options);

   for Provider_Options'Read use Read_Provider_Options;
   for Provider_Options'Write use Write_Provider_Options;

   package Optional_Provider_Package is
     new LSP.Generic_Optional (Provider_Options);

   type Optional_Provider_Options is
     new Optional_Provider_Package.Optional_Type;

   type CodeActionOptions is record
      codeActionKinds: Optional_CodeActionKindSet;
   end record;

   procedure Read_CodeActionOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CodeActionOptions);
   procedure Write_CodeActionOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CodeActionOptions);
   for CodeActionOptions'Write use Write_CodeActionOptions;
   for CodeActionOptions'Read use Read_CodeActionOptions;

   package Optional_CodeActionOptions_Package is
     new LSP.Generic_Optional (CodeActionOptions);
   type Optional_CodeActionOptions is
     new Optional_CodeActionOptions_Package.Optional_Type;

   type CodeLensOptions is record
      resolveProvider: LSP.Types.Optional_Boolean;
   end record;

   procedure Read_CodeLensOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CodeLensOptions);
   procedure Write_CodeLensOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CodeLensOptions);
   for CodeLensOptions'Read use Read_CodeLensOptions;
   for CodeLensOptions'Write use Write_CodeLensOptions;

   package Optional_CodeLens_Package is
     new LSP.Generic_Optional (CodeLensOptions);

   type Optional_CodeLensOptions is
     new Optional_CodeLens_Package.Optional_Type;

   type DocumentOnTypeFormattingOptions is record
      firstTriggerCharacter: LSP.Types.LSP_String;
      moreTriggerCharacter: LSP.Types.LSP_String_Vector;
   end record;

   procedure Read_DocumentOnTypeFormattingOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DocumentOnTypeFormattingOptions);

   for DocumentOnTypeFormattingOptions'Read use Read_DocumentOnTypeFormattingOptions;

   procedure Write_DocumentOnTypeFormattingOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DocumentOnTypeFormattingOptions);

   for DocumentOnTypeFormattingOptions'Write use Write_DocumentOnTypeFormattingOptions;

   package Optional_DocumentOnTypeFormatting_Package is
     new LSP.Generic_Optional (DocumentOnTypeFormattingOptions);

   type Optional_DocumentOnTypeFormattingOptions is
     new Optional_DocumentOnTypeFormatting_Package.Optional_Type;

   type RenameOptions is record
      prepareProvider: LSP.Types.Optional_Boolean;
   end record;

   procedure Read_RenameOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out RenameOptions);

   procedure Write_RenameOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : RenameOptions);

   for RenameOptions'Write use Write_RenameOptions;
   for RenameOptions'Read use Read_RenameOptions;

   package Optional_Rename_Package is new LSP.Generic_Optional (RenameOptions);
   type Optional_RenameOptions is new Optional_Rename_Package.Optional_Type;

   type DocumentLinkOptions is record
      resolveProvider: LSP.Types.Optional_Boolean;
   end record;

   procedure Read_DocumentLinkOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DocumentLinkOptions);

   procedure Write_DocumentLinkOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DocumentLinkOptions);

   for DocumentLinkOptions'Write use Write_DocumentLinkOptions;
   for DocumentLinkOptions'Read use Read_DocumentLinkOptions;

   type ExecuteCommandOptions is record
      commands: LSP.Types.LSP_String_Vector;
   end record;

   procedure Read_ExecuteCommandOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ExecuteCommandOptions);
   procedure Write_ExecuteCommandOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ExecuteCommandOptions);
   for ExecuteCommandOptions'Write use Write_ExecuteCommandOptions;
   for ExecuteCommandOptions'Read use Read_ExecuteCommandOptions;

   package Optional_Boolean_Or_String_Package is
     new LSP.Generic_Optional (LSP_Boolean_Or_String);

   type Optional_Boolean_Or_String is
     new Optional_Boolean_Or_String_Package.Optional_Type;

   type workspaceFolders is record
      supported: Optional_Boolean;
      changeNotifications: Optional_Boolean_Or_String;
   end record;

   procedure Read_workspaceFolders
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out workspaceFolders);
   procedure Write_workspaceFolders
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : workspaceFolders);
   for workspaceFolders'Write use Write_workspaceFolders;
   for workspaceFolders'Read use Read_workspaceFolders;

   package Optional_workspaceFolders_Package is
     new LSP.Generic_Optional (workspaceFolders);

   type Optional_workspaceFolders is
     new Optional_workspaceFolders_Package.Optional_Type;

   type workspace_Options is record
      workspaceFolders: Optional_workspaceFolders;
   end record;

   procedure Read_workspace_Options
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out workspace_Options);
   procedure Write_workspace_Options
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : workspace_Options);
   for workspace_Options'Write use Write_workspace_Options;
   for workspace_Options'Read use Read_workspace_Options;

   package Optional_workspace_Options_Package is
     new LSP.Generic_Optional (workspace_Options);

   type Optional_workspace_Options is
     new Optional_workspace_Options_Package.Optional_Type;

   type ServerCapabilities is record
      textDocumentSync: Optional_TextDocumentSyncOptions;
      hoverProvider: Optional_Boolean;
      completionProvider: Optional_CompletionOptions;
      signatureHelpProvider: Optional_SignatureHelpOptions;
      definitionProvider: Optional_Boolean;
      typeDefinitionProvider: Optional_Provider_Options;
      implementationProvider: Optional_Provider_Options;
      referencesProvider: Optional_Boolean;
      documentHighlightProvider: Optional_Boolean;
      documentSymbolProvider: Optional_Boolean;
      workspaceSymbolProvider: Optional_Boolean;
      codeActionProvider: Optional_CodeActionOptions;
      codeLensProvider: Optional_CodeLensOptions;
      documentFormattingProvider: Optional_Boolean;
      documentRangeFormattingProvider: Optional_Boolean;
      documentOnTypeFormattingProvider: Optional_DocumentOnTypeFormattingOptions;
      renameProvider: Optional_RenameOptions;
      documentLinkProvider: DocumentLinkOptions;
      colorProvider: Optional_Provider_Options;
      foldingRangeProvider: Optional_Provider_Options;
      declarationProvider: Optional_Provider_Options;
      executeCommandProvider: ExecuteCommandOptions;
      workspace: Optional_workspace_Options;
      --	experimental?: any;

      --  ALS-specific capabilities
      alsCalledByProvider : Optional_Boolean;
      alsReferenceKinds   : Optional_AlsReferenceKind_Set;
   end record;

   procedure Read_ServerCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ServerCapabilities);
   procedure Write_ServerCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ServerCapabilities);
   for ServerCapabilities'Read use Read_ServerCapabilities;
   for ServerCapabilities'Write use Write_ServerCapabilities;

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

   procedure Read_InitializeResult
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out InitializeResult);
   procedure Write_InitializeResult
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : InitializeResult);
   for InitializeResult'Write use Write_InitializeResult;
   for InitializeResult'Read use Read_InitializeResult;

   --```typescript
   --interface InitializedParams {
   --}
   --```
   type InitializedParams is null record;

   not overriding procedure Read_InitializedParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out InitializedParams);

   not overriding procedure Write_InitializedParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : InitializedParams);

   for InitializedParams'Read use Read_InitializedParams;
   for InitializedParams'Write use Write_InitializedParams;

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

   --
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

   procedure Read_ShowMessageParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ShowMessageParams);
   procedure Write_ShowMessageParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ShowMessageParams);
   for ShowMessageParams'Read use Read_ShowMessageParams;
   for ShowMessageParams'Write use Write_ShowMessageParams;

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

   procedure Read_LogMessageParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LogMessageParams);
   procedure Write_LogMessageParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LogMessageParams);
   for LogMessageParams'Read use Read_LogMessageParams;
   for LogMessageParams'Write use Write_LogMessageParams;

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
   -- * Describe options to be used when registering for text document change events.
   -- */
   --export interface TextDocumentChangeRegistrationOptions extends TextDocumentRegistrationOptions {
   --	/**
   --	 * How documents are synced to the server. See TextDocumentSyncKind.Full
   --	 * and TextDocumentSyncKind.Incremental.
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
   --	 * Most tools trigger completion request automatically without explicitly requesting
   --	 * it using a keyboard shortcut (e.g. Ctrl+Space). Typically they do so when the user
   --	 * starts to type an identifier. For example if the user types `c` in a JavaScript file
   --	 * code complete will automatically pop up present `console` besides others as a
   --	 * completion item. Characters that make up identifiers don't need to be listed here.
   --	 *
   --	 * If code complete should automatically be trigger on characters not being valid inside
   --	 * an identifier (for example `.` in JavaScript) list them in `triggerCharacters`.
   --	 */
   --	triggerCharacters?: string[];
   --
   --	/**
   --	 * The list of all possible characters that commit a completion. This field can be used
   --	 * if clients don't support individual commmit characters per completion item. See
   --	 * `ClientCapabilities.textDocument.completion.completionItem.commitCharactersSupport`.
   --	 *
   --	 * If a server provides both `allCommitCharacters` and commit characters on an individual
   --	 * completion item the ones on the completion item win.
   --	 *
   --     * Since 3.2.0
   --	 */
   --	allCommitCharacters?: string[];
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
      allCommitCharacters: LSP_String_Vector;
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

   package Unregistration_Vectors is new LSP.Generic_Vectors (Unregistration);

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

   procedure Read_DidChangeConfigurationParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DidChangeConfigurationParams);
   procedure Write_DidChangeConfigurationParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DidChangeConfigurationParams);
   for DidChangeConfigurationParams'Read use Read_DidChangeConfigurationParams;
   for DidChangeConfigurationParams'Write use Write_DidChangeConfigurationParams;

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

   procedure Read_DidOpenTextDocumentParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DidOpenTextDocumentParams);

   procedure Write_DidOpenTextDocumentParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DidOpenTextDocumentParams);

   for DidOpenTextDocumentParams'Read use Read_DidOpenTextDocumentParams;
   for DidOpenTextDocumentParams'Write use Write_DidOpenTextDocumentParams;

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
   --	 * The actual content changes. The content changes describe single state changes
   --	 * to the document. So if there are two content changes c1 and c2 for a document
   --	 * in state S then c1 move the document to S' and c2 to S''.
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

   procedure Read_TextDocumentContentChangeEvent
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out TextDocumentContentChangeEvent);
   procedure Write_TextDocumentContentChangeEvent
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : TextDocumentContentChangeEvent);
   for TextDocumentContentChangeEvent'Read use Read_TextDocumentContentChangeEvent;
   for TextDocumentContentChangeEvent'Write use Write_TextDocumentContentChangeEvent;

   package TextDocumentContentChangeEvent_Vectors is new LSP.Generic_Vectors
     (TextDocumentContentChangeEvent);

   type TextDocumentContentChangeEvent_Vector is
     new TextDocumentContentChangeEvent_Vectors.Vector with null record;

   type DidChangeTextDocumentParams is record
      textDocument: VersionedTextDocumentIdentifier;
      contentChanges: TextDocumentContentChangeEvent_Vector;
   end record;

   procedure Read_DidChangeTextDocumentParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DidChangeTextDocumentParams);
   procedure Write_DidChangeTextDocumentParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DidChangeTextDocumentParams);
   for DidChangeTextDocumentParams'Read use Read_DidChangeTextDocumentParams;
   for DidChangeTextDocumentParams'Write use Write_DidChangeTextDocumentParams;

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
   --	 * when the save notification was requested.
   --	 */
   --	text?: string;
   --}
   --```
   type DidSaveTextDocumentParams is record
      textDocument: TextDocumentIdentifier;
      text: Optional_String;
   end record;

   procedure Read_DidSaveTextDocumentParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DidSaveTextDocumentParams);
   procedure Write_DidSaveTextDocumentParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DidSaveTextDocumentParams);
   for DidSaveTextDocumentParams'Read use Read_DidSaveTextDocumentParams;
   for DidSaveTextDocumentParams'Write use Write_DidSaveTextDocumentParams;

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

   procedure Read_DidCloseTextDocumentParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DidCloseTextDocumentParams);
   procedure Write_DidCloseTextDocumentParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DidCloseTextDocumentParams);

   for DidCloseTextDocumentParams'Read use Read_DidCloseTextDocumentParams;
   for DidCloseTextDocumentParams'Write use Write_DidCloseTextDocumentParams;

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

   package FileEvent_Vectors is new LSP.Generic_Vectors (FileEvent);
   type FileEvent_Vector is new FileEvent_Vectors.Vector with null record;

   --```typescript
   --interface DidChangeWatchedFilesParams {
   --	/**
   --	 * The actual file events.
   --	 */
   --	changes: FileEvent[];
   --}
   --```
   type DidChangeWatchedFilesParams is record
      changes: FileEvent_Vector;
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

   procedure Read_PublishDiagnosticsParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out PublishDiagnosticsParams);
   procedure Write_PublishDiagnosticsParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : PublishDiagnosticsParams);
   for PublishDiagnosticsParams'Read use Read_PublishDiagnosticsParams;
   for PublishDiagnosticsParams'Write use Write_PublishDiagnosticsParams;

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
   --
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
   --
   --	/**
   --	 * The kind of this completion item. Based of the kind
   --	 * an icon is chosen by the editor. The standardized set
   --	 * of available values is defined in `CompletionItemKind`.
   --	 */
   --	kind?: number;
   --
   --	/**
   --	 * A human-readable string with additional information
   --	 * about this item, like type or symbol information.
   --	 */
   --	detail?: string;
   --
   --	/**
   --	 * A human-readable string that represents a doc-comment.
   --	 */
   --	documentation?: string | MarkupContent;
   --
   --	/**
   --	 * Indicates if this item is deprecated.
   --	 */
   --	deprecated?: boolean;
   --
   --	/**
   --	 * Select this item when showing.
   --	 *
   --	 * *Note* that only one completion item can be selected and that the
   --	 * tool / client decides which item that is. The rule is that the *first*
   --	 * item of those that match best is selected.
   --	 */
   --	preselect?: boolean;
   --
   --	/**
   --	 * A string that should be used when comparing this item
   --	 * with other items. When `falsy` the label is used.
   --	 */
   --	sortText?: string;
   --
   --	/**
   --	 * A string that should be used when filtering a set of
   --	 * completion items. When `falsy` the label is used.
   --	 */
   --	filterText?: string;
   --
   --	/**
   --	 * A string that should be inserted into a document when selecting
   --	 * this completion. When `falsy` the label is used.
   --	 *
   --	 * The `insertText` is subject to interpretation by the client side.
   --	 * Some tools might not take the string literally. For example
   --	 * VS Code when code complete is requested in this example `con<cursor position>`
   --	 * and a completion item with an `insertText` of `console` is provided it
   --	 * will only insert `sole`. Therefore it is recommended to use `textEdit` instead
   --	 * since it avoids additional client side interpretation.
   --	 */
   --	insertText?: string;
   --
   --	/**
   --	 * The format of the insert text. The format applies to both the `insertText` property
   --	 * and the `newText` property of a provided `textEdit`. If ommitted defaults to
   --	 * `InsertTextFormat.PlainText`.
   --	 */
   --	insertTextFormat?: InsertTextFormat;
   --
   --	/**
   --	 * An edit which is applied to a document when selecting this completion. When an edit is provided the value of
   --	 * `insertText` is ignored.
   --	 *
   --	 * *Note:* The range of the edit must be a single line range and it must contain the position at which completion
   --	 * has been requested.
   --	 */
   --	textEdit?: TextEdit;
   --
   --	/**
   --	 * An optional array of additional text edits that are applied when
   --	 * selecting this completion. Edits must not overlap (including the same insert position)
   --	 * with the main edit nor with themselves.
   --	 *
   --	 * Additional text edits should be used to change text unrelated to the current cursor position
   --	 * (for example adding an import statement at the top of the file if the completion item will
   --	 * insert an unqualified type).
   --	 */
   --	additionalTextEdits?: TextEdit[];
   --
   --	/**
   --	 * An optional set of characters that when pressed while this completion is active will accept it first and
   --	 * then type that character. *Note* that all commit characters should have `length=1` and that superfluous
   --	 * characters will be ignored.
   --	 */
   --	commitCharacters?: string[];
   --
   --	/**
   --	 * An optional command that is executed *after* inserting this completion. *Note* that
   --	 * additional modifications to the current document should be described with the
   --	 * additionalTextEdits-property.
   --	 */
   --	command?: Command;
   --
   --	/**
   --	 * A data entry field that is preserved on a completion item between
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
   --	export const Folder = 19;
   --	export const EnumMember = 20;
   --	export const Constant = 21;
   --	export const Struct = 22;
   --	export const Event = 23;
   --	export const Operator = 24;
   --	export const TypeParameter = 25;
   --}
   --```
   type InsertTextFormat is (PlainText, Snippet);

   procedure Read_InsertTextFormat
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out InsertTextFormat);
   procedure Write_InsertTextFormat
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : InsertTextFormat);
   for InsertTextFormat'Read use Read_InsertTextFormat;
   for InsertTextFormat'Write use Write_InsertTextFormat;

   package Optional_InsertTextFormats is new LSP.Generic_Optional (InsertTextFormat);
   type Optional_InsertTextFormat is new Optional_InsertTextFormats.Optional_Type;

   package Optional_CompletionItemKinds is new LSP.Generic_Optional (CompletionItemKind);
   type Optional_CompletionItemKind is new Optional_CompletionItemKinds.Optional_Type;

   package Optional_Commands is new LSP.Generic_Optional (Command);
   type Optional_Command is new Optional_Commands.Optional_Type;

   type CompletionItem is record
      label: LSP_String;
      kind: Optional_CompletionItemKind;
      detail: Optional_String;
      documentation: Optional_String_Or_MarkupContent;
      deprecated: Optional_Boolean;
      preselect: Optional_Boolean;
      sortText: Optional_String;
      filterText: Optional_String;
      insertText: Optional_String;
      insertTextFormat: Optional_InsertTextFormat;
      textEdit: Optional_TextEdit;
      additionalTextEdits: TextEdit_Vector;
      commitCharacters: LSP_String_Vector;
      command: Optional_Command;
   --	data?: any
   end record;

   procedure Read_CompletionItem
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CompletionItem);
   procedure Write_CompletionItem
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CompletionItem);
   for CompletionItem'Read use Read_CompletionItem;
   for CompletionItem'Write use Write_CompletionItem;

   package CompletionItem_Vectors is new LSP.Generic_Vectors (CompletionItem);
   type CompletionItem_Vector is
     new CompletionItem_Vectors.Vector with null record;

   type CompletionList is record
      isIncomplete: Boolean := False;
      items: CompletionItem_Vector;
   end record;

   procedure Read_CompletionList
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CompletionList);
   procedure Write_CompletionList
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CompletionList);
   for CompletionList'Read use Read_CompletionList;
   for CompletionList'Write use Write_CompletionList;

   --```typescript
   --/**
   -- * MarkedString can be used to render human readable text. It is either a markdown string
   -- * or a code-block that provides a language and a code snippet. The language identifier
   -- * is semantically equal to the optional language identifier in fenced code blocks in GitHub
   -- * issues. See https://help.github.com/articles/creating-and-highlighting-code-blocks/#syntax-highlighting
   -- *
   -- * The pair of a language and a value is an equivalent to markdown:
   -- * ```${language}
   -- * ${value}
   -- * ```
   -- *
   -- * Note that markdown strings will be sanitized - that means html will be escaped.
   --* @deprecated use MarkupContent instead.
   --*/
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

   procedure Read_MarkedString
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out MarkedString);
   procedure Write_MarkedString
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : MarkedString);
   for MarkedString'Read use Read_MarkedString;
   for MarkedString'Write use Write_MarkedString;

   package MarkedString_Vectors is new LSP.Generic_Vectors (MarkedString);
   type MarkedString_Vector is new MarkedString_Vectors.Vector with null record;

   type MarkupContent_Or_MarkedString_Vector (Is_MarkupContent : Boolean := False) is record
      case Is_MarkupContent is
         when True =>
            MarkupContent : LSP.Messages.MarkupContent;
         when False =>
            Vector : MarkedString_Vector;
      end case;
   end record;

   procedure Read_MarkupContent_Or_MarkedString_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out MarkupContent_Or_MarkedString_Vector);
   procedure Write_MarkupContent_Or_MarkedString_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : MarkupContent_Or_MarkedString_Vector);
   for MarkupContent_Or_MarkedString_Vector'Read use Read_MarkupContent_Or_MarkedString_Vector;
   for MarkupContent_Or_MarkedString_Vector'Write use Write_MarkupContent_Or_MarkedString_Vector;

   --```typescript
   --/**
   -- * The result of a hover request.
   -- */
   --interface Hover {
   --	/**
   --	 * The hover's content
   --	 */
   --	contents: MarkedString | MarkedString[] | MarkupContent;
   --
   --	/**
   --	 * An optional range is a range inside a text document
   --	 * that is used to visualize a hover, e.g. by changing the background color.
   --	 */
   --	range?: Range;
   --}
   --```
   type Hover is record
      contents: MarkupContent_Or_MarkedString_Vector;
      Span: Optional_Span;
   end record;

   procedure Read_Hover
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Hover);
   procedure Write_Hover
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Hover);
   for Hover'Read use Read_Hover;
   for Hover'Write use Write_Hover;

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
   --	 * mandatory to better express this.
   --	 */
   --	activeSignature?: number;
   --
   --	/**
   --	 * The active parameter of the active signature. If omitted or the value
   --	 * lies outside the range of `signatures[activeSignature].parameters`
   --	 * defaults to 0 if the active signature has parameters. If
   --	 * the active signature has no parameters it is ignored.
   --	 * In future version of the protocol this property might become
   --	 * mandatory to better express the active parameter if the
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
   --	documentation?: string | MarkupContent;
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
   --
   --	/**
   --	 * The label of this parameter information.
   --	 *
   --	 * Either a string or an inclusive start and exclusive end offsets within its containing
   --	 * signature label. (see SignatureInformation.label). The offsets are based on a UTF-16
   --	 * string representation as `Position` and `Range` does.
   --	 *
   --	 * *Note*: a label of type string should be a substring of its containing signature label.
   --	 * Its intended use case is to highlight the parameter label part in the `SignatureInformation.label`.
   --	 */
   --	label: string | [number, number];
   --
   --	/**
   --	 * The human-readable doc-comment of this parameter. Will be shown
   --	 * in the UI but can be omitted.
   --	 */
   --	documentation?: string | MarkupContent;
   --}
   --```
   type Parameter_Label (Is_String : Boolean := True) is record
      case Is_String is
         when True =>
            String : LSP_String;
         when False =>
            From, Till : UTF_16_Index;
      end case;
   end record;

   procedure Read_Parameter_Label
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Parameter_Label);
   procedure Write_Parameter_Label
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Parameter_Label);
   for Parameter_Label'Read use Read_Parameter_Label;
   for Parameter_Label'Write use Write_Parameter_Label;

   type ParameterInformation is record
      label: Parameter_Label;
      documentation: Optional_String_Or_MarkupContent;
   end record;

   procedure Read_ParameterInformation
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ParameterInformation);
   procedure Write_ParameterInformation
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ParameterInformation);
   for ParameterInformation'Read use Read_ParameterInformation;
   for ParameterInformation'Write use Write_ParameterInformation;

   package ParameterInformation_Vectors is new LSP.Generic_Vectors
     (ParameterInformation);
   type ParameterInformation_Vector is
     new ParameterInformation_Vectors.Vector with null record;

   type SignatureInformation is record
      label: LSP_String;
      documentation: Optional_String_Or_MarkupContent;
      parameters: ParameterInformation_Vector;
   end record;

   procedure Read_SignatureInformation
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out SignatureInformation);
   procedure Write_SignatureInformation
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SignatureInformation);
   for SignatureInformation'Read use Read_SignatureInformation;
   for SignatureInformation'Write use Write_SignatureInformation;

   package SignatureInformation_Vectors is new LSP.Generic_Vectors
     (SignatureInformation);
   type SignatureInformation_Vector is
     new SignatureInformation_Vectors.Vector with null record;

   type SignatureHelp is record
	signatures: SignatureInformation_Vector;
	activeSignature: Optional_Number;
	activeParameter: Optional_Number;
   end record;

   procedure Read_SignatureHelp
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out SignatureHelp);
   procedure Write_SignatureHelp
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SignatureHelp);
   for SignatureHelp'Write use Write_SignatureHelp;
   for SignatureHelp'Read use Read_SignatureHelp;

   --```typescript
   --interface ReferenceParams extends TextDocumentPositionParams {
   --	context: ReferenceContext
   --}
   --
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

   procedure Read_ReferenceContext
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ReferenceContext);
   procedure Write_ReferenceContext
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ReferenceContext);
   for ReferenceContext'Read use Read_ReferenceContext;
   for ReferenceContext'Write use Write_ReferenceContext;

   type ReferenceParams is new TextDocumentPositionParams with record
      context: ReferenceContext;
   end record;

   procedure Read_ReferenceParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ReferenceParams);
   procedure Write_ReferenceParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ReferenceParams);
   for ReferenceParams'Read use Read_ReferenceParams;
   for ReferenceParams'Write use Write_ReferenceParams;

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
   type DocumentHighlightKind is (Text, Read, Write);

   procedure Read_DocumentHighlightKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DocumentHighlightKind);
   procedure Write_DocumentHighlightKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DocumentHighlightKind);
   for DocumentHighlightKind'Read use Read_DocumentHighlightKind;
   for DocumentHighlightKind'Write use Write_DocumentHighlightKind;

   package Optional_DocumentHighlightKinds is
     new LSP.Generic_Optional (DocumentHighlightKind);
   type Optional_DocumentHighlightKind is
     new Optional_DocumentHighlightKinds.Optional_Type;

   type DocumentHighlight is record
      span: LSP.Messages.Span;
      kind: DocumentHighlightKind;
   end record;

   procedure Read_DocumentHighlight
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DocumentHighlight);
   procedure Write_DocumentHighlight
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DocumentHighlight);
   for DocumentHighlight'Read use Read_DocumentHighlight;
   for DocumentHighlight'Write use Write_DocumentHighlight;

   package DocumentHighlight_Vectors is new LSP.Generic_Vectors
     (DocumentHighlight);

   type DocumentHighlight_Vector is
     new DocumentHighlight_Vectors.Vector with null record;

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

   procedure Read_DocumentSymbolParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DocumentSymbolParams);
   procedure Write_DocumentSymbolParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DocumentSymbolParams);
   for DocumentSymbolParams'Read use Read_DocumentSymbolParams;
   for DocumentSymbolParams'Write use Write_DocumentSymbolParams;

   --```typescript
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
   --	export const Object = 19;
   --	export const Key = 20;
   --	export const Null = 21;
   --	export const EnumMember = 22;
   --	export const Struct = 23;
   --	export const Event = 24;
   --	export const Operator = 25;
   --	export const TypeParameter = 26;
   --}
   --
   --/**
   -- * Represents programming constructs like variables, classes, interfaces etc. that appear in a document. Document symbols can be
   -- * hierarchical and they have two ranges: one that encloses its definition and one that points to its most interesting range,
   -- * e.g. the range of an identifier.
   -- */
   --export class DocumentSymbol {
   --
   --	/**
   --	 * The name of this symbol. Will be displayed in the user interface and therefore must not be
   --	 * an empty string or a string only consisting of white spaces.
   --	 */
   --	name: string;
   --
   --	/**
   --	 * More detail for this symbol, e.g the signature of a function.
   --	 */
   --	detail?: string;
   --
   --	/**
   --	 * The kind of this symbol.
   --	 */
   --	kind: SymbolKind;
   --
   --	/**
   --	 * Indicates if this symbol is deprecated.
   --	 */
   --	deprecated?: boolean;
   --
   --	/**
   --	 * The range enclosing this symbol not including leading/trailing whitespace but everything else
   --	 * like comments. This information is typically used to determine if the clients cursor is
   --	 * inside the symbol to reveal in the symbol in the UI.
   --	 */
   --	range: Range;
   --
   --	/**
   --	 * The range that should be selected and revealed when this symbol is being picked, e.g the name of a function.
   --	 * Must be contained by the `range`.
   --	 */
   --	selectionRange: Range;
   --
   --	/**
   --	 * Children of this symbol, e.g. properties of a class.
   --	 */
   --	children?: DocumentSymbol[];
   --}
   --
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
   --	 * Indicates if this symbol is deprecated.
   --	 */
   --	deprecated?: boolean;
   --
   --	/**
   --	 * The location of this symbol. The location's range is used by a tool
   --	 * to reveal the location in the editor. If the symbol is selected in the
   --	 * tool the range's start information is used to position the cursor. So
   --	 * the range usually spans more then the actual symbol's name and does
   --	 * normally include things like visibility modifiers.
   --	 *
   --	 * The range doesn't have to denote a node range in the sense of a abstract
   --	 * syntax tree. It can therefore not be used to re-construct a hierarchy of
   --	 * the symbols.
   --	 */
   --	location: Location;
   --
   --	/**
   --	 * The name of the symbol containing this symbol. This information is for
   --	 * user interface purposes (e.g. to render a qualifier in the user interface
   --	 * if necessary). It can't be used to re-infer a hierarchy for the document
   --	 * symbols.
   --	 */
   --	containerName?: string;
   --}
   --
   --```
   type DocumentSymbol is record
      name: LSP_String;
      detail: Optional_String;
      kind: SymbolKind;
      deprecated: Optional_Boolean;
      span: LSP.Messages.Span;
      selectionRange: LSP.Messages.Span;
      children: Boolean;  --  True if emit children in JSON
   end record;

   package DocumentSymbol_Trees is new Ada.Containers.Multiway_Trees
     (DocumentSymbol);

   type DocumentSymbol_Tree is
     new DocumentSymbol_Trees.Tree with null record;

   procedure Read_DocumentSymbol_Tree
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DocumentSymbol_Tree);
   procedure Write_DocumentSymbol_Tree
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DocumentSymbol_Tree);
   for DocumentSymbol_Tree'Read use Read_DocumentSymbol_Tree;
   for DocumentSymbol_Tree'Write use Write_DocumentSymbol_Tree;

   type SymbolInformation is record
      name: LSP_String;
      kind: SymbolKind;
      deprecated: Optional_Boolean;
      location: LSP.Messages.Location;
      containerName: Optional_String;
   end record;

   procedure Read_SymbolInformation
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out SymbolInformation);
   procedure Write_SymbolInformation
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SymbolInformation);
   for SymbolInformation'Read use Read_SymbolInformation;
   for SymbolInformation'Write use Write_SymbolInformation;

   package SymbolInformation_Vectors is new LSP.Generic_Vectors
     (SymbolInformation);

   type SymbolInformation_Vector is
     new SymbolInformation_Vectors.Vector with null record;

   type Symbol_Vector (Is_Tree : Boolean := False) is record
      case Is_Tree is
         when True =>
            Tree : DocumentSymbol_Tree;
         when False =>
            Vector : SymbolInformation_Vector;
      end case;
   end record;

   procedure Read_Symbol_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Symbol_Vector);
   procedure Write_Symbol_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Symbol_Vector);
   for Symbol_Vector'Read use Read_Symbol_Vector;
   for Symbol_Vector'Write use Write_Symbol_Vector;

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

   procedure Read_WorkspaceSymbolParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out WorkspaceSymbolParams);
   procedure Write_WorkspaceSymbolParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : WorkspaceSymbolParams);
   for WorkspaceSymbolParams'Read use Read_WorkspaceSymbolParams;
   for WorkspaceSymbolParams'Write use Write_WorkspaceSymbolParams;

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
   -- * The kind of a code action.
   -- *
   -- * Kinds are a hierarchical list of identifiers separated by `.`, e.g. `"refactor.extract.function"`.
   -- *
   -- * The set of kinds is open and client needs to announce the kinds it supports to the server during
   -- * initialization.
   -- */
   --export type CodeActionKind = string;
   --
   --/**
   -- * A set of predefined code action kinds
   -- */
   --export namespace CodeActionKind {
   --
   --	/**
   --	 * Empty kind.
   --	 */
   --	export const Empty: CodeActionKind = '';
   --
   --	/**
   --	 * Base kind for quickfix actions: 'quickfix'
   --	 */
   --	export const QuickFix: CodeActionKind = 'quickfix';
   --
   --	/**
   --	 * Base kind for refactoring actions: 'refactor'
   --	 */
   --	export const Refactor: CodeActionKind = 'refactor';
   --
   --	/**
   --	 * Base kind for refactoring extraction actions: 'refactor.extract'
   --	 *
   --	 * Example extract actions:
   --	 *
   --	 * - Extract method
   --	 * - Extract function
   --	 * - Extract variable
   --	 * - Extract interface from class
   --	 * - ...
   --	 */
   --	export const RefactorExtract: CodeActionKind = 'refactor.extract';
   --
   --	/**
   --	 * Base kind for refactoring inline actions: 'refactor.inline'
   --	 *
   --	 * Example inline actions:
   --	 *
   --	 * - Inline function
   --	 * - Inline variable
   --	 * - Inline constant
   --	 * - ...
   --	 */
   --	export const RefactorInline: CodeActionKind = 'refactor.inline';
   --
   --	/**
   --	 * Base kind for refactoring rewrite actions: 'refactor.rewrite'
   --	 *
   --	 * Example rewrite actions:
   --	 *
   --	 * - Convert JavaScript function to class
   --	 * - Add or remove parameter
   --	 * - Encapsulate field
   --	 * - Make method static
   --	 * - Move method to base class
   --	 * - ...
   --	 */
   --	export const RefactorRewrite: CodeActionKind = 'refactor.rewrite';
   --
   --	/**
   --	 * Base kind for source actions: `source`
   --	 *
   --	 * Source code actions apply to the entire file.
   --	 */
   --	export const Source: CodeActionKind = 'source';
   --
   --	/**
   --	 * Base kind for an organize imports source action: `source.organizeImports`
   --	 */
   --	export const SourceOrganizeImports: CodeActionKind = 'source.organizeImports';
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
   --
   --	/**
   --	 * Requested kind of actions to return.
   --	 *
   --	 * Actions not of this kind are filtered out by the client before being shown. So servers
   --	 * can omit computing them.
   --	 */
   --	only?: CodeActionKind[];
   --}
   --```
   type CodeActionContext is record
      diagnostics: Diagnostic_Vector;
      only: Optional_CodeActionKindSet;
   end record;

   procedure Read_CodeActionContext
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CodeActionContext);
   procedure Write_CodeActionContext
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CodeActionContext);
   for CodeActionContext'Read use Read_CodeActionContext;
   for CodeActionContext'Write use Write_CodeActionContext;

   type CodeActionParams is record
      textDocument: TextDocumentIdentifier;
      span: LSP.Messages.Span;
      context: CodeActionContext;
   end record;

   procedure Read_CodeActionParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CodeActionParams);
   procedure Write_CodeActionParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CodeActionParams);
   for CodeActionParams'Read use Read_CodeActionParams;
   for CodeActionParams'Write use Write_CodeActionParams;

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
   --	/**
   --	 * A data entry field that is preserved on a document link between a
   --	 * DocumentLinkRequest and a DocumentLinkResolveRequest.
   --	 */
   --	data?: any;
   --}
   --```
   type DocumentLink is record
      span: LSP.Messages.Span;
      target: DocumentUri;  --  Optional ???
      --  data?: any
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
   --	 * The document to rename.
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

   procedure Read_RenameParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out RenameParams);
   procedure Write_RenameParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : RenameParams);
   for RenameParams'Read use Read_RenameParams;
   for RenameParams'Write use Write_RenameParams;

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

   procedure Read_ExecuteCommandParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ExecuteCommandParams);
   procedure Write_ExecuteCommandParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ExecuteCommandParams);
   for ExecuteCommandParams'Read use Read_ExecuteCommandParams;
   for ExecuteCommandParams'Write use Write_ExecuteCommandParams;

   --```typescript
   --export interface ApplyWorkspaceEditParams {
   --	/**
   --	 * An optional label of the workspace edit. This label is
   --	 * presented in the user interface for example on an undo
   --	 * stack to undo the workspace edit.
   --	 */
   --	label?: string;
   --
   --	/**
   --	 * The edits to apply.
   --	 */
   --	edit: WorkspaceEdit;
   --}
   --```
   type ApplyWorkspaceEditParams is record
      label: Optional_String;
      edit: WorkspaceEdit;
   end record;

   --```typescript
   --export interface ApplyWorkspaceEditResponse {
   --	/**
   --	 * Indicates whether the edit was applied or not.
   --	 */
   --	applied: boolean;
   --
   --	/**
   --	 * An optional textual description for why the edit was not applied.
   --	 * This may be used may be used by the server for diagnostic
   --	 * logging or to provide a suitable error for a request that
   --	 * triggered the edit.
   --	 */
   --	failureReason?: string;
   --}
   --```
   type ApplyWorkspaceEditResult is record
      applied: Boolean;
      failureReason: Optional_String;
   end record;

   procedure Read_ApplyWorkspaceEditResult
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ApplyWorkspaceEditResult);
   procedure Write_ApplyWorkspaceEditResult
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ApplyWorkspaceEditResult);
   for ApplyWorkspaceEditResult'Read use Read_ApplyWorkspaceEditResult;
   for ApplyWorkspaceEditResult'Write use Write_ApplyWorkspaceEditResult;

   subtype CompletionParams is TextDocumentPositionParams;
   --  ??? this is not in sync with protocol v3

   ----------------------
   -- Present in v3.15 --
   ----------------------

   --  Here are protocol elements defined in the v3.15 working version, which
   --  is not yet published: these might change when the spec is finalized.

   --```typescript
   --type ProgressToken = number | string;
   --interface ProgressParams<T> {
   --	/**
   --	 * The progress token provided by the client or server.
   --	 */
   --	token: ProgressToken;
   --
   --	/**
   --	 * The progress data.
   --	 */
   --	value: T;
   --}
   --```

   generic
      type T is private;
   package Generic_ProgressParam is
      type ProgressParam is record
         token: ProgressToken;
         value: T;
      end record;
   end Generic_ProgressParam;

   --export interface WorkDoneProgressBegin {
   --
   --  kind: 'begin';
   --
   --	/**
   --	 * Mandatory title of the progress operation. Used to briefly inform about
   --	 * the kind of operation being performed.
   --	 *
   --	 * Examples: "Indexing" or "Linking dependencies".
   --	 */
   --	title: string;
   --
   --	/**
   --	 * Controls if a cancel button should show to allow the user to cancel the
   --	 * long running operation. Clients that don't support cancellation are allowed
   --	 * to ignore the setting.
   --	 */
   --	cancellable?: boolean;
   --
   --	/**
   --	 * Optional, more detailed associated progress message. Contains
   --	 * complementary information to the `title`.
   --	 *
   --	 * Examples: "3/25 files", "project/src/module2", "node_modules/some_dep".
   --	 * If unset, the previous progress message (if any) is still valid.
   --	 */
   --	message?: string;
   --
   --	/**
   --	 * Optional progress percentage to display (value 100 is considered 100%).
   --	 * If not provided infinite progress is assumed and clients are allowed
   --	 * to ignore the `percentage` value in subsequent in report notifications.
   --	 *
   --	 * The value should be steadily rising. Clients are free to ignore values
   --	 * that are not following this rule.
   --	 */
   --	percentage?: number;
   --}
   --```
   type WorkDoneProgressBegin is record
      kind        : LSP_String := LSP.Types.To_LSP_String ("begin");
      title       : LSP_String;
      cancellable : Optional_Boolean;
      message     : Optional_String;
      percentage  : Optional_Number;
   end record;

   --```typescript
   --export interface WorkDoneProgressReport {
   --
   --	kind: 'report';

   --	/**
   --	 * Controls enablement state of a cancel button. This property is only valid if a cancel
   --	 * button got requested in the `WorkDoneProgressStart` payload.
   --	 *
   --	 * Clients that don't support cancellation or don't support control the button's
   --	 * enablement state are allowed to ignore the setting.
   --	 */
   --	cancellable?: boolean;

   --	/**
   --	 * Optional, more detailed associated progress message. Contains
   --	 * complementary information to the `title`.
   --	 *
   --	 * Examples: "3/25 files", "project/src/module2", "node_modules/some_dep".
   --	 * If unset, the previous progress message (if any) is still valid.
   --	 */
   --	message?: string;

   --	/**
   --	 * Optional progress percentage to display (value 100 is considered 100%).
   --	 * If not provided infinite progress is assumed and clients are allowed
   --	 * to ignore the `percentage` value in subsequent in report notifications.
   --	 *
   --	 * The value should be steadily rising. Clients are free to ignore values
   --	 * that are not following this rule.
   --	 */
   --	percentage?: number;
   --}
   --```
   type WorkDoneProgressReport is record
      kind        : LSP_String := LSP.Types.To_LSP_String ("report");
      cancellable : Optional_Boolean;
      message     : Optional_String;
      percentage  : Optional_Number;
   end record;

   --export interface WorkDoneProgressEnd {
   --
   --	kind: 'end';
   --
   --	/**
   --	 * Optional, a final message indicating to for example indicate the outcome
   --	 * of the operation.
   --	 */
   --	message?: string;
   --}
   type WorkDoneProgressEnd is record
      kind    : LSP_String := LSP.Types.To_LSP_String ("end");
      message : Optional_String;
   end record;

   --  Writers
   procedure Write_WorkDoneProgressBegin
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : WorkDoneProgressBegin);
   for WorkDoneProgressBegin'Write use Write_WorkDoneProgressBegin;
   procedure Write_WorkDoneProgressReport
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : WorkDoneProgressReport);
   for WorkDoneProgressReport'Write use Write_WorkDoneProgressReport;
   procedure Write_WorkDoneProgressEnd
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : WorkDoneProgressEnd);
   for WorkDoneProgressEnd'Write use Write_WorkDoneProgressEnd;

   --  Pre-instantiate the use cases for the ProgressParams

   package ProgressParam_Begin_Package is new Generic_ProgressParam
     (WorkDoneProgressBegin);
   subtype Progress_Begin_Params is ProgressParam_Begin_Package.ProgressParam;

   package ProgressParam_Report_Package is new Generic_ProgressParam
     (WorkDoneProgressReport);
   subtype Progress_Report_Params is ProgressParam_Report_Package.ProgressParam;

   package ProgressParam_End_Package is new Generic_ProgressParam
     (WorkDoneProgressEnd);
   subtype Progress_End_Params is ProgressParam_Begin_Package.ProgressParam;

   --  The $/progress request has a parameter of the form
   --    {
   --       token: ProgressToken;
   --       value: {
   --          kind : <a string>
   --          <other fields that depend on the value of 'kind' above>
   --
   --  The code below provides an enumerated type and a record with
   --  discriminant to give a type-safe representation of this.

   type Progress_Kind is (Progress_Begin, Progress_Report, Progress_End);
   type Progress_Params (Kind : Progress_Kind := Progress_Begin) is record
      case Kind is
         when Progress_Begin =>
            Begin_Param : Progress_Begin_Params;
         when Progress_Report =>
            Report_Param : Progress_Report_Params;
         when Progress_End =>
            End_Param : Progress_End_Params;
      end case;
   end record;

   procedure Read_Progress_Params
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Progress_Params);
   for Progress_Params'Read use Read_Progress_Params;

   procedure Write_Progress_Params
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Progress_Params);
   for Progress_Params'Write use Write_Progress_Params;

   -----------------------------------------
   -- ALS-specific messages and responses --
   -----------------------------------------

   --  These define protocol extensions.
   --  These are tagged with "ALS_" to avoid namespace clashes.

   type ALS_Subprogram_And_References is record
      --  Location and name of the defining subprogram
      loc  : Location;
      name : LSP_String;

      --  The list of result locations within the defining subprogram
      refs : Location_Vector;
   end record;

   procedure Read_ALS_Subprogram_And_References
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ALS_Subprogram_And_References);
   for ALS_Subprogram_And_References'Read use
     Read_ALS_Subprogram_And_References;

   procedure Write_ALS_Subprogram_And_References
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ALS_Subprogram_And_References);
   for ALS_Subprogram_And_References'Write use
     Write_ALS_Subprogram_And_References;

   package ALS_Subprogram_And_References_Vectors is new LSP.Generic_Vectors
     (ALS_Subprogram_And_References);
   type ALS_Subprogram_And_References_Vector is
     new ALS_Subprogram_And_References_Vectors.Vector with null record;

   type ALS_Debug_Kinds is (Suspend_Execution);
   --  Suspend_Execution - stop processing task until input queue has given
   --  number of messages. After getting this request ALS stops message
   --  processing, but still accepts new requests/notifications. Once
   --  number of input messages reaches given limit, ALS resumes message
   --  processing.

   type ALSDebugParams (Kind : ALS_Debug_Kinds := Suspend_Execution) is record
      case Kind is
         when Suspend_Execution =>
            inputQueueLength : LSP.Types.LSP_Number;
      end case;
   end record;

   procedure Read_ALSDebugParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ALSDebugParams);
   for ALSDebugParams'Read use Read_ALSDebugParams;

   procedure Write_ALSDebugParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ALSDebugParams);
   for ALSDebugParams'Write use Write_ALSDebugParams;

private

   procedure Read_ApplyWorkspaceEditParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ApplyWorkspaceEditParams);

   procedure Read_ShowMessageRequestParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ShowMessageRequestParams);

   procedure Write_ApplyWorkspaceEditParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ApplyWorkspaceEditParams);

   procedure Write_ShowMessageRequestParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ShowMessageRequestParams);

   for ApplyWorkspaceEditParams'Write use Write_ApplyWorkspaceEditParams;
   for ShowMessageRequestParams'Write use Write_ShowMessageRequestParams;

   for ApplyWorkspaceEditParams'Read use Read_ApplyWorkspaceEditParams;
   for ShowMessageRequestParams'Read use Read_ShowMessageRequestParams;

end LSP.Messages;
