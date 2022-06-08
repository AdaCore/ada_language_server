------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2018-2021, AdaCore                     --
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
--  types, corresponding encoding/decoding procedures to/from JSON stream.
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
with Ada.Tags;

with VSS.String_Vectors;
with VSS.Strings;

with LSP.Commands;
with LSP.Errors;
with LSP.Generic_Optional;
with LSP.Generic_Sets;
with LSP.Generic_Vectors;
with LSP.Types; use LSP.Types;

package LSP.Messages is

   pragma Style_Checks ("M175-bcht");
   --  Disable style checks, because some TypeScript snippets are too wide.

   --```typescript
   --/**
   -- * Defines an integer number in the range of -2^31 to 2^31 - 1.
   -- */
   --export type integer = number;
   --```
   --  subtype integer is LSP_Number;

   --```typescript
   --/**
   -- * Defines an unsigned integer number in the range of 0 to 2^31 - 1.
   -- */
   --export type uinteger = number;
   --```
   subtype uinteger is LSP_Number range 0 .. LSP_Number'Last;

   package uinteger_Vectors is new LSP.Generic_Vectors
     (uinteger, Write_Empty => LSP.Write_Array);
   type uinteger_Vector is new uinteger_Vectors.Vector with null record;

   package Optional_uintegers is new LSP.Generic_Optional (uinteger);
   type Optional_uinteger is new Optional_uintegers.Optional_Type;

   --```typescript
   --/**
   -- * Defines a decimal number. Since decimal numbers are very
   -- * rare in the language server specification we denote the
   -- * exact range with every decimal using the mathematics
   -- * interval notation (e.g. [0, 1] denotes all decimals d with
   -- * 0 <= d <= 1.
   -- */
   --export type decimal = number;
   --```
   subtype decimal is LSP.Types.LSP_Number;  --  FIXME

   --```typescript
   --interface Message {
   --	jsonrpc: string;
   --}
   --```
   type Message is abstract tagged record
      jsonrpc: VSS.Strings.Virtual_String := VSS.Strings.Empty_Virtual_String;
      --  Assignment is needed to suppress compiler warning about uninitialized
      --  variables.
   end record;

   --```typescript
   --interface RequestMessage extends Message {
   --
   --	/**
   --	 * The request id.
   --	 */
   --	id: integer | string;
   --
   --	/**
   --	 * The method to be invoked.
   --	 */
   --	method: string;
   --
   --	/**
   --	 * The method's params.
   --	 */
   --	params?: array | object;
   --}
   --```
   type RequestMessage is new Message with record
      id: LSP_Number_Or_String;
      method: VSS.Strings.Virtual_String;
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
   --	id: integer | string | null;
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
   --	error?: ResponseError;
   --}
   --
   --interface ResponseError {
   --	/**
   --	 * A number indicating the error type that occurred.
   --	 */
   --	code: integer;
   --
   --	/**
   --	 * A string providing a short description of the error.
   --	 */
   --	message: string;
   --
   --	/**
   --	 * A primitive or structured value that contains additional
   --	 * information about the error. Can be omitted.
   --	 */
   --	data?: string | number | boolean | array | object | null;
   --}
   --
   --export namespace ErrorCodes {
   --	// Defined by JSON RPC
   --	export const ParseError: integer = -32700;
   --	export const InvalidRequest: integer = -32600;
   --	export const MethodNotFound: integer = -32601;
   --	export const InvalidParams: integer = -32602;
   --	export const InternalError: integer = -32603;
   --
   --	/**
   --	 * This is the start range of JSON RPC reserved error codes.
   --	 * It doesn't denote a real error code. No LSP error codes should
   --	 * be defined between the start and end range. For backwards
   --	 * compatibility the `ServerNotInitialized` and the `UnknownErrorCode`
   --	 * are left in the range.
   --	 *
   --	 * @since 3.16.0
   --	*/
   --	export const jsonrpcReservedErrorRangeStart: integer = -32099;
   --	/** @deprecated use  jsonrpcReservedErrorRangeStart */
   --	export const serverErrorStart: integer = jsonrpcReservedErrorRangeStart;
   --
   --	export const ServerNotInitialized: integer = -32002;
   --	export const UnknownErrorCode: integer = -32001;
   --
   --	/**
   --	 * This is the start range of JSON RPC reserved error codes.
   --	 * It doesn't denote a real error code.
   --	*/
   --	export const jsonrpcReservedErrorRangeEnd = -32000;
   --	/** @deprecated use  jsonrpcReservedErrorRangeEnd */
   --	export const serverErrorEnd: integer = jsonrpcReservedErrorRangeEnd;
   --
   --	/**
   --	 * This is the start range of LSP reserved error codes.
   --	 * It doesn't denote a real error code.
   --	 *
   --	 * @since 3.16.0
   --	 */
   --	export const lspReservedErrorRangeStart: integer = -32899;
   --
   --	export const ContentModified: integer = -32801;
   --	export const RequestCancelled: integer = -32800;
   --
   --	/**
   --	 * This is the end range of LSP reserved error codes.
   --	 * It doesn't denote a real error code.
   --	 *
   --	 * @since 3.16.0
   --	 */
   --	export const lspReservedErrorRangeEnd: integer = -32800;
   --}
   --```
   subtype ErrorCodes is LSP.Errors.ErrorCodes;
   MethodNotFound : constant ErrorCodes := LSP.Errors.MethodNotFound;

   subtype ResponseError is LSP.Errors.ResponseError;

   subtype Optional_ResponseError is LSP.Errors.Optional_ResponseError;

   type ResponseMessage (Is_Error : Boolean) is new Message with record
      id: LSP_Number_Or_String := (others => <>);  --  or null?
      --  Assignment is needed to suppress compiler warning about uninitialized
      --  variables.
      error: Optional_ResponseError (Is_Error);
   end record;

   procedure Write_ResponseMessage
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ResponseMessage);

   for ResponseMessage'Write use Write_ResponseMessage;

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
   --	params?: array | object;
   --}
   --```
   type NotificationMessage is new Message with record
      method: VSS.Strings.Virtual_String := VSS.Strings.Empty_Virtual_String;
      --  Assignment is needed to suppress compiler warning about uninitialized
      --  variables.
   end record;

   procedure Read_NotificationMessage
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out NotificationMessage);
   for NotificationMessage'Read use Read_NotificationMessage;

   procedure Write_NotificationMessage
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : NotificationMessage);
   for NotificationMessage'Write use Write_NotificationMessage;

   --```typescript
   --interface CancelParams {
   --	/**
   --	 * The request id to cancel.
   --	 */
   --	id: integer | string;
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
   subtype DocumentUri is LSP.Types.LSP_URI;

   --```typescript
   --type URI = string;
   --```
   subtype URI is LSP.Types.LSP_URI;

   --```typescript
   --export const EOL: string[] = ['\n', '\r\n', '\r'];
   --```

   --  This is intentionally empty. Nothing to declare for EOL

   --```typescript
   --interface Position {
   --	/**
   --	 * Line position in a document (zero-based).
   --	 */
   --	line: uinteger;
   --
   --	/**
   --	 * Character offset on a line in a document (zero-based). Assuming that
   --	 * the line is represented as a string, the `character` value represents
   --	 * the gap between the `character` and `character + 1`.
   --	 *
   --	 * If the character value is greater than the line length it defaults back
   --	 * to the line length.
   --	 */
   --	character: uinteger;
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

   package Position_Vectors is new LSP.Generic_Vectors
     (Position, Write_Empty => LSP.Write_Array);

   type Position_Vector is new Position_Vectors.Vector with null record;

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
      last: Position;  --  end: is reserved word
   end record;
   --  `Range` is a reserved word in Ada, so let's name it Span

   procedure Read_Span
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Span);
   for Span'Read use Read_Span;

   procedure Write_Span
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Span);
   for Span'Write use Write_Span;

   Empty_Span : constant Span := ((0, 0), (0, 0));

   package Optional_Spans is new LSP.Generic_Optional (Span);
   type Optional_Span is new Optional_Spans.Optional_Type;

   package Optional_Span_Or_Nulls is
     new LSP.Generic_Optional (Span, Write_Unset_As_Null => True);
   type Optional_Span_Or_Null is new Optional_Span_Or_Nulls.Optional_Type;

   package Span_Vectors is new LSP.Generic_Vectors
     (Span, Write_Empty => LSP.Write_Array);

   type Span_Vector is new Span_Vectors.Vector with null record;

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
     new LSP.Generic_Sets (CodeActionKind, LSP.Write_Array);
   type CodeActionKindSet is new CodeActionKindSets.Set;

   package Optional_CodeActionKindSets is
     new LSP.Generic_Optional (CodeActionKindSet);
   type Optional_CodeActionKindSet is
     new Optional_CodeActionKindSets.Optional_Type;

   package Optional_CodeActionKinds is new LSP.Generic_Optional (CodeActionKind);
   type Optional_CodeActionKind is new Optional_CodeActionKinds.Optional_Type;

   --  reference_kinds ALS extension:
   --
   --  export type AlsReferenceKind = 'w' | 'a' | 'c' | 'd' | 'p' | 'h';
   --
   --  export namespace AlsReferenceKind {
   --     export const Write            : AlsReferenceKind = 'write';
   --     export const Access           : AlsReferenceKind = 'access';
   --     export const Static_Call      : AlsReferenceKind = 'call';
   --     export const Dispatching_Call : AlsReferenceKind = 'dispatching call';
   --     export const Parent           : AlsReferenceKind = 'parent';
   --     export const Child            : AlsReferenceKind = 'child';
   --  }

   type AlsReferenceKind is
     (Simple,
      Access_Ref,
      Write,
      Static_Call,
      Dispatching_Call,
      Parent,
      Child,
      Overriding_Decl);

   procedure Read_AlsReferenceKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out AlsReferenceKind);
   procedure Write_AlsReferenceKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : AlsReferenceKind);
   for AlsReferenceKind'Read use Read_AlsReferenceKind;
   for AlsReferenceKind'Write use Write_AlsReferenceKind;

   type AlsReferenceKind_Array is array (AlsReferenceKind) of Boolean;
   type AlsReferenceKind_Set (Is_Server_Side : Boolean := True) is record
      case Is_Server_Side is
         when True =>
            As_Flags   : AlsReferenceKind_Array := (others => False);

         when False =>
            As_Strings : VSS.String_Vectors.Virtual_String_Vector;
      end case;
   end record;
   function Empty_Set return AlsReferenceKind_Set;

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

   package AlsReferenceKind_Vectors is new LSP.Generic_Vectors
     (AlsReferenceKind, Write_Empty => LSP.Write_Array);

   type AlsReferenceKind_Vector is new AlsReferenceKind_Vectors.Vector with
     null record;

   --  Display method ancestry on navigation ALS extension:
   --
   --  export type AlsDisplayMethodAncestryOnNavigationPolicy =
   --     'never' | 'usage_and_abstract_only' | 'definition_only' | 'always';
   --
   --  export namespace AlsDisplayMethodAncestryOnNavigationPolicy {
   --     export const Never                   : AlsReferenceKind = 'never';
   --     export const Usage_And_Abstract_Only : AlsReferenceKind = 'usage_and_absract_only';
   --     export const Definition_Only         : AlsReferenceKind = 'definition_only';
   --     export const Always                  : AlsReferenceKind = 'always';
   --  }

   type AlsDisplayMethodAncestryOnNavigationPolicy is
     (Never, Usage_And_Abstract_Only, Definition_Only, Always);

   procedure Read_AlsDisplayMethodAncestryOnNavigationPolicy
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out AlsDisplayMethodAncestryOnNavigationPolicy);
   procedure Write_AlsDisplayMethodAncestryOnNavigationPolicy
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : AlsDisplayMethodAncestryOnNavigationPolicy);
   for AlsDisplayMethodAncestryOnNavigationPolicy'Read
   use Read_AlsDisplayMethodAncestryOnNavigationPolicy;
   for AlsDisplayMethodAncestryOnNavigationPolicy'Write
   use Write_AlsDisplayMethodAncestryOnNavigationPolicy;

   package Optional_AlsDisplayMethodAncestryOnNavigationPolicies is
     new LSP.Generic_Optional (AlsDisplayMethodAncestryOnNavigationPolicy);
   type Optional_AlsDisplayMethodAncestryOnNavigationPolicy is
     new Optional_AlsDisplayMethodAncestryOnNavigationPolicies.Optional_Type;

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

   overriding function "=" (Left, Right : Location) return Boolean;

   procedure Read_Location
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Location);
   procedure Write_Location
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Location);
   for Location'Read use Read_Location;
   for Location'Write use Write_Location;

   package Location_Vectors is new LSP.Generic_Vectors
     (Location, Write_Empty => LSP.Write_Array);

   type Location_Vector is new Location_Vectors.Vector with null record;

   package Optional_Locations is new LSP.Generic_Optional (Location);
   type Optional_Location is new Optional_Locations.Optional_Type;

   --```typescript
   --interface LocationLink {
   --
   --	/**
   --	 * Span of the origin of this link.
   --	 *
   --	 * Used as the underlined span for mouse interaction. Defaults to the word
   --	 * range at the mouse position.
   --	 */
   --	originSelectionRange?: Range;
   --
   --	/**
   --	 * The target resource identifier of this link.
   --	 */
   --	targetUri: DocumentUri;
   --
   --	/**
   --	 * The full target range of this link. If the target for example is a symbol
   --	 * then target range is the range enclosing this symbol not including
   --	 * leading/trailing whitespace but everything else like comments. This
   --	 * information is typically used to highlight the range in the editor.
   --	 */
   --	targetRange: Range;
   --
   --	/**
   --	 * The range that should be selected and revealed when this link is being
   --	 * followed, e.g the name of a function. Must be contained by the the
   --	 * `targetRange`. See also `DocumentSymbol#range`
   --	 */
   --	targetSelectionRange: Range;
   --}
   --```
   type LocationLink is record
      originSelectionRange : Optional_Span;
      targetUri            : VSS.Strings.Virtual_String;
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

   package LocationLink_Vectors is new LSP.Generic_Vectors
     (LocationLink, Write_Empty => LSP.Write_Array);

   type LocationLink_Vector is new LocationLink_Vectors.Vector with null record;

   type Location_Or_Link_Kind is
     (Empty_Vector_Kind,
      Location_Vector_Kind,
      LocationLink_Vector_Kind);

   type Location_Or_Link_Vector
     (Kind : Location_Or_Link_Kind := Location_Vector_Kind) is
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
   --export namespace DiagnosticSeverity {
   --	/**
   --	 * Reports an error.
   --	 */
   --	export const Error: 1 = 1;
   --	/**
   --	 * Reports a warning.
   --	 */
   --	export const Warning: 2 = 2;
   --	/**
   --	 * Reports an information.
   --	 */
   --	export const Information: 3 = 3;
   --	/**
   --	 * Reports a hint.
   --	 */
   --	export const Hint: 4 = 4;
   --}
   --
   --export type DiagnosticSeverity = 1 | 2 | 3 | 4;
   --
   --/**
   -- * The diagnostic tags.
   -- *
   -- * @since 3.15.0
   -- */
   --export namespace DiagnosticTag {
   --    /**
   --     * Unused or unnecessary code.
   --     *
   --     * Clients are allowed to render diagnostics with this tag faded out
   --	 * instead of having an error squiggle.
   --     */
   --    export const Unnecessary: 1 = 1;
   --    /**
   --     * Deprecated or obsolete code.
   --     *
   --     * Clients are allowed to rendered diagnostics with this tag strike through.
   --     */
   --    export const Deprecated: 2 = 2;
   --}
   --
   --export type DiagnosticTag = 1 | 2;
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

   type DiagnosticTag is (Unnecessary, Deprecated);

   procedure Read_DiagnosticTag
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DiagnosticTag);
   procedure Write_DiagnosticTag
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DiagnosticTag);
   for DiagnosticTag'Read use Read_DiagnosticTag;
   for DiagnosticTag'Write use Write_DiagnosticTag;

   package DiagnosticTagSets is new LSP.Generic_Sets (DiagnosticTag, LSP.Write_Array);

   type DiagnosticTagSet is new DiagnosticTagSets.Set;

   package Optional_DiagnosticTagSets is
     new LSP.Generic_Optional (DiagnosticTagSet);

   type Optional_DiagnosticTagSet is
     new Optional_DiagnosticTagSets.Optional_Type;

   --```typescript
   --/**
   -- * Structure to capture a description for an error code.
   -- *
   -- * @since 3.16.0
   -- */
   --export interface CodeDescription {
   --	/**
   --	 * An URI to open with more information about the diagnostic error.
   --	 */
   --	href: URI;
   --}
   --```
   type CodeDescription is record
      href: URI;
   end record;

   procedure Read_CodeDescription
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CodeDescription);
   procedure Write_CodeDescription
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CodeDescription);
   for CodeDescription'Read use Read_CodeDescription;
   for CodeDescription'Write use Write_CodeDescription;

   package Optional_CodeDescriptions is
     new LSP.Generic_Optional (CodeDescription);

   type Optional_CodeDescription is
     new Optional_CodeDescriptions.Optional_Type;

   --```typescript
   --/**
   -- * Represents a related message and source code location for a diagnostic.
   -- * This should be used to point to code locations that cause or are related to
   -- * a diagnostics, e.g when duplicating a symbol in a scope.
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
      message: VSS.Strings.Virtual_String;
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
     (DiagnosticRelatedInformation, Write_Empty => LSP.Skip);

   type DiagnosticRelatedInformation_Vector is
     new DiagnosticRelatedInformation_Vectors.Vector with null record;

   --```typescript
   --export interface Diagnostic {
   --	/**
   --	 * The range at which the message applies.
   --	 */
   --	range: Range;
   --
   --	/**
   --	 * The diagnostic's severity. Can be omitted. If omitted it is up to the
   --	 * client to interpret diagnostics as error, warning, info or hint.
   --	 */
   --	severity?: DiagnosticSeverity;
   --
   --	/**
   --	 * The diagnostic's code, which might appear in the user interface.
   --	 */
   --	code?: integer | string;
   --
   --	/**
   --	 * An optional property to describe the error code.
   --	 *
   --	 * @since 3.16.0
   --	 */
   --	codeDescription?: CodeDescription;
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
   --	 * Additional metadata about the diagnostic.
   --	 *
   --	 * @since 3.15.0
   --	 */
   --	tags?: DiagnosticTag[];
   --
   --	/**
   --	 * An array of related diagnostic information, e.g. when symbol-names within
   --	 * a scope collide all definitions can be marked via this property.
   --	 */
   --	relatedInformation?: DiagnosticRelatedInformation[];
   --
   --	/**
   --	 * A data entry field that is preserved between a
   --	 * `textDocument/publishDiagnostics` notification and
   --	 * `textDocument/codeAction` request.
   --	 *
   --	 * @since 3.16.0
   --	 */
   --	data?: unknown;
   --}
   --```
   type Diagnostic is record
      span               : LSP.Messages.Span;
      severity           : Optional_DiagnosticSeverity;
      code               : LSP_Number_Or_String;
      codeDescription    : Optional_CodeDescription;
      source             : Optional_Virtual_String;
      message            : VSS.Strings.Virtual_String;
      tags               : Optional_DiagnosticTagSet;
      relatedInformation : DiagnosticRelatedInformation_Vector;
      --  data?: unknown;
   end record;

   procedure Read_Diagnostic
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Diagnostic);
   for Diagnostic'Read use Read_Diagnostic;

   procedure Write_Diagnostic
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Diagnostic);
   for Diagnostic'Write use Write_Diagnostic;

   package Diagnostic_Vectors is new LSP.Generic_Vectors
     (Diagnostic, Write_Empty => LSP.Write_Array);

   type Diagnostic_Vector is new Diagnostic_Vectors.Vector with null record;

   package Optional_Diagnostic_Vectors is new LSP.Generic_Optional (Diagnostic_Vector);
   type Optional_Diagnostic_Vector is new Optional_Diagnostic_Vectors.Optional_Type;

   package Any_Vectors is new LSP.Generic_Vectors
     (LSP.Types.LSP_Any, Write_Empty => LSP.Write_Array);
   type Any_Vector is new Any_Vectors.Vector with null record;

   package Optional_Any_Vectors is new LSP.Generic_Optional (Any_Vector);
   type Optional_Any_Vector is new Optional_Any_Vectors.Optional_Type;

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
   type Command (Is_Unknown : Boolean := True) is record
      title : VSS.Strings.Virtual_String;

      case Is_Unknown is
         when True =>
            command   : VSS.Strings.Virtual_String;
            arguments : Optional_Any_Vector;
            --  Unknown commands are represented as Any_Vector. The client has
            --  only unknown commands.
         when False =>
            Custom    : LSP.Commands.Command_Pointer;
            --  The server has a predefined set of known commands.
      end case;
   end record;

   procedure Read_Command
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Command);
   procedure Write_Command
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Command);
   for Command'Read use Read_Command;
   for Command'Write use Write_Command;

   package Optional_Commands is new LSP.Generic_Optional (Command);
   type Optional_Command is new Optional_Commands.Optional_Type;

   package Command_Vectors is new LSP.Generic_Vectors
     (Command, Write_Empty => LSP.Write_Array);

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
   type TextEdit is tagged record
      span    : LSP.Messages.Span;
      newText : VSS.Strings.Virtual_String;
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

   package TextEdit_Vectors is new LSP.Generic_Vectors
     (TextEdit, Write_Empty => LSP.Write_Array);

   type TextEdit_Vector is new TextEdit_Vectors.Vector with null record;

   --```typescript
   --
   --/**
   -- * An identifier referring to a change annotation managed by a workspace
   -- * edit.
   -- *
   -- * @since 3.16.0
   -- */
   --export type ChangeAnnotationIdentifier = string;
   --
   --
   --/**
   -- * A special text edit with an additional change annotation.
   -- *
   -- * @since 3.16.0
   -- */
   --export interface AnnotatedTextEdit extends TextEdit {
   --	/**
   --	 * The actual annotation identifier.
   --	 */
   --	annotationId: ChangeAnnotationIdentifier;
   --}
   --```
   type AnnotatedTextEdit is new TextEdit with record
      --  Make id optional to represent both AnnotatedTextEdit and TextEdit
      annotationId : Optional_Virtual_String;
   end record;

   procedure Read_AnnotatedTextEdit
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out AnnotatedTextEdit);

   procedure Write_AnnotatedTextEdit
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : AnnotatedTextEdit);

   for AnnotatedTextEdit'Read use Read_AnnotatedTextEdit;
   for AnnotatedTextEdit'Write use Write_AnnotatedTextEdit;

   package AnnotatedTextEdit_Vectors is new LSP.Generic_Vectors
     (AnnotatedTextEdit, Write_Empty => LSP.Write_Array);

   type AnnotatedTextEdit_Vector is new AnnotatedTextEdit_Vectors.Vector
     with null record;

   subtype ChangeAnnotationIdentifier is VSS.Strings.Virtual_String;

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
   --	 * The version number of this document.
   --	 *
   --	 * The version number of a document will increase after each change,
   --	 * including undo/redo. The number doesn't need to be consecutive.
   --	 */
   --	version: integer;
   --}
   --```
   type VersionedTextDocumentIdentifier is new TextDocumentIdentifier with record
      version: LSP_Number;
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

   package Nullable_Numbers is new LSP.Generic_Optional
     (LSP_Number, Write_Unset_As_Null => True);
   type Nullable_Number is new Nullable_Numbers.Optional_Type;

   --```typescript
   --interface OptionalVersionedTextDocumentIdentifier extends TextDocumentIdentifier {
   --	/**
   --	 * The version number of this document. If an optional versioned text document
   --	 * identifier is sent from the server to the client and the file is not
   --	 * open in the editor (the server has not received an open notification
   --	 * before) the server can send `null` to indicate that the version is
   --	 * known and the content on disk is the master (as specified with document
   --	 * content ownership).
   --	 *
   --	 * The version number of a document will increase after each change,
   --	 * including undo/redo. The number doesn't need to be consecutive.
   --	 */
   --	version: integer | null;
   --}
   --```
   type OptionalVersionedTextDocumentIdentifier is new TextDocumentIdentifier with record
      version: Nullable_Number;
   end record;

   procedure Read_OptionalVersionedTextDocumentIdentifier
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out OptionalVersionedTextDocumentIdentifier);
   for OptionalVersionedTextDocumentIdentifier'Read use
     Read_OptionalVersionedTextDocumentIdentifier;

   procedure Write_OptionalVersionedTextDocumentIdentifier
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : OptionalVersionedTextDocumentIdentifier);
   for OptionalVersionedTextDocumentIdentifier'Write use
     Write_OptionalVersionedTextDocumentIdentifier;

   --```typescript
   --export interface TextDocumentEdit {
   --	/**
   --	 * The text document to change.
   --	 */
   --	textDocument: OptionalVersionedTextDocumentIdentifier;
   --
   --	/**
   --	 * The edits to be applied.
   --	 *
   --	 * @since 3.16.0 - support for AnnotatedTextEdit. This is guarded by the
   --	 * client capability `workspace.workspaceEdit.changeAnnotationSupport`
   --	 */
   --	edits: (TextEdit | AnnotatedTextEdit)[];
   --}
   --```
   type TextDocumentEdit is record
      textDocument: OptionalVersionedTextDocumentIdentifier;
      edits: AnnotatedTextEdit_Vector;
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
      Equivalent_Keys => LSP.Types.Equal);

   --```typescript
   --/**
   -- * Options to create a file.
   -- */
   --export interface CreateFileOptions {
   --	/**
   --	 * Overwrite existing file. Overwrite wins over `ignoreIfExists`
   --	 */
   --	overwrite?: boolean;
   --
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
   --
   --	/**
   --	 * The resource to create.
   --	 */
   --	uri: DocumentUri;
   --
   --	/**
   --	 * Additional options
   --	 */
   --	options?: CreateFileOptions;
   --
   --	/**
   --	 * An optional annotation identifer describing the operation.
   --	 *
   --	 * @since 3.16.0
   --	 */
   --	annotationId?: ChangeAnnotationIdentifier;
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
   --
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
   --
   --	/**
   --	 * The old (existing) location.
   --	 */
   --	oldUri: DocumentUri;
   --
   --	/**
   --	 * The new location.
   --	 */
   --	newUri: DocumentUri;
   --
   --	/**
   --	 * Rename options.
   --	 */
   --	options?: RenameFileOptions;
   --
   --	/**
   --	 * An optional annotation identifer describing the operation.
   --	 *
   --	 * @since 3.16.0
   --	 */
   --	annotationId?: ChangeAnnotationIdentifier;
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
   --
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
   --
   --	/**
   --	 * The file to delete.
   --	 */
   --	uri: DocumentUri;
   --
   --	/**
   --	 * Delete options.
   --	 */
   --	options?: DeleteFileOptions;
   --
   --	/**
   --	 * An optional annotation identifer describing the operation.
   --	 *
   --	 * @since 3.16.0
   --	 */
   --	annotationId?: ChangeAnnotationIdentifier;
   --}
   --```

   type FileResourceChangeKind is (create, rename, delete);

   procedure Read_FileResourceChangeKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.FileResourceChangeKind);
   for FileResourceChangeKind'Read use Read_FileResourceChangeKind;

   procedure Write_FileResourceChangeKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.FileResourceChangeKind);
   for FileResourceChangeKind'Write use Write_FileResourceChangeKind;

   type CreateFileOptions is record
      overwrite      : Optional_Boolean;
      ignoreIfExists : Optional_Boolean;
   end record;

   procedure Read_CreateFileOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.CreateFileOptions);
   for CreateFileOptions'Read use Read_CreateFileOptions;

   procedure Write_CreateFileOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.CreateFileOptions);
   for CreateFileOptions'Write use Write_CreateFileOptions;

   type CreateFile is record
      kind         : FileResourceChangeKind range create .. create := create;
      uri          : DocumentUri;
      options      : CreateFileOptions;
      annotationId : Optional_Virtual_String;
   end record;

   procedure Read_CreateFile
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.CreateFile);
   for CreateFile'Read use Read_CreateFile;

   procedure Write_CreateFile
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.CreateFile);
   for CreateFile'Write use Write_CreateFile;

   type RenameFileOptions is record
      overwrite      : Optional_Boolean;
      ignoreIfExists : Optional_Boolean;
   end record;

   procedure Read_RenameFileOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.RenameFileOptions);
   for RenameFileOptions'Read use Read_RenameFileOptions;

   procedure Write_RenameFileOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.RenameFileOptions);
   for RenameFileOptions'Write use Write_RenameFileOptions;

   type RenameFile is record
      kind         : FileResourceChangeKind range rename .. rename := rename;
      oldUri       : DocumentUri;
      newUri       : DocumentUri;
      options      : RenameFileOptions;
      annotationId : Optional_Virtual_String;
   end record;

   procedure Read_RenameFile
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.RenameFile);
   for RenameFile'Read use Read_RenameFile;

   procedure Write_RenameFile
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.RenameFile);
   for RenameFile'Write use Write_RenameFile;

   type DeleteFileOptions is record
      recursive         : Optional_Boolean;
      ignoreIfNotExists : Optional_Boolean;
   end record;

   procedure Read_DeleteFileOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.DeleteFileOptions);
   for DeleteFileOptions'Read use Read_DeleteFileOptions;

   procedure Write_DeleteFileOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.DeleteFileOptions);
   for DeleteFileOptions'Write use Write_DeleteFileOptions;

   type DeleteFile is record
      kind         : FileResourceChangeKind range delete .. delete := delete;
      uri          : DocumentUri;
      options      : DeleteFileOptions;
      annotationId : Optional_Virtual_String;
   end record;

   procedure Read_DeleteFile
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.DeleteFile);
   for DeleteFile'Read use Read_DeleteFile;

   procedure Write_DeleteFile
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.DeleteFile);
   for DeleteFile'Write use Write_DeleteFile;

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

   package Document_Change_Vectors is new LSP.Generic_Vectors
     (Document_Change, Write_Empty => LSP.Write_Array);

   type Document_Change_Vector is
     new Document_Change_Vectors.Vector with null record;

   --```typescript
   --/**
   -- * Additional information that describes document changes.
   -- *
   -- * @since 3.16.0
   -- */
   --export interface ChangeAnnotation {
   --	/**
   --	 * A human-readable string describing the actual change. The string
   --	 * is rendered prominent in the user interface.
   --	 */
   --	label: string;
   --
   --	/**
   --	 * A flag which indicates that user confirmation is needed
   --	 * before applying the change.
   --	 */
   --	needsConfirmation?: boolean;
   --
   --	/**
   --	 * A human-readable string which is rendered less prominent in
   --	 * the user interface.
   --	 */
   --	description?: string;
   --}
   --```
   type ChangeAnnotation is record
      label             : VSS.Strings.Virtual_String;
      needsConfirmation : Optional_Boolean;
      description       : Optional_Virtual_String;
   end record;

   procedure Read_ChangeAnnotation
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ChangeAnnotation);

   procedure Write_ChangeAnnotation
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ChangeAnnotation);

   for ChangeAnnotation'Read use Read_ChangeAnnotation;
   for ChangeAnnotation'Write use Write_ChangeAnnotation;

   package ChangeAnnotation_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => ChangeAnnotationIdentifier,
      Element_Type    => ChangeAnnotation,
      Hash            => LSP.Types.Hash,
      Equivalent_Keys => VSS.Strings."=");

   --```typescript
   --export interface WorkspaceEdit {
   --	/**
   --	 * Holds changes to existing resources.
   --	 */
   --	changes?: { [uri: DocumentUri]: TextEdit[]; };
   --
   --	/**
   --	 * Depending on the client capability
   --	 * `workspace.workspaceEdit.resourceOperations` document changes are either
   --	 * an array of `TextDocumentEdit`s to express changes to n different text
   --	 * documents where each text document edit addresses a specific version of
   --	 * a text document. Or it can contain above `TextDocumentEdit`s mixed with
   --	 * create, rename and delete file / folder operations.
   --	 *
   --	 * Whether a client supports versioned document edits is expressed via
   --	 * `workspace.workspaceEdit.documentChanges` client capability.
   --	 *
   --	 * If a client neither supports `documentChanges` nor
   --	 * `workspace.workspaceEdit.resourceOperations` then only plain `TextEdit`s
   --	 * using the `changes` property are supported.
   --	 */
   --	documentChanges?: (
   --		TextDocumentEdit[] |
   --		(TextDocumentEdit | CreateFile | RenameFile | DeleteFile)[]
   --	);
   --
   --	/**
   --	 * A map of change annotations that can be referenced in
   --	 * `AnnotatedTextEdit`s or create, rename and delete file / folder
   --	 * operations.
   --	 *
   --	 * Whether clients honor this property depends on the client capability
   --	 * `workspace.changeAnnotationSupport`.
   --	 *
   --	 * @since 3.16.0
   --	 */
   --	changeAnnotations?: {
   --		[id: string /* ChangeAnnotationIdentifier */]: ChangeAnnotation;
   --	}
   --}
   --```
   type WorkspaceEdit is record
      changes: TextDocumentEdit_Maps.Map;
      documentChanges: Document_Change_Vector;
      changeAnnotations: ChangeAnnotation_Maps.Map;
   end record;

   procedure Read_WorkspaceEdit
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out WorkspaceEdit);
   for WorkspaceEdit'Read use Read_WorkspaceEdit;

   procedure Write_WorkspaceEdit
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : WorkspaceEdit);
   for WorkspaceEdit'Write use Write_WorkspaceEdit;

   package Optional_WorkspaceEdits is new LSP.Generic_Optional (WorkspaceEdit);
   type Optional_WorkspaceEdit is new Optional_WorkspaceEdits.Optional_Type;

   package Optional_WorkspaceEdit_Or_Nulls is
     new LSP.Generic_Optional (WorkspaceEdit, Write_Unset_As_Null => True);
   type Optional_WorkspaceEdit_Or_Null is
     new Optional_WorkspaceEdit_Or_Nulls.Optional_Type;

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
   --	version: integer;
   --
   --	/**
   --	 * The content of the opened text document.
   --	 */
   --	text: string;
   --}
   --```
   type TextDocumentItem is record
      uri        : DocumentUri;
      languageId : VSS.Strings.Virtual_String;
      version    : Version_Id;
      text       : VSS.Strings.Virtual_String;
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

   procedure Get_TextDocumentPositionParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out TextDocumentPositionParams'Class);

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
   --	 * - `{}` to group sub patterns into an OR expression. (e.g. `**​/*.{ts,js}`
   --	 *   matches all TypeScript and JavaScript files)
   --	 * - `[]` to declare a range of characters to match in a path segment
   --	 *   (e.g., `example.[0-9]` to match on `example.0`, `example.1`, …)
   --	 * - `[!...]` to negate a range of characters to match in a path segment
   --	 *   (e.g., `example.[!0-9]` to match on `example.a`, `example.b`, but
   --	 *   not `example.0`)
   --	 */
   --	pattern?: string;
   --}
   --```
   type DocumentFilter is record
      language : LSP.Types.Optional_Virtual_String;
      scheme   : LSP.Types.Optional_Virtual_String;
      pattern  : LSP.Types.Optional_Virtual_String;
   end record;

   package DocumentFilter_Vectors is new
     LSP.Generic_Vectors (DocumentFilter, Write_Empty => LSP.Write_Null);
   --```typescript
   --export type DocumentSelector = DocumentFilter[];
   --```
   type DocumentSelector is new DocumentFilter_Vectors.Vector with null record;

   type dynamicRegistration is record
      dynamicRegistration: Optional_Boolean;
   end record;

   procedure Read_dynamicRegistration
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out dynamicRegistration);

   procedure Write_dynamicRegistration
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : dynamicRegistration);

   for dynamicRegistration'Read use Read_dynamicRegistration;
   for dynamicRegistration'Write use Write_dynamicRegistration;

   package Optional_dynamicRegistrations is
     new LSP.Generic_Optional (dynamicRegistration);

   type Optional_dynamicRegistration is
     new Optional_dynamicRegistrations.Optional_Type;

   --
   --```typescript
   --export interface WorkspaceEditClientCapabilities {
   --	/**
   --	 * The client supports versioned document changes in `WorkspaceEdit`s
   --	 */
   --	documentChanges?: boolean;
   --
   --	/**
   --	 * The resource operations the client supports. Clients should at least
   --	 * support 'create', 'rename' and 'delete' files and folders.
   --	 *
   --	 * @since 3.13.0
   --	 */
   --	resourceOperations?: ResourceOperationKind[];
   --
   --	/**
   --	 * The failure handling strategy of a client if applying the workspace edit
   --	 * fails.
   --	 *
   --	 * @since 3.13.0
   --	 */
   --	failureHandling?: FailureHandlingKind;
   --
   --	/**
   --	 * Whether the client normalizes line endings to the client specific
   --	 * setting.
   --	 * If set to `true` the client will normalize line ending characters
   --	 * in a workspace edit to the client specific new line character(s).
   --	 *
   --	 * @since 3.16.0
   --	 */
   --	normalizesLineEndings?: boolean;
   --
   --	/**
   --	 * Whether the client in general supports change annotations on text edits,
   --	 * create file, rename file and delete file changes.
   --	 *
   --	 * @since 3.16.0
   --	 */
   --	changeAnnotationSupport?: {
   --        /**
   --         * Whether the client groups edits with equal labels into tree nodes,
   --         * for instance all edits labelled with "Changes in Strings" would
   --         * be a tree node.
   --         */
   --        groupsOnLabel?: boolean;
   --	};
   --}
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
   --export type FailureHandlingKind = 'abort' | 'transactional' | 'undo'
   --	| 'textOnlyTransactional';
   --
   --export namespace FailureHandlingKind {
   --
   --	/**
   --	 * Applying the workspace change is simply aborted if one of the changes
   --	 * provided fails. All operations executed before the failing operation
   --	 * stay executed.
   --	 */
   --	export const Abort: FailureHandlingKind = 'abort';
   --
   --	/**
   --	 * All operations are executed transactional. That means they either all
   --	 * succeed or no changes at all are applied to the workspace.
   --	 */
   --	export const Transactional: FailureHandlingKind = 'transactional';
   --
   --
   --	/**
   --	 * If the workspace edit contains only textual file changes they are
   --	 * executed transactional. If resource changes (create, rename or delete
   --	 * file) are part of the change the failure handling strategy is abort.
   --	 */
   --	export const TextOnlyTransactional: FailureHandlingKind
   --		= 'textOnlyTransactional';
   --
   --	/**
   --	 * The client tries to undo the operations already executed. But there is no
   --	 * guarantee that this is succeeding.
   --	 */
   --	export const Undo: FailureHandlingKind = 'undo';
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
     new LSP.Generic_Sets (ResourceOperationKind, LSP.Write_Array);

   type ResourceOperationKindSet is new ResourceOperationKindSets.Set;

   package Optional_ResourceOperationKindSets is
     new LSP.Generic_Optional (ResourceOperationKindSet);

   type Optional_ResourceOperationKindSet is
     new Optional_ResourceOperationKindSets.Optional_Type;

   type FailureHandlingKind is
     (abortApplying,  --  'abort' is reserver word in Ada, so change it
      transactional, undo, textOnlyTransactional);

   procedure Read_FailureHandlingKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out FailureHandlingKind);

   procedure Write_FailureHandlingKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : FailureHandlingKind);

   for FailureHandlingKind'Read use Read_FailureHandlingKind;
   for FailureHandlingKind'Write use Write_FailureHandlingKind;

   package Optional_FailureHandlingKinds is
     new LSP.Generic_Optional (FailureHandlingKind);

   type Optional_FailureHandlingKind is
     new Optional_FailureHandlingKinds.Optional_Type;

   type AnnotationSupport is record
      groupsOnLabel: Optional_Boolean;
   end record;

   procedure Read_AnnotationSupport
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out AnnotationSupport);

   procedure Write_AnnotationSupport
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : AnnotationSupport);

   for AnnotationSupport'Read use Read_AnnotationSupport;
   for AnnotationSupport'Write use Write_AnnotationSupport;

   package Optional_AnnotationSupports is
     new LSP.Generic_Optional (AnnotationSupport);

   type Optional_AnnotationSupport is
     new Optional_AnnotationSupports.Optional_Type;

   type WorkspaceEditClientCapabilities is record
      documentChanges : Optional_Boolean;
      resourceOperations : Optional_ResourceOperationKindSet;
      failureHandling : Optional_FailureHandlingKind;
      normalizesLineEndings : Optional_Boolean;
      changeAnnotationSupport : Optional_AnnotationSupport;
   end record;

   procedure Read_WorkspaceEditClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out WorkspaceEditClientCapabilities);

   procedure Write_WorkspaceEditClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : WorkspaceEditClientCapabilities);

   for WorkspaceEditClientCapabilities'Read use
     Read_WorkspaceEditClientCapabilities;

   for WorkspaceEditClientCapabilities'Write use
     Write_WorkspaceEditClientCapabilities;

   --```typescript
   --export interface DidChangeConfigurationClientCapabilities {
   --	/**
   --	 * Did change configuration notification supports dynamic registration.
   --	 */
   --	dynamicRegistration?: boolean;
   --}
   --```
   subtype DidChangeConfigurationClientCapabilities
     is Optional_dynamicRegistration;

   --```typescript
   --export interface DidChangeWatchedFilesClientCapabilities {
   --	/**
   --	 * Did change watched files notification supports dynamic registration.
   --	 * Please note that the current protocol doesn't support static
   --	 * configuration for file changes from the server side.
   --	 */
   --	dynamicRegistration?: boolean;
   --}
   --```
   subtype DidChangeWatchedFilesClientCapabilities
     is Optional_dynamicRegistration;

   --```typescript
   --interface WorkspaceSymbolClientCapabilities {
   --	/**
   --	 * Symbol request supports dynamic registration.
   --	 */
   --	dynamicRegistration?: boolean;
   --
   --	/**
   --	 * Specific capabilities for the `SymbolKind` in the `workspace/symbol`
   --	 * request.
   --	 */
   --	symbolKind?: {
   --		/**
   --		 * The symbol kind values the client supports. When this
   --		 * property exists the client also guarantees that it will
   --		 * handle values outside its set gracefully and falls back
   --		 * to a default value when unknown.
   --		 *
   --		 * If this property is not present the client only supports
   --		 * the symbol kinds from `File` to `Array` as defined in
   --		 * the initial version of the protocol.
   --		 */
   --		valueSet?: SymbolKind[];
   --	}
   --
   --	/**
   --	 * The client supports tags on `SymbolInformation`.
   --	 * Clients supporting tags have to handle unknown tags gracefully.
   --	 *
   --	 * @since 3.16.0
   --	 */
   --	tagSupport?: {
   --		/**
   --		 * The tags supported by the client.
   --		 */
   --		valueSet: SymbolTag[]
   --	}
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

   package SymbolKindSets is new LSP.Generic_Sets (SymbolKind, LSP.Write_Array);

   type SymbolKindSet is new SymbolKindSets.Set;

   Default_SymbolKindSet : constant SymbolKindSet :=
     To_Set (From => File, To => An_Array);

   package Optional_SymbolKindSets is
     new LSP.Generic_Optional (SymbolKindSet);

   type Optional_SymbolKindSet is
     new Optional_SymbolKindSets.Optional_Type;

   type symbolKindCapabilities is record
      valueSet: Optional_SymbolKindSet;
   end record;

   procedure Read_symbolKindCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out symbolKindCapabilities);

   procedure Write_symbolKindCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : symbolKindCapabilities);

   for symbolKindCapabilities'Read use Read_symbolKindCapabilities;
   for symbolKindCapabilities'Write use Write_symbolKindCapabilities;

   package Optional_symbolKindCapabilities_Package is
     new LSP.Generic_Optional (symbolKindCapabilities);

   type Optional_symbolKindCapabilities is
     new Optional_symbolKindCapabilities_Package.Optional_Type;

   type SymbolTag is (Deprecated);

   procedure Read_SymbolTag
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out SymbolTag);
   procedure Write_SymbolTag
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SymbolTag);
   for SymbolTag'Read use Read_SymbolTag;
   for SymbolTag'Write use Write_SymbolTag;

   package SymbolTagSets is new LSP.Generic_Sets (SymbolTag, LSP.Skip);
   type SymbolTagSet is new SymbolTagSets.Set;

   package Optional_SymbolTagSets is
     new LSP.Generic_Optional (SymbolTagSet);

   type Optional_SymbolTagSet is
     new Optional_SymbolTagSets.Optional_Type;

   type tagSupportCapability is record
      valueSet: SymbolTagSet;
   end record;

   procedure Read_tagSupportCapability
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out tagSupportCapability);

   procedure Write_tagSupportCapability
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : tagSupportCapability);

   for tagSupportCapability'Read use Read_tagSupportCapability;
   for tagSupportCapability'Write use Write_tagSupportCapability;

   package Optional_tagSupportCapability_Package is
     new LSP.Generic_Optional (tagSupportCapability);
   type Optional_tagSupportCapability is
     new Optional_tagSupportCapability_Package.Optional_Type;

   type Als_Visibility is
     (Als_Public,
      Als_Protected,
      Als_Private);

   procedure Read_Als_Visibility
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Als_Visibility);

   procedure Write_Als_Visibility
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Als_Visibility);

   for Als_Visibility'Read use Read_Als_Visibility;
   for Als_Visibility'Write use Write_Als_Visibility;

   package Optional_Als_Visibilities is
     new LSP.Generic_Optional (Als_Visibility);

   type Optional_Als_Visibility is
     new Optional_Als_Visibilities.Optional_Type;

   type WorkspaceSymbolClientCapabilities is record
      dynamicRegistration: Optional_Boolean;
      symbolKind: Optional_symbolKindCapabilities;
      tagSupport: Optional_tagSupportCapability;
   end record;

   procedure Read_WorkspaceSymbolClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out WorkspaceSymbolClientCapabilities);

   procedure Write_WorkspaceSymbolClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : WorkspaceSymbolClientCapabilities);

   for WorkspaceSymbolClientCapabilities'Read use
     Read_WorkspaceSymbolClientCapabilities;

   for WorkspaceSymbolClientCapabilities'Write use
     Write_WorkspaceSymbolClientCapabilities;

   package Optional_WorkspaceSymbolClientCapabilities_Package is
     new LSP.Generic_Optional (WorkspaceSymbolClientCapabilities);

   type Optional_WorkspaceSymbolClientCapabilities is
     new Optional_WorkspaceSymbolClientCapabilities_Package.Optional_Type;

   --```typescript
   --export interface ExecuteCommandClientCapabilities {
   --	/**
   --	 * Execute command supports dynamic registration.
   --	 */
   --	dynamicRegistration?: boolean;
   --}
   --```
   subtype ExecuteCommandClientCapabilities is Optional_dynamicRegistration;

   --```typescript
   --export interface SemanticTokensWorkspaceClientCapabilities {
   --	/**
   --	 * Whether the client implementation supports a refresh request sent from
   --	 * the server to the client.
   --	 *
   --	 * Note that this event is global and will force the client to refresh all
   --	 * semantic tokens currently shown. It should be used with absolute care
   --	 * and is useful for situation where a server for example detect a project
   --	 * wide change that requires such a calculation.
   --	 */
   --	refreshSupport?: boolean;
   --}
   --```
   type SemanticTokensWorkspaceClientCapabilities is record
      refreshSupport: Optional_Boolean;
   end record;

   procedure Read_SemanticTokensWorkspaceClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out SemanticTokensWorkspaceClientCapabilities);

   procedure Write_SemanticTokensWorkspaceClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SemanticTokensWorkspaceClientCapabilities);

   for SemanticTokensWorkspaceClientCapabilities'Read use
     Read_SemanticTokensWorkspaceClientCapabilities;

   for SemanticTokensWorkspaceClientCapabilities'Write use
     Write_SemanticTokensWorkspaceClientCapabilities;

   package Optional_SemanticTokensWorkspaceClientCapabilities_Package is
     new LSP.Generic_Optional (SemanticTokensWorkspaceClientCapabilities);

   type Optional_SemanticTokensWorkspaceClientCapabilities is
     new Optional_SemanticTokensWorkspaceClientCapabilities_Package.Optional_Type;

   --```typescript
   --export interface CodeLensWorkspaceClientCapabilities {
   --	/**
   --	 * Whether the client implementation supports a refresh request sent from the
   --	 * server to the client.
   --	 *
   --	 * Note that this event is global and will force the client to refresh all
   --	 * code lenses currently shown. It should be used with absolute care and is
   --	 * useful for situation where a server for example detect a project wide
   --	 * change that requires such a calculation.
   --	 */
   --	refreshSupport?: boolean;
   --}
   --```
   type CodeLensWorkspaceClientCapabilities is record
      refreshSupport: Optional_Boolean;
   end record;

   procedure Read_CodeLensWorkspaceClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CodeLensWorkspaceClientCapabilities);
   procedure Write_CodeLensWorkspaceClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CodeLensWorkspaceClientCapabilities);

   for CodeLensWorkspaceClientCapabilities'Read use
     Read_CodeLensWorkspaceClientCapabilities;

   for CodeLensWorkspaceClientCapabilities'Write use
     Write_CodeLensWorkspaceClientCapabilities;

   package Optional_CodeLensWorkspaceClientCapabilities_Package is
     new LSP.Generic_Optional (CodeLensWorkspaceClientCapabilities);

   type Optional_CodeLensWorkspaceClientCapabilities is
     new Optional_CodeLensWorkspaceClientCapabilities_Package.Optional_Type;

   type FileOperationsClientCapabilities is record
      dynamicRegistration: Optional_Boolean;
      didCreate: Optional_Boolean;
      willCreate: Optional_Boolean;
      didRename: Optional_Boolean;
      willRename: Optional_Boolean;
      didDelete: Optional_Boolean;
      willDelete: Optional_Boolean;
   end record;

   procedure Read_FileOperationsClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out FileOperationsClientCapabilities);

   procedure Write_FileOperationsClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : FileOperationsClientCapabilities);

   for FileOperationsClientCapabilities'Read use Read_FileOperationsClientCapabilities;
   for FileOperationsClientCapabilities'Write use Write_FileOperationsClientCapabilities;

   package Optional_FileOperationsClientCapabilities_Package is
     new LSP.Generic_Optional (FileOperationsClientCapabilities);

   type Optional_FileOperationsClientCapabilities is
     new Optional_FileOperationsClientCapabilities_Package.Optional_Type;

   type WorkspaceClientCapabilities is record
      applyEdit: Optional_Boolean;
      workspaceEdit: WorkspaceEditClientCapabilities;
      didChangeConfiguration: DidChangeConfigurationClientCapabilities;
      didChangeWatchedFiles: DidChangeWatchedFilesClientCapabilities;
      symbol: Optional_WorkspaceSymbolClientCapabilities;
      executeCommand: ExecuteCommandClientCapabilities;
      workspaceFolders: Optional_Boolean;
      configuration: Optional_Boolean;
      semanticTokens: Optional_SemanticTokensWorkspaceClientCapabilities;
      codeLens: Optional_CodeLensWorkspaceClientCapabilities;
      fileOperations: Optional_FileOperationsClientCapabilities;
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
   -- * A `MarkupContent` literal represents a string value which content is
   -- * interpreted base on its kind flag. Currently the protocol supports
   -- * `plaintext` and `markdown` as markup kinds.
   -- *
   -- * If the kind is `markdown` then the value can contain fenced code blocks like
   -- * in GitHub issues.
   -- *
   -- * Here is an example how such a string can be constructed using
   -- * JavaScript / TypeScript:
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
   -- * *Please Note* that clients might sanitize the return markdown. A client could
   -- * decide to remove HTML from the markdown to avoid script execution.
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

   package MarkupKind_Vectors is new LSP.Generic_Vectors
     (MarkupKind, Write_Empty => LSP.Write_Array);

   type MarkupKind_Vector is new MarkupKind_Vectors.Vector with null record;

   package Optional_MarkupKind_Vectors is
     new LSP.Generic_Optional (MarkupKind_Vector);

   type Optional_MarkupKind_Vector is
     new Optional_MarkupKind_Vectors.Optional_Type;

   type MarkupContent is record
      kind  : MarkupKind;
      value : VSS.Strings.Virtual_String;
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
            String : VSS.Strings.Virtual_String;
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
   --export interface SaveOptions {
   --	/**
   --	 * The client is supposed to include the content on save.
   --	 */
   --	includeText?: boolean;
   --}
   --```
   type SaveOptions is record
      includeText: Optional_Boolean;
   end record;

   procedure Read_SaveOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out SaveOptions);
   procedure Write_SaveOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SaveOptions);
   for SaveOptions'Read use Read_SaveOptions;
   for SaveOptions'Write use Write_SaveOptions;

   package Optional_SaveOptions_Package is
     new LSP.Generic_Optional (SaveOptions);

   type Optional_SaveOptions is
     new Optional_SaveOptions_Package.Optional_Type;

   --
   --```typescript
   --/**
   -- * Defines how the host (editor) should sync document changes to the language
   -- * server.
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
   --export interface TextDocumentSyncOptions {
   --	/**
   --	 * Open and close notifications are sent to the server. If omitted open
   --	 * close notification should not be sent.
   --	 */
   --	openClose?: boolean;
   --
   --	/**
   --	 * Change notifications are sent to the server. See
   --	 * TextDocumentSyncKind.None, TextDocumentSyncKind.Full and
   --	 * TextDocumentSyncKind.Incremental. If omitted it defaults to
   --	 * TextDocumentSyncKind.None.
   --	 */
   --	change?: TextDocumentSyncKind;
   --}
   --```
   --  LSP 3.15 has two definitions for TextDocumentSyncKind and TextDocumentSyncOptions
   --  Put the shorter one first. This TextDocumentSyncOptions definition is incomplete.
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
      save: Optional_SaveOptions;
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

   --```typescript
   --export interface TextDocumentSyncClientCapabilities {
   --	/**
   --	 * Whether text document synchronization supports dynamic registration.
   --	 */
   --	dynamicRegistration?: boolean;
   --
   --	/**
   --	 * The client supports sending will save notifications.
   --	 */
   --	willSave?: boolean;
   --
   --	/**
   --	 * The client supports sending a will save request and
   --	 * waits for a response providing text edits which will
   --	 * be applied to the document before it is saved.
   --	 */
   --	willSaveWaitUntil?: boolean;
   --
   --	/**
   --	 * The client supports did save notifications.
   --	 */
   --	didSave?: boolean;
   --}
   --
   --/**
   -- * Defines how the host (editor) should sync document changes to the language
   -- * server.
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
   --export type TextDocumentSyncKind = 0 | 1 | 2;
   --
   --export interface TextDocumentSyncOptions {
   --	/**
   --	 * Open and close notifications are sent to the server. If omitted open
   --	 * close notification should not be sent.
   --	 */
   --	openClose?: boolean;
   --	/**
   --	 * Change notifications are sent to the server. See
   --	 * TextDocumentSyncKind.None, TextDocumentSyncKind.Full and
   --	 * TextDocumentSyncKind.Incremental. If omitted it defaults to
   --	 * TextDocumentSyncKind.None.
   --	 */
   --	change?: TextDocumentSyncKind;
   --	/**
   --	 * If present will save notifications are sent to the server. If omitted
   --	 * the notification should not be sent.
   --	 */
   --	willSave?: boolean;
   --	/**
   --	 * If present will save wait until requests are sent to the server. If
   --	 * omitted the request should not be sent.
   --	 */
   --	willSaveWaitUntil?: boolean;
   --	/**
   --	 * If present save notifications are sent to the server. If omitted the
   --	 * notification should not be sent.
   --	 */
   --	save?: boolean | SaveOptions;
   --}
   --```
   type TextDocumentSyncClientCapabilities is record
      dynamicRegistration : Optional_Boolean;
      willSave : Optional_Boolean;
      willSaveWaitUntil : Optional_Boolean;
      didSave : Optional_Boolean;
   end record;

   procedure Read_TextDocumentSyncClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out TextDocumentSyncClientCapabilities);

   procedure Write_TextDocumentSyncClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : TextDocumentSyncClientCapabilities);

   for TextDocumentSyncClientCapabilities'Read use
     Read_TextDocumentSyncClientCapabilities;

   for TextDocumentSyncClientCapabilities'Write use
     Write_TextDocumentSyncClientCapabilities;

   --```typescript
   --export interface CompletionClientCapabilities {
   --	/**
   --	 * Whether completion supports dynamic registration.
   --	 */
   --	dynamicRegistration?: boolean;
   --
   --	/**
   --	 * The client supports the following `CompletionItem` specific
   --	 * capabilities.
   --	 */
   --	completionItem?: {
   --		/**
   --		 * Client supports snippets as insert text.
   --		 *
   --		 * A snippet can define tab stops and placeholders with `$1`, `$2`
   --		 * and `${3:foo}`. `$0` defines the final tab stop, it defaults to
   --		 * the end of the snippet. Placeholders with equal identifiers are
   --		 * linked, that is typing in one will update others too.
   --		 */
   --		snippetSupport?: boolean;
   --
   --		/**
   --		 * Client supports commit characters on a completion item.
   --		 */
   --		commitCharactersSupport?: boolean
   --
   --		/**
   --		 * Client supports the following content formats for the documentation
   --		 * property. The order describes the preferred format of the client.
   --		 */
   --		documentationFormat?: MarkupKind[];
   --
   --		/**
   --		 * Client supports the deprecated property on a completion item.
   --		 */
   --		deprecatedSupport?: boolean;
   --
   --		/**
   --		 * Client supports the preselect property on a completion item.
   --		 */
   --		preselectSupport?: boolean;
   --
   --		/**
   --		 * Client supports the tag property on a completion item. Clients
   --		 * supporting tags have to handle unknown tags gracefully. Clients
   --		 * especially need to preserve unknown tags when sending a completion
   --		 * item back to the server in a resolve call.
   --		 *
   --		 * @since 3.15.0
   --		 */
   --		tagSupport?: {
   --			/**
   --			 * The tags supported by the client.
   --			 */
   --			valueSet: CompletionItemTag[]
   --		}
   --
   --		/**
   --		 * Client supports insert replace edit to control different behavior if
   --		 * a completion item is inserted in the text or should replace text.
   --		 *
   --		 * @since 3.16.0
   --		 */
   --		insertReplaceSupport?: boolean;
   --
   --		/**
   --		 * Indicates which properties a client can resolve lazily on a
   --		 * completion item. Before version 3.16.0 only the predefined properties
   --		 * `documentation` and `details` could be resolved lazily.
   --		 *
   --		 * @since 3.16.0
   --		 */
   --		resolveSupport?: {
   --			/**
   --			 * The properties that a client can resolve lazily.
   --			 */
   --			properties: string[];
   --		};
   --
   --		/**
   --		 * The client supports the `insertTextMode` property on
   --		 * a completion item to override the whitespace handling mode
   --		 * as defined by the client (see `insertTextMode`).
   --		 *
   --		 * @since 3.16.0
   --		 */
   --		insertTextModeSupport?: {
   --			valueSet: InsertTextMode[];
   --		}
   --	};
   --
   --	completionItemKind?: {
   --		/**
   --		 * The completion item kind values the client supports. When this
   --		 * property exists the client also guarantees that it will
   --		 * handle values outside its set gracefully and falls back
   --		 * to a default value when unknown.
   --		 *
   --		 * If this property is not present the client only supports
   --		 * the completion items kinds from `Text` to `Reference` as defined in
   --		 * the initial version of the protocol.
   --		 */
   --		valueSet?: CompletionItemKind[];
   --	};
   --
   --	/**
   --	 * The client supports to send additional context information for a
   --	 * `textDocument/completion` request.
   --	 */
   --	contextSupport?: boolean;
   --}
   --```

   type CompletionItemTag is (Deprecated);

   procedure Read_CompletionItemTag
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CompletionItemTag);

   procedure Write_CompletionItemTag
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CompletionItemTag);

   for CompletionItemTag'Read use Read_CompletionItemTag;
   for CompletionItemTag'Write use Write_CompletionItemTag;

   package CompletionItemTagSets is new LSP.Generic_Sets (CompletionItemTag, LSP.Write_Array);

   type CompletionItemTagSet is new CompletionItemTagSets.Set;

   package Optional_CompletionItemTagSets is
     new LSP.Generic_Optional (CompletionItemTagSet);

   type Optional_CompletionItemTagSet is
     new Optional_CompletionItemTagSets.Optional_Type;

   type CompletionItemTagSupport is record
      valueSet : CompletionItemTagSet;
   end record;

   procedure Read_CompletionItemTagSupport
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CompletionItemTagSupport);

   procedure Write_CompletionItemTagSupport
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CompletionItemTagSupport);

   for CompletionItemTagSupport'Read use Read_CompletionItemTagSupport;
   for CompletionItemTagSupport'Write use Write_CompletionItemTagSupport;

   package Optional_CompletionItemTagSupport_Package is
     new LSP.Generic_Optional (CompletionItemTagSupport);

   type Optional_CompletionItemTagSupport is
     new Optional_CompletionItemTagSupport_Package.Optional_Type;

   type resolveSupportCapability is record
      properties: VSS.String_Vectors.Virtual_String_Vector;
   end record;

   procedure Read_resolveSupportCapability
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out resolveSupportCapability);

   procedure Write_resolveSupportCapability
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : resolveSupportCapability);

   for resolveSupportCapability'Read use Read_resolveSupportCapability;
   for resolveSupportCapability'Write use Write_resolveSupportCapability;

   package Optional_resolveSupportCapability_Package is
     new LSP.Generic_Optional (resolveSupportCapability);

   type Optional_resolveSupportCapability is
     new Optional_resolveSupportCapability_Package.Optional_Type;

   type InsertTextMode is (asIs, adjustIndentation);

   procedure Read_InsertTextMode
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out InsertTextMode);
   procedure Write_InsertTextMode
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : InsertTextMode);

   for InsertTextMode'Read use Read_InsertTextMode;
   for InsertTextMode'Write use Write_InsertTextMode;

   package Optional_InsertTextMode_Package is
     new LSP.Generic_Optional (InsertTextMode);
   type Optional_InsertTextMode is
     new Optional_InsertTextMode_Package.Optional_Type;

   package InsertTextModeSets is
     new LSP.Generic_Sets (InsertTextMode, LSP.Write_Array);
   type InsertTextModeSet is new InsertTextModeSets.Set;

   type insertTextModeSupportCapability is record
      valueSet: InsertTextModeSet;
   end record;

   procedure Read_insertTextModeSupportCapability
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out insertTextModeSupportCapability);
   procedure Write_insertTextModeSupportCapability
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : insertTextModeSupportCapability);

   for insertTextModeSupportCapability'Read
     use Read_insertTextModeSupportCapability;
   for insertTextModeSupportCapability'Write
     use Write_insertTextModeSupportCapability;

   package Optional_insertTextModeSupportCapability_Package is
     new LSP.Generic_Optional (insertTextModeSupportCapability);

   type Optional_insertTextModeSupportCapability is
     new Optional_insertTextModeSupportCapability_Package.Optional_Type;

   type completionItemCapability is record
      snippetSupport : Optional_Boolean;
      commitCharactersSupport : Optional_Boolean;
      documentationFormat : MarkupKind_Vector;
      deprecatedSupport : Optional_Boolean;
      preselectSupport : Optional_Boolean;
      tagSupport : Optional_CompletionItemTagSupport;
      insertReplaceSupport : Optional_Boolean;
      resolveSupport : Optional_resolveSupportCapability;
      insertTextModeSupport: Optional_insertTextModeSupportCapability;
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

   package Optional_CompletionItemKinds is new LSP.Generic_Optional (CompletionItemKind);
   type Optional_CompletionItemKind is new Optional_CompletionItemKinds.Optional_Type;

   package CompletionItemKindSets is new LSP.Generic_Sets (CompletionItemKind, LSP.Write_Array);

   type CompletionItemKindSet is new CompletionItemKindSets.Set;

   Default_CompletionItemKindSet : constant CompletionItemKindSet :=
     To_Set (From => Text, To => Reference);

   package Optional_CompletionItemKindSets is
     new LSP.Generic_Optional (CompletionItemKindSet);

   type Optional_CompletionItemKindSet is
     new Optional_CompletionItemKindSets.Optional_Type;

   type CompletionItemKindSetCapabilities is record
      valueSet : Optional_CompletionItemKindSet;
   end record;

   procedure Read_CompletionItemKindSetCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CompletionItemKindSetCapabilities);
   procedure Write_CompletionItemKindSetCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CompletionItemKindSetCapabilities);
   for CompletionItemKindSetCapabilities'Read
     use Read_CompletionItemKindSetCapabilities;
   for CompletionItemKindSetCapabilities'Write
     use Write_CompletionItemKindSetCapabilities;

   package Optional_CompletionItemKindSetCapabilities_Package is
     new LSP.Generic_Optional (CompletionItemKindSetCapabilities);

   type Optional_CompletionItemKindSetCapabilities is
     new Optional_CompletionItemKindSetCapabilities_Package.Optional_Type;

   type CompletionClientCapabilities is record
      dynamicRegistration : Optional_Boolean;
      completionItem : Optional_completionItemCapability;
      completionItemKind : Optional_CompletionItemKindSetCapabilities;
      contextSupport : Optional_Boolean;
   end record;

   procedure Read_CompletionClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CompletionClientCapabilities);

   procedure Write_CompletionClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CompletionClientCapabilities);

   for CompletionClientCapabilities'Read use Read_CompletionClientCapabilities;
   for CompletionClientCapabilities'Write use Write_CompletionClientCapabilities;

   --```typescript
   --export interface HoverClientCapabilities {
   --	/**
   --	 * Whether hover supports dynamic registration.
   --	 */
   --	dynamicRegistration?: boolean;
   --
   --	/**
   --	 * Client supports the following content formats if the content
   --	 * property refers to a `literal of type MarkupContent`.
   --	 * The order describes the preferred format of the client.
   --	 */
   --	contentFormat?: MarkupKind[];
   --}
   --```
   type HoverClientCapabilities is record
      dynamicRegistration: Optional_Boolean;
      contentFormat: Optional_MarkupKind_Vector;
   end record;

   procedure Read_HoverClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out HoverClientCapabilities);

   procedure Write_HoverClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : HoverClientCapabilities);

   for HoverClientCapabilities'Read use Read_HoverClientCapabilities;
   for HoverClientCapabilities'Write use Write_HoverClientCapabilities;

   package Optional_HoverClientCapabilities_Package is
     new LSP.Generic_Optional (HoverClientCapabilities);

   type Optional_HoverClientCapabilities is
     new Optional_HoverClientCapabilities_Package.Optional_Type;

   --```typescript
   --export interface SignatureHelpClientCapabilities {
   --	/**
   --	 * Whether signature help supports dynamic registration.
   --	 */
   --	dynamicRegistration?: boolean;
   --
   --	/**
   --	 * The client supports the following `SignatureInformation`
   --	 * specific properties.
   --	 */
   --	signatureInformation?: {
   --		/**
   --		 * Client supports the following content formats for the documentation
   --		 * property. The order describes the preferred format of the client.
   --		 */
   --		documentationFormat?: MarkupKind[];
   --
   --		/**
   --		 * Client capabilities specific to parameter information.
   --		 */
   --		parameterInformation?: {
   --			/**
   --			 * The client supports processing label offsets instead of a
   --			 * simple label string.
   --			 *
   --			 * @since 3.14.0
   --			 */
   --			labelOffsetSupport?: boolean;
   --		};
   --
   --		/**
   --		 * The client supports the `activeParameter` property on
   --		 * `SignatureInformation` literal.
   --		 *
   --		 * @since 3.16.0
   --		 */
   --		activeParameterSupport?: boolean;
   --	};
   --
   --	/**
   --	 * The client supports to send additional context information for a
   --	 * `textDocument/signatureHelp` request. A client that opts into
   --	 * contextSupport will also support the `retriggerCharacters` on
   --	 * `SignatureHelpOptions`.
   --	 *
   --	 * @since 3.15.0
   --	 */
   --	contextSupport?: boolean;
   --}
   --```
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
      activeParameterSupport: Optional_Boolean;
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

   type SignatureHelpClientCapabilities is record
      dynamicRegistration: Optional_Boolean;
      signatureInformation : Optional_signatureInformation_Capability;
      contextSupport: Optional_Boolean;
   end record;

   procedure Read_SignatureHelpClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out SignatureHelpClientCapabilities);

   procedure Write_SignatureHelpClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SignatureHelpClientCapabilities);

   for SignatureHelpClientCapabilities'Read use Read_SignatureHelpClientCapabilities;
   for SignatureHelpClientCapabilities'Write use Write_SignatureHelpClientCapabilities;

   package Optional_SignatureHelpClientCapabilities_Package is
     new LSP.Generic_Optional (SignatureHelpClientCapabilities);

   type Optional_SignatureHelpClientCapabilities is
     new Optional_SignatureHelpClientCapabilities_Package.Optional_Type;

   --```typescript
   --export interface DocumentSymbolClientCapabilities {
   --	/**
   --	 * Whether document symbol supports dynamic registration.
   --	 */
   --	dynamicRegistration?: boolean;
   --
   --	/**
   --	 * Specific capabilities for the `SymbolKind` in the
   --	 * `textDocument/documentSymbol` request.
   --	 */
   --	symbolKind?: {
   --		/**
   --		 * The symbol kind values the client supports. When this
   --		 * property exists the client also guarantees that it will
   --		 * handle values outside its set gracefully and falls back
   --		 * to a default value when unknown.
   --		 *
   --		 * If this property is not present the client only supports
   --		 * the symbol kinds from `File` to `Array` as defined in
   --		 * the initial version of the protocol.
   --		 */
   --		valueSet?: SymbolKind[];
   --	}
   --
   --	/**
   --	 * The client supports hierarchical document symbols.
   --	 */
   --	hierarchicalDocumentSymbolSupport?: boolean;
   --
   --	/**
   --	 * The client supports tags on `SymbolInformation`. Tags are supported on
   --	 * `DocumentSymbol` if `hierarchicalDocumentSymbolSupport` is set to true.
   --	 * Clients supporting tags have to handle unknown tags gracefully.
   --	 *
   --	 * @since 3.16.0
   --	 */
   --	tagSupport?: {
   --		/**
   --		 * The tags supported by the client.
   --		 */
   --		valueSet: SymbolTag[]
   --	}
   --
   --	/**
   --	 * The client supports an additional label presented in the UI when
   --	 * registering a document symbol provider.
   --	 *
   --	 * @since 3.16.0
   --	 */
   --	labelSupport?: boolean;
   --}
   --```
   type DocumentSymbolClientCapabilities is record
      dynamicRegistration: Optional_Boolean;
      symbolKind: Optional_symbolKindCapabilities;
      labelSupport: Optional_Boolean;
      hierarchicalDocumentSymbolSupport: Optional_Boolean;
      tagSupport: Optional_tagSupportCapability;
   end record;

   procedure Read_DocumentSymbolClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DocumentSymbolClientCapabilities);

   procedure Write_DocumentSymbolClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DocumentSymbolClientCapabilities);

   for DocumentSymbolClientCapabilities'Read use Read_DocumentSymbolClientCapabilities;
   for DocumentSymbolClientCapabilities'Write use Write_DocumentSymbolClientCapabilities;

   package Optional_DocumentSymbolClientCapabilities_Package is
     new LSP.Generic_Optional (DocumentSymbolClientCapabilities);

   type Optional_DocumentSymbolClientCapabilities is
     new Optional_DocumentSymbolClientCapabilities_Package.Optional_Type;

   --```typescript
   --export interface DeclarationClientCapabilities {
   --	/**
   --	 * Whether declaration supports dynamic registration. If this is set to
   --	 * `true` the client supports the new `DeclarationRegistrationOptions`
   --	 * return value for the corresponding server capability as well.
   --	 */
   --	dynamicRegistration?: boolean;
   --
   --	/**
   --	 * The client supports additional metadata in the form of declaration links.
   --	 */
   --	linkSupport?: boolean;
   --}
   --```
   type DeclarationClientCapabilities is record
      dynamicRegistration: Optional_Boolean;
      linkSupport: Optional_Boolean;
   end record;

   procedure Read_DeclarationClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DeclarationClientCapabilities);

   procedure Write_DeclarationClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DeclarationClientCapabilities);

   for DeclarationClientCapabilities'Read use Read_DeclarationClientCapabilities;
   for DeclarationClientCapabilities'Write use Write_DeclarationClientCapabilities;

   package Optional_DeclarationClientCapabilities_Package is
     new LSP.Generic_Optional (DeclarationClientCapabilities);

   type Optional_DeclarationClientCapabilities is
     new Optional_DeclarationClientCapabilities_Package.Optional_Type;

   --```typescript
   --export interface DefinitionClientCapabilities {
   --	/**
   --	 * Whether definition supports dynamic registration.
   --	 */
   --	dynamicRegistration?: boolean;
   --
   --	/**
   --	 * The client supports additional metadata in the form of definition links.
   --	 *
   --	 * @since 3.14.0
   --	 */
   --	linkSupport?: boolean;
   --}
   --```
   subtype Optional_DefinitionClientCapabilities is Optional_DeclarationClientCapabilities;

   --```typescript
   --export interface TypeDefinitionClientCapabilities {
   --	/**
   --	 * Whether implementation supports dynamic registration. If this is set to
   --	 * `true` the client supports the new `TypeDefinitionRegistrationOptions`
   --	 * return value for the corresponding server capability as well.
   --	 */
   --	dynamicRegistration?: boolean;
   --
   --	/**
   --	 * The client supports additional metadata in the form of definition links.
   --	 *
   --	 * @since 3.14.0
   --	 */
   --	linkSupport?: boolean;
   --}
   --```
   subtype Optional_TypeDefinitionClientCapabilities is
     Optional_DeclarationClientCapabilities;

   --```typescript
   --export interface ImplementationClientCapabilities {
   --	/**
   --	 * Whether implementation supports dynamic registration. If this is set to
   --	 * `true` the client supports the new `ImplementationRegistrationOptions`
   --	 * return value for the corresponding server capability as well.
   --	 */
   --	dynamicRegistration?: boolean;
   --
   --	/**
   --	 * The client supports additional metadata in the form of definition links.
   --	 *
   --	 * @since 3.14.0
   --	 */
   --	linkSupport?: boolean;
   --}
   --```
   subtype Optional_ImplementationClientCapabilities is
     Optional_DeclarationClientCapabilities;

   --```typescript
   --export interface ReferenceClientCapabilities {
   --	/**
   --	 * Whether references supports dynamic registration.
   --	 */
   --	dynamicRegistration?: boolean;
   --}
   --```
   subtype ReferenceClientCapabilities is Optional_dynamicRegistration;

   --```typescript
   --export interface DocumentHighlightClientCapabilities {
   --	/**
   --	 * Whether document highlight supports dynamic registration.
   --	 */
   --	dynamicRegistration?: boolean;
   --}
   --```
   subtype DocumentHighlightClientCapabilities is Optional_dynamicRegistration;

   --```typescript
   --export interface CodeActionClientCapabilities {
   --	/**
   --	 * Whether code action supports dynamic registration.
   --	 */
   --	dynamicRegistration?: boolean;
   --
   --	/**
   --	 * The client supports code action literals as a valid
   --	 * response of the `textDocument/codeAction` request.
   --	 *
   --	 * @since 3.8.0
   --	 */
   --	codeActionLiteralSupport?: {
   --		/**
   --		 * The code action kind is supported with the following value
   --		 * set.
   --		 */
   --		codeActionKind: {
   --
   --			/**
   --			 * The code action kind values the client supports. When this
   --			 * property exists the client also guarantees that it will
   --			 * handle values outside its set gracefully and falls back
   --			 * to a default value when unknown.
   --			 */
   --			valueSet: CodeActionKind[];
   --		};
   --	};
   --
   --	/**
   --	 * Whether code action supports the `isPreferred` property.
   --	 *
   --	 * @since 3.15.0
   --	 */
   --	isPreferredSupport?: boolean;
   --
   --	/**
   --	 * Whether code action supports the `disabled` property.
   --	 *
   --	 * @since 3.16.0
   --	 */
   --	disabledSupport?: boolean;
   --
   --	/**
   --	 * Whether code action supports the `data` property which is
   --	 * preserved between a `textDocument/codeAction` and a
   --	 * `codeAction/resolve` request.
   --	 *
   --	 * @since 3.16.0
   --	 */
   --	dataSupport?: boolean;
   --
   --
   --	/**
   --	 * Whether the client supports resolving additional code action
   --	 * properties via a separate `codeAction/resolve` request.
   --	 *
   --	 * @since 3.16.0
   --	 */
   --	resolveSupport?: {
   --		/**
   --		 * The properties that a client can resolve lazily.
   --		*/
   --		properties: string[];
   --	};
   --
   --	/**
   --	 * Whether the client honors the change annotations in
   --	 * text edits and resource operations returned via the
   --	 * `CodeAction#edit` property by for example presenting
   --	 * the workspace edit in the user interface and asking
   --	 * for confirmation.
   --	 *
   --	 * @since 3.16.0
   --	 */
   --	honorsChangeAnnotations?: boolean;
   --}
   --```
   type codeActionKindCapability is record
      valueSet: CodeActionKindSet;
   end record;

   procedure Read_codeActionKindCapability
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out codeActionKindCapability);

   procedure Write_codeActionKindCapability
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : codeActionKindCapability);

   for codeActionKindCapability'Read use Read_codeActionKindCapability;
   for codeActionKindCapability'Write use Write_codeActionKindCapability;

   type codeActionLiteralSupport_Capability is record
      codeActionKind: codeActionKindCapability;
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

   type CodeActionClientCapabilities is record
      dynamicRegistration: Optional_Boolean;
      codeActionLiteralSupport: Optional_codeActionLiteralSupport_Capability;
      isPreferredSupport: Optional_Boolean;
      disabledSupport: Optional_Boolean;
      dataSupport: Optional_Boolean;
      resolveSupport : Optional_resolveSupportCapability;
      honorsChangeAnnotations: Optional_Boolean;
   end record;

   procedure Read_CodeActionClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CodeActionClientCapabilities);

   procedure Write_CodeActionClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CodeActionClientCapabilities);

   for CodeActionClientCapabilities'Read use Read_CodeActionClientCapabilities;
   for CodeActionClientCapabilities'Write use Write_CodeActionClientCapabilities;

   package Optional_CodeActionClientCapabilities_Package is
     new LSP.Generic_Optional (CodeActionClientCapabilities);

   type Optional_CodeActionClientCapabilities is
     new Optional_CodeActionClientCapabilities_Package.Optional_Type;

   --```typescript
   --export interface CodeLensClientCapabilities {
   --	/**
   --	 * Whether code lens supports dynamic registration.
   --	 */
   --	dynamicRegistration?: boolean;
   --}
   --```
   subtype CodeLensClientCapabilities is Optional_dynamicRegistration;

   --```typescript
   --export interface DocumentLinkClientCapabilities {
   --	/**
   --	 * Whether document link supports dynamic registration.
   --	 */
   --	dynamicRegistration?: boolean;
   --
   --	/**
   --	 * Whether the client supports the `tooltip` property on `DocumentLink`.
   --	 *
   --	 * @since 3.15.0
   --	 */
   --	tooltipSupport?: boolean;
   --}
   --```
   type DocumentLinkClientCapabilities is record
      dynamicRegistration: Optional_Boolean;
      tooltipSupport: Optional_Boolean;
   end record;

   procedure Read_DocumentLinkClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DocumentLinkClientCapabilities);

   procedure Write_DocumentLinkClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DocumentLinkClientCapabilities);

   for DocumentLinkClientCapabilities'Read use Read_DocumentLinkClientCapabilities;
   for DocumentLinkClientCapabilities'Write use Write_DocumentLinkClientCapabilities;

   package Optional_DocumentLinkClientCapabilities_Package is
     new LSP.Generic_Optional (DocumentLinkClientCapabilities);

   type Optional_DocumentLinkClientCapabilities is
     new Optional_DocumentLinkClientCapabilities_Package.Optional_Type;

   --```typescript
   --export interface DocumentColorClientCapabilities {
   --	/**
   --	 * Whether document color supports dynamic registration.
   --	 */
   --	dynamicRegistration?: boolean;
   --}
   --```
   subtype DocumentColorClientCapabilities is Optional_dynamicRegistration;

   --```typescript
   --export interface DocumentFormattingClientCapabilities {
   --	/**
   --	 * Whether formatting supports dynamic registration.
   --	 */
   --	dynamicRegistration?: boolean;
   --}
   --```
   subtype DocumentFormattingClientCapabilities is Optional_dynamicRegistration;

   --```typescript
   --export interface DocumentRangeFormattingClientCapabilities {
   --	/**
   --	 * Whether formatting supports dynamic registration.
   --	 */
   --	dynamicRegistration?: boolean;
   --}
   --```
   subtype DocumentRangeFormattingClientCapabilities is Optional_dynamicRegistration;

   --```typescript
   --export interface DocumentOnTypeFormattingClientCapabilities {
   --	/**
   --	 * Whether on type formatting supports dynamic registration.
   --	 */
   --	dynamicRegistration?: boolean;
   --}
   --```
   subtype DocumentOnTypeFormattingClientCapabilities is Optional_dynamicRegistration;

   --```typescript
   --export namespace PrepareSupportDefaultBehavior {
   --	/**
   --	 * The client's default behavior is to select the identifier
   --	 * according the to language's syntax rule.
   --	 */
   --	 export const Identifier: 1 = 1;
   --}
   --export interface RenameClientCapabilities {
   --	/**
   --	 * Whether rename supports dynamic registration.
   --	 */
   --	dynamicRegistration?: boolean;
   --
   --	/**
   --	 * Client supports testing for validity of rename operations
   --	 * before execution.
   --	 *
   --	 * @since 3.12.0
   --	 */
   --	prepareSupport?: boolean;
   --
   --	/**
   --	 * Client supports the default behavior result
   --	 * (`{ defaultBehavior: boolean }`).
   --	 *
   --	 * The value indicates the default behavior used by the
   --	 * client.
   --	 *
   --	 * @since 3.16.0
   --	 */
   --	prepareSupportDefaultBehavior?: PrepareSupportDefaultBehavior;
   --
   --	/**
   --	 * Whether th client honors the change annotations in
   --	 * text edits and resource operations returned via the
   --	 * rename request's workspace edit by for example presenting
   --	 * the workspace edit in the user interface and asking
   --	 * for confirmation.
   --	 *
   --	 * @since 3.16.0
   --	 */
   --	honorsChangeAnnotations?: boolean;
   --}
   --```
   type PrepareSupportDefaultBehavior is (Identifier);

   procedure Read_PrepareSupportDefaultBehavior
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out PrepareSupportDefaultBehavior);

   procedure Write_PrepareSupportDefaultBehavior
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : PrepareSupportDefaultBehavior);

   for PrepareSupportDefaultBehavior'Read use Read_PrepareSupportDefaultBehavior;
   for PrepareSupportDefaultBehavior'Write use Write_PrepareSupportDefaultBehavior;

   package Optional_PrepareSupportDefaultBehavior_Package is
     new LSP.Generic_Optional (PrepareSupportDefaultBehavior);

   type Optional_PrepareSupportDefaultBehavior is
     new Optional_PrepareSupportDefaultBehavior_Package.Optional_Type;

   type RenameClientCapabilities is record
      dynamicRegistration: Optional_Boolean;
      prepareSupport: Optional_Boolean;
      prepareSupportDefaultBehavior: Optional_PrepareSupportDefaultBehavior;
      honorsChangeAnnotations: Optional_Boolean;
   end record;

   procedure Read_RenameClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out RenameClientCapabilities);

   procedure Write_RenameClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : RenameClientCapabilities);

   for RenameClientCapabilities'Read use Read_RenameClientCapabilities;
   for RenameClientCapabilities'Write use Write_RenameClientCapabilities;

   package Optional_RenameClientCapabilities_Package is
     new LSP.Generic_Optional (RenameClientCapabilities);

   type Optional_RenameClientCapabilities is
     new Optional_RenameClientCapabilities_Package.Optional_Type;

   --```typescript
   --export interface PublishDiagnosticsClientCapabilities {
   --	/**
   --	 * Whether the clients accepts diagnostics with related information.
   --	 */
   --	relatedInformation?: boolean;
   --
   --	/**
   --	 * Client supports the tag property to provide meta data about a diagnostic.
   --	 * Clients supporting tags have to handle unknown tags gracefully.
   --	 *
   --	 * @since 3.15.0
   --	 */
   --	tagSupport?: {
   --		/**
   --		 * The tags supported by the client.
   --		 */
   --		valueSet: DiagnosticTag[];
   --	};
   --
   --	/**
   --	 * Whether the client interprets the version property of the
   --	 * `textDocument/publishDiagnostics` notification's parameter.
   --	 *
   --	 * @since 3.15.0
   --	 */
   --	versionSupport?: boolean;
   --
   --	/**
   --	 * Client supports a codeDescription property
   --	 *
   --	 * @since 3.16.0
   --	 */
   --	codeDescriptionSupport?: boolean;
   --
   --	/**
   --	 * Whether code action supports the `data` property which is
   --	 * preserved between a `textDocument/publishDiagnostics` and
   --	 * `textDocument/codeAction` request.
   --	 *
   --	 * @since 3.16.0
   --	 */
   --	dataSupport?: boolean;
   --}
   --```
   type DiagnosticTagSupport is record
      valueSet : DiagnosticTagSet;
   end record;

   procedure Read_DiagnosticTagSupport
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DiagnosticTagSupport);

   procedure Write_DiagnosticTagSupport
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DiagnosticTagSupport);

   for DiagnosticTagSupport'Read use Read_DiagnosticTagSupport;
   for DiagnosticTagSupport'Write use Write_DiagnosticTagSupport;

   package Optional_DiagnosticTagSupport_Package is
     new LSP.Generic_Optional (DiagnosticTagSupport);

   type Optional_DiagnosticTagSupport is
     new Optional_DiagnosticTagSupport_Package.Optional_Type;

   type PublishDiagnosticsClientCapabilities is record
      relatedInformation : Optional_Boolean;
      tagSupport: Optional_DiagnosticTagSupport;
      versionSupport: Optional_Boolean;
      codeDescriptionSupport: Optional_Boolean;
      dataSupport: Optional_Boolean;
   end record;

   procedure Read_PublishDiagnosticsClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out PublishDiagnosticsClientCapabilities);

   procedure Write_PublishDiagnosticsClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : PublishDiagnosticsClientCapabilities);

   for PublishDiagnosticsClientCapabilities'Read use Read_PublishDiagnosticsClientCapabilities;
   for PublishDiagnosticsClientCapabilities'Write use Write_PublishDiagnosticsClientCapabilities;

   package Optional_PublishDiagnosticsClientCapabilities_Package is
     new LSP.Generic_Optional (PublishDiagnosticsClientCapabilities);

   type Optional_PublishDiagnosticsClientCapabilities is
     new Optional_PublishDiagnosticsClientCapabilities_Package.Optional_Type;

   --```typescript
   --export interface FoldingRangeClientCapabilities {
   --	/**
   --	 * Whether implementation supports dynamic registration for folding range
   --	 * providers. If this is set to `true` the client supports the new
   --	 * `FoldingRangeRegistrationOptions` return value for the corresponding
   --	 * server capability as well.
   --	 */
   --	dynamicRegistration?: boolean;
   --	/**
   --	 * The maximum number of folding ranges that the client prefers to receive
   --	 * per document. The value serves as a hint, servers are free to follow the
   --	 * limit.
   --	 */
   --	rangeLimit?: uinteger;
   --	/**
   --	 * If set, the client signals that it only supports folding complete lines.
   --	 * If set, client will ignore specified `startCharacter` and `endCharacter`
   --	 * properties in a FoldingRange.
   --	 */
   --	lineFoldingOnly?: boolean;
   --}
   --```
   type FoldingRangeClientCapabilities is record
      dynamicRegistration: Optional_Boolean;
      rangeLimit: Optional_Number;
      lineFoldingOnly: Optional_Boolean;
   end record;

   procedure Read_FoldingRangeClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out FoldingRangeClientCapabilities);

   procedure Write_FoldingRangeClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : FoldingRangeClientCapabilities);

   for FoldingRangeClientCapabilities'Read use Read_FoldingRangeClientCapabilities;
   for FoldingRangeClientCapabilities'Write use Write_FoldingRangeClientCapabilities;

   package Optional_FoldingRangeClientCapabilities_Package is
     new LSP.Generic_Optional (FoldingRangeClientCapabilities);

   type Optional_FoldingRangeClientCapabilities is
     new Optional_FoldingRangeClientCapabilities_Package.Optional_Type;

   --```typescript
   --export interface SelectionRangeClientCapabilities {
   --	/**
   --	 * Whether implementation supports dynamic registration for selection range
   --	 * providers. If this is set to `true` the client supports the new
   --	 * `SelectionRangeRegistrationOptions` return value for the corresponding
   --	 * server capability as well.
   --	 */
   --	dynamicRegistration?: boolean;
   --}
   --```
   subtype SelectionRangeClientCapabilities is Optional_dynamicRegistration;

   --```typescript
   --export interface LinkedEditingRangeClientCapabilities {
   --	/**
   --	 * Whether implementation supports dynamic registration.
   --	 * If this is set to `true` the client supports the new
   --	 * `(TextDocumentRegistrationOptions & StaticRegistrationOptions)`
   --	 * return value for the corresponding server capability as well.
   --	 */
   --	dynamicRegistration?: boolean;
   --}
   --```
   subtype LinkedEditingRangeClientCapabilities is Optional_dynamicRegistration;

   --```typescript
   --interface CallHierarchyClientCapabilities {
   --	/**
   --	 * Whether implementation supports dynamic registration. If this is set to
   --	 * `true` the client supports the new `(TextDocumentRegistrationOptions &
   --	 * StaticRegistrationOptions)` return value for the corresponding server
   --	 * capability as well.
   --	 */
   --	dynamicRegistration?: boolean;
   --}
   --```
   subtype CallHierarchyClientCapabilities is Optional_dynamicRegistration;

   --```typescript
   --export enum SemanticTokenTypes {
   --	namespace = 'namespace',
   --	/**
   --	 * Represents a generic type. Acts as a fallback for types which
   --	 * can't be mapped to a specific type like class or enum.
   --	 */
   --	type = 'type',
   --	class = 'class',
   --	enum = 'enum',
   --	interface = 'interface',
   --	struct = 'struct',
   --	typeParameter = 'typeParameter',
   --	parameter = 'parameter',
   --	variable = 'variable',
   --	property = 'property',
   --	enumMember = 'enumMember',
   --	event = 'event',
   --	function = 'function',
   --	method = 'method',
   --	macro = 'macro',
   --	keyword = 'keyword',
   --	modifier = 'modifier',
   --	comment = 'comment',
   --	string = 'string',
   --	number = 'number',
   --	regexp = 'regexp',
   --	operator = 'operator'
   --}
   --
   --export enum SemanticTokenModifiers {
   --	declaration = 'declaration',
   --	definition = 'definition',
   --	readonly = 'readonly',
   --	static = 'static',
   --	deprecated = 'deprecated',
   --	abstract = 'abstract',
   --	async = 'async',
   --	modification = 'modification',
   --	documentation = 'documentation',
   --	defaultLibrary = 'defaultLibrary'
   --}
   --```
   type SemanticTokenTypes is
     (namespace,
      a_type,
      class,
      enum,
      an_interface,
      struct,
      typeParameter,
      parameter,
      variable,
      property,
      enumMember,
      event,
      a_function,
      method,
      macro,
      keyword,
      modifier,
      comment,
      a_string,
      number,
      regexp,
      operator);

   procedure Read_SemanticTokenTypes
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out SemanticTokenTypes);
   procedure Write_SemanticTokenTypes
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SemanticTokenTypes);
   for SemanticTokenTypes'Write use Write_SemanticTokenTypes;
   for SemanticTokenTypes'Read use Read_SemanticTokenTypes;

   package SemanticTokenTypes_Vectors is new LSP.Generic_Vectors
     (SemanticTokenTypes, Write_Empty => LSP.Write_Array);
   type SemanticTokenTypes_Vector is new SemanticTokenTypes_Vectors.Vector
     with null record;

   type SemanticTokenModifiers is
     (declaration,
      definition,
      readonly,
      static,
      deprecated,
      an_abstract,
      async,
      modification,
      documentation,
      defaultLibrary);

   procedure Read_SemanticTokenModifiers
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out SemanticTokenModifiers);
   procedure Write_SemanticTokenModifiers
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SemanticTokenModifiers);
   for SemanticTokenModifiers'Write use Write_SemanticTokenModifiers;
   for SemanticTokenModifiers'Read use Read_SemanticTokenModifiers;

   package SemanticTokenModifiers_Vectors is new LSP.Generic_Vectors
     (SemanticTokenModifiers, Write_Empty => LSP.Write_Array);
   type SemanticTokenModifiers_Vector is
     new SemanticTokenModifiers_Vectors.Vector with null record;

   --```typescript
   --export namespace TokenFormat {
   --	export const Relative: 'relative' = 'relative';
   --}
   --
   --export type TokenFormat = 'relative';
   --```
   type TokenFormat is (relative);

   procedure Read_TokenFormat
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out TokenFormat);

   procedure Write_TokenFormat
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : TokenFormat);

   for TokenFormat'Read use Read_TokenFormat;
   for TokenFormat'Write use Write_TokenFormat;

   package TokenFormatSets is new LSP.Generic_Sets (TokenFormat, LSP.Write_Array);
   type TokenFormatSet is new TokenFormatSets.Set;

   type SemanticTokensFullCapabilities is record
      diff: Optional_Boolean;  --  "delta" is a reserver word in Ada
   end record;

   procedure Read_SemanticTokensFullCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out SemanticTokensFullCapabilities);

   procedure Write_SemanticTokensFullCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SemanticTokensFullCapabilities);

   for SemanticTokensFullCapabilities'Read use Read_SemanticTokensFullCapabilities;
   for SemanticTokensFullCapabilities'Write use Write_SemanticTokensFullCapabilities;

   package Optional_SemanticTokensFullCapabilities_Package is
     new LSP.Generic_Optional (SemanticTokensFullCapabilities);

   type Optional_SemanticTokensFullCapabilities is
     new Optional_SemanticTokensFullCapabilities_Package.Optional_Type;

   type SemanticTokensRequestCapabilities is record
      span: Optional_Boolean;
      full: Optional_SemanticTokensFullCapabilities;
   end record;

   procedure Read_SemanticTokensRequestCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out SemanticTokensRequestCapabilities);

   procedure Write_SemanticTokensRequestCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SemanticTokensRequestCapabilities);

   for SemanticTokensRequestCapabilities'Read use Read_SemanticTokensRequestCapabilities;
   for SemanticTokensRequestCapabilities'Write use Write_SemanticTokensRequestCapabilities;

   --```typescript
   --interface SemanticTokensClientCapabilities {
   --	/**
   --	 * Whether implementation supports dynamic registration. If this is set to
   --	 * `true` the client supports the new `(TextDocumentRegistrationOptions &
   --	 * StaticRegistrationOptions)` return value for the corresponding server
   --	 * capability as well.
   --	 */
   --	dynamicRegistration?: boolean;
   --
   --	/**
   --	 * Which requests the client supports and might send to the server
   --	 * depending on the server's capability. Please note that clients might not
   --	 * show semantic tokens or degrade some of the user experience if a range
   --	 * or full request is advertised by the client but not provided by the
   --	 * server. If for example the client capability `requests.full` and
   --	 * `request.range` are both set to true but the server only provides a
   --	 * range provider the client might not render a minimap correctly or might
   --	 * even decide to not show any semantic tokens at all.
   --	 */
   --	requests: {
   --		/**
   --		 * The client will send the `textDocument/semanticTokens/range` request
   --		 * if the server provides a corresponding handler.
   --		 */
   --		range?: boolean | {
   --		};
   --
   --		/**
   --		 * The client will send the `textDocument/semanticTokens/full` request
   --		 * if the server provides a corresponding handler.
   --		 */
   --		full?: boolean | {
   --			/**
   --			 * The client will send the `textDocument/semanticTokens/full/delta`
   --			 * request if the server provides a corresponding handler.
   --			*/
   --			delta?: boolean
   --		}
   --	}
   --
   --	/**
   --	 * The token types that the client supports.
   --	 */
   --	tokenTypes: string[];
   --
   --	/**
   --	 * The token modifiers that the client supports.
   --	 */
   --	tokenModifiers: string[];
   --
   --	/**
   --	 * The formats the clients supports.
   --	 */
   --	formats: TokenFormat[];
   --
   --	/**
   --	 * Whether the client supports tokens that can overlap each other.
   --	 */
   --	overlappingTokenSupport?: boolean;
   --
   --	/**
   --	 * Whether the client supports tokens that can span multiple lines.
   --	 */
   --	multilineTokenSupport?: boolean;
   --}
   --```
   type SemanticTokensClientCapabilities is record
      dynamicRegistration: Optional_Boolean;
      requests: SemanticTokensRequestCapabilities;
      tokenTypes: SemanticTokenTypes_Vector;
      tokenModifiers: SemanticTokenModifiers_Vector;
      formats: TokenFormatSet;
      overlappingTokenSupport: Optional_Boolean;
      multilineTokenSupport: Optional_Boolean;
   end record;

   procedure Read_SemanticTokensClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out SemanticTokensClientCapabilities);

   procedure Write_SemanticTokensClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SemanticTokensClientCapabilities);

   for SemanticTokensClientCapabilities'Read use Read_SemanticTokensClientCapabilities;
   for SemanticTokensClientCapabilities'Write use Write_SemanticTokensClientCapabilities;

   package Optional_SemanticTokensClientCapabilities_Package is
     new LSP.Generic_Optional (SemanticTokensClientCapabilities);

   type Optional_SemanticTokensClientCapabilities is
     new Optional_SemanticTokensClientCapabilities_Package.Optional_Type;

   --```typescript
   --interface MonikerClientCapabilities {
   --	/**
   --	 * Whether implementation supports dynamic registration. If this is set to
   --	 * `true` the client supports the new `(TextDocumentRegistrationOptions &
   --	 * StaticRegistrationOptions)` return value for the corresponding server
   --	 * capability as well.
   --	 */
   --	dynamicRegistration?: boolean;
   --}
   --```
   subtype MonikerClientCapabilities is Optional_dynamicRegistration;

   --```typescript
   --/**
   -- * Text document specific client capabilities.
   -- */
   --export interface TextDocumentClientCapabilities {
   --
   --	synchronization?: TextDocumentSyncClientCapabilities;
   --
   --	/**
   --	 * Capabilities specific to the `textDocument/completion` request.
   --	 */
   --	completion?: CompletionClientCapabilities;
   --
   --	/**
   --	 * Capabilities specific to the `textDocument/hover` request.
   --	 */
   --	hover?: HoverClientCapabilities;
   --
   --	/**
   --	 * Capabilities specific to the `textDocument/signatureHelp` request.
   --	 */
   --	signatureHelp?: SignatureHelpClientCapabilities;
   --
   --	/**
   --	 * Capabilities specific to the `textDocument/declaration` request.
   --	 *
   --	 * @since 3.14.0
   --	 */
   --	declaration?: DeclarationClientCapabilities;
   --
   --	/**
   --	 * Capabilities specific to the `textDocument/definition` request.
   --	 */
   --	definition?: DefinitionClientCapabilities;
   --
   --	/**
   --	 * Capabilities specific to the `textDocument/typeDefinition` request.
   --	 *
   --	 * @since 3.6.0
   --	 */
   --	typeDefinition?: TypeDefinitionClientCapabilities;
   --
   --	/**
   --	 * Capabilities specific to the `textDocument/implementation` request.
   --	 *
   --	 * @since 3.6.0
   --	 */
   --	implementation?: ImplementationClientCapabilities;
   --
   --	/**
   --	 * Capabilities specific to the `textDocument/references` request.
   --	 */
   --	references?: ReferenceClientCapabilities;
   --
   --	/**
   --	 * Capabilities specific to the `textDocument/documentHighlight` request.
   --	 */
   --	documentHighlight?: DocumentHighlightClientCapabilities;
   --
   --	/**
   --	 * Capabilities specific to the `textDocument/documentSymbol` request.
   --	 */
   --	documentSymbol?: DocumentSymbolClientCapabilities;
   --
   --	/**
   --	 * Capabilities specific to the `textDocument/codeAction` request.
   --	 */
   --	codeAction?: CodeActionClientCapabilities;
   --
   --	/**
   --	 * Capabilities specific to the `textDocument/codeLens` request.
   --	 */
   --	codeLens?: CodeLensClientCapabilities;
   --
   --	/**
   --	 * Capabilities specific to the `textDocument/documentLink` request.
   --	 */
   --	documentLink?: DocumentLinkClientCapabilities;
   --
   --	/**
   --	 * Capabilities specific to the `textDocument/documentColor` and the
   --	 * `textDocument/colorPresentation` request.
   --	 *
   --	 * @since 3.6.0
   --	 */
   --	colorProvider?: DocumentColorClientCapabilities;
   --
   --	/**
   --	 * Capabilities specific to the `textDocument/formatting` request.
   --	 */
   --	formatting?: DocumentFormattingClientCapabilities
   --
   --	/**
   --	 * Capabilities specific to the `textDocument/rangeFormatting` request.
   --	 */
   --	rangeFormatting?: DocumentRangeFormattingClientCapabilities;
   --
   --	/** request.
   --	 * Capabilities specific to the `textDocument/onTypeFormatting` request.
   --	 */
   --	onTypeFormatting?: DocumentOnTypeFormattingClientCapabilities;
   --
   --	/**
   --	 * Capabilities specific to the `textDocument/rename` request.
   --	 */
   --	rename?: RenameClientCapabilities;
   --
   --	/**
   --	 * Capabilities specific to the `textDocument/publishDiagnostics`
   --	 * notification.
   --	 */
   --	publishDiagnostics?: PublishDiagnosticsClientCapabilities;
   --
   --	/**
   --	 * Capabilities specific to the `textDocument/foldingRange` request.
   --	 *
   --	 * @since 3.10.0
   --	 */
   --	foldingRange?: FoldingRangeClientCapabilities;
   --
   --	/**
   --	 * Capabilities specific to the `textDocument/selectionRange` request.
   --	 *
   --	 * @since 3.15.0
   --	 */
   --	selectionRange?: SelectionRangeClientCapabilities;
   --
   --	/**
   --	 * Capabilities specific to the `textDocument/linkedEditingRange` request.
   --	 *
   --	 * @since 3.16.0
   --	 */
   --	linkedEditingRange?: LinkedEditingRangeClientCapabilities;
   --
   --	/**
   --	 * Capabilities specific to the various call hierarchy requests.
   --	 *
   --	 * @since 3.16.0
   --	 */
   --	callHierarchy?: CallHierarchyClientCapabilities;
   --
   --	/**
   --	 * Capabilities specific to the various semantic token requests.
   --	 *
   --	 * @since 3.16.0
   --	 */
   --	semanticTokens?: SemanticTokensClientCapabilities;
   --
   --	/**
   --	 * Capabilities specific to the `textDocument/moniker` request.
   --	 *
   --	 * @since 3.16.0
   --	 */
   --	moniker?: MonikerClientCapabilities;
   --}
   --```
   type TextDocumentClientCapabilities is record
      synchronization    : TextDocumentSyncClientCapabilities;
      completion         : CompletionClientCapabilities;
      hover              : Optional_HoverClientCapabilities;
      signatureHelp      : Optional_SignatureHelpClientCapabilities;
      declaration        : Optional_DeclarationClientCapabilities;
      definition         : Optional_DefinitionClientCapabilities;
      typeDefinition     : Optional_TypeDefinitionClientCapabilities;
      implementation     : Optional_ImplementationClientCapabilities;
      references         : ReferenceClientCapabilities;
      documentHighlight  : DocumentHighlightClientCapabilities;
      documentSymbol     : Optional_DocumentSymbolClientCapabilities;
      codeAction         : Optional_CodeActionClientCapabilities;
      codeLens           : CodeLensClientCapabilities;
      documentLink       : Optional_DocumentLinkClientCapabilities;
      colorProvider      : DocumentColorClientCapabilities;
      formatting         : DocumentFormattingClientCapabilities;
      rangeFormatting    : DocumentRangeFormattingClientCapabilities;
      onTypeFormatting   : DocumentOnTypeFormattingClientCapabilities;
      rename             : Optional_RenameClientCapabilities;
      publishDiagnostics : Optional_PublishDiagnosticsClientCapabilities;
      foldingRange       : Optional_FoldingRangeClientCapabilities;
      selectionRange     : SelectionRangeClientCapabilities;
      linkedEditingRange : LinkedEditingRangeClientCapabilities;
      callHierarchy      : CallHierarchyClientCapabilities;
      semanticTokens     : Optional_SemanticTokensClientCapabilities;
      moniker            : MonikerClientCapabilities;
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
   --/**
   -- * Client capabilities for the show document request.
   -- *
   -- * @since 3.16.0
   -- */
   --export interface ShowDocumentClientCapabilities {
   --	/**
   --	 * The client has support for the show document
   --	 * request.
   --	 */
   --	support: boolean;
   --}
   --```
   type ShowDocumentClientCapabilities is record
      support: Boolean;
   end record;

   procedure Read_ShowDocumentClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ShowDocumentClientCapabilities);
   procedure Write_ShowDocumentClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ShowDocumentClientCapabilities);

   for ShowDocumentClientCapabilities'Read use Read_ShowDocumentClientCapabilities;
   for ShowDocumentClientCapabilities'Write use Write_ShowDocumentClientCapabilities;

   package Optional_ShowDocumentClientCapabilities_Package is
     new LSP.Generic_Optional (ShowDocumentClientCapabilities);

   type Optional_ShowDocumentClientCapabilities is
     new Optional_ShowDocumentClientCapabilities_Package.Optional_Type;

   type MessageActionItemCapabilities is record
      additionalPropertiesSupport: Optional_Boolean;
   end record;

   procedure Read_MessageActionItemCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out MessageActionItemCapabilities);
   procedure Write_MessageActionItemCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : MessageActionItemCapabilities);

   for MessageActionItemCapabilities'Read use Read_MessageActionItemCapabilities;
   for MessageActionItemCapabilities'Write use Write_MessageActionItemCapabilities;

   package Optional_MessageActionItemCapabilities_Package is
     new LSP.Generic_Optional (MessageActionItemCapabilities);

   type Optional_MessageActionItemCapabilities is
     new Optional_MessageActionItemCapabilities_Package.Optional_Type;

   --```typescript
   --/**
   -- * Show message request client capabilities
   -- */
   --export interface ShowMessageRequestClientCapabilities {
   --	/**
   --	 * Capabilities specific to the `MessageActionItem` type.
   --	 */
   --	messageActionItem?: {
   --		/**
   --		 * Whether the client supports additional attributes which
   --		 * are preserved and sent back to the server in the
   --		 * request's response.
   --		 */
   --		additionalPropertiesSupport?: boolean;
   --	}
   --}
   --```
   type ShowMessageRequestClientCapabilities is record
      messageActionItem: Optional_MessageActionItemCapabilities;
   end record;

   procedure Read_ShowMessageRequestClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ShowMessageRequestClientCapabilities);
   procedure Write_ShowMessageRequestClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ShowMessageRequestClientCapabilities);

   for ShowMessageRequestClientCapabilities'Read
     use Read_ShowMessageRequestClientCapabilities;
   for ShowMessageRequestClientCapabilities'Write
     use Write_ShowMessageRequestClientCapabilities;

   package Optional_ShowMessageRequestClientCapabilities_Package is
     new LSP.Generic_Optional (ShowMessageRequestClientCapabilities);

   type Optional_ShowMessageRequestClientCapabilities is
     new Optional_ShowMessageRequestClientCapabilities_Package.Optional_Type;

   --```typescript
   --	/**
   --	 * Window specific client capabilities.
   --	 */
   --	window?: {
   --		/**
   --		 * Whether client supports server initiated progress using the
   --		 * `window/workDoneProgress/create` request.
   --		 */
   --		workDoneProgress?: boolean;
   --	}
   --```
   type WindowClientCapabilities is record
      workDoneProgress: Optional_Boolean;
      showMessage: Optional_ShowMessageRequestClientCapabilities;
      showDocument: Optional_ShowDocumentClientCapabilities;
   end record;

   procedure Read_WindowClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out WindowClientCapabilities);

   procedure Write_WindowClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : WindowClientCapabilities);

   for WindowClientCapabilities'Read use Read_WindowClientCapabilities;
   for WindowClientCapabilities'Write use Write_WindowClientCapabilities;

   package Optional_WindowClientCapabilities_Package is
     new LSP.Generic_Optional (WindowClientCapabilities);

   type Optional_WindowClientCapabilities is
     new Optional_WindowClientCapabilities_Package.Optional_Type;

   --```typescript
   --/**
   -- * Client capabilities specific to the used markdown parser.
   -- *
   -- * @since 3.16.0
   -- */
   --export interface MarkdownClientCapabilities {
   --	/**
   --	 * The name of the parser.
   --	 */
   --	parser: string;
   --
   --	/**
   --	 * The version of the parser.
   --	 */
   --	version?: string;
   --}
   --```
   type MarkdownClientCapabilities is record
      parser  : VSS.Strings.Virtual_String;
      version : Optional_Virtual_String;
   end record;

   procedure Read_MarkdownClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out MarkdownClientCapabilities);

   procedure Write_MarkdownClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : MarkdownClientCapabilities);

   for MarkdownClientCapabilities'Read use Read_MarkdownClientCapabilities;
   for MarkdownClientCapabilities'Write use Write_MarkdownClientCapabilities;

   package Optional_MarkdownClientCapabilities_Package is
     new LSP.Generic_Optional (MarkdownClientCapabilities);

   type Optional_MarkdownClientCapabilities is
     new Optional_MarkdownClientCapabilities_Package.Optional_Type;

   --```typescript
   --/**
   -- * Client capabilities specific to regular expressions.
   -- */
   --export interface RegularExpressionsClientCapabilities {
   --	/**
   --	 * The engine's name.
   --	 */
   --	engine: string;
   --
   --	/**
   --	 * The engine's version.
   --	 */
   --	version?: string;
   --}
   --```
   type RegularExpressionsClientCapabilities is record
      engine  : VSS.Strings.Virtual_String;
      version : Optional_Virtual_String;
   end record;

   procedure Read_RegularExpressionsClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out RegularExpressionsClientCapabilities);

   procedure Write_RegularExpressionsClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : RegularExpressionsClientCapabilities);

   for RegularExpressionsClientCapabilities'Read use Read_RegularExpressionsClientCapabilities;
   for RegularExpressionsClientCapabilities'Write use Write_RegularExpressionsClientCapabilities;

   package Optional_RegularExpressionsClientCapabilities_Package is
     new LSP.Generic_Optional (RegularExpressionsClientCapabilities);

   type Optional_RegularExpressionsClientCapabilities is
     new Optional_RegularExpressionsClientCapabilities_Package.Optional_Type;

   type GeneralClientCapabilities is record
      regularExpressions: Optional_RegularExpressionsClientCapabilities;
      markdown: Optional_MarkdownClientCapabilities;
   end record;

   procedure Read_GeneralClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out GeneralClientCapabilities);

   procedure Write_GeneralClientCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : GeneralClientCapabilities);

   for GeneralClientCapabilities'Read use Read_GeneralClientCapabilities;
   for GeneralClientCapabilities'Write use Write_GeneralClientCapabilities;

   package Optional_GeneralClientCapabilities_Package is
     new LSP.Generic_Optional (GeneralClientCapabilities);

   type Optional_GeneralClientCapabilities is
     new Optional_GeneralClientCapabilities_Package.Optional_Type;

   --```typescript
   --interface ClientCapabilities {
   --	/**
   --	 * Workspace specific client capabilities.
   --	 */
   --	workspace?: {
   --		/**
   --		 * The client supports applying batch edits
   --		 * to the workspace by supporting the request
   --		 * 'workspace/applyEdit'
   --		 */
   --		applyEdit?: boolean;
   --
   --		/**
   --		 * Capabilities specific to `WorkspaceEdit`s
   --		 */
   --		workspaceEdit?: WorkspaceEditClientCapabilities;
   --
   --		/**
   --		 * Capabilities specific to the `workspace/didChangeConfiguration`
   --		 * notification.
   --		 */
   --		didChangeConfiguration?: DidChangeConfigurationClientCapabilities;
   --
   --		/**
   --		 * Capabilities specific to the `workspace/didChangeWatchedFiles`
   --		 * notification.
   --		 */
   --		didChangeWatchedFiles?: DidChangeWatchedFilesClientCapabilities;
   --
   --		/**
   --		 * Capabilities specific to the `workspace/symbol` request.
   --		 */
   --		symbol?: WorkspaceSymbolClientCapabilities;
   --
   --		/**
   --		 * Capabilities specific to the `workspace/executeCommand` request.
   --		 */
   --		executeCommand?: ExecuteCommandClientCapabilities;
   --
   --		/**
   --		 * The client has support for workspace folders.
   --		 *
   --		 * @since 3.6.0
   --		 */
   --		workspaceFolders?: boolean;
   --
   --		/**
   --		 * The client supports `workspace/configuration` requests.
   --		 *
   --		 * @since 3.6.0
   --		 */
   --		configuration?: boolean;
   --
   --		/**
   --		 * Capabilities specific to the semantic token requests scoped to the
   --		 * workspace.
   --		 *
   --		 * @since 3.16.0
   --		 */
   --		 semanticTokens?: SemanticTokensWorkspaceClientCapabilities;
   --
   --		/**
   --		 * Capabilities specific to the code lens requests scoped to the
   --		 * workspace.
   --		 *
   --		 * @since 3.16.0
   --		 */
   --		codeLens?: CodeLensWorkspaceClientCapabilities;
   --
   --		/**
   --		 * The client has support for file requests/notifications.
   --		 *
   --		 * @since 3.16.0
   --		 */
   --		fileOperations?: {
   --			/**
   --			 * Whether the client supports dynamic registration for file
   --			 * requests/notifications.
   --			 */
   --			dynamicRegistration?: boolean;
   --
   --			/**
   --			 * The client has support for sending didCreateFiles notifications.
   --			 */
   --			didCreate?: boolean;
   --
   --			/**
   --			 * The client has support for sending willCreateFiles requests.
   --			 */
   --			willCreate?: boolean;
   --
   --			/**
   --			 * The client has support for sending didRenameFiles notifications.
   --			 */
   --			didRename?: boolean;
   --
   --			/**
   --			 * The client has support for sending willRenameFiles requests.
   --			 */
   --			willRename?: boolean;
   --
   --			/**
   --			 * The client has support for sending didDeleteFiles notifications.
   --			 */
   --			didDelete?: boolean;
   --
   --			/**
   --			 * The client has support for sending willDeleteFiles requests.
   --			 */
   --			willDelete?: boolean;
   --		}
   --	};
   --
   --	/**
   --	 * Text document specific client capabilities.
   --	 */
   --	textDocument?: TextDocumentClientCapabilities;
   --
   --	/**
   --	 * Window specific client capabilities.
   --	 */
   --	window?: {
   --		/**
   --		 * Whether client supports handling progress notifications. If set
   --		 * servers are allowed to report in `workDoneProgress` property in the
   --		 * request specific server capabilities.
   --		 *
   --		 * @since 3.15.0
   --		 */
   --		workDoneProgress?: boolean;
   --
   --		/**
   --		 * Capabilities specific to the showMessage request
   --		 *
   --		 * @since 3.16.0
   --		 */
   --		showMessage?: ShowMessageRequestClientCapabilities;
   --
   --		/**
   --		 * Client capabilities for the show document request.
   --		 *
   --		 * @since 3.16.0
   --		 */
   --		showDocument?: ShowDocumentClientCapabilities;
   --	}
   --
   --	/**
   --	 * General client capabilities.
   --	 *
   --	 * @since 3.16.0
   --	 */
   --	general?: {
   --		/**
   --		 * Client capabilities specific to regular expressions.
   --		 *
   --		 * @since 3.16.0
   --		 */
   --		regularExpressions?: RegularExpressionsClientCapabilities;
   --
   --		/**
   --		 * Client capabilities specific to the client's markdown parser.
   --		 *
   --		 * @since 3.16.0
   --		 */
   --		markdown?: MarkdownClientCapabilities;
   --	}
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
      window: Optional_WindowClientCapabilities;
      general: Optional_GeneralClientCapabilities;
      experimental: Optional_LSP_Any;
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
      uri  : DocumentUri;
      name : VSS.Strings.Virtual_String;
   end record;

   procedure Read_WorkspaceFolder
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out WorkspaceFolder);

   procedure Write_WorkspaceFolder
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : WorkspaceFolder);

   for WorkspaceFolder'Read use Read_WorkspaceFolder;
   for WorkspaceFolder'Write use Write_WorkspaceFolder;

   package WorkspaceFolder_Vectors is new
     LSP.Generic_Vectors (WorkspaceFolder, Write_Empty => LSP.Write_Array);

   type WorkspaceFolder_Vector is new WorkspaceFolder_Vectors.Vector with null record;

   package Optional_WorkspaceFolder_Vectors is new LSP.Generic_Optional (WorkspaceFolder_Vector);
   type Optional_WorkspaceFolder_Vector is new Optional_WorkspaceFolder_Vectors.Optional_Type;

   --```typescript
   --type ProgressToken = integer | string;
   --
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
   package Optional_ProgressToken_Package is
     new LSP.Generic_Optional (ProgressToken);

   type Optional_ProgressToken is
     new Optional_ProgressToken_Package.Optional_Type;

   generic
      type T is private;
   package Generic_ProgressParam is
      type ProgressParam is record
         token: ProgressToken;
         value: T;
      end record;
   end Generic_ProgressParam;

   --```typescript
   --export interface WorkDoneProgressCreateParams {
   --	/**
   --	 * The token to be used to report progress.
   --	 */
   --	token: ProgressToken;
   --}
   --```
   type WorkDoneProgressCreateParams is record
      token: ProgressToken;
   end record;

   procedure Read_WorkDoneProgressCreateParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out WorkDoneProgressCreateParams);

   procedure Write_WorkDoneProgressCreateParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : WorkDoneProgressCreateParams);

   for WorkDoneProgressCreateParams'Read use Read_WorkDoneProgressCreateParams;
   for WorkDoneProgressCreateParams'Write use Write_WorkDoneProgressCreateParams;

   --```typescript
   --export interface WorkDoneProgressParams {
   --	/**
   --	 * An optional token that a server can use to report work done progress.
   --	 */
   --	workDoneToken?: ProgressToken;
   --}
   --```
   type WorkDoneProgressParams is tagged record
      workDoneToken: Optional_ProgressToken;
   end record;

   --```typescript
   --export interface PartialResultParams {
   --	/**
   --	 * An optional token that a server can use to report partial results (e.g.
   --	 * streaming) to the client.
   --	 */
   --	partialResultToken?: ProgressToken;
   --}
   --```
   type PartialResultParams is abstract tagged record
      partialResultToken: Optional_ProgressToken;
   end record;

   --  Common type for `extends WorkDoneProgressParams, PartialResultParams`
   type Progress_Partial_Params is abstract tagged record
      workDoneToken: Optional_ProgressToken;
      partialResultToken: Optional_ProgressToken;
   end record;

   --  Common type for `extends TextDocumentPositionParams, WorkDoneProgressParams, PartialResultParams`
   type Text_Progress_Partial_Params is abstract new TextDocumentPositionParams with record
      workDoneToken: Optional_ProgressToken;
      partialResultToken: Optional_ProgressToken;
   end record;

   --  Common type for `extends TextDocumentPositionParams, WorkDoneProgressParams`
   type Text_Progress_Params is abstract new TextDocumentPositionParams with record
      workDoneToken: Optional_ProgressToken;
   end record;

   procedure Read_Text_Progress_Params
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LSP.Messages.Text_Progress_Params);

   procedure Write_Text_Progress_Params
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LSP.Messages.Text_Progress_Params);

   for Text_Progress_Params'Read use Read_Text_Progress_Params;
   for Text_Progress_Params'Write use Write_Text_Progress_Params;

   type ProgramInfo is record
      name         : VSS.Strings.Virtual_String;
      version      : Optional_Virtual_String;

      log_filename : Optional_Virtual_String;
      --  The absolute path of the Ada Language Server log file, if any
   end record;

   procedure Read_ProgramInfo
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ProgramInfo);

   procedure Write_ProgramInfo
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ProgramInfo);

   for ProgramInfo'Read use Read_ProgramInfo;
   for ProgramInfo'Write use Write_ProgramInfo;

   package Optional_ProgramInfo_Package is
     new LSP.Generic_Optional (ProgramInfo);

   type Optional_ProgramInfo is
     new Optional_ProgramInfo_Package.Optional_Type;

   --```typescript
   --export type TraceValue = 'off' | 'message' | 'verbose'
   --```
   type TraceValue is (off, messages_trace, verbose);

   procedure Read_TraceValue
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out TraceValue);

   procedure Write_TraceValue
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : TraceValue);

   for TraceValue'Read use Read_TraceValue;
   for TraceValue'Write use Write_TraceValue;

   package Optional_TraceValue_Package is
     new LSP.Generic_Optional (TraceValue);

   type Optional_TraceValue is
     new Optional_TraceValue_Package.Optional_Type;

   package Optional_Nullable_Strings is
     new LSP.Generic_Optional (Nullable_String);

   type Optional_Nullable_String is
     new Optional_Nullable_Strings.Optional_Type;

   --```typescript
   --interface InitializeParams extends WorkDoneProgressParams {
   --	/**
   --	 * The process Id of the parent process that started the server. Is null if
   --	 * the process has not been started by another process. If the parent
   --	 * process is not alive then the server should exit (see exit notification)
   --	 * its process.
   --	 */
   --	processId: integer | null;
   --
   --	/**
   --	 * Information about the client
   --	 *
   --	 * @since 3.15.0
   --	 */
   --	clientInfo?: {
   --		/**
   --		 * The name of the client as defined by the client.
   --		 */
   --		name: string;
   --
   --		/**
   --		 * The client's version as defined by the client.
   --		 */
   --		version?: string;
   --	};
   --
   --	/**
   --	 * The locale the client is currently showing the user interface
   --	 * in. This must not necessarily be the locale of the operating
   --	 * system.
   --	 *
   --	 * Uses IETF language tags as the value's syntax
   --	 * (See https://en.wikipedia.org/wiki/IETF_language_tag)
   --	 *
   --	 * @since 3.16.0
   --	 */
   --	locale?: string;
   --
   --	/**
   --	 * The rootPath of the workspace. Is null
   --	 * if no folder is open.
   --	 *
   --	 * @deprecated in favour of `rootUri`.
   --	 */
   --	rootPath?: string | null;
   --
   --	/**
   --	 * The rootUri of the workspace. Is null if no
   --	 * folder is open. If both `rootPath` and `rootUri` are set
   --	 * `rootUri` wins.
   --	 *
   --	 * @deprecated in favour of `workspaceFolders`
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
   --	trace?: TraceValue;
   --
   --	/**
   --	 * The workspace folders configured in the client when the server starts.
   --	 * This property is only available if the client supports workspace folders.
   --	 * It can be `null` if the client supports workspace folders but none are
   --	 * configured.
   --	 *
   --	 * @since 3.6.0
   --	 */
   --	workspaceFolders?: WorkspaceFolder[] | null;
   --}
   --```
   type InitializeParams is new WorkDoneProgressParams with record
      processId        : Optional_Number;
      clientInfo       : Optional_ProgramInfo;
      locale           : Optional_Virtual_String;
      rootPath         : Optional_Nullable_String;
      rootUri          : Nullable_String;
      --  initializationOptions?: any;
      capabilities     : ClientCapabilities;
      trace            : Optional_TraceValue;
      workspaceFolders : Optional_WorkspaceFolder_Vector;
   end record;

   procedure Read_InitializeParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out InitializeParams);

   procedure Write_InitializeParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : InitializeParams);

   for InitializeParams'Read use Read_InitializeParams;
   for InitializeParams'Write use Write_InitializeParams;

   --```typescript
   --export interface WorkDoneProgressOptions {
   --	workDoneProgress?: boolean;
   --}
   --```
   type WorkDoneProgressOptions is tagged record
      workDoneProgress: Optional_Boolean;
   end record;

   procedure Read_WorkDoneProgressOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out WorkDoneProgressOptions);

   procedure Write_WorkDoneProgressOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : WorkDoneProgressOptions);

   for WorkDoneProgressOptions'Read use Read_WorkDoneProgressOptions;
   for WorkDoneProgressOptions'Write use Write_WorkDoneProgressOptions;

   package Optional_WorkDoneProgressOptions_Package is
     new LSP.Generic_Optional (WorkDoneProgressOptions);

   type Optional_WorkDoneProgressOptions is
     new Optional_WorkDoneProgressOptions_Package.Optional_Type;

   --```typescript
   --/**
   -- * Completion options.
   -- */
   --export interface CompletionOptions extends WorkDoneProgressOptions {
   --	/**
   --	 * Most tools trigger completion request automatically without explicitly
   --	 * requesting it using a keyboard shortcut (e.g. Ctrl+Space). Typically they
   --	 * do so when the user starts to type an identifier. For example if the user
   --	 * types `c` in a JavaScript file code complete will automatically pop up
   --	 * present `console` besides others as a completion item. Characters that
   --	 * make up identifiers don't need to be listed here.
   --	 *
   --	 * If code complete should automatically be trigger on characters not being
   --	 * valid inside an identifier (for example `.` in JavaScript) list them in
   --	 * `triggerCharacters`.
   --	 */
   --	triggerCharacters?: string[];
   --
   --	/**
   --	 * The list of all possible characters that commit a completion. This field
   --	 * can be used if clients don't support individual commit characters per
   --	 * completion item. See client capability
   --	 * `completion.completionItem.commitCharactersSupport`.
   --	 *
   --	 * If a server provides both `allCommitCharacters` and commit characters on
   --	 * an individual completion item the ones on the completion item win.
   --	 *
   --	 * @since 3.2.0
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
   type CompletionOptions is new WorkDoneProgressOptions with record
      triggerCharacters   : Optional_Virtual_String_Vector;
      allCommitCharacters : Optional_Virtual_String_Vector;
      resolveProvider     : LSP.Types.Optional_Boolean;
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

   --```typescript
   --export interface HoverOptions extends WorkDoneProgressOptions {
   --}
   --```
   subtype HoverOptions is Optional_WorkDoneProgressOptions;

   --```typescript
   --export interface SignatureHelpOptions extends WorkDoneProgressOptions {
   --	/**
   --	 * The characters that trigger signature help
   --	 * automatically.
   --	 */
   --	triggerCharacters?: string[];
   --
   --	/**
   --	 * List of characters that re-trigger signature help.
   --	 *
   --	 * These trigger characters are only active when signature help is already
   --	 * showing. All trigger characters are also counted as re-trigger
   --	 * characters.
   --	 *
   --	 * @since 3.15.0
   --	 */
   --	retriggerCharacters?: string[];
   --}
   --```
   type SignatureHelpOptions is new WorkDoneProgressOptions with record
      triggerCharacters   : Optional_Virtual_String_Vector;
      retriggerCharacters : Optional_Virtual_String_Vector;
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

   --```typescript
   --/**
   -- * General text document registration options.
   -- */
   --export interface TextDocumentRegistrationOptions {
   --	/**
   --	 * A document selector to identify the scope of the registration. If set to
   --	 * null the document selector provided on the client side will be used.
   --	 */
   --	documentSelector: DocumentSelector | null;
   --}
   --```
   type TextDocumentRegistrationOptions is tagged record
      documentSelector:  LSP.Messages.DocumentSelector;
   end record;

   procedure Read_TextDocumentRegistrationOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out TextDocumentRegistrationOptions);
   for TextDocumentRegistrationOptions'Read use Read_TextDocumentRegistrationOptions;
   procedure Write_TextDocumentRegistrationOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : TextDocumentRegistrationOptions);
   for TextDocumentRegistrationOptions'Write use Write_TextDocumentRegistrationOptions;

   --```typescript
   --/**
   -- * Static registration options to be returned in the initialize request.
   -- */
   --export interface StaticRegistrationOptions {
   --	/**
   --	 * The id used to register the request. The id can be used to deregister
   --	 * the request again. See also Registration#id.
   --	 */
   --	id?: string;
   --}
   --```

   --  Here we define Ada type TSW_RegistrationOptions that corresponds to
   --  (TextDocumentRegistrationOptions, StaticRegistrationOptions, WorkDoneProgressOptions)
   --  typescript type, because it is always used with
   --  TextDocumentRegistrationOptions and we don't have multiple inheritance (or
   --  something similar) for Ada types.
   type TSW_RegistrationOptions is new WorkDoneProgressOptions with record
      id              : Optional_Virtual_String;
      documentSelector: LSP.Messages.DocumentSelector;
   end record;

   procedure Read_TSW_RegistrationOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out TSW_RegistrationOptions);

   procedure Write_TSW_RegistrationOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : TSW_RegistrationOptions);

   for TSW_RegistrationOptions'Read use Read_TSW_RegistrationOptions;
   for TSW_RegistrationOptions'Write use Write_TSW_RegistrationOptions;

   package Optional_TSW_RegistrationOptions_Package is
     new LSP.Generic_Optional (TSW_RegistrationOptions);

   type Optional_TSW_RegistrationOptions is
     new Optional_TSW_RegistrationOptions_Package.Optional_Type;

   --  Ada type Provider_Options correspond to this typescript type:
   --  boolean | (TextDocumentRegistrationOptions, StaticRegistrationOptions, WorkDoneProgressOptions)
   type Provider_Options (Is_Boolean : Boolean := False) is record
      case Is_Boolean is
         when True =>
            Bool : Boolean;
         when False =>
            Options : Optional_TSW_RegistrationOptions;
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

   --```typescript
   --export interface DeclarationOptions extends WorkDoneProgressOptions {
   --}
   --```
   subtype DeclarationOptions is Optional_Provider_Options;

   --```typescript
   --export interface DeclarationRegistrationOptions extends DeclarationOptions,
   --	TextDocumentRegistrationOptions, StaticRegistrationOptions  {
   --}
   --```

   --```typescript
   --export interface DefinitionOptions extends WorkDoneProgressOptions {
   --}
   --```
   subtype DefinitionOptions is Optional_WorkDoneProgressOptions;

   --```typescript
   --export interface DefinitionRegistrationOptions extends
   --	TextDocumentRegistrationOptions, DefinitionOptions {
   --}
   --```

   --```typescript
   --export interface TypeDefinitionOptions extends WorkDoneProgressOptions {
   --}
   --```
   subtype TypeDefinitionOptions is Optional_Provider_Options;

   --```typescript
   --export interface TypeDefinitionRegistrationOptions extends
   --	TextDocumentRegistrationOptions, TypeDefinitionOptions,
   --	StaticRegistrationOptions {
   --}
   --```

   --```typescript
   --export interface ImplementationOptions extends WorkDoneProgressOptions {
   --}
   --```
   subtype ImplementationOptions is Optional_Provider_Options;

   --```typescript
   --export interface ImplementationRegistrationOptions extends
   --	TextDocumentRegistrationOptions, ImplementationOptions,
   --	StaticRegistrationOptions {
   --}
   --```

   --```typescript
   --export interface ReferenceOptions extends WorkDoneProgressOptions {
   --}
   --```
   subtype ReferenceOptions is Optional_WorkDoneProgressOptions;

   --```typescript
   --export interface ReferenceRegistrationOptions extends
   --	TextDocumentRegistrationOptions, ReferenceOptions {
   --}
   --```

   --```typescript
   --export interface DocumentHighlightOptions extends WorkDoneProgressOptions {
   --}
   --```
   subtype DocumentHighlightOptions is Optional_WorkDoneProgressOptions;

   --```typescript
   --export interface DocumentHighlightRegistrationOptions extends
   --	TextDocumentRegistrationOptions, DocumentHighlightOptions {
   --}
   --```

   --```typescript
   --export interface DocumentSymbolOptions extends WorkDoneProgressOptions {
   --	/**
   --	 * A human-readable string that is shown when multiple outlines trees
   --	 * are shown for the same document.
   --	 *
   --	 * @since 3.16.0
   --	 */
   --	label?: string;
   --}
   --```
   type DocumentSymbolOptions is new WorkDoneProgressOptions with record
      label : Optional_Virtual_String;
   end record;

   procedure Read_DocumentSymbolOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DocumentSymbolOptions);

   procedure Write_DocumentSymbolOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DocumentSymbolOptions);

   for DocumentSymbolOptions'Read use Read_DocumentSymbolOptions;
   for DocumentSymbolOptions'Write use Write_DocumentSymbolOptions;

   package Optional_DocumentSymbolOptions_Package is
     new LSP.Generic_Optional
       (Element_Type          => DocumentSymbolOptions,
        Write_Default_As_True => True);
   --  LSP4J 0.10 expects documentSymbolProvider to be boolean.

   type Optional_DocumentSymbolOptions is
     new Optional_DocumentSymbolOptions_Package.Optional_Type;

   --```typescript
   --export interface DocumentSymbolRegistrationOptions extends
   --	TextDocumentRegistrationOptions, DocumentSymbolOptions {
   --}
   --```

   --```typescript
   --export interface CodeActionOptions extends WorkDoneProgressOptions {
   --	/**
   --	 * CodeActionKinds that this server may return.
   --	 *
   --	 * The list of kinds may be generic, such as `CodeActionKind.Refactor`,
   --	 * or the server may list out every specific kind they provide.
   --	 */
   --	codeActionKinds?: CodeActionKind[];
   --
   --	/**
   --	 * The server provides support to resolve additional
   --	 * information for a code action.
   --	 *
   --	 * @since 3.16.0
   --	 */
   --	resolveProvider?: boolean;
   --}
   --```
   type CodeActionOptions is new WorkDoneProgressOptions with record
      codeActionKinds: Optional_CodeActionKindSet;
      resolveProvider: Optional_Boolean;
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

   --```typescript
   --export interface CodeLensOptions extends WorkDoneProgressOptions {
   --	/**
   --	 * Code lens has a resolve provider as well.
   --	 */
   --	resolveProvider?: boolean;
   --}
   --```
   type CodeLensOptions is new WorkDoneProgressOptions with record
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

   --```typescript
   --export interface DocumentColorOptions extends WorkDoneProgressOptions {
   --}
   --```
   subtype DocumentColorOptions is Optional_Provider_Options;

   --```typescript
   --export interface DocumentColorRegistrationOptions extends
   --	TextDocumentRegistrationOptions, StaticRegistrationOptions,
   --	DocumentColorOptions {
   --}
   --```

   --```typescript
   --export interface DocumentFormattingOptions extends WorkDoneProgressOptions {
   --}
   --```
   subtype DocumentFormattingOptions is Optional_WorkDoneProgressOptions;

   --```typescript
   --export interface DocumentFormattingRegistrationOptions extends
   --	TextDocumentRegistrationOptions, DocumentFormattingOptions {
   --}
   --```

   --```typescript
   --export interface DocumentRangeFormattingOptions extends
   --	WorkDoneProgressOptions {
   --}
   --```
   subtype DocumentRangeFormattingOptions is Optional_WorkDoneProgressOptions;

   --```typescript
   --export interface DocumentRangeFormattingRegistrationOptions extends
   --	TextDocumentRegistrationOptions, DocumentRangeFormattingOptions {
   --}
   --```

   --```typescript
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
   --```
   type DocumentOnTypeFormattingOptions is new WorkDoneProgressOptions with record
      firstTriggerCharacter : VSS.Strings.Virtual_String;
      moreTriggerCharacter  : Optional_Virtual_String_Vector;
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

   --```typescript
   --export interface RenameOptions extends WorkDoneProgressOptions {
   --	/**
   --	 * Renames should be checked and tested before being executed.
   --	 */
   --	prepareProvider?: boolean;
   --}
   --```
   type RenameOptions is new WorkDoneProgressOptions with record
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

   --```typescript
   --export interface DocumentLinkOptions extends WorkDoneProgressOptions {
   --	/**
   --	 * Document links have a resolve provider as well.
   --	 */
   --	resolveProvider?: boolean;
   --}
   --```
   type DocumentLinkOptions is new WorkDoneProgressOptions with record
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

   package Optional_DocumentLinkOptions_Package is
     new LSP.Generic_Optional (DocumentLinkOptions);
   type Optional_DocumentLinkOptions is
     new Optional_DocumentLinkOptions_Package.Optional_Type;

   --```typescript
   --export interface FoldingRangeOptions extends WorkDoneProgressOptions {
   --}
   --```
   subtype FoldingRangeOptions is Optional_Provider_Options;

   --```typescript
   --export interface FoldingRangeRegistrationOptions extends
   --	TextDocumentRegistrationOptions, FoldingRangeOptions,
   --	StaticRegistrationOptions {
   --}
   --```

   --```typescript
   --export interface ExecuteCommandOptions extends WorkDoneProgressOptions {
   --	/**
   --	 * The commands to be executed on the server
   --	 */
   --	commands: string[]
   --}
   --```
   type ExecuteCommandOptions is new WorkDoneProgressOptions with record
      commands: VSS.String_Vectors.Virtual_String_Vector;
   end record;

   procedure Read_ExecuteCommandOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ExecuteCommandOptions);
   procedure Write_ExecuteCommandOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ExecuteCommandOptions);
   for ExecuteCommandOptions'Write use Write_ExecuteCommandOptions;
   for ExecuteCommandOptions'Read use Read_ExecuteCommandOptions;

   package Optional_ExecuteCommandOptions_Package is
     new LSP.Generic_Optional (ExecuteCommandOptions);
   type Optional_ExecuteCommandOptions is
     new Optional_ExecuteCommandOptions_Package.Optional_Type;

   --```typescript
   --export interface WorkspaceSymbolOptions extends WorkDoneProgressOptions {
   --}
   --```
   subtype WorkspaceSymbolOptions is Optional_WorkDoneProgressOptions;

   --```typescript
   --export interface WorkspaceSymbolRegistrationOptions
   --	extends WorkspaceSymbolOptions {
   --}
   --```

   package Optional_Boolean_Or_String_Package is
     new LSP.Generic_Optional (LSP_Boolean_Or_String);

   type Optional_Boolean_Or_String is
     new Optional_Boolean_Or_String_Package.Optional_Type;

   --```typescript
   --export interface WorkspaceFoldersServerCapabilities {
   --	/**
   --	 * The server has support for workspace folders
   --	 */
   --	supported?: boolean;
   --
   --	/**
   --	 * Whether the server wants to receive workspace folder
   --	 * change notifications.
   --	 *
   --	 * If a string is provided, the string is treated as an ID
   --	 * under which the notification is registered on the client
   --	 * side. The ID can be used to unregister for these events
   --	 * using the `client/unregisterCapability` request.
   --	 */
   --	changeNotifications?: string | boolean;
   --}
   --```
   type WorkspaceFoldersServerCapabilities is record
      supported: Optional_Boolean;
      changeNotifications: Optional_Boolean_Or_String;
   end record;

   procedure Read_WorkspaceFoldersServerCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out WorkspaceFoldersServerCapabilities);
   procedure Write_WorkspaceFoldersServerCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : WorkspaceFoldersServerCapabilities);

   for WorkspaceFoldersServerCapabilities'Write use
     Write_WorkspaceFoldersServerCapabilities;
   for WorkspaceFoldersServerCapabilities'Read use
     Read_WorkspaceFoldersServerCapabilities;

   package Optional_WorkspaceFoldersServerCapabilities_Package is
     new LSP.Generic_Optional (WorkspaceFoldersServerCapabilities);

   type Optional_WorkspaceFoldersServerCapabilities is
     new Optional_WorkspaceFoldersServerCapabilities_Package.Optional_Type;

   --```typescript
   --/**
   -- * The options to register for file operations.
   -- *
   -- * @since 3.16.0
   -- */
   --interface FileOperationRegistrationOptions {
   --	/**
   --	 * The actual filters.
   --	 */
   --	filters: FileOperationFilter[];
   --}
   --
   --/**
   -- * A pattern kind describing if a glob pattern matches a file a folder or
   -- * both.
   -- *
   -- * @since 3.16.0
   -- */
   --export namespace FileOperationPatternKind {
   --	/**
   --	 * The pattern matches a file only.
   --	 */
   --	export const file: 'file' = 'file';
   --
   --	/**
   --	 * The pattern matches a folder only.
   --	 */
   --	export const folder: 'folder' = 'folder';
   --}
   --
   --export type FileOperationPatternKind = 'file' | 'folder';
   --
   --/**
   -- * Matching options for the file operation pattern.
   -- *
   -- * @since 3.16.0
   -- */
   --export interface FileOperationPatternOptions {
   --
   --	/**
   --	 * The pattern should be matched ignoring casing.
   --	 */
   --	ignoreCase?: boolean;
   --}
   --
   --/**
   -- * A pattern to describe in which file operation requests or notifications
   -- * the server is interested in.
   -- *
   -- * @since 3.16.0
   -- */
   --interface FileOperationPattern {
   --	/**
   --	 * The glob pattern to match. Glob patterns can have the following syntax:
   --	 * - `*` to match one or more characters in a path segment
   --	 * - `?` to match on one character in a path segment
   --	 * - `**` to match any number of path segments, including none
   --	 * - `{}` to group sub patterns into an OR expression. (e.g. `**​/*.{ts,js}`
   --	 *   matches all TypeScript and JavaScript files)
   --	 * - `[]` to declare a range of characters to match in a path segment
   --	 *   (e.g., `example.[0-9]` to match on `example.0`, `example.1`, …)
   --	 * - `[!...]` to negate a range of characters to match in a path segment
   --	 *   (e.g., `example.[!0-9]` to match on `example.a`, `example.b`, but
   --	 *   not `example.0`)
   --	 */
   --	glob: string;
   --
   --	/**
   --	 * Whether to match files or folders with this pattern.
   --	 *
   --	 * Matches both if undefined.
   --	 */
   --	matches?: FileOperationPatternKind;
   --
   --	/**
   --	 * Additional options used during matching.
   --	 */
   --	options?: FileOperationPatternOptions;
   --}
   --
   --/**
   -- * A filter to describe in which file operation requests or notifications
   -- * the server is interested in.
   -- *
   -- * @since 3.16.0
   -- */
   --export interface FileOperationFilter {
   --
   --	/**
   --	 * A Uri like `file` or `untitled`.
   --	 */
   --	scheme?: string;
   --
   --	/**
   --	 * The actual file operation pattern.
   --	 */
   --	pattern: FileOperationPattern;
   --}
   --```
   type FileOperationPatternKind is (file, folder);

   procedure Read_FileOperationPatternKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out FileOperationPatternKind);
   procedure Write_FileOperationPatternKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : FileOperationPatternKind);

   for FileOperationPatternKind'Write use
     Write_FileOperationPatternKind;
   for FileOperationPatternKind'Read use
     Read_FileOperationPatternKind;

   package Optional_FileOperationPatternKind_Package is
     new LSP.Generic_Optional (FileOperationPatternKind);

   type Optional_FileOperationPatternKind is
     new Optional_FileOperationPatternKind_Package.Optional_Type;

   type FileOperationPatternOptions is record
      ignoreCase: Optional_Boolean;
   end record;

   procedure Read_FileOperationPatternOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out FileOperationPatternOptions);
   procedure Write_FileOperationPatternOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : FileOperationPatternOptions);

   for FileOperationPatternOptions'Write use Write_FileOperationPatternOptions;
   for FileOperationPatternOptions'Read use Read_FileOperationPatternOptions;

   package Optional_FileOperationPatternOptions_Package is
     new LSP.Generic_Optional (FileOperationPatternOptions);

   type Optional_FileOperationPatternOptions is
     new Optional_FileOperationPatternOptions_Package.Optional_Type;

   type FileOperationPattern is record
      glob    : VSS.Strings.Virtual_String;
      matches : Optional_FileOperationPatternKind;
      options : Optional_FileOperationPatternOptions;
   end record;

   procedure Read_FileOperationPattern
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out FileOperationPattern);
   procedure Write_FileOperationPattern
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : FileOperationPattern);

   for FileOperationPattern'Write use Write_FileOperationPattern;
   for FileOperationPattern'Read use Read_FileOperationPattern;

   type FileOperationFilter is record
      scheme  : Optional_Virtual_String;
      pattern : FileOperationPattern;
   end record;

   procedure Read_FileOperationFilter
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out FileOperationFilter);
   procedure Write_FileOperationFilter
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : FileOperationFilter);

   for FileOperationFilter'Write use Write_FileOperationFilter;
   for FileOperationFilter'Read use Read_FileOperationFilter;

   package FileOperationFilter_Vectors is new LSP.Generic_Vectors
     (FileOperationFilter, Write_Empty => LSP.Write_Array);
   type FileOperationFilter_Vector is new FileOperationFilter_Vectors.Vector with null record;

   type FileOperationRegistrationOptions is record
      filters: FileOperationFilter_Vector;
   end record;

   procedure Read_FileOperationRegistrationOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out FileOperationRegistrationOptions);
   procedure Write_FileOperationRegistrationOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : FileOperationRegistrationOptions);

   for FileOperationRegistrationOptions'Write use
     Write_FileOperationRegistrationOptions;
   for FileOperationRegistrationOptions'Read use
     Read_FileOperationRegistrationOptions;

   package Optional_FileOperationRegistrationOptions_Package is
     new LSP.Generic_Optional (FileOperationRegistrationOptions);

   type Optional_FileOperationRegistrationOptions is
     new Optional_FileOperationRegistrationOptions_Package.Optional_Type;

   type FileOperationsServerCapabilities is record
      didCreate: Optional_FileOperationRegistrationOptions;
      willCreate: Optional_FileOperationRegistrationOptions;
      didRename: Optional_FileOperationRegistrationOptions;
      willRename: Optional_FileOperationRegistrationOptions;
      didDelete: Optional_FileOperationRegistrationOptions;
      willDelete: Optional_FileOperationRegistrationOptions;
   end record;

   procedure Read_FileOperationsServerCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out FileOperationsServerCapabilities);
   procedure Write_FileOperationsServerCapabilities
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : FileOperationsServerCapabilities);

   for FileOperationsServerCapabilities'Write use
     Write_FileOperationsServerCapabilities;
   for FileOperationsServerCapabilities'Read use
     Read_FileOperationsServerCapabilities;

   package Optional_FileOperationsServerCapabilities_Package is
     new LSP.Generic_Optional (FileOperationsServerCapabilities);

   type Optional_FileOperationsServerCapabilities is
     new Optional_FileOperationsServerCapabilities_Package.Optional_Type;

   type workspace_Options is record
      workspaceFolders: Optional_WorkspaceFoldersServerCapabilities;
      fileOperations: Optional_FileOperationsServerCapabilities;
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

   --```typescript
   --export interface SelectionRangeOptions extends WorkDoneProgressOptions {
   --}
   --```
   subtype SelectionRangeOptions is Optional_Provider_Options;

   --```typescript
   --export interface SelectionRangeRegistrationOptions extends
   --	SelectionRangeOptions, TextDocumentRegistrationOptions,
   --	StaticRegistrationOptions {
   --}
   --```

   --```typescript
   --export interface LinkedEditingRangeOptions extends WorkDoneProgressOptions {
   --}
   --```
   subtype LinkedEditingRangeOptions is Optional_Provider_Options;

   --```typescript
   --export interface LinkedEditingRangeRegistrationOptions extends
   --	TextDocumentRegistrationOptions, LinkedEditingRangeOptions,
   --	StaticRegistrationOptions {
   --}
   --```

   --```typescript
   --export interface CallHierarchyOptions extends WorkDoneProgressOptions {
   --}
   --```
   subtype CallHierarchyOptions is Optional_Provider_Options;

   --```typescript
   --export interface CallHierarchyRegistrationOptions extends
   --	TextDocumentRegistrationOptions, CallHierarchyOptions,
   --	StaticRegistrationOptions {
   --}
   --```

   --```typescript
   --export interface SemanticTokensLegend {
   --	/**
   --	 * The token types a server uses.
   --	 */
   --	tokenTypes: string[];
   --
   --	/**
   --	 * The token modifiers a server uses.
   --	 */
   --	tokenModifiers: string[];
   --}
   --```
   type SemanticTokensLegend is record
      tokenTypes: VSS.String_Vectors.Virtual_String_Vector;
      tokenModifiers: VSS.String_Vectors.Virtual_String_Vector;
   end record;

   procedure Read_SemanticTokensLegend
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out SemanticTokensLegend);
   procedure Write_SemanticTokensLegend
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SemanticTokensLegend);
   for SemanticTokensLegend'Write use Write_SemanticTokensLegend;
   for SemanticTokensLegend'Read use Read_SemanticTokensLegend;

   --```typescript
   --export interface SemanticTokensOptions extends WorkDoneProgressOptions {
   --	/**
   --	 * The legend used by the server
   --	 */
   --	legend: SemanticTokensLegend;
   --
   --	/**
   --	 * Server supports providing semantic tokens for a specific range
   --	 * of a document.
   --	 */
   --	range?: boolean | {
   --	};
   --
   --	/**
   --	 * Server supports providing semantic tokens for a full document.
   --	 */
   --	full?: boolean | {
   --		/**
   --		 * The server supports deltas for full documents.
   --		 */
   --		delta?: boolean;
   --	}
   --}
   --```
   type SemanticTokensOptions is new WorkDoneProgressOptions with record
      legend: SemanticTokensLegend;
      span: Optional_Boolean;    --  Range is a reserved Ada keyword
      full: Optional_SemanticTokensFullCapabilities;
   end record;

   procedure Read_SemanticTokensOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out SemanticTokensOptions);
   procedure Write_SemanticTokensOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SemanticTokensOptions);
   for SemanticTokensOptions'Write use Write_SemanticTokensOptions;
   for SemanticTokensOptions'Read use Read_SemanticTokensOptions;

   package Optional_SemanticTokensOptions_Package is
     new LSP.Generic_Optional (SemanticTokensOptions);
   type Optional_SemanticTokensOptions is
     new Optional_SemanticTokensOptions_Package.Optional_Type;

   --```typescript
   --export interface SemanticTokensRegistrationOptions extends
   --	TextDocumentRegistrationOptions, SemanticTokensOptions,
   --	StaticRegistrationOptions {
   --}
   --```

   --```typescript
   --export interface MonikerOptions extends WorkDoneProgressOptions {
   --}
   --```
   subtype MonikerOptions is Optional_WorkDoneProgressOptions;

   --```typescript
   --export interface MonikerRegistrationOptions extends
   --	TextDocumentRegistrationOptions, MonikerOptions {
   --}
   --```

   --
   --```typescript
   --interface ServerCapabilities {
   --	/**
   --	 * Defines how text documents are synced. Is either a detailed structure
   --	 * defining each notification or for backwards compatibility the
   --	 * TextDocumentSyncKind number. If omitted it defaults to
   --	 * `TextDocumentSyncKind.None`.
   --	 */
   --	textDocumentSync?: TextDocumentSyncOptions | TextDocumentSyncKind;
   --
   --	/**
   --	 * The server provides completion support.
   --	 */
   --	completionProvider?: CompletionOptions;
   --
   --	/**
   --	 * The server provides hover support.
   --	 */
   --	hoverProvider?: boolean | HoverOptions;
   --
   --	/**
   --	 * The server provides signature help support.
   --	 */
   --	signatureHelpProvider?: SignatureHelpOptions;
   --
   --	/**
   --	 * The server provides go to declaration support.
   --	 *
   --	 * @since 3.14.0
   --	 */
   --	declarationProvider?: boolean | DeclarationOptions
   --		| DeclarationRegistrationOptions;
   --
   --	/**
   --	 * The server provides goto definition support.
   --	 */
   --	definitionProvider?: boolean | DefinitionOptions;
   --
   --	/**
   --	 * The server provides goto type definition support.
   --	 *
   --	 * @since 3.6.0
   --	 */
   --	typeDefinitionProvider?: boolean | TypeDefinitionOptions
   --		| TypeDefinitionRegistrationOptions;
   --
   --	/**
   --	 * The server provides goto implementation support.
   --	 *
   --	 * @since 3.6.0
   --	 */
   --	implementationProvider?: boolean | ImplementationOptions
   --		| ImplementationRegistrationOptions;
   --
   --	/**
   --	 * The server provides find references support.
   --	 */
   --	referencesProvider?: boolean | ReferenceOptions;
   --
   --	/**
   --	 * The server provides document highlight support.
   --	 */
   --	documentHighlightProvider?: boolean | DocumentHighlightOptions;
   --
   --	/**
   --	 * The server provides document symbol support.
   --	 */
   --	documentSymbolProvider?: boolean | DocumentSymbolOptions;
   --
   --	/**
   --	 * The server provides code actions. The `CodeActionOptions` return type is
   --	 * only valid if the client signals code action literal support via the
   --	 * property `textDocument.codeAction.codeActionLiteralSupport`.
   --	 */
   --	codeActionProvider?: boolean | CodeActionOptions;
   --
   --	/**
   --	 * The server provides code lens.
   --	 */
   --	codeLensProvider?: CodeLensOptions;
   --
   --	/**
   --	 * The server provides document link support.
   --	 */
   --	documentLinkProvider?: DocumentLinkOptions;
   --
   --	/**
   --	 * The server provides color provider support.
   --	 *
   --	 * @since 3.6.0
   --	 */
   --	colorProvider?: boolean | DocumentColorOptions
   --		| DocumentColorRegistrationOptions;
   --
   --	/**
   --	 * The server provides document formatting.
   --	 */
   --	documentFormattingProvider?: boolean | DocumentFormattingOptions;
   --
   --	/**
   --	 * The server provides document range formatting.
   --	 */
   --	documentRangeFormattingProvider?: boolean | DocumentRangeFormattingOptions;
   --
   --	/**
   --	 * The server provides document formatting on typing.
   --	 */
   --	documentOnTypeFormattingProvider?: DocumentOnTypeFormattingOptions;
   --
   --	/**
   --	 * The server provides rename support. RenameOptions may only be
   --	 * specified if the client states that it supports
   --	 * `prepareSupport` in its initial `initialize` request.
   --	 */
   --	renameProvider?: boolean | RenameOptions;
   --
   --	/**
   --	 * The server provides folding provider support.
   --	 *
   --	 * @since 3.10.0
   --	 */
   --	foldingRangeProvider?: boolean | FoldingRangeOptions
   --		| FoldingRangeRegistrationOptions;
   --
   --	/**
   --	 * The server provides execute command support.
   --	 */
   --	executeCommandProvider?: ExecuteCommandOptions;
   --
   --	/**
   --	 * The server provides selection range support.
   --	 *
   --	 * @since 3.15.0
   --	 */
   --	selectionRangeProvider?: boolean | SelectionRangeOptions
   --		| SelectionRangeRegistrationOptions;
   --
   --	/**
   --	 * The server provides linked editing range support.
   --	 *
   --	 * @since 3.16.0
   --	 */
   --	linkedEditingRangeProvider?: boolean | LinkedEditingRangeOptions
   --		| LinkedEditingRangeRegistrationOptions;
   --
   --	/**
   --	 * The server provides call hierarchy support.
   --	 *
   --	 * @since 3.16.0
   --	 */
   --	callHierarchyProvider?: boolean | CallHierarchyOptions
   --		| CallHierarchyRegistrationOptions;
   --
   --	/**
   --	 * The server provides semantic tokens support.
   --	 *
   --	 * @since 3.16.0
   --	 */
   --	semanticTokensProvider?: SemanticTokensOptions
   --		| SemanticTokensRegistrationOptions;
   --
   --	/**
   --	 * Whether server provides moniker support.
   --	 *
   --	 * @since 3.16.0
   --	 */
   --    monikerProvider?: boolean | MonikerOptions | MonikerRegistrationOptions;
   --
   --	/**
   --	 * The server provides workspace symbol support.
   --	 */
   --	workspaceSymbolProvider?: boolean | WorkspaceSymbolOptions;
   --
   --	/**
   --	 * Workspace specific server capabilities
   --	 */
   --	workspace?: {
   --		/**
   --		 * The server supports workspace folder.
   --		 *
   --		 * @since 3.6.0
   --		 */
   --		workspaceFolders?: WorkspaceFoldersServerCapabilities;
   --
   --		/**
   --		 * The server is interested in file notifications/requests.
   --		 *
   --		 * @since 3.16.0
   --		 */
   --		fileOperations?: {
   --			/**
   --			 * The server is interested in receiving didCreateFiles
   --			 * notifications.
   --			 */
   --			didCreate?: FileOperationRegistrationOptions;
   --
   --			/**
   --			 * The server is interested in receiving willCreateFiles requests.
   --			 */
   --			willCreate?: FileOperationRegistrationOptions;
   --
   --			/**
   --			 * The server is interested in receiving didRenameFiles
   --			 * notifications.
   --			 */
   --			didRename?: FileOperationRegistrationOptions;
   --
   --			/**
   --			 * The server is interested in receiving willRenameFiles requests.
   --			 */
   --			willRename?: FileOperationRegistrationOptions;
   --
   --			/**
   --			 * The server is interested in receiving didDeleteFiles file
   --			 * notifications.
   --			 */
   --			didDelete?: FileOperationRegistrationOptions;
   --
   --			/**
   --			 * The server is interested in receiving willDeleteFiles file
   --			 * requests.
   --			 */
   --			willDelete?: FileOperationRegistrationOptions;
   --		}
   --	}
   --
   --	/**
   --	 * Experimental server capabilities.
   --	 */
   --	experimental?: any;
   --}
   --```
   type ServerCapabilities is record
      textDocumentSync: Optional_TextDocumentSyncOptions;
      completionProvider: Optional_CompletionOptions;
      hoverProvider: HoverOptions;
      signatureHelpProvider: Optional_SignatureHelpOptions;
      declarationProvider: DeclarationOptions;
      definitionProvider: DefinitionOptions;
      typeDefinitionProvider: TypeDefinitionOptions;
      implementationProvider: ImplementationOptions;
      referencesProvider: ReferenceOptions;
      documentHighlightProvider: DocumentHighlightOptions;
      documentSymbolProvider: Optional_DocumentSymbolOptions;
      codeActionProvider: Optional_CodeActionOptions;
      codeLensProvider: Optional_CodeLensOptions;
      documentLinkProvider: Optional_DocumentLinkOptions;
      colorProvider: DocumentColorOptions;
      documentFormattingProvider: DocumentFormattingOptions;
      documentRangeFormattingProvider: DocumentRangeFormattingOptions;
      documentOnTypeFormattingProvider: Optional_DocumentOnTypeFormattingOptions;
      renameProvider: Optional_RenameOptions;
      foldingRangeProvider: FoldingRangeOptions;
      executeCommandProvider: Optional_ExecuteCommandOptions;
      selectionRangeProvider: SelectionRangeOptions;
      linkedEditingRangeProvider: LinkedEditingRangeOptions;
      semanticTokensProvider: Optional_SemanticTokensOptions;
      monikerProvider: MonikerOptions;
      workspaceSymbolProvider: WorkspaceSymbolOptions;
      workspace: Optional_workspace_Options;
      callHierarchyProvider: CallHierarchyOptions;
      --	experimental?: any;

      --  ALS-specific capabilities
      alsShowDepsProvider     : Optional_Boolean;
      alsReferenceKinds       : Optional_AlsReferenceKind_Set;
      alsCheckSyntaxProvider  : Optional_Boolean;
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
   --
   --	/**
   --	 * Information about the server.
   --	 *
   --	 * @since 3.15.0
   --	 */
   --	serverInfo?: {
   --		/**
   --		 * The name of the server as defined by the server.
   --		 */
   --		name: string;
   --
   --		/**
   --		 * The server's version as defined by the server.
   --		 */
   --		version?: string;
   --	};
   --}
   --```
   type InitializeResult is record
      capabilities: ServerCapabilities;
      serverInfo: Optional_ProgramInfo;
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
   --	 * If the protocol version provided by the client can't be handled by the
   --	 * server.
   --	 *
   --	 * @deprecated This initialize error got replaced by client capabilities.
   --	 * There is no version handshake in version 3.0x
   --	 */
   --	export const unknownProtocolVersion: 1 = 1;
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
   --
   --export type MessageType = 1 | 2 | 3 | 4;
   --```
   type MessageType is (Error, Warning, Info, Log);

   procedure Read_MessageType
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out MessageType);
   procedure Write_MessageType
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : MessageType);
   for MessageType'Read use Read_MessageType;
   for MessageType'Write use Write_MessageType;

   --```typescript
   --interface ShowMessageParams {
   --	/**
   --	 * The message type. See {@link MessageType}.
   --	 */
   --	type: MessageType;
   --
   --	/**
   --	 * The actual message.
   --	 */
   --	message: string;
   --}
   --```
   type ShowMessageParams is record
      a_type  : MessageType;  --  type: is reserver word
      message : VSS.Strings.Virtual_String;
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
   --	type: MessageType;
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
      a_type  : MessageType;  --  type: is reserver word
      message : VSS.Strings.Virtual_String;
      actions : MessageActionItem_Vector;
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
   --	type: MessageType;
   --
   --	/**
   --	 * The actual message
   --	 */
   --	message: string;
   --}
   --```
   type LogMessageParams is record
      a_type  : MessageType;  --  type: is reserver word
      message : VSS.Strings.Virtual_String;
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
   --/**
   -- * Describe options to be used when registering for text document change events.
   -- */
   --export interface TextDocumentChangeRegistrationOptions
   --	extends TextDocumentRegistrationOptions {
   --	/**
   --	 * How documents are synced to the server. See TextDocumentSyncKind.Full
   --	 * and TextDocumentSyncKind.Incremental.
   --	 */
   --	syncKind: TextDocumentSyncKind;
   --}
   --```
   type TextDocumentChangeRegistrationOptions is
     new TextDocumentRegistrationOptions with
   record
      syncKind: TextDocumentSyncKind;
   end record;

   procedure Read_TextDocumentChangeRegistrationOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out TextDocumentChangeRegistrationOptions);
   for TextDocumentChangeRegistrationOptions'Read use Read_TextDocumentChangeRegistrationOptions;
   procedure Write_TextDocumentChangeRegistrationOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : TextDocumentChangeRegistrationOptions);
   for TextDocumentChangeRegistrationOptions'Write use Write_TextDocumentChangeRegistrationOptions;

   --```typescript
   --export interface TextDocumentSaveRegistrationOptions
   --	extends TextDocumentRegistrationOptions {
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

   procedure Read_TextDocumentSaveRegistrationOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out TextDocumentSaveRegistrationOptions);
   for TextDocumentSaveRegistrationOptions'Read use Read_TextDocumentSaveRegistrationOptions;
   procedure Write_TextDocumentSaveRegistrationOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : TextDocumentSaveRegistrationOptions);
   for TextDocumentSaveRegistrationOptions'Write use Write_TextDocumentSaveRegistrationOptions;

   --```typescript
   --export interface CompletionRegistrationOptions
   --	extends TextDocumentRegistrationOptions, CompletionOptions {
   --}
   --```
   type CompletionRegistrationOptions is new TextDocumentRegistrationOptions with record
      triggerCharacters   : Optional_Virtual_String_Vector;
      allCommitCharacters : Optional_Virtual_String_Vector;
      resolveProvider     : Optional_Boolean;
   end record;

   procedure Read_CompletionRegistrationOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CompletionRegistrationOptions);
   for CompletionRegistrationOptions'Read use Read_CompletionRegistrationOptions;
   procedure Write_CompletionRegistrationOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CompletionRegistrationOptions);
   for CompletionRegistrationOptions'Write use Write_CompletionRegistrationOptions;

   --```typescript
   --export interface SignatureHelpRegistrationOptions
   --	extends TextDocumentRegistrationOptions, SignatureHelpOptions {
   --}
   --```
   type SignatureHelpRegistrationOptions is new TextDocumentRegistrationOptions with record
      triggerCharacters   : Optional_Virtual_String_Vector;
      retriggerCharacters : Optional_Virtual_String_Vector;
   end record;

   procedure Read_SignatureHelpRegistrationOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out SignatureHelpRegistrationOptions);
   for SignatureHelpRegistrationOptions'Read use Read_SignatureHelpRegistrationOptions;
   procedure Write_SignatureHelpRegistrationOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SignatureHelpRegistrationOptions);
   for SignatureHelpRegistrationOptions'Write use Write_SignatureHelpRegistrationOptions;

   --```typescript
   --export interface CodeLensRegistrationOptions extends
   --	TextDocumentRegistrationOptions, CodeLensOptions {
   --}
   --```
   type CodeLensRegistrationOptions is new TextDocumentRegistrationOptions with record
      resolveProvider: Optional_Boolean;
   end record;

   procedure Read_CodeLensRegistrationOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CodeLensRegistrationOptions);
   for CodeLensRegistrationOptions'Read use Read_CodeLensRegistrationOptions;
   procedure Write_CodeLensRegistrationOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CodeLensRegistrationOptions);
   for CodeLensRegistrationOptions'Write use Write_CodeLensRegistrationOptions;

   --```typescript
   --export interface DocumentLinkRegistrationOptions extends
   --	TextDocumentRegistrationOptions, DocumentLinkOptions {
   --}
   --```
   subtype DocumentLinkRegistrationOptions is CodeLensRegistrationOptions;

   --```typescript
   --export interface DocumentOnTypeFormattingRegistrationOptions extends
   --	TextDocumentRegistrationOptions, DocumentOnTypeFormattingOptions {
   --}
   --```
   type DocumentOnTypeFormattingRegistrationOptions is new TextDocumentRegistrationOptions with record
      firstTriggerCharacter : VSS.Strings.Virtual_String;
      moreTriggerCharacter  : Optional_Virtual_String_Vector;
   end record;

   procedure Read_DocumentOnTypeFormattingRegistrationOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DocumentOnTypeFormattingRegistrationOptions);
   for DocumentOnTypeFormattingRegistrationOptions'Read use Read_DocumentOnTypeFormattingRegistrationOptions;
   procedure Write_DocumentOnTypeFormattingRegistrationOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DocumentOnTypeFormattingRegistrationOptions);
   for DocumentOnTypeFormattingRegistrationOptions'Write use Write_DocumentOnTypeFormattingRegistrationOptions;

   --```typescript
   --/**
   -- * Execute command registration options.
   -- */
   --export interface ExecuteCommandRegistrationOptions
   --	extends ExecuteCommandOptions {
   --}
   --```
   type ExecuteCommandRegistrationOptions is record
      commands: VSS.String_Vectors.Virtual_String_Vector;
   end record;

   procedure Read_ExecuteCommandRegistrationOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ExecuteCommandRegistrationOptions);
   for ExecuteCommandRegistrationOptions'Read use Read_ExecuteCommandRegistrationOptions;
   procedure Write_ExecuteCommandRegistrationOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ExecuteCommandRegistrationOptions);
   for ExecuteCommandRegistrationOptions'Write use Write_ExecuteCommandRegistrationOptions;

   --```typescript
   --/**
   -- * Describe options to be used when registering for file system change events.
   -- */
   --export interface DidChangeWatchedFilesRegistrationOptions {
   --	/**
   --	 * The watchers to register.
   --	 */
   --	watchers: FileSystemWatcher[];
   --}
   --
   --export interface FileSystemWatcher {
   --	/**
   --	 * The  glob pattern to watch.
   --	 *
   --	 * Glob patterns can have the following syntax:
   --	 * - `*` to match one or more characters in a path segment
   --	 * - `?` to match on one character in a path segment
   --	 * - `**` to match any number of path segments, including none
   --	 * - `{}` to group sub patterns into an OR expression. (e.g. `**​/*.{ts,js}`
   --	 *   matches all TypeScript and JavaScript files)
   --	 * - `[]` to declare a range of characters to match in a path segment
   --	 *   (e.g., `example.[0-9]` to match on `example.0`, `example.1`, …)
   --	 * - `[!...]` to negate a range of characters to match in a path segment
   --	 *   (e.g., `example.[!0-9]` to match on `example.a`, `example.b`, but not
   --	 *   `example.0`)
   --	 */
   --	globPattern: string;
   --
   --	/**
   --	 * The kind of events of interest. If omitted it defaults
   --	 * to WatchKind.Create | WatchKind.Change | WatchKind.Delete
   --	 * which is 7.
   --	 */
   --	kind?: uinteger;
   --}
   --
   --export namespace WatchKind {
   --	/**
   --	 * Interested in create events.
   --	 */
   --	export const Create = 1;
   --
   --	/**
   --	 * Interested in change events
   --	 */
   --	export const Change = 2;
   --
   --	/**
   --	 * Interested in delete events
   --	 */
   --	export const Delete = 4;
   --}
   --```
   type WatchKind is (Create, Change, Delete);
   type WatchKind_Set is array (WatchKind) of Boolean
     with Default_Component_Value => True;

   procedure Read_WatchKind_Set
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out WatchKind_Set);
   procedure Write_WatchKind_Set
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : WatchKind_Set);
   for WatchKind_Set'Read use Read_WatchKind_Set;
   for WatchKind_Set'Write use Write_WatchKind_Set;

   Default_WatchKind_Set : constant WatchKind_Set := (WatchKind => True);

   type FileSystemWatcher is record
      globPattern : VSS.Strings.Virtual_String;
      kind        : WatchKind_Set;
   end record;

   procedure Read_FileSystemWatcher
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out FileSystemWatcher);
   procedure Write_FileSystemWatcher
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : FileSystemWatcher);
   for FileSystemWatcher'Read use Read_FileSystemWatcher;
   for FileSystemWatcher'Write use Write_FileSystemWatcher;

   package FileSystemWatcher_Vectors is new LSP.Generic_Vectors
     (FileSystemWatcher, Write_Empty => LSP.Write_Array);

   type FileSystemWatcher_Vector is new FileSystemWatcher_Vectors.Vector with null record;

   type DidChangeWatchedFilesRegistrationOptions is record
      watchers: FileSystemWatcher_Vector;
   end record;

   procedure Read_DidChangeWatchedFilesRegistrationOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DidChangeWatchedFilesRegistrationOptions);
   procedure Write_DidChangeWatchedFilesRegistrationOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DidChangeWatchedFilesRegistrationOptions);
   for DidChangeWatchedFilesRegistrationOptions'Read use Read_DidChangeWatchedFilesRegistrationOptions;
   for DidChangeWatchedFilesRegistrationOptions'Write use Write_DidChangeWatchedFilesRegistrationOptions;

   --```typescript
   --export interface CodeActionRegistrationOptions extends
   --	TextDocumentRegistrationOptions, CodeActionOptions {
   --}
   --```
   type CodeActionRegistrationOptions is
     new TextDocumentRegistrationOptions with
   record
      This : CodeActionOptions;
   end record;

   procedure Read_CodeActionRegistrationOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CodeActionRegistrationOptions);
   procedure Write_CodeActionRegistrationOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CodeActionRegistrationOptions);
   for CodeActionRegistrationOptions'Read use Read_CodeActionRegistrationOptions;
   for CodeActionRegistrationOptions'Write use Write_CodeActionRegistrationOptions;

   --```typescript
   --export interface RenameRegistrationOptions extends
   --	TextDocumentRegistrationOptions, RenameOptions {
   --}
   --```
   type RenameRegistrationOptions is new TextDocumentRegistrationOptions with record
      prepareProvider: Optional_Boolean;
   end record;

   procedure Read_RenameRegistrationOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out RenameRegistrationOptions);
   procedure Write_RenameRegistrationOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : RenameRegistrationOptions);
   for RenameRegistrationOptions'Read use Read_RenameRegistrationOptions;
   for RenameRegistrationOptions'Write use Write_RenameRegistrationOptions;

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
         when Did_Change_Watched_Files_Registration_Option =>
            DidChangeWatchedFiles : DidChangeWatchedFilesRegistrationOptions;
         when Code_Action_Registration_Option =>
            CodeAction : CodeActionRegistrationOptions;
         when Rename_Registration_Option =>
            Rename : RenameRegistrationOptions;
         when File_Operation_Registration_Option =>
            FileOperation : FileOperationRegistrationOptions;
      end case;
   end record;

   procedure Read_Registration_Option
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Registration_Option);
   procedure Write_Registration_Option
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Registration_Option);
   for Registration_Option'Read use Read_Registration_Option;
   for Registration_Option'Write use Write_Registration_Option;

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
      id              : VSS.Strings.Virtual_String;
      method          : VSS.Strings.Virtual_String;
      registerOptions : Registration_Option;
   end record;

   procedure Read_Registration
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Registration);
   procedure Write_Registration
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Registration);
   for Registration'Read use Read_Registration;
   for Registration'Write use Write_Registration;

   package Registration_Vectors is new LSP.Generic_Vectors
     (Registration, Write_Empty => LSP.Write_Array);

   type Registration_Vector is new Registration_Vectors.Vector with null record;

   type RegistrationParams is record
      registrations: Registration_Vector;
   end record;

   procedure Read_RegistrationParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out RegistrationParams);
   procedure Write_RegistrationParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : RegistrationParams);
   for RegistrationParams'Read use Read_RegistrationParams;
   for RegistrationParams'Write use Write_RegistrationParams;

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
   --	// This should correctly be named `unregistrations`. However changing this
   --	// is a breaking change and needs to wait until we deliver a 4.x version
   --	// of the specification.
   --	unregisterations: Unregistration[];
   --}
   --```
   type Unregistration is record
      id     : VSS.Strings.Virtual_String;
      method : VSS.Strings.Virtual_String;
   end record;

   procedure Read_Unregistration
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Unregistration);
   procedure Write_Unregistration
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Unregistration);
   for Unregistration'Read use Read_Unregistration;
   for Unregistration'Write use Write_Unregistration;

   package Unregistration_Vectors is new LSP.Generic_Vectors
     (Unregistration, Write_Empty => LSP.Write_Array);
   type Unregistration_Vector is new Unregistration_Vectors.Vector with null record;

   type UnregistrationParams is record
      unregisterations : Unregistration_Vector;
   end record;

   procedure Read_UnregistrationParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out UnregistrationParams);
   procedure Write_UnregistrationParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : UnregistrationParams);
   for UnregistrationParams'Read use Read_UnregistrationParams;
   for UnregistrationParams'Write use Write_UnregistrationParams;

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
   --	 * The actual content changes. The content changes describe single state
   --	 * changes to the document. So if there are two content changes c1 (at
   --	 * array index 0) and c2 (at array index 1) for a document in state S then
   --	 * c1 moves the document from S to S' and c2 from S' to S''. So c1 is
   --	 * computed on the state S and c2 is computed on the state S'.
   --	 *
   --	 * To mirror the content of a document using change events use the following
   --	 * approach:
   --	 * - start with the same initial content
   --	 * - apply the 'textDocument/didChange' notifications in the order you
   --	 *   receive them.
   --	 * - apply the `TextDocumentContentChangeEvent`s in a single notification
   --	 *   in the order you receive them.
   --	 */
   --	contentChanges: TextDocumentContentChangeEvent[];
   --}
   --
   --/**
   -- * An event describing a change to a text document. If range and rangeLength are
   -- * omitted the new text is considered to be the full content of the document.
   -- */
   --export type TextDocumentContentChangeEvent = {
   --	/**
   --	 * The range of the document that changed.
   --	 */
   --	range: Range;
   --
   --	/**
   --	 * The optional length of the range that got replaced.
   --	 *
   --	 * @deprecated use range instead.
   --	 */
   --	rangeLength?: uinteger;
   --
   --	/**
   --	 * The new text for the provided range.
   --	 */
   --	text: string;
   --} | {
   --	/**
   --	 * The new text of the whole document.
   --	 */
   --	text: string;
   --}
   --```
   type TextDocumentContentChangeEvent is record
      span        : Optional_Span;
      rangeLength : LSP.Types.Optional_Number;
      text        : VSS.Strings.Virtual_String;
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
     (TextDocumentContentChangeEvent, Write_Empty => LSP.Write_Array);

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
   --	reason: TextDocumentSaveReason;
   --}
   --
   --/**
   -- * Represents reasons why a text document is saved.
   -- */
   --export namespace TextDocumentSaveReason {
   --
   --	/**
   --	 * Manually triggered, e.g. by the user pressing save, by starting
   --	 * debugging, or by an API call.
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
   --
   --export type TextDocumentSaveReason = 1 | 2 | 3;
   --```
   type TextDocumentSaveReason is (Manual, AfterDelay, FocusOut);

   procedure Read_TextDocumentSaveReason
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out TextDocumentSaveReason);
   procedure Write_TextDocumentSaveReason
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : TextDocumentSaveReason);
   for TextDocumentSaveReason'Read use Read_TextDocumentSaveReason;
   for TextDocumentSaveReason'Write use Write_TextDocumentSaveReason;

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
      textDocument : TextDocumentIdentifier;
      text         : Optional_Virtual_String;
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
   --	type: uinteger;
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

   procedure Read_FileChangeType
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out FileChangeType);
   procedure Write_FileChangeType
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : FileChangeType);
   for FileChangeType'Read use Read_FileChangeType;
   for FileChangeType'Write use Write_FileChangeType;

   type FileEvent is record
      uri: DocumentUri;
      a_type : FileChangeType;  -- type: is reserver word
   end record;

   procedure Read_FileEvent
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out FileEvent);
   procedure Write_FileEvent
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : FileEvent);
   for FileEvent'Read use Read_FileEvent;
   for FileEvent'Write use Write_FileEvent;

   package FileEvent_Vectors is new LSP.Generic_Vectors
     (FileEvent, Write_Empty => LSP.Write_Array);

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

   procedure Read_DidChangeWatchedFilesParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DidChangeWatchedFilesParams);
   procedure Write_DidChangeWatchedFilesParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DidChangeWatchedFilesParams);
   for DidChangeWatchedFilesParams'Read use Read_DidChangeWatchedFilesParams;
   for DidChangeWatchedFilesParams'Write use Write_DidChangeWatchedFilesParams;

   --```typescript
   --interface PublishDiagnosticsParams {
   --	/**
   --	 * The URI for which diagnostic information is reported.
   --	 */
   --	uri: DocumentUri;
   --
   --	/**
   --	 * Optional the version number of the document the diagnostics are published
   --	 * for.
   --	 *
   --	 * @since 3.15.0
   --	 */
   --	version?: uinteger;
   --
   --	/**
   --	 * An array of diagnostic information items.
   --	 */
   --	diagnostics: Diagnostic[];
   --}
   --```
   type PublishDiagnosticsParams is record
      uri: DocumentUri;
      version: Optional_Number;
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
   -- * Represents a collection of [completion items](#CompletionItem) to be
   -- * presented in the editor.
   -- */
   --export interface CompletionList {
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
   --export namespace InsertTextFormat {
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
   --export type InsertTextFormat = 1 | 2;
   --
   --/**
   -- * Completion item tags are extra annotations that tweak the rendering of a
   -- * completion item.
   -- *
   -- * @since 3.15.0
   -- */
   --export namespace CompletionItemTag {
   --	/**
   --	 * Render a completion as obsolete, usually using a strike-out.
   --	 */
   --	export const Deprecated = 1;
   --}
   --
   --export type CompletionItemTag = 1;
   --
   --/**
   -- * A special text edit to provide an insert and a replace operation.
   -- *
   -- * @since 3.16.0
   -- */
   --export interface InsertReplaceEdit {
   --	/**
   --	 * The string to be inserted.
   --	 */
   --	newText: string;
   --
   --	/**
   --	 * The range if the insert is requested
   --	 */
   --	insert: Range;
   --
   --	/**
   --	 * The range if the replace is requested.
   --	 */
   --	replace: Range;
   --}
   --
   --/**
   -- * How whitespace and indentation is handled during completion
   -- * item insertion.
   -- *
   -- * @since 3.16.0
   -- */
   --export namespace InsertTextMode {
   --	/**
   --	 * The insertion or replace strings is taken as it is. If the
   --	 * value is multi line the lines below the cursor will be
   --	 * inserted using the indentation defined in the string value.
   --	 * The client will not apply any kind of adjustments to the
   --	 * string.
   --	 */
   --	export const asIs: 1 = 1;
   --
   --	/**
   --	 * The editor adjusts leading whitespace of new lines so that
   --	 * they match the indentation up to the cursor of the line for
   --	 * which the item is accepted.
   --	 *
   --	 * Consider a line like this: <2tabs><cursor><3tabs>foo. Accepting a
   --	 * multi line completion item is indented using 2 tabs and all
   --	 * following lines inserted will be indented using 2 tabs as well.
   --	 */
   --	export const adjustIndentation: 2 = 2;
   --}
   --
   --export type InsertTextMode = 1 | 2;
   --
   --export interface CompletionItem {
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
   --	kind?: CompletionItemKind;
   --
   --	/**
   --	 * Tags for this completion item.
   --	 *
   --	 * @since 3.15.0
   --	 */
   --	tags?: CompletionItemTag[];
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
   --	 *
   --	 * @deprecated Use `tags` instead if supported.
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
   --	 * VS Code when code complete is requested in this example
   --	 * `con<cursor position>` and a completion item with an `insertText` of
   --	 * `console` is provided it will only insert `sole`. Therefore it is
   --	 * recommended to use `textEdit` instead since it avoids additional client
   --	 * side interpretation.
   --	 */
   --	insertText?: string;
   --
   --	/**
   --	 * The format of the insert text. The format applies to both the
   --	 * `insertText` property and the `newText` property of a provided
   --	 * `textEdit`. If omitted defaults to `InsertTextFormat.PlainText`.
   --	 */
   --	insertTextFormat?: InsertTextFormat;
   --
   --	/**
   --	 * How whitespace and indentation is handled during completion
   --	 * item insertion. If not provided the client's default value depends on
   --	 * the `textDocument.completion.insertTextMode` client capability.
   --	 *
   --	 * @since 3.16.0
   --	 */
   --	insertTextMode?: InsertTextMode;
   --
   --	/**
   --	 * An edit which is applied to a document when selecting this completion.
   --	 * When an edit is provided the value of `insertText` is ignored.
   --	 *
   --	 * *Note:* The range of the edit must be a single line range and it must
   --	 * contain the position at which completion has been requested.
   --	 *
   --	 * Most editors support two different operations when accepting a completion
   --	 * item. One is to insert a completion text and the other is to replace an
   --	 * existing text with a completion text. Since this can usually not be
   --	 * predetermined by a server it can report both ranges. Clients need to
   --	 * signal support for `InsertReplaceEdits` via the
   --	 * `textDocument.completion.insertReplaceSupport` client capability
   --	 * property.
   --	 *
   --	 * *Note 1:* The text edit's range as well as both ranges from an insert
   --	 * replace edit must be a [single line] and they must contain the position
   --	 * at which completion has been requested.
   --	 * *Note 2:* If an `InsertReplaceEdit` is returned the edit's insert range
   --	 * must be a prefix of the edit's replace range, that means it must be
   --	 * contained and starting at the same position.
   --	 *
   --	 * @since 3.16.0 additional type `InsertReplaceEdit`
   --	 */
   --	textEdit?: TextEdit | InsertReplaceEdit;
   --
   --	/**
   --	 * An optional array of additional text edits that are applied when
   --	 * selecting this completion. Edits must not overlap (including the same
   --	 * insert position) with the main edit nor with themselves.
   --	 *
   --	 * Additional text edits should be used to change text unrelated to the
   --	 * current cursor position (for example adding an import statement at the
   --	 * top of the file if the completion item will insert an unqualified type).
   --	 */
   --	additionalTextEdits?: TextEdit[];
   --
   --	/**
   --	 * An optional set of characters that when pressed while this completion is
   --	 * active will accept it first and then type that character. *Note* that all
   --	 * commit characters should have `length=1` and that superfluous characters
   --	 * will be ignored.
   --	 */
   --	commitCharacters?: string[];
   --
   --	/**
   --	 * An optional command that is executed *after* inserting this completion.
   --	 * *Note* that additional modifications to the current document should be
   --	 * described with the additionalTextEdits-property.
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
   --export namespace CompletionItemKind {
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

   type InsertReplaceEdit is record
     newText : VSS.Strings.Virtual_String;
     insert  : Span;
     replace : Span;
   end record;

   procedure Read_InsertReplaceEdit
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out InsertReplaceEdit);
   procedure Write_InsertReplaceEdit
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : InsertReplaceEdit);
   for InsertReplaceEdit'Read use Read_InsertReplaceEdit;
   for InsertReplaceEdit'Write use Write_InsertReplaceEdit;

   type TextEdit_Or_InsertReplaceEdit (Is_TextEdit : Boolean := True) is record
      case Is_TextEdit is
         when True =>
            TextEdit : LSP.Messages.TextEdit;
         when False =>
            InsertReplaceEdit : LSP.Messages.InsertReplaceEdit;
      end case;
   end record;

   procedure Read_TextEdit_Or_InsertReplaceEdit
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out TextEdit_Or_InsertReplaceEdit);
   procedure Write_TextEdit_Or_InsertReplaceEdit
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : TextEdit_Or_InsertReplaceEdit);
   for TextEdit_Or_InsertReplaceEdit'Read
     use Read_TextEdit_Or_InsertReplaceEdit;
   for TextEdit_Or_InsertReplaceEdit'Write
     use Write_TextEdit_Or_InsertReplaceEdit;

   package Optional_TextEdit_Or_InsertReplaceEdits is
     new LSP.Generic_Optional (TextEdit_Or_InsertReplaceEdit);
   type Optional_TextEdit_Or_InsertReplaceEdit is
     new Optional_TextEdit_Or_InsertReplaceEdits.Optional_Type;

   type CompletionItem is record
      label               : VSS.Strings.Virtual_String;
      kind                : Optional_CompletionItemKind;
      tags                : Optional_CompletionItemTagSet;
      detail              : Optional_Virtual_String;
      documentation       : Optional_String_Or_MarkupContent;
      deprecated          : Optional_Boolean;
      preselect           : Optional_Boolean;
      sortText            : Optional_Virtual_String;
      filterText          : Optional_Virtual_String;
      insertText          : Optional_Virtual_String;
      insertTextFormat    : Optional_InsertTextFormat;
      insertTextMode      : Optional_InsertTextMode;
      textEdit            : Optional_TextEdit_Or_InsertReplaceEdit;
      additionalTextEdits : TextEdit_Vector;
      commitCharacters    : Optional_Virtual_String_Vector;
      command             : Optional_Command;
      data                : Optional_Location;
   end record;

   procedure Read_CompletionItem
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CompletionItem);
   procedure Write_CompletionItem
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CompletionItem);
   for CompletionItem'Read use Read_CompletionItem;
   for CompletionItem'Write use Write_CompletionItem;

   package CompletionItem_Vectors is new LSP.Generic_Vectors
     (CompletionItem, Write_Empty => LSP.Write_Array);

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
   -- * MarkedString can be used to render human readable text. It is either a
   -- * markdown string or a code-block that provides a language and a code snippet.
   -- * The language identifier is semantically equal to the optional language
   -- * identifier in fenced code blocks in GitHub issues.
   -- *
   -- * The pair of a language and a value is an equivalent to markdown:
   -- * ```${language}
   -- * ${value}
   -- * ```
   -- *
   -- * Note that markdown strings will be sanitized - that means html will be
   -- * escaped.
   -- *
   -- * @deprecated use MarkupContent instead.
   -- */
   --type MarkedString = string | { language: string; value: string };
   --```
   type MarkedString (Is_String : Boolean := True) is record
      value : VSS.Strings.Virtual_String;

      case Is_String is
         when True =>
            null;
         when False =>
            language : VSS.Strings.Virtual_String;
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

   package MarkedString_Vectors is new LSP.Generic_Vectors
     (MarkedString, Write_Empty => LSP.Write_Array);

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
   --export interface Hover {
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

   package Optional_Hovers is
     new LSP.Generic_Optional (Hover, Write_Unset_As_Null => True);
   type Optional_Hover is new Optional_Hovers.Optional_Type;

   --```typescript
   --/**
   -- * Signature help represents the signature of something
   -- * callable. There can be multiple signature but only one
   -- * active and only one active parameter.
   -- */
   --export interface SignatureHelp {
   --	/**
   --	 * One or more signatures. If no signatures are available the signature help
   --	 * request should return `null`.
   --	 */
   --	signatures: SignatureInformation[];
   --
   --	/**
   --	 * The active signature. If omitted or the value lies outside the
   --	 * range of `signatures` the value defaults to zero or is ignored if
   --	 * the `SignatureHelp` has no signatures.
   --	 *
   --	 * Whenever possible implementors should make an active decision about
   --	 * the active signature and shouldn't rely on a default value.
   --	 *
   --	 * In future version of the protocol this property might become
   --	 * mandatory to better express this.
   --	 */
   --	activeSignature?: uinteger;
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
   --	activeParameter?: uinteger;
   --}
   --
   --/**
   -- * Represents the signature of something callable. A signature
   -- * can have a label, like a function-name, a doc-comment, and
   -- * a set of parameters.
   -- */
   --export interface SignatureInformation {
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
   --
   --	/**
   --	 * The index of the active parameter.
   --	 *
   --	 * If provided, this is used in place of `SignatureHelp.activeParameter`.
   --	 *
   --	 * @since 3.16.0
   --	 */
   --	activeParameter?: uinteger;
   --}
   --
   --/**
   -- * Represents a parameter of a callable-signature. A parameter can
   -- * have a label and a doc-comment.
   -- */
   --export interface ParameterInformation {
   --
   --	/**
   --	 * The label of this parameter information.
   --	 *
   --	 * Either a string or an inclusive start and exclusive end offsets within
   --	 * its containing signature label. (see SignatureInformation.label). The
   --	 * offsets are based on a UTF-16 string representation as `Position` and
   --	 * `Range` does.
   --	 *
   --	 * *Note*: a label of type string should be a substring of its containing
   --	 * signature label. Its intended use case is to highlight the parameter
   --	 * label part in the `SignatureInformation.label`.
   --	 */
   --	label: string | [uinteger, uinteger];
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
            String : VSS.Strings.Virtual_String;
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
     (ParameterInformation, Write_Empty => LSP.Skip);
   type ParameterInformation_Vector is
     new ParameterInformation_Vectors.Vector with null record;

   type SignatureInformation is record
      label           : VSS.Strings.Virtual_String;
      documentation   : Optional_String_Or_MarkupContent;
      parameters      : ParameterInformation_Vector;
      activeParameter : Optional_uinteger;
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
     (SignatureInformation, Write_Empty => LSP.Write_Array);
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

   package Optional_SignatureHelps is
     new LSP.Generic_Optional (SignatureHelp);
   type Optional_SignatureHelp is new
     Optional_SignatureHelps.Optional_Type;

   --```typescript
   --export interface ReferenceParams extends TextDocumentPositionParams,
   --	WorkDoneProgressParams, PartialResultParams {
   --	context: ReferenceContext
   --}
   --
   --export interface ReferenceContext {
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

   type ReferenceParams is new Text_Progress_Partial_Params with record
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
   --export interface DocumentHighlight {
   --	/**
   --	 * The range this highlight applies to.
   --	 */
   --	range: Range;
   --
   --	/**
   --	 * The highlight kind, default is DocumentHighlightKind.Text.
   --	 */
   --	kind?: DocumentHighlightKind;
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
   --
   --export type DocumentHighlightKind = 1 | 2 | 3;
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
      kind: Optional_DocumentHighlightKind;
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
     (DocumentHighlight, Write_Empty => LSP.Write_Array);

   type DocumentHighlight_Vector is
     new DocumentHighlight_Vectors.Vector with null record;

   -- Search_Kind --
   type Search_Kind is
     (Full_Text, Regexp, Fuzzy, Approximate, Start_Word_Text);
   --  A Full_Text match searches the pattern exactly in the contents.
   --
   --  A Start_Word_Text works like a Full_Text but tested word should mutch
   --  patters from the first letter
   --
   --  A regexp parses the pattern as a regular expression.
   --
   --  A fuzzy match will search for some contents that contains all the
   --  characters of the pattern, in the same order, but possibly with
   --  other characters in-between. The number of characters in-between is not
   --  limited, so this mode really only makes sense when matching short text
   --  (and not, for instance, in text editors).
   --
   --  Approximate allows one or two errors to appear in the match (character
   --  insertion, deletion or substitution). This is mostly suitable when
   --  matching in long texts. The implementation of this algorithm is
   --  optimized so that characters are matched only once, but the total length
   --  of the pattern is limited to 64 characters. The exact number of errors
   --  depends on the length of the pattern:
   --      patterns of length <= 4  => no error allowed
   --      patterns of length <= 10 => one error allowed
   --      long patterns            => up to two errors

   procedure Read_Search_Kind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Search_Kind);
   procedure Write_Search_Kind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Search_Kind);
   for Search_Kind'Read use Read_Search_Kind;
   for Search_Kind'Write use Write_Search_Kind;

   package Optional_Search_Kinds is new LSP.Generic_Optional (Search_Kind);
   type Optional_Search_Kind is new Optional_Search_Kinds.Optional_Type;

   --```typescript
   --export interface DocumentSymbolParams extends WorkDoneProgressParams,
   --	PartialResultParams {
   --	/**
   --	 * The text document.
   --	 */
   --	textDocument: TextDocumentIdentifier;
   --}
   --```
   type DocumentSymbolParams is new Progress_Partial_Params with record
      textDocument   : TextDocumentIdentifier;
      query          : VSS.Strings.Virtual_String;
      case_sensitive : Optional_Boolean;
      whole_word     : Optional_Boolean;
      negate         : Optional_Boolean;
      kind           : Optional_Search_Kind;
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
   -- * Symbol tags are extra annotations that tweak the rendering of a symbol.
   -- *
   -- * @since 3.16.0
   -- */
   --export namespace SymbolTag {
   --
   --	/**
   --	 * Render a symbol as obsolete, usually using a strike-out.
   --	 */
   --	export const Deprecated: 1 = 1;
   --}
   --
   --export type SymbolTag = 1;
   --
   --
   --/**
   -- * Represents programming constructs like variables, classes, interfaces etc.
   -- * that appear in a document. Document symbols can be hierarchical and they
   -- * have two ranges: one that encloses its definition and one that points to its
   -- * most interesting range, e.g. the range of an identifier.
   -- */
   --export interface DocumentSymbol {
   --
   --	/**
   --	 * The name of this symbol. Will be displayed in the user interface and
   --	 * therefore must not be an empty string or a string only consisting of
   --	 * white spaces.
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
   --	 * Tags for this document symbol.
   --	 *
   --	 * @since 3.16.0
   --	 */
   --	tags?: SymbolTag[];
   --
   --	/**
   --	 * Indicates if this symbol is deprecated.
   --	 *
   --	 * @deprecated Use tags instead
   --	 */
   --	deprecated?: boolean;
   --
   --	/**
   --	 * The range enclosing this symbol not including leading/trailing whitespace
   --	 * but everything else like comments. This information is typically used to
   --	 * determine if the clients cursor is inside the symbol to reveal in the
   --	 * symbol in the UI.
   --	 */
   --	range: Range;
   --
   --	/**
   --	 * The range that should be selected and revealed when this symbol is being
   --	 * picked, e.g. the name of a function. Must be contained by the `range`.
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
   --export interface SymbolInformation {
   --	/**
   --	 * The name of this symbol.
   --	 */
   --	name: string;
   --
   --	/**
   --	 * The kind of this symbol.
   --	 */
   --	kind: SymbolKind;
   --
   --	/**
   --	 * Tags for this completion item.
   --	 *
   --	 * @since 3.16.0
   --	 */
   --	tags?: SymbolTag[];
   --
   --	/**
   --	 * Indicates if this symbol is deprecated.
   --	 *
   --	 * @deprecated Use tags instead
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
   --```
   type DocumentSymbol is record
      name              : VSS.Strings.Virtual_String;
      detail            : Optional_Virtual_String;
      kind              : SymbolKind;
      tags              : SymbolTagSet;
      deprecated        : Optional_Boolean;
      span              : LSP.Messages.Span;
      selectionRange    : LSP.Messages.Span;
      alsIsDeclaration  : Optional_Boolean;
      alsIsAdaProcedure : Optional_Boolean;
      alsVisibility     : Optional_Als_Visibility;
      children          : Boolean;  --  True if emit children in JSON
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
      name              : VSS.Strings.Virtual_String;
      kind              : SymbolKind;
      alsIsAdaProcedure : Optional_Boolean;
      tags              : SymbolTagSet;
      deprecated        : Optional_Boolean;
      location          : LSP.Messages.Location;
      containerName     : Optional_Virtual_String;
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
     (SymbolInformation, Write_Empty => LSP.Write_Array);

   type SymbolInformation_Vector is
     new SymbolInformation_Vectors.Vector with null record;

   package ProgressParam_SymbolInformation_Vectors is
     new Generic_ProgressParam (SymbolInformation_Vector);
   type Progress_SymbolInformation_Vector is new
     ProgressParam_SymbolInformation_Vectors.ProgressParam;

   procedure Read_Progress_SymbolInformation_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Progress_SymbolInformation_Vector);
   for Progress_SymbolInformation_Vector'Read use
     Read_Progress_SymbolInformation_Vector;

   procedure Write_Progress_SymbolInformation_Vector
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Progress_SymbolInformation_Vector);
   for Progress_SymbolInformation_Vector'Write use
     Write_Progress_SymbolInformation_Vector;

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
   --interface WorkspaceSymbolParams extends WorkDoneProgressParams,
   --	PartialResultParams {
   --	/**
   --	 * A query string to filter symbols by. Clients may send an empty
   --	 * string here to request all symbols.
   --	 */
   --	query: string;
   --}
   --```

   type WorkspaceSymbolParams is new Progress_Partial_Params with record
      query          : VSS.Strings.Virtual_String;
      case_sensitive : Optional_Boolean;
      whole_word     : Optional_Boolean;
      negate         : Optional_Boolean;
      kind           : Optional_Search_Kind;
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
   --export interface CodeActionParams extends WorkDoneProgressParams,
   --	PartialResultParams {
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
   -- * Kinds are a hierarchical list of identifiers separated by `.`,
   -- * e.g. `"refactor.extract.function"`.
   -- *
   -- * The set of kinds is open and client needs to announce the kinds it supports
   -- * to the server during initialization.
   -- */
   --export type CodeActionKind = string;
   --
   --/**
   -- * A set of predefined code action kinds.
   -- */
   --export namespace CodeActionKind {
   --
   --	/**
   --	 * Empty kind.
   --	 */
   --	export const Empty: CodeActionKind = '';
   --
   --	/**
   --	 * Base kind for quickfix actions: 'quickfix'.
   --	 */
   --	export const QuickFix: CodeActionKind = 'quickfix';
   --
   --	/**
   --	 * Base kind for refactoring actions: 'refactor'.
   --	 */
   --	export const Refactor: CodeActionKind = 'refactor';
   --
   --	/**
   --	 * Base kind for refactoring extraction actions: 'refactor.extract'.
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
   --	 * Base kind for refactoring inline actions: 'refactor.inline'.
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
   --	 * Base kind for refactoring rewrite actions: 'refactor.rewrite'.
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
   --	 * Base kind for source actions: `source`.
   --	 *
   --	 * Source code actions apply to the entire file.
   --	 */
   --	export const Source: CodeActionKind = 'source';
   --
   --	/**
   --	 * Base kind for an organize imports source action:
   --	 * `source.organizeImports`.
   --	 */
   --	export const SourceOrganizeImports: CodeActionKind =
   --		'source.organizeImports';
   --}
   --
   --/**
   -- * Contains additional diagnostic information about the context in which
   -- * a code action is run.
   -- */
   --export interface CodeActionContext {
   --	/**
   --	 * An array of diagnostics known on the client side overlapping the range
   --	 * provided to the `textDocument/codeAction` request. They are provided so
   --	 * that the server knows which errors are currently presented to the user
   --	 * for the given range. There is no guarantee that these accurately reflect
   --	 * the error state of the resource. The primary parameter
   --	 * to compute code actions is the provided range.
   --	 */
   --	diagnostics: Diagnostic[];
   --
   --	/**
   --	 * Requested kind of actions to return.
   --	 *
   --	 * Actions not of this kind are filtered out by the client before being
   --	 * shown. So servers can omit computing them.
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

   type CodeActionParams is new Progress_Partial_Params with record
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
   --interface CodeLensParams extends WorkDoneProgressParams, PartialResultParams {
   --	/**
   --	 * The document to request code lens for.
   --	 */
   --	textDocument: TextDocumentIdentifier;
   --}
   --```
   type CodeLensParams is new Progress_Partial_Params with record
      textDocument: TextDocumentIdentifier;
   end record;

   --```typescript
   --/**
   -- * A code lens represents a command that should be shown along with
   -- * source text, like the number of references, a way to run tests, etc.
   -- *
   -- * A code lens is _unresolved_ when no command is associated to it. For
   -- * performance reasons the creation of a code lens and resolving should be done
   -- * in two stages.
   -- */
   --interface CodeLens {
   --	/**
   --	 * The range in which this code lens is valid. Should only span a single
   --	 * line.
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
      command: Optional_Command;
      --  data?: any
   end record;

   --```typescript
   --interface DocumentLinkParams extends WorkDoneProgressParams,
   --	PartialResultParams {
   --	/**
   --	 * The document to provide document links for.
   --	 */
   --	textDocument: TextDocumentIdentifier;
   --}
   --```
   type DocumentLinkParams is new Progress_Partial_Params with record
      textDocument: TextDocumentIdentifier;
   end record;

   --```typescript
   --/**
   -- * A document link is a range in a text document that links to an internal or
   -- * external resource, like another text document or a web site.
   -- */
   --interface DocumentLink {
   --	/**
   --	 * The range this link applies to.
   --	 */
   --	range: Range;
   --
   --	/**
   --	 * The uri this link points to. If missing a resolve request is sent later.
   --	 */
   --	target?: DocumentUri;
   --
   --	/**
   --	 * The tooltip text when you hover over this link.
   --	 *
   --	 * If a tooltip is provided, is will be displayed in a string that includes
   --	 * instructions on how to trigger the link, such as `{0} (ctrl + click)`.
   --	 * The specific instructions vary depending on OS, user settings, and
   --	 * localization.
   --	 *
   --	 * @since 3.15.0
   --	 */
   --	tooltip?: string;
   --
   --	/**
   --	 * A data entry field that is preserved on a document link between a
   --	 * DocumentLinkRequest and a DocumentLinkResolveRequest.
   --	 */
   --	data?: any;
   --}
   --```
   type DocumentLink is record
      span    : LSP.Messages.Span;
      target  : Optional_Virtual_String;
      tooltip : Optional_Virtual_String;
      --  data?: any
   end record;

   package DocumentLink_Vectors is new LSP.Generic_Vectors
     (DocumentLink, Write_Empty => LSP.Write_Array);

   type DocumentLink_Vector is new DocumentLink_Vectors.Vector with null record;

   --```typescript
   --interface DocumentFormattingParams extends WorkDoneProgressParams {
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
   --	tabSize: uinteger;
   --
   --	/**
   --	 * Prefer spaces over tabs.
   --	 */
   --	insertSpaces: boolean;
   --
   --	/**
   --	 * Trim trailing whitespace on a line.
   --	 *
   --	 * @since 3.15.0
   --	 */
   --	trimTrailingWhitespace?: boolean;
   --
   --	/**
   --	 * Insert a newline character at the end of the file if one does not exist.
   --	 *
   --	 * @since 3.15.0
   --	 */
   --	insertFinalNewline?: boolean;
   --
   --	/**
   --	 * Trim all newlines after the final newline at the end of the file.
   --	 *
   --	 * @since 3.15.0
   --	 */
   --	trimFinalNewlines?: boolean;
   --
   --	/**
   --	 * Signature for further properties.
   --	 */
   --	[key: string]: boolean | integer | string;
   --}
   --```
   type FormattingOptions is record
      tabSize: LSP_Number := 3;  --  Safety net defaults
      insertSpaces: Boolean := True;
      trimTrailingWhitespace: Optional_Boolean;
      insertFinalNewline: Optional_Boolean;
      trimFinalNewlines: Optional_Boolean;
      --  [key: string]: boolean | number | string; ???
   end record;
   procedure Read_FormattingOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out FormattingOptions);
   procedure Write_FormattingOptions
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : FormattingOptions);
   for FormattingOptions'Read use Read_FormattingOptions;
   for FormattingOptions'Write use Write_FormattingOptions;

   type DocumentFormattingParams is new WorkDoneProgressParams with record
      textDocument: TextDocumentIdentifier;
      options: FormattingOptions;
   end record;
   procedure Read_DocumentFormattingParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DocumentFormattingParams);
   procedure Write_DocumentFormattingParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DocumentFormattingParams);
   for DocumentFormattingParams'Read use
     Read_DocumentFormattingParams;
   for DocumentFormattingParams'Write use
     Write_DocumentFormattingParams;

   --```typescript
   --interface DocumentRangeFormattingParams extends WorkDoneProgressParams {
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
   type DocumentRangeFormattingParams is new WorkDoneProgressParams with record
      textDocument: TextDocumentIdentifier;
      span: LSP.Messages.Span;
      options: FormattingOptions;
   end record;
   procedure Read_DocumentRangeFormattingParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DocumentRangeFormattingParams);
   procedure Write_DocumentRangeFormattingParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DocumentRangeFormattingParams);
   for DocumentRangeFormattingParams'Read use
     Read_DocumentRangeFormattingParams;
   for DocumentRangeFormattingParams'Write use
     Write_DocumentRangeFormattingParams;

   --```typescript
   --interface DocumentOnTypeFormattingParams extends TextDocumentPositionParams {
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
   type DocumentOnTypeFormattingParams is new TextDocumentPositionParams with record
      ch      : VSS.Strings.Virtual_String;
      options : FormattingOptions;
   end record;
   procedure Read_DocumentOnTypeFormattingParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DocumentOnTypeFormattingParams);
   procedure Write_DocumentOnTypeFormattingParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DocumentOnTypeFormattingParams);
   for DocumentOnTypeFormattingParams'Read use
     Read_DocumentOnTypeFormattingParams;
   for DocumentOnTypeFormattingParams'Write use
     Write_DocumentOnTypeFormattingParams;

   --```typescript
   --interface RenameParams extends TextDocumentPositionParams,
   --	WorkDoneProgressParams {
   --	/**
   --	 * The new name of the symbol. If the given name is not valid the
   --	 * request must return a [ResponseError](#ResponseError) with an
   --	 * appropriate message set.
   --	 */
   --	newName: string;
   --}
   --```
   type RenameParams is new Text_Progress_Params with record
      newName: VSS.Strings.Virtual_String;
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
   --export interface ExecuteCommandParams extends WorkDoneProgressParams {
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
   type ExecuteCommandParams (Is_Unknown : Boolean := True) is record
      Base    : WorkDoneProgressParams;
      command : VSS.Strings.Virtual_String;

      case Is_Unknown is
         when True =>
            arguments : Optional_Any_Vector;
         when False =>
            Custom    : LSP.Commands.Command_Pointer;
      end case;
   end record;
   --  See Command for Is_Unknown documentation

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
      label : Optional_Virtual_String;
      edit  : WorkspaceEdit;
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
   --	 * This may be used by the server for diagnostic logging or to provide
   --	 * a suitable error for a request that triggered the edit.
   --	 */
   --	failureReason?: string;
   --
   --	/**
   --	 * Depending on the client's failure handling strategy `failedChange`
   --	 * might contain the index of the change that failed. This property is
   --	 * only available if the client signals a `failureHandlingStrategy`
   --	 * in its client capabilities.
   --	 */
   --	failedChange?: uinteger;
   --}
   --```
   type ApplyWorkspaceEditResult is record
      applied       : Boolean;
      failureReason : Optional_Virtual_String;
      failedChange  : Optional_uinteger;
   end record;

   procedure Read_ApplyWorkspaceEditResult
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ApplyWorkspaceEditResult);
   procedure Write_ApplyWorkspaceEditResult
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ApplyWorkspaceEditResult);
   for ApplyWorkspaceEditResult'Read use Read_ApplyWorkspaceEditResult;
   for ApplyWorkspaceEditResult'Write use Write_ApplyWorkspaceEditResult;

   --```typescript
   --export interface WorkDoneProgressBegin {
   --
   --	kind: 'begin';
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
   --	 * long running operation. Clients that don't support cancellation are
   --	 * allowed to ignore the setting.
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
   --	 * that are not following this rule. The value range is [0, 100]
   --	 */
   --	percentage?: uinteger;
   --}
   --```
   type WorkDoneProgressBegin is record
      kind        : VSS.Strings.Virtual_String := "begin";
      title       : VSS.Strings.Virtual_String;
      cancellable : Optional_Boolean;
      message     : Optional_Virtual_String;
      percentage  : Optional_Number;
   end record;

   --```typescript
   --export interface WorkDoneProgressReport {
   --
   --	kind: 'report';
   --
   --	/**
   --	 * Controls enablement state of a cancel button. This property is only valid
   --	 *  if a cancel button got requested in the `WorkDoneProgressStart` payload.
   --	 *
   --	 * Clients that don't support cancellation or don't support control the
   --	 * button's enablement state are allowed to ignore the setting.
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
   --	 * that are not following this rule. The value range is [0, 100]
   --	 */
   --	percentage?: uinteger;
   --}
   --```
   type WorkDoneProgressReport is record
      kind        : VSS.Strings.Virtual_String := "report";
      cancellable : Optional_Boolean;
      message     : Optional_Virtual_String;
      percentage  : Optional_Number;
   end record;

   --```typescript
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
   --```
   type WorkDoneProgressEnd is record
      kind    : VSS.Strings.Virtual_String := "end";
      message : Optional_Virtual_String;
   end record;

   --  Reads/Writers
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

   procedure Read_WorkDoneProgressBegin
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out WorkDoneProgressBegin);
   for WorkDoneProgressBegin'Read use Read_WorkDoneProgressBegin;
   procedure Read_WorkDoneProgressReport
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out WorkDoneProgressReport);
   for WorkDoneProgressReport'Read use Read_WorkDoneProgressReport;
   procedure Read_WorkDoneProgressEnd
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out WorkDoneProgressEnd);
   for WorkDoneProgressEnd'Read use Read_WorkDoneProgressEnd;

   --  Pre-instantiate the use cases for the ProgressParams

   package ProgressParam_Begin_Package is new Generic_ProgressParam
     (WorkDoneProgressBegin);
   subtype Progress_Begin_Params is ProgressParam_Begin_Package.ProgressParam;

   package ProgressParam_Report_Package is new Generic_ProgressParam
     (WorkDoneProgressReport);
   subtype Progress_Report_Params is ProgressParam_Report_Package.ProgressParam;

   package ProgressParam_End_Package is new Generic_ProgressParam
     (WorkDoneProgressEnd);
   subtype Progress_End_Params is ProgressParam_End_Package.ProgressParam;

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

   --```typescript
   --export interface DidChangeWorkspaceFoldersParams {
   --	/**
   --	 * The actual workspace folder change event.
   --	 */
   --	event: WorkspaceFoldersChangeEvent;
   --}
   --
   --/**
   -- * The workspace folder change event.
   -- */
   --export interface WorkspaceFoldersChangeEvent {
   --	/**
   --	 * The array of added workspace folders
   --	 */
   --	added: WorkspaceFolder[];
   --
   --	/**
   --	 * The array of the removed workspace folders
   --	 */
   --	removed: WorkspaceFolder[];
   --}
   --```
   type WorkspaceFoldersChangeEvent is record
      added: WorkspaceFolder_Vector;
      removed: WorkspaceFolder_Vector;
   end record;

   procedure Read_WorkspaceFoldersChangeEvent
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out WorkspaceFoldersChangeEvent);
   procedure Write_WorkspaceFoldersChangeEvent
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : WorkspaceFoldersChangeEvent);
   for WorkspaceFoldersChangeEvent'Read use Read_WorkspaceFoldersChangeEvent;
   for WorkspaceFoldersChangeEvent'Write use Write_WorkspaceFoldersChangeEvent;

   type DidChangeWorkspaceFoldersParams is record
      event: WorkspaceFoldersChangeEvent;
   end record;

   procedure Read_DidChangeWorkspaceFoldersParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DidChangeWorkspaceFoldersParams);
   procedure Write_DidChangeWorkspaceFoldersParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DidChangeWorkspaceFoldersParams);
   for DidChangeWorkspaceFoldersParams'Read use Read_DidChangeWorkspaceFoldersParams;
   for DidChangeWorkspaceFoldersParams'Write use Write_DidChangeWorkspaceFoldersParams;

   --```typescript
   --export interface ConfigurationParams {
   --	items: ConfigurationItem[];
   --}
   --
   --export interface ConfigurationItem {
   --	/**
   --	 * The scope to get the configuration section for.
   --	 */
   --	scopeUri?: DocumentUri;
   --
   --	/**
   --	 * The configuration section asked for.
   --	 */
   --	section?: string;
   --}
   --```
   type ConfigurationItem is record
      scopeUri : Optional_Virtual_String;
      section  : Optional_Virtual_String;
   end record;

   procedure Read_ConfigurationItem
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ConfigurationItem);
   procedure Write_ConfigurationItem
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ConfigurationItem);
   for ConfigurationItem'Read use Read_ConfigurationItem;
   for ConfigurationItem'Write use Write_ConfigurationItem;

   package ConfigurationItem_Vectors is new LSP.Generic_Vectors
     (ConfigurationItem, Write_Empty => LSP.Write_Array);

   type ConfigurationItem_Vector is new ConfigurationItem_Vectors.Vector with null record;

   type ConfigurationParams is record
      items: ConfigurationItem_Vector;
   end record;

   procedure Read_ConfigurationParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ConfigurationParams);
   procedure Write_ConfigurationParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ConfigurationParams);
   for ConfigurationParams'Read use Read_ConfigurationParams;
   for ConfigurationParams'Write use Write_ConfigurationParams;

   --```typescript
   --export interface CompletionParams extends TextDocumentPositionParams,
   --	WorkDoneProgressParams, PartialResultParams {
   --	/**
   --	 * The completion context. This is only available if the client specifies
   --	 * to send this using the client capability
   --	 * `completion.contextSupport === true`
   --	 */
   --	context?: CompletionContext;
   --}
   --
   --/**
   -- * How a completion was triggered
   -- */
   --export namespace CompletionTriggerKind {
   --	/**
   --	 * Completion was triggered by typing an identifier (24x7 code
   --	 * complete), manual invocation (e.g Ctrl+Space) or via API.
   --	 */
   --	export const Invoked: 1 = 1;
   --
   --	/**
   --	 * Completion was triggered by a trigger character specified by
   --	 * the `triggerCharacters` properties of the
   --	 * `CompletionRegistrationOptions`.
   --	 */
   --	export const TriggerCharacter: 2 = 2;
   --
   --	/**
   --	 * Completion was re-triggered as the current completion list is incomplete.
   --	 */
   --	export const TriggerForIncompleteCompletions: 3 = 3;
   --}
   --export type CompletionTriggerKind = 1 | 2 | 3;
   --
   --
   --/**
   -- * Contains additional information about the context in which a completion
   -- * request is triggered.
   -- */
   --export interface CompletionContext {
   --	/**
   --	 * How the completion was triggered.
   --	 */
   --	triggerKind: CompletionTriggerKind;
   --
   --	/**
   --	 * The trigger character (a single character) that has trigger code
   --	 * complete. Is undefined if
   --	 * `triggerKind !== CompletionTriggerKind.TriggerCharacter`
   --	 */
   --	triggerCharacter?: string;
   --}
   --```

   type CompletionTriggerKind is
     (Invoked, TriggerCharacter, TriggerForIncompleteCompletions);

   procedure Read_CompletionTriggerKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CompletionTriggerKind);
   procedure Write_CompletionTriggerKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CompletionTriggerKind);
   for CompletionTriggerKind'Read use Read_CompletionTriggerKind;
   for CompletionTriggerKind'Write use Write_CompletionTriggerKind;

   type CompletionContext is record
      triggerKind: CompletionTriggerKind;
      triggerCharacter: Optional_Virtual_String;
   end record;

   procedure Read_CompletionContext
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CompletionContext);
   procedure Write_CompletionContext
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CompletionContext);
   for CompletionContext'Read use Read_CompletionContext;
   for CompletionContext'Write use Write_CompletionContext;

   package Optional_CompletionContexts is new LSP.Generic_Optional (CompletionContext);
   type Optional_CompletionContext is new Optional_CompletionContexts.Optional_Type;

   type CompletionParams is new Text_Progress_Partial_Params with record
      context: Optional_CompletionContext;
   end record;

   procedure Read_CompletionParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CompletionParams);
   procedure Write_CompletionParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CompletionParams);
   for CompletionParams'Read use Read_CompletionParams;
   for CompletionParams'Write use Write_CompletionParams;

   type Disable_Reason is record
      reason: VSS.Strings.Virtual_String;
   end record;

   procedure Read_Disable_Reason
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Disable_Reason);
   procedure Write_Disable_Reason
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Disable_Reason);
   for Disable_Reason'Read use Read_Disable_Reason;
   for Disable_Reason'Write use Write_Disable_Reason;

   package Optional_Disable_Reasons is new LSP.Generic_Optional (Disable_Reason);
   type Optional_Disable_Reason is new Optional_Disable_Reasons.Optional_Type;

   --```typescript
   --/**
   -- * A code action represents a change that can be performed in code, e.g. to fix
   -- * a problem or to refactor code.
   -- *
   -- * A CodeAction must set either `edit` and/or a `command`. If both are supplied,
   -- * the `edit` is applied first, then the `command` is executed.
   -- */
   --export interface CodeAction {
   --
   --	/**
   --	 * A short, human-readable, title for this code action.
   --	 */
   --	title: string;
   --
   --	/**
   --	 * The kind of the code action.
   --	 *
   --	 * Used to filter code actions.
   --	 */
   --	kind?: CodeActionKind;
   --
   --	/**
   --	 * The diagnostics that this code action resolves.
   --	 */
   --	diagnostics?: Diagnostic[];
   --
   --	/**
   --	 * Marks this as a preferred action. Preferred actions are used by the
   --	 * `auto fix` command and can be targeted by keybindings.
   --	 *
   --	 * A quick fix should be marked preferred if it properly addresses the
   --	 * underlying error. A refactoring should be marked preferred if it is the
   --	 * most reasonable choice of actions to take.
   --	 *
   --	 * @since 3.15.0
   --	 */
   --	isPreferred?: boolean;
   --
   --	/**
   --	 * Marks that the code action cannot currently be applied.
   --	 *
   --	 * Clients should follow the following guidelines regarding disabled code
   --	 * actions:
   --	 *
   --	 * - Disabled code actions are not shown in automatic lightbulbs code
   --	 *   action menus.
   --	 *
   --	 * - Disabled actions are shown as faded out in the code action menu when
   --	 *   the user request a more specific type of code action, such as
   --	 *   refactorings.
   --	 *
   --	 * - If the user has a keybinding that auto applies a code action and only
   --	 *   a disabled code actions are returned, the client should show the user
   --	 *   an error message with `reason` in the editor.
   --	 *
   --	 * @since 3.16.0
   --	 */
   --	disabled?: {
   --
   --		/**
   --		 * Human readable description of why the code action is currently
   --		 * disabled.
   --		 *
   --		 * This is displayed in the code actions UI.
   --		 */
   --		reason: string;
   --	};
   --
   --	/**
   --	 * The workspace edit this code action performs.
   --	 */
   --	edit?: WorkspaceEdit;
   --
   --	/**
   --	 * A command this code action executes. If a code action
   --	 * provides an edit and a command, first the edit is
   --	 * executed and then the command.
   --	 */
   --	command?: Command;
   --
   --	/**
   --	 * A data entry field that is preserved on a code action between
   --	 * a `textDocument/codeAction` and a `codeAction/resolve` request.
   --	 *
   --	 * @since 3.16.0
   --	 */
   --	data?: any
   --}
   --```
   type CodeAction is record
      title       : VSS.Strings.Virtual_String;
      kind        : Optional_CodeActionKind;
      diagnostics : Optional_Diagnostic_Vector;
      isPreferred : Optional_Boolean;
      disabled    : Optional_Disable_Reason;
      edit        : Optional_WorkspaceEdit;
      command     : Optional_Command;
   end record;

   procedure Read_CodeAction
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CodeAction);
   procedure Write_CodeAction
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CodeAction);
   for CodeAction'Read use Read_CodeAction;
   for CodeAction'Write use Write_CodeAction;

   package CodeAction_Vectors is new LSP.Generic_Vectors
     (CodeAction, Write_Empty => LSP.Write_Array);

   type CodeAction_Vector is new CodeAction_Vectors.Vector with null record;
   --  CodeAction_Vector represents a sequence of CodeAction OR Command.
   --  Command is represented as CodeAction with just `command` property set.

   --```typescript
   --interface ColorInformation {
   --	/**
   --	 * The range in the document where this color appears.
   --	 */
   --	range: Range;
   --
   --	/**
   --	 * The actual color value for this color range.
   --	 */
   --	color: Color;
   --}
   --
   --/**
   -- * Represents a color in RGBA space.
   -- */
   --interface Color {
   --
   --	/**
   --	 * The red component of this color in the range [0-1].
   --	 */
   --	readonly red: decimal;
   --
   --	/**
   --	 * The green component of this color in the range [0-1].
   --	 */
   --	readonly green: decimal;
   --
   --	/**
   --	 * The blue component of this color in the range [0-1].
   --	 */
   --	readonly blue: decimal;
   --
   --	/**
   --	 * The alpha component of this color in the range [0-1].
   --	 */
   --	readonly alpha: decimal;
   --}
   --```
   type RGBA_Color is record
      red: LSP_Number;
      green: LSP_Number;
      blue: LSP_Number;
      alpha: LSP_Number;
   end record;

   procedure Read_RGBA_Color
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out RGBA_Color);
   procedure Write_RGBA_Color
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : RGBA_Color);
   for RGBA_Color'Read use Read_RGBA_Color;
   for RGBA_Color'Write use Write_RGBA_Color;

   type ColorInformation is record
      span: LSP.Messages.Span;  --  Range is reservet keyword
      color: RGBA_Color;
   end record;

   procedure Read_ColorInformation
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ColorInformation);
   procedure Write_ColorInformation
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ColorInformation);
   for ColorInformation'Read use Read_ColorInformation;
   for ColorInformation'Write use Write_ColorInformation;

   package ColorInformation_Vectors is new LSP.Generic_Vectors
     (ColorInformation, Write_Empty => LSP.Write_Array);

   type ColorInformation_Vector is
     new ColorInformation_Vectors.Vector with null record;

   --```typescript
   --interface ColorPresentationParams extends WorkDoneProgressParams,
   --	PartialResultParams {
   --	/**
   --	 * The text document.
   --	 */
   --	textDocument: TextDocumentIdentifier;
   --
   --	/**
   --	 * The color information to request presentations for.
   --	 */
   --	color: Color;
   --
   --	/**
   --	 * The range where the color would be inserted. Serves as a context.
   --	 */
   --	range: Range;
   --}
   --```
   type ColorPresentationParams is new Progress_Partial_Params with record
      textDocument: TextDocumentIdentifier;
      color: RGBA_Color;
      span: LSP.Messages.Span;  --  Range is reservet keyword
   end record;

   procedure Read_ColorPresentationParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ColorPresentationParams);
   procedure Write_ColorPresentationParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ColorPresentationParams);
   for ColorPresentationParams'Read use Read_ColorPresentationParams;
   for ColorPresentationParams'Write use Write_ColorPresentationParams;

   --```typescript
   --interface ColorPresentation {
   --	/**
   --	 * The label of this color presentation. It will be shown on the color
   --	 * picker header. By default this is also the text that is inserted when
   --	 * selecting this color presentation.
   --	 */
   --	label: string;
   --	/**
   --	 * An [edit](#TextEdit) which is applied to a document when selecting
   --	 * this presentation for the color.  When `falsy` the
   --	 * [label](#ColorPresentation.label) is used.
   --	 */
   --	textEdit?: TextEdit;
   --	/**
   --	 * An optional array of additional [text edits](#TextEdit) that are applied
   --	 * when selecting this color presentation. Edits must not overlap with the
   --	 * main [edit](#ColorPresentation.textEdit) nor with themselves.
   --	 */
   --	additionalTextEdits?: TextEdit[];
   --}
   --```
   type ColorPresentation is record
      label               : VSS.Strings.Virtual_String;
      textEdit            : Optional_TextEdit;
      additionalTextEdits : TextEdit_Vector;
   end record;

   procedure Read_ColorPresentation
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ColorPresentation);
   procedure Write_ColorPresentation
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ColorPresentation);
   for ColorPresentation'Read use Read_ColorPresentation;
   for ColorPresentation'Write use Write_ColorPresentation;

   package ColorPresentation_Vectors is new LSP.Generic_Vectors
     (ColorPresentation, Write_Empty => LSP.Write_Array);

   type ColorPresentation_Vector is
     new ColorPresentation_Vectors.Vector with null record;

   --```typescript
   --export interface FoldingRangeParams extends WorkDoneProgressParams,
   --	PartialResultParams {
   --	/**
   --	 * The text document.
   --	 */
   --	textDocument: TextDocumentIdentifier;
   --}
   --```
   type FoldingRangeParams is new Progress_Partial_Params with record
      textDocument: TextDocumentIdentifier;
   end record;

   procedure Read_FoldingRangeParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out FoldingRangeParams);
   procedure Write_FoldingRangeParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : FoldingRangeParams);
   for FoldingRangeParams'Read use Read_FoldingRangeParams;
   for FoldingRangeParams'Write use Write_FoldingRangeParams;

   --```typescript
   --/**
   -- * Enum of known range kinds
   -- */
   --export enum FoldingRangeKind {
   --	/**
   --	 * Folding range for a comment
   --	 */
   --	Comment = 'comment',
   --	/**
   --	 * Folding range for a imports or includes
   --	 */
   --	Imports = 'imports',
   --	/**
   --	 * Folding range for a region (e.g. `#region`)
   --	 */
   --	Region = 'region'
   --}
   --
   --/**
   -- * Represents a folding range. To be valid, start and end line must be bigger
   -- * than zero and smaller than the number of lines in the document. Clients
   -- * are free to ignore invalid ranges.
   -- */
   --export interface FoldingRange {
   --
   --	/**
   --	 * The zero-based start line of the range to fold. The folded area starts
   --	 * after the line's last character. To be valid, the end must be zero or
   --	 * larger and smaller than the number of lines in the document.
   --	 */
   --	startLine: uinteger;
   --
   --	/**
   --	 * The zero-based character offset from where the folded range starts. If
   --	 * not defined, defaults to the length of the start line.
   --	 */
   --	startCharacter?: uinteger;
   --
   --	/**
   --	 * The zero-based end line of the range to fold. The folded area ends with
   --	 * the line's last character. To be valid, the end must be zero or larger
   --	 * and smaller than the number of lines in the document.
   --	 */
   --	endLine: uinteger;
   --
   --	/**
   --	 * The zero-based character offset before the folded range ends. If not
   --	 * defined, defaults to the length of the end line.
   --	 */
   --	endCharacter?: uinteger;
   --
   --	/**
   --	 * Describes the kind of the folding range such as `comment` or `region`.
   --	 * The kind is used to categorize folding ranges and used by commands like
   --	 * 'Fold all comments'. See [FoldingRangeKind](#FoldingRangeKind) for an
   --	 * enumeration of standardized kinds.
   --	 */
   --	kind?: string;
   --}
   --```
   type FoldingRange is record
      startLine      : Line_Number;
      startCharacter : Optional_Number;
      endLine        : Line_Number;
      endCharacter   : Optional_Number;
      kind           : Optional_Virtual_String;
   end record;

   procedure Read_FoldingRange
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out FoldingRange);
   procedure Write_FoldingRange
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : FoldingRange);
   for FoldingRange'Read use Read_FoldingRange;
   for FoldingRange'Write use Write_FoldingRange;

   package FoldingRange_Vectors is new LSP.Generic_Vectors
     (FoldingRange, Write_Empty => LSP.Write_Array);

   type FoldingRange_Vector is
     new FoldingRange_Vectors.Vector with null record;

   subtype FoldingRangeKind is VSS.Strings.Virtual_String;
   --  FIXME: replace with enumeration type???
   function Comment return FoldingRangeKind is ("comment");
   Imports : constant FoldingRangeKind := "imports";
   Region  : constant FoldingRangeKind := "region";

   --```typescript
   --interface DocumentColorParams extends WorkDoneProgressParams,
   --	PartialResultParams {
   --	/**
   --	 * The text document.
   --	 */
   --	textDocument: TextDocumentIdentifier;
   --}
   --```
   type DocumentColorParams is new Progress_Partial_Params with record
      textDocument: TextDocumentIdentifier;
   end record;

   procedure Read_DocumentColorParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DocumentColorParams);
   procedure Write_DocumentColorParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DocumentColorParams);
   for DocumentColorParams'Read use Read_DocumentColorParams;
   for DocumentColorParams'Write use Write_DocumentColorParams;

   --```typescript
   --export interface HoverRegistrationOptions
   --	extends TextDocumentRegistrationOptions, HoverOptions {
   --}
   --```

   --```typescript
   --export interface HoverParams extends TextDocumentPositionParams,
   --	WorkDoneProgressParams {
   --}
   --```
   type HoverParams is new Text_Progress_Params with null record;

   --```typescript
   --export interface SignatureHelpParams extends TextDocumentPositionParams,
   --	WorkDoneProgressParams {
   --	/**
   --	 * The signature help context. This is only available if the client
   --	 * specifies to send this using the client capability
   --	 * `textDocument.signatureHelp.contextSupport === true`
   --	 *
   --	 * @since 3.15.0
   --	 */
   --	context?: SignatureHelpContext;
   --}

   --/**
   -- * How a signature help was triggered.
   -- *
   -- * @since 3.15.0
   -- */
   --export namespace SignatureHelpTriggerKind {
   --	/**
   --	 * Signature help was invoked manually by the user or by a command.
   --	 */
   --	export const Invoked: 1 = 1;
   --	/**
   --	 * Signature help was triggered by a trigger character.
   --	 */
   --	export const TriggerCharacter: 2 = 2;
   --	/**
   --	 * Signature help was triggered by the cursor moving or by the document
   --	 * content changing.
   --	 */
   --	export const ContentChange: 3 = 3;
   --}
   --export type SignatureHelpTriggerKind = 1 | 2 | 3;
   type SignatureHelpTriggerKind is
     (Invoked, TriggerCharacter, ContentChange);
   procedure Read_SignatureHelpTriggerKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out SignatureHelpTriggerKind);
   procedure Write_SignatureHelpTriggerKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SignatureHelpTriggerKind);
   for SignatureHelpTriggerKind'Read use Read_SignatureHelpTriggerKind;
   for SignatureHelpTriggerKind'Write use Write_SignatureHelpTriggerKind;

   --/**
   -- * Additional information about the context in which a signature help request
   -- * was triggered.
   -- *
   -- * @since 3.15.0
   -- */
   --export interface SignatureHelpContext {
   --	/**
   --	 * Action that caused signature help to be triggered.
   --	 */
   --	triggerKind: SignatureHelpTriggerKind;
   --
   --	/**
   --	 * Character that caused signature help to be triggered.
   --	 *
   --	 * This is undefined when triggerKind !==
   --	 * SignatureHelpTriggerKind.TriggerCharacter
   --	 */
   --	triggerCharacter?: string;
   --
   --	/**
   --	 * `true` if signature help was already showing when it was triggered.
   --	 *
   --	 * Retriggers occur when the signature help is already active and can be
   --	 * caused by actions such as typing a trigger character, a cursor move, or
   --	 * document content changes.
   --	 */
   --	isRetrigger: boolean;
   --
   --	/**
   --	 * The currently active `SignatureHelp`.
   --	 *
   --	 * The `activeSignatureHelp` has its `SignatureHelp.activeSignature` field
   --	 * updated based on the user navigating through available signatures.
   --	 */
   --	activeSignatureHelp?: SignatureHelp;
   --}
   --```
   type SignatureHelpContext is record
      triggerKind         : SignatureHelpTriggerKind;
      triggerCharacter    : Optional_Virtual_String;
      isRetrigger         : Boolean;
      activeSignatureHelp : Optional_SignatureHelp;
   end record;
   procedure Read_SignatureHelpContext
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out SignatureHelpContext);
   procedure Write_SignatureHelpContext
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SignatureHelpContext);
   for SignatureHelpContext'Read use Read_SignatureHelpContext;
   for SignatureHelpContext'Write use Write_SignatureHelpContext;

   package Optional_SignatureHelpContexts is new Generic_Optional
     (SignatureHelpContext);
   type Optional_SignatureHelpContext
   is new Optional_SignatureHelpContexts.Optional_Type;

   type SignatureHelpParams is new Text_Progress_Params with record
      context : Optional_SignatureHelpContext;
   end record;
   procedure Read_SignatureHelpParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out SignatureHelpParams);
   procedure Write_SignatureHelpParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SignatureHelpParams);
   for SignatureHelpParams'Read use Read_SignatureHelpParams;
   for SignatureHelpParams'Write use Write_SignatureHelpParams;

   --  Base class for navigation requests, embedding an optional flag to
   --  control whether or now we should list overriding/overridden subprograms.
   type NavigationRequestParams is new Text_Progress_Partial_Params with record
      alsDisplayMethodAncestryOnNavigation :
      Optional_AlsDisplayMethodAncestryOnNavigationPolicy;
   end record;
   procedure Read_NavigationRequestParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out NavigationRequestParams);
   procedure Write_NavigationRequestParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : NavigationRequestParams);
   for NavigationRequestParams'Read use Read_NavigationRequestParams;
   for NavigationRequestParams'Write use Write_NavigationRequestParams;

   --```typescript
   --export interface DeclarationParams extends TextDocumentPositionParams,
   --	WorkDoneProgressParams, PartialResultParams {
   --}
   --```
   type DeclarationParams is new NavigationRequestParams with null record;

   --```typescript
   --export interface DefinitionParams extends TextDocumentPositionParams,
   --	WorkDoneProgressParams, PartialResultParams {
   --}
   --```
   type DefinitionParams is new NavigationRequestParams with null record;

   --```typescript
   --export interface TypeDefinitionParams extends TextDocumentPositionParams,
   --	WorkDoneProgressParams, PartialResultParams {
   --}
   --```
   type TypeDefinitionParams is new Text_Progress_Partial_Params with null record;

   --```typescript
   --export interface ImplementationParams extends TextDocumentPositionParams,
   --	WorkDoneProgressParams, PartialResultParams {
   --}
   --```
   type ImplementationParams is new NavigationRequestParams with null record;

   --```typescript
   --export interface DocumentHighlightParams extends TextDocumentPositionParams,
   --	WorkDoneProgressParams, PartialResultParams {
   --}
   --```
   type DocumentHighlightParams is new Text_Progress_Partial_Params with null record;

   --```typescript
   --export interface SelectionRangeParams extends WorkDoneProgressParams,
   --	PartialResultParams {
   --	/**
   --	 * The text document.
   --	 */
   --	textDocument: TextDocumentIdentifier;
   --
   --	/**
   --	 * The positions inside the text document.
   --	 */
   --	positions: Position[];
   --}
   --```
   type SelectionRangeParams is new Progress_Partial_Params with record
      textDocument: TextDocumentIdentifier;
      positions: Position_Vector;
   end record;

   procedure Read_SelectionRangeParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out SelectionRangeParams);
   procedure Write_SelectionRangeParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SelectionRangeParams);
   for SelectionRangeParams'Read use Read_SelectionRangeParams;
   for SelectionRangeParams'Write use Write_SelectionRangeParams;

   --```typescript
   --export interface SelectionRange {
   --    /**
   --     * The [range](#Range) of this selection range.
   --     */
   --    range: Range;
   --    /**
   --     * The parent selection range containing this range. Therefore
   --	 * `parent.range` must contain `this.range`.
   --     */
   --    parent?: SelectionRange;
   --}
   --```
   type SelectionRange is record
      span    : LSP.Messages.Span;  --  range: is reserved word
      --  parent : FIXME: TBD
   end record;

   procedure Read_SelectionRange
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out SelectionRange);
   procedure Write_SelectionRange
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SelectionRange);
   for SelectionRange'Read use Read_SelectionRange;
   for SelectionRange'Write use Write_SelectionRange;

   package SelectionRange_Vectors is new LSP.Generic_Vectors
     (SelectionRange, Write_Empty => LSP.Write_Array);

   type SelectionRange_Vector is new SelectionRange_Vectors.Vector
     with null record;

   --```typescript
   --export interface LinkedEditingRangeParams extends TextDocumentPositionParams,
   --	WorkDoneProgressParams {
   --}
   --```
   subtype LinkedEditingRangeParams is Text_Progress_Params;

   --```typescript
   --export interface LinkedEditingRanges {
   --	/**
   --	 * A list of ranges that can be renamed together. The ranges must have
   --	 * identical length and contain identical text content. The ranges cannot overlap.
   --	 */
   --	ranges: Range[];
   --
   --	/**
   --	 * An optional word pattern (regular expression) that describes valid contents for
   --	 * the given ranges. If no pattern is provided, the client configuration's word
   --	 * pattern will be used.
   --	 */
   --	wordPattern?: string;
   --}
   --```
   type LinkedEditingRanges is record
      ranges      : Span_Vector;
      wordPattern : Optional_Virtual_String;
   end record;

   procedure Read_LinkedEditingRanges
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LinkedEditingRanges);
   procedure Write_LinkedEditingRanges
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LinkedEditingRanges);
   for LinkedEditingRanges'Read use Read_LinkedEditingRanges;
   for LinkedEditingRanges'Write use Write_LinkedEditingRanges;

   --```typescript
   --export interface WorkDoneProgressCancelParams {
   --	/**
   --	 * The token to be used to report progress.
   --	 */
   --	token: ProgressToken;
   --}
   --```

   --```typescript
   --export interface PrepareRenameParams extends TextDocumentPositionParams {
   --}
   --```
   subtype PrepareRenameParams is TextDocumentPositionParams;

   --```typescript
   --export interface CallHierarchyPrepareParams extends TextDocumentPositionParams,
   --	WorkDoneProgressParams {
   --}
   --```
   type CallHierarchyPrepareParams is new Text_Progress_Params with null record;

   --```typescript
   --export interface CallHierarchyItem {
   --	/**
   --	 * The name of this item.
   --	 */
   --	name: string;
   --
   --	/**
   --	 * The kind of this item.
   --	 */
   --	kind: SymbolKind;
   --
   --	/**
   --	 * Tags for this item.
   --	 */
   --	tags?: SymbolTag[];
   --
   --	/**
   --	 * More detail for this item, e.g. the signature of a function.
   --	 */
   --	detail?: string;
   --
   --	/**
   --	 * The resource identifier of this item.
   --	 */
   --	uri: DocumentUri;
   --
   --	/**
   --	 * The range enclosing this symbol not including leading/trailing whitespace
   --	 * but everything else, e.g. comments and code.
   --	 */
   --	range: Range;
   --
   --	/**
   --	 * The range that should be selected and revealed when this symbol is being
   --	 * picked, e.g. the name of a function. Must be contained by the
   --	 * [`range`](#CallHierarchyItem.range).
   --	 */
   --	selectionRange: Range;
   --
   --	/**
   --	 * A data entry field that is preserved between a call hierarchy prepare and
   --	 * incoming calls or outgoing calls requests.
   --	 */
   --	data?: unknown;
   --}
   --```
   type CallHierarchyItem is record
      name           : VSS.Strings.Virtual_String;
      kind           : SymbolKind;
      tags           : Optional_SymbolTagSet;
      detail         : Optional_Virtual_String;
      uri            : DocumentUri;
      span           : LSP.Messages.Span;  --  range: is reserved word
      selectionRange : LSP.Messages.Span;
      --  data?: unknown;
   end record;

   procedure Read_CallHierarchyItem
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CallHierarchyItem);
   procedure Write_CallHierarchyItem
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CallHierarchyItem);
   for CallHierarchyItem'Read use Read_CallHierarchyItem;
   for CallHierarchyItem'Write use Write_CallHierarchyItem;

   package CallHierarchyItem_Vectors is new LSP.Generic_Vectors
     (CallHierarchyItem, Write_Empty => LSP.Write_Null);

   type CallHierarchyItem_Vector is
     new CallHierarchyItem_Vectors.Vector with null record;

   --```typescript
   --export interface CallHierarchyIncomingCallsParams extends
   --	WorkDoneProgressParams, PartialResultParams {
   --	item: CallHierarchyItem;
   --}
   --```
   type CallHierarchyIncomingCallsParams is new Progress_Partial_Params with record
      item: CallHierarchyItem;
   end record;

   procedure Read_CallHierarchyIncomingCallsParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CallHierarchyIncomingCallsParams);
   procedure Write_CallHierarchyIncomingCallsParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CallHierarchyIncomingCallsParams);
   for CallHierarchyIncomingCallsParams'Read use Read_CallHierarchyIncomingCallsParams;
   for CallHierarchyIncomingCallsParams'Write use Write_CallHierarchyIncomingCallsParams;

   --```typescript
   --export interface CallHierarchyIncomingCall {
   --
   --	/**
   --	 * The item that makes the call.
   --	 */
   --	from: CallHierarchyItem;
   --
   --	/**
   --	 * The ranges at which the calls appear. This is relative to the caller
   --	 * denoted by [`this.from`](#CallHierarchyIncomingCall.from).
   --	 */
   --	fromRanges: Range[];
   --}
   --```
   type CallHierarchyIncomingCall is record
      from: CallHierarchyItem;
      fromRanges: Span_Vector;
      kinds: AlsReferenceKind_Vector;
   end record;

   procedure Read_CallHierarchyIncomingCall
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CallHierarchyIncomingCall);
   procedure Write_CallHierarchyIncomingCall
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CallHierarchyIncomingCall);
   for CallHierarchyIncomingCall'Read use Read_CallHierarchyIncomingCall;
   for CallHierarchyIncomingCall'Write use Write_CallHierarchyIncomingCall;

   package CallHierarchyIncomingCall_Vectors is new LSP.Generic_Vectors
     (CallHierarchyIncomingCall, Write_Empty => LSP.Write_Null);

   type CallHierarchyIncomingCall_Vector is
     new CallHierarchyIncomingCall_Vectors.Vector with null record;

   --```typescript
   --export interface CallHierarchyOutgoingCallsParams extends
   --	WorkDoneProgressParams, PartialResultParams {
   --	item: CallHierarchyItem;
   --}
   --```
   subtype CallHierarchyOutgoingCallsParams is CallHierarchyIncomingCallsParams;

   --```typescript
   --export interface CallHierarchyOutgoingCall {
   --
   --	/**
   --	 * The item that is called.
   --	 */
   --	to: CallHierarchyItem;
   --
   --	/**
   --	 * The range at which this item is called. This is the range relative to
   --	 * the caller, e.g the item passed to `callHierarchy/outgoingCalls` request.
   --	 */
   --	fromRanges: Range[];
   --}
   --```
   type CallHierarchyOutgoingCall is record
      to: CallHierarchyItem;
      fromRanges: Span_Vector;
      kinds: AlsReferenceKind_Vector;
   end record;

   procedure Read_CallHierarchyOutgoingCall
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CallHierarchyOutgoingCall);
   procedure Write_CallHierarchyOutgoingCall
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CallHierarchyOutgoingCall);
   for CallHierarchyOutgoingCall'Read use Read_CallHierarchyOutgoingCall;
   for CallHierarchyOutgoingCall'Write use Write_CallHierarchyOutgoingCall;

   package CallHierarchyOutgoingCall_Vectors is new LSP.Generic_Vectors
     (CallHierarchyOutgoingCall, Write_Empty => LSP.Write_Null);

   type CallHierarchyOutgoingCall_Vector is
     new CallHierarchyOutgoingCall_Vectors.Vector with null record;

   --```typescript
   --export interface SemanticTokensParams extends WorkDoneProgressParams,
   --	PartialResultParams {
   --	/**
   --	 * The text document.
   --	 */
   --	textDocument: TextDocumentIdentifier;
   --}
   --```
   type SemanticTokensParams is new Progress_Partial_Params with record
      textDocument   : TextDocumentIdentifier;
   end record;

   procedure Read_SemanticTokensParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out SemanticTokensParams);
   procedure Write_SemanticTokensParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SemanticTokensParams);
   for SemanticTokensParams'Read use Read_SemanticTokensParams;
   for SemanticTokensParams'Write use Write_SemanticTokensParams;

   --```typescript
   --export interface SemanticTokens {
   --	/**
   --	 * An optional result id. If provided and clients support delta updating
   --	 * the client will include the result id in the next semantic token request.
   --	 * A server can then instead of computing all semantic tokens again simply
   --	 * send a delta.
   --	 */
   --	resultId?: string;
   --
   --	/**
   --	 * The actual tokens.
   --	 */
   --	data: uinteger[];
   --}
   --```
   type SemanticTokens is record
      resultId : Optional_Virtual_String;
      data     : uinteger_Vector;
   end record;

   procedure Read_SemanticTokens
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out SemanticTokens);
   procedure Write_SemanticTokens
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SemanticTokens);
   for SemanticTokens'Read use Read_SemanticTokens;
   for SemanticTokens'Write use Write_SemanticTokens;

   --```typescript
   --export interface SemanticTokensPartialResult {
   --	data: uinteger[];
   --}
   --```
   type SemanticTokensPartialResult is record
      data: uinteger_Vector;
   end record;

   procedure Read_SemanticTokensPartialResult
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out SemanticTokensPartialResult);
   procedure Write_SemanticTokensPartialResult
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SemanticTokensPartialResult);
   for SemanticTokensPartialResult'Read use Read_SemanticTokensPartialResult;
   for SemanticTokensPartialResult'Write use Write_SemanticTokensPartialResult;

   --```typescript
   --export interface SemanticTokensDeltaParams extends WorkDoneProgressParams,
   --	PartialResultParams {
   --	/**
   --	 * The text document.
   --	 */
   --	textDocument: TextDocumentIdentifier;
   --
   --	/**
   --	 * The result id of a previous response. The result Id can either point to
   --	 * a full response or a delta response depending on what was received last.
   --	 */
   --	previousResultId: string;
   --}
   --```
   type SemanticTokensDeltaParams is new Progress_Partial_Params with record
      textDocument     : TextDocumentIdentifier;
      previousResultId : VSS.Strings.Virtual_String;
   end record;

   procedure Read_SemanticTokensDeltaParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out SemanticTokensDeltaParams);
   procedure Write_SemanticTokensDeltaParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SemanticTokensDeltaParams);
   for SemanticTokensDeltaParams'Read use Read_SemanticTokensDeltaParams;
   for SemanticTokensDeltaParams'Write use Write_SemanticTokensDeltaParams;

   --```typescript
   --export interface SemanticTokensDelta {
   --	readonly resultId?: string;
   --	/**
   --	 * The semantic token edits to transform a previous result into a new
   --	 * result.
   --	 */
   --	edits: SemanticTokensEdit[];
   --}
   --
   --export interface SemanticTokensEdit {
   --	/**
   --	 * The start offset of the edit.
   --	 */
   --	start: uinteger;
   --
   --	/**
   --	 * The count of elements to remove.
   --	 */
   --	deleteCount: uinteger;
   --
   --	/**
   --	 * The elements to insert.
   --	 */
   --	data?: uinteger[];
   --}
   --```
   type SemanticTokensEdit is record
      start: uinteger;
      deleteCount: uinteger;
      data: uinteger_Vector;
   end record;

   procedure Read_SemanticTokensEdit
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out SemanticTokensEdit);
   procedure Write_SemanticTokensEdit
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SemanticTokensEdit);
   for SemanticTokensEdit'Read use Read_SemanticTokensEdit;
   for SemanticTokensEdit'Write use Write_SemanticTokensEdit;

   package SemanticTokensEdit_Vectors is new LSP.Generic_Vectors
     (SemanticTokensEdit, Write_Empty => LSP.Write_Array);
   type SemanticTokensEdit_Vector is new SemanticTokensEdit_Vectors.Vector
     with null record;

   type SemanticTokensDelta is record
      resultId : Optional_Virtual_String;
      edits    : SemanticTokensEdit_Vector;
   end record;

   procedure Read_SemanticTokensDelta
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out SemanticTokensDelta);
   procedure Write_SemanticTokensDelta
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SemanticTokensDelta);
   for SemanticTokensDelta'Read use Read_SemanticTokensDelta;
   for SemanticTokensDelta'Write use Write_SemanticTokensDelta;

   --```typescript
   --export interface SemanticTokensDeltaPartialResult {
   --	edits: SemanticTokensEdit[]
   --}
   --```
   type SemanticTokensDeltaPartialResult is record
      edits: SemanticTokensEdit_Vector;
   end record;

   procedure Read_SemanticTokensDeltaPartialResult
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out SemanticTokensDeltaPartialResult);
   procedure Write_SemanticTokensDeltaPartialResult
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SemanticTokensDeltaPartialResult);
   for SemanticTokensDeltaPartialResult'Read use Read_SemanticTokensDeltaPartialResult;
   for SemanticTokensDeltaPartialResult'Write use Write_SemanticTokensDeltaPartialResult;

   --```typescript
   --export interface SemanticTokensRangeParams extends WorkDoneProgressParams,
   --	PartialResultParams {
   --	/**
   --	 * The text document.
   --	 */
   --	textDocument: TextDocumentIdentifier;
   --
   --	/**
   --	 * The range the semantic tokens are requested for.
   --	 */
   --	range: Range;
   --}
   --```
   type SemanticTokensRangeParams is new Progress_Partial_Params with record
      textDocument: TextDocumentIdentifier;
      span: LSP.Messages.Span;  --  range: is reserved word
   end record;

   procedure Read_SemanticTokensRangeParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out SemanticTokensRangeParams);
   procedure Write_SemanticTokensRangeParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SemanticTokensRangeParams);
   for SemanticTokensRangeParams'Read use Read_SemanticTokensRangeParams;
   for SemanticTokensRangeParams'Write use Write_SemanticTokensRangeParams;

   --```typescript
   --export interface MonikerParams extends TextDocumentPositionParams,
   --	WorkDoneProgressParams, PartialResultParams {
   --}
   --```
   type MonikerParams is new Text_Progress_Partial_Params with null record;

   --```typescript
   --/**
   --  * Moniker uniqueness level to define scope of the moniker.
   --  */
   --export enum UniquenessLevel {
   --	/**
   --	 * The moniker is only unique inside a document
   --	 */
   --	document = 'document',
   --
   --	/**
   --	 * The moniker is unique inside a project for which a dump got created
   --	 */
   --	project = 'project',
   --
   --	/**
   --	 * The moniker is unique inside the group to which a project belongs
   --	 */
   --	group = 'group',
   --
   --	/**
   --	 * The moniker is unique inside the moniker scheme.
   --	 */
   --	scheme = 'scheme',
   --
   --	/**
   --	 * The moniker is globally unique
   --	 */
   --	global = 'global'
   --}
   --
   --/**
   -- * The moniker kind.
   -- */
   --export enum MonikerKind {
   --	/**
   --	 * The moniker represent a symbol that is imported into a project
   --	 */
   --	import = 'import',
   --
   --	/**
   --	 * The moniker represents a symbol that is exported from a project
   --	 */
   --	export = 'export',
   --
   --	/**
   --	 * The moniker represents a symbol that is local to a project (e.g. a local
   --	 * variable of a function, a class not visible outside the project, ...)
   --	 */
   --	local = 'local'
   --}
   --
   --/**
   -- * Moniker definition to match LSIF 0.5 moniker definition.
   -- */
   --export interface Moniker {
   --	/**
   --	 * The scheme of the moniker. For example tsc or .Net
   --	 */
   --	scheme: string;
   --
   --	/**
   --	 * The identifier of the moniker. The value is opaque in LSIF however
   --	 * schema owners are allowed to define the structure if they want.
   --	 */
   --	identifier: string;
   --
   --	/**
   --	 * The scope in which the moniker is unique
   --	 */
   --	unique: UniquenessLevel;
   --
   --	/**
   --	 * The moniker kind if known.
   --	 */
   --	kind?: MonikerKind;
   --}
   --```
   type UniquenessLevel is (document, project, group, scheme, global);

   procedure Read_UniquenessLevel
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out UniquenessLevel);
   procedure Write_UniquenessLevel
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : UniquenessLevel);
   for UniquenessLevel'Read use Read_UniquenessLevel;
   for UniquenessLevel'Write use Write_UniquenessLevel;

   type MonikerKind is (import, export, local);

   procedure Read_MonikerKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out MonikerKind);
   procedure Write_MonikerKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : MonikerKind);
   for MonikerKind'Read use Read_MonikerKind;
   for MonikerKind'Write use Write_MonikerKind;

   package Optional_MonikerKinds is new LSP.Generic_Optional (MonikerKind);
   type Optional_MonikerKind is new Optional_MonikerKinds.Optional_Type;

   type Moniker is record
      scheme     : VSS.Strings.Virtual_String;
      identifier : VSS.Strings.Virtual_String;
      unique     : UniquenessLevel;
      kind       : Optional_MonikerKind;
   end record;

   procedure Read_Moniker
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out Moniker);
   procedure Write_Moniker
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Moniker);
   for Moniker'Read use Read_Moniker;
   for Moniker'Write use Write_Moniker;

   package Moniker_Vectors is new LSP.Generic_Vectors
     (Moniker, Write_Empty => LSP.Write_Array);
   type Moniker_Vector is new Moniker_Vectors.Vector with null record;

   --```typescript
   --/**
   -- * Params to show a document.
   -- *
   -- * @since 3.16.0
   -- */
   --export interface ShowDocumentParams {
   --	/**
   --	 * The document uri to show.
   --	 */
   --	uri: URI;
   --
   --	/**
   --	 * Indicates to show the resource in an external program.
   --	 * To show for example `https://code.visualstudio.com/`
   --	 * in the default WEB browser set `external` to `true`.
   --	 */
   --	external?: boolean;
   --
   --	/**
   --	 * An optional property to indicate whether the editor
   --	 * showing the document should take focus or not.
   --	 * Clients might ignore this property if an external
   --	 * program is started.
   --	 */
   --	takeFocus?: boolean;
   --
   --	/**
   --	 * An optional selection range if the document is a text
   --	 * document. Clients might ignore the property if an
   --	 * external program is started or the file is not a text
   --	 * file.
   --	 */
   --	selection?: Range;
   --}
   --```
   type ShowDocumentParams is record
      uri: DocumentUri;
      external: Optional_Boolean;
      takeFocus: Optional_Boolean;
      selection: Optional_Span;
   end record;

   procedure Read_ShowDocumentParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ShowDocumentParams);
   procedure Write_ShowDocumentParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ShowDocumentParams);
   for ShowDocumentParams'Read use Read_ShowDocumentParams;
   for ShowDocumentParams'Write use Write_ShowDocumentParams;

   --```typescript
   --/**
   -- * The result of an show document request.
   -- *
   -- * @since 3.16.0
   -- */
   --export interface ShowDocumentResult {
   --	/**
   --	 * A boolean indicating if the show was successful.
   --	 */
   --	success: boolean;
   --}
   --```
   type ShowDocumentResult is record
      success: Boolean;
   end record;

   procedure Read_ShowDocumentResult
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ShowDocumentResult);
   procedure Write_ShowDocumentResult
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ShowDocumentResult);
   for ShowDocumentResult'Read use Read_ShowDocumentResult;
   for ShowDocumentResult'Write use Write_ShowDocumentResult;

   --```typescript
   --/**
   -- * The parameters sent in notifications/requests for user-initiated creation
   -- * of files.
   -- *
   -- * @since 3.16.0
   -- */
   --export interface CreateFilesParams {
   --
   --	/**
   --	 * An array of all files/folders created in this operation.
   --	 */
   --	files: FileCreate[];
   --}
   --/**
   -- * Represents information on a file/folder create.
   -- *
   -- * @since 3.16.0
   -- */
   --export interface FileCreate {
   --
   --	/**
   --	 * A file:// URI for the location of the file/folder being created.
   --	 */
   --	uri: string;
   --}
   --```
   type FileCreate is record
      uri: VSS.Strings.Virtual_String;
   end record;

   procedure Read_FileCreate
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out FileCreate);
   procedure Write_FileCreate
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : FileCreate);
   for FileCreate'Read use Read_FileCreate;
   for FileCreate'Write use Write_FileCreate;

   package FileCreate_Vectors is new LSP.Generic_Vectors
     (FileCreate, Write_Empty => LSP.Write_Array);
   type FileCreate_Vector is new FileCreate_Vectors.Vector with null record;

   type CreateFilesParams is record
      files: FileCreate_Vector;
   end record;

   procedure Read_CreateFilesParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out CreateFilesParams);
   procedure Write_CreateFilesParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : CreateFilesParams);
   for CreateFilesParams'Read use Read_CreateFilesParams;
   for CreateFilesParams'Write use Write_CreateFilesParams;

   --```typescript
   --/**
   -- * The parameters sent in notifications/requests for user-initiated renames
   -- * of files.
   -- *
   -- * @since 3.16.0
   -- */
   --export interface RenameFilesParams {
   --
   --	/**
   --	 * An array of all files/folders renamed in this operation. When a folder
   --	 * is renamed, only the folder will be included, and not its children.
   --	 */
   --	files: FileRename[];
   --}
   --/**
   -- * Represents information on a file/folder rename.
   -- *
   -- * @since 3.16.0
   -- */
   --export interface FileRename {
   --
   --	/**
   --	 * A file:// URI for the original location of the file/folder being renamed.
   --	 */
   --	oldUri: string;
   --
   --	/**
   --	 * A file:// URI for the new location of the file/folder being renamed.
   --	 */
   --	newUri: string;
   --}
   --```
   type FileRename is record
      oldUri: VSS.Strings.Virtual_String;
      newUri: VSS.Strings.Virtual_String;
   end record;

   procedure Read_FileRename
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out FileRename);
   procedure Write_FileRename
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : FileRename);
   for FileRename'Read use Read_FileRename;
   for FileRename'Write use Write_FileRename;

   package FileRename_Vectors is new LSP.Generic_Vectors
     (FileRename, Write_Empty => LSP.Write_Array);
   type FileRename_Vector is new FileRename_Vectors.Vector with null record;

   type RenameFilesParams is record
      files: FileRename_Vector;
   end record;

   procedure Read_RenameFilesParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out RenameFilesParams);
   procedure Write_RenameFilesParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : RenameFilesParams);
   for RenameFilesParams'Read use Read_RenameFilesParams;
   for RenameFilesParams'Write use Write_RenameFilesParams;

   --```typescript
   --/**
   -- * The parameters sent in notifications/requests for user-initiated deletes
   -- * of files.
   -- *
   -- * @since 3.16.0
   -- */
   --export interface DeleteFilesParams {
   --
   --	/**
   --	 * An array of all files/folders deleted in this operation.
   --	 */
   --	files: FileDelete[];
   --}
   --/**
   -- * Represents information on a file/folder delete.
   -- *
   -- * @since 3.16.0
   -- */
   --export interface FileDelete {
   --
   --	/**
   --	 * A file:// URI for the location of the file/folder being deleted.
   --	 */
   --	uri: string;
   --}
   --```
   type FileDelete is record
      uri: VSS.Strings.Virtual_String;
   end record;

   procedure Read_FileDelete
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out FileDelete);
   procedure Write_FileDelete
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : FileDelete);
   for FileDelete'Read use Read_FileDelete;
   for FileDelete'Write use Write_FileDelete;

   package FileDelete_Vectors is new LSP.Generic_Vectors
     (FileDelete, Write_Empty => LSP.Write_Array);
   type FileDelete_Vector is new FileDelete_Vectors.Vector with null record;

   type DeleteFilesParams is record
      files: FileDelete_Vector;
   end record;

   procedure Read_DeleteFilesParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out DeleteFilesParams);
   procedure Write_DeleteFilesParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : DeleteFilesParams);
   for DeleteFilesParams'Read use Read_DeleteFilesParams;
   for DeleteFilesParams'Write use Write_DeleteFilesParams;

   --```typescript
   --interface LogTraceParams {
   --	/**
   --	 * The message to be logged.
   --	 */
   --	message: string;
   --	/**
   --	 * Additional information that can be computed if the `trace` configuration
   --	 * is set to `'verbose'`
   --	 */
   --	verbose?: string;
   --}
   --```
   type LogTraceParams is record
      message : VSS.Strings.Virtual_String;
      verbose : Optional_Virtual_String;
   end record;

   procedure Read_LogTraceParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out LogTraceParams);
   procedure Write_LogTraceParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : LogTraceParams);
   for LogTraceParams'Read use Read_LogTraceParams;
   for LogTraceParams'Write use Write_LogTraceParams;

   --```typescript
   --interface SetTraceParams {
   --	/**
   --	 * The new value that should be assigned to the trace setting.
   --	 */
   --	value: TraceValue;
   --}
   --```
   type SetTraceParams is record
      value: TraceValue;
   end record;

   procedure Read_SetTraceParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out SetTraceParams);
   procedure Write_SetTraceParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : SetTraceParams);
   for SetTraceParams'Read use Read_SetTraceParams;
   for SetTraceParams'Write use Write_SetTraceParams;

   -----------------------------------------
   -- ALS-specific messages and responses --
   -----------------------------------------

   --  These define protocol extensions.
   --  These are tagged with "ALS_" to avoid namespace clashes.

   type ALS_Subprogram_And_References is record
      --  Location and name of the defining subprogram
      loc  : Location;
      name : VSS.Strings.Virtual_String;

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
     (ALS_Subprogram_And_References, Write_Empty => LSP.Write_Array);

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

   type ALS_Source_Dir_Description is record
      name : VSS.Strings.Virtual_String;
      uri  : DocumentUri;
   end record;

   procedure Read_ALS_Source_Dir_Description
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ALS_Source_Dir_Description);
   for ALS_Source_Dir_Description'Read use Read_ALS_Source_Dir_Description;

   procedure Write_ALS_Source_Dir_Description
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ALS_Source_Dir_Description);
   for ALS_Source_Dir_Description'Write use Write_ALS_Source_Dir_Description;

   package ALS_Source_Dir_Description_Vectors is new LSP.Generic_Vectors
     (ALS_Source_Dir_Description, Write_Empty => LSP.Write_Array);
   type ALS_Source_Dir_Description_Vector is
     new ALS_Source_Dir_Description_Vectors.Vector with null record;

   type ALS_ShowDependenciesKind is (Show_Imported, Show_Importing);

   procedure Read_ALS_ShowDependenciesKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ALS_ShowDependenciesKind);
   for ALS_ShowDependenciesKind'Read use Read_ALS_ShowDependenciesKind;

   procedure Write_ALS_ShowDependenciesKind
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ALS_ShowDependenciesKind);
   for ALS_ShowDependenciesKind'Write use Write_ALS_ShowDependenciesKind;

   type ALS_ShowDependenciesParams is record
      textDocument : TextDocumentIdentifier;
      kind         : ALS_ShowDependenciesKind;
      showImplicit : Boolean := False;
   end record;

   procedure Read_ALS_ShowDependenciesParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ALS_ShowDependenciesParams);
   for ALS_ShowDependenciesParams'Read use Read_ALS_ShowDependenciesParams;

   procedure Write_ALS_ShowDependenciesParams
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ALS_ShowDependenciesParams);
   for ALS_ShowDependenciesParams'Write use Write_ALS_ShowDependenciesParams;

   type ALS_Unit_Description is record
      uri        : DocumentUri;
      projectUri : DocumentUri;
   end record;

   procedure Read_ALS_Unit_Description
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ALS_Unit_Description);
   for ALS_Unit_Description'Read use
     Read_ALS_Unit_Description;

   procedure Write_ALS_Unit_Description
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ALS_Unit_Description);
   for ALS_Unit_Description'Write use
     Write_ALS_Unit_Description;

   package ALS_Unit_Description_Vectors is new LSP.Generic_Vectors
     (ALS_Unit_Description, Write_Empty => LSP.Write_Array);

   type ALS_Unit_Description_Vector is
     new ALS_Unit_Description_Vectors.Vector with null record;

   type ALS_Check_Syntax_Params is record
      Input : VSS.Strings.Virtual_String;
      Rules : VSS.String_Vectors.Virtual_String_Vector;
   end record;

   procedure Read_ALS_Check_Syntax_Params
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ALS_Check_Syntax_Params);
   for ALS_Check_Syntax_Params'Read use Read_ALS_Check_Syntax_Params;

   procedure Write_ALS_Check_Syntax_Params
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ALS_Check_Syntax_Params);
   for ALS_Check_Syntax_Params'Write use Write_ALS_Check_Syntax_Params;

   type ALS_Check_Syntax_Result is new Optional_Virtual_String;

   procedure Read_ALS_Check_Syntax_Result
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : out ALS_Check_Syntax_Result);
   for ALS_Check_Syntax_Result'Read use Read_ALS_Check_Syntax_Result;

   procedure Write_ALS_Check_Syntax_Result
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : ALS_Check_Syntax_Result);
   for ALS_Check_Syntax_Result'Write use Write_ALS_Check_Syntax_Result;

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

   package Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => VSS.Strings.Virtual_String,
      Element_Type    => Ada.Tags.Tag,
      Hash            => LSP.Types.Hash,
      Equivalent_Keys => VSS.Strings."=",
      "="             => Ada.Tags."=");

   function Method_To_Tag
     (Map    : Maps.Map;
      Method : VSS.Strings.Virtual_String) return Ada.Tags.Tag;

   --  Here are useless typescript snippets from LSP specification, such as
   --  examples. We keep them to be able to restore all snippets and compare
   --  them to the LSP specification on upgrading LSP version.
   --
   --```typescript
   --{
   --    start: { line: 5, character: 23 },
   --    end : { line: 6, character: 0 }
   --}
   --```
   --```typescript
   --textDocument.codeAction.resolveSupport = { properties: ['edit']};
   --```
   --```typescript
   --{
   --    "title": "Do Foo"
   --}
   --```
   --```typescript
   --{ line: 2, startChar:  5, length: 3, tokenType: "property",
   --	tokenModifiers: ["private", "static"]
   --},
   --{ line: 2, startChar: 10, length: 4, tokenType: "type", tokenModifiers: [] },
   --{ line: 5, startChar:  2, length: 7, tokenType: "class", tokenModifiers: [] }
   --```
   --```typescript
   --{
   --   tokenTypes: ['property', 'type', 'class'],
   --   tokenModifiers: ['private', 'static']
   --}
   --```
   --```typescript
   --{ line: 2, startChar:  5, length: 3, tokenType: 0, tokenModifiers: 3 },
   --{ line: 2, startChar: 10, length: 4, tokenType: 1, tokenModifiers: 0 },
   --{ line: 5, startChar:  2, length: 7, tokenType: 2, tokenModifiers: 0 }
   --```
   --```typescript
   --{ deltaLine: 2, deltaStartChar: 5, length: 3, tokenType: 0, tokenModifiers: 3 },
   --{ deltaLine: 0, deltaStartChar: 5, length: 4, tokenType: 1, tokenModifiers: 0 },
   --{ deltaLine: 3, deltaStartChar: 2, length: 7, tokenType: 2, tokenModifiers: 0 }
   --```
   --```typescript
   --// 1st token,  2nd token,  3rd token
   --[  2,5,3,0,3,  0,5,4,1,0,  3,2,7,2,0 ]
   --```
   --```typescript
   --{ line: 3, startChar:  5, length: 3, tokenType: "property",
   --	tokenModifiers: ["private", "static"]
   --},
   --{ line: 3, startChar: 10, length: 4, tokenType: "type", tokenModifiers: [] },
   --{ line: 6, startChar:  2, length: 7, tokenType: "class", tokenModifiers: [] }
   --```
   --```typescript
   --// 1st token,  2nd token,  3rd token
   --[  3,5,3,0,3,  0,5,4,1,0,  3,2,7,2,0]
   --```

end LSP.Messages;
