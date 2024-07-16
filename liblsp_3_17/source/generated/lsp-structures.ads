--
--  Copyright (C) <YEAR>, <COPYRIGHT HOLDER>
--
--  SPDX-License-Identifier: MIT
--
--  DON'T EDIT THIS FILE! It was generated from metaModel.json.
--

with Ada.Containers.Hashed_Maps;
with Ada.Containers.Vectors;
with Ada.Finalization;

with VSS.JSON.Streams;
with VSS.Strings;

with VSS.String_Vectors;

with LSP.Enumerations; use LSP.Enumerations;

package LSP.Structures is
   pragma Preelaborate;

   subtype Virtual_String is VSS.Strings.Virtual_String;
   subtype Virtual_String_Optional is VSS.Strings.Virtual_String;
   subtype Virtual_String_Vector is VSS.String_Vectors.Virtual_String_Vector;

   type DocumentUri is new VSS.Strings.Virtual_String with null record;

   function Get_Hash (Self : DocumentUri) return Ada.Containers.Hash_Type is
     (Ada.Containers.Hash_Type'Mod (Self.Hash));

   package JSON_Event_Vectors is new Ada.Containers.Vectors
     (Positive, VSS.JSON.Streams.JSON_Stream_Element, VSS.JSON.Streams."=");

   subtype Boolean_Or_Any is JSON_Event_Vectors.Vector;

   type SelectionRange_Optional is tagged private;

   type Boolean_Optional (Is_Set : Boolean := False) is record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : Boolean;
      end case;
   end record;

   type Natural_Optional (Is_Set : Boolean := False) is record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : Natural;
      end case;
   end record;

   type Integer_Optional (Is_Set : Boolean := False) is record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : Integer;
      end case;
   end record;

   type ProgressToken (Is_Integer : Boolean := True) is record
      case Is_Integer is
         when True =>
            Integer : Standard.Integer;
         when False =>
            Virtual_String : LSP.Structures.Virtual_String;
      end case;
   end record;

   type ProgressToken_Optional (Is_Set : Boolean := False) is record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : ProgressToken;
      end case;
   end record;

   type PartialResultParams is interface;

   function partialResultToken
     (Self : PartialResultParams)
      return LSP.Structures.ProgressToken_Optional is abstract;
   --  An optional token that a server can use to report partial results (e.g.
   --  streaming) to the client.

   type StaticRegistrationOptions is interface;
   --  Static registration options to be returned in the initialize request.

   function id
     (Self : StaticRegistrationOptions)
      return Virtual_String_Optional is abstract;
   --  The id used to register the request. The id can be used to deregister
   --  the request again. See also Registration#id.

   type WorkDoneProgressOptions is interface;

   function workDoneProgress
     (Self : WorkDoneProgressOptions) return Boolean_Optional is abstract;

   type WorkDoneProgressParams is interface;

   function workDoneToken
     (Self : WorkDoneProgressParams)
      return LSP.Structures.ProgressToken_Optional is abstract;
   --  An optional token that a server can use to report work done progress.

   type AlsCheckSyntaxParams is record
      input : LSP.Structures.Virtual_String;
      --  Text to check syntax.

      rules : LSP.Structures.Virtual_String_Vector;
      --  Libadalang `Ada_Node_Kind_Type` values.

   end record;

   type AlsCheckSyntaxResult is record
      diagnostic : Virtual_String_Optional;
      --  Diagnostic from check syntax.

   end record;

   type Position is record
      line : Natural;
      --  Line position in a document (zero-based).
      --
      --  If a line number is greater than the number of lines in a document,
      --  it defaults back to the number of lines in the document. If a line
      --  number is negative, it defaults to 0.

      character : Natural;
      --  Character offset on a line in a document (zero-based).
      --
      --  The meaning of this offset is determined by the negotiated
      --  `PositionEncodingKind`.
      --
      --  If the character value is greater than the line length it defaults
      --  back to the line length.

   end record;
   --  Position in a text document expressed as zero-based line and character
   --  offset. Prior to 3.17 the offsets were always based on a UTF-16 string
   --  representation. So a string of the form `a["010400"]b` the character
   --  offset of the character `a` is 0, the character offset of `["010400"]`
   --  is 1 and the character offset of b is 3 since `["010400"]` is
   --  represented using two code units in UTF-16. Since 3.17 clients and
   --  servers can agree on a different string encoding representation (e.g.
   --  UTF-8). The client announces it's supported encoding via the client
   --  capability [`general.positionEncodings`](#clientCapabilities). The value
   --  is an array of position encodings the client supports, with decreasing
   --  preference (e.g. the encoding at index `0` is the most preferred one).
   --  To stay backwards compatible the only mandatory encoding is UTF-16
   --  represented via the string `utf-16`. The server can pick one of
   --  the encodings offered by the client and signals that encoding
   --  back to the client via the initialize result's property
   --  [`capabilities.positionEncoding`](#serverCapabilities). If the
   --  string value `utf-16` is missing from the client's capability
   --  `general.positionEncodings` servers can safely assume that the client
   --  supports UTF-16. If the server omits the position encoding in its
   --  initialize result the encoding defaults to the string value `utf-16`.
   --  Implementation considerations: since the conversion from one encoding
   --  into another requires the content of the file / line the conversion is
   --  best done where the file is read which is usually on the server side.
   --
   --  Positions are line end character agnostic. So you can not specify
   --  a position that denotes `\r|\n` or `\n|` where `|` represents the
   --  character offset.
   --
   --  @since 3.17.0 - support for negotiated position encoding.

   type A_Range is record
      start : LSP.Structures.Position;
      --  The range's start position.

      an_end : LSP.Structures.Position;
      --  The range's end position.

   end record;
   --  A range in a text document expressed as (zero-based) start and end
   --  positions.
   --
   --  If you want to specify a range that contains a line including the line
   --  ending character(s) then use an end position denoting the start of the
   --  next line. For example: ```ts {
   --      start: { line: 5, character: 23 }
   --      end : { line 6, character : 0 }
   --  }
   --  ```

   type TextEdit is tagged record
      a_range : LSP.Structures.A_Range;
      --  The range of the text document to be manipulated. To insert text into
      --  a document create a range where start === end.

      newText : LSP.Structures.Virtual_String;
      --  The string to be inserted. For delete operations use an empty string.

   end record;
   --  A text edit applicable to a text document.

   type ChangeAnnotationIdentifier is
   new VSS.Strings.Virtual_String with null record;
   --  An identifier to refer to a change annotation stored with a workspace
   --  edit.

   function Get_Hash
     (Self : ChangeAnnotationIdentifier) return Ada.Containers.Hash_Type is
     (Ada.Containers.Hash_Type'Mod (Self.Hash));

   type AnnotatedTextEdit is new TextEdit with record
      annotationId : LSP.Structures.ChangeAnnotationIdentifier;
      --  The actual identifier of the change annotation

   end record;
   --  A special text edit with an additional change annotation.
   --
   --  @since 3.16.0.

   type RenameFileOptions is record
      overwrite : Boolean_Optional;
      --  Overwrite target if existing. Overwrite wins over `ignoreIfExists`

      ignoreIfExists : Boolean_Optional;
      --  Ignores if target exists.

   end record;
   --  Rename file options

   type RenameFileOptions_Optional (Is_Set : Boolean := False) is record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : RenameFileOptions;
      end case;
   end record;

   type ChangeAnnotationIdentifier_Optional (Is_Set : Boolean := False) is
   record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : ChangeAnnotationIdentifier;
      end case;
   end record;

   type ResourceOperation is tagged record
      kind : LSP.Structures.Virtual_String;
      --  The resource operation kind.

      annotationId : ChangeAnnotationIdentifier_Optional;
      --  An optional annotation identifier describing the operation.
      --
      --  @since 3.16.0

   end record;
   --  A generic resource operation.

   type RenameFile is new ResourceOperation with record
      oldUri : LSP.Structures.DocumentUri;
      --  The old (existing) location.

      newUri : LSP.Structures.DocumentUri;
      --  The new location.

      options : RenameFileOptions_Optional;
      --  Rename options.

   end record;
   --  Rename file operation

   type TextDocumentIdentifier is tagged record
      uri : LSP.Structures.DocumentUri;
      --  The text document's uri.

   end record;
   --  A literal to identify a text document in the client.

   type Null_Record is null record;

   type Integer_Or_Null (Is_Null : Boolean := True) is record
      case Is_Null is
         when True =>
            null;
         when False =>
            Value : Standard.Integer;
      end case;
   end record;

   type OptionalVersionedTextDocumentIdentifier is
   new TextDocumentIdentifier with record
      version : LSP.Structures.Integer_Or_Null;
      --  The version number of this document. If a versioned text document
      --  identifier is sent from the server to the client and the file is not
      --  open in the editor (the server has not received an open notification
      --  before) the server can send `null` to indicate that the version
      --  is unknown and the content on disk is the truth (as specified
      --  with document content ownership).

   end record;
   --  A text document identifier to optionally denote a specific version of a
   --  text document.

   type TextEdit_Or_AnnotatedTextEdit (Is_TextEdit : Boolean := True) is record
      case Is_TextEdit is
         when True =>
            TextEdit : LSP.Structures.TextEdit;
         when False =>
            AnnotatedTextEdit : LSP.Structures.AnnotatedTextEdit;
      end case;
   end record;

   package TextEdit_Or_AnnotatedTextEdit_Vectors is new Ada.Containers.Vectors
     (Positive, TextEdit_Or_AnnotatedTextEdit, "=");

   type TextEdit_Or_AnnotatedTextEdit_Vector is
   new TextEdit_Or_AnnotatedTextEdit_Vectors.Vector with null record;

   type TextDocumentEdit is record
      textDocument : LSP.Structures.OptionalVersionedTextDocumentIdentifier;
      --  The text document to change.

      edits : LSP.Structures.TextEdit_Or_AnnotatedTextEdit_Vector;
      --  The edits to be applied.
      --
      --  @since 3.16.0 - support for AnnotatedTextEdit. This is guarded using
      --  a client capability.

   end record;
   --  Describes textual changes on a text document. A TextDocumentEdit
   --  describes all changes on a document version Si and after they are
   --  applied move the document to version Si+1. So the creator of a
   --  TextDocumentEdit doesn't need to sort the array of edits or do any
   --  kind of ordering. However the edits must be non overlapping.

   type DeleteFileOptions is record
      recursive : Boolean_Optional;
      --  Delete the content recursively if a folder is denoted.

      ignoreIfNotExists : Boolean_Optional;
      --  Ignore the operation if the file doesn't exist.

   end record;
   --  Delete file options

   type DeleteFileOptions_Optional (Is_Set : Boolean := False) is record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : DeleteFileOptions;
      end case;
   end record;

   type DeleteFile is new ResourceOperation with record
      uri : LSP.Structures.DocumentUri;
      --  The file to delete.

      options : DeleteFileOptions_Optional;
      --  Delete options.

   end record;
   --  Delete file operation

   type CreateFileOptions is record
      overwrite : Boolean_Optional;
      --  Overwrite existing file. Overwrite wins over `ignoreIfExists`

      ignoreIfExists : Boolean_Optional;
      --  Ignore if exists.

   end record;
   --  Options to create a file.

   type CreateFileOptions_Optional (Is_Set : Boolean := False) is record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : CreateFileOptions;
      end case;
   end record;

   type CreateFile is new ResourceOperation with record
      uri : LSP.Structures.DocumentUri;
      --  The resource to create.

      options : CreateFileOptions_Optional;
      --  Additional options

   end record;
   --  Create file operation.

   type ChangeAnnotation is record
      label : LSP.Structures.Virtual_String;
      --  A human-readable string describing the actual change. The string is
      --  rendered prominent in the user interface.

      needsConfirmation : Boolean_Optional;
      --  A flag which indicates that user confirmation is needed before
      --  applying the change.

      description : Virtual_String_Optional;
      --  A human-readable string which is rendered less prominent in the user
      --  interface.

   end record;
   --  Additional information that describes document changes.
   --
   --  @since 3.16.0

   package TextEdit_Vectors is new Ada.Containers.Vectors
     (Positive, TextEdit, "=");

   type TextEdit_Vector is new TextEdit_Vectors.Vector with null record;

   package TextEdit_Vector_Maps is new Ada.Containers.Hashed_Maps
     (DocumentUri, TextEdit_Vector, Get_Hash, "=");

   type changes_OfWorkspaceEdit is
   new TextEdit_Vector_Maps.Map with null record;

   type documentChanges_OfWorkspaceEdit_Item_Variant is
     (Variant_1, create, rename, delete);

   type documentChanges_OfWorkspaceEdit_Item
     (Kind : documentChanges_OfWorkspaceEdit_Item_Variant :=
        documentChanges_OfWorkspaceEdit_Item_Variant'First)
   is
   record
      case Kind is
         when Variant_1 =>
            Variant_1 : LSP.Structures.TextDocumentEdit;
         when create =>
            create : LSP.Structures.CreateFile;
         when rename =>
            rename : LSP.Structures.RenameFile;
         when delete =>
            delete : LSP.Structures.DeleteFile;
      end case;
   end record;

   package documentChanges_OfWorkspaceEdit_Item_Vectors is new Ada.Containers
     .Vectors
     (Positive, documentChanges_OfWorkspaceEdit_Item, "=");

   type documentChanges_OfWorkspaceEdit is
   new documentChanges_OfWorkspaceEdit_Item_Vectors.Vector with null record;

   package ChangeAnnotation_Maps is new Ada.Containers.Hashed_Maps
     (ChangeAnnotationIdentifier, ChangeAnnotation, Get_Hash, "=");

   type changeAnnotations_OfWorkspaceEdit is
   new ChangeAnnotation_Maps.Map with null record;

   type WorkspaceEdit is record
      changes : LSP.Structures.changes_OfWorkspaceEdit;
      --  Holds changes to existing resources.

      documentChanges : LSP.Structures.documentChanges_OfWorkspaceEdit;
      --  Depending on the client capability
      --  `workspace.workspaceEdit.resourceOperations` document changes are
      --  either an array of `TextDocumentEdit`s to express changes to n
      --  different text documents where each text document edit addresses
      --  a specific version of a text document. Or it can contain above
      --  `TextDocumentEdit`s mixed with create, rename and delete file /
      --  folder operations.
      --
      --  Whether a client supports versioned document edits is expressed via
      --  `workspace.workspaceEdit.documentChanges` client capability.
      --
      --  If a client neither supports `documentChanges` nor
      --  `workspace.workspaceEdit.resourceOperations` then only
      --  plain `TextEdit`s using the `changes` property are supported.

      changeAnnotations : LSP.Structures.changeAnnotations_OfWorkspaceEdit;
      --  A map of change annotations that can be referenced in
      --  `AnnotatedTextEdit`s or create, rename and delete file /
      --  folder operations.
      --
      --  Whether clients honor this property depends on the client capability
      --  `workspace.changeAnnotationSupport`.
      --
      --  @since 3.16.0

   end record;
   --  A workspace edit represents changes to many resources managed
   --  in the workspace. The edit should either provide `changes` or
   --  `documentChanges`. If documentChanges are present they are preferred
   --  over `changes` if the client can handle versioned document edits.
   --
   --  Since version 3.13.0 a workspace edit can contain resource operations
   --  as well. If resource operations are present clients need to execute the
   --  operations in the order in which they are provided. So a workspace edit
   --  for example can consist of the following two changes: (1) a create file
   --  a.txt and (2) a text document edit which insert text into file a.txt.
   --
   --  An invalid sequence (e.g. (1) delete file a.txt and (2) insert text
   --  into file a.txt) will cause failure of the operation. How the client
   --  recovers from the failure is described by the client capability:
   --  `workspace.workspaceEdit.failureHandling`

   type ApplyWorkspaceEditParams is record
      label : Virtual_String_Optional;
      --  An optional label of the workspace edit. This label is presented in
      --  the user interface for example on an undo stack to undo the workspace
      --  edit.

      edit : LSP.Structures.WorkspaceEdit;
      --  The edits to apply.

   end record;
   --  The parameters passed via a apply workspace edit request.

   type ApplyWorkspaceEditResult is record
      applied : Standard.Boolean;
      --  Indicates whether the edit was applied or not.

      failureReason : Virtual_String_Optional;
      --  An optional textual description for why the edit was not applied.
      --  This may be used by the server for diagnostic logging or to provide
      --  a suitable error for a request that triggered the edit.

      failedChange : Natural_Optional;
      --  Depending on the client's failure handling strategy `failedChange`
      --  might contain the index of the change that failed. This property is
      --  only available if the client signals a `failureHandlingStrategy` in
      --  its client capabilities.

   end record;
   --  The result returned from the apply workspace edit request.
   --
   --  @since 3.17 renamed from ApplyWorkspaceEditResponse

   type SymbolTag_Set is array (SymbolTag) of Boolean with
     Pack, Default_Component_Value => False;

   type BaseSymbolInformation is tagged record
      name : LSP.Structures.Virtual_String;
      --  The name of this symbol.

      kind : LSP.Enumerations.SymbolKind;
      --  The kind of this symbol.

      tags : LSP.Structures.SymbolTag_Set;
      --  Tags for this symbol.
      --
      --  @since 3.16.0

      containerName : Virtual_String_Optional;
      --  The name of the symbol containing this symbol. This information is
      --  for user interface purposes (e.g. to render a qualifier in the user
      --  interface if necessary). It can't be used to re-infer a hierarchy for
      --  the document symbols.

   end record;
   --  A base for all symbol information.

   type CallHierarchyClientCapabilities is record
      dynamicRegistration : Boolean_Optional;
      --  Whether implementation supports dynamic registration.
      --  If this is set to `true` the client supports the new
      --  `(TextDocumentRegistrationOptions & StaticRegistrationOptions)`
      --  return value for the corresponding server capability as well.

   end record;
   --  @since 3.16.0

   type LSPAny is new JSON_Event_Vectors.Vector with null record;
   --  The LSP any type.
   --  Please note that strictly speaking a property with the value `undefined`
   --  can't be converted into JSON preserving the property name. However for
   --  convenience it is allowed and assumed that all these properties are
   --  optional as well. @since 3.17.0

   subtype LSPAny_Vector is LSPAny;

   subtype LSPAny_Optional is LSPAny;

   type CallHierarchyItem is record
      name : LSP.Structures.Virtual_String;
      --  The name of this item.

      kind : LSP.Enumerations.SymbolKind;
      --  The kind of this item.

      tags : LSP.Structures.SymbolTag_Set;
      --  Tags for this item.

      detail : Virtual_String_Optional;
      --  More detail for this item, e.g. the signature of a function.

      uri : LSP.Structures.DocumentUri;
      --  The resource identifier of this item.

      a_range : LSP.Structures.A_Range;
      --  The range enclosing this symbol not including leading/trailing
      --  whitespace but everything else, e.g. comments and code.

      selectionRange : LSP.Structures.A_Range;
      --  The range that should be selected and revealed when this symbol is
      --  being picked, e.g. the name of a function. Must be contained by the
      --  [`range`](#CallHierarchyItem.range).

      data : LSPAny_Optional;
      --  A data entry field that is preserved between a call hierarchy prepare
      --  and incoming calls or outgoing calls requests.

   end record;
   --  Represents programming constructs like functions or constructors in the
   --  context of call hierarchy.
   --
   --  @since 3.16.0

   package Range_Vectors is new Ada.Containers.Vectors
     (Positive, A_Range, "=");

   type Range_Vector is new Range_Vectors.Vector with null record;

   package Boolean_Vectors is new Ada.Containers.Vectors
     (Positive, Boolean, "=");

   type Boolean_Vector is new Boolean_Vectors.Vector with null record;

   type CallHierarchyIncomingCall is record
      from : LSP.Structures.CallHierarchyItem;
      --  The item that makes the call.

      fromRanges : LSP.Structures.Range_Vector;
      --  The ranges at which the calls appear. This is relative to the caller
      --  denoted by [`this.from`](#CallHierarchyIncomingCall.from).

      dispatching_calls : LSP.Structures.Boolean_Vector;

   end record;
   --  Represents an incoming call, e.g. a caller of a method or constructor.
   --
   --  @since 3.16.0

   type CallHierarchyIncomingCallsParams is
   new WorkDoneProgressParams and PartialResultParams with record
      workDoneToken : ProgressToken_Optional;
      --  An optional token that a server can use to report work done progress.

      partialResultToken : ProgressToken_Optional;
      --  An optional token that a server can use to report partial results
      --  (e.g. streaming) to the client.

      item : LSP.Structures.CallHierarchyItem;

   end record;
   --  The parameter of a `callHierarchy/incomingCalls` request.
   --
   --  @since 3.16.0

   overriding function workDoneToken
     (Self : CallHierarchyIncomingCallsParams)
      return LSP.Structures.ProgressToken_Optional is
     (Self.workDoneToken);

   overriding function partialResultToken
     (Self : CallHierarchyIncomingCallsParams)
      return LSP.Structures.ProgressToken_Optional is
     (Self.partialResultToken);

   type CallHierarchyOptions is new WorkDoneProgressOptions with record
      workDoneProgress : Boolean_Optional;

   end record;
   --  Call hierarchy options used during static registration.
   --
   --  @since 3.16.0

   overriding function workDoneProgress
     (Self : CallHierarchyOptions) return Boolean_Optional is
     (Self.workDoneProgress);

   type CallHierarchyOutgoingCall is record
      to : LSP.Structures.CallHierarchyItem;
      --  The item that is called.

      fromRanges : LSP.Structures.Range_Vector;
      --  The range at which this item is called. This is the
      --  range relative to the caller, e.g the item passed to
      --  [`provideCallHierarchyOutgoingCalls`](#CallHierarchyItemProvider.provideCallHierarchyOutgoingCalls)
      --  and not [`this.to`](#CallHierarchyOutgoingCall.to).

      dispatching_calls : LSP.Structures.Boolean_Vector;

   end record;
   --  Represents an outgoing call, e.g. calling a getter from a method or a
   --  method from a constructor etc.
   --
   --  @since 3.16.0

   type CallHierarchyOutgoingCallsParams is
   new WorkDoneProgressParams and PartialResultParams with record
      workDoneToken : ProgressToken_Optional;
      --  An optional token that a server can use to report work done progress.

      partialResultToken : ProgressToken_Optional;
      --  An optional token that a server can use to report partial results
      --  (e.g. streaming) to the client.

      item : LSP.Structures.CallHierarchyItem;

   end record;
   --  The parameter of a `callHierarchy/outgoingCalls` request.
   --
   --  @since 3.16.0

   overriding function workDoneToken
     (Self : CallHierarchyOutgoingCallsParams)
      return LSP.Structures.ProgressToken_Optional is
     (Self.workDoneToken);

   overriding function partialResultToken
     (Self : CallHierarchyOutgoingCallsParams)
      return LSP.Structures.ProgressToken_Optional is
     (Self.partialResultToken);

   type TextDocumentPositionParams is tagged record
      textDocument : LSP.Structures.TextDocumentIdentifier;
      --  The text document.

      position : LSP.Structures.Position;
      --  The position inside the text document.

   end record;
   --  A parameter literal used in requests to pass a text document and a
   --  position inside that document.

   type CallHierarchyPrepareParams is
   new TextDocumentPositionParams and WorkDoneProgressParams with record
      workDoneToken : ProgressToken_Optional;
      --  An optional token that a server can use to report work done progress.

   end record;
   --  The parameter of a `textDocument/prepareCallHierarchy` request.
   --
   --  @since 3.16.0

   overriding function workDoneToken
     (Self : CallHierarchyPrepareParams)
      return LSP.Structures.ProgressToken_Optional is
     (Self.workDoneToken);

   type TextDocumentFilter is record
      language : Virtual_String_Optional;
      --  A language id, like `typescript`. */

      scheme : Virtual_String_Optional;
      --  A Uri [scheme](#Uri.scheme), like `file` or `untitled`. */

      pattern : Virtual_String_Optional;
      --  A glob pattern, like `*.{ts,js}`. */

   end record;
   --  A document filter denotes a document by different properties like
   --  the [language](#TextDocument.languageId), the [scheme](#Uri.scheme)
   --  of its resource, or a glob-pattern that is applied to the
   --  [path](#TextDocument.fileName).
   --
   --  Glob patterns can have the following syntax: - `*` to match one or
   --  more characters in a path segment - `?` to match on one character in
   --  a path segment - `**` to match any number of path segments, including
   --  none - `{}` to group sub patterns into an OR expression. (e.g.
   --  `**["200B"]/*.{ts,js}` matches all TypeScript and JavaScript files) -
   --  `[]` to declare a range of characters to match in a path segment (e.g.,
   --  `example.[0-9]` to match on `example.0`, `example.1`, ["2026"]) -
   --  `[!...]` to negate a range of characters to match in a path segment
   --  (e.g., `example.[!0-9]` to match on `example.a`, `example.b`, but not
   --  `example.0`)
   --
   --  @sample A language filter that applies to typescript files on disk:
   --  `{ language: 'typescript', scheme: 'file' }` @sample A language filter
   --  that applies to all package.json paths: `{ language: 'json', pattern:
   --  '**package.json' }`
   --
   --  @since 3.17.0

   type NotebookDocumentFilter is record
      notebookType : Virtual_String_Optional;
      --  The type of the enclosing notebook. */

      scheme : Virtual_String_Optional;
      --  A Uri [scheme](#Uri.scheme), like `file` or `untitled`. */

      pattern : Virtual_String_Optional;
      --  A glob pattern. */

   end record;
   --  A notebook document filter denotes a notebook document by different
   --  properties. The properties will be match against the notebook's URI
   --  (same as with documents)
   --
   --  @since 3.17.0

   type Virtual_String_Or_NotebookDocumentFilter
     (Is_Virtual_String : Boolean := True) is
   record
      case Is_Virtual_String is
         when True =>
            Virtual_String : LSP.Structures.Virtual_String;
         when False =>
            NotebookDocumentFilter : LSP.Structures.NotebookDocumentFilter;
      end case;
   end record;

   type NotebookCellTextDocumentFilter is record
      notebook : LSP.Structures.Virtual_String_Or_NotebookDocumentFilter;
      --  A filter that matches against the notebook containing the notebook
      --  cell. If a string value is provided it matches against the notebook
      --  type. '*' matches every notebook.

      language : Virtual_String_Optional;
      --  A language id like `python`.
      --
      --  Will be matched against the language id of the notebook cell
      --  document. '*' matches every language.

   end record;
   --  A notebook cell text document filter denotes a cell text document by
   --  different properties.
   --
   --  @since 3.17.0

   type DocumentFilter (Is_TextDocumentFilter : Boolean := True) is record
      case Is_TextDocumentFilter is
         when True =>
            TextDocumentFilter : LSP.Structures.TextDocumentFilter;
         when False =>
            NotebookCellTextDocumentFilter : LSP.Structures
              .NotebookCellTextDocumentFilter;
      end case;
   end record;
   --  A document filter describes a top level text document or a notebook cell
   --  document.
   --
   --  @since 3.17.0 - proposed support for NotebookCellTextDocumentFilter.

   type Virtual_String_Or_DocumentFilter (Is_Virtual_String : Boolean := True)
   is
   record
      case Is_Virtual_String is
         when True =>
            Virtual_String : LSP.Structures.Virtual_String;
         when False =>
            DocumentFilter : LSP.Structures.DocumentFilter;
      end case;
   end record;

   package Virtual_String_Or_DocumentFilter_Vectors is new Ada.Containers
     .Vectors
     (Positive, Virtual_String_Or_DocumentFilter, "=");

   type DocumentSelector is
   new Virtual_String_Or_DocumentFilter_Vectors.Vector with null record;

   --  A document selector is the combination of one or many document filters.
   --
   --  @sample `let sel:DocumentSelector = [{ language: 'typescript' }, {
   --  language: 'json', pattern: '**["2215"]tsconfig.json' }]`;
   --
   --  The use of a string as a document filter is deprecated @since 3.16.0.

   type DocumentSelector_Or_Null (Is_Null : Boolean := True) is record
      case Is_Null is
         when True =>
            null;
         when False =>
            Value : DocumentSelector;
      end case;
   end record;

   type TextDocumentRegistrationOptions is tagged record
      documentSelector : LSP.Structures.DocumentSelector_Or_Null;
      --  A document selector to identify the scope of the registration. If
      --  set to null the document selector provided on the client side will
      --  be used.

   end record;
   --  General text document registration options.

   type CallHierarchyRegistrationOptions is
   new CallHierarchyOptions and StaticRegistrationOptions with record
      id : Virtual_String_Optional;
      --  The id used to register the request. The id can be used to deregister
      --  the request again. See also Registration#id.

      Parent : LSP.Structures.TextDocumentRegistrationOptions;
   end record;
   --  Call hierarchy options used during static or dynamic registration.
   --
   --  @since 3.16.0

   overriding function id
     (Self : CallHierarchyRegistrationOptions)
      return Virtual_String_Optional is
     (Self.id);

   type Integer_Or_Virtual_String (Is_Integer : Boolean := True) is record
      case Is_Integer is
         when True =>
            Integer : Standard.Integer;
         when False =>
            Virtual_String : LSP.Structures.Virtual_String;
      end case;
   end record;

   type CancelParams is record
      id : LSP.Structures.Integer_Or_Virtual_String;
      --  The request id to cancel.

   end record;

   type ShowDocumentClientCapabilities is record
      support : Standard.Boolean;
      --  The client has support for the showDocument request.

   end record;
   --  Client capabilities for the showDocument request.
   --
   --  @since 3.16.0

   type ShowDocumentClientCapabilities_Optional (Is_Set : Boolean := False) is
   record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : ShowDocumentClientCapabilities;
      end case;
   end record;

   type messageActionItem_OfShowMessageRequestClientCapabilities is record
      additionalPropertiesSupport : Boolean_Optional;
      --  Whether the client supports additional attributes which are preserved
      --  and send back to the server in the request's response.

   end record;
   type messageActionItem_OfShowMessageRequestClientCapabilities_Optional
     (Is_Set : Boolean := False) is
   record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : messageActionItem_OfShowMessageRequestClientCapabilities;
      end case;
   end record;

   type ShowMessageRequestClientCapabilities is record
      messageActionItem : messageActionItem_OfShowMessageRequestClientCapabilities_Optional;
      --  Capabilities specific to the `MessageActionItem` type.

   end record;
   --  Show message request client capabilities

   type ShowMessageRequestClientCapabilities_Optional
     (Is_Set : Boolean := False) is
   record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : ShowMessageRequestClientCapabilities;
      end case;
   end record;

   type WindowClientCapabilities is record
      workDoneProgress : Boolean_Optional;
      --  It indicates whether the client supports server initiated progress
      --  using the `window/workDoneProgress/create` request.
      --
      --  The capability also controls Whether client supports handling
      --  of progress notifications. If set servers are allowed to report
      --  a `workDoneProgress` property in the request specific server
      --  capabilities.
      --
      --  @since 3.15.0

      showMessage : ShowMessageRequestClientCapabilities_Optional;
      --  Capabilities specific to the showMessage request.
      --
      --  @since 3.16.0

      showDocument : ShowDocumentClientCapabilities_Optional;
      --  Capabilities specific to the showDocument request.
      --
      --  @since 3.16.0

   end record;

   type WindowClientCapabilities_Optional (Is_Set : Boolean := False) is record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : WindowClientCapabilities;
      end case;
   end record;

   type FailureHandlingKind_Optional (Is_Set : Boolean := False) is record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : FailureHandlingKind;
      end case;
   end record;

   type ResourceOperationKind_Set is
     array (ResourceOperationKind) of Boolean with
     Pack, Default_Component_Value => False;

   type changeAnnotationSupport_OfWorkspaceEditClientCapabilities is record
      groupsOnLabel : Boolean_Optional;
      --  Whether the client groups edits with equal labels into tree nodes,
      --  for instance all edits labelled with "Changes in Strings" would be
      --  a tree node.

   end record;
   type changeAnnotationSupport_OfWorkspaceEditClientCapabilities_Optional
     (Is_Set : Boolean := False) is
   record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : changeAnnotationSupport_OfWorkspaceEditClientCapabilities;
      end case;
   end record;

   type WorkspaceEditClientCapabilities is record
      documentChanges : Boolean_Optional;
      --  The client supports versioned document changes in `WorkspaceEdit`s

      resourceOperations : LSP.Structures.ResourceOperationKind_Set;
      --  The resource operations the client supports. Clients should at least
      --  support 'create', 'rename' and 'delete' files and folders.
      --
      --  @since 3.13.0

      failureHandling : FailureHandlingKind_Optional;
      --  The failure handling strategy of a client if applying the workspace
      --  edit fails.
      --
      --  @since 3.13.0

      normalizesLineEndings : Boolean_Optional;
      --  Whether the client normalizes line endings to the client specific
      --  setting. If set to `true` the client will normalize line ending
      --  characters in a workspace edit to the client-specified new line
      --  character.
      --
      --  @since 3.16.0

      changeAnnotationSupport : changeAnnotationSupport_OfWorkspaceEditClientCapabilities_Optional;
      --  Whether the client in general supports change annotations on text
      --  edits, create file, rename file and delete file changes.
      --
      --  @since 3.16.0

   end record;

   type WorkspaceEditClientCapabilities_Optional (Is_Set : Boolean := False) is
   record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : WorkspaceEditClientCapabilities;
      end case;
   end record;

   type InlayHintWorkspaceClientCapabilities is record
      refreshSupport : Boolean_Optional;
      --  Whether the client implementation supports a refresh request sent
      --  from the server to the client.
      --
      --  Note that this event is global and will force the client to refresh
      --  all inlay hints currently shown. It should be used with absolute
      --  care and is useful for situation where a server for example detects
      --  a project wide change that requires such a calculation.

   end record;
   --  Client workspace capabilities specific to inlay hints.
   --
   --  @since 3.17.0

   type InlayHintWorkspaceClientCapabilities_Optional
     (Is_Set : Boolean := False) is
   record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : InlayHintWorkspaceClientCapabilities;
      end case;
   end record;

   type DiagnosticWorkspaceClientCapabilities is record
      refreshSupport : Boolean_Optional;
      --  Whether the client implementation supports a refresh request sent
      --  from the server to the client.
      --
      --  Note that this event is global and will force the client to refresh
      --  all pulled diagnostics currently shown. It should be used with
      --  absolute care and is useful for situation where a server for example
      --  detects a project wide change that requires such a calculation.

   end record;
   --  Workspace client capabilities specific to diagnostic pull requests.
   --
   --  @since 3.17.0

   type DiagnosticWorkspaceClientCapabilities_Optional
     (Is_Set : Boolean := False) is
   record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : DiagnosticWorkspaceClientCapabilities;
      end case;
   end record;

   type ExecuteCommandClientCapabilities is record
      dynamicRegistration : Boolean_Optional;
      --  Execute command supports dynamic registration.

   end record;
   --  The client capabilities of a
   --  [ExecuteCommandRequest](#ExecuteCommandRequest).

   type ExecuteCommandClientCapabilities_Optional (Is_Set : Boolean := False)
   is
   record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : ExecuteCommandClientCapabilities;
      end case;
   end record;

   type DidChangeWatchedFilesClientCapabilities is record
      dynamicRegistration : Boolean_Optional;
      --  Did change watched files notification supports dynamic registration.
      --  Please note that the current protocol doesn't support static
      --  configuration for file changes from the server side.

      relativePatternSupport : Boolean_Optional;
      --  Whether the client has support for {@link RelativePattern relative
      --  pattern} or not.
      --
      --  @since 3.17.0

   end record;

   type DidChangeWatchedFilesClientCapabilities_Optional
     (Is_Set : Boolean := False) is
   record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : DidChangeWatchedFilesClientCapabilities;
      end case;
   end record;

   type InlineValueWorkspaceClientCapabilities is record
      refreshSupport : Boolean_Optional;
      --  Whether the client implementation supports a refresh request sent
      --  from the server to the client.
      --
      --  Note that this event is global and will force the client to refresh
      --  all inline values currently shown. It should be used with absolute
      --  care and is useful for situation where a server for example detects
      --  a project wide change that requires such a calculation.

   end record;
   --  Client workspace capabilities specific to inline values.
   --
   --  @since 3.17.0

   type InlineValueWorkspaceClientCapabilities_Optional
     (Is_Set : Boolean := False) is
   record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : InlineValueWorkspaceClientCapabilities;
      end case;
   end record;

   type FileOperationClientCapabilities is record
      dynamicRegistration : Boolean_Optional;
      --  Whether the client supports dynamic registration for file
      --  requests/notifications.

      didCreate : Boolean_Optional;
      --  The client has support for sending didCreateFiles notifications.

      willCreate : Boolean_Optional;
      --  The client has support for sending willCreateFiles requests.

      didRename : Boolean_Optional;
      --  The client has support for sending didRenameFiles notifications.

      willRename : Boolean_Optional;
      --  The client has support for sending willRenameFiles requests.

      didDelete : Boolean_Optional;
      --  The client has support for sending didDeleteFiles notifications.

      willDelete : Boolean_Optional;
      --  The client has support for sending willDeleteFiles requests.

   end record;
   --  Capabilities relating to events from file operations by the user in the
   --  client.
   --
   --  These events do not come from the file system, they come from user
   --  operations like renaming a file in the UI.
   --
   --  @since 3.16.0

   type FileOperationClientCapabilities_Optional (Is_Set : Boolean := False) is
   record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : FileOperationClientCapabilities;
      end case;
   end record;

   type DidChangeConfigurationClientCapabilities is record
      dynamicRegistration : Boolean_Optional;
      --  Did change configuration notification supports dynamic registration.

   end record;

   type DidChangeConfigurationClientCapabilities_Optional
     (Is_Set : Boolean := False) is
   record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : DidChangeConfigurationClientCapabilities;
      end case;
   end record;

   type SymbolKind_Set is array (SymbolKind) of Boolean with
     Pack, Default_Component_Value => False;

   type symbolKind_OfWorkspaceSymbolClientCapabilities is record
      valueSet : LSP.Structures.SymbolKind_Set;
      --  The symbol kind values the client supports. When this property exists
      --  the client also guarantees that it will handle values outside its set
      --  gracefully and falls back to a default value when unknown.
      --
      --  If this property is not present the client only supports the symbol
      --  kinds from `File` to `Array` as defined in the initial version of the
      --  protocol.

   end record;
   type symbolKind_OfWorkspaceSymbolClientCapabilities_Optional
     (Is_Set : Boolean := False) is
   record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : symbolKind_OfWorkspaceSymbolClientCapabilities;
      end case;
   end record;

   type tagSupport_OfWorkspaceSymbolClientCapabilities is record
      valueSet : LSP.Structures.SymbolTag_Set;
      --  The tags supported by the client.

   end record;
   type tagSupport_OfWorkspaceSymbolClientCapabilities_Optional
     (Is_Set : Boolean := False) is
   record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : tagSupport_OfWorkspaceSymbolClientCapabilities;
      end case;
   end record;

   type resolveSupport_OfWorkspaceSymbolClientCapabilities is record
      properties : LSP.Structures.Virtual_String_Vector;
      --  The properties that a client can resolve lazily. Usually
      --  `location.range`

   end record;
   type resolveSupport_OfWorkspaceSymbolClientCapabilities_Optional
     (Is_Set : Boolean := False) is
   record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : resolveSupport_OfWorkspaceSymbolClientCapabilities;
      end case;
   end record;

   type WorkspaceSymbolClientCapabilities is record
      dynamicRegistration : Boolean_Optional;
      --  Symbol request supports dynamic registration.

      symbolKind : symbolKind_OfWorkspaceSymbolClientCapabilities_Optional;
      --  Specific capabilities for the `SymbolKind` in the `workspace/symbol`
      --  request.

      tagSupport : tagSupport_OfWorkspaceSymbolClientCapabilities_Optional;
      --  The client supports tags on `SymbolInformation`. Clients supporting
      --  tags have to handle unknown tags gracefully.
      --
      --  @since 3.16.0

      resolveSupport : resolveSupport_OfWorkspaceSymbolClientCapabilities_Optional;
      --  The client support partial workspace symbols. The client will send
      --  the request `workspaceSymbol/resolve` to the server to resolve
      --  additional properties.
      --
      --  @since 3.17.0

   end record;
   --  Client capabilities for a
   --  [WorkspaceSymbolRequest](#WorkspaceSymbolRequest).

   type WorkspaceSymbolClientCapabilities_Optional (Is_Set : Boolean := False)
   is
   record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : WorkspaceSymbolClientCapabilities;
      end case;
   end record;

   type SemanticTokensWorkspaceClientCapabilities is record
      refreshSupport : Boolean_Optional;
      --  Whether the client implementation supports a refresh request sent
      --  from the server to the client.
      --
      --  Note that this event is global and will force the client to refresh
      --  all semantic tokens currently shown. It should be used with absolute
      --  care and is useful for situation where a server for example detects a
      --  project wide change that requires such a calculation.

   end record;
   --  @since 3.16.0

   type SemanticTokensWorkspaceClientCapabilities_Optional
     (Is_Set : Boolean := False) is
   record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : SemanticTokensWorkspaceClientCapabilities;
      end case;
   end record;

   type CodeLensWorkspaceClientCapabilities is record
      refreshSupport : Boolean_Optional;
      --  Whether the client implementation supports a refresh request sent
      --  from the server to the client.
      --
      --  Note that this event is global and will force the client to refresh
      --  all code lenses currently shown. It should be used with absolute
      --  care and is useful for situation where a server for example detect
      --  a project wide change that requires such a calculation.

   end record;
   --  @since 3.16.0

   type CodeLensWorkspaceClientCapabilities_Optional
     (Is_Set : Boolean := False) is
   record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : CodeLensWorkspaceClientCapabilities;
      end case;
   end record;

   type WorkspaceClientCapabilities is record
      applyEdit : Boolean_Optional;
      --  The client supports applying batch edits to the workspace by
      --  supporting the request 'workspace/applyEdit'

      workspaceEdit : WorkspaceEditClientCapabilities_Optional;
      --  Capabilities specific to `WorkspaceEdit`s.

      didChangeConfiguration : DidChangeConfigurationClientCapabilities_Optional;
      --  Capabilities specific to the `workspace/didChangeConfiguration`
      --  notification.

      didChangeWatchedFiles : DidChangeWatchedFilesClientCapabilities_Optional;
      --  Capabilities specific to the `workspace/didChangeWatchedFiles`
      --  notification.

      symbol : WorkspaceSymbolClientCapabilities_Optional;
      --  Capabilities specific to the `workspace/symbol` request.

      executeCommand : ExecuteCommandClientCapabilities_Optional;
      --  Capabilities specific to the `workspace/executeCommand` request.

      workspaceFolders : Boolean_Optional;
      --  The client has support for workspace folders.
      --
      --  @since 3.6.0

      configuration : Boolean_Optional;
      --  The client supports `workspace/configuration` requests.
      --
      --  @since 3.6.0

      semanticTokens : SemanticTokensWorkspaceClientCapabilities_Optional;
      --  Capabilities specific to the semantic token requests scoped to the
      --  workspace.
      --
      --  @since 3.16.0.

      codeLens : CodeLensWorkspaceClientCapabilities_Optional;
      --  Capabilities specific to the code lens requests scoped to the
      --  workspace.
      --
      --  @since 3.16.0.

      fileOperations : FileOperationClientCapabilities_Optional;
      --  The client has support for file notifications/requests for user
      --  operations on files.
      --
      --  Since 3.16.0

      inlineValue : InlineValueWorkspaceClientCapabilities_Optional;
      --  Capabilities specific to the inline values requests scoped to the
      --  workspace.
      --
      --  @since 3.17.0.

      inlayHint : InlayHintWorkspaceClientCapabilities_Optional;
      --  Capabilities specific to the inlay hint requests scoped to the
      --  workspace.
      --
      --  @since 3.17.0.

      diagnostics : DiagnosticWorkspaceClientCapabilities_Optional;
      --  Capabilities specific to the diagnostic requests scoped to the
      --  workspace.
      --
      --  @since 3.17.0.

   end record;
   --  Workspace specific client capabilities.

   type WorkspaceClientCapabilities_Optional (Is_Set : Boolean := False) is
   record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : WorkspaceClientCapabilities;
      end case;
   end record;

   type RegularExpressionsClientCapabilities is record
      engine : LSP.Structures.Virtual_String;
      --  The engine's name.

      version : Virtual_String_Optional;
      --  The engine's version.

   end record;
   --  Client capabilities specific to regular expressions.
   --
   --  @since 3.16.0

   type RegularExpressionsClientCapabilities_Optional
     (Is_Set : Boolean := False) is
   record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : RegularExpressionsClientCapabilities;
      end case;
   end record;

   type MarkdownClientCapabilities is record
      parser : LSP.Structures.Virtual_String;
      --  The name of the parser.

      version : Virtual_String_Optional;
      --  The version of the parser.

      allowedTags : LSP.Structures.Virtual_String_Vector;
      --  A list of HTML tags that the client allows / supports in Markdown.
      --
      --  @since 3.17.0

   end record;
   --  Client capabilities specific to the used markdown parser.
   --
   --  @since 3.16.0

   type MarkdownClientCapabilities_Optional (Is_Set : Boolean := False) is
   record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : MarkdownClientCapabilities;
      end case;
   end record;

   type staleRequestSupport_OfGeneralClientCapabilities is record
      cancel : Standard.Boolean;
      --  The client will actively cancel the request.

      retryOnContentModified : LSP.Structures.Virtual_String_Vector;
      --  The list of requests for which the client will retry the request if
      --  it receives a response with error code `ContentModified`

   end record;
   type staleRequestSupport_OfGeneralClientCapabilities_Optional
     (Is_Set : Boolean := False) is
   record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : staleRequestSupport_OfGeneralClientCapabilities;
      end case;
   end record;

   package PositionEncodingKind_Vectors is new Ada.Containers.Vectors
     (Positive, PositionEncodingKind, "=");

   type PositionEncodingKind_Set is
   new PositionEncodingKind_Vectors.Vector with null record;

   type GeneralClientCapabilities is record
      staleRequestSupport : staleRequestSupport_OfGeneralClientCapabilities_Optional;
      --  Client capability that signals how the client handles stale requests
      --  (e.g. a request for which the client will not process the response
      --  anymore since the information is outdated).
      --
      --  @since 3.17.0

      regularExpressions : RegularExpressionsClientCapabilities_Optional;
      --  Client capabilities specific to regular expressions.
      --
      --  @since 3.16.0

      markdown : MarkdownClientCapabilities_Optional;
      --  Client capabilities specific to the client's markdown parser.
      --
      --  @since 3.16.0

      positionEncodings : LSP.Structures.PositionEncodingKind_Set;
      --  The position encodings supported by the client. Client and server
      --  have to agree on the same position encoding to ensure that offsets
      --  (e.g. character position in a line) are interpreted the same on both
      --  sides.
      --
      --  To keep the protocol backwards compatible the following applies: if
      --  the value 'utf-16' is missing from the array of position encodings
      --  servers can assume that the client supports UTF-16. UTF-16 is
      --  therefore a mandatory encoding.
      --
      --  If omitted it defaults to ['utf-16'].
      --
      --  Implementation considerations: since the conversion from one encoding
      --  into another requires the content of the file / line the conversion
      --  is best done where the file is read which is usually on the server
      --  side.
      --
      --  @since 3.17.0

   end record;
   --  General client capabilities.
   --
   --  @since 3.16.0

   type GeneralClientCapabilities_Optional (Is_Set : Boolean := False) is
   record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : GeneralClientCapabilities;
      end case;
   end record;

   type NotebookDocumentSyncClientCapabilities is record
      dynamicRegistration : Boolean_Optional;
      --  Whether implementation supports dynamic registration.
      --  If this is set to `true` the client supports the new
      --  `(TextDocumentRegistrationOptions & StaticRegistrationOptions)`
      --  return value for the corresponding server capability as well.

      executionSummarySupport : Boolean_Optional;
      --  The client supports sending execution summary data per cell.

   end record;
   --  Notebook specific client capabilities.
   --
   --  @since 3.17.0

   type NotebookDocumentClientCapabilities is record
      synchronization : LSP.Structures.NotebookDocumentSyncClientCapabilities;
      --  Capabilities specific to notebook document synchronization
      --
      --  @since 3.17.0

   end record;
   --  Capabilities specific to the notebook document support.
   --
   --  @since 3.17.0

   type NotebookDocumentClientCapabilities_Optional (Is_Set : Boolean := False)
   is
   record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : NotebookDocumentClientCapabilities;
      end case;
   end record;

   type DiagnosticClientCapabilities is record
      dynamicRegistration : Boolean_Optional;
      --  Whether implementation supports dynamic registration.
      --  If this is set to `true` the client supports the new
      --  `(TextDocumentRegistrationOptions & StaticRegistrationOptions)`
      --  return value for the corresponding server capability as well.

      relatedDocumentSupport : Boolean_Optional;
      --  Whether the clients supports related documents for document
      --  diagnostic pulls.

   end record;
   --  Client capabilities specific to diagnostic pull requests.
   --
   --  @since 3.17.0

   type DiagnosticClientCapabilities_Optional (Is_Set : Boolean := False) is
   record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : DiagnosticClientCapabilities;
      end case;
   end record;

   type CodeLensClientCapabilities is record
      dynamicRegistration : Boolean_Optional;
      --  Whether code lens supports dynamic registration.

   end record;
   --  The client capabilities of a [CodeLensRequest](#CodeLensRequest).

   type CodeLensClientCapabilities_Optional (Is_Set : Boolean := False) is
   record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : CodeLensClientCapabilities;
      end case;
   end record;

   type InlayHintClientCapabilities is record
      dynamicRegistration : Boolean_Optional;
      --  Whether inlay hints support dynamic registration.

      resolveSupport : resolveSupport_OfWorkspaceSymbolClientCapabilities_Optional;
      --  Indicates which properties a client can resolve lazily on an inlay
      --  hint.

   end record;
   --  Inlay hint client capabilities.
   --
   --  @since 3.17.0

   type InlayHintClientCapabilities_Optional (Is_Set : Boolean := False) is
   record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : InlayHintClientCapabilities;
      end case;
   end record;

   type DeclarationClientCapabilities is record
      dynamicRegistration : Boolean_Optional;
      --  Whether declaration supports dynamic registration. If this is set to
      --  `true` the client supports the new `DeclarationRegistrationOptions`
      --  return value for the corresponding server capability as well.

      linkSupport : Boolean_Optional;
      --  The client supports additional metadata in the form of declaration
      --  links.

   end record;
   --  @since 3.14.0

   type DeclarationClientCapabilities_Optional (Is_Set : Boolean := False) is
   record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : DeclarationClientCapabilities;
      end case;
   end record;

   type DocumentFormattingClientCapabilities is record
      dynamicRegistration : Boolean_Optional;
      --  Whether formatting supports dynamic registration.

   end record;
   --  Client capabilities of a
   --  [DocumentFormattingRequest](#DocumentFormattingRequest).

   type DocumentFormattingClientCapabilities_Optional
     (Is_Set : Boolean := False) is
   record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : DocumentFormattingClientCapabilities;
      end case;
   end record;

   type DiagnosticTag_Set is array (DiagnosticTag) of Boolean with
     Pack, Default_Component_Value => False;

   type tagSupport_OfPublishDiagnosticsClientCapabilities is record
      valueSet : LSP.Structures.DiagnosticTag_Set;
      --  The tags supported by the client.

   end record;
   type tagSupport_OfPublishDiagnosticsClientCapabilities_Optional
     (Is_Set : Boolean := False) is
   record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : tagSupport_OfPublishDiagnosticsClientCapabilities;
      end case;
   end record;

   type PublishDiagnosticsClientCapabilities is record
      relatedInformation : Boolean_Optional;
      --  Whether the clients accepts diagnostics with related information.

      tagSupport : tagSupport_OfPublishDiagnosticsClientCapabilities_Optional;
      --  Client supports the tag property to provide meta data about a
      --  diagnostic. Clients supporting tags have to handle unknown tags
      --  gracefully.
      --
      --  @since 3.15.0

      versionSupport : Boolean_Optional;
      --  Whether the client interprets the version property of the
      --  `textDocument/publishDiagnostics` notification's parameter.
      --
      --  @since 3.15.0

      codeDescriptionSupport : Boolean_Optional;
      --  Client supports a codeDescription property
      --
      --  @since 3.16.0

      dataSupport : Boolean_Optional;
      --  Whether code action supports the `data` property which is
      --  preserved between a `textDocument/publishDiagnostics` and
      --  `textDocument/codeAction` request.
      --
      --  @since 3.16.0

   end record;
   --  The publish diagnostic client capabilities.

   type PublishDiagnosticsClientCapabilities_Optional
     (Is_Set : Boolean := False) is
   record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : PublishDiagnosticsClientCapabilities;
      end case;
   end record;

   type LinkedEditingRangeClientCapabilities is record
      dynamicRegistration : Boolean_Optional;
      --  Whether implementation supports dynamic registration.
      --  If this is set to `true` the client supports the new
      --  `(TextDocumentRegistrationOptions & StaticRegistrationOptions)`
      --  return value for the corresponding server capability as well.

   end record;
   --  Client capabilities for the linked editing range request.
   --
   --  @since 3.16.0

   type LinkedEditingRangeClientCapabilities_Optional
     (Is_Set : Boolean := False) is
   record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : LinkedEditingRangeClientCapabilities;
      end case;
   end record;

   package CodeActionKind_Vectors is new Ada.Containers.Vectors
     (Positive, CodeActionKind, "=");

   type CodeActionKind_Set is
   new CodeActionKind_Vectors.Vector with null record;

   type codeActionKind_OfcodeActionLiteralSupport_OfCodeActionClientCapabilities is
   record
      valueSet : LSP.Structures.CodeActionKind_Set;
      --  The code action kind values the client supports. When this property
      --  exists the client also guarantees that it will handle values outside
      --  its set gracefully and falls back to a default value when unknown.

   end record;
   type codeActionLiteralSupport_OfCodeActionClientCapabilities is record
      codeActionKind : LSP.Structures
        .codeActionKind_OfcodeActionLiteralSupport_OfCodeActionClientCapabilities;
      --  The code action kind is support with the following value set.

   end record;
   type codeActionLiteralSupport_OfCodeActionClientCapabilities_Optional
     (Is_Set : Boolean := False) is
   record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : codeActionLiteralSupport_OfCodeActionClientCapabilities;
      end case;
   end record;

   type CodeActionClientCapabilities is record
      dynamicRegistration : Boolean_Optional;
      --  Whether code action supports dynamic registration.

      codeActionLiteralSupport : codeActionLiteralSupport_OfCodeActionClientCapabilities_Optional;
      --  The client support code action literals of type `CodeAction` as
      --  a valid response of the `textDocument/codeAction` request. If the
      --  property is not set the request can only return `Command` literals.
      --
      --  @since 3.8.0

      isPreferredSupport : Boolean_Optional;
      --  Whether code action supports the `isPreferred` property.
      --
      --  @since 3.15.0

      disabledSupport : Boolean_Optional;
      --  Whether code action supports the `disabled` property.
      --
      --  @since 3.16.0

      dataSupport : Boolean_Optional;
      --  Whether code action supports the `data` property which is preserved
      --  between a `textDocument/codeAction` and a `codeAction/resolve`
      --  request.
      --
      --  @since 3.16.0

      resolveSupport : resolveSupport_OfWorkspaceSymbolClientCapabilities_Optional;
      --  Whether the client supports resolving additional code action
      --  properties via a separate `codeAction/resolve` request.
      --
      --  @since 3.16.0

      honorsChangeAnnotations : Boolean_Optional;
      --  Whether the client honors the change annotations in text edits and
      --  resource operations returned via the `CodeAction#edit` property by
      --  for example presenting the workspace edit in the user interface and
      --  asking for confirmation.
      --
      --  @since 3.16.0

   end record;
   --  The Client Capabilities of a [CodeActionRequest](#CodeActionRequest).

   type CodeActionClientCapabilities_Optional (Is_Set : Boolean := False) is
   record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : CodeActionClientCapabilities;
      end case;
   end record;

   type DocumentColorClientCapabilities is record
      dynamicRegistration : Boolean_Optional;
      --  Whether implementation supports dynamic registration.
      --  If this is set to `true` the client supports the new
      --  `DocumentColorRegistrationOptions` return value for the
      --  corresponding server capability as well.

   end record;

   type DocumentColorClientCapabilities_Optional (Is_Set : Boolean := False) is
   record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : DocumentColorClientCapabilities;
      end case;
   end record;

   type DocumentLinkClientCapabilities is record
      dynamicRegistration : Boolean_Optional;
      --  Whether document link supports dynamic registration.

      tooltipSupport : Boolean_Optional;
      --  Whether the client supports the `tooltip` property on `DocumentLink`.
      --
      --  @since 3.15.0

   end record;
   --  The client capabilities of a
   --  [DocumentLinkRequest](#DocumentLinkRequest).

   type DocumentLinkClientCapabilities_Optional (Is_Set : Boolean := False) is
   record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : DocumentLinkClientCapabilities;
      end case;
   end record;

   type Boolean_Or_Any_Optional (Is_Set : Boolean := False) is record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : Boolean_Or_Any;
      end case;
   end record;

   type Boolean_Or_Something (Is_Boolean : Boolean := True) is record
      case Is_Boolean is
         when True =>
            Boolean : Standard.Boolean;
         when False =>
            a_delta : Boolean_Optional;
            --  The client will send the
            --  `textDocument/semanticTokens/full/delta` request if the
            --  server provides a corresponding handler.

      end case;
   end record;

   type Boolean_Or_Something_Optional (Is_Set : Boolean := False) is record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : Boolean_Or_Something;
      end case;
   end record;

   type requests_OfSemanticTokensClientCapabilities is record
      a_range : Boolean_Or_Any_Optional;
      --  The client will send the `textDocument/semanticTokens/range` request
      --  if the server provides a corresponding handler.

      full : Boolean_Or_Something_Optional;
      --  The client will send the `textDocument/semanticTokens/full` request
      --  if the server provides a corresponding handler.

   end record;
   type TokenFormat_Set is array (TokenFormat) of Boolean with
     Pack, Default_Component_Value => False;

   type SemanticTokensClientCapabilities is record
      dynamicRegistration : Boolean_Optional;
      --  Whether implementation supports dynamic registration.
      --  If this is set to `true` the client supports the new
      --  `(TextDocumentRegistrationOptions & StaticRegistrationOptions)`
      --  return value for the corresponding server capability as well.

      requests : LSP.Structures.requests_OfSemanticTokensClientCapabilities;
      --  Which requests the client supports and might send to the server
      --  depending on the server's capability. Please note that clients might
      --  not show semantic tokens or degrade some of the user experience if a
      --  range or full request is advertised by the client but not provided by
      --  the server. If for example the client capability `requests.full` and
      --  `request.range` are both set to true but the server only provides
      --  a range provider the client might not render a minimap correctly
      --  or might even decide to not show any semantic tokens at all.

      tokenTypes : LSP.Structures.Virtual_String_Vector;
      --  The token types that the client supports.

      tokenModifiers : LSP.Structures.Virtual_String_Vector;
      --  The token modifiers that the client supports.

      formats : LSP.Structures.TokenFormat_Set;
      --  The token formats the clients supports.

      overlappingTokenSupport : Boolean_Optional;
      --  Whether the client supports tokens that can overlap each other.

      multilineTokenSupport : Boolean_Optional;
      --  Whether the client supports tokens that can span multiple lines.

      serverCancelSupport : Boolean_Optional;
      --  Whether the client allows the server to actively cancel a semantic
      --  token request, e.g. supports returning LSPErrorCodes.ServerCancelled.
      --  If a server does the client needs to retrigger the request.
      --
      --  @since 3.17.0

      augmentsSyntaxTokens : Boolean_Optional;
      --  Whether the client uses semantic tokens to augment existing syntax
      --  tokens. If set to `true` client side created syntax tokens and
      --  semantic tokens are both used for colorization. If set to `false`
      --  the client only uses the returned semantic tokens for colorization.
      --
      --  If the value is `undefined` then the client behavior is not
      --  specified.
      --
      --  @since 3.17.0

   end record;
   --  @since 3.16.0

   type SemanticTokensClientCapabilities_Optional (Is_Set : Boolean := False)
   is
   record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : SemanticTokensClientCapabilities;
      end case;
   end record;

   package MarkupKind_Vectors is new Ada.Containers.Vectors
     (Positive, MarkupKind, "=");

   type MarkupKind_Vector is new MarkupKind_Vectors.Vector with null record;

   type HoverClientCapabilities is record
      dynamicRegistration : Boolean_Optional;
      --  Whether hover supports dynamic registration.

      contentFormat : LSP.Structures.MarkupKind_Vector;
      --  Client supports the following content formats for the content
      --  property. The order describes the preferred format of the client.

   end record;

   type HoverClientCapabilities_Optional (Is_Set : Boolean := False) is record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : HoverClientCapabilities;
      end case;
   end record;

   type TypeDefinitionClientCapabilities is record
      dynamicRegistration : Boolean_Optional;
      --  Whether implementation supports dynamic registration.
      --  If this is set to `true` the client supports the new
      --  `TypeDefinitionRegistrationOptions` return value for
      --  the corresponding server capability as well.

      linkSupport : Boolean_Optional;
      --  The client supports additional metadata in the form of definition
      --  links.
      --
      --  Since 3.14.0

   end record;
   --  Since 3.6.0

   type TypeDefinitionClientCapabilities_Optional (Is_Set : Boolean := False)
   is
   record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : TypeDefinitionClientCapabilities;
      end case;
   end record;

   type DocumentOnTypeFormattingClientCapabilities is record
      dynamicRegistration : Boolean_Optional;
      --  Whether on type formatting supports dynamic registration.

   end record;
   --  Client capabilities of a
   --  [DocumentOnTypeFormattingRequest](#DocumentOnTypeFormattingRequest).

   type DocumentOnTypeFormattingClientCapabilities_Optional
     (Is_Set : Boolean := False) is
   record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : DocumentOnTypeFormattingClientCapabilities;
      end case;
   end record;

   package FoldingRangeKind_Vectors is new Ada.Containers.Vectors
     (Positive, FoldingRangeKind, "=");

   type FoldingRangeKind_Set is
   new FoldingRangeKind_Vectors.Vector with null record;

   type foldingRangeKind_OfFoldingRangeClientCapabilities is record
      valueSet : LSP.Structures.FoldingRangeKind_Set;
      --  The folding range kind values the client supports. When this property
      --  exists the client also guarantees that it will handle values outside
      --  its set gracefully and falls back to a default value when unknown.

   end record;
   type foldingRangeKind_OfFoldingRangeClientCapabilities_Optional
     (Is_Set : Boolean := False) is
   record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : foldingRangeKind_OfFoldingRangeClientCapabilities;
      end case;
   end record;

   type foldingRange_OfFoldingRangeClientCapabilities is record
      collapsedText : Boolean_Optional;
      --  If set, the client signals that it supports setting collapsedText on
      --  folding ranges to display custom labels instead of the default text.
      --
      --  @since 3.17.0

   end record;
   type foldingRange_OfFoldingRangeClientCapabilities_Optional
     (Is_Set : Boolean := False) is
   record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : foldingRange_OfFoldingRangeClientCapabilities;
      end case;
   end record;

   type FoldingRangeClientCapabilities is record
      dynamicRegistration : Boolean_Optional;
      --  Whether implementation supports dynamic registration for folding
      --  range providers. If this is set to `true` the client supports the new
      --  `FoldingRangeRegistrationOptions` return value for the corresponding
      --  server capability as well.

      rangeLimit : Natural_Optional;
      --  The maximum number of folding ranges that the client prefers to
      --  receive per document. The value serves as a hint, servers are free
      --  to follow the limit.

      lineFoldingOnly : Boolean_Optional;
      --  If set, the client signals that it only supports folding complete
      --  lines. If set, client will ignore specified `startCharacter` and
      --  `endCharacter` properties in a FoldingRange.

      foldingRangeKind : foldingRangeKind_OfFoldingRangeClientCapabilities_Optional;
      --  Specific options for the folding range kind.
      --
      --  @since 3.17.0

      foldingRange : foldingRange_OfFoldingRangeClientCapabilities_Optional;
      --  Specific options for the folding range.
      --
      --  @since 3.17.0

   end record;

   type FoldingRangeClientCapabilities_Optional (Is_Set : Boolean := False) is
   record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : FoldingRangeClientCapabilities;
      end case;
   end record;

   type CallHierarchyClientCapabilities_Optional (Is_Set : Boolean := False) is
   record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : CallHierarchyClientCapabilities;
      end case;
   end record;

   type DefinitionClientCapabilities is record
      dynamicRegistration : Boolean_Optional;
      --  Whether definition supports dynamic registration.

      linkSupport : Boolean_Optional;
      --  The client supports additional metadata in the form of definition
      --  links.
      --
      --  @since 3.14.0

   end record;
   --  Client Capabilities for a [DefinitionRequest](#DefinitionRequest).

   type DefinitionClientCapabilities_Optional (Is_Set : Boolean := False) is
   record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : DefinitionClientCapabilities;
      end case;
   end record;

   type TextDocumentSyncClientCapabilities is record
      dynamicRegistration : Boolean_Optional;
      --  Whether text document synchronization supports dynamic registration.

      willSave : Boolean_Optional;
      --  The client supports sending will save notifications.

      willSaveWaitUntil : Boolean_Optional;
      --  The client supports sending a will save request and waits for a
      --  response providing text edits which will be applied to the document
      --  before it is saved.

      didSave : Boolean_Optional;
      --  The client supports did save notifications.

   end record;

   type TextDocumentSyncClientCapabilities_Optional (Is_Set : Boolean := False)
   is
   record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : TextDocumentSyncClientCapabilities;
      end case;
   end record;

   type PrepareSupportDefaultBehavior_Optional (Is_Set : Boolean := False) is
   record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : PrepareSupportDefaultBehavior;
      end case;
   end record;

   type RenameClientCapabilities is record
      dynamicRegistration : Boolean_Optional;
      --  Whether rename supports dynamic registration.

      prepareSupport : Boolean_Optional;
      --  Client supports testing for validity of rename operations before
      --  execution.
      --
      --  @since 3.12.0

      prepareSupportDefaultBehavior : PrepareSupportDefaultBehavior_Optional;
      --  Client supports the default behavior result.
      --
      --  The value indicates the default behavior used by the client.
      --
      --  @since 3.16.0

      honorsChangeAnnotations : Boolean_Optional;
      --  Whether the client honors the change annotations in text edits and
      --  resource operations returned via the rename request's workspace edit
      --  by for example presenting the workspace edit in the user interface
      --  and asking for confirmation.
      --
      --  @since 3.16.0

   end record;

   type RenameClientCapabilities_Optional (Is_Set : Boolean := False) is record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : RenameClientCapabilities;
      end case;
   end record;

   type TypeHierarchyClientCapabilities is record
      dynamicRegistration : Boolean_Optional;
      --  Whether implementation supports dynamic registration.
      --  If this is set to `true` the client supports the new
      --  `(TextDocumentRegistrationOptions & StaticRegistrationOptions)`
      --  return value for the corresponding server capability as well.

   end record;
   --  @since 3.17.0

   type TypeHierarchyClientCapabilities_Optional (Is_Set : Boolean := False) is
   record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : TypeHierarchyClientCapabilities;
      end case;
   end record;

   type ReferenceClientCapabilities is record
      dynamicRegistration : Boolean_Optional;
      --  Whether references supports dynamic registration.

   end record;
   --  Client Capabilities for a [ReferencesRequest](#ReferencesRequest).

   type ReferenceClientCapabilities_Optional (Is_Set : Boolean := False) is
   record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : ReferenceClientCapabilities;
      end case;
   end record;

   type DocumentRangeFormattingClientCapabilities is record
      dynamicRegistration : Boolean_Optional;
      --  Whether range formatting supports dynamic registration.

   end record;
   --  Client capabilities of a
   --  [DocumentRangeFormattingRequest](#DocumentRangeFormattingRequest).

   type DocumentRangeFormattingClientCapabilities_Optional
     (Is_Set : Boolean := False) is
   record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : DocumentRangeFormattingClientCapabilities;
      end case;
   end record;

   type parameterInformation_OfsignatureInformation_OfSignatureHelpClientCapabilities is
   record
      labelOffsetSupport : Boolean_Optional;
      --  The client supports processing label offsets instead of a simple
      --  label string.
      --
      --  @since 3.14.0

   end record;
   type parameterInformation_OfsignatureInformation_OfSignatureHelpClientCapabilities_Optional
     (Is_Set : Boolean := False) is
   record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : parameterInformation_OfsignatureInformation_OfSignatureHelpClientCapabilities;
      end case;
   end record;

   type signatureInformation_OfSignatureHelpClientCapabilities is record
      documentationFormat : LSP.Structures.MarkupKind_Vector;
      --  Client supports the following content formats for the documentation
      --  property. The order describes the preferred format of the client.

      parameterInformation : parameterInformation_OfsignatureInformation_OfSignatureHelpClientCapabilities_Optional;
      --  Client capabilities specific to parameter information.

      activeParameterSupport : Boolean_Optional;
      --  The client supports the `activeParameter` property on
      --  `SignatureInformation` literal.
      --
      --  @since 3.16.0

   end record;
   type signatureInformation_OfSignatureHelpClientCapabilities_Optional
     (Is_Set : Boolean := False) is
   record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : signatureInformation_OfSignatureHelpClientCapabilities;
      end case;
   end record;

   type SignatureHelpClientCapabilities is record
      dynamicRegistration : Boolean_Optional;
      --  Whether signature help supports dynamic registration.

      signatureInformation : signatureInformation_OfSignatureHelpClientCapabilities_Optional;
      --  The client supports the following `SignatureInformation` specific
      --  properties.

      contextSupport : Boolean_Optional;
      --  The client supports to send additional context information for
      --  a `textDocument/signatureHelp` request. A client that opts into
      --  contextSupport will also support the `retriggerCharacters` on
      --  `SignatureHelpOptions`.
      --
      --  @since 3.15.0

   end record;
   --  Client Capabilities for a [SignatureHelpRequest](#SignatureHelpRequest).

   type SignatureHelpClientCapabilities_Optional (Is_Set : Boolean := False) is
   record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : SignatureHelpClientCapabilities;
      end case;
   end record;

   type ImplementationClientCapabilities is record
      dynamicRegistration : Boolean_Optional;
      --  Whether implementation supports dynamic registration.
      --  If this is set to `true` the client supports the new
      --  `ImplementationRegistrationOptions` return value for
      --  the corresponding server capability as well.

      linkSupport : Boolean_Optional;
      --  The client supports additional metadata in the form of definition
      --  links.
      --
      --  @since 3.14.0

   end record;
   --  @since 3.6.0

   type ImplementationClientCapabilities_Optional (Is_Set : Boolean := False)
   is
   record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : ImplementationClientCapabilities;
      end case;
   end record;

   type MonikerClientCapabilities is record
      dynamicRegistration : Boolean_Optional;
      --  Whether moniker supports dynamic registration. If this is set to
      --  `true` the client supports the new `MonikerRegistrationOptions`
      --  return value for the corresponding server capability as well.

   end record;
   --  Client capabilities specific to the moniker request.
   --
   --  @since 3.16.0

   type MonikerClientCapabilities_Optional (Is_Set : Boolean := False) is
   record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : MonikerClientCapabilities;
      end case;
   end record;

   type DocumentHighlightClientCapabilities is record
      dynamicRegistration : Boolean_Optional;
      --  Whether document highlight supports dynamic registration.

   end record;
   --  Client Capabilities for a
   --  [DocumentHighlightRequest](#DocumentHighlightRequest).

   type DocumentHighlightClientCapabilities_Optional
     (Is_Set : Boolean := False) is
   record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : DocumentHighlightClientCapabilities;
      end case;
   end record;

   type SelectionRangeClientCapabilities is record
      dynamicRegistration : Boolean_Optional;
      --  Whether implementation supports dynamic registration for selection
      --  range providers. If this is set to `true` the client supports
      --  the new `SelectionRangeRegistrationOptions` return value for
      --  the corresponding server capability as well.

   end record;

   type SelectionRangeClientCapabilities_Optional (Is_Set : Boolean := False)
   is
   record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : SelectionRangeClientCapabilities;
      end case;
   end record;

   type InsertTextMode_Optional (Is_Set : Boolean := False) is record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : InsertTextMode;
      end case;
   end record;

   type CompletionItemTag_Set is array (CompletionItemTag) of Boolean with
     Pack, Default_Component_Value => False;

   type tagSupport_OfcompletionItem_OfCompletionClientCapabilities is record
      valueSet : LSP.Structures.CompletionItemTag_Set;
      --  The tags supported by the client.

   end record;
   type tagSupport_OfcompletionItem_OfCompletionClientCapabilities_Optional
     (Is_Set : Boolean := False) is
   record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : tagSupport_OfcompletionItem_OfCompletionClientCapabilities;
      end case;
   end record;

   type InsertTextMode_Set is array (InsertTextMode) of Boolean with
     Pack, Default_Component_Value => False;

   type insertTextModeSupport_OfcompletionItem_OfCompletionClientCapabilities is
   record
      valueSet : LSP.Structures.InsertTextMode_Set;

   end record;
   type insertTextModeSupport_OfcompletionItem_OfCompletionClientCapabilities_Optional
     (Is_Set : Boolean := False) is
   record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : insertTextModeSupport_OfcompletionItem_OfCompletionClientCapabilities;
      end case;
   end record;

   type completionItem_OfCompletionClientCapabilities is record
      snippetSupport : Boolean_Optional;
      --  Client supports snippets as insert text.
      --
      --  A snippet can define tab stops and placeholders with `$1`, `$2` and
      --  `${3:foo}`. `$0` defines the final tab stop, it defaults to the end
      --  of the snippet. Placeholders with equal identifiers are linked, that
      --  is typing in one will update others too.

      commitCharactersSupport : Boolean_Optional;
      --  Client supports commit characters on a completion item.

      documentationFormat : LSP.Structures.MarkupKind_Vector;
      --  Client supports the following content formats for the documentation
      --  property. The order describes the preferred format of the client.

      deprecatedSupport : Boolean_Optional;
      --  Client supports the deprecated property on a completion item.

      preselectSupport : Boolean_Optional;
      --  Client supports the preselect property on a completion item.

      tagSupport : tagSupport_OfcompletionItem_OfCompletionClientCapabilities_Optional;
      --  Client supports the tag property on a completion item. Clients
      --  supporting tags have to handle unknown tags gracefully. Clients
      --  especially need to preserve unknown tags when sending a completion
      --  item back to the server in a resolve call.
      --
      --  @since 3.15.0

      insertReplaceSupport : Boolean_Optional;
      --  Client support insert replace edit to control different behavior if a
      --  completion item is inserted in the text or should replace text.
      --
      --  @since 3.16.0

      resolveSupport : resolveSupport_OfWorkspaceSymbolClientCapabilities_Optional;
      --  Indicates which properties a client can resolve lazily on a
      --  completion item. Before version 3.16.0 only the predefined
      --  properties `documentation` and `details` could be resolved lazily.
      --
      --  @since 3.16.0

      insertTextModeSupport : insertTextModeSupport_OfcompletionItem_OfCompletionClientCapabilities_Optional;
      --  The client supports the `insertTextMode` property on a completion
      --  item to override the whitespace handling mode as defined by the
      --  client (see `insertTextMode`).
      --
      --  @since 3.16.0

      labelDetailsSupport : Boolean_Optional;
      --  The client has support for completion item label details (see also
      --  `CompletionItemLabelDetails`).
      --
      --  @since 3.17.0

   end record;
   type completionItem_OfCompletionClientCapabilities_Optional
     (Is_Set : Boolean := False) is
   record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : completionItem_OfCompletionClientCapabilities;
      end case;
   end record;

   type CompletionItemKind_Set is array (CompletionItemKind) of Boolean with
     Pack, Default_Component_Value => False;

   type completionItemKind_OfCompletionClientCapabilities is record
      valueSet : LSP.Structures.CompletionItemKind_Set;
      --  The completion item kind values the client supports. When this
      --  property exists the client also guarantees that it will handle values
      --  outside its set gracefully and falls back to a default value when
      --  unknown.
      --
      --  If this property is not present the client only supports the
      --  completion items kinds from `Text` to `Reference` as defined in
      --  the initial version of the protocol.

   end record;
   type completionItemKind_OfCompletionClientCapabilities_Optional
     (Is_Set : Boolean := False) is
   record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : completionItemKind_OfCompletionClientCapabilities;
      end case;
   end record;

   type completionList_OfCompletionClientCapabilities is record
      itemDefaults : LSP.Structures.Virtual_String_Vector;
      --  The client supports the following itemDefaults on a completion list.
      --
      --  The value lists the supported property names of the
      --  `CompletionList.itemDefaults` object. If omitted no properties
      --  are supported.
      --
      --  @since 3.17.0

   end record;
   type completionList_OfCompletionClientCapabilities_Optional
     (Is_Set : Boolean := False) is
   record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : completionList_OfCompletionClientCapabilities;
      end case;
   end record;

   type CompletionClientCapabilities is record
      dynamicRegistration : Boolean_Optional;
      --  Whether completion supports dynamic registration.

      completionItem : completionItem_OfCompletionClientCapabilities_Optional;
      --  The client supports the following `CompletionItem` specific
      --  capabilities.

      completionItemKind : completionItemKind_OfCompletionClientCapabilities_Optional;

      insertTextMode : InsertTextMode_Optional;
      --  Defines how the client handles whitespace and indentation when
      --  accepting a completion item that uses multi line text in either
      --  `insertText` or `textEdit`.
      --
      --  @since 3.17.0

      contextSupport : Boolean_Optional;
      --  The client supports to send additional context information for a
      --  `textDocument/completion` request.

      completionList : completionList_OfCompletionClientCapabilities_Optional;
      --  The client supports the following `CompletionList` specific
      --  capabilities.
      --
      --  @since 3.17.0

   end record;
   --  Completion client capabilities

   type CompletionClientCapabilities_Optional (Is_Set : Boolean := False) is
   record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : CompletionClientCapabilities;
      end case;
   end record;

   type DocumentSymbolClientCapabilities is record
      dynamicRegistration : Boolean_Optional;
      --  Whether document symbol supports dynamic registration.

      symbolKind : symbolKind_OfWorkspaceSymbolClientCapabilities_Optional;
      --  Specific capabilities for the `SymbolKind` in the
      --  `textDocument/documentSymbol` request.

      hierarchicalDocumentSymbolSupport : Boolean_Optional;
      --  The client supports hierarchical document symbols.

      tagSupport : tagSupport_OfWorkspaceSymbolClientCapabilities_Optional;
      --  The client supports tags on `SymbolInformation`. Tags are supported
      --  on `DocumentSymbol` if `hierarchicalDocumentSymbolSupport` is set to
      --  true. Clients supporting tags have to handle unknown tags gracefully.
      --
      --  @since 3.16.0

      labelSupport : Boolean_Optional;
      --  The client supports an additional label presented in the UI when
      --  registering a document symbol provider.
      --
      --  @since 3.16.0

   end record;
   --  Client Capabilities for a
   --  [DocumentSymbolRequest](#DocumentSymbolRequest).

   type DocumentSymbolClientCapabilities_Optional (Is_Set : Boolean := False)
   is
   record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : DocumentSymbolClientCapabilities;
      end case;
   end record;

   type InlineValueClientCapabilities is record
      dynamicRegistration : Boolean_Optional;
      --  Whether implementation supports dynamic registration for inline value
      --  providers.

   end record;
   --  Client capabilities specific to inline values.
   --
   --  @since 3.17.0

   type InlineValueClientCapabilities_Optional (Is_Set : Boolean := False) is
   record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : InlineValueClientCapabilities;
      end case;
   end record;

   type TextDocumentClientCapabilities is record
      synchronization : TextDocumentSyncClientCapabilities_Optional;
      --  Defines which synchronization capabilities the client supports.

      completion : CompletionClientCapabilities_Optional;
      --  Capabilities specific to the `textDocument/completion` request.

      hover : HoverClientCapabilities_Optional;
      --  Capabilities specific to the `textDocument/hover` request.

      signatureHelp : SignatureHelpClientCapabilities_Optional;
      --  Capabilities specific to the `textDocument/signatureHelp` request.

      declaration : DeclarationClientCapabilities_Optional;
      --  Capabilities specific to the `textDocument/declaration` request.
      --
      --  @since 3.14.0

      definition : DefinitionClientCapabilities_Optional;
      --  Capabilities specific to the `textDocument/definition` request.

      typeDefinition : TypeDefinitionClientCapabilities_Optional;
      --  Capabilities specific to the `textDocument/typeDefinition` request.
      --
      --  @since 3.6.0

      implementation : ImplementationClientCapabilities_Optional;
      --  Capabilities specific to the `textDocument/implementation` request.
      --
      --  @since 3.6.0

      references : ReferenceClientCapabilities_Optional;
      --  Capabilities specific to the `textDocument/references` request.

      documentHighlight : DocumentHighlightClientCapabilities_Optional;
      --  Capabilities specific to the `textDocument/documentHighlight`
      --  request.

      documentSymbol : DocumentSymbolClientCapabilities_Optional;
      --  Capabilities specific to the `textDocument/documentSymbol` request.

      codeAction : CodeActionClientCapabilities_Optional;
      --  Capabilities specific to the `textDocument/codeAction` request.

      codeLens : CodeLensClientCapabilities_Optional;
      --  Capabilities specific to the `textDocument/codeLens` request.

      documentLink : DocumentLinkClientCapabilities_Optional;
      --  Capabilities specific to the `textDocument/documentLink` request.

      colorProvider : DocumentColorClientCapabilities_Optional;
      --  Capabilities specific to the `textDocument/documentColor` and the
      --  `textDocument/colorPresentation` request.
      --
      --  @since 3.6.0

      formatting : DocumentFormattingClientCapabilities_Optional;
      --  Capabilities specific to the `textDocument/formatting` request.

      rangeFormatting : DocumentRangeFormattingClientCapabilities_Optional;
      --  Capabilities specific to the `textDocument/rangeFormatting` request.

      onTypeFormatting : DocumentOnTypeFormattingClientCapabilities_Optional;
      --  Capabilities specific to the `textDocument/onTypeFormatting` request.

      rename : RenameClientCapabilities_Optional;
      --  Capabilities specific to the `textDocument/rename` request.

      foldingRange : FoldingRangeClientCapabilities_Optional;
      --  Capabilities specific to the `textDocument/foldingRange` request.
      --
      --  @since 3.10.0

      selectionRange : SelectionRangeClientCapabilities_Optional;
      --  Capabilities specific to the `textDocument/selectionRange` request.
      --
      --  @since 3.15.0

      publishDiagnostics : PublishDiagnosticsClientCapabilities_Optional;
      --  Capabilities specific to the `textDocument/publishDiagnostics`
      --  notification.

      callHierarchy : CallHierarchyClientCapabilities_Optional;
      --  Capabilities specific to the various call hierarchy requests.
      --
      --  @since 3.16.0

      semanticTokens : SemanticTokensClientCapabilities_Optional;
      --  Capabilities specific to the various semantic token request.
      --
      --  @since 3.16.0

      linkedEditingRange : LinkedEditingRangeClientCapabilities_Optional;
      --  Capabilities specific to the `textDocument/linkedEditingRange`
      --  request.
      --
      --  @since 3.16.0

      moniker : MonikerClientCapabilities_Optional;
      --  Client capabilities specific to the `textDocument/moniker` request.
      --
      --  @since 3.16.0

      typeHierarchy : TypeHierarchyClientCapabilities_Optional;
      --  Capabilities specific to the various type hierarchy requests.
      --
      --  @since 3.17.0

      inlineValue : InlineValueClientCapabilities_Optional;
      --  Capabilities specific to the `textDocument/inlineValue` request.
      --
      --  @since 3.17.0

      inlayHint : InlayHintClientCapabilities_Optional;
      --  Capabilities specific to the `textDocument/inlayHint` request.
      --
      --  @since 3.17.0

      diagnostic : DiagnosticClientCapabilities_Optional;
      --  Capabilities specific to the diagnostic pull model.
      --
      --  @since 3.17.0

   end record;
   --  Text document specific client capabilities.

   type TextDocumentClientCapabilities_Optional (Is_Set : Boolean := False) is
   record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : TextDocumentClientCapabilities;
      end case;
   end record;

   type ClientCapabilities is record
      workspace : WorkspaceClientCapabilities_Optional;
      --  Workspace specific client capabilities.

      textDocument : TextDocumentClientCapabilities_Optional;
      --  Text document specific client capabilities.

      notebookDocument : NotebookDocumentClientCapabilities_Optional;
      --  Capabilities specific to the notebook document support.
      --
      --  @since 3.17.0

      window : WindowClientCapabilities_Optional;
      --  Window specific client capabilities.

      general : GeneralClientCapabilities_Optional;
      --  General client capabilities.
      --
      --  @since 3.16.0

      experimental : LSPAny_Optional;
      --  Experimental client capabilities.

   end record;
   --  Defines the capabilities provided by the client.

   type WorkspaceEdit_Optional (Is_Set : Boolean := False) is record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : WorkspaceEdit;
      end case;
   end record;

   type CodeActionKind_Optional (Is_Set : Boolean := False) is record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : CodeActionKind;
      end case;
   end record;

   type AlsReferenceKind_Set is array (AlsReferenceKind) of Boolean with
     Pack, Default_Component_Value => False;

   type Location is record
      uri : LSP.Structures.DocumentUri;

      a_range : LSP.Structures.A_Range;

      alsKind : LSP.Structures.AlsReferenceKind_Set;

   end record;
   --  Represents a location inside a resource, such as a line inside a text
   --  file.

   type DiagnosticRelatedInformation is record
      location : LSP.Structures.Location;
      --  The location of this related diagnostic information.

      message : LSP.Structures.Virtual_String;
      --  The message of this related diagnostic information.

   end record;
   --  Represents a related message and source code location for a diagnostic.
   --  This should be used to point to code locations that cause or related to
   --  a diagnostics, e.g when duplicating a symbol in a scope.

   type URI is new VSS.Strings.Virtual_String with null record;
   --  A tagging type for string properties that are actually URIs
   --
   --  @since 3.16.0

   function Get_Hash (Self : URI) return Ada.Containers.Hash_Type is
     (Ada.Containers.Hash_Type'Mod (Self.Hash));

   type CodeDescription is record
      href : LSP.Structures.URI;
      --  An URI to open with more information about the diagnostic error.

   end record;
   --  Structure to capture a description for an error code.
   --
   --  @since 3.16.0

   type CodeDescription_Optional (Is_Set : Boolean := False) is record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : CodeDescription;
      end case;
   end record;

   type DiagnosticSeverity_Optional (Is_Set : Boolean := False) is record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : DiagnosticSeverity;
      end case;
   end record;

   type Integer_Or_Virtual_String_Optional (Is_Set : Boolean := False) is
   record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : Integer_Or_Virtual_String;
      end case;
   end record;

   package DiagnosticRelatedInformation_Vectors is new Ada.Containers.Vectors
     (Positive, DiagnosticRelatedInformation, "=");

   type DiagnosticRelatedInformation_Vector is
   new DiagnosticRelatedInformation_Vectors.Vector with null record;

   type Diagnostic is record
      a_range : LSP.Structures.A_Range;
      --  The range at which the message applies

      severity : DiagnosticSeverity_Optional;
      --  The diagnostic's severity. Can be omitted. If omitted it is up to the
      --  client to interpret diagnostics as error, warning, info or hint.

      code : Integer_Or_Virtual_String_Optional;
      --  The diagnostic's code, which usually appear in the user interface.

      codeDescription : CodeDescription_Optional;
      --  An optional property to describe the error code. Requires the code
      --  field (above) to be present/not null.
      --
      --  @since 3.16.0

      source : Virtual_String_Optional;
      --  A human-readable string describing the source of this diagnostic,
      --  e.g. 'typescript' or 'super lint'. It usually appears in the user
      --  interface.

      message : LSP.Structures.Virtual_String;
      --  The diagnostic's message. It usually appears in the user interface

      tags : LSP.Structures.DiagnosticTag_Set;
      --  Additional metadata about the diagnostic.
      --
      --  @since 3.15.0

      relatedInformation : LSP.Structures.DiagnosticRelatedInformation_Vector;
      --  An array of related diagnostic information, e.g. when symbol-names
      --  within a scope collide all definitions can be marked via this
      --  property.

      data : LSPAny_Optional;
      --  A data entry field that is preserved between a
      --  `textDocument/publishDiagnostics` notification
      --  and `textDocument/codeAction` request.
      --
      --  @since 3.16.0

   end record;
   --  Represents a diagnostic, such as a compiler error or warning. Diagnostic
   --  objects are only valid in the scope of a resource.

   type Command is record
      title : LSP.Structures.Virtual_String;
      --  Title of the command, like `save`.

      command : LSP.Structures.Virtual_String;
      --  The identifier of the actual command handler.

      arguments : LSP.Structures.LSPAny_Vector;
      --  Arguments that the command handler should be invoked with.

   end record;
   --  Represents a reference to a command. Provides a title which will be used
   --  to represent a command in the UI and, optionally, an array of arguments
   --  which will be passed to the command handler function when invoked.

   type Command_Optional (Is_Set : Boolean := False) is record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : Command;
      end case;
   end record;

   package Diagnostic_Vectors is new Ada.Containers.Vectors
     (Positive, Diagnostic, "=");

   type Diagnostic_Vector is new Diagnostic_Vectors.Vector with null record;

   type disabled_OfCodeAction is record
      reason : LSP.Structures.Virtual_String;
      --  Human readable description of why the code action is currently
      --  disabled.
      --
      --  This is displayed in the code actions UI.

   end record;
   type disabled_OfCodeAction_Optional (Is_Set : Boolean := False) is record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : disabled_OfCodeAction;
      end case;
   end record;

   type CodeAction is record
      title : LSP.Structures.Virtual_String;
      --  A short, human-readable, title for this code action.

      kind : CodeActionKind_Optional;
      --  The kind of the code action.
      --
      --  Used to filter code actions.

      diagnostics : LSP.Structures.Diagnostic_Vector;
      --  The diagnostics that this code action resolves.

      isPreferred : Boolean_Optional;
      --  Marks this as a preferred action. Preferred actions are used by the
      --  `auto fix` command and can be targeted by keybindings.
      --
      --  A quick fix should be marked preferred if it properly addresses the
      --  underlying error. A refactoring should be marked preferred if it is
      --  the most reasonable choice of actions to take.
      --
      --  @since 3.15.0

      disabled : disabled_OfCodeAction_Optional;
      --  Marks that the code action cannot currently be applied.
      --
      --  Clients should follow the following guidelines regarding disabled
      --  code actions:
      --
      --    - Disabled code actions are not shown in automatic [lightbulbs](https://code.visualstudio.com/docs/editor/editingevolved#_code-action)
      --      code action menus.
      --
      --    - Disabled actions are shown as faded out in the code action menu when the user requests a more specific type
      --      of code action, such as refactorings.
      --
      --    - If the user has a [keybinding](https://code.visualstudio.com/docs/editor/refactoring#_keybindings-for-code-actions)
      --      that auto applies a code action and only disabled code actions are returned, the client should show the user an
      --      error message with `reason` in the editor.
      --
      --  @since 3.16.0

      edit : WorkspaceEdit_Optional;
      --  The workspace edit this code action performs.

      command : Command_Optional;
      --  A command this code action executes. If a code action provides an
      --  edit and a command, first the edit is executed and then the command.

      data : LSPAny_Optional;
      --  A data entry field that is preserved on a code action between a
      --  `textDocument/codeAction` and a `codeAction/resolve` request.
      --
      --  @since 3.16.0

   end record;
   --  A code action represents a change that can be performed in code, e.g. to
   --  fix a problem or to refactor code.
   --
   --  A CodeAction must set either `edit` and/or a `command`. If both are
   --  supplied, the `edit` is applied first, then the `command` is executed.

   type CodeActionTriggerKind_Optional (Is_Set : Boolean := False) is record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : CodeActionTriggerKind;
      end case;
   end record;

   type CodeActionContext is record
      diagnostics : LSP.Structures.Diagnostic_Vector;
      --  An array of diagnostics known on the client side overlapping the
      --  range provided to the `textDocument/codeAction` request. They
      --  are provided so that the server knows which errors are currently
      --  presented to the user for the given range. There is no guarantee that
      --  these accurately reflect the error state of the resource. The primary
      --  parameter to compute code actions is the provided range.

      only : LSP.Structures.CodeActionKind_Set;
      --  Requested kind of actions to return.
      --
      --  Actions not of this kind are filtered out by the client before being
      --  shown. So servers can omit computing them.

      triggerKind : CodeActionTriggerKind_Optional;
      --  The reason why code actions were requested.
      --
      --  @since 3.17.0

   end record;
   --  Contains additional diagnostic information about the context in which a
   --  [code action](#CodeActionProvider.provideCodeActions) is run.

   type CodeActionOptions is new WorkDoneProgressOptions with record
      workDoneProgress : Boolean_Optional;

      codeActionKinds : LSP.Structures.CodeActionKind_Set;
      --  CodeActionKinds that this server may return.
      --
      --  The list of kinds may be generic, such as `CodeActionKind.Refactor`,
      --  or the server may list out every specific kind they provide.

      resolveProvider : Boolean_Optional;
      --  The server provides support to resolve additional information for a
      --  code action.
      --
      --  @since 3.16.0

   end record;
   --  Provider options for a [CodeActionRequest](#CodeActionRequest).

   overriding function workDoneProgress
     (Self : CodeActionOptions) return Boolean_Optional is
     (Self.workDoneProgress);

   type CodeActionParams is
   new WorkDoneProgressParams and PartialResultParams with record
      workDoneToken : ProgressToken_Optional;
      --  An optional token that a server can use to report work done progress.

      partialResultToken : ProgressToken_Optional;
      --  An optional token that a server can use to report partial results
      --  (e.g. streaming) to the client.

      textDocument : LSP.Structures.TextDocumentIdentifier;
      --  The document in which the command was invoked.

      a_range : LSP.Structures.A_Range;
      --  The range for which the command was invoked.

      context : LSP.Structures.CodeActionContext;
      --  Context carrying additional information.

   end record;
   --  The parameters of a [CodeActionRequest](#CodeActionRequest).

   overriding function workDoneToken
     (Self : CodeActionParams) return LSP.Structures.ProgressToken_Optional is
     (Self.workDoneToken);

   overriding function partialResultToken
     (Self : CodeActionParams) return LSP.Structures.ProgressToken_Optional is
     (Self.partialResultToken);

   type CodeActionRegistrationOptions is new CodeActionOptions with record
      Parent : LSP.Structures.TextDocumentRegistrationOptions;
   end record;
   --  Registration options for a [CodeActionRequest](#CodeActionRequest).

   type CodeLens is record
      a_range : LSP.Structures.A_Range;
      --  The range in which this code lens is valid. Should only span a single
      --  line.

      command : Command_Optional;
      --  The command this code lens represents.

      data : LSPAny_Optional;
      --  A data entry field that is preserved on a code lens item between a
      --  [CodeLensRequest](#CodeLensRequest) and a [CodeLensResolveRequest]
      --  (#CodeLensResolveRequest)

   end record;
   --  A code lens represents a [command](#Command) that should be shown along
   --  with source text, like the number of references, a way to run tests,
   --  etc.
   --
   --  A code lens is _unresolved_ when no command is associated to it. For
   --  performance reasons the creation of a code lens and resolving should
   --  be done in two stages.

   type CodeLensOptions is new WorkDoneProgressOptions with record
      workDoneProgress : Boolean_Optional;

      resolveProvider : Boolean_Optional;
      --  Code lens has a resolve provider as well.

   end record;
   --  Code Lens provider options of a [CodeLensRequest](#CodeLensRequest).

   overriding function workDoneProgress
     (Self : CodeLensOptions) return Boolean_Optional is
     (Self.workDoneProgress);

   type CodeLensParams is
   new WorkDoneProgressParams and PartialResultParams with record
      workDoneToken : ProgressToken_Optional;
      --  An optional token that a server can use to report work done progress.

      partialResultToken : ProgressToken_Optional;
      --  An optional token that a server can use to report partial results
      --  (e.g. streaming) to the client.

      textDocument : LSP.Structures.TextDocumentIdentifier;
      --  The document to request code lens for.

   end record;
   --  The parameters of a [CodeLensRequest](#CodeLensRequest).

   overriding function workDoneToken
     (Self : CodeLensParams) return LSP.Structures.ProgressToken_Optional is
     (Self.workDoneToken);

   overriding function partialResultToken
     (Self : CodeLensParams) return LSP.Structures.ProgressToken_Optional is
     (Self.partialResultToken);

   type CodeLensRegistrationOptions is new CodeLensOptions with record
      Parent : LSP.Structures.TextDocumentRegistrationOptions;
   end record;
   --  Registration options for a [CodeLensRequest](#CodeLensRequest).

   type Color is record
      red : Float;
      --  The red component of this color in the range [0-1].

      green : Float;
      --  The green component of this color in the range [0-1].

      blue : Float;
      --  The blue component of this color in the range [0-1].

      alpha : Float;
      --  The alpha component of this color in the range [0-1].

   end record;
   --  Represents a color in RGBA space.

   type ColorInformation is record
      a_range : LSP.Structures.A_Range;
      --  The range in the document where this color appears.

      color : LSP.Structures.Color;
      --  The actual color value for this color range.

   end record;
   --  Represents a color range from a document.

   type TextEdit_Optional (Is_Set : Boolean := False) is record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : TextEdit;
      end case;
   end record;

   type ColorPresentation is record
      label : LSP.Structures.Virtual_String;
      --  The label of this color presentation. It will be shown on the color
      --  picker header. By default this is also the text that is inserted when
      --  selecting this color presentation.

      textEdit : TextEdit_Optional;
      --  An [edit](#TextEdit) which is applied to a document when
      --  selecting this presentation for the color. When `falsy`
      --  the [label](#ColorPresentation.label) is used.

      additionalTextEdits : LSP.Structures.TextEdit_Vector;
      --  An optional array of additional [text edits](#TextEdit) that are
      --  applied when selecting this color presentation. Edits must not
      --  overlap with the main [edit](#ColorPresentation.textEdit) nor
      --  with themselves.

   end record;

   type ColorPresentationParams is
   new WorkDoneProgressParams and PartialResultParams with record
      workDoneToken : ProgressToken_Optional;
      --  An optional token that a server can use to report work done progress.

      partialResultToken : ProgressToken_Optional;
      --  An optional token that a server can use to report partial results
      --  (e.g. streaming) to the client.

      textDocument : LSP.Structures.TextDocumentIdentifier;
      --  The text document.

      color : LSP.Structures.Color;
      --  The color to request presentations for.

      a_range : LSP.Structures.A_Range;
      --  The range where the color would be inserted. Serves as a context.

   end record;
   --  Parameters for a [ColorPresentationRequest](#ColorPresentationRequest).

   overriding function workDoneToken
     (Self : ColorPresentationParams)
      return LSP.Structures.ProgressToken_Optional is
     (Self.workDoneToken);

   overriding function partialResultToken
     (Self : ColorPresentationParams)
      return LSP.Structures.ProgressToken_Optional is
     (Self.partialResultToken);

   type CompletionContext is record
      triggerKind : LSP.Enumerations.CompletionTriggerKind;
      --  How the completion was triggered.

      triggerCharacter : Virtual_String_Optional;
      --  The trigger character (a single character) that has
      --  trigger code complete. Is undefined if `triggerKind
      --  !== CompletionTriggerKind.TriggerCharacter`

   end record;
   --  Contains additional information about the context in which a completion
   --  request is triggered.

   type CompletionItemLabelDetails is record
      detail : Virtual_String_Optional;
      --  An optional string which is rendered less prominently directly after
      --  {@link CompletionItem.label label}, without any spacing. Should be
      --  used for function signatures and type annotations.

      description : Virtual_String_Optional;
      --  An optional string which is rendered less prominently after {@link
      --  CompletionItem.detail}. Should be used for fully qualified names and
      --  file paths.

   end record;
   --  Additional details for a completion item label.
   --
   --  @since 3.17.0

   type CompletionItemLabelDetails_Optional (Is_Set : Boolean := False) is
   record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : CompletionItemLabelDetails;
      end case;
   end record;

   type InsertTextFormat_Optional (Is_Set : Boolean := False) is record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : InsertTextFormat;
      end case;
   end record;

   type CompletionItemKind_Optional (Is_Set : Boolean := False) is record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : CompletionItemKind;
      end case;
   end record;

   type InsertReplaceEdit is record
      newText : LSP.Structures.Virtual_String;
      --  The string to be inserted.

      insert : LSP.Structures.A_Range;
      --  The range if the insert is requested

      replace : LSP.Structures.A_Range;
      --  The range if the replace is requested.

   end record;
   --  A special text edit to provide an insert and a replace operation.
   --
   --  @since 3.16.0

   type MarkupContent is record
      kind : LSP.Enumerations.MarkupKind;
      --  The type of the Markup

      value : LSP.Structures.Virtual_String;
      --  The content itself

   end record;
   --  A `MarkupContent` literal represents a string value which content is
   --  interpreted base on its kind flag. Currently the protocol supports
   --  `plaintext` and `markdown` as markup kinds.
   --
   --  If the kind is `markdown` then the value can contain
   --  fenced code blocks like in GitHub issues. See
   --  https://help.github.com/articles/creating-and-highlighting-code-blocks/#syntax-highlighting
   --
   --  Here is an example how such a string can be constructed using JavaScript
   --  / TypeScript: ```ts let markdown: MarkdownContent = {
   --   kind: MarkupKind.Markdown,
   --   value: [
   --     '# Header',
   --     'Some text',
   --     '```typescript',
   --     'someCode();',
   --     '```'
   --   ].join('\n')
   --  };
   --  ```
   --
   --  *Please Note* that clients might sanitize the return markdown. A client
   --  could decide to remove HTML from the markdown to avoid script execution.

   type Virtual_String_Or_MarkupContent (Is_Virtual_String : Boolean := True)
   is
   record
      case Is_Virtual_String is
         when True =>
            Virtual_String : LSP.Structures.Virtual_String;
         when False =>
            MarkupContent : LSP.Structures.MarkupContent;
      end case;
   end record;

   type Virtual_String_Or_MarkupContent_Optional (Is_Set : Boolean := False) is
   record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : Virtual_String_Or_MarkupContent;
      end case;
   end record;

   type TextEdit_Or_InsertReplaceEdit (Is_TextEdit : Boolean := True) is record
      case Is_TextEdit is
         when True =>
            TextEdit : LSP.Structures.TextEdit;
         when False =>
            InsertReplaceEdit : LSP.Structures.InsertReplaceEdit;
      end case;
   end record;

   type TextEdit_Or_InsertReplaceEdit_Optional (Is_Set : Boolean := False) is
   record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : TextEdit_Or_InsertReplaceEdit;
      end case;
   end record;

   type CompletionItem is record
      label : LSP.Structures.Virtual_String;
      --  The label of this completion item.
      --
      --  The label property is also by default the text that is inserted when
      --  selecting this completion.
      --
      --  If label details are provided the label itself should be an
      --  unqualified name of the completion item.

      labelDetails : CompletionItemLabelDetails_Optional;
      --  Additional details for the label
      --
      --  @since 3.17.0

      kind : CompletionItemKind_Optional;
      --  The kind of this completion item. Based of the kind an icon is chosen
      --  by the editor.

      tags : LSP.Structures.CompletionItemTag_Set;
      --  Tags for this completion item.
      --
      --  @since 3.15.0

      detail : Virtual_String_Optional;
      --  A human-readable string with additional information about this item,
      --  like type or symbol information.

      documentation : Virtual_String_Or_MarkupContent_Optional;
      --  A human-readable string that represents a doc-comment.

      deprecated : Boolean_Optional;
      --  Indicates if this item is deprecated. @deprecated Use `tags` instead.

      preselect : Boolean_Optional;
      --  Select this item when showing.
      --
      --  *Note* that only one completion item can be selected and that the
      --  tool / client decides which item that is. The rule is that the
      --  *first* item of those that match best is selected.

      sortText : Virtual_String_Optional;
      --  A string that should be used when comparing this item with other
      --  items. When `falsy` the [label](#CompletionItem.label) is used.

      filterText : Virtual_String_Optional;
      --  A string that should be used when filtering a set of completion
      --  items. When `falsy` the [label](#CompletionItem.label) is used.

      insertText : Virtual_String_Optional;
      --  A string that should be inserted into a document when selecting this
      --  completion. When `falsy` the [label](#CompletionItem.label) is used.
      --
      --  The `insertText` is subject to interpretation by the client side.
      --  Some tools might not take the string literally. For example VS
      --  Code when code complete is requested in this example `con<cursor
      --  position>` and a completion item with an `insertText` of `console`
      --  is provided it will only insert `sole`. Therefore it is recommended
      --  to use `textEdit` instead since it avoids additional client side
      --  interpretation.

      insertTextFormat : InsertTextFormat_Optional;
      --  The format of the insert text. The format applies to both the
      --  `insertText` property and the `newText` property of a provided
      --  `textEdit`. If omitted defaults to `InsertTextFormat.PlainText`.
      --
      --  Please note that the insertTextFormat doesn't apply to
      --  `additionalTextEdits`.

      insertTextMode : InsertTextMode_Optional;
      --  How whitespace and indentation is handled during completion item
      --  insertion. If not provided the clients default value depends on
      --  the `textDocument.completion.insertTextMode` client capability.
      --
      --  @since 3.16.0

      textEdit : TextEdit_Or_InsertReplaceEdit_Optional;
      --  An [edit](#TextEdit) which is applied to a document when
      --  selecting this completion. When an edit is provided the value
      --  of [insertText](#CompletionItem.insertText) is ignored.
      --
      --  Most editors support two different operations when accepting a
      --  completion item. One is to insert a completion text and the other is
      --  to replace an existing text with a completion text. Since this can
      --  usually not be predetermined by a server it can report both ranges.
      --  Clients need to signal support for `InsertReplaceEdits` via the
      --  `textDocument.completion.insertReplaceSupport` client capability
      --  property.
      --
      --  *Note 1:* The text edit's range as well as both ranges from an
      --  insert replace edit must be a [single line] and they must contain
      --  the position at which completion has been requested. *Note 2:* If
      --  an `InsertReplaceEdit` is returned the edit's insert range must be a
      --  prefix of the edit's replace range, that means it must be contained
      --  and starting at the same position.
      --
      --  @since 3.16.0 additional type `InsertReplaceEdit`

      textEditText : Virtual_String_Optional;
      --  The edit text used if the completion item is part of a CompletionList
      --  and CompletionList defines an item default for the text edit range.
      --
      --  Clients will only honor this property if they opt into
      --  completion list item defaults using the capability
      --  `completionList.itemDefaults`.
      --
      --  If not provided and a list's default range is provided the label
      --  property is used as a text.
      --
      --  @since 3.17.0

      additionalTextEdits : LSP.Structures.TextEdit_Vector;
      --  An optional array of additional [text edits](#TextEdit) that
      --  are applied when selecting this completion. Edits must not
      --  overlap (including the same insert position) with the main
      --  [edit](#CompletionItem.textEdit) nor with themselves.
      --
      --  Additional text edits should be used to change text unrelated to the
      --  current cursor position (for example adding an import statement at
      --  the top of the file if the completion item will insert an unqualified
      --  type).

      commitCharacters : LSP.Structures.Virtual_String_Vector;
      --  An optional set of characters that when pressed while this completion
      --  is active will accept it first and then type that character.
      --  *Note* that all commit characters should have `length=1` and
      --  that superfluous characters will be ignored.

      command : Command_Optional;
      --  An optional [command](#Command) that is executed *after*
      --  inserting this completion. *Note* that additional modifications
      --  to the current document should be described with the
      --  [additionalTextEdits](#CompletionItem.additionalTextEdits)-property.

      data : LSPAny_Optional;
      --  A data entry field that is preserved on a completion item
      --  between a [CompletionRequest](#CompletionRequest) and a
      --  [CompletionResolveRequest](#CompletionResolveRequest).

   end record;
   --  A completion item represents a text snippet that is proposed to complete
   --  text that is being typed.

   type Range_Or_Something (Is_A_Range : Boolean := True) is record
      case Is_A_Range is
         when True =>
            A_Range : LSP.Structures.A_Range;
         when False =>
            insert : LSP.Structures.A_Range;

            replace : LSP.Structures.A_Range;

      end case;
   end record;

   type Range_Or_Something_Optional (Is_Set : Boolean := False) is record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : Range_Or_Something;
      end case;
   end record;

   type itemDefaults_OfCompletionList is record
      commitCharacters : LSP.Structures.Virtual_String_Vector;
      --  A default commit character set.
      --
      --  @since 3.17.0

      editRange : Range_Or_Something_Optional;
      --  A default edit range.
      --
      --  @since 3.17.0

      insertTextFormat : InsertTextFormat_Optional;
      --  A default insert text format.
      --
      --  @since 3.17.0

      insertTextMode : InsertTextMode_Optional;
      --  A default insert text mode.
      --
      --  @since 3.17.0

      data : LSPAny_Optional;
      --  A default data value.
      --
      --  @since 3.17.0

   end record;
   type itemDefaults_OfCompletionList_Optional (Is_Set : Boolean := False) is
   record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : itemDefaults_OfCompletionList;
      end case;
   end record;

   package CompletionItem_Vectors is new Ada.Containers.Vectors
     (Positive, CompletionItem, "=");

   type CompletionItem_Vector is
   new CompletionItem_Vectors.Vector with null record;

   type CompletionList is record
      isIncomplete : Standard.Boolean;
      --  This list it not complete. Further typing results in recomputing this
      --  list.
      --
      --  Recomputed lists have all their items replaced (not appended) in the
      --  incomplete completion sessions.

      itemDefaults : itemDefaults_OfCompletionList_Optional;
      --  In many cases the items of an actual completion result share the same
      --  value for properties like `commitCharacters` or the range of a text
      --  edit. A completion list can therefore define item defaults which will
      --  be used if a completion item itself doesn't specify the value.
      --
      --  If a completion list specifies a default value and a completion item
      --  also specifies a corresponding value the one from the item is used.
      --
      --  Servers are only allowed to return default values if the client
      --  signals support for this via the `completionList.itemDefaults`
      --  capability.
      --
      --  @since 3.17.0

      items : LSP.Structures.CompletionItem_Vector;
      --  The completion items.

   end record;
   --  Represents a collection of [completion items](#CompletionItem) to be
   --  presented in the editor.

   type completionItem_OfCompletionOptions is record
      labelDetailsSupport : Boolean_Optional;
      --  The server has support for completion item label details (see also
      --  `CompletionItemLabelDetails`) when receiving a completion item in a
      --  resolve call.
      --
      --  @since 3.17.0

   end record;
   type completionItem_OfCompletionOptions_Optional (Is_Set : Boolean := False)
   is
   record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : completionItem_OfCompletionOptions;
      end case;
   end record;

   type CompletionOptions is new WorkDoneProgressOptions with record
      workDoneProgress : Boolean_Optional;

      triggerCharacters : LSP.Structures.Virtual_String_Vector;
      --  Most tools trigger completion request automatically without
      --  explicitly requesting it using a keyboard shortcut (e.g. Ctrl+Space).
      --  Typically they do so when the user starts to type an identifier. For
      --  example if the user types `c` in a JavaScript file code complete will
      --  automatically pop up present `console` besides others as a completion
      --  item. Characters that make up identifiers don't need to be listed
      --  here.
      --
      --  If code complete should automatically be trigger on characters not
      --  being valid inside an identifier (for example `.` in JavaScript)
      --  list them in `triggerCharacters`.

      allCommitCharacters : LSP.Structures.Virtual_String_Vector;
      --  The list of all possible characters that commit a
      --  completion. This field can be used if clients don't support
      --  individual commit characters per completion item. See
      --  `ClientCapabilities.textDocument.completion.completionItem.commitCharactersSupport`
      --
      --  If a server provides both `allCommitCharacters` and commit characters
      --  on an individual completion item the ones on the completion item win.
      --
      --  @since 3.2.0

      resolveProvider : Boolean_Optional;
      --  The server provides support to resolve additional information for a
      --  completion item.

      completionItem : completionItem_OfCompletionOptions_Optional;
      --  The server supports the following `CompletionItem` specific
      --  capabilities.
      --
      --  @since 3.17.0

   end record;
   --  Completion options.

   overriding function workDoneProgress
     (Self : CompletionOptions) return Boolean_Optional is
     (Self.workDoneProgress);

   type CompletionContext_Optional (Is_Set : Boolean := False) is record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : CompletionContext;
      end case;
   end record;

   type CompletionParams is
   new TextDocumentPositionParams and WorkDoneProgressParams and
     PartialResultParams with record
      workDoneToken : ProgressToken_Optional;
      --  An optional token that a server can use to report work done progress.

      partialResultToken : ProgressToken_Optional;
      --  An optional token that a server can use to report partial results
      --  (e.g. streaming) to the client.

      context : CompletionContext_Optional;
      --  The completion context. This is only available it the
      --  client specifies to send this using the client capability
      --  `textDocument.completion.contextSupport === true`

   end record;
   --  Completion parameters

   overriding function workDoneToken
     (Self : CompletionParams) return LSP.Structures.ProgressToken_Optional is
     (Self.workDoneToken);

   overriding function partialResultToken
     (Self : CompletionParams) return LSP.Structures.ProgressToken_Optional is
     (Self.partialResultToken);

   type CompletionRegistrationOptions is new CompletionOptions with record
      Parent : LSP.Structures.TextDocumentRegistrationOptions;
   end record;
   --  Registration options for a [CompletionRequest](#CompletionRequest).

   type ConfigurationItem is record
      scopeUri : Virtual_String_Optional;
      --  The scope to get the configuration section for.

      section : Virtual_String_Optional;
      --  The configuration section asked for.

   end record;

   package ConfigurationItem_Vectors is new Ada.Containers.Vectors
     (Positive, ConfigurationItem, "=");

   type ConfigurationItem_Vector is
   new ConfigurationItem_Vectors.Vector with null record;

   type ConfigurationParams is record
      items : LSP.Structures.ConfigurationItem_Vector;

   end record;
   --  The parameters of a configuration request.

   type FileCreate is record
      uri : LSP.Structures.Virtual_String;
      --  A file:// URI for the location of the file/folder being created.

   end record;
   --  Represents information on a file/folder create.
   --
   --  @since 3.16.0

   package FileCreate_Vectors is new Ada.Containers.Vectors
     (Positive, FileCreate, "=");

   type FileCreate_Vector is new FileCreate_Vectors.Vector with null record;

   type CreateFilesParams is record
      files : LSP.Structures.FileCreate_Vector;
      --  An array of all files/folders created in this operation.

   end record;
   --  The parameters sent in notifications/requests for user-initiated
   --  creation of files.
   --
   --  @since 3.16.0

   type DeclarationOptions is new WorkDoneProgressOptions with record
      workDoneProgress : Boolean_Optional;

   end record;

   overriding function workDoneProgress
     (Self : DeclarationOptions) return Boolean_Optional is
     (Self.workDoneProgress);

   type AlsDisplayMethodAncestryOnNavigationPolicy_Optional
     (Is_Set : Boolean := False) is
   record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : AlsDisplayMethodAncestryOnNavigationPolicy;
      end case;
   end record;

   type DeclarationParams is
   new TextDocumentPositionParams and WorkDoneProgressParams and
     PartialResultParams with record
      workDoneToken : ProgressToken_Optional;
      --  An optional token that a server can use to report work done progress.

      partialResultToken : ProgressToken_Optional;
      --  An optional token that a server can use to report partial results
      --  (e.g. streaming) to the client.

      alsDisplayMethodAncestryOnNavigation : AlsDisplayMethodAncestryOnNavigationPolicy_Optional;
      --  whether or now we should list overriding/overridden subprograms.

   end record;

   overriding function workDoneToken
     (Self : DeclarationParams) return LSP.Structures.ProgressToken_Optional is
     (Self.workDoneToken);

   overriding function partialResultToken
     (Self : DeclarationParams) return LSP.Structures.ProgressToken_Optional is
     (Self.partialResultToken);

   type DeclarationRegistrationOptions is
   new DeclarationOptions and StaticRegistrationOptions with record
      id : Virtual_String_Optional;
      --  The id used to register the request. The id can be used to deregister
      --  the request again. See also Registration#id.

      Parent : LSP.Structures.TextDocumentRegistrationOptions;
   end record;

   overriding function id
     (Self : DeclarationRegistrationOptions) return Virtual_String_Optional is
     (Self.id);

   type DefinitionOptions is new WorkDoneProgressOptions with record
      workDoneProgress : Boolean_Optional;

   end record;
   --  Server Capabilities for a [DefinitionRequest](#DefinitionRequest).

   overriding function workDoneProgress
     (Self : DefinitionOptions) return Boolean_Optional is
     (Self.workDoneProgress);

   type DefinitionParams is
   new TextDocumentPositionParams and WorkDoneProgressParams and
     PartialResultParams with record
      workDoneToken : ProgressToken_Optional;
      --  An optional token that a server can use to report work done progress.

      partialResultToken : ProgressToken_Optional;
      --  An optional token that a server can use to report partial results
      --  (e.g. streaming) to the client.

      alsDisplayMethodAncestryOnNavigation : AlsDisplayMethodAncestryOnNavigationPolicy_Optional;
      --  whether or now we should list overriding/overridden subprograms.

   end record;
   --  Parameters for a [DefinitionRequest](#DefinitionRequest).

   overriding function workDoneToken
     (Self : DefinitionParams) return LSP.Structures.ProgressToken_Optional is
     (Self.workDoneToken);

   overriding function partialResultToken
     (Self : DefinitionParams) return LSP.Structures.ProgressToken_Optional is
     (Self.partialResultToken);

   type DefinitionRegistrationOptions is new DefinitionOptions with record
      Parent : LSP.Structures.TextDocumentRegistrationOptions;
   end record;
   --  Registration options for a [DefinitionRequest](#DefinitionRequest).

   type FileDelete is record
      uri : LSP.Structures.Virtual_String;
      --  A file:// URI for the location of the file/folder being deleted.

   end record;
   --  Represents information on a file/folder delete.
   --
   --  @since 3.16.0

   package FileDelete_Vectors is new Ada.Containers.Vectors
     (Positive, FileDelete, "=");

   type FileDelete_Vector is new FileDelete_Vectors.Vector with null record;

   type DeleteFilesParams is record
      files : LSP.Structures.FileDelete_Vector;
      --  An array of all files/folders deleted in this operation.

   end record;
   --  The parameters sent in notifications/requests for user-initiated deletes
   --  of files.
   --
   --  @since 3.16.0

   type DiagnosticOptions is new WorkDoneProgressOptions with record
      workDoneProgress : Boolean_Optional;

      identifier : Virtual_String_Optional;
      --  An optional identifier under which the diagnostics are managed by the
      --  client.

      interFileDependencies : Standard.Boolean;
      --  Whether the language has inter file dependencies meaning that editing
      --  code in one file can result in a different diagnostic set in another
      --  file. Inter file dependencies are common for most programming
      --  languages and typically uncommon for linters.

      workspaceDiagnostics : Standard.Boolean;
      --  The server provides support for workspace diagnostics as well.

   end record;
   --  Diagnostic options.
   --
   --  @since 3.17.0

   overriding function workDoneProgress
     (Self : DiagnosticOptions) return Boolean_Optional is
     (Self.workDoneProgress);

   type DiagnosticRegistrationOptions is
   new DiagnosticOptions and StaticRegistrationOptions with record
      id : Virtual_String_Optional;
      --  The id used to register the request. The id can be used to deregister
      --  the request again. See also Registration#id.

      Parent : LSP.Structures.TextDocumentRegistrationOptions;
   end record;
   --  Diagnostic registration options.
   --
   --  @since 3.17.0

   overriding function id
     (Self : DiagnosticRegistrationOptions) return Virtual_String_Optional is
     (Self.id);

   type DiagnosticServerCancellationData is record
      retriggerRequest : Standard.Boolean;

   end record;
   --  Cancellation data returned from a diagnostic request.
   --
   --  @since 3.17.0

   type DidChangeConfigurationParams is record
      settings : LSP.Structures.LSPAny;
      --  The actual changed settings

   end record;
   --  The parameters of a change configuration notification.

   type Virtual_String_Vector_Optional (Is_Set : Boolean := False) is record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : Virtual_String_Vector;
      end case;
   end record;

   type DidChangeConfigurationRegistrationOptions is record
      section : Virtual_String_Vector_Optional;

   end record;

   type LSPObject is new LSPAny with record
      null;
   end record;
   --  LSP object definition.
   --  @since 3.17.0

   type LSPObject_Optional (Is_Set : Boolean := False) is record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : LSPObject;
      end case;
   end record;

   type ExecutionSummary is record
      executionOrder : Natural;
      --  A strict monotonically increasing value indicating the execution
      --  order of a cell inside a notebook.

      success : Boolean_Optional;
      --  Whether the execution was successful or not if known by the client.

   end record;

   type ExecutionSummary_Optional (Is_Set : Boolean := False) is record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : ExecutionSummary;
      end case;
   end record;

   type NotebookCell is record
      kind : LSP.Enumerations.NotebookCellKind;
      --  The cell's kind

      document : LSP.Structures.DocumentUri;
      --  The URI of the cell's text document content.

      metadata : LSPObject_Optional;
      --  Additional metadata stored with the cell.
      --
      --  Note: should always be an object literal (e.g. LSPObject)

      executionSummary : ExecutionSummary_Optional;
      --  Additional execution summary information if supported by the client.

   end record;
   --  A notebook cell.
   --
   --  A cell's document URI must be unique across ALL notebook cells and can
   --  therefore be used to uniquely identify a notebook cell or the cell's
   --  text document.
   --
   --  @since 3.17.0

   package NotebookCell_Vectors is new Ada.Containers.Vectors
     (Positive, NotebookCell, "=");

   type NotebookCell_Vector is
   new NotebookCell_Vectors.Vector with null record;

   type NotebookCellArrayChange is record
      start : Natural;
      --  The start oftest of the cell that changed.

      deleteCount : Natural;
      --  The deleted cells

      cells : LSP.Structures.NotebookCell_Vector;
      --  The new cells, if any

   end record;
   --  A change describing how to move a `NotebookCell` array from state S to
   --  S'.
   --
   --  @since 3.17.0

   type TextDocumentItem is record
      uri : LSP.Structures.DocumentUri;
      --  The text document's uri.

      languageId : LSP.Structures.Virtual_String;
      --  The text document's language identifier.

      version : Standard.Integer;
      --  The version number of this document (it will increase after each
      --  change, including undo/redo).

      text : LSP.Structures.Virtual_String;
      --  The content of the opened text document.

   end record;
   --  An item to transfer a text document from the client to the server.

   type VersionedTextDocumentIdentifier is
   new TextDocumentIdentifier with record
      version : Standard.Integer;
      --  The version number of this document.

   end record;
   --  A text document identifier to denote a specific version of a text
   --  document.

   type A_Range_Optional (Is_Set : Boolean := False) is record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : A_Range;
      end case;
   end record;

   type TextDocumentContentChangeEvent is record
      a_range : A_Range_Optional;
      --  The range of the document that changed.

      rangeLength : Natural_Optional;
      --  The optional length of the range that got replaced.
      --
      --  @deprecated use range instead.

      text : LSP.Structures.Virtual_String;
      --  The new text for the provided range.

   end record;
   --  An event describing a change to a text document. If only a text is
   --  provided it is considered to be the full content of the document.

   package TextDocumentItem_Vectors is new Ada.Containers.Vectors
     (Positive, TextDocumentItem, "=");

   type TextDocumentItem_Vector is
   new TextDocumentItem_Vectors.Vector with null record;

   package TextDocumentIdentifier_Vectors is new Ada.Containers.Vectors
     (Positive, TextDocumentIdentifier, "=");

   type TextDocumentIdentifier_Vector is
   new TextDocumentIdentifier_Vectors.Vector with null record;

   type structure_Ofcells_OfNotebookDocumentChangeEvent is record
      an_array : LSP.Structures.NotebookCellArrayChange;
      --  The change to the cell array.

      didOpen : LSP.Structures.TextDocumentItem_Vector;
      --  Additional opened cell text documents.

      didClose : LSP.Structures.TextDocumentIdentifier_Vector;
      --  Additional closed cell text documents.

   end record;
   type structure_Ofcells_OfNotebookDocumentChangeEvent_Optional
     (Is_Set : Boolean := False) is
   record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : structure_Ofcells_OfNotebookDocumentChangeEvent;
      end case;
   end record;

   package TextDocumentContentChangeEvent_Vectors is new Ada.Containers.Vectors
     (Positive, TextDocumentContentChangeEvent, "=");

   type TextDocumentContentChangeEvent_Vector is
   new TextDocumentContentChangeEvent_Vectors.Vector with null record;

   type textContent_Ofcells_OfNotebookDocumentChangeEvent_Item is record
      document : LSP.Structures.VersionedTextDocumentIdentifier;

      changes : LSP.Structures.TextDocumentContentChangeEvent_Vector;

   end record;
   package textContent_Ofcells_OfNotebookDocumentChangeEvent_Item_Vectors is new Ada
     .Containers
     .Vectors
     (Positive, textContent_Ofcells_OfNotebookDocumentChangeEvent_Item, "=");

   type textContent_Ofcells_OfNotebookDocumentChangeEvent is
   new textContent_Ofcells_OfNotebookDocumentChangeEvent_Item_Vectors
     .Vector with
   null record;

   type cells_OfNotebookDocumentChangeEvent is record
      structure : structure_Ofcells_OfNotebookDocumentChangeEvent_Optional;
      --  Changes to the cell structure to add or remove cells.

      data : LSP.Structures.NotebookCell_Vector;
      --  Changes to notebook cells properties like its kind, execution summary
      --  or metadata.

      textContent : LSP.Structures
        .textContent_Ofcells_OfNotebookDocumentChangeEvent;
      --  Changes to the text content of notebook cells.

   end record;
   type cells_OfNotebookDocumentChangeEvent_Optional
     (Is_Set : Boolean := False) is
   record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : cells_OfNotebookDocumentChangeEvent;
      end case;
   end record;

   type NotebookDocumentChangeEvent is record
      metadata : LSPObject_Optional;
      --  The changed meta data if any.
      --
      --  Note: should always be an object literal (e.g. LSPObject)

      cells : cells_OfNotebookDocumentChangeEvent_Optional;
      --  Changes to cells

   end record;
   --  A change event for a notebook document.
   --
   --  @since 3.17.0

   type VersionedNotebookDocumentIdentifier is record
      version : Standard.Integer;
      --  The version number of this notebook document.

      uri : LSP.Structures.URI;
      --  The notebook document's uri.

   end record;
   --  A versioned notebook document identifier.
   --
   --  @since 3.17.0

   type DidChangeNotebookDocumentParams is record
      notebookDocument : LSP.Structures.VersionedNotebookDocumentIdentifier;
      --  The notebook document that did change. The version number points to
      --  the version after all provided changes have been applied. If only the
      --  text document content of a cell changes the notebook version doesn't
      --  necessarily have to change.

      change : LSP.Structures.NotebookDocumentChangeEvent;
      --  The actual changes to the notebook document.
      --
      --  The changes describe single state changes to the notebook document.
      --  So if there are two changes c1 (at array index 0) and c2 (at array
      --  index 1) for a notebook in state S then c1 moves the notebook from S
      --  to S' and c2 from S' to S''. So c1 is computed on the state S and c2
      --  is computed on the state S'.
      --
      --  To mirror the content of a notebook using change events use the
      --  following approach: - start with the same initial content - apply the
      --  'notebookDocument/didChange' notifications in the order you receive
      --  them. - apply the `NotebookChangeEvent`s in a single notification in
      --  the order
      --    you receive them.

   end record;
   --  The params sent in a change notebook document notification.
   --
   --  @since 3.17.0

   type DidChangeTextDocumentParams is record
      textDocument : LSP.Structures.VersionedTextDocumentIdentifier;
      --  The document that did change. The version number points to the
      --  version after all provided content changes have been applied.

      contentChanges : LSP.Structures.TextDocumentContentChangeEvent_Vector;
      --  The actual content changes. The content changes describe single state
      --  changes to the document. So if there are two content changes c1 (at
      --  array index 0) and c2 (at array index 1) for a document in state S
      --  then c1 moves the document from S to S' and c2 from S' to S''. So
      --  c1 is computed on the state S and c2 is computed on the state S'.
      --
      --  To mirror the content of a document using change events use the
      --  following approach: - start with the same initial content - apply
      --  the 'textDocument/didChange' notifications in the order you receive
      --  them. - apply the `TextDocumentContentChangeEvent`s in a single
      --  notification in the order
      --    you receive them.

   end record;
   --  The change text document notification's parameters.

   type FileEvent is record
      uri : LSP.Structures.DocumentUri;
      --  The file's uri.

      a_type : LSP.Enumerations.FileChangeType;
      --  The change type.

   end record;
   --  An event describing a file change.

   package FileEvent_Vectors is new Ada.Containers.Vectors
     (Positive, FileEvent, "=");

   type FileEvent_Vector is new FileEvent_Vectors.Vector with null record;

   type DidChangeWatchedFilesParams is record
      changes : LSP.Structures.FileEvent_Vector;
      --  The actual file events.

   end record;
   --  The watched files change notification's parameters.

   type WatchKind_Optional (Is_Set : Boolean := False) is record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : WatchKind;
      end case;
   end record;

   type Pattern is new VSS.Strings.Virtual_String with null record;
   --  The glob pattern to watch relative to the base path. Glob patterns can
   --  have the following syntax: - `*` to match one or more characters in a
   --  path segment - `?` to match on one character in a path segment - `**`
   --  to match any number of path segments, including none - `{}` to group
   --  conditions (e.g. `**["200B"]/*.{ts,js}` matches all TypeScript and
   --  JavaScript files) - `[]` to declare a range of characters to match
   --  in a path segment (e.g., `example.[0-9]` to match on `example.0`,
   --  `example.1`, ["2026"]) - `[!...]` to negate a range of characters to
   --  match in a path segment (e.g., `example.[!0-9]` to match on `example.a`,
   --  `example.b`, but not `example.0`)
   --
   --  @since 3.17.0

   function Get_Hash (Self : Pattern) return Ada.Containers.Hash_Type is
     (Ada.Containers.Hash_Type'Mod (Self.Hash));

   type WorkspaceFolder is record
      uri : LSP.Structures.URI;
      --  The associated URI for this workspace folder.

      name : LSP.Structures.Virtual_String;
      --  The name of the workspace folder. Used to refer to this workspace
      --  folder in the user interface.

   end record;
   --  A workspace folder inside a client.

   type WorkspaceFolder_Or_URI (Is_WorkspaceFolder : Boolean := True) is record
      case Is_WorkspaceFolder is
         when True =>
            WorkspaceFolder : LSP.Structures.WorkspaceFolder;
         when False =>
            URI : LSP.Structures.URI;
      end case;
   end record;

   type RelativePattern is record
      baseUri : LSP.Structures.WorkspaceFolder_Or_URI;
      --  A workspace folder or a base URI to which this pattern will be
      --  matched against relatively.

      pattern : LSP.Structures.Pattern;
      --  The actual glob pattern;

   end record;
   --  A relative pattern is a helper to construct glob patterns that are
   --  matched relatively to a base URI. The common value for a `baseUri` is
   --  a workspace folder root, but it can be another absolute URI as well.
   --
   --  @since 3.17.0

   type GlobPattern (Is_Pattern : Boolean := True) is record
      case Is_Pattern is
         when True =>
            Pattern : LSP.Structures.Pattern;
         when False =>
            RelativePattern : LSP.Structures.RelativePattern;
      end case;
   end record;
   --  The glob pattern. Either a string pattern or a relative pattern.
   --
   --  @since 3.17.0

   type FileSystemWatcher is record
      globPattern : LSP.Structures.GlobPattern;
      --  The glob pattern to watch. See {@link GlobPattern glob pattern} for
      --  more detail.
      --
      --  @since 3.17.0 support for relative patterns.

      kind : WatchKind_Optional;
      --  The kind of events of interest. If omitted it defaults to
      --  WatchKind.Create | WatchKind.Change | WatchKind.Delete which is 7.

   end record;

   package FileSystemWatcher_Vectors is new Ada.Containers.Vectors
     (Positive, FileSystemWatcher, "=");

   type FileSystemWatcher_Vector is
   new FileSystemWatcher_Vectors.Vector with null record;

   type DidChangeWatchedFilesRegistrationOptions is record
      watchers : LSP.Structures.FileSystemWatcher_Vector;
      --  The watchers to register.

   end record;
   --  Describe options to be used when registered for text document change
   --  events.

   package WorkspaceFolder_Vectors is new Ada.Containers.Vectors
     (Positive, WorkspaceFolder, "=");

   type WorkspaceFolder_Vector is
   new WorkspaceFolder_Vectors.Vector with null record;

   type WorkspaceFoldersChangeEvent is record
      added : LSP.Structures.WorkspaceFolder_Vector;
      --  The array of added workspace folders

      removed : LSP.Structures.WorkspaceFolder_Vector;
      --  The array of the removed workspace folders

   end record;
   --  The workspace folder change event.

   type DidChangeWorkspaceFoldersParams is record
      event : LSP.Structures.WorkspaceFoldersChangeEvent;
      --  The actual workspace folder change event.

   end record;
   --  The parameters of a `workspace/didChangeWorkspaceFolders` notification.

   type NotebookDocumentIdentifier is record
      uri : LSP.Structures.URI;
      --  The notebook document's uri.

   end record;
   --  A literal to identify a notebook document in the client.
   --
   --  @since 3.17.0

   type DidCloseNotebookDocumentParams is record
      notebookDocument : LSP.Structures.NotebookDocumentIdentifier;
      --  The notebook document that got closed.

      cellTextDocuments : LSP.Structures.TextDocumentIdentifier_Vector;
      --  The text documents that represent the content of a notebook cell that
      --  got closed.

   end record;
   --  The params sent in a close notebook document notification.
   --
   --  @since 3.17.0

   type DidCloseTextDocumentParams is record
      textDocument : LSP.Structures.TextDocumentIdentifier;
      --  The document that was closed.

   end record;
   --  The parameters sent in a close text document notification

   type NotebookDocument is record
      uri : LSP.Structures.URI;
      --  The notebook document's uri.

      notebookType : LSP.Structures.Virtual_String;
      --  The type of the notebook.

      version : Standard.Integer;
      --  The version number of this document (it will increase after each
      --  change, including undo/redo).

      metadata : LSPObject_Optional;
      --  Additional metadata stored with the notebook document.
      --
      --  Note: should always be an object literal (e.g. LSPObject)

      cells : LSP.Structures.NotebookCell_Vector;
      --  The cells of a notebook.

   end record;
   --  A notebook document.
   --
   --  @since 3.17.0

   type DidOpenNotebookDocumentParams is record
      notebookDocument : LSP.Structures.NotebookDocument;
      --  The notebook document that got opened.

      cellTextDocuments : LSP.Structures.TextDocumentItem_Vector;
      --  The text documents that represent the content of a notebook cell.

   end record;
   --  The params sent in an open notebook document notification.
   --
   --  @since 3.17.0

   type DidOpenTextDocumentParams is record
      textDocument : LSP.Structures.TextDocumentItem;
      --  The document that was opened.

   end record;
   --  The parameters sent in an open text document notification

   type DidSaveNotebookDocumentParams is record
      notebookDocument : LSP.Structures.NotebookDocumentIdentifier;
      --  The notebook document that got saved.

   end record;
   --  The params sent in a save notebook document notification.
   --
   --  @since 3.17.0

   type DidSaveTextDocumentParams is record
      textDocument : LSP.Structures.TextDocumentIdentifier;
      --  The document that was saved.

      text : Virtual_String_Optional;
      --  Optional the content when saved. Depends on the includeText value
      --  when the save notification was requested.

   end record;
   --  The parameters sent in a save text document notification

   type DocumentColorOptions is new WorkDoneProgressOptions with record
      workDoneProgress : Boolean_Optional;

   end record;

   overriding function workDoneProgress
     (Self : DocumentColorOptions) return Boolean_Optional is
     (Self.workDoneProgress);

   type DocumentColorParams is
   new WorkDoneProgressParams and PartialResultParams with record
      workDoneToken : ProgressToken_Optional;
      --  An optional token that a server can use to report work done progress.

      partialResultToken : ProgressToken_Optional;
      --  An optional token that a server can use to report partial results
      --  (e.g. streaming) to the client.

      textDocument : LSP.Structures.TextDocumentIdentifier;
      --  The text document.

   end record;
   --  Parameters for a [DocumentColorRequest](#DocumentColorRequest).

   overriding function workDoneToken
     (Self : DocumentColorParams)
      return LSP.Structures.ProgressToken_Optional is
     (Self.workDoneToken);

   overriding function partialResultToken
     (Self : DocumentColorParams)
      return LSP.Structures.ProgressToken_Optional is
     (Self.partialResultToken);

   type DocumentColorRegistrationOptions is
   new DocumentColorOptions and StaticRegistrationOptions with record
      id : Virtual_String_Optional;
      --  The id used to register the request. The id can be used to deregister
      --  the request again. See also Registration#id.

      Parent : LSP.Structures.TextDocumentRegistrationOptions;
   end record;

   overriding function id
     (Self : DocumentColorRegistrationOptions)
      return Virtual_String_Optional is
     (Self.id);

   type DocumentDiagnosticParams is
   new WorkDoneProgressParams and PartialResultParams with record
      workDoneToken : ProgressToken_Optional;
      --  An optional token that a server can use to report work done progress.

      partialResultToken : ProgressToken_Optional;
      --  An optional token that a server can use to report partial results
      --  (e.g. streaming) to the client.

      textDocument : LSP.Structures.TextDocumentIdentifier;
      --  The text document.

      identifier : Virtual_String_Optional;
      --  The additional identifier provided during registration.

      previousResultId : Virtual_String_Optional;
      --  The result id of a previous response if provided.

   end record;
   --  Parameters of the document diagnostic request.
   --
   --  @since 3.17.0

   overriding function workDoneToken
     (Self : DocumentDiagnosticParams)
      return LSP.Structures.ProgressToken_Optional is
     (Self.workDoneToken);

   overriding function partialResultToken
     (Self : DocumentDiagnosticParams)
      return LSP.Structures.ProgressToken_Optional is
     (Self.partialResultToken);

   type UnchangedDocumentDiagnosticReport is tagged record
      resultId : LSP.Structures.Virtual_String;
      --  A result id which will be sent on the next diagnostic request for the
      --  same document.

   end record;
   --  A diagnostic report indicating that the last returned report is still
   --  accurate.
   --
   --  @since 3.17.0

   type FullDocumentDiagnosticReport is tagged record
      resultId : Virtual_String_Optional;
      --  An optional result id. If provided it will be sent on the next
      --  diagnostic request for the same document.

      items : LSP.Structures.Diagnostic_Vector;
      --  The actual items.

   end record;
   --  A diagnostic report with a full set of problems.
   --
   --  @since 3.17.0

   type relatedDocuments_OfDocumentDiagnosticReportPartialResult_Item_Variant is
     (full, unchanged);

   type relatedDocuments_OfDocumentDiagnosticReportPartialResult_Item
     (Kind : relatedDocuments_OfDocumentDiagnosticReportPartialResult_Item_Variant :=
        relatedDocuments_OfDocumentDiagnosticReportPartialResult_Item_Variant'
          First)
   is
   record
      case Kind is
         when full =>
            full : LSP.Structures.FullDocumentDiagnosticReport;
         when unchanged =>
            unchanged : LSP.Structures.UnchangedDocumentDiagnosticReport;
      end case;
   end record;

   package relatedDocuments_OfDocumentDiagnosticReportPartialResult_Item_Maps is new Ada
     .Containers
     .Hashed_Maps
     (DocumentUri,
      relatedDocuments_OfDocumentDiagnosticReportPartialResult_Item, Get_Hash,
      "=");

   type relatedDocuments_OfDocumentDiagnosticReportPartialResult is
   new relatedDocuments_OfDocumentDiagnosticReportPartialResult_Item_Maps
     .Map with
   null record;

   type DocumentDiagnosticReportPartialResult is record
      relatedDocuments : LSP.Structures
        .relatedDocuments_OfDocumentDiagnosticReportPartialResult;

   end record;
   --  A partial result for a document diagnostic report.
   --
   --  @since 3.17.0

   type DocumentFormattingOptions is new WorkDoneProgressOptions with record
      workDoneProgress : Boolean_Optional;

   end record;
   --  Provider options for a
   --  [DocumentFormattingRequest](#DocumentFormattingRequest).

   overriding function workDoneProgress
     (Self : DocumentFormattingOptions) return Boolean_Optional is
     (Self.workDoneProgress);

   type FormattingOptions is record
      tabSize : Natural;
      --  Size of a tab in spaces.

      insertSpaces : Standard.Boolean;
      --  Prefer spaces over tabs.

      trimTrailingWhitespace : Boolean_Optional;
      --  Trim trailing whitespace on a line.
      --
      --  @since 3.15.0

      insertFinalNewline : Boolean_Optional;
      --  Insert a newline character at the end of the file if one does not
      --  exist.
      --
      --  @since 3.15.0

      trimFinalNewlines : Boolean_Optional;
      --  Trim all newlines after the final newline at the end of the file.
      --
      --  @since 3.15.0

   end record;
   --  Value-object describing what options formatting should use.

   type DocumentFormattingParams is new WorkDoneProgressParams with record
      workDoneToken : ProgressToken_Optional;
      --  An optional token that a server can use to report work done progress.

      textDocument : LSP.Structures.TextDocumentIdentifier;
      --  The document to format.

      options : LSP.Structures.FormattingOptions;
      --  The format options.

   end record;
   --  The parameters of a
   --  [DocumentFormattingRequest](#DocumentFormattingRequest).

   overriding function workDoneToken
     (Self : DocumentFormattingParams)
      return LSP.Structures.ProgressToken_Optional is
     (Self.workDoneToken);

   type DocumentFormattingRegistrationOptions is
   new DocumentFormattingOptions with record
      Parent : LSP.Structures.TextDocumentRegistrationOptions;
   end record;
   --  Registration options for a
   --  [DocumentFormattingRequest](#DocumentFormattingRequest).

   type DocumentHighlightKind_Optional (Is_Set : Boolean := False) is record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : DocumentHighlightKind;
      end case;
   end record;

   type DocumentHighlight is record
      a_range : LSP.Structures.A_Range;
      --  The range this highlight applies to.

      kind : DocumentHighlightKind_Optional;
      --  The highlight kind, default is [text](#DocumentHighlightKind.Text).

   end record;
   --  A document highlight is a range inside a text document which deserves
   --  special attention. Usually a document highlight is visualized by
   --  changing the background color of its range.

   type DocumentHighlightOptions is new WorkDoneProgressOptions with record
      workDoneProgress : Boolean_Optional;

   end record;
   --  Provider options for a
   --  [DocumentHighlightRequest](#DocumentHighlightRequest).

   overriding function workDoneProgress
     (Self : DocumentHighlightOptions) return Boolean_Optional is
     (Self.workDoneProgress);

   type DocumentHighlightParams is
   new TextDocumentPositionParams and WorkDoneProgressParams and
     PartialResultParams with record
      workDoneToken : ProgressToken_Optional;
      --  An optional token that a server can use to report work done progress.

      partialResultToken : ProgressToken_Optional;
      --  An optional token that a server can use to report partial results
      --  (e.g. streaming) to the client.

   end record;
   --  Parameters for a [DocumentHighlightRequest](#DocumentHighlightRequest).

   overriding function workDoneToken
     (Self : DocumentHighlightParams)
      return LSP.Structures.ProgressToken_Optional is
     (Self.workDoneToken);

   overriding function partialResultToken
     (Self : DocumentHighlightParams)
      return LSP.Structures.ProgressToken_Optional is
     (Self.partialResultToken);

   type DocumentHighlightRegistrationOptions is
   new DocumentHighlightOptions with record
      Parent : LSP.Structures.TextDocumentRegistrationOptions;
   end record;
   --  Registration options for a
   --  [DocumentHighlightRequest](#DocumentHighlightRequest).

   type DocumentLink is record
      a_range : LSP.Structures.A_Range;
      --  The range this link applies to.

      target : Virtual_String_Optional;
      --  The uri this link points to. If missing a resolve request is sent
      --  later.

      tooltip : Virtual_String_Optional;
      --  The tooltip text when you hover over this link.
      --
      --  If a tooltip is provided, is will be displayed in a string that
      --  includes instructions on how to trigger the link, such as `{0} (ctrl
      --  + click)`. The specific instructions vary depending on OS, user
      --  settings, and localization.
      --
      --  @since 3.15.0

      data : LSPAny_Optional;
      --  A data entry field that is preserved on a document link between a
      --  DocumentLinkRequest and a DocumentLinkResolveRequest.

   end record;
   --  A document link is a range in a text document that links to an internal
   --  or external resource, like another text document or a web site.

   type DocumentLinkOptions is new WorkDoneProgressOptions with record
      workDoneProgress : Boolean_Optional;

      resolveProvider : Boolean_Optional;
      --  Document links have a resolve provider as well.

   end record;
   --  Provider options for a [DocumentLinkRequest](#DocumentLinkRequest).

   overriding function workDoneProgress
     (Self : DocumentLinkOptions) return Boolean_Optional is
     (Self.workDoneProgress);

   type DocumentLinkParams is
   new WorkDoneProgressParams and PartialResultParams with record
      workDoneToken : ProgressToken_Optional;
      --  An optional token that a server can use to report work done progress.

      partialResultToken : ProgressToken_Optional;
      --  An optional token that a server can use to report partial results
      --  (e.g. streaming) to the client.

      textDocument : LSP.Structures.TextDocumentIdentifier;
      --  The document to provide document links for.

   end record;
   --  The parameters of a [DocumentLinkRequest](#DocumentLinkRequest).

   overriding function workDoneToken
     (Self : DocumentLinkParams)
      return LSP.Structures.ProgressToken_Optional is
     (Self.workDoneToken);

   overriding function partialResultToken
     (Self : DocumentLinkParams)
      return LSP.Structures.ProgressToken_Optional is
     (Self.partialResultToken);

   type DocumentLinkRegistrationOptions is new DocumentLinkOptions with record
      Parent : LSP.Structures.TextDocumentRegistrationOptions;
   end record;
   --  Registration options for a [DocumentLinkRequest](#DocumentLinkRequest).

   type DocumentOnTypeFormattingOptions is tagged record
      firstTriggerCharacter : LSP.Structures.Virtual_String;
      --  A character on which formatting should be triggered, like `{`.

      moreTriggerCharacter : LSP.Structures.Virtual_String_Vector;
      --  More trigger characters.

   end record;
   --  Provider options for a
   --  [DocumentOnTypeFormattingRequest](#DocumentOnTypeFormattingRequest).

   type DocumentOnTypeFormattingParams is record
      textDocument : LSP.Structures.TextDocumentIdentifier;
      --  The document to format.

      position : LSP.Structures.Position;
      --  The position around which the on type formatting should happen. This
      --  is not necessarily the exact position where the character denoted by
      --  the property `ch` got typed.

      ch : LSP.Structures.Virtual_String;
      --  The character that has been typed that triggered the formatting
      --  on type request. That is not necessarily the last character that
      --  got inserted into the document since the client could auto insert
      --  characters as well (e.g. like automatic brace completion).

      options : LSP.Structures.FormattingOptions;
      --  The formatting options.

   end record;
   --  The parameters of a
   --  [DocumentOnTypeFormattingRequest](#DocumentOnTypeFormattingRequest).

   type DocumentOnTypeFormattingRegistrationOptions is
   new DocumentOnTypeFormattingOptions with record
      Parent : LSP.Structures.TextDocumentRegistrationOptions;
   end record;
   --  Registration options for a
   --  [DocumentOnTypeFormattingRequest](#DocumentOnTypeFormattingRequest).

   type DocumentRangeFormattingOptions is
   new WorkDoneProgressOptions with record
      workDoneProgress : Boolean_Optional;

   end record;
   --  Provider options for a
   --  [DocumentRangeFormattingRequest](#DocumentRangeFormattingRequest).

   overriding function workDoneProgress
     (Self : DocumentRangeFormattingOptions) return Boolean_Optional is
     (Self.workDoneProgress);

   type DocumentRangeFormattingParams is new WorkDoneProgressParams with record
      workDoneToken : ProgressToken_Optional;
      --  An optional token that a server can use to report work done progress.

      textDocument : LSP.Structures.TextDocumentIdentifier;
      --  The document to format.

      a_range : LSP.Structures.A_Range;
      --  The range to format

      options : LSP.Structures.FormattingOptions;
      --  The format options

   end record;
   --  The parameters of a
   --  [DocumentRangeFormattingRequest](#DocumentRangeFormattingRequest).

   overriding function workDoneToken
     (Self : DocumentRangeFormattingParams)
      return LSP.Structures.ProgressToken_Optional is
     (Self.workDoneToken);

   type DocumentRangeFormattingRegistrationOptions is
   new DocumentRangeFormattingOptions with record
      Parent : LSP.Structures.TextDocumentRegistrationOptions;
   end record;
   --  Registration options for a
   --  [DocumentRangeFormattingRequest](#DocumentRangeFormattingRequest).

   type AlsVisibility_Optional (Is_Set : Boolean := False) is record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : AlsVisibility;
      end case;
   end record;

   type DocumentSymbol_Vector is tagged private with
     Variable_Indexing => Get_DocumentSymbol_Variable_Reference,
     Constant_Indexing => Get_DocumentSymbol_Constant_Reference;

   type DocumentSymbol is record
      name : LSP.Structures.Virtual_String;
      --  The name of this symbol. Will be displayed in the user interface and
      --  therefore must not be an empty string or a string only consisting of
      --  white spaces.

      detail : Virtual_String_Optional;
      --  More detail for this symbol, e.g the signature of a function.

      kind : LSP.Enumerations.SymbolKind;
      --  The kind of this symbol.

      tags : LSP.Structures.SymbolTag_Set;
      --  Tags for this document symbol.
      --
      --  @since 3.16.0

      deprecated : Boolean_Optional;
      --  Indicates if this symbol is deprecated.
      --
      --  @deprecated Use tags instead

      a_range : LSP.Structures.A_Range;
      --  The range enclosing this symbol not including leading/trailing
      --  whitespace but everything else like comments. This information is
      --  typically used to determine if the clients cursor is inside the
      --  symbol to reveal in the symbol in the UI.

      selectionRange : LSP.Structures.A_Range;
      --  The range that should be selected and revealed when this symbol is
      --  being picked, e.g the name of a function. Must be contained by the
      --  `range`.

      children : LSP.Structures.DocumentSymbol_Vector;
      --  Children of this symbol, e.g. properties of a class.

      alsIsDeclaration : Boolean_Optional;

      alsIsAdaProcedure : Boolean_Optional;

      alsVisibility : AlsVisibility_Optional;

   end record;
   --  Represents programming constructs like variables, classes, interfaces
   --  etc. that appear in a document. Document symbols can be hierarchical
   --  and they have two ranges: one that encloses its definition and one that
   --  points to its most interesting range, e.g. the range of an identifier.

   type DocumentSymbolOptions is new WorkDoneProgressOptions with record
      workDoneProgress : Boolean_Optional;

      label : Virtual_String_Optional;
      --  A human-readable string that is shown when multiple outlines trees
      --  are shown for the same document.
      --
      --  @since 3.16.0

   end record;
   --  Provider options for a [DocumentSymbolRequest](#DocumentSymbolRequest).

   overriding function workDoneProgress
     (Self : DocumentSymbolOptions) return Boolean_Optional is
     (Self.workDoneProgress);

   type AlsSearchKind_Optional (Is_Set : Boolean := False) is record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : AlsSearchKind;
      end case;
   end record;

   type DocumentSymbolParams is
   new WorkDoneProgressParams and PartialResultParams with record
      workDoneToken : ProgressToken_Optional;
      --  An optional token that a server can use to report work done progress.

      partialResultToken : ProgressToken_Optional;
      --  An optional token that a server can use to report partial results
      --  (e.g. streaming) to the client.

      textDocument : LSP.Structures.TextDocumentIdentifier;
      --  The text document.

      query : Virtual_String_Optional;
      --  A query string to filter symbols by. Clients may send an empty string
      --  here to request all symbols.

      case_sensitive : Boolean_Optional;
      --  To take letters' case into account.

      whole_word : Boolean_Optional;
      --  To match the whole word instead of a part of it.

      negate : Boolean_Optional;
      --  To invert matching.

      kind : AlsSearchKind_Optional;

   end record;
   --  Parameters for a [DocumentSymbolRequest](#DocumentSymbolRequest).

   overriding function workDoneToken
     (Self : DocumentSymbolParams)
      return LSP.Structures.ProgressToken_Optional is
     (Self.workDoneToken);

   overriding function partialResultToken
     (Self : DocumentSymbolParams)
      return LSP.Structures.ProgressToken_Optional is
     (Self.partialResultToken);

   type DocumentSymbolRegistrationOptions is
   new DocumentSymbolOptions with record
      Parent : LSP.Structures.TextDocumentRegistrationOptions;
   end record;
   --  Registration options for a
   --  [DocumentSymbolRequest](#DocumentSymbolRequest).

   type ExecuteCommandOptions is new WorkDoneProgressOptions with record
      workDoneProgress : Boolean_Optional;

      commands : LSP.Structures.Virtual_String_Vector;
      --  The commands to be executed on the server

   end record;
   --  The server capabilities of a
   --  [ExecuteCommandRequest](#ExecuteCommandRequest).

   overriding function workDoneProgress
     (Self : ExecuteCommandOptions) return Boolean_Optional is
     (Self.workDoneProgress);

   type ExecuteCommandParams is new WorkDoneProgressParams with record
      workDoneToken : ProgressToken_Optional;
      --  An optional token that a server can use to report work done progress.

      command : LSP.Structures.Virtual_String;
      --  The identifier of the actual command handler.

      arguments : LSP.Structures.LSPAny_Vector;
      --  Arguments that the command should be invoked with.

   end record;
   --  The parameters of a [ExecuteCommandRequest](#ExecuteCommandRequest).

   overriding function workDoneToken
     (Self : ExecuteCommandParams)
      return LSP.Structures.ProgressToken_Optional is
     (Self.workDoneToken);

   type ExecuteCommandRegistrationOptions is
   new ExecuteCommandOptions with record
      null;
   end record;
   --  Registration options for a
   --  [ExecuteCommandRequest](#ExecuteCommandRequest).

   type FileOperationPatternKind_Optional (Is_Set : Boolean := False) is record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : FileOperationPatternKind;
      end case;
   end record;

   type FileOperationPatternOptions is record
      ignoreCase : Boolean_Optional;
      --  The pattern should be matched ignoring casing.

   end record;
   --  Matching options for the file operation pattern.
   --
   --  @since 3.16.0

   type FileOperationPatternOptions_Optional (Is_Set : Boolean := False) is
   record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : FileOperationPatternOptions;
      end case;
   end record;

   type FileOperationPattern is record
      glob : LSP.Structures.Virtual_String;
      --  The glob pattern to match. Glob patterns can have the following
      --  syntax: - `*` to match one or more characters in a path segment -
      --  `?` to match on one character in a path segment - `**` to match any
      --  number of path segments, including none - `{}` to group sub patterns
      --  into an OR expression. (e.g. `**["200B"]/*.{ts,js}` matches all
      --  TypeScript and JavaScript files) - `[]` to declare a range of
      --  characters to match in a path segment (e.g., `example.[0-9]` to match
      --  on `example.0`, `example.1`, ["2026"]) - `[!...]` to negate a range
      --  of characters to match in a path segment (e.g., `example.[!0-9]` to
      --  match on `example.a`, `example.b`, but not `example.0`)

      matches : FileOperationPatternKind_Optional;
      --  Whether to match files or folders with this pattern.
      --
      --  Matches both if undefined.

      options : FileOperationPatternOptions_Optional;
      --  Additional options used during matching.

   end record;
   --  A pattern to describe in which file operation requests or notifications
   --  the server is interested in receiving.
   --
   --  @since 3.16.0

   type FileOperationFilter is record
      scheme : Virtual_String_Optional;
      --  A Uri scheme like `file` or `untitled`.

      pattern : LSP.Structures.FileOperationPattern;
      --  The actual file operation pattern.

   end record;
   --  A filter to describe in which file operation requests or notifications
   --  the server is interested in receiving.
   --
   --  @since 3.16.0

   package FileOperationFilter_Vectors is new Ada.Containers.Vectors
     (Positive, FileOperationFilter, "=");

   type FileOperationFilter_Vector is
   new FileOperationFilter_Vectors.Vector with null record;

   type FileOperationRegistrationOptions is record
      filters : LSP.Structures.FileOperationFilter_Vector;
      --  The actual filters.

   end record;
   --  The options to register for file operations.
   --
   --  @since 3.16.0

   type FileOperationRegistrationOptions_Optional (Is_Set : Boolean := False)
   is
   record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : FileOperationRegistrationOptions;
      end case;
   end record;

   type FileOperationOptions is record
      didCreate : FileOperationRegistrationOptions_Optional;
      --  The server is interested in receiving didCreateFiles notifications.

      willCreate : FileOperationRegistrationOptions_Optional;
      --  The server is interested in receiving willCreateFiles requests.

      didRename : FileOperationRegistrationOptions_Optional;
      --  The server is interested in receiving didRenameFiles notifications.

      willRename : FileOperationRegistrationOptions_Optional;
      --  The server is interested in receiving willRenameFiles requests.

      didDelete : FileOperationRegistrationOptions_Optional;
      --  The server is interested in receiving didDeleteFiles file
      --  notifications.

      willDelete : FileOperationRegistrationOptions_Optional;
      --  The server is interested in receiving willDeleteFiles file requests.

   end record;
   --  Options for notifications/requests for user operations on files.
   --
   --  @since 3.16.0

   type FileRename is record
      oldUri : LSP.Structures.Virtual_String;
      --  A file:// URI for the original location of the file/folder being
      --  renamed.

      newUri : LSP.Structures.Virtual_String;
      --  A file:// URI for the new location of the file/folder being renamed.

   end record;
   --  Represents information on a file/folder rename.
   --
   --  @since 3.16.0

   type FoldingRangeKind_Optional (Is_Set : Boolean := False) is record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : FoldingRangeKind;
      end case;
   end record;

   type FoldingRange is record
      startLine : Natural;
      --  The zero-based start line of the range to fold. The folded area
      --  starts after the line's last character. To be valid, the end must be
      --  zero or larger and smaller than the number of lines in the document.

      startCharacter : Natural_Optional;
      --  The zero-based character offset from where the folded range starts.
      --  If not defined, defaults to the length of the start line.

      endLine : Natural;
      --  The zero-based end line of the range to fold. The folded area ends
      --  with the line's last character. To be valid, the end must be zero
      --  or larger and smaller than the number of lines in the document.

      endCharacter : Natural_Optional;
      --  The zero-based character offset before the folded range ends. If not
      --  defined, defaults to the length of the end line.

      kind : FoldingRangeKind_Optional;
      --  Describes the kind of the folding range such as `comment'
      --  or 'region'. The kind is used to categorize folding
      --  ranges and used by commands like 'Fold all comments'. See
      --  [FoldingRangeKind](#FoldingRangeKind) for an enumeration
      --  of standardized kinds.

      collapsedText : Virtual_String_Optional;
      --  The text that the client should show when the specified range is
      --  collapsed. If not defined or not supported by the client, a default
      --  will be chosen by the client.
      --
      --  @since 3.17.0

   end record;
   --  Represents a folding range. To be valid, start and end line must be
   --  bigger than zero and smaller than the number of lines in the document.
   --  Clients are free to ignore invalid ranges.

   type FoldingRangeOptions is new WorkDoneProgressOptions with record
      workDoneProgress : Boolean_Optional;

   end record;

   overriding function workDoneProgress
     (Self : FoldingRangeOptions) return Boolean_Optional is
     (Self.workDoneProgress);

   type FoldingRangeParams is
   new WorkDoneProgressParams and PartialResultParams with record
      workDoneToken : ProgressToken_Optional;
      --  An optional token that a server can use to report work done progress.

      partialResultToken : ProgressToken_Optional;
      --  An optional token that a server can use to report partial results
      --  (e.g. streaming) to the client.

      textDocument : LSP.Structures.TextDocumentIdentifier;
      --  The text document.

   end record;
   --  Parameters for a [FoldingRangeRequest](#FoldingRangeRequest).

   overriding function workDoneToken
     (Self : FoldingRangeParams)
      return LSP.Structures.ProgressToken_Optional is
     (Self.workDoneToken);

   overriding function partialResultToken
     (Self : FoldingRangeParams)
      return LSP.Structures.ProgressToken_Optional is
     (Self.partialResultToken);

   type FoldingRangeRegistrationOptions is
   new FoldingRangeOptions and StaticRegistrationOptions with record
      id : Virtual_String_Optional;
      --  The id used to register the request. The id can be used to deregister
      --  the request again. See also Registration#id.

      Parent : LSP.Structures.TextDocumentRegistrationOptions;
   end record;

   overriding function id
     (Self : FoldingRangeRegistrationOptions) return Virtual_String_Optional is
     (Self.id);

   type MarkedString (Is_Virtual_String : Boolean := True) is record
      case Is_Virtual_String is
         when True =>
            Virtual_String : LSP.Structures.Virtual_String;
         when False =>
            language : LSP.Structures.Virtual_String;

            value : LSP.Structures.Virtual_String;

      end case;
   end record;
   --  MarkedString can be used to render human readable text. It is either
   --  a markdown string or a code-block that provides a language and a code
   --  snippet. The language identifier is semantically equal to the optional
   --  language identifier in fenced code blocks in GitHub issues. See
   --  https://help.github.com/articles/creating-and-highlighting-code-blocks/#syntax-highlighting
   --
   --  The pair of a language and a value is an equivalent to markdown:
   --  ```${language} ${value} ```
   --
   --  Note that markdown strings will be sanitized - that means html will be
   --  escaped. @deprecated use MarkupContent instead.

   package MarkedString_Vectors is new Ada.Containers.Vectors
     (Positive, MarkedString, "=");

   type MarkedString_Vector is
   new MarkedString_Vectors.Vector with null record;

   type MarkupContent_Or_MarkedString_Vector
     (Is_MarkupContent : Boolean := True) is
   record
      case Is_MarkupContent is
         when True =>
            MarkupContent : LSP.Structures.MarkupContent;
         when False =>
            MarkedString_Vector : LSP.Structures.MarkedString_Vector;
      end case;
   end record;

   type Hover is record
      contents : LSP.Structures.MarkupContent_Or_MarkedString_Vector;
      --  The hover's content

      a_range : A_Range_Optional;
      --  An optional range inside the text document that is used to visualize
      --  the hover, e.g. by changing the background color.

   end record;
   --  The result of a hover request.

   type HoverOptions is new WorkDoneProgressOptions with record
      workDoneProgress : Boolean_Optional;

   end record;
   --  Hover options.

   overriding function workDoneProgress
     (Self : HoverOptions) return Boolean_Optional is
     (Self.workDoneProgress);

   type HoverParams is
   new TextDocumentPositionParams and WorkDoneProgressParams with record
      workDoneToken : ProgressToken_Optional;
      --  An optional token that a server can use to report work done progress.

   end record;
   --  Parameters for a [HoverRequest](#HoverRequest).

   overriding function workDoneToken
     (Self : HoverParams) return LSP.Structures.ProgressToken_Optional is
     (Self.workDoneToken);

   type HoverRegistrationOptions is new HoverOptions with record
      Parent : LSP.Structures.TextDocumentRegistrationOptions;
   end record;
   --  Registration options for a [HoverRequest](#HoverRequest).

   type ImplementationOptions is new WorkDoneProgressOptions with record
      workDoneProgress : Boolean_Optional;

   end record;

   overriding function workDoneProgress
     (Self : ImplementationOptions) return Boolean_Optional is
     (Self.workDoneProgress);

   type ImplementationParams is
   new TextDocumentPositionParams and WorkDoneProgressParams and
     PartialResultParams with record
      workDoneToken : ProgressToken_Optional;
      --  An optional token that a server can use to report work done progress.

      partialResultToken : ProgressToken_Optional;
      --  An optional token that a server can use to report partial results
      --  (e.g. streaming) to the client.

      alsDisplayMethodAncestryOnNavigation : AlsDisplayMethodAncestryOnNavigationPolicy_Optional;
      --  whether or now we should list overriding/overridden subprograms.

   end record;

   overriding function workDoneToken
     (Self : ImplementationParams)
      return LSP.Structures.ProgressToken_Optional is
     (Self.workDoneToken);

   overriding function partialResultToken
     (Self : ImplementationParams)
      return LSP.Structures.ProgressToken_Optional is
     (Self.partialResultToken);

   type ImplementationRegistrationOptions is
   new ImplementationOptions and StaticRegistrationOptions with record
      id : Virtual_String_Optional;
      --  The id used to register the request. The id can be used to deregister
      --  the request again. See also Registration#id.

      Parent : LSP.Structures.TextDocumentRegistrationOptions;
   end record;

   overriding function id
     (Self : ImplementationRegistrationOptions)
      return Virtual_String_Optional is
     (Self.id);

   type InitializeError is record
      retry : Standard.Boolean;
      --  Indicates whether the client execute the following retry logic: (1)
      --  show the message provided by the ResponseError to the user (2) user
      --  selects retry or cancel (3) if user selected retry the initialize
      --  method is sent again.

   end record;
   --  The data type of the ResponseError if the initialize request fails.

   subtype WorkspaceFolder_Vector_Or_Null is WorkspaceFolder_Vector;

   type WorkspaceFolder_Vector_Or_Null_Optional (Is_Set : Boolean := False) is
   record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : WorkspaceFolder_Vector_Or_Null;
      end case;
   end record;

   type WorkspaceFoldersInitializeParams is tagged record
      workspaceFolders : WorkspaceFolder_Vector_Or_Null_Optional;
      --  The workspace folders configured in the client when the server
      --  starts.
      --
      --  This property is only available if the client supports workspace
      --  folders. It can be `null` if the client supports workspace folders
      --  but none are configured.
      --
      --  @since 3.6.0

   end record;

   type clientInfo_Of_InitializeParams is record
      name : LSP.Structures.Virtual_String;
      --  The name of the client as defined by the client.

      version : Virtual_String_Optional;
      --  The client's version as defined by the client.

   end record;
   type clientInfo_Of_InitializeParams_Optional (Is_Set : Boolean := False) is
   record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : clientInfo_Of_InitializeParams;
      end case;
   end record;

   type Virtual_String_Or_Null (Is_Null : Boolean := True) is record
      case Is_Null is
         when True =>
            null;
         when False =>
            Value : LSP.Structures.Virtual_String;
      end case;
   end record;

   type Virtual_String_Or_Null_Optional (Is_Set : Boolean := False) is record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : Virtual_String_Or_Null;
      end case;
   end record;

   type DocumentUri_Or_Null (Is_Null : Boolean := True) is record
      case Is_Null is
         when True =>
            null;
         when False =>
            Value : LSP.Structures.DocumentUri;
      end case;
   end record;

   type trace_Of_InitializeParams is (off, messages, compact, verbose);

   type trace_Of_InitializeParams_Optional (Is_Set : Boolean := False) is
   record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : trace_Of_InitializeParams;
      end case;
   end record;

   type An_InitializeParams is new WorkDoneProgressParams with record
      workDoneToken : ProgressToken_Optional;
      --  An optional token that a server can use to report work done progress.

      processId : LSP.Structures.Integer_Or_Null;
      --  The process Id of the parent process that started the server.
      --
      --  Is `null` if the process has not been started by another process. If
      --  the parent process is not alive then the server should exit.

      clientInfo : clientInfo_Of_InitializeParams_Optional;
      --  Information about the client
      --
      --  @since 3.15.0

      locale : Virtual_String_Optional;
      --  The locale the client is currently showing the user interface in.
      --  This must not necessarily be the locale of the operating system.
      --
      --  Uses IETF language tags as the value's syntax (See
      --  https://en.wikipedia.org/wiki/IETF_language_tag)
      --
      --  @since 3.16.0

      rootPath : Virtual_String_Or_Null_Optional;
      --  The rootPath of the workspace. Is null if no folder is open.
      --
      --  @deprecated in favour of rootUri.

      rootUri : LSP.Structures.DocumentUri_Or_Null;
      --  The rootUri of the workspace. Is null if no folder is open. If both
      --  `rootPath` and `rootUri` are set `rootUri` wins.
      --
      --  @deprecated in favour of workspaceFolders.

      capabilities : LSP.Structures.ClientCapabilities;
      --  The capabilities provided by the client (editor or tool)

      initializationOptions : LSPAny_Optional;
      --  User provided initialization options.

      trace : trace_Of_InitializeParams_Optional;
      --  The initial trace setting. If omitted trace is disabled ('off').

   end record;
   --  The initialize parameters

   overriding function workDoneToken
     (Self : An_InitializeParams)
      return LSP.Structures.ProgressToken_Optional is
     (Self.workDoneToken);

   type InitializeParams is new An_InitializeParams with record
      Parent : LSP.Structures.WorkspaceFoldersInitializeParams;
   end record;

   type MonikerOptions is new WorkDoneProgressOptions with record
      workDoneProgress : Boolean_Optional;

   end record;

   overriding function workDoneProgress
     (Self : MonikerOptions) return Boolean_Optional is
     (Self.workDoneProgress);

   type SemanticTokensLegend is record
      tokenTypes : LSP.Structures.Virtual_String_Vector;
      --  The token types a server uses.

      tokenModifiers : LSP.Structures.Virtual_String_Vector;
      --  The token modifiers a server uses.

   end record;
   --  @since 3.16.0

   type SemanticTokensOptions is new WorkDoneProgressOptions with record
      workDoneProgress : Boolean_Optional;

      legend : LSP.Structures.SemanticTokensLegend;
      --  The legend used by the server

      a_range : Boolean_Or_Any_Optional;
      --  Server supports providing semantic tokens for a specific range of a
      --  document.

      full : Boolean_Or_Something_Optional;
      --  Server supports providing semantic tokens for a full document.

   end record;
   --  @since 3.16.0

   overriding function workDoneProgress
     (Self : SemanticTokensOptions) return Boolean_Optional is
     (Self.workDoneProgress);

   type SemanticTokensRegistrationOptions is
   new SemanticTokensOptions and StaticRegistrationOptions with record
      id : Virtual_String_Optional;
      --  The id used to register the request. The id can be used to deregister
      --  the request again. See also Registration#id.

      Parent : LSP.Structures.TextDocumentRegistrationOptions;
   end record;
   --  @since 3.16.0

   overriding function id
     (Self : SemanticTokensRegistrationOptions)
      return Virtual_String_Optional is
     (Self.id);

   type Virtual_String_Or_Boolean (Is_Virtual_String : Boolean := True) is
   record
      case Is_Virtual_String is
         when True =>
            Virtual_String : LSP.Structures.Virtual_String;
         when False =>
            Boolean : Standard.Boolean;
      end case;
   end record;

   type Virtual_String_Or_Boolean_Optional (Is_Set : Boolean := False) is
   record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : Virtual_String_Or_Boolean;
      end case;
   end record;

   type WorkspaceFoldersServerCapabilities is record
      supported : Boolean_Optional;
      --  The server has support for workspace folders

      changeNotifications : Virtual_String_Or_Boolean_Optional;
      --  Whether the server wants to receive workspace folder change
      --  notifications.
      --
      --  If a string is provided the string is treated as an ID under
      --  which the notification is registered on the client side.
      --  The ID can be used to unregister for these events using
      --  the `client/unregisterCapability` request.

   end record;

   type WorkspaceFoldersServerCapabilities_Optional (Is_Set : Boolean := False)
   is
   record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : WorkspaceFoldersServerCapabilities;
      end case;
   end record;

   type SignatureHelpOptions is new WorkDoneProgressOptions with record
      workDoneProgress : Boolean_Optional;

      triggerCharacters : LSP.Structures.Virtual_String_Vector;
      --  List of characters that trigger signature help automatically.

      retriggerCharacters : LSP.Structures.Virtual_String_Vector;
      --  List of characters that re-trigger signature help.
      --
      --  These trigger characters are only active when signature help
      --  is already showing. All trigger characters are also counted
      --  as re-trigger characters.
      --
      --  @since 3.15.0

   end record;
   --  Server Capabilities for a [SignatureHelpRequest](#SignatureHelpRequest).

   overriding function workDoneProgress
     (Self : SignatureHelpOptions) return Boolean_Optional is
     (Self.workDoneProgress);

   type SignatureHelpOptions_Optional (Is_Set : Boolean := False) is record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : SignatureHelpOptions;
      end case;
   end record;

   type WorkspaceSymbolOptions is new WorkDoneProgressOptions with record
      workDoneProgress : Boolean_Optional;

      resolveProvider : Boolean_Optional;
      --  The server provides support to resolve additional information for a
      --  workspace symbol.
      --
      --  @since 3.17.0

   end record;
   --  Server capabilities for a
   --  [WorkspaceSymbolRequest](#WorkspaceSymbolRequest).

   overriding function workDoneProgress
     (Self : WorkspaceSymbolOptions) return Boolean_Optional is
     (Self.workDoneProgress);

   type TypeHierarchyOptions is new WorkDoneProgressOptions with record
      workDoneProgress : Boolean_Optional;

   end record;
   --  Type hierarchy options used during static registration.
   --
   --  @since 3.17.0

   overriding function workDoneProgress
     (Self : TypeHierarchyOptions) return Boolean_Optional is
     (Self.workDoneProgress);

   type InlineValueOptions is new WorkDoneProgressOptions with record
      workDoneProgress : Boolean_Optional;

   end record;
   --  Inline value options used during static registration.
   --
   --  @since 3.17.0

   overriding function workDoneProgress
     (Self : InlineValueOptions) return Boolean_Optional is
     (Self.workDoneProgress);

   type InlineValueRegistrationOptions is
   new InlineValueOptions and StaticRegistrationOptions with record
      id : Virtual_String_Optional;
      --  The id used to register the request. The id can be used to deregister
      --  the request again. See also Registration#id.

      Parent : LSP.Structures.TextDocumentRegistrationOptions;
   end record;
   --  Inline value options used during static or dynamic registration.
   --
   --  @since 3.17.0

   overriding function id
     (Self : InlineValueRegistrationOptions) return Virtual_String_Optional is
     (Self.id);

   type Virtual_String_Or_NotebookDocumentFilter_Optional
     (Is_Set : Boolean := False) is
   record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : Virtual_String_Or_NotebookDocumentFilter;
      end case;
   end record;

   type cells_OfnotebookSelector_OfNotebookDocumentSyncOptions_Item_Item is
   record
      language : LSP.Structures.Virtual_String;

   end record;
   package cells_OfnotebookSelector_OfNotebookDocumentSyncOptions_Item_Item_Vectors is new Ada
     .Containers
     .Vectors
     (Positive,
      cells_OfnotebookSelector_OfNotebookDocumentSyncOptions_Item_Item, "=");

   type cells_OfnotebookSelector_OfNotebookDocumentSyncOptions_Item is
   new cells_OfnotebookSelector_OfNotebookDocumentSyncOptions_Item_Item_Vectors
     .Vector with
   null record;

   type notebookSelector_OfNotebookDocumentSyncOptions_Item is record
      notebook : Virtual_String_Or_NotebookDocumentFilter_Optional;
      --  The notebook to be synced If a string value is provided it matches
      --  against the notebook type. '*' matches every notebook.

      cells : LSP.Structures
        .cells_OfnotebookSelector_OfNotebookDocumentSyncOptions_Item;
      --  The cells of the matching notebook to be synced.

   end record;

   package notebookSelector_OfNotebookDocumentSyncOptions_Item_Vectors is new Ada
     .Containers
     .Vectors
     (Positive, notebookSelector_OfNotebookDocumentSyncOptions_Item, "=");

   type notebookSelector_OfNotebookDocumentSyncOptions is
   new notebookSelector_OfNotebookDocumentSyncOptions_Item_Vectors.Vector with
   null record;

   type NotebookDocumentSyncOptions is tagged record
      notebookSelector : LSP.Structures
        .notebookSelector_OfNotebookDocumentSyncOptions;
      --  The notebooks to be synced

      save : Boolean_Optional;
      --  Whether save notification should be forwarded to the server. Will
      --  only be honored if mode === `notebook`.

   end record;
   --  Options specific to a notebook plus its cells to be synced to the
   --  server.
   --
   --  If a selector provides a notebook document filter but no cell selector
   --  all cells of a matching notebook document will be synced.
   --
   --  If a selector provides no notebook document filter but only a cell
   --  selector all notebook document that contain at least one matching
   --  cell will be synced.
   --
   --  @since 3.17.0

   type MonikerRegistrationOptions is new MonikerOptions with record
      Parent : LSP.Structures.TextDocumentRegistrationOptions;
   end record;

   type SelectionRangeOptions is new WorkDoneProgressOptions with record
      workDoneProgress : Boolean_Optional;

   end record;

   overriding function workDoneProgress
     (Self : SelectionRangeOptions) return Boolean_Optional is
     (Self.workDoneProgress);

   type SelectionRangeRegistrationOptions is
   new SelectionRangeOptions and StaticRegistrationOptions with record
      id : Virtual_String_Optional;
      --  The id used to register the request. The id can be used to deregister
      --  the request again. See also Registration#id.

      Parent : LSP.Structures.TextDocumentRegistrationOptions;
   end record;

   overriding function id
     (Self : SelectionRangeRegistrationOptions)
      return Virtual_String_Optional is
     (Self.id);

   type DocumentLinkOptions_Optional (Is_Set : Boolean := False) is record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : DocumentLinkOptions;
      end case;
   end record;

   type PositionEncodingKind_Optional (Is_Set : Boolean := False) is record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : PositionEncodingKind;
      end case;
   end record;

   type LinkedEditingRangeOptions is new WorkDoneProgressOptions with record
      workDoneProgress : Boolean_Optional;

   end record;

   overriding function workDoneProgress
     (Self : LinkedEditingRangeOptions) return Boolean_Optional is
     (Self.workDoneProgress);

   type TypeDefinitionOptions is new WorkDoneProgressOptions with record
      workDoneProgress : Boolean_Optional;

   end record;

   overriding function workDoneProgress
     (Self : TypeDefinitionOptions) return Boolean_Optional is
     (Self.workDoneProgress);

   type TypeDefinitionRegistrationOptions is
   new TypeDefinitionOptions and StaticRegistrationOptions with record
      id : Virtual_String_Optional;
      --  The id used to register the request. The id can be used to deregister
      --  the request again. See also Registration#id.

      Parent : LSP.Structures.TextDocumentRegistrationOptions;
   end record;

   overriding function id
     (Self : TypeDefinitionRegistrationOptions)
      return Virtual_String_Optional is
     (Self.id);

   type LinkedEditingRangeRegistrationOptions is
   new LinkedEditingRangeOptions and StaticRegistrationOptions with record
      id : Virtual_String_Optional;
      --  The id used to register the request. The id can be used to deregister
      --  the request again. See also Registration#id.

      Parent : LSP.Structures.TextDocumentRegistrationOptions;
   end record;

   overriding function id
     (Self : LinkedEditingRangeRegistrationOptions)
      return Virtual_String_Optional is
     (Self.id);

   type InlayHintOptions is new WorkDoneProgressOptions with record
      workDoneProgress : Boolean_Optional;

      resolveProvider : Boolean_Optional;
      --  The server provides support to resolve additional information for an
      --  inlay hint item.

   end record;
   --  Inlay hint options used during static registration.
   --
   --  @since 3.17.0

   overriding function workDoneProgress
     (Self : InlayHintOptions) return Boolean_Optional is
     (Self.workDoneProgress);

   type TextDocumentSyncKind_Optional (Is_Set : Boolean := False) is record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : TextDocumentSyncKind;
      end case;
   end record;

   type SaveOptions is tagged record
      includeText : Boolean_Optional;
      --  The client is supposed to include the content on save.

   end record;
   --  Save options.

   type Boolean_Or_SaveOptions (Is_Boolean : Boolean := True) is record
      case Is_Boolean is
         when True =>
            Boolean : Standard.Boolean;
         when False =>
            SaveOptions : LSP.Structures.SaveOptions;
      end case;
   end record;

   type Boolean_Or_SaveOptions_Optional (Is_Set : Boolean := False) is record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : Boolean_Or_SaveOptions;
      end case;
   end record;

   type TextDocumentSyncOptions is record
      openClose : Boolean_Optional;
      --  Open and close notifications are sent to the server. If omitted open
      --  close notifications should not be sent.

      change : TextDocumentSyncKind_Optional;
      --  Change notifications are sent to the server. See
      --  TextDocumentSyncKind.None, TextDocumentSyncKind.Full and
      --  TextDocumentSyncKind.Incremental. If omitted it defaults
      --  to TextDocumentSyncKind.None.

      willSave : Boolean_Optional;
      --  If present will save notifications are sent to the server. If omitted
      --  the notification should not be sent.

      willSaveWaitUntil : Boolean_Optional;
      --  If present will save wait until requests are sent to the server. If
      --  omitted the request should not be sent.

      save : Boolean_Or_SaveOptions_Optional;
      --  If present save notifications are sent to the server. If omitted the
      --  notification should not be sent.

   end record;

   type NotebookDocumentSyncRegistrationOptions is
   new NotebookDocumentSyncOptions and StaticRegistrationOptions with record
      id : Virtual_String_Optional;
      --  The id used to register the request. The id can be used to deregister
      --  the request again. See also Registration#id.

   end record;
   --  Registration options specific to a notebook.
   --
   --  @since 3.17.0

   overriding function id
     (Self : NotebookDocumentSyncRegistrationOptions)
      return Virtual_String_Optional is
     (Self.id);

   type TypeHierarchyRegistrationOptions is
   new TypeHierarchyOptions and StaticRegistrationOptions with record
      id : Virtual_String_Optional;
      --  The id used to register the request. The id can be used to deregister
      --  the request again. See also Registration#id.

      Parent : LSP.Structures.TextDocumentRegistrationOptions;
   end record;
   --  Type hierarchy options used during static or dynamic registration.
   --
   --  @since 3.17.0

   overriding function id
     (Self : TypeHierarchyRegistrationOptions)
      return Virtual_String_Optional is
     (Self.id);

   type CompletionOptions_Optional (Is_Set : Boolean := False) is record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : CompletionOptions;
      end case;
   end record;

   type DocumentOnTypeFormattingOptions_Optional (Is_Set : Boolean := False) is
   record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : DocumentOnTypeFormattingOptions;
      end case;
   end record;

   type FileOperationOptions_Optional (Is_Set : Boolean := False) is record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : FileOperationOptions;
      end case;
   end record;

   type ExecuteCommandOptions_Optional (Is_Set : Boolean := False) is record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : ExecuteCommandOptions;
      end case;
   end record;

   type ReferenceOptions is new WorkDoneProgressOptions with record
      workDoneProgress : Boolean_Optional;

   end record;
   --  Reference options.

   overriding function workDoneProgress
     (Self : ReferenceOptions) return Boolean_Optional is
     (Self.workDoneProgress);

   type InlayHintRegistrationOptions is
   new InlayHintOptions and StaticRegistrationOptions with record
      id : Virtual_String_Optional;
      --  The id used to register the request. The id can be used to deregister
      --  the request again. See also Registration#id.

      Parent : LSP.Structures.TextDocumentRegistrationOptions;
   end record;
   --  Inlay hint options used during static or dynamic registration.
   --
   --  @since 3.17.0

   overriding function id
     (Self : InlayHintRegistrationOptions) return Virtual_String_Optional is
     (Self.id);

   type CodeLensOptions_Optional (Is_Set : Boolean := False) is record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : CodeLensOptions;
      end case;
   end record;

   type RenameOptions is new WorkDoneProgressOptions with record
      workDoneProgress : Boolean_Optional;

      prepareProvider : Boolean_Optional;
      --  Renames should be checked and tested before being executed.
      --
      --  @since version 3.12.0

   end record;
   --  Provider options for a [RenameRequest](#RenameRequest).

   overriding function workDoneProgress
     (Self : RenameOptions) return Boolean_Optional is
     (Self.workDoneProgress);

   type T is record
      null;
   end record;

   type T_Optional (Is_Set : Boolean := False) is record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : T;
      end case;
   end record;

   type TextDocumentSyncOptions_Or_TextDocumentSyncKind
     (Is_TextDocumentSyncOptions : Boolean := True) is
   record
      case Is_TextDocumentSyncOptions is
         when True =>
            TextDocumentSyncOptions : LSP.Structures.TextDocumentSyncOptions;
         when False =>
            TextDocumentSyncKind : LSP.Enumerations.TextDocumentSyncKind;
      end case;
   end record;

   type TextDocumentSyncOptions_Or_TextDocumentSyncKind_Optional
     (Is_Set : Boolean := False) is
   record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : TextDocumentSyncOptions_Or_TextDocumentSyncKind;
      end case;
   end record;

   type NotebookDocumentSyncOptions_Or_NotebookDocumentSyncRegistrationOptions
     (Is_NotebookDocumentSyncOptions : Boolean := True) is
   record
      case Is_NotebookDocumentSyncOptions is
         when True =>
            NotebookDocumentSyncOptions : LSP.Structures
              .NotebookDocumentSyncOptions;
         when False =>
            NotebookDocumentSyncRegistrationOptions : LSP.Structures
              .NotebookDocumentSyncRegistrationOptions;
      end case;
   end record;

   type NotebookDocumentSyncOptions_Or_NotebookDocumentSyncRegistrationOptions_Optional
     (Is_Set : Boolean := False) is
   record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : NotebookDocumentSyncOptions_Or_NotebookDocumentSyncRegistrationOptions;
      end case;
   end record;

   type Boolean_Or_HoverOptions (Is_Boolean : Boolean := True) is record
      case Is_Boolean is
         when True =>
            Boolean : Standard.Boolean;
         when False =>
            HoverOptions : LSP.Structures.HoverOptions;
      end case;
   end record;

   type Boolean_Or_HoverOptions_Optional (Is_Set : Boolean := False) is record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : Boolean_Or_HoverOptions;
      end case;
   end record;

   type declarationProvider_OfServerCapabilities_Variant is
     (Variant_1, Variant_2, Variant_3);

   type declarationProvider_OfServerCapabilities
     (Kind : declarationProvider_OfServerCapabilities_Variant :=
        declarationProvider_OfServerCapabilities_Variant'First)
   is
   record
      case Kind is
         when Variant_1 =>
            Variant_1 : Standard.Boolean;
         when Variant_2 =>
            Variant_2 : LSP.Structures.DeclarationOptions;
         when Variant_3 =>
            Variant_3 : LSP.Structures.DeclarationRegistrationOptions;
      end case;
   end record;

   type declarationProvider_OfServerCapabilities_Optional
     (Is_Set : Boolean := False) is
   record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : declarationProvider_OfServerCapabilities;
      end case;
   end record;

   type Boolean_Or_DefinitionOptions (Is_Boolean : Boolean := True) is record
      case Is_Boolean is
         when True =>
            Boolean : Standard.Boolean;
         when False =>
            DefinitionOptions : LSP.Structures.DefinitionOptions;
      end case;
   end record;

   type Boolean_Or_DefinitionOptions_Optional (Is_Set : Boolean := False) is
   record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : Boolean_Or_DefinitionOptions;
      end case;
   end record;

   type typeDefinitionProvider_OfServerCapabilities_Variant is
     (Variant_1, Variant_2, Variant_3);

   type typeDefinitionProvider_OfServerCapabilities
     (Kind : typeDefinitionProvider_OfServerCapabilities_Variant :=
        typeDefinitionProvider_OfServerCapabilities_Variant'First)
   is
   record
      case Kind is
         when Variant_1 =>
            Variant_1 : Standard.Boolean;
         when Variant_2 =>
            Variant_2 : LSP.Structures.TypeDefinitionOptions;
         when Variant_3 =>
            Variant_3 : LSP.Structures.TypeDefinitionRegistrationOptions;
      end case;
   end record;

   type typeDefinitionProvider_OfServerCapabilities_Optional
     (Is_Set : Boolean := False) is
   record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : typeDefinitionProvider_OfServerCapabilities;
      end case;
   end record;

   type implementationProvider_OfServerCapabilities_Variant is
     (Variant_1, Variant_2, Variant_3);

   type implementationProvider_OfServerCapabilities
     (Kind : implementationProvider_OfServerCapabilities_Variant :=
        implementationProvider_OfServerCapabilities_Variant'First)
   is
   record
      case Kind is
         when Variant_1 =>
            Variant_1 : Standard.Boolean;
         when Variant_2 =>
            Variant_2 : LSP.Structures.ImplementationOptions;
         when Variant_3 =>
            Variant_3 : LSP.Structures.ImplementationRegistrationOptions;
      end case;
   end record;

   type implementationProvider_OfServerCapabilities_Optional
     (Is_Set : Boolean := False) is
   record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : implementationProvider_OfServerCapabilities;
      end case;
   end record;

   type Boolean_Or_ReferenceOptions (Is_Boolean : Boolean := True) is record
      case Is_Boolean is
         when True =>
            Boolean : Standard.Boolean;
         when False =>
            ReferenceOptions : LSP.Structures.ReferenceOptions;
      end case;
   end record;

   type Boolean_Or_ReferenceOptions_Optional (Is_Set : Boolean := False) is
   record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : Boolean_Or_ReferenceOptions;
      end case;
   end record;

   type Boolean_Or_DocumentHighlightOptions (Is_Boolean : Boolean := True) is
   record
      case Is_Boolean is
         when True =>
            Boolean : Standard.Boolean;
         when False =>
            DocumentHighlightOptions : LSP.Structures.DocumentHighlightOptions;
      end case;
   end record;

   type Boolean_Or_DocumentHighlightOptions_Optional
     (Is_Set : Boolean := False) is
   record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : Boolean_Or_DocumentHighlightOptions;
      end case;
   end record;

   type Boolean_Or_DocumentSymbolOptions (Is_Boolean : Boolean := True) is
   record
      case Is_Boolean is
         when True =>
            Boolean : Standard.Boolean;
         when False =>
            DocumentSymbolOptions : LSP.Structures.DocumentSymbolOptions;
      end case;
   end record;

   type Boolean_Or_DocumentSymbolOptions_Optional (Is_Set : Boolean := False)
   is
   record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : Boolean_Or_DocumentSymbolOptions;
      end case;
   end record;

   type Boolean_Or_CodeActionOptions (Is_Boolean : Boolean := True) is record
      case Is_Boolean is
         when True =>
            Boolean : Standard.Boolean;
         when False =>
            CodeActionOptions : LSP.Structures.CodeActionOptions;
      end case;
   end record;

   type Boolean_Or_CodeActionOptions_Optional (Is_Set : Boolean := False) is
   record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : Boolean_Or_CodeActionOptions;
      end case;
   end record;

   type colorProvider_OfServerCapabilities_Variant is
     (Variant_1, Variant_2, Variant_3);

   type colorProvider_OfServerCapabilities
     (Kind : colorProvider_OfServerCapabilities_Variant :=
        colorProvider_OfServerCapabilities_Variant'First)
   is
   record
      case Kind is
         when Variant_1 =>
            Variant_1 : Standard.Boolean;
         when Variant_2 =>
            Variant_2 : LSP.Structures.DocumentColorOptions;
         when Variant_3 =>
            Variant_3 : LSP.Structures.DocumentColorRegistrationOptions;
      end case;
   end record;

   type colorProvider_OfServerCapabilities_Optional (Is_Set : Boolean := False)
   is
   record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : colorProvider_OfServerCapabilities;
      end case;
   end record;

   type Boolean_Or_WorkspaceSymbolOptions (Is_Boolean : Boolean := True) is
   record
      case Is_Boolean is
         when True =>
            Boolean : Standard.Boolean;
         when False =>
            WorkspaceSymbolOptions : LSP.Structures.WorkspaceSymbolOptions;
      end case;
   end record;

   type Boolean_Or_WorkspaceSymbolOptions_Optional (Is_Set : Boolean := False)
   is
   record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : Boolean_Or_WorkspaceSymbolOptions;
      end case;
   end record;

   type Boolean_Or_DocumentFormattingOptions (Is_Boolean : Boolean := True) is
   record
      case Is_Boolean is
         when True =>
            Boolean : Standard.Boolean;
         when False =>
            DocumentFormattingOptions : LSP.Structures
              .DocumentFormattingOptions;
      end case;
   end record;

   type Boolean_Or_DocumentFormattingOptions_Optional
     (Is_Set : Boolean := False) is
   record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : Boolean_Or_DocumentFormattingOptions;
      end case;
   end record;

   type Boolean_Or_DocumentRangeFormattingOptions
     (Is_Boolean : Boolean := True) is
   record
      case Is_Boolean is
         when True =>
            Boolean : Standard.Boolean;
         when False =>
            DocumentRangeFormattingOptions : LSP.Structures
              .DocumentRangeFormattingOptions;
      end case;
   end record;

   type Boolean_Or_DocumentRangeFormattingOptions_Optional
     (Is_Set : Boolean := False) is
   record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : Boolean_Or_DocumentRangeFormattingOptions;
      end case;
   end record;

   type Boolean_Or_RenameOptions (Is_Boolean : Boolean := True) is record
      case Is_Boolean is
         when True =>
            Boolean : Standard.Boolean;
         when False =>
            RenameOptions : LSP.Structures.RenameOptions;
      end case;
   end record;

   type Boolean_Or_RenameOptions_Optional (Is_Set : Boolean := False) is record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : Boolean_Or_RenameOptions;
      end case;
   end record;

   type foldingRangeProvider_OfServerCapabilities_Variant is
     (Variant_1, Variant_2, Variant_3);

   type foldingRangeProvider_OfServerCapabilities
     (Kind : foldingRangeProvider_OfServerCapabilities_Variant :=
        foldingRangeProvider_OfServerCapabilities_Variant'First)
   is
   record
      case Kind is
         when Variant_1 =>
            Variant_1 : Standard.Boolean;
         when Variant_2 =>
            Variant_2 : LSP.Structures.FoldingRangeOptions;
         when Variant_3 =>
            Variant_3 : LSP.Structures.FoldingRangeRegistrationOptions;
      end case;
   end record;

   type foldingRangeProvider_OfServerCapabilities_Optional
     (Is_Set : Boolean := False) is
   record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : foldingRangeProvider_OfServerCapabilities;
      end case;
   end record;

   type selectionRangeProvider_OfServerCapabilities_Variant is
     (Variant_1, Variant_2, Variant_3);

   type selectionRangeProvider_OfServerCapabilities
     (Kind : selectionRangeProvider_OfServerCapabilities_Variant :=
        selectionRangeProvider_OfServerCapabilities_Variant'First)
   is
   record
      case Kind is
         when Variant_1 =>
            Variant_1 : Standard.Boolean;
         when Variant_2 =>
            Variant_2 : LSP.Structures.SelectionRangeOptions;
         when Variant_3 =>
            Variant_3 : LSP.Structures.SelectionRangeRegistrationOptions;
      end case;
   end record;

   type selectionRangeProvider_OfServerCapabilities_Optional
     (Is_Set : Boolean := False) is
   record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : selectionRangeProvider_OfServerCapabilities;
      end case;
   end record;

   type callHierarchyProvider_OfServerCapabilities_Variant is
     (Variant_1, Variant_2, Variant_3);

   type callHierarchyProvider_OfServerCapabilities
     (Kind : callHierarchyProvider_OfServerCapabilities_Variant :=
        callHierarchyProvider_OfServerCapabilities_Variant'First)
   is
   record
      case Kind is
         when Variant_1 =>
            Variant_1 : Standard.Boolean;
         when Variant_2 =>
            Variant_2 : LSP.Structures.CallHierarchyOptions;
         when Variant_3 =>
            Variant_3 : LSP.Structures.CallHierarchyRegistrationOptions;
      end case;
   end record;

   type callHierarchyProvider_OfServerCapabilities_Optional
     (Is_Set : Boolean := False) is
   record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : callHierarchyProvider_OfServerCapabilities;
      end case;
   end record;

   type linkedEditingRangeProvider_OfServerCapabilities_Variant is
     (Variant_1, Variant_2, Variant_3);

   type linkedEditingRangeProvider_OfServerCapabilities
     (Kind : linkedEditingRangeProvider_OfServerCapabilities_Variant :=
        linkedEditingRangeProvider_OfServerCapabilities_Variant'First)
   is
   record
      case Kind is
         when Variant_1 =>
            Variant_1 : Standard.Boolean;
         when Variant_2 =>
            Variant_2 : LSP.Structures.LinkedEditingRangeOptions;
         when Variant_3 =>
            Variant_3 : LSP.Structures.LinkedEditingRangeRegistrationOptions;
      end case;
   end record;

   type linkedEditingRangeProvider_OfServerCapabilities_Optional
     (Is_Set : Boolean := False) is
   record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : linkedEditingRangeProvider_OfServerCapabilities;
      end case;
   end record;

   type SemanticTokensOptions_Or_SemanticTokensRegistrationOptions
     (Is_SemanticTokensOptions : Boolean := True) is
   record
      case Is_SemanticTokensOptions is
         when True =>
            SemanticTokensOptions : LSP.Structures.SemanticTokensOptions;
         when False =>
            SemanticTokensRegistrationOptions : LSP.Structures
              .SemanticTokensRegistrationOptions;
      end case;
   end record;

   type SemanticTokensOptions_Or_SemanticTokensRegistrationOptions_Optional
     (Is_Set : Boolean := False) is
   record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : SemanticTokensOptions_Or_SemanticTokensRegistrationOptions;
      end case;
   end record;

   type monikerProvider_OfServerCapabilities_Variant is
     (Variant_1, Variant_2, Variant_3);

   type monikerProvider_OfServerCapabilities
     (Kind : monikerProvider_OfServerCapabilities_Variant :=
        monikerProvider_OfServerCapabilities_Variant'First)
   is
   record
      case Kind is
         when Variant_1 =>
            Variant_1 : Standard.Boolean;
         when Variant_2 =>
            Variant_2 : LSP.Structures.MonikerOptions;
         when Variant_3 =>
            Variant_3 : LSP.Structures.MonikerRegistrationOptions;
      end case;
   end record;

   type monikerProvider_OfServerCapabilities_Optional
     (Is_Set : Boolean := False) is
   record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : monikerProvider_OfServerCapabilities;
      end case;
   end record;

   type typeHierarchyProvider_OfServerCapabilities_Variant is
     (Variant_1, Variant_2, Variant_3);

   type typeHierarchyProvider_OfServerCapabilities
     (Kind : typeHierarchyProvider_OfServerCapabilities_Variant :=
        typeHierarchyProvider_OfServerCapabilities_Variant'First)
   is
   record
      case Kind is
         when Variant_1 =>
            Variant_1 : Standard.Boolean;
         when Variant_2 =>
            Variant_2 : LSP.Structures.TypeHierarchyOptions;
         when Variant_3 =>
            Variant_3 : LSP.Structures.TypeHierarchyRegistrationOptions;
      end case;
   end record;

   type typeHierarchyProvider_OfServerCapabilities_Optional
     (Is_Set : Boolean := False) is
   record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : typeHierarchyProvider_OfServerCapabilities;
      end case;
   end record;

   type inlineValueProvider_OfServerCapabilities_Variant is
     (Variant_1, Variant_2, Variant_3);

   type inlineValueProvider_OfServerCapabilities
     (Kind : inlineValueProvider_OfServerCapabilities_Variant :=
        inlineValueProvider_OfServerCapabilities_Variant'First)
   is
   record
      case Kind is
         when Variant_1 =>
            Variant_1 : Standard.Boolean;
         when Variant_2 =>
            Variant_2 : LSP.Structures.InlineValueOptions;
         when Variant_3 =>
            Variant_3 : LSP.Structures.InlineValueRegistrationOptions;
      end case;
   end record;

   type inlineValueProvider_OfServerCapabilities_Optional
     (Is_Set : Boolean := False) is
   record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : inlineValueProvider_OfServerCapabilities;
      end case;
   end record;

   type inlayHintProvider_OfServerCapabilities_Variant is
     (Variant_1, Variant_2, Variant_3);

   type inlayHintProvider_OfServerCapabilities
     (Kind : inlayHintProvider_OfServerCapabilities_Variant :=
        inlayHintProvider_OfServerCapabilities_Variant'First)
   is
   record
      case Kind is
         when Variant_1 =>
            Variant_1 : Standard.Boolean;
         when Variant_2 =>
            Variant_2 : LSP.Structures.InlayHintOptions;
         when Variant_3 =>
            Variant_3 : LSP.Structures.InlayHintRegistrationOptions;
      end case;
   end record;

   type inlayHintProvider_OfServerCapabilities_Optional
     (Is_Set : Boolean := False) is
   record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : inlayHintProvider_OfServerCapabilities;
      end case;
   end record;

   type DiagnosticOptions_Or_DiagnosticRegistrationOptions
     (Is_DiagnosticOptions : Boolean := True) is
   record
      case Is_DiagnosticOptions is
         when True =>
            DiagnosticOptions : LSP.Structures.DiagnosticOptions;
         when False =>
            DiagnosticRegistrationOptions : LSP.Structures
              .DiagnosticRegistrationOptions;
      end case;
   end record;

   type DiagnosticOptions_Or_DiagnosticRegistrationOptions_Optional
     (Is_Set : Boolean := False) is
   record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : DiagnosticOptions_Or_DiagnosticRegistrationOptions;
      end case;
   end record;

   type workspace_OfServerCapabilities is record
      workspaceFolders : WorkspaceFoldersServerCapabilities_Optional;
      --  The server supports workspace folder.
      --
      --  @since 3.6.0

      fileOperations : FileOperationOptions_Optional;
      --  The server is interested in notifications/requests for operations on
      --  files.
      --
      --  @since 3.16.0

   end record;
   type workspace_OfServerCapabilities_Optional (Is_Set : Boolean := False) is
   record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : workspace_OfServerCapabilities;
      end case;
   end record;

   type ServerCapabilities is record
      positionEncoding : PositionEncodingKind_Optional;
      --  The position encoding the server picked from the encodings offered by
      --  the client via the client capability `general.positionEncodings`.
      --
      --  If the client didn't provide any position encodings the only valid
      --  value that a server can return is 'utf-16'.
      --
      --  If omitted it defaults to 'utf-16'.
      --
      --  @since 3.17.0

      textDocumentSync : TextDocumentSyncOptions_Or_TextDocumentSyncKind_Optional;
      --  Defines how text documents are synced. Is either a detailed structure
      --  defining each notification or for backwards compatibility the
      --  TextDocumentSyncKind number.

      notebookDocumentSync : NotebookDocumentSyncOptions_Or_NotebookDocumentSyncRegistrationOptions_Optional;
      --  Defines how notebook documents are synced.
      --
      --  @since 3.17.0

      completionProvider : CompletionOptions_Optional;
      --  The server provides completion support.

      hoverProvider : Boolean_Or_HoverOptions_Optional;
      --  The server provides hover support.

      signatureHelpProvider : SignatureHelpOptions_Optional;
      --  The server provides signature help support.

      declarationProvider : declarationProvider_OfServerCapabilities_Optional;
      --  The server provides Goto Declaration support.

      definitionProvider : Boolean_Or_DefinitionOptions_Optional;
      --  The server provides goto definition support.

      typeDefinitionProvider : typeDefinitionProvider_OfServerCapabilities_Optional;
      --  The server provides Goto Type Definition support.

      implementationProvider : implementationProvider_OfServerCapabilities_Optional;
      --  The server provides Goto Implementation support.

      referencesProvider : Boolean_Or_ReferenceOptions_Optional;
      --  The server provides find references support.

      documentHighlightProvider : Boolean_Or_DocumentHighlightOptions_Optional;
      --  The server provides document highlight support.

      documentSymbolProvider : Boolean_Or_DocumentSymbolOptions_Optional;
      --  The server provides document symbol support.

      codeActionProvider : Boolean_Or_CodeActionOptions_Optional;
      --  The server provides code actions. CodeActionOptions may
      --  only be specified if the client states that it supports
      --  `codeActionLiteralSupport` in its initial `initialize` request.

      codeLensProvider : CodeLensOptions_Optional;
      --  The server provides code lens.

      documentLinkProvider : DocumentLinkOptions_Optional;
      --  The server provides document link support.

      colorProvider : colorProvider_OfServerCapabilities_Optional;
      --  The server provides color provider support.

      workspaceSymbolProvider : Boolean_Or_WorkspaceSymbolOptions_Optional;
      --  The server provides workspace symbol support.

      documentFormattingProvider : Boolean_Or_DocumentFormattingOptions_Optional;
      --  The server provides document formatting.

      documentRangeFormattingProvider : Boolean_Or_DocumentRangeFormattingOptions_Optional;
      --  The server provides document range formatting.

      documentOnTypeFormattingProvider : DocumentOnTypeFormattingOptions_Optional;
      --  The server provides document formatting on typing.

      renameProvider : Boolean_Or_RenameOptions_Optional;
      --  The server provides rename support. RenameOptions may only be
      --  specified if the client states that it supports `prepareSupport`
      --  in its initial `initialize` request.

      foldingRangeProvider : foldingRangeProvider_OfServerCapabilities_Optional;
      --  The server provides folding provider support.

      selectionRangeProvider : selectionRangeProvider_OfServerCapabilities_Optional;
      --  The server provides selection range support.

      executeCommandProvider : ExecuteCommandOptions_Optional;
      --  The server provides execute command support.

      callHierarchyProvider : callHierarchyProvider_OfServerCapabilities_Optional;
      --  The server provides call hierarchy support.
      --
      --  @since 3.16.0

      linkedEditingRangeProvider : linkedEditingRangeProvider_OfServerCapabilities_Optional;
      --  The server provides linked editing range support.
      --
      --  @since 3.16.0

      semanticTokensProvider : SemanticTokensOptions_Or_SemanticTokensRegistrationOptions_Optional;
      --  The server provides semantic tokens support.
      --
      --  @since 3.16.0

      monikerProvider : monikerProvider_OfServerCapabilities_Optional;
      --  The server provides moniker support.
      --
      --  @since 3.16.0

      typeHierarchyProvider : typeHierarchyProvider_OfServerCapabilities_Optional;
      --  The server provides type hierarchy support.
      --
      --  @since 3.17.0

      inlineValueProvider : inlineValueProvider_OfServerCapabilities_Optional;
      --  The server provides inline values.
      --
      --  @since 3.17.0

      inlayHintProvider : inlayHintProvider_OfServerCapabilities_Optional;
      --  The server provides inlay hints.
      --
      --  @since 3.17.0

      diagnosticProvider : DiagnosticOptions_Or_DiagnosticRegistrationOptions_Optional;
      --  The server has support for pull model diagnostics.
      --
      --  @since 3.17.0

      workspace : workspace_OfServerCapabilities_Optional;
      --  Workspace specific server capabilities.

      experimental : T_Optional;
      --  Experimental server capabilities.

      alsReferenceKinds : LSP.Structures.AlsReferenceKind_Set;
      --  List of reference kind supported by the server.

   end record;
   --  Defines the capabilities provided by a language server.

   type InitializeResult is record
      capabilities : LSP.Structures.ServerCapabilities;
      --  The capabilities the language server provides.

      serverInfo : clientInfo_Of_InitializeParams_Optional;
      --  Information about the server.
      --
      --  @since 3.15.0

   end record;
   --  The result returned from an initialize request.

   type InitializedParams is record
      null;
   end record;

   type Location_Optional (Is_Set : Boolean := False) is record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : Location;
      end case;
   end record;

   type InlayHintLabelPart is record
      value : LSP.Structures.Virtual_String;
      --  The value of this label part.

      tooltip : Virtual_String_Or_MarkupContent_Optional;
      --  The tooltip text when you hover over this label part. Depending
      --  on the client capability `inlayHint.resolveSupport` clients might
      --  resolve this property late using the resolve request.

      location : Location_Optional;
      --  An optional source code location that represents this label part.
      --
      --  The editor will use this location for the hover and for code
      --  navigation features: This part will become a clickable link that
      --  resolves to the definition of the symbol at the given location (not
      --  necessarily the location itself), it shows the hover that shows at
      --  the given location, and it shows a context menu with further code
      --  navigation commands.
      --
      --  Depending on the client capability `inlayHint.resolveSupport` clients
      --  might resolve this property late using the resolve request.

      command : Command_Optional;
      --  An optional command for this label part.
      --
      --  Depending on the client capability `inlayHint.resolveSupport` clients
      --  might resolve this property late using the resolve request.

   end record;
   --  An inlay hint label part allows for interactive and composite labels of
   --  inlay hints.
   --
   --  @since 3.17.0

   type InlayHintKind_Optional (Is_Set : Boolean := False) is record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : InlayHintKind;
      end case;
   end record;

   package InlayHintLabelPart_Vectors is new Ada.Containers.Vectors
     (Positive, InlayHintLabelPart, "=");

   type InlayHintLabelPart_Vector is
   new InlayHintLabelPart_Vectors.Vector with null record;

   type Virtual_String_Or_InlayHintLabelPart_Vector
     (Is_Virtual_String : Boolean := True) is
   record
      case Is_Virtual_String is
         when True =>
            Virtual_String : LSP.Structures.Virtual_String;
         when False =>
            InlayHintLabelPart_Vector : LSP.Structures
              .InlayHintLabelPart_Vector;
      end case;
   end record;

   type InlayHint is record
      position : LSP.Structures.Position;
      --  The position of this hint.

      label : LSP.Structures.Virtual_String_Or_InlayHintLabelPart_Vector;
      --  The label of this hint. A human readable string or an array of
      --  InlayHintLabelPart label parts.
      --
      --  *Note* that neither the string nor the label part can be empty.

      kind : InlayHintKind_Optional;
      --  The kind of this hint. Can be omitted in which case the client should
      --  fall back to a reasonable default.

      textEdits : LSP.Structures.TextEdit_Vector;
      --  Optional text edits that are performed when accepting this inlay
      --  hint.
      --
      --  *Note* that edits are expected to change the document so that the
      --  inlay hint (or its nearest variant) is now part of the document and
      --  the inlay hint itself is now obsolete.

      tooltip : Virtual_String_Or_MarkupContent_Optional;
      --  The tooltip text when you hover over this item.

      paddingLeft : Boolean_Optional;
      --  Render padding before the hint.
      --
      --  Note: Padding should use the editor's background color, not the
      --  background color of the hint itself. That means padding can be
      --  used to visually align/separate an inlay hint.

      paddingRight : Boolean_Optional;
      --  Render padding after the hint.
      --
      --  Note: Padding should use the editor's background color, not the
      --  background color of the hint itself. That means padding can be
      --  used to visually align/separate an inlay hint.

      data : LSPAny_Optional;
      --  A data entry field that is preserved on an inlay hint between a
      --  `textDocument/inlayHint` and a `inlayHint/resolve` request.

   end record;
   --  Inlay hint information.
   --
   --  @since 3.17.0

   type InlayHintParams is new WorkDoneProgressParams with record
      workDoneToken : ProgressToken_Optional;
      --  An optional token that a server can use to report work done progress.

      textDocument : LSP.Structures.TextDocumentIdentifier;
      --  The text document.

      a_range : LSP.Structures.A_Range;
      --  The document range for which inlay hints should be computed.

   end record;
   --  A parameter literal used in inlay hint requests.
   --
   --  @since 3.17.0

   overriding function workDoneToken
     (Self : InlayHintParams) return LSP.Structures.ProgressToken_Optional is
     (Self.workDoneToken);

   type InlineValueContext is record
      frameId : Standard.Integer;
      --  The stack frame (as a DAP Id) where the execution has stopped.

      stoppedLocation : LSP.Structures.A_Range;
      --  The document range where execution has stopped. Typically the end
      --  position of the range denotes the line where the inline values are
      --  shown.

   end record;
   --  @since 3.17.0

   type InlineValueEvaluatableExpression is record
      a_range : LSP.Structures.A_Range;
      --  The document range for which the inline value applies. The range
      --  is used to extract the evaluatable expression from the underlying
      --  document.

      expression : Virtual_String_Optional;
      --  If specified the expression overrides the extracted expression.

   end record;
   --  Provide an inline value through an expression evaluation. If only a
   --  range is specified, the expression will be extracted from the underlying
   --  document. An optional expression can be used to override the extracted
   --  expression.
   --
   --  @since 3.17.0

   type InlineValueParams is new WorkDoneProgressParams with record
      workDoneToken : ProgressToken_Optional;
      --  An optional token that a server can use to report work done progress.

      textDocument : LSP.Structures.TextDocumentIdentifier;
      --  The text document.

      a_range : LSP.Structures.A_Range;
      --  The document range for which inline values should be computed.

      context : LSP.Structures.InlineValueContext;
      --  Additional information about the context in which inline values were
      --  requested.

   end record;
   --  A parameter literal used in inline value requests.
   --
   --  @since 3.17.0

   overriding function workDoneToken
     (Self : InlineValueParams) return LSP.Structures.ProgressToken_Optional is
     (Self.workDoneToken);

   type InlineValueText is record
      a_range : LSP.Structures.A_Range;
      --  The document range for which the inline value applies.

      text : LSP.Structures.Virtual_String;
      --  The text of the inline value.

   end record;
   --  Provide inline value as text.
   --
   --  @since 3.17.0

   type InlineValueVariableLookup is record
      a_range : LSP.Structures.A_Range;
      --  The document range for which the inline value applies. The range is
      --  used to extract the variable name from the underlying document.

      variableName : Virtual_String_Optional;
      --  If specified the name of the variable to look up.

      caseSensitiveLookup : Standard.Boolean;
      --  How to perform the lookup.

   end record;
   --  Provide inline value through a variable lookup. If only a range is
   --  specified, the variable name will be extracted from the underlying
   --  document. An optional variable name can be used to override the
   --  extracted name.
   --
   --  @since 3.17.0

   type LinkedEditingRangeParams is
   new TextDocumentPositionParams and WorkDoneProgressParams with record
      workDoneToken : ProgressToken_Optional;
      --  An optional token that a server can use to report work done progress.

   end record;

   overriding function workDoneToken
     (Self : LinkedEditingRangeParams)
      return LSP.Structures.ProgressToken_Optional is
     (Self.workDoneToken);

   type LinkedEditingRanges is record
      ranges : LSP.Structures.Range_Vector;
      --  A list of ranges that can be edited together. The ranges must have
      --  identical length and contain identical text content. The ranges
      --  cannot overlap.

      wordPattern : Virtual_String_Optional;
      --  An optional word pattern (regular expression) that describes valid
      --  contents for the given ranges. If no pattern is provided, the client
      --  configuration's word pattern will be used.

   end record;
   --  The result of a linked editing range request.
   --
   --  @since 3.16.0

   type LocationLink is record
      originSelectionRange : A_Range_Optional;
      --  Span of the origin of this link.
      --
      --  Used as the underlined span for mouse interaction. Defaults to the
      --  word range at the definition position.

      targetUri : LSP.Structures.DocumentUri;
      --  The target resource identifier of this link.

      targetRange : LSP.Structures.A_Range;
      --  The full target range of this link. If the target for example is
      --  a symbol then target range is the range enclosing this symbol not
      --  including leading/trailing whitespace but everything else like
      --  comments. This information is typically used to highlight the
      --  range in the editor.

      targetSelectionRange : LSP.Structures.A_Range;
      --  The range that should be selected and revealed when this link is
      --  being followed, e.g the name of a function. Must be contained by
      --  the `targetRange`. See also `DocumentSymbol#range`

   end record;
   --  Represents the connection of two locations. Provides additional metadata
   --  over normal [locations](#Location), including an origin range.

   type LogMessageParams is record
      a_type : LSP.Enumerations.MessageType;
      --  The message type. See {@link MessageType}

      message : LSP.Structures.Virtual_String;
      --  The actual message.

   end record;
   --  The log message parameters.

   type LogTraceParams is record
      message : LSP.Structures.Virtual_String;

      verbose : Virtual_String_Optional;

   end record;

   type MessageActionItem is record
      title : LSP.Structures.Virtual_String;
      --  A short title like 'Retry', 'Open Log' etc.

   end record;

   type MonikerKind_Optional (Is_Set : Boolean := False) is record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : MonikerKind;
      end case;
   end record;

   type Moniker is record
      scheme : LSP.Structures.Virtual_String;
      --  The scheme of the moniker. For example tsc or .Net

      identifier : LSP.Structures.Virtual_String;
      --  The identifier of the moniker. The value is opaque in LSIF however
      --  schema owners are allowed to define the structure if they want.

      unique : LSP.Enumerations.UniquenessLevel;
      --  The scope in which the moniker is unique

      kind : MonikerKind_Optional;
      --  The moniker kind if known.

   end record;
   --  Moniker definition to match LSIF 0.5 moniker definition.
   --
   --  @since 3.16.0

   type MonikerParams is
   new TextDocumentPositionParams and WorkDoneProgressParams and
     PartialResultParams with record
      workDoneToken : ProgressToken_Optional;
      --  An optional token that a server can use to report work done progress.

      partialResultToken : ProgressToken_Optional;
      --  An optional token that a server can use to report partial results
      --  (e.g. streaming) to the client.

   end record;

   overriding function workDoneToken
     (Self : MonikerParams) return LSP.Structures.ProgressToken_Optional is
     (Self.workDoneToken);

   overriding function partialResultToken
     (Self : MonikerParams) return LSP.Structures.ProgressToken_Optional is
     (Self.partialResultToken);

   type Natural_Tuple is array (1 .. 2) of Natural;

   type String_Or_Natural_Tuple (Is_Virtual_String : Boolean := True) is record
      case Is_Virtual_String is
         when True =>
            Virtual_String : LSP.Structures.Virtual_String;
         when False =>
            Natural_Tuple : LSP.Structures.Natural_Tuple;
      end case;
   end record;

   type ParameterInformation is record
      label : LSP.Structures.String_Or_Natural_Tuple;
      --  The label of this parameter information.
      --
      --  Either a string or an inclusive start and exclusive
      --  end offsets within its containing signature label. (see
      --  SignatureInformation.label). The offsets are based on a UTF-16
      --  string representation as `Position` and `Range` does.
      --
      --  *Note*: a label of type string should be a substring of its
      --  containing signature label. Its intended use case is to highlight
      --  the parameter label part in the `SignatureInformation.label`.

      documentation : Virtual_String_Or_MarkupContent_Optional;
      --  The human-readable doc-comment of this parameter. Will be shown in
      --  the UI but can be omitted.

   end record;
   --  Represents a parameter of a callable-signature. A parameter can have a
   --  label and a doc-comment.

   type PrepareRenameParams is
   new TextDocumentPositionParams and WorkDoneProgressParams with record
      workDoneToken : ProgressToken_Optional;
      --  An optional token that a server can use to report work done progress.

   end record;

   overriding function workDoneToken
     (Self : PrepareRenameParams)
      return LSP.Structures.ProgressToken_Optional is
     (Self.workDoneToken);

   type PreviousResultId is record
      uri : LSP.Structures.DocumentUri;
      --  The URI for which the client knows a result id.

      value : LSP.Structures.Virtual_String;
      --  The value of the previous result id.

   end record;
   --  A previous result id in a workspace pull request.
   --
   --  @since 3.17.0

   type ProgressParams is record
      token : LSP.Structures.ProgressToken;
      --  The progress token provided by the client or server.

      value : LSP.Structures.LSPAny;
      --  The progress data.

   end record;

   type PublishDiagnosticsParams is record
      uri : LSP.Structures.DocumentUri;
      --  The URI for which diagnostic information is reported.

      version : Integer_Optional;
      --  Optional the version number of the document the diagnostics are
      --  published for.
      --
      --  @since 3.15.0

      diagnostics : LSP.Structures.Diagnostic_Vector;
      --  An array of diagnostic information items.

   end record;
   --  The publish diagnostic notification's parameters.

   type ReferenceContext is record
      includeDeclaration : Standard.Boolean;
      --  Include the declaration of the current symbol.

   end record;
   --  Value-object that contains additional information when requesting
   --  references.

   type ReferenceParams is
   new TextDocumentPositionParams and WorkDoneProgressParams and
     PartialResultParams with record
      workDoneToken : ProgressToken_Optional;
      --  An optional token that a server can use to report work done progress.

      partialResultToken : ProgressToken_Optional;
      --  An optional token that a server can use to report partial results
      --  (e.g. streaming) to the client.

      context : LSP.Structures.ReferenceContext;

   end record;
   --  Parameters for a [ReferencesRequest](#ReferencesRequest).

   overriding function workDoneToken
     (Self : ReferenceParams) return LSP.Structures.ProgressToken_Optional is
     (Self.workDoneToken);

   overriding function partialResultToken
     (Self : ReferenceParams) return LSP.Structures.ProgressToken_Optional is
     (Self.partialResultToken);

   type ReferenceRegistrationOptions is new ReferenceOptions with record
      Parent : LSP.Structures.TextDocumentRegistrationOptions;
   end record;
   --  Registration options for a [ReferencesRequest](#ReferencesRequest).

   type Registration is record
      id : LSP.Structures.Virtual_String;
      --  The id used to register the request. The id can be used to deregister
      --  the request again.

      method : LSP.Structures.Virtual_String;
      --  The method / capability to register for.

      registerOptions : LSPAny_Optional;
      --  Options necessary for the registration.

   end record;
   --  General parameters to to register for an notification or to register a
   --  provider.

   package Registration_Vectors is new Ada.Containers.Vectors
     (Positive, Registration, "=");

   type Registration_Vector is
   new Registration_Vectors.Vector with null record;

   type RegistrationParams is record
      registrations : LSP.Structures.Registration_Vector;

   end record;

   type RelatedFullDocumentDiagnosticReport is
   new FullDocumentDiagnosticReport with record
      relatedDocuments : LSP.Structures
        .relatedDocuments_OfDocumentDiagnosticReportPartialResult;
      --  Diagnostics of related documents. This information is useful in
      --  programming languages where code in a file A can generate diagnostics
      --  in a file B which A depends on. An example of such a language is
      --  C/C++ where marco definitions in a file a.cpp and result in errors
      --  in a header file b.hpp.
      --
      --  @since 3.17.0

   end record;
   --  A full diagnostic report with a set of related documents.
   --
   --  @since 3.17.0

   type RelatedUnchangedDocumentDiagnosticReport is
   new UnchangedDocumentDiagnosticReport with record
      relatedDocuments : LSP.Structures
        .relatedDocuments_OfDocumentDiagnosticReportPartialResult;
      --  Diagnostics of related documents. This information is useful in
      --  programming languages where code in a file A can generate diagnostics
      --  in a file B which A depends on. An example of such a language is
      --  C/C++ where marco definitions in a file a.cpp and result in errors
      --  in a header file b.hpp.
      --
      --  @since 3.17.0

   end record;
   --  An unchanged diagnostic report with a set of related documents.
   --
   --  @since 3.17.0

   package FileRename_Vectors is new Ada.Containers.Vectors
     (Positive, FileRename, "=");

   type FileRename_Vector is new FileRename_Vectors.Vector with null record;

   type RenameFilesParams is record
      files : LSP.Structures.FileRename_Vector;
      --  An array of all files/folders renamed in this operation. When a
      --  folder is renamed, only the folder will be included, and not its
      --  children.

   end record;
   --  The parameters sent in notifications/requests for user-initiated renames
   --  of files.
   --
   --  @since 3.16.0

   type RenameParams is new WorkDoneProgressParams with record
      workDoneToken : ProgressToken_Optional;
      --  An optional token that a server can use to report work done progress.

      textDocument : LSP.Structures.TextDocumentIdentifier;
      --  The document to rename.

      position : LSP.Structures.Position;
      --  The position at which this request was sent.

      newName : LSP.Structures.Virtual_String;
      --  The new name of the symbol. If the given name is not valid the
      --  request must return a [ResponseError](#ResponseError) with an
      --  appropriate message set.

   end record;
   --  The parameters of a [RenameRequest](#RenameRequest).

   overriding function workDoneToken
     (Self : RenameParams) return LSP.Structures.ProgressToken_Optional is
     (Self.workDoneToken);

   type RenameRegistrationOptions is new RenameOptions with record
      Parent : LSP.Structures.TextDocumentRegistrationOptions;
   end record;
   --  Registration options for a [RenameRequest](#RenameRequest).

   type SelectionRange is record
      a_range : LSP.Structures.A_Range;
      --  The [range](#Range) of this selection range.

      parent : SelectionRange_Optional;
      --  The parent selection range containing this range. Therefore
      --  `parent.range` must contain `this.range`.

   end record;
   --  A selection range represents a part of a selection hierarchy. A
   --  selection range may have a parent selection range that contains it.

   package Position_Vectors is new Ada.Containers.Vectors
     (Positive, Position, "=");

   type Position_Vector is new Position_Vectors.Vector with null record;

   type SelectionRangeParams is
   new WorkDoneProgressParams and PartialResultParams with record
      workDoneToken : ProgressToken_Optional;
      --  An optional token that a server can use to report work done progress.

      partialResultToken : ProgressToken_Optional;
      --  An optional token that a server can use to report partial results
      --  (e.g. streaming) to the client.

      textDocument : LSP.Structures.TextDocumentIdentifier;
      --  The text document.

      positions : LSP.Structures.Position_Vector;
      --  The positions inside the text document.

   end record;
   --  A parameter literal used in selection range requests.

   overriding function workDoneToken
     (Self : SelectionRangeParams)
      return LSP.Structures.ProgressToken_Optional is
     (Self.workDoneToken);

   overriding function partialResultToken
     (Self : SelectionRangeParams)
      return LSP.Structures.ProgressToken_Optional is
     (Self.partialResultToken);

   package Natural_Vectors is new Ada.Containers.Vectors
     (Positive, Natural, "=");

   type Natural_Vector is new Natural_Vectors.Vector with null record;

   type SemanticTokens is record
      resultId : Virtual_String_Optional;
      --  An optional result id. If provided and clients support delta updating
      --  the client will include the result id in the next semantic token
      --  request. A server can then instead of computing all semantic tokens
      --  again simply send a delta.

      data : LSP.Structures.Natural_Vector;
      --  The actual tokens.

   end record;
   --  @since 3.16.0

   type SemanticTokensEdit is record
      start : Natural;
      --  The start offset of the edit.

      deleteCount : Natural;
      --  The count of elements to remove.

      data : LSP.Structures.Natural_Vector;
      --  The elements to insert.

   end record;
   --  @since 3.16.0

   package SemanticTokensEdit_Vectors is new Ada.Containers.Vectors
     (Positive, SemanticTokensEdit, "=");

   type SemanticTokensEdit_Vector is
   new SemanticTokensEdit_Vectors.Vector with null record;

   type SemanticTokensDelta is record
      resultId : Virtual_String_Optional;

      edits : LSP.Structures.SemanticTokensEdit_Vector;
      --  The semantic token edits to transform a previous result into a new
      --  result.

   end record;
   --  @since 3.16.0

   type SemanticTokensDeltaParams is
   new WorkDoneProgressParams and PartialResultParams with record
      workDoneToken : ProgressToken_Optional;
      --  An optional token that a server can use to report work done progress.

      partialResultToken : ProgressToken_Optional;
      --  An optional token that a server can use to report partial results
      --  (e.g. streaming) to the client.

      textDocument : LSP.Structures.TextDocumentIdentifier;
      --  The text document.

      previousResultId : LSP.Structures.Virtual_String;
      --  The result id of a previous response. The result Id can either point
      --  to a full response or a delta response depending on what was received
      --  last.

   end record;
   --  @since 3.16.0

   overriding function workDoneToken
     (Self : SemanticTokensDeltaParams)
      return LSP.Structures.ProgressToken_Optional is
     (Self.workDoneToken);

   overriding function partialResultToken
     (Self : SemanticTokensDeltaParams)
      return LSP.Structures.ProgressToken_Optional is
     (Self.partialResultToken);

   type SemanticTokensDeltaPartialResult is record
      edits : LSP.Structures.SemanticTokensEdit_Vector;

   end record;
   --  @since 3.16.0

   type SemanticTokensParams is
   new WorkDoneProgressParams and PartialResultParams with record
      workDoneToken : ProgressToken_Optional;
      --  An optional token that a server can use to report work done progress.

      partialResultToken : ProgressToken_Optional;
      --  An optional token that a server can use to report partial results
      --  (e.g. streaming) to the client.

      textDocument : LSP.Structures.TextDocumentIdentifier;
      --  The text document.

   end record;
   --  @since 3.16.0

   overriding function workDoneToken
     (Self : SemanticTokensParams)
      return LSP.Structures.ProgressToken_Optional is
     (Self.workDoneToken);

   overriding function partialResultToken
     (Self : SemanticTokensParams)
      return LSP.Structures.ProgressToken_Optional is
     (Self.partialResultToken);

   type SemanticTokensPartialResult is record
      data : LSP.Structures.Natural_Vector;

   end record;
   --  @since 3.16.0

   type SemanticTokensRangeParams is
   new WorkDoneProgressParams and PartialResultParams with record
      workDoneToken : ProgressToken_Optional;
      --  An optional token that a server can use to report work done progress.

      partialResultToken : ProgressToken_Optional;
      --  An optional token that a server can use to report partial results
      --  (e.g. streaming) to the client.

      textDocument : LSP.Structures.TextDocumentIdentifier;
      --  The text document.

      a_range : LSP.Structures.A_Range;
      --  The range the semantic tokens are requested for.

   end record;
   --  @since 3.16.0

   overriding function workDoneToken
     (Self : SemanticTokensRangeParams)
      return LSP.Structures.ProgressToken_Optional is
     (Self.workDoneToken);

   overriding function partialResultToken
     (Self : SemanticTokensRangeParams)
      return LSP.Structures.ProgressToken_Optional is
     (Self.partialResultToken);

   type SetTraceParams is record
      value : LSP.Enumerations.TraceValues;

   end record;

   type ShowDocumentParams is record
      uri : LSP.Structures.URI;
      --  The document uri to show.

      external : Boolean_Optional;
      --  Indicates to show the resource in an external program. To show for
      --  example `https://code.visualstudio.com/` in the default WEB browser
      --  set `external` to `true`.

      takeFocus : Boolean_Optional;
      --  An optional property to indicate whether the editor showing the
      --  document should take focus or not. Clients might ignore this
      --  property if an external program is started.

      selection : A_Range_Optional;
      --  An optional selection range if the document is a text document.
      --  Clients might ignore the property if an external program is started
      --  or the file is not a text file.

   end record;
   --  Params to show a document.
   --
   --  @since 3.16.0

   type ShowDocumentResult is record
      success : Standard.Boolean;
      --  A boolean indicating if the show was successful.

   end record;
   --  The result of a showDocument request.
   --
   --  @since 3.16.0

   type ShowMessageParams is record
      a_type : LSP.Enumerations.MessageType;
      --  The message type. See {@link MessageType}

      message : LSP.Structures.Virtual_String;
      --  The actual message.

   end record;
   --  The parameters of a notification message.

   package MessageActionItem_Vectors is new Ada.Containers.Vectors
     (Positive, MessageActionItem, "=");

   type MessageActionItem_Vector is
   new MessageActionItem_Vectors.Vector with null record;

   type ShowMessageRequestParams is record
      a_type : LSP.Enumerations.MessageType;
      --  The message type. See {@link MessageType}

      message : LSP.Structures.Virtual_String;
      --  The actual message.

      actions : LSP.Structures.MessageActionItem_Vector;
      --  The message action items to present.

   end record;

   package ParameterInformation_Vectors is new Ada.Containers.Vectors
     (Positive, ParameterInformation, "=");

   type ParameterInformation_Vector is
   new ParameterInformation_Vectors.Vector with null record;

   type SignatureInformation is record
      label : LSP.Structures.Virtual_String;
      --  The label of this signature. Will be shown in the UI.

      documentation : Virtual_String_Or_MarkupContent_Optional;
      --  The human-readable doc-comment of this signature. Will be shown in
      --  the UI but can be omitted.

      parameters : LSP.Structures.ParameterInformation_Vector;
      --  The parameters of this signature.

      activeParameter : Natural_Optional;
      --  The index of the active parameter.
      --
      --  If provided, this is used in place of
      --  `SignatureHelp.activeParameter`.
      --
      --  @since 3.16.0

   end record;
   --  Represents the signature of something callable. A signature can have a
   --  label, like a function-name, a doc-comment, and a set of parameters.

   package SignatureInformation_Vectors is new Ada.Containers.Vectors
     (Positive, SignatureInformation, "=");

   type SignatureInformation_Vector is
   new SignatureInformation_Vectors.Vector with null record;

   type SignatureHelp is record
      signatures : LSP.Structures.SignatureInformation_Vector;
      --  One or more signatures.

      activeSignature : Natural_Optional;
      --  The active signature. If omitted or the value lies outside the range
      --  of `signatures` the value defaults to zero or is ignored if the
      --  `SignatureHelp` has no signatures.
      --
      --  Whenever possible implementors should make an active decision about
      --  the active signature and shouldn't rely on a default value.
      --
      --  In future version of the protocol this property might become
      --  mandatory to better express this.

      activeParameter : Natural_Optional;
      --  The active parameter of the active signature. If omitted or the value
      --  lies outside the range of `signatures[activeSignature].parameters`
      --  defaults to 0 if the active signature has parameters. If the active
      --  signature has no parameters it is ignored. In future version of the
      --  protocol this property might become mandatory to better express the
      --  active parameter if the active signature does have any.

   end record;
   --  Signature help represents the signature of something callable. There can
   --  be multiple signature but only one active and only one active parameter.

   type SignatureHelp_Optional (Is_Set : Boolean := False) is record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : SignatureHelp;
      end case;
   end record;

   type SignatureHelpContext is record
      triggerKind : LSP.Enumerations.SignatureHelpTriggerKind;
      --  Action that caused signature help to be triggered.

      triggerCharacter : Virtual_String_Optional;
      --  Character that caused signature help to be triggered.
      --
      --  This is undefined when `triggerKind !==
      --  SignatureHelpTriggerKind.TriggerCharacter`

      isRetrigger : Standard.Boolean;
      --  `true` if signature help was already showing when it was triggered.
      --
      --  Retriggers occurs when the signature help is already active and can
      --  be caused by actions such as typing a trigger character, a cursor
      --  move, or document content changes.

      activeSignatureHelp : SignatureHelp_Optional;
      --  The currently active `SignatureHelp`.
      --
      --  The `activeSignatureHelp` has its `SignatureHelp.activeSignature`
      --  field updated based on the user navigating through available
      --  signatures.

   end record;
   --  Additional information about the context in which a signature help
   --  request was triggered.
   --
   --  @since 3.15.0

   type SignatureHelpContext_Optional (Is_Set : Boolean := False) is record
      case Is_Set is
         when False =>
            null;
         when True =>
            Value : SignatureHelpContext;
      end case;
   end record;

   type SignatureHelpParams is
   new TextDocumentPositionParams and WorkDoneProgressParams with record
      workDoneToken : ProgressToken_Optional;
      --  An optional token that a server can use to report work done progress.

      context : SignatureHelpContext_Optional;
      --  The signature help context. This is only available if the
      --  client specifies to send this using the client capability
      --  `textDocument.signatureHelp.contextSupport === true`
      --
      --  @since 3.15.0

   end record;
   --  Parameters for a [SignatureHelpRequest](#SignatureHelpRequest).

   overriding function workDoneToken
     (Self : SignatureHelpParams)
      return LSP.Structures.ProgressToken_Optional is
     (Self.workDoneToken);

   type SignatureHelpRegistrationOptions is
   new SignatureHelpOptions with record
      Parent : LSP.Structures.TextDocumentRegistrationOptions;
   end record;
   --  Registration options for a
   --  [SignatureHelpRequest](#SignatureHelpRequest).

   type SymbolInformation is new BaseSymbolInformation with record
      deprecated : Boolean_Optional;
      --  Indicates if this symbol is deprecated.
      --
      --  @deprecated Use tags instead

      location : LSP.Structures.Location;
      --  The location of this symbol. The location's range is used by a tool
      --  to reveal the location in the editor. If the symbol is selected
      --  in the tool the range's start information is used to position the
      --  cursor. So the range usually spans more than the actual symbol's
      --  name and does normally include things like visibility modifiers.
      --
      --  The range doesn't have to denote a node range in the sense of an
      --  abstract syntax tree. It can therefore not be used to re-construct
      --  a hierarchy of the symbols.

   end record;
   --  Represents information about programming constructs like variables,
   --  classes, interfaces etc.

   type TextDocumentChangeRegistrationOptions is
   new TextDocumentRegistrationOptions with record
      syncKind : LSP.Enumerations.TextDocumentSyncKind;
      --  How documents are synced to the server.

   end record;
   --  Describe options to be used when registered for text document change
   --  events.

   type TextDocumentSaveRegistrationOptions is new SaveOptions with record
      Parent : LSP.Structures.TextDocumentRegistrationOptions;
   end record;
   --  Save registration options.

   type TypeDefinitionParams is
   new TextDocumentPositionParams and WorkDoneProgressParams and
     PartialResultParams with record
      workDoneToken : ProgressToken_Optional;
      --  An optional token that a server can use to report work done progress.

      partialResultToken : ProgressToken_Optional;
      --  An optional token that a server can use to report partial results
      --  (e.g. streaming) to the client.

   end record;

   overriding function workDoneToken
     (Self : TypeDefinitionParams)
      return LSP.Structures.ProgressToken_Optional is
     (Self.workDoneToken);

   overriding function partialResultToken
     (Self : TypeDefinitionParams)
      return LSP.Structures.ProgressToken_Optional is
     (Self.partialResultToken);

   type TypeHierarchyItem is record
      name : LSP.Structures.Virtual_String;
      --  The name of this item.

      kind : LSP.Enumerations.SymbolKind;
      --  The kind of this item.

      tags : LSP.Structures.SymbolTag_Set;
      --  Tags for this item.

      detail : Virtual_String_Optional;
      --  More detail for this item, e.g. the signature of a function.

      uri : LSP.Structures.DocumentUri;
      --  The resource identifier of this item.

      a_range : LSP.Structures.A_Range;
      --  The range enclosing this symbol not including leading/trailing
      --  whitespace but everything else, e.g. comments and code.

      selectionRange : LSP.Structures.A_Range;
      --  The range that should be selected and revealed when this symbol is
      --  being picked, e.g. the name of a function. Must be contained by the
      --  [`range`](#TypeHierarchyItem.range).

      data : LSPAny_Optional;
      --  A data entry field that is preserved between a type hierarchy
      --  prepare and supertypes or subtypes requests. It could also be used
      --  to identify the type hierarchy in the server, helping improve the
      --  performance on resolving supertypes and subtypes.

   end record;
   --  @since 3.17.0

   type TypeHierarchyPrepareParams is
   new TextDocumentPositionParams and WorkDoneProgressParams with record
      workDoneToken : ProgressToken_Optional;
      --  An optional token that a server can use to report work done progress.

   end record;
   --  The parameter of a `textDocument/prepareTypeHierarchy` request.
   --
   --  @since 3.17.0

   overriding function workDoneToken
     (Self : TypeHierarchyPrepareParams)
      return LSP.Structures.ProgressToken_Optional is
     (Self.workDoneToken);

   type TypeHierarchySubtypesParams is
   new WorkDoneProgressParams and PartialResultParams with record
      workDoneToken : ProgressToken_Optional;
      --  An optional token that a server can use to report work done progress.

      partialResultToken : ProgressToken_Optional;
      --  An optional token that a server can use to report partial results
      --  (e.g. streaming) to the client.

      item : LSP.Structures.TypeHierarchyItem;

   end record;
   --  The parameter of a `typeHierarchy/subtypes` request.
   --
   --  @since 3.17.0

   overriding function workDoneToken
     (Self : TypeHierarchySubtypesParams)
      return LSP.Structures.ProgressToken_Optional is
     (Self.workDoneToken);

   overriding function partialResultToken
     (Self : TypeHierarchySubtypesParams)
      return LSP.Structures.ProgressToken_Optional is
     (Self.partialResultToken);

   type TypeHierarchySupertypesParams is
   new WorkDoneProgressParams and PartialResultParams with record
      workDoneToken : ProgressToken_Optional;
      --  An optional token that a server can use to report work done progress.

      partialResultToken : ProgressToken_Optional;
      --  An optional token that a server can use to report partial results
      --  (e.g. streaming) to the client.

      item : LSP.Structures.TypeHierarchyItem;

   end record;
   --  The parameter of a `typeHierarchy/supertypes` request.
   --
   --  @since 3.17.0

   overriding function workDoneToken
     (Self : TypeHierarchySupertypesParams)
      return LSP.Structures.ProgressToken_Optional is
     (Self.workDoneToken);

   overriding function partialResultToken
     (Self : TypeHierarchySupertypesParams)
      return LSP.Structures.ProgressToken_Optional is
     (Self.partialResultToken);

   type Unregistration is record
      id : LSP.Structures.Virtual_String;
      --  The id used to unregister the request or notification. Usually an id
      --  provided during the register request.

      method : LSP.Structures.Virtual_String;
      --  The method to unregister for.

   end record;
   --  General parameters to unregister a request or notification.

   package Unregistration_Vectors is new Ada.Containers.Vectors
     (Positive, Unregistration, "=");

   type Unregistration_Vector is
   new Unregistration_Vectors.Vector with null record;

   type UnregistrationParams is record
      unregisterations : LSP.Structures.Unregistration_Vector;

   end record;

   type WillSaveTextDocumentParams is record
      textDocument : LSP.Structures.TextDocumentIdentifier;
      --  The document that will be saved.

      reason : LSP.Enumerations.TextDocumentSaveReason;
      --  The 'TextDocumentSaveReason'.

   end record;
   --  The parameters sent in a will save text document notification.

   type WorkDoneProgressBegin is record
      title : LSP.Structures.Virtual_String;
      --  Mandatory title of the progress operation. Used to briefly inform
      --  about the kind of operation being performed.
      --
      --  Examples: "Indexing" or "Linking dependencies".

      cancellable : Boolean_Optional;
      --  Controls if a cancel button should show to allow the user to cancel
      --  the long running operation. Clients that don't support cancellation
      --  are allowed to ignore the setting.

      message : Virtual_String_Optional;
      --  Optional, more detailed associated progress message. Contains
      --  complementary information to the `title`.
      --
      --  Examples: "3/25 files", "project/src/module2",
      --  "node_modules/some_dep". If unset, the previous progress message
      --  (if any) is still valid.

      percentage : Natural_Optional;
      --  Optional progress percentage to display (value 100 is considered
      --  100%). If not provided infinite progress is assumed and clients are
      --  allowed to ignore the `percentage` value in subsequent in report
      --  notifications.
      --
      --  The value should be steadily rising. Clients are free to ignore
      --  values that are not following this rule. The value range is [0, 100].

   end record;

   type WorkDoneProgressCancelParams is record
      token : LSP.Structures.ProgressToken;
      --  The token to be used to report progress.

   end record;

   type WorkDoneProgressCreateParams is record
      token : LSP.Structures.ProgressToken;
      --  The token to be used to report progress.

   end record;

   type WorkDoneProgressEnd is record
      message : Virtual_String_Optional;
      --  Optional, a final message indicating to for example indicate the
      --  outcome of the operation.

   end record;

   type WorkDoneProgressReport is record
      cancellable : Boolean_Optional;
      --  Controls enablement state of a cancel button.
      --
      --  Clients that don't support cancellation or don't support controlling
      --  the button's enablement state are allowed to ignore the property.

      message : Virtual_String_Optional;
      --  Optional, more detailed associated progress message. Contains
      --  complementary information to the `title`.
      --
      --  Examples: "3/25 files", "project/src/module2",
      --  "node_modules/some_dep". If unset, the previous progress message
      --  (if any) is still valid.

      percentage : Natural_Optional;
      --  Optional progress percentage to display (value 100 is considered
      --  100%). If not provided infinite progress is assumed and clients are
      --  allowed to ignore the `percentage` value in subsequent in report
      --  notifications.
      --
      --  The value should be steadily rising. Clients are free to ignore
      --  values that are not following this rule. The value range is [0, 100]

   end record;

   package PreviousResultId_Vectors is new Ada.Containers.Vectors
     (Positive, PreviousResultId, "=");

   type PreviousResultId_Vector is
   new PreviousResultId_Vectors.Vector with null record;

   type WorkspaceDiagnosticParams is
   new WorkDoneProgressParams and PartialResultParams with record
      workDoneToken : ProgressToken_Optional;
      --  An optional token that a server can use to report work done progress.

      partialResultToken : ProgressToken_Optional;
      --  An optional token that a server can use to report partial results
      --  (e.g. streaming) to the client.

      identifier : Virtual_String_Optional;
      --  The additional identifier provided during registration.

      previousResultIds : LSP.Structures.PreviousResultId_Vector;
      --  The currently known diagnostic reports with their previous result
      --  ids.

   end record;
   --  Parameters of the workspace diagnostic request.
   --
   --  @since 3.17.0

   overriding function workDoneToken
     (Self : WorkspaceDiagnosticParams)
      return LSP.Structures.ProgressToken_Optional is
     (Self.workDoneToken);

   overriding function partialResultToken
     (Self : WorkspaceDiagnosticParams)
      return LSP.Structures.ProgressToken_Optional is
     (Self.partialResultToken);

   type WorkspaceUnchangedDocumentDiagnosticReport is
   new UnchangedDocumentDiagnosticReport with record
      uri : LSP.Structures.DocumentUri;
      --  The URI for which diagnostic information is reported.

      version : LSP.Structures.Integer_Or_Null;
      --  The version number for which the diagnostics are reported. If the
      --  document is not marked as open `null` can be provided.

   end record;
   --  An unchanged document diagnostic report for a workspace diagnostic
   --  result.
   --
   --  @since 3.17.0

   type WorkspaceFullDocumentDiagnosticReport is
   new FullDocumentDiagnosticReport with record
      uri : LSP.Structures.DocumentUri;
      --  The URI for which diagnostic information is reported.

      version : LSP.Structures.Integer_Or_Null;
      --  The version number for which the diagnostics are reported. If the
      --  document is not marked as open `null` can be provided.

   end record;
   --  A full document diagnostic report for a workspace diagnostic result.
   --
   --  @since 3.17.0

   type WorkspaceDocumentDiagnosticReport_Variant is (full, unchanged);

   type WorkspaceDocumentDiagnosticReport
     (Kind : WorkspaceDocumentDiagnosticReport_Variant :=
        WorkspaceDocumentDiagnosticReport_Variant'First)
   is
   record
      case Kind is
         when full =>
            full : LSP.Structures.WorkspaceFullDocumentDiagnosticReport;
         when unchanged =>
            unchanged : LSP.Structures
              .WorkspaceUnchangedDocumentDiagnosticReport;
      end case;
   end record;
   --  A workspace diagnostic document report.
   --
   --  @since 3.17.0

   package WorkspaceDocumentDiagnosticReport_Vectors is new Ada.Containers
     .Vectors
     (Positive, WorkspaceDocumentDiagnosticReport, "=");

   type WorkspaceDocumentDiagnosticReport_Vector is
   new WorkspaceDocumentDiagnosticReport_Vectors.Vector with null record;

   type WorkspaceDiagnosticReport is record
      items : LSP.Structures.WorkspaceDocumentDiagnosticReport_Vector;

   end record;
   --  A workspace diagnostic report.
   --
   --  @since 3.17.0

   type WorkspaceDiagnosticReportPartialResult is record
      items : LSP.Structures.WorkspaceDocumentDiagnosticReport_Vector;

   end record;
   --  A partial result for a workspace diagnostic report.
   --
   --  @since 3.17.0

   type Location_Or_Something (Is_Location : Boolean := True) is record
      case Is_Location is
         when True =>
            Location : LSP.Structures.Location;
         when False =>
            uri : LSP.Structures.DocumentUri;

      end case;
   end record;

   type WorkspaceSymbol is new BaseSymbolInformation with record
      location : LSP.Structures.Location_Or_Something;
      --  The location of the symbol. Whether a server is allowed to return
      --  a location without a range depends on the client capability
      --  `workspace.symbol.resolveSupport`.
      --
      --  See SymbolInformation#location for more details.

      data : LSPAny_Optional;
      --  A data entry field that is preserved on a workspace symbol between a
      --  workspace symbol request and a workspace symbol resolve request.

   end record;
   --  A special workspace symbol that supports locations without a range.
   --
   --  See also SymbolInformation.
   --
   --  @since 3.17.0

   type WorkspaceSymbolParams is
   new WorkDoneProgressParams and PartialResultParams with record
      workDoneToken : ProgressToken_Optional;
      --  An optional token that a server can use to report work done progress.

      partialResultToken : ProgressToken_Optional;
      --  An optional token that a server can use to report partial results
      --  (e.g. streaming) to the client.

      query : LSP.Structures.Virtual_String;
      --  A query string to filter symbols by. Clients may send an empty string
      --  here to request all symbols.

      case_sensitive : Boolean_Optional;
      --  To take letters' case into account.

      whole_word : Boolean_Optional;
      --  To match the whole word instead of a part of it.

      negate : Boolean_Optional;
      --  To invert matching.

      kind : AlsSearchKind_Optional;

   end record;
   --  The parameters of a [WorkspaceSymbolRequest](#WorkspaceSymbolRequest).

   overriding function workDoneToken
     (Self : WorkspaceSymbolParams)
      return LSP.Structures.ProgressToken_Optional is
     (Self.workDoneToken);

   overriding function partialResultToken
     (Self : WorkspaceSymbolParams)
      return LSP.Structures.ProgressToken_Optional is
     (Self.partialResultToken);

   type WorkspaceSymbolRegistrationOptions is
   new WorkspaceSymbolOptions with record
      null;
   end record;
   --  Registration options for a
   --  [WorkspaceSymbolRequest](#WorkspaceSymbolRequest).

   package Location_Vectors is new Ada.Containers.Vectors
     (Positive, Location, "=");

   type Location_Vector is new Location_Vectors.Vector with null record;

   subtype Declaration is Location_Vector;
   --  The declaration of a symbol representation as one or many
   --  [locations](#Location).

   subtype DeclarationLink is LocationLink;
   --  Information about where a symbol is declared.
   --
   --  Provides additional metadata over normal [location](#Location)
   --  declarations, including the range of the declaring symbol.
   --
   --  Servers should prefer returning `DeclarationLink` over `Declaration` if
   --  supported by the client.

   subtype Definition is Location_Vector;
   --  The definition of a symbol represented as one or many
   --  [locations](#Location). For most programming languages there is only
   --  one location at which a symbol is defined.
   --
   --  Servers should prefer returning `DefinitionLink` over `Definition` if
   --  supported by the client.

   subtype DefinitionLink is LocationLink;
   --  Information about where a symbol is defined.
   --
   --  Provides additional metadata over normal [location](#Location)
   --  definitions, including the range of the defining symbol

   type DocumentDiagnosticReport_Variant is (full, unchanged);

   type DocumentDiagnosticReport
     (Kind : DocumentDiagnosticReport_Variant :=
        DocumentDiagnosticReport_Variant'First)
   is
   record
      case Kind is
         when full =>
            full : LSP.Structures.RelatedFullDocumentDiagnosticReport;
         when unchanged =>
            unchanged : LSP.Structures
              .RelatedUnchangedDocumentDiagnosticReport;
      end case;
   end record;
   --  The result of a document diagnostic pull request. A report can either
   --  be a full report containing all diagnostics for the requested document
   --  or an unchanged report indicating that nothing has changed in terms of
   --  diagnostics in comparison to the last pull request.
   --
   --  @since 3.17.0

   type InlineValue_Variant is (Variant_1, Variant_2, Variant_3);

   type InlineValue (Kind : InlineValue_Variant := InlineValue_Variant'First)
   is
   record
      case Kind is
         when Variant_1 =>
            Variant_1 : LSP.Structures.InlineValueText;
         when Variant_2 =>
            Variant_2 : LSP.Structures.InlineValueVariableLookup;
         when Variant_3 =>
            Variant_3 : LSP.Structures.InlineValueEvaluatableExpression;
      end case;
   end record;
   --  Inline value information can be provided by different means: - directly
   --  as a text value (class InlineValueText). - as a name to use for a
   --  variable lookup (class InlineValueVariableLookup) - as an evaluatable
   --  expression (class InlineValueEvaluatableExpression) The InlineValue
   --  types combines all inline value types into one type.
   --
   --  @since 3.17.0

   type PrepareRenameResult_2 is record
      a_range : LSP.Structures.A_Range;

      placeholder : LSP.Structures.Virtual_String;

   end record;

   type PrepareRenameResult_3 is record
      defaultBehavior : Standard.Boolean;

   end record;

   type PrepareRenameResult_Variant is (Variant_1, Variant_2, Variant_3);

   type PrepareRenameResult
     (Kind : PrepareRenameResult_Variant := PrepareRenameResult_Variant'First)
   is
   record
      case Kind is
         when Variant_1 =>
            Variant_1 : LSP.Structures.A_Range;
         when Variant_2 =>
            Variant_2 : LSP.Structures.PrepareRenameResult_2;
         when Variant_3 =>
            Variant_3 : LSP.Structures.PrepareRenameResult_3;
      end case;
   end record;

   package CallHierarchyIncomingCall_Vectors is new Ada.Containers.Vectors
     (Positive, CallHierarchyIncomingCall, "=");

   type CallHierarchyIncomingCall_Vector is
   new CallHierarchyIncomingCall_Vectors.Vector with null record;

   subtype CallHierarchyIncomingCall_Vector_Or_Null is
     CallHierarchyIncomingCall_Vector;

   package CallHierarchyOutgoingCall_Vectors is new Ada.Containers.Vectors
     (Positive, CallHierarchyOutgoingCall, "=");

   type CallHierarchyOutgoingCall_Vector is
   new CallHierarchyOutgoingCall_Vectors.Vector with null record;

   subtype CallHierarchyOutgoingCall_Vector_Or_Null is
     CallHierarchyOutgoingCall_Vector;

   type Command_Or_CodeAction (Is_Command : Boolean := True) is record
      case Is_Command is
         when True =>
            Command : LSP.Structures.Command;
         when False =>
            CodeAction : LSP.Structures.CodeAction;
      end case;
   end record;

   package Command_Or_CodeAction_Vectors is new Ada.Containers.Vectors
     (Positive, Command_Or_CodeAction, "=");

   type Command_Or_CodeAction_Vector is
   new Command_Or_CodeAction_Vectors.Vector with null record;

   subtype Command_Or_CodeAction_Vector_Or_Null is
     Command_Or_CodeAction_Vector;

   package CodeLens_Vectors is new Ada.Containers.Vectors
     (Positive, CodeLens, "=");

   type CodeLens_Vector is new CodeLens_Vectors.Vector with null record;

   subtype CodeLens_Vector_Or_Null is CodeLens_Vector;

   package ColorPresentation_Vectors is new Ada.Containers.Vectors
     (Positive, ColorPresentation, "=");

   type ColorPresentation_Vector is
   new ColorPresentation_Vectors.Vector with null record;

   type Completion_Result_Variant is (Variant_1, Variant_2, Variant_3);

   type Completion_Result
     (Kind : Completion_Result_Variant := Completion_Result_Variant'First) is
   record
      case Kind is
         when Variant_1 =>
            Variant_1 : LSP.Structures.CompletionItem_Vector;
         when Variant_2 =>
            Variant_2 : LSP.Structures.CompletionList;
         when Variant_3 =>
            Variant_3 : LSP.Structures.Null_Record;
      end case;
   end record;

   package DeclarationLink_Vectors is new Ada.Containers.Vectors
     (Positive, DeclarationLink, "=");

   type DeclarationLink_Vector is
   new DeclarationLink_Vectors.Vector with null record;

   type Declaration_Result_Variant is (Variant_1, Variant_2, Variant_3);

   type Declaration_Result
     (Kind : Declaration_Result_Variant := Declaration_Result_Variant'First) is
   record
      case Kind is
         when Variant_1 =>
            Variant_1 : LSP.Structures.Declaration;
         when Variant_2 =>
            Variant_2 : LSP.Structures.DeclarationLink_Vector;
         when Variant_3 =>
            Variant_3 : LSP.Structures.Null_Record;
      end case;
   end record;

   type Declaration_Progress_Report_Variant is (Variant_1, Variant_2);

   type Declaration_Progress_Report
     (Kind : Declaration_Progress_Report_Variant :=
        Declaration_Progress_Report_Variant'First)
   is
   record
      case Kind is
         when Variant_1 =>
            Variant_1 : LSP.Structures.Location_Vector;
         when Variant_2 =>
            Variant_2 : LSP.Structures.DeclarationLink_Vector;
      end case;
   end record;

   package DefinitionLink_Vectors is new Ada.Containers.Vectors
     (Positive, DefinitionLink, "=");

   type DefinitionLink_Vector is
   new DefinitionLink_Vectors.Vector with null record;

   type Definition_Result_Variant is (Variant_1, Variant_2, Variant_3);

   type Definition_Result
     (Kind : Definition_Result_Variant := Definition_Result_Variant'First) is
   record
      case Kind is
         when Variant_1 =>
            Variant_1 : LSP.Structures.Definition;
         when Variant_2 =>
            Variant_2 : LSP.Structures.DefinitionLink_Vector;
         when Variant_3 =>
            Variant_3 : LSP.Structures.Null_Record;
      end case;
   end record;

   type Definition_Progress_Report_Variant is (Variant_1, Variant_2);

   type Definition_Progress_Report
     (Kind : Definition_Progress_Report_Variant :=
        Definition_Progress_Report_Variant'First)
   is
   record
      case Kind is
         when Variant_1 =>
            Variant_1 : LSP.Structures.Location_Vector;
         when Variant_2 =>
            Variant_2 : LSP.Structures.DefinitionLink_Vector;
      end case;
   end record;

   package ColorInformation_Vectors is new Ada.Containers.Vectors
     (Positive, ColorInformation, "=");

   type ColorInformation_Vector is
   new ColorInformation_Vectors.Vector with null record;

   package DocumentHighlight_Vectors is new Ada.Containers.Vectors
     (Positive, DocumentHighlight, "=");

   type DocumentHighlight_Vector is
   new DocumentHighlight_Vectors.Vector with null record;

   subtype DocumentHighlight_Vector_Or_Null is DocumentHighlight_Vector;

   package DocumentLink_Vectors is new Ada.Containers.Vectors
     (Positive, DocumentLink, "=");

   type DocumentLink_Vector is
   new DocumentLink_Vectors.Vector with null record;

   subtype DocumentLink_Vector_Or_Null is DocumentLink_Vector;

   package SymbolInformation_Vectors is new Ada.Containers.Vectors
     (Positive, SymbolInformation, "=");

   type SymbolInformation_Vector is
   new SymbolInformation_Vectors.Vector with null record;

   type DocumentSymbol_Result_Variant is (Variant_1, Variant_2, Variant_3);

   type DocumentSymbol_Result
     (Kind : DocumentSymbol_Result_Variant :=
        DocumentSymbol_Result_Variant'First)
   is
   record
      case Kind is
         when Variant_1 =>
            Variant_1 : LSP.Structures.SymbolInformation_Vector;
         when Variant_2 =>
            Variant_2 : LSP.Structures.DocumentSymbol_Vector;
         when Variant_3 =>
            Variant_3 : LSP.Structures.Null_Record;
      end case;
   end record;

   type DocumentSymbol_Progress_Report_Variant is (Variant_1, Variant_2);

   type DocumentSymbol_Progress_Report
     (Kind : DocumentSymbol_Progress_Report_Variant :=
        DocumentSymbol_Progress_Report_Variant'First)
   is
   record
      case Kind is
         when Variant_1 =>
            Variant_1 : LSP.Structures.SymbolInformation_Vector;
         when Variant_2 =>
            Variant_2 : LSP.Structures.DocumentSymbol_Vector;
      end case;
   end record;

   package FoldingRange_Vectors is new Ada.Containers.Vectors
     (Positive, FoldingRange, "=");

   type FoldingRange_Vector is
   new FoldingRange_Vectors.Vector with null record;

   subtype FoldingRange_Vector_Or_Null is FoldingRange_Vector;

   subtype TextEdit_Vector_Or_Null is TextEdit_Vector;

   type Hover_Or_Null (Is_Null : Boolean := True) is record
      case Is_Null is
         when True =>
            null;
         when False =>
            Value : Hover;
      end case;
   end record;

   package InlayHint_Vectors is new Ada.Containers.Vectors
     (Positive, InlayHint, "=");

   type InlayHint_Vector is new InlayHint_Vectors.Vector with null record;

   subtype InlayHint_Vector_Or_Null is InlayHint_Vector;

   package InlineValue_Vectors is new Ada.Containers.Vectors
     (Positive, InlineValue, "=");

   type InlineValue_Vector is new InlineValue_Vectors.Vector with null record;

   subtype InlineValue_Vector_Or_Null is InlineValue_Vector;

   type LinkedEditingRanges_Or_Null (Is_Null : Boolean := True) is record
      case Is_Null is
         when True =>
            null;
         when False =>
            Value : LinkedEditingRanges;
      end case;
   end record;

   package Moniker_Vectors is new Ada.Containers.Vectors
     (Positive, Moniker, "=");

   type Moniker_Vector is new Moniker_Vectors.Vector with null record;

   subtype Moniker_Vector_Or_Null is Moniker_Vector;

   package CallHierarchyItem_Vectors is new Ada.Containers.Vectors
     (Positive, CallHierarchyItem, "=");

   type CallHierarchyItem_Vector is
   new CallHierarchyItem_Vectors.Vector with null record;

   subtype CallHierarchyItem_Vector_Or_Null is CallHierarchyItem_Vector;

   type PrepareRenameResult_Or_Null (Is_Null : Boolean := True) is record
      case Is_Null is
         when True =>
            null;
         when False =>
            Value : PrepareRenameResult;
      end case;
   end record;

   package TypeHierarchyItem_Vectors is new Ada.Containers.Vectors
     (Positive, TypeHierarchyItem, "=");

   type TypeHierarchyItem_Vector is
   new TypeHierarchyItem_Vectors.Vector with null record;

   subtype TypeHierarchyItem_Vector_Or_Null is TypeHierarchyItem_Vector;

   subtype Location_Vector_Or_Null is Location_Vector;

   type WorkspaceEdit_Or_Null (Is_Null : Boolean := True) is record
      case Is_Null is
         when True =>
            null;
         when False =>
            Value : WorkspaceEdit;
      end case;
   end record;

   type SelectionRange_Vector is tagged private with
     Variable_Indexing => Get_SelectionRange_Variable_Reference,
     Constant_Indexing => Get_SelectionRange_Constant_Reference;

   subtype SelectionRange_Vector_Or_Null is SelectionRange_Vector;

   type SemanticTokens_Or_Null (Is_Null : Boolean := True) is record
      case Is_Null is
         when True =>
            null;
         when False =>
            Value : SemanticTokens;
      end case;
   end record;

   type Tokens_Delta_Result_Variant is (Variant_1, Variant_2, Variant_3);

   type Tokens_Delta_Result
     (Kind : Tokens_Delta_Result_Variant := Tokens_Delta_Result_Variant'First)
   is
   record
      case Kind is
         when Variant_1 =>
            Variant_1 : LSP.Structures.SemanticTokens;
         when Variant_2 =>
            Variant_2 : LSP.Structures.SemanticTokensDelta;
         when Variant_3 =>
            Variant_3 : LSP.Structures.Null_Record;
      end case;
   end record;

   type SemanticTokensPartialResult_Or_SemanticTokensDeltaPartialResult
     (Is_SemanticTokensPartialResult : Boolean := True) is
   record
      case Is_SemanticTokensPartialResult is
         when True =>
            SemanticTokensPartialResult : LSP.Structures
              .SemanticTokensPartialResult;
         when False =>
            SemanticTokensDeltaPartialResult : LSP.Structures
              .SemanticTokensDeltaPartialResult;
      end case;
   end record;

   type SignatureHelp_Or_Null (Is_Null : Boolean := True) is record
      case Is_Null is
         when True =>
            null;
         when False =>
            Value : SignatureHelp;
      end case;
   end record;

   type MessageActionItem_Or_Null (Is_Null : Boolean := True) is record
      case Is_Null is
         when True =>
            null;
         when False =>
            Value : MessageActionItem;
      end case;
   end record;

   type LSPAny_Or_Null (Is_Null : Boolean := True) is record
      case Is_Null is
         when True =>
            null;
         when False =>
            Value : LSPAny;
      end case;
   end record;

   package WorkspaceSymbol_Vectors is new Ada.Containers.Vectors
     (Positive, WorkspaceSymbol, "=");

   type WorkspaceSymbol_Vector is
   new WorkspaceSymbol_Vectors.Vector with null record;

   type Symbol_Result_Variant is (Variant_1, Variant_2, Variant_3);

   type Symbol_Result
     (Kind : Symbol_Result_Variant := Symbol_Result_Variant'First) is
   record
      case Kind is
         when Variant_1 =>
            Variant_1 : LSP.Structures.SymbolInformation_Vector;
         when Variant_2 =>
            Variant_2 : LSP.Structures.WorkspaceSymbol_Vector;
         when Variant_3 =>
            Variant_3 : LSP.Structures.Null_Record;
      end case;
   end record;

   type Symbol_Progress_Report_Variant is (Variant_1, Variant_2);

   type Symbol_Progress_Report
     (Kind : Symbol_Progress_Report_Variant :=
        Symbol_Progress_Report_Variant'First)
   is
   record
      case Kind is
         when Variant_1 =>
            Variant_1 : LSP.Structures.SymbolInformation_Vector;
         when Variant_2 =>
            Variant_2 : LSP.Structures.WorkspaceSymbol_Vector;
      end case;
   end record;

   function Length (Self : DocumentSymbol_Vector) return Natural;

   procedure Clear (Self : in out DocumentSymbol_Vector);

   procedure Append
     (Self : in out DocumentSymbol_Vector; Value : DocumentSymbol);

   type DocumentSymbol_Variable_Reference
     (Element : not null access DocumentSymbol) is
   null record with
     Implicit_Dereference => Element;

   function Get_DocumentSymbol_Variable_Reference
     (Self  : aliased in out DocumentSymbol_Vector;
      Index : Positive)
      return DocumentSymbol_Variable_Reference with
     Inline;

   type DocumentSymbol_Constant_Reference
     (Element : not null access constant DocumentSymbol) is
   null record with
     Implicit_Dereference => Element;

   function Get_DocumentSymbol_Constant_Reference
     (Self  : aliased DocumentSymbol_Vector;
      Index : Positive)
      return DocumentSymbol_Constant_Reference with
     Inline;

   function Length (Self : SelectionRange_Vector) return Natural;

   procedure Clear (Self : in out SelectionRange_Vector);

   procedure Append
     (Self : in out SelectionRange_Vector; Value : SelectionRange);

   type SelectionRange_Variable_Reference
     (Element : not null access SelectionRange) is
   null record with
     Implicit_Dereference => Element;

   function Get_SelectionRange_Variable_Reference
     (Self  : aliased in out SelectionRange_Vector;
      Index : Positive)
      return SelectionRange_Variable_Reference with
     Inline;

   type SelectionRange_Constant_Reference
     (Element : not null access constant SelectionRange) is
   null record with
     Implicit_Dereference => Element;

   function Get_SelectionRange_Constant_Reference
     (Self  : aliased SelectionRange_Vector;
      Index : Positive)
      return SelectionRange_Constant_Reference with
     Inline;

   function Is_Set (Self : SelectionRange_Optional) return Boolean;
   function Value (Self : SelectionRange_Optional) return SelectionRange;
   procedure Set
     (Self : in out SelectionRange_Optional; Value : SelectionRange);
   procedure Clear (Self : in out SelectionRange_Optional);

private

   type DocumentSymbol_Array is
     array (Positive range <>) of aliased DocumentSymbol;
   type DocumentSymbol_Array_Access is access all DocumentSymbol_Array;

   type DocumentSymbol_Vector is new Ada.Finalization.Controlled with record
      Data : DocumentSymbol_Array_Access;
   end record;

   overriding procedure Finalize (Self : in out DocumentSymbol_Vector);
   overriding procedure Adjust (Self : in out DocumentSymbol_Vector);

   type SelectionRange_Array is
     array (Positive range <>) of aliased SelectionRange;
   type SelectionRange_Array_Access is access all SelectionRange_Array;

   type SelectionRange_Vector is new Ada.Finalization.Controlled with record
      Data : SelectionRange_Array_Access;
   end record;

   overriding procedure Finalize (Self : in out SelectionRange_Vector);
   overriding procedure Adjust (Self : in out SelectionRange_Vector);

   type SelectionRange_Access is access all SelectionRange;
   type SelectionRange_Optional is new Ada.Finalization.Controlled with record
      Value : SelectionRange_Access;
   end record;
   overriding procedure Finalize (Self : in out SelectionRange_Optional);
   overriding procedure Adjust (Self : in out SelectionRange_Optional);
end LSP.Structures;
