--
--  Copyright (C) <YEAR>, <COPYRIGHT HOLDER>
--
--  SPDX-License-Identifier: MIT
--

package LSP.Enumerations is
   pragma Preelaborate;

   type CodeActionKind is
     (Empty, QuickFix, Refactor, RefactorExtract, RefactorInline,
      RefactorRewrite, Source, SourceOrganizeImports, SourceFixAll);
   --  A set of predefined code action kinds
   --
   --  @value Empty
   --  Empty kind.
   --
   --  @value QuickFix
   --  Base kind for quickfix actions: 'quickfix'
   --
   --  @value Refactor
   --  Base kind for refactoring actions: 'refactor'
   --
   --  @value RefactorExtract
   --  Base kind for refactoring extraction actions: 'refactor.extract'
   --
   --  Example extract actions:
   --
   --  - Extract method
   --  - Extract function
   --  - Extract variable
   --  - Extract interface from class - ...
   --
   --  @value RefactorInline
   --  Base kind for refactoring inline actions: 'refactor.inline'
   --
   --  Example inline actions:
   --
   --  - Inline function
   --  - Inline variable
   --  - Inline constant
   --  - ...
   --
   --  @value RefactorRewrite
   --  Base kind for refactoring rewrite actions: 'refactor.rewrite'
   --
   --  Example rewrite actions:
   --
   --  - Convert JavaScript function to class - Add or remove parameter -
   --  Encapsulate field - Make method static - Move method to base class - ...
   --
   --  @value Source
   --  Base kind for source actions: `source`
   --
   --  Source code actions apply to the entire file.
   --
   --  @value SourceOrganizeImports
   --  Base kind for an organize imports source action:
   --  `source.organizeImports`
   --
   --  @value SourceFixAll
   --  Base kind for auto-fix source actions: `source.fixAll`.
   --
   --  Fix all actions automatically fix errors that have a clear fix that
   --  do not require user input. They should not suppress errors or perform
   --  unsafe fixes such as generating new types or classes.
   --
   --  @since 3.15.0

   type CodeActionTriggerKind is (Invoked, Automatic);
   --  The reason why code actions were requested.
   --
   --  @since 3.17.0
   --
   --  @value Invoked
   --  Code actions were explicitly requested by the user or by an extension.
   --
   --  @value Automatic
   --  Code actions were requested automatically.
   --
   --  This typically happens when current selection in a file changes, but can
   --  also be triggered when file content changes.

   type CompletionItemKind is
     (Text, Method, A_Function, Constructor, Field, Variable, Class,
      An_Interface, Module, Property, Unit, Value, Enum, Keyword, Snippet,
      Color, File, Reference, Folder, EnumMember, A_Constant, Struct, Event,
      Operator, TypeParameter);
   --  The kind of a completion entry.

   type CompletionItemTag is (Deprecated);
   --  Completion item tags are extra annotations that tweak the rendering of a
   --  completion item.
   --
   --  @since 3.15.0
   --
   --  @value Deprecated
   --  Render a completion as obsolete, usually using a strike-out.

   type CompletionTriggerKind is
     (Invoked, TriggerCharacter, TriggerForIncompleteCompletions);
   --  How a completion was triggered
   --
   --  @value Invoked
   --  Completion was triggered by typing an identifier (24x7 code complete),
   --  manual invocation (e.g Ctrl+Space) or via API.
   --
   --  @value TriggerCharacter
   --  Completion was triggered by a trigger character specified by the
   --  `triggerCharacters` properties of the `CompletionRegistrationOptions`.
   --
   --  @value TriggerForIncompleteCompletions
   --  Completion was re-triggered as current completion list is incomplete

   type DiagnosticSeverity is (Error, Warning, Information, Hint);
   --  The diagnostic's severity.
   --
   --  @value Error
   --  Reports an error.
   --
   --  @value Warning
   --  Reports a warning.
   --
   --  @value Information
   --  Reports an information.
   --
   --  @value Hint
   --  Reports a hint.

   type DiagnosticTag is (Unnecessary, Deprecated);
   --  The diagnostic tags.
   --
   --  @since 3.15.0
   --
   --  @value Unnecessary
   --  Unused or unnecessary code.
   --
   --  Clients are allowed to render diagnostics with this tag faded out
   --  instead of having an error squiggle.
   --
   --  @value Deprecated
   --  Deprecated or obsolete code.
   --
   --  Clients are allowed to rendered diagnostics with this tag strike
   --  through.

   type DocumentHighlightKind is (Text, Read, Write);
   --  A document highlight kind.
   --
   --  @value Text
   --  A textual occurrence.
   --
   --  @value Read
   --  Read-access of a symbol, like reading a variable.
   --
   --  @value Write
   --  Write-access of a symbol, like writing to a variable.

   type ErrorCodes is
     (ParseError, InvalidRequest, MethodNotFound, InvalidParams, InternalError,
      jsonrpcReservedErrorRangeStart, serverErrorStart, ServerNotInitialized,
      UnknownErrorCode, jsonrpcReservedErrorRangeEnd, serverErrorEnd);
   --  Predefined error codes.
   --
   --  @value jsonrpcReservedErrorRangeStart
   --  This is the start range of JSON RPC reserved error codes. It doesn't
   --  denote a real error code. No application error codes should be defined
   --  between the start and end range. For backwards compatibility the
   --  `ServerNotInitialized` and the `UnknownErrorCode` are left in the range.
   --
   --  @since 3.16.0
   --
   --  @value serverErrorStart
   --  @deprecated use jsonrpcReservedErrorRangeStart */
   --
   --  @value ServerNotInitialized
   --  Error code indicating that a server received a notification or request
   --  before the server has received the `initialize` request.
   --
   --  @value jsonrpcReservedErrorRangeEnd
   --  This is the end range of JSON RPC reserved error codes. It doesn't
   --  denote a real error code.
   --
   --  @since 3.16.0
   --
   --  @value serverErrorEnd
   --  @deprecated use jsonrpcReservedErrorRangeEnd */

   type FailureHandlingKind is
     (An_Abort, Transactional, TextOnlyTransactional, Undo);
   --
   --  @value An_Abort
   --  Applying the workspace change is simply aborted if one of the changes
   --  provided fails. All operations executed before the failing operation
   --  stay executed.
   --
   --  @value Transactional
   --  All operations are executed transactional. That means they either all
   --  succeed or no changes at all are applied to the workspace.
   --
   --  @value TextOnlyTransactional
   --  If the workspace edit contains only textual file changes they are
   --  executed transactional. If resource changes (create, rename or delete
   --  file) are part of the change the failure handling strategy is abort.
   --
   --  @value Undo
   --  The client tries to undo the operations already executed. But there is
   --  no guarantee that this is succeeding.

   type FileChangeType is (Created, Changed, Deleted);
   --  The file event type
   --
   --  @value Created
   --  The file got created.
   --
   --  @value Changed
   --  The file got changed.
   --
   --  @value Deleted
   --  The file got deleted.

   type FileOperationPatternKind is (file, folder);
   --  A pattern kind describing if a glob pattern matches a file a folder or
   --  both.
   --
   --  @since 3.16.0
   --
   --  @value file
   --  The pattern matches a file only.
   --
   --  @value folder
   --  The pattern matches a folder only.

   type FoldingRangeKind is (Comment, Imports, Region);
   --  A set of predefined range kinds.
   --
   --  @value Comment
   --  Folding range for a comment
   --
   --  @value Imports
   --  Folding range for an import or include
   --
   --  @value Region
   --  Folding range for a region (e.g. `#region`)

   type InlayHintKind is (A_Type, Parameter);
   --  Inlay hint kinds.
   --
   --  @since 3.17.0
   --
   --  @value A_Type
   --  An inlay hint that for a type annotation.
   --
   --  @value Parameter
   --  An inlay hint that is for a parameter.

   type InsertTextFormat is (PlainText, Snippet);
   --  Defines whether the insert text in a completion item should be
   --  interpreted as plain text or a snippet.
   --
   --  @value PlainText
   --  The primary text to be inserted is treated as a plain string.
   --
   --  @value Snippet
   --  The primary text to be inserted is treated as a snippet.
   --
   --  A snippet can define tab stops and placeholders with `$1`, `$2` and
   --  `${3:foo}`. `$0` defines the final tab stop, it defaults to the end of
   --  the snippet. Placeholders with equal identifiers are linked, that is
   --  typing in one will update others too.
   --
   --  See also:
   --  https://microsoft.github.io/language-server-protocol/specifications/specification-current/#snippet_syntax

   type InsertTextMode is (asIs, adjustIndentation);
   --  How whitespace and indentation is handled during completion item
   --  insertion.
   --
   --  @since 3.16.0
   --
   --  @value asIs
   --  The insertion or replace strings is taken as it is. If the value
   --  is multi line the lines below the cursor will be inserted using the
   --  indentation defined in the string value. The client will not apply
   --  any kind of adjustments to the string.
   --
   --  @value adjustIndentation
   --  The editor adjusts leading whitespace of new lines so that they match
   --  the indentation up to the cursor of the line for which the item is
   --  accepted.
   --
   --  Consider a line like this: <2tabs><cursor><3tabs>foo. Accepting a multi
   --  line completion item is indented using 2 tabs and all following lines
   --  inserted will be indented using 2 tabs as well.

   type LSPErrorCodes is
     (lspReservedErrorRangeStart, RequestFailed, ServerCancelled,
      ContentModified, RequestCancelled, lspReservedErrorRangeEnd);
   --
   --  @value lspReservedErrorRangeStart
   --  This is the start range of LSP reserved error codes. It doesn't denote a
   --  real error code.
   --
   --  @since 3.16.0
   --
   --  @value RequestFailed
   --  A request failed but it was syntactically correct, e.g the method
   --  name was known and the parameters were valid. The error message
   --  should contain human readable information about why the request failed.
   --
   --  @since 3.17.0
   --
   --  @value ServerCancelled
   --  The server cancelled the request. This error code should only be used
   --  for requests that explicitly support being server cancellable.
   --
   --  @since 3.17.0
   --
   --  @value ContentModified
   --  The server detected that the content of a document got modified outside
   --  normal conditions. A server should NOT send this error code if it
   --  detects a content change in it unprocessed messages. The result
   --  even computed on an older state might still be useful for the client.
   --
   --  If a client decides that a result is not of any use anymore the client
   --  should cancel the request.
   --
   --  @value RequestCancelled
   --  The client has canceled a request and a server as detected the cancel.
   --
   --  @value lspReservedErrorRangeEnd
   --  This is the end range of LSP reserved error codes. It doesn't denote a
   --  real error code.
   --
   --  @since 3.16.0

   type MarkupKind is (PlainText, Markdown);
   --  Describes the content type that a client supports in various result
   --  literals like `Hover`, `ParameterInfo` or `CompletionItem`.
   --
   --  Please note that `MarkupKinds` must not start with a `$`. This kinds are
   --  reserved for internal usage.
   --
   --  @value PlainText
   --  Plain text is supported as a content format
   --
   --  @value Markdown
   --  Markdown is supported as a content format

   type MessageType is (Error, Warning, Info, Log);
   --  The message type
   --
   --  @value Error
   --  An error message.
   --
   --  @value Warning
   --  A warning message.
   --
   --  @value Info
   --  An information message.
   --
   --  @value Log
   --  A log message.

   type MonikerKind is (import, export, local);
   --  The moniker kind.
   --
   --  @since 3.16.0
   --
   --  @value import
   --  The moniker represent a symbol that is imported into a project
   --
   --  @value export
   --  The moniker represents a symbol that is exported from a project
   --
   --  @value local
   --  The moniker represents a symbol that is local to a project (e.g. a local
   --  variable of a function, a class not visible outside the project, ...)

   type NotebookCellKind is (Markup, Code);
   --  A notebook cell kind.
   --
   --  @since 3.17.0
   --
   --  @value Markup
   --  A markup-cell is formatted source that is used for display.
   --
   --  @value Code
   --  A code-cell is source code.

   type PositionEncodingKind is (UTF8, UTF16, UTF32);
   --  A set of predefined position encoding kinds.
   --
   --  @since 3.17.0
   --
   --  @value UTF8
   --  Character offsets count UTF-8 code units.
   --
   --  @value UTF16
   --  Character offsets count UTF-16 code units.
   --
   --  This is the default and must always be supported by servers
   --
   --  @value UTF32
   --  Character offsets count UTF-32 code units.
   --
   --  Implementation note: these are the same as Unicode code points, so
   --  this `PositionEncodingKind` may also be used for an encoding-agnostic
   --  representation of character offsets.

   type PrepareSupportDefaultBehavior is (Identifier);
   --
   --  @value Identifier
   --  The client's default behavior is to select the identifier according the
   --  to language's syntax rule.

   type ResourceOperationKind is (Create, Rename, Delete);
   --
   --  @value Create
   --  Supports creating new files and folders.
   --
   --  @value Rename
   --  Supports renaming existing files and folders.
   --
   --  @value Delete
   --  Supports deleting existing files and folders.

   type SemanticTokenModifiers is
     (declaration, definition, readonly, static, deprecated, an_abstract,
      async, modification, documentation, defaultLibrary);
   --  A set of predefined token modifiers. This set is not fixed an clients
   --  can specify additional token types via the corresponding client
   --  capabilities.
   --
   --  @since 3.16.0

   type SemanticTokenTypes is
     (namespace, a_type, class, enum, an_interface, struct, typeParameter,
      parameter, variable, property, enumMember, event, a_function, method,
      macro, keyword, modifier, comment, string, number, regexp, operator,
      decorator);
   --  A set of predefined token types. This set is not fixed an clients
   --  can specify additional token types via the corresponding client
   --  capabilities.
   --
   --  @since 3.16.0
   --
   --  @value a_type
   --  Represents a generic type. Acts as a fallback for types which can't be
   --  mapped to a specific type like class or enum.
   --
   --  @value decorator
   --  @since 3.17.0

   type SignatureHelpTriggerKind is (Invoked, TriggerCharacter, ContentChange);
   --  How a signature help was triggered.
   --
   --  @since 3.15.0
   --
   --  @value Invoked
   --  Signature help was invoked manually by the user or by a command.
   --
   --  @value TriggerCharacter
   --  Signature help was triggered by a trigger character.
   --
   --  @value ContentChange
   --  Signature help was triggered by the cursor moving or by the document
   --  content changing.

   type SymbolKind is
     (File, Module, Namespace, A_Package, Class, Method, Property, Field,
      Constructor, Enum, An_Interface, A_Function, Variable, A_Constant,
      String, Number, Boolean, An_Array, Object, Key, A_Null, EnumMember,
      Struct, Event, Operator, TypeParameter);
   --  A symbol kind.

   type SymbolTag is (Deprecated);
   --  Symbol tags are extra annotations that tweak the rendering of a symbol.
   --
   --  @since 3.16
   --
   --  @value Deprecated
   --  Render a symbol as obsolete, usually using a strike-out.

   type TextDocumentSaveReason is (Manual, AfterDelay, FocusOut);
   --  Represents reasons why a text document is saved.
   --
   --  @value Manual
   --  Manually triggered, e.g. by the user pressing save, by starting
   --  debugging, or by an API call.
   --
   --  @value AfterDelay
   --  Automatic after a delay.
   --
   --  @value FocusOut
   --  When the editor lost focus.

   type TextDocumentSyncKind is (None, Full, Incremental);
   --  Defines how the host (editor) should sync document changes to the
   --  language server.
   --
   --  @value None
   --  Documents should not be synced at all.
   --
   --  @value Full
   --  Documents are synced by always sending the full content of the document.
   --
   --  @value Incremental
   --  Documents are synced by sending the full content on open. After that
   --  only incremental updates to the document are sent.

   type TokenFormat is (Relative);

   type TraceValues is (Off, Messages, Verbose);
   --
   --  @value Off
   --  Turn tracing off.
   --
   --  @value Messages
   --  Trace messages only.
   --
   --  @value Verbose
   --  Verbose message tracing.

   type UniquenessLevel is (document, project, group, scheme, global);
   --  Moniker uniqueness level to define scope of the moniker.
   --
   --  @since 3.16.0
   --
   --  @value document
   --  The moniker is only unique inside a document
   --
   --  @value project
   --  The moniker is unique inside a project for which a dump got created
   --
   --  @value group
   --  The moniker is unique inside the group to which a project belongs
   --
   --  @value scheme
   --  The moniker is unique inside the moniker scheme.
   --
   --  @value global
   --  The moniker is globally unique

   type WatchKind is (Create, Change, Delete);
   --
   --  @value Create
   --  Interested in create events.
   --
   --  @value Change
   --  Interested in change events
   --
   --  @value Delete
   --  Interested in delete events

end LSP.Enumerations;
